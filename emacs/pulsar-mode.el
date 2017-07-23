;;; pulsar-mode.el --- major mode for editing Pulsar -*- lexical-binding: t; -*-

;;; Copyright (C) 2017 Adrien Guatto

;; Author: Adrien Guatto (adrien@guatto.org)
;; Version: 0.1
;; Created: 21 July 2017
;; Keywords: languages
;; Homepage: http://github.com/adrieng/pulsar

;; This file is not part of GNU Emacs.

;;; License:

;; You can redistribute this program and/or modify it under the terms of the GNU
;; General Public License version 3.

(require 'rx)
(require 'smie)

;; Syntax table

(defvar pulsar-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?\( "()1n" st)
    (modify-syntax-entry ?* ". 23n" st)
    (modify-syntax-entry ?\) ")(4" st)
    (modify-syntax-entry ?' "w" st)
    (modify-syntax-entry ?⊛ "w" st)
    (modify-syntax-entry ?λ "w" st)
    (modify-syntax-entry ?→ "w" st)
    (modify-syntax-entry ?× "w" st)
    (modify-syntax-entry ?⇒ "w" st)
    (modify-syntax-entry ?- "w" st)
    (modify-syntax-entry ?> "w" st)
    st)
  "Syntax table for `pulsar-mode'.")

;; Syntax coloring

(defconst pulsar-keywords-regex
  (rx
   word-boundary
   (group-n 1 (or "where" "rec" "seq" "par" "when" "merge" "by" "let" "in"
                  "extern" "λ" "=>" "⇒"))
   word-boundary
   ))

(defconst pulsar-types-regex
  (rx
   word-boundary
   (group-n 1 (or "bool" "char" "float" "int" "unit" "stream"
                  "→" "->" "⊛" "<*>" "×"))
   word-boundary
   ))

(defconst pulsar-coercions-regex
  (rx
   word-boundary
   (group-n 1 (or "id" "wrap" "unwrap" "concat" "decat" "dist" "fact" "infl"
                  "defl" "delay"))
   word-boundary
   ))

(defconst pulsar-constants-regex
  (rx
   word-boundary
   (group-n 1 (or "true" "false" (+ digit)))
   word-boundary
   ))

(defconst pulsar-warps-regex
  (rx
   "\`"
   (* (any "0-9" "{" "}" "^" " "))
   "(" (+ (any "0-9" "{" "}" "^" " " "ω")) ")"
   ))

(defconst pulsar-extern-ident-regex
  (rx "extern" (+ whitespace) (group-n 1 (+ word)) not-wordchar))

(defconst pulsar-def-ident-regex
  (rx
   (any "{" ";") (+ (any whitespace "\n"))
   (group-n 1 (+ word))
   ))

(defvar pulsar-font-lock-keywords
  `(
    (,pulsar-warps-regex 0 font-lock-builtin-face)
    (,pulsar-types-regex 1 font-lock-type-face)
    (,pulsar-coercions-regex 1 font-lock-constant-face)
    (,pulsar-keywords-regex 1 font-lock-keyword-face)
    (,pulsar-constants-regex 1 font-lock-constant-face)
    (,pulsar-def-ident-regex 1 font-lock-function-name-face)
    (,pulsar-extern-ident-regex 1 font-lock-function-name-face)
    )
  "Syntax highlighting specification of `pulsar-mode'.")

(defun pulsar-find-largest-def-end-before-pos (pos)
  (save-excursion
    (goto-char pos)
    (skip-chars-backward "a-zA-Z0-9")
    (skip-chars-backward " \t\n")
    (let ((c (char-before (point))))
      (cond
       ((char-equal c ?\{) (- (point) 1))
       ((char-equal c ?\;) (- (point) 1))
       (t pos)))))

(defun pulsar-find-smallest-def-end-after-pos (pos)
  (save-excursion
    (goto-char pos)
    (skip-chars-backward " \t\n")
    (let ((c (char-before (point))))
      (cond
       ((not (or (char-equal c ?\{) (char-equal c ?\;))) pos)
       (t
        (skip-chars-forward " \n")
        (skip-chars-forward "a-zA-Z0-9")
        (point))))))

(defun pulsar-font-lock-extend-after-change-region-function
    (beg end old-len)
  "Extends the region to refontify in `pulsar-mode'."
  (let ((new-beg (pulsar-find-largest-def-end-before-pos beg))
        (new-end (pulsar-find-smallest-def-end-after-pos end)))
    (cons new-beg new-end)
    ))

;; Indentation

(defvar pulsar-indent-level
  2
  "Basic indentation level for `pulsar-mode'.")

(defvar pulsar-indent-arg
  2
  "Indentation level of arguments list for `pulsar-mode'.")

;; (defun pulsar-indent-function ()
;;   "Indentation function for `pulsar-mode'."
;;    (let ((savep (> (current-column) (current-indentation)))
;;          (indent (condition-case nil (max (pulsar-calculate-indentation) 0)
;;                    (error 0))))
;;      (message "indent = %d" indent)
;;      (if savep
;;          (save-excursion (indent-line-to indent))
;;        (indent-line-to indent))))

;; (defun pulsar-calculate-indentation ()
;;   (save-excursion
;;     (move-to-column 0)
;;     (skip-chars-backward " \t\n")
;;     (let ((c (char-before (point)))
;;           (i (current-indentation)))
;;       (message "char-before: %c" c)
;;       (cond
;;        ((eq c ?\;) (+ i pulsar-indent-basic))
;;        ((eq c ?\{) (+ i pulsar-indent-basic))
;;        (t i)))))

(defconst pulsar-smie-grammar
  (smie-prec2->grammar
   (smie-bnf->prec2
    '(
      (id)
      (exp ("let" block "in" exp)
           (exp "where" block)
           ("λ" tyann "⇒" exp)
           (exp "::" exp)
           (siexp))
      (siexp (id)
             ("(" exps ")"))
      (exps (exps "," exps)
            (exp))
      (tyann (siexp ":" type))
      (type (type "×" type)
            (type "→" type))
      (block ("rec" block_eqs)
             ("seq" block_eqs)
             ("par" block_eqs)
             (block_eqs))
      (block_eqs ("{" eqs "}"))
      (eqs (eqs ";" eqs)
           (eq))
      (eq (tyann "=" exp))
      (decl ("extern" tyann))
      )
    '((assoc "::" "⇒" ",")
      (assoc "×")
      (assoc "→")
      (assoc "in")
      (assoc "where")
      (assoc ";"))
    )))

(defun pulsar-smie-rules (kind cond)
  (pcase (cons kind cond)
    (`(:elem . basic) pulsar-indent-level)
    (`(:after ":") pulsar-indent-level)
    (`(:after "in") (smie-rule-parent 0))
    (`(:before . ":") pulsar-indent-level)
    (`(:before . "extern") '(column . 0))
    (`(:before . "rec") (when (smie-rule-bolp) '(column . 0)))
    (`(:before . "{")
     (when (smie-rule-parent-p "rec" "seq" "par") (smie-rule-parent 0)))
    (`(:after . "=") pulsar-indent-level)
    ))

;; Key map

(defvar pulsar-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap for `pulsar-mode'.")

;; Main entry point

;;;###autoload
(define-derived-mode pulsar-mode prog-mode "Pulsar"
  "A major mode for editing Pulsar files.
\\{pulsar-mode-map}"
  :syntax-table pulsar-mode-syntax-table
  (setq-local comment-start "(*")
  (setq-local comment-start "*)")

  ;; Font-lock
  (setq-local font-lock-defaults '(pulsar-font-lock-keywords))
  (setq-local font-lock-multiline t)
  (setq-local
   font-lock-extend-after-change-region-function
   'pulsar-font-lock-extend-after-change-region-function)

  ;; Indentation
  (smie-setup pulsar-smie-grammar 'pulsar-smie-rules)
  )

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.pul\\'" . pulsar-mode))

;;;###autoload
(provide 'pulsar-mode)
