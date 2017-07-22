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
    (modify-syntax-entry ?= "w" st)
    (modify-syntax-entry ?> "w" st)
    st)
  "Syntax table for `pulsar-mode'.")

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
   not-wordchar))

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

(defvar pulsar-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap for `pulsar-mode'.")

 ;;;###autoload
(define-derived-mode pulsar-mode prog-mode "Pulsar"
  "A major mode for editing Pulsar files.
\\{pulsar-mode-map}"
  :syntax-table pulsar-mode-syntax-table
  (setq-local comment-start "(*")
  (setq-local comment-start "*)")
  (setq-local font-lock-defaults '(pulsar-font-lock-keywords))
  )

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.pul\\'" . pulsar-mode))

;;;###autoload
(provide 'pulsar-mode)
