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

;;; Code:

(require 'rx)
(require 'smie)
(require 'compile)
(require 'json)

;; Customization

(defgroup pulsar nil
  "Pulsar temporal functional language"
  :prefix 'pulsar
  :group 'languages)

(defcustom pulsar-command "pulsar"
  "The command to be run for Pulsar."
  :group 'pulsar
  :type 'string
  :tag "Command for Pulsar"
  :options '("pulsar"))

(defcustom pulsar-beam-command "pulsar-beam"
  "The command to be run for Pulsar."
  :group 'pulsar
  :type 'string
  :tag "Command for Pulsar Beam"
  :options '("beam"))

;; Global variables

(defvar pulsar--debug
  nil
  "Set to non-nil to show debugging messages")

(defun pulsar--column-number-at-pos (pos)
  (save-excursion
    (goto-char pos)
    (current-column)))

(defun pulsar--pair-of-pos (pos)
  (let ((lnum (line-number-at-pos pos))
        (cnum (pulsar--column-number-at-pos pos)))
    `(,lnum ,cnum)))

;; Communication with pulsar-beam

(defun pulsar--beam-request (req)
  (when (buffer-modified-p) (save-buffer))
  (let ((command (format "%s \'%s\'" pulsar-beam-command req)))
    (when pulsar--debug (message "call: %s" command))
    (let ((response (shell-command-to-string command)))
      (when pulsar--debug (message "resp: %s" response))
      response)))

(defun pulsar--json-show-type (file pos)
  (json-encode
   `(:tag "show"
     :value (:file ,file :pos ,(pulsar--pair-of-pos pos) :kind "Type"))))

(defun pulsar--json-diagnosis (file)
  (json-encode
   `(:tag "diagnosis"
     :value (:file ,file))))

(defun pulsar--json-passes ()
  (json-encode
   `(:tag "passes"
     :value ())))

(defun pulsar--pos-of-array (arr)
  (pcase arr
    (`(,lnum ,cnum)
     (save-excursion
       (goto-line lnum)
       (move-to-column cnum)
       (point)))))

(defun pulsar--unpack-loc (loc)
  (pcase loc
    (`(:file ,file :start ,start :stop ,stop)
     `(,file ,(pulsar--pos-of-array start) ,(pulsar--pos-of-array stop)))
    (_ (error "not a loc %S" loc))))

(defvar pulsar--show-overlay
  nil)

(defun pulsar--clear-show-overlay-look ()
  (move-overlay pulsar--show-overlay 1 1))

(defun pulsar--after-changes-hook (beg end old-len)
  (pulsar-clear-diagnosis-overlays))

(defun pulsar--highlight-range (loc message)
  (pcase (pulsar--unpack-loc loc)
    (`(,file ,start ,end)
     (move-overlay pulsar--show-overlay start end)
     (message "%s" message))))

(defun pulsar--highlight-diagnosis (diag)
  (pcase diag
    (`(:loc ,loc :pass ,pass :kind ,kind :body ,message)
     (pcase (pulsar--unpack-loc loc)
       (`(,file ,start ,end)
        (let ((ov (make-overlay start end))
              (color (pcase kind
                       ("Error" "red")
                       ("Warning" "yellow")
                       ("Info" "green")
                       (_ (error "unknown diagnosis kind %s" kind)))))
          (overlay-put ov 'name "pulsar-diagnosis")
          (overlay-put ov 'face `(:underline (:color ,color :style wave)))
          (overlay-put ov 'help-echo message)))))
    ))

(defun pulsar--process-response (out)
  (let* ((json-object-type 'plist)
         (json-array-type 'list)
         (resp (json-read-from-string out)))
    (pcase resp
      (`(:tag "ok" :value ,ok)
       (pcase ok
         (`(:tag "silent" :value nil) ())
         (`(:tag "show" :value (:loc ,loc :content ,content))
          (pulsar--highlight-range loc content))
         (`(:tag "diagnoses" :value ,diags)
          (dolist (diag diags) (pulsar--highlight-diagnosis diag)))
         (`(:tag "passes" :value ,passes)
          passes)
         (_ (message "unknown answer %S" resp))))
      (_ ())
      )))

(defun pulsar--compile-until-pass (filename pass)
  (let* ((temp-dir (make-temp-file "pulsar" t))
         (basen (file-name-nondirectory (file-name-sans-extension filename)))
         (command (concat
                   pulsar-command " -serialize-dir " temp-dir
                   " -serialize " pass " -stop-after " pass " " filename)))
    (progn
      (shell-command-to-string command)
      (concat (file-name-as-directory temp-dir) basen "." pass ".pul"))))

;; Interactive features

(defun pulsar-clear-diagnosis-overlays ()
  (interactive)
  (remove-overlays nil nil "pulsar-diagnosis"))

(defun pulsar-show-type-at-point ()
  (interactive)
  (let ((req (pulsar--json-show-type (buffer-file-name) (point))))
    (pulsar--process-response (pulsar--beam-request req))))

(defun pulsar-diagnosis-buffer ()
  (interactive)
  (pulsar--process-response
   (pulsar--beam-request (pulsar--json-diagnosis (buffer-file-name)))))

(defun pulsar-debug-toggle ()
  "Toggle debugging features on and off"
  (interactive)
  (setq pulsar--debug (not pulsar--debug))
  (if pulsar--debug
      (message "pulsar: enabled debug mode")
    (message "pulsar: disabled debug mode")))

(defun pulsar-display-pass-result ()
  (interactive)
  (let* ((out (pulsar--beam-request (pulsar--json-passes)))
         (passes (pulsar--process-response out))
         (pass (completing-read "Display pass result: " passes nil t)))
    (find-file (pulsar--compile-until-pass (buffer-file-name) pass))))

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
    (modify-syntax-entry ?< "w" st)
    (modify-syntax-entry ?{ "w" st)
    (modify-syntax-entry ?} "w" st)
    (modify-syntax-entry ?! "w" st)
    st)
  "Syntax table for `pulsar-mode'.")

;; Syntax coloring

(defconst pulsar-keywords-regex
  (rx
   word-boundary
   (group-n 1 (or "where" "rec" "seq" "par" "when" "merge" "by" "let" "in"
                  "extern" "λ" "=>" "⇒" ">>" "<<" "{!" "!}"))
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

;; Compilation

(defconst pulsar--compilation-buffer-name
  "*Pulsar*"
  "The name to use for Pulsar compilation buffers.")

(defun pulsar--compilation-buffer-name-function (_mode)
  "Compute a buffer name for the `pulsar-mode' compilation buffer."
  pulsar--compilation-buffer-name)

(defun pulsar-compile-buffer ()
  "Compile the current file with Pulsar."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if filename
        (progn
          (when (buffer-modified-p) (save-buffer))
          (let* ((dir (file-name-directory filename))
                 (file (file-name-nondirectory filename))
                 (command (concat pulsar-command " " file))

                 ;; Emacs compile config stuff - these are special vars
                 (compilation-buffer-name-function
                  'pulsar--compilation-buffer-name-function)
                 (default-directory dir))
            (compile command)))
      (error "Buffer has no file name"))))

(defconst pulsar--compilation-error-regex
  (rx
   line-start
   (group (+ (any word "," "." "_"))) whitespace
   (group (+ num)) ":" (group (+ num))
   "-" (group (+ num)) ":" (group (+ num))
   whitespace "[" (or (group "error") (group "warning") (group "info")) "]")
  "Regex matching the compiler errors")

(defun pulsar--install-compilation-error-regex ()
  (interactive)
  (add-to-list 'compilation-error-regexp-alist
               `(,pulsar--compilation-error-regex
                 1 (2 . 4) (3 . 5) nil nil
                 (6 compilation-error-face)
                 (7 compilation-error-face)
                 (8 compilation-error-face)
                 )))

;; Key map

(defvar pulsar-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-d") 'pulsar-debug-toggle)
    (define-key map (kbd "C-c C-c") 'pulsar-compile-buffer)
    (define-key map (kbd "C-c C-t") 'pulsar-show-type-at-point)
    (define-key map (kbd "C-c C-l") 'pulsar-diagnosis-buffer)
    (define-key map (kbd "C-c C-p") 'pulsar-display-pass-result)
    map)
  "Keymap for `pulsar-mode'.")

;; Main entry point

;;;###autoload
(define-derived-mode pulsar-mode prog-mode "Pulsar"
  "A major mode for editing Pulsar files.
\\{pulsar-mode-map}"
  :syntax-table pulsar-mode-syntax-table
  :group 'pulsar

  (setq-local comment-start "(*")
  (setq-local comment-start "*)")

  ;; Interactive features
  (make-local-variable 'pulsar--show-overlay)
  (setq pulsar--show-overlay (make-overlay 1 1))
  (overlay-put pulsar--show-overlay 'face '(:inverse-video t))
  (add-hook 'echo-area-clear-hook 'pulsar--clear-show-overlay-look nil t)
  (add-hook 'after-change-functions 'pulsar--after-changes-hook)
  (setq inhibit-modification-hooks nil)

  ;; Font-lock
  (setq-local font-lock-defaults '(pulsar-font-lock-keywords))
  (setq-local font-lock-multiline t)
  (setq-local
   font-lock-extend-after-change-region-function
   'pulsar-font-lock-extend-after-change-region-function)

  ;; Indentation
  (smie-setup pulsar-smie-grammar 'pulsar-smie-rules)

  ;; Flycheck
  (eval-after-load 'flycheck
    '(progn
       (flycheck-define-checker
        pulsar
        "A Pulsar program checker."
        :command ("pulsar" source)
        :error-patterns
        ((error line-start
                (file-name) whitespace
                line ":" column "-" (+ num) ":" (+ num)
                whitespace "[error]" whitespace (+ word)
                ": " (message) line-end))
        :modes pulsar-mode)

       (add-to-list 'flycheck-checkers 'pulsar)))

  ;; Compile
  (pulsar--install-compilation-error-regex)
  )

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.pul\\'" . pulsar-mode))

;;;###autoload
(provide 'pulsar-mode)
;;; pulsar-mode.el ends here
