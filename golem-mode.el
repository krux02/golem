;;; package --- Summary
;;; Commentary:
;;; Code:
(require 'compile)
(require 'project)

(defconst golem-highlights
  (rx-let ((ident (seq (any alpha "_") (* (any alnum "_")))))
    (list
     (cons (rx (? "-") bow (1+ digit) (? "." (1+ digit)) eow) font-lock-constant-face)
     (cons (rx bow "0x" (1+ hex) eow) font-lock-constant-face)
     (cons (rx bow (or
                    "const" "for" "while" "do" "in" "notin" "return" "discard" "addr" "var" "let" "if" "proc" "emit" "and" "or" "not" "type" "else" "struct" "union" "trait" "static" "import" "enum") eow) font-lock-keyword-face)
     (cons (rx bow "`" ident "`") font-lock-variable-name-face)
     (cons (rx (+ "\t")) font-lock-warning-face)
     (list (rx bow (or "true" "false") eow) 0 font-lock-constant-face)
     ;; anchored pattern to match `in' keyword in a for loop
     (list (rx ":" (* " ") (group ident)) 1 font-lock-type-face)
     (list (rx (group ident) "(") 1 font-lock-function-name-face)
     (list 'golem-string-escape-matcher 0 font-lock-preprocessor-face 'prepend)
     )))

(add-to-list 'compilation-error-regexp-alist 'golem)
(setf
 (alist-get 'golem compilation-error-regexp-alist-alist)
 `(,(rx
     ;; line-start
     ;; File
     (group-n 1 (1+ (any alnum "\\" "/" "_" "." "-")) ".golem")
     "("
     ;; Line
     (group-n 2 (1+ digit))
     ;; Column -- this parameter is optional when it
     ;; comes from stacktrace (see #171)
     ", " (group-n 3 (1+ digit)) "-" (group-n 4 (1+ digit))
     ") "
     ;; Type -- also this parameter doesn't show up when it
     ;; come from stacktrace
     (or "Error:" (group-n 5 "Warning:") (group-n 6 "Info:")))
   1 2 (3 . 4) (5 . 6)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.golem\\'" . golem-mode))

;; Create the syntax table for this mode.
(defvar golem-mode-syntax-table (make-syntax-table prog-mode-syntax-table) "Syntax table used while in `golem-mode'.")

(progn
  (modify-syntax-entry ?\# "<" golem-mode-syntax-table)
  (modify-syntax-entry ?\n ">#" golem-mode-syntax-table)
  (modify-syntax-entry ?_ "w" golem-mode-syntax-table))

(defun golem-string-escape-matcher (&optional limit)
  "Highlight matcher escape sequence in string within LIMIT."
  (let ((string-escape-regexp
         (rx (or (and "\\" (any "abfnrtv\\'\""))
                 (and "%" (any "%duxXfscv")))))
        res)
    (while
        (and
         (setq res (re-search-forward
                    string-escape-regexp limit t))
         (not (nth 3 (syntax-ppss)))))
    res))



;;;###autoload
(define-derived-mode golem-mode prog-mode "golem"
  "Major mode for editing golem language code."
  :group 'golem

  (setq-local comment-start "#")
  (setq-local comment-start-skip "#+ *")
  (setq-local font-lock-defaults '(golem-highlights))

  (setq-local compilation-directory (project-root (project-current)))
  (setq-local
   compile-command
   (let* ((project-root (project-root (project-current)))
          (project-relative-file-name (file-relative-name  buffer-file-name project-root))
          (raw-file-name (file-name-nondirectory buffer-file-name)))
     (concat
      "go build && ./golem "
      project-relative-file-name))))

(provide 'golem-mode)
;;; golem-mode.el ends here
