;;; package --- Summary
;;; Commentary:
;;; Code:

(require 'compile)

;; TODO if and for constructs don't have a required block statement anymore.
(defconst golem-highlights
  (rx-let ((ident (seq alpha (* (any alnum "_"))))
           ;; keywords in golem only have a meaning when placed at the
           ;; beginning of a statement.
           (stmt-context (seq (or line-start ";" "{") (* " "))))
    (list
     (cons (rx (? "-") bow (1+ digit) (? "." (1+ digit)) eow) font-lock-constant-face)
     (list (rx stmt-context (group (or "return" "var" "let" "if" "proc" "type")) eow) 1 font-lock-keyword-face)
     ;; match else only when it follows a closing } (not correct anymore)
     (list (rx bow (or "and" "or") eow) 0 font-lock-keyword-face)
     (list (rx bow (or "true" "false") eow) 0 font-lock-constant-face)
     (list (rx "}" (* " ") (group "else") eow) 1 font-lock-keyword-face)
     (cons "\t+" font-lock-warning-face)
     ;; anchored pattern to match `in' keyword in a for loop
     (list (rx stmt-context (group "for") eow) (list 1 font-lock-keyword-face)
           (list (rx bow (or "do" "in") eow) nil nil (list 0 font-lock-keyword-face)))
     (list (rx stmt-context (group "if") eow) (list 1 font-lock-keyword-face)
           (list (rx bow (or "do" "else") eow) nil nil (list 0 font-lock-keyword-face)))
     (list (rx ":" (* " ") (group ident)) 1 font-lock-type-face)
     (list (rx (group ident) "(") 1 font-lock-function-name-face)
     (list (rx (group-n 1 "struct") (* " ") "{") 1 font-lock-keyword-face)
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
  (modify-syntax-entry ?\n ">#" golem-mode-syntax-table))

(defun golem-string-escape-matcher (&optional limit)
  "Highlight matcher escape sequence in string within LIMIT."
  (let ((string-escape-regexp
         (rx (or (and "\\" (any "abfnrtv\\'\""))
                 (and "%" (any "%dfs")))))
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
  (setq-local compile-command
              (concat "go build && ./golem build " (file-relative-name buffer-file-name))))

(provide 'golem-mode)
;;; golem-mode.el ends here
