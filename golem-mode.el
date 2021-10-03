;;; package --- Summary
;;; Commentary:
;;; Code:

(require 'compile)

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

(defconst golem-highlights
  (rx-let ((ident (seq alpha (* (any alnum "_")))))
    (list
     (cons (rx line-start (or "proc" "type")) font-lock-keyword-face)
     (cons "\t+" font-lock-warning-face)
     (list (rx (group ident) "(") 1 font-lock-function-name-face)
     (list (rx ":" (* " ") (group ident)) 1 font-lock-type-face)
     (list (rx (group-n 1 "struct") (* " ") "{") 1 font-lock-keyword-face)
     )))

;;;###autoload
(define-derived-mode golem-mode prog-mode "golem"
  "major mode for editing golem language code"
  (set (make-local-variable 'font-lock-defaults) '(golem-highlights))
  (set (make-local-variable 'compile-command)
       (concat "golem " (file-relative-name buffer-file-name))))

(provide 'golem-mode)
;;; golem-mode.el ends here
