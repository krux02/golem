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
     (group-n 1 (1+ (in alnum "\\" "/" "_" "." "-")) ".golem")
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

(defconst golem-highlights
      (list
       (cons (rx (or "Sin" "Cos" "Sum")) font-lock-function-name-face)
       (cons (rx (or "Pi" "Infinity")) font-lock-constant-face)))

(define-derived-mode golem-mode prog-mode "golem"
  "major mode for editing golem language code"
  (setq font-lock-defaults '(golem-highlights)))


(provide 'golem-mode)
;;; golem-mode.el ends here
