
(defun eval-last-sexp-or-region (eval-last-sexp-arg-internal)
  (interactive "P")

  (if (use-region-p)
      (call-interactively 'eval-region)
    (eval-last-sexp eval-last-sexp-arg-internal)))

(provide 'dhnam-elisp)
