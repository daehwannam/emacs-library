
(defun eval-last-sexp-or-region (eval-last-sexp-arg-internal)
  (interactive "P")

  (if (use-region-p)
      (call-interactively 'eval-region)
    (eval-last-sexp eval-last-sexp-arg-internal)))

(defun dhnam/edebug-eval-at-point ()
  (interactive)

  (edebug-eval-expression
   (read
    (if (region-active-p)
        (progn
          (let ((beg (region-beginning))
                (end (region-end)))
            (deactivate-mark)
            (buffer-substring-no-properties beg end)))
      (thing-at-point 'symbol)))))

(provide 'dhnam-elisp)
