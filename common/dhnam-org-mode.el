
(defun dhnam/org-babel-execute-region (beg end &optional arg)
  (interactive "r")
  (if (use-region-p)
      (save-excursion
        (save-restriction
          (narrow-to-region beg end)
          (org-babel-execute-buffer arg)
          (comment (widen))))
    (org-babel-execute-buffer arg)))

(provide 'dhnam-org-mode)
