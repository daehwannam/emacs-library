
(defun dhnam/org-html-export-to-html-from-cmd ()
  (interactive)
  (shell-command (format "emacs --eval '(progn (setq enable-local-variables :all) (find-file \"%s\") (org-html-export-to-html) (kill-emacs))'" (file-name-nondirectory (buffer-file-name)))))

(provide 'dhnam-cmd-line-eval)
