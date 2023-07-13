

(defun dhnam/shell-new-instance (arg)
  (interactive "P")
  (let ((default-directory (if arg
                               (read-directory-name "Directory: ")
                             default-directory)))
    (shell (get-buffer-create (generate-new-buffer-name "*shell*")))))

(comment
  (defun dhnam/shell-new-instance-other-window (count)
    (interactive "p")
    (split-window-sensibly)
    (other-window count)
    (dhnam/shell-new-instance)))

(defun bash-command-to-string (command)
  (let ((shell-file-name "/bin/bash"))
    (shell-command-to-string command)))

(provide 'dhnam-shell)
