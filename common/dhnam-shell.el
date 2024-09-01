

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

(defun dhnam/bash-command-to-string (command)
  (let ((shell-file-name "/bin/bash"))
    (shell-command-to-string command)))

(defun dhnam/insert-command-from-bash-history ()
  "Insert a command in '~/.bash_history'.
Note that '~/.bash_history' is updated only when the current bash shell is closed."
  ;; This code is modified from:
  ;; https://www.reddit.com/r/emacs/comments/fq8lub/ivy_custom_action/?utm_source=share&utm_medium=web3x&utm_name=web3xcss&utm_term=1&utm_content=share_button

  (interactive)
  (let ((command (with-temp-buffer
                   (insert-file-contents-literally "~/.bash_history")
                   (let ((history-list (->
                                        (buffer-string)
                                        (split-string "\n" t)
                                        (cl-delete-duplicates :test #'string=)
                                        (reverse))))
                     (ivy-read "Command: " history-list)))))
    (when command
      (insert command))))

(provide 'dhnam-shell)
