
(provide 'dhnam-application)

(defun dhnam/app-command-execute-shell (command)
  (interactive (list (read-shell-command "$ ")))
  (start-process-shell-command command nil command))

(defun dhnam/app-command-open-terminal-emulator ()
  (interactive)
  (start-process-shell-command "terminal" nil "kitty"))

(defun dhnam/app-command-open-flameshot-gui ()
  (interactive)
  (start-process-shell-command "screenshot" nil "flameshot gui"))
