
(progn
  ;; This code is modified from Matt Duck's code:
  ;; - https://www.mattduck.com/2021-06-exwm-screenshots.html
  ;; - https://github.com/mattduck

  (defvar dhnam/screenshot-command (concat dhnam/lib-root-dir "common/dependent/screenshot.sh"))

  (defun dhnam/screenshot-run-picom-daemon ()
    (interactive)
    (let ((picom-daemon-command (concat "source " dhnam/lib-root-dir "common/dependent/picom-daemon.sh")))
      (shell-command picom-daemon-command)))

  (defun dhnam/screenshot-image-selection ()
    (interactive)
    (shell-command (concat dhnam/screenshot-command " --image-selection")))

  (defun dhnam/screenshot-video-selection-start ()
    (interactive)
    (shell-command (concat dhnam/screenshot-command " --video-selection-start")))

  (defun dhnam/screenshot-video-stop ()
    (interactive)
    (shell-command (concat dhnam/screenshot-command " --video-stop"))))


(provide 'dhnam-screenshot)
