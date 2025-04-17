

(defvar xset-r-rate-value-delay 250)
(defvar xset-r-rate-value-rate 60)

(defun dhnam/xset-r-rate ()
  (interactive)
  (shell-command
   (format
    "xset r rate %s %s"
    xset-r-rate-value-delay
    xset-r-rate-value-rate)))


(provide 'dhnam-linux)
