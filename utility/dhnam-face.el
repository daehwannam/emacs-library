(defun dhnam/set-cursor-color (color)
  (if (display-graphic-p)
      (set-cursor-color color)
    (progn
      ;; https://stackoverflow.com/questions/13806363/emacs-inside-terminal-change-cursor-color-dynamically
      (send-string-to-terminal (format "\033]12;%s\007" color)))))

(provide 'dhnam-face)
