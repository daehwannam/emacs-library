
(defun dhnam/xterm-paste-without-key-chord (event)
  (interactive "e")
  (if key-chord-mode
      (progn
        (key-chord-mode 0)
        (xterm-paste event)
        (key-chord-mode 1))
    (xterm-paste event)))


(provide 'dhnam-tui)
