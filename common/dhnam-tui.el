
(defun dhnam/xterm-paste-without-key-chord (event)
  (interactive "e")
  (if key-chord-mode
      (progn
        (key-chord-mode 0)
        (xterm-paste event)
        (key-chord-mode 1))
    (xterm-paste event)))

(defun dhnam/clipetty--emit-advice (orig-func &rest args)
  "Advice function for `clipetty--emit' to disable `sit-for', which sleeps for 1 second."

  (dhnam/disable-fn sit-for
    (apply orig-func args)))


(provide 'dhnam-tui)
