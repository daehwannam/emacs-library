
(defun dhnam/push-button-same-window ()
  ;; https://emacs.stackexchange.com/a/33908
  (interactive)
    (let ((display-buffer-overriding-action
           '((display-buffer-reuse-window
              display-buffer-same-window)
             (inhibit-same-window . nil))))
      (call-interactively #'push-button)))

(defun dhnam/xref-goto-xref-same-window ()
  (interactive)
  (let ((display-buffer-overriding-action
         '((display-buffer-reuse-window
            display-buffer-same-window)
           (inhibit-same-window . nil))))
    (call-interactively #'xref-goto-xref)))

(defun dhnam/occur-mode-goto-occurrence-current-window ()
  "Go to the occurrence the current line describes, in the current window."
  (interactive)
  (let ((buffer (current-buffer))
        (pos (occur-mode-find-occurrence)))
    (switch-to-buffer (marker-buffer pos))
    (goto-char pos)
    (next-error-found buffer (current-buffer))
    (run-hooks 'occur-mode-find-occurrence-hook)))

(progn
  (defun dhnam/diff-goto-old-source (&optional other-file event)
    "Jump to the corresponding destination line."
    (interactive (list current-prefix-arg last-input-event))
    (diff-goto-source (not other-file) event))

  (defun dhnam/diff-goto-conditionally (&optional other-file event)
    "Jump to the corresponding line of source or old source"
    (interactive (list current-prefix-arg last-input-event))
    (save-excursion
      (let* ((line-start-point (progn (move-beginning-of-line 1) (point)))
             (first-char (buffer-substring-no-properties line-start-point (1+ line-start-point))))
        (cond
         ((string= first-char "+")
          (diff-goto-source current-prefix-arg event))
         ((string= first-char "-")
          (dhnam/diff-goto-old-source current-prefix-arg event))
         (t
          (diff-goto-source current-prefix-arg event)))))))

(provide 'dhnam-miscellaneous)
