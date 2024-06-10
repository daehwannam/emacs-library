
(defun dhnam/push-button-same-window ()
  (interactive)
  (dhnam/call-command-same-window #'push-button))

(defun dhnam/xref-goto-xref-same-window ()
  (interactive)
  (dhnam/call-command-same-window #'xref-goto-xref))

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
  (defun dhnam/diff-goto-source (&optional other-file event same-window)
    "Jump to the corresponding source line.
This function is modified from `diff-goto-source', where the `same-window' parameter is added
"
    (interactive (list current-prefix-arg last-input-event nil))
    ;; When pointing at a removal line, we probably want to jump to
    ;; the old location, and else to the new (i.e. as if reverting).
    ;; This is a convenient detail when using smerge-diff.
    (if event (posn-set-point (event-end event)))
    (let ((buffer (when event (current-buffer)))
          (reverse (not (save-excursion (beginning-of-line) (looking-at "[-<]")))))
      (pcase-let ((`(,buf ,line-offset ,pos ,src ,_dst ,switched)
                   (diff-find-source-location other-file reverse)))
        (if same-window
            (pop-to-buffer-same-window buf)
          (pop-to-buffer buf))
        (goto-char (+ (car pos) (cdr src)))
        (when buffer (next-error-found buffer (current-buffer)))
        (diff-hunk-status-msg line-offset (xor reverse switched) t))))

  (defun dhnam/diff-goto-old-source (&optional other-file event same-window)
    "Jump to the corresponding destination line."
    (interactive (list current-prefix-arg last-input-event nil))
    (dhnam/diff-goto-source (not other-file) event same-window))

  (defun dhnam/diff-goto-conditionally (&optional other-file event same-window)
    "Jump to the corresponding line of source or old source"
    (interactive (list current-prefix-arg last-input-event nil))
    (save-excursion
      (let* ((line-beg-point (line-beginning-position))
             (first-char (buffer-substring-no-properties line-beg-point (1+ line-beg-point))))
        (cond
         ((string= first-char "+")
          (dhnam/diff-goto-source current-prefix-arg event same-window))
         ((string= first-char "-")
          (dhnam/diff-goto-old-source current-prefix-arg event same-window))
         (t
          (dhnam/diff-goto-source current-prefix-arg event same-window))))))

  (defun dhnam/diff-goto-conditionally-in-same-window (&optional other-file event)
    (interactive (list current-prefix-arg last-input-event))
    (dhnam/diff-goto-conditionally other-file event t)))

(provide 'dhnam-miscellaneous)
