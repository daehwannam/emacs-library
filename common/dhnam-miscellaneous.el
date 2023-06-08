
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

(provide 'dhnam-miscellaneous)
