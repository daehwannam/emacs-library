
(defun dhnam/get-current-frame-number ()
  (let ((frames (frame-list))
        (curr-frame (selected-frame)))
    (length (member curr-frame frames))))

(provide 'dhnam-window-frame)
