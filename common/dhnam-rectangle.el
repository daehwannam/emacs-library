
(defun dhnam/yank-rectangle-after-inserting-space ()
  "Yank the last killed rectangle with upper left corner at point,
After inserting new-lines as much as the rectangle.
This function is modified from `yank-rectangle'."
  (interactive "*")
  (save-excursion
    (dotimes (i (length killed-rectangle))
      (insert "\n")))
  (insert-rectangle killed-rectangle))


(provide 'dhnam-rectangle)
