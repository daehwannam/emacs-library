(comment
  (defhydra dhnam/volume-control ()
    "volume"
    ("q" nil "quit")
    ("+" volume-raise-10)
    ("=" volume-raise-10)
    ("-" volume-lower-10)
    ("s" volume-set)
    ("0" volume-set-to-0%))
  )

(defun dhnam/volume-raise-3 (&optional n)
  "Raise the volume by 3 N percentage units."
  (interactive "p")
  (volume-raise (* (or n 1) 3)))

(defun dhnam/volume-lower-3 (&optional n)
  "Lower the volume by 3 N percentage units."
  (interactive "p")
  (volume-lower (* (or n 1) 3)))


(provide 'dhnam-volume)
