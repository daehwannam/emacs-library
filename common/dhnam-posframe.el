
(defun dhnam/vertico-posframe-get-size (buffer)
  "A modified functon of `vertico-posframe-get-size'."
  (let ((default-size (vertico-posframe-get-size buffer)))
    (append
     default-size
     (list
      :max-width (plist-get default-size :min-width)))))

(comment (setq vertico-posframe-size-function 'dhnam/vertico-posframe-get-size))

(defun dhnam/ivy-posframe-get-size ()
  "A modified functon of `ivy-posframe-get-size'."
  (let ((default-size (ivy-posframe-get-size)))
    (append
     default-size
     (list
      :max-width (plist-get default-size :min-width)))))

(comment (setq ivy-posframe-size-function 'dhnam/ivy-posframe-get-size))


(provide 'dhnam-posframe)
