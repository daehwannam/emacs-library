
;; Presentation Overlay Stepper (POS)

(defvar-local dhnam/pos-list nil)

(defun dhnam/pos-clear-overlays ()
  (interactive)
  (mapc #'delete-overlay dhnam/pos-list)
  (setq dhnam/pos-list nil))

(defun dhnam/pos-hide-all ()
  "Hide all text."
  (interactive)
  (dhnam/pos-clear-overlays)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (let ((overlay (make-overlay (point) (1+ (point)))))
        (overlay-put overlay 'invisible t)
        (push overlay dhnam/pos-list))
      (goto-char (1+ (point))))
    (setq dhnam/pos-list (reverse dhnam/pos-list))))

(comment
  (defun dhnam/pos-reveal-all ()
    "Reveal all text."
    (interactive)
    (save-excursion
      (goto-char (point-min))
      (while dhnam/pos-list
        (let ((overlay (pop dhnam/pos-list)))
          (overlay-put overlay 'invisible nil))))))

(defvar dhnam/pos-delay 0.02)

(defun dhnam/pos-reveal-non-empty-line ()
  "Reveal a non-empty line."

  (interactive)
  (save-excursion
    (let ((new-line-p nil)
          (overlay nil)
          (any-non-space-p nil))
      (while (and dhnam/pos-list (or (not any-non-space-p) (not new-line-p)))
        (setq overlay (pop dhnam/pos-list))
        (setq char-str (buffer-substring-no-properties
                        (overlay-start overlay)
                        (overlay-end overlay)))
        (setq new-line-p (string= char-str "\n"))
        (when (not (string-empty-p (string-trim char-str)))
          (setq any-non-space-p t))
        (overlay-put overlay 'invisible nil)
        (sit-for dhnam/pos-delay)))))

(provide 'dhnam-presentation-overlay-stepper)
