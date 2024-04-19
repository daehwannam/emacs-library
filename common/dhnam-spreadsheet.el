
(defvar dhnam/csv-toggle-key)

(defun dhnam/toggle-csv (&optional mode)
  (interactive)

  (unless mode
    (setq mode 'csv-mode))

  (let ((toggle-fn nil))
    (cond
     ((eq mode 'csv-mode)
      (setq toggle-fn 'dhnam/toggle-csv))
     ((eq mode 'tsv-mode)
      (setq toggle-fn 'dhnam/toggle-tsv))
     (t
      (error "Unknown mode")))

    (if (eq major-mode mode)
        (progn
          (csv-align-mode 0)
          (text-mode)
          (local-set-key dhnam/csv-toggle-key toggle-fn))
      (progn
        (funcall mode)))))

(defun dhnam/toggle-tsv ()
  (interactive)
  (dhnam/toggle-csv 'tsv-mode))


(provide 'dhnam-spreadsheet)
