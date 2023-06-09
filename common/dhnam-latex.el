
(with-eval-after-load 'pdf-view
  (progn
    (defun dhnam/pdf-view-previous-page-command-in-other-window (&optional n)
      (interactive)
      (dhnam/pdf-view-next-page-command-in-other-window (- (or n 1))))

    (defun dhnam/pdf-view-next-page-command-in-other-window (&optional n)
      (interactive)
      (with-current-buffer (window-buffer (next-window (selected-window)))
        (pdf-view-next-page-command n))))

  (progn
    (defun dhnam/pdf-view-previous-page-in-multiple-columns-command (n)
      (interactive "p")
      (dhnam/pdf-view-next-page-in-multiple-columns-command (- n)))

    (comment
      (defun dhnam/pdf-view-next-page-in-multiple-columns-command (n)
        (interactive "p")

        (let ((window (selected-window))
              (sorted-same-buffer-windows nil))
          (dotimes (n (length (window-list)))
            (when (eq (current-buffer) (window-buffer window))
              (push window sorted-same-buffer-windows))
            (setq window (next-window window)))

          (let ((current-page (pdf-view-current-page (selected-window))))
            (mapcar (lambda (window)
                      (let ((offset (- (length (member window sorted-same-buffer-windows))
                                       (length (member (selected-window) sorted-same-buffer-windows))
                                       )))
                        (pdf-view-goto-page (+ offset current-page n) window)))
                    sorted-same-buffer-windows)))))

    (defun dhnam/pdf-view-next-page-in-multiple-columns-command (n &optional non-overlapping)
      (interactive "p")

      (let ((found nil)
            (leftmost-window (selected-window)))
        (while (not found)
          (let ((new-window (windmove-find-other-window 'left 1 leftmost-window)))
            (if new-window
                (setq leftmost-window new-window)
              (setq found t))))

        (let ((window leftmost-window)
              (sorted-same-buffer-windows nil))
          (dotimes (i (length (window-list))) ; right (newly created) windows are first in window-list.
            (when (eq (current-buffer) (window-buffer window))
              (push window sorted-same-buffer-windows))
            (setq window (next-window window)))

          (let ((current-page (pdf-view-current-page (selected-window))))
            (mapcar (lambda (window)
                      (let ((offset (- (length (member window sorted-same-buffer-windows))
                                       (length (member (selected-window) sorted-same-buffer-windows))
                                       )))
                        (pdf-view-goto-page (+ offset current-page
                                               (if non-overlapping
                                                   (* n (length sorted-same-buffer-windows))
                                                 n))
                                            window)))
                    sorted-same-buffer-windows)))))

    (defun dhnam/pdf-view-previous-non-overlapping-page-in-multiple-columns-command (n)
      (interactive "p")
      (dhnam/pdf-view-next-non-overlapping-page-in-multiple-columns-command (- n)))

    (defun dhnam/pdf-view-next-non-overlapping-page-in-multiple-columns-command (n)
      (interactive "p")
      (dhnam/pdf-view-next-page-in-multiple-columns-command n t))))

(with-eval-after-load 'biblio
  (progn
    (defun dhnam/post-process-biblio-bibtex (bibtex)
      (let* ((normalized (replace-regexp-in-string
                          "@[a-zA-Z]*"
                          #'downcase ; (lambda (matched) (downcase matched))
                          bibtex))
             (entries (split-string normalized "\n\n")))
        (if (= (length entries) 2)
            (if (string-match "@inproceedings" bibtex)
                (car entries)
              normalized)
          normalized)))

    (comment
      (defun dhnam/biblio--selection-copy-callback (bibtex entry)
        "Add BIBTEX (from ENTRY) to kill ring."
        (kill-new (dhnam/post-process-biblio-bibtex bibtex))
        (message "Killed bibtex entry for %S."
                 (biblio--prepare-title (biblio-alist-get 'title entry))))

      (defun dhnam/biblio--selection-insert-callback (bibtex entry)
        "Add BIBTEX (from ENTRY) to kill ring."
        (let ((target-buffer biblio--target-buffer))
          (with-selected-window (or (biblio--target-window) (selected-window))
            (with-current-buffer target-buffer
              (insert (dhnam/post-process-biblio-bibtex bibtex)
                      "\n\n"))))
        (message "Inserted bibtex entry for %S."
                 (biblio--prepare-title (biblio-alist-get 'title entry)))))

    (comment
      (defun dhnam/biblio--selection-copy-callback-advice (orig-func bibtex &rest args)
        (funcall orig-func (cons (dhnam/post-process-biblio-bibtex bibtex) args)))

      (defun dhnam/biblio--selection-insert-callback-advice (orig-func bibtex &rest args)
        (funcall orig-func (cons (dhnam/post-process-biblio-bibtex bibtex) args))))

    (progn
      (defun dhnam/biblio-format-bibtex-advice (orig-func &rest args)
        (dhnam/post-process-biblio-bibtex (apply orig-func args)))
      (advice-add 'biblio-format-bibtex :around #'dhnam/biblio-format-bibtex-advice)
      (comment (advice-remove 'biblio-format-bibtex #'dhnam/biblio-format-bibtex-advice))))

  (progn
    (defun dhnam/biblio--copy-url-callback (bibtex entry)
      (let* ((metadata (biblio--selection-metadata-at-point)))
        (let-alist metadata
          (if .direct-url
              (let ((url-str (replace-regexp-in-string "v..?$" "" .direct-url)))
                (kill-new url-str)
                (message (concat "Copied: " url-str))
                )
            (user-error "This record does not contain a direct URL (try arXiv or HAL)")))))

    (defun dhnam/biblio--copy-url ()
      (interactive)
      (biblio--selection-forward-bibtex #'dhnam/biblio--copy-url-callback))

    (defun dhnam/biblio--copy-url-quit ()
      (interactive)
      (biblio--selection-forward-bibtex #'dhnam/biblio--copy-url-callback t))))

(provide 'dhnam-latex)
