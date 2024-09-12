
(with-eval-after-load 'pdf-view
  (progn
    (defun dhnam/pdf-view-previous-page-command-in-other-window (&optional num-steps)
      (interactive)
      (dhnam/pdf-view-next-page-command-in-other-window (- (or num-steps 1))))

    (defun dhnam/pdf-view-next-page-command-in-other-window (&optional num-steps)
      (interactive)
      (with-current-buffer (window-buffer (next-window (selected-window)))
        (pdf-view-next-page-command num-steps))))

  (progn
    (defun dhnam/pdf-view-previous-page-in-multiple-columns-command (num-steps)
      (interactive "p")
      (dhnam/pdf-view-next-page-in-multiple-columns-command (- num-steps)))

    (defun dhnam/pdf-view-next-page-in-multiple-columns-command (num-steps &optional non-overlapping)
      (interactive "p")

      (let ((found nil)
            (rightmost-window (selected-window)))
        (while (not found)
          (let ((new-window (windmove-find-other-window 'right 1 rightmost-window)))
            (if new-window
                (setq rightmost-window new-window)
              (setq found t))))

        (let ((window rightmost-window)
              (same-buffer-windows-from-left-to-right nil))
          (dotimes (i (length (window-list)))
            (when (eq (current-buffer) (window-buffer window))
              ;; right windows are pushed first
              (push window same-buffer-windows-from-left-to-right))
            (setq window (previous-window window)))

          (let* ((pivot-window (selected-window))
                 (current-page (pdf-view-current-page pivot-window)))
            (mapcar (lambda (window)
                      (let ((offset (- (length (member pivot-window same-buffer-windows-from-left-to-right))
                                       (length (member window same-buffer-windows-from-left-to-right))
                                       )))
                        (pdf-view-goto-page (+ offset current-page
                                               (if non-overlapping
                                                   (* num-steps (length same-buffer-windows-from-left-to-right))
                                                 num-steps))
                                            window)))
                    (if (> num-steps 0)
                        (reverse same-buffer-windows-from-left-to-right)
                      same-buffer-windows-from-left-to-right))))))

    (defun dhnam/pdf-view-previous-non-overlapping-page-in-multiple-columns-command (num-steps)
      (interactive "p")
      (dhnam/pdf-view-next-non-overlapping-page-in-multiple-columns-command (- num-steps)))

    (defun dhnam/pdf-view-next-non-overlapping-page-in-multiple-columns-command (num-steps)
      (interactive "p")
      (dhnam/pdf-view-next-page-in-multiple-columns-command num-steps t)))

  (progn
    (progn
      (defvar dhnam/pdf-view-page-ring nil)
      ;; (put 'dhnam/pdf-view-page-ring 'permanent-local t)
      (make-variable-buffer-local 'dhnam/pdf-view-page-ring)

      (defvar dhnam/pdf-view-page-ring-max-len 16)
      (defvar dhnam/pdf-view-last-push-pop-command nil)
      (make-variable-buffer-local 'dhnam/pdf-view-last-push-pop-command))

    (defun dhnam/pdf-view-push (&optional new-page)
      (let ((new-page (or new-page (pdf-view-current-page))))
        (push new-page dhnam/pdf-view-page-ring)
        (when (>= (length dhnam/pdf-view-page-ring) dhnam/pdf-view-page-ring-max-len)
          (setf (nthcdr (1- dhnam/pdf-view-page-ring-max-len) dhnam/pdf-view-page-ring) nil))
        (setq dhnam/pdf-view-last-push-pop-command 'dhnam/pdf-view-push)
        new-page))

    (defun dhnam/pdf-view-push-command ()
      (interactive)
      (let ((new-page (dhnam/pdf-view-push)))
        (message (format "Page %s is marked" new-page))))

    (defun dhnam/pdf-view-pop ()
      (let ((popped-page (pop dhnam/pdf-view-page-ring)))
        (setf (nthcdr (min (length dhnam/pdf-view-page-ring) dhnam/pdf-view-page-ring-max-len)
                      dhnam/pdf-view-page-ring)
              (list popped-page))
        popped-page))

    (defun dhnam/pdf-view-pop-command ()
      (interactive)
      (when dhnam/pdf-view-page-ring
        (when (eq dhnam/pdf-view-last-push-pop-command 'dhnam/pdf-view-unpop-command)
          (dhnam/pdf-view-pop))
        (let ((popped-page (dhnam/pdf-view-pop)))
          (setq dhnam/pdf-view-last-push-pop-command 'dhnam/pdf-view-pop-command)
          (pdf-view-goto-page popped-page)
          popped-page)))

    (defun dhnam/pdf-view-unpop ()
      (let ((old-page (car (last dhnam/pdf-view-page-ring))))
        (setf (nthcdr (1- (length dhnam/pdf-view-page-ring)) dhnam/pdf-view-page-ring) nil)
        (push old-page dhnam/pdf-view-page-ring)
        old-page))

    (defun dhnam/pdf-view-unpop-command ()
      (interactive)
      (when dhnam/pdf-view-page-ring
        (when (eq dhnam/pdf-view-last-push-pop-command 'dhnam/pdf-view-pop-command)
          (dhnam/pdf-view-unpop))
        (let ((unpopped-page (dhnam/pdf-view-unpop)))
          (setq dhnam/pdf-view-last-push-pop-command 'dhnam/pdf-view-unpop-command)
          (pdf-view-goto-page unpopped-page)
          unpopped-page)))

    (defun dhnam/pdf-view-first-page ()
      (interactive)
      (dhnam/pdf-view-push)
      (pdf-view-first-page))

    (defun dhnam/pdf-view-last-page ()
      (interactive)
      (dhnam/pdf-view-push)
      (pdf-view-last-page))

    (defun dhnam/pdf-tools-relocation-advice (orig-func &rest args)
      (dhnam/pdf-view-push-command)
      (let ((result (apply orig-func args)))
        (progn
          (dhnam/pdf-view-push-command)
          (dhnam/pdf-view-pop))
        result))))

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

(progn
  (defun dhnam/get-align-region-boundary (&optional forward)
    (cl-flet ((line-num (p) (count-lines 1 p)))
      (let ((re-search (if forward 're-search-forward 're-search-backward)))
        (let ((prev-point (point))
              (curr-point (point)))
          (save-excursion
            (while (and curr-point (< (abs (- (line-num curr-point) (line-num prev-point))) 2))
              (setq prev-point curr-point)
              (setq curr-point (funcall re-search "&" nil t))))
          prev-point))))

  (defun dhnam/align-ampersands ()
    (interactive)
    (let ((align-start nil)
          (align-end nil))
      (if (region-active-p)
          (progn
            (setq align-start (region-beginning))
            (setq align-end  (region-end)))
        (progn
          (setq align-start (save-excursion (goto-char (dhnam/get-align-region-boundary nil)) (beginning-of-line) (point)))
          (setq align-end (save-excursion (goto-char (dhnam/get-align-region-boundary t)) (end-of-line) (point)))))
      (save-excursion
        (dhnam/without-message
         ;; when query-replace-highlight = nil -> disable highlights while replacing
         ;;
         ;; when cursor-type = nil -> hide the cursor while replacing
         ;; https://emacs.stackexchange.com/questions/18374/persistently-hide-cursor-evil-mode-problem
         (let ((query-replace-highlight nil)
               (cursor-type nil))
           (replace-regexp " *& *" " & " nil align-start align-end)))

        (let ((new-align-end (point)))
          (let ((start-line-num (line-number-at-pos align-start))
                (end-line-num (line-number-at-pos new-align-end)))
            (align-regexp align-start new-align-end "\\(\\s-*\\)&" 1 1 t)
            (indent-region (progn (goto-line start-line-num) (line-beginning-position))
                           (progn (goto-line end-line-num) (line-end-position)))))))))



(progn
  (defhydra dhnam/TeX-error-navigation ()
    "TeX error navigation"

    ("i" TeX-previous-error)
    ("o" TeX-next-error)
    ;; ("RET" nil "quit")
    ;; ("q" nil "quit")
    ;; ("SPC" nil "quit")
    )

  ;; Disable any hint message
  (hydra-set-property 'dhnam/TeX-error-navigation :verbosity 0))

(provide 'dhnam-latex)
