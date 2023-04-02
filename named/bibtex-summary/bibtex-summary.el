
;; my bibliography utilities

(defvar bibs/bibliography-file-as-source-of-reference nil
  "The path to a bibliography file. It should be used as a local variable.")
(comment
  (make-local-variable 'bibs/bibliography-file-as-source-of-reference)
  (setq-default bibs/bibliography-file-as-source-of-reference "some-bibliography.bib"))

(defun bibs/get-start-end-content-positions-of-curly-brackets ()
  (let (start-pos end-pos)
    (save-excursion
      ;; find identifiers separated by curly bracket, comma, white-space
      (save-excursion
        (when (re-search-backward "\[{,[:blank:]\]" nil t)
          (setq start-pos (1+ (point)))))
      (save-excursion
        (when (re-search-forward "\[},[:blank:]\]" nil t)
          (setq end-pos (1- (point))))))
    (list start-pos end-pos)))

(defun bibs/kill-current-content-of-curly-brackets-to-clipboard ()
  (interactive)
  (let (start-pos end-pos)
    (let ((start-end-pos-pair (bibs/get-start-end-content-positions-of-curly-brackets)))
      (setq start-pos (car start-end-pos-pair)
            end-pos (cadr start-end-pos-pair))
      (when (and start-pos end-pos)
        (copy-region-as-kill start-pos end-pos)))))

(defun bibs/goto-ref-id (ref-id-str &optional opening-in-other-window)
  (let ((ref-pos nil))
    (funcall (if opening-in-other-window #'find-file-other-window #'find-file)
             bibs/bibliography-file-as-source-of-reference)
    (save-excursion
      (beginning-of-buffer)
      (re-search-forward (format "@.*\\(article\\|inproceedings\\).*%s[, \n]" ref-id-str))
      (setq ref-pos (point)))
    (when ref-pos
      (goto-char ref-pos)
      (recenter-top-bottom)

      ;; return position
      ref-pos)))

(defun bibs/get-ref-id-str-from-curly-brackets ()
  (let (start-pos end-pos)
    (let ((start-end-pos-pair (bibs/get-start-end-content-positions-of-curly-brackets)))
      (setq start-pos (car start-end-pos-pair)
            end-pos (cadr start-end-pos-pair)))
    (let ((ref-id-str nil)
          (ref-id-str-valid nil))
      (when (and start-pos end-pos)
        (setq ref-id-str (dhnam/string-trim (buffer-substring-no-properties start-pos end-pos)))
        (setq ref-id-str-valid (not (or (string-match-p "[{}]" ref-id-str)
                                        (string-empty-p ref-id-str))))
        (comment
          (unless ref-id-str-valid
            (comment (xref-find-definitions (xref-backend-identifier-at-point (xref-find-backend))))
            (call-interactively 'xref-find-definitions)))
        (when ref-id-str-valid
          ref-id-str)))))

(defvar bibs/org-bib-source-symbol nil
  "The symbol of bib source. It should be used as a local variable.")


(with-eval-after-load 'org
  ;; define a link type
  ;; https://orgmode.org/manual/Adding-Hyperlink-Types.html
  ;; https://kitchingroup.cheme.cmu.edu/blog/2016/11/04/New-link-features-in-org-9/
  ;; https://stackoverflow.com/a/57251655
  (org-link-set-parameters "bib-id"
                           :face '(:foreground "green")
                           :follow #'bibs/goto-ref-id)

  (org-link-set-parameters "code-url"
                           :face '(:foreground "yellow")
                           :follow #'org-link-open-from-string))

(defun bibs/get-ref-id-str-from-raw-link-at-end ()
  (save-excursion
    (move-end-of-line 1)
    (re-search-backward (or bibs/org-bib-source-symbol "[[.*]]") nil t)
    (when (eq major-mode 'org-mode)
      (substring-no-properties (plist-get (cadr (org-element-context)) :raw-link) (length "bib-id:")))))

(defun bibs/get-ref-id-str-from-curly-brackets-or-raw-link-at-end ()
  (or (bibs/get-ref-id-str-from-raw-link-at-end)
      (bibs/get-ref-id-str-from-curly-brackets)))

(defalias 'bibs/find-reference-in-bibliography-file
  'bibs/find-reference-in-bibliography-file-with-curly-brackets-or-raw-link-at-end)

(defun bibs/find-reference-in-bibliography-file-with-curly-brackets () ;; (&optional opening-in-other-window)
  (interactive)
  (let ((opening-in-other-window nil))
    (let ((ref-id-str (bibs/get-ref-id-str-from-curly-brackets)))
      (when ref-id-str
        (bibs/goto-ref-id ref-id-str opening-in-other-window)))))

(defun bibs/find-reference-in-bibliography-file-with-raw-link () ;; (&optional opening-in-other-window)
  (interactive)
  (let ((opening-in-other-window nil))
    (let ((ref-id-str (plist-get (cadr (org-element-context)) :raw-link)))
      (bibs/goto-ref-id ref-id-str opening-in-other-window))))

(defun bibs/find-reference-in-bibliography-file-with-raw-link-at-end ()
  (interactive)
  (let ((opening-in-other-window nil))
    (let ((ref-id-str (bibs/get-ref-id-str-from-raw-link-at-end)))
      (bibs/goto-ref-id ref-id-str opening-in-other-window))))

(defun bibs/find-reference-in-bibliography-file-with-curly-brackets-or-raw-link-at-end ()
  (interactive)
  (let ((opening-in-other-window nil))
    (let ((ref-id-str (bibs/get-ref-id-str-from-curly-brackets-or-raw-link-at-end)))
      (bibs/goto-ref-id ref-id-str opening-in-other-window))))

(defun bibs/open-pdfurl-of-reference-in-bibliography-file ()
  (interactive)
  (when (bibs/find-reference-in-bibliography-file)
    (save-excursion
      (let ((new-point
             (save-restriction
               (narrow-to-region (save-excursion (move-beginning-of-line 1) (point))
                                 (save-excursion (move-beginning-of-line 1) (forward-list) (point)))
               (re-search-forward "\\(pdfurl\\|@\\)")
               (re-search-forward "{")
               (re-search-forward "[^ ]")
               (point))))
        (goto-char new-point)
        (let ((url-thing (thing-at-point 'url)))
          (when url-thing
            (dhnam/exwm-command-open-web-browser (substring-no-properties url-thing))))))))


(defvar bibs/pdf-file-dir-as-source-of-reference nil
  "The path to a pdf collection directory. It should be used as a local variable.")
(comment
  (make-local-variable 'bibs/pdf-file-dir-as-source-of-reference)
  (setq-default bibs/pdf-file-dir-as-source-of-reference "some-pdf-directory-path"))

(defun bibs/goto-pdf-file-of-reference (ref-id-str &optional opening-in-other-window)
  (if ref-id-str
      (let* ((file-name (concat
                         (replace-regexp-in-string
                          "/" "+" (replace-regexp-in-string
                                   ":" "=" ref-id-str))
                         ".pdf"))
             (file-path (dhnam/join-dirs bibs/pdf-file-dir-as-source-of-reference file-name)))

        (if (file-exists-p file-path)
            (funcall (if opening-in-other-window #'find-file-other-window #'find-file)
                     file-path)
          (message (format "The file %s doesn't exist" file-path))))
    (message "Invalid cursor position")))

(defalias 'bibs/open-pdf-file-of-reference
  'bibs/open-pdf-file-of-reference-with-curly-brackets-or-raw-link-at-end)

(defun bibs/open-pdf-file-of-reference-with-curly-brackets ()
  (interactive)
  (let ((ref-id-str (bibs/get-ref-id-str-from-curly-brackets)))
    (bibs/goto-pdf-file-of-reference ref-id-str)))

(defun bibs/open-pdf-file-of-reference-with-raw-link-at-end ()
  (interactive)
  (let ((ref-id-str (bibs/get-ref-id-str-from-raw-link-at-end)))
    (bibs/goto-pdf-file-of-reference ref-id-str)))

(defun bibs/open-pdf-file-of-reference-with-curly-brackets-or-raw-link-at-end ()
  (interactive)
  (let ((ref-id-str (bibs/get-ref-id-str-from-curly-brackets-or-raw-link-at-end)))
    (bibs/goto-pdf-file-of-reference ref-id-str)))

(defun bibs/pdfgrep-with-default-dir (command-args)
  "This function is modified from `pdfgrep'. 
Run pdfgrep with user-specified COMMAND-ARGS, collect output in a buffer.
You can use \\[next-error], or RET in the `pdfgrep-buffer-name'
buffer, to go to the lines where PDFGrep found matches.  To kill
the PDFGrep job before it finishes, type \\[kill-compilation]."
  (interactive (list (read-shell-command "Run pdfgrep (like this): "
                                         (let ((default-command
                                                 (concat (pdfgrep-default-command) "'"))
                                               (appended-arg-str
                                                (concat "' " (dhnam/join-paths bibs/pdf-file-dir-as-source-of-reference "*"))))
					                       (cons (concat default-command appended-arg-str)
                                                 (1+ (length default-command))))
					                     'pdfgrep-history)))
  (unless pdfgrep-mode
    (error "PDFGrep is not enabled, run `pdfgrep-mode' first."))
  (unless (executable-find "pdfgrep")
    (error "The 'pdfgrep' command not available on your system."))
  (compilation-start command-args 'grep-mode
		             (lambda (_x) pdfgrep-buffer-name)))

(provide 'bibtex-summary)
