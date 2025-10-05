
;; bibtex summary (bibs)

(progn
  (defconst dhnam/bibs-root-dir (file-name-directory load-file-name))

  (defun bibs/make-dir-locals-example (dirpath)
    (interactive (list (read-directory-name "Directory: ")))
    (let ((new-dir-locals-path (concat dirpath ".dir-locals.el")))
      (if (file-exists-p new-dir-locals-path)
          (message (format "The file %s already exists" new-dir-locals-path))
        (let ((example-dir-locals-path (concat dhnam/bibs-root-dir "example/.dir-locals.el")))
          (copy-file example-dir-locals-path new-dir-locals-path)))))

  (defun bibs/insert-dir-locals-example ()
    (interactive)
    (let ((example-dir-locals-path (concat dhnam/bibs-root-dir "example/.dir-locals.el")))
      (insert (dhnam/get-string-from-file example-dir-locals-path)))))

(progn
  (defvar bibs/merged-bibliography-file-as-source-of-reference nil
    "The path to a bibliography file. It should be used as a local variable.")

  (defun bibs/get-start-end-content-positions-of-curly-brackets ()
    (let (start-pos end-pos)
      (save-excursion
        ;; find identifiers separated by curly bracket, comma, white-space
        (save-excursion
          (when (re-search-backward "\[{,\][:blank:]*" nil t)
            (setq start-pos (1+ (point)))))
        (save-excursion
          (when (re-search-forward "[:blank:]*\[},\]" nil t)
            (setq end-pos (1- (point))))))
      (list start-pos end-pos)))

  (defun bibs/get-ref-id-in-curly-brackets ()
    (let (start-pos end-pos)
      (let ((start-end-pos-pair (bibs/get-start-end-content-positions-of-curly-brackets)))
        (setq start-pos (car start-end-pos-pair)
              end-pos (cadr start-end-pos-pair))
        (when (and start-pos end-pos)
          (let ((content (dhnam/string-trim (buffer-substring-no-properties start-pos end-pos))))
            (when (and
                   ;; the content should not be empty.
                   (not (string-empty-p content))
                   ;; the content should not include "{" or "}".
                   (not (string-match "[{}]" content))
                   ;; the content should not include space
                   (not (string-match " " content)))
              content))))))

  (defun bibs/copy-ref-id-str-in-curly-brackets ()
    (interactive)
    (let ((content (or (bibs/get-ref-id-in-curly-brackets)
                       (bibs/get-ref-id-from-raw-link-at-end))))
      (if content
          (progn
            (kill-new content)
            (message content))
        (message "Invalid cursor position"))))

  (progn
    (defvar bibs/bib-file-dir-as-source-of-reference nil
      "The path to a bibliography collection directory. It should be used as a local variable.")

    (defun bibs/get-bib-file-path (ref-id-str)
      (let ((file-name (concat
                        (bibs/ref-id-str-to-file-name ref-id-str)
                        ".bib")))
        (dhnam/join-dirs bibs/bib-file-dir-as-source-of-reference file-name)))

    (defun bibs/goto-ref-id (ref-id-str &optional opening-in-other-window)
      (if ref-id-str
          (let ((bib-file-path (bibs/get-bib-file-path ref-id-str)))
            (funcall (if opening-in-other-window #'find-file-other-window #'find-file) bib-file-path))
        (message "Invalid cursor position"))))

  (progn
    (defconst bibs/ref-id-in-bib-regex-format "@[a-zA-z]+[ \n]*{[ \n]*%s[ \n]*,")

    (defun bibs/find-ref-id-point-from-merged-file (ref-id-str)
      (with-current-buffer (find-file-noselect bibs/merged-bibliography-file-as-source-of-reference)
        (save-excursion
          (beginning-of-buffer)
          (comment (re-search-forward (format "@.*\\(article\\|inproceedings\\).*%s[, \n]" ref-id-str)))
          (re-search-forward (format bibs/ref-id-in-bib-regex-format ref-id-str))
          (backward-char 2)
          (point))))

    (defun bibs/goto-ref-id-from-merged-file (ref-id-str &optional opening-in-other-window)
      (let ((ref-pos (bibs/find-ref-id-point-from-merged-file ref-id-str)))
        (funcall (if opening-in-other-window #'find-file-other-window #'find-file)
                 bibs/merged-bibliography-file-as-source-of-reference)
        (when ref-pos
          (goto-char ref-pos)
          (recenter-top-bottom)

          ;; return position
          ref-pos)))))

(progn
  (defvar bibs/org-bib-source-symbol nil
    "The symbol of bib source. It should be used as a local variable.")

  (with-eval-after-load 'org
    ;; define a link type
    ;; https://orgmode.org/manual/Adding-Hyperlink-Types.html
    ;; https://kitchingroup.cheme.cmu.edu/blog/2016/11/04/New-link-features-in-org-9/
    ;; https://stackoverflow.com/a/57251655
    (org-link-set-parameters "bibs-bib-id"
                             :face '(:foreground "green")
                             :follow #'bibs/goto-ref-id)

    (org-link-set-parameters "bibs-code-url"
                             :face '(:foreground "yellow")
                             :follow #'org-link-open-from-string)

    (org-link-set-parameters "bibs-note-file"
                             :face '(:foreground "cyan")
                             :follow #'find-file))

  (defun bibs/get-ref-id-from-raw-link-at-end ()
    (save-excursion
      (when (string= (uniquify-buffer-base-name) bibs/bibliography-org-name)
        (move-end-of-line 1)
        (re-search-backward (or bibs/org-bib-source-symbol "[[.*]]") nil t)
        ;; (eq major-mode 'org-mode)
        (substring-no-properties (plist-get (cadr (org-element-context)) :raw-link) (length "bibs-bib-id:")))))

  (defun bibs/child-parent-path-relation-p (path parent-path)
    (dhnam/real-path= (file-name-directory path) parent-path))

  (defun bibs/get-ref-id-from-file-name ()
    (let ((file-path (buffer-file-name)))
      (when file-path
        (let* ((file-name (file-name-nondirectory file-path))
               (extension (file-name-extension file-name)))
          (when (and (member extension '("org" "pdf" "bib"))
                     ;; (not (member file-name (list bibs/bibliography-bib-name bibs/bibliography-org-name)))
                     (or
                      (bibs/child-parent-path-relation-p file-path bibs/note-file-dir-as-source-of-reference)
                      (bibs/child-parent-path-relation-p file-path bibs/bib-file-dir-as-source-of-reference)
                      (bibs/child-parent-path-relation-p file-path bibs/pdf-file-dir-as-source-of-reference)
                      (bibs/child-parent-path-relation-p file-path bibs/pdf-extra-file-dir-as-source-of-reference)))
            (let ((file-name-without-extension (file-name-sans-extension file-name)))
              (bibs/file-name-to-ref-id-str file-name-without-extension)))))))

  (defun bibs/get-ref-id-from-org-cite ()
    (when (eq major-mode 'org-mode)
      (let ((cite-context (org-element-context)))
        (when cite-context 
          (cond
           ((eq (car cite-context) 'citation)
            (buffer-substring-no-properties
             (1+ (plist-get (cadr cite-context) :contents-begin))
             (plist-get (cadr cite-context) :contents-end)))
           ((eq (car cite-context) 'citation-reference)
            (plist-get (cadr cite-context) :key)))))))

  (defun bibs/get-ref-id-flexibly ()
    (let ((file-path (buffer-file-name)))
      (when file-path
        (let* ((file-name (file-name-nondirectory file-path))
               (extension (file-name-extension file-name)))
          (cond
           ((string= extension "pdf")
            (bibs/get-ref-id-from-file-name))
           (t
            (or (bibs/get-ref-id-in-curly-brackets)
                (bibs/get-ref-id-from-raw-link-at-end)
                (bibs/get-ref-id-from-org-cite)
                (bibs/get-ref-id-from-file-name)))))))))

(progn
  (defun bibs/find-reference-in-bibliography-file ()
    (interactive)
    (let ((opening-in-other-window nil))
      (let ((ref-id-str (bibs/get-ref-id-flexibly)))
        (bibs/goto-ref-id ref-id-str opening-in-other-window))))

  (defun bibs/open-pdfurl-of-reference-in-bibliography-file ()
    (interactive)
    (let ((ref-id-str (bibs/get-ref-id-flexibly)))
      (with-current-buffer (find-file-noselect (bibs/get-bib-file-path ref-id-str))
        (progn
          (beginning-of-buffer)
          (re-search-forward "pdfurl")
          (re-search-forward "{")
          (re-search-forward "[^ ]"))
        (let ((url-thing (thing-at-point 'url)))
          (when url-thing
            (message url-thing)
            (dhnam/app-command-open-web-browser (substring-no-properties url-thing))))))))

(progn
  (defvar bibs/pdf-file-dir-as-source-of-reference nil
    "The path to a pdf collection directory. It should be used as a local variable.")

  (defvar bibs/pdf-extra-file-dir-as-source-of-reference nil
    "The path to a extra pdf collection directory. It should be used as a local variable.")

  (defun bibs/ref-id-str-to-file-name (ref-id-str)
    (replace-regexp-in-string
     "/" "+" (replace-regexp-in-string
              ":" "=" ref-id-str)))

  (defun bibs/file-name-to-ref-id-str (file-name)
    (replace-regexp-in-string
     "+" "/" (replace-regexp-in-string
              "=" ":" file-name)))

  (defun bibs/goto-pdf-file-of-reference (ref-id-str &optional opening-in-other-window)
    (if ref-id-str
        (let* ((file-name (concat
                           (bibs/ref-id-str-to-file-name ref-id-str)
                           ".pdf"))
               (file-path (dhnam/join-dirs bibs/pdf-file-dir-as-source-of-reference file-name)))

          (if (file-exists-p file-path)
              (funcall (if opening-in-other-window #'find-file-other-window #'find-file)
                       file-path)
            (progn
             (comment (message (format "The file %s doesn't exist" file-path)))
             (let ((extra-file-path (dhnam/join-dirs bibs/pdf-extra-file-dir-as-source-of-reference file-name)))
               (if (file-exists-p extra-file-path)
                   (funcall (if opening-in-other-window #'find-file-other-window #'find-file)
                            extra-file-path)
                 (message (format "The file %s or %s doesn't exist" file-path extra-file-path)))))))
      (message "Invalid cursor position")))

  (defun bibs/open-pdf-file-of-reference ()
    (interactive)
    (let ((ref-id-str (bibs/get-ref-id-flexibly)))
      (bibs/goto-pdf-file-of-reference ref-id-str)))

  (defun bibs/pdfgrep-with-default-dir (command-args)
    "This function is modified from `pdfgrep'. 
Run pdfgrep with user-specified COMMAND-ARGS, collect output in a buffer.
You can use \\[next-error], or RET in the `pdfgrep-buffer-name'
buffer, to go to the lines where PDFGrep found matches.  To kill
the PDFGrep job before it finishes, type \\[kill-compilation]."
    (interactive (list (read-shell-command "Run pdfgrep (like this): "
                                           (let ((default-command
                                                   (concat (pdfgrep-default-command) "'' "))
                                                 (appended-arg-str
                                                  (concat
                                                   (dhnam/join-paths bibs/pdf-file-dir-as-source-of-reference "*")
                                                   " "
                                                   (dhnam/join-paths bibs/pdf-extra-file-dir-as-source-of-reference "*"))))
					                         (cons (concat default-command appended-arg-str)
                                                   (- (length default-command) 1)))
					                       'pdfgrep-history)))
    (unless pdfgrep-mode
      (error "PDFGrep is not enabled, run `pdfgrep-mode' first."))
    (unless (executable-find "pdfgrep")
      (error "The 'pdfgrep' command not available on your system."))
    (compilation-start command-args 'grep-mode
		               (lambda (_x) pdfgrep-buffer-name))))

(progn
  (defvar bibs/note-file-dir-as-source-of-reference nil
    "The path to a note collection directory. It should be used as a local variable.")

  (defun bibs/open-note-file-of-reference ()
    (interactive)
    (let ((ref-id-str (bibs/get-ref-id-flexibly)))
      (bibs/goto-note-file-of-reference ref-id-str)))

  (defun bibs/goto-note-file-of-reference (ref-id-str &optional opening-in-other-window)
    (if ref-id-str
        (let* ((file-name (concat
                           (bibs/ref-id-str-to-file-name ref-id-str)
                           ".org"))
               (file-path (dhnam/join-dirs bibs/note-file-dir-as-source-of-reference file-name)))

          (with-current-buffer (funcall (if opening-in-other-window #'find-file-other-window #'find-file) file-path)
            (when (= (buffer-size) 0)
              (insert (format "\nTitle: %s\n\n" (bibs/get-title-from-ref-id-str ref-id-str))))))
      (message "Invalid cursor position")))

  (defun bibs/get-title-from-ref-id-str (ref-id-str)
    (with-current-buffer (find-file-noselect (bibs/get-bib-file-path ref-id-str))
      (save-excursion
        (beginning-of-buffer)
        (let (title-begin title-end)
          (re-search-forward "title[[:space:]]*=[[:space:]]*[{\"]")
          (backward-char)
          (setq title-begin (1+ (point)))
          (forward-sexp)
          (setq title-end (1- (point)))
          (let ((title (buffer-substring-no-properties title-begin title-end)))
            (setq title (replace-regexp-in-string "[{}]" "" title))
            (setq title (replace-regexp-in-string "[[:space:]\n]+" " " title))
            title)))))

  (defun bibs/get-title-from-ref-id-str-from-merged-file (ref-id-str)
    (with-current-buffer (find-file-noselect bibs/merged-bibliography-file-as-source-of-reference)
      (let ((ref-id-point (bibs/find-ref-id-point-from-merged-file ref-id-str)))
        (goto-char (bibs/find-ref-id-point-from-merged-file ref-id-str))
        (let (title-begin title-end)
          (re-search-forward "title[[:space:]]*=[[:space:]]*[{\"]")
          (backward-char)
          (setq title-begin (1+ (point)))
          (forward-sexp)
          (setq title-end (1- (point)))
          (let ((title (buffer-substring-no-properties title-begin title-end)))
            (setq title (replace-regexp-in-string "[{}]" "" title))
            (setq title (replace-regexp-in-string "[[:space:]\n]+" " " title))
            title))))))

(progn
  (defvar bibs/collection-file-as-source-of-reference nil
    "The path to a bibliography file. It should be used as a local variable.")

  (defun bibs/find-reference-in-collection-file ()
    (interactive)
    (let ((opening-in-other-window nil))
      (let ((ref-id-str (bibs/get-ref-id-flexibly)))
        (bibs/goto-ref-id-in-collection ref-id-str opening-in-other-window))))

  (defun bibs/goto-ref-id-in-collection (ref-id-str &optional opening-in-other-window)
    (funcall (if opening-in-other-window #'find-file-other-window #'find-file)
             bibs/collection-file-as-source-of-reference)
    (when ref-id-str
      (let ((ref-pos nil))
        (save-excursion
          (beginning-of-buffer)
          (when (re-search-forward (format "bibs-bib-id:%s" ref-id-str) nil t)
            (move-beginning-of-line 1)
            (setq ref-pos (point))))
        (when ref-pos
          (goto-char ref-pos)
          (recenter-top-bottom)

          ;; return position
          ref-pos)))))

(defun bibs/copy-heading-path-at-point ()
  (interactive)

  ;; Getting org-mode headings
  ;; https://emacs.stackexchange.com/a/41872
  (let ((heading-path (string-join (org-get-outline-path t) "->")))
    (kill-new heading-path)
    (message heading-path)))

(progn
  (defvar bibs/bib-content-history nil)
  (defconst bibs/any-ref-id-in-bib-regex (format bibs/ref-id-in-bib-regex-format "\\(.+\\)"))

  (defun bibs/create-new-bib-file (bib-content)
    (interactive (list (read-string "BibTeX: " nil 'bibs/bib-content-history)))

    (let ((ref-id-str (dhnam/string-trim (dhnam/get-matched-substring-no-properties
                                          bibs/any-ref-id-in-bib-regex bib-content 1))))
      (let ((bib-file-path (bibs/get-bib-file-path ref-id-str)))
        (if (file-exists-p bib-file-path)
            (progn
              (message "The BibTeX file already exists")
              (find-file bib-file-path))
          (with-current-buffer (find-file bib-file-path)
            (insert bib-content)))))))

(progn
  (defvar bibs/update-script-path nil
    "The symbol of bib source. It should be used as a local variable.")

  (defun bibs/run-update-script ()
    (interactive)

    (let ((cmd (format "cd %s && %s" (file-name-directory bibs/update-script-path) bibs/update-script-path))
          (buffer-name "*bibtex-summary update*"))
      (dhnam/comint-with-command-in-same-window cmd buffer-name))))

(progn
  (require 'dhnam-web-browser)

  (defun bibs/search-in-google-scholar ()
    (interactive)
    (let* ((ref-id-str (bibs/get-ref-id-flexibly))
           (title (bibs/get-title-from-ref-id-str ref-id-str)))
      (dhnam/app-command-query-to-browser (concat "scholar " title)))))


(progn
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "s") 'bibs/search-in-google-scholar)
    (define-key map (kbd "p") 'bibs/open-pdfurl-of-reference-in-bibliography-file)

    (defvar bibs/web-command-map map)
    (fset 'bibs/web-command-map bibs/web-command-map)))

(defvar bibs/bibliography-bib-name "bibliography.bib")
(defvar bibs/bibliography-org-name "bibliography.org")

(defun bibs/setup-dir-locals (base-dir)
  (progn
    ;; bib
    (progn
      (make-local-variable 'bibs/bib-file-dir-as-source-of-reference)
      (setq bibs/bib-file-dir-as-source-of-reference (concat base-dir "bib")))

    (progn
      (make-local-variable 'bibs/merged-bibliography-file-as-source-of-reference)
      (setq bibs/merged-bibliography-file-as-source-of-reference (concat base-dir bibs/bibliography-bib-name)))

    (dhnam/buffer-local-set-key (kbd "C-c M-.") 'bibs/find-reference-in-bibliography-file)
    (dhnam/buffer-local-set-key-chord "q." 'bibs/find-reference-in-bibliography-file)
    (dhnam/buffer-local-set-key-chord "ql" 'bibs/web-command-map)
    (dhnam/buffer-local-set-key-chord "wk" 'bibs/copy-ref-id-str-in-curly-brackets)
    (dhnam/buffer-local-set-key-chord "wj" 'bibs/copy-heading-path-at-point)
    (dhnam/buffer-local-set-key-chord "c;" 'bibs/create-new-bib-file)

    (progn
      (make-local-variable 'bibs/org-bib-source-symbol)
      (setq bibs/org-bib-source-symbol "♣")))

  (progn
    ;; pdf
    (progn
      (make-local-variable 'bibs/pdf-file-dir-as-source-of-reference)
      (setq bibs/pdf-file-dir-as-source-of-reference (concat base-dir "pdf"))

      (make-local-variable 'bibs/pdf-extra-file-dir-as-source-of-reference)
      (setq bibs/pdf-extra-file-dir-as-source-of-reference (concat base-dir "pdf-extra")))

    (dhnam/buffer-local-set-key-chord "qp" 'bibs/open-pdf-file-of-reference)
    (dhnam/buffer-local-set-key-chord "pg" 'bibs/pdfgrep-with-default-dir))

  (progn
    ;; note
    (progn
      (make-local-variable 'bibs/note-file-dir-as-source-of-reference)
      (setq bibs/note-file-dir-as-source-of-reference (concat base-dir "note")))

    (dhnam/buffer-local-set-key-chord "qn" 'bibs/open-note-file-of-reference))

  (progn
    ;; collection
    (progn
      (make-local-variable 'bibs/collection-file-as-source-of-reference)
      (setq bibs/collection-file-as-source-of-reference (concat base-dir bibs/bibliography-org-name)))

    (dhnam/buffer-local-set-key-chord "q," 'bibs/find-reference-in-collection-file))

  (progn
    ;; update script
    (progn
      (make-local-variable 'bibs/update-script-path)
      (setq bibs/update-script-path (concat base-dir "update.sh")))

    (dhnam/buffer-local-set-key (kbd "C-c u") 'bibs/run-update-script)))


(provide 'bibtex-summary)
