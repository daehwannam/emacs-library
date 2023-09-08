;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

((nil . ((eval . (let* ((current-dir (locate-dominating-file default-directory ".dir-locals.el"))
                        ;; (current-dir (locate-dominating-file default-directory ".git"))
                        (base-dir (concat current-dir "./")))

                   (require 'bibtex-summary)

                   (progn
                     ;; bib
                     (progn
                       (make-local-variable 'bibs/bibliography-file-as-source-of-reference)
                       (setq bibs/bibliography-file-as-source-of-reference (concat base-dir "paper-bibliography.bib")))

                     (dhnam/buffer-local-set-key (kbd "C-c M-.") 'bibs/find-reference-in-bibliography-file)
                     (dhnam/buffer-local-set-key-chord "q." 'bibs/find-reference-in-bibliography-file)
                     (dhnam/buffer-local-set-key-chord "ql" 'bibs/open-pdfurl-of-reference-in-bibliography-file)
                     (dhnam/buffer-local-set-key-chord "wk" 'bibs/kill-current-content-of-curly-brackets-to-clipboard)

                     (progn
                       (make-local-variable 'bibs/org-bib-source-symbol)
                       (setq bibs/org-bib-source-symbol "♣")))

                   (progn
                     ;; pdf
                     (progn
                       (make-local-variable 'bibs/pdf-file-dir-as-source-of-reference)
                       (setq bibs/pdf-file-dir-as-source-of-reference (concat base-dir "pdf")))

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
                       (setq bibs/collection-file-as-source-of-reference (concat base-dir "paper-collection.org")))

                     (dhnam/buffer-local-set-key-chord "q," 'bibs/find-reference-in-collectionb-file))

                   )))))
