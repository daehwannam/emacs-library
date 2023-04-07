;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

((nil . ((eval . (let ((current-dir (locate-dominating-file default-directory ".git")))

                   (require 'bibtex-summary)

                   (make-local-variable 'bibs/bibliography-file-as-source-of-reference)
                   (setq bibs/bibliography-file-as-source-of-reference (concat current-dir "paper-bibliography.bib"))
                   (dhnam/buffer-local-set-key (kbd "C-c M-.") 'bibs/find-reference-in-bibliography-file)
                   (dhnam/buffer-local-set-key-chord "q." 'bibs/find-reference-in-bibliography-file)
                   (dhnam/buffer-local-set-key-chord "ql" 'bibs/open-pdfurl-of-reference-in-bibliography-file)

                   (make-local-variable 'bibs/org-bib-source-symbol)
                   (setq bibs/org-bib-source-symbol "♣")

                   (make-local-variable 'bibs/pdf-file-dir-as-source-of-reference)
                   (setq bibs/pdf-file-dir-as-source-of-reference (concat current-dir "pdf"))
                   (dhnam/buffer-local-set-key-chord "qp" 'bibs/open-pdf-file-of-reference)
                   (dhnam/buffer-local-set-key-chord "pg" 'bibs/pdfgrep-with-default-dir)

                   (dhnam/buffer-local-set-key-chord "wk" 'bibs/kill-current-content-of-curly-brackets-to-clipboard)
                   )))))
