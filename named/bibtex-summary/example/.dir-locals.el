;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

((nil . ((eval . (let* ((current-dir (locate-dominating-file default-directory ".dir-locals.el"))
                        ;; (current-dir (locate-dominating-file default-directory ".git"))
                        ;; (base-dir (concat current-dir "./path/to/paper-collection/"))
                        (base-dir (concat current-dir "./")))

                   (require 'bibtex-summary)
                   (bibs/setup-dir-locals base-dir))))))
