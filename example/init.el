(progn
  ;; path of emacs config
  (defconst dhnam/emacs-root-dir (file-name-directory (file-name-directory load-file-name))))

(progn
  ;; initialize dhnamlib
  (require 'dhnam-lib-init (concat dhnam/emacs-root-dir "dhnamlib/dhnam-lib-init.el")))

(progn
  (package-initialize)
  (require 'dhnam-package)

  (dhnam/package-initialize
   '(("melpa" . "https://melpa.org/packages/")
     ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

  (let ((package-names '(magit vlf wgrep use-package eglot counsel)))
    (mapc #'dhnam/install-package-unless-installed
          package-names)))
