
(defvar dhnam/package--initialized nil)

(defun dhnam/package-initialize (&optional package-archive-list)
  (when dhnam/package--initialized
    (error "dhnam/package-initialize is called already."))

  (progn
    ;; load emacs 24's package system. Add MELPA repository.
    ;; http://ergoemacs.org/emacs/emacs_package_system.html

    (when (>= emacs-major-version 24)
      (require 'package)

      ;; https://emacs.stackexchange.com/a/27599
      (let ((package-archive-list (or package-archive-list
                                      '(("melpa" . "https://melpa.org/packages/")
                                        ("nongnu" . "https://elpa.nongnu.org/nongnu/")))))
        (comment
          ;; other example repositories
          ("marmalade" . "https://marmalade-repo.org/packages/")
          ("org" . "https://orgmode.org/elpa/"))
	    
        (mapc #'(lambda (archive-pair) (add-to-list 'package-archives archive-pair))
	          package-archive-list)))

    (unless package--initialized
      ;; package initialization
      ;; unnecessary call ‘package-initialize’ twice in init file
      (package-initialize))

    ;; fetch the list of packages available
    ;; https://stackoverflow.com/a/10093312
    (comment
      (unless package-archive-contents
        (package-refresh-contents)))))

(progn
  (defvar dhnam/package-content-refreshed nil)
  (defun dhnam/install-package-unless-installed (package &optional refreshing)
    (unless (package-installed-p package)
      (when (or refreshing (not dhnam/package-content-refreshed))
	    ;; https://magit.vc/manual/magit/Installing-from-Melpa.html
	    (package-refresh-contents)
	    (setq dhnam/package-content-refreshed t))
      (package-install package))))

(with-eval-after-load 'use-package
  ;; use-package initialization

  (defmacro use-existing-pkg (name &rest args)
    `(when (package-installed-p ',name)
       (use-package ,name ,@args)))

  (progn
    ;; change argument colors of use-existing-pkg
    (defconst use-existing-pkg-font-lock-keywords
      ;; It's the same definition with `use-package-font-lock-keywords'
      '(("(\\(use-existing-pkg\\)\\_>[ \t']*\\(\\(?:\\sw\\|\\s_\\)+\\)?"
	     (1 font-lock-keyword-face)
	     (2 font-lock-constant-face nil t))))

    (font-lock-add-keywords 'emacs-lisp-mode use-existing-pkg-font-lock-keywords))

  (progn
    ;; change indentation
    (function-put 'use-package 'lisp-indent-function '1)
    (function-put 'use-existing-pkg 'lisp-indent-function '1)))


(provide 'dhnam-package)
