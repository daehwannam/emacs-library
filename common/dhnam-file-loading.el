
(require 'dhnam-directory-files-recursively nil t)

(defun dhnam/load-directory (dir)
  ;; https://www.emacswiki.org/emacs/LoadingLispFiles
  (let ((load-it (lambda (f)
		           (load-file (concat (file-name-as-directory dir) f)))))
    (mapc load-it (directory-files dir nil "\\.el$"))))

(defun dhnam/require-directory (dir)
  (let* ((post-fix-len (length ".el"))
         (load-it (lambda (file-name)
                    (require (intern (substring file-name 0 (- post-fix-len)))))))
    (mapcar load-it (directory-files dir nil "\\.el$"))))

(defun dhnam/require-directory-with-prefix (dir prefix)
  (let* ((post-fix-len (length ".el"))
         (dir-path (file-name-as-directory dir))
         (load-it (lambda (file-name)
                    (require (intern (concat prefix (substring file-name 0 (- post-fix-len))))
                             (concat dir-path file-name)))))
    (mapcar load-it (directory-files dir nil "\\.el$"))))

(defun dhnam/require-directory-with-prefix (dir prefix)
  (let* ((post-fix-len (length ".el"))
         (dir-path (file-name-as-directory dir))
         (load-it (lambda (file-name)
                    (require (intern (concat prefix (substring file-name 0 (- post-fix-len))))
                             (concat dir-path file-name)))))
    (mapcar load-it (directory-files dir nil "\\.el$"))))

(defmacro define-localized-require (fn-name feature-prefix directory-path)
  `(defun ,fn-name (file-name)
     (require (intern (concat ,feature-prefix file-name)) (concat ,directory-path file-name ".el"))))

(provide 'dhnam-file-loading)

