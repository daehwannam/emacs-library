
(require 'dhnam-directory-files-recursively nil t)

(defun dhnam/add-to-load-path (path)
  (add-to-list 'load-path path))

(defun dhnam/add-to-load-path-recursively (path)
  "Add to load-path recursively. If you want to skip a
subdirectory, the subdirectory should contains a file named
'.nosearch'. `normal-top-level-add-subdirs-to-load-path' ignores
directory with `.nosearch`."

  (add-to-list 'load-path path)

  (let ((default-directory path))
    ;; recursively add packages from a directory to load-path
    ;; https://stackoverflow.com/a/7322812
    (normal-top-level-add-subdirs-to-load-path)))

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

