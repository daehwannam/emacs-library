
(defconst dhnam/lib-root-dir (file-name-directory (file-name-directory load-file-name)))

(progn
  ;; use functions to load files and require features
  (require 'dhnam-file-loading (concat dhnam/lib-root-dir "base/dhnam-file-loading.el")))

(progn
  ;; update load-path
  (dhnam/add-to-load-path-recursively dhnam/lib-root-dir))

(progn
  ;; common utility
  (dhnam/require-directory (concat dhnam/lib-root-dir "utility")))

(provide 'dhnam-lib-init)
