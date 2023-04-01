
(defconst dhnam/lib-root-path (file-name-directory (file-name-directory load-file-name)))

(progn
  ;; use functions to load files and require features
  (require 'dhnam-file-loading (concat dhnam/lib-root-path "base/dhnam-file-loading.el")))

(progn
  ;; update load-path
  (dhnam/add-to-load-path-recursively dhnam/lib-root-path))

(progn
  ;; common utility
  (dhnam/require-directory (concat dhnam/lib-root-path "utility")))

(provide 'dhnam-lib-init)
