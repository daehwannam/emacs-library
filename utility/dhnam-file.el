

(progn
  ;; read file contents
  ;; http://ergoemacs.org/emacs/elisp_read_file_content.html
  (defun dhnam/get-string-from-file (filePath)
    "Return filePath's file content."
    (with-temp-buffer
      (insert-file-contents filePath)
      (buffer-string))))

(progn
  ;; save and load a list
  ;; https://stackoverflow.com/a/44834833
  ;;
  ;; e.g.
  ;; (dhnam/print-to-file '(a f c (q e g)) "test.el")
  ;; (dhnam/read-from-file "test.el")

  (defun dhnam/print-to-file (data filename)
    (with-temp-file filename
      (prin1 data (current-buffer))))

  (defun dhnam/read-from-file (filename)
    (with-temp-buffer
      (insert-file-contents filename)
      (cl-assert (eq (point) (point-min)))
      (read (current-buffer)))))

(defun dhnam/join-dirs (root &rest dirs)
  "Joins a series of directories together, like Python's os.path.join,
  (dhnam/join-dirs \"/tmp\" \"a\" \"b\" \"c\") => /tmp/a/b/c"

  ;; https://stackoverflow.com/a/13473856

  (if (not dirs)
      root
    (apply 'dhnam/join-dirs
           (expand-file-name (car dirs) root)
           (cdr dirs))))

(defun dhnam/join-paths (&rest paths)
  "Joins a series of paths together, like Python's os.path.join,
  (joinpaths \"a/\" \"b/\" \"c\") => a/b/c"

  ;; https://stackoverflow.com/a/13473856
  (assert paths)
  (s-join "/" (mapcar (lambda (path) (if (string= (substring path -1) "/")
                                         (substring path 0 -1)
                                       path))
                      paths)))

(defun dhnam/buffer-local-set-key (key command)
  "Set a key binding for a specific buffer only"
  ;; https://stackoverflow.com/a/21493693
  (interactive "KSet key buffer-locally: \nCSet key %s buffer-locally to command: ")
  (let ((oldmap (current-local-map))
        (newmap (make-sparse-keymap)))
    (when oldmap
      (set-keymap-parent newmap oldmap))
    (define-key newmap key command)
    (use-local-map newmap)))

(defun dhnam/buffer-local-set-key-chord (key-chord command)
  "Set a key-chord binding for a specific buffer only"
  ;; https://stackoverflow.com/a/21493693
  (interactive "KSet key buffer-locally: \nCSet key %s buffer-locally to command: ")
  (let ((oldmap (current-local-map))
        (newmap (make-sparse-keymap)))
    (when oldmap
      (set-keymap-parent newmap oldmap))
    (key-chord-define newmap key-chord command)
    (use-local-map newmap)))

(provide 'dhnam-file)
