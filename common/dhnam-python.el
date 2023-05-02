
(defun dhnam/py-repl-kill-ring-save-without-empty-lines (beg end &optional region)
  (interactive (list (mark) (point)
		             (prefix-numeric-value current-prefix-arg)))
  ;; https://stackoverflow.com/questions/605846/how-do-i-access-the-contents-of-the-current-region-in-emacs-lisp
  ;; https://stackoverflow.com/questions/6236196/elisp-split-string-function-to-split-a-string-by-character
  (let (lines char-lists indent-sizes min-indent result)
    (setq lines (nbutlast (split-string (buffer-substring-no-properties beg end) "$") 1)) ; https://stackoverflow.com/a/605931
    (setf (car lines) (concat " " (car lines)))
    (setq lines (mapcar (lambda (x) (substring x 1)) lines))
    (setq lines (seq-filter (lambda (x) (not (string-empty-p (string-trim x)))) lines))
    (setq char-lists nil)
    (dolist (line lines)
      (setq char-lists (cons (mapcar (lambda (x) (char-to-string x)) line) char-lists)))
    (setq char-lists (reverse char-lists))
    (setq indent-sizes (mapcar
			            (lambda (x) (let ((size 0) (li x))
				                      (while (string= (car x) " ")
					                    (setq size (+ 1 size))
					                    (setq x (cdr x)))
				                      size))
			            char-lists))
    (setq min-indent (seq-min indent-sizes))
    (setq result "")
    (dolist (line lines)
      (setq result (concat result (substring line min-indent) "\n")))
    (kill-new result)
    (setq deactivate-mark t)))

(with-eval-after-load 'python
  (defun dhnam/elpy-occur-definitions ()
    "Display an occur buffer of all definitions in the current buffer,
then go to the closest uppser location. Also, switch to that buffer.
This function is modified from `elpy-occur-definitions'"

    (interactive)
    (let* ((regexp "^\s*\\(\\(async\s\\|\\)def\\|class\\)\s")
           (closest-upper-point
            (save-excursion
              (move-end-of-line 1)
              (re-search-backward regexp nil t)
              (line-number-at-pos))))
      (let ((list-matching-lines-face nil))
        (occur regexp))
      (let ((window (get-buffer-window "*Occur*")))
        (if window
            (select-window window)
          (switch-to-buffer "*Occur*"))
        (when closest-upper-point
          (re-search-forward (format "%d:" closest-upper-point))))))

  (defun dhnam/bind-additional-python-commands (map)
    (define-key map (kbd "C-c C-o") 'dhnam/elpy-occur-definitions))

  (dhnam/bind-additional-python-commands python-mode-map)

  (with-eval-after-load 'realgud
    (dhnam/bind-additional-python-commands realgud:shortkey-mode-map)))

(progn
  (defun dhnam/convert-path-to-package ()
    ""
    ;; https://stackoverflow.com/a/25886353
    (interactive)
    (save-excursion
      (let* ((bounds (bounds-of-thing-at-point 'symbol))
             (start (car bounds))
             (end (cdr bounds)))
	    (if (use-region-p)
	        (replace-regexp "/" "." nil (region-beginning) (region-end))
	      (replace-regexp "/" "." nil start end)))))

  (defun dhnam/convert-package-to-path ()
    ""
    ;; https://stackoverflow.com/a/25886353
    (interactive)
    (save-excursion
      (let* ((bounds (bounds-of-thing-at-point 'symbol))
             (start (car bounds))
             (end (cdr bounds)))
	    (if (use-region-p)
	        (replace-regexp "." "/" nil (region-beginning) (region-end))
	      (replace-regexp "." "/" nil start end))))))

(progn
  (defun dhnam/copy-full-package-name ()
    (interactive)
    (let ((project-path (expand-file-name (locate-dominating-file default-directory ".git")))
          (file-path (buffer-file-name)))

      (let ((full-package-name
             (replace-regexp-in-string
              "/" "."
              (file-name-sans-extension (substring file-path (length project-path))))))
        (kill-new full-package-name)))))

(defun dhnam/insert-ipdb-config-example ()
  (interactive)
  (insert (dhnam/get-string-from-file (concat dhnam/lib-root-dir "common/dependent/ipdb-config-example.sh"))))

(progn
  (defhydra dhnam/python-indent ()
    ;; ,dhnam-iokl/plist-1

    "python indentation"

    ("<" python-indent-shift-left)
    (">" python-indent-shift-right)

    ;; ("q" nil "quit")
    ;; ("SPC" nil "quit")
    ("RET" nil "quit"))
  (hydra-set-property 'dhnam/python-indent :verbosity 0))


(progn
  (defvar dhnam/python-working-directory-files '(".working-dir" ".git"))
  (defun dhnam/get-python-working-directory ()
    (let ((file-names dhnam/python-working-directory-files)
          (directory nil))
      (while file-names
        (setq directory (locate-dominating-file default-directory (car file-names)))
        (if directory
            (setq file-names nil)
          (setq file-names (cdr file-names))))
      directory))

  (defun dhnam/run-python (&optional dir cmd dedicated show)
    "Modified from `run-python'"
    (interactive
     (if current-prefix-arg
         (list
          (read-directory-name "Directory: ")
          (read-shell-command "Run Python: " (python-shell-calculate-command))
          (y-or-n-p "Make dedicated process? ")
          (= (prefix-numeric-value current-prefix-arg) 4))
       (list (dhnam/get-python-working-directory) (python-shell-calculate-command) nil t)))

    (let ((default-directory (or dir default-directory)))
      (run-python cmd dedicated show))))

(defun dhnam/python-shell-send-region-or-buffer (start end &optional send-main msg)
  "`python-shell-send-region' when region is activated or `python-shell-send-buffer'."
  (interactive
   (list (region-beginning) (region-end) current-prefix-arg t))
  (if (use-region-p)
      (python-shell-send-region start end send-main msg)
    (python-shell-send-buffer send-main msg)))

(provide 'dhnam-python)
