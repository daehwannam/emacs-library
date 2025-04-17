
(defun dhnam/get-py-code-without-empty-lines (beg end &optional with-empty-lines)
  (let* ((lines (let* ((remaining-lines (split-string (buffer-substring-no-properties beg end) "^")))
                  (while (and remaining-lines (string-empty-p (string-trim (car remaining-lines))))
                    ;; Remove top empty lines
                    (pop remaining-lines))

                  (when remaining-lines
                    (let* ((reversed-remaining-lines (reverse remaining-lines)))
                      (while (and reversed-remaining-lines (string-empty-p (string-trim (car reversed-remaining-lines))))
                        ;; Remove bottem empty lines
                        (pop reversed-remaining-lines))

                      ;; remove a new-line character for the last line
                      (setf (car reversed-remaining-lines) (string-trim-right (car reversed-remaining-lines)))

                      ;; recover from the reverse
                      (reverse reversed-remaining-lines)))))
         (min-indent (if lines
                         (let* ((char-lists (mapcar (lambda (line) (mapcar (lambda (ch) (char-to-string ch)) line))
                                                    (seq-remove (lambda (x) (string-empty-p (string-trim x))) lines)))
                                (indent-sizes (mapcar
			                                   (lambda (x) (let ((size 0) (li x))
				                                             (while (string= (car x) " ")
					                                           (setq size (+ 1 size))
					                                           (setq x (cdr x)))
				                                             size))
			                                   char-lists)))
                           (seq-min indent-sizes))
                       0)))


    (let ((unfiltered-lines (if with-empty-lines
                                lines
                              (seq-remove (lambda (x) (string-empty-p (string-trim x))) lines))))
      (string-join (mapcar (lambda (line)
                             (if (string-empty-p (string-trim line))
                                 "\n"
                               (substring line min-indent))) unfiltered-lines)))))

(comment
  (defun dhnam/get-py-code-without-empty-lines (beg end &optional with-empty-lines)
    ;; https://stackoverflow.com/questions/605846/how-do-i-access-the-contents-of-the-current-region-in-emacs-lisp
    ;; https://stackoverflow.com/questions/6236196/elisp-split-string-function-to-split-a-string-by-character
    (let (lines char-lists indent-sizes min-indent result)
      (setq lines (nbutlast (split-string (buffer-substring-no-properties beg end) "$") 1)) ; https://stackoverflow.com/a/605931
      (setf (car lines) (concat " " (car lines)))
      (setq lines (mapcar (lambda (x) (substring x 1)) lines))
      (unless with-empty-lines
        (setq lines (seq-filter (lambda (x) (not (string-empty-p (string-trim x)))) lines)))
      (setq char-lists nil)
      (dolist (line lines)
        (when 
            (setq char-lists (cons (mapcar (lambda (x) (char-to-string x)) line) char-lists))))
      (setq char-lists (reverse char-lists))
      (setq indent-sizes (mapcar
			              (lambda (x) (let ((size 0) (li x))
				                        (while (string= (car x) " ")
					                      (setq size (+ 1 size))
					                      (setq x (cdr x)))
				                        size))
			              char-lists))
      (setq min-indent (seq-min indent-sizes))
      (setq result
            (string-join (mapcar (lambda (line) (concat (substring line min-indent) "\n")) lines)))
      ;; (setq result "")
      ;; (dolist (line lines)
      ;;   (setq result (concat result (substring line min-indent) "\n")))

      (string-trim-right result))))

(defun dhnam/py-repl-kill-ring-save-without-empty-lines (beg end &optional region with-empty-lines)
  (interactive (list (mark) (point)
		             (prefix-numeric-value current-prefix-arg)))

  (kill-new (dhnam/get-py-code-without-empty-lines beg end with-empty-lines))
  (setq deactivate-mark t))

(comment
  (defun dhnam/python-shell-buffer-substring (start end &optional nomain)
    "Send buffer substring from START to END formatted for shell.
This is Modified from `python-shell-buffer-substring'
"
    (let* ((start (save-excursion
                    ;; Normalize start to the line beginning position.
                    (goto-char start)
                    (line-beginning-position)))
           (substring (buffer-substring-no-properties start end))
           (encoding (python-info-encoding))
           (toplevel-p (zerop (save-excursion
                                (goto-char start)
                                (python-util-forward-comment 1)
                                (current-indentation)))))
      (with-temp-buffer
        (python-mode)
        (insert substring)
        (goto-char (point-min))
        (when nomain
          (let* ((if-name-main-start-end
                  (and nomain
                       (save-excursion
                         (when (python-nav-if-name-main)
                           (cons (point)
                                 (progn (python-nav-forward-sexp-safe)
                                        ;; Include ending newline
                                        (forward-line 1)
                                        (point)))))))
                 ;; Oh destructuring bind, how I miss you.
                 (if-name-main-start (car if-name-main-start-end))
                 (if-name-main-end (cdr if-name-main-start-end)))
            (when if-name-main-start-end
              (goto-char if-name-main-start)
              (delete-region if-name-main-start if-name-main-end)
              (insert fillstr))))
        ;; Ensure there's only one coding cookie in the generated string.
        (goto-char (point-min))
        (when (looking-at-p (python-rx coding-cookie))
          (forward-line 1)
          (when (looking-at-p (python-rx coding-cookie))
            (delete-region
             (line-beginning-position) (line-end-position))))
        (buffer-substring-no-properties (point-min) (point-max))))))

(with-eval-after-load 'python
  (defvar dhnam/python-definition-regex "^\s*\\(\\(async\s\\|\\)def\\|class\\)\s")
  (defun dhnam/elpy-occur-definitions ()
    "Display an occur buffer of all definitions in the current buffer,
then go to the closest uppser location. Also, switch to that buffer.
This function is modified from `elpy-occur-definitions'"

    (interactive)
    (let* ((regexp dhnam/python-definition-regex)
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

  (defun dhnam/convert-module-to-path ()
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
  (defun dhnam/get-full-module-name ()
    (let ((file-path (buffer-file-name))
          (project-path nil)
          (site-packages-parent-path nil))
      (cond
       ((setq project-path (locate-dominating-file default-directory ".git"))
        (replace-regexp-in-string
         "/" "."
         (file-name-sans-extension (substring file-path (length (expand-file-name project-path))))))
       ((setq site-packages-parent-path (locate-dominating-file default-directory "site-packages"))
        (let ((site-packages-path (concat site-packages-parent-path "site-packages/")))
          (replace-regexp-in-string
           "/" "."
           (file-name-sans-extension (substring file-path (length (expand-file-name site-packages-path))))))))))

  (defun dhnam/copy-full-module-name ()
    (interactive)
    (kill-new (dhnam/get-full-module-name)))

  (defun dhnam/copy-statement-of-importing-all-from-full-module-name ()
    (interactive)
    (kill-new (format "from %s import *" (dhnam/get-full-module-name))))

  (defun dhnam/copy-statement-of-importing-symbol-at-point ()
    (interactive)
    (kill-new (format "from %s import %s"
                      (dhnam/get-full-module-name)
                      (substring-no-properties (thing-at-point 'symbol))))))

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

(defun dhnam/python-shell-send-from-package-import-all ()
  (interactive)
  (python-shell-send-buffer (format "from %s import *" (dhnam/get-full-module-name))))

(defun dhnam/python-get-doc-string-code (start end &optional space-between-blocks)
  (let ((code nil))
    (save-excursion
      (save-restriction
        (narrow-to-region start end)
        (goto-char (point-min))
        (let ((prev-line-number nil)
              (code-line-prefix "^ *\\(>>>\\|\\.\\.\\.\\) ")
              (new-code-lines nil))
          (while (re-search-forward code-line-prefix nil t)
            (let ((line-start (save-excursion (move-beginning-of-line 1) (point)))
                  (line-end (save-excursion (move-end-of-line 1) (point))))

              (let* ((old-line (buffer-substring-no-properties line-start line-end))
                     (new-line (replace-regexp-in-string code-line-prefix "" old-line)))
                (when (and space-between-blocks
                           prev-line-number
                           (> (- (line-number-at-pos) prev-line-number) 1))
                  (push "" new-code-lines))
                (push new-line new-code-lines)))
            (setq prev-line-number (line-number-at-pos)))
          (string-join (reverse new-code-lines) "\n"))))))

(defun dhnam/python-copy-code-from-docstring (start end)
  (interactive (list (region-beginning) (region-end)))
  (kill-new (dhnam/python-get-doc-string-code start end t))
  (setq deactivate-mark t))

(defun dhnam/python-get-example-code (start end &optional space-between-blocks)
  (if (save-excursion
        ;; Check ">>>" exists in the region.
        (save-restriction
          (narrow-to-region start end)
          (beginning-of-buffer)
          (re-search-forward ">>>" nil t)))
      (dhnam/python-get-doc-string-code start end space-between-blocks)
    (progn
      (comment (dhnam/python-shell-buffer-substring start end))
      (dhnam/get-py-code-without-empty-lines start end t))))

(require 'dhnam-comint)

(defun dhnam/comint-with-python-command (command &optional dir)
  (interactive
   (if current-prefix-arg
       (list
        (read-shell-command "Run Python: " (python-shell-calculate-command))
        (read-directory-name "Directory: "))
     (list (python-shell-calculate-command) nil)))

  (let ((default-directory (or dir (dhnam/get-python-working-directory))))
    (dhnam/comint-with-command command)))

(defvar dhnam/python-dhnamlib-doctesting-format "python -m dhnamlib.pylib.doctesting -v %s")
(defun dhnam/run-python-dhnamlib-doctesting (module)
  (interactive
   (list (dhnam/get-full-module-name)))

  (let ((cmd (format dhnam/python-dhnamlib-doctesting-format module)))
    (comment (dhnam/run-python (dhnam/get-python-working-directory) cmd t t))
    (let ((buffer (dhnam/comint-with-python-command cmd)))
      (set-buffer buffer)
      (end-of-buffer))))

(progn
  (defvar dhnam/python-module-function-history nil)

  (defun dhnam/run-python-module-function (module function)
    (interactive
     (list (dhnam/get-full-module-name)
           (let ((default-function
                   (let ((symbol-at-point (thing-at-point 'symbol)))
                     (when symbol-at-point
                       (substring-no-properties symbol-at-point)))))
             (read-string
              (if default-function (format "Function (%s): " default-function) "Function: ")
              nil dhnam/python-module-function-history default-function))))

    (let* ((cmd (format "python -c 'import %s; %s.%s()'" module module function)))
      (dhnam/comint-with-python-command cmd))))

(progn
  (defvar dhnam/python-doc-code-temp-file-format "/tmp/XXXXX.py")
  (defvar dhnam/python-dhnamlib-script-interacting-format
    "PYTHONPATH=%s python -m dhnamlib.pylib.script_interacting %s")

  (defun dhnam/run-python-doc-code (start end &optional copying-command)
    (interactive "r")

    (comment (dhnam/without-message (shell-command (format "rm -f /tmp/%s*" "SOME-PREFIX"))))
    (comment (concat (make-temp-file "SOME-PREFIX") ".py"))

    (dhnam/without-message
      (shell-command (format "mkdir -p $(dirname %s)" dhnam/python-doc-code-temp-file-format)))

    (let ((temp-file-path (string-trim (shell-command-to-string
                                        (format "mktemp %s" dhnam/python-doc-code-temp-file-format))))
          (doc-string-code (dhnam/python-get-example-code start end t))
          (module-full-name (dhnam/get-full-module-name)))
      (with-temp-file temp-file-path
        (insert (format "from %s import *" module-full-name))
        (insert "\n\n")
        (insert doc-string-code))

      (dhnam/without-message
        (shell-command (format "chmod 600 %s" temp-file-path)))

      (let ((command (format dhnam/python-dhnamlib-script-interacting-format
                             (dhnam/get-python-working-directory) temp-file-path)))
        (setq deactivate-mark t)
        (if (or copying-command current-prefix-arg)
            (progn
              (kill-new command)
              (message command))
          (dhnam/comint-with-python-command command)))))

  (defun dhnam/copy-python-doc-code-command (start end)
    (interactive "r")
    (dhnam/run-python-doc-code start end t)))

(defun dhnam/get-conda-environment-python-interpreter-path (env-name)
  "When `env-name' is nil, return path of the base interpreter"
  (let ((conda-path (dhnam/string-trim (shell-command-to-string "echo $(conda info --base)"))))
    (if env-name
        (format (concat conda-path "/envs/%s/bin/python") env-name)
      (concat conda-path "/bin/python"))))

(defun dhnam/get-base-tramp-path (path)
  (concat (string-join (butlast (split-string path ":")) ":") ":"))

(defun dhnam/get-conda-environment-python-interpreter-tramp-path (env-name)
  "When `env-name' is nil, return path of the base interpreter"

  (let ((local-interpreter-path (dhnam/get-local-conda-environment-python-interpreter-path env-name)))
    (if (tramp-tramp-file-p default-directory)
        (concat (dhnam/get-base-tramp-path default-directory)
                local-interpreter-path)
      local-interpreter-path)))

(defun dhnam/unset-python-pdbtrack-setup-tracking ()
  "Disable `python-pdbtrack-setup-tracking', which is called when initializing `inferior-python-mode'.
It can be used to disable the default gud pdb when a breakpoint is activated in a *Python* buffer.
"

  (setq comint-input-filter-functions
        (remove #'python-pdbtrack-comint-input-filter-function
                comint-input-filter-functions))
  (setq comint-output-filter-functions
        (remove #'python-pdbtrack-comint-output-filter-function
                comint-output-filter-functions))
  (setq kill-buffer-hook
        (remove #'python-pdbtrack-tracking-finish kill-buffer-hook)))

(progn
  (defun dhnam/python-toggle-doc-string-code (start end)
    "Toggle doc-string code"

    (interactive "r")

    (save-excursion
      (goto-char start)
      (while (and (< (point) (point-max))
                  (white-space-line-p))
        (next-line))

      (back-to-indentation)
      (if (and (or (eq ?> (char-after))
                   (eq ?. (char-after))
                   (let ((prefix (buffer-substring-no-properties (point) (min (+ (point) 4) (point-max)))))
                     (or (string= prefix ">>> ")
                         (string= prefix "... ")))))
          (dhnam/python-remove-doc-string-code start end)
        (dhnam/python-add-doc-string-code start end))))

  (defun dhnam/get-line-indent (&optional p)
    (save-excursion
      (let ((p (or p (point))))
        (goto-char p)
        (- (progn (back-to-indentation) (point))
           (progn (beginning-of-line) (point))))))

  (defun white-space-line-p (&optional p)
    (save-excursion
      (let ((p (or p (point))))
        (goto-char p)
        (let ((start (progn (beginning-of-line) (point)))
              (end (progn (end-of-line) (point))))
          (string=
           (string-trim (buffer-substring-no-properties start end))
           "")))))

  (defun dhnam/python-add-doc-string-code (start end)
    "Add doc-string code"

    (interactive "r")

    (save-excursion
      (goto-char start)
      (while (and (< (point) (point-max))
                  (white-space-line-p))
        (next-line))

      (let ((base-indent (dhnam/get-line-indent))
            (last-point nil)
            (num-added-chars 0))

        (back-to-indentation)

        (while (and (< (point) (+ end num-added-chars))
                    (not (equal (point) last-point))
                    (< (point) (point-max)))

          (setq last-point (point))

          (let ((indent (dhnam/get-line-indent))
                (prev-line-using-deco   ; when a previous line uses decoration that starts with @
                 (save-excursion
                   (previous-line)
                   (back-to-indentation)
                   (equal (buffer-substring-no-properties (point) (+ (point) 5))
                          ">>> @")))
                (closing-paren
                 (save-excursion
                   (back-to-indentation)
                   (eq ?\) (char-after)))))
            (cond
             ((and (= indent base-indent)
                   (not prev-line-using-deco)
                   (not closing-paren))
              (beginning-of-line)
              (forward-char base-indent)
              (insert ">>> ")
              (backward-char 4)
              (setq num-added-chars (+ num-added-chars 4)))
             ((or (> indent base-indent)
                  prev-line-using-deco
                  closing-paren)
              (beginning-of-line)
              (forward-char base-indent)
              (insert "... ")
              (backward-char 4)
              (setq num-added-chars (+ num-added-chars 4)))))
          (next-line)))))

  (defun dhnam/python-remove-doc-string-code (start end)
    "Remove doc-string code"

    (interactive "r")

    (save-excursion
      (goto-char start)
      (while (and (< (point) (point-max))
                  (white-space-line-p))
        (next-line))

      (let ((base-indent (dhnam/get-line-indent))
            (last-point nil)
            (num-removed-chars 0))

        (back-to-indentation)

        (while (and (< (point) (- end num-removed-chars))
                    (not (equal (point) last-point))
                    (< (point) (point-max)))

          (setq last-point (point))

          (let ((indent (dhnam/get-line-indent)))
            (when (and (= indent base-indent)
                       (progn
                         (beginning-of-line)
                         (forward-char base-indent)
                         (let ((prefix (buffer-substring-no-properties (point) (min (+ (point) 4) (point-max)))))
                           (or (string= prefix ">>> ")
                               (string= prefix "... ")))))
              (delete-char 4)
              (setq num-removed-chars (+ num-removed-chars 4))))
          (next-line))))))

(provide 'dhnam-python)
