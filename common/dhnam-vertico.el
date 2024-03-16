

(when (package-installed-p 'consult)
  (defun dhnam/consult-grep-on-default-directory (&optional dir initial)
    "Similar to `consult-grep' but it always searches over the current directory.
In contrast, `consult-grep' searches over the current git project if the current directory belongs to a git project."
    (interactive "P")
    (setq dir (or dir default-directory))
    (consult--grep
     "Grep"
     (cond
      ((fboundp 'consult--grep-make-builder) #'consult--grep-make-builder)
      ;; `consult--grep-builder' is used in old version of vertico
      ((fboundp 'consult--grep-builder) #'consult--grep-builder))
     dir
     initial))

  (defun dhnam/consult-find-from-current-dir (&optional dir initial)
    "This function is modified from `consult-find'.
Unlike `consult-find', the default location is always the current directory rather than the project root.
Search for files in DIR matching input regexp given INITIAL input.
See `consult-grep' for details regarding the asynchronous search
and the arguments."
    (interactive "P")
    (let ((dir (or dir default-directory)))
      (pcase-let* ((`(,prompt ,paths ,dir) (consult--directory-prompt "Find" dir))
                   (default-directory dir)
                   (builder (consult--find-make-builder paths)))
        (find-file (consult--find prompt builder initial))))))

(progn
  (defvar dhnam/minibuffer-boundary-start "\\_<")
  (defvar dhnam/minibuffer-boundary-end "\\_>")

  (defun dhnam/minibuffer--insert-symbol-boundaries ()
    "Insert symbol boundaries."
    (move-end-of-line 1)
    (insert dhnam/minibuffer-boundary-end)
    (move-beginning-of-line 1)
    (insert dhnam/minibuffer-boundary-start)
    (move-end-of-line 1))

  (defun dhnam/minibuffer--remove-symbol-boundaries ()
    "Similar to `dhnam/minibuffer--insert-symbol-boundaries', but it removes symbol boundaries."
    (move-beginning-of-line 1)
    (delete-char (length dhnam/minibuffer-boundary-start))
    (move-end-of-line 1)
    (backward-delete-char (length dhnam/minibuffer-boundary-end)))

  (defun dhnam/minibuffer-toggle-symbol-boundaries ()
    (interactive)
    (let ((line-begin (save-excursion (move-beginning-of-line 1) (point)))
          (line-end (save-excursion (move-end-of-line 1) (point))))
      (when (not (string= (buffer-substring-no-properties line-begin line-end) "#"))
        (if (and (>= (- line-end line-begin) (+ (length dhnam/minibuffer-boundary-start) (length dhnam/minibuffer-boundary-end)))
                 (let ((prefix (buffer-substring-no-properties
                                line-begin (+ line-begin (length dhnam/minibuffer-boundary-start)))))
                   (string= prefix dhnam/minibuffer-boundary-start))
                 (let ((suffix (buffer-substring-no-properties
                                (- line-end (length dhnam/minibuffer-boundary-end)) line-end)))
                   (string= suffix dhnam/minibuffer-boundary-end)))
            (dhnam/minibuffer--remove-symbol-boundaries)
          (dhnam/minibuffer--insert-symbol-boundaries))))))

(provide 'dhnam-vertico)
