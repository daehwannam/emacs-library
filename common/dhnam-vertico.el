

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
     initial)))

(provide 'dhnam-vertico)
