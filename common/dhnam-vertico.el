

(when (package-installed-p 'consult)
  (defun consult-grep-on-default-directory (&optional dir initial)
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
