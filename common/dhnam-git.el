
(defun dhnam/insert-gitignore-example ()
  (interactive)
  (insert (dhnam/get-string-from-file "~/.emacs.d/config/init/dependent/gitignore.gitignore")))


(progn
  ;; Org-mode & tikz example
  ;; https://www.homepages.ucl.ac.uk/~ucahjde/blog/tikz.html

  (defun dhnam/insert-org-tikz-output-frame ()
    (interactive)
    (insert (dhnam/get-string-from-file "~/.emacs.d/config/init/dependent/org-tikz-output-frame.org")))

  (defun dhnam/insert-org-tikz-silent-frame ()
    (interactive)
    (insert (dhnam/get-string-from-file "~/.emacs.d/config/init/dependent/org-tikz-silent-frame.org"))))

(provide 'dhnam-git)
