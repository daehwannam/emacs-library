
(defmacro clone-hydra (new-name old-name body &optional docstring &rest heads)
  ;; This function is modified from `defhydra+'
  ;; `defhydra+' : https://github.com/abo-abo/hydra/issues/185

  "Redefine an existing hydra by adding new heads and give a new name.
`clone-hydra' is similar with `defhydra+'."

  (declare (indent defun) (doc-string 3))
  (unless (stringp docstring)
    (setq heads
          (cons docstring heads))
    (setq docstring nil))
  `(defhydra ,new-name ,(or body (hydra--prop old-name "/params"))
     ,(or docstring (hydra--prop old-name "/docstring"))
     ,@(cl-delete-duplicates
        (append (hydra--prop old-name "/heads") heads)
        :key #'car
        :test #'equal)))

(provide 'dhnam-hydra)
