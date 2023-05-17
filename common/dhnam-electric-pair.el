
(progn
  ;; https://stackoverflow.com/a/27871987

  (defvar python-electric-pairs '((?' . ?')) "Electric pairs for python-mode.")
  (defun dhnam/python-add-electric-pairs ()
    (setq-local electric-pair-pairs (append electric-pair-pairs python-electric-pairs))
    (setq-local electric-pair-text-pairs electric-pair-pairs)))


(provide 'dhnam-electric-pair)
