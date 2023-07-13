
(progn
  ;; https://stackoverflow.com/a/27871987

  (defvar dhnam/python-additional-electric-pairs '((?' . ?')) "Electric pairs for python-mode.")
  (defun dhnam/set-python-electric-pairs ()
    (setq-local electric-pair-pairs (append electric-pair-pairs dhnam/python-additional-electric-pairs))
    (setq-local electric-pair-text-pairs electric-pair-pairs)))

(progn
  ;; https://stackoverflow.com/a/69765466
  (defun dhnam/set-org-electric-pair-inhibit-predicate ()
    (setq-local electric-pair-inhibit-predicate
                `(lambda (c)
                   (if (char-equal c ?<) t (,electric-pair-inhibit-predicate c))))))

(provide 'dhnam-electric-pair)
