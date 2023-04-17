(defun dhnam-iokl-org/init ()
  (eval
   `(when (package-installed-p 'org)

      (require 'org)

      (defhydra dhnam-iokl-org-navigation
        ,dhnam-iokl/plist-2

        "org structure editing"

	    ("i" org-previous-visible-heading)
	    ("o" org-next-visible-heading)
	    ("k" org-backward-heading-same-level)
	    ("l" org-forward-heading-same-level)

        (,dhnam-iokl/quit-key nil "quit")
        ;; (,dhnam-iokl/quit-key dhnam-iokl-org/body :exit t)
        ("SPC" nil "quit")
        ("q" nil "quit"))

     (clone-hydra dhnam-iokl-org dhnam-iokl-base
       ,dhnam-iokl/plist-1

       "iokl"

       ("s" dhnam-iokl-org-navigation/body  :exit t))

     (hydra-set-property 'dhnam-iokl-org-navigation :verbosity 0) ; disable any hint message
     (hydra-set-property 'dhnam-iokl-org :verbosity 0) ; disable any hint message
     (define-key org-mode-map (kbd ,dhnam-iokl/activation-key) 'dhnam-iokl-org/body))))


(provide 'dhnam-iokl-org)