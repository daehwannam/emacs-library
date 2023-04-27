
(eval
 `(progn
    (defhydra dhnam-iokl-compilation-move
      ,dhnam-iokl/plist-2

      "compilation"

      ("i" compilation-previous-file)
      ("o" compilation-next-file)
      ("k" compilation-previous-error)
      ("l" compilation-next-error)

      ;; (,dhnam-iokl/quit-key nil "quit")
      ;; (,dhnam-iokl/quit-key dhnam-iokl-compilation/body :exit t)
      ("SPC" nil "quit")
      ("RET" nil "quit")
      ("q" nil "quit"))

    (clone-hydra dhnam-iokl-compilation dhnam-iokl-puni-nav
      ,dhnam-iokl/plist-1

      "iokl"

      ("s" dhnam-iokl-compilation-move/body  :exit t))))

(hydra-set-property 'dhnam-iokl-compilation-move :verbosity 0) ; disable any hint message
(hydra-set-property 'dhnam-iokl-compilation :verbosity 0) ; disable any hint message

(with-eval-after-load 'compile
  (define-key compilation-shell-minor-mode-map (kbd dhnam-iokl/activation-key) 'dhnam-iokl-compilation/body))

(provide 'dhnam-iokl-compilation)
