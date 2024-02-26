
(eval
 `(progn
    (require 'dhnam-iokl-puni)

    (clone-hydra dhnam-iokl-python-nav dhnam-iokl-puni-nav
      ,dhnam-iokl/plist-1

      "iokl"

      ("i" python-nav-backward-up-list)
      ("o" python-nav-up-list))

    (hydra-set-property 'dhnam-iokl-python-nav :verbosity 0) ; disable any hint message

    (comment
      (with-eval-after-load 'python
        (define-key python-mode-map (kbd ,dhnam-iokl/activation-key) 'dhnam-iokl-python-nav/body)))))

(provide 'dhnam-iokl-python)
