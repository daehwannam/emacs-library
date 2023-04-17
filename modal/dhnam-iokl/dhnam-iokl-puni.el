
(defun dhnam-iokl-puni/init ()
  (eval
   `(when (package-installed-p 'puni)
      (require 'dhnam-puni)

      (defhydra dhnam-iokl-puni-struct
        ,dhnam-iokl/plist-2

        "puni structure editing"

        ("k" puni-slurp-backward)
        ("K" puni-barf-backward)
        ("l" puni-slurp-forward)
        ("L" puni-barf-forward)

        ("i" puni-split)
        ("o" paredit-join-sexps)         ; puni-join doesn't exist
        ("I" puni-raise)
        ("O" puni-convolute)

        ("j" puni-splice-killing-backward)
        (";" puni-splice-killing-forward)

        ;; ("/" undo)
        ("C-/" undo)

        (,dhnam-iokl/quit-key nil "quit")
        ;; (,dhnam-iokl/quit-key dhnam-iokl-puni-move/body :exit t)
        ("q" nil "quit")
        ("SPC" nil "quit")
        ("RET" nil "quit"))

      (clone-hydra dhnam-iokl-puni-move dhnam-iokl-base
        ,dhnam-iokl/plist-1

        "iokl"

        ("k" puni-backward-sexp)
        ("l" puni-forward-sexp)

        ("a" dhnam-iokl-puni-struct/body  :exit t))

      (hydra-set-property 'dhnam-iokl-puni-struct :verbosity 0) ; disable any hint message
      (hydra-set-property 'dhnam-iokl-puni-move :verbosity 0) ; disable any hint message
      (define-key puni-mode-map (kbd ,dhnam-iokl/activation-key) 'dhnam-iokl-puni-move/body))))

(provide 'dhnam-iokl-puni)
