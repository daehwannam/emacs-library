
(eval
 `(when (package-installed-p 'paredit)
    (require 'dhnam-paredit)
    (require 'dhnam-iokl-puni)

    (defhydra dhnam-iokl-paredit-struct
      ,dhnam-iokl/plist-2

      "paredit structure editing"

      ("k" paredit-backward-slurp-sexp)
      ("K" paredit-backward-barf-sexp)
      ("l" paredit-forward-slurp-sexp)
      ("L" paredit-forward-barf-sexp)

      ("i" paredit-split-sexp)
      ("o" paredit-join-sexps)
      ("I" paredit-raise-sexp)
      ("O" paredit-convolute-sexp)

      ("j" paredit-splice-sexp-killing-backward)
      (";" paredit-splice-sexp-killing-forward)

      ("s" paredit-splice-sexp)
      ("a" paredit-raise-sexp)

      ;; ("/" undo)
      ("C-/" undo)

      (,dhnam-iokl/quit-key nil "quit")
      ;; (,dhnam-iokl/quit-key dhnam-iokl-paredit-move/body :exit t)
      ("SPC" nil "quit")
      ("RET" nil "quit")
      ("q" nil "quit"))

    (clone-hydra dhnam-iokl-paredit-nav dhnam-iokl-puni-nav
      ,dhnam-iokl/plist-1

      "iokl"

      ;; ("j" dhnam/backward-sexp-to-beginning)
      ;; ("k" backward-sexp)
      ;; ("l" forward-sexp)
      ;; (";" dhnam/forward-sexp-to-end)

      ("a" dhnam-iokl-paredit-struct/body  :exit t))

    (hydra-set-property 'dhnam-iokl-paredit-struct :verbosity 0) ; disable any hint message
    (hydra-set-property 'dhnam-iokl-paredit-nav :verbosity 0) ; disable any hint message
    (comment
      (define-key paredit-mode-map (kbd ,dhnam-iokl/activation-key) 'dhnam-iokl-paredit-nav/body))))


(provide 'dhnam-iokl-paredit)
