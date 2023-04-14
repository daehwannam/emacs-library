
(defun dhnam-iokl-base/init ()
  (eval
   `(progn
      (progn
        ;; dhnam-iokl-base
        (defhydra dhnam-iokl-base
          ,dhnam-iokl/plist-1

          "iokl"

          ;; ("i" previous-line)
          ;; ("o" next-line)
          ;; ("j" backward-word)
          ;; (";" forward-word)
          ;; ("k" backward-char)
          ;; ("l" forward-char)

          ("k" backward-sexp)
          ("l" forward-sexp)
          ;; ("j" backward-list)
          ;; (";" forward-list)

          ;; ("u" backward-up-list)
          ;; ("i" down-list)
          ;; ("o" paredit-backward-down)
          ;; ("p" paredit-forward-up)

          ("i" backward-up-list)
          ("I" down-list)
          ("O" paredit-backward-down)
          ("o" paredit-forward-up)

          (","  dhnam/scroll-down-small)
          ("."  dhnam/scroll-up-small)
          ("<"  scroll-down)
          (">"  scroll-up)

          ("M-r" move-to-window-line-top-bottom)
          ("M-R" dhnam/reverse-move-to-window-line-top-bottom)

          ;; ("q"  dhnam/scroll-down-small)
          ;; ("w"  dhnam/scroll-up-small)
          ;; ("Q"  scroll-down)
          ;; ("W"  scroll-up)

          ;; ("a" move-beginning-of-line)
          ;; ("s" move-end-of-line)
          ;; ("m" back-to-indentation)

          ;; ("z" beginning-of-buffer)
          ;; ("x" end-of-buffer)

          ;; ("e" move-to-window-line-top-bottom)
          ;; ("E" dhnam/reverse-move-to-window-line-top-bottom)
          ;; ("d" recenter-top-bottom)
          ;; ("D" dhnam/reverse-recenter-top-bottom)

          ;; ("C-SPC" set-mark-command)
          ;; ("C-@" set-mark-command)
          ;; ("r" set-mark-command)
          ;; ("'" exchange-point-and-mark)

          ;; ;; ("/" undo)
          ;; ("C-/" undo)
          ("SPC" nil "quit")
          ("q" nil "quit")

          ;; ("#" eval-last-sexp-or-region)
          ;; ("$" eval-print-last-sexp)
          )

        (progn
          ;; Disable any hint message
          (hydra-set-property 'dhnam-iokl-base :verbosity 0))

        (define-key global-map (kbd ,dhnam-iokl/activation-key) 'dhnam-iokl-base/body))

      (comment
        (with-eval-after-load 'ivy
          (clone-hydra dhnam-iokl-ivy dhnam-iokl-base
            ,dhnam-iokl/plist-1

            "iokl"

            ("i" ivy-previous-line)
            ("o" ivy-next-line)

            ("z" ivy-beginning-of-buffer)
            ("x" ivy-end-of-buffer)
            ;; (,dhnam-iokl/quit-key vtsl/copy-mode-exit :exit t)
            ;; (,dhnam-iokl/quit-key nil "quit") ; it defines `dhnam-iokl-vterm-seamless-copy/nil'
            ;; ("RET" vtsl/copy-mode-exit :exit t)
            ;; ("<return>" vtsl/copy-mode-exit :exit t)
            )

          (define-key ivy-minibuffer-map ,dhnam-iokl/activation-key 'dhnam-iokl-ivy/body)
          (hydra-set-property 'dhnam-iokl-ivy :verbosity 0))))))


(provide 'dhnam-iokl-base)
