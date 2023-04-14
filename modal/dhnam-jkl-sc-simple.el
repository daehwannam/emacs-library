
(require 'dhnam-macro)
(require 'dhnam-hydra)

(defvar dhnam-jkl-sc/default-cursor-color "orchid")
(defvar dhnam-jkl-sc/activated-cursor-color "cyan")
(defvar dhnam-jkl-sc-paredit-struct-cursor-color "orange")

(defconst dhnam-jkl-sc/activation-key "₢")
(defconst dhnam-jkl-sc/quit-key "₫")
(defconst dhnam-avy-key "₣")

(dhnam/set-cursor-color dhnam-jkl-sc/default-cursor-color)

(defconst dhnam-jkl-sc/plist-1
  '(:pre (dhnam/set-cursor-color dhnam-jkl-sc/activated-cursor-color)
    :post (dhnam/set-cursor-color dhnam-jkl-sc/default-cursor-color)))

(defconst dhnam-jkl-sc/plist-2
  '(:pre (dhnam/set-cursor-color dhnam-jkl-sc-paredit-struct-cursor-color)
    :post (dhnam/set-cursor-color dhnam-jkl-sc/default-cursor-color)))

(defun dhnam-jkl-sc/init ()
  (eval
   `(progn
      (progn
        ;; dhnam-jkl-sc
        (defhydra dhnam-jkl-sc
          ,dhnam-jkl-sc/plist-1

          "jkl-sc"

          ;; ("i" previous-line)
          ;; ("o" next-line)
          ;; ("j" backward-word)
          ;; (";" forward-word)
          ;; ("k" backward-char)
          ;; ("l" forward-char)

          ("k" backward-sexp)
          ("l" forward-sexp)
          ("j" backward-list)
          (";" forward-list)

          ;; ("u" backward-up-list)
          ;; ("i" down-list)
          ;; ("o" paredit-backward-down)
          ;; ("p" paredit-forward-up)

          ("i" backward-up-list)
          ("I" down-list)
          ("O" paredit-backward-down)
          ("o" paredit-forward-up)

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
          ;; ("SPC" nil "quit")

          ;; ("#" eval-last-sexp-or-region)
          ;; ("$" eval-print-last-sexp)
          )

        (progn
          ;; Disable any hint message
          (hydra-set-property 'dhnam-jkl-sc :verbosity 0))

        (define-key global-map (kbd ,dhnam-jkl-sc/activation-key) 'dhnam-jkl-sc/body))

      (when (package-installed-p 'paredit)
        (require 'dhnam-paredit)

        (defhydra dhnam-jkl-sc-paredit-struct
          ,dhnam-jkl-sc/plist-2

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

          ;; ("/" undo)
          ("C-/" undo)

          (,dhnam-jkl-sc/quit-key nil "quit")
          ;; (,dhnam-jkl-sc/quit-key dhnam-jkl-sc-paredit-move/body :exit t)
          ("SPC" nil "quit"))

        (clone-hydra dhnam-jkl-sc-paredit-move dhnam-jkl-sc
          ,dhnam-jkl-sc/plist-1

          "jkl-sc"

          ("s" dhnam-jkl-sc-paredit-struct/body  :exit t))

        (hydra-set-property 'dhnam-jkl-sc-paredit-struct :verbosity 0) ; disable any hint message
        (hydra-set-property 'dhnam-jkl-sc-paredit-move :verbosity 0) ; disable any hint message
        (define-key paredit-mode-map (kbd ,dhnam-jkl-sc/activation-key) 'dhnam-jkl-sc-paredit-move/body))

      (when (package-installed-p 'puni)
        (require 'dhnam-puni)

        (defhydra dhnam-jkl-sc-puni-struct
          ,dhnam-jkl-sc/plist-2

          "puni structure editing"

          ("k" puni-slurp-backward)
          ("K" puni-barf-backward)
          ("l" puni-slurp-forward)
          ("L" puni-barf-forward)

          ("i" puni-split)
          ("o" paredit-join-sexps)  ; puni-join doesn't exist
          ("I" puni-raise)
          ("O" puni-convolute)

          ("j" puni-splice-killing-backward)
          (";" puni-splice-killing-forward)

          ;; ("/" undo)
          ("C-/" undo)

          (,dhnam-jkl-sc/quit-key nil "quit")
          ;; (,dhnam-jkl-sc/quit-key dhnam-jkl-sc-puni-move/body :exit t)
          ("SPC" nil "quit"))

        (clone-hydra dhnam-jkl-sc-puni-move dhnam-jkl-sc
          ,dhnam-jkl-sc/plist-1

          "jkl-sc"

          ("k" puni-backward-sexp)
          ("l" puni-forward-sexp)

          ("s" dhnam-jkl-sc-puni-struct/body  :exit t))

        (hydra-set-property 'dhnam-jkl-sc-puni-struct :verbosity 0) ; disable any hint message
        (hydra-set-property 'dhnam-jkl-sc-puni-move :verbosity 0) ; disable any hint message
        (define-key puni-mode-map (kbd ,dhnam-jkl-sc/activation-key) 'dhnam-jkl-sc-puni-move/body))

      (progn
        (with-eval-after-load 'vterm-seamless
          (let ((map vterm-seamless-mode-map))
            (define-key map (kbd ,dhnam-jkl-sc/activation-key) nil)))

        (comment
          (with-eval-after-load 'vterm-seamless
            (clone-hydra dhnam-jkl-sc-vterm-seamless-copy dhnam-jkl-sc
              ,dhnam-jkl-sc/plist-1

              "jkl-sc"

              ("S" vtsl/end-of-buffer)
              ("SPC" (vterm-copy-mode 1) :exit t)
              ;; (,dhnam-jkl-sc/quit-key vtsl/copy-mode-exit :exit t)
              (,dhnam-jkl-sc/quit-key nil "quit") ; it defines `dhnam-jkl-sc-vterm-seamless-copy/nil'
              ("RET" vtsl/copy-mode-exit :exit t)
              ("<return>" vtsl/copy-mode-exit :exit t))

            (progn
              (defun dhnam-jkl-sc-vterm-seamless-copy/copy-mode-exit ()
                (interactive)
                (when (and vterm-copy-mode
                           (= (line-number-at-pos (point)) vtsl/last-cursor-line-num))
                  (dhnam-jkl-sc-vterm-seamless-copy/nil)
                  (dhnam/set-cursor-color dhnam-jkl-sc/default-cursor-color)))

              (advice-add 'vtsl/trigger-copy-mode-exit :before 'dhnam-jkl-sc-vterm-seamless-copy/copy-mode-exit))

            (defun dhnam-jkl-sc-vterm-seamless-copy/body-after-previous-line ()
              (interactive)
              (previous-line)
              (dhnam-jkl-sc-vterm-seamless-copy/body))

            (progn
              ;; make vtsl/copy-mode-then-dhnam-jkl-sc-vterm-seamless-copy/body-after-previous-line
              (vtsl/copy-mode-then 'dhnam-jkl-sc-vterm-seamless-copy/body-after-previous-line)
              (assert (fboundp 'vtsl/copy-mode-then-dhnam-jkl-sc-vterm-seamless-copy/body-after-previous-line)))

            (let ((map vterm-seamless-mode-map))
              (define-key map (kbd ,dhnam-jkl-sc/activation-key)
                (vtsl/copy-mode-then 'dhnam-jkl-sc-vterm-seamless-copy/body-after-previous-line)))

            ;; disable any hint message
            (hydra-set-property 'dhnam-jkl-sc-vterm-seamless-copy :verbosity 0)

            (comment
              (defhydra dhnam-jkl-sc-vterm-seamless
                ,dhnam-jkl-sc/plist-1

                "jkl-sc"

                ("i" vtsl/copy-mode-then-dhnam-jkl-sc-vterm-seamless-copy/body-after-previous-line :exit t)
                ("k" (vterm-send-key "b" nil nil t))
                ("l" (vterm-send-key "f" nil nil t))
                ("j" (vterm-send-key "b" nil t))
                (";" (vterm-send-key "f" nil t))

                ("q" dhnam/scroll-down-small)
                ("w" dhnam/scroll-up-small)
                ("a" (vterm-send-key "a" nil nil t))
                ("s" (vterm-send-key "e" nil nil t)))

              (let ((map vterm-seamless-mode-map))
                (define-key map (kbd ,dhnam-jkl-sc/activation-key) 'dhnam-jkl-sc-vterm-seamless/body))

              (let ((map vterm-seamless-copy-mode-map))
                (define-key map (kbd ,dhnam-jkl-sc/activation-key) 'dhnam-jkl-sc-vterm-seamless-copy/body))

              ;; disable any hint message
              (hydra-set-property 'dhnam-jkl-sc-vterm-seamless :verbosity 0)))))

      (comment
        (with-eval-after-load 'ivy
          (clone-hydra dhnam-jkl-sc-ivy dhnam-jkl-sc
            ,dhnam-jkl-sc/plist-1

            "jkl-sc"

            ("i" ivy-previous-line)
            ("o" ivy-next-line)

            ("z" ivy-beginning-of-buffer)
            ("x" ivy-end-of-buffer)
            ;; (,dhnam-jkl-sc/quit-key vtsl/copy-mode-exit :exit t)
            ;; (,dhnam-jkl-sc/quit-key nil "quit") ; it defines `dhnam-jkl-sc-vterm-seamless-copy/nil'
            ;; ("RET" vtsl/copy-mode-exit :exit t)
            ;; ("<return>" vtsl/copy-mode-exit :exit t)
            )

          (define-key ivy-minibuffer-map ,dhnam-jkl-sc/activation-key 'dhnam-jkl-sc-ivy/body)
          (hydra-set-property 'dhnam-jkl-sc-ivy :verbosity 0))))))


(provide 'dhnam-jkl-sc-simple)
