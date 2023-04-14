
;;; Bindings with IOKL keys

(require 'dhnam-macro)
(require 'dhnam-hydra)

(defvar dhnam-iokl/default-cursor-color "orchid")
(defvar dhnam-iokl/activated-cursor-color "cyan")
(defvar dhnam-iokl-paredit-struct-cursor-color "orange")

(defconst dhnam-iokl/activation-key "₢")
(defconst dhnam-iokl/quit-key "₫")
(defconst dhnam-avy-key "₣")

(dhnam/set-cursor-color dhnam-iokl/default-cursor-color)

(defconst dhnam-iokl/plist-1
  '(:pre (dhnam/set-cursor-color dhnam-iokl/activated-cursor-color)
    :post (dhnam/set-cursor-color dhnam-iokl/default-cursor-color)))

(defconst dhnam-iokl/plist-2
  '(:pre (dhnam/set-cursor-color dhnam-iokl-paredit-struct-cursor-color)
    :post (dhnam/set-cursor-color dhnam-iokl/default-cursor-color)))

(defun dhnam-iokl/init ()
  (eval
   `(progn
      (progn
        ;; dhnam-iokl
        (defhydra dhnam-iokl
          ,dhnam-iokl/plist-1

          "iok"

          ("i" previous-line)
          ("o" next-line)
          ("j" backward-word)
          (";" forward-word)
          ("k" backward-char)
          ("l" forward-char)

          ("J" backward-sexp)
          (":" forward-sexp)
          ("K" backward-list)
          ("L" forward-list)

          ("U" backward-up-list)
          ("I" down-list)
          ("O" paredit-backward-down)
          ("P" paredit-forward-up)

          ("q"  dhnam/scroll-down-small)
          ("w"  dhnam/scroll-up-small)
          ("Q"  scroll-down)
          ("W"  scroll-up)

          ("a" move-beginning-of-line)
          ("s" move-end-of-line)
          ("m" back-to-indentation)

          ("z" beginning-of-buffer)
          ("x" end-of-buffer)

          ("e" move-to-window-line-top-bottom)
          ("E" dhnam/reverse-move-to-window-line-top-bottom)
          ("d" recenter-top-bottom)
          ("D" dhnam/reverse-recenter-top-bottom)

          ("C-SPC" set-mark-command)
          ("C-@" set-mark-command)
          ("r" set-mark-command)
          ("'" exchange-point-and-mark)

          ;; ("/" undo)
          ("C-/" undo)
          ("SPC" nil "quit")

          ("#" eval-last-sexp-or-region)
          ("$" eval-print-last-sexp))

        (progn
          ;; Disable any hint message
          (hydra-set-property 'dhnam-iokl :verbosity 0))

        (define-key global-map (kbd ,dhnam-iokl/activation-key) 'dhnam-iokl/body))

      (when (package-installed-p 'paredit)
        (require 'dhnam-paredit)

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

          ;; ("/" undo)
          ("C-/" undo)

          (,dhnam-iokl/quit-key nil "quit")
          ;; (,dhnam-iokl/quit-key dhnam-iokl-paredit-move/body :exit t)
          ("SPC" nil "quit"))

        (clone-hydra dhnam-iokl-paredit-move dhnam-iokl
          ,dhnam-iokl/plist-1

          "iok"

          ("f" dhnam-iokl-paredit-struct/body  :exit t))

        (hydra-set-property 'dhnam-iokl-paredit-struct :verbosity 0) ; disable any hint message
        (hydra-set-property 'dhnam-iokl-paredit-move :verbosity 0) ; disable any hint message
        (define-key paredit-mode-map (kbd ,dhnam-iokl/activation-key) 'dhnam-iokl-paredit-move/body))

      (when (package-installed-p 'puni)
        (require 'dhnam-puni)

        (defhydra dhnam-iokl-puni-struct
          ,dhnam-iokl/plist-2

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

          (,dhnam-iokl/quit-key nil "quit")
          ;; (,dhnam-iokl/quit-key dhnam-iokl-puni-move/body :exit t)
          ("SPC" nil "quit"))

        (clone-hydra dhnam-iokl-puni-move dhnam-iokl
          ,dhnam-iokl/plist-1

          "iok"

          ("K" puni-backward-sexp)
          ("L" puni-forward-sexp)

          ("f" dhnam-iokl-puni-struct/body  :exit t))

        (hydra-set-property 'dhnam-iokl-puni-struct :verbosity 0) ; disable any hint message
        (hydra-set-property 'dhnam-iokl-puni-move :verbosity 0) ; disable any hint message
        (define-key puni-mode-map (kbd ,dhnam-iokl/activation-key) 'dhnam-iokl-puni-move/body))

      (with-eval-after-load 'vterm-seamless
        (clone-hydra dhnam-iokl-vterm-seamless-copy dhnam-iokl
          ,dhnam-iokl/plist-1

          "iok"

          ("S" vtsl/end-of-buffer)
          ("SPC" (vterm-copy-mode 1) :exit t)
          ;; (,dhnam-iokl/quit-key vtsl/copy-mode-exit :exit t)
          (,dhnam-iokl/quit-key nil "quit") ; it defines `dhnam-iokl-vterm-seamless-copy/nil'
          ("RET" vtsl/copy-mode-exit :exit t)
          ("<return>" vtsl/copy-mode-exit :exit t))

        (progn
          (defun dhnam-iokl-vterm-seamless-copy/copy-mode-exit ()
            (interactive)
            (when (and vterm-copy-mode
                       (= (line-number-at-pos (point)) vtsl/last-cursor-line-num))
              (dhnam-iokl-vterm-seamless-copy/nil)
              (dhnam/set-cursor-color dhnam-iokl/default-cursor-color)))

          (advice-add 'vtsl/trigger-copy-mode-exit :before 'dhnam-iokl-vterm-seamless-copy/copy-mode-exit))

        (defun dhnam-iokl-vterm-seamless-copy/body-after-previous-line ()
          (interactive)
          (previous-line)
          (dhnam-iokl-vterm-seamless-copy/body))

        (progn
          ;; make vtsl/copy-mode-then-dhnam-iokl-vterm-seamless-copy/body-after-previous-line
          (vtsl/copy-mode-then 'dhnam-iokl-vterm-seamless-copy/body-after-previous-line)
          (assert (fboundp 'vtsl/copy-mode-then-dhnam-iokl-vterm-seamless-copy/body-after-previous-line)))

        (defhydra dhnam-iokl-vterm-seamless
          ,dhnam-iokl/plist-1

          "iok"

          ("k" vtsl/copy-mode-then-dhnam-iokl-vterm-seamless-copy/body-after-previous-line :exit t)
          ("j" (vterm-send-key "b" nil t))
          (";" (vterm-send-key "f" nil t))
          ("i" (vterm-send-key "b" nil nil t))
          ("o" (vterm-send-key "f" nil nil t))

          ("q" dhnam/scroll-down-small)
          ("w" dhnam/scroll-up-small)
          ("a" (vterm-send-key "a" nil nil t))
          ("s" (vterm-send-key "e" nil nil t)))

        (let ((map vterm-seamless-mode-map))
          (define-key map (kbd ,dhnam-iokl/activation-key) 'dhnam-iokl-vterm-seamless/body))

        (let ((map vterm-seamless-copy-mode-map))
          (define-key map (kbd ,dhnam-iokl/activation-key) 'dhnam-iokl-vterm-seamless-copy/body))

        (let ((map vterm-seamless-copy-mode-map))
          (comment))

        ;; disable any hint message
        (hydra-set-property 'dhnam-iokl-vterm-seamless-copy :verbosity 0)
        (hydra-set-property 'dhnam-iokl-vterm-seamless :verbosity 0))

      (with-eval-after-load 'ivy
        (clone-hydra dhnam-iokl-ivy dhnam-iokl
          ,dhnam-iokl/plist-1

          "iok"

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
        (hydra-set-property 'dhnam-iokl-ivy :verbosity 0)))))
 

(provide 'dhnam-iokl)
