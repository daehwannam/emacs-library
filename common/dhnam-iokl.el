
(require 'dhnam-macro)
(require 'dhnam-hydra)

(unless (fboundp 'comment)
  (defmacro comment (&rest args)
    `nil))

(defvar dhnam-iokl/default-cursor-color "orchid")
(defvar dhnam-iokl/activated-cursor-color "cyan")
(defvar dhnam-iokl-paredit-struct-cursor-color "orange")

(defconst dhnam-iokl/activation-key "₢")
(defconst dhnam-iokl/quit-key "₫")
(defconst dhnam-avy-key "₣")

(defun dhnam-iokl/set-cursor-color (color)
  (if (display-graphic-p)
      (set-cursor-color color)
    (send-string-to-terminal (format "\033]12;%s\007" color))))

(dhnam-iokl/set-cursor-color dhnam-iokl/default-cursor-color)

(defconst dhnam-iokl/plist-1
  '(:pre (dhnam-iokl/set-cursor-color dhnam-iokl/activated-cursor-color)
    :post (dhnam-iokl/set-cursor-color dhnam-iokl/default-cursor-color)))

(defconst dhnam-iokl/plist-2
  '(:pre (dhnam-iokl/set-cursor-color dhnam-iokl-paredit-struct-cursor-color)
    :post (dhnam-iokl/set-cursor-color dhnam-iokl/default-cursor-color)))

(eval
 `(progn
    (progn
      ;; dhnam-iokl
      (defhydra dhnam-iokl
        ,dhnam-iokl/plist-1

        "iokl"

        ("i" previous-line)
        ("o" next-line)
        ("k" backward-char)
        ("l" forward-char)
        ("j" backward-word)
        (";" forward-word)

        ("K" backward-sexp)
        ("L" forward-sexp)
        ("J" backward-list)
        (":" forward-list)

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
        ("A" back-to-indentation)

        ("z" beginning-of-buffer)
        ("x" end-of-buffer)

        ("e" move-to-window-line-top-bottom)
        ("E" dhnam/reverse-move-to-window-line-top-bottom)
        ("d" recenter-top-bottom)
        ("D" dhnam/reverse-recenter-top-bottom)

        ("C-SPC" set-mark-command)
        ("C-@" set-mark-command)
        ("'" exchange-point-and-mark)

        ;; ("/" undo)
        ("C-/" undo)
        ("SPC" nil "quit"))

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

        "iokl"

        ("f" dhnam-iokl-paredit-struct/body  :exit t))

      (hydra-set-property 'dhnam-iokl-paredit-struct :verbosity 0) ; disable any hint message
      (hydra-set-property 'dhnam-iokl-paredit-move :verbosity 0) ; disable any hint message
      (define-key paredit-mode-map (kbd ,dhnam-iokl/activation-key) 'dhnam-iokl-paredit-move/body))

    (with-eval-after-load 'vterm-seamless
      (clone-hydra dhnam-iokl-vterm dhnam-iokl
        ,dhnam-iokl/plist-1

        "iokl"

        ("S" vtsl/end-of-buffer)
        ("SPC" (vterm-copy-mode 1) :exit t)
        ;; (,dhnam-iokl/quit-key vtsl/copy-mode-exit :exit t)
        (,dhnam-iokl/quit-key nil "quit")  ; it defines `dhnam-iokl-vterm/nil'
        ("RET" vtsl/copy-mode-exit :exit t)
        ("<return>" vtsl/copy-mode-exit :exit t))

      (progn
        (defun dhnam-iokl-vterm/copy-mode-exit ()
          (interactive)
          (when (and vterm-copy-mode
                     (= (line-number-at-pos (point)) vtsl/last-cursor-line-num))
            (dhnam-iokl-vterm/nil)
            (dhnam-iokl/set-cursor-color dhnam-iokl/default-cursor-color)))

        (advice-add 'vtsl/trigger-copy-mode-exit :before 'dhnam-iokl-vterm/copy-mode-exit))

      (defun dhnam-iokl-vterm/body-after-previous-line ()
        (interactive)
        (previous-line)
        (dhnam-iokl-vterm/body))

      (let ((map vterm-seamless-mode-map))
        (define-key map (kbd ,dhnam-iokl/activation-key) (vtsl/copy-mode-then 'dhnam-iokl-vterm/body-after-previous-line)))

      (let ((map vterm-seamless-copy-mode-map))
        (define-key map (kbd ,dhnam-iokl/activation-key) 'dhnam-iokl-vterm/body))

      (let ((map vterm-seamless-copy-mode-map))
        (comment))

      ;; disable any hint message
      (hydra-set-property 'dhnam-iokl-vterm :verbosity 0))))
 

(provide 'dhnam-iokl)
