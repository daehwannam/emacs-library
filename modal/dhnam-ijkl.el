
(require 'dhnam-macro)
(require 'dhnam-hydra)

(unless (fboundp 'comment)
  (defmacro comment (&rest args)
    `nil))

(defvar dhnam-ijkl/default-cursor-color "orchid")
(defvar dhnam-ijkl/activated-cursor-color "cyan")
(defvar dhnam-ijkl-paredit-struct-cursor-color "orange")
(defvar dhnam-ijkl-paredit-struct-cursor-color "orange")

(defconst dhnam-ijkl/activation-key "₢")
(defconst dhnam-ijkl/quit-key "₫")
(defconst dhnam-avy-key "₣")

(defun dhnam-ijkl/set-cursor-color (color)
  (if (display-graphic-p)
      (set-cursor-color color)
    (send-string-to-terminal (format "\033]12;%s\007" color))))

(dhnam-ijkl/set-cursor-color dhnam-ijkl/default-cursor-color)

(defconst dhnam-ijkl/plist-1
  '(:pre (dhnam-ijkl/set-cursor-color dhnam-ijkl/activated-cursor-color)
    :post (dhnam-ijkl/set-cursor-color dhnam-ijkl/default-cursor-color)))

(defconst dhnam-ijkl/plist-2
  '(:pre (dhnam-ijkl/set-cursor-color dhnam-ijkl-paredit-struct-cursor-color)
    :post (dhnam-ijkl/set-cursor-color dhnam-ijkl/default-cursor-color)))

(eval
 `(progn
    (progn
      ;; dhnam-ijkl
      (defhydra dhnam-ijkl
        ,dhnam-ijkl/plist-1

        "ijkl"

        ("i" previous-line)
        ("k" next-line)
        ("j" backward-char)
        ("l" forward-char)
        ("u" backward-word)
        ("o" forward-word)

        ("I" backward-up-list)
        ("M-I" paredit-backward-down)
        ("M-k" paredit-forward-up)
        ("K" down-list)

        ("J" backward-sexp)
        ("L" forward-sexp)
        ("U" backward-list)
        ("O" forward-list)

        ("w"  dhnam/scroll-down-small)
        ("s"  dhnam/scroll-up-small)
        ("W"  scroll-down)
        ("S"  scroll-up)

        ("a" move-beginning-of-line)
        ("d" move-end-of-line)
        ("A" beginning-of-buffer)
        ("D" end-of-buffer)
        ("z" back-to-indentation)

        ("h" move-to-window-line-top-bottom)
        ("H" dhnam/reverse-move-to-window-line-top-bottom)
        ("f" recenter-top-bottom)
        ("F" dhnam/reverse-recenter-top-bottom)

        ("C-SPC" set-mark-command)
        ("C-@" set-mark-command)
        ("'" exchange-point-and-mark)

        ;; ("/" undo)
        ("C-/" undo)
        ("SPC" nil "quit"))

      (progn
        ;; Disable any hint message
        (hydra-set-property 'dhnam-ijkl :verbosity 0))

      (define-key global-map (kbd ,dhnam-ijkl/activation-key) 'dhnam-ijkl/body))

    (when (package-installed-p 'paredit)
      (require 'dhnam-paredit)

      (defhydra dhnam-ijkl-paredit-struct
        ,dhnam-ijkl/plist-2

        "paredit structure editing"

        ("i" paredit-split-sexp)
        ("k" paredit-join-sexps)
        ("I" paredit-raise-sexp)
        ("K" paredit-convolute-sexp)

        ("j" paredit-backward-slurp-sexp)
        ("J" paredit-backward-barf-sexp)
        ("l" paredit-forward-slurp-sexp)
        ("L" paredit-forward-barf-sexp)

        ("u" paredit-splice-sexp-killing-backward)
        ("o" paredit-splice-sexp-killing-forward)

        ;; ("/" undo)
        ("C-/" undo)

        (,dhnam-ijkl/quit-key nil "quit")
        ;; (,dhnam-ijkl/quit-key dhnam-ijkl-paredit-move/body :exit t)
        ("q" nil "quit"))

      (clone-hydra dhnam-ijkl-paredit-move dhnam-ijkl
        ,dhnam-ijkl/plist-1

        "ijkl"

        ("r" dhnam-ijkl-paredit-struct/body  :exit t))

      (hydra-set-property 'dhnam-ijkl-paredit-struct :verbosity 0) ; disable any hint message
      (hydra-set-property 'dhnam-ijkl-paredit-move :verbosity 0) ; disable any hint message
      (define-key paredit-mode-map (kbd ,dhnam-ijkl/activation-key) 'dhnam-ijkl-paredit-move/body))

    (with-eval-after-load 'vterm-seamless
      (clone-hydra dhnam-ijkl-vterm dhnam-ijkl
        ,dhnam-ijkl/plist-1

        "ijkl"

        ("S" vtsl/end-of-buffer)
        (,dhnam-ijkl/quit-key vtsl/copy-mode-exit :exit t))

      (defun dhnam-ijkl-vterm/body-after-previous-line ()
        (interactive)
        (previous-line)
        (dhnam-ijkl-vterm/body))

      (let ((map vterm-seamless-mode-map))
        (define-key map (kbd ,dhnam-ijkl/activation-key) (vtsl/copy-mode-then 'dhnam-ijkl-vterm/body-after-previous-line)))

      (let ((map vterm-seamless-copy-mode-map))
        (comment))

      ;; disable any hint message
      (hydra-set-property 'dhnam-ijkl-vterm :verbosity 0))))
 

(provide 'dhnam-ijkl)
