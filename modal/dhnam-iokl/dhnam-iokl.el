(require 'dhnam-macro)
(require 'dhnam-hydra)

(defvar dhnam-iokl/default-cursor-color "orchid")
(defvar dhnam-iokl/activated-cursor-color "cyan")
(defvar dhnam-iokl-paredit-struct-cursor-color "orange")

(defvar dhnam-iokl/activation-key "₢")
(defvar dhnam-iokl/quit-key "₫")
(defvar dhnam-avy-key "₣")

(dhnam/set-cursor-color dhnam-iokl/default-cursor-color)

(defconst dhnam-iokl/plist-1
  '(:pre (dhnam/set-cursor-color dhnam-iokl/activated-cursor-color)
    :post (dhnam/set-cursor-color dhnam-iokl/default-cursor-color)))

(defconst dhnam-iokl/plist-2
  '(:pre (dhnam/set-cursor-color dhnam-iokl-paredit-struct-cursor-color)
    :post (dhnam/set-cursor-color dhnam-iokl/default-cursor-color)))


(require 'dhnam-iokl-base)
(require 'dhnam-iokl-paredit)
(require 'dhnam-iokl-puni)
(require 'dhnam-iokl-python)
(require 'dhnam-iokl-vterm-seamless)
(require 'dhnam-iokl-org)
(require 'dhnam-iokl-compilation)

(define-key global-map (kbd dhnam-iokl/activation-key) 'dhnam-iokl-puni-nav/body)
(setq dhnam-iokl-puni-nav/prohibited-major-modes '(python-mode))

(provide 'dhnam-iokl)
