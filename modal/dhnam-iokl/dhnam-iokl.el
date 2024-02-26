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

(setq dhnam-iokl-puni-nav/prohibited-major-modes '(python-mode))
(defvar dhnam-iokl/keys-for-body nil)

(defun dhnam-iokl/define-key-for-body (key)
  (add-to-list 'dhnam-iokl/keys-for-body key)

  (comment (define-key global-map key 'dhnam-iokl-base/body))
  (define-key global-map key 'dhnam-iokl-puni-nav/body)

  (with-eval-after-load 'compile
    (mapc (lambda (key) (define-key compilation-shell-minor-mode-map key 'dhnam-iokl-compilation/body))
          dhnam-iokl/keys-for-body))
  (define-key org-mode-map key 'dhnam-iokl-org/body)
  (define-key paredit-mode-map key 'dhnam-iokl-paredit-nav/body)

  (with-eval-after-load 'python
    (mapc (lambda (key) (define-key python-mode-map key 'dhnam-iokl-python-nav/body))
          dhnam-iokl/keys-for-body))

  (with-eval-after-load 'vterm-seamless
    (mapc (lambda (key) (define-key vterm-seamless-mode-map key nil))
          dhnam-iokl/keys-for-body)))

(dhnam-iokl/define-key-for-body (kbd dhnam-iokl/activation-key))

(provide 'dhnam-iokl)
