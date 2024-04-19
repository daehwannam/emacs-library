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

(defun dhnam-iokl/define-key-for-body (map body)
  (mapc (lambda (key) (define-key map key body))
        dhnam-iokl/keys-for-body))

(defvar dhnam-iokl/chords-for-body nil)

(defun dhnam-iokl/define-chord-for-body (map body)
  (mapc (lambda (key) (key-chord-define map key body))
        dhnam-iokl/chords-for-body))

(defmacro dhnam-iokl/make-define-keys-chords-for-bodies
    (name define-key-or-chord-for-body keys-or-chords-for-body)
  `(defun ,name (&rest keys-or-chords)
     (mapc (lambda (key-or-chord) (add-to-list ',keys-or-chords-for-body key-or-chord))
           keys-or-chords)

     (comment (,define-key-or-chord-for-body global-map 'dhnam-iokl-base/body))
     (,define-key-or-chord-for-body global-map 'dhnam-iokl-puni-nav/body)
     (with-eval-after-load 'compile
       (,define-key-or-chord-for-body compilation-shell-minor-mode-map 'dhnam-iokl-compilation/body))
     (,define-key-or-chord-for-body org-mode-map 'dhnam-iokl-org/body)
     (,define-key-or-chord-for-body paredit-mode-map 'dhnam-iokl-paredit-nav/body)
     (with-eval-after-load 'python
       (,define-key-or-chord-for-body python-mode-map 'dhnam-iokl-python-nav/body))
     (with-eval-after-load 'vterm-seamless
       (,define-key-or-chord-for-body vterm-seamless-mode-map nil))
     ;; (with-eval-after-load 'csv-mode
     ;;   (,define-key-or-chord-for-body csv-mode-map 'dhnam-iokl-paredit-nav/body)
     ;;   (,define-key-or-chord-for-body tsv-mode-map 'dhnam-iokl-paredit-nav/body))
     ))

(dhnam-iokl/make-define-keys-chords-for-bodies
 dhnam-iokl/define-keys-for-bodies
 dhnam-iokl/define-key-for-body
 dhnam-iokl/keys-for-body)

(dhnam-iokl/make-define-keys-chords-for-bodies
 dhnam-iokl/define-chords-for-bodies
 dhnam-iokl/define-chord-for-body
 dhnam-iokl/chords-for-body)


(provide 'dhnam-iokl)
