
(unless (fboundp 'comment)
  (defmacro comment (&rest args)
    `nil))

(comment
  (require 'dhnam-paredit))

(defvar dhnam-ijkl-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-i") 'previous-line)
    (define-key map (kbd "M-k") 'next-line)
    (define-key map (kbd "M-j") 'backward-char)
    (define-key map (kbd "M-l") 'forward-char)
    (define-key map (kbd "M-u") 'backward-word)
    (define-key map (kbd "M-o") 'forward-word)

    (define-key map (kbd "C-M-i") 'backward-sexp)
    (define-key map (kbd "C-M-k") 'forward-sexp)
    (define-key map (kbd "C-M-j") 'backward-list)
    (define-key map (kbd "C-M-l") 'forward-list)
    (define-key map (kbd "C-M-u") 'backward-up-list)
    (define-key map (kbd "C-M-o") 'down-list)
    (comment (define-key map (kbd "C-M-u") 'dhnam/paredit-backward-up-or-down))
    (comment (define-key map (kbd "C-M-o") 'dhnam/paredit-forward-up-or-down))

    (define-key map (kbd "M-n") 'electric-newline-and-maybe-indent)
    (define-key map (kbd "C-M-n") 'default-indent-new-line)

    (define-key map (kbd "M-'") dhnam-ijkl/default-global-map)
    map)
  "Keymap for `dhnam-ijkl-mode'.")


(fset 'dhnam-ijkl/default-global-map global-map)


(define-minor-mode dhnam-ijkl-mode
  "IJKL key binding"
  nil                                ; Initial value, nil for disabled
  :global t
  :lighter " IJKL"
  :keymap dhnam-ijkl-mode-map

  (if dhnam-ijkl-mode
      (comment)
    (comment)))

(provide 'dhnam-ijkl)
