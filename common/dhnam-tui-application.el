(require 'dhnam-vterm)


(progn
  (defun dhnam/vterm-nmtui ()
    (interactive)
    (dhnam/vterm-command "nmtui"))

  (defvar dhnam/vterm-nmtui-mode-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "C-g") #'vterm-send-escape)

      map)
    "Keymap for `dhnam/vterm-nmtui-mode'.")

  (define-minor-mode dhnam/vterm-nmtui-mode
    "vterm-mode extension"
    nil                          ; Initial value, nil for disabled
    :global nil
    :lighter " nmtui"
    :keymap dhnam/vterm-nmtui-mode-map)

  (defun dhnam/vterm-nmtui-advice (orig-fun &rest args)
    (let ((buf (apply orig-fun args)))
      (with-current-buffer buf
        (dhnam/vterm-nmtui-mode 1))
      buf))

  (advice-add 'dhnam/vterm-nmtui
              :around 'dhnam/vterm-nmtui-advice))


(provide 'dhnam-tui-application)
