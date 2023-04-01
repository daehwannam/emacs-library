(when (package-installed-p 'realgud)
  (defvar dhnam/realgud-loaded nil)
  (unless dhnam/realgud-loaded
    (load-library "realgud")
    (setq dhnam/realgud-loaded t))

  (comment (add-hook 'shell-mode-hook 'realgud-track-mode))
  (comment (remove-hook 'shell-mode-hook 'dhnam/pdbtrace-shell-mode-hook))
  (setq realgud-safe-mode nil)
  (with-eval-after-load 'comint
    (key-chord-define comint-mode-map "qp" 'realgud-track-mode))

  (progn
    (defun dhnam/realgud-populate-common-keys-advice (orig-fun &rest args)
      (cl-destructuring-bind (map) args
        (key-chord-define map "qp" 'realgud-short-key-mode))
      (apply orig-fun args))

    (advice-add 'realgud-populate-common-keys :around #'dhnam/realgud-populate-common-keys-advice))

  (progn
    (defun dhnam/realgud:cmd-eval-region-advice (orig-fun &rest args)
      (apply orig-fun args)
      (deactivate-mark))

    (advice-add 'realgud:cmd-eval-region :around #'dhnam/realgud:cmd-eval-region-advice)
    (comment (advice-add 'realgud:cmd-eval-dwim :around #'dhnam/realgud:cmd-eval-region-advice)))

  (progn
    (defun dhnam/realgud:cmd-eval-at-point-advice (orig-fun &rest args)
      (apply orig-fun args)
      (deactivate-mark))

    (advice-add 'realgud:cmd-eval-at-point :around #'dhnam/realgud:cmd-eval-at-point-advice))

  (progn
    (defvar dhnam/realgud:debugger-history nil)
    (defun dhnam/realgud:track-set-debugger-advice (orig-fun &rest args)
      (interactive
       (list (completing-read "Debugger name: " realgud-pat-hash nil t nil 'dhnam/realgud:debugger-history)))
      (apply orig-fun args))

    (advice-add 'realgud:track-set-debugger :around #'dhnam/realgud:track-set-debugger-advice))

  (progn
    (defun dhnam/realgud:cmd-eval-dwim()
      "Eval the current region if active; otherwise, eval the symbol at point."
      (interactive)
      (call-interactively (if (region-active-p)
                              #'realgud:cmd-eval-region
                            #'dhnam/realgud:cmd-eval-at-point-directly)))

    (defun dhnam/realgud:cmd-eval-at-point-directly()
      "Eval symbol under point."
      (interactive)
      (realgud:cmd-run-command (thing-at-point 'symbol) "eval"))

    (define-key realgud:shortkey-mode-map "e" 'dhnam/realgud:cmd-eval-dwim)
    (define-key realgud:shortkey-mode-map "E" 'realgud:cmd-eval))

  (progn
    (defun dhnam/realgud:cmd-return()
      "Continue execution until the current function returns"
      (interactive)
      (if (realgud:prompt-if-safe-mode
	       "Continue until the function returns?")
          (realgud:cmd-run-command nil "return")))

    (defun dhnam/realgud:cmd-where()
      "Continue execution until the current function returns"
      (interactive)
      (realgud:cmd-run-command nil "where"))

    (let ((hash realgud-cmd:default-hash))
      (puthash "return" "return" hash)
      (puthash "where" "where" hash))

    (define-key realgud:shortkey-mode-map "r" 'dhnam/realgud:cmd-return)
    (define-key realgud:shortkey-mode-map "w" 'dhnam/realgud:cmd-where)))

(provide 'dhnam-realgud)
