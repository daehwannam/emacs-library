
(defvar dhnam/cl-debug-declaim-statement "(declaim (optimize (debug 3) (speed 0)))")
(defun dhnam/insert-cl-debug-declaim-statement ()
  (interactive)
  (insert dhnam/cl-debug-declaim-statement))

(defun dhnam/slime-eval-cl-debug-declaim-statement ()
  (interactive)
  (slime-interactive-eval dhnam/cl-debug-declaim-statement))

(progn
  ;; Hyperspec

  (comment (defvar dhnam/hyperspec-lookup-func #'slime-hyperspec-lookup))
  (defvar dhnam/hyperspec-lookup-func #'sly-hyperspec-lookup)
  (defvar dhnam/hyperspec-lookup-key (kbd "C-c H"))

  (defun dhnam/set-eww-hyperspec-keys ()
    (dhnam/buffer-local-set-key dhnam/hyperspec-lookup-key dhnam/hyperspec-lookup-func)
    (remove-hook 'eww-after-render-hook 'dhnam/set-eww-hyperspec-keys))

  (defun dhnam/eww-new-hyperspec (url &optional arg)
    (add-hook 'eww-after-render-hook 'dhnam/set-eww-hyperspec-keys)
    (dhnam/eww-new url arg))

  (defun dhnam/hyperspec-lookup-with-eww-advice (orig-fun &rest args)
    ;; https://emacs.stackexchange.com/a/62549
    (setq-local browse-url-browser-function 'dhnam/eww-new-hyperspec)
    (apply orig-fun args))

  (comment
    ;; Example
    (setq dhnam/hyperspec-lookup-key (kbd "C-c H"))
    (advice-add 'hyperspec-lookup
                :around 'dhnam/hyperspec-lookup-with-eww-advice)
    (advice-add 'slime-hyperspec-lookup
                :around 'dhnam/hyperspec-lookup-with-eww-advice)
    (advice-add 'sly-hyperspec-lookup
                :around 'dhnam/hyperspec-lookup-with-eww-advice)))

(progn
  ;; SLIME

  (defun dhnam/slime-eval-last-expression-or-region ()
    "Evaluate the expression preceding point or the currently activated region."
    (interactive)
    (if (use-region-p)
        (progn
          (slime-eval-region (region-beginning) (region-end)))
      (slime-eval-last-expression))))

(progn
  ;; Sly

  (defun dhnam/sly-eval-last-expression-or-region ()
    "Evaluate the expression preceding point or the currently activated region."
    (interactive)
    (if (use-region-p)
        (progn
          (sly-eval-region (region-beginning) (region-end)))
      (sly-eval-last-expression)))

  (progn
    (defvar dhnam/sly-paredit-mode-map
      (let ((map (make-sparse-keymap)))
        (define-key map (kbd "RET") 'sly-mrepl-return)
        (define-key map (kbd "C-c C-o") 'sly-mrepl-clear-recent-output)
        (define-key map (kbd "C-c M-p") 'sly-button-backward)
        (define-key map (kbd "C-c M-n") 'sly-button-forward)
        map))

    (define-minor-mode dhnam/sly-paredit-mode
      "A minor mode of Sly when Paredit is used together."
      :lighter " sly-paredit"
      :keymap dhnam/sly-paredit-mode-map
      (if dhnam/sly-paredit-mode
          (comment "Enabled")
        (comment "Disabled")))

    (comment
      (add-hook 'sly-mrepl-mode-hook 'dhnam/sly-paredit-mode))))


(provide 'dhnam-lisp)
