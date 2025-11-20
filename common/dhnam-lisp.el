
(progn
  (defun dhnam/set-eww-hyperspec-keys ()
    (dhnam/buffer-local-set-key (kbd "C-c H") #'slime-hyperspec-lookup)
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
    (advice-add 'hyperspec-lookup
                :around 'dhnam/hyperspec-lookup-with-eww-advice)
    (advice-add 'slime-hyperspec-lookup
                :around 'dhnam/hyperspec-lookup-with-eww-advice)))

(defvar dhnam/cl-debug-declaim-statement "(declaim (optimize (debug 3) (speed 0)))")
(defun dhnam/insert-cl-debug-declaim-statement ()
  (interactive)
  (insert dhnam/cl-debug-declaim-statement))

(defun dhnam/slime-eval-cl-debug-declaim-statement ()
  (interactive)
  (slime-interactive-eval dhnam/cl-debug-declaim-statement))

(defun dhnam/slime-eval-last-expression-or-region ()
  "Evaluate the expression preceding point or the currently activated region."
  (interactive)
  (if (use-region-p)
      (progn
        (slime-eval-region (region-beginning) (region-end)))
    (slime-eval-last-expression)))


(provide 'dhnam-lisp)
