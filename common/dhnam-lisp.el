
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

(defun dhnam/insert-cl-debug-declaration ()
  (interactive)
  (insert "(declaim (optimize (debug 3) (speed 0)))"))


(provide 'dhnam-lisp)
