
(defmacro comment (&rest args)
  `nil)

(defmacro assert (test-form)
  ;; https://emacs.stackexchange.com/a/22082
  `(when (not ,test-form)
     (error "Assertion failed: %s" (format "%s" ',test-form))))

(progn
  (defmacro dhnam/define-self-insert-commands-unless-bound (keymap &optional prefix)
    (append
     '(progn)
     (mapcar (lambda (ch)
	       `(unless (lookup-key ,keymap (kbd (char-to-string ,ch)))
		  (define-key ,keymap (kbd (char-to-string ,ch))
		    (lambda () (interactive) (insert (or ,prefix "") (char-to-string ,ch))))))
	     (append (number-sequence ?A ?Z) (number-sequence ?a ?z))))))

(defmacro defun-override (function-name &rest args)
  `(progn
     (assert (fboundp ',function-name))
     (defun ,function-name ,@args)))

(defmacro once-only (&rest args)
  "Evaluate expressions only once"
  (let ((evaluated-p (gensym)))
    `(progn
       (defvar ,evaluated-p nil)
       (unless ,evaluated-p
         (setq ,evaluated-p t)
         ,@args))))

(provide 'dhnam-macro)
