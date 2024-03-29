
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

(progn
  (require 'cl-extra)
  (defmacro dhnam/with-eval-except-modes (excepted-modes &rest body)
    `(unless (cl-some (lambda (mode) (derived-mode-p mode)) ,excepted-modes)
       ,@body))

  (require 'cl-indent)

  (put 'dhnam/with-eval-except-modes 'lisp-indent-function
       (get 'with-eval-after-load 'lisp-indent-function))

  (defmacro dhnam/hook-except-modes (hook excepted-modes)
    `(lambda ()
       (dhnam/with-eval-except-modes ,excepted-modes
         (funcall ,hook)))))

(progn
  (require 'cl-macs)

  (defmacro dhnam/without-message (expr)
    "Disable `message'"
    `(cl-letf (((symbol-function 'message)
                (lambda (&rest args))))
       ,expr)))

(progn
  (defmacro dhnam/displaying-buffer-same-window (buffer-name &rest body)
    `(let ((display-buffer-alist
            (cons (list ,buffer-name 'display-buffer-same-window)
                  display-buffer-alist)))
       ,@body))

  (put 'dhnam/displaying-buffer-same-window 'lisp-indent-function
       (get 'when 'lisp-indent-function)))


(provide 'dhnam-macro)
