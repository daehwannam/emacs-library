
(defun cmd-line-arg/empyt-handler (switch))

(defun cmd-line-arg/removal-handler (cmd-line-arg-key)
  "Remove arguments of --key-binding-style"
  (let* ((remaining-cmd-args (cdr command-line-args))
         (matched-cmd-line-args (member cmd-line-arg-key remaining-cmd-args)))
    (when matched-cmd-line-args
      (let ((cmd-arg-value (cadr matched-cmd-line-args)))
        (setq command-line-args (remove cmd-arg-value (remove cmd-line-arg-key command-line-args)))))))

(defun cmd-line-arg/register (cmd-line-arg-key with-value)
  (add-to-list 'command-switch-alist `(,cmd-line-arg-key . cmd-line-arg/empyt-handler))
  (when with-value
    (add-to-list 'command-line-functions `(lambda () (cmd-line-arg/removal-handler ,cmd-line-arg-key)))))

(defun cmd-line-arg/passed-p (cmd-line-arg-key)
  (let ((remaining-cmd-args (cdr command-line-args)))
    ;; originally '(cdr command-line-args) is passed into `command-line-1'
    (if (member cmd-line-arg-key remaining-cmd-args) t nil)))

(defun cmd-line-arg/value (cmd-line-arg-key)
  (let* ((remaining-cmd-args (cdr command-line-args))
         (matched-cmd-line-args (member cmd-line-arg-key remaining-cmd-args)))
    (when matched-cmd-line-args
      (cadr matched-cmd-line-args))))

(defun cmd-line-arg/register-then-get (cmd-line-arg-key with-value)
  (cmd-line-arg/register cmd-line-arg-key with-value)
  (if with-value
      (cmd-line-arg/value cmd-line-arg-key)
    (cmd-line-arg/passed-p cmd-line-arg-key)))


(provide 'dhnam-cmd-line-arg)
