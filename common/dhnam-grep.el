
(defun dhnam/grep-file (command-args)
  ;; modified from `grep'
  (interactive
   (progn
     (grep-compute-defaults)
     (let ((default (grep-default-command)))
       (list (read-shell-command "Run grep (like this): "
                                 (if current-prefix-arg default "grep --color -nH -i -m 1 -e ")
                                 'grep-history
                                 (if current-prefix-arg nil default))))))

  ;; Setting process-setup-function makes exit-message-function work
  ;; even when async processes aren't supported.
  (compilation-start (if (and grep-use-null-device null-device)
			             (concat command-args " " null-device)
		               command-args)
		             'grep-mode))

(provide 'dhnam-grep)
