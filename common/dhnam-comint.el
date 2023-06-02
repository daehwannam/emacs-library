
(defun dhnam/comint-with-command (command &optional buffer-name)
  "Run shell with COMMAND. It makes a temporary script to run the command."
  (interactive
   (list
    (read-shell-command (if shell-command-prompt-show-cwd
                            (format-message "Shell command in `%s': "
                                            (abbreviate-file-name
                                             default-directory))
                          "Shell command: ")
                        nil nil
			            (let ((filename
			                   (cond
				                (buffer-file-name)
				                ((eq major-mode 'dired-mode)
				                 (dired-get-filename nil t)))))
			              (and filename (file-relative-name filename))))))

  (let* ((mktemp-cmd (format "mktemp %s" (concat "/tmp/" "cmd-XXXXX")))
         (script-path (dhnam/string-trim (shell-command-to-string mktemp-cmd))))
    (with-temp-file script-path
      (dhnam/insert-line "#!/usr/bin/sh")
      (dhnam/insert-line (format "trap 'rm -f %s' EXIT" script-path))
      (dhnam/insert-line command))
    (shell-command (format "chmod +x %s" script-path))
    (let ((explicit-shell-file-name script-path)
          (buffer-name (or buffer-name (format "*%s*" command))))
      (make-comint-in-buffer buffer-name buffer-name explicit-shell-file-name)
      (pop-to-buffer buffer-name))))

(provide 'dhnam-comint)
