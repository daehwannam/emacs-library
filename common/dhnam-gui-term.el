(progn
  ;; https://gist.github.com/dfeich/50ee86c3d4338dbc878b

  (defvar dhnam/gui-terminal-command "kitty")
  (defvar dhnam/gui-terminal-command-arg "-e" )

  (defun dhnam/gui-terminal (&optional path)
    "Opens a gnome terminal at PATH. If no PATH is given, it uses
the value of `default-directory'. PATH may be a tramp remote path."
    (interactive)
    (unless path (setq path default-directory))
    (if (tramp-tramp-file-p path)
        (let ((tstruct (tramp-dissect-file-name path)))
	      (cond
	       ((equal (tramp-file-name-method tstruct) "ssh")
	        (start-process "terminal" nil
                           dhnam/gui-terminal-command
                           dhnam/gui-terminal-command-arg
			               "--" "bash" "-c"
			               (format "ssh -t %s@%s -p %s 'cd %s; exec bash'; exec bash"
                                   (tramp-file-name-user-domain tstruct)
				                   (tramp-file-name-host tstruct)
                                   (or (tramp-file-name-port-or-default tstruct) 22)
				                   (tramp-file-name-localname tstruct))))
	       (t (error "not implemented for method%s"
		             (tramp-file-name-method tstruct)))))
      (start-process "terminal" nil dhnam/gui-terminal-command))))


(provide 'dhnam-gui-term)
