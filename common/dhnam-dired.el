
(progn
  ;; Copy files asynchronously
  ;;
  ;; https://stackoverflow.com/questions/379940/dired-copy-asynchronously
  ;; https://oremacs.com/2016/02/24/dired-rsync/

  (require 'dired-x)

  (defun dhnam/run-dired-rsync (dest-path &optional max-size)
    (let ((default-args (list "-rlpt"
                              "--info=progress2" "--no-i-r"))
          (marked-files (dired-get-marked-files
                         nil current-prefix-arg))
          (ssh-port-arg nil)
          (max-size-arg nil)
          (source-args nil)
          (dest-arg nil))

      (setq source-args (mapcar 'shell-quote-argument marked-files))

      (if (tramp-tramp-file-p dest-path)
          (let ((tstruct (tramp-dissect-file-name dest-path)))
            (let ((port (or (tramp-file-name-port-or-default tstruct) 22)))
              (setq ssh-port-arg (format "-e 'ssh -p %s'" port)))

            (let ((user-name (tramp-file-name-user-domain tstruct))
                  (host-address (tramp-file-name-host tstruct))
                  (local-path (tramp-file-name-localname tstruct)))

              (setq dest-arg (format "%s@%s:%s" user-name host-address local-path))))
        (setq dest-arg (shell-quote-argument dest-path)))

      (when max-size
        (setq max-size-arg (format "--max-size=%s" max-size)))

      (async-shell-command
       (s-join " " (append (list "rsync")
                           default-args
                           (list ssh-port-arg)
                           (list max-size-arg)
                           source-args
                           (list dest-arg)))
       "*rsync*")

      ;; switch to the window of *rsync*
      (other-window 1)))

  (defun dhnam/dired-rsync (dest-path)
    (interactive
     (list
      (expand-file-name
       (read-file-name
	    "Rsync to: "
	    (dired-dwim-target-directory)))))

    (dhnam/run-dired-rsync dest-path))

  (defvar dhnam/dired-rsync-max-size-history nil)

  (defun dhnam/dired-rsync-with-max-size (dest-path max-size)
    ;; e.g.
    ;; - dest-path: /ssh:user@address.com#22:/path/to/dir
    ;; - max-size: 500k, 100m, 2g, ...
    (interactive
     (list
      (expand-file-name
       (read-file-name
	    "Rsync to: "
	    (dired-dwim-target-directory)))
      (read-string "Max size: " nil dhnam/dired-rsync-max-size-history)))

    (dhnam/run-dired-rsync dest-path max-size)))

(provide 'dhnam-dired)
