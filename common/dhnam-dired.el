
(progn
  ;; Copy files asynchronously
  ;;
  ;; https://stackoverflow.com/questions/379940/dired-copy-asynchronously
  ;; https://oremacs.com/2016/02/24/dired-rsync/

  (require 'dired-x)

  (defun dhnam/dired-rsync-run (dest-path &optional max-size)
    (let ((default-args (list "-rlpt"
                              "--info=progress2" "--no-i-r"))
          (marked-files (dired-get-marked-files
                         nil current-prefix-arg))
          (ssh-port-arg nil)
          (max-size-arg nil)
          (source-args nil)
          (dest-arg nil)
          (remote-source-p nil))

      (let ((source-info-list (mapcar 'dhnam/dired-rsync-extract-args-from-path marked-files))
            (dest-info (dhnam/dired-rsync-extract-args-from-path dest-path)))

        (let ((source-hosts (mapcar (lambda (x) (nth 1 x)) source-info-list))
              (dest-host (nth 1 dest-info)))
          (if (cl-every (lambda (source-host) (equal source-host dest-host)) source-hosts)
              (progn
                ;; Both source and destination in the same machine, so ssh is unnecessary
                (setq source-args (mapcar (lambda (x) (nth 2 x)) source-info-list))
                (setq dest-arg (nth 2 dest-info)))
            (progn
              (progn
                (setq source-args (mapcar 'car source-info-list))
                (setq ssh-port-arg (nth 3 (car source-info-list)))
                (when ssh-port-arg
                  (setq remote-source-p t)))

              (progn
                (setq dest-arg (car dest-info))
                (unless ssh-port-arg
                  (setq ssh-port-arg (nth 3 dest-info))))))))

      (when max-size
        (setq max-size-arg (format "--max-size=%s" max-size)))

      (let ((default-directory (if remote-source-p dest-path default-directory)))
        (async-shell-command
         (s-join " " (append (list "rsync")
                             default-args
                             (list ssh-port-arg)
                             (list max-size-arg)
                             source-args
                             (list dest-arg)))
         "*rsync*"))

      ;; switch to the window of *rsync*
      (other-window 1)))

  (defun dhnam/dired-rsync-extract-args-from-path (path)
    (if (tramp-tramp-file-p path)
        (let ((tstruct (tramp-dissect-file-name path)))
          (let ((port (or (tramp-file-name-port-or-default tstruct) 22)))
            (setq ssh-port-arg (format "-e 'ssh -p %s'" port)))

          (let ((user-name (tramp-file-name-user-domain tstruct))
                (host-address (tramp-file-name-host tstruct))
                (local-path (tramp-file-name-localname tstruct)))

            (let ((quoted-local-path (shell-quote-argument local-path)))
              (list (format "%s@%s:%s" user-name host-address quoted-local-path)
                    host-address
                    quoted-local-path
                    ssh-port-arg))))
      (let ((quoted-path (shell-quote-argument path)))
        (list quoted-path nil quoted-path nil))))

  (defun dhnam/dired-rsync (dest-path)
    (interactive
     (list
      (expand-file-name
       (read-file-name
	    "Rsync to: "
	    (dired-dwim-target-directory)))))

    (dhnam/dired-rsync-run dest-path))

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
      (read-string "Max size: " nil 'dhnam/dired-rsync-max-size-history)))

    (dhnam/dired-rsync-run dest-path max-size)))

(provide 'dhnam-dired)
