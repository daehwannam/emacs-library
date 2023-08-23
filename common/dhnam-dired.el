
(progn
  ;; Copy files asynchronously and remotely via rsync
  ;;
  ;; Reference:
  ;; - https://stackoverflow.com/questions/379940/dired-copy-asynchronously
  ;; - https://oremacs.com/2016/02/24/dired-rsync/

  (progn
    ;; Required packages

    (require 'tramp)
    (require 'dired-x)
    (require 'dired-aux)

    ;; s.el
    ;; https://github.com/magnars/s.el
    (require 's))

  (defvar dhnam/dired-rsync/default-args
    (list "-rlptP"
          "--info=progress2"
          ;; "--no-i-r"
          ))

  (defvar dhnam/dired-rsync/displaying-in-current-window t)

  (defvar dhnam/dired-rsync/buffer-name "*rsync*")

  (defun dhnam/dired-rsync/run (dest-path &optional max-size)
    "Run rsync to copy marked files to DEST-PATH.
The rsync command can transfer files in the directions of
local-to-local, local-to-remote, remote-to-local and remote-to-remote.
This function would not work for multi-hop SSH connections.
"
    (let ((default-args dhnam/dired-rsync/default-args)
          (marked-files (dired-get-marked-files
                         nil current-prefix-arg))

          (source-ssh-port-arg nil)
          (dest-ssh-port-arg nil)

          (dest-as-default-dir nil)

          (source-path-args nil)
          (dest-path-arg nil)

          (ssh-port-arg nil)
          (max-size-arg nil))

      (let ((source-info-list (mapcar 'dhnam/dired-rsync/extract-info-from-path marked-files))
            (dest-info (dhnam/dired-rsync/extract-info-from-path dest-path)))

        (cl-flet ((assocv (key alist) (cdr (assoc key alist))))
          (let ((source-hosts (mapcar (lambda (x) (assocv 'host-address x)) source-info-list))
                (dest-host (assocv 'host-address dest-info)))
            (if (cl-every (lambda (source-host) (equal source-host dest-host)) source-hosts)
                (progn
                  ;; Both source and destination are in the same machine, then ssh is unnecessary
                  (setq source-path-args (mapcar (lambda (x) (assocv 'quoted-local-path x)) source-info-list))
                  (setq dest-path-arg (assocv 'quoted-local-path dest-info)))
              (progn
                (progn
                  (setq dest-path-arg (assocv 'quoted-full-path dest-info))
                  (setq dest-ssh-port-arg (assocv 'ssh-port-arg dest-info)))

                (progn
                  (setq source-ssh-port-arg (assocv 'ssh-port-arg (car source-info-list)))
                  (let ((source-path-args-key 'quoted-full-path))
                    (if dest-ssh-port-arg
                        (progn
                          (setq ssh-port-arg dest-ssh-port-arg)
                          (when source-ssh-port-arg
                            (setq source-path-args-key 'quoted-local-path)))
                      (when source-ssh-port-arg
                        (setq ssh-port-arg source-ssh-port-arg)
                        (setq dest-as-default-dir t)))
                    (setq source-path-args (mapcar (lambda (x) (assocv source-path-args-key x)) source-info-list)))))))))

      (when max-size
        (setq max-size-arg (format "--max-size=%s" max-size)))

      (let ((default-directory (if dest-as-default-dir dest-path default-directory)))
        (let ((rsync-command (s-join " " (append (list "rsync")
                                                 default-args
                                                 (list ssh-port-arg)
                                                 (list max-size-arg)
                                                 source-path-args
                                                 (list dest-path-arg)))))
          (cl-flet ((run-rsync-command () (async-shell-command rsync-command dhnam/dired-rsync/buffer-name)))
            (if dhnam/dired-rsync/displaying-in-current-window
                (let ((display-buffer-alist
                       (cons (list dhnam/dired-rsync/buffer-name 'display-buffer-same-window)
                             display-buffer-alist)))
                  (run-rsync-command))
              (progn
                (run-rsync-command)
                ;; switch to the window of the command buffer
                (other-window 1))))))))

  (defun dhnam/dired-rsync/extract-info-from-path (path)
    (if (tramp-tramp-file-p path)
        (let ((tstruct (tramp-dissect-file-name path)))
          (let ((port (or (tramp-file-name-port-or-default tstruct) 22)))
            (setq ssh-port-arg (format "-e 'ssh -p %s'" port)))

          (let ((user-name (tramp-file-name-user-domain tstruct))
                (host-address (tramp-file-name-host tstruct))
                (local-path (tramp-file-name-localname tstruct)))

            (let ((quoted-local-path (shell-quote-argument local-path)))
              (list
               (cons 'quoted-full-path (format "%s@%s:%s" user-name host-address quoted-local-path))
               (cons 'host-address host-address)
               (cons 'quoted-local-path quoted-local-path)
               (cons 'ssh-port-arg ssh-port-arg)))))
      (let ((quoted-local-path (shell-quote-argument path)))
        (list (cons 'quoted-full-path quoted-local-path)
              (cons 'quoted-local-path quoted-local-path)))))

  (defun dhnam/dired-rsync/do (dest-path)
    "Do rsync to copy marked files to DEST-PATH."

    (interactive
     (list
      (expand-file-name
       (read-file-name
	    "Rsync to: "
	    (dired-dwim-target-directory)))))

    (dhnam/dired-rsync/run dest-path))

  (defvar dhnam/dired-rsync/max-size-history nil)

  (defun dhnam/dired-rsync/do-with-max-size (dest-path max-size)
    ;; e.g.
    ;; - dest-path: /ssh:user@address.com#22:/path/to/dir
    ;; - max-size: 500k, 100m, 2g, ...
    (interactive
     (list
      (expand-file-name
       (read-file-name
	    "Rsync to: "
	    (dired-dwim-target-directory)))
      (read-string "Max size: " nil 'dhnam/dired-rsync/max-size-history)))

    (dhnam/dired-rsync/run dest-path max-size)))

(provide 'dhnam-dired)
