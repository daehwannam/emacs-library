
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
    (progn
      (dhnam/install-package-unless-installed 's)
      (require 's nil t)))

  (defvar dhnam/dired-rsync/default-args
    ;; "-rlptP"
    (list "-rlpt"
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
                (dhnam/displaying-buffer-same-window dhnam/dired-rsync/buffer-name
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

(progn
  (defun dhnam/dired-open-next ()
    (interactive)
    (let ((window (selected-window)))
	  (dired-next-line 1)
	  (dired-find-file-other-window)
	  (select-window window)))

  (defun dhnam/dired-open-prev ()
    (interactive)
    (let ((window (selected-window)))
	  (dired-previous-line 1)
	  (dired-find-file-other-window)
	  (select-window window))))

(progn
  ;; File path copy
  ;; https://stackoverflow.com/a/9414763

  (defun dhnam/get-current-file-path ()
    (if (equal major-mode 'dired-mode)
        default-directory
      (or (buffer-file-name) default-directory)))

  (defun dhnam/kill-path-to-clipboard (arg)
    "Copy the current buffer file name to the clipboard."
    (interactive "P")
    (let ((path (dhnam/get-current-file-path)))
      (when path
        (let ((dir-or-file-path (if arg (file-name-directory path) path)))
          (kill-new dir-or-file-path)
          (message "'%s'" dir-or-file-path)))))

  (defun dhnam/kill-file-name-to-clipboard ()
    "Copy the current buffer file name to the clipboard."
    (interactive)
    (let ((path (dhnam/get-current-file-path)))
      (when path
        (let* ((file-or-dir-name (file-name-nondirectory path))
               (dir-name ))
          (when (and (string= file-or-dir-name "")
                     (string-match "^\\(.*\\)/$" path))
            (setq file-or-dir-name (file-name-nondirectory (match-string 1 path))))
	      (kill-new (or file-or-dir-name dir-name))
	      (message "'%s'" (or file-or-dir-name dir-name))))))

  (defun dhnam/kill-buffer-name-to-clipboard ()
    "Copy the current buffer file name to the clipboard."
    (interactive)
    (let ((name (buffer-name)))
      (kill-new name)
      (message "'%s'" name)))

  (defun dhnam/kill-other-window-path-to-clipboard (count)
    "Copy the other window's path."
    (interactive "p")
    (let ((path (progn (other-window count)
		               (let ((path default-directory))
			             (other-window (- count))
			             path))))
      (when path
        (kill-new path)
        (message "'%s'" path)))))

(progn
  ;; Deletion with Trash
  (setq delete-by-moving-to-trash t)

  (comment
    (defun dhnam/toggle-delete-by-moving-to-trash ()
      (interactive)
      (setq delete-by-moving-to-trash (not delete-by-moving-to-trash))
      (if delete-by-moving-to-trash
	      (message "Trashing is activated")
        (message "Deleting is activated"))))

  (defun dhnam/dired-do-direct-delete (&optional arg)
    (interactive "P")
    (let ((delete-by-moving-to-trash nil))
      (dired-do-delete arg)))

  (defun dhnam/dired-do-direct-flagged-delete (&optional nomessage)
    (interactive)
    (let ((delete-by-moving-to-trash nil))
      (dired-do-flagged-delete nomessage)))

  (define-key dired-mode-map (kbd "C-c D") 'dhnam/dired-do-direct-delete)
  (define-key dired-mode-map (kbd "C-c X") 'dhnam/dired-do-direct-flagged-delete))

(defun dhnam/dired-find-actual-file ()
  "In Dired, visit the file or directory named on this line.
Open a buffer with the actual path rather than the path of symbolic link."
  (interactive)
  (let ((find-file-visit-truename t))
    (dired-find-file)))


(provide 'dhnam-dired)
