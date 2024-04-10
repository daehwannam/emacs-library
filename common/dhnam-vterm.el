
(progn
  ;; async-shell-command and signal example:
  ;; https://emacs.stackexchange.com/a/42174

  (defun dhnam/run-vterm-when-signal (process signal)
    (when (memq (process-status process) '(exit signal))
      (vterm)
      (shell-command-sentinel process signal)))


  (defun dhnam/vterm-setup ()
    (interactive)
    (let* ((output-buffer (generate-new-buffer "*Async shell command*"))
           (proc (progn
                   (dhnam/display-async-shell-command
                    (async-shell-command (concat "conda install -y -c anaconda cmake" " && "
                                                 "conda install -y -c conda-forge libtool" " && "
                                                 "conda install -y gxx_linux-64")
                                         output-buffer))
                   (get-buffer-process output-buffer))))
      (if (process-live-p proc)
          (set-process-sentinel proc #'dhnam/run-vterm-when-signal)
        (vterm)))))

(progn
  (defun dhnam/vterm-new-instance (arg)
    (interactive "P")
    (let ((default-directory (if arg
                                 (read-directory-name "Directory: ")
                               default-directory)))
      (vterm t)))

  (progn
    (require 'vterm-seamless)
    (comment (add-hook 'vterm-mode-hook 'vtsl/activate))

    (progn
      (defun dhnam/vterm-new-instance-advice (orig-fun &rest args)
        (let ((buf (apply orig-fun args)))
          (with-current-buffer buf
            (vtsl/activate))
          buf))

      (advice-add 'dhnam/vterm-new-instance
                  :around 'dhnam/vterm-new-instance-advice))))

(defun dhnam/vterm-insert-tty-fix-template ()
  ;; fix for vterm when opened via ssh-tramp
  ;; https://github.com/akermu/emacs-libvterm/issues/569
  ;; https://unix.stackexchange.com/questions/404173/shell-command-tmux-throws-cant-use-dev-tty-error/512979

  (interactive)
  (vterm-insert "( exec </dev/tty; exec <&1; )")
  (vterm-send-left))

(defun vtsl/vterm-send-ctrl-r ()
  "Send `C-r' to the libvterm."
  (interactive)
  (vterm-send-key "r" nil nil t))

(defun vtsl/vterm-send-ctrl-s ()
  "Send `C-s' to the libvterm."
  (interactive)
  (vterm-send-key "s" nil nil t))

(defun vtsl/vterm-send-ctrl-c ()
  "Send `C-c' to the libvterm."
  (interactive)
  (vterm-send-key "c" nil nil t))

(defun dhnam/vterm-yank (&optional arg)
  (interactive "P")
  (vterm-send-stop)
  (vterm-yank arg)
  (vterm-send-start))


(progn
  ;; Running a command in vterm.
  ;;
  ;; This code is copied from:
  ;; https://www.reddit.com/r/emacs/comments/ft84xy/run_shell_command_in_new_vterm/?rdt=55699

  (defun dhnam/vterm-kill-process-if-alive (process event)
    "A process sentinel. Kills PROCESS's buffer if it is live."
    (let ((b (process-buffer process)))
      (and (buffer-live-p b)
           (kill-buffer b))))

  (defun dhnam/vterm-command (command)
    "Execute string COMMAND in a new vterm.

Interactively, prompt for COMMAND with the current buffer's file
name supplied. When called from Dired, supply the name of the
file at point.

Like `async-shell-command`, but run in a vterm for full terminal features.

The new vterm buffer is named in the form `*foo bar.baz*`, the
command and its arguments in earmuffs.

When the command terminates, the shell remains open, but when the
shell exits, the buffer is killed."
    (interactive
     (list
      (let* ((f (cond (buffer-file-name)
                      ((eq major-mode 'dired-mode)
                       (dired-get-filename nil t))))
             (filename (concat " " (shell-quote-argument (and f (file-relative-name f))))))
        (read-shell-command "Terminal command: "
                            nil
                            (cons 'shell-command-history 1)
                            (list filename)))))
    (let ((buffer (vterm (concat "*" command "*"))))
      (with-current-buffer buffer
        (set-process-sentinel vterm--process #'dhnam/vterm-kill-process-if-alive)
        (let ((extended-command (concat command " && exit")))
          (vterm-send-string extended-command))
        (vterm-send-return))
      buffer)))

(provide 'dhnam-vterm)
