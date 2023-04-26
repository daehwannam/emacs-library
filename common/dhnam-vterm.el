
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


(provide 'dhnam-vterm)
