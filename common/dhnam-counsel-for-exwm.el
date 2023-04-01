(progn
  ;; additional functions for convenience
  (defun dhnam/counsel-switch-buffer-within-app ()
    "Switch to another buffer within application.
Display a preview of the selected ivy completion candidate buffer
in the current window."
    (interactive)
    (let ((ivy-update-fns-alist
           '((ivy-switch-buffer . counsel--switch-buffer-update-fn)))
          (ivy-unwind-fns-alist
           '((ivy-switch-buffer . counsel--switch-buffer-unwind))))
      (dhnam/ivy-switch-buffer-within-app)))

  (defun dhnam/ivy-switch-buffer-within-app ()
    "Switch to another buffer within application.."
    (interactive)
    (let ((ivy-ignore-buffers
           (if exwm-class-name ; (string-match-p dhnam/exwm-buffer-name-joint (buffer-name (current-buffer)))
               ivy-ignore-buffers
             (cons dhnam/exwm-buffer-name-joint ivy-ignore-buffers))))
      (ivy-read "Switch to buffer: " #'internal-complete-buffer
                :keymap ivy-switch-buffer-map
                :preselect (unless exwm-class-name (buffer-name (other-buffer (current-buffer))))
                :action #'ivy--switch-buffer-action
                :matcher #'ivy--switch-buffer-matcher
                :caller 'ivy-switch-buffer
                :initial-input (when exwm-class-name
                                 (concat (downcase exwm-class-name) dhnam/exwm-buffer-name-joint))))))

(progn
  ;; additional functions for convenience
  (defun dhnam/counsel-switch-buffer-from-current ()
    "Switch to another buffer within application.
Display a preview of the selected ivy completion candidate buffer
in the current window."
    (interactive)
    (let ((ivy-update-fns-alist
           '((ivy-switch-buffer . counsel--switch-buffer-update-fn)))
          (ivy-unwind-fns-alist
           '((ivy-switch-buffer . counsel--switch-buffer-unwind))))
      (dhnam/ivy-switch-buffer-from-current)))

  (defun dhnam/ivy-switch-buffer-from-current ()
    "Switch to another buffer within application.."
    (interactive)
    (let ((ivy-ignore-buffers
           (if exwm-class-name ; (string-match-p dhnam/exwm-buffer-name-joint (buffer-name (current-buffer)))
               ivy-ignore-buffers
             (cons dhnam/exwm-buffer-name-joint ivy-ignore-buffers))))
      (ivy-read "Switch to buffer: " #'internal-complete-buffer
                :keymap ivy-switch-buffer-map
                ;; :preselect (buffer-name (current-buffer))
                :action #'ivy--switch-buffer-action
                :matcher #'ivy--switch-buffer-matcher
                :caller 'ivy-switch-buffer))))

(progn
  ;; additional functions for convenience
  (defun dhnam/counsel-switch-buffer-excluding-exwm ()
    "Switch to another buffer within application.
Display a preview of the selected ivy completion candidate buffer
in the current window."
    (interactive)
    (let ((ivy-update-fns-alist
           '((ivy-switch-buffer . counsel--switch-buffer-update-fn)))
          (ivy-unwind-fns-alist
           '((ivy-switch-buffer . counsel--switch-buffer-unwind))))
      (dhnam/ivy-switch-buffer-excluding-exwm)))

  (defun dhnam/ivy-switch-buffer-excluding-exwm ()
    "Switch to another buffer within application.."
    (interactive)
    (let ((ivy-ignore-buffers
           (cons (buffer-name (current-buffer))
                 (cons dhnam/exwm-buffer-name-joint ivy-ignore-buffers))))
      (ivy-read "Switch to buffer: " #'internal-complete-buffer
                :keymap ivy-switch-buffer-map
                ;; :preselect (buffer-name (current-buffer))
                :action #'ivy--switch-buffer-action
                :matcher #'ivy--switch-buffer-matcher
                :caller 'ivy-switch-buffer))))

(with-eval-after-load 'ivy
  (defun dhnam/ivy-insert-current-exwm-class-name ()
    "Insert `exwm-class-name'"
    (interactive)
    (when dhnam/ivy-original-exwm-class-name
      (delete-minibuffer-contents)
      (insert (concat (downcase dhnam/ivy-original-exwm-class-name) dhnam/exwm-buffer-name-joint))))

  (progn
    (defun dhnam/ivy-switch-buffer-advice-for-exwm (orig-fun &rest args)
      (let ((dhnam/ivy-original-exwm-class-name exwm-class-name))
        (apply orig-fun args)))

    (advice-add 'ivy-switch-buffer :around #'dhnam/ivy-switch-buffer-advice-for-exwm)
    (advice-add 'dhnam/ivy-switch-buffer-within-app :around #'dhnam/ivy-switch-buffer-advice-for-exwm)
    (advice-add 'dhnam/ivy-switch-buffer-from-current :around #'dhnam/ivy-switch-buffer-advice-for-exwm)
    (advice-add 'dhnam/ivy-switch-buffer-excluding-exwm :around #'dhnam/ivy-switch-buffer-advice-for-exwm))

  (ivy-define-key ivy-minibuffer-map (kbd "s-j") 'dhnam/ivy-insert-current-exwm-class-name)
  (ivy-define-key ivy-minibuffer-map (kbd "C-s-j") 'dhnam/ivy-insert-current-exwm-class-name))

(progn
  ;; extended emacs commands for exwm

  (defun dhnam/counsel-find-file-in-downloads (&optional initial-input initial-directory)
    "Forward to `find-file'.
When INITIAL-INPUT is non-nil, use it in the minibuffer during completion."
    (interactive)
    (let ((default-directory "~/Downloads/"))
      (counsel-find-file initial-input initial-directory)))

  (progn
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "d") 'dhnam/counsel-find-file-in-downloads)
      (define-key map (kbd "w") 'dhnam/kill-gc)
      (define-key map (kbd "k") 'dhnam/ivy-kill-marked)
      (define-key map (kbd "s") 'dhnam/switch-to-scratch-buffer)
      (define-key map (kbd "q") 'dhnam/eww-new)

      (defvar dhnam/exwm-extended-emacs-command-prefix-map map
	    "Keymap for emacs related commands."))

    (fset 'dhnam/exwm-extended-emacs-command-prefix-map dhnam/exwm-extended-emacs-command-prefix-map)))

(provide 'dhnam-counsel-for-exwm)
