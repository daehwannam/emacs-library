
(provide 'dhnam-exwm)

(progn
  ;; interactive functions

  (defun dhnam/exwm-get-pid-of-buffer (buffer-or-name)
    (interactive "bBuffer name: ")
    (let* ((buf (or buffer-or-name (current-buffer)))
           (id (exwm--buffer->id (get-buffer buf)))) ; ID of X window being displayed
      (message
       (if id
           (slot-value (xcb:+request-unchecked+reply
                           exwm--connection
                           (make-instance 'xcb:ewmh:get-_NET_WM_PID :window id))
                       'value)
         (user-error "Target buffer %S is not an X window managed by EXWM!"
                     buf)))))

  (defun dhnam/exwm-other-workspace (count)
    (interactive "p")
    (exwm-workspace-switch (% (+ exwm-workspace-current-index 1)
                              (exwm-workspace--count))))

  (defun dhnam/exwm-other-workspace-backwards () (interactive) (dhnam/exwm-other-workspace -1))

  (defun dhnam/xrandr-set-brightness (brightness)
    (interactive "NEnter brightness percentage: ")
    (if (<= 0 brightness 100)
        (start-process-shell-command
         "dhnam/xrandr-set-brightness" nil
         (concat dhnam/lib-root-dir "common/dependent/xrandr-set-brightness.sh")
         (number-to-string (/ brightness 100.0)))
      (user-error "Out of range"))))

(progn
  ;; split functions
  (comment (defvar dhnam/exwm-split-defined t))

  (defun dhnam/exwm-split-window-below (&optional size)
    ;; https://github.com/ch11ng/exwm/issues/685#issuecomment-879903947
    (interactive "P")
    (split-window-below size)
    (comment (buf-move-down))
    (windmove-down)
    (redisplay)
    (windmove-up))

  (defun dhnam/exwm-split-window-right (&optional size)
    ;; https://github.com/ch11ng/exwm/issues/685#issuecomment-879903947
    (interactive "P")
    (split-window-right size)
    (comment (buf-move-right))
    (windmove-right)
    (redisplay)
    (windmove-left))

  (defun dhnam/exwm-split-move-window-below (&optional size)
    ;; https://github.com/ch11ng/exwm/issues/685#issuecomment-879903947
    (interactive "P")
    (split-window-below size)
    (redisplay)
    (windmove-down))

  (defun dhnam/exwm-split-move-window-right (&optional size)
    ;; https://github.com/ch11ng/exwm/issues/685#issuecomment-879903947
    (interactive "P")
    (split-window-right size)
    (redisplay)
    (windmove-right)))

(with-eval-after-load 'exwm-edit
  (defvar dhnam/exwm-temp-copied-text nil
    "Temporary text copied from an application.")

  (defun dhnam/exwm-edit--send-to-exwm-buffer (text)
    "Sends TEXT to the exwm window.
It's modified from `exwm-edit--send-to-exwm-buffer'.
"
    (progn
      ;; added by dhnam
      (setq dhnam/exwm-temp-copied-text
            (progn
              ;; This snippet is copied from `current-kill'
              (and interprogram-paste-function (funcall interprogram-paste-function)))))

    (kill-new text)
    (set-window-configuration exwm-edit--last-window-configuration)
    (setq exwm-edit--last-window-configuration nil)
    (exwm-input--set-focus (exwm--buffer->id (window-buffer (selected-window))))
    (if (string= text "")
        ;; If everything is deleted in the exwm-edit buffer, then simply delete the selected text in the exwm buffer
        (run-with-timer
         exwm-edit-paste-delay
         nil
         (lambda () (exwm-input--fake-key 'delete)))

      (run-with-timer
       exwm-edit-paste-delay
       nil
       (lambda ()
		 (exwm-input--fake-key ?\C-v)
		 ;; Clean up the kill ring
		 ;; It needs to be run on a timer because of some reason
		 (run-with-timer
          exwm-edit-clean-kill-ring-delay
          nil
          (lambda ()
			(pop kill-ring)
			;; Kill-ring weirdness
            (comment
              ;; commented by dhnam
			  (if kill-ring
				  (kill-new (car kill-ring))
			    (kill-new "")))
            (if dhnam/exwm-temp-copied-text
                ;; added by dhnam
                (kill-new dhnam/exwm-temp-copied-text)
              (kill-new (pop kill-ring)))

            (when dhnam/exwm-temp-post-fn
              (funcall dhnam/exwm-temp-post-fn))))))))

  (defvar dhnam/exwm-temp-post-fn nil)
  (defun dhnam/exwm-edit-send-text (text &optional delay post-fn)
    (let ((exwm-edit--last-window-configuration (current-window-configuration))
          (exwm-edit-paste-delay delay))
      (comment (exwm-edit--send-to-exwm-buffer text))
      (setq dhnam/exwm-temp-post-fn post-fn)
      (dhnam/exwm-edit--send-to-exwm-buffer text)
      text))

  (defun dhnam/exwm-edit-send-key-only (key)
    ;; Example of `key' = (kbd "<return>")

    (comment
      ;; Send only a sigle key
      (exwm-input--fake-key (aref key 0)))

    (seq-do-indexed
     (lambda (value idx)
       (exwm-input--fake-key value))
     key))

  (defmacro dhnam/exwm-edit-send-key (key &optional delay)
    `(progn
       (let ((delay (or ,delay 0)))
         (if (> delay 0)
             (run-with-timer
              delay nil
              (lambda () (dhnam/exwm-edit-send-key-only ,key)))
           (dhnam/exwm-edit-send-key-only ,key)))
       nil))
  )
