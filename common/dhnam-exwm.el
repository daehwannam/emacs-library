
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
  (defun dhnam/exwm-edit-send-text (text &optional delay)
    (let ((exwm-edit--last-window-configuration (current-window-configuration))
          (exwm-edit-paste-delay delay))
      (exwm-edit--send-to-exwm-buffer text)
      text))

  (defun dhnam/exwm-edit-send-key-only (key)
    ;; Example of `key' = (kbd "<return>")
    (exwm-input--fake-key (aref key 0)))

  (defmacro dhnam/exwm-edit-send-key (key &optional delay)
    (let ((delay (or delay 0)))
      `(progn
         (let ((delay ,delay))
           (if (> delay 0)
               (run-with-timer
                delay nil
                (lambda () (dhnam/exwm-edit-send-key-only ,key)))
             (dhnam/exwm-edit-send-key-only ,key)))
         nil))))
