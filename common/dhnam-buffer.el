(defun dhnam/copy-whole-buffer ()
  (interactive)
  (kill-ring-save (point-min) (point-max)))

(progn
  ;; dhnam/buf-shift-* functions are defined for application buffers of EXWM

  (comment (require 'buffer-move))

  (defun dhnam/buf-shift-up ()
    "It's modified from `buf-move-up'"
    (interactive)
    (let* ((other-win (windmove-find-other-window 'up))
	       (buf-this-buf (window-buffer (selected-window))))
      (if (null other-win)
          (error "No window above this one")
        ;; swap top with this one
        (comment (set-window-buffer (selected-window) (window-buffer other-win)))
        ;; move this one to top
        (set-window-buffer other-win buf-this-buf)
        (select-window other-win))))

  (defun dhnam/buf-shift-down ()
    "It's modified from `buf-move-down'"
    (interactive)
    (let* ((other-win (windmove-find-other-window 'down))
	       (buf-this-buf (window-buffer (selected-window))))
      (if (or (null other-win) 
              (string-match "^ \\*Minibuf" (buffer-name (window-buffer other-win))))
          (error "No window under this one")
        ;; swap top with this one
        (comment (set-window-buffer (selected-window) (window-buffer other-win)))
        ;; move this one to top
        (set-window-buffer other-win buf-this-buf)
        (select-window other-win))))

  (defun dhnam/buf-shift-left ()
    "It's modified from `buf-move-left'"
    (interactive)
    (let* ((other-win (windmove-find-other-window 'left))
	       (buf-this-buf (window-buffer (selected-window))))
      (if (null other-win)
          (error "No left split")
        ;; swap top with this one
        (comment (set-window-buffer (selected-window) (window-buffer other-win)))
        ;; move this one to top
        (set-window-buffer other-win buf-this-buf)
        (select-window other-win))))

  (defun dhnam/buf-shift-right ()
    "It's modified from `buf-move-right'"
    (interactive)
    (let* ((other-win (windmove-find-other-window 'right))
	       (buf-this-buf (window-buffer (selected-window))))
      (if (null other-win)
          (error "No right split")
        ;; swap top with this one
        (comment (set-window-buffer (selected-window) (window-buffer other-win)))
        ;; move this one to top
        (set-window-buffer other-win buf-this-buf)
        (select-window other-win)))))

(defun dhnam/rename-file-and-buffer (new-name)
  ;; https://stackoverflow.com/a/384346
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
	  (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))

(defun dhnam/find-actual-file (filename &optional wildcards)
  "Open an actual file indicated by a symlink"
  ;; https://emacs.stackexchange.com/a/41292

  (interactive
   (find-file-read-args "Find an actual file: "
                        (confirm-nonexistent-file-or-buffer)))
  (let ((find-file-visit-truename t))
    (find-file filename wildcards)))

(defun dhnam/switch-to-scratch-buffer ()
  (interactive)
  (switch-to-buffer "*scratch*"))

(provide 'dhnam-buffer)
