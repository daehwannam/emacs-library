
(progn
;;; poporg mode
;;; https://github.com/QBobWatson/poporg

  (when (package-installed-p 'poporg)
    ;; (require 'poporg nil t)
    (autoload 'poporg-dwim "poporg" nil t)

    (progn
      ;; common
      (defun dhnam/poporg-exit-buffer ()
	    (interactive)

        ;; `delete-window' is called by `poporg-kill-buffer-routine',
        ;; Therefore, `delete-window' should be disabled not to close the window.

	    (when (or (not (buffer-modified-p))
                  (yes-or-no-p "Really abandon this edit? "))
          (dhnam/disable-fn delete-window
	        (poporg-kill-buffer-routine))
          (kill-buffer buffer-or-name)))

      (defun dhnam/poporg-kill-buffer (&optional buffer-or-name)
        (interactive (list (get-buffer (read-buffer (format "Kill buffer: " (buffer-name)) (current-buffer)))))

        ;; `delete-window' is called by `poporg-kill-buffer-routine',
        ;; Therefore, `delete-window' should be disabled not to close the window.
        (if (not (eq (current-buffer) (get-buffer buffer-or-name)))
            (kill-buffer buffer-or-name)

          (when (or (not (buffer-modified-p))
                    (yes-or-no-p "Really abandon this edit? "))
            (dhnam/disable-fn delete-window
	          (poporg-kill-buffer-routine))
            (kill-buffer buffer-or-name))))

      (comment
        (require 'poporg nil t)
        (define-key poporg-mode-map (kbd "C-x k") #'dhnam/poporg-kill-buffer))

      (comment
        (use-existing-pkg poporg
          :bind (:map poporg-mode-map
		         ("C-x k" . dhnam/poporg-kill-buffer))))

      (with-eval-after-load 'poporg
        (define-key poporg-mode-map (kbd "C-x k") 'dhnam/poporg-kill-buffer)
        (define-key poporg-mode-map (kbd "C-c s") 'poporg-update-and-save)
        (define-key poporg-mode-map (kbd "C-c C-c") nil)))

    (defun dhnam/pop-rst-dwim ()
      ;; rst-mode
      (interactive)
      (poporg-dwim)
      (rst-mode)
      (poporg-mode +1))))

(provide 'dhnam-documentation)
