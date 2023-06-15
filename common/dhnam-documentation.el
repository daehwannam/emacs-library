
(progn
;;; poporg mode
;;; https://github.com/QBobWatson/poporg

  (when (package-installed-p 'poporg)
    ;; (require 'poporg nil t)
    (autoload 'poporg-dwim "poporg" nil t)

    (progn
      ;; common
      (progn
        (defun dhnam/poporg-kill-buffer-exit ()
	      (interactive)
	      (when (yes-or-no-p "Really abandon this edit? ")
	        (poporg-kill-buffer-routine))))
      (comment
        (require 'poporg nil t)
        (define-key poporg-mode-map (kbd "C-x k") #'dhnam/poporg-kill-buffer-exit))

      (comment
        (use-existing-pkg poporg
          :bind (:map poporg-mode-map
		         ("C-x k" . dhnam/poporg-kill-buffer-exit))))

      (with-eval-after-load 'poporg
        (define-key poporg-mode-map (kbd "C-x k") 'dhnam/poporg-kill-buffer-exit)
        (define-key poporg-mode-map (kbd "C-c s") 'poporg-update-and-save)
        (define-key poporg-mode-map (kbd "C-c C-c") nil)))

    (progn
      ;; org-mode
      (global-set-key (kbd "C-c / /") 'poporg-dwim))

    (progn
      ;; rst-mode
      (defun dhnam/pop-rst-dwim ()
        (interactive)
        (poporg-dwim)
        (rst-mode)
        (poporg-mode +1))

      (global-set-key (kbd "C-c / r") 'dhnam/pop-rst-dwim))))

(provide 'dhnam-documentation)
