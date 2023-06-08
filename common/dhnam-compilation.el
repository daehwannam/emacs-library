
(defun dhnam/compile-goto-error-same-window ()
  ;; https://emacs.stackexchange.com/a/33908
  (interactive)
  (let ((display-buffer-overriding-action
         '((display-buffer-reuse-window
            display-buffer-same-window)
           (inhibit-same-window . nil))))
    (call-interactively #'compile-goto-error)))

(defun dhnam/compile-goto-error-in-vlf ()
  (interactive)
  (save-excursion
    (let ((line-begin (progn (move-beginning-of-line 1) (point)))
          (line-end (progn (move-end-of-line 1) (point))))
      (save-restriction
        (narrow-to-region line-begin line-end)
        (let ((line-num-text-end (progn (move-beginning-of-line 1)
                                        (re-search-forward "[0-9]+:" nil t))))
          (if line-num-text-end
              (let ((num-begin (progn (backward-word) (point)))
                    (num-end (1- line-num-text-end)))
                (let ((path-end (1- num-begin)))
                  (let ((line-number (string-to-number (buffer-substring-no-properties num-begin num-end)))
                        (file-path (buffer-substring-no-properties line-begin path-end)))

                    (let ((buffer (or (find-buffer-visiting file-path)
                                      (dhnam/vlf-no-pop-buffer file-path))))
                      (switch-to-buffer-other-window buffer)
                      (with-current-buffer buffer
                        (vlf-goto-line line-number))))))
            (progn
              (message "Invalid hit here"))))))))

(defun dhnam/vlf-no-pop-buffer (file &optional minimal)
  (interactive (list (read-file-name "File to open: ") nil))
  (cl-letf (((symbol-function 'switch-to-buffer)
             (lambda (&rest args))))
    (vlf file minimal)))

(provide 'dhnam-compilation)
