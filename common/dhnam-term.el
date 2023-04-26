
(progn
  ;; Enable opening terminal with tramp via ssh
  ;; https://emacs.stackexchange.com/a/69784

  (defvar dhnam/ansi-term-char-mode-as-default t)

  (defun dhnam/ansi-term (&optional path name)
    "Opens a terminal at PATH. If no PATH is given, it uses
the value of `default-directory'. PATH may be a tramp remote path.
The term buffer is named based on `name' "
    (interactive)
    (require 'term)
    (unless path (setq path default-directory))
    (unless name (setq name (generate-new-buffer-name "*ansi-term*")))
    (let ((path (replace-regexp-in-string "^file:" "" path))
          (cd-str "fn=%s; if test ! -d $fn; then fn=$(dirname $fn); fi; cd $fn; exec bash")
          (start-term (lambda (termbuf)
                        (progn
                          (set-buffer termbuf)
                          (term-mode)
                          (if dhnam/ansi-term-char-mode-as-default
                              (term-char-mode)
                            (term-line-mode))
                          (switch-to-buffer termbuf)))))
      (if (tramp-tramp-file-p path)
          (let* ((tstruct (tramp-dissect-file-name path))
                 (cd-str-ssh (format cd-str (tramp-file-name-localname tstruct)))
                 (user (or (tramp-file-name-user tstruct) user-login-name))
                 (switches (list "-l" user
                                 "-t" (tramp-file-name-host tstruct)
                                 cd-str-ssh))
                 (termbuf (apply 'term-ansi-make-term name "ssh" nil switches)))
            (cond
             ((equal (tramp-file-name-method tstruct) "ssh")
              (funcall start-term termbuf))
             (t (error "not implemented for method %s"
                       (tramp-file-name-method tstruct)))))
        (let* ((cd-str-local (format cd-str path))
               (termbuf (apply 'term-ansi-make-term name "/bin/sh" nil (list "-c" cd-str-local))))
          (funcall start-term termbuf)))))

  (comment (key-chord-define-global "o2" 'dhnam/ansi-term)))


(progn
  (defun dhnam/move-beginning-of-command-line ()
    (interactive)
    (let* ((line-begin-point (save-excursion (move-beginning-of-line 1) (point)))
           (input-begin-point (save-excursion
                                (comment (move-end-of-line 1))
                                (re-search-backward "[$\n]" nil t)
                                (forward-char 2)
                                (point)))
           (input-end-point (save-excursion (move-end-of-line 1) (point))))
      (goto-char
       (if (< (+ line-begin-point 1) input-begin-point)
           (if (<= input-begin-point input-end-point)
               input-begin-point
             input-end-point)
         line-begin-point))))

  (defun dhnam/term-previous-prompt ()
    (interactive)
    (let ((original-point (point)))
      (move-beginning-of-line 1)
      (if (re-search-backward "\\$" nil t)
          (forward-char 2)
        (goto-char original-point))))

  (defun dhnam/term-next-prompt ()
    (interactive)
    (let ((original-point (point)))
      (if (re-search-forward "\\$" nil t)
          (forward-char 1)
        (goto-char original-point)))))


(provide 'dhnam-term)
