
;;; otc: org todo comment

(defun dhnam/otc-get-current-time-stamp ()
  (format-time-string "<%Y-%m-%d %H:%M>" (org-current-time)))

(defvar dhnam/otc-time-stamp-regex "<[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} [0-9]\\{2\\}:[0-9]\\{2\\}>")

(defun dhnam/otc-insert-time-stamp ()
  (interactive)

  (insert (dhnam/otc-get-current-time-stamp)))

(defun dhnam/otc-toggle ()
  "Return non-nil if current line contains a TODO timestamp like: 'TODO <2025-11-18 16:40>:'"

  (interactive)

  (or
   (let ((pattern (format "TODO \\(%s\\):" dhnam/otc-time-stamp-regex)))
     (save-excursion
       (beginning-of-line)
       (let ((found (re-search-forward pattern (line-end-position) t)))
         (when found
           (replace-match (format "DONE %s--%s:"
                                  (match-string 1)
                                  (dhnam/otc-get-current-time-stamp)))
           t))))
   (let ((pattern (format "DONE \\(%s\\)--\\(%s\\):" dhnam/otc-time-stamp-regex dhnam/otc-time-stamp-regex)))
     (save-excursion
       (beginning-of-line)
       (let ((found (re-search-forward pattern (line-end-position) t)))
         (when found
           (replace-match (format "TODO %s:"
                                  (match-string 1)))
           t))))
   (progn
     (insert (format "TODO %s: " (dhnam/otc-get-current-time-stamp)))
     t)))


(defvar dhnam/otc-file-path-display-length 30)
(defvar dhnam/otc-file-path-omission "···")

(defun dhnam/otc-get-displayed-file-path (file-path)
  (if (> (length file-path) dhnam/otc-file-path-display-length)
      (concat
       dhnam/otc-file-path-omission
       (substring-no-properties
        file-path
        (+ (- (length file-path) dhnam/otc-file-path-display-length)
           (length dhnam/otc-file-path-omission))))
    (progn
      (comment file-path)
      (concat (make-string (- dhnam/otc-file-path-display-length (length file-path)) ? ) file-path))))


(defun dhnam/otc-run-command (command)
  (let ((buf (get-buffer-create (format "* %s *" command)))
        ;; (output-start nil)
        )
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "# -*- mode: org -*-\n\n")
        (insert (format "Command: %s\n\n" command))
        ;; (setq output-start (point))
        (special-mode)                  ; read-only UI mode
        ;; (org-mode)
        ))

    ;; Create the process
    (make-process
     :name command
     :buffer buf
     :command (list shell-file-name shell-command-switch command)
     :filter (lambda (proc output)
               ;; When the output of the process is passed
               (with-current-buffer (process-buffer proc)
                 (let ((inhibit-read-only t))
                   (goto-char (point-max))
                   (insert output))))
     :sentinel (lambda (proc event)
                 ;; When the process is ended
                 (when (memq (process-status proc) '(exit signal))
                   (with-current-buffer (process-buffer proc)
                     (let ((tuples nil))
                       (let ((inhibit-read-only t)
                             (remaining t))
                         (beginning-of-buffer)
                         (re-search-forward "^Command: [^\n]+\n\n")
                         (while (and remaining (re-search-forward ":" (line-end-position) t))
                           (let ((path (buffer-substring (line-beginning-position) (match-beginning 0)))
                                 (file-line (let ((file-line-start (point)))
                                              (re-search-forward ":" (line-end-position) t)
                                              (buffer-substring file-line-start (match-beginning 0))))
                                 (text (buffer-substring (point) (line-end-position))))
                             (push (list path file-line text) tuples))
                           (progn
                             (end-of-line)
                             (let ((cur-point (point)))
                               (ignore-errors (forward-char))
                               (when (equal cur-point (point))
                                 (setq remaining nil))))))

                       (beginning-of-buffer)
                       (re-search-forward "^Command: [^\n]+\n\n")
                       (let ((inhibit-read-only t))
                         (delete-region (point) (point-max))
                         (dolist (tuple tuples)
                           (seq-let (path file-line text) tuple
                             (insert (format "[[%s::%s][%s]] " path file-line (dhnam/otc-get-displayed-file-path path)))
                             (seq-let
                                 (date content)
                                 (cond
                                  ((save-excursion
                                     (string-match (format "TODO \\(%s\\):\\(.*\\)$" dhnam/otc-time-stamp-regex) text))
                                   (list (match-string 1 text) (string-trim (match-string 2 text))))
                                  ((save-excursion
                                     (string-match (format "TODO *: *\\(.*\\)$" dhnam/otc-time-stamp-regex) text))
                                   (list nil (string-trim (match-string 1 text))))
                                  ((save-excursion
                                     (string-match (format "TODO\\(.*\\)$" dhnam/otc-time-stamp-regex) text))
                                   (list nil (string-trim (match-string 1 text))))
                                  (t
                                   (error "No valid match")))
                               (insert (format "%s %s" (or date (make-string 18 ? )) content)))
                             (insert "\n")))))
                     (org-mode)))))
    (display-buffer buf)))

(defun dhnam/otc-summarize-todo (&optional regexp files dir)
  "Find TODO items and summarize them.
This function is modified from `rgrep'"

  (interactive
   (progn
     (grep-compute-defaults)
     (let* ((regexp "TODO.*:.*")
		    (files (grep-read-files regexp))
		    (dir (read-directory-name "Base directory: "
					                  nil default-directory t)))
	   (list regexp files dir))))
  (dhnam/otc-run-command (format "grep -I -nH -r -o -e %s --include='%s' %s" regexp files dir)))

(defun dhnam/otc-summarize-todo-again ()
  "Find TODO items and summarize them again."

  (interactive)

  (let ((command (progn
                   (beginning-of-buffer)
                   (re-search-forward "^Command: \\([^\n]+\\)\n\n")
                   (match-string 1))))
    (dhnam/otc-run-command command)))

(defun dhnam/otc-sort-lines ()
  ;; TODO <2025-11-20 03:11>
  )

(progn
  (unless (fboundp 'dhnam/after-otc-summarize-todo)
    (defun dhnam/after-otc-summarize-todo ()
      (comment
        (local-set-key (kbd "C-c D") #'dhnam/otc-summarize-todo-again))))

  (defun dhnam/otc-summarize-todo-advice (original &rest args)
    (let ((buffer (apply original args)))
      (set-buffer buffer)
      (dhnam/after-otc-summarize-todo)
      buffer))

  (advice-add 'dhnam/otc-summarize-todo
              :around #'dhnam/otc-summarize-todo-advice))

(provide 'dhnam-org-todo-comment)
