
;;; otc: org todo comment

(defun dhnam/otc-get-current-time-stamp ()
  (format-time-string "[%Y-%m-%d %H:%M]" (org-current-time)))

(defvar dhnam/otc-time-stamp-regex "\\[[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} [0-9]\\{2\\}:[0-9]\\{2\\}\\]")

(defun dhnam/otc-insert-time-stamp ()
  (interactive)

  (insert (dhnam/otc-get-current-time-stamp)))


(defun dhnam/otc-toggle (&optional using-comment-dwim)
  "Toggle a TODO timestamp like: 'TODO [2025-11-18 16:40]:'"

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
     (when using-comment-dwim
       (comment-dwim nil)
       (when (not (eq (char-after (1- (point))) ?\ ))
         (insert " ")))
     (insert (format "TODO %s: " (dhnam/otc-get-current-time-stamp)))
     t)))

(defun dhnam/otc-toggle-in-program ()
  (interactive)

  (dhnam/otc-toggle
   (and (derived-mode-p 'prog-mode)
        (not (dhnam/in-string-p))
        (not (dhnam/in-comment-p)))))


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
      (comment (concat (make-string (- dhnam/otc-file-path-display-length (length file-path)) ? ) file-path))
      file-path)))

(require 'dhnam-org-mode)

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
        (comment
          ;; read-only UI mode; it also activates `read-only-mode'
          (special-mode))
        (read-only-mode 1)
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
                         (delete-region (point) (point-max)))

                       (let ((rows nil)
                             (row-number 0))
                         (dolist (tuple tuples)
                           (cl-incf row-number 1)
                           (seq-let (path file-line text) tuple
                             (let ((location (format "[[%s::%s][%s]] " path file-line (dhnam/otc-get-displayed-file-path path))))
                               (seq-let
                                   (date content)
                                   (cond
                                    ((save-excursion
                                       (string-match (format "TODO \\(%s\\):\\(.*\\)$" dhnam/otc-time-stamp-regex) text))
                                     (list (match-string 1 text) (string-trim (match-string 2 text))))
                                    ((save-excursion
                                       (string-match "TODO *: *\\(.*\\)$" text))
                                     (list nil (string-trim (match-string 1 text))))
                                    ((save-excursion
                                       (string-match (format "TODO\\(.*\\)$" dhnam/otc-time-stamp-regex) text))
                                     (list nil (string-trim (match-string 1 text))))
                                    (t
                                     (error "No valid match")))
                                 (push (list (number-to-string row-number) location date content) rows)))))
                         (org-mode)
                         (comment (setq-local org-startup-truncated t))
                         (setq-local truncate-lines t)
                         (dhnam/otc-table-mode 1)
                         (let ((inhibit-read-only t))
                           (insert (dhnam/rows-to-org-table '("No." "Location" "Date" "Comment") (reverse rows)))
                           (org-table-align)
                           (comment
                             (when (fboundp 'valign-mode)
                               (previous-line)
                               (valign-table))))))))))
    (comment (display-buffer buf))
    (display-buffer buf '(display-buffer-same-window))))

(progn
  ;; The grep options:
  ;; -I : ignore binary files
  ;; -n : print line numbers
  ;; -H : print file paths
  ;; -r : search recursively
  ;; -o : print mached parts only
  ;; -e <pattern> : specify the  pattern to find
  ;; --include=<extension> : specify the file extension to search

  (defvar dhnam/otc-grep-command-format "grep -I -n -H -r -o -e %s --include='%s' %s"))


(defvar dhnam/otc-todo-grep-regexp "TODO.*")
(comment (defvar dhnam/otc-todo-grep-regexp "TODO.*:.*"))
(defun dhnam/otc-summarize-todo (&optional regexp files dir)
  "Find TODO items and summarize them.
This function is modified from `rgrep'"

  (interactive
   (progn
     (grep-compute-defaults)
     (let* ((regexp dhnam/otc-todo-grep-regexp)
            (files (grep-read-files regexp))
		    (dir (read-directory-name "Base directory: "
					                  nil default-directory t)))
	   (list regexp files dir))))
  (let* ((window (dhnam/otc-run-command (format dhnam/otc-grep-command-format regexp files dir)))
         (buffer (window-buffer window)))
    buffer))

(defun dhnam/otc-summarize-todo-again ()
  "Find TODO items and summarize them again."

  (interactive)

  (let ((command (save-excursion
                   (beginning-of-buffer)
                   (re-search-forward "^Command: \\([^\n]+\\)\n\n")
                   (match-string 1))))
    (dhnam/otc-run-command command)))


(comment
  (unless (fboundp 'dhnam/otc-after-otc-summarize-todo)
    (defun dhnam/otc-after-otc-summarize-todo ()
      (comment
        (local-set-key (kbd "C-c D") #'dhnam/otc-summarize-todo-again))))

  (defun dhnam/otc-summarize-todo-advice (original &rest args)
    (let ((buffer (apply original args)))
      (set-buffer buffer)
      (dhnam/otc-after-otc-summarize-todo)
      buffer))

  (advice-add 'dhnam/otc-summarize-todo
              :around #'dhnam/otc-summarize-todo-advice))


(defun dhnam/otc-sort-by-date-column (&optional reversed)
  (interactive "P")

  (save-excursion
   (let ((inhibit-read-only t))
     (beginning-of-buffer)
     (re-search-forward "| \\(Date\\)[^\n|]*|")
     (goto-char (match-beginning 1))
     (next-line 2)
     (org-table-sort-lines nil (if reversed ?T ?t)))))


(defun dhnam/otc-sort-by-date-column-reversely ()
  (interactive)
  (dhnam/otc-sort-by-date-column t))


(defun dhnam/otc-sort-by-number-column (&optional reversed)
  (interactive "P")

  (save-excursion
   (let ((inhibit-read-only t))
     (beginning-of-buffer)
     (re-search-forward "| \\(No.\\)[^\n|]*|")
     (goto-char (match-beginning 1))
     (next-line 2)
     (org-table-sort-lines nil (if reversed ?N ?n)))))


(defun dhnam/otc-sort-by-number-column-reversely ()
  (interactive)
  (dhnam/otc-sort-by-number-column t))


(require 'cl-lib)


(defun dhnam/otc-sort-by-location-date-columns (reversed)
  (interactive "P")
  
  (save-excursion
    (let ((inhibit-read-only t))
      (beginning-of-buffer)
      (re-search-forward "| \\(Location\\)[^\n|]*|")
      (goto-char (match-beginning 1))

      (assert (org-at-table-p))
      (let ((table (org-table-to-lisp))
            (compare
             (if reversed
                 (lambda (row1 row2)
                   (if (not (string= (nth 0 row1) (nth 0 row2)))
                       (progn
                         ;; Currently, the line numbers of links are not considered
                         ;; e.g. [[path::line][repr]]
                         (string> (nth 0 row1) (nth 0 row2)))
                     (not (time-less-p (org-time-string-to-time (nth 1 row1))
                                       (org-time-string-to-time (nth 1 row2))))))
               (lambda (row1 row2)
                 (if (not (string= (nth 0 row1) (nth 0 row2)))
                     (string< (nth 0 row1) (nth 0 row2))
                   (time-less-p (org-time-string-to-time (nth 1 row1))
                                (org-time-string-to-time (nth 1 row2))))))))
        (let ((headers (car table))
              (rows (cddr table)))
          (let ((sorted (cl-sort (cl-copy-seq rows) compare)))
            ;; insert the sorted table
            (org-table-goto-line 0)
            (beginning-of-line)
            (delete-region (point) (org-table-end))
            (insert (dhnam/rows-to-org-table headers sorted))))))))

(defun dhnam/otc-undo ()
  (interactive)
  (let ((inhibit-read-only t))
    (undo)))

(defun dhnam/otc-visit-location ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (re-search-forward "\\[\\[" (line-end-position) t)
    (comment (dhnam/org-open-at-point-same-window))
    (org-open-at-point)))


(defvar dhnam/otc-table-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'quit-window)
    (define-key map (kbd "d") 'dhnam/otc-sort-by-date-column)
    (define-key map (kbd "D") 'dhnam/otc-sort-by-date-column-reversely)
    (define-key map (kbd "m") 'dhnam/otc-sort-by-number-column)
    (define-key map (kbd "M") 'dhnam/otc-sort-by-number-column-reversely)
    (define-key map (kbd "C-/")'dhnam/otc-undo)
    (define-key map (kbd "g") 'dhnam/otc-summarize-todo-again)
    (define-key map (kbd "RET") 'dhnam/otc-visit-location)
    map))

(define-minor-mode dhnam/otc-table-mode
  "Org TODO Comment Table Mode."

  nil
  :gloal nil
  :lighter " otc"
  :keymap dhnam/otc-table-mode-map
  (if dhnam/otc-table-mode
      (comment "when enabled")
    (comment "when disabled")))


(provide 'dhnam-org-todo-comment)
