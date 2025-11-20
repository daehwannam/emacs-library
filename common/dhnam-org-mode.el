
(defun dhnam/org-babel-execute-region (beg end &optional arg)
  (interactive "r")
  (if (use-region-p)
      (save-excursion
        (save-restriction
          (narrow-to-region beg end)
          (org-babel-execute-buffer arg)
          (comment (widen))))
    (org-babel-execute-buffer arg)))

(progn
  ;; Deprecated: loading a large org-mode file is slow with this fix.
  ;;
  ;; fix the problem of unbalanced parentheses by "<" and ">"
  ;; https://emacs.stackexchange.com/a/52209

  (defun dhnam/org-mode-<>-syntax-fix (start end)
    "Change syntax of characters ?< and ?> to symbol within source code blocks."
    (let ((case-fold-search t))
      (when (eq major-mode 'org-mode)
        (save-excursion
          (goto-char start)
          (while (re-search-forward "<\\|>" end t)
            (when (save-excursion
                    (and
                     (re-search-backward "[[:space:]]*#\\+\\(begin\\|end\\)_src\\_>" nil t)
                     (string-equal (downcase (match-string 1)) "begin")))
              ;; This is a < or > in an org-src block
              (put-text-property (point) (1- (point))
                                 'syntax-table (string-to-syntax "_"))))))))

  (defun dhnam/org-setup-<>-syntax-fix ()
    "Setup for characters ?< and ?> in source code blocks.
Add this function to `org-mode-hook'.
However, this fix slows down loading a large org-mode file.
"
    (make-local-variable 'syntax-propertize-function)
    (setq syntax-propertize-function 'dhnam/org-mode-<>-syntax-fix)
    (syntax-propertize (point-max)))

  (comment
    ;; Example of usage
    (add-hook 'org-mode-hook #'dhnam/org-setup-<>-syntax-fix)))

(progn
  ;; fix the problem of unbalanced parentheses by "<" and ">"
  ;; https://emacs.stackexchange.com/a/68321
  (defun dhnam/org-syntax-table-modify ()
    "Modify `org-mode-syntax-table' for the current org buffer."
    (modify-syntax-entry ?< "." org-mode-syntax-table)
    (modify-syntax-entry ?> "." org-mode-syntax-table))

  (comment
    ;; Example of usage
    (add-hook 'org-mode-hook #'dhnam/org-syntax-table-modify)))

(progn
  (defun dhnam/org-add-hard-indentation (arg)
    ;; This function can be replaced with `org-indent-region' when `org-indent-mode' is `nil'
    (interactive "p")
    (save-excursion
      (beginning-of-buffer)
      (while
          (let ((indent-size
                 (progn
                   (when (re-search-forward "^\\*+ " nil t)
                     (length (match-string 0))))))
            (when indent-size
              (let ((start
                     (progn
                       (next-line)
                       (move-beginning-of-line 1)
                       (point)))
                    (end
                     (progn
                       (if (re-search-forward "^\\*+ " nil t)
                           (progn (previous-line) (move-end-of-line 1))
                         (end-of-buffer))
                       (point))))

                (indent-rigidly start end (* indent-size arg))))
            indent-size))))

  (defun dhnam/org-delete-hard-indentation (arg)
    ;; This function can be replaced with `org-unindent-buffer'
    ;; https://www.reddit.com/r/orgmode/comments/qt2mmd/remove_hard_indentation_from_org_file_made_with/
    (interactive "p")
    (dhnam/org-add-hard-indentation (- arg))))

(progn
  ;; Dates

  (defun dhnam/count-calendar-days (&optional date1 date2 with-message)
    "Count the number of calendar days -- includes holidays, weekends, etc."

    ;; This is modified from:
    ;; https://emacs.stackexchange.com/a/23894

    (let (abs-date1 abs-date2 total-days)
      (unless date1
        (setq date1 (org-read-date nil nil nil "Insert First Date:  ")))
      (let* ((d1-parsed (org-parse-time-string date1))
             (d1-day (nth 3 d1-parsed))
             (d1-month (nth 4 d1-parsed))
             (d1-year (nth 5 d1-parsed))
             (d1-list (list d1-month d1-day d1-year)))
        (setq abs-date1 (calendar-absolute-from-gregorian d1-list)))

      (unless date2
        (setq date2 (org-read-date nil nil nil "Insert Second Date:  ")))
      (let* ((d2-parsed (org-parse-time-string date2))
             (d2-day (nth 3 d2-parsed))
             (d2-month (nth 4 d2-parsed))
             (d2-year (nth 5 d2-parsed))
             (d2-list (list d2-month d2-day d2-year)))
        (setq abs-date2 (calendar-absolute-from-gregorian d2-list)))

      (setq total-days (- abs-date2 abs-date1)
            ;; (let* ((days (- abs-date1 abs-date2))
            ;;        (days (if (> days 0) days (- days))))
            ;;   days)
            )

      (when with-message
        (message "%s - %s = %s" date2 date1 total-days))

      total-days))

  (defun dhnam/display-calendar-days (&optional date1 date2)
    (interactive)
    (dhnam/count-calendar-days date1 date2 t))

  (defun dhnam/org-today-timestr (&optional org-with-time)
    (let* ((org-defdecode (decode-time (org-current-time)))
           (org-def (org-encode-time org-defdecode)  )
           (timestr (format-time-string
		             (if org-with-time "%Y-%m-%d %H:%M" "%Y-%m-%d")
		             org-def)))
      timestr))


  (defun dhnam/get-org-date-from-timestr (timestr &optional org-with-time)
    "This functions used code in `org-read-date'

e.g. (dhnam/get-org-date-from-timestr \"Thursday   21 September 2023\")"

    (let* ((org-defdecode (decode-time (org-current-time)))
           (org-def (org-encode-time org-defdecode)))
      (let ((analysis (org-read-date-analyze timestr org-def org-defdecode)))
        (if org-with-time
            (format "%04d-%02d-%02d %02d:%02d"
                    (nth 5 analysis) (nth 4 analysis) (nth 3 analysis)
		            (nth 2 analysis) (nth 1 analysis))
          (format "%04d-%02d-%02d" (nth 5 analysis) (nth 4 analysis) (nth 3 analysis))))))

  (defun dhnam/count-calendar-days-from-today (&optional date)
    (dhnam/count-calendar-days (dhnam/org-today-timestr)
                               (dhnam/get-org-date-from-timestr date)))

  (defun dhnam/org-display-remaining-days ()
    (interactive)
    (let ((line-text (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
      (let ((days (dhnam/count-calendar-days-from-today line-text)))
        (message "Remaining days: %s" days))))

  (defun dhnam/agenda-remaining-days-until-current-line ()
    (let* ((line-text (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
           (words (split-string line-text))
           (timestr (string-join (cdr words) " ")))
      (dhnam/count-calendar-days-from-today timestr)))

  (defun dhnam/agenda-display-remaining-days ()
    (interactive)
    (save-excursion
      (cl-flet ((empty-first-char-p () (let ((line-beg-pos (line-beginning-position)))
                                         (equal (buffer-substring-no-properties line-beg-pos (1+ line-beg-pos)) " ")))
                (not-first-line-p () (not (= (line-beginning-position) 1))))
        (let ((prev-line-count 0)
              (max-prev-line-count 100)
              (within-limit (lambda () (< (setq prev-line-count (1+ prev-line-count)) max-prev-line-count))))
          (while (and (funcall within-limit) (empty-first-char-p) (not-first-line-p))
            (previous-line)))
        (when (and (not (empty-first-char-p)) (not-first-line-p))
          (message "Remaining days: %s" (dhnam/agenda-remaining-days-until-current-line))))))

  (defun dhnam/agenda-move-to-prev-span (&optional arg)
    "Move to the previous span in the current agenda list"

    (interactive "p")
    (unless arg (setq arg 1))
    (dhnam/agenda-move-to-next-span (- arg)))

  (defun dhnam/agenda-move-to-next-span (&optional arg)
    "Move to the next span in the current agenda list"

    (interactive "p")
    (unless arg (setq arg 1))
    (save-excursion
      (beginning-of-buffer)
      (next-line)
      (let ((start-day-in-list (dhnam/agenda-remaining-days-until-current-line))
            (span-days (org-agenda-ndays-to-span org-agenda-span)))
        (org-agenda-list nil (+ (org-today) start-day-in-list (* arg span-days)))))))

(defun dhnam/org-open-at-point-same-window ()
  (interactive)
  (dhnam/call-command-same-window #'org-open-at-point))

(defun dhnam/org-insert-exact-time-stamp ()
  (interactive)
  (comment (format-time-string "%Y-%m-%d %H:%M" (org-current-time)))
  (insert (format-time-string "<%Y-%m-%d %H:%M>" (org-current-time))))

(defun dhnam/rows-to-org-table (headers rows)
  (let ((unaligned-table
         (concat
          (format "| %s |\n" (mapconcat #'identity headers " | "))
          "|-"
          (mapconcat
           (lambda (row) (format "| %s |" (mapconcat #'identity row " | ")))
           rows
           "\n"))))
    (with-temp-buffer
      (insert unaligned-table)
      (org-table-align)
      (buffer-string))))

(provide 'dhnam-org-mode)
