
(unless (fboundp 'comment)
  (defmacro comment (&rest args)
    `nil))

(comment
  (defcustom python-shell-prompt-pdb-regexp "[(<]*[Ii]?[Pp]db[>)]+ "
    "Regular expression matching pdb input prompt of Python shell.
It should not contain a caret (^) at the beginning."
    :type 'string)

  (defcustom python-pdbtrack-stacktrace-info-regexp
    "> \\([^\"(]+\\)(\\([0-9]+\\))\\([?a-zA-Z0-9_<>]+\\)()"
    "Regular expression matching stacktrace information.
Used to extract the current line and module being inspected.

Must match lines with real filename, like
 > /path/to/file.py(42)<module>()->None
and lines in which filename starts with `<', e.g.
 > <stdin>(1)<module>()->None

In the first case /path/to/file.py file will be visited and overlay icon
will be placed in line 42.
In the second case pdbtracking session will be considered over because
the top stack frame has been reached.

Filename is expected in the first parenthesized expression.
Line number is expected in the second parenthesized expression."
    :type 'string
    :version "27.1"
    :safe 'stringp))

(progn
  (defvar pdb-tracking/python-shell-prompt-pdb-regexp "[(<]*[Ii]?[Pp]db\\(\\+\\+\\)?[>)]+ "
    "It replaces `python-shell-prompt-pdb-regexp' to recognize (Pdb++)")

  (defvar pdb-tracking/check-python-shell-prompt-pdb-regexp nil)

  (defvar pdb-tracking/search-from-end-of-the-current-line t)
  
  (defvar pdb-tracking/python-pdbtrack-stacktrace-info-regexp
    "\\(>\\|\\[[0-9]+\\]\\) \\([^\"(]+\\)(\\([0-9]+\\))\\([?a-zA-Z0-9_<>]*\\)\\(()\\)?"
    "It replaces `python-pdbtrack-stacktrace-info-regexp' to recognize of Pdb++'s 'sticky' mode and 'where' command.
The expression uses mached parentheses of index 2 and 3 while `python-pdbtrack-stacktrace-info-regexp' uses those of index 1 and 2")

  (defvar pdb-tracking/file-name-match-idx 2))

(defun pdb-tracking/display-current-line (&optional switching-buffer)
  (interactive)
  (let (file-name line-number)
    (save-excursion
      (when (and (or (not pdb-tracking/check-python-shell-prompt-pdb-regexp)
                     (save-excursion
                       (progn
                         ;; Use the below code instead of `move-beginning-of-line',
                         ;; whose action is different in `shell-mode'
                         (skip-chars-backward "^\n")
                         (looking-at pdb-tracking/python-shell-prompt-pdb-regexp))))
                 (save-excursion
                   (when pdb-tracking/search-from-end-of-the-current-line
                     (move-end-of-line 1))
                   (re-search-backward pdb-tracking/python-pdbtrack-stacktrace-info-regexp
                                       (comment (- (point) 10000)) t)))

        (setq file-name (match-string-no-properties pdb-tracking/file-name-match-idx))
        (setq line-number (string-to-number (match-string-no-properties (+ pdb-tracking/file-name-match-idx 1))))))

    (when (and file-name line-number (not (string-match "<.*>" file-name)))
      (let* ((original-window (selected-window))
             (tracked-buffer (find-file-other-window file-name))
             (tracked-buffer-window (get-buffer-window tracked-buffer))
             (tracked-buffer-line-pos nil))
        (with-current-buffer tracked-buffer
          (set (make-local-variable 'overlay-arrow-position) (make-marker))
          (setq tracked-buffer-line-pos (progn
                                          (goto-char (point-min))
                                          (forward-line (1- line-number))
                                          (comment (recenter nil t))
                                          (point-marker)))
          (when tracked-buffer-window
            (set-window-point
             tracked-buffer-window tracked-buffer-line-pos))
          (set-marker overlay-arrow-position tracked-buffer-line-pos))
        (pop-to-buffer tracked-buffer)
        (unless switching-buffer
          (select-window original-window))))))

(defun pdb-tracking/go-to-current-line ()
  (interactive)
  (pdb-tracking/display-current-line t))

(comment
  (defun pdb-tracking/trigger ()
    (when (equal this-command (key-binding (kbd "RET")))
      (pdb-tracking/go-to-current-line)))

  (defvar automatic-tracking-p nil)

  (defun pdb-tracking/toggle-automatic-tracking ()
    (interactive)
    (setq automatic-tracking-p (not automatic-tracking-p))
    (if automatic-tracking-p
        (progn
          (add-hook 'post-command-hook #'pdb-tracking/trigger)
          (message "Automatic pdb tracking is enabled"))
      (progn
        (remove-hook 'post-command-hook #'pdb-tracking/trigger)
        (message "Automatic pdb tracking is disabled")))))

(defun pdb-tracking/search-backward-pdbpp-arrow ()
  (interactive)
  (re-search-backward "->")
  (forward-word)
  (backward-word))

(defvar pdb-tracking-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-o") 'pdb-tracking/go-to-current-line)
    (define-key map (kbd "M-o") 'pdb-tracking/display-current-line)
    map)
  "Keymap for `pdb-tracking-mode'.")

(define-minor-mode pdb-tracking-mode
  "Tracking code for PDB"
  nil   ; Initial value, nil for disabled
  :global nil
  :lighter " pdb-tracking"
  :keymap pdb-tracking-mode-map

  (comment
    (if pdb-tracking-mode
        (add-hook 'post-command-hook #'pdb-tracking/trigger)
      (remove-hook 'post-command-hook #'pdb-tracking/trigger))))

(provide 'pdb-tracking)
