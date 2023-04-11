(comment
  (defun dhnam/puni-copy-line (&optional arg)
    ;; Not working! Because, `puni-kill-line' makes `kill-region' as `last-command'.

    "Similar to `puni-kill-line', but copy a line rather than kill a line."
    (interactive "P")
    (when (eq last-command 'dhnam/puni-copy-line)
      (message "dfsdfsd"))
    (let ((buffer-read-only t)
          (kill-read-only-ok t))
      (cl-letf (;; ((symbol-function 'message)
                ;;  (lambda (&rest args)))
                )
        ;; `message' is temporarily disabled
        (let ((last-kill-lenth (if (eq last-command 'dhnam/puni-copy-line)
                                   (length (substring-no-properties (car kill-ring)))
                                 0)))
          (if (eq last-command 'dhnam/puni-copy-line)
              (message (format "----------- last-kill-lenth: %s" last-kill-lenth))
            (message (format "----------- last command: %s" last-command)))
          (puni-kill-line arg)
          (let ((beg (point)))
            (forward-char (- (length (substring-no-properties (car kill-ring)))
                             last-kill-lenth))
            (let ((end (point)))
              (assert (string= (car kill-ring) (buffer-substring beg end))))
            (comment
              (when (eq last-command 'dhnam/puni-copy-line)
                (kill-append (pop kill-ring) nil))))
          )))
    (setq this-command 'dhnam/puni-copy-line)))

(comment
  ;; not implemented
  (defun dhnam/puni-forward-line ()
    (let ((beg (point))
          (eol (point-at-eol)))
      (if (= beg eol)
          (forward-char)
        (while (< (point) eol)
          (puni-forward-sexp)))
      (if (eq last-command 'dhnam/puni-copy-line)
          (kill-ring-save beg (point))
        (kill-append (buffer-substring beg (point)) nil)))
    (setq this-command 'dhnam/puni-copy-line))

  (progn
    (defvar  dhnam/puni-copy-beg nil)
    (defconst dhnam/puni-copy/puni-forward-sexp-key "M-k")
    (defconst dhnam/puni-copy/puni-forward-sexp-key "M-k")
    (eval
     `(defhydra dhnam/puni-copy (:pre (setq dhnam/puni-copy-beg (point))
                                 :post (progn
                                         (kill-ring-save dhnam/puni-copy-beg (point))
                                         (setq dhnam/puni-copy-beg nil)))
        "puni-copy"
        (,dhnam/puni-copy/puni-forward-sexp-key puni-forward-sexp)
        ("q" nil "quit")))))
