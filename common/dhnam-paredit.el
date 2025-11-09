
(require 'paredit)

(defun dhnam/backward-sexp-to-beginning ()
  (interactive)
  (let ((prev (1+ (point-max))))
    (while (not (= prev (point)))
      (setq prev (point))
      (backward-sexp))))

(defun dhnam/forward-sexp-to-end ()
  (interactive)
  (let ((prev (1- (point-min))))
    (while (not (= prev (point)))
      (setq prev (point))
      (forward-sexp))))

(defun dhnam/copy-and-forward-sexp (&optional arg)
  "Save the sexp following point to the kill ring.
ARG has the same meaning as for `kill-sexp'."
  (interactive "p")
  (save-excursion
	(let ((orig-point (point)))
	  (forward-sexp (or arg 1))
	  (if (eq last-command 'dhnam/copy-and-forward-sexp)
		  (kill-append (buffer-substring orig-point (point)) nil)
	    (kill-ring-save orig-point (point)))))
  (forward-sexp arg))

(defun dhnam/paredit-copy (&optional arg)
  "Do a `paredit-kill' but copy rather than kill."
  (interactive "P")
  (let ((buffer-read-only t)
        (kill-read-only-ok t))

    (dhnam/without-message
     (ignore-errors
       (let ((beg (point))
             (eol (point-at-eol)))
         (paredit-kill arg)
         (when (< (point) eol)
           ;; to fix error that occurs in the last line of a buffer
           (forward-sexp)))))))

(defun dhnam/paredit-backward-up-or-down (&optional arg)
  (interactive "^p")
  (let ((start-point (point))
        (up-point (save-excursion (ignore-errors (paredit-backward-up arg)) (point)))
        (down-point (save-excursion (ignore-errors  (paredit-backward-down arg)) (point))))

    (goto-char
     (cond
      ((and (not (= start-point up-point)) (not (= start-point down-point)))
       (max up-point down-point))
      ((not (= start-point up-point)) up-point)
      ((not (= start-point down-point)) down-point)
      start-point))))

(defun dhnam/paredit-forward-up-or-down (&optional arg)
  (interactive "^p")
  (let ((start-point (point))
        (up-point (save-excursion (ignore-errors (paredit-forward-up arg)) (point)))
        (down-point (save-excursion (ignore-errors  (paredit-forward-down arg)) (point))))

    (goto-char
     (cond
      ((and (not (= start-point up-point)) (not (= start-point down-point)))
       (min up-point down-point))
      ((not (= start-point up-point)) up-point)
      ((not (= start-point down-point)) down-point)
      start-point))))

(defun dhnam/paredit-kill-ring-save (start end)
  "Save the region as if killed, but don't kill it, like `kill-region'.
If that text is unbalanced, signal an error instead."
  (interactive "r")
  (if (and start end (not current-prefix-arg))
      (paredit-check-region-for-delete start end))
  ;; (setq this-command 'kill-ring-save)
  (kill-ring-save start end))

(defun dhnam/paredit-comment-dwim (&optional argument)
  "Similar to `paredit-comment-dwim'.
However, if text is unbalanced, signal an error instead."
  (interactive "*P")
  (paredit-initialize-comment-dwim)
  (cond ((paredit-region-active-p)
         (paredit-check-region-for-delete (region-beginning) (region-end))
         (comment-or-uncomment-region (region-beginning)
                                      (region-end)
                                      argument))
        ((paredit-comment-on-line-p)
         (if argument
             (comment-kill (if (integerp argument) argument nil))
           (comment-indent)))
        (t (paredit-insert-comment))))


(defun dhnam/paredit-backward-delete (&optional argument)
  (interactive "P")
  (if (paredit-region-active-p)
      (progn
        (paredit-check-region-for-delete (region-beginning) (region-end))
        (delete-region (region-beginning) (region-end)))
    (paredit-backward-delete argument)))

(progn
  (defvar dhnam-paredit-iokl/default-cursor-color "orchid")
  (defvar dhnam-paredit-iokl/activated-cursor-color "cyan")
  (defvar dhnam-paredit-iokl-paredit-struct-cursor-color "orange")


  (defun dhnam-paredit-iokl/set-cursor-color (color)
    (if (display-graphic-p)
        (set-cursor-color color)
      (send-string-to-terminal (format "\033]12;%s\007" color))))

  (dhnam-paredit-iokl/set-cursor-color dhnam-paredit-iokl/default-cursor-color)

  (defconst dhnam-paredit-iokl/plist-1
    '(:pre (dhnam-paredit-iokl/set-cursor-color dhnam-paredit-iokl/activated-cursor-color)
      :post (dhnam-paredit-iokl/set-cursor-color dhnam-paredit-iokl/default-cursor-color)))

  (eval-after-load 'paredit
    `(progn
       ;; dhnam-paredit-iokl
       (defhydra dhnam-paredit-iokl
         ,dhnam-paredit-iokl/plist-1

         "paredit"

         ("k" paredit-backward-slurp-sexp)
         ("l" paredit-forward-slurp-sexp)
         ("i" paredit-backward-barf-sexp)
         ("o" paredit-forward-barf-sexp)
         ("j" paredit-splice-sexp-killing-backward)
         (";" paredit-splice-sexp-killing-forward)

         ("a" paredit-raise-sexp)
         ("s" paredit-splice-sexp)
         ("d" paredit-convolute-sexp)

         ("q" paredit-join-sexps)
         ("w" paredit-split-sexp)

         ("SPC" nil "quit")
         ;; ("RET" nil "quit")
         )

       (progn
         ;; Disable any hint message
         (hydra-set-property 'dhnam-paredit-iokl :verbosity 0))

       ;; (define-key global-map (kbd ,dhnam-paredit-iokl/activation-key) 'dhnam-paredit-iokl/body)
       )))


(defun dhnam/paredit-open-round-with-$-advice (orig-fun &rest args)
  (let ((orig-return (apply orig-fun args)))
    (let ((pos (point)))
      (when (string= (buffer-substring-no-properties (- pos 3) pos)
                     "$ (")
        (save-excursion
          (backward-char 1)
          (delete-backward-char 1))))
    orig-return))


(defun dhnam/paredit-open-square-with-$-advice (orig-fun &rest args)
  (let ((orig-return (apply orig-fun args)))
    (let ((pos (point)))
      (when (string= (buffer-substring-no-properties (- pos 3) pos)
                     "$ [")
        (save-excursion
          (backward-char 1)
          (delete-backward-char 1))))
    orig-return))


(progn
  (defun dhnam/paredit-insert-pair-no-space (n open close forward)

    ;; Modified from `paredit-insert-pair'

    (let* ((regionp
            (and (paredit-region-active-p)
                 (paredit-region-safe-for-insert-p)))
           (end
            (and regionp
                 (not n)
                 (prog1 (region-end) (goto-char (region-beginning))))))
      (let ((spacep (paredit-space-for-delimiter-p nil open)))
        (comment
          ;; commented by dhnam
          (if spacep (insert " ")))
        (insert open)
        (save-excursion
          ;; Move past the desired region.
          (cond (n
                 (funcall forward
                          (paredit-scan-sexps-hack (point)
                                                   (prefix-numeric-value n))))
                (regionp
                 (funcall forward (+ end (if spacep 2 1)))))
          ;; The string case can happen if we are inserting string
          ;; delimiters.  The comment case may happen by moving to the
          ;; end of a buffer that has a comment with no trailing newline.
          (if (and (not (paredit-in-string-p))
                   (paredit-in-comment-p))
              (newline))
          (insert close)
          (if (paredit-space-for-delimiter-p t close)
              (insert " "))))))

  (defun dhnam/paredit-doublequote (&optional n)
    "Insert a pair of double-quotes.
With a prefix argument N, wrap the following N S-expressions in
  double-quotes, escaping intermediate characters if necessary.
If the region is active, `transient-mark-mode' is enabled, and the
  region's start and end fall in the same parenthesis depth, insert a
  pair of double-quotes around the region, again escaping intermediate
  characters if necessary.
Inside a comment, insert a literal double-quote.
At the end of a string, move past the closing double-quote.
In the middle of a string, insert a backslash-escaped double-quote.
If in a character literal, do nothing.  This prevents accidentally
  changing a what was in the character literal to become a meaningful
  delimiter unintentionally."

    ;; Modified from `paredit-doublequote'

    (interactive "P")
    (cond ((paredit-in-string-p)
           (if (eq (point) (- (paredit-enclosing-string-end) 1))
               (forward-char)             ; Just move past the closing quote.
             ;; Don't split a \x into an escaped backslash and a string end.
             (if (paredit-in-string-escape-p) (forward-char))
             (insert ?\\ ?\" )))
          ((paredit-in-comment-p)
           (insert ?\" ))
          ((not (paredit-in-char-p))
           (dhnam/paredit-insert-pair-no-space n ?\" ?\" 'paredit-forward-for-quote)))))


(provide 'dhnam-paredit)
