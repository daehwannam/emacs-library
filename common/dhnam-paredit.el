
(require 'paredit)

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


         ("RET" nil "quit"))

       (progn
         ;; Disable any hint message
         (hydra-set-property 'dhnam-paredit-iokl :verbosity 0))

       ;; (define-key global-map (kbd ,dhnam-paredit-iokl/activation-key) 'dhnam-paredit-iokl/body)
       )))

(provide 'dhnam-paredit)
