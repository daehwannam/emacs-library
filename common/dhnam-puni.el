
(defun dhnam/puni-kill-sexp (&optional copying)
  (interactive)

  (let ((beg (point))
        (end (save-excursion
               (puni-forward-sexp) (point))))
    (if (eq last-command this-command)
        (kill-append (buffer-substring beg end) nil)
      (kill-ring-save beg end))
    (unless copying
      (delete-region beg end))))

(defun dhnam/puni-kill-line (&optional copying)
  (interactive)

  (let ((beg (point))
        (eol (point-at-eol)))
    (if (= beg eol)
        (forward-char)
      (cl-flet ((space-only-before-eol-p () (string-match-p "\\`[[:space:]]*\\'" (buffer-substring-no-properties (point) eol))))
        (when (space-only-before-eol-p)
          (goto-char eol))
        (let ((prev 0))
          (while (< prev (point) eol)
            (setq prev (point))
            (puni-forward-sexp)
            (when (space-only-before-eol-p)
              (goto-char eol))))))
    (if (eq last-command this-command)
        (kill-append (buffer-substring beg (point)) nil)
      (kill-ring-save beg (point)))
    (unless copying
      (delete-region beg (point)))))

(defun dhnam/puni-copy-line ()
  (interactive)

  (dhnam/puni-kill-line t))

(defun dhnam/puni-copy-and-forward-sexp ()
  (interactive)

  (let ((beg (point))
        (eol (point-at-eol)))
    (if (= beg eol)
        (forward-char)
      (puni-forward-sexp))
    (if (eq last-command 'dhnam/puni-copy-and-forward-sexp)
        (kill-append (buffer-substring beg (point)) nil)
      (kill-ring-save beg (point)))))

(defun dhnam/puni-syntactic-backward-punct-or-word ()
  (interactive)
  (let ((punct-point (save-excursion (or (ignore-errors (puni-syntactic-backward-punct) (point))
                                         (point-min))))
        (word-point (save-excursion (backward-word) (point))))
    (goto-char (max punct-point word-point))))

(defun dhnam/puni-syntactic-forward-punct-or-word ()
  (interactive)
  (let ((punct-point (save-excursion (or (ignore-errors (puni-syntactic-forward-punct) (point))
                                         (point-max))))
        (word-point (save-excursion (forward-word) (point))))
    (goto-char (min punct-point word-point))))

(defun dhnam/puni-backward-sexp-to-beginning ()
  (interactive)
  (let ((prev (1+ (point-max))))
    (while (not (= prev (point)))
      (setq prev (point))
      (puni-backward-sexp))))

(defun dhnam/puni-forward-sexp-to-end ()
  (interactive)
  (let ((prev (1- (point-min))))
    (while (not (= prev (point)))
      (setq prev (point))
      (puni-forward-sexp))))

(comment
  (defun dhnam/puni-slurp-backward ()
    (interactive)
    (puni-slurp-backward)
    (dhnam/puni-backward-sexp-to-beginning))

  (defun dhnam/puni-slurp-forward ()
    (interactive)
    (puni-slurp-forward)
    (dhnam/puni-forward-sexp-to-end)))

(comment
  (defvar dhnam-puni-iokl/default-cursor-color "orchid")
  (defvar dhnam-puni-iokl/activated-cursor-color "cyan")
  (defvar dhnam-puni-iokl-puni-struct-cursor-color "orange")


  (defun dhnam-puni-iokl/set-cursor-color (color)
    (if (display-graphic-p)
        (set-cursor-color color)
      (send-string-to-terminal (format "\033]12;%s\007" color))))

  (dhnam-puni-iokl/set-cursor-color dhnam-puni-iokl/default-cursor-color)

  (defconst dhnam-puni-iokl/plist-1
    '(:pre (dhnam-puni-iokl/set-cursor-color dhnam-puni-iokl/activated-cursor-color)
      :post (dhnam-puni-iokl/set-cursor-color dhnam-puni-iokl/default-cursor-color)))

  (eval-after-load 'puni
    `(progn
       ;; dhnam-puni-iokl
       (defhydra dhnam-puni-iokl
         ,dhnam-puni-iokl/plist-1

         "puni"

         ("k" puni-slurp-backward)
         ("l" puni-slurp-forward)
         ("i" puni-barf-backward)
         ("o" puni-barf-forward)
         ("j" puni-splice-killing-backward)
         (";" puni-splice-killing-forward)

         ("a" puni-raise)
         ("s" puni-splice)
         ("d" puni-convolute)

         ("w" puni-split)

         ("q" nil "quit")
         ("SPC" nil "quit")
         ;; ("RET" nil "quit")
         )

       (progn
         ;; Disable any hint message
         (hydra-set-property 'dhnam-puni-iokl :verbosity 0))

       ;; (define-key global-map (kbd ,dhnam-puni-iokl/activation-key) 'dhnam-puni-iokl/body)
       )))


(provide 'dhnam-puni)
