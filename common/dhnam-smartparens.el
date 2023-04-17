(with-eval-after-load 'smartparens
  (comment
    ;; it's not working when the start/end of killed region has space ("Buffer is read-only").
    (defun dhnam/sp-copy-hybrid-sexp (&optional arg)
      "Do a `sp-kill-hybrid-sexp' but copy rather than kill."
      (interactive "P")
      (let ((buffer-read-only t)
            (kill-read-only-ok t))
        (cl-letf (((symbol-function 'message)
                   (lambda (&rest args))))
          ;; `message' is temporarily disabled
          (sp-kill-hybrid-sexp arg)
          (let ((beg (point)))
            (forward-char (length (car kill-ring)))
            (let ((end (point)))
              (assert (string= (car kill-ring) (buffer-substring beg end))))
            (when (eq last-command 'dhnam/sp-copy-hybrid-sexp)
              (kill-append (pop kill-ring) nil)))))))

  (comment
    ;; Not working
    (defun dhnam/sp-copy-hybrid-sexp ()
      "Similar to `sp-kill-hybrid-sexp' but copy rather than kill."
      (interactive)
      (let ((beg-point (point))
            (eol-point (point-at-eol)))
        (cl-letf (((symbol-function 'message)
                   (lambda (&rest args))))
          (while (< (point) eol-point)
            (sp-forward-sexp)))
        (let ((end-point (point)))
          (if (eq last-command 'dhnam/sp-copy-hybrid-sexp)
              (kill-append (if (= beg-point end-point)
                               (if (= (point) (point-max))
                                   (message "End of buffer")
                                 (progn (forward-char) "\n"))
                             (buffer-substring beg-point end-point)))
            (kill-ring-save beg-point end-point))))))

  (comment
    (defvar dhnam-smartparens-iokl/default-cursor-color "orchid")
    (defvar dhnam-smartparens-iokl/activated-cursor-color "cyan")
    (defvar dhnam-smartparens-iokl-paredit-struct-cursor-color "orange")


    (dhnam/set-cursor-color dhnam-smartparens-iokl/default-cursor-color)

    (defconst dhnam-smartparens-iokl/plist-1
      '(:pre (dhnam/set-cursor-color dhnam-smartparens-iokl/activated-cursor-color)
        :post (dhnam/set-cursor-color dhnam-smartparens-iokl/default-cursor-color)))

    (eval-after-load 'smartparens
      `(progn
         ;; dhnam-smartparens-iokl
         (defhydra dhnam-smartparens-iokl
           ,dhnam-smartparens-iokl/plist-1

           "smartparens"

           ("k" sp-backward-slurp-sexp)
           ("K" sp-backward-barf-sexp)
           ("l" sp-forward-slurp-sexp)
           ("L" sp-forward-barf-sexp)

           ("i" sp-split-sexp)
           ("o" sp-join-sexps)
           ("I" sp-raise-sexp)
           ("O" sp-convolute-sexp)

           ("j" sp-splice-sexp-killing-backward)
           (";" sp-splice-sexp-killing-forward)

           ("RET" nil "quit"))

         (progn
           ;; Disable any hint message
           (hydra-set-property 'dhnam-smartparens-iokl :verbosity 0))

         ;; (define-key global-map (kbd ,dhnam-smartparens-iokl/activation-key) 'dhnam-smartparens-iokl/body)
         ))))

(provide 'dhnam-smartparens)
