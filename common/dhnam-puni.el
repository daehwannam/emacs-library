
(with-eval-after-load 'puni

  (defun dhnam/puni-copy-line ()
    (interactive)

    (let ((beg (point))
          (eol (point-at-eol)))
      (if (= beg eol)
          (forward-char)
        (progn
          (when (string-match-p "\\`[[:space:]]*\\'" (buffer-substring-no-properties (point) eol))
            (goto-char eol))
          (while (< (point) eol)
            (puni-forward-sexp)
            (when (string-match-p "\\`[[:space:]]*\\'" (buffer-substring-no-properties (point) eol))
              (goto-char eol)))))
      (if (eq last-command 'dhnam/puni-copy-line)
          (kill-append (buffer-substring beg (point)) nil)
        (kill-ring-save beg (point)))))

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

  (progn
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
           ("q" puni-split)

           ("RET" nil "quit"))

         (progn
           ;; Disable any hint message
           (hydra-set-property 'dhnam-puni-iokl :verbosity 0))

         ;; (define-key global-map (kbd ,dhnam-puni-iokl/activation-key) 'dhnam-puni-iokl/body)
         ))))


(provide 'dhnam-puni)
