
(with-eval-after-load 'ivy
  (defun dhnam/ivy-toggle-mark ()
    ;; https://www.reddit.com/r/emacs/comments/b6zh91/comment/ejpujuq/?utm_source=share&utm_medium=web2x&context=3
    "Toggle mark for current candidate and move forwards."
    (interactive)
    (if (ivy--marked-p)
        (ivy-unmark)
      (ivy-mark)))

  (defun dhnam/ivy-kill-marked ()
    (interactive)
    ;; it's modified from `ivy--call-marked'
    ;; this function is defined to kill `exwm' buffers
    (let* ((action 'ivy--kill-buffer-action)
           (prefix-len (length ivy-mark-prefix))
           (marked-candidates
            (mapcar
             (lambda (s)
               (let ((cand (substring s prefix-len)))
                 (if ivy--directory
                     (expand-file-name cand ivy--directory)
                   cand)))
             ivy-marked-candidates))
           (multi-action (ivy--get-multi-action ivy-last)))
      ;; (minibuffer-keyboard-quit)
      ;; (ivy-done)
      (if multi-action
          (let ((default-directory (ivy-state-directory ivy-last)))
            (funcall multi-action (mapcar #'ivy--call-cand marked-candidates)))
        ;; 
        (dolist (c marked-candidates)
          (let ((default-directory (ivy-state-directory ivy-last)))
            (funcall action (ivy--call-cand c)))))))

  (defun dhnam/ivy-next-history-element (arg)
    "Forward to `next-history-element' with ARG. It's modified from `ivy-next-history-element' not to check (derived-mode-p 'prog-mode)"
    (interactive "p")
    (if (and (= minibuffer-history-position 0)
             (equal ivy-text ""))
        (progn
          (when minibuffer-default
            (setq ivy--default (car minibuffer-default)))
          (insert ivy--default)
          (when (and
                 ;; (with-ivy-window (derived-mode-p 'prog-mode))
                 (eq (ivy-state-caller ivy-last) 'swiper)
                 (not (file-exists-p ivy--default))
                 (not (ivy-ffap-url-p ivy--default))
                 (not (ivy-state-dynamic-collection ivy-last))
                 (> (point) (minibuffer-prompt-end)))
            (ivy--insert-symbol-boundaries)))
      (next-history-element arg))
    (ivy--cd-maybe)
    (move-end-of-line 1)
    (ivy--maybe-scroll-history)))

(when (package-installed-p 'counsel)
  (defun dhnam/swiper-with-text-in-region (start end)
    (interactive "r")
    (deactivate-mark)
    (swiper (buffer-substring start end)))

  (progn
    ;; https://github.com/abo-abo/swiper/issues/1206#issuecomment-345455888

    (defun dhnam/ivy-ignore-buffers-with-different-major-mode (str)
      "Return non-nil if STR names a buffer of a different major mode.
This function is intended for use with `ivy-ignore-buffers'."
      (let ((current-major-mode (buffer-local-value 'major-mode (current-buffer)))
            (buf (get-buffer str)))
        (or (not buf)
            (not (eq (buffer-local-value 'major-mode buf)
                     current-major-mode)))))

    (defun dhnam/counsel-switch-buffer-within-same-major-mode ()
      "Switch to another buffer within the same major mode.
Display a preview of the selected ivy completion candidate buffer
in the current window."
      (interactive)
      (let ((ivy-update-fns-alist
             '((ivy-switch-buffer . counsel--switch-buffer-update-fn)))
            (ivy-unwind-fns-alist
             '((ivy-switch-buffer . counsel--switch-buffer-unwind)))

            (ivy-ignore-buffers (cons #'dhnam/ivy-ignore-buffers-with-different-major-mode
                                      ivy-ignore-buffers)))
        (ivy-read "Switch to buffer: " #'internal-complete-buffer
                  :keymap ivy-switch-buffer-map
                  :preselect (buffer-name (other-buffer (current-buffer)))
                  :action #'ivy--switch-buffer-action
                  :matcher #'ivy--switch-buffer-matcher
                  :caller 'ivy-switch-buffer)))

    )

  (progn
    ;; swiper within highlited strings
    (require 'cl-lib)

    (comment
      (defun dhnam/swiper-over-highlights-simple ()
        (interactive)
        (let ((original-swiper--candidates (symbol-function 'swiper--candidates)))
          (cl-letf (((symbol-function 'swiper--candidates)
                     (lambda ()
                       (let ((pattern (mapconcat #'car hi-lock-interactive-patterns "\\|")))
                         (cl-remove-if-not (lambda (x) (string-match-p pattern x))
                                           (funcall original-swiper--candidates))))))
            (swiper)))))


    (defun dhnam/swiper-over-highlights (&optional initial-input)
      (interactive)
      (let ((original-swiper--candidates (symbol-function 'swiper--candidates))
            (pattern (mapconcat #'car hi-lock-interactive-patterns "\\|")))
        (cl-letf (((symbol-function 'swiper--candidates)
                   (lambda ()
                     (cl-remove-if-not (lambda (x) (string-match-p pattern x))
                                       (funcall original-swiper--candidates)))))
          (let ((candidates (swiper--candidates)))
            (swiper--init)
            (setq swiper-invocation-face
                  (plist-get (text-properties-at (point)) 'face))
            (let ((preselect
                   (save-excursion
                     (search-forward-regexp pattern nil t)
                     (let* ((current-line-value (current-line))
                            (candidate-line-numbers (mapcar (lambda (x) (cadr (text-properties-at 0 x)))
                                                            candidates))
                            (preselect-line-num (cl-find-if (lambda (x) (<= current-line-value x))
                                                            candidate-line-numbers)))
                       (- (length candidate-line-numbers)
                          (length (member preselect-line-num candidate-line-numbers))))))
                  (minibuffer-allow-text-properties t)
                  res)
              (unwind-protect
                  (and
                   (setq res
                         (ivy-read
                          "Swiper: "
                          candidates
                          :initial-input initial-input
                          :keymap swiper-map
                          :preselect preselect
                          :require-match t
                          :action #'swiper--action
                          :re-builder #'swiper--re-builder
                          :history 'swiper-history
                          :extra-props (list :fname (buffer-file-name))
                          :caller 'swiper))
                   (point))
                (unless (or res swiper-stay-on-quit)
                  (goto-char swiper--opoint))
                (isearch-clean-overlays)
                (unless (or res (string= ivy-text ""))
                  (cl-pushnew ivy-text swiper-history))
                (setq swiper--current-window-start nil)
                (when swiper--reveal-mode
                  (reveal-mode 1)))))))))

  (defun dhnam/swiper-within-region (&optional initial-input)
    "`isearch-forward' with an overview.
When non-nil, INITIAL-INPUT is the initial search pattern."
    (interactive)
    (if (use-region-p)
        (save-restriction
          (deactivate-mark t)
          (narrow-to-region (region-beginning) (region-end))
          (swiper initial-input))
      (swiper initial-input))))


(provide 'dhnam-counsel)
