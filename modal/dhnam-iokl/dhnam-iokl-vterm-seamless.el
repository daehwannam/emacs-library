

(eval
 `(progn
    (with-eval-after-load 'vterm-seamless
      (let ((map vterm-seamless-mode-map))
        (define-key map (kbd ,dhnam-iokl/activation-key) nil)))

    (comment
      (with-eval-after-load 'vterm-seamless
        (clone-hydra dhnam-iokl-vterm-seamless-copy dhnam-iokl-base
          ,dhnam-iokl/plist-1

          "iokl"

          ("S" vtsl/end-of-buffer)
          ("SPC" (vterm-copy-mode 1) :exit t)
          ;; (,dhnam-iokl/quit-key vtsl/copy-mode-exit :exit t)
          (,dhnam-iokl/quit-key nil "quit") ; it defines `dhnam-iokl-vterm-seamless-copy/nil'
          ("RET" vtsl/copy-mode-exit :exit t)
          ("<return>" vtsl/copy-mode-exit :exit t))

        (progn
          (defun dhnam-iokl-vterm-seamless-copy/copy-mode-exit ()
            (interactive)
            (when (and vterm-copy-mode
                       (= (line-number-at-pos (point)) vtsl/last-cursor-line-num))
              (dhnam-iokl-vterm-seamless-copy/nil)
              (dhnam/set-cursor-color dhnam-iokl/default-cursor-color)))

          (advice-add 'vtsl/trigger-copy-mode-exit :before 'dhnam-iokl-vterm-seamless-copy/copy-mode-exit))

        (defun dhnam-iokl-vterm-seamless-copy/body-after-previous-line ()
          (interactive)
          (previous-line)
          (dhnam-iokl-vterm-seamless-copy/body))

        (progn
          ;; make vtsl/copy-mode-then-dhnam-iokl-vterm-seamless-copy/body-after-previous-line
          (vtsl/copy-mode-then 'dhnam-iokl-vterm-seamless-copy/body-after-previous-line)
          (assert (fboundp 'vtsl/copy-mode-then-dhnam-iokl-vterm-seamless-copy/body-after-previous-line)))

        (let ((map vterm-seamless-mode-map))
          (define-key map (kbd ,dhnam-iokl/activation-key)
            (vtsl/copy-mode-then 'dhnam-iokl-vterm-seamless-copy/body-after-previous-line)))

        ;; disable any hint message
        (hydra-set-property 'dhnam-iokl-vterm-seamless-copy :verbosity 0)

        (comment
          (defhydra dhnam-iokl-vterm-seamless
            ,dhnam-iokl/plist-1

            "iokl"

            ("i" vtsl/copy-mode-then-dhnam-iokl-vterm-seamless-copy/body-after-previous-line :exit t)
            ("k" (vterm-send-key "b" nil nil t))
            ("l" (vterm-send-key "f" nil nil t))
            ("j" (vterm-send-key "b" nil t))
            (";" (vterm-send-key "f" nil t))

            ("q" dhnam/scroll-down-small)
            ("w" dhnam/scroll-up-small)
            ("a" (vterm-send-key "a" nil nil t))
            ("s" (vterm-send-key "e" nil nil t)))

          (let ((map vterm-seamless-mode-map))
            (define-key map (kbd ,dhnam-iokl/activation-key) 'dhnam-iokl-vterm-seamless/body))

          (let ((map vterm-seamless-copy-mode-map))
            (define-key map (kbd ,dhnam-iokl/activation-key) 'dhnam-iokl-vterm-seamless-copy/body))

          ;; disable any hint message
          (hydra-set-property 'dhnam-iokl-vterm-seamless :verbosity 0))))))


(provide 'dhnam-iokl-vterm-seamless)
