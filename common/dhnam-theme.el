

(defun dhnam/update-dhnam-manoj-dark ()
  (custom-set-faces
   ;; mode-line config
   '(mode-line ((t (:background "gray20" :foreground "gray80" :box nil))))
   '(mode-line-inactive ((t (:background "gray10" :foreground "gray50" :box nil :weight light)))))

  (comment
    (when (and dhnam/exwm-cmd-line-arg-passed (display-graphic-p))
      (custom-set-faces
       '(default ((t (:background "gray12")))))))

  (progn
    ;; org mode code block begin/end color
    ;; https://emacs.stackexchange.com/a/26783
    (custom-theme-set-faces 'user `(org-meta-line ((t (:foreground "goldenrod"))))))
  )

(defun dhnam/update-dark-theme ()
  (progn
    (require 'hl-line)
    (custom-set-faces
     ;; hl-line config (highlighting line without losing character color)
     (set-face-foreground 'hl-line nil)))

  (progn
    ;; tab-bar color
    (custom-set-variables
     '(tab-bar-close-button-show nil))
    (custom-set-faces
     '(tab-bar-mode t)
     '(tab-bar ((t (:inherit variable-pitch :background "black" :foreground "black"))))
     '(tab-bar-tab ((t (:inherit default :background "gray30" :foreground "white" :box (:line-width 2 :color "gray40")))))
     '(tab-bar-tab-inactive ((t (:inherit tab-bar-tab :background "black" :foreground "gray90"))))))

  (progn
    ;; disable fringe color
    (comment (fringe-mode 1))
    (set-face-attribute 'fringe nil
                        :foreground (face-foreground 'default)
                        :background (face-background 'default)))

  (progn
    (custom-set-faces
     ;; highlighting color
     ;;
     ;; e.g. highlight-symbol-at-point highlight-regexp
     '(hi-yellow ((t (:background "yellow1" :foreground "dodger blue"))))
     '(hi-pink ((t (:background "pink" :foreground "forest green"))))
     '(hi-green ((t (:background "green" :foreground "magenta"))))
     '(hi-blue ((t (:background "light blue" :foreground "red"))))
     '(hi-black ((t (:background "light purple" :foreground "green"))))
     '(hi-salmon ((t (:background "salmon" :foreground "blue"))))
     '(hi-aquamarine ((t (:background "aquamarine" :foreground "red")))))

    (progn
      ;; restrict colors
      (setq hi-lock-face-defaults '("hi-yellow" "hi-pink" "hi-green" "hi-blue" "hi-salmon" "hi-aquamarine"))))

  (progn
    ;; flymake color
    ;; https://groups.google.com/g/gnu.emacs.help/c/gJpdhosRByY
    (require 'flymake)
    (progn
	  ;; text color
	  (custom-set-faces
	   '(flymake-errline ((((class color)) (:foreground "red"))))
	   '(flymake-warnline ((((class color)) (:foreground "yellow")))))))

  (custom-set-faces
   ;; swiper use underline instead of highlight
   '(swiper-line-face ((t (:inherit nil :overline nil :underline t))))
   (comment
     ;; grayxx overlays highlighted colors
     '(swiper-line-face ((t (:inherit nil :background "gray25" :overline nil :underline nil)))))
   (comment
     ;; color-xxx is only working terminal
     '(swiper-line-face ((t (:inherit nil :background "color-236" :overline nil :underline nil))))))

  (progn
    (custom-set-faces
     '(highlight-indentation-face ((t (:background "gray30"))))))

  (when (fboundp 'ein:run)
    (custom-set-faces
     '(ein:cell-input-area ((t (:background "black"))))))

  (comment(custom-set-faces
           '(org-scheduled ((t (:foreground "PaleGreen"))))))

  (progn
    ;; vertico & consult config
    (custom-set-faces
     ;; '(vertico-current ((t (:inherit highlight :background "royal blue"))))
     ;; '(consult-preview-line ((t (:inherit highlight :background "royal blue"))))
     '(vertico-current ((t (:inherit nil :underline t))))
     '(consult-preview-line ((t (:inherit nil :underline t))))))

  (custom-set-faces
   ;; makefile comment indentation color
   '(makefile-space ((t (:background "gray25"))))))

(provide 'dhnam-theme)
