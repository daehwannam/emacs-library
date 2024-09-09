
(defun dhnam/eww-new (url &optional arg)
  "Make new eww buffer"
  (interactive
   (let* ((uris (eww-suggested-uris))
	      (prompt (concat "Enter URL or keywords"
			              (if uris (format " (default %s)" (car uris)) "")
			              ": ")))
     (list (read-string prompt nil 'eww-prompt-history uris))))

  (let ((default-prefix-arg 4))
    (eww url default-prefix-arg)))

(progn
  ;; web browser commands
  (defun dhnam/url-string-p (str)
    (string-match "[^[:blank:][:space:]]*://[^[:blank:][:space:]]*"
                  str))

  (defun dhnam/url-candidate-p (str)
    "Roughly check whether a string can be a URL candidate."
    (or (dhnam/url-string-p str)
        (and (not (string-match " " str)) ; a candidate URL should not contain a space character
             (or (string-match "\\." str) ; e.g. google.com
                 (string-match "^/" str)  ; matching a file path. e.g. /tmp/test.txt
                 ))))

  (defun dhnam/get-web-search-query-url-string (query-string)
    (cond
     ((string-empty-p query-string)
      "https://www.google.com/")
     ((dhnam/url-string-p query-string) ; when query is an URL
      query-string)
     (t
      (format "\"https://www.google.com/search?q=%s\"" query-string))))

  (defun dhnan/open-web-browser (browser-command &optional url)
    (let ((web-search-query-url-string
           (cond
            (url url)
            ((use-region-p)
             (deactivate-mark)
             (dhnam/get-web-search-query-url-string
              (buffer-substring-no-properties (region-beginning) (region-end))))
            (t ""))))
      (start-process-shell-command "web-browser" nil
                                   (concat browser-command " " web-search-query-url-string))))

  (defun old-dhnam/app-command-open-web-browser ()
    (interactive)
    (start-process-shell-command "web-browser" nil "nyxt")
    (comment (start-process-shell-command "web-browser" nil "qutebrowser"))
    (comment (start-process-shell-command "web-browser" nil "firefox -new-window"))
    (comment (start-process-shell-command "web-browser" nil "google-chrome --app=https://www.google.com/"))
    (comment (start-process-shell-command "web-browser" nil "firefox -new-window https://www.google.com/"))
    (comment (start-process-shell-command "web-browser" nil "google-chrome --app=https://www.google.com/ --start-fullscreen"))
    (comment (start-process-shell-command "web-browser" nil "google-chrome --new-window"))
    (comment (start-process-shell-command "web-browser" nil "xdg-open https://")))

  (comment
    (defun old-dhnam/app-command-open-web-browser-incognito ()
      (interactive)
      (start-process-shell-command "web-browser" nil "qutebrowser ':open -p'")
      (comment (start-process-shell-command "web-browser" nil "firefox --private-window"))
      (comment (start-process-shell-command "web-browser" nil "firefox --private-window https://www.google.com/"))
      (comment (start-process-shell-command "web-browser" nil "google-chrome --new-window google.com --incognito"))
      (comment (start-process-shell-command "web-browser" nil "google-chrome --new-window google.com --incognito --start-fullscreen"))
      (comment (start-process-shell-command "web-browser" nil "google-chrome --new-window --incognito"))
      (comment (start-process-shell-command "web-browser" nil "xdg-open https://"))))

  (progn
    (comment (fset 'dhnam/app-command-open-web-browser 'dhnam/app-command-open-nyxt))
    (comment (fset 'dhnam/app-command-open-web-browser 'dhnam/app-command-open-firefox-private))
    (fset 'dhnam/app-command-open-web-browser 'dhnam/app-command-open-firefox)
    (fset 'dhnam/app-command-open-web-browser-private 'dhnam/app-command-open-firefox-private))

  (defun dhnam/app-command-open-nyxt (&optional url)
    (interactive)
    (dhnan/open-web-browser "nyxt" url))

  (progn
    (defvar dhnam/primary-web-search-engine-list-file-path
      (concat dhnam/lib-root-dir "common/dependent/search-engines-example.lisp"))

    (defvar dhnam/web-search-engine-list-file-paths
      (list dhnam/primary-web-search-engine-list-file-path))

    (defun dhnam/get-search-engines-from-file-path (file-path)
      (when (file-exists-p file-path)
        (car (read-from-string (dhnam/get-string-from-file file-path)))))

    (defun dhnam/get-search-engines-from-file-paths (file-paths)
      (apply 'append (mapcar 'dhnam/get-search-engines-from-file-path file-paths)))

    (defvar dhnam/web-search-engines
      (dhnam/get-search-engines-from-file-paths dhnam/web-search-engine-list-file-paths))

    (defun dhnam/open-primary-web-search-engine-list-file ()
      (interactive)
      (find-file dhnam/primary-web-search-engine-list-file-path))

    (defvar dhnam/default-web-search-engine-name "gg")
    (defvar dhnam/default-web-search-engine-entry
      '("gg" "https://www.google.com/search?q=~a" "https://www.google.com/"))

    (defun dhnam/update-web-search-engines ()
      (interactive)
      (setq dhnam/web-search-engines
            (dhnam/get-search-engines-from-file-paths dhnam/web-search-engine-list-file-paths)))

    (defun dhnam/save-buffer-and-update-web-search-engines ()
      (interactive)
      (save-buffer)
      (dhnam/update-web-search-engines))

    (defvar dhnam/web-query-placeholder "~a")

    (defun dhnam/search-query-to-browser (query open-web-browser)
      (let* ((splits (split-string query " "))
             (search-engine-entry
              (assoc (car splits) dhnam/web-search-engines))
             (query-string nil)
             (url nil))
        (cond
         (search-engine-entry
          (setq query-string (string-join (cdr splits) " ")))
         ((and (= (length splits) 1) (dhnam/url-candidate-p query))
          (setq url query))
         (t
          (progn
            (setq search-engine-entry (or (assoc dhnam/default-web-search-engine-name dhnam/web-search-engines)
                                          dhnam/default-web-search-engine-entry))
            (setq query-string query))))

        (unless url
          (setq url (if (string-empty-p query-string)
                        (caddr search-engine-entry)
                      (string-replace dhnam/web-query-placeholder query-string (cadr search-engine-entry)))))
        (funcall open-web-browser url)))

    (defun dhnam/surround-with-double-quotes (url)
      (comment (concat "\"" url "\""))
      (let ((new-url (string-replace "\"" "\\\"" url)))
        (concat "\"" new-url "\"")))

    (defvar dhnam/web-browser-query-history nil)
    (defvar dhnam/web-search-query-start-match t)

    (defun dhnam/read-from-pairs (pairs prompt immediate start-match)
      (interactive)

      (let* ((candidates
              (let* ((max-shortcut-len (apply #'max (mapcar (lambda (tuple) (length (car tuple))) pairs)))
                     (format-str (concat "%-" (number-to-string max-shortcut-len) "s  %s")))
                ;; e.g. format-str = "%-12s  %s" when max-shortcut-len = 12
                (mapcar (lambda (tuple) (format format-str (car tuple) (cadr tuple))) pairs)))
             (ivy-output
              (cl-letf (((symbol-function 'ivy-done) (if immediate 'ivy-immediate-done (symbol-function 'ivy-done)))
                        ((symbol-function 'ivy-insert-current) (if start-match
                                                                   'dhnam/ivy-insert-except-last-with-begin-symbol
                                                                 'dhnam/ivy-insert-except-last))
                        ((symbol-function 'ivy-partial) 'dhnam/ivy-partial-without-last))
                (ivy-read prompt candidates
                          :history 'dhnam/web-browser-query-history
                          :initial-input (if start-match "^" "")))))
        (if start-match
            (if (string= (substring-no-properties ivy-output 0 1) "^")
                (substring-no-properties ivy-output 1)
              ivy-output)
          ivy-output)))

    (defun dhnam/read-web-search-query ()
      (interactive)
      (dhnam/read-from-pairs dhnam/web-search-engines "Search query: " t dhnam/web-search-query-start-match))

    (defvar dhnam/primary-web-bookmark-list-file-path
      (concat dhnam/lib-root-dir "common/dependent/web-bookmarks-example.org"))

    (defvar dhnam/web-bookmark-list-file-paths
      (list dhnam/primary-web-bookmark-list-file-path))

    (defun dhnam/get-bookmarks-from-file-path (file-path)
      (when (file-exists-p file-path)
        (with-current-buffer (find-file-noselect file-path)
          (end-of-buffer)
          (let ((web-bookmarks nil)
                (found t))
            (while (setq found (re-search-backward org-link-any-re nil t))
              (let ((url (thing-at-point 'url))
                    (name (let ((raw-name
                                 (let ((end (point))
                                       (start (progn (move-beginning-of-line 1) (point))))
                                   (string-trim (buffer-substring-no-properties start end)))))
                            (if (string-match "^- [^ ]+" raw-name)
                                (string-trim-left (substring-no-properties raw-name 2))
                              raw-name)))
                    (outline-path (org-get-outline-path t)))
                (let ((bookmark-name (string-join (append outline-path (list name)) "/") ))
                  (push (list bookmark-name url) web-bookmarks))))
            web-bookmarks))))

    (defun dhnam/get-bookmarks-from-file-paths (file-paths)
      (apply 'append (mapcar 'dhnam/get-bookmarks-from-file-path file-paths)))

    (defvar dhnam/web-bookmarks
      (dhnam/get-bookmarks-from-file-paths dhnam/web-bookmark-list-file-paths))

    (defun dhnam/open-primary-web-bookmark-list-file ()
      (interactive)
      (find-file dhnam/primary-web-bookmark-list-file-path))

    (defun dhnam/update-web-bookmarks ()
      (interactive)
      (setq dhnam/web-bookmarks
            (dhnam/get-bookmarks-from-file-paths dhnam/web-bookmark-list-file-paths)))

    (defun dhnam/save-buffer-and-update-web-bookmarks ()
      (interactive)
      (save-buffer)
      (dhnam/update-web-bookmarks))

    (defvar dhnam/web-browser-bookmark-history nil)
    (defvar dhnam/web-bookmark-start-match nil)

    (defun dhnam/read-web-bookmark-entry ()
      (interactive)
      (let* ((raw-output (string-trim-left
                          (dhnam/read-from-pairs
                           dhnam/web-bookmarks "Bookmark: " nil dhnam/web-bookmark-start-match)))
             (splits (split-string raw-output " "))
             (last-split (car (last splits)))
             (except-last (string-trim-right
                           (substring-no-properties raw-output 0 (- (length raw-output) (length last-split))))))
        (list except-last last-split)))

    (comment
      (defun dhnam/app-command-query-to-browser (&optional query)
        ;; (interactive (list (read-string "Search query: " nil 'dhnam/web-browser-query-history)))
        (interactive (list (dhnam/read-web-search-query)))
        (comment (interactive "sSearch query: "))
        (dhnam/search-query-to-browser query #'dhnam/app-command-open-web-browser))

      (defun dhnam/app-command-query-to-browser-private (&optional query)
        ;; (interactive (list (read-string "Search query: " nil 'dhnam/web-browser-query-history)))
        (interactive (list (dhnam/read-web-search-query)))
        (comment (interactive "sSearch query: "))
        (dhnam/search-query-to-browser query #'dhnam/app-command-open-web-browser-private))

      (defun dhnam/app-command-open-bookmark (&optional bookmark-entry)
        (interactive (list (dhnam/read-web-bookmark-entry)))
        (let ((url (cadr bookmark-entry)))
          (dhnam/app-command-open-web-browser url)))

      (defun dhnam/app-command-open-bookmark-private (&optional bookmark-entry)
        (interactive (list (dhnam/read-web-bookmark-entry)))
        (let ((url (cadr bookmark-entry)))
          (dhnam/app-command-open-web-browser-private url))))

    (progn
      (defun dhnam/app-command-query-to-firefox (&optional query)
        (interactive (list (dhnam/read-web-search-query)))
        (dhnam/search-query-to-browser query #'dhnam/app-command-open-firefox))

      (defun dhnam/app-command-query-to-firefox-private (&optional query)
        (interactive (list (dhnam/read-web-search-query)))
        (let ((firefox-running (dhnam/exwm-firefox-running-p)))
          (if firefox-running
              (dhnam/search-query-to-browser query #'dhnam/app-command-delayed-open-firefox-private)
            (dhnam/search-query-to-browser query #'dhnam/app-command-open-firefox-private))))

      (defun dhnam/app-command-open-bookmark-in-firefox (&optional bookmark-entry)
        (interactive (list (dhnam/read-web-bookmark-entry)))
        (let ((url (cadr bookmark-entry)))
          (dhnam/app-command-open-firefox url)))

      (defun dhnam/app-command-open-bookmark-in-firefox-private (&optional bookmark-entry)
        (interactive (list (dhnam/read-web-bookmark-entry)))
        (let ((url (cadr bookmark-entry)))
          (let ((firefox-running (dhnam/exwm-firefox-running-p)))
            (if firefox-running
                (dhnam/app-command-delayed-open-firefox-private url)
              (dhnam/app-command-open-firefox-private url)))))))

  (defun dhnam/app-command-open-google-chrome (&optional url)
    (interactive)
    (dhnan/open-web-browser "google-chrome" url)
    (comment (dhnan/open-web-browser "google-chrome --app=https://www.google.com/")))

  (defun dhnam/app-command-open-google-chrome-incognito (&optional url)
    (interactive)
    (dhnan/open-web-browser "google-chrome --incognito" url)
    (comment (dhnan/open-web-browser "google-chrome --new-window google.com --incognito")))

  (defun dhnam/app-command-open-firefox (&optional url)
    (interactive)
    (dhnan/open-web-browser "firefox --new-window" (dhnam/surround-with-double-quotes url)))

  (defun dhnam/app-command-open-firefox-private (&optional url)
    (interactive)
    (dhnan/open-web-browser "firefox --private-window" (dhnam/surround-with-double-quotes url)))

  (defun dhnam/app-command-delayed-open-firefox (&optional url)
    (interactive)
    (dhnan/open-web-browser "firefox --new-window")
    (dhnam/exwm-app-command-open-link-with-existing-firefox url dhnam/firefox-app-open-delay))

  (defun dhnam/app-command-delayed-open-firefox-private (&optional url)
    (interactive)
    (dhnan/open-web-browser "firefox --private-window")
    (dhnam/exwm-app-command-open-link-with-existing-firefox url dhnam/firefox-app-open-delay)))

(with-eval-after-load 'dhnam-exwm
  (require 'exwm-edit)

  (defvar dhnam/firefox-app-open-delay 0.2)
  (defvar dhnam/firefox-text-insertion-delay 0.05)
  ;; (defvar dhnam/firefox-address-bar-delay 0.05)
  (defvar dhnam/firefox-address-bar-delay 0.05)
  (defvar dhnam/firefox-new-tab-delay 0.2)

  (defvar dhnam/firefox-address-bar-shortcut (kbd "C-l"))
  (comment (defvar dhnam/firefox-address-bar-shortcut (kbd "<f6>"))) ; (kbd "<f6>") does not work for firefox
  (defvar dhnam/firefox-copy-shortcut (kbd "C-c"))
  (defvar dhnam/firefox-new-tab-shortcut (kbd "C-t"))

  (defun dhnam/exwm-app-command-open-link-with-existing-firefox (url &optional initial-delay)
    (let ((delay (or initial-delay 0)))
      (dhnam/exwm-edit-send-key dhnam/firefox-address-bar-shortcut delay)
      (dhnam/exwm-edit-send-text url (setq delay (+ delay dhnam/firefox-address-bar-delay)))
      (dhnam/exwm-edit-send-key (kbd "<return>") (setq delay (+ delay dhnam/firefox-text-insertion-delay)))))

  (defun dhnam/exwm-app-command-query-to-existing-firefox (&optional query)
    ;; (interactive (list (read-string "Search query: " nil 'dhnam/firefox-query-history)))
    (interactive (list (dhnam/read-web-search-query)))
    (comment (interactive "sSearch query: "))
    (dhnam/search-query-to-browser query #'dhnam/exwm-app-command-open-link-with-existing-firefox))

  (comment
    (defun dhnam/exwm-app-command-open-address-bar ()
      (interactive)
      (dhnam/exwm-edit-send-key dhnam/firefox-address-bar-shortcut)))

  (defun dhnam/exwm-app-command-open-link-with-new-firefox-tab (url)
    (let ((delay 0))
      (dhnam/exwm-edit-send-key dhnam/firefox-new-tab-shortcut)
      (dhnam/exwm-edit-send-key dhnam/firefox-address-bar-shortcut (setq delay (+ delay dhnam/firefox-new-tab-delay)))
      (dhnam/exwm-edit-send-text url (setq delay (+ delay dhnam/firefox-address-bar-delay)))
      (dhnam/exwm-edit-send-key (kbd "<return>") (setq delay (+ delay dhnam/firefox-text-insertion-delay)))))

  (defun dhnam/exwm-app-command-query-to-new-firefox-tab (&optional query)
    ;; (interactive (list (read-string "Search query: " nil 'dhnam/firefox-query-history)))
    (interactive (list (dhnam/read-web-search-query)))
    (comment (interactive "sSearch query: "))
    (dhnam/search-query-to-browser query #'dhnam/exwm-app-command-open-link-with-new-firefox-tab))

  (defun dhnam/exwm-app-command-select-bookmark-in-existing-firefox (&optional bookmark-entry)
    (interactive (list (dhnam/read-web-bookmark-entry)))
    (let ((url (cadr bookmark-entry)))
      (dhnam/exwm-app-command-open-link-with-existing-firefox url)))

  (defun dhnam/exwm-app-command-select-bookmark-for-new-firefox-tab (&optional bookmark-entry)
    (interactive (list (dhnam/read-web-bookmark-entry)))
    (let ((url (cadr bookmark-entry)))
      (dhnam/exwm-app-command-open-link-with-new-firefox-tab url)))

  (progn
    (defvar dhnam/exwm-firefox-line-mode-map
      (let ((map (make-sparse-keymap)))
        (define-key map (kbd "C-l") #'dhnam/exwm-app-command-query-to-existing-firefox)
        (define-key map (kbd "M-l") #'dhnam/exwm-app-command-query-to-new-firefox-tab)
        (define-key map (kbd "C-m") #'dhnam/exwm-app-command-select-bookmark-in-existing-firefox)
        (define-key map (kbd "M-m") #'dhnam/exwm-app-command-select-bookmark-for-new-firefox-tab)
        (define-key map (kbd "M-L") #'dhnam/open-primary-web-search-engine-list-file)
        (define-key map (kbd "M-M") #'dhnam/open-primary-web-bookmark-list-file)

        map)
      "Keymap for `dhnam/exwm-firefox-mode'.")

    (define-minor-mode dhnam/exwm-firefox-mode
      "EXWM with Firefox"
      nil                            ; Initial value, nil for disabled
      :global nil
      :lighter " firefox"

      (if (eq exwm--selected-input-mode 'line-mode)
          (dhnam/exwm-firefox-line-enable)
        (dhnam/exwm-firefox-line-mode 0)))

    (define-minor-mode dhnam/exwm-firefox-line-mode
      "EXWM with Firefox"
      nil                            ; Initial value, nil for disabled
      :global nil
      :lighter " ff-line"
      :keymap dhnam/exwm-firefox-line-mode-map)

    (defun dhnam/exwm-firefox-p (&optional name)
      (member (or name exwm-class-name) '("Firefox" "firefox")))

    (defun dhnam/exwm-firefox-running-p ()
      (cl-member-if (lambda (x) (with-current-buffer x (dhnam/exwm-firefox-p))) (buffer-list)))

    (defun dhnam/exwm-firefox-enable ()
      (when (dhnam/exwm-firefox-p)
        (dhnam/exwm-firefox-mode 1)))

    (defun dhnam/exwm-firefox-line-enable ()
      (when (dhnam/exwm-firefox-p)
        (dhnam/exwm-firefox-line-mode 1)))

    (advice-add 'exwm-input-grab-keyboard :after (lambda (&rest args) (dhnam/exwm-firefox-line-enable)))
    (advice-add 'exwm-input-release-keyboard :after (lambda (&rest args) (dhnam/exwm-firefox-line-mode 0)))))

(provide 'dhnam-web-browser)
