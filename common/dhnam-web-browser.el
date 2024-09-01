
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
  (defun dhnam/url-string-p (s)
    (string-match "[^[:blank:][:space:]]*://[^[:blank:][:space:]]*"
                  s))

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

  (comment (fset 'dhnam/app-command-open-web-browser 'dhnam/app-command-open-nyxt))
  (comment (fset 'dhnam/app-command-open-web-browser 'dhnam/app-command-open-firefox-private))
  (fset 'dhnam/app-command-open-web-browser 'dhnam/app-command-open-firefox)
  (fset 'dhnam/app-command-open-web-browser-private 'dhnam/app-command-open-firefox-private)

  (defun dhnam/app-command-open-nyxt (&optional url)
    (interactive)
    (dhnan/open-web-browser "nyxt" url))

  (progn
    (defvar dhnam/default-web-search-engine-list-file-path
      (concat dhnam/lib-root-dir "common/dependent/search-engines.lisp"))

    (defvar dhnam/web-search-engine-list-file-paths
      (list dhnam/default-web-search-engine-list-file-path))

    (defun dhnam/get-search-engines-from-file-path (file-path)
      (car (read-from-string (dhnam/get-string-from-file file-path))))

    (defun dhnam/get-search-engines-from-file-paths (file-paths)
      (apply 'append (mapcar 'dhnam/get-search-engines-from-file-path file-paths)))

    (defvar dhnam/web-search-engines
      (dhnam/get-search-engines-from-file-paths dhnam/web-search-engine-list-file-paths))

    (defun dhnam/update-web-search-engines ()
      (interactive)
      (setq dhnam/web-search-engines
            (dhnam/get-search-engines-from-file-paths dhnam/web-search-engine-list-file-paths)))

    (defun dhnam/search-query-to-browser (query open-web-browser &optional no-double-quote)
      (let* ((splits (split-string query " "))
             (search-engine-entry
              (assoc (car splits) dhnam/web-search-engines))
             (query-string nil)
             (url nil))
        (cond
         (search-engine-entry
          (setq query-string (string-join (cdr splits) " ")))
         ((and (= (length splits) 1) (dhnam/url-string-p query))
          (setq url query))
         (t
          (progn
            (setq search-engine-entry (assoc "gg" dhnam/web-search-engines))
            (setq query-string query))))

        (unless url
          (setq url (if (string-empty-p query-string)
                        (caddr search-engine-entry)
                      (let ((replaced-query (string-replace "~a" query-string (cadr search-engine-entry))))
                        (if no-double-quote
                            replaced-query
                          (concat "\"" replaced-query "\""))))))
        (funcall open-web-browser url)))

    (defvar dhnam/web-browser-query-history nil)

    (defun dhnam/read-web-search-query ()
      (interactive)
      (comment (candidates (mapcar (lambda (search-engine-tuple) (string-join search-engine-tuple " ")) dhnam/web-search-engines)))
      (comment (candidates (mapcar #'car dhnam/web-search-engines)))

      (let* ((candidates
              (let* ((max-shortcut-len (apply #'max (mapcar (lambda (tuple) (length (car tuple))) dhnam/web-search-engines)))
                     (format-str (concat "%-" (number-to-string max-shortcut-len) "s  %s")))
                ;; e.g. format-str = "%-12s  %s"
                (mapcar (lambda (tuple) (format format-str (car tuple) (cadr tuple))) dhnam/web-search-engines)))
             (ivy-output
              (cl-letf (((symbol-function 'ivy-done) 'ivy-immediate-done)
                        ((symbol-function 'ivy-insert-current) 'dhnam/ivy-insert-current-first))
                (ivy-read "Search query: " candidates :history 'dhnam/web-browser-query-history))))
        ivy-output))

    (defun dhnam/app-command-query-to-browser (&optional query)
      ;; (interactive (list (read-string "Search query: " nil 'dhnam/web-browser-query-history)))
      (interactive (list (dhnam/read-web-search-query)))
      (comment (interactive "sSearch query: "))
      (dhnam/search-query-to-browser query #'dhnam/app-command-open-web-browser)))

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
    (dhnan/open-web-browser "firefox --new-window" url))

  (defun dhnam/app-command-open-firefox-private (&optional url)
    (interactive)
    (dhnan/open-web-browser "firefox --private-window" url)))

(with-eval-after-load 'dhnam-exwm
  (defvar dhnam/exwm-text-insertion-delay 0.05)
  (defvar dhnam/exwm-return-insertion-delay 0.05)

  (defun dhnam/exwm-app-command-open-existing-firefox (url)
    (comment (exwm-input--fake-key (aref (kbd "<f6>") 0)))
    (exwm-input--fake-key (aref (kbd "C-l") 0))
    (dhnam/exwm-edit-send-text url dhnam/exwm-text-insertion-delay)
    (run-with-timer
     dhnam/exwm-return-insertion-delay nil
     (lambda () (exwm-input--fake-key (aref (kbd "<return>") 0)))))

  (defun dhnam/exwm-app-command-query-to-existing-browser (&optional query)
    ;; (interactive (list (read-string "Search query: " nil 'dhnam/web-browser-query-history)))
    (interactive (list (dhnam/read-web-search-query)))
    (comment (interactive "sSearch query: "))
    (dhnam/search-query-to-browser query #'dhnam/exwm-app-command-open-existing-firefox t)))

(provide 'dhnam-web-browser)
