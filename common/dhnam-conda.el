
(defvar dhnam/conda-command (or (getenv "CONDA") "conda"))

(progn
  (defun dhnam/insert-source-conda ()
    (interactive)
    (insert (format "source $(%s info --base)/etc/profile.d/conda.sh" dhnam/conda-command)))

  (defun dhnam/dired-conda-env-activate (env-name)
    (interactive (list (dhnam/get-conda-activate-env)))
    (let ((dir-path (dhnam/string-trim
                     (shell-command-to-string
                      (format "echo $(%s info --base)/envs/%s/etc/conda/activate.d" dhnam/conda-command env-name)))))
      ;; conda environment doesn't have the directory "$(conda info --base)/envs/%s/etc" by default
      (make-directory dir-path t)
      (dired dir-path)))

  (defun dhnam/get-conda-activate-env ()
    (if (tramp-tramp-file-p default-directory)
        (dhnam/get-conda-activate-env--no-check)
      (dhnam/get-conda-activate-env--check)))

  (defun dhnam/get-conda-activate-env--check ()
    (completing-read "Environment name: " (pyvenv-virtualenv-list)
                     nil t nil 'pyvenv-workon-history nil nil))

  (defun dhnam/get-conda-activate-env--no-check ()
    (read-string "Environment name: "
                 nil 'pyvenv-workon-history))

  (defun dhnam/insert-conda-activate-env (env-name)
    (interactive (list (dhnam/get-conda-activate-env)))
    (insert "%s activate " dhnam/conda-command env-name))

  (comment
    (defun dhnam/conda-env-create (env-name python-version &optional cmd-eval-fn)
      (interactive (list (read-string "Environment name: " nil 'dhnam/conda-new-env-name-history)
                         (read-string "Python version: " "3" 'dhnam/python-version-history)))
      (let ((cmd (format "%s create -y -n %s python=%s" dhnam/conda-command env-name python-version)))
        (funcall (or cmd-eval-fn #'shell-command) cmd))))

  (defun dhnam/conda-env-remove (env-name &optional cmd-eval-fn)
    (interactive (list (dhnam/get-conda-activate-env)))
    (when (y-or-n-p (format "Are you sure you want to remove \"%s\"" env-name))
      (let ((cmd (format "%s env remove -n %s" dhnam/conda-command env-name)))
        (comment (start-process-shell-command cmd nil cmd))
        (comment (shell-command cmd))
        (funcall (or cmd-eval-fn #'shell-command) cmd))))

  (defvar dhnam/python-debugger-install-command
    "pip install ipdb git+https://github.com/pdbpp/pdbpp.git"
    "Install ipbd and pdbpp"))

(progn
  (defvar dhnam/conda-new-env-name-history nil)
  (defvar dhnam/python-version-history nil))

(defmacro dhnam/define-conda-commands (package name run-command prefix-map mode-key-map)
  `(with-eval-after-load ',package
     (defun ,(dhnam/format-symbol 'dhnam/%s-send-conda-activate name) (env-name)
       "This command is defined by `dhnam/define-conda-commands'"
       (interactive (list (dhnam/get-conda-activate-env)))
       (,run-command (format "%s activate %s" dhnam/conda-command env-name)))

     (defun ,(dhnam/format-symbol 'dhnam/%s-send-conda-deactivate name) ()
       "This command is defined by `dhnam/define-conda-commands'"
       (interactive)
       (,run-command (format "%s deactivate" dhnam/conda-command)))

     (defun ,(dhnam/format-symbol 'dhnam/%s-send-conda-env-create name) (env-name python-version)
       "This command is defined by `dhnam/define-conda-commands'"
       (interactive (list (read-string "Environment name: " nil 'dhnam/conda-new-env-name-history)
                          (read-string "Python version: " "3" 'dhnam/python-version-history)))
       (,run-command (format "%s create -y -n %s python=%s" dhnam/conda-command env-name python-version)))

     (defun ,(dhnam/format-symbol 'dhnam/%s-send-conda-env-remove name) (env-name)
       "This command is defined by `dhnam/define-conda-commands'"
       (interactive (list (dhnam/get-conda-activate-env)))
       (when (y-or-n-p (format "Are you sure you want to remove \"%s\"" env-name))
         (,run-command (format "%s env remove -n %s" dhnam/conda-command env-name))))

     (defun ,(dhnam/format-symbol 'dhnam/%s-install-python-debugger name) ()
       "This command is defined by `dhnam/define-conda-commands'"
       (interactive)
       (,run-command dhnam/python-debugger-install-command))

     (let ((map (make-sparse-keymap)))
       (define-key map (kbd "a") ',(dhnam/format-symbol 'dhnam/%s-send-conda-activate name))
       (define-key map (kbd "d") ',(dhnam/format-symbol 'dhnam/%s-send-conda-deactivate name))
       (define-key map (kbd "c") ',(dhnam/format-symbol 'dhnam/%s-send-conda-env-create name))
       (define-key map (kbd "r") ',(dhnam/format-symbol 'dhnam/%s-send-conda-env-remove name))

	   (defvar ,prefix-map map
	     "Keymap for conda in %s.")

       (fset ',prefix-map ,prefix-map)

       (when (package-installed-p 'key-chord)
         (key-chord-define ,mode-key-map "qn" ',prefix-map)))))

(provide 'dhnam-conda)
