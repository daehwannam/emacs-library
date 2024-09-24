
(defun dhnam/string-to-chars (str)
  ;; example
  ;; (setq chars (dhnam/string-to-chars "abcde"))
  ;; (setq char-strs (mapcar #'char-to-string chars))
  (append str nil))

(defun dhnam/string-trim (string)
  ;; http://ergoemacs.org/emacs/modernization_elisp_lib_problem.html
  "Remove white spaces in beginning and ending of STRING.
White space here is any of: space, tab, emacs newline (line feed, ASCII 10).
This function is deprecated as `string-trim' was introduced.
"
  (replace-regexp-in-string "\\`[ \t\n]*" "" (replace-regexp-in-string "[ \t\n]*\\'" "" string)))

(defun dhnam/string-starts-with (s begins)
  ;; https://www.emacswiki.org/emacs/ElispCookbook
  "Return non-nil if string S starts with BEGINS."
  (cond ((>= (length s) (length begins))
         (string-equal (substring s 0 (length begins)) begins))
        (t nil)))

(defun dhnam/string-get-all-matched (regexp str)
  (let ((last-match t)
        (matched-list nil)
        (start 0))
    (while (and last-match (< start (length str)))
      (string-match regexp str start)
      (setq last-match (match-string 0 str))
      (setq start (+ start (length last-match)))
      (push last-match matched-list))
    (reverse matched-list)))

(defun dhnam/string-split-by-lengths (str lengths &optional including-rest)
  (let ((splits nil)
        (start 0)
        (end nil))
    (dolist (length lengths)
      (setq end (+ start length))
      (push (substring-no-properties str start end) splits)
      (setq start end))
    (when including-rest
      (push (substring-no-properties str start) splits))
    (reverse splits)))

(defun dhnam-key-swap/string-to-char (s)
  (let ((k (kbd s)))
    (cond ((stringp k) (string-to-char k))
          ((vectorp k) (elt k 0)))))

(defun dhnam/insert-line (s)
  (insert s)
  (insert "\n"))

(defun dhnam/format-symbol (s &rest objects)
  ;; e.g. (dhnam/format-symbol 'aa-bb-%s-dd 'yeah-hoo)
  (intern (apply #'format (cons (symbol-name s) objects))))

(defun dhnam/get-matched-substring-no-properties (regexp string num)

  ;; e.g.
  ;; (let ((regexp "@[a-zA-z]+[ \n]*{[ \n]*\\(.+\\)[ \n]*,")
  ;;       (string "@inproceedings{BIB-ID, ...}"))
  ;;   (dhnam/get-matched-substring-no-properties regexp string 1))
  ;;
  ;; => BIB-ID

  (string-match regexp string)
  (match-string-no-properties num string))

(provide 'dhnam-string)
