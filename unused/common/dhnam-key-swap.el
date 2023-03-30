
;; https://www.gnu.org/software/emacs/manual/html_node/efaq/Swapping-keys.html

(defvar dhnam-key-swap/key-pairs
  '(;; Remap keys
    ;; C- ijkl
    ("C-i" "C-p")
    ("C-j" "C-b")
    ("C-k" "C-n")
    ("C-l" "C-f")

    ;; ;; C-M- ijkl -> not working
    ;; ("C-M-i" "C-M-p")
    ;; ("C-M-j" "C-M-b")
    ;; ("C-M-k" "C-M-n")
    ;; ("C-M-l" "C-M-f")

    ;; Give other bindgins to the lost keys
    ("C-p" "C-i")
    ("C-b" "C-l")
    ("C-n" "C-j")
    ("C-f" "C-k")

    ;; ("C-M-p" "C-M-i")
    ;; ("C-M-b" "C-M-l")
    ;; ("C-M-n" "C-M-j")
    ;; ("C-M-f" "C-M-k")
    ))

(defun dhnam-key-swap/string-to-char (s)
  (let ((k (kbd s)))
    (cond ((stringp k) (string-to-char k))
          ((vectorp k) (elt k 0)))))

(defun dhnam-key-swap/apply ()
  (mapc
   (lambda (pair)
     (let ((from (car pair))
           (to (cadr pair)))
       (keyboard-translate (dhnam-key-swap/string-to-char from) (dhnam-key-swap/string-to-char to))))
   dhnam-key-swap/key-pairs))


(defun dhnam-key-swap/recover ()
  (mapc
   (lambda (pair)
     (let ((from (car pair))
           (to (cadr pair)))
       (keyboard-translate (dhnam-key-swap/string-to-char from) (dhnam-key-swap/string-to-char from))
       (keyboard-translate (dhnam-key-swap/string-to-char to) (dhnam-key-swap/string-to-char to))))
   dhnam-key-swap/key-pairs))
