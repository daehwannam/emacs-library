
(defvar dhnam-key-swap/key-pairs
  '(;; ijkl
    ("C-i" "C-p")
    ("C-j" "C-b")
    ("C-k" "C-n")
    ("C-l" "C-f")

    ("C-p" "C-i")
    ("C-b" "C-l")
    ("C-n" "C-j")
    ("C-f" "C-k")

    ("M-i" "M-p")
    ("M-j" "M-b")
    ("M-k" "M-n")
    ("M-l" "M-f")

    ("M-p" "M-i")
    ("M-b" "M-l")
    ("M-n" "M-j")
    ("M-f" "M-k")

    ("C-M-i" "C-M-p")
    ("C-M-j" "C-M-b")
    ("C-M-k" "C-M-n")
    ("C-M-l" "C-M-f")

    ("C-M-p" "C-M-i")
    ("C-M-b" "C-M-l")
    ("C-M-n" "C-M-j")
    ("C-M-f" "C-M-k")

    ("C-c C-i" "C-c C-p")
    ("C-c C-j" "C-c C-b")
    ("C-c C-k" "C-c C-n")
    ("C-c C-l" "C-c C-f")

    ("C-c C-p" "C-c C-i")
    ("C-c C-b" "C-c C-l")
    ("C-c C-n" "C-c C-j")
    ("C-c C-f" "C-c C-k")

    ;; aesr
    ;; ("C-r" "C-a")
    ;; ("C-a" "C-r")

    ;; ("M-r" "M-a")
    ;; ("M-a" "M-r")

    ;; ("C-M-r" "C-M-a")
    ;; ("C-M-a" "C-M-r")

    ;; hv
    ("C-h" "C-v")
    ("C-v" "C-h")

    ("M-h" "M-v")
    ("M-v" "M-h")

    ("C-M-h" "C-M-v")
    ("C-M-v" "C-M-h"))

  "Pairs of <from> and <to> keys")

(defvar dhnam-key-swap/char-pairs
  '(;; ijkl
    ("i" "p")
    ("j" "b")
    ("k" "n")
    ("l" "f")

    ("p" "i")
    ("b" "l")
    ("n" "j")
    ("f" "k"))

  "Pairs of <from> and <to> characters")

(defvar dhnam-key-swap/key-char-pairs
  (append dhnam-key-swap/key-pairs dhnam-key-swap/char-pairs)
  "Pairs of <from> and <to> keys and characters")

(defun dhnam-key-swap/get-all-keys (&optional pairs)
  "Non-overlapped keys"
  (unless pairs (setq pairs dhnam-key-swap/key-pairs))
  (let ((keys nil))
    (mapc (lambda (k) (add-to-list 'keys k))
          (apply #'append pairs))
    keys))

(defun dhnam-key-swap/get-key-command-alist (map &optional pairs)
  (unless pairs (setq pairs dhnam-key-swap/key-pairs))
  (mapcar (lambda (k) (cons k (lookup-key map (kbd k))))
          (dhnam-key-swap/get-all-keys pairs)))

(defvar dhnam-key-swap/map-to-alist nil)

(defun dhnam-key-swap/apply (map &optional pairs)
  (unless pairs (setq pairs dhnam-key-swap/key-pairs))
  (unless (assoc map dhnam-key-swap/map-to-alist #'eq)
    (let ((key-command-alist (dhnam-key-swap/get-key-command-alist map pairs)))
      (mapc
       (lambda (pair)
         (let ((from (car pair))
               (to (cadr pair)))
           (define-key map (kbd from) (cdr (assoc to key-command-alist)))))
       pairs)
      (push (cons map key-command-alist) dhnam-key-swap/map-to-alist))))

(defun dhnam-key-swap/apply-maps (maps &optional pairs)
  (unless pairs (setq pairs dhnam-key-swap/key-pairs))
  (mapc (lambda (map) (dhnam-key-swap/apply map pairs))
        (mapcar #'symbol-value (remove-if (lambda (x) (not (boundp x))) maps))))

(defun dhnam-key-swap/recover (map &optional pairs)
  (unless pairs (setq pairs dhnam-key-swap/key-pairs))
  (let ((map-key-command-alist (assoc map dhnam-key-swap/map-to-alist #'eq)))
    (when map-key-command-alist
      (let ((map (car map-key-command-alist))
            (key-command-alist (cdr map-key-command-alist)))
        (mapc
         (lambda (pair)
           (let ((from (car pair))
                 ;; (to (cadr pair))
                 )
             (define-key map (kbd from) (cdr (assoc from key-command-alist)))))
         pairs)
        (assoc-delete-all map dhnam-key-swap/map-to-alist #'eq)))))

(defun dhnam-key-swap/recover-maps (maps &optional pairs)
  (unless pairs (setq pairs dhnam-key-swap/key-pairs))
  (mapc (lambda (map) (dhnam-key-swap/recover map pairs))
        (mapcar #'symbol-value (remove-if (lambda (x) (not (boundp x))) maps))))

(comment
  ;; Example

  (progn
    (require 'dhnam-key-swap)
    (dhnam-key-swap/apply-maps '(global-map paredit-mode-map lisp-interaction-mode-map company-mode-map))
    (dhnam-key-swap/recover-maps '(global-map paredit-mode-map lisp-interaction-mode-map company-mode-map)))

  (progn
    (require 'dhnam-key-swap)
    (dhnam-key-swap/apply-maps '(doc-view-mode-map pdf-view-mode-map) dhnam-key-swap/key-char-pairs)
    (dhnam-key-swap/recover-maps '(doc-view-mode-map pdf-view-mode-map) dhnam-key-swap/key-char-pairs)))


(provide 'dhnam-key-swap)
