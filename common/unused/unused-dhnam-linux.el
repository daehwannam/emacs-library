
(comment
  (defun dhnam/split-shell-output-columns (str)
    (let* ((lines (cl-remove-if (lambda (line) (= (length (string-trim line)) 0)) (split-string str "\n")))
           (first-line (car lines))
           (rest-lines (cdr lines))
           (col-lengths (mapcar #'length (dhnam/string-get-all-matched "[^ ]+ *" first-line)))
           (col-lengths-except-last (reverse (cdr (reverse col-lengths))))
           (max-line-length (apply #'max (mapcar #'length rest-lines)))
           )

      ;; In the first argument of `format',
      ;; %% means the raw percent sign.

      (mapcar
       (lambda (line)
         (mapcar
          #'string-trim
          (dhnam/string-split-by-lengths
           (format (format "%%-%ss" max-line-length) line)
           col-lengths-except-last t)))
       rest-lines)))

  (comment
    ;; Example `dhnam/split-shell-output-columns' when the input is from
    ;; $ lsblk -o name,mountpoint,label,size,uuid | grep -v loop
    (dhnam/split-shell-output-columns
     "
NAME   MOUNTPOINT                          LABEL   SIZE UUID
sda                                              238.5G 
├─sda1                                               1M 1E22-3914
├─sda2 /boot/efi                                   977M B276-650A
├─sda3 /boot                                       977M f19e836e-7228-4e8e-97fa-9e6569b36e92
├─sda4 [SWAP]                                      3.8G 5d447635-3731-43fc-8eda-a13b2f6f8ab4
├─sda5 /home                                     130.4G 702ce02e-7ca9-46fe-b1e2-4512d5717945
└─sda6 /                                         102.4G 0f88d3a9-af65-4255-9289-33c5aae37482
")

    ;; Bug:
    ;; It only works when columns are left-aligned
    )

  (defun dhnam/delimit-shell-output-columns (str delimiter)
    (string-join
     (mapcar
      (lambda (row-tokens) (string-join row-tokens delimiter))
      (dhnam/split-shell-output-columns str))
     "\n")))

(defun dhnam/get-lsblk-info-list ()
  (mapcar
   (lambda (line)
     (split-string line "|"))
   (split-string
    (shell-command-to-string
     (concat dhnam/lib-root-dir "common/dependent/lsblk-partition.sh"))
    "\n")))

(defun dhnam/get-portable-dev-info-list ()
  "Get pairs of device disks and device names"

  (let ((command-output
         (string-trim
          (shell-command-to-string
           (concat dhnam/lib-root-dir "common/dependent/ls-portable-dev.sh")))))
    (when (> (length command-output) 0)
      (mapcar
       (lambda (line)
         (string-split
          ;; "\\1" in `replace-regexp-in-string' means the content the 1st parenthesis pair
          (replace-regexp-in-string
           "\"\\([^\"]*\\)\"" "\\1"
           line)))
       (split-string
        command-output
        "\n")))))

(defun dhnam/get-device-partitions ()
  (cl-remove-if
   #'null
   (mapcar
    (lambda (token-seq)
      (let ((name-token))
        ;; e.g. "└─sdb1" "├─sdb2"
        (when (string-match "^\\(├─\\|└─\\)\\(.+\\)" (car token-seq))
          (setq name-token (match-string 2 (car token-seq)))
          (cons name-token (cdr token-seq)))))
    (dhnam/get-lsblk-info-list))))


(defun dhnam/get-portable-partition-info-list (&optional portable-dev-info-list)
  ;;
  ;; e.g.
  ;; (dhnam/get-portable-partition-info-list '(("sda" "DeviceName")))
  ;;
  (let* ((portable-dev-info-list (or portable-dev-info-list
                                     (dhnam/get-portable-dev-info-list))))
    (cl-remove-if
     #'null
     (mapcar
      (lambda (token-seq)
        (let* ((partition (car token-seq))
               (portable-dev-info
                (cl-find-if
                 (lambda (dev-info)
                   (let ((dev-disk (car dev-info)))
                     (when (string-match (concat "^" (car dev-info)) partition)
                       dev-info)))
                 portable-dev-info-list)))
          (when portable-dev-info
            (append token-seq (cdr portable-dev-info)))))
      (dhnam/get-device-partitions)))))


(defun dhnam/mount-portable-partition ()
  (let* ((portable-dev-info-list (dhnam/get-portable-dev-info-list))
         (partition-info-list (dhnam/get-portable-partition-info-list portable-dev-info-list)))
    
    ;; partition-info-list
    ;; TODO
    ))

(provide 'unused-dhnam-linux)
