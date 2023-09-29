
(defun dhnam/vlf-re-search-forward-last ()
  (interactive)
  (vlf-re-search-forward (car regexp-history) 1))

(defun dhnam/vlf-re-search-backward-last ()
  (interactive)
  (vlf-re-search-backward (car regexp-history) 1))

(provide 'dhnam-vlf)
