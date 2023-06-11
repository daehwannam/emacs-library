
;;; yassnippet tip
;;; https://www.emacswiki.org/emacs/Yasnippet

;;; Example of custom yassnippet
;;; https://pragmaticemacs.wordpress.com/2016/01/04/smart-text-templates-with-yasnippet/

(progn
  (defvar dhnam/yasnippet-snippets-dir "~/.emacs.d/dhnamlib/yasnippet-snippets")
  (add-to-list 'yas-snippet-dirs 'dhnam/yasnippet-snippets-dir))

(provide 'dhnam-yasnippet)
