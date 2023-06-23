
;;; yassnippet tip
;;; https://www.emacswiki.org/emacs/Yasnippet

;;; Example of custom yassnippet
;;; https://pragmaticemacs.wordpress.com/2016/01/04/smart-text-templates-with-yasnippet/

(progn
  (defvar dhnam/yasnippet-snippets-dir (concat dhnam/lib-root-dir "yasnippet-snippets"))
  (add-to-list 'yas-snippet-dirs 'dhnam/yasnippet-snippets-dir))

(provide 'dhnam-yasnippet)
