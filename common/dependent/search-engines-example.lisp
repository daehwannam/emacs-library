;;; -*- eval: (dhnam/buffer-local-set-key (kbd "C-x C-s") 'dhnam/save-buffer-and-update-web-search-engines); -*-
;;; -*- eval: (dhnam/buffer-local-set-key-chord "xs" 'dhnam/save-buffer-and-update-web-search-engines); -*-

(
 ("gg" "https://www.google.com/search?q=~a" "https://www.google.com/")
 ("gt" "https://translate.google.co.kr/?sl=auto&tl=en&text=~a&op=translate" "https://translate.google.com")
 ("ddg" "https://duckduckgo.com/?q=~a&t=h_&ia=web" "https://duckduckgo.com")

 ("wiki" "https://en.wikipedia.org/w/index.php?search=~a" "https://en.wikipedia.org/")
 ("python3" "https://docs.python.org/3/search.html?q=~a" "https://docs.python.org/3")
 ("doi" "https://dx.doi.org/~a" "https://dx.doi.org/")
 ("cambridge-en" "https://dictionary.cambridge.org/dictionary/english/~a" "https://dictionary.cambridge.org/dictionary/english")

 ("dblp" "https://dblp.org/search?q=~a" "https://dblp.uni-trier.de/")
 ("arxiv" "https://arxiv.org/search/?query=~a&searchtype=all&abstracts=show&order=-announced_date_first" "https://arxiv.org/")
 ("scholar" "https://scholar.google.com/scholar?hl=en&as_sdt=0%2C5&q=~a&btnG=" "https://scholar.google.com/")

 ("reddit-emacs" "https://www.reddit.com/r/emacs/search/?q=~a&restrict_sr=1&sr_nsfw=" "https://www.reddit.com/r/emacs/")
 ("reddit-lisp" "https://www.reddit.com/r/lisp/search/?q=~a&restrict_sr=1&sr_nsfw=" "https://www.reddit.com/r/lisp/")
 ("reddit-python" "https://www.reddit.com/r/python/search/?q=~a&restrict_sr=1&sr_nsfw=" "https://www.reddit.com/r/python/")

 ("about:config" "about:config" "about:config")
 ("config" "about:config" "about:config")
 ("about:addons" "about:addons" "about:addons")
 ("addons" "about:addons" "about:addons")
 )
