org-mode

;; https://github.com/terlar/emacs-config/commit/f898f84c0f6222421e5dba4bca7c30c83610c90e
(title "#+title: " p n "#+author: " p n "#+language: " p n n)
(quote "#+begin_quote" n> r> n> "#+end_quote")
(example "#+begin_example" n> r> n> "#+end_example")
(center "#+begin_center" n> r> n> "#+end_center")
(comment "#+begin_comment" n> r> n> "#+end_comment")
(verse "#+begin_verse" n> r> n> "#+end_verse")
(src "#+begin_src " p n> r> n> "#+end_src" :post (org-edit-src-code))
(elisp "#+begin_src emacs-lisp :tangle yes" n> r> n "#+end_src" :post (org-edit-src-code))

(epkg "** " p n "#+begin_quote" n> r> n> "#+end_quote" n "#+begin_src emacs-lisp" n> r> n> "#+end_src" :post (org-edit-src-code))
