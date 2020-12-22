
;; setup for org-roam, but failed...
;; the error is 'ol' could not been found. But I don't know how to install that package. 
(executable-find "sqlite3")
(when *win32*
  (add-to-list 'exec-path "C:/Users/fuqiang.luo/software/sqlite-tools-win32-x86-3340000")
  (add-to-list 'load-path "c:/Users/fuqiang.luo/.emacs.d/elpa/emacsql-3.0.0/")
  (add-to-list 'load-path "c:/Users/fuqiang.luo/.emacs.d/elpa/emacsql-sqlite3-1.0.2/")
  (add-to-list 'load-path "c:/Users/fuqiang.luo/.emacs.d/elpa/org-roam-1.2.3/"))


(setq org-roam-directory (format "%s/org-roam" org-directory))
(setq emacsql-sqlite3-executable (executable-find "sqlite3"))
;; (defalias 'f-descendant-of-p 'f-descendant-of?)

(require 'emacsql)
(require 'emacsql-sqlite3)
(require 'org-roam)

(org-roam-find-file)
