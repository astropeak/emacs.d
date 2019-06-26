(setq *no-window* t)

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

;; On windows, the default HOME dir is C:/Users/your-name/AppData/Roaming,
;; so we may need to adjust the HOME dir
(when  (eq system-type 'windows-nt)
  (setenv "HOME" (format "C:/Users/%s" (getenv "USERNAME"))))

(load-file "~/OneDrive/Dropbox/project/emacs.d/init.el")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(bmkp-last-as-first-bookmark-file "~/.emacs.d/.bookmarks.el")
 '(package-selected-packages
   (quote
    (yasnippet yaml-mode writeroom-mode wgrep w3m unfill textile-mode tagedit switch-window string-edit sr-speedbar session scss-mode scratch sass-mode rvm robe rinari regex-tool rainbow-delimiters quack project-local-variables pretty-mode pomodoro pointback paredit page-break-lines org-fstree mwe-log-commands multiple-cursors multi-term move-text mic-paren maxframe markdown-mode magit lua-mode less-css-mode legalese json-mode js2-mode iedit idomenu ibuffer-vc htmlize hl-sexp haskell-mode gitignore-mode gitconfig-mode git-timemachine git-gutter ggtags fringe-helper flyspell-lazy flymake-sass flymake-ruby flymake-python-pyflakes flymake-lua flymake-jslint flymake-css flymake-coffee fancy-narrow expand-region exec-path-from-shell erlang emmet-mode elnode dsvn dropdown-list dired-details dired+ diminish csharp-mode crontab-mode cpputils-cmake company-c-headers company-anaconda company color-theme coffee-mode buffer-move bookmark+ bbdb auto-compile anaconda-mode ag ace-jump-mode)))
 '(session-use-package t nil (session)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(window-numbering-face ((t (:foreground "DeepPink" :underline "DeepPink" :weight bold))) t))
