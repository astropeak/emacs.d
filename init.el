
;; -*- coding: utf-8 -*-
(setq emacs-load-start-time (current-time))
(setq emacs-init-dir (file-name-directory load-file-name))
(message "emacs init directory : %s" emacs-init-dir)
(add-to-list 'load-path (expand-file-name (format "%slisp" emacs-init-dir)))

;; include aspk-package.
(setq aspk-package-dir (expand-file-name (concat emacs-init-dir "../aspk-code-base/elisp")))
(unless (file-exists-p aspk-package-dir) (signal 'error-symbol "Directory not exist: aspk-code-base. It should be put in the same directory as emacs.d"))
(add-to-list 'load-path aspk-package-dir)

;; load another version of org-mode
(setq org-src-dir (format "%s/site-lisp/org-mode" emacs-init-dir))
(add-to-list 'load-path (format "%s/lisp" org-src-dir))
(add-to-list 'load-path (format "%s/contrib/lisp" org-src-dir) t)


;;----------------------------------------------------------------------------
;; Which functionality to enable (use t or nil for true and false)
;;----------------------------------------------------------------------------
(setq *macbook-pro-support-enabled* t)
(setq *is-a-mac* (eq system-type 'darwin))
(setq *is-carbon-emacs* (and *is-a-mac* (eq window-system 'mac)))
(setq *is-cocoa-emacs* (and *is-a-mac* (eq window-system 'ns)))
(setq *win32* (eq system-type 'windows-nt) )
(setq *cygwin* (eq system-type 'cygwin) )
(setq *linux* (or (eq system-type 'gnu/linux) (eq system-type 'linux)) )
(setq *unix* (or *linux* (eq system-type 'usg-unix-v) (eq system-type 'berkeley-unix)) )
(setq *linux-x* (and window-system *linux*) )
(setq *xemacs* (featurep 'xemacs) )
(setq *emacs23* (and (not *xemacs*) (or (>= emacs-major-version 23))) )
(setq *is-emacs23* (and (not *xemacs*) (or (= emacs-major-version 23))) )
(setq *emacs24* (and (not *xemacs*) (or (>= emacs-major-version 24))) )
(setq *emacs25* (and (not *xemacs*) (or (>= emacs-major-version 25))) )
(setq *no-memory*
      (cond
       (*is-a-mac*
        ;;(< (string-to-number (nth 1 (split-string (shell-command-to-string "sysctl hw.physmem")))) 4000000000))
        t)
       (*linux* nil)
       (t nil)
       )
      )

;;----------------------------------------------------------------------------
;; Functions (load all files in defuns-dir)
;; Copied from https://github.com/magnars/.emacs.d/blob/master/init.el
;;----------------------------------------------------------------------------
(setq defuns-dir (expand-file-name (format "%s/defuns" emacs-init-dir)))
(dolist (file (directory-files defuns-dir t "\\w+"))
  (when (file-regular-p file)
    (load file)))
;;----------------------------------------------------------------------------
;; Load configs for specific features and modes
;;----------------------------------------------------------------------------

;; (require 'init-modeline)

;;----------------------------------------------------------------------------
;; Load configs for specific features and modes
;;----------------------------------------------------------------------------
(require 'cl-lib)
(require 'init-compat)
(require 'init-utils)
(require 'init-site-lisp) ;; Must come before elpa, as it may provide package.el

;; win32 auto configuration, assuming that cygwin is installed at "c:/cygwin"
(condition-case nil
    (when *win32*
      (setq cygwin-mount-cygwin-bin-directory "c:/cygwin/bin")
      (require 'setup-cygwin)
      ;; better to set HOME env in GUI
      ;; (setenv "HOME" "c:/cygwin/home/someuser")
      )
  (error
   (message "setup-cygwin failed, continue anyway")
   ))

(require 'idle-require)

(require 'init-elpa)
(require 'init-exec-path) ;; Set up $PATH
;; (require 'init-frame-hooks)
;; any file use flyspell should be initialized after init-spelling.el
;; actually, I don't know which major-mode use flyspell.
;; (require 'init-spelling)

;; (require 'init-xterm)
(require 'init-osx-keys)
;; (require 'init-gui-frames)
(require 'init-ido)
;; (require 'init-maxframe)
;; (require 'init-proxies)
(require 'init-dired)
(require 'init-isearch)
(require 'init-uniquify)
;; (require 'init-ibuffer)
;; (require 'init-flymake)
(require 'init-recentf)
;; (require 'init-smex)
(when (or *emacs24* *emacs25*) (require 'init-helm))
(require 'init-hippie-expand)
(require 'init-windows)
(require 'init-sessions)
(require 'init-fonts)
;; (require 'init-git)
;; (require 'init-crontab)
;; (require 'init-textile)
;; (require 'init-markdown)
;; (require 'init-csv)
;; (require 'init-erlang)
;; (require 'init-javascript)
;; (require 'init-css)
(require 'init-python-mode)
;; (require 'init-haskell)
;; (require 'init-ruby-mode)
(require 'init-elisp)
;; (unless *emacs25*
(require 'init-yasnippet)
;; )

;; Use bookmark instead
;; (require 'init-zencoding-mode)
;; (require 'init-cc-mode)
;; (require 'init-gud)
;; (require 'init-cmake-mode)
;; (require 'init-csharp-mode)
(require 'init-linum-mode)
;; (require 'init-which-func)
(require 'init-move-window-buffer)
;; (require 'init-gist)
;; (require 'init-moz)
;; (require 'init-gtags)
;; use evil mode (vi key binding)
(require 'init-evil)
;; (require 'init-sh)
;; (require 'init-ctags)
;; (require 'init-ace-jump-mode)
;; (require 'init-bbdb)
;; (require 'init-gnus)
;; (require 'init-lua-mode)
;; (require 'init-workgroups2)
;; (require 'init-term-mode)
;; (require 'init-web-mode)
;; (require 'init-sr-speedbar)
;; (require 'init-slime)
(when *emacs24* (require 'init-company))
;; stripe buffer shows different row colors for org table, but it also make org mode very slow for big org file. So I decide disabling it.
;; (require 'init-stripe-buffer)
;; (require 'init-eim) ;;  cannot be idle-required
(require 'init-hs-minor-mode)
(when *is-emacs23* (require 'init-anything))
(require 'init-scb)
;; C-c c is bind in evil-nerd-commenter, so put this after 
;; (when *emacs24*
(require 'init-org)
(require 'init-org-mime)

;; misc has some crucial tools I need immediately
(require 'init-misc)
(require 'init-wubi)
(require 'init-eink)

;; color theme
;; (require 'color-theme)
;;(require 'color-theme-molokai)
;; (color-theme-molokai)
;;(color-theme-aspk)
;; ;; This line must be after color-theme-molokai! Don't know why.
;; (setq color-theme-illegal-faces "^\\(w3-\\|dropdown-\\|info-\\|linum\\|yas-\\|font-lock\\)")
;; ;; (color-theme-select 'color-theme-xp)
;; ;; (color-theme-xp)

;; (setq idle-require-idle-delay 3)
;; (setq idle-require-symbols '(init-writting
;;                              init-lisp
;;                              init-keyfreq
;;                              init-elnode
;;                              init-doxygen
;;                              init-pomodoro
;;                              init-emacspeak
;;                              init-artbollocks-mode
;;                              init-emacs-w3m
;;                              init-semantic))
;; (idle-require-mode 1) ;; starts loading

;;----------------------------------------------------------------------------
;; Variables configured via the interactive 'customize' interface
;;----------------------------------------------------------------------------
(if (file-exists-p "~/.custom.el") (load-file "~/.custom.el"))

(when (require 'time-date nil t)
  (message "Emacs startup time: %d seconds."
           (time-to-seconds (time-since emacs-load-start-time)))
  )
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(bmkp-last-as-first-bookmark-file "~/.emacs.d/.bookmarks.el")
 '(session-use-package t nil (session)))
(when (not *no-window*)
  (custom-set-faces
   ;; custom-set-faces was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   ;; '(default ((t (:inherit nil :stipple nil :background "white" :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 140 :width normal :foundry "outline" :family "NSimSun"))))
   '(default ((t (:inherit nil :stipple nil :background "white" :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 180 :width normal :foundry "outline" ))))
   ;;'(window-numbering-face ((t (:foreground "DeepPink" :underline "DeepPink" :weight bold))) t)
   )
  )

;; This font looks quite good on windows
;; change the number following sans- to change font size.
(when *win32*
  (set-frame-font "-outline-Microsoft JhengHei-normal-normal-normal-sans-27-*-*-*-p-*-iso8859-1" nil t))

;; ;;; Local Variables:
;; ;;; no-byte-compile: t
;; ;;; End:
;; (put 'erase-buffer 'disabled nil)

;; added by astropeak
(setq default-directory "~/")
(load-file (format "%s/my-lisp.el" emacs-init-dir))
;; TODO: move the above two line into init-aspk
(require 'init-aspk)

;; disable menu bar mode, but either commands don't work
;; (call-interactively 'menu-bar-mode)
(menu-bar-mode -1)