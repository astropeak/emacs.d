;; This file include all settings for displaying for a eink.
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cursor ((t (:background "#ffffff"))))
 '(magit-item-highlight ((t (:background "white" :foreground "black"))))
 '(diredp-dir-priv ((t (:background "white" :foreground "black"))))
 '(eww-form-text ((t (:background "#eeeeee" :foreground "white" :box 1))))
 '(header-line ((t (:inherit mode-line))))
 '(helm-match ((t (:inherit region))))
 '(magit-branch ((t (:background "white" :foreground "LightSkyBlue1"))))
 '(region ((t (:background "#eeeeee" :foreground "white"))))
 '(sh-heredoc ((t (:foreground "black" :weight bold))))
 '(window-numbering-face ((t (:foreground "DeepPink" :underline "DeepPink" :weight bold))) t))

;; Disable font lock mode totally, because in a eink display, not need to use different style
(setq font-lock-global-modes nil)



(provide 'init-eink)