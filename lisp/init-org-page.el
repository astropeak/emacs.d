(require 'org-page)

(setq op/repository-directory (concat (file-name-directory (directory-file-name emacs-init-dir)) "astropeak.github.io"))

(setq op/site-domain "https://astropeak.github.io/")

;; ;;; for commenting, you can choose either disqus, duoshuo or hashover
;; (setq op/personal-disqus-shortname "your_disqus_shortname")
;; (setq op/personal-duoshuo-shortname "your_duoshuo_shortname")
;; (setq op/hashover-comments t)
;; ;;; the configuration below are optional
;; (setq op/personal-google-analytics-id "your_google_analytics_id")