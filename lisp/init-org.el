(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)

;; {{ export org-mode in Chinese into PDF
;; @see http://freizl.github.io/posts/tech/2012-04-06-export-orgmode-file-in-Chinese.html
;; and you need install texlive-xetex on different platforms
;; To install texlive-xetex:
;;    `sudo USE="cjk" emerge texlive-xetex` on Gentoo Linux
(setq org-latex-to-pdf-process
      '("xelatex -interaction nonstopmode -output-directory %o %f"
        "xelatex -interaction nonstopmode -output-directory %o %f"
        "xelatex -interaction nonstopmode -output-directory %o %f"))
;; }}

(if (and *is-a-mac* (file-exists-p "/Applications/LibreOffice.app/Contents/MacOS/soffice"))
    (setq org-export-odt-convert-processes '(("LibreOffice" "/Applications/LibreOffice.app/Contents/MacOS/soffice --headless --convert-to %f%x --outdir %d %i"))))

;; @see https://gist.github.com/mwfogleman/95cc60c87a9323876c6c
(defun narrow-or-widen-dwim ()
  "If the buffer is narrowed, it widens. Otherwise, it narrows to region, or Org subtree."
  (interactive)
  (cond ((buffer-narrowed-p) (widen))
        ((region-active-p) (narrow-to-region (region-beginning) (region-end)))
        ((equal major-mode 'org-mode) (org-narrow-to-subtree))
        (t (error "Please select a region to narrow to"))))

;; Various preferences
(setq org-log-done t
      org-completion-use-ido t
      org-edit-src-content-indentation 0
      org-edit-timestamp-down-means-later t
      org-agenda-start-on-weekday nil
      org-agenda-span 14
      org-agenda-include-diary t
      org-agenda-window-setup 'current-window
      org-fast-tag-selection-single-key 'expert
      org-export-kill-product-buffer-when-displayed t
      org-export-odt-preferred-output-format "doc"
      org-tags-column 80
      ;; org-startup-indented t
      )

;; Refile targets include this file and any file contributing to the agenda - up to 5 levels deep
(setq org-refile-targets (quote ((nil :maxlevel . 5) (org-agenda-files :maxlevel . 5))))
;; Targets start with the file name - allows creating level 1 tasks
(setq org-refile-use-outline-path (quote file))
;; Targets complete in steps so we start with filename, TAB shows the next level of targets etc
(setq org-outline-path-complete-in-steps t)





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org clock
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Change task state to STARTED when clocking in
(setq org-clock-in-switch-to-state "STARTED")
;; Save clock data and notes in the LOGBOOK drawer
(setq org-clock-into-drawer t)
;; Removes clocked tasks with 0:00 duration
(setq org-clock-out-remove-zero-time-clocks t)

;; Show the clocked-in task - if any - in the header line
(defun sanityinc/show-org-clock-in-header-line ()
  (setq-default header-line-format '((" " org-mode-line-string " "))))

(defun sanityinc/hide-org-clock-from-header-line ()
  (setq-default header-line-format nil))

(add-hook 'org-clock-in-hook 'sanityinc/show-org-clock-in-header-line)
(add-hook 'org-clock-out-hook 'sanityinc/hide-org-clock-from-header-line)
(add-hook 'org-clock-cancel-hook 'sanityinc/hide-org-clock-from-header-line)

(eval-after-load 'org-clock
  '(progn
     (define-key org-clock-mode-line-map [header-line mouse-2] 'org-clock-goto)
     (define-key org-clock-mode-line-map [header-line mouse-1] 'org-clock-menu)))

(eval-after-load 'org
  '(progn
     (require 'org-clock)
                                        ; @see http://irreal.org/blog/?p=671
     (setq org-src-fontify-natively t)
     (require 'org-fstree)
     (defun soft-wrap-lines ()
       "Make lines wrap at window edge and on word boundary,
        in current buffer."
       (interactive)
       (setq truncate-lines nil)
       (setq word-wrap t)
       )
     (add-hook 'org-mode-hook '(lambda ()
                                 (setq evil-auto-indent nil)
                                 (soft-wrap-lines)
                                 ))))

(defadvice org-open-at-point (around org-open-at-point-choose-browser activate)
  (let ((browse-url-browser-function
         (cond ((equal (ad-get-arg 0) '(4))
                'browse-url-generic)
               ((equal (ad-get-arg 0) '(16))
                'choose-browser)
               (t
                (lambda (url &optional new)
                  (w3m-browse-url url t))))))
    ad-do-it))

;; {{ org2nikola set up
(setq org2nikola-output-root-directory "~/projs/blog.binchen.org")
(setq org2nikola-use-google-code-prettify t)
(setq org2nikola-prettify-unsupported-language
      '(elisp "lisp"
              emacs-lisp "lisp"))
;; }}


;; org capture
(require 'org-capture)
(setq org-directory "~/Dropbox/org")
(setq org-default-notes-file (concat org-directory "/notes.org"))
(define-key global-map "\C-cc" 'org-capture)

(setq org-capture-templates
      '(("t" "Todo" entry (file+headline (concat org-directory "/todo.org") "Tasks")
         "* TODO %?\n  %U\n  %i\n  %a")
        ("a" "Appt" entry (file+headline (concat org-directory "/todo.org") "Appt")
         "* %^{Appt}\n  %^T%? \n %a")
        ("h" "Howto" entry (file+headline "" "Howto")
         "* %^{Title}\n  %U\n\n  %? \n  ======\n%i\n  %a \n")
        ("e" "Experience & Tips" entry (file+headline "" "Experience & Tips")
         "* %^{Title}\n  %U\n\n  %? \n  ======\n  %i\n  %a \n")
        ("l" "English learning" entry (file+headline "" "English learning")
         "* %^{Title}\n  %?")
        ("w" "Web development" entry (file+headline (concat org-directory "/webdev.org") "Random notes") "* %^{Title}\n  %U\n  %?")
        ("m" "Misc" entry (file+headline "" "Miscellaneous") "* %^{Title}\n  %U\n  %i\n\n  %?")
        ("p" "Perl" entry (file+headline (concat org-directory "/perl.org") "Random notes") "* %^{Title}\n  %U\n  %?")
        ("j" "Journal" entry (file+datetree (concat org-directory "/journal.org"))
         "* %^{Title}\n  %U\n  %? \n\n  %a\n")))

(add-to-list 'org-capture-mode-hook
             (lambda ()
               (evil-insert-state)))

;; (defadvice org-add-note (before my-addings 'activate)
;;   (message "************"))
;; (advice-add 'org-add-note :after (lambda ()
;;                                     (evil-insert-state)))

;; (add-function :after (symbol-function 'org-add-note) (lambda ()
;;                                                         "I will enter insert state after org-add-note done, using change state"
;; (evil-change-state 'insert)))

;; (custom-set-variables
;; '(org-agenda-files (quote ("~/todo.org")))
;; '(org-default-notes-file "~/notes.org")
(setq org-agenda-span 7)
(setq org-deadline-warning-days 14)
(setq org-agenda-show-all-dates t)
(setq org-agenda-skip-deadline-if-done t)
(setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-start-on-weekday nil)
(setq org-reverse-note-order t)
(setq org-fast-tag-selection-single-key (quote expert))
(setq org-log-note-state 'note)
(setq org-todo-log-states 'note)
(setq org-agenda-dim-blocked-tasks 'invisible)
(setq org-enforce-todo-dependencies t)

(setq org-todo-keywords
      '((sequence "TODO(t@/!)" "STARTED(s@/!)" "DELAYED(f@/!)" "CHECK(v@/!)" "|" "DONE(d@/!)")
        (sequence "WAITING(w@/!)" "SOMEDAY(S@/!)" "PROJECT(P@/!)" "|" "CANCELLED(c@/!)")))

;; org mobile
(setq org-mobile-directory "~/Box Sync/mobileOrg")
(setq org-mobile-inbox-for-pull "~/Dropbox/org/frmo-mobile.org")
;; no need to set org-mobile-files, it will be set to (org-agenda-files) when pushing first time.

;; modify org-agenda-mode-map
;; ref: /usr/share/emacs/24.4/lisp/org/org-agenda.el.gz
(require 'org-agenda)
(org-defkey org-agenda-mode-map "j" 'org-agenda-next-line)
(org-defkey org-agenda-mode-map "k" 'org-agenda-previous-line)
(org-defkey org-agenda-mode-map "p"        'org-agenda-goto-date)
(org-defkey org-agenda-mode-map "n"        'org-agenda-capture)
(org-defkey org-agenda-mode-map "l" 'org-agenda-do-date-later)
(org-defkey org-agenda-mode-map "h" 'org-agenda-do-date-earlier)

(setq org-agenda-files (list (concat org-directory "/todo.org")))

;; always use sticky agenda
(org-toggle-sticky-agenda 1)

(defadvice org-agenda-schedule (after aspk-add-appt activate)
  "Add a appt when schedule a HH:MM time for a todo"
  (org-agenda-to-appt t)
  (org-agenda-redo))

(defadvice org-add-log-note (after aspk-enter-evil-insert-state activate)
  "When add a note, enter evil insert state"
  (evil-insert-state)
  (aspk-set-mode-line-color))

;; below line deactivate all advices added to org-add-log-note
;; (ad-deactivate 'org-add-log-note)


(setq org-clocktable-defaults
      (list
       :maxlevel 5
       :lang (or (org-bound-and-true-p org-export-default-language) "en")
       :scope 'file
       :block 'today
       :wstart 1
       :mstart 1
       :tstart nil
       :tend nil
       :step nil
       :stepskip0 nil
       :fileskip0 nil
       :tags nil
       :emphasize nil
       :link nil
       :narrow '40!
       :indent t
       :formula nil
       :timestamp nil
       :level nil
       :tcolumns nil
       :formatter nil))

;; active Babel languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '((perl . t)
   (emacs-lisp . t)
   (python . t)
   (java . t)
   (C . t) ;;both for C and C++
   (js . t)
   (sh         . t)
   (scala      . t)
   (clojure    . t)
   (python     . t)
   (ruby       . t)
   (dot        . t)
   (css        . t)
   (plantuml   . t)))

;; supress query before execute code block
(setq org-confirm-babel-evaluate nil)

;; org-return-indent works much better than org-return
(define-key org-mode-map (kbd "RET") 'org-return-indent)
(setq org-agenda-search-view-always-boolean t)

;; org2blog setup
(require  'metaweblog)
(require 'org2blog-autoloads)
(setq org2blog/wp-blog-alist
      '(("cnblogs"
         :url "http://rpc.cnblogs.com/metaweblog/astropeak"
         :username "astropeak"
         :password "ngjg9uemc@cb%"
         :default-title "Hello World"
         :default-categories ("org2blog" "emacs")
         :tags-as-categories nil)))

(defun peak-org-capture-journal ()
  (interactive)
  (org-capture nil "j"))

;; THis problem happens on mac, emacs 25.
(when *emacs25*
  (require 'remove-url-http-error))

(provide 'init-org)
