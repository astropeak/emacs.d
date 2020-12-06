(require 'f)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)


;; C-return is bind to this originally
;; (org-insert-heading-respect-content &optional INVISIBLE-OK)
(define-key org-mode-map (kbd "<C-return>") 'org-meta-return)
(when (featurep 'evil-leader)
  (evil-leader/set-key "e" 'org-meta-return)
  (evil-leader/set-key "E" 'org-insert-heading))

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
(setq org-log-done 'time
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
      org-tags-column -60
      ;; org-tags-column 0
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
(setq org-directory (expand-file-name (or (and (boundp 'org-directory-local) org-directory-local)
                                          "~/OneDrive/Dropbox/org")))
(setq org-default-notes-file (concat org-directory "/notes.org"))
(define-key global-map "\C-cc" 'org-capture)

(setq org-capture-templates
      `(("t" "Todo" entry (file+headline ,(concat org-directory "/todo.org") "Tasks")
         "* TODO %?\n  %T\n  %i\n  %a")
        ("a" "Appt" entry (file+headline ,(concat org-directory "/todo.org") "Appt")
         "* %^{Appt}\n  %^T%? \n %a")
        ("h" "Howto" entry (file+headline "" "Howto")
         "* %^{Title}\n  %T\n\n  %? \n  ======\n%i\n  %a \n")
        ("e" "Experience & Tips" entry (file+headline "" "Experience & Tips")
         "* %^{Title}\n  %T\n\n  %? \n  ======\n  %i\n  %a \n")
        ("l" "English learning" entry (file+headline "" "English learning")
         "* %^{Title}\n  %?")
        ("w" "Web development" entry (file+headline ,(concat org-directory "/webdev.org") "Random notes") "* %^{Title}\n  %T\n  %?")
        ("m" "Misc" entry (file+headline "" "Miscellaneous") "* %^{Title}\n  %T\n  %i\n\n  %?")
        ("p" "Perl" entry (file+headline ,(concat org-directory "/perl.org") "Random notes") "* %^{Title}\n  %T\n  %?")
        ("j" "Journal" entry (file+olp+datetree ,(concat org-directory "/journal.org"))

         (function aspk-org-caputre-journal)
         ;; "* %^{Title}\n  %T\n  %? \n\n  %a\n"
         :empty-lines-before 1
         :tree-type week
         )))

;; copied from aspk-code-reading-create-a-snippet 
(defun aspk-org-caputre-journal ()
  (let ((lang (pns-get-current-mode))
        (region-str (if mark-active (buffer-substring-no-properties (region-beginning) (region-end)) "")))
    (if (equal region-str "")
        "* %?"
      (format "* %s: %%?\n\n  #+file %s:%s\n  #+begin_src %s\n%s\n  #+end_src\n\n"
              (file-name-nondirectory (or buffer-file-name (buffer-name)))
              (aspk-get-current-buffer-file-name)
              (line-number-at-pos (region-beginning))
              lang
              (if (equal region-str "")
                  "  "
                ;; delete empty lines in beginning and end of region-str
                ;; TODO: below line is buggy, so commente ti for now
                ;; (setq region-str (replace-regexp-in-string "^[ \t\n]*\\(.*\\)[ \t]*$" "\\1" region-str))
                (pns-indent-src-code-string (s-trim region-str) lang 2))))))



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
(setq org-agenda-span 'day)
(setq org-deadline-warning-days 7)
(setq org-agenda-show-all-dates nil)
(setq org-agenda-skip-deadline-if-done t)
(setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-start-on-weekday 1)
(setq org-reverse-note-order nil)
(setq org-fast-tag-selection-single-key (quote expert))
(setq org-log-note-state 'time)
(setq org-todo-log-states 'time)
(setq org-agenda-dim-blocked-tasks 'invisible)
;; (setq org-agenda-dim-blocked-tasks 't)
(setq org-enforce-todo-dependencies t)
(setq org-agenda-include-diary nil)  ;; do not include diary

;; (setq org-todo-keywords
;;       '((sequence "TODO(t@)" "STARTED(s@/!)" "DEFERED(f@/!)" "CHECK(v@/!)" "|" "DONE(d@/!)")
;;         (sequence "WAITING(w@/!)" "SOMEDAY(S@/!)" "PROJECT(P@/!)" "|" "CANCELLED(c@/!)")))

(setq org-todo-keywords
      '((sequence "TODO(t)" "STARTED(s)" "DEFERED(f)" "CHECK(v" "|" "DONE(d)")
        (sequence "WAITING(w)" "SOMEDAY(S)" "PROJECT(P)" "|" "CANCELLED(c)")))

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

(setq aspk-tmp-all-org-files
      (let ((dir (concat org-directory "/code-reading")))
        (when (file-exists-p dir)
          (cl-loop for f in (f-files dir nil t)
                   if (and (or (s-ends-with-p ".org" f)
                               (s-ends-with-p ".org_archive" f))
                           ;; DONE: add starts not with .
                           (not (s-starts-with-p "." f))
                           )
                   collect f))))

(setq org-agenda-files (append (list
                                (concat org-directory "/todo.org")
                                (concat org-directory "/journal.org")
                                (concat org-directory "/investment.org")
                                (concat org-directory "/book.org")
                                (concat org-directory "/cerence.org")
                                (concat org-directory "/me.org")
                                (concat org-directory "/../project/resume/resume.org")
                                ;; (concat org-directory "/notes.org")
                                )
                               aspk-tmp-all-org-files
                               ))

;; (setq org-agenda-files aspk-tmp-all-org-files)
;; (setq org-agenda-files (list (concat org-directory "/journal.org")))


;; fix bug: tags are not displayed fully in agenda view due to the actual width of Chinese character is wider.
;; (setq org-agenda-tags-column (- (min (- (window-text-width) 4) 100)))
(setq org-agenda-tags-column (- (- (window-text-width) 4) ))

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
   (shell         . t) ;; ob-sh=>ob-shell in new org version
   ;; (scala      . t) ;; ob-scale.el not exists in new org version
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

;; (add-to-list 'org-drawers "TODO")
;; (add-to-list 'org-drawers "sidenote")

(defun aspk-org-time-stamp ()
  "Like org-time-stamp, but just insert the current timestamp without prompting"
  (interactive)
  (insert (format-time-string (org-time-stamp-format 'long nil) (org-current-effective-time)))
  )
;;; overwrite the old org-time-stamp keybinding
(define-key org-mode-map "\C-c." 'aspk-org-time-stamp)
(define-key org-mode-map (kbd "C-c C-.") 'aspk-org-time-stamp)


;; define tag hierarchy
;; (setq org-tag-persistent-alist
;;       '(
;;         (:startgrouptag)
;;         ("asr" . ?a)
;;         (:grouptags)
;;         ("kaldi" . ?k)
;;         (:endgrouptag)

;;         (:startgrouptag)
;;         ("emacs" . ?e)
;;         (:grouptags)
;;         ("orgmode")
;;         ("wubi")
;;         (:endgrouptag)

;;         ("work" . ?w)
;;         ("body" . ?b)
;;         ("wow" . ?o)
;;         ("career" . ?c)
;;         ("home" . ?h)   ;; for things that can be done at home
;;         ("parenting" . ?p)
;;         ("family" . ?f)
;;         ("investment" . ?i)

;;         ))


;; '("ALLTAGS" "BLOCKED" "CLOCKSUM" "CLOCKSUM_T" "CLOSED" "DEADLINE" "FILE"
;; "ITEM" "PRIORITY" "SCHEDULED" "TAGS" "TIMESTAMP" "TIMESTAMP_IA" "TODO")
(setq org-columns-default-format "%TIMESTAMP %75ITEM %ALLTAGS %TODO %PRIORITY")


(org-defkey org-agenda-mode-map "S" 'org-agenda-schedule)

(when (featurep 'evil-leader)
  (evil-leader/set-key "a" 'org-agenda-list)
  (evil-leader/set-key "t" 'org-todo)
  (evil-leader/set-key "d" 'org-set-tags-command)
  )


(defun aspk-org-get-all-tags-in-agenda-view ()
  "get all tags in agenda view"
  ;; (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((tags nil))
      (while (not (eolp))
        ;; (message "%s" (org-get-at-bol 'tags))
        (mapcar (lambda (x) (add-to-list 'tags x)) (org-get-at-bol 'tags))
        (forward-line))
      ;; (message "all tags: %s" tags)
      tags)))


;; (setq src-block-name "main")
;; (setq filename "~/org/tags.org")

(defun aspk-execute-org-src-block (filename src-block-name)
  "Load the given src-block-name in filename"
  (interactive)
  (with-current-buffer (find-file-noselect filename)
    ;; (save-excursion
    (org-show-all)
    (goto-char (point-min))
    (while (re-search-forward (concat org-babel-src-name-regexp src-block-name) nil t)
      (save-excursion  ;; below line might change point
        (ignore-errors (org-babel-execute-src-block))))
    (save-buffer)
    ;; )
    )
  )

(defun aspk-org-load-tags ()
  (interactive)
  (aspk-execute-org-src-block (concat org-directory "/tags.org") "main")
  )

(aspk-org-load-tags)

(setq aspk-org-electric-insert-time-stamp-chars '(10))
(defun aspk-org-electric-insert-time-stamp ()
  "Insert a time stamp when newline is pressed on a headline"
  (when (and
         (save-excursion (forward-line -1) (outline-on-heading-p))
         (memq last-command-event aspk-org-electric-insert-time-stamp-chars))
    (indent-according-to-mode)
    (aspk-org-time-stamp)
    (org-return-indent)
    ))
;; (remove-hook 'post-self-insert-hook #'aspk-org-electric-insert-time-stamp))
(add-hook 'post-self-insert-hook #'aspk-org-electric-insert-time-stamp)

(setq org-agenda-custom-commands
      '(("n" "Agenda and all TODOs"
         ((tags "vcjobs" (org-agenda-sorting-strategy timestamp-up)))
         )))


;; Load aspk-org-month-agenda from ~/org/sources/aspk-org.org
(let ((file (concat org-directory "/sources/aspk-org.org")))
  (when (file-exists-p file)
    (aspk-execute-org-src-block file "main")))


;; disable undo-tree mode in org-mode
;; (add-to-list 'undo-tree-incompatible-major-modes 'org-mode)


(provide 'init-org)
