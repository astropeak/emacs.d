
(setq pcs-snippet-dir
      (expand-file-name (format "%s/../aspk-code-snippets/snippets" emacs-init-dir)))
(defvar pcs--mode-names nil "All modes supported")
(defvar pcs--create-file-name-function nil
      "The function that create a snipplet file name")

(defun helm-yas-get-cmp-context ()
  "Return list (initial-input point-start point-end)
like `yas--current-key'"
  (if mark-active
      (values "" (region-beginning) (region-end))
    (values "" (point) (point))))

(defun pcs--get-src-block-mode ()
  (let (info lang)
    (when (and (eq major-mode 'org-mode)
               (fboundp 'org-edit-src-find-region-and-lang))
      (setq info (org-edit-src-find-region-and-lang)))

    (when info
      (setq lang (or (cdr (assoc (nth 2 info) org-src-lang-modes))
                     (nth 2 info)))
      (setq lang (if (symbolp lang) (symbol-name lang) lang))
      (setq lang-f (intern (concat lang "-mode")))
      )
    lang))

(setq pcs--change-mode-p nil)
(defun pcs--change-mode (func-name &rest args)
  (message "Enter function %s." func-name)
  (let ((mode (pcs--get-src-block-mode)))
    (when mode
      (message "Change to %s-mode" mode)
      (funcall (intern (concat mode "-mode")))
      (setq pcs--change-mode-p t)
      )))

(defun pcs--restore-mode-coolie ()
  (interactive)
  (let (old-flag)
    (message "Change to org-mode")
    (setq old-flag org-inhibit-startup-visibility-stuff)
    ;; avoid org file automatically collapsed
    (setq org-inhibit-startup-visibility-stuff t)
    (org-mode)
    (setq org-inhibit-startup-visibility-stuff old-flag)))

(defun pcs--restore-mode (func-name &rest args)
  (message "Exit function %s." func-name)
  (when pcs--change-mode-p
    (message "Change to org-mode")
    (pcs--restore-mode-coolie)
    (setq pcs--change-mode-p nil)))
(defun pcs--all-snippet-names (&optional mode)
  "Get all snippets's names in mode, as a list. If mode is missing, then for all modes."
  ;; '("string length" "loop array" "loop hash" "error handling")

  ;; process all file under directory pcs-snippet-dir with pattern *
  (require 'f)
  ;; (error "I am raised by error function explicitly")
  ;; body goes here. May raise an error, then will go to the error handler part
  (delete nil 
          (mapcar (lambda (file)
                    ;; create temp buffer without undo record or font lock. (more efficient)
                    ;; first space in temp buff name is necessary
                    (message "file=%s" file)

                    (condition-case *error-info*
                        (progn 
                          (set-buffer (get-buffer-create " myTemp"))
                          (insert-file-contents file nil nil nil t))
                      (error
                       (message "Error happened: %S" *error-info*)
                       ;; handler body goes here when error happens
                       (debug *error-info*)
                       (list "Error: Fail to get snippet name. Error info below" *error-info*)))

                    ;; process it ...
                    ;; (goto-char 0) ; move to begining of file's content (in case it was open)
                    ;; ... do something here
                    ;; (write-file fpath) ;; write back to the file
                    (goto-char (point-min))
                    (when (re-search-forward "^\s*#\s*name\s*:\s*\\(.*\\)" (point-max))
                      (prog1
                          (match-string 1)
                        (kill-buffer " myTemp"))))
                  (f-files (if (not mode) pcs-snippet-dir
                             (format "%s/%s-mode" pcs-snippet-dir mode))
                           nil t))))

(defun pcs--create-file-name-random-string (dir)
  "Create a new file name under dir using the random number string method"
  (let ((not-found t) (file))
    (while not-found
      (setq file (replace-regexp-in-string " " "0" (format "%16s" (random 10000000000000000))))
      (unless (file-exists-p (format "%s/%s" dir file))
        (setq not-found nil)))
    (format "%s/%s" dir file)
    ))

(defun pcs--create-file-name-date-time (dir)
  "Create a new file name under dir using the current date time"
  (let ((not-found t) (file))
    (while not-found
      (setq file (format-time-string "%Y%m%d-%H%M%S-%3N"))

      (unless (file-exists-p (format "%s/%s" dir file))
        (setq not-found nil)))
    (format "%s/%s" dir file)
    ))

(defun pcs--create-file-name (dir)
  "Create a new file name under dir "
  (funcall pcs--create-file-name-function dir))
(defun pcs-create-snippet ()
  (interactive)
  (let* ((selected-text (if mark-active (buffer-substring-no-properties (region-beginning) (region-end)) ""))
         (org-local-edit-mode (pcs--get-src-block-mode))
         (mode (or (helm-other-buffer
                    `(((name . "Current mode")
                       (candidates . ;; (replace-regexp-in-string "^\(\s*\)," "" content)
                                   ,(list (replace-regexp-in-string
                                           "-mode$" ""
                                           (format "%s"  (or (pcs--get-src-block-mode) major-mode)))))
                       (action . (lambda (c ) c))
                       (accept-empty . t))
                      ((name . "Which mode ?")
                       (candidates . pcs--mode-names)
                       (action . (lambda (c) c))
                       (accept-empty . t)))
                    "Which mode ")
                   helm-pattern))
         (name (or (helm-other-buffer
                    `((name . "What name ?")
                      (candidates . ,(pcs--all-snippet-names))
                      (action . (lambda (c) c)))
                    "What name")
                   helm-pattern))
         (dir (format "%s/%s-mode" pcs-snippet-dir mode))
         (file (pcs--create-file-name dir))
         (string-format "# -*- mode: snippet -*-\n#name : %s\n#contributor : %s\n#description : \n\n# --\n%s"))

    (if (or (string-equal mode "") (string-equal name ""))
        (message "mode or name empty, do't create.")
      (message "mode=%S name=%S, selected-text=%S" mode name selected-text)
      (unless (file-exists-p dir) (message "Creating dir: %s" dir) (mkdir dir t))
      (find-file file)
      (snippet-mode)
      (insert (format string-format name user-full-name selected-text))
      (save-buffer))))


(require 'aspk-advice)
(aspk/advice-add 'helm-yas-complete 'before 'pcs--change-mode)
(aspk/advice-add 'helm-yas-complete 'after 'pcs--restore-mode)
(setq pcs--mode-names '("c++" "c" "cc" "cmake" "csharp" "css" "emacs-lisp" "erlang" "html" "inf-ruby" "java" "javascript" "js" "js2" "js3" "jsp" "less-css" "lua" "nxml" "objc" "org" "perl" "python" "rhtml" "rspec" "ruby" "scala" "scss" "sh" "snippet" "web"))
(setq pcs--create-file-name-function 'pcs--create-file-name-date-time)
 (provide 'pcs)
