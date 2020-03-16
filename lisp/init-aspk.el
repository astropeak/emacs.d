(require 'f)
(require 's)

(defun aspk-code-reading-get-projects ()
  "a org file under org-directory / code-reading is a project"
  (let* ((dir (concat org-directory "/code-reading"))
         (files (f-files dir nil t)))
    (mapcar (lambda (x) (file-name-base x)) files)
    )
  )


(defun aspk-create-new-project ()
  (let ((project helm-input))
    (message "project: %s" project)
    (aspk-code-reading-add-to-org-templates project)
    )
  )

(defun aspk-code-reading ()
  (interactive)
  ;; (let ((helm-quit-if-no-candidate 'aspk-create-new-project))
  (let ((aspk-called nil))
    ;; (setq helm-quit-if-no-candidate 'aspk-create-new-project)
    (helm-other-buffer `((name . "project")
                         (candidates . aspk-code-reading-get-projects)
                         (action . (lambda (cand)
                                     (aspk-code-reading-add-to-org-templates cand)
                                     (setq aspk-called t)
                                     )))
                       "project")
    (unless aspk-called (aspk-create-new-project))))



(defun  aspk-code-reading-add-to-org-templates (project)
  (setq org-capture-templates 
        (cons (aspk-code-reading-org-capture-template project)
              (cl-loop for x in org-capture-templates if (not (string-equal (car x) aspk-code-reading-key))
                       collect x))
        )
  )



(setq aspk-code-reading-key "c")
(setq aspk-code-reading-dir (concat org-directory "/code-reading"))

(defun aspk-code-reading-org-capture-template (project)
  `(,aspk-code-reading-key ,project entry
                           (file+headline
                            ,(concat aspk-code-reading-dir "/" project ".org")
                            "Captured")
                           ;; "* %^{Title}\n  %U\n  %i\n\n  %F\n\n%?"
                           (function aspk-code-reading-create-a-snippet)
                           :empty-lines-before 1
                           ))


;; (aspk-code-reading-org-capture-template "kaldi")


(defun aspk-get-current-buffer-file-name ()
  (replace-regexp-in-string (getenv "HOME") "~" (or buffer-file-name (buffer-name))))

;; (defun pns-create-a-snippet ()
(defun aspk-code-reading-create-a-snippet ()
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



(defun aspk-delete-demo-version-text ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (setq aspk-tmp 0)
    (while (re-search-forward "(DEMO VERSION!) " (point-max) t)
      ;; here (match-string 1) is the matched text by first (), (match-string 0) is th whole matched data.
      ;; add processing codes here
      ;; you can replace the matched text with another text
      (replace-match "")
      (incf aspk-tmp)
      )
    (save-buffer)
    (message "Deleted %s occurences" aspk-tmp)
    ))


(provide 'init-aspk)