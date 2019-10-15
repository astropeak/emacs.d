
;; global
;;     #+NAME: global-definition

(require 'helm)
(require 'helm-utils)

(defvar eoh-base-dir (getenv "HOME") "The base dir for all files in `eoh-files', which store list of org files. By using this eoh-base-dir, cross platform org file path is possible")
(defvar eoh-files nil "Type: list; element: string(file full path name). the files should be searched for headings. This is the input to generate-headlines.")
(defvar eoh-headlines nil "Type: list; element: list(file-name, line-no, heading)")
(defvar eoh-perl-executable "perl")
(defvar eoh-home-dir (format "%s/%s" (getenv "HOME") "OneDrive/Dropbox/Apps/emacs-org-headlines") "the data dir for the eoh applicaton. All generated headlines will be saved in this dir")
(defvar eoh-headlines-dir (format "%s/headlines" eoh-home-dir) "the generated files by perl are all saved under this dir")
(defvar eoh-perl-script-name (format "%s/%s" (file-name-directory load-file-name) "make_headline.pl") "the perl script that generate the headline files")
(defvar eoh-files-file-name (format "%s/%s" eoh-home-dir "files") "The file is used to save the `eoh-files' variable")

;; interface functions
(defvar eoh-generate-headlines-function 'eoh-generate-headlines-update "The generate headline file function")
(defvar eoh-load-headlines-function 'eoh-load-headlines-update "The load headlines function")

;; config
;;     config the functon interfaces
;;     #+NAME: config

(defun eoh-config-update ()
  (interactive)
  (setq eoh-generate-headlines-function 'eoh-generate-headlines-update)
  (setq eoh-load-headlines-function 'eoh-load-headlines-update)
  )

(defun eoh-config-always ()
  (interactive)
  (setq eoh-generate-headlines-function 'eoh-generate-headlines-always)
  (setq eoh-load-headlines-function 'eoh-load-headlines-always)
  )

;; #+NAME: init

(defun eoh-init ()
  (cond
   ;; (*win32* (setq eoh-perl-executable "C:/Perl64/bin/perl.exe"))
   )

  ;; create the home dir
  (unless (file-exists-p eoh-home-dir)
    (message "Create eoh home dir: %s" eoh-home-dir)
    (mkdir eoh-home-dir t))

  ;; create the data dir
  (unless (file-exists-p eoh-headlines-dir)
    (message "Create eoh headlines dir: %s" eoh-headlines-dir)
    (mkdir eoh-headlines-dir t))

   ;; create the history file
  (unless (file-exists-p eoh-history-file-name)
         (aspk-util-dump-vars-to-file '(eoh-historys) eoh-history-file-name))

  )

;; util
;;     #+NAME: util

(defun aspk-util-file-name-to-string (name)
  "Convert a file full path name to a string without seperator so that it can be used as a base file name"
  (replace-regexp-in-string "/\\|\\\\" "-" name))

 (defun aspk-util-file-content-to-string (filePath)
   "Return filePath's file content."
   (with-temp-buffer
     (insert-file-contents filePath)
     (buffer-string)))

;; generate the headline file
;;    It works!! good
   
;;    #+NAME: generate-headlines

(defun eoh-generate-headlines-coolie (file)
  "Generate for one single file. File is relative path to `eoh-base-dir'"
  (let ((out (format "%s/%s" eoh-headlines-dir (aspk-util-file-name-to-string file))))
    (message "Generate headline files. Input: %s, output: %s" file out)
    (shell-command (format "%s %s  %s %s > %s" eoh-perl-executable eoh-perl-script-name eoh-base-dir file out))))


(defun eoh-generate-headlines-always (files)
  "always re generate"
  (mapcar 'eoh-generate-headlines-coolie files))

(defun eoh-org-file-newer-p (file)
  "Check if the org `file' is newer than the generated headline file"
  (file-newer-than-file-p (format "%s/%s" eoh-base-dir file)
                          (format "%s/%s" eoh-headlines-dir (aspk-util-file-name-to-string file))))

(defun eoh-generate-headlines-update (files)
  "Re generate only when org file is newer than the generated file"
  (mapcar (lambda (file)
            (when (eoh-org-file-newer-p file)
              (eoh-generate-headlines-coolie file)))
          files))

;; get the headlines from a file, and saved to ~eoh-headlines~, which is a list of (filename line-number headline-content).
;;      #+NAME: load-headlines

(defun eoh-load-headlines-coolier (file)
  "Load the headlines in file, and return that list"
  (message "Load headlines for file: %s" file)
  (read (aspk-util-file-content-to-string
         (format "%s/%s" eoh-headlines-dir (aspk-util-file-name-to-string file)))))

(defun eoh-load-headlines-always ()
  "always re load from all files in `eoh-headline-dir', and return as a list. The input is in the `eoh-files' global variable."
  (let ((rst nil))
    (mapc (lambda (file)
            (setq rst (append (eoh-load-headlines-coolier file) rst)))
          eoh-files)
    rst))

(defvar eoh-loaded-headlines-time (make-hash-table :test 'equal) "Type: hash. Key: file name, value: time when this file is loaded")
(defvar eoh-loaded-headlines-value (make-hash-table :test 'equal) "Type: hash. Key: file name, value: this list for this file")

(defun aspk-util-file-modified-time (file)
  "Get the seconds since epoc of the time when file is last modified"
  (time-to-seconds (nth 5 (file-attributes file 'string))))

(defun eoh-load-headlines-update ()
  "Only load from files in `eoh-headline-dir' if a file is newer than, and return as a list. The input is in the `eoh-files' global variable."
  (let ((rst nil)
        (time nil))
    (mapc (lambda (file)
            (setq time (aspk-util-file-modified-time
                        (format "%s/%s" eoh-headlines-dir (aspk-util-file-name-to-string file))))
            (when (< (gethash file eoh-loaded-headlines-time 0) time)
              (puthash file (eoh-load-headlines-coolier file) eoh-loaded-headlines-value)
              (puthash file time eoh-loaded-headlines-time))
            (setq rst (append (gethash file eoh-loaded-headlines-value) rst)))
          eoh-files)
    rst))

;; select headlines with helm. 
;;      can't goto the file.
     
;;      #+NAME: select-headlines

(defun eoh-goto-file-line (candidate)
  (add-to-list 'eoh-historys candidate)
  (aspk-util-dump-vars-to-file '(eoh-historys) eoh-history-file-name)
  (let ((file (nth 0 candidate))
        (line (nth 1 candidate)))
    (message "file: %s, line: %s" file line)
    (helm-goto-file-line line "" (format "%s/%s" eoh-base-dir file))
    (org-cycle 3)))

(setq eoh-helm-source
      '((name . "HELM at the Emacs")
        (candidates . (lambda ()
                        (mapcar (lambda (item)
                                  (cons (nth 2 item) item))
                                eoh-headlines)))

        ;; (subexp . 1)
        ;; (migemo)
        ;; (persistent-action . 'eoh-goto-file-line)

        (action-transformer
         . (lambda (actions candidate)
             '(("Godo" . eoh-goto-file-line)
               ;; '(("Godo" . (lambda (candidate)
               ;;               (setq eoh-selected-candidate candidate)))
               ("Display" .  (lambda (candidate)
                               (message-box "%S" candidate)))
               ("None" . identify)
               ))
         )))

(setq eoh-history-source
      '((name . "History")
        (candidates . (lambda ()
                        (mapcar (lambda (item)
                                  (cons (nth 2 item) item))
                                eoh-historys)))

        ;; (subexp . 1)
        ;; (migemo)
        ;; (persistent-action . 'eoh-goto-file-line)

        (action-transformer
         . (lambda (actions candidate)
             '(("Godo" . eoh-goto-file-line)
               ;; '(("Godo" . (lambda (candidate)
               ;;               (setq eoh-selected-candidate candidate)))
               ("Display" .  (lambda (candidate)
                               (message-box "%S" candidate)))
               ("None" . identify)
               ))
         )))

(defun eoh-select ()
  "Select headlines with helm. input is `eoh-headlines'"
  (interactive)
  (helm :sources '(eoh-history-source eoh-helm-source) :buffer "EOH"))

;; add files
;;    #+NAME: add-files

(defun eoh-add-file-single (file)
  "Add a file to `eoh-files', and remove the base part"
  (interactive "fFile: ")
  ;; (push (replace-regexp-in-string (concat eoh-base-dir "/") "" file) eoh-files)
  (add-to-list 'eoh-files (replace-regexp-in-string (concat eoh-base-dir "/") "" file))
  (eoh-save-files)
  )

(defun eoh-add-directory (dir)
  "Add all org files under `dir' to `eoh-files', and remove the base part"
  (interactive "fDir: ")
  (mapcar (lambda (file)
            ;; (message "Process file %S" file)
            (add-to-list 'eoh-files (replace-regexp-in-string (concat eoh-base-dir "/") "" file))
            ;; (eoh-add-file-single file)
            )
          (find-lisp-find-files dir "\\.org$"))
  (eoh-save-files))


(defun eoh-clear-files ()
  "Clear all eoh files"
  (interactive)
  (setq eoh-files nil)
  )

(setq eoh-history-loaded-p nil)
(defun eoh-load-files ()
  "load the file names to `eoh-files' from `eoh-files-file-name."
  (load eoh-files-file-name)
  (unless eoh-history-loaded-p
    (load eoh-history-file-name)
    (setq eoh-history-loaded-p t))
  )

(defun eoh-save-files ()
  "save `eoh-files' to `eoh-files-file-name'"
  (aspk-util-dump-vars-to-file '(eoh-files) eoh-files-file-name)
  )

;; http://stackoverflow.com/questions/2321904/elisp-how-to-save-data-in-a-file
(defun aspk-util-dump-vars-to-file (varlist filename)
  "simplistic dumping of variables in VARLIST to a file FILENAME"
  (save-excursion
    (let ((buf (find-file-noselect filename)))
      (set-buffer buf)
      (erase-buffer)
      (aspk-util-dump varlist buf)
      (save-buffer)
      (kill-buffer))))

(defun aspk-util-dump (varlist buffer)
  "insert into buffer the setq statement to recreate the variables in VARLIST"
  (loop for var in varlist do
        (print (list 'setq var (list 'quote (symbol-value var)))
               buffer)))

;; history 
    
;;     #+NAME: no-name

(defvar eoh-historys nil "The select history")
;; (setq eoh-historys nil)
(defvar eoh-history-file-name (format "%s/%s" eoh-home-dir "history") "The file is used to save the `eoh-historys' variable")

;; integrate

(defun eoh ()
  (interactive)
  (eoh-init)
  (eoh-load-files)
  (funcall eoh-generate-headlines-function eoh-files)
  (setq eoh-headlines (funcall eoh-load-headlines-function))
  (eoh-select)
  )

(provide 'eoh)
