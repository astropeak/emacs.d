;;----------------------------------------------------------------------------
;; Set load path
;;----------------------------------------------------------------------------
(eval-when-compile (require 'cl))
(if (fboundp 'normal-top-level-add-to-load-path)
    (let* ((my-lisp-dir (format "%s/site-lisp/" emacs-init-dir))
           (default-directory my-lisp-dir))
      (progn
        (setq load-path
              (append
               (loop for dir in (directory-files my-lisp-dir)
                     unless (string-match "^\\." dir)
                     collecting (expand-file-name dir))
               load-path)))))


;;----------------------------------------------------------------------------
;; Utilities for grabbing upstream libs
;;----------------------------------------------------------------------------
(defun site-lisp-dir-for (name)
  (expand-file-name (format "%s/site-lisp/%s" emacs-init-dir name)))

(defun site-lisp-library-el-path (name)
  (expand-file-name (format "%s.el" name) (site-lisp-dir-for name)))

(defun download-site-lisp-module (name url)
  (let ((dir (site-lisp-dir-for name)))
    (message "Downloading %s from %s" name url)
    (unless (file-directory-p dir)
      (make-directory dir)
      (add-to-list 'load-path dir))
    (let ((el-file (site-lisp-library-el-path name)))
      (url-copy-file url el-file t nil)
      el-file)))

(defun ensure-lib-from-url (name url)
  (unless (site-lisp-library-loadable-p name)
    (byte-compile-file (download-site-lisp-module name url))))

(defun site-lisp-library-loadable-p (name)
  "Return whether or not the library `name' can be loaded from a
source file under ~/.emacs.d/site-lisp/name/"
  (let ((f (locate-library (symbol-name name))))
    (and f (string-prefix-p (file-name-as-directory (site-lisp-dir-for name)) f))))

;;----------------------------------------------------------------------------
;; Fix up some load paths for libs from git submodules
;;----------------------------------------------------------------------------
(add-to-list 'load-path (expand-file-name (format "%s/site-lisp/session/lisp" emacs-init-dir)))
;; emacs23's org-mode is too old, here replace it
(when *is-emacs23*
  (add-to-list 'load-path (expand-file-name (format "%s/site-lisp/emacs23/org/lisp" emacs-init-dir)))
  (add-to-list 'load-path (expand-file-name (format "%s/site-lisp/emacs23/org/contrib/lisp" emacs-init-dir))))

;;----------------------------------------------------------------------------
;; Download these upstream libs
;;----------------------------------------------------------------------------

(defun remove-site-lisp-libs ()
  (shell-command "cd ~/.emacs.d && grep -e '^site-lisp/' .gitignore|xargs rm -rf"))

(defun ensure-site-lisp-libs ()
  (unless (> emacs-major-version 23)
    (ensure-lib-from-url 'package "http://bit.ly/pkg-el23")))



(defun refresh-site-lisp ()
  (interactive)
  (remove-site-lisp-libs)
  (ensure-site-lisp-libs))


(ensure-site-lisp-libs)

(provide 'init-site-lisp)
