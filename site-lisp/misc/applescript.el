;; # mac 上 emacs 直接编辑二进制applescript
;;;; Allow editing of binary .scpt files (applescript) on mac.
(add-to-list 'jka-compr-compression-info-list
             `["\\.scpt\\'"
               "converting text applescript to binary applescprit "
               ,(expand-file-name "bin/applescript-helper.sh" emacs-init-dir) nil
               "converting binary applescript to text applescprit "
               ,(expand-file-name "bin/applescript-helper.sh" emacs-init-dir) ("-d")
               nil t "FasdUAS"])
;;It is necessary to perform an update!
(jka-compr-update)
