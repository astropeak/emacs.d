(require 'scb)

;; for etags.exe. Maybe put etags.exe to "/bin" is better.
(add-to-list 'exec-path (concat emacs-init-dir "/site-lisp/silly-code-browser"))
(provide 'init-scb)