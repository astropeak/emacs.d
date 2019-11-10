;; Switch to wubi input method: M-x set-input-method, then select chinese-wubi
;; switch back to method: toggle-input-method
;; Note: there is a input method named english-dvorak

(require 'wubi)
(register-input-method "chinese-wubi" "Chinese-GB" 'quail-use-package "wubi" "wubi")

(setq default-input-method "chinese-wubi")


;; key bindding
(define-key global-map "\C-c]" 'toggle-input-method)
(when (featurep 'evil-leader)
  (evil-leader/set-key "]]" 'toggle-input-method))

(wubi-toggle-quanjiao-banjiao -1)

(setq wubi-phrases-file (format "%s.wubi-phrases.el" emacs-init-dir))
(wubi-load-local-phrases)


;; # add popup list and temperary english support. elisp is good that you can modify existing functions by advice. Quite nice feature.
(require 'aspk-app-wubi)

(provide 'init-wubi)

