;; Switch to wubi input method: M-x set-input-method, then select chinese-wubi
;; switch back to method: toggle-input-method
;; Note: there is a input method named english-dvorak

(require 'wubi)
(register-input-method "chinese-wubi" "Chinese-GB" 'quail-use-package "wubi" "wubi")
(wubi-load-local-phrases)

(setq default-input-method "chinese-wubi")


(when (featurep 'evil-leader)
  (evil-leader/set-key "i" 'toggle-input-method))

;; # add popup list and temperary english support. elisp is good that you can modify existing functions by advice. Quite nice feature.
(require 'aspk-app-wubi)

(provide 'init-wubi)

