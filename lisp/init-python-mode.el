(autoload 'doctest-mode "doctest-mode" "Python doctest editing mode." t)

(setq interpreter-mode-alist
      (cons '("python" . python-mode) interpreter-mode-alist))

(setq python-indent-offset 2)
;; (setq python-indent-guess-indent-offset nil)

;;----------------------------------------------------------------------------
;; On-the-fly syntax checking via flymake
;;----------------------------------------------------------------------------
(eval-after-load 'python
  '(require 'flymake-python-pyflakes))

(add-hook 'python-mode-hook '(lambda ()
                               (unless (is-buffer-file-temp)
                                 (message "python-mode-hook called")
                                 (when *emacs24*
                                   (anaconda-mode)
                                   (add-to-list 'company-backends 'company-anaconda)
                                   (eldoc-mode))
                                 (flymake-python-pyflakes-load)
                                 )))


(require 'aspk-outline)
(add-hook 'python-mode-hook 'aspk/outline-python-mode-hook)

;; # Disable automatic indent when yas snippet expands
;; # Another variable is: yas-indent-line. If this is nil, then there will be no indent
;; (delete 'yas-expand python-indent-trigger-commands)
;; (delete 'yas/expand python-indent-trigger-commands)

(provide 'init-python-mode)
