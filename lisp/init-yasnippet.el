(require 'yasnippet)

;; my private snippets
(setq my-yasnippets (expand-file-name "~/my-yasnippets"))
(if (and  (file-exists-p my-yasnippets) (not (member my-yasnippets yas/snippet-dirs)))
    (add-to-list 'yas/snippet-dirs my-yasnippets))
;; (message "yas/snippet-dirs=%s" (mapconcat 'identity yas-snippet-dirs ":"))

;; this dir is Chenbin's snippets
(add-to-list 'yas/snippet-dirs (expand-file-name (format "%s/snippets" emacs-init-dir)))

;; TODO: remove this dir, move all files to aspk-code-snippet-dir
;; this is aspk's snippets
(defvar aspk-local-snippet-dir (expand-file-name (format "%s/local-snippets" emacs-init-dir)))
(add-to-list 'yas/snippet-dirs aspk-local-snippet-dir)

;; this is aspk-code-snippets, the new idea
(defvar aspk-code-snippet-dir (expand-file-name (format "%s/../aspk-code-snippets/snippets" emacs-init-dir)))
(add-to-list 'yas/snippet-dirs aspk-code-snippet-dir)

(yas/global-mode 1)

(defun my-yas-expand ()
  (interactive)
  (if (buffer-file-name)
      (let ((ext (car (cdr (split-string (buffer-file-name) "\\."))) )
            (old-yas-flag yas-indent-line)
            )
        (when (or (string= ext "ftl") (string= ext "jsp"))
          (setq yas-indent-line nil)
          )
        (yas-expand)
        ;; restore the flag
        (setq yas-indent-line old-yas-flag)
        )
    (yas-expand)
    )
  )

;; default TAB key is occupied by auto-complete
(global-set-key (kbd "C-c k") 'my-yas-expand)
;; default hotkey `C-c C-s` is still valid
;; (global-set-key (kbd "C-c l") 'yas-insert-snippet)
;; give yas/dropdown-prompt in yas/prompt-functions a chance
(require 'dropdown-list)
(setq yas-prompt-functions '(yas-dropdown-prompt
                             yas-ido-prompt
                             yas-completing-prompt))
;; use yas/completing-prompt when ONLY when `M-x yas-insert-snippet'
;; thanks to capitaomorte for providing the trick.
(defadvice yas-insert-snippet (around use-completing-prompt activate)
  "Use `yas-completing-prompt' for `yas-prompt-functions' but only here..."
  (let ((yas-prompt-functions '(yas-completing-prompt)))
    ad-do-it))
;; @see http://stackoverflow.com/questions/7619640/emacs-latex-yasnippet-why-are-newlines-inserted-after-a-snippet
(setq-default mode-require-final-newline nil)
(setq yas-wrap-around-region t)
(provide 'init-yasnippet)
