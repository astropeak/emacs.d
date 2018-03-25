(require 'anything)
(require 'anything-match-plugin)
(require 'anything-config)

(define-key evil-normal-state-map ",mx" 'anything-M-x)

(provide 'init-anything)