(require 'racket-mode)
(require 'clojure-mode)
(require 'lua-mode)

(add-to-list 'auto-mode-alist '("\\.ocamlinit\\'" . tuareg-mode))
(add-to-list 'auto-mode-alist '("\\.clj\\'" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.rkt\\'" . racket-mode))
(add-to-list 'auto-mode-alist '("\\.lua\\'" . lua-mode))

(provide 'core/auto)
