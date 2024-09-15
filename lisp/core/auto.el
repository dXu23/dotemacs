(require 'racket-mode)
(require 'clojure-mode)
(require 'lua-mode)
(require 'nov)

(add-to-list 'auto-mode-alist '("\\.ocamlinit\\'" . tuareg-mode))
(add-to-list 'auto-mode-alist '("\\.cl\\'" . lisp-mode))
(add-to-list 'auto-mode-alist '("\\.clj\\'" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.rkt\\'" . racket-mode))
(add-to-list 'auto-mode-alist '("\\.lua\\'" . lua-mode))
(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))

(provide 'core/auto)
