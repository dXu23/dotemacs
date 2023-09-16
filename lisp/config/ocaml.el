(require 'tuareg)
(require 'dune)
(require 'merlin)
(require 'merlin-eldoc)
(require 'flycheck-ocaml)

;; (add-to-list 'auto-mode-alist '("\\.ocamlinit\\'" . tuareg-mode))

(customize-set-variable 'merlin-error-after-save nil)

;;;###autoload
(defun my/ocaml-setup ()
  (tuareg-mode)
  (merlin-mode)
  (merlin-eldoc-setup)
  (flycheck-ocaml-setup))

(provide 'config/ocaml)
