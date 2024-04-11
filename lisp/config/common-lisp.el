(require 'settings/paredit)
(require 'slime)
(require 'slime-autoloads)


;;;###autoload
(defun my/common-lisp-setup ()
  """Setup for Common Lisp"""
  (setq prettify-symbols-alist '(("lambda" . ?λ)))
  (prettify-symbols-mode)

  (when (bound-and-true-p slime-autodoc-mode)
    (slime-autodoc-mode -1))
  (enable-paredit-mode))

(provide 'config/common-lisp)
