(require 'settings/paredit)
(require 'sly)


;;;###autoload
(defun my/common-lisp-setup ()
  """Setup for Common Lisp"""
  (setq prettify-symbols-alist '(("lambda" . ?λ)))
  (prettify-symbols-mode)

  (when (bound-and-true-p sly-autodoc-mode)
    (sly-autodoc-mode -1))
  (enable-paredit-mode))

(provide 'config/common-lisp)
