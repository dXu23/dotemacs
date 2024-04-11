(require 'tide)
(require 'flycheck)
(require 'web-mode)

;;;###autoload
(defun my/typescript-setup ()
  (tide-setup)
  (web-mode)
  (flycheck-mode +1)
  (setq-local flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1))

(flycheck-add-mode 'typescript-tslint #'web-mode)
(add-hook 'before-save-hook #'tide-format-before-save)

(provide 'config/typescript)
