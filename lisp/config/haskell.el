(require 'flycheck)

(require 'lsp)
(require 'lsp-ui-mode)

(customize-set-variable 'lsp-haskell-server-path "haskell-language-server-wrapper")


;;;###autoload
(defun my/haskell-setup ()
  (flycheck-mode)
  (lsp)
  (lsp-ui-mode))

(provide 'config/haskell)
