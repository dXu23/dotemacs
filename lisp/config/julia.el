(require 'settings/lsp)
(require 'julia)

;;;###autoload
(defun my/julia-setup ()
  "Sets up Julia"
  (setq julia-repl-set-terminal-backend 'vterm)

  (julia-repl-mode)
  (yas-minor-mode))

(provide 'config/julia)
