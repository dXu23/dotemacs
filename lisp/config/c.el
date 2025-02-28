(require 'settings/lsp)

;;;###autoload
(defun my/c-setup ()
  "Sets up Treesitter for C."
  (setq-local electric-quote-comment nil)
  (setq-local electric-quote-string nil)
  (setq-local find-sibling-rules
              '(("\\([^/]+\\)\\.c\\'" "\\1.h")))
  (setopt c-ts-mode-indent-offset 4
          c-ts-indent-style 'k&r)
  (treesit-font-lock-recompute-features
   '(function variable) '(definition))

  (lsp-deferred)
  (lsp-ui-mode))

(provide 'config/c)
