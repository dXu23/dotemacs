(require 'settings/lsp)
(require 'rustic)

(define-key 'rustic-mode-map "C-c C-c l" . #'flycheck-list-errors)
(define-key 'rustic-mode-map "C-c C-c a" . #'lsp-execute-code-action)
(define-key 'rustic-mode-map "C-c C-c r" . #'lsp-rename)
(define-key 'rustic-mode-map "C-c C-c q" . #'lsp-workspace-restart)
(define-key 'rustic-mode-map "C-c C-c Q" . #'lsp-workspace-shutdown)
(define-key 'rustic-mode-map "C-c C-c s" . #'lsp-rust-analyzer-status)

(setopt lifetime-elision-hints-enable "skip_trivial"
        chaining-hints t
        lifetime-elision-hints-use-parameter-names nil
        closure-return-type-hints t
        parameter-hints nil
        reborrow-hints nil)

(setq rustic-format-on-save t)

(defun my/rustic-mode-hook ()
  (when buffer-file-name
    (setq-local buffer-save-without-query t))
  (add-hook 'before-save-hook 'lsp-format-buffer nil t))

(add-hook 'rustic-mode-hook #'my/rustic-mode-hook)

;;;###autoload
(defun my/rust-setup ()
  (lsp-deferred)
  )
