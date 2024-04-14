(customize-set-variable 'major-mode-remap-alist
                        '((c-mode . c-ts-mode)
                          (yaml-mode . yaml-ts-mode)
                          (rust-mode . rust-ts-mode)
                          (bash-mode . bash-ts-mode)
                          (js-mode . js-ts-mode)
                          (typescript-mode . typescript-ts-mode)
                          (json-mode . json-ts-mode)
                          (css-mode . css-ts-mode)
                          (python-mode . python-ts-mode)))

(defun my/remove-treesit-sexp-changes ()
  (when (eq forward-sexp-function #'treesit-forward-sexp)
    (setq forward-sexp-function nil))
  (when (eq transpose-sexps-function #'treesit-transpose-sexps)
    (setq transpose-sexps-function nil))
  (when (eq forward-sentence-function #'treesit-forward-sentence)
    (setq forward-sentence-function nil)))

(add-hook 'prog-mode-hook #'my/remove-treesit-sexp-changes)

(provide 'core/prog)
