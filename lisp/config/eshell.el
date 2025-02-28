(require 'em-smart)
(require 'capf-autosuggest)

(setopt eshell-where-to-jump 'begin
        eshell-review-quick-commands nil
        eshell-smart-space-goes-to-end t
        eshell-scroll-to-bottom-on-input t
        eshell-history-size 10000
        eshell-hist-ignoredups t)

(setq-local tab-always-indent 'complete)

(add-to-list 'eshell-visual-options '("git" "--help" "--paginate"))
(add-to-list 'eshell-visual-subcommands '("git" "log" "diff" "show"))

;;;###autoload
(defun my/eshell-setup ()
  (capf-autosuggest-mode))

(provide 'config/eshell)
