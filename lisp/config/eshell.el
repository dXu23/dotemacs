(require 'eshell-mode)
(require 'em-smart)

(setopt eshell-where-to-jump 'begin
        eshell-review-quick-commands nil
        eshell-smart-space-goes-to-end t)

(add-to-list 'eshell-visual-options '("git" "--help" "--paginate"))
(add-to-list 'eshell-visual-subcommands '("git" "log" "diff" "show"))

;;;###autoload
(defun my/eshell-setup ()
  )

(provide 'config/eshell)
