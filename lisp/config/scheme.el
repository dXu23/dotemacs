(require 'geiser)
(require 'geiser-mit)

(when-let (scheme-binary (executable-find "scheme"))
  (customize-set-variable 'geiser-mit-binary scheme-binary))

(customize-set-variable 'geiser-repl-skip-version-check-p t)
(customize-set-variable 'geiser-active-implementations '(mit))

;;;###autoload
(defun my/scheme-setup ()
  (prettify-symbols-mode)
  (geiser-mode)
  )

(provide 'config/scheme)
