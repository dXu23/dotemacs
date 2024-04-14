(require 'settings/geiser)
(require 'settings/paredit)

(require 'geiser-mit)

(when-let (scheme-binary (executable-find "scheme"))
  (customize-set-variable 'geiser-mit-binary scheme-binary))

;;;###autoload
(defun my/scheme-setup ()
  (prettify-symbols-mode)
  (geiser-mode)
  )

(provide 'config/scheme)
