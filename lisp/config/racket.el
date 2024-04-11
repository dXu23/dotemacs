(require 'settings/geiser)
(require 'settings/paredit)

(when-let (racket-binary (executable-find "racket"))
  (customize-set-variable 'geiser-racket-binary racket-binary))

;;;###autoload
(defun my/racket-setup ()
  (geiser-mode)
  (paredit-mode))
