(require 'settings/paredit)

;;;###autoload
(defun my/clojure-setup ()
  (enable-paredit-mode)
  (subword-mode)
  (electric-indent-mode -1)
  (flycheck-mode)
  )
