(require 'tide)
(require 'web)

(setopt web-mode-enable-auto-quoting nil
        web-mode-markup-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-attr-indent-offset 2
        web-mode-attr-value-indent-offset 2)

;;;###autoload
(defun my/react-setup ()
  (tide-setup)
  (combobulate-mode)
  (eldoc-mode)
  (tide-hl-identifier-mode +1)
  )
