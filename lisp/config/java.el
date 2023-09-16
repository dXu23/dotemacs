(require 'cc-mode)

;;;###autoload
(defun my/java-setup ()
  (hs-minor-mode)
  (setq c-basic-offset 4)
  (glasses-mode 1))

(provide 'config/java)
