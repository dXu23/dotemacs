(require 'docker-mode)

;;;###autoload
(defun my/docker-setup ()
  (modify-syntax-entry ?$ ".")
  (modify-syntax-entry ?/ "."))

(provide 'config/docker)
