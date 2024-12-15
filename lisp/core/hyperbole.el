;;; hyperbole.el --- My Hyperbole settings


;;; Commentary:
;;

;;; Code:
(require 'hyperbole)
(add-hook 'hyperbole-init-hook
          (lambda () (hkey-set-key (kbd "C-c C-o") #'hkey-either)))

(hyperbole-mode 1)
(hkey-ace-window-setup "\M-o")

(provide 'core/hyperbole)

;;; hyperbole.el ends here
