;;; help.el --- Help configuration

;;; Commentary:
;;

(require 'helpful)
(keymap-global-set "C-h f" #'helpful-function)
(keymap-global-set "C-h x" #'helpful-command)
(keymap-global-set "C-h k" #'helpful-key)
(keymap-global-set "C-h v" #'helpful-variable)
(keymap-global-set "C-h o" #'helpful-symbol)
(provide 'core/help)

;;; help.el ends here
