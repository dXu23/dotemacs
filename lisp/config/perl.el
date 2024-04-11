(require 'cperl-mode)
(require 'flycheck)

;;;###autoload
(defun my/perl-setup ()
  (setopt cperl-invalid-face nil
          cperl-indent-parens-as-block t
          cperl-close-paren-offset (- cperl-indent-level))

  (setopt flycheck-check-syntax-automatically '(mode-enabled save)
          flycheck-display-errors-delay 0.3)

  (setq-local compile-command (concat "perl " buffer-file-name))

  (flycheck-mode)
  )

(provide 'config/perl)
