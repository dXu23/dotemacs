(add-hook 'java-mode-hook #'my/java-setup)
(add-hook 'ocaml-mode-hook #'my/ocaml-setup)
(add-hook 'scheme-mode-hook #'my/scheme-setup)
(add-hook 'org-mode-hook #'my/org-setup)

(add-hook 'python-ts-mode-hook #'my/python-setup)

(provide 'hooks)
