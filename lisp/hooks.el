(add-hook 'ocaml-mode-hook #'my/ocaml-setup)
(add-hook 'scheme-mode-hook #'my/scheme-setup)

;; Since cider and inf-clojure rely on clojure-mode,
;; will be using clojure-mode instead of clojure-ts-mode
(add-hook 'clojure-mode-hook #'my/clojure-setup)
(add-hook 'racket-mode-hook #'my/racket-setup)
(add-hook 'lisp-mode-hook #'my/common-lisp-setup)
(add-hook 'org-mode-hook #'my/org-setup)
(add-hook 'latex-mode-hook #'my/latex-setup)
(add-hook 'eshell-mode-hook #'my/eshell-setup)
(add-hook 'julia-mode-hook #'my/julia-setup)
(add-hook 'calc-mode-hook #'my/calc-setup)

;; cperl mode is better than perl
(add-hook 'perl-mode-hook #'my/perl-setup)
;; (defalias 'perl-mode 'cperl-mode)

(add-hook 'java-ts-mode-hook #'my/java-setup)
(add-hook 'c-ts-mode-hook #'my/c-setup)
(add-hook 'rust-ts-mode-hook #'my/rust-setup)
(add-hook 'python-ts-mode-hook #'my/python-setup)
(add-hook 'typescript-ts-mode-hook #'my/typescript-setup)

(provide 'hooks)
