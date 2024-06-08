;;; config/python.el --- Config file for python files -*- lexical-binding: t; -*-

(require 'compile)
(require 'pet)
(require 'combobulate)
(require 'lsp)
(require 'lsp-ui)

(customize-set-variable 'python-check-command "poetry run pyright")

(defun python-compile-command ()
  (concat "mypy " buffer-file-name))

(defun move-to-next-container ()
  (interactive)
  (with-navigation-nodes (:nodes '("dictionary" "set" "list"))
    (combobulate-visual-move-to-node
     (combobulate-nav-logical-next) t)))

;;;###autoload
(defun my/python-setup ()
  (setq-local compile-command #'python-compile-command)
  (lsp-deferred)
  (combobulate-mode)
  (if (project-current nil)
      (pet-mode -10)))
