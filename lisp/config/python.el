;;; config/python.el --- Config file for python files -*- lexical-binding: t; -*-

(require 'compile)
(require 'lsp)
(require 'lsp-ui)

(customize-set-variable 'python-check-command "poetry run pyright")

(defun python-compile-command ()
  (concat "mypy " buffer-file-name))

;;;###autoload
(defun my/python-setup ()
  (setq-local compile-command #'python-compile-command)
  (lsp-deferred))
