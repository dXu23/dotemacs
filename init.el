;;; init.el --- Entry point for my Emacs config -*- lexical-binding: t; -*-

;;; Commentary:
;; init.el contains basic settings, as well as package declarations
;; that are not applicable to any one specific mode associated with
;; a filetype (e.g. Vertico).
;;
;; The following keystrokes may be useful for navigating the config
;; file:
;; C-c C-@ C-t : Fold text under subheadings
;; C-c C-@ C-a : Reveal text under subheadings
;; C-c C-@ C-n : Move to next visible heading
;; C-c C-@ C-p : Move to previous visible heading
;; C-c C-@ C-f : Move to next heading on same level
;; C-c C-@ C-p : Move to previous heading on same level

;;; Basic config for Borg
(when (< emacs-major-version 27)
  (setq package-enable-at-startup nil))

(add-to-list 'load-path (expand-file-name "lib/borg" user-emacs-directory))

(require 'borg)
(borg-initialize)

(with-eval-after-load 'magit
  (magit-add-section-hook 'magit-status-sections-hook
			  'magit-insert-modules
			  'magit-insert-stashes
			  'append))

(require 'auto-compile)
(auto-compile-on-load-mode)
(auto-compile-on-save-mode)


;;; Set emacs init file
(defconst *emacs-init-file*
  (locate-user-emacs-file "init.el"))

;;; Visit config file
(defun config-visit ()
  "Visits config file, specified by *emacs-init-file."
  (interactive)
  (find-file *emacs-init-file*))

(global-set-key (kbd "C-c e") 'config-visit)

;;; Reload config file
(defun config-reload ()
  "Reloads config, specified by *emacs-init-file*."
  (interactive)
  (load *emacs-init-file*))

(global-set-key (kbd "C-c r") 'config-reload)

;;; Basic Settings
(setq inhibit-startup-message t)

(repeat-mode 1)

(setq create-lockfiles nil)

(setq
 make-backup-files t
 backup-by-copying nil
 version-control t
 kept-old-versions 10000
 kept-new-versions kept-old-versions
 backup-directory-alist
 `(("." . ,(expand-file-name "~/.backups"))))

(customize-set-variable 'bookmark-save-flag 1)

; Allows user to type y or n instead of yes or no
(defalias 'yes-or-no-p 'y-or-n-p)

(prefer-coding-system 'utf-8)

(setq gc-cons-threshold 50000000)

(setq large-file-warning-threshold 100000000)

(save-place-mode 1)

;; Fill column marker for line wraps
(setq fill-column 80)

;; Buffer related settings
(setq switch-to-buffer-in-dedicated-window 'pop
      switch-to-buffer-obey-display-actions t)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Tab bar mode
(tab-bar-mode 1)

;;; ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)

(customize-set-variable 'ibuffer-saved-filter-groups
			(quote (("default"
				 ("dired" (mode . dired-mode))
				 ("org" (mode . org-mode))
				 ("magit" (name . "^magit"))
				 ("planner" (or
					     (name . "^\\*Calendar\\*$")
					     (name . "^\\*Org Agenda\\*")))
				 ("emacs" (or
					   (name . "^\\*scratch\\*$")
					   (name . "^\\*Messages\\*$")))))))

(require 'ibuf-ext)
(add-hook 'ibuffer-mode-hook
	  (lambda ()
	    (ibuffer-switch-to-saved-filter-groups "default")))

(put 'downcase-region 'disabled nil)
(put 'set-goal-column 'disabled nil)
(put 'upcase-region 'disabled nil)

(require 'kmacro)
(defalias 'kmacro-insert-macro 'insert-kbd-macro)
;; (define-key kmacro-keymap (kbd "I") #'kmacro-insert-macro)

;; Prevent blank strings from being put in kill ring
(customize-set-variable 'kill-transform-function
			(lambda (string)
			  (and (not (not string-blank-p string))
			       string)))

(pixel-scroll-precision-mode)

;; Delete any redundant white space after saving a file
(add-hook 'before-save-hook 'whitespace-cleanup)

;; Dired-X
(with-eval-after-load 'dired
  (customize-set-variable 'dired-x-hands-off-my-keys nil)
  (require 'dired-x))

(require 'which-key)
(which-key-mode)

;; Trying to configure vulpea with org just seems too difficult
;; (require 'core-org)
(require 'core/appearance)
(require 'core/window)
(require 'core/completion)
(require 'core/minibuffer)
(require 'core/prog)

(require 'hooks)
(load "loaddefs" nil t)

(require 'init/org)

;;; init.el ends here
;;
;; Local Variables:
;; outline-regexp: ";;;\\(;* [^ \t\n]\\|###autoload\\)"
;; eval: (outline-minor-mode 1)
;; End:
