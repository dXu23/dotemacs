;;; init.el --- Entry point for my Emacs config -*- lexical-binding: t; -*-

;;; Commentary:
;; init.el contains basic settings, as well as package declarations
;; that are not applicable to any one specific mode associated with
;; a filetype (e.g. Vertico).
;;
;; The following keystrokes may be useful for navigating the config
;; file:
;; C-c @ C-t : Fold text under subheadings
;; C-c @ C-a : Reveal text under subheadings
;; C-c @ C-n : Move to next visible heading
;; C-c @ C-p : Move to previous visible heading
;; C-c @ C-f : Move to next heading on same level
;; C-c @ C-p : Move to previous heading on same level

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
(setq-default indent-tabs-mode nil)

(setq inhibit-startup-message t)

(repeat-mode 1)

(setq create-lockfiles nil)

(setopt
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

;; Adjust garbage collection thresholds during startup
(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold
                             normal-gc-cons-threshold))))


(setq large-file-warning-threshold 100000000)

(save-place-mode 1)

;; Fill column marker for line wraps
(setq fill-column 80)

;; Buffer related settings
(setopt switch-to-buffer-in-dedicated-window 'pop
        switch-to-buffer-obey-display-actions t)

(let ((emacs-home (if-let ((xdg (getenv "XDG_CONFIG_HOME")))
                      (expand-file-name "emacs/" xdg)
                    user-emacs-directory)))
  (add-to-list 'load-path (expand-file-name "lisp" emacs-home)))

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
(put 'scroll-left 'disabled nil)
(put 'scroll-right 'disabled nil)

;;; Macro settings
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

;;; Dired-X
(with-eval-after-load 'dired
  (customize-set-variable 'dired-x-hands-off-my-keys nil)
  (require 'dired-x))

(require 'which-key)
(which-key-mode)

; (advice-add 'compile :before (lambda () (ad-set-arg 1 t)))

(defadvice compile (before ad-compile-start activate)
  "Advises `compile' so that it sets the argument COMINT to t."
  (ad-set-arg 1 t))

(pdf-tools-install)
(pdf-loader-install)

(customize-set-variable 'display-raw-bytes-as-hex t)

(require 'core/auto)

(require 'core/appearance)
(require 'core/filter)
(require 'core/window)
(require 'core/completion)
(require 'core/minibuffer)
(require 'core/prog)

(require 'init/org)
(require 'init/elfeed)
(require 'init/common-lisp)

(require 'hooks)
(load "loaddefs" nil t)



;;; init.el ends here
;;
;; Local Variables:
;; outline-regexp: ";;;\\(;* [^ \t\n]\\|###autoload\\)"
;; eval: (outline-minor-mode 1)
;; End:
