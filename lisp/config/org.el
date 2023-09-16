(require 'org)
(require 'org-bullets)

;;; Org Babel
(org-babel-do-load-languages
 'org-babel-load-languages
 '((R . t)
   (awk . t)
   (python . t)
   (ruby . t)
   (emacs-lisp . t)
   (dot . t)))

;;; Speed keys
(customize-set-variable 'org-use-speed-command t)

;; Set priority range from A to C with default A
(setq org-highest-priority ?A
      org-lowest-priority ?C
      org-default-priority ?A)

;; Set org-log-done to true
(customize-set-variable 'org-log-done 'note)

;;;; Turn off org-goto-auto-isearch
(customize-set-variable 'org-goto-auto-isearch nil)



;;;###autoload
(defun my/org-setup ()
  (org-bullets-mode)
  (prettify-symbols-mode))

;; Open org agenda in current window
(setq org-agenda-window-setup (quote current-window))

;; Hide redundant tags in org-agenda
(setq org-agenda-hide-tags-regexp ".")

;; Warn about any deadline in 4 days
(setq org-deadline-warning-days 4)

;; Show tasks schedules/due in next fornight
(customize-set-variable 'org-agenda-span 'fortnight)

;; Do not show tasks as scheduled if already shown as deadline
(setq org-agenda-skip-scheduled-if-deadline-is-shown t)

;; Do not give warning colors to tasks w/ impending deadlines
(setq org-agenda-skip-deadline-prewarning-if-scheduled 'pre-scheduled)

(customize-set-variable 'org-agenda-skip-deadline-if-done t)

;;;###autoload
(defun my/org-agenda-setup ())
