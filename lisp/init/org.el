;;; core-org.el --- Settigs for GTD and org mode -*- lexical-binding: t; eval: (outline-minor-mode 1) -*-

;; Many thanks to the following Emacs content creators
;; and their articles/videos
;;
;; Nicholas P. Rougier
;; Website: <https://www.labri.fr/perso/nrougier/>
;; Article: <https://www.labri.fr/perso/nrougier/GTD/index.html>
;;
;; Ben J. Maughan
;; Website: <https://pragmaticemacs.wordpress.com>
;; Article: <https://pragmaticemacs.wordpress.com/2015/12/08/org-mode-basics-vii-a-todo-list-with-schedules-and-deadlines/>

;;; Requires
(require 'org)
(require 'org-agenda)
(require 'org-capture)
(require 'org-protocol)

(require 'org-roam)

(require 'lib/vulpea)
(require 'lib/vulpea-agenda)
(require 'lib/vulpea-capture)

(defvar *org-root* "~/Documents/org")

;;;; Org Directory
(customize-set-variable 'org-directory *org-root*)
(customize-set-variable 'org-roam-directory *org-root*)
(setq vulpea-directory *org-root*)

;;; Agenda Settings
;; The following code optimizes org agenda indexing. It
;; takes too long to simply iterate over each file in
;; `org-roam-directory' and then add each TODO to the
;; agenda.

;;;; Disables inheritance for project tags
(add-to-list 'org-tags-exclude-from-inheritance "project")

(setq org-agenda-prefix-format
      '((agenda . " %i %(vulpea-agenda-category 12)%?-12t% s")
	(todo . " %i %(vulpea-agenda-category 12) ")
	(tags . " %i %(vulpea-agenda-category 12) ")
	(search . " %i %(vulpea-agenda-category 12) ")))

(setq org-agenda-custom-commands
      '(("g" "Get Things Done (GTD)"
	 ((agenda ""
		  ((org-agenda-skip-function
		    '(org-agenda-skip-entry-if 'deadline))
		   (org-deadline-warning-days 0)))
	  (todo "NEXT"
		((org-agenda-skip-function
		  '(org-agenda-skip-entry-if 'deadline))
		 (org-agenda-prefix-format " %i %-12:c [%e] ")
		 (org-agenda-overriding-header "\nTasks\n")))
	  (agenda nil
		  ((org-agenda-entry-types '(:deadline))
		   (org-agenda-format-date "")
		   (org-deadline-warning-days 7)
		   (org-agenda-skip-function
		    '(org-agenda-skip-entry-if 'notregexp "\\* NEXT"))
		   (org-agenda-overriding-header "\nDeadlines")))
	  (tags-todo "inbox"
		     ((org-agenda-prefix-format "  %?-12t% s")
		      (org-agenda-overriding-header "\nInbox\n")))
	  (tags "CLOSED>=\"<today\""
		((org-agenda-overriding-header "\nCompleted today\n")))))
	("u" "Unscheduled tasks" alltodo ""
	  (org-agenda-skip-entry-if 'scheduled 'deadline 'regexp "\n]+>")
	  (org-agenda-overriding-header "Unscheduled TODO entries: "))
	("W" "Weekly Review"
	  ((agenda "" ((org-agenda-span 7)))
	   (stuck "")
	   (todo "PROJECT")
	   (todo "MAYBE")
	   (todo "WAITING")))))

(setq org-agenda-hide-tags-regexp ".")

(add-hook 'find-file-hook #'my/vulpea-project-update-tag)
(add-hook 'before-save-hook #'my/vulpea-project-update-tag)

(advice-add 'org-agenda :before #'my/vulpea-agenda-files-update)
(advice-add 'org-todo-list :before #'my/vulpea-agenda-files-update)

(setq org-capture-templates `(("t" "Todo [inbox]" entry
			       (file+headline "inbox.org" "Tasks")
			       ,(concat "* TODO [#A] %i%?\n"
					"SCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n"))
			      ("T" "Tickler" entry
			       (file+headline "tickler.org" "Tickler")
			       "* %i%? \n %U")
			      ("p" "Protocol" entry
			       (file+headline "refile.org" "Notes")
			       ,(concat "* %:description :RESEARCH:\n"
					"#+BEGIN_QUOTE\n"
					"%i\n\n -- %:link %u\n"
					"#+END_QUOTE\n\n%?"))
			      ("L" "Protocol Link" entry
			       (file+headline "refile.org" "Notes")
			       "* %? [[%:link][%:description]] \nCaptured On: %u")
			      ("@" "Inbox [mu4e]" entry (file "inbox.org")
			       ,(concat "* TODO Process \"%a\" %?\n"
					"/Entered on/ %U"))
			      ("m" "Meeting" entry
			       (function vulpea-capture-meeting-target)
			       (function vulpea-capture-meeting-template))))


(setq org-deadline-string "DUE:")
(setq org-scheduled-string "SCHEDULED:")

;; Capture functions
(defun org-capture-meeting ()
  "Caputre a meeting."
  (interactive)
  (org-capture nil "m"))

(defun org-capture-task ()
  "Capture a task."
  (interactive)
  (org-capture nil "t"))

(defun org-capture-mail ()
  "Creates and captures a reply todo from email and puts it into inbox.org"
  (interactive)
  (call-interactively 'org-store-link)
  (org-capture nil "@"))

;; Use full window for org-capture
(add-hook 'org-capture-mode-hook 'delete-other-windows)

;;; Org mode keybindings
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c b") 'org-switchb)

;;; Refiling
(customize-set-variable 'org-refile-use-outline-path 'file)
(customize-set-variable 'org-outline-path-complete-in-steps nil)
(customize-set-variable 'org-refile-targets '(("projects.org" :regexp . "\\(?:\\(?:Note\\|Task\\)s\\)")
					      ("someday.org" :level . 1)
					      ("tickler.org" :maxlevel . 2)))


(defun gtd-save-org-buffers ()
  "Save `org-agenda-files' buffers without user confirmation.
See also `org-save-all-org-buffers'"
  (interactive)
  (message "Saving org-agenda-files buffers...")
  (save-some-buffers
   t
   (lambda ()
     (when (member (buffer-file-name) org-agenda-files)
       t)))
  (message "Saving org-agenda-files buffers... done"))


;; Add it after refile
(advice-add 'org-refile :after
	    (lambda (&rest _)
	      (gtd-save-org-buffers)))

;;;; Set org todo keywords
(setq org-todo-keywords
      '((sequence "TODO(t@/!)" "NEXT(n)" "SOMEDAY(s)" "PROJ(p)" "WAITING(w@/!)" "|"
		  "DONE(d@/!)" "CANCELED(c)")))

(defun log-todo-next-creation-date (&rest _)
  "Log NEXT creation time in the property drawer under the key `ACTIVATED'"
  (when (and (string= (org-get-todo-state) "NEXT")
	     (not (org-entry-get nil "ACTIVATED")))
    (org-entry-put nil "ACTIVATED" (format-time-string "[%Y-%m-%d]"))))

(add-hook 'org-after-todo-state-change-hook #'log-todo-next-creation-date)

(add-hook 'vulpea-insert-handle-functions
	  #'my/vulpea-insert-handle)

;;; Basic Org Settings
(setq org-protocol-default-template-key "1")

;;; Org Roam Settings
(setq org-roam-v2-ack t)
(with-eval-after-load 'org-roam
  (org-roam-db-autosync-enable))

;;;; Keybindings
(global-set-key (kbd "C-c n l") 'org-roam-buffer-toggle)
(global-set-key (kbd "C-c n f") 'org-roam-node-find)
(global-set-key (kbd "C-c n v") 'org-roam-node-visit)
(global-set-key (kbd "C-c n i") 'org-roam-insert)
(global-set-key (kbd "C-c q") 'org-roam-add)
(define-key org-mode-map (kbd "C-M-i") 'completion-at-point)

(provide 'init/org)
