;;; org.el --- Settigs for GTD and org mode -*- lexical-binding: t; eval: (outline-minor-mode 1) -*-

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
;;
;;

;;; Requires
(require 'cl-lib)

(require 'org)
(require 'org-agenda)
(require 'org-capture)
(require 'org-protocol)
(require 'org-habit)
(require 'org-noter)

(require 'denote)
(require 'denote-journal-extras)
(require 'elfeed)

(defvar *org-root* "~/Documents/org")

;;;; Org Directory
(customize-set-variable 'org-directory *org-root*)
(customize-set-variable 'denote-directory (expand-file-name "zettel" *org-root*))

;;;; Disables inheritance for project tags
(add-to-list 'org-tags-exclude-from-inheritance "project")

(customize-set-variable 'org-default-notes-file (expand-file-name "inbox.org" org-directory))
(customize-set-variable 'org-agenda-files (list org-default-notes-file "agenda.org"
                                                "notes.org" "projects.org"))

(customize-set-variable 'denote-journal-extras-directory "journal")

(customize-set-variable 'org-habit-show-all-today t)

(customize-set-variable 'org-capture-templates
                        `(("t" "New task [inbox]" entry
                           (file+headline org-default-notes-file "Tasks")
                           ,(concat "* TODO [#A] %i%?\n"
                                    "SCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n"))
                          ("@" "Inbox [mu4e]" entry
                           (file+headline org-default-notes-file "Tasks")
                           ,(concat "* TODO Reply \"%a\" %?\n"
                                    "/Entered on/ %U"))
                          ("f" "Fleeting note [inbox]" item
                           (file+headline org-default-notes-file "Notes")
                           "- %?")
                          ("h" "Habit" entry
                           (file+headline org-default-notes-file "Tasks")
                           ,(concat "* TODO %i\n"
                                    "SCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n"
                                    ":PROPERTIES:\n"
                                    ":STYLE: habit\n"
                                    ":END:"))
                          ("P" "Project" entry
                           (file+headline "projects.org" "Projects")
                           ("** PROJ %i%?\n"
                            ":PROPERTIES:\n"
                            ":COOKIE_STYLE: todo recursive"))
                          ("p" "Permanent note" plain
                           (file denote-last-path)
                           (function
                            (lambda ()
                              (let ((denote-directory (expand-file-name "zettel/" org-directory)))
                                (denote-org-capture))))
                           :no-save t
                           :immediate-finish nil
                           :kill-buffer t
                           :jump-to-captured t)
                          ("j" "Journal" entry
                           (file denote-last-path)
                           (function
                            (lambda ()
                              ;; The "journal" subdirectory of the `denote-directory'---this must exist!
                              (let* ((denote-use-directory (expand-file-name "journal" org-directory))
                                     ;; Use the existing `denote-prompts' as well as the one for a date.
                                     (denote-prompts (denote-add-prompts '(date))))
                                (denote-org-capture))))
                           :no-save t
                           :immediate-finish nil
                           :kill-buffer t
                           :jump-to-captured t)
                          ("T" "Tickler" entry
                           (file+headline "tickler.org" "Tickler")
                           "* %i%? \n %U")
                          ("w" "Protocol" entry
                           (file+headline "refile.org" "Notes")
                           ,(concat "* %:description :RESEARCH:\n"
                                    "#+BEGIN_QUOTE\n"
                                    "%i\n\n -- %:link %u\n"
                                    "#+END_QUOTE\n\n%?"))
                          ("L" "Protocol Link" entry
                           (file+headline "refile.org" "Notes")
                           "* %? [[%:link][%:description]] \nCaptured On: %u")
                          ("m" "Meeting" entry (file+headline "agenda.org" "Future")
                           ,(concat "* %? :meeting:\n"
                                    "<%<%Y-%m-%d %a %H:00>>")
                           )))

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
         ((org-agenda-skip-entry-if 'scheduled 'deadline 'regexp "\n]+>")
          (org-agenda-overriding-header "Unscheduled TODO entries: ")))
        ("W" "Weekly Review"
          ((agenda "" ((org-agenda-span 7)))
           (stuck "")
           (todo "PROJECT")
           (todo "MAYBE")
           (todo "WAITING")))))

(setq org-agenda-hide-tags-regexp ".")

(setq org-deadline-string "DUE:")
(setq org-scheduled-string "SCHEDULED:")

;; Capture functions
(defun org-capture-meeting ()
  "Capture a meeting."
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
(keymap-global-set "C-c l" #'org-store-link)
(keymap-global-set "C-c a" #'org-agenda)
(keymap-global-set "C-c c" #'org-capture)
(keymap-global-set "C-c b" #'org-switchb)

(keymap-global-set "C-c n n" #'denote)
(keymap-global-set "C-c n N" #'denote-type)
(keymap-global-set "C-c n o" #'denote-sort-dired)
(keymap-global-set "C-c n r" #'denote-rename-file)

(keymap-set text-mode-map "C-c n i" #'denote-link)
(keymap-set text-mode-map "C-c n I" #'denote-add-links)
(keymap-set text-mode-map "C-c n b" #'denote-backlinks)

; (keymap-set org-mode-map "C-c n d l" #'denote-org-extras-dbl)

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

(defun my/org-after-tag-change-hook ()
  "This function makes sure that the current heading has
(1) the tag :project:
(2) has property COOKIE_DATA set to \"todo recursive\"
(3) has any TODO keyword and
(4) a leading progress indicator"
  (save-excursion
    (when (member "project" (org-get-tags))
      (org-set-property "COOKIE_DATA" "todo recursive")
      (org-back-to-heading t)
      (pcase-let ((`(_ _ ,keyword _ ,title) (org-heading-components)))
        (if (bound-and-true-p keyword)
            (when (not (string-prefix-p "[" title))
              (forward-whitespace 2)
              (insert "[/] "))
          (progn
            (forward-whitespace 1)
            (message
             (if (string-prefix-p "[" title)
                 "NEXT "
               "NEXT [/] "))))))))

(add-hook 'org-after-tags-change-hook #'my/org-after-tag-change-hook)

(defun log-todo-next-creation-date (&rest _)
  "Log NEXT creation time in the property drawer under the key `ACTIVATED'"
  (when (and (string= (org-get-todo-state) "NEXT")
             (not (org-entry-get nil "ACTIVATED")))
    (org-entry-put nil "ACTIVATED" (format-time-string "[%Y-%m-%d]"))))

(add-hook 'org-after-todo-state-change-hook #'log-todo-next-creation-date)


;;; Basic Org Settings
(customize-set-variable 'org-protocol-default-template-key "1")
(customize-set-variable 'org-enforce-todo-dependencies t)

;(defun my/org-insert-image ()
;  "Select and insert an image at point."
;  (interactive)
;  (let* ((file-name (format "%s-%s.png"
;                            (file-name-sans-extension (buffer-name))
;                            (random (expt 2 31))))
;         (path (format "%s/%s/%s" org-roam-directory "images" file-name)))
;    ;; The mouse movement via xdotool is needed because otherwise, if
;    ;; unclutter is active, the pointer will remain idden.
;    (call-process "xdotool" nil 0 nil "mousemove_relative" "--" "-1" "0")
;    (let ((scrot-exit (call-process "scrot" nil nil nil
;                                     "-z" "-f" "-s" "--file" path)))
;      (when (= scrot-exit 0)
;        (insert (format "[[../images/%s]]" file-name))))))

;; 來自 "https://takeonrules.com/2024/08/11/exporting-org-mode-elfeed-links/"

;;; Citations in Org
(customize-set-variable 'org-cite-global-bibliography
                        (list (expand-file-name "biblio.bib" *org-root*)))

(customize-set-variable 'org-cite-export-processors
                        '((md . (csl "chicago-fullnote-bibliography.csl"))
                          (latex bibtex)
                          (odt . (csl "chicago-fullnote-bibliography.csl"))
                          (t . (csl "modern-language-association.csl"))))


(org-link-set-parameters "elfeed"
  :follow #'elfeed-link-open
  :store #'elfeed-link-store-link
  :export #'elfeed-link-export-link)

(defun elfeed-link-export-link (link desc format _protocol)
  "Export `org-mode' `elfeed' LINK with DESC for FORMAT."
  (if (string-match "\\([^#]+\\)#\\(.+\\)" link)
      (if-let* ((entry
                 (elfeed-db-get-entry
                  (cons (match-string 1 link)
                        (match-string 2 link))))
                (url
                 (elfeed-entry-link entry))
                (title
                 (elfeed-entry-title entry)))
          (pcase format
            ('html  (format "<a href=\"%s\">%s</a>" url desc))
            ('md    (format "[%s](%s)" desc url))
            ('latex (format "@uref{%s,%s}" url desc))
            (_      (format "%s (%s)" desc url)))
        (format "%s (%s)" desc url))
    (format "%s (%s)" desc link)))

(provide 'init/org)

;;; init.el ends here
;;
;; Local Variables:
;; outline-regexp: ";;;\\(;* [^ \t\n]\\|###autoload\\)"
;; eval: (outline-minor-mode 1)
;; End:
