(require 'seq)
(require 'org)
(require 'org-capture)

(require 'org-roam)
(require 'vulpea)

;;; Capturing
;;;; Meetings



(defun my/vulpea-capture-meeting-template ()
  "Return a template for a meeting capture."
  (let ((person (vulpea-select
		 "Person"
		 :filter-fn
		 (lambda (note)
		   (let ((tags (vulpea-note-tags note)))
		     (seq-contains-p tags "people"))))))
    (org-capture-put :meeting-person person)
    (if (vulpea-note-id person)
	"* MEETING [%<%Y-%m-%d %a>] :REFILE:MEETING:\n%U\n\n%?"
      (concat "* MEETING with "
	      (vulpea-note-title person)
	      " on [%<%Y-%m-%d %a>] :MEETING:\n%U\n\n%?"))))



(defun my/vulpea-capture-meeting-target ()
  "Return a target for a meeting capture."
  (let ((person (org-capture-get :meeting-person)))
    ;; unfortunately, I could not find a way to reuse
    ;; `org-capture-set-target-location'
    (if (vulpea-note-id person)
	(let ((path (vulpea-note-path person))
	      (headline "Meetings"))
	  (set-buffer (org-capture-target-buffer path))
	  ;; Org expects the target file to be in Org mode, otherwise
	  ;; it throws an error. However, the default notes files
	  ;; should work out of the box. In this case, we switch it to
	  ;; Org mode.
	  (unless (derived-mode-p 'org-mode)
	    (org-display-warning
	     (format
	      "Capture requirement: switching buffer %S to Org mode"
	      (current-buffer)))
	    (org-mode))
	  (org-capture-put-target-region-and-position)
	  (widen)
	  (goto-char (point-min))
	  (if (re-search-forward
	       (format org-complex-heading-regexp-format
		       (regexp-quote headline))
	       nil t)
	      (beginning-of-line)
	    (goto-char (point-max))
	    (unless (bolp) (insert "\n"))
	    (insert "* " headline "\n")
	    (beginning-of-line 0)))
      (let ((path vulpea-capture-inbox-file))
	(set-buffer (org-capture-target-buffer path))
	(org-capture-put-target-region-and-position)
	(widen)))))

(provide 'lib/vulpea-capture)
