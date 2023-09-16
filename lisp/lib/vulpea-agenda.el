(require 'org)
(require 'vulpea)

(require 'lib/vulpea)



;;;###autoload
(defun my/vulpea-agenda-person ()
  "Show main `org-agenda' view."
  (interactive)
  (let* ((person (vulpea-select-from
		  "Person"
		  (vulpea-db-query-by-tags-some '("people"))))
	 (node (org-roam-node-from-id (vulpea-note-id person)))
	 (names (cons (org-roam-node-title node)
		      (org-roam-node-aliases node)))
	 (tags (seq-map #'vulpea--title-to-tag names))
	 (query (string-join tags "|")))
    (let ((org-agenda-overriding-arguments (list t query)))
      (org-agenda nil "M"))))



;;;###autoload
(defun my/vulpea-agenda-files-update (&rest _)
  "Update the value of `org-agenda-files'."
  (setq org-agenda-files (vulpea-project-files)))



;;;###autoload
(defun my/vulpea-agenda-category (&optional len)
  "Get category of item at point for agenda.

Category is defined by one of the following items:

- CATEGORY property
- TITLE keyword
- TITLE property
- filename without directory and extension

When LEN is a number, resulting string is padded right with
spaces and then truncated with ... on the right if result is
longer than LEN.

Usage example:

  (setq org-agenda-prefix-format
	'((agenda . \" %(vulpea-agenda-category) %?-12t %12s\")))

Refer to `org-agenda-prefix-format' for more information."
  (let* ((file-name (when buffer-file-name
		      (file-name-sans-extension
		       (file-name-nondirectory buffer-file-name))))
	 (title (vulpea-buffer-prop-get "title"))
	 (category (org-get-category))
	 (result
	  (or (if (and
		   title
		   (string-equal category file-name))
		  title
		category)
	      "")))
    (if (numberp len)
	(s-truncate len (s-pad-right len " " result))
      result)))


(provide 'lib/vulpea-agenda)
