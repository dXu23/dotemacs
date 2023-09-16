;;; Org mode for exercise
(defun get-exercise-date-from-item (item)
  "Return the date of an exercise headline.

ITEM is an Org headline \"Exercise\" element from a journal buffer."
  (org-element-property :raw-value (org-element-property :parent item)))

(defun get-exercise-data-from-item (item)
  "Return an alist of (exercise-type . value) for all child
headlines of an item.

ITEM is an Org headline \"Exercise\" element from a journal
buffer."
  (org-element-map (org-element-contents item) 'headline
    (lambda (exercise-headline)
      (let ((raw-str (org-element-property :raw-value exercise-headline)))
	(string-match "\\(.*\\): +\\(.*\\)$" raw-str)
	`(,(match-string 1 raw-str) . ,(match-string 2 raw-str))))))

(defun get-exercises-from-buffer ()
  "Parse the current Org mode buffer and return all exercises.

The returned list is an alist with (date . exercise-list), where
exercise-list is an alist of (exercise-type . value) returned
from item-exercise-data"
  (org-element-map (org-element-parse-buffer) 'headline
    (lambda (item)
      (when (string-match "^Exercise$" (org-element-property :raw-value item))
	(let ((date (item-exercise-date item)))
	  `(,date . ,(item-exercise-data item)))))))

(defun get-exercise-set (exercises)
  "Return a sorted set of all exercise types from a list of exercise data.

EXERCISES is an alist of exercise type (string) and value (e.g.
number of sets for that exercise)."
  (let ((exercise-set '()))
    (dolist (rec exercises)
      (dolist (ex (cdr rec))
	      (unless (member (car ex) exercise-set) (push (car ex) exercise-set))))
    (sort exercise-set #'string-lessp)))

(defun get-exercise-table-header (exercise-set)
  "Return a list for the exercise table header.

This list consists of \"Date\" followed by a string for each
exercise type in the set.

EXERCISE-SET is a sorted list of distinct exercise types as
obtained from get-exercise-set."
  (let ((header '("Date")))
    (dolist (x exercise-set)
      (add-to-list 'header x t))
    header))

(defun build-exercise-data (exercises types)
  "Return a list of table rows containing the date and values for each exercise type.

EXERCISES is a list obtained from get-exercises-from-buffer.

TYPES is a set obtained from get-exercise-set."
  (mapcar (lambda (rec)
	    (let (
		  (row (mapcar (lambda (type)
				 (alist-get type (cdr rec) 0 nil 'string-equal))
			       types)))
	      (push (car rec) row)
	      row))
  exercises))

(defun output-exercise-table-for-buffer ()
  "Build and return the complete exercise data for the current Org mode buffer.

When evaluated in an Org mode code block, this function will
produce an exercise table."
  (let* (
	 (exercises (get-exercises-from-buffer))
	 (types (get-exercise-set exercises))
	 (header (get-exercise-table-header types))
	 (data (build-exercise-data exercises types)))
    `(,header . ,data)))
