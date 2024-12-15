;;; misc.el --- Miscellaneous elisp functions

;;; Commentary:
;;

;;; Code:

(defun get-wikipedia-references (subject)
  "Gets references for a wikipedia article.
Argument SUBJECT Wikipedia subject to obtain references on."
  (let* ((wikipedia-prefix-url "https://en.wikipedia.org/wiki/")
         (wikipedia-url (concat wikipedia-prefix-url subject)))
    (with-current-buffer
        (url-retrieve-synchronously wikipedia-url)
      (let* ((html-start (1+ url-http-end-of-headers))
             (dom (libxml-parse-html-region html-start (point-max)))
             result)
        (dolist (cite-tag (dom-by-tag dom 'cite) result)
          (let ((cite-class (dom-attr cite-tag 'class)))
            (cond ((string-search "journal" cite-class)
                   (let ((a-tag (dom-search cite-tag (lambda (tag) (string-prefix-p "https://doi.org" (dom-attr tag 'href))))))
                     (push (cons (concat "doi:" (dom-text a-tag))
                                              (let* ((cite-texts (dom-texts cite-tag))
                                                     (title-beg (1+ (string-search "\"" cite-texts)))
                                                     (title-end (string-search "\"" cite-texts (1+ title-beg))))
                                                (substring cite-texts title-beg title-end)))
                           result)))
                  ((string-search "book" cite-class)
                   (let ((a-tag (dom-search cite-tag (lambda (tag) (string-prefix-p "/wiki/Special:BookSources" (dom-attr tag 'href))))))
                     (push (cons (concat "isbn:" (dom-text (dom-child-by-tag a-tag 'bdi)))
                                 (dom-text (dom-child-by-tag cite-tag 'i)))
                           result)))
                  (t
                   (let ((a-tag (assoc 'a cite-tag)))
                     (push (cons (dom-attr a-tag 'href) (dom-text a-tag)) result))))
                  ))))))

(defun apply-function-to-region (fn)
  "Applies Elisp function that takes a string as
input and changes text in region to output"
  (interactive "XFunction to apply to region: ")
  (save-excursion
    (let* ((beg (region-beginning))
           (end (region-end))
           (resulting-text
            (funcall
             fn
             (buffer-substring-no-properties beg end))))
      (kill-region beg end)
      (insert resulting-text))))

(provide 'core/misc)

;;; misc.el ends here
