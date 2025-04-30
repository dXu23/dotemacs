;;; elfeed.el --- Elfeed customization -*- lexical-binding: t; -*-

;;; Commentary:
;;

(require 'elfeed)

(customize-set-variable 'elfeed-feeds
                        '(
                          "https://nullprogram.com/feed"
                          "http://devonzuegel.com/feed.xml"
                          "http://feeds.feedburner.com/JakeMccrarysMusings"
                          "https://www.socialmediatoday.com/feeds/news/"
                          "https://simondobson.org/rss.xml"
                          "https://michaelnielsen.org/ddi/feed/"
                          ("https://weworkremotely.com/categories/remote-devops-sysadmin-jobs.rss" jobs)
                          ("https://apnews.com/index.rss" news)
                          ("https://congressionaldish.libsyn.com/rss" news)
                          ("https://www.reutersagency.com/feed/?taxonomy=best-topics&post_type=best" news)
                          "https://skeptric.com/index.xml"
                          ("https://feeds.simplecast.com/Xf79_U6x" jobs)
                          "https://realsocialseo.com/feed/"
                          ("https://weworkremotely.com/categories/remote-programming-jobs.rss" jobs)
                          "https://grass.show/index.xml"
                          "https://tilde.town/~ramin_hal9001/atom.xml"
                          "https://blog.lilydjwg.me/posts.rss"
                          "https://karthinks.com/software/index.xml"
                          "https://www.nayuki.io/rss20.xml"
                          "https://dannyda.com/feed"
                          "https://arcan-fe.com/feed/"
                          "https://susam.net/feed.xml"
                          ))

(customize-set-variable 'elfeed-show-entry #'my/elfeed-display-buffer)


(defun my/elfeed-display-buffer (buf &optional act)
  (pop-to-buffer buf)
  (set-window-text-height (get-buffer-window) (round (* 0.7 (frame-height)))))


(defun my/elfeed-search-show-entry-pre (&optional lines)
  "Returns a function to scroll forward or backward in the Elfeed
search results, displaying entries without switching to them."
  (lambda (times)
    (interactive "p")
    (forward-line (* times (or lines 0)))
    (recenter)
    (call-interactively #'elfeed-search-show-entry)
    (select-window (previous-window))
    (unless elfeed-search-remain-on-entry (forward-line -1))))

(keymap-set elfeed-search-mode-map "n" (my/elfeed-search-show-entry-pre +1))
(keymap-set elfeed-search-mode-map "p" (my/elfeed-search-show-entry-pre -1))
(keymap-set elfeed-search-mode-map "M-RET" (my/elfeed-search-show-entry-pre))


(defun my/elfeed-scroll-up-command (&optional arg)
  "Scroll up or go to next feed item in Elfeed"
  (interactive "^P")
  (let ((scroll-error-top-bottom nil))
    (condition-case-unless-debug nil
        (scroll-up-command arg)
      (error (elfeed-show-next)))))



(defun my/elfeed-scroll-down-command (&optional arg)
  "Scroll down or go to previous feed item in Elfeed"
  (interactive "^P")
  (let ((scroll-error-top-bottom nil))
    (condition-case-unless-debug nil
        (scroll-down-command arg)
      (error (elfeed-show-prev)))))



(keymap-set elfeed-show-mode-map "SPC" #'my/elfeed-scroll-up-command)
(keymap-set elfeed-show-mode-map "S-SPC" #'my/elfeed-scroll-down-command)

(add-hook 'elfeed-new-entry-hook #'elfeed-declickbait-entry)


(defun elfeed-declickbait-entry (entry)
  (let ((title (elfeed-entry-title entry)))
    (setf (elfeed-meta entry :title)
          (elfeed-title-transform title))))



(defun elfeed-title-transform (title)
  "Declickbait string TITLE."
  (let* ((trim "\\(?:\\(?:\\.\\.\\.\\|[!?]\\)+\\)")
         (arr (split-string title nil t trim))
         (s-table (copy-syntax-table)))
    (modify-syntax-entry ?\' "w" s-table)
    (with-syntax-table s-table
      (mapconcat (lambda (word)
                   (cond
                    ((member word '("AND" "OR" "IF" "ON" "IT" "TO"
                                    "A" "OF" "VS" "IN" "FOR" "WAS"
                                    "IS" "BE"))
                     (downcase word))
                    ((member word '("WE" "DAY" "HOW" "WHY" "NOW" "OLD"
                                    "NEW" "MY" "TOO" "GOT" "GET" "THE"
                                    "ONE" "DO" "YOU"))
                     (capitalize word))
                    ((> (length word) 3) (capitalize word))
                    (t word)))
                 arr " "))))



(defun my/elfeed-tag-selection-as (mytag)
  "Returns a function that tags an elfeed entry or selection as MYTAG"
  (lambda ()
    "Toggle a tag on an Elfeed search selection"
    (interactive)
    (elfeed-search-toggle-all mytag)))

(keymap-set elfeed-search-mode-map "l" (my/elfeed-tag-selection-as 'readlater))
(keymap-set elfeed-search-mode-map "d" (my/elfeed-tag-selection-as 'junk))



(defun my/elfeed-show-eww-open (&optional use-generic-p)
  "open with eww"
  (interactive "P")
  (let ((browse-url-browser-function #'eww-browse-url))
    (elfeed-show-visit use-generic-p)))


(defun my/elfeed-search-eww-open (&optional use-generic-p)
  "open with eww"
  (interactive "P")
  (let ((browse-url-browser-function #'eww-browse-url))
    (elfeed-search-browse-url use-generic-p)))

(customize-set-variable 'browse-url-handlers
                        '(("https:\\/\\/www\\.youtu\\.*be." . my/browse-url-mpv)
                          ("." . browse-url-generic)))

(defun my/browse-url-mpv (url &optional single)
  (start-process "mpv" nil "mpv" (shell-quote-argument url)))

(provide 'init/elfeed)

;;; elfeed.el ends here
