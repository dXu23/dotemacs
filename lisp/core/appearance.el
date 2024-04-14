;; Get rid of all bars
(if (fboundp 'menu-bar-mode)
    (tool-bar-mode -1))

(if (fboundp 'tool-bar-mode)
    (menu-bar-mode -1))

(if (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1))

;;; load modus theme
(setq modus-themes-mode-line '(accented borderless padded)
      modus-themes-bold-constructs t
      modus-themes-italic-constructs t
      modus-themes-fringes 'subtle
      modus-themes-tabs-accented t
      modus-themes-paren-match '(bold intense)
      modus-themes-prompts '(bold intense)
      modus-themes-completions
      '((matches . (extrabold))
        (selection . (semibold italic text-also)))
      modus-themes-org-blocks 'tinted-background
      modus-themes-common-palete-overrides
      '((date-deadline magenta-warmer)
        (date-scheduled green-cooler)
        (date-weekday fg-main)
        (date-event fg-dim)
        (date-now blue)
        (prose-done fg-alt)
        (prose-todo yellow))
      modus-themes-region '(bg-only)
      modus-themes-syntax '(alt-syntax)
      modus-themes-headings
      '((1 . (rainbow overline background 1.4))
        (2 . (rainbow background 1.3))
        (3 . (rainbow bold 1.2))
        (t . (semilight 1.1)))
      modus-themes-scale-headings t)

(load-theme 'modus-vivendi t)

(set-face-attribute 'default nil :height 160)

; Necessary for Emacs client to work.
(add-to-list 'default-frame-alist '(font . "Fira Code 16"))

(require 'display-line-numbers)

(defcustom display-line-numbers-exempt-modes
  '(vterm-mode eshell-mode shell-mode inf-ruby-mode term-mode
               ansi-term-mode help-mode paradox-mode comint-mode
               inferior-emacs-lisp-mode pdf-view-mode)
  "Major modes on which to disable line numbers."
  :group 'display-line-numbers
  :type 'list
  :version "green")

(defun display-line-numbers--turn-on ()
  "Turn on line numbers except for certain major modes.
Exempt modes are defined in `display-line-numbers-exempt-modes'."
  (unless (or (minibufferp)
              (member major-mode display-line-numbers-exempt-modes))
    (display-line-numbers-mode)))

(global-display-line-numbers-mode 1)

;; Highlight patterns found in file automatically
(global-hi-lock-mode 1)
(setq hi-lock-file-patterns-policy #'(lambda (dummy) t))

;;; Change modeline
(column-number-mode 1)
(setq mode-line-position
        '((line-number-mode ("%l" (column-number-mode ":%c")))))

(defun shorten-directory (dir max-length)
  "Show up to MAX-LENGTH characters of a directory name DIR."
  (let ((path (reverse (split-string (abbreviate-file-name dir) "/")))
          (output ""))
    (when (and path (equal "" (car path)))
        (setq path (cdr path)))
    (while (and path (< (length output) (- max-length 4)))
        (setq output (concat (car path) "/" output))
        (setq path (cdr path)))
    (when path
        (setq output (concat ".../" output)))
    output))

(defvar mode-line-directory
  '(:propertize
    (:eval (if (buffer-file-name)
               (concat " " (shorten-directory default-directory 20)) " ")))
  "Formats the current directory.")

(put 'mode-line-directory 'risky-local-variable t)

(setq-default mode-line-buffer-identification
                (propertized-buffer-identification "%b "))

(setq-default mode-line-format
                '("%e"
                  mode-line-front-space
                  ;; mode-line-mule-info --
                  mode-line-client
                  mode-line-modified
                  ;; mode-line-remote -- no need to indicate this specially
                  ;; mode-line-frame-identification
                  " "
                  mode-line-directory
                  mode-line-buffer-identication
                  " "
                  mode-line-position
                  (flycheck-mode flycheck-mode-line)
                  " "
                  mode-line-modes
                  mode-line-misc-info
                  mode-line-end-spaces))

;;; Tabs
(defcustom tab-line-tab-min-width 10
  "Minimum width of a tab in characters."
  :type 'integer
  :group 'tab-line)

(defcustom tab-line-tab-max-width 30
  "Maximum width of a tab in characters."
  :type 'integer
  :group 'tab-line)

(defun my/tab-line-name-buffer (buffer &rest _buffers)
  (with-current-buffer buffer
    (let* ((window-width (window-width (get-buffer-window)))
           (close-button-size (if tab-line-close-button-show
                                  (length (substring-no-properties tab-line-close-button))
                                0))
           (tab-amount (length (tab-line-tabs-window-buffers)))
           (window-max-tab-width (/ window-width tab-amount))
           (tab-width (- (cond ((>= window-max-tab-width tab-line-tab-max-width)
                                ((< window-max-tab-width tab-line-tab-min-width)
                                 tab-line-tab-min-width)
                                (t window-max-tab-width))
                               close-button-size))
                      (buffer-name (string-trim (buffer-name)))
                      (name-width (length buffer-name)))))))

(provide 'core/appearance)
