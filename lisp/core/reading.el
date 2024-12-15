;;; reading.el --- Reading mode

;;; Commentary:
;;

(require 'olivetti)

(defun my/center-visual-fill-on ()
  "Center the text in one column."
  (visual-line-mode 1)
  (setq olivetti-body-width 100)
  (olivetti-mode 1))

(defun my/center-visual-fill-off ()
  (visual-line-mode 0)
  (kill-local-variable 'olivetti-body-width)
  (olivetti-mode 0))

(defun my/kill-modeline ()
  "Temporariliy removes the modeline."
  (setq-local mode-line-format nil))

(defun my/restore-modeline ()
  "Restores the modeline."
  (kill-local-variable 'mode-line-format))

(defun my/toggle-modeline ()
  (if (null mode-line-format)
      (my/restore-modeline)
    (my/kill-modeline)))

(defun my/hide-cursor ()
    "Hides the cursor."
  (setq-local cursor-type nil))

(defun my/show-cursor ()
  (kill-local-variable 'cursor-type))

(defun my/toggle-cursor (&optional arg)
  "Toggles the cursor."
  (if (null cursor-type)
      (my/hide-cursor)
    (my/show-cursor)))

(define-minor-mode reading-mode
  "Toggle reading mode"
  :version "28.1"
  (if reading-mode
      (progn
        (my/center-visual-fill-on)
        (my/hide-cursor)
        (my/kill-modeline)
        )
    (progn
      (my/center-visual-fill-off)
      (my/show-cursor)
      (my/restore-modeline))))

(provide 'core/reading)

;;; reading.el ends here
