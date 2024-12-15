;;; window.el --- Settings for Emacs window management -*- lexical-binding: t; -*-

(windmove-default-keybindings)

;; Windmove map
(defvar windmove-repeat-map
  (let ((map (make-sparse-keymap)))
    (keymap-set map "<left>" 'windmove-left)
    (keymap-set map "C-<left>" 'windmove-swap-states-left)
    (keymap-set map "<right>" 'windmove-right)
    (keymap-set map "C-<right>" 'windmove-swap-stats-right)
    (keymap-set map "<up>" 'windmove-up)
    (keymap-set map "C-<up>" 'windmove-swap-states-up)
    (keymap-set map "<down>" 'windmove-down)
    (keymap-set map "C-<down>" 'windmove-swap-states-down)
    map))

(map-keymap
 (lambda (_key cmd)
   (when (symbolp cmd)
     (put cmd 'repeat-map 'windmove-repeat-map)))
 windmove-repeat-map)

;; Winner mode
(winner-mode 1)

(advice-add 'other-window :before
            (defun my/other-window-split-if-single (&rest _)
              "Split the frame if there is a single window."
              (when (one-window-p) (split-window-sensibly))))

(defalias 'other-window-alternating
  (let ((direction 1))
    (lambda (&optional arg)
      "Call `other-window', switching directions each time."
      (interactive)
      (if (equal last-command 'other-window-alternating)
          (other-window (* direction (or arg 1)))
        (setq direction (- direction))
        (other-window (* direction (or arg 1)))))))

(defun ace-window-prefix ()
  "Use `ace-window' to display the buffer of the next command.
The next buffer is the buffer displayed by the next command invoked
immediately after this command (ignoring reading from the minibuffer).
Creates a new window before displaying the buffer.
When `switch-to-buffer-obey-display-actions' is non-nil,
`switch-to-buffer' commands are also supported."
  (interactive)
  (display-buffer-override-next-command
   (lambda (buffer _)
     (let ((window (aw-select (propertize " ACE" 'face 'mode-line-highlight)))
           (type 'reuse))
       (cons window type)))
   nil "[ace-window]")
  (message "Use `ace-window' to display next command buffer..."))

(keymap-global-set "M-o" 'other-window-alternating)

(put 'other-window-alternating 'repeat-map 'other-window-repeat-map)
(keymap-set other-window-repeat-map "o" 'other-window-alternating)

(provide 'core/window)
