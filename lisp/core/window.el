;;; window.el --- Settings for Emacs window management -*- lexical-binding: t; -*-

(windmove-default-keybindings)

;; Windmove map
(defvar windmove-repeat-map
  (let ((map (make-sparse-keymap)))
    (keymap-set map "<left>" 'windmove-left)
    (keymap-set map "S-<left>" 'windmove-swap-states-left)
    (keymap-set map "<right>" 'windmove-right)
    (keymap-set map "S-<right>" 'windmove-swap-stats-right)
    (keymap-set map "<up>" 'windmove-up)
    (keymap-set map "S-<up>" 'windmove-swap-states-up)
    (keymap-set map "<down>" 'windmove-down)
    (keymap-set map "S-<down>" 'windmove-swap-states-down)
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

(keymap-global-set "M-o" 'other-window-alternating)

(put 'other-window-alternating 'repeat-map 'other-window-repeat-map)
(keymap-set other-window-repeat-map "o" 'other-window-alternating)

(provide 'core/window)
