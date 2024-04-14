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

(provide 'core/window)
