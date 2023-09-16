;; Windmove map
(defvar windmove-repeat-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<left>") 'windmove-left)
    (define-key map (kbd "S-<left>") 'windmove-swap-states-left)
    (define-key map (kbd "<right>") 'windmove-right)
    (define-key map (kbd "S-<right>") 'windmove-swap-stats-right)
    (define-key map (kbd "<up>") 'windmove-up)
    (define-key map (kbd "S-<up>") 'windmove-swap-states-up)
    (define-key map (kbd "<down>") 'windmove-down)
    (define-key map (kbd "S-<down>") 'windmove-swap-states-down)
    map))

(map-keymap
 (lambda (_key cmd)
   (when (symbolp cmd)
     (put cmd 'repeat-map 'windmove-repeat-map)))
 windmove-repeat-map)

;; Winner mode
(winner-mode 1)

(provide 'core/window)
