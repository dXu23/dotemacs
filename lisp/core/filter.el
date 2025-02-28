;;; filter.el --- Filter workflow for Emacs

;;; Commentary:
;; This code comes from Karthink's article.
;;

;;; Code:

(require 'avy)
(require 'embark)

(customize-set-variable 'avy-case-fold-search nil)

(avy-setup-default)
(keymap-global-set "C-;" #'avy-goto-char-2)
(keymap-global-set "M-g c" #'avy-goto-char)
(keymap-global-set "M-g e" #'avy-goto-word-0)
(keymap-global-set "M-g g" #'avy-goto-line)
(keymap-global-set "M-g w" #'avy-goto-word-1)
(keymap-global-set "M-g t" #'avy-goto-char-timer)
(keymap-global-set "M-g P" #'avy-pop-mark)

(customize-set-variable 'avy-keys '(?q ?r ?u ?o ?p
                                    ?a ?s ?d ?f ?g ?h ?j
                                    ?l ?' ?c ?v ?b ?n ?, ?/))

(defun my/avy-action-kill-whole-line (pt)
  "Kills whole line that `PT' is on and pushes mark to avy-ring."
  (save-excursion
    (goto-char pt)
    (kill-whole-line))
  (select-window
   (cdr
    (ring-ref avy-ring 0)))
  t)

(setf (alist-get ?k avy-dispatch-alist) #'avy-action-kill-stay
      (alist-get ?K avy-dispatch-alist) #'my/avy-action-kill-whole-line)

(defun my/avy-action-copy-whole-line (pt)
  (save-excursion
    (goto-char pt)
    (pcase-let ((`(,start . ,end) (bounds-of-thing-at-point 'line)))
      (copy-region-as-kill start end)))
  (select-window
   (cdr
    (ring-ref avy-ring 0)))
  t)

(defun my/avy-action-yank-whole-line (pt)
  (my/avy-action-copy-whole-line pt)
  (save-excursion (yank))
  t)

(setf (alist-get ?y avy-dispatch-alist) #'avy-action-yank
      (alist-get ?w avy-dispatch-alist) #'avy-action-copy
      (alist-get ?W avy-dispatch-alist) #'my/avy-action-copy-whole-line
      (alist-get ?Y avy-dispatch-alist) #'my/avy-action-yank-whole-line)

(defun my/avy-action-teleport-whole-line (pt)
  (my/avy-action-kill-whole-line pt)
  (save-excursion (yank)) t)

(setf (alist-get ?t avy-dispatch-alist) #'avy-action-teleport
      (alist-get ?T avy-dispatch-alist) #'my/avy-action-teleport-whole-line)

(defun my/avy-action-mark-to-char (pt)
  (activate-mark)
  (goto-char pt))

(setf (alist-get ?  avy-dispatch-alist) #'my/avy-action-mark-to-char)

(defun my/avy-action-flyspell (pt)
  (save-excursion
    (goto-char pt)
    (when (require 'flyspell nil t)
      (flyspell-auto-correct-word)))
  (select-window
   (cdr (ring-ref avy-ring 0)))
  t)

(setf (alist-get ?\; avy-dispatch-alist) #'my/avy-action-flyspell)

(defun my/avy-action-helpful (pt)
  (save-excursion
    (goto-char pt)
    (helpful-at-point))
  (select-window
   (cdr (ring-ref avy-ring 0)))
  t)

(setf (alist-get ?H avy-dispatch-alist) #'my/avy-action-helpful)

(defun my/avy-action-embark (pt)
  "Use Embark to act on the item at `pt'"
  (unwind-protect
      (save-excursion
        (goto-char pt)
        (embark-act))
    (select-window
     (cdr (ring-ref avy-ring 0))))
  t)

(setf (alist-get ?. avy-dispatch-alist) #'my/avy-action-embark)

(defun my/avy-action-exchange (pt)
  "Exchange sexp at `pt' with the one at point."
  (set-mark pt)
  (transpose-sexps 0))

(setf (alist-get ?e avy-dispatch-alist) #'my/avy-action-exchange)

(provide 'core/filter)

;;; filter.el ends here
