;;; filter.el --- Filter workflow for Emacs

;;; Commentary:
;; This code comes from Karthink's article.
;;

;;; Code:

(customize-set-variable 'avy-case-fold-search nil)
(keymap-global-set "C-;" #'avy-goto-char-2)
(keymap-global-set "C-'" #'avy-goto-line)
(keymap-global-set "M-g c" #'avy-goto-char)
(keymap-global-set "M-g e" #'avy-goto-word-0)
(keymap-global-set "M-g g" #'avy-goto-line)
(keymap-global-set "M-g w" #'avy-goto-word-1)
(keymap-global-set "M-g (" #'avy-goto-open-paren)
(keymap-global-set "M-g )" #'avy-goto-close-paren)
(keymap-global-set "M-g t" #'avy-goto-char-timer)
(keymap-global-set "M-g P" #'avy-pop-mark)

(customize-set-variable 'avy-keys '(?q ?e ?r ?y ?u ?o ?p
                                       ?a ?s ?d ?f ?g ?h ?j
                                       ?k ?l ?' ?x ?c ?v ?b
                                       ?n ?, ?/))

(defun my/avy-action-kill-whole-line (pt)
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
    (pcase-let (('(,start . ,end) (bounds-of-thing-at-point 'line)))
      (copy-region-as-kill start end)))
  (select-window
   (cdr
    (ring-ref avy-ring 0)))
  t)

(defun avy-action-yank-whole-line (pt)
  (avy-action-copy-whole-line pt)
  (save-excursion (yank))
  t)

(setf (alist-get ?y avy-dispatch-alist) #'avy-action-yank
      (alist-get ?w avy-dispatch-alist) #'avy-action-copy
      (alist-get ?W avy-dispatch-alist) #'my/avy-action-copy-whole-line
      (alist-get ?Y avy-dispatch-alist) #'my/avy-action-yank-whole-line)

(defun my/avy-action-teleport-whole-line (pt)
  (avy-action-kill-whole-line pt)
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

(setf (alist-get ?H avy-dispatch-alist) #'avy-action-helpful)

(defun my/avy-action-embark (pt)
  (unwind-protect
      (save-excursion
        (goto-char pt)
        (embark-act))
    (select-window
     (cdr (ring-ref avy-ring 0))))
  t)

(setf (alist-get ?. avy-dispatch-alist) #'my/avy-action-embark)

(provide 'core/filter)

;;; filter.el ends here
