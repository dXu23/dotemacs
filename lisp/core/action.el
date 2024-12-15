;;; action.el --- This was created from Karthik's article

;;; Commentary:
;;

(require 'embark)

(keymap-global-set "C-." #'embark-act)
(keymap-global-set "M-." #'embark-dwim)
(keymap-global-set "C-h B" #'embark-bindings)

(eval-when-compile
  (defmacro my/embark-ace-action (fn)
    `(defun ,(intern (concat "my/embark-ace-" (symbol-name fn))) ()
       (interactive)
       (with-demoted-errors "%s"
         (require 'ace-window)
         (let ((aw-dispatch-always t))
           (aw-switch-to-window (aw-select nil))
           (call-interactively (symbol-function ',fn)))))))

(keymap-set embark-file-map "o" (my/embark-ace-action find-file))
(keymap-set embark-buffer-map "o" (my/embark-ace-action switch-to-buffer))
(keymap-set embark-bookmark-map "o" (my/embark-ace-action bookmark-jump))

(eval-when-compile
  (defmacro my/embark-split-action (fn split-type)
  `(defun ,(intern (concat "my/embark-"
                           (symbol-name fn)
                           "-"
                           (car (last (split-string
                                       (symbol-name split-type) "-"))))) ()
     (interactive)
     (funcall #',split-type)
     (call-interactively #',fn))))

(keymap-set embark-file-map "2" (my/embark-split-action find-file split-window-below))
(keymap-set embark-buffer-map "2" (my/embark-split-action switch-to-buffer split-window-below))
(keymap-set embark-bookmark-map "2" (my/embark-split-action bookmark-jump split-window-below))

(keymap-set embark-file-map "3" (my/embark-split-action find-file split-window-right))
(keymap-set embark-buffer-map "3" (my/embark-split-action switch-to-buffer split-window-right))
(keymap-set embark-bookmark-map "3" (my/embark-split-action bookmark-jump split-window-right))

(provide 'core/action)

;;; action.el ends here
