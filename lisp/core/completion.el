(require 'corfu)

(keymap-set corfu-map "C-n" #'corfu-next)
(keymap-set corfu-map "C-p" #'corfu-previous)
(keymap-set corfu-map "<escape>" #'corfu-quit)
(keymap-set corfu-map "<return>" #'corfu-insert)
(keymap-set corfu-map "M-d" #'corfu-show-documentation)
(keymap-set corfu-map "M-l" #'corfu-show-location)

(global-corfu-mode)

(require 'cape)

(keymap-global-set "C-c p p" #'completion-at-point)
(keymap-global-set "C-c p t" #'complete-tag)
(keymap-global-set "C-c p d" #'cape-abbrev)
(keymap-global-set "C-c p f" #'cape-file)
(keymap-global-set "C-c p k" #'cape-keyword)
(keymap-global-set "C-c p s" #'cape-symbol)
(keymap-global-set "C-c p a" #'cape-abbrev)
(keymap-global-set "C-c p i" #'cape-ispell)
(keymap-global-set "C-c p l" #'cape-line)
(keymap-global-set "C-c p w" #'cape-dict)
(keymap-global-set "C-c p \\" #'cape-tex)
(keymap-global-set "C-c p _" #'cape-tex)
(keymap-global-set "C-c p ^" #'cape-tex)
(keymap-global-set "C-c p &" #'cape-sgml)
(keymap-global-set "C-c p r" #'cape-rfc1345)

(advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)
(advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify)

(provide 'core/completion)
