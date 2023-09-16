(require 'corfu)

(define-key corfu-map (kbd "C-n") #'corfu-next)
(define-key corfu-map (kbd "C-p") #'corfu-previous)
(define-key corfu-map (kbd "<escape>") #'corfu-quit)
(define-key corfu-map (kbd "<return>") #'corfu-insert)
(define-key corfu-map (kbd "M-d") #'corfu-show-documentation)
(define-key corfu-map (kbd "M-l") #'corfu-show-location)

(global-corfu-mode)

(require 'cape)

(global-set-key (kbd "C-c p p") #'completion-at-point)
(global-set-key (kbd "C-c p t") #'complete-tag)
(global-set-key (kbd "C-c p d") #'cape-abbrev)
(global-set-key (kbd "C-c p f") #'cape-file)
(global-set-key (kbd "C-c p k") #'cape-keyword)
(global-set-key (kbd "C-c p s") #'cape-symbol)
(global-set-key (kbd "C-c p a") #'cape-abbrev)
(global-set-key (kbd "C-c p i") #'cape-ispell)
(global-set-key (kbd "C-c p l") #'cape-line)
(global-set-key (kbd "C-c p w") #'cape-dict)
(global-set-key (kbd "C-c p \\") #' cape-tex)
(global-set-key (kbd "C-c p _") #'cape-tex)
(global-set-key (kbd "C-c p ^") #'cape-tex)
(global-set-key (kbd "C-c p &") #'cape-sgml)
(global-set-key (kbd "C-c p r") #'cape-rfc1345)

(advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)
(advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify)

(provide 'core/completion)
