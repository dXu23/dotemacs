;; Utility libraries
(require 'seq)

;;; Vertico configuration
(require 'vertico)
(require 'vertico-sort)
(setopt vertico-count 13
        vertico-resize t
        vertico-cycle nil
        vertico-sort-function #'vertico-sort-history-length-alpha)

(keymap-set vertico-map "<tab>" #'vertico-insert)
(keymap-set vertico-map "<escape>" #'minibuffer-keyboard-quit)
(keymap-set vertico-map "C-M-n" #'vertico-next-group)
(keymap-set vertico-map "C-M-p" #'vertico-previous-group)

;;;; Vertico flat
(require 'vertico-flat)

;;;; Vertico indexed
(require 'vertico-indexed)

;;;; Vertico grid
(require 'vertico-grid)
(customize-set-variable 'vertico-grid-separator "       ")
(customize-set-variable 'vertico-grid-lookahead 50)

;;;; Vertico mouse
(require 'vertico-mouse)

;;;; Vertico quick
(require 'vertico-quick)
(define-key vertico-map (kbd "C-i") #'vertico-quick-insert)
(define-key vertico-map (kbd "C-o") #'vertico-quick-exit)

;;;; Vertico buffer
(require 'vertico-buffer)
(customize-set-variable 'vertico-buffer-display-action '(display-buffer-reuse-window))

;;;; Vertico repeat
(require 'vertico-repeat)
(add-hook 'minibuffer-setup #'vertico-repeat-save)

;;;; Vertico reverse
(require 'vertico-reverse)

;;;; Vertico directory
(require 'vertico-directory)

(keymap-set vertico-map "<backspace>" #'vertico-directory-delete-char)
(keymap-set vertico-map "C-<backspace>" #'vertico-directory-delete-word)
(keymap-set vertico-map "RET" #'vertico-directory-enter)

;;;; Vertico unobtrusive
(require 'vertico-unobtrusive)

;;;; Vertico multiform
(require 'vertico-multiform)
(customize-set-variable 'vertico-multiform-categories
                        '((file reverse)
                          (imenu buffer)
                          (library reverse indexed)
                          (t reverse)))

(keymap-set vertico-map "M-G" #'vertico-multiform-grid)
(keymap-set vertico-map "M-F" #'vertico-multiform-flat)
(keymap-set vertico-map "M-R" #'vertico-multiform-reverse)
(keymap-set vertico-map "M-U" #'vertico-multiform-unobtrusive)

;;;; Vertico miscellaneous settings
(add-hook 'rfn-eshadow-update-overlay #'vertico-directory-tidy)
(add-hook 'minibuffer-setup #'vertico-repeat-save)

(advice-add #'vertico--format-candidate :around
            (lambda (orig cand prefix suffix index _start)
              (setq cand (funcall orig cand prefix suffix index _start))
              (concat
               (if (= vertico--index index)
                   (propertize "» " 'face 'vertico-current)
                 "  ")
               cand)))

(vertico-mode)
(vertico-multiform-mode)

;;; Marginalia configuration
(require 'marginalia)

(keymap-set minibuffer-local-map "M-a" 'marginalia-cycle)
(customize-set-variable 'marginalia-max-relative-age 0)
(customize-set-variable 'marginalia-align 'right)

(marginalia-mode)

;;; All-the-icons-completion
(require 'all-the-icons-completion)
(add-hook 'marginalia-mode #'all-the-icons-completion-marginalia-setup)
(all-the-icons-completion-mode)


;;; Orderless
(require 'orderless)
(setopt completion-styles '(orderless basic)
        customize-category-defaults nil
        customize-category-overrides '((file (styles basic-remote orderless))))

(customize-set-variable 'orderless-style-dispatchers
                        '(prot-orderless-literal-dispatcher
                          prot-orderless-strict-initialism-dispatcher
                          prot-orderless-flex-dispatcher))

(defun orderless--strict-*-initialism (component &optional anchored)
  "Match a COMPONENT as a strict initialism, optionally ANCHORED.
The characters in COMPONENT must occur in the candidate in that
order at the beginning of subsequent words comprised of letters.
Only non-letters can be used in between the words that start with the
initials.

If ANCHORED is `start' requres that the first initial appear in
the first word of the candidate. If ANCHORED is `both' require
that the first and last initials appear in the first and last
words of the candidate, respectively"
  (orderless--separated-by
      '(seq (zero-or-more alpha) word-end (zero-or-more (not alpha)))
    (seq-map (lambda (char) `(seq word-start char)) component)
    (when anchored '(seq (group buffer-start) (zero-or-more (not alpha))))
    (when (eq anchored 'both)
      '(seq (zero-or-more alpha) word-end (zero-or-more (not alpha)) eol))
    ))

(defun orderless-strict-initialism (component)
  "Match a COMPONENT as a strict initialism.
This means the characters in COMPONENT must occur in the
candidate in that order at the beginning of subsequent words
comprised of letter. Only non-letters can be in between the
words that start with the initials."
  (orderless--strict-*-initialism component))

(defun prot-orderless-literal-dispatcher (pattern _index _total)
  "Literal style dispatcher using the equals sign as a suffix.
It matches PATTERN _INDEX and _TOTAL according to how Orderless
parses its input."
  (when (string-suffix-p "=" pattern)
    `(orderless-literal . ,(substring pattern 0 -1))))

(defun prot-orderless-strict-initialism-dispatcher (pattern _index _total)
  "Leading initialism dispatcher using the comma suffix.
It matches PATTERN _INDEX and _TOTAL according to how Orderless
parses its input."
  (when (string-suffix-p "," pattern)
    `(orderless-strict-initialism . ,(substring pattern 0 -1))))

(defun prot-orderless-flex-dispatcher (pattern _index _total)
  "Flex dispatcher using the tilde suffix.
It matches PATTERN, _INDEX, and _TOTAL according to how Orderless
parses its input."
  (when (string-suffix-p "." pattern)
    `(orderless-flex . ,(substring pattern 0 -1))))

(require 'consult)
(require 'consult-register)

(keymap-global-set "C-x b" #'consult-buffer)
(keymap-global-set "C-x 4 b" #'consult-buffer-other-window)
(keymap-global-set "C-x 5 b" #'consult-buffer-other-frame)
(keymap-global-set "C-x r b" #'consult-bookmark)
(keymap-global-set "C-x p b" #'consult-project-buffer)
(keymap-global-set "M-y" #'consult-yank-pop)
(keymap-global-set "M-g g" #'consult-goto-line)
(keymap-global-set "M-g i" #'consult-imenu)

(setopt register-preview-delay 0.5
        register-preview-function #'consult-register-format)

(advice-add #'register-preview :override #'consult-register-window)

(setopt xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)


(provide 'core/minibuffer)

;;; core/minibuffer.el ends here
;;
;; Local Variables:
;; outline-regexp: ";;;\\(;* [^ \t\n]\\|###autoload\\)"
;; eval: (outline-minor-mode 1)
;; End:
