(require 'org)
(require 'org-bullets)

(require 'tex)
(require 'latex)

(require 'xref)
(require 'reftex-ref)

(setq org-plantuml-jar-path "/usr/share/plantuml/plantuml.jar")

;;; Org Babel
(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (awk . t)
   (ditaa . t)
   (dot . t)
   (emacs-lisp . t)
   (latex . t)
   (octave . t)
   (plantuml . t)
   (python . t)
   (R . t)
   (ruby . t)
   (shell . t)))

;;; Speed keys
(customize-set-variable 'org-use-speed-commands t)

(font-lock-add-keywords
 'org-mode
 '(("\\(\\(?:\\\\\\(?:label\\|ref\\|eqref\\)\\)\\){\\(.+?\\)}"
    (1 font-lock-keyword-face)
    (1 font-lock-constant-face))))

(defun my/show-org-tex-item-at-point ()
  (when (and (derived-p 'org-mode) (org-invisible-p))
    (org-fold-show-context 'link-search))
  (when eldoc-mode (eldoc--invoke-strategy t))
  (pcase-let
      ((`(,_ . ,ov)
        (get-char-property-and-overlay (point) 'TeX-fold-type))
       (when ov (TeX-fold-show-item ov)))))

(defun my/search-org-cite-face (text-prop-search-defun &optional tff)
  (let ((face-list '(font-lock-constant-face org-cite)))
    (funcall text-prop-search-defun
             'face
             nil
             (if tff
                 (cons 'TeX-fold-folded-face face-list)
               face-list)
             t)))

(defun my/next-reference-or-label (_arg)
  (interactive "p")
  (pcase-let
      ((`(,_ . ,ov)
        (get-char-property-and-overlay (point) 'Tex-fold-type))
       (prop (save-excursion
               (my/search-org-cite-face #'text-property-search-forward))))
    (when ov (TeX-fold-hide-item ov))
    (if prop
        (progn (goto-char (prop-match-beginning prop))
               (my/show-org-tex-item-at-point))
      (message "No more references/labels."))))

(defun my/previous-reference-or-label (_arg)
  (interactive "p")
  (pcase-let
      ((`(,_ . ,ov)
        (get-char-property-and-overlay (point) 'TeX-fold-type))
       (p (save-excursion
            (and (my/search-org-cite-face #'text-property-search-backward t)
                 (point)))))
    (when ov (TeX-fold-hide-item ov))
    (when p
      (goto-char p)
      (my/show-org-tex-item-at-point))))

(keymap-set org-mode-map "M-g r" #'my/next-reference-or-label)
(keymap-set org-mode-map "M-g R" #'my/previous-reference-or-label)

(defvar-keymap my/TeX-ref-map
  :repeat t
  "r" #'my/next-reference-or-label
  "R" #'my/previous-reference-or-label
  )

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql reftex)))
  (when (looking-back "\\\\\\(?:page\\|eq\\|auto\\|c\\)?ref{[-a-zA-Z0-9_*.:]*"
                      (line-beginning-position))
    (reftex-this-word "-a-zA-Z0-9_*.:")))

(cl-defmethod xref-backend-definitions ((_backend (eql reftex)) prompt)
  (unless (symbol-value reftex-docstruct-symbol) (reftex-parse-one))
  (when-let* ((docstruct (symbol-value reftex-docstruct-symbol))
              (data (assoc prompt docstruct))
              (label (nth 0 data))
              (file (nth 3 data))
              (buffer (or (find-buffer-visiting file)
                          (reftex-get-file-buffer-force
                           file (not reftex-keep-temporary-buffers))))
              (re (format reftex-find-label-regexp-format (regexp-quote label)))
              (found (with-current-buffer buffer
                       (or (re-search-backward re nil t)
                           (progn (goto-char (point-min))
                                  (re-search-forward
                                   (format reftex-find-label-regexp-format2
                                           (regexp-quote label))
                                   nil t))))))
    (list (xref-make prompt (xref-make-buffer-location
                             buffer found)))))

;; Set priority range from A to C with default A
(setq org-highest-priority ?A
      org-lowest-priority ?C
      org-default-priority ?A)

;; Set org-log-done to true
(customize-set-variable 'org-log-done 'time)

;;;; Turn off org-goto-auto-isearch
(customize-set-variable 'org-goto-auto-isearch nil)

(customize-set-variable 'org-format-latex-options
                        '(:foreground default :background default :scale 2.0 :html-foreground
                                      "Black" :html-background "Transparent" :html-scale 1.5
                                      :matchers ("begin" "$1" "$" "$$" "\\(" "\\[")))


(defun my/org-ispell ()
  "Configure `ispell-skip-region-alist' for `org-mode'."
  (make-local-variable 'ispell-skip-region-alist)
  (add-to-list 'ispell-skip-region-alist '(org-property-drawer-re))
  (add-to-list 'ispell-skip-region-alist '("~" "~"))
  (add-to-list 'ispell-skip-region-alist '("=" "="))
  (add-to-list 'ispell-skip-region-alist '("^#\\+begin_src" . "^#\\+end_src")))

;;;###autoload
(defun my/org-setup ()
  (org-bullets-mode)
  (auto-fill-mode)
  (prettify-symbols-mode)
  (my/org-ispell))

;; Open org agenda in current window
(setq org-agenda-window-setup (quote current-window))

;; Hide redundant tags in org-agenda
(setq org-agenda-hide-tags-regexp ".")

;; Warn about any deadline in 4 days
(setq org-deadline-warning-days 4)

;; Show tasks schedules/due in next fornight
(customize-set-variable 'org-agenda-span 'fortnight)

;; Do not show tasks as scheduled if already shown as deadline
(setq org-agenda-skip-scheduled-if-deadline-is-shown t)

;; Do not give warning colors to tasks w/ impending deadlines
(setq org-agenda-skip-deadline-prewarning-if-scheduled 'pre-scheduled)

(customize-set-variable 'org-agenda-skip-deadline-if-done t)

;;;###autoload
(defun my/org-agenda-setup ()
  (Tex-fold-mode))
