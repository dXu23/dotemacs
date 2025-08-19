(require 'auctex)
(require 'preview-latex)

(setopt TeX-auto-save t
        TeX-parse-self t)

(setq-default TeX-master nil)

(defun my/LaTeX-self-insert (&optional arg char)
  "`self-insert-command' for LaTeX mode.
If the previous word is just a single character, surround it with
dollar signs. If already in math mode, do nothing. If the
character is a single `a', do nothing.

If called with a single \\[universal-argument], just call
`self-insert-command'."
  (interactive "P")
  (pcase arg
    ('(4) (self-insert-command 1)
     (_ (let ((ppoint (save-excursion (backward-word)       (point)))
              (ipoint (save-excursion (back-to-indentation) (point)))
              (word   (word-at-point)))
          (unless (or (length> word 1)
                      (not word)
                      (= ipoint ppoint)
                      (equal "a" word)
                      (number-at-point)
                      (texmathp))
            (pcase-let ('(open . close) math-delimiters-inline)
              (backward-char)
              (insert open)
              (forward-char 1)
              (insert-close)))
          (self-insert-command 1 char))))))

;;;###autoload
(defun my/latex-setup ()
  (push ("\\blank"      . ?—) prettify-symbols-alist)
  (push ("\\otimes"     . ?⨂) prettify-symbols-alist)
  (push ("\\defeq"      . ?≔) prettify-symbols-alist)
  (push ("\\mathcal{C}" . ?𝓒) prettify-symbols-alist)
  (push ("\\cat{C}"     . ?𝓒) prettify-symbols-alist)

  (push ("{}"      . ("emph"                    )) TeX-fold-macro-spec-list)
  (push ("[1]:||►" . ("item"                    )) TeX-fold-macro-spec-list)
  (push ("§ {1}"   . ("section" "section*"      )) TeX-fold-macro-spec-list)
  (push ("§§ {1}"  . ("subsection" "subsection*")) TeX-fold-macro-spec-list)
  (push ("¶ {1}"   . ("paragraph" "paragraph*"  )) TeX-fold-macro-spec-list)

  (prettify-symbols-mode)
  (cdlatex-mode)
  )

(provide 'config/latex)
