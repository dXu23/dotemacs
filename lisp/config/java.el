(require 'cc-mode)

(defun my/get-constructor-name (node)
  "Gets Treesitter name for `node'"
  (treesit-node-text
   (treesit-node-child-by-field-name "name")))

(defun my/get-field-name (node)
  (treesit-node-text
   (treesit-node-child-by-field-name (treesit-node-child-by-field-name last "declarator") "name")))


;;;###autoload
(defun my/java-setup ()
  (hs-minor-mode)
  (setq c-basic-offset 4
        tab-width 4
        lsp-java-compile-null-analysis-mode "automatic")
  (setq-local treesit-simple-imenu-settings
              '(("Class" "\\`class_declaration\\'" nil nil)
                ("Interface" "\\`interface_declaration\\'"  nil nil)
                ("Enum" "\\`enum_declaration\\'"  nil nil)
                ("Constructor" "\\`constructor_declaration\\'"  nil my/get-constructor-name)
                ("Field" "\\`field_declaration\\'" nil my/get-field-name)
                ("Method" "\\`method_declaration\\'" nil nil)))
  (glasses-mode 1))

(provide 'config/java)
