;;; klv-features.el --- Features -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;; Code:

(defconst klv--builtin-use-font-declarations
  '(("Fira Code"
     :github "ryanoasis/nerd-fonts"
     :release "v3.1.1/FiraCode.zip")
    ("JetBrains Mono"
     TODO: JetBrains Mono url)))

(defmacro KlvEmacs (&rest args)
  '())

(KlvEmacs
 :this-person
 ("Dmitry Klementiev"
  "klementievd08@yandex.ru"
  :github "klvdmyyy")

 :use-font "Fira Code"     ; Built-in `use-font' declaration in constant

 :appearance
 (visual-fill-column
  spacemacs-theme
  taoline)

 :completion
 (vertico
  orderless
  marginalia
  consult
  company)

 :languages
 ((go +tree-sitter)
  (c +tree-sitter)
  (cpp +tree-sitter)
  dockerfile
  yaml
  protobuf))

;;; klv-features.el ends here
