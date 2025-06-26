;;; tree-sitter.el --- Tree Sitter -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;; Code:

(use-package treesit
  :ensure nil
  :hook ((after-init . treesit-install-all))
  :init
  ;; Tree Sitter source
  (setq treesit-language-source-alist
        '((go "https://github.com/tree-sitter/tree-sitter-go")
          (gomod "https://github.com/camdencheek/tree-sitter-go-mod")
          (c "https://github.com/tree-sitter/tree-sitter-c")
          (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
          (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
          (yaml "https://github.com/ikatyang/tree-sitter-yaml")
          (python "https://github.com/tree-sitter/tree-sitter-python")
          (bash "https://github.com/tree-sitter/tree-sitter-bash")))

  :config
  (defun treesit-install-all ()
    "Installs all language grammars from `treesit-language-source-alist' variable
by `treesit-install-language-grammar' function.

This function install language grammar only when it unavailable."
    (interactive)
    (mapc
     (lambda (lang)
       (when (not (treesit-language-available-p lang))
         (treesit-install-language-grammar lang)))
     (mapcar #'car treesit-language-source-alist))))

;;; tree-sitter.el ends here
