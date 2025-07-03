;;; hl-todo.el --- Hightlight TODO keywords -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;; Code:

(use-package hl-todo
  :hook ((prog-mode . hl-todo-mode))
  :custom
  (hl-todo-require-punctuation t)
  (hl-todo-highlight-punctuation ":"))

;;; hl-todo.el ends here
