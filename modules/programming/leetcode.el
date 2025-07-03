;;; leetcode.el --- LeetCode Programming module -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;; Leet Code client for GNU Emacs.
;;
;;; Code:

(use-package leetcode
  :commands (leetcode)
  :custom
  (leetcode-directory "~/leetcode")
  (leetcode-save-solutions t)
  (leetcode-prefer-language "go")
  (leetcode-prefer-sql "postgresql"))

;;; leetcode.el ends here
