;;; zig.el --- Zig language -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;; Code:

(defun zig-semicolon-maybe-after-pairs ()
  (interactive)
  (if (and (close-pair-p (char-before (point)))
           (open-pair-p (char-before (- (point) 1))))
      (progn (insert ";")
             (backward-char 2))
    (insert ";")))

(use-package zig-mode
  :mode ("\\.zig\\'" . zig-mode)
  :config
  (define-key zig-mode-map (kbd ";") 'zig-semicolon-maybe-after-pairs))

;;; zig.el ends here
