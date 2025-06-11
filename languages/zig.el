;;; zig.el
;;
;;; Commentary:
;;
;;; Code:

(defun zig-semicolon-maybe-after-pairs ()
  (if (and (member (char-before (point)) after-pairs-list)
           (member (char-before (- (point) 1)) before-pairs-list))
      (progn (insert ";")
             (backward-char 2))
    (insert ";")))

(use-package zig-mode
  :mode ("\\.zig\\'" . zig-mode)
  :config
  (define-key zig-mode-map (kbd ";") (cmd! (zig-semicolon-maybe-after-pairs))))

;;; zig.el ends here
