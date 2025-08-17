;;; format-on-save.el --- Formatting on save for GNU Emacs -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;; Данный пакет позволяет упростить менеджмент автоматического форматирования.
;;
;;; Code:

(defun emacs-lisp-format-buffer ()
  "Just indent and untabify Emacs Lisp buffer from `point-min' to `point-max'."
  (interactive)
  (indent-region (point-min) (point-max))
  (untabify (point-min) (point-max)))

(defcustom format-on-save-formatters
  '((emacs-lisp-mode . emacs-lisp-format-buffer)
    (c-mode . clang-format-buffer)
    (c++-mode . clang-format-buffer)
    (c-or-c++-mode . clang-format-buffer)
    (c-ts-mode . clang-format-buffer)
    (c++-ts-mode . clang-format-buffer)
    (c-or-c++-ts-mode . clang-format-buffer))
  "Formatters."
  :group 'format-on-save
  :type 'alist)

(defun format-on-save--format ()
  "Format buffer by formatters."
  (cond
   ;; Formatting functions from alist.
   ((alist-get major-mode format-on-save-formatters)
    (let ((format-p (alist-get major-mode format-on-save-formatters)))
      (funcall format-p)))

   ;; Formatting by eglot.
   ((eglot-managed-p)
    (eglot-format-buffer))

   ;; No formatter.
   (t (message "Formatter isn't found for current buffer."))))

(define-minor-mode format-on-save-mode
  "Format on save."
  :group 'format-on-save
  :global nil
  (if format-on-save-mode
      (add-hook 'before-save-hook 'format-on-save--format)
    (remove-hook 'before-save-hook 'format-on-save--format)))

(provide 'format-on-save)

;;; format-on-save.el ends here
