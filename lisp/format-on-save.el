;;; format-on-save.el --- Formatting on save for GNU Emacs -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;; Данный пакет позволяет упростить менеджмент автоматического форматирования.
;;
;; FIXME: clang-format doesn't work properly.
;;
;;; Code:

;; Built-in formatting function for Emacs Lisp.
(defun emacs-lisp-format-buffer ()
  "Just indent Emacs Lisp buffer from `point-min' to `point-max'."
  (interactive)
  (indent-region (point-min) (point-max)))

(defun format-on-save-tabify-buffer ()
  "Tabify current buffer."
  (interactive)
  (tabify (point-min) (point-max)))

(defun format-on-save-untabify-buffer ()
  "Untabify current buffer."
  (interactive)
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

(defcustom format-on-save-tabified-major-modes
  '(makefile-mode
    nasm-mode
    asm-mode)
  "Hard-tabified major modes."
  :group 'format-on-save
  :type 'list
  :safe 'listp)

(defun format-on-save--format ()
  "Format buffer by formatters.

Also this function untabifys current buffer
if spaces prefered."
  ;; Always untabify buffers if spaces prefered.
  (if (or indent-tabs-mode
          (member major-mode format-on-save-tabified-major-modes))
      (funcall 'format-on-save-tabify-buffer)
    (funcall 'format-on-save-untabify-buffer))

  ;; Format buffer.
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
