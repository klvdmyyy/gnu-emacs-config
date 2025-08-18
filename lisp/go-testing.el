;;; go-testing.el --- Beatifull Golang testing -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Klementiev Dmitry <klementievd08@yandex.ru>
;;
;; Author:  Klementiev Dmitry
;; Email:   klementievd08@yandex.ru
;; License: None
;; Date:    2025-08-17 18:12
;;
;;; Commentary:
;;
;; Just better go testing mode.
;;
;; FIXME: Font lock default for go-testing-mode doesn't works.
;;
;;; Code:

;;; Majore mode:

(eval-when-compile
  (require 'rx))

(defface go-testing-pass-face '((t :foreground "green"))
  "Go Testing PASS face."
  :group 'go-testing)

(defface go-testing-fail-face '((t :foreground "red"))
  "Go Testing FAIL face."
  :group 'go-testing)

(defconst go-testing-font-lock-defaults
  (let ((pass '("PASS"))
        (fail '("FAIL")))
    `((("RUN"  (0 go-testing-pass-face))
       ("PASS" (0 go-testing-pass-face))
       ("FAIL" (0 go-testing-fail-face))))))

(defvar go-testing-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'kill-current-buffer)
    (define-key map (kbd "p") 'previous-line)
    (define-key map (kbd "n") 'next-line)
    map))

(define-derived-mode go-testing-mode special-mode "go-testing"
  "Major mode for Go Testing output."
  (setq font-lock-defaults go-testing-font-lock-defaults))

;;; Testing functions:

(defcustom go-testing-buffer-major-mode 'go-testing-mode
  "Major mode for Golang testing buffer."
  :group 'go-testing
  :type 'symbol)

(defun go-testing--bufer-name ()
  "Return a string with buffer name."
  (concat "*Go Testing: " (project-root (project-current)) "*"))

(defun go-testing--files ()
  "Return a list with go testing files."
  (directory-files-recursively (project-root (project-current)) "_test\\.go\\'"))

(defun go-testing--directories ()
  "Return a list of directories with go testing files."
  (delete-dups
   (seq-map 'file-name-directory (go-testing--files))))

(defun go-testing--run-all ()
  "Run all go testing directories and return output."
  (apply 'concat
         (seq-map
          (lambda (dir)
            (shell-command-to-string (concat "go test " dir " -cover -v")))
          (go-testing--directories))))

;;;###autoload
(defun go-testing-project ()
  "Run testing in project for all Golang files."
  (interactive)
  (let* ((name (go-testing--bufer-name))
         (existing-buffer (get-buffer name))
         (buffer (get-buffer-create name)))
    (switch-to-buffer buffer)
    (erase-buffer)

    (setq-local buffer-read-only nil)

    ;; (if (not existing-buffer)
    ;;     (insert (concat "-*- mode: "
    ;;                     (string-remove-suffix
    ;;                      "-mode"
    ;;                      (prin1-to-string go-testing-buffer-major-mode))
    ;;                     "; -*-\n\n"))
    ;;   (insert "\n"))

    (insert
     (concat (format-time-string "%Y-%m-%d %H:%M:%S") "\n"
			 "^^^^^^^^^^^^^^^^^^^" "\n\n"))
    (end-of-buffer)

    (insert (go-testing--run-all))

    (setq-local buffer-read-only t)
    
    (funcall go-testing-buffer-major-mode)
    
    (hl-line-mode 1)

    (previous-buffer)
    (unless existing-buffer
      (display-buffer-in-side-window buffer '()))))

(provide 'go-testing)

;;; go-testing.el ends here
