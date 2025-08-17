;;; go-testing.el --- Beatifull Golang testing -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Klementiev Dmitry <klementievd08@yandex.ru>
;;
;; Author:  Klementiev Dmitry
;; Email:   klementievd08@yandex.ru
;; License: None
;; Date:    2025-08-17 23:12
;;
;;; Commentary:
;;
;; Just better go testing mode.
;;
;;; Code:

(defcustom go-testing-buffer-major-mode 'org-mode
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

    (setq-local buffer-read-only nil)

    (if (not existing-buffer)
        (insert (concat "-*- mode: "
                        (string-remove-suffix
                         "-mode"
                         (prin1-to-string go-testing-buffer-major-mode))
                        "; -*-\n\n"))
      (insert "\n"))

    (insert
     (concat "* " (format-time-string "%Y-%m-%d %H:%M:%S") "\n\n"))
    (end-of-buffer)

    (insert "#+begin_example\n")

    (insert (go-testing--run-all))

    (insert "#+end_example\n")

    (setq-local buffer-read-only t)
    
    (funcall go-testing-buffer-major-mode)

    (local-set-key "q" 'kill-current-buffer)
    (local-set-key "p" 'previous-line)
    (local-set-key "P" 'org-previous-visible-heading)
    (local-set-key "n" 'next-line)
    (local-set-key "N" 'org-next-visible-heading)
    
    (hl-line-mode 1)

    (previous-buffer)
    (unless existing-buffer
      (display-buffer-in-side-window buffer '()))))

(provide 'go-testing)

;;; go-testing.el ends here
