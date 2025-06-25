;;; org.el --- Org Mode configuration -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;;; Code:

(use-package org
  :ensure nil
  :hook ((org-mode . org-indent-mode))
  :bind (("C-c o t s" . org-timer-start)
         ("C-c o t e" . org-timer-stop)
         ("C-c o t p" . org-timer-pause-or-continue)
         ("C-c o t t" . org-timer-set-timer)

         ("C-c o e" . org-babel-execute-src-block-maybe)

         ("C-c o c" . org-capture)
         
         ;; Specific maps
         :map org-mode-map
         ("C-s" . consult-org-heading)  ; MAYBE `consult-outline'
         )
  :config
  (setq org-src-lang-modes
        (append
         (seq-map (lambda (al)
                    ;; Use Tree Sitter for C and C++ in Org Mode Source Blocks
                    (cond
                     ((or (string-equal (car al) "C")
                          (string-equal (car al) "c"))
                      '("C" . c-ts))
                     ((or (string-equal (car al) "C++")
                          (string-equal (car al) "cpp"))
                      '("C++" . c++-ts))
                     (t al)))
                  org-src-lang-modes)
         ;; TODO Maybe move it to languages/go.el configuration
         '(("go" . go-ts) ("Go" . go-ts) ("golang" . go-ts) ("Golang" . go-ts))))
  :custom
  (org-directory "~/org")
  (org-capture-templates
   `(("l" "Learning Task" entry
      (file+headline "~/org/agenda/YandexLearning.org" "Tasks")
      "* TODO %?\nSCHEDULED: <%<%Y-%m-%d %a>>\n%a\n%i"
      :empty-lines 1
      :prepend t)))

  ;; TODO Check it
  (org-confirm-babel-evaluate nil)      ; No confirmation for Org Babel evaluation
  (org-id-locations-file "~/org/cache/.org-id-locations")
  (org-deadline-warning-days 7)
  (org-clock-sound (concat user-emacs-directory "assets/org-clock-sound.wav")))

;;; org.el ends here
