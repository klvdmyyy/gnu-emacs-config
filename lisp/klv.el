;;; klv.el -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;; Code:

;; Specific constants

(defconst before-pairs-list (list ?\( ?\[ ?\{)
  "Opening pairs list constant")
(defconst after-pairs-list (list ?\) ?\] ?\})
  "Closing pairs list constant")

;; Functions

(defun which-linux-distribution ()
  "Maybe useful for future development of my GNU Emacs configuration.

I don't use it now.

[IMPORTANT] You need a `lsb_release' executable. Without it function doesn't
work and just return Unknown."
  (interactive)
  (when (eq system-type 'gnu/linux)
    (let* ((lsb_release (executable-find "lsb_release"))
           (cmd (if lsb_release (concat lsb_release " -sd") nil))
           (result (if cmd (shell-command-to-string cmd) "Unknown")))
      (message result))))

;; Macroses

(defmacro cmd! (&rest args)
  `(lambda () (interactive) ,@args))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;; Org Roam ;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO Move and make more beatifull

(defun roam-make-header (&key tags)
  "Make a header for Org Roam capture

#+title: ${title}
#+author: `user-full-name'
#+email: `user-mail-address'
#+date: %<%Y-%m-%d>
#+filetags: <tags>

Following fields doesn't mean anything:
#+language: Russian
#+license: CC BY-SA 4.0"
  (let* ((klv/roam-headers `("#+title: ${title}"
                             ,(concat "#+author: " user-full-name)
                             ,(concat "#+email: " user-mail-address)
                             "#+date: %<%Y-%m-%d>"))
         (headers (seq-map (lambda (h) (concat h "\n")) klv/roam-headers))
         (tags (or tags '()))
         (with-tags (append headers
                            (list
                             (concat "#+filetags: :"
                                     (apply #'concat (seq-map
                                                      (lambda (tg)
                                                        (concat tg ":"))
                                                      tags)))))))
    (apply #'concat (append with-tags '("\n")))))

(defun roam-make-daily-header ()
  "Make a header for daily Org Roam capture

#+title: %<%Y-%m-%d>
#+author: `user-full-name'
#+email: `user-mail-address'
#+filetags: dailies:%<%Y-%m-%d>:daily

Following fields doesn't mean anything:
#+language: Russian
#+license: CC BY-SA 4.0"
  (let* ((klv/roam-headers `("#+title: %<%Y-%m-%d>"
                             ,(concat "#+author: " user-full-name)
                             ,(concat "#+email: " user-mail-address)))
         (headers (seq-map (lambda (h) (concat h "\n")) klv/roam-headers))
         (tags '("dailies" "%<%Y-%m-%d>" "daily"))
         (with-tags (append headers
                            (list
                             (concat "#+filetags: :"
                                     (apply #'concat (seq-map
                                                      (lambda (tg)
                                                        (concat tg ":"))
                                                      tags)))))))
    (apply #'concat (append with-tags '("\n")))))

(defconst klv/roam-dailies-capture-templates
  `(("d" "default" entry
     "* %?"
     :target (file+head "%<%Y-%m-%d>.org"
                        ,(roam-make-daily-header)))))

(defconst klv/roam-capture-templates
  `(("y" "Yandex" plain
     "%?"
     :target (file+head "yandex/${slug}.org"
                        ,(roam-make-header
                          :tags '("yandex")))
     :unarrowed t)
    ("p" "Programming" plain
     "%?"
     :target (file+head "programming/${slug}.org"
                        ,(roam-make-header
                          :tags '("programming"))))
    ("a" "Algorithms" plain
     "#+begin_src go :imports '(\"fmt\")%?\n#+end_src\n"
     :target (file+head "algorithms/${slug}.org"
                        ,(roam-make-header
                          :tags '("algorithm")))
     :unarrowed t)
    ("d" "Data Structures" plain
     "#+begin_src go :imports '(\"fmt\")%?\n#+end_src\n"
     :target (file+head "data_structures/${slug}.org"
                        ,(roam-make-header
                          :tags '("data_structure"))))
    ("b" "Business" plain
     "%?"
     :target (file+head "business/${slug}.org"
                        ,(roam-make-header
                          :tags '("business")))
     :unarrowed t)))

(provide 'klv)

;;; klv.el ends here
