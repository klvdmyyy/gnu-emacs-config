;;; roam.el --- Org Roam module -*- lexical-bindings: t; -*-
;;
;;; Commentary:
;;
;;; Code:

(defun make-roam-header--parse-daily (args)
  (pcase args
    ((pred seq-empty-p) nil)
    (`(:daily t . ,next-args) t)
    (`(:daily nil . ,next-args) nil)
    (`(:daily . ,next-args) t)
    (code (make-roam-header--parse-daily (cdr args)))))

(defun make-roam-header--parse-tags (args)
  (pcase args
    ((pred seq-empty-p) nil)
    (`(:tags ,tags . ,next-args)
     tags)
    (code (make-roam-header--parse-tags (cdr args)))))

(defun make-roam-header--parse-no-author (args)
  (pcase args
    ((pred seq-empty-p) nil)
    (`(:no-author t . ,next-args) t)
    (`(:no-author nil . ,next-args) nil)
    (`(:no-author . ,next-args) t)
    (code (make-roam-header--parse-no-author (cdr args)))))

(defun make-roam-header--parse-no-email (args)
  (pcase args
    ((pred seq-empty-p) nil)
    (`(:no-email t . ,next-args) t)
    (`(:no-email nil . ,next-args) nil)
    (`(:no-email . ,next-args) t)
    (code (make-roam-header--parse-no-email (cdr args)))))

(defmacro make-roam-header (&rest args)
  "Making header for Org Roam capture.

Following keys are supported:

:no-author --- if `t' don't place author field in header
:no-email  --- if `t' don't place email field in header
:daily     --- if `t' don't place date field in header. instead use date as a title
:tags      --- list of default tags for cap"
  (let* ((daily (make-roam-header--parse-daily args))
         (no-author (make-roam-header--parse-no-author args))
         (no-email (make-roam-header--parse-no-email args))
         (tags (make-roam-header--parse-tags args))
         
         (title (if daily "#+title: %<%Y-%m-%d>" "#+title: ${title}"))
         (author (if no-author "" (concat "#+author: " user-full-name)))
         (email (if no-email "" (concat "#+email: " user-mail-address)))
         (date (if daily "" "#+date: %<%Y-%m-%d>"))

         ;; Concatenate all data
         (headers (seq-map (lambda (h) (if (string-equal h "") h (concat h "\n")))
                           (list title
                                 author
                                 email
                                 date)))
         (with-tags (append headers
                            (list (concat "#+filetags: :"
                                          (apply #'concat (seq-map
                                                           (lambda (tg)
                                                             (concat tg ":"))
                                                           tags)))))))
    (apply #'concat (append with-tags '("\n")))))

(defconst klv/roam-dailies-capture-templates
  `(;; Only one default daily capture template
    ("d" "default" entry
     "* %?"
     :target (file+head "%<%Y-%m-%d>.org"
                        ,(make-roam-header
                          :daily t
                          :tags ("dailies"
                                 "%<%Y-%m-%d>"
                                 "daily"))))))

(defconst klv/roam-capture-templates
  `(;; Replacement for Work capture
    ("y" "Yandex" plain
     "%?"
     :target (file+head "yandex/${slug}.org"
                        ,(make-roam-header
                          :tags ("yandex")))
     :unarrowed t)

    ;; Programming related things
    ("p" "Programming" plain
     "%?"
     :target (file+head "programming/${slug}.org"
                        ,(make-roam-header
                          :tags ("programming"))))

    ;; Algorithms for programming
    ("a" "Algorithms" plain
     "#+begin_src go :imports '(\"fmt\")%?\n#+end_src\n"
     :target (file+head "algorithms/${slug}.org"
                        ,(make-roam-header
                          :tags ("algorithm")))
     :unarrowed t)

    ;; Data Structures for programming
    ("d" "Data Structures" plain
     "#+begin_src go :imports '(\"fmt\")%?\n#+end_src\n"
     :target (file+head "data_structures/${slug}.org"
                        ,(make-roam-header
                          :tags ("data_structure"))))

    ;; Business related things
    ("b" "Business" plain
     "%?"
     :target (file+head "business/${slug}.org"
                        ,(make-roam-header
                          :tags ("business")))
     :unarrowed t)

    ;; Investment related things
    ("i" "Investment" plain
     "%?"
     :target (file+head "investment/${slug}.org"
                        ,(make-roam-header
                          :tags ("investment"))))
    ))

(use-package org-roam
  :init
  (setq org-roam-v2-ack t)
  ;; MAYBE: Use `after-init-hook'
  ;; FIXME: With `after-init-hook' it may cause issues maybe
  ;; FIXME: No lazy-loading
  :hook ((emacs-startup . org-roam-db-autosync-mode))
  :bind (:map
         org-mode-map
         ("C-c r r" . org-roam-ref-add)
         ("C-c r R" . org-roam-ref-remove)
         ("C-c r f" . org-roam-ref-find)
         ("C-c r t" . org-roam-tag-add)
         ("C-c r T" . org-roam-tag-remove)
         ("C-c r a" . org-roam-alias-add)
         ("C-c r A" . org-roam-alias-remove)
         ;; NOTE: See https://git.sr.ht/~abcdw/rde/tree/master/item/src/rde/features/emacs-xyz.scm#L4883
         ;; ("C-c r O" . rde-org-roam-open-ref)
         :map
         mode-specific-map
         ("n t" . org-roam-dailies-capture-today)
         ("n n" . org-roam-buffer-toggle)
         ("n f" . org-roam-node-find)
         ("n i" . org-roam-node-insert)
         ("n r" . org-roam-ref-find)
         ("n C" . org-roam-capture))
  :custom
  (org-roam-directory (expand-file-name "roam" org-directory))
  (org-roam-db-location (expand-file-name "cache/org-roam.db" org-directory))
  (org-roam-db-update-on-save t)

  ;; Display template (display tags in vertico)
  (org-roam-node-display-template
   (concat "${title:*} "
           (propertize "${tags:30}" 'face 'org-tag)))

  ;; Dailies directory
  (org-roam-dailies-directory "daily/")
  
  ;; Dailies templates
  (org-roam-dailies-capture-templates klv/roam-dailies-capture-templates)

  ;; Templates
  (org-roam-capture-templates klv/roam-capture-templates))

;;; roam.el ends here
