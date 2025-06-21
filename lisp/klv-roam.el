;;; klv-roam.el --- The best thing in the world! -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;; Code:

(provide 'klv-roam)

(cl-defmacro make-roam-header (&key (tags '())
                                    (daily nil)
                                    (no-author nil)
                                    (no-email nil))
  "Making header for Org Roam capture.

Following keys are supported:

:no-author --- if `t' don't place author field in header
:no-email  --- if `t' don't place email field in header
:daily     --- if `t' don't place date field in header. instead use date as a title
:tags      --- list of default tags for cap"
  (let* ((title (if daily "#+title: %<%Y-%m-%d>" "#+title: ${title}"))
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

;;; klv-roam.el ends here
