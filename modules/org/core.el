;;; core.el --- Org Core module -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;; Code:

(setq-default
 ;; Setup default Org directory
 org-directory "~/org"

 ;; Setup Org Captures
 org-capture-templates
 '(("l" "Learning Task" entry
    (file+headline "~/org/agenda/YandexLearning.org" "Tasks")
    "* TODO %?\nSCHEDULED: <%<%Y-%m-%d %a>>\n%a\n%i"
    :empty-lines 1
    :prepend t))

 org-confirm-babel-evaluate nil
 org-id-locations-file (expand-file-name "cache/.org-id-locations" org-directory)
 org-deadline-warning-days 7
 org-clock-sound (expand-file-name "assets/org-clock-sound.wav" user-emacs-directory))

(bind-key "o t s" 'org-timer-start mode-specific-map)
(bind-key "o t e" 'org-timer-stop mode-specific-map)
(bind-key "o t p" 'org-timer-pause-or-continue mode-specific-map)
(bind-key "o t t" 'org-timer-set-timer mode-specific-map)
(bind-key "o c" 'org-capture mode-specific-map)

(with-eval-after-load 'org
  (setq org-src-lang-modes
        (append
         org-src-lang-modes
         '(("go" . go-ts)
           ("Go" . go-ts)
           ("golang" . go-ts)
           ("Golang" . go-ts))))
  (add-hook 'org-mode-hook 'org-indent-mode))

;;; core.el ends here
