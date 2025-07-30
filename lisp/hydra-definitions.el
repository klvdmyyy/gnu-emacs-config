;;; hydra-definitions.el --- Optimizations for Hydra bindings. -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;; MAYBE: Setup Casual package.
;;
;;; Code:

(eval-when-compile
  (require 'hydra))

(defhydra hydra-zoom ()
  "Zoom"
  ("i" text-scale-increase "in")
  ("d" text-scale-decrease "out"))

(defhydra hydra-org-timer ()
  "Org Timer"
  ("s" org-timer-start "Start timer")
  ("e" org-timer-stop "Timer end (Stop timer)")
  ("p" org-timer-pause-or-continue "Pause/Continue timer")
  ("t" org-timer-set-timer "Set timer")
  ("q" nil "Quit"))

(defhydra hydra-org-roam ()
  "Org Roam"
  ("r" org-roam-ref-add "add ref")
  ("R" org-roam-ref-remove "remove ref")
  ("f" org-roam-ref-find "find ref")
  ("t" org-roam-tag-add "add tag")
  ("T" org-roam-tag-remove "remove tag")
  ("a" org-roam-alias-add "add alias")
  ("A" org-roam-alias-remove "remove alias"))

(defhydra hydra-org-roam-node ()
  "Org Roam Node"
  ("t" org-roam-dailies-capture-today "capture daily" :exit t)
  ("n" org-roam-buffer-toggle "toggle buffer")
  ("f" org-roam-node-find "find node" :exit t)
  ("i" org-roam-node-insert "insert node" :exit t)
  ("r" org-roam-ref-find "find ref" :exit t)
  ("C" org-roam-capture "capture" :exit t))

(provide 'hydra-definitions)

;;; hydra-definitions.el ends here
