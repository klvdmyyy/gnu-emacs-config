;;; early-config.el --- Early Configuration of GNU Emacs -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Klementiev Dmitry <klementievd08@yandex.ru>
;;
;; Author:  Klementiev Dmitry
;; Email:   klementievd08@yandex.ru
;; License: None
;; Date:    2025-08-21 13:43
;;
;;; Commentary:
;;
;; This file provide high-level configuration
;; for my GNU Emacs distribution.
;;
;;; Code:

;; Some optimizations disabled by default.
(setopt bootstrap-maximize-frame-at-startup t
		bootstrap-optimize-loading t)

;;; Modus theme customization:

;; Setup headings.
(setopt modus-themes-headings
		'((1 . (1.4))
		  (2 . (1.3))
		  (3 . (1.2))
		  (t . (1.1))))

(provide 'early-config)

;;; early-config.el ends here
