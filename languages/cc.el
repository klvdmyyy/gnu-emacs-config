;;; cc.el --- C/C++ Languages -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;; Code:

(use-package c-ts-mode
  :ensure nil
  :after treesit
  :mode "\\.\\(c\\|h\\)\\'")

(use-package c++-ts-mode
  :ensure nil
  :after treesit
  :mode "\\.\\(cc\\|cpp\\|hh\\|hpp\\)\\'")

;; (use-package cc-mode
;;   :ensure nil
;;   :mode (("\\.\\(c\\|h\\)\\'" . c-mode)
;; 	 ;; ("\\.c\\'" . c-mode)
;; 	 ;; ("\\.h\\'" . c-mode)
;; 	 ("\\.\\(cc\\|hh\\)\\'" . c++-mode)
;; 	 ;; ("\\.cc\\'" . c++-mode)
;; 	 ;; ("\\.hh\\'" . c++-mode)
;; 	 ("\\.\\(cpp\\|hpp\\)\\'" . c++-mode)
;; 	 ;; ("\\.cpp\\'" . c++-mode)
;; 	 ;; ("\\.hpp\\'" . c++-mode)
;; 	 )
;;   :bind (:map
;; 	     c-mode-base-map
;; 	     ("RET" . indent-between-pairs)
;; 	     ("<TAB>" . indent-for-tab-command)))

;;; cc.el ends here
