;;; early-init.el --- Early Init -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;; Code:

;;; Internal variables

(defvar gc-cons-threshold--backup gc-cons-threshold)
(defvar gc-cons-percentage--backup gc-cons-percentage)

(setq gc-cons-threshold most-positive-fixnum)
(setq gc-cons-percentage 1.0)

;;; Variables

(defvar emacs-debug (bound-and-true-p init-file-debug)
  "Non-nil to enable debugging")

(defvar gc-cons-threshold-restore-delay nil
  "Number of seconds to wait before restoring `gc-cons-threshold'.")

;;; Elisp files loading

;; Prefer loading newer compiled files
(setq load-prefer-newer t)
(when emacs-debug
  (setq debug-on-error emacs-debug))

(setq custom-theme-directory
      (expand-file-name "themes/" user-emacs-directory))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;;; Garbage collection
;; Garbage collection significantly affects startup times. This setting delays
;; garbage collection during startup but will be reset later.

(defun restore-gc-values ()
  "Restore garbage collection values."
  (setq gc-cons-threshold (* 32 1024 1024))
  (setq gc-cons-percentage gc-cons-percentage))

(defun restore-gc ()
  "Restore garbage collection settings."
  (if (and (bound-and-true-p gc-cons-threshold-restore-delay)
           ;; In noninteractive mode, the event loop does not run
           (not noninteractive))
      ;; Defer garbage collection during initialization to avoid 2 collections.
      (run-with-timer gc-cons-threshold-restore-delay nil
                      #'restore-gc-values)
    (restore-gc-values)))

(add-hook 'emacs-startup-hook #'restore-gc 105)

;;; Miscellaneous

(set-language-environment "UTF-8")

(setq read-process-output-max (* 2 1024 1024))

(setq process-adaptive-read-buffering nil)

(setq ffap-machine-p-known 'reject)

(setq warning-minimum-level (if emacs-debug :warning :error))
(setq warning-suppress-types '((lexical-binding)))

(when emacs-debug
  (setq message-log-max 16384))

(when (boundp 'pgtk-wait-for-event-timeout)
  (setq pgtk-wait-for-event-timeout 0.001))

;;; Performance: Miscellaneous options

;; Font compacting can be very resource-intensive, especially when rendering
;; icon fonts on Windows. This will increase memory usage.
(setq inhibit-compacting-font-caches t)

(when (not noninteractive)
  ;; Resizing the Emacs frame can be costly when changing the font. Disable this
  ;; to improve startup times with fonts larger than the system default.
  (setq frame-resize-pixelwise t)

  ;; Without this, Emacs will try to resize itself to a specific column size
  (setq frame-inhibit-implied-resize t)

  ;; A second, case-insensitive pass over `auto-mode-alist' is time wasted.
  ;; No second pass of case-insensitive search over auto-mode-alist.
  (setq auto-mode-case-fold nil)

  ;; Reduce *Message* noise at startup. An empty scratch buffer (or the
  ;; dashboard) is more than enough, and faster to display.
  (setq inhibit-startup-screen t
        inhibit-startup-echo-area-message user-login-name)
  (setq initial-buffer-choice nil
        inhibit-startup-buffer-menu t
        inhibit-x-resources t)

  ;; Disable bidirectional text scanning for a modest performance boost.
  (setq-default bidi-display-reordering 'left-to-right
                bidi-paragraph-direction 'left-to-right)

  ;; Give up some bidirectional functionality for slightly faster re-display.
  (setq bidi-inhibit-bpa t)

  ;; Remove "For information about GNU Emacs..." message at startup
  (advice-add 'display-startup-echo-area-message :override #'ignore)

  ;; Suppress the vanilla startup screen completely. We've disabled it with
  ;; `inhibit-startup-screen', but it would still initialize anyway.
  (advice-add 'display-startup-screen :override #'ignore)

  (unless emacs-debug
    ;; Unset command line options irrelevant to the current OS. These options
    ;; are still processed by `command-line-1` but have no effect.
    (unless (eq system-type 'darwin)
      (setq command-line-ns-option-alist nil))
    (unless (memq initial-window-system '(x pgtk))
      (setq command-line-x-option-alist nil))))

;;; UI elements

;; Disable startup screens and messages
(setq inhibit-splash-screen t)

(push '(menu-bar-lines . 0) default-frame-alist)
(unless (memq window-system '(mac ns))
  (setq menu-bar-mode nil))

(setq default-frame-scroll-bars 'right)
(push '(vertical-scroll-bars) default-frame-alist)
(push '(horizontal-scroll-bars) default-frame-alist)
(setq scroll-bar-mode nil)

(when (bound-and-true-p tooltip-mode)
  (tooltip-mode -1))

;; Disable GUIs because they are inconsistent across systems, desktop
;; environments, and themes, and they don't match the look of Emacs.
(setq use-file-dialog nil)
(setq use-dialog-box nil)

(push '(tool-bar-lines . 0) default-frame-alist)
(setq tool-bar-mode nil)

;; Maximized window by default.
;; PERFORMANCE WARNING!
(push '(fullscreen . maximized) default-frame-alist)

;;; Security

(setq gnutls-verify-error t)  ; Prompts user if there are certificate issues
(setq tls-checktrust t)  ; Ensure SSL/TLS connections undergo trust verification
(setq gnutls-min-prime-bits 3072)  ; Stronger GnuTLS encryption

;; This results in a more compact output that emphasizes performance
(setq use-package-expand-minimally t)

(setq use-package-minimum-reported-time (if emacs-debug 0 0.1))
(setq use-package-verbose emacs-debug)
(setq use-package-always-ensure (not noninteractive))
(setq use-package-always-defer t)
(setq use-package-enable-imenu-support t)

;; package.el
(setq package-enable-at-startup nil)  ; Let the init.el file handle this
(setq package-archives '(;; ("melpa"        . "https://melpa.org/packages/")
                         ("gnu"          . "https://elpa.gnu.org/packages/")
                         ("nongnu"       . "https://elpa.nongnu.org/nongnu/")
                         ;; ("melpa-stable" . "https://stable.melpa.org/packages/")
			 ))
(setq package-archive-priorities '(("gnu"    . 99)
                                   ("nongnu" . 80)
                                   ;; ("melpa"  . 70)
                                   ;; ("melpa-stable" . 50)
				   ))

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

;;; early-init.el ends here
