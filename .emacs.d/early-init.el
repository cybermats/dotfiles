;;; early-init.el --- Early initialization

;;; Commentary:
;; This file runs before the GUI is initialized, preventing flicker.

;;; Code:

;; Disable package.el early (we use use-package in init.el)
(setq package-enable-at-startup nil)

;; Don't set frame size - let Emacs/GTK handle it naturally
;; This avoids GTK assertion errors
;(setq frame-inhibit-implied-resize t)

;; Set the frame size
(setq default-frame-alist
      '((width . 100) ;; e.g. 100 characters wide
	(height . 50))) ;; e.g. 50 lines high


;; Set default font and size
(add-to-list 'default-frame-alist '(font . "Ubuntu Sans Mono-11"))

;; Disable UI elements early to prevent flicker
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(menu-bar-lines . 1) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Faster startup
(setq gc-cons-threshold most-positive-fixnum)  ; Delay GC during startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 16 1024 1024))))  ; Reset to 16MB after startup

(provide 'early-init)
;;; early-init.el ends here
