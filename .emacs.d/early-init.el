;;; early-init.el --- Early initialization

;;; Commentary:
;; This file runs before the GUI is initialized, preventing flicker
;; and allowing frame size/font to be set cleanly.

;;; Code:

;; Disable package.el early (we use use-package in init.el)
(setq package-enable-at-startup nil)

;; Set frame size BEFORE GUI initialization (prevents flickering)
(add-to-list 'default-frame-alist '(width . 120))   ; 120 characters wide
(add-to-list 'default-frame-alist '(height . 40))   ; 40 lines tall

;; Or maximize on startup (uncomment if preferred):
;; (add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Set default font and size
;; Common monospace fonts: "Monospace", "DejaVu Sans Mono", "JetBrains Mono", "Fira Code"
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
