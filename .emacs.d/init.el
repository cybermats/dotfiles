;;; init.el --- Mats init.el

;;; Author: Mats Fredriksson <cybermats@gmail.com>

;;; Commentary:

;;; Code:
(tool-bar-mode -1)
(setq inhibit-startup-screen t)


;;; Configure WindMove
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Programming stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Enable company-mode for all buffers
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)

;; Enable lsp-mode
(require 'lsp)
(add-hook 'c-mode-hook #'lsp)

;; Configure ccls
(require 'ccls)
(setq ccls-executable "/usr/bin/ccls")

;; Configure Flymake
(require 'flymake)
(define-key flymake-mode-map (kbd "M-n") 'flymake-goto-next-error)
(define-key flymake-mode-map (kbd "M-p") 'flymake-goto-prev-error)

;; Set up find other file (for .h to .c)
(global-set-key (kbd "C-c o") 'ff-find-other-file)


(add-hook 'prog-mode-hook 'display-line-numbers-mode)

(require 'google-c-style)
(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook 'google-make-newline-indent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Other
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; melpa package manager
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
		    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl
    (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)


					; Handle SCHEME support through guile
(setq geiser-active-implementations '(chicken))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(custom-safe-themes
   '("bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" default))
 '(global-display-line-numbers-mode nil)
 '(package-selected-packages
   '(magit toml-mode protobuf-mode dockerfile-mode luarocks lua-mode google-c-style popwin ccls lsp-mode company project markdown-mode geiser mwim ewal))
 '(popwin-mode nil)
 '(tab-bar-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;Compilation autoscroll
(setq compilation-scroll-output t)

(setq ewal-use-built-in-on-failure-p t)

;; Auto revert mode
(setq global-auto-revert-mode t)


;; Add my personal elisp lib dir
(add-to-list 'load-path "~/.emacs.d/lisp")


;; Load support for Dead Keys (such as tilde)
(require 'iso-transl)

;; Add Slime for SBCL
(load (expand-file-name "~/quicklisp/slime-helper.el"))
;; Replace "sbcl" with the path to your implementation
(setq inferior-lisp-program "/usr/bin/sbcl")





(provide 'init)
;;; init.el ends here


