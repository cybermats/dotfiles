;;; init.el --- Mats init.el

;;; Author: Mats Fredriksson <cybermats@gmail.com>

;;; Commentary:
;; Refactored for Emacs 30.2 with better load order, lazy loading,
;; and modern practices using use-package.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Package Management Setup (MUST BE FIRST)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'package)

;; Configure package archives
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))

(package-initialize)

;; Bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)  ; Auto-install packages

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Basic UI Configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tool-bar-mode -1)
(setq inhibit-startup-screen t)
(setq compilation-scroll-output t)
(setq global-auto-revert-mode t)

;; Enable tab-bar-mode
(tab-bar-mode t)

;; Line numbers in programming modes
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; Note: Frame size and font are configured in early-init.el to prevent flickering

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Window Navigation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package windmove
  :ensure nil
  :bind*
  (("M-<left>" . windmove-left)
   ("M-<right>" . windmove-right)
   ("M-<up>" . windmove-up)
   ("M-<down>" . windmove-down)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Programming - General
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Company mode for auto-completion
(use-package company
  :diminish company-mode
  :hook (after-init . global-company-mode)
  :config
  (setq company-idle-delay 0.2
        company-minimum-prefix-length 2))

;; Use built-in eglot instead of lsp-mode (available in Emacs 29+)
;; eglot is lighter and now the recommended LSP client
(use-package eglot
  :ensure nil  ; Built-in
  :hook ((c-mode . eglot-ensure)
         (c++-mode . eglot-ensure))
  :config
  ;; Configure ccls as the C/C++ language server
  (add-to-list 'eglot-server-programs
               '((c-mode c++-mode) . ("ccls")))
  :bind (:map eglot-mode-map
              ("C-c l r" . eglot-rename)
              ("C-c l f" . eglot-format)
              ("C-c l a" . eglot-code-actions)))

;; Note: If you prefer lsp-mode over eglot, uncomment below and comment out eglot section:
;; (use-package lsp-mode
;;   :hook ((c-mode . lsp)
;;          (c++-mode . lsp))
;;   :commands lsp
;;   :config
;;   (setq lsp-prefer-flymake nil))
;;
;; (use-package ccls
;;   :after lsp-mode
;;   :config
;;   (setq ccls-executable "/usr/bin/ccls"))

;; Flymake (built-in) - works with both eglot and lsp-mode
(use-package flymake
  :ensure nil
  :bind (:map flymake-mode-map
              ("M-n" . flymake-goto-next-error)
              ("M-p" . flymake-goto-prev-error)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Programming - C/C++
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Google C style
(use-package google-c-style
  :hook ((c-mode-common . google-set-c-style)
         (c-mode-common . google-make-newline-indent)))

;; Find other file (.h <-> .c)
(global-set-key (kbd "C-c o") 'ff-find-other-file)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Programming - Other Languages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package markdown-mode
  :mode "\\.md\\'")

(use-package lua-mode
  :mode "\\.lua\\'")

(use-package toml-mode
  :mode "\\.toml\\'")

(use-package protobuf-mode
  :mode "\\.proto\\'")

(use-package dockerfile-mode
  :mode "Dockerfile\\'")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Lisp Development
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Scheme with Geiser
(use-package geiser
  :defer t
  :config
  (setq geiser-active-implementations '(chicken)))

;; Common Lisp with SLIME (installed via Quicklisp, not MELPA)
;; SLIME is loaded from ~/quicklisp/slime-helper.el
(setq inferior-lisp-program "/usr/bin/sbcl")
(let ((slime-helper (expand-file-name "~/quicklisp/slime-helper.el")))
  (when (file-exists-p slime-helper)
    (load slime-helper)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Version Control
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package magit
  :bind ("C-x g" . magit-status))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package mwim
  :bind (("C-a" . mwim-beginning-of-code-or-line)
         ("C-e" . mwim-end-of-code-or-line)))

;; Popwin for better popup management
(use-package popwin
  :config
  (popwin-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Theming
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package ewal
  :config
  (setq ewal-use-built-in-on-failure-p t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Additional Configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Add personal elisp library directory
(add-to-list 'load-path "~/.emacs.d/lisp")

;; Dead keys support (e.g., tilde)
(require 'iso-transl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Custom Variables (auto-generated)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(custom-safe-themes
   '("bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476"
     default))
 '(global-display-line-numbers-mode nil)
 '(package-selected-packages nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(provide 'init)
;;; init.el ends here
