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
        ("melpa" . "https://melpa.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

(package-initialize)

;; Refresh package contents if archive contents are missing
(unless (file-exists-p (concat package-user-dir "/archives/melpa/archive-contents"))
  (message "Refreshing package archives...")
  (package-refresh-contents))

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
;;; Modern Completion UI (Vertico + Consult + Marginalia)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Vertico: Modern minibuffer completion
(use-package vertico
  :init
  (vertico-mode))

;; Orderless: Flexible completion style
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; Consult: Enhanced search and navigation
(use-package consult
  :bind (("C-x b" . consult-buffer)
         ("C-x C-r" . consult-recent-file)
         ("M-g g" . consult-goto-line)
         ("M-s g" . consult-grep)
         ("M-s r" . consult-ripgrep)
         ("C-s" . consult-line)))

;; Marginalia: Rich annotations in minibuffer
(use-package marginalia
  :init
  (marginalia-mode))

;; Embark: Context actions
(use-package embark
  :bind (("C-." . embark-act)
         ("M-." . embark-dwim)))

(use-package embark-consult
  :ensure t
  :after (embark consult)
  :demand t)

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
;;; Project Management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Projectile: Project interaction library
(use-package projectile
  :init
  (projectile-mode +1)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :config
  (setq projectile-completion-system 'default))

;; Which-key: Show available keybindings
(use-package which-key
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.5))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Programming - General
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Company mode for auto-completion
(use-package company
  :diminish company-mode
  :hook (after-init . global-company-mode)
  :config
  (setq company-idle-delay 0.2
        company-minimum-prefix-length 2
        company-show-quick-access t))

;; YASnippet: Snippet system
(use-package yasnippet
  :diminish yas-minor-mode
  :hook (prog-mode . yas-minor-mode)
  :config
  (yas-reload-all))

(use-package yasnippet-snippets
  :after yasnippet)

;; Use built-in eglot instead of lsp-mode (available in Emacs 29+)
;; eglot is lighter and now the recommended LSP client
(use-package eglot
  :ensure nil  ; Built-in
  :hook ((c-mode . eglot-ensure)
         (c++-mode . eglot-ensure)
         (c-ts-mode . eglot-ensure)
         (c++-ts-mode . eglot-ensure))
  :config
  ;; Configure clangd as the C/C++ language server (more modern than ccls)
  (add-to-list 'eglot-server-programs
               '((c-mode c++-mode c-ts-mode c++-ts-mode) .
                 ("clangd"
                  "--background-index"
                  "--clang-tidy"
                  "--completion-style=detailed"
                  "--header-insertion=iwyu")))
  :bind (:map eglot-mode-map
              ("C-c l r" . eglot-rename)
              ("C-c l f" . eglot-format)
              ("C-c l a" . eglot-code-actions)
              ("C-c l d" . eglot-find-declaration)
              ("C-c l i" . eglot-find-implementation)))

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
;;; Tree-sitter (Built-in Emacs 29+)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Enable tree-sitter modes automatically for supported languages
;; Using v0.23.x tags for ABI version 14 compatibility with Emacs 30.2
(setq treesit-language-source-alist
      '((c "https://github.com/tree-sitter/tree-sitter-c" "v0.23.0" "src")
        (cpp "https://github.com/tree-sitter/tree-sitter-cpp" "v0.23.1" "src")
        (python "https://github.com/tree-sitter/tree-sitter-python" "v0.23.2" "src")
        (rust "https://github.com/tree-sitter/tree-sitter-rust" "v0.23.0" "src")
        (bash "https://github.com/tree-sitter/tree-sitter-bash" "v0.23.1" "src")
        (json "https://github.com/tree-sitter/tree-sitter-json" "v0.24.3" "src")
        (toml "https://github.com/tree-sitter/tree-sitter-toml" "v0.23.0" "src")
        (yaml "https://github.com/tree-sitter/tree-sitter-yaml" "v0.6.1" "src")))

;; Auto-remap to tree-sitter modes when available
(setq major-mode-remap-alist
      '((c-mode . c-ts-mode)
        (c++-mode . c++-ts-mode)
        (python-mode . python-ts-mode)
        (bash-mode . bash-ts-mode)
        (json-mode . json-ts-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Programming - C/C++
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Google C style (only for CC Mode, not tree-sitter modes)
(use-package google-c-style
  :hook ((c-mode-common . google-set-c-style)
         (c-mode-common . google-make-newline-indent)))

;; Configure indentation for tree-sitter C modes
(defun my-c-ts-mode-indent-style ()
  "Configure Google-style indentation for c-ts-mode."
  (setq c-ts-mode-indent-offset 2
        c-ts-mode-indent-style 'google))

(add-hook 'c-ts-mode-hook 'my-c-ts-mode-indent-style)
(add-hook 'c++-ts-mode-hook 'my-c-ts-mode-indent-style)

;; Find other file (.h <-> .c)
(global-set-key (kbd "C-c o") 'ff-find-other-file)

;; Configure search paths for ff-find-other-file
(setq cc-search-directories
      '("."
        "../include"
        "../inc"
        "./include"
        "./inc"
        "../src"
        "/usr/include"
        "/usr/local/include/*"))

;; Make ff-find-other-file work in tree-sitter modes too
(add-hook 'c-ts-mode-hook
          (lambda ()
            (setq-local ff-search-directories cc-search-directories)))
(add-hook 'c++-ts-mode-hook
          (lambda ()
            (setq-local ff-search-directories cc-search-directories)))

;; CMake support
(use-package cmake-mode
  :mode ("CMakeLists\\.txt\\'" "\\.cmake\\'"))

;; Modern C++ font-lock
(use-package modern-cpp-font-lock
  :diminish modern-c++-font-lock-mode
  :hook (c++-mode . modern-c++-font-lock-mode))

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
;;; Development Enhancements
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Rainbow delimiters for better bracket visibility
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Diff-hl: Show git diff in the fringe
(use-package diff-hl
  :config
  (global-diff-hl-mode)
  (diff-hl-flydiff-mode)
  :hook ((magit-pre-refresh . diff-hl-magit-pre-refresh)
         (magit-post-refresh . diff-hl-magit-post-refresh)))

;; Better help system
(use-package helpful
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)
         ("C-h C" . helpful-command)))

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

;; Multiple cursors
(use-package multiple-cursors
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)))

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
