;;
;; Package management
;;


(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
			             ("melpa" . "https://melpa.org/packages/")
			             ("org" . "https://orgmode.org/elpa/")))
(package-initialize)

;; Bootstrap `use-package`
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Tree-sitter language pack
(add-to-list 'package-selected-packages 'tree-sitter-langs)

;; Language modes
(add-to-list 'package-selected-packages 'dockerfile-mode)
(add-to-list 'package-selected-packages 'markdown-mode)
(add-to-list 'package-selected-packages 'toml-mode)
(add-to-list 'package-selected-packages 'yaml-mode)

;; Auto-complete
(add-to-list 'package-selected-packages 'company)

;; Inline docs
(add-to-list 'package-selected-packages 'eldoc)

;; Pretty things
(add-to-list 'package-selected-packages 'ido)
(add-to-list 'package-selected-packages 'smooth-scrolling)
(add-to-list 'package-selected-packages 'solarized-theme)

(package-install-selected-packages)


;;
;; Look & Feel
;;


;; Set windows title
(setq frame-title-format '("" "%b - Emacs " emacs-version))

;; Line numbering
(global-display-line-numbers-mode)
(setq-default display-line-numbers-width-start t)

;; Show column number
(column-number-mode 1)

;; Highlight matching bracket
(show-paren-mode 1)

;; Line wrapping
(set-default 'truncate-lines t)
(global-visual-line-mode +1)

;; We don't need these
(setq inhibit-startup-message t)
(toggle-scroll-bar -1)
(tool-bar-mode -1)
(menu-bar-mode -1)

;; Nice colors
(load-theme 'solarized-dark t)

;; Nice font
(add-to-list 'default-frame-alist '(font . "Terminus-8"))
(set-face-attribute 'default t :font "Terminus-8")
(set-face-italic-p 'italic nil)

;; Smooth scrolling
(smooth-scrolling-mode)

;; Version control sidebar highlight
(global-diff-hl-mode)

;; Show stray whitespace
(setq-default show-trailing-whitespace t)

;; Highlight current line
(global-hl-line-mode +1)

;; Allow ElDoc to use multiple lines in the echo area
(setq-default eldoc-echo-area-use-multiline-p t)


;;
;; Editor Behavior
;;


;; Make M-arrow work for window switching
(windmove-default-keybindings 'meta)

;; Automatically insert matching bracket or quote
(electric-pair-mode +1)

;; Automatically indent every new line
(electric-indent-mode +1)

;; For (stupid) indent based languages, disable electric indent
(defun ignore-electric-indent (char)
  "Ignore electric indent for some modes"
  (if (member major-mode '(python-mode yaml-mode))
      `no-indent'
    nil))
(add-hook 'electric-indent-functions 'ignore-electric-indent)

;; Use UTF-8 already
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)
(setq-default default-buffer-file-coding-system 'utf-8-unix)
(setq-default default-keyboard-coding-system 'utf-8-unix)
(setq-default default-terminal-coding-system 'utf-8-unix)

;; Every file should end with a newline
(setq-default require-final-newline t)

;; Unique double filenames as buffers
(require 'uniquify)

;; Default mode
(setq initial-major-mode 'fundamental-mode)

;; Fix archaic defaults
(setq-default sentence-end-double-space nil)
(setq-default make-backup-files nil)
(setq-default create-lockfiles nil)

;; Make right-click do something sensible
(when (display-graphic-p)
  (context-menu-mode))

;; Save history of minibuffer
(savehist-mode)

;; Keep buffers in sync with filesystem
(setq-default auto-revert-interval 1)
(setq-default auto-revert-check-vc-info t)
(global-auto-revert-mode t)

;; Overwrite selected text
(delete-selection-mode +1)

;; Remember where we were in files
(save-place-mode +1)

;; Nicely fill out chunks of text up to the fill-column with M-q
(setq-default fill-column 79)
(defun unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
        ;; This would override `fill-column' if it's an integer.
        (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))
(define-key global-map "\M-Q" 'unfill-paragraph)

;; Auto completion within buffers
(add-hook 'after-init-hook 'global-company-mode)

;; Auto completion within the minibuffer
(setq-default ido-enable-flex-matching t)
(setq-default ido-everywhere t)
(ido-mode t)

;; Turn tabs into four spaces (go will have its own config later on)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; Use tree-sitter by default
(global-tree-sitter-mode)


;;
;; Package-specific Config
;;


;; LSP client
;; We want to both organize imports and format code before saving files
(defun eglot-format-and-imports-on-save ()
  "Run both formatting and import organization before saving."
  (when eglot--managed-mode
    (when (eglot--server-capable :documentFormattingProvider)
      (eglot-format-buffer))
    (when (eglot--server-capable :codeActionProvider)
      (eglot-code-action-organize-imports))))

(use-package eglot
  :custom
  (eglot-send-changes-idle-time 0.1)

  :config
  (fset #'jsonrpc--log-event #'ignore)

  :hook
  (eglot-managed-mode . (lambda ()
                          (add-hook 'before-save-hook #'eglot-format-and-imports-on-save nil 'local)))
  )

;; Go
(use-package go-ts-mode
  :ensure t

  :mode
  "\\.go\\'"

  :init
  (setq go-ts-mode-indent-offset 4) ;; Because our tab-width is 4

  :hook
  (go-ts-mode . (lambda ()
                  (eglot-ensure))))

;; Python
(use-package python-ts-mode
  ;; :ensure t

  :mode
  "\\.py\\'"

  ;; :init
  ;; (setq something)

  :hook
  (python-ts-mode . (lambda ()
                      (eglot-ensure))))

;; JSON
(use-package json-ts-mode
  :ensure t

  :mode
  "\\.json\\'"

  :hook
  (json-ts-mode . (lambda ()
                    (setq js-indent-level 2))))

;; Web
(use-package web-mode
  :ensure t

  :mode
  (("\\.html\\'" . web-mode)
   ("\\.gohtml\\'" . web-mode)
   ("\\.tpl\\'" . web-mode)
   ("\\.erb\\'" . web-mode)
   ("\\.djhtml\\'" . web-mode))

  :init
  (setq web-mode-enable-current-column-highlight t)
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)

  :hook
  (web-mode . (lambda ()
                (electric-pair-mode -1))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(company dockerfile-mode eldoc ido json-mode markdown-mode smooth-scrolling
             solarized-theme toml-mode tree-sitter-langs yaml-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(web-mode-html-attr-value-face ((t (:foreground "#2aa198" :slant normal)))))
