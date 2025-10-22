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

;; Smooth scrolling
(smooth-scrolling-mode)

;; Version control sidebar highlight
(global-diff-hl-mode)

;; Show stray whitespace
(setq-default show-trailing-whitespace t)
(setq-default indicate-empty-lines t)

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

;; Use treesitter by default
(setq major-mode-remap-alist
      '((go-mode . go-ts-mode)
        (python-mode . python-ts-mode)
        (yaml-mode . yaml-ts-mode)
        (js-mode . js-ts-mode)))


;;
;; Package-specific Config
;;


;; Stolen list of working treesitter grammars from combobulate
;; https://github.com/mickeynp/combobulate/blob/master/README.rst
(use-package treesit
  :preface
  (defun mp-setup-install-grammars ()
    "Install Tree-sitter grammars if they are absent."
    (interactive)
    (dolist (grammar
             ;; Note the version numbers. These are the versions that
             ;; are known to work with Combobulate *and* Emacs.
             '((css . ("https://github.com/tree-sitter/tree-sitter-css" "v0.20.0"))
               (go . ("https://github.com/tree-sitter/tree-sitter-go" "v0.20.0"))
               (html . ("https://github.com/tree-sitter/tree-sitter-html" "v0.20.1"))
               (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "v0.20.1" "src"))
               (json . ("https://github.com/tree-sitter/tree-sitter-json" "v0.20.2"))
               (markdown . ("https://github.com/ikatyang/tree-sitter-markdown" "v0.7.1"))
               (python . ("https://github.com/tree-sitter/tree-sitter-python" "v0.20.4"))
               (rust . ("https://github.com/tree-sitter/tree-sitter-rust" "v0.21.2"))
               (toml . ("https://github.com/tree-sitter/tree-sitter-toml" "v0.5.1"))
               (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "tsx/src"))
               (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "typescript/src"))
               (yaml . ("https://github.com/ikatyang/tree-sitter-yaml" "v0.5.0"))))
      (add-to-list 'treesit-language-source-alist grammar)
      ;; Only install `grammar' if we don't already have it
      ;; installed. However, if you want to *update* a grammar then
      ;; this obviously prevents that from happening.
      (unless (treesit-language-available-p (car grammar))
        (treesit-install-language-grammar (car grammar)))))

  :config
  (mp-setup-install-grammars))


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
  (electric-pair-mode -1)
  (setq web-mode-enable-current-column-highlight t)
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))
