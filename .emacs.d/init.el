;; Package management
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")))
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))
(defvar my-packages '(smooth-scrolling yaml-mode
		      graphviz-dot-mode tramp markdown-mode
		      dockerfile-mode ag toml-mode company
		      project xref eldoc go-mode zig-mode
		      solarized-theme diff-hl web-mode)
  "Nice packages I depend upon.")
(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; We don't need these
(toggle-scroll-bar -1)
(tool-bar-mode -1)
(menu-bar-mode -1)

;; Unique double filenames
(require 'uniquify)

;; Set windows title
(setq frame-title-format '("" "%b - Emacs " emacs-version))

;; Default mode
(setq initial-major-mode 'fundamental-mode)

;; Fix archaic defaults
(setq sentence-end-double-space nil)

;; Make right-click do something sensible
(when (display-graphic-p)
  (context-menu-mode))

;; Save history of minibuffer
(savehist-mode)

;; Nice colors
(load-theme 'solarized-dark t)

;; Nice font
(add-to-list 'default-frame-alist '(font . "Terminus-8"))
(set-face-attribute 'default t :font "Terminus-8")

;; We use versioning. Backup files are not needed.
(setq make-backup-files nil)

;; We also never share files on the same machine
(setq create-lockfiles nil)

;; Fill mode
(setq-default fill-column 79)
(defun unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
        ;; This would override `fill-column' if it's an integer.
        (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))
(define-key global-map "\M-Q" 'unfill-paragraph)

;; Damn you tabs!
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; Line numbering
(global-display-line-numbers-mode)

;; Show column number
(column-number-mode 1)

;; Highlight matching bracket
(show-paren-mode 1)

;; Make M-arrow work for window switching
(windmove-default-keybindings 'meta)

;; Electric features that rock
(electric-pair-mode +1)
(electric-indent-mode +1)

;; For indent based languages, disable electric indent
(defun ignore-electric-indent (char)
  "Ignore electric indent for some modes"
  (if (member major-mode '(python-mode yaml-mode))
      `no-indent'
    nil))
(add-hook 'electric-indent-functions 'ignore-electric-indent)

;; Function to replace electric indent for indent based languages
(defun set-newline-and-indent ()
  "Map the return key with `newline-and-indent'"
  (local-set-key (kbd "RET") 'newline-and-indent))

;; Keep buffers in sync with filesystem
(setq auto-revert-interval 1)
(setq auto-revert-check-vc-info t)
(global-auto-revert-mode t)

;; Auto completion
(add-hook 'after-init-hook 'global-company-mode)

;; Auto completion for minibuf
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode t)

;; Settings for ag
(setq ag-reuse-window 't)
(setq ag-reuse-buffers 't)

;; VC sidebar highlight
(global-diff-hl-mode)

;; Tramp
(setq tramp-default-method "ssh")

;; Auto-indent yanked code
(dolist (command '(yank yank-pop))
   (eval `(defadvice ,command (after indent-region activate)
            (and (not current-prefix-arg)
                 (member major-mode '(emacs-lisp-mode lisp-mode
                                                      python-mode
                                                      c-mode
                                                      go-mode))
                 (let ((mark-even-if-inactive transient-mark-mode))
                   (indent-region (region-beginning) (region-end) nil))))))


;; Remove trailing white space
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; My spelling is sub-par
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

;; Tree sitter stuff (not enabled in Debian yet)
;; (setq major-mode-remap-alist
;;       '((yaml-mode . yaml-ts-mode)
;;         (bash-mode . bash-ts-mode)
;;         (js2-mode . js-ts-mode)
;;         (typescript-mode . typescript-ts-mode)
;;         (json-mode . json-ts-mode)
;;         (css-mode . css-ts-mode)
;;         (python-mode . python-ts-mode)
;;         (c-mode . c-ts-mode)
;;         (go-mode . go-ts-mode)))

;; Eglot fixes
(use-package eglot
  :custom
  (eglot-send-changes-idle-time 0.1)

  :config
  (fset #'jsonrpc--log-event #'ignore)
  )

;; Python
(add-hook 'python-mode-hook
          'eglot-ensure)
(add-hook 'python-mode-hook
          (lambda ()
            (add-hook 'before-save-hook 'eglot-format nil t)
            (set-newline-and-indent)))

;; Go
(add-hook 'go-mode-hook
          'eglot-ensure)
(add-hook 'go-mode-hook
          (lambda ()
            (add-hook 'before-save-hook 'eglot-format nil t)
            (add-hook 'before-save-hook (lambda ()
                                          (call-interactively 'eglot-code-action-organize-imports))
                      nil t)))

;; Zig
(add-hook 'zig-mode-hook
          'eglot-ensure)
(add-hook 'zig-mode-hook
          (lambda ()
            (add-hook 'before-save-hook 'eglot-format nil t)))

;; Web
(add-to-list 'auto-mode-alist '("\\.gohtml\\'" . web-mode))
(add-hook 'web-mode-hook
          (lambda ()
            (electric-pair-mode -1)
            (setq web-mode-markup-indent-offset 2)
            (setq web-mode-css-indent-offset 2)
            (setq web-mode-code-indent-offset 2)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t)
 '(js-indent-level 2)
 '(markdown-code-lang-modes
   '(("ocaml" . tuareg-mode) ("elisp" . emacs-lisp-mode) ("ditaa" . artist-mode)
     ("asymptote" . asy-mode) ("dot" . fundamental-mode) ("sqlite" . sql-mode)
     ("calc" . fundamental-mode) ("C" . c-mode) ("cpp" . c++-mode)
     ("C++" . c++-mode) ("screen" . shell-script-mode) ("shell" . sh-mode)
     ("bash" . sh-mode) ("yaml" . yaml-mode)))
 '(markdown-enable-highlighting-syntax t)
 '(markdown-fontify-code-blocks-natively t)
 '(package-selected-packages
   '(ag company diff-hl dockerfile-mode go-mode graphviz-dot-mode just-mode
        markdown-mode smooth-scrolling solarized-theme toml-mode web-mode
        yaml-mode zig-mode))
 '(safe-local-variable-values
   '((format-all-formatters ("Python" black)) (python-sort-imports-on-save t)))
 '(warning-suppress-log-types '((comp)))
 '(warning-suppress-types '((comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :background "#002b36" :foreground "#839496" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 80 :width normal :foundry "PfEd" :family "Terminus"))))
 '(fixed-pitch ((t nil)))
 '(fixed-pitch-serif ((t nil))))
