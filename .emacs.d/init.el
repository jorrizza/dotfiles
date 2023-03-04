;; Package management
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")))
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))
(defvar my-packages '(smooth-scrolling yaml-mode eglot
		      graphviz-dot-mode tramp markdown-mode
		      dockerfile-mode ag toml-mode company
		      pyenv-mode project xref eldoc
		      solarized-theme diff-hl)
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

;; Nice colors
(load-theme 'solarized-dark t)

;; We use versioning. Backup files are not needed.
(setq make-backup-files nil)

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

;; Line numbering
(global-linum-mode)
(setq linum-format "%4d")

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
(global-auto-revert-mode t)

;; Auto completion
(add-hook 'after-init-hook 'global-company-mode)

;; Auto completion for minibuf
(ido-mode t)

;; Settings for ag
(setq ag-reuse-window 't)
(setq ag-reuse-buffers 't)

;; VC sidebar hightlight
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

;; Python
(add-hook 'python-mode-hook 'eglot-ensure)
(add-hook 'python-mode-hook
          (lambda ()
            (add-hook 'before-save-hook 'eglot-format nil t)
            (set-newline-and-indent)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t)
 '(js-indent-level 2)
 '(markdown-code-lang-modes
   '(("ocaml" . tuareg-mode)
     ("elisp" . emacs-lisp-mode)
     ("ditaa" . artist-mode)
     ("asymptote" . asy-mode)
     ("dot" . fundamental-mode)
     ("sqlite" . sql-mode)
     ("calc" . fundamental-mode)
     ("C" . c-mode)
     ("cpp" . c++-mode)
     ("C++" . c++-mode)
     ("screen" . shell-script-mode)
     ("shell" . sh-mode)
     ("bash" . sh-mode)
     ("yaml" . yaml-mode)))
 '(markdown-enable-highlighting-syntax t)
 '(markdown-fontify-code-blocks-natively t)
 '(package-selected-packages
   '(k8s-mode dart-mode terraform-mode go-mode lua-mode counsel plantuml-mode pyenv-mode company toml-mode ag dockerfile-mode markdown-mode graphviz-dot-mode yaml-mode smooth-scrolling))
 '(safe-local-variable-values
   '((format-all-formatters
      ("Python" black))
     (python-sort-imports-on-save t)))
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
