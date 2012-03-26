; .emacs of jorrizza@jrrzz.net
; GNU Emacs 23.3 (Debian 23.3+1-4)

(add-to-list 'load-path "~/.emacs.d/")
(add-to-list 'load-path "~/.emacs.d/nyan-mode/")
(add-to-list 'load-path "~/go/misc/emacs/" t)

; We don't need these
(toggle-scroll-bar -1)
(tool-bar-mode -1)

; Nice font FTW
(set-default-font "Terminus 8")

; Nice colors even more FTW
(require 'color-theme)
(eval-after-load "color-theme"
  '(progn
     (color-theme-initialize)
     (color-theme-comidia)))

; Transparency is just cool, but my composite manager is b0rken
;(set-frame-parameter (selected-frame) 'alpha '(85 50))
;(add-to-list 'default-frame-alist '(alpha 85 50))

; Column Marker
(require 'column-marker)

; Nyan Cat!
(require 'nyan-mode)

; Standard indent size
(setq standard-indent 2)

; Make scrolling easier
(setq scroll-step 1)

; We use versioning. Backup files are not needed.
(setq make-backup-files nil)

; Fill mode
(setq-default auto-fill-function 'do-auto-fill)
(setq fill-column 80)

; Damn you tabs!
(setq-default indent-tabs-mode nil)

; Line numbering
(global-linum-mode)
(setq linum-format "%4d")

; Show column number
(column-number-mode 1)

; Highlight matching bracket
(show-paren-mode 1)

; Go
(require 'go-mode-load)
(add-hook 'go-mode-hook '(lambda ()
                           (set 'tab-width 2)
                           (set 'indent-tabs-mode t)
                           (interactive) (column-marker-1 80)
                           (local-set-key (kbd "RET") 'reindent-then-newline-and-indent)))

; C
(add-hook 'c-mode-hook '(lambda ()
                          (interactive) (column-marker-1 80)
                          (local-set-key (kbd "RET") 'reindent-then-newline-and-indent)))

; Ruby
(add-hook 'ruby-mode-hook '(lambda ()
                             (interactive) (column-marker-1 80)
                             (local-set-key (kbd "RET") 'reindent-then-newline-and-indent)))

; YAML
(add-hook 'yaml-mode-hook '(lambda ()
                             (local-set-key (kbd "RET") 'reindent-then-newline-and-indent)))

; PHP same as ruby, but take care of the braindeadness in indenting
(add-hook 'php-mode-hook '(lambda ()
                            (local-set-key (kbd "RET") 'reindent-then-newline-and-indent)
                            (c-set-style "my-php-style")
                            (c-set-offset 'case-label '+)
                            (c-set-offset 'arglist-intro '+)
                            (c-set-offset 'arglist-cont-nonempty 'c-lineup-math)
                            (set 'tab-width 2)
                            (set 'c-basic-offset 2)))
(defconst my-php-style
  '((c-offsets-alist . (
                        (arglist-close . c-lineup-close-paren)
                        )))
  "Fix the damn PHP-Mode indention"
  )
(c-add-style "my-php-style" my-php-style)

; CSS
(add-hook 'css-mode-hook '(lambda ()
                            (local-set-key (kbd "RET") 'reindent-then-newline-and-indent)))
(setq cssm-indent-function #'cssm-c-style-indenter)
(setq cssm-indent-level 2)

; HTML
(add-hook 'html-mode-hook '(lambda ()
                             (local-set-key (kbd "RET") 'reindent-then-newline-and-indent)))

; LaTeX
(add-hook 'latex-mode-hook '(lambda ()
                              (interactive) (column-marker-1 80)
                              (local-set-key (kbd "RET") 'reindent-then-newline-and-indent)))

; DOT
(add-hook 'graphviz-dot-mode-hook '(lambda ()
                                     (local-set-key (kbd "RET") 'reindent-then-newline-and-indent)))

; JavaScript
(add-hook 'js2-mode-hook '(lambda ()
                            (interactive) (column-marker-1 80)
                            (local-set-key (kbd "RET") 'reindent-then-newline-and-indent)
                            (set 'js2-cleanup-whitespace t)
                            (set 'js2-basic-offset 2)
                            (set 'js2-user-font-lock-faces t)))

; Auto-indent with a yank
(dolist (command '(yank yank-pop))
  (eval `(defadvice ,command (after indent-region activate)
           (and (not current-prefix-arg)
                (member major-mode '(emacs-lisp-mode lisp-mode
                                     clojure-mode    scheme-mode
                                     haskell-mode    ruby-mode
                                     rspec-mode      python-mode
                                     c-mode          c++-mode
                                     objc-mode       latex-mode
                                     plain-tex-mode  php-mode
                                     css-mode        js2-mode
                                     go-mode         html-mode))
                (let ((mark-even-if-inactive transient-mark-mode))
                  (indent-region (region-beginning) (region-end) nil))))))

; Remove trailing white space
(add-hook 'before-save-hook 'delete-trailing-whitespace)

; Extra file associations
(add-to-list 'auto-mode-alist '("\\.erb$" . html-mode))
(add-to-list 'auto-mode-alist '("\\.erubis$" . html-mode))
(add-to-list 'auto-mode-alist '("Rakefile" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
