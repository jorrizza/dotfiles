; .emacs of jorrizza@jrrzz.net
; GNU Emacs 24.1.1

(add-to-list 'load-path "~/.emacs.d/")
(add-to-list 'load-path "~/.emacs.d/nyan-mode/")
(add-to-list 'load-path "~/.emacs.d/less-css-mode/")

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

; Column Marker
(require 'column-marker)

; Smooth scrolling
(require 'smooth-scroll)
(smooth-scroll-mode t)
(setq scroll-step 1)

; Nyan Cat!
(require 'nyan-mode)
(nyan-mode t)

; Standard indent size
(setq-default standard-indent 2)
(setq-default c-basic-offset 2)

; We use versioning. Backup files are not needed.
(setq make-backup-files nil)

; Fill mode
(setq-default fill-column 80)

; Damn you tabs!
(setq-default indent-tabs-mode nil)

; Line numbering
(global-linum-mode)
(setq linum-format "%4d")

; Show column number
(column-number-mode 1)

; Highlight matching bracket
(show-paren-mode 1)

; Highlight current line
(global-hl-line-mode 1)
(set-face-background 'hl-line "#033")

; Make M-arrow work for window switching
(windmove-default-keybindings 'meta)

; Go
(add-hook 'go-mode-hook '(lambda ()
                           (set 'tab-width 2)
                           (set 'indent-tabs-mode t)
                           (local-set-key (kbd "RET") 'reindent-then-newline-and-indent)))

; C
(add-hook 'c-mode-hook '(lambda ()
                          (interactive) (column-marker-1 80)
                          (local-set-key (kbd "RET") 'reindent-then-newline-and-indent)))

; C++
(add-hook 'c++-mode-hook '(lambda ()
                          (interactive) (column-marker-1 80)
                          (local-set-key (kbd "RET") 'reindent-then-newline-and-indent)))
; Ruby
(add-hook 'ruby-mode-hook '(lambda ()
                             (interactive) (column-marker-1 80)
                             (local-set-key (kbd "RET") 'reindent-then-newline-and-indent)))

; YAML
(add-hook 'yaml-mode-hook '(lambda ()
                             (local-set-key (kbd "RET") 'reindent-then-newline-and-indent)))

; CSS
(add-hook 'css-mode-hook '(lambda ()
                            (local-set-key (kbd "RET") 'reindent-then-newline-and-indent)
                            (set 'css-indent-offset 2)))

; LESS
(require 'less-css-mode)
(add-hook 'less-css-mode-hook '(lambda ()
                            (local-set-key (kbd "RET") 'reindent-then-newline-and-indent)))

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
                                     go-mode         html-mode
                                     less-css-mode))
                (let ((mark-even-if-inactive transient-mark-mode))
                  (indent-region (region-beginning) (region-end) nil))))))

; Remove trailing white space
(add-hook 'before-save-hook 'delete-trailing-whitespace)

; DHH
(defun this-is-nasty ()
  (interactive)
  (start-process-shell-command "dhh-player" "dhh" "mplayer" "~/.emacs.d/dhh_nasty.mp3"))
(global-set-key (kbd "C-c d h h")
                'this-is-nasty)
; Sjongejonge
(defun sjongejonge ()
  (interactive)
  (start-process-shell-command "sjo-player" "sjo" "mplayer" "~/.emacs.d/sjongejonge.mp3"))
(global-set-key (kbd "C-c s j o")
                'sjongejonge)

; Extra file associations
(add-to-list 'auto-mode-alist '("\\.erb$" . html-mode))
(add-to-list 'auto-mode-alist '("\\.erubis$" . html-mode))
(add-to-list 'auto-mode-alist '("Rakefile" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.nut$" . c++-mode))

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
