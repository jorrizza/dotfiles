;; .emacs of jorrizza@jrrzz.net
;; GNU Emacs 25.2.2

;; Package management
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")))
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))
(defvar my-packages '(fill-column-indicator go-mode js2-mode
  xref-js2 ruby-mode ruby-end robe less-css-mode lua-mode org
  smooth-scrolling yaml-mode graphviz-dot-mode tramp coffee-mode
  php-mode markdown-mode dockerfile-mode solarized-theme ag
  web-mode erlang rust-mode toml-mode company company-web elpy
  pyenv-mode kotlin-mode)
  "Nice packages I depend upon.")
(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; Don't use Simplified English
  (setq ispell-dictionary "en_GB")

;; We don't need these
(toggle-scroll-bar -1)
(tool-bar-mode -1)
(menu-bar-mode -1)

;; Unique double filenames
(require 'uniquify)

;; Set windows title
(setq frame-title-format '("" "%b - Emacs " emacs-version))

;; Nice font FTW
(set-default-font "Terminus 8")

;; Nice colours even more FTW
(load-theme 'solarized-dark t)

;; Standard indent size
(setq-default standard-indent 2)
(setq-default c-basic-offset 2)

;; We use versioning. Backup files are not needed.
(setq make-backup-files nil)

;; Fill mode
(setq-default fill-column 79)

;; Damn you tabs!
(setq-default indent-tabs-mode nil)

;; Line numbering
(global-linum-mode)
(setq linum-format "%4d")

;; Show column number
(column-number-mode 1)

;; Highlight matching bracket
(show-paren-mode 1)

;; Highlight current line
;; (global-hl-line-mode 1)
;; (set-face-background 'hl-line "#033")

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

;; ido-mode
(ido-mode t)

;; Tramp
(setq tramp-default-method "ssh")

;; Org
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

;; Company mode is autocompletion
(add-hook 'after-init-hook 'global-company-mode)
(eval-after-load 'company
  '(lambda ()
     (push 'company-robe company-backends)
     (push 'company-web-html company-backends)
     ))
;;  '(push 'company-robe company-backends))

;; Company mode breaks when fci-mode is open, this is a workaround
;; https://github.com/company-mode/company-mode/issues/180
(defvar-local company-fci-mode-on-p nil)

(defun company-turn-off-fci (&rest ignore)
  (when (boundp 'fci-mode)
    (setq company-fci-mode-on-p fci-mode)
    (when fci-mode (fci-mode -1))))

(defun company-maybe-turn-on-fci (&rest ignore)
  (when company-fci-mode-on-p (fci-mode 1)))

(add-hook 'company-completion-started-hook 'company-turn-off-fci)
(add-hook 'company-completion-finished-hook 'company-maybe-turn-on-fci)
(add-hook 'company-completion-cancelled-hook 'company-maybe-turn-on-fci)

;; Go
(add-hook 'go-mode-hook '(lambda ()
                           (set 'tab-width 2)
                           (set 'indent-tabs-mode t)))

;; C
(add-hook 'c-mode-hook '(lambda ()
                          (interactive) (fci-mode)))

;; C++
(add-hook 'c++-mode-hook '(lambda ()
                          (interactive) (fci-mode)))

;; Ruby
(add-hook 'ruby-mode-hook '(lambda ()
                             (set 'ruby-insert-encoding-magic-comment nil)
                             (robe-mode)
                             (interactive) (fci-mode)))

;; Kotlin
(add-hook 'kotlin-mode-hook '(lambda ()
                               (set 'kotlin-tab-width 4)
                               (interactive) (fci-mode)))

;; Python
(pyenv-mode)
(elpy-enable)
(add-hook 'python-mode-hook '(lambda ()
                               (interactive) (fci-mode)
                               (interactive) (elpy-mode)
                               (set 'python-indent-offset 4)
                               (set-newline-and-indent)))

;; YAML
(add-hook 'yaml-mode-hook '(lambda ()
                             (set-newline-and-indent)))

;; CSS
(add-hook 'css-mode-hook '(lambda ()
                            (set 'css-indent-offset 2)))

;; JavaScript
(add-hook 'js2-mode-hook '(lambda ()
                            (interactive) (fci-mode)
                            ;; js-mode already has M-. but we want to use it for xref-js2
                            (define-key js-mode-map (kbd "M-.") nil)
                            (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)
                            (set 'js2-cleanup-whitespace t)
                            (set 'js2-basic-offset 2)
                            (set 'js2-user-font-lock-faces t)))
;; Templates
(add-hook 'web-mode-hook '(lambda ()
                            (set 'web-mode-enable-current-element-highlight t)
                            (set 'web-mode-enable-current-column-highlight t)))

;; CoffeeScript
(add-hook 'coffee-mode-hook '(lambda ()
                               (set 'coffee-tab-width 2)))

;; Markdown
(add-hook 'markdown-mode-hook '(lambda ()
                                 (interactive) (flyspell-mode)
                                 (interactive) (fci-mode)))

;; Auto-indent with a yank
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
                                     less-css-mode   scss-mode
                                     web-mode        coffee-mode))
                (let ((mark-even-if-inactive transient-mark-mode))
                  (indent-region (region-beginning) (region-end) nil))))))

;; Remove trailing white space
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Support for ctags
(defun create-tags (dir-name)
  "Create tags file."
  (interactive "DDirectory: ")
  (shell-command
   (format "%s -f %s/TAGS -e -R %s" "ctags" dir-name (directory-file-name dir-name))))

;; Emacs Sound Board
(setq soundboard '(("d h h" "this-is-nasty")
                   ("s j o" "sjongejonge")
                   ("n e t" "netjes")
                   ("j u i" "juist")
                   ("h a n" "hannibal")
                   ("g i n" "zuilen")
                   ("i k p" "is-kapot")
                   ("m k p" "maak-kapot")
                   ("f i n" "financieel-voordeel")
                   ("w e g" "weg-kwijt")
                   ("v u r" "vuurbal")
                   ("r u t" "mooi")
                   ("g a y" "ha-gay")
                   ("h a h" "hahaha")
                   ("h e h" "haha")))
(dolist (sound soundboard)
  (defalias (intern (nth 1 sound))
    `(lambda ()
       ,(format "Play the %s sound" (nth 1 sound))
       (interactive)
       (start-process-shell-command ,(concat (nth 1 sound) "-player")
                                    ,(nth 1 sound)
                                    "mpv"
                                    "--really-quiet"
                                    ,(format "~/.emacs.d/%s.mp3" (nth 1 sound)))))
  (global-set-key (kbd (concat "C-c " (car sound)))
                  (intern (nth 1 sound))))

;; Extra file associations
(add-to-list 'auto-mode-alist '("\\.html?$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erubis$" . web-mode))
(add-to-list 'auto-mode-alist '("Rakefile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.prawn$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.nut$" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.kts$" . kotlin-mode))



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t)
 '(org-agenda-files
   (quote
    ("/ssh:codeventur.es:/home/jorrizza/org/business.org" "/ssh:codeventur.es:/home/jorrizza/org/personal.org")))
 '(package-selected-packages
   (quote
    (protobuf-mode flycheck-kotlin kotlin-mode xref-js2 pyenv-mode fill-column-indicator yaml-mode web-mode toml-mode solarized-theme smooth-scrolling slim-mode sass-mode rust-mode php-mode org markdown-mode lua-mode less-css-mode js2-mode hackernews graphviz-dot-mode go-mode erlang dockerfile-mode column-marker coffee-mode ag))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-tooltip-selection ((t (:underline t :weight bold)))))
