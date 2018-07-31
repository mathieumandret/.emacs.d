;; Setup package repositories
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

(package-initialize)
;; Init use-package
(dolist (package 
	 '(use-package)) 
  (unless (package-installed-p package) 
    (package-install package)))

;; No toolbar
(tool-bar-mode -1)
;; No menu bar
(menu-bar-mode -1)
;; No scrollbar
(scroll-bar-mode -1)
;; No cursor blinking
(blink-cursor-mode 0)
;; Line numbers
(global-linum-mode 0)
;; No bell
(setq ring-bell-function 'ignore)
;; Font
(set-frame-font "Iosevka Term Semibold-12")
;; No line wrap
(set-default 'truncate-lines 0)
;; Custom startup
(setq inhibit-startup-screen t)
;; Kek
(setq initial-scratch-message ";; Welcome to emacs !")

;; Backup files
(setq backup-directory-alist `((".*" . ,temporary-file-directory))) 
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;; Exec Path
(setq exec-path (append exec-path '("/home/mathieu/.local/bin")))


;; Vi emulation
(use-package 
  evil 
  :ensure t 
  :config (evil-mode 1))

;; Allow rebinding of <ESC>
(use-package evil-escape
  :ensure t
  :init
  (setq-default evil-escape-key-sequence "jk")
  :config
  (evil-escape-mode 1))

;; Theme
(use-package 
  doom-themes
  :ensure t 
  :config (load-theme 'doom-tomorrow-night t))

;; Powerline
(use-package spaceline
  :ensure t
  :init
  (setq powerline-default-separator 'slant)
  :config
  (spaceline-emacs-theme)
  (spaceline-toggle-minor-modes-off)
  (spaceline-toggle-buffer-size-off)
  (spaceline-toggle-evil-state-on))


(use-package 
  rainbow-delimiters 
  :ensure t 
  :config (rainbow-delimiters-mode 1))
;; Rainbow delimiters on programming modes
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; Highlight matching parentheses
(show-paren-mode 1)

;; Completion framework
(use-package 
  company 
  :ensure t 
  :config (setq company-idle-delay 0.2) 
  (setq copany-selection-wrap-around t) 
  (define-key company-active-map [tab] 'company-complete) 
  (define-key company-active-map (kbd "C-n") 'company-select-next) 
  (define-key company-active-map (kbd "C-p") 'company-select-previous))

;; Command completion
(use-package 
  ivy 
  :ensure t 
  :config (ivy-mode 1))

;; Syntax checking
(use-package
  flycheck
  :ensure t
  )

;; Automatch parentheses
(use-package 
  smartparens 
  :ensure t 
  :config (smartparens-global-mode 1))

;; Explorer tree
(use-package
  neotree
  :ensure t
  :config
  ;; Use icons on GUI
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  )

;; Icons
(use-package
  all-the-icons
  :ensure t
  )

;; Clojure REPL
(use-package
  cider
  :ensure t)

;; Clojure mode
(use-package
  clojure-mode
  :ensure t
  :config
  ;; Enable cider on clojure mode
  (add-hook 'clojure-mode-hook (lambda () (cider-mode 1)))
  )

;; Python
(use-package
  py-autopep8
  :ensure t)

(use-package
  elpy
  :ensure t
  :config
  (elpy-enable)
  ;; Syntax
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode)
  ;; PEP8
  (add-hook  'elpy-mode-hook 'py-autopep8-enable-on-save)
  )



;; Use fancy symbols (e.g lambda)
(prettify-symbols-mode 1)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("3380a2766cf0590d50d6366c5a91e976bdc3c413df963a0ab9952314b4577299" "4feee83c4fbbe8b827650d0f9af4ba7da903a5d117d849a3ccee88262805f40d" "190a9882bef28d7e944aa610aa68fe1ee34ecea6127239178c7ac848754992df" default)))
 '(package-selected-packages
   (quote
    (yasnippet-classic-snippets py-autopep8 py-autopep flycheck elpy all-the-icons-gnus neotree evil-escape haskell-mode clojure-mode zenburn-theme elisp-format smartparense use-package spaceline smartparens rainbow-delimiters ivy evil company base16-theme))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
