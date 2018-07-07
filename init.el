(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)
(dolist (package 
	 '(use-package)) 
  (unless (package-installed-p package) 
    (package-install package)))

;; UI config

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
(set-frame-font "Fira Code Medium-12")
;; No line wrap
(set-default 'truncate-lines 0)
;; Backup files
(setq backup-directory-alist `((".*" . ,temporary-file-directory))) 
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))


;; Vi emulation
(use-package 
  evil 
  :ensure t 
  :config (evil-mode 1))

;; Theme
(use-package 
  base16-theme 
  :ensure t 
  :config (load-theme 'base16-tomorrow-night t))

;; Spaceline
(use-package 
  spaceline 
  :ensure t 
  :config (setq powerline-default-separator 'utf-8) 
  (setq spaceline-separator-dir-left '(right . right)) 
  (setq spaceline-separator-dir-right '(right . right)) 
  (setq powerline-default-separator 'alternate) 
  (setq spaceline-workspace-numbers-unicode t) 
  (setq spaceline-window-numbers-unicode t) 
  (setq spaceline-highlight-face-func #'spaceline-highlight-face-evil-state) 
  (spaceline-spacemacs-theme) 
  (spaceline-compile))


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

;; Automatch parentheses
(use-package 
  smartparens 
  :ensure t 
  :config (smartparens-global-mode 1))

;; Clojure mode
(use-package
  clojure-mode
  :ensure t)
;; Clojure REPL
(use-package
  cider
  :ensure t)

;; Enable cider on clojure mode
(add-hook 'clojure-mode-hook (lambda () (cider-mode 1)))

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
    (haskell-mode clojure-mode zenburn-theme elisp-format smartparense use-package spaceline smartparens rainbow-delimiters ivy evil company base16-theme))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
