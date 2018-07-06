(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

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
(set-frame-font "Gohufont-11")
;; Rainbow delimiters
(rainbow-delimiters-mode 1)
;; No line wrap
(set-default 'truncate-lines 0)
;; Backup files
(setq backup-directory-alist
          `((".*" . ,temporary-file-directory)))
    (setq auto-save-file-name-transforms
          `((".*" ,temporary-file-directory t)))


;; Vi emulation
(require 'evil)
    (evil-mode 1)
;; Theme
(load-theme 'base16-tomorrow-night t)

;; Spaceline
(require 'spaceline)
    (setq powerline-default-separator 'utf-8)
    (setq spaceline-separator-dir-left '(right . right))
    (setq spaceline-separator-dir-right '(right . right))
    (setq powerline-default-separator 'alternate)
    (setq spaceline-workspace-numbers-unicode t)
    (setq spaceline-window-numbers-unicode t)
    (setq spaceline-highlight-face-func #'spaceline-highlight-face-evil-state)
(require 'spaceline-config)
    (spaceline-spacemacs-theme)
    (spaceline-compile)

;; Rainbow delimiters on programming modes
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; Highlight matching parentheses
(show-paren-mode 1)

;; Completion
(require 'company)
    (setq company-idle-delay 0.2)
    (setq copany-selection-wrap-around t)
    (define-key company-active-map [tab] 'company-complete)
    (define-key company-active-map (kbd "C-n") 'company-select-next)
    (define-key company-active-map (kbd "C-p") 'company-select-previous)

;; Command completion
(require 'ivy)
    (ivy-mode 1)

;; Automatch parentheses
(smartparens-global-mode 1)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("3380a2766cf0590d50d6366c5a91e976bdc3c413df963a0ab9952314b4577299" "f27c3fcfb19bf38892bc6e72d0046af7a1ded81f54435f9d4d09b3bff9c52fc1" "cd4d1a0656fee24dc062b997f54d6f9b7da8f6dc8053ac858f15820f9a04a679" "1c082c9b84449e54af757bcae23617d11f563fc9f33a832a8a2813c4d7dfb652" "b35a14c7d94c1f411890d45edfb9dc1bd61c5becd5c326790b51df6ebf60f402" "3a3de615f80a0e8706208f0a71bbcc7cc3816988f971b6d237223b6731f91605" "25c06a000382b6239999582dfa2b81cc0649f3897b394a75ad5a670329600b45" "6daa09c8c2c68de3ff1b83694115231faa7e650fdbb668bc76275f0f2ce2a437" "f0dc4ddca147f3c7b1c7397141b888562a48d9888f1595d69572db73be99a024" "151bde695af0b0e69c3846500f58d9a0ca8cb2d447da68d7fbf4154dcf818ebc" "9d9fda57c476672acd8c6efeb9dc801abea906634575ad2c7688d055878e69d6" "d1b4990bd599f5e2186c3f75769a2c5334063e9e541e37514942c27975700370" "a566448baba25f48e1833d86807b77876a899fc0c3d33394094cf267c970749f" "93a0885d5f46d2aeac12bf6be1754faa7d5e28b27926b8aa812840fe7d0b7983" "a3fa4abaf08cc169b61dea8f6df1bbe4123ec1d2afeb01c17e11fdc31fc66379" "4697a2d4afca3f5ed4fdf5f715e36a6cac5c6154e105f3596b44a4874ae52c45" "26d49386a2036df7ccbe802a06a759031e4455f07bda559dcf221f53e8850e69" default)))
 '(package-selected-packages
   (quote
    (smartparens gruvbox-theme cider 0xc ivy company htmlize magit rainbow-mode spacemacs-theme spaceline-all-the-icons spaceline doom-themes moe-theme rainbow-delimiters clojure-mode evil base16-theme))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
