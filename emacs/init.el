
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(indent-tabs-mode nil)
 '(package-selected-packages
   (quote
    (smex magit company ivy-hydra erlang creamsody-theme base16-theme ahungry-theme))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; === TB CUSTOMIZATION START ===

;; Add MELPA
;;(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)

;; Setup load-path to ~/.emacs.d/local
(add-to-list 'load-path (expand-file-name "local" user-emacs-directory))

;; Enable use-package
(eval-when-compile (require 'use-package))

;; Turn off TABS
(setq-default indent-tabs-mode nil)

;; Turn off backup files
(setq make-backup-files nil)

;; Remeber file positions
(save-place-mode 1)

;; Line numbers
(global-linum-mode t)
(setq linum-format "%4d \u2502 ")

(use-package init-ido)
;; (use-package init-ivy)
;; (use-package init-company)
(use-package lux-mode)
(use-package yang-mode)
(use-package init-magit)
(use-package init-erlang)
