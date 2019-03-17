
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

;; Column number in status bar
(column-number-mode)

;; Revert buffer
(defun tb-revert-buffer ()
  (interactive)
  (revert-buffer t t t)
  (message (concat "Reverted buffer " (buffer-name))))

;; Rebind C-x C-b to (ibuffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Copy to OS X clipboard using pbcopy
(defun tb-pbcopy (text &optional push)
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" nil "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))
(setq interprogram-cut-function 'tb-pbcopy)

;; --- Vim style stuff (begin) ---
;; Open new line below like Vim(tm) does
(defun tb-open-line-below ()
  "Open a new line below the current point and indent."
  (interactive)
  (let ((oldpos (point)))
    (end-of-line)
    (newline-and-indent)))

;; Open new line above like Vim(tm) does
(defun tb-open-line-above ()
  "Open a new line above the current line and indent."
  (interactive)
  (let ((oldpos (point)))
    (previous-line)
    (end-of-line)
    (newline-and-indent)))

;; And finally bind them to C-o and M-o
(global-set-key (kbd "M-o") 'tb-open-line-below)
(global-set-key (kbd "C-M-o") 'tb-open-line-above)
;; --- Vim style stuff (end) ---

(use-package init-ido)
;; (use-package init-ivy)
;; (use-package init-company)
(use-package lux-mode)
(use-package yang-mode)
(use-package init-magit)
(use-package init-erlang)
