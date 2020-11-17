
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
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#f5f5f5" "#ac4142" "#90a959" "#f4bf75" "#6a9fb5" "#aa759f" "#6a9fb5" "#303030"])
 '(ansi-term-color-vector
   [unspecified "#181818" "#ab4642" "#a1b56c" "#f7ca88" "#7cafc2" "#ba8baf" "#7cafc2" "#d8d8d8"])
 '(case-fold-search nil)
 '(company-quickhelp-color-background "#4F4F4F")
 '(company-quickhelp-color-foreground "#DCDCCC")
 '(custom-safe-themes
   (quote
    ("2a998a3b66a0a6068bcb8b53cd3b519d230dd1527b07232e54c8b9d84061d48d" "146061a7ceea4ccc75d975a3bb41432382f656c50b9989c7dc1a7bb6952f6eb4" "e4486d0ad184fb7511e391b6ecb8c4d7e5ab29e2d33bc65403e2315dbacaa4aa" default)))
 '(fci-rule-color "#383838")
 '(git-gutter:diff-option "HEAD")
 '(ibuffer-formats
   (quote
    ((mark modified read-only locked " "
           (name 35 35 :left :elide)
           " "
           (mode 16 16 :left :elide)
           " " filename-and-process)
     (mark " "
           (name 16 -1)
           " " filename))))
 '(indent-tabs-mode nil)
 '(nrepl-message-colors
   (quote
    ("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3")))
 '(package-selected-packages
   (quote
    (plantuml-mode ## markdown-mode git-gutter smex magit company ivy-hydra erlang creamsody-theme base16-theme ahungry-theme)))
 '(pdf-view-midnight-colors (quote ("#DCDCCC" . "#383838")))
 '(pos-tip-background-color "#1A3734")
 '(pos-tip-foreground-color "#FFFFC8")
 '(red "#ffffff")
 '(send-mail-function (quote smtpmail-send-it)))

;; === TB CUSTOMIZATION START ===

;; Add MELPA
;;(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)

;; Setup load-path to ~/.emacs.d/local
(add-to-list 'load-path (expand-file-name "local" user-emacs-directory))
(add-to-list 'custom-theme-load-path (expand-file-name "themes" custom-theme-directory))

;; Dark theme
(setq zenburn-override-colors-alist '(("zenburn-bg"  . "#2a2a2a")))
(load-theme 'zenburn t)

;; Enable use-package
(eval-when-compile (require 'use-package))

;; Make it lean and mean
(menu-bar-mode 0)
(setq inhibit-startup-screen 1)
(defalias 'yes-or-no-p 'y-or-n-p)

;; Prefer UTF-8
(prefer-coding-system 'utf-8)

;; Turn off TABS
(setq-default indent-tabs-mode nil)

;; Turn off backup files
(setq make-backup-files nil)

;; Remeber file positions
(save-place-mode 1)

;; Verical split (possibly FIXME)
(setq split-height-threshold nil)

;; Line numbers
(global-display-line-numbers-mode t)

;; Column number in status bar
(column-number-mode)

;; Git-gutter
(global-git-gutter-mode 1)
(global-set-key (kbd "M-]") 'git-gutter:next-hunk)
(global-set-key (kbd "M-[") 'git-gutter:previous-hunk)
(global-set-key (kbd "M-\\") 'git-gutter:popup-hunk)

;; Whitespace for programming modes
(defun tb-prog-mode-hook ()
  (setq whitespace-style '(face trailing tabs lines-tail empty))
  (whitespace-mode)
  (which-function-mode)
  (show-paren-mode))
(add-hook 'prog-mode-hook 'tb-prog-mode-hook)

;; Bind key to org-capture
;; (global-set-key (kbd "C-c n") (lambda () (interactive) (org-capture nil "n")))
;; A nice template for capturing notes
;; (setq org-capture-templates
;;       '(("n" "Note" entry (file+headline "~/Documents/notes.org" "Notes")
;;          "* NOTE %?\n  %i\n  %a")))

;; Revert buffer
(defun tb-revert-buffer ()
  (interactive)
  (revert-buffer t t t)
  (message (concat "Reverted buffer " (buffer-name))))

;; Rebind C-x C-b to (ibuffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Bind region commenting globally
(global-set-key (kbd "C-c C-c") 'comment-region)
(global-set-key (kbd "C-c C-u") 'uncomment-region)

;; Open file under cursor
(global-set-key (kbd "C-x F") 'find-file-at-point)

;; Rebind M-z to 'zap-up-to-char (was 'zap-to-char)
(global-set-key (kbd "M-z") 'zap-up-to-char)

;; Redraw display
(global-set-key (kbd "<f1>") 'redraw-display)

;; Copy to remote using rpbcopy
(defun tb-rpbcopy (text &optional push)
  (let ((process-connection-type nil))
    (let ((proc (start-process "rpb-copy" nil "rpb")))
      (process-send-string proc text)
      (process-send-eof proc))))
(setq interprogram-cut-function 'tb-rpbcopy)

;; Paste from remote using rpb
(defun tb-rpbpaste ()
  (shell-command-to-string "rpb p"))
(setq interprogram-paste-function 'tb-rpbpaste)

;; Window move together with tmux
(defun windmove-emacs-or-tmux(dir tmux-cmd)
  (interactive)
  (if (ignore-errors (funcall (intern (concat "windmove-" dir))))
      nil ;; Moving within emacs
    (shell-command tmux-cmd)) ;; At edges, send command to tmux
  )

(global-set-key (kbd "M-P")
                '(lambda ()
                   (interactive)
                   (windmove-emacs-or-tmux "up" "tmux select-pane -U")))
(global-set-key (kbd "M-N")
                '(lambda ()
                   (interactive)
                   (windmove-emacs-or-tmux "down" "tmux select-pane -D")))
(global-set-key (kbd "M-F")
                '(lambda ()
                   (interactive)
                   (windmove-emacs-or-tmux "right" "tmux select-pane -R")))
(global-set-key (kbd "M-B")
                '(lambda ()
                   (interactive)
                   (windmove-emacs-or-tmux "left"  "tmux select-pane -L")))


;; --- Vim style stuff (begin) ---------------------------------------
;; Open new line below like Vim(tm) does
;; (defun tb-open-line-below ()
;;   "Open a new line below the current point and indent."
;;   (interactive)
;;   (let ((oldpos (point)))
;;     (end-of-line)
;;     (newline-and-indent)))

;; ;; Open new line above like Vim(tm) does
;; (defun tb-open-line-above ()
;;   "Open a new line above the current line and indent."
;;   (interactive)
;;   (let ((oldpos (point)))
;;     (previous-line)
;;     (end-of-line)
;;     (newline-and-indent)))

;; Join line below point with current line
(defun tb-join-line-below ()
  "Joins the line below point with the current line."
  (interactive)
  (move-end-of-line nil)
  (delete-char 1)
  (just-one-space))
;; Bind M-j to tb-join-line-below
(global-set-key (kbd "M-j") 'tb-join-line-below)
;; --- Vim style stuff (end) -----------------------------------------

(setq-default c-basic-offset 4)

(require 'yang-mode)
(defun my-yang-mode-hook ()
  "Configuration for YANG Mode. Add this to `yang-mode-hook'."
  (progn
    (setq c-basic-offset 2)))
(add-hook 'yang-mode-hook 'my-yang-mode-hook)


(use-package init-ido)
;; (use-package init-ivy)
;; (use-package init-company)
(use-package lux-mode)
(use-package cc-mode
  :config (setq c-basic-offset 4))
(use-package init-magit)
(use-package init-erlang)
(use-package smex
  :ensure t
  :bind (("M-x" . smex))
  :config (smex-initialize))

;; Color and font configuration - dark theme
(use-package whitespace
  :config (set-face-attribute 'whitespace-tab nil :background "#2c2c2c"))
(use-package faces
  :config
  (set-face-attribute 'region nil :background "#ced872" :foreground "#000000"))
(use-package isearch
  :config
  (set-face-attribute 'isearch nil :background "#ced872" :foreground "#000000"))
(use-package ediff
  :config
  (setq ediff-split-window-function 'split-window-horizontally))
(use-package font-lock
  :config
  (set-face-attribute 'font-lock-comment-face nil :foreground "color-246"))
;; Dark Mode
(use-package diff-mode
  :config
  (set-face-attribute 'diff-added nil :foreground "#000000" :background "#77cc77")
  (set-face-attribute 'diff-removed nil :foreground "#000000" :background "#bb4444"))

(setq global-hl-line-sticky-mode t)
(global-hl-line-mode t)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
