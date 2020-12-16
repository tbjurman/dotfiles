
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
 '(case-fold-search nil)
 '(custom-enabled-themes (quote (wombat)))
 '(git-gutter:diff-option "HEAD")
 '(indent-tabs-mode nil)
 '(package-selected-packages
   (quote
    (realgud-lldb yaml-mode rust-mode markdown-mode git-gutter smex magit company ivy-hydra erlang creamsody-theme base16-theme ahungry-theme))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-comment-face ((t (:foreground "color-246" :slant italic)))))

;; === TB CUSTOMIZATION START ===

;; Add MELPA
;;(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)

;; Setup load-path to ~/.emacs.d/local
(add-to-list 'load-path (expand-file-name "local" user-emacs-directory))

;; Enable use-package
(eval-when-compile
  (add-to-list 'load-path "/Users/tbjurman/dev/ext/use-package")
  (require 'use-package))

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

(global-hl-line-mode t)

;; Copy to pasteboard using pbcopy
(defun tb-pbcopy (text &optional push)
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" nil "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))
(setq interprogram-cut-function 'tb-pbcopy)

(defun tb-pbpaste ()
  (shell-command-to-string "pbpaste"))
(setq interprogram-paste-function 'tb-pbpaste)

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

(use-package init-ido)
;; (use-package init-ivy)
;; (use-package init-company)
(use-package lux-mode)
(use-package yang-mode)
(use-package init-magit)
(use-package init-erlang)
(use-package realgud-lldb)
