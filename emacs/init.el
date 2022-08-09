;; Add MELPA
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

;; Move customization variables to a separate file and load it
(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessage)

;; Enable use-package
;; (eval-when-compile
;;  (add-to-list 'load-path "/Users/tbjurman/dev/ext/use-package")
(require 'use-package)

;; === TB CUSTOMIZATION START ===

;; Style it
;;(use-package solarized-theme :ensure t)
;;(load-theme 'solarized-selenized-light t)
;;(use-package github-theme :ensure t)
;;(load-theme 'github t)
;; (use-package dracula-theme :ensure t)
;; (load-theme 'dracula t)
;; (use-package zenburn-theme :ensure t)
;; (load-theme 'zenburn t)
;; (use-package solarized-theme :ensure t)
;; (load-theme 'solarized-light t)

;; (use-package github-theme
;;   :ensure t
;;   :config
;;   (load-theme 'github t))

(use-package modus-themes
  :ensure
  :init
  ;; Add all your customizations prior to loading the themes
  (setq modus-themes-italic-constructs nil
        modus-themes-bold-constructs nil
        modus-themes-region '(bg-only)
        modus-themes-diffs 'desaturated
        )

  ;; Load the theme files before enabling a theme
  (modus-themes-load-themes)
  :config
  ;; Load the theme of your choice:
  (modus-themes-load-operandi) ;; OR (modus-themes-load-vivendi)
  :bind ("C-c t" . modus-themes-toggle))


;; Setup load-path to ~/.emacs.d/local
(add-to-list 'load-path (expand-file-name "local" user-emacs-directory))

;; Make it lean and mean
(menu-bar-mode -1)

(if (display-graphic-p)
    (progn (toggle-scroll-bar -1)
           (tool-bar-mode -1)))

;; Setup exec-path
(if (display-graphic-p)
    (progn
      (defun set-exec-path-from-shell-PATH ()
        "Set up Emacs' `exec-path' and PATH environment variable to match
that used by the user's shell.

This is particularly useful under Mac OS X and macOS, where GUI
apps are not started from a shell."
      (interactive)
      (let ((path-from-shell (replace-regexp-in-string
                              "[ \t\n]*$" "" (shell-command-to-string
                                              "$SHELL --login -c 'echo $PATH'"
                                              ))))
        (message path-from-shell)
        (setenv "PATH" path-from-shell)
        (setq exec-path (split-string path-from-shell path-separator))))
      (set-exec-path-from-shell-PATH)))

(setq inhibit-startup-screen 1)
(defalias 'yes-or-no-p 'y-or-n-p)

;; Prefer UTF-8
(prefer-coding-system 'utf-8)

;; Turn off TABS
(setq-default indent-tabs-mode nil)

;; do case sensitive search
(setq-default case-fold-search nil)

;; Turn off backup files
(setq make-backup-files nil)

;; Be quiet - flash the mode-line instead
(defun tb-mode-line-visual-bell--flash ()
  (let ((frame (selected-frame)))
    (run-with-timer
     0.1 nil
     (lambda (frame)
       (let ((inhibit-quit)
             (inhibit-redisplay t))
         (invert-face 'mode-line frame)
         (invert-face 'mode-line-inactive frame)))
     frame)
    (let ((inhibit-quit)
          (inhibit-redisplay t))
      (invert-face 'mode-line frame)
      (invert-face 'mode-line-inactive frame))))

(setq ring-bell-function 'tb-mode-line-visual-bell--flash)

;; Remeber file positions
(save-place-mode 1)

;; Verical split (possibly FIXME)
(setq split-height-threshold nil)

;; Line numbers
(global-display-line-numbers-mode t)

;; Colmn number in status bar
(column-number-mode)

;; Default 4 spaces indentation
(setq-default c-basic-offset 4)

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
  (show-paren-mode)
  (setq fill-column 80)
  (display-fill-column-indicator-mode))
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
(if (not (display-graphic-p))
    (progn
      (defun windmove-emacs-or-tmux(dir tmux-cmd)
        (interactive)
        (if (ignore-errors (funcall (intern (concat "windmove-" dir))))
            nil ;; Moving within emacs
          (shell-command tmux-cmd)) ;; At edges, send command to tmux
        )

      (global-set-key (kbd "M-P")
                      (lambda ()
                        (interactive)
                        (windmove-emacs-or-tmux "up" "tmux select-pane -U")))
      (global-set-key (kbd "M-N")
                      (lambda ()
                        (interactive)
                        (windmove-emacs-or-tmux "down" "tmux select-pane -D")))
      (global-set-key (kbd "M-F")
                      (lambda ()
                        (interactive)
                        (windmove-emacs-or-tmux "right" "tmux select-pane -R")))
      (global-set-key (kbd "M-B")
                      (lambda ()
                        (interactive)
                        (windmove-emacs-or-tmux "left"  "tmux select-pane -L"))))
  (progn
    (global-set-key (kbd "C-s-p")
                    (lambda ()
                      (interactive)
                      (windmove-up)))
    (global-set-key (kbd "C-s-n")
                    (lambda ()
                      (interactive)
                      (windmove-down)))
    (global-set-key (kbd "C-s-f")
                    (lambda ()
                      (interactive)
                      (windmove-right)))
    (global-set-key (kbd "C-s-b")
                    (lambda ()
                      (interactive)
                      (windmove-left)))))

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
  (end-of-line)
  (delete-char 1)
  (just-one-space))

;; Bind M-j to tb-join-line-below
(global-set-key (kbd "M-j") 'tb-join-line-below)
;; --- Vim style stuff (end) -----------------------------------------

;; ############################################################################
(use-package rust-mode
  :ensure t)

;; ############################################################################
;; (use-package rustic
;;   :ensure t
;;   :bind (:map rustic-mode-map
;;               ("C-c C-c j" . lsp-ui-imenu)
;;               ("C-c C-c ?" . lsp-find-references)
;;               ("C-c C-c l" . flycheck-list-errors)
;;               ("C-c C-c a" . lsp-execute-code-action)
;;               ("C-c C-c r" . lsp-rename)
;;               ("C-c C-c q" . lsp-workspace-restart)
;;               ("C-c C-c Q" . lsp-workspace-shutdown)
;;               ("C-c C-c s" . lsp-rust-analyzer-status))
;;   :config
;;   ;; uncomment for less flashiness
;;   ;; (setq lsp-eldoc-hook nil)
;;   ;; (setq lsp-enable-symbol-highlighting nil)
;;   ;; (setq lsp-signature-auto-activate nil)

;;   ;; comment to disable rustfmt on save
;;   (setq rustic-format-on-save t)
;;   (add-hook 'rustic-mode-hook 'rk/rustic-mode-hook))

;; (defun rk/r>84;0;0custic-mode-hook ()
;;   ;; so that run C-c C-c C-r works without having to confirm
;;   (setq-local buffer-save-without-query t))

;; ############################################################################
;; (use-package flycheck :ensure t)
;; ############################################################################
;; (use-package lsp-mode
;;   :ensure
;;   :commands lsp
;;   :custom
;;   ;; what to use when checking on-save. "check" is default, I prefer clippy
;;   (lsp-rust-analyzer-cargo-watch-command "clippy")
;;   (lsp-eldoc-render-all t)
;;   (lsp-idle-delay 0.5)
;;   (lsp-rust-analyzer-server-display-inlay-hints nil))

  ;; :hook ((rustic-mode . lsp)
  ;;        (lsp-mode . lsp-enable-which-key-integration)))
  ;; :config
  ;; (add-hook 'lsp-mode-hook 'lsp-ui-mode))

;; ############################################################################
;; (use-package lsp-ui
;;   :ensure
;;   :commands lsp-ui-mode
;;   :custom
;;   (lsp-ui-peek-always-show t)
;;   (lsp-ui-sideline-show-hover nil)
;;   (lsp-ui-doc-enable t))

;; ############################################################################
;; (use-package rust-mode :ensure t)

;; ############################################################################
(use-package sql-indent :ensure t)

;; ############################################################################
(use-package graphviz-dot-mode :ensure t)

;; ############################################################################
(use-package realgud-lldb :ensure t)

;; ############################################################################
(use-package yaml-mode :ensure t)

;; ############################################################################
(use-package markdown-mode :ensure t)

(use-package diff-mode
  :ensure t
  :config
  ;; (set-face-attribute
  ;;  'diff-header nil :background "gray55")
  ;; (set-face-attribute
  ;;  'diff-added nil :background "green")
  ;; (set-face-attribute
  ;;  'diff-removed nil :background "red" :foreground "black")
  ;; (set-face-attribute
  ;;  'diff-changed nil :foreground "purple"))
  ;; (set-face-attribute 'diff-refine-header nil
  ;;                     :background "gray55")
  (set-face-attribute 'diff-refine-added nil
                      :foreground "white" :background "darkgreen")
  (set-face-attribute 'diff-refine-removed nil
                      :foreground "white" :background "darkred")
  (set-face-attribute 'diff-refine-changed nil
                      :foreground "white" :background "darkblue"))

;; ############################################################################
(use-package git-gutter
  :ensure t
  :config
  (setq git-gutter:diff-option "HEAD"))

;; ############################################################################
(use-package smex :ensure t)

;; ############################################################################
(use-package magit
  :ensure t
  :config
  (global-set-key (kbd "C-x g") 'magit-status))

;; ############################################################################
(use-package lux-mode :ensure t)

;; ############################################################################
(use-package yang-mode :ensure t)

;; ############################################################################
(use-package erlang :ensure t)

;; ############################################################################
(use-package tailf)

;; ############################################################################
(use-package erl-find-source
  :init
  :hook erlang-mode-hook
  :bind (:map erlang-mode-map
         ("M-." . erlfs-find-source-under-point)
         ("M-," . erlfs-find-source-unwind)
         ("M-?" . erlfs-find-callers)))

;; ############################################################################
(use-package ido
  :ensure t
  :config
  (setq ido-enable-flex-matching t)
  (setq ido-everywhere t)
  (setq confirm-nonexistent-file-or-buffer nil)
  (setq ido-create-new-buffer 'always)
  (ido-mode 1))

;; ############################################################################
(use-package company
  :ensure t
  :config
  (add-hook 'after-init-hook 'global-company-mode))

;; ############################################################################
;; (use-package doom-modeline
;;   :ensure t
;;   :hook (after-init . doom-modeline-mode)
;;   :config
;;   (setq doom-modeline-vcs-max-length 32))

;; ############################################################################
(use-package org-superstar
  :ensure t
  :config
  (org-superstar-configure-like-org-bullets)
  (add-hook 'org-mode-hook (lambda () (org-superstar-mode 1))))
