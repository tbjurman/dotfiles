;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

;; Add MELPA
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)

;; Enable use-package
(eval-when-compile (require 'use-package))

;; === TB CUSTOMIZATION START ===

;; Style it
;; (use-package solarized-theme :ensure t)
;; (load-theme 'solarized-light t)

;; Setup load-path to ~/.emacs.d/local
(add-to-list 'load-path (expand-file-name "local" user-emacs-directory))

;; Make it lean and mean
(menu-bar-mode 0)
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

;; Horizontal line
(setq global-hl-line-sticky-mode t)
(global-hl-line-mode t)

;; Default 4 spaces indentation
(setq-default c-basic-offset 4)

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
(defun tb-join-line-below ()
  "Joins the line below point with the current line."
  (interactive)
  (move-end-of-line nil)
  (delete-char 1)
  (just-one-space))

;; Bind M-j to tb-join-line-below
(global-set-key (kbd "M-j") 'tb-join-line-below)
;; --- Vim style stuff (end) -----------------------------------------


;; ############################################################################
(use-package flycheck :ensure t)
;; ############################################################################
(use-package lsp-mode
  :ensure
  :commands lsp
  :custom
  ;; what to use when checking on-save. "check" is default, I prefer clippy
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-eldoc-render-all t)
  (lsp-idle-delay 0.6)
  (lsp-rust-analyzer-server-display-inlay-hints nil)
  :config
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))

;; ############################################################################
(use-package lsp-ui
  :ensure
  :commands lsp-ui-mode
  :custom
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-hover nil)
  (lsp-ui-doc-enable nil))

;; ############################################################################
(use-package rustic
  :ensure t
  :bind (:map rustic-mode-map
              ("M-j" . lsp-ui-imenu)
              ("M-?" . lsp-find-references)
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c a" . lsp-execute-code-action)
              ("C-c C-c r" . lsp-rename)
              ("C-c C-c q" . lsp-workspace-restart)
              ("C-c C-c Q" . lsp-workspace-shutdown)
              ("C-c C-c s" . lsp-rust-analyzer-status))
  :config
  ;; uncomment for less flashiness
  (setq lsp-eldoc-hook nil)
  (setq lsp-enable-symbol-highlighting nil)
  (setq lsp-signature-auto-activate nil)

  ;; comment to disable rustfmt on save
  (setq rustic-format-on-save t)
  (add-hook 'rustic-mode-hook 'rk/rustic-mode-hook))

(defun rk/rustic-mode-hook ()
  ;; so that run C-c C-c C-r works without having to confirm
  (setq-local buffer-save-without-query t))

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

;; ############################################################################
(use-package git-gutter
  :ensure t
  :config
  (setq git-gutter:diff-option "HEAD"))

;; ############################################################################
(use-package smex
  :ensure t
  :bind (("M-x" . smex))
  :config (smex-initialize))

;; ############################################################################
(use-package magit
  :ensure t
  :config
  (global-set-key (kbd "C-x g") 'magit-status))

;; ############################################################################
(use-package lux-mode :ensure t)

;; ############################################################################
(use-package yang-mode :ensure t)

(defun my-yang-mode-hook ()
  "Configuration for YANG Mode. Add this to `yang-mode-hook'."
  (progn
    (setq c-basic-offset 2)))
(add-hook 'yang-mode-hook 'my-yang-mode-hook)

;; ############################################################################
(use-package erlang :ensure t)

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
(use-package cc-mode
  :config
  (setq c-basic-offset 4))

;; ############################################################################
;; Styling
 (use-package whitespace
   :config
   (set-face-attribute 'whitespace-tab nil :foreground "#fcfcfc")
   (set-face-attribute 'whitespace-line nil :background "#eeeeee"))
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
  (set-face-attribute 'diff-added nil :foreground "#000000" :background "#99cc99")
  (set-face-attribute 'diff-removed nil :foreground "#000000" :background "#d07c7c"))

;; -----------------------------------------------------------------------------
;; -----------------------------------------------------------------------------
;; -----------------------------------------------------------------------------
;;  A hash table mapping from file names to stacks of vc-annotate calls
(defvar vc-annotate-call-stacks (make-hash-table :test 'equal))

;; Define a structure type to store the details needed to redisplay a revision
;; rev: the revision annotated
;; point: the cursor point when the annotation was first displayed (perhaps this
;;        could be improved to be the last point moved to on the annotation)
(require 'cl-lib)
(cl-defstruct annotation-details rev point)

;; The vc-annotate-mode-hook can't be used because it is run before
;; the vc-annotate-parent-* variables are set.
;;
;; So instead use a advise function for vc-annotate, called before
;; vc-annotate, which stores the arguments on the appropriate stack.
(defun record-annotation-call (file rev &optional display-mode buf move-point-to vc-bk)
  (message "Recording annotation: file %S rev %S" file rev)
  (let ((annotation-stack (gethash file vc-annotate-call-stacks)))
    (push (make-annotation-details :rev rev
                                   :point move-point-to)
          annotation-stack)
    (puthash file annotation-stack vc-annotate-call-stacks)))

;; DISABLED!!!
;; (advice-add 'vc-annotate :before #'record-annotation-call)

(defun vc-annotate-previous-annotation ()
  "Go back to showing the annotation of the previous displayed annotation"
  (interactive)
  (when (not (equal major-mode 'vc-annotate-mode))
    (error "Can only be used in vc-annotate-mode"))
  (let ((annotation-stack (gethash vc-annotate-parent-file vc-annotate-call-stacks)))
       (when (< (length annotation-stack) 2)
         (error "No previous vc-annotate calls"))
       ;; The entry at the top of the stack is the current annotation.
       ;; So need to pop two entries to get the previous annotation.
       (let
           ((curr-annotation (pop annotation-stack))
            (prev-annotation (pop annotation-stack)))
         ;; Update the annotation-stack in the hash table after removing the entries.
         ;; The entry for the one we're returning to will be re-added by
         ;; the advise function for vc-annotate.
         (puthash vc-annotate-parent-file annotation-stack vc-annotate-call-stacks)

         (vc-annotate vc-annotate-parent-file
                      (annotation-details-rev prev-annotation)
                      vc-annotate-parent-display-mode
                      (current-buffer)
                      (annotation-details-point prev-annotation)
                      vc-annotate-backend))))


;; -----------------------------------------------------------------------------
(add-hook 'vc-annotate-mode-hook
  (lambda ()
   (local-set-key (kbd "b") 'vc-annotate-previous-annotation)))

;; Configure

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-comment-face ((t (:foreground "color-246" :slant italic))))
 '(line-number ((t (:inherit (shadow default) :foreground "grey"))))
 '(line-number-current-line ((t (:inherit line-number :foreground "black")))))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (yang-mode yaml-mode use-package sql-indent solarized-theme smex rustic rust-mode realgud-lldb plantuml-mode magit lux-mode lsp-ui ivy graphviz-dot-mode git-gutter flycheck erlang creamsody-theme company base16-theme ahungry-theme))))
