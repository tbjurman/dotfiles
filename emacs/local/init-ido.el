(require 'ido)
(use-package ido
  :config
  (setq ido-enable-flex-matching t)
  (setq ido-everywhere t)
  (setq confirm-nonexistent-file-or-buffer nil)
  (setq ido-create-new-buffer 'always)
  (ido-mode 1))
(provide 'init-ido)
