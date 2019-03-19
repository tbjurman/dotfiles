(require 'edts)
(use-package edts
  :ensure t)
(provide 'init-edts)


; (when (getenv "CONFD_DIR")
;   (setq exec-path (cons (concat (getenv "CONFD_DIR") "/../otp/installed/bin")
;                         exec-path)))
;
; (add-hook 'after-init-hook 'my-after-init-hook)
; (defun my-after-init-hook ()
;   (require 'edts-start))
