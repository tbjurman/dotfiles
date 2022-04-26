;; A collection of useful tools.

(provide 'tailf)

(defun tailf-try-find-top-dir ()
  "Try to find the top directory of a core/tailf working copy
of current buffer. The function first looks at the 'W' environment
variable and if that is unset it starts traversing the directory
tree upwards until 'LICENSE.confdc' is found.

Returns the found directory or nil."
  (let ((top-dir
         (or (getenv "W")
             (when buffer-file-name (file-name-directory buffer-file-name))))
        (tmp))
    (while (and top-dir (not (directory-files top-dir nil "LICENSE.confdc")))
      (setq tmp (cdr (reverse(split-string top-dir "/"))))
      (when tmp (setq tmp (string-join (reverse tmp) "/")))
      (setq top-dir tmp))
    top-dir))

;; NB. requires erlang-mode
(defun tailf-xref-using-fun ()
  "Runs 'make -C $W xref-using-MODULE,FUNCTION,ARITY' from the
source tree top directory with MODULE, FUNCTION and ARITY filled in
with information from point.

The result ends up in a buffer called *tailf-xref-using-fun*.

N.B. This function requires erlang-mode."
  (interactive)
  (save-excursion
    (erlang-end-of-function)
    (erlang-beginning-of-function)
    (let ((top-dir (tailf-try-find-top-dir))
          (module (erlang-get-module))
          (function (erlang-get-function-name))
          (arity (erlang-get-function-arity)))
      (if (and top-dir (and module (and function arity)))
          (let ((cmd (concat "make -C " top-dir " xref-using-"
                             module "," function ","
                             (number-to-string arity))))
            (let ((output (shell-command-to-string cmd))
                  (bufname "*tailf-xref-using-fun*"))
              (get-buffer-create bufname)
              (set-buffer bufname)
              (read-only-mode)
              (let ((inhibit-read-only t))
                (erase-buffer)
                (insert
                 (concat "* functions using " module ":" function "/"
                         (number-to-string arity) "\n\n"))
                (insert output))))
        nil))))
