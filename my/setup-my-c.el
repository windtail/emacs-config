
(require 'setup-my-auto-install)

(auto-install '(ycmd company-ycmd flycheck-ycmd))

(require 'setup-my-prog-common)

(defun my-gdb-config ()
  (defvar gdb-many-windows t)
  (defvar gdb-show-main t)
  (define-key c-mode-map (kbd "<f5>") 'gdb))

(defun my-ycmd-config ()
  (let ((ycmd-root (file-truename "~/ycmd")))
    (if (getenv "YCMD_ROOT") (setq ycmd-root (getenv "YCMD_ROOT")))
    (require 'ycmd)
    (set-variable 'ycmd-server-command (list "python" "-u" (f-join ycmd-root "ycmd")))
    (set-variable 'ycmd-global-config (f-join ycmd-root "examples" ".ycm_extra_conf.py"))
    (require 'company-ycmd)
    (company-ycmd-setup)
    (require 'flycheck-ycmd)
    (flycheck-ycmd-setup)
    )  
  )

(defun my-on-c-mode ()
  (hs-minor-mode)  
  ;; gnu, k&r, bsd, whitesmith, stroustrup, ellemtel, linux, python, java, user
  (setq c-default-style "linux")
  (my-gdb-config)
  (my-ycmd-config)
  (ycmd-mode)
  (company-mode)
  (flycheck-mode)
  (yas-minor-mode 1)
  )

(add-hook 'c-mode-hook 'my-on-c-mode)
(add-hook 'c++-mode-hook 'my-on-c-mode)

(provide 'setup-my-c)
