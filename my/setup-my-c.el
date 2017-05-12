
(require 'setup-my-auto-install)

(auto-install '(ycmd company-ycmd flycheck-ycmd))

(require 'setup-my-prog-common)

(defun my-gdb-config ()
  (defvar gdb-many-windows t)
  (defvar gdb-show-main t)
  (define-key c-mode-map (kbd "<f5>") 'gdb))

(defun my-on-c-mode ()
  (hs-minor-mode)
  (setq company-backend '((company-dabbrev-code company-ycmd)))
  ;; gnu, k&r, bsd, whitesmith, stroustrup, ellemtel, linux, python, java, user
  (setq c-default-style "linux")
  (my-gdb-config)
  (yas-minor-mode 1)
  )

(add-hook 'c-mode-hook 'my-on-c-mode)
(add-hook 'c++-mode-hook 'my-on-c-mode)

(provide 'setup-my-c)
