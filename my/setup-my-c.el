
(require 'setup-my-auto-install)

(auto-install '(counsel-gtags xcscope ycmd company-ycmd flycheck-ycmd))

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

(defun my-ycmd-enable ()
  (interactive)
  (my-ycmd-config)
  (ycmd-mode)
  (flycheck-mode)
  )

(defun my-on-c-mode ()
  (hs-minor-mode)  
  ;; gnu, k&r, bsd, whitesmith, stroustrup, ellemtel, linux, python, java, user
  (setq c-default-style "linux")
  (my-gdb-config)
  (company-mode)
  (if (eq system-type 'windows-nt) (setq company-backends '(company-gtags company-dabbrev-code company-keywords)))
  (yas-minor-mode 1)
  )

(add-hook 'c-mode-hook 'my-on-c-mode)
(add-hook 'c++-mode-hook 'my-on-c-mode)

(cscope-setup)
(add-hook 'c-mode-hook 'counsel-gtags-mode)
(add-hook 'c++-mode-hook 'counsel-gtags-mode)

(with-eval-after-load 'counsel-gtags
  (define-key counsel-gtags-mode-map (kbd "M-t") 'counsel-gtags-find-definition)
  (define-key counsel-gtags-mode-map (kbd "M-r") 'counsel-gtags-find-reference)
  (define-key counsel-gtags-mode-map (kbd "M-s") 'counsel-gtags-find-symbol)
  (define-key counsel-gtags-mode-map (kbd "M-,") 'counsel-gtags-go-backward)
  (define-key counsel-gtags-mode-map (kbd "M-.") 'counsel-gtags-dwim)
  )


(if (getenv "GTAGSLIBPATH") nil
  (if (eq system-type 'windows-nt)
      (setenv "GTAGSLIBPATH" "c:/msys64/mingw64/include;c:/msys64/mingw64/x86_64-w64-mingw32/include")
    (setenv "GTAGSLIBPATH" "/usr/include:/usr/local/include")
    )
  )

(provide 'setup-my-c)
