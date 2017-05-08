
(require 'setup-my-auto-install)

(auto-install '(counsel-gtags))

(require 'setup-my-prog-common)

(defun my-on-c-mode ()
  (counsel-gtags-mode)
  (hs-minor-mode)
  (setq company-backend '((company-dabbrev-code company-gtags)))
  ;; gnu, k&r, bsd, whitesmith, stroustrup, ellemtel, linux, python, java, user
  (setq c-default-style "linux")
  (defvar gdb-many-windows t)
  (defvar gdb-show-main t)
  (define-key c-mode-map (kbd "<f5>") 'gdb)
  )

(add-hook 'c-mode-hook 'my-on-c-mode)

(if (eq system-type 'windows-nt)
  nil
  (setenv "GTAGSLIBPATH" (concat "/usr/include" ":" "/usr/local/include"))
  (setenv "MAKEOBJDIRPREFIX" (file-truename "~/.gtags"))
  )

(with-eval-after-load 'counsel-gtags
  (define-key counsel-gtags-mode-map (kbd "M-t") 'counsel-gtags-find-definition)
  (define-key counsel-gtags-mode-map (kbd "M-r") 'counsel-gtags-find-reference)
  (define-key counsel-gtags-mode-map (kbd "M-s") 'counsel-gtags-find-symbol)
  (define-key counsel-gtags-mode-map (kbd "M-,") 'counsel-gtags-go-backward))

(provide 'setup-my-c)
