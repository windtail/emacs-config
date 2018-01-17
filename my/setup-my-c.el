
(require 'setup-my-auto-install)

(auto-install '(counsel-gtags xcscope company-c-headers))
(auto-download-contrib "https://raw.githubusercontent.com/y2q-actionman/Kconfig-Mode/master/makefile-mode-ext.el" "makefile-mode-ext.el")
(auto-download-contrib "https://raw.githubusercontent.com/y2q-actionman/Kconfig-Mode/master/kconfig-mode.el" "kconfig-mode.el")

(require 'setup-my-prog-common)

;; setup system include path if not set
(if (getenv "GTAGSLIBPATH") nil
  (if (eq system-type 'windows-nt)
      (setenv "GTAGSLIBPATH" "c:/msys64/mingw64/include;c:/msys64/mingw64/x86_64-w64-mingw32/include")
    (setenv "GTAGSLIBPATH" "/usr/include:/usr/local/include")
    )
  )

(load "kconfig-mode")
(load "makefile-mode-ext")
(add-to-list 'auto-mode-alist '("[Mm]akefile\\..*" . makefile-gmake-mode)) ;; like Makefile.host

(defun my-gdb-config ()
  (defvar gdb-many-windows t)
  (defvar gdb-show-main t)
  (require 'realgud)
  (define-key c-mode-map (kbd "<f5>") 'realgud:gdb))

(defun my-on-c-mode ()
  (hs-minor-mode)  
  ;; gnu, k&r, bsd, whitesmith, stroustrup, ellemtel, linux, python, java, user
  (setq c-default-style "linux")
  (company-mode)
  (setq company-backends '(company-c-headers company-gtags company-dabbrev-code company-keywords))
  (setq company-c-headers-path-system
        (split-string
         (getenv "GTAGSLIBPATH")
         (if (eq system-type 'windows-nt) ";" ":"))))

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

(provide 'setup-my-c)
