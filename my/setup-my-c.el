
(require 'setup-my-auto-install)

(auto-install '(counsel-gtags xcscope company-c-headers dts-mode cmake-mode cmake-project))
(auto-download-contrib "https://raw.githubusercontent.com/y2q-actionman/Kconfig-Mode/master/makefile-mode-ext.el" "makefile-mode-ext.el")
(auto-download-contrib "https://raw.githubusercontent.com/y2q-actionman/Kconfig-Mode/master/kconfig-mode.el" "kconfig-mode.el")

(require 'setup-my-prog-common)

;; setup system include path if not set
(setq my-cc-include-dirs
      (if (getenv "CROSS_COMPILE")
          (list (concat (shell-command-to-string "${CROSS_COMPILE}gcc -print-sysroot") "/usr/include"))
        (if (eq system-type 'windows-nt)
            '("c:/msys64/mingw64/include" "c:/msys64/mingw64/x86_64-w64-mingw32/include")
          '("/usr/include" "/usr/local/include"))))
(setq my-system-path-env-sep (if (eq system-type 'windows-nt) ";" ":"))

(if (getenv "GTAGSLIBPATH")
    nil
  (setenv "GTAGSLIBPATH" (mapconcat 'identity my-cc-include-dirs my-system-path-env-sep)))

(load "kconfig-mode")
(load "makefile-mode-ext")
(add-to-list 'auto-mode-alist '("[Mm]akefile\\..*" . makefile-gmake-mode)) ; like Makefile.host
(add-to-list 'auto-mode-alist '("/Kbuild\\..*" . makefile-gmake-mode))     ; like Kbuild.include

(require 'cmake-mode)
(require 'cmake-project)

(defun maybe-cmake-project-hook ()
  (if (file-exists-p "CMakeLists.txt") (cmake-project-mode)))
(add-hook 'c-mode-hook 'maybe-cmake-project-hook)
(add-hook 'c++-mode-hook 'maybe-cmake-project-hook)

(defun my-gdb-config ()
  (defvar gdb-many-windows t)
  (defvar gdb-show-main t)
  (define-key c-mode-map (kbd "<f5>") 'gdb))

(defun my-cscope-root-set-p (dir-path)
  (if (file-exists-p (concat dir-path "cscope.files"))
      (or (cscope-set-initial-directory dir-path) t)
    nil))

(defun my-cscope-root-try (subfiles)
  (or (null subfiles)
      (let ((f (car subfiles)))
        (and (file-directory-p f) (my-cscope-root-set-p (concat f "/"))))
      (my-cscope-root-try (cdr subfiles))))

(defun my-projectile-auto-cscope-init-dirs ()
  "Automatically find cscope database file in project root or xxx-build directories.
xxx-build directories are for u-boot and linux in-source separate build directories"
  (interactive)
  (if (projectile-project-p) (let ((proj-root (projectile-project-root)))
    (or (my-cscope-root-set-p proj-root)
        (my-cscope-root-try (directory-files proj-root t ".*build$" t))))))

(defun linux-c-indent ()
  "adjusted defaults for C/C++ mode use"
  (interactive)
  (setq tab-width 8)
  (setq indent-tabs-mode t)
  (setq c-basic-offset 8)
  (setq c-set-style "linux"))

(defun iwb ()
  "indent whole buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))

(defun my-on-c-mode ()
  (hs-minor-mode)
  (my-gdb-config)
  (linux-c-indent)

  ;; Don't ask before rereading the TAGS files if they have changed
  (setq tags-revert-without-query t)
  ;; Don't warn when TAGS files are large
  (setq large-file-warning-threshold nil)

  (company-mode)
  (setq company-backends '(company-c-headers company-gtags company-dabbrev-code company-keywords))
  (setq company-c-headers-path-system my-cc-include-dirs)

  (local-set-key (kbd "<f1> d") 'man)
  (local-set-key (kbd "C-c s M") 'my-projectile-auto-cscope-init-dirs))

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
  (define-key counsel-gtags-mode-map (kbd "M-.") 'counsel-gtags-dwim))

(provide 'setup-my-c)
