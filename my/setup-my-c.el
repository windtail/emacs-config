
(require 'setup-my-auto-install)

(auto-install '(counsel-gtags xcscope company-c-headers dts-mode cmake-mode cpputils-cmake))
(auto-download-contrib "https://raw.githubusercontent.com/y2q-actionman/Kconfig-Mode/master/kconfig-mode.el" "kconfig-mode.el")

(require 'setup-my-prog-common)

;; -----------------------------------------
;; kbuild, makefile, cmake

(load "kconfig-mode")
(add-to-list 'auto-mode-alist '("[Mm]akefile\\..*" . makefile-gmake-mode)) ; like Makefile.host
(add-to-list 'auto-mode-alist '("/Kbuild\\'" . makefile-gmake-mode))
(add-to-list 'auto-mode-alist '("/Kbuild\\..*" . makefile-gmake-mode))     ; like Kbuild.include

(require 'cmake-mode)
(add-hook 'cmake-mode-hook (lambda ()
                             (local-set-key (kbd "<f1> d") 'cmake-help)
                             (local-set-key (kbd "C-c C-d") 'cmake-help)))

(projectile-register-project-type 'cmake '("CMakeLists.txt")
                                  :compile "cmake --build cmake-build-debug"
                                  :configure "mkdir -p cmake-build-debug && cd cmake-build-debug && cmake -DCMAKE_BUILD_TYPE=Debug ..")

;; ------------------------------------------------------------
;; gdb debug configurations

(setq-default gdb-many-windows t gdb-show-main t)

;; ----------------------------------------------------
;; symbol and semantic

;; Don't ask before rereading the TAGS files if they have changed
(setq tags-revert-without-query t)

;; Don't warn when TAGS files are large
(setq large-file-warning-threshold nil)

(cscope-setup)

(defadvice cscope-set-initial-directory (around cscope-set-initial-directory-from-old activate)
  "set cscope initial directory starting from old one"
  (interactive (list
                (read-directory-name "cscope initial directory: "
                                     (or cscope-initial-directory default-directory))))
  ad-do-it)

(add-hook 'projectile-before-switch-project-hook
          #'(lambda ()
              (cscope-unset-initial-directory)))

;; set default cscope initial directory to project root
;; only if cscope initial directory not set in .project-setup.el
(add-hook 'projectile-after-switch-project-hook
          #'(lambda ()
              (unless cscope-initial-directory
                (cscope-set-initial-directory (projectile-project-root))))
          t)

(global-set-key (kbd "C-c g d") 'counsel-gtags-find-definition)
(global-set-key (kbd "C-c g r") 'counsel-gtags-find-reference)
(global-set-key (kbd "C-c g s") 'counsel-gtags-find-symbol)
(global-set-key (kbd "C-c g f") 'counsel-gtags-find-file)
(global-set-key (kbd "C-c g .") 'counsel-gtags-go-forward)
(global-set-key (kbd "C-c g ,") 'counsel-gtags-go-backward)
(global-set-key (kbd "C-c g m") 'counsel-gtags-dwim)

;; -----------------------------------------------------

(defun linux-c-indent ()
  "adjusted defaults for C/C++ mode use"
  (interactive)
  (c-set-style "linux")
  (setq tab-width 4)
  (setq indent-tabs-mode nil)
  (setq c-basic-offset 4))

(defun company-c-headers-path-system ()
  (if (boundp 'semantic-c-dependency-system-include-path)
      semantic-c-dependency-system-include-path
    nil))

(defun my-on-c-mode ()
  (hs-minor-mode)
  (linux-c-indent)
  (add-to-list (make-local-variable 'company-backends) 'company-c-headers)
  (define-key c-mode-map (kbd "<f1> d") 'man)
  (define-key c-mode-map (kbd "<f5> g") 'gdb)
  (cppcm-reload-all))

(add-hook 'c-mode-hook 'my-on-c-mode)
(add-hook 'c++-mode-hook 'my-on-c-mode)

(provide 'setup-my-c)
