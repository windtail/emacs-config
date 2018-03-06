
(require 'setup-my-auto-install)

(auto-install '(counsel-gtags xcscope company-c-headers dts-mode cmake-mode cmake-project))
(auto-download-contrib "https://raw.githubusercontent.com/y2q-actionman/Kconfig-Mode/master/makefile-mode-ext.el" "makefile-mode-ext.el")
(auto-download-contrib "https://raw.githubusercontent.com/y2q-actionman/Kconfig-Mode/master/kconfig-mode.el" "kconfig-mode.el")

(require 'setup-my-prog-common)

(defvar cross-compile nil "cross compile prefix, such as arm-linux-gnueabihf-")

;; -----------------------------------------
;; kbuild, makefile, cmake

(load "kconfig-mode")
(load "makefile-mode-ext")
(add-to-list 'auto-mode-alist '("[Mm]akefile\\..*" . makefile-gmake-mode)) ; like Makefile.host
(add-to-list 'auto-mode-alist '("/Kbuild\\..*" . makefile-gmake-mode))     ; like Kbuild.include

(require 'cmake-mode)
(add-hook 'cmake-mode-hook (lambda ()
                             (define-key c-mode-map (kbd "<f1> d") 'cmake-help)
                             (define-key c-mode-map (kbd "C-c C-d") 'cmake-help)))

(require 'cmake-project)
(defun maybe-cmake-project-hook ()
  (if (file-exists-p "CMakeLists.txt") (cmake-project-mode)))
(add-hook 'c-mode-hook 'maybe-cmake-project-hook)
(add-hook 'c++-mode-hook 'maybe-cmake-project-hook)

(projectile-register-project-type 'cmake '("CMakeLists.txt")
                                  :compile "cmake --build build"
                                  :configure "mkdir -p build && cd build && cmake ..")

;; ------------------------------------------------------------
;; gdb debug configurations

(setq-default gdb-many-windows t gdb-show-main t)

(defvar my-gdb-debug-executable nil "executable used for gdb")

(defun my-gdb (executable)
  (interactive (list (if (or current-prefix-arg (null my-gdb-debug-executable))
                         (read-file-name "executable to debug: ")
                       my-gdb-debug-executable)))
  (if (file-directory-p executable)
      (message (concat executable "is a directory, file expected"))
   (gdb (concat cross-compile "gdb -i=mi " executable))))

(defun my-gdb-remote (executable &optional host port)
  (interactive (list (if (or current-prefix-arg (null my-gdb-debug-executable))
                         (read-file-name "executable to debug: ")
                       my-gdb-debug-executable)
                     (if current-prefix-arg
                         (read-minibuffer "host gdbserver is running: " "localhost")
                       "localhost")
                     (if current-prefix-arg (read-minibuffer "port gdbserver is listening: " "2345")
                       "2345")))
  (if (file-directory-p executable)
      (message (concat executable "is a directory, file expected"))
    (gdb (concat cross-compile
                 "gdb -i=mi "
                 (format "-ex target remote %s:%s " host port)
                 executable))))

;; ----------------------------------------------------
;; symbol and semantic

(cscope-setup)

(defvar my-projectile-cscope-project-root nil
  "Ensure running `cscope-set-initial-directory' only once for a project")

(defun my-projectile-cscope-set-initial-directory (cs-id)
  "set cscope initial directory for a project only once"
  (when (projectile-project-p)
    (unless (equal my-projectile-cscope-project-root (projectile-project-root))
      (setq my-projectile-cscope-project-root (projectile-project-root))
      (cscope-set-initial-directory cs-id))))

;; set default cscope initial directory to project root
(add-hook 'projectile-after-switch-project-hook
          #'(lambda ()
              (my-projectile-cscope-set-initial-directory (projectile-project-root))) t)

;; set initial directory starting from old one
(defun my-cscope-set-initial-directory (cs-id)
  (interactive (list
                (read-directory-name "cscope initial directory: "
                                     (or cscope-initial-directory default-directory))))
  (cscope-set-initial-directory cs-id))
(define-key cscope-minor-mode-keymap (kbd "C-c s a") 'my-cscope-set-initial-directory)

(global-set-key (kbd "C-c g d") 'counsel-gtags-find-definition)
(global-set-key (kbd "C-c g r") 'counsel-gtags-find-reference)
(global-set-key (kbd "C-c g s") 'counsel-gtags-find-symbol)
(global-set-key (kbd "C-c g f") 'counsel-gtags-find-file)
(global-set-key (kbd "C-c g .") 'counsel-gtags-go-forward)
(global-set-key (kbd "C-c g ,") 'counsel-gtags-go-backward)
(global-set-key (kbd "C-c g w") 'counsel-gtags-dwim)

;; -----------------------------------------------------

(defun linux-c-indent ()
  "adjusted defaults for C/C++ mode use"
  (interactive)
  (c-set-style "linux")
  (setq tab-width 4)
  (setq indent-tabs-mode nil)
  (setq c-basic-offset 4))

;; Don't ask before rereading the TAGS files if they have changed
(setq tags-revert-without-query t)

;; Don't warn when TAGS files are large
(setq large-file-warning-threshold nil)

(defun company-c-headers-path-system ()
  (if (boundp 'semantic-c-dependency-system-include-path)
      semantic-c-dependency-system-include-path
    nil))

(defun my-on-c-mode ()
  (hs-minor-mode)
  (linux-c-indent)
  (add-to-list (make-local-variable 'company-backends) 'company-c-headers)
  (define-key c-mode-map (kbd "<f1> d") 'man)
  (define-key c-mode-map (kbd "<f5> g") 'my-gdb)
  (define-key c-mode-map (kbd "<f5> r") 'my-gdb-remote))

(add-hook 'c-mode-hook 'my-on-c-mode)
(add-hook 'c++-mode-hook 'my-on-c-mode)

(provide 'setup-my-c)
