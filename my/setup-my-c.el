
(require 'setup-my-auto-install)

(auto-install '(counsel-gtags xcscope company-c-headers dts-mode cmake-mode cmake-project))
(auto-download-contrib "https://raw.githubusercontent.com/y2q-actionman/Kconfig-Mode/master/makefile-mode-ext.el" "makefile-mode-ext.el")
(auto-download-contrib "https://raw.githubusercontent.com/y2q-actionman/Kconfig-Mode/master/kconfig-mode.el" "kconfig-mode.el")

(require 'setup-my-prog-common)

(defvar cross-compile (getenv "CROSS_COMPILE") "cross compile prefix, such as arm-linux-gnueabihf-")
(defvar cross-compile-arch (getenv "ARCH") "architecture for cross compiling")

(defun my-setup-cross-compile(toolchain-prefix arch)
  "Setup cross compile environment"
  (interactive (list
				(read-string "Prefix of toolchain: " "arm-linux-gnueabihf-")
				(read-string "Architecture of target: " "arm")))
  (setq-default cross-compile toolchain-prefix)
  (setenv "CROSS_COMPILE" cross-compile)
  (setq-default cross-compile-arch arch)
  (setenv "ARCH" cross-compile-arch))

(defvar saved-cross-compile nil "`cross-compile' before `my-save-cross-compile'")
(defvar saved-cross-compile-arch nil "`cross-compile-arch' before `my-save-cross-compile'")

(defun my-save-cross-compile ()
  (setq saved-cross-compile cross-compile)
  (setq saved-cross-compile-arch cross-compile-arch))

(defun my-restore-cross-compile ()
  (setq cross-compile saved-cross-compile)
  (setq cross-compile-arch saved-cross-compile-arch))

;; -----------------------------------------
;; kbuild, makefile, cmake

(load "kconfig-mode")
(load "makefile-mode-ext")
(add-to-list 'auto-mode-alist '("[Mm]akefile\\..*" . makefile-gmake-mode)) ; like Makefile.host
(add-to-list 'auto-mode-alist '("/Kbuild\\..*" . makefile-gmake-mode))     ; like Kbuild.include

(require 'cmake-mode)
(add-hook 'cmake-mode-hook (lambda ()
                             (local-set-key (kbd "<f1> d") 'cmake-help)
                             (local-set-key (kbd "C-c C-d") 'cmake-help)))

(require 'cmake-project)
(defun maybe-cmake-project-hook ()
  (if (file-exists-p "CMakeLists.txt") (cmake-project-mode)))
(add-hook 'c-mode-hook 'maybe-cmake-project-hook)
(add-hook 'c++-mode-hook 'maybe-cmake-project-hook)

(projectile-register-project-type 'cmake '("CMakeLists.txt")
                                  :compile "cmake --build build"
                                  :configure "mkdir -p build && cd build && cmake ..")

(defun my-cmake-configure (source-directory build-directory &optional flags)
  (interactive "Dsource directory:\nDbuild directory:\nMadditional flags:")
  (make-directory build-directory t)
  (let ((default-directory build-directory))
      (compilation-start
       (concat
        "cd ."
        " && cd " (shell-quote-argument (expand-file-name build-directory))
        " && cmake " flags (shell-quote-argument (expand-file-name source-directory))))
      (setq compile-command (concat "cmake --build " (expand-file-name build-directory)))))

(defun my-projectile-cmake-configure (build-directory &optional flags)
  (interactive "Dbuild directory:\nMadditional flags:")
  (my-cmake-configure (projectile-project-root) build-directory flags))

;; ------------------------------------------------------------
;; gdb debug configurations

(setq-default gdb-many-windows t gdb-show-main t)

(defvar my-gdb-debug-executable nil "executable used for gdb")
(defvar my-gdb-debug-host "localhost" "gdbserver host")
(defvar my-gdb-debug-port "2345" "gdbserver port")

(defun my-gdb-start (executable &optional host port)
  (if (file-directory-p executable)
      (message (concat executable "is a directory, file expected"))
    (gdb (concat cross-compile
                 "gdb -i=mi "
                 (if host (format "-ex target remote %s:%s " host port))
                 executable))
	(setq my-gdb-debug-executable executable)
	(if host (setq my-gdb-debug-host host))
	(if port (setq my-gdb-debug-port port))))

(defun my-gdb (executable)
  (interactive (list (if (or current-prefix-arg (null my-gdb-debug-executable))
                         (read-file-name "executable to debug: ")
                       my-gdb-debug-executable)))
  (my-gdb-start (expand-file-name executable)))

(defun my-gdb-remote (executable &optional host port)
  (interactive (list (if (or current-prefix-arg (null my-gdb-debug-executable))
                         (read-file-name "executable to debug: ")
                       my-gdb-debug-executable)
                     (if current-prefix-arg
                         (read-minibuffer "host gdbserver is running: " "localhost")
                       "localhost")
                     (if current-prefix-arg
                         (read-minibuffer "port gdbserver is listening: " "2345")
                       "2345")))
  (my-gdb-start ((expand-file-name executable) host port)))

;; ----------------------------------------------------
;; symbol and semantic

;; Don't ask before rereading the TAGS files if they have changed
(setq tags-revert-without-query t)

;; Don't warn when TAGS files are large
(setq large-file-warning-threshold nil)

(cscope-setup)

(defvar my-projectile-cscope-project-root nil
  "Ensure running `cscope-set-initial-directory' only once for a project")

(defun my-projectile-cscope-set-initial-directory (cs-id)
  "set cscope initial directory for a project only once"
  (when (projectile-project-p)
    (unless (string= my-projectile-cscope-project-root (projectile-project-root)))
      (setq my-projectile-cscope-project-root (projectile-project-root))
      (cscope-set-initial-directory cs-id)))

;; set default cscope initial directory to project root
;; only if cscope initial directory not set in .project-setup.el
(add-hook 'projectile-after-switch-project-hook
          #'(lambda ()
			(my-projectile-cscope-set-initial-directory (projectile-project-root)))
		  t)

;; set initial directory starting from old one
(defun my-cscope-set-initial-directory (cs-id)
  (interactive (list
                (read-directory-name "cscope initial directory: "
                                     (or cscope-initial-directory default-directory))))
  (cscope-set-initial-directory cs-id))
(define-key cscope-minor-mode-keymap (kbd "C-c s a") 'my-cscope-set-initial-directory)

(defun my-semantic-try-add-system-include (base-dir rel-dir)
  (let ((inc-dir (concat (file-name-as-directory base-dir) rel-dir)))
    (if (file-directory-p inc-dir)
        (semantic-add-system-include inc-dir 'c-mode))))

(defun my-semantic-gcc-setup (triplet)
  (interactive "sTriplet: ")
  (let* ((gcc (concat triplet "gcc"))
         (gcc-sysroot (s-trim (shell-command-to-string (concat gcc " -print-sysroot"))))
         (gcc-inc (concat (file-name-as-directory gcc-sysroot) "usr/include")))
    (semantic-add-system-include gcc-inc 'c-mode)))

(defun my-projectile-setup-arm-embedded (project-root &optional build-root project-incs build-incs)
  "setup arm embedded c project

incs are all relative to project-root or build-root"
  (let* ((build-root (or build-root project-root))
        (triplet "arm-linux-gnueabihf-")
        (arch "arm")
        (p-incs (append (list "include" (format "arch/%s/include" arch)) project-incs))
        (b-incs (append (list "include" "include/generated") build-incs)))
    (my-save-cross-compile)
    (my-setup-cross-compile triplet arch)
	(my-projectile-cscope-set-initial-directory build-root)
	(semantic-reset-system-include 'c-mode)
    (dolist (inc p-incs) (my-semantic-try-add-system-include project-root inc))
    (dolist (inc b-incs) (my-semantic-try-add-system-include build-root inc))
    (my-semantic-gcc-setup triplet)))

(defun my-projectile-teardown-arm-embedded ()
  (my-restore-cross-compile)
  (semantic-reset-system-include 'c-mode)
  (semantic-gcc-setup))

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
  (define-key c-mode-map (kbd "<f5> g") 'my-gdb)
  (define-key c-mode-map (kbd "<f5> r") 'my-gdb-remote))

(add-hook 'c-mode-hook 'my-on-c-mode)
(add-hook 'c++-mode-hook 'my-on-c-mode)

(provide 'setup-my-c)
