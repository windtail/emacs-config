;;; init --- my personal emacs config
;;; Commentary:
;;; Code:

(setq inhibit-startup-screen t       ; do not display turtorial
      package-enable-at-startup nil  ; prevent accidentally loading packages twice
      initial-major-mode 'fundamental-mode) ; disable lisp-mode for *scatch*

;; load custome package archives or china mirror
(unless (load (concat user-emacs-directory "my-package-archives") t t t)
  (setq package-archives '(("gnu" . "http://elpa.emacs-china.org/gnu/")
                           ("melpa" . "http://elpa.emacs-china.org/melpa/"))))

(add-to-list 'load-path (concat user-emacs-directory "my/"))
(add-to-list 'load-path (concat user-emacs-directory "contrib/"))

(package-initialize)

(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(horizontal-scroll-bar-mode 0)

(setq auto-window-vscroll nil
      message-log-max 16384
      mode-require-final-newline t
      column-number-mode t
      gc-cons-threshold 1073741824 ; 1G
      gc-cons-percentage 0.6)

(when (eq system-type 'windows-nt)
  ;; use local encoding avoid chinese encoding problem on Windows
  (set-locale-environment)

  ;; create new file as utf-8 encoding
  (defun my/set-new-file-to-utf-8 ()
    (with-current-buffer (current-buffer)
      (setq buffer-file-coding-system 'utf-8))
    t)
  (add-to-list 'find-file-not-found-functions #'my/set-new-file-to-utf-8))

(setq-default indent-tabs-mode nil
              tab-width 4)
(defalias 'yes-or-no-p 'y-or-n-p)

(global-set-key (kbd "M-/") #'hippie-expand)
(global-set-key (kbd "C-x C-b") #'ibuffer)
(global-set-key (kbd "C-c w") #'whitespace-mode)

(global-set-key (kbd "C-s") #'isearch-forward-regexp)
(global-set-key (kbd "C-r") #'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") #'isearch-forward)
(global-set-key (kbd "C-M-r") #'isearch-backward)
(global-set-key (kbd "C-c C-f") #'find-file-at-point)

(setq save-interprogram-paste-before-kill t
      apropos-do-all t
      mouse-yank-at-point t
      require-final-newline t
      visible-bell t
      load-prefer-newer t
      ediff-window-setup-function 'ediff-setup-windows-plain
      save-place-file (concat user-emacs-directory "places")
      backup-directory-alist `(("." . ,(concat user-emacs-directory
                                               "backups"))))

;; make file executable if it contains a shebang comment
(add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)

(defun my/edit-init-file ()
  "Open init.el file for editing."
  (interactive)
  (find-file user-init-file))

(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;; install use-package automatically (use :ensure t for other package)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package)
  (require 'my-use-package-ensure-contrib))
(use-package bind-key)
(use-package diminish
  :ensure t)

(diminish 'abbrev-mode)

(use-package misc
  :bind ("M-z" . zap-up-to-char))

(use-package uniquify
  :init (setq uniquify-buffer-name-style 'forward))

(use-package saveplace
  :init (setq-default save-place t))

(use-package linum
  :defer 1
  :config (global-linum-mode t))

(use-package autorevert
  :defer 1
  :config (global-auto-revert-mode))

(use-package paren
  :defer 1
  :config (show-paren-mode 1))

(use-package hl-line
  :defer 2
  :config (global-hl-line-mode))

(use-package f
  :ensure t
  :defer t)

(use-package s
  :ensure t
  :defer t)

(use-package dash
  :ensure t
  :defer t)

(use-package hl-todo
  :ensure t
  :defer 2
  :config (global-hl-todo-mode))

(use-package volatile-highlights
  :ensure t
  :defer 2
  :diminish volatile-highlights-mode
  :config (volatile-highlights-mode t))

(use-package danneskjold-theme
  :ensure t
  :init
  (load-theme 'danneskjold t))

;; loading spaceline
(use-package spaceline
  :ensure t
  :defer t)

(use-package spaceline-config
  :commands (spaceline-emacs-theme spaceline-spacemacs-theme)
  :init
  (defun my/load-spaceline ()
    "Load spaceline (two slow to load automatically)."
    (interactive)
    (spaceline-emacs-theme))
  :config (setq spaceline-window-numbers-unicode t))
;; TODO custom spaceline!

(use-package spaceline-all-the-icons
  :ensure t
  :after (spaceline)
  :config (spaceline-all-the-icons-theme))

(use-package all-the-icons-dired
  :ensure t
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package company
  :ensure t
  :diminish company-mode
  :commands (company-complete company-complete-common)
  :config
  (if (eq system-type 'windows-nt)
      (setq-default company-idle-delay 0.05 ; 0 will cause company-clany error on windows
                    company-async-timeout 5) ; company-clang very slow on windows
    (setq-default company-idle-delay 0))
  (setq-default company-minimum-prefix-length 2)
  ;; remove unused backend
  (setq company-backends (delq #'company-bbdb company-backends))
  (setq company-backends (delq #'company-nxml company-backends))
  (setq company-backends (delq #'company-css company-backends))
  (setq company-backends (delq #'company-eclim company-backends))
  (setq company-backends (delq #'company-xcode company-backends))
  (setq company-backends (delq #'company-oddmuse company-backends))
  :hook (prog-mode . global-company-mode))

(use-package helpful
  :ensure t
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)
         ("C-h F" . helpful-function)
         ("C-h C" . helpful-command)
         ("C-h p" . helpful-at-point)))

(use-package which-key
  :ensure t
  :defer 2
  :diminish which-key-mode
  :config (which-key-mode))

(use-package ivy
  :ensure t
  :defer 2
  :diminish ivy-mode
  :bind ("C-x C-r" . ivy-resume)
  :config
  (setq ivy-use-virtual-buffers t
        enable-recursive-minibuffers t)
  (ivy-mode 1))

(use-package hydra
  :ensure t
  :defer t)

(use-package avy
  :ensure t
  :defer t)

(use-package ivy-hydra
  :ensure t
  :after (ivy))

(use-package swiper
  :ensure t
  :bind ("C-s" . swiper))

(use-package counsel
  :ensure t
  :bind (("M-x" . counsel-M-x)
         ("M-y" . counsel-yank-pop)
         ("C-x C-f" . counsel-find-file)
         ("C-x l" . counsel-locate)
         ("C-c i" . counsel-semantic-or-imenu)
         ("C-h l" . counsel-find-library)
         ("<C-tab>" . counsel-company)))

(use-package wgrep
  :ensure t
  :defer 2)

(use-package bm
  :ensure t
  :bind (("<f2>" . bm-toggle)
         ("C-<f2>" . bm-next)
         ("S-<f2>" . bm-previous)))

(use-package ace-jump-mode
  :ensure t
  :bind ("C-c SPC" . ace-jump-mode))

(use-package ace-window
  :ensure t
  :bind ("M-o" . ace-window)
  :config (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(use-package ace-link
  :ensure t
  :after (:any xref info compile helpful woman eww)
  :config (ace-link-setup-default))

(use-package projectile
  :ensure t
  :defer t
  :config
  (setq-default projectile-globally-ignored-files '("TAGS" "cscope.*"))
  (projectile-register-project-type 'cmake '("CMakeLists.txt"))
  (projectile-register-project-type 'kernel '("Kbuild" "Kconfig" "kernel" "mm" "init"))
  (projectile-register-project-type 'u-boot '("Kbuild" "Kconfig" "include/u-boot"))
  (projectile-register-project-type 'kernel-module '(".kernel-module"))
  (require 'f)
  (require 's)
  (require 'dash)
  (require 'compile)
  (defun my/projectile--kbuild-build-dir-p (dir)
    "Whether `DIR' is a build-dir directory."
    (f-exists-p (f-join dir ".config")))
  (defun my/projectile--kbuild-build-dirs (root)
    "All build-dir directories found at top directories of `ROOT'."
    (let ((build-dirs (f-directories root #'my/projectile--kbuild-build-dir-p)))
      (if (my/projectile--kbuild-build-dir-p root)
          (cons root build-dirs)
        build-dirs)))
  (defun my/projectile--kbuild-default-build-dir (root)
    "Default build-dir directory on switching project.

If no build-dir directory found, `ROOT' is returned."
    (let ((build-dirs (my/projectile--kbuild-build-dirs root)))
      (if (null build-dirs)
          root
        (car build-dirs))))
  (defun my/projectile--kbuild-finish-selecting-build-dir (source-dir build-dir &optional source-incs build-incs)
    (setq cscope-option-do-not-update-database t)
    (cscope-set-initial-directory build-dir)
    (let ((incs (-concat (--map (f-join source-dir it) source-incs)
                         (--map (f-join build-dir it) build-incs))))
      ;; includes for semantic
      (when (my/semantic-enabled?)
        (my/semantic-clear-cc-includes)
        (-map #'my/semantic-add-cc-include incs))
      ;; includes for company-c-headers
      (setq-default company-c-headers-path-system incs)))
  (defun my/projectile--kbuild-select-default-build-dir (source-dir &optional source-incs build-incs)
    (let ((build-dir (my/projectile--kbuild-default-build-dir source-dir)))
      (my/projectile--kbuild-finish-selecting-build-dir source-dir build-dir source-incs build-incs)))
  (defun my/projectile--kbuild-select-build-dir (source-dir &optional source-incs build-incs)
    (ivy-read "Select build directory: "
              (my/projectile--kbuild-build-dirs source-dir)
              :require-match t
              :action (lambda (build-dir)
                        (my/projectile--kbuild-finish-selecting-build-dir source-dir build-dir source-incs build-incs))
              :caller 'my/projectile--kbuild-select-build-dir))
  (defvar my/projectile--kernel-includes '(("include" "include/uapi" "arch/arm/include" "arch/arm/include/uapi")
                                           . ("include" "include/uapi" "include/generated" "include/generated/uapi" "arch/arm/include/generated" "arch/arm/include/generated/uapi"))
    "Linux kernel includes")
  (defvar my/projectile--u-boot-includes '(("include" "arch/arm/include")
                                           . ("include" "include/generated" "arch/arm/include/generated"))
    "U-boot includes")
  (defun my/projectile-kbuild-select-build-dir ()
    "Select build directory for kbuild project."
    (interactive)
    (let ((ptype (projectile-project-type))
          (source-dir (projectile-project-root)))
      (cond
       ((equal ptype 'kernel)
        (my/projectile--kbuild-select-build-dir source-dir (car my/projectile--kernel-includes)
                                                (cdr my/projectile--kernel-includes)))
       ((equal ptype 'u-boot)
        (my/projectile--kbuild-select-build-dir source-dir (car my/projectile--u-boot-includes)
                                                (cdr my/projectile--u-boot-includes)))
       (t (message "Not a Kbuild project")))))
  (defun my/projectile--kbuild-teardown ()
    (cscope-unset-initial-directory)
    (setq cscope-option-do-not-update-database nil)
    (if (my/semantic-enabled?)
        (my/semantic-reset-cc-includes))
    (setq company-c-headers-path-system '("/usr/include" "/usr/local/include")))
  (defun my/projectile--kernel-module-current-kernel-dirs ()
    (let* ((kver (s-trim (shell-command-to-string "uname -r")))
           (kmod (concat "/lib/modules/" kver "/")))
      (cons (concat kmod "source") (concat kmod "build"))))
  (defun my/projectile--kernel-module-auto-kernel-dirs (proj-root)
    (let ((maybe-build (f-join proj-root "build"))
          (maybe-source (f-join proj-root "source")))
      (if (and (f-symlink-p maybe-build) (f-directory-p maybe-build))
          (cons maybe-source maybe-build)
        (my/projectile--kernel-module-current-kernel-dirs))))
  (defun my/projectile--kernel-module-setup (proj-root)
    "Read kernel source and build directories from .kernel-module.

If .kernel-module is empty, current kernel is used, otherwise the format
is ('source dir' . 'build-dir')."
    (let* ((dot-kernel-module-path (f-join proj-root ".kernel-module"))
           (dot-kernel-module-text (s-trim (f-read-text dot-kernel-module-path)))
           (kernel-dirs (if (s-blank-p dot-kernel-module-text)
                            (my/projectile--kernel-module-auto-kernel-dirs proj-root)
                          (car (read-from-string dot-kernel-module-text))))
           (kernel-source-dir (car kernel-dirs))
           (kernel-build-dir (cdr kernel-dirs)))
      (my/projectile--kbuild-finish-selecting-build-dir kernel-source-dir kernel-build-dir
                                                        (car my/projectile--kernel-includes)
                                                        (cdr my/projectile--kernel-includes))
      (setq compile-command (concat "make " "KDIR=" kernel-build-dir))
      (message "Linux kernel source %s, build %s" kernel-source-dir kernel-build-dir)))
  (defun my/cmake--configs-path (proj-root)
    (f-join proj-root ".cmake-configs"))
  (defun my/cmake--configs (proj-root)
    (let ((dot-configs-path (my/cmake--configs-path proj-root)))
      (if (file-readable-p dot-configs-path)
          (let* ((dot-configs-text (s-trim (f-read-text dot-configs-path)))
                 (configs (if (s-blank-p dot-configs-text)
                              nil
                            (car (read-from-string dot-configs-text)))))
            configs)
        nil)))
  (defun my/cmake--set-configs (proj-root configs)
    (f-write-text (prin1-to-string configs) 'utf-8 (my/cmake--configs-path proj-root)))
  (defun my/cmake--clear-configs (proj-root)
    (my/cmake--set-configs proj-root '()))
  (defun my/cmake--config-name (cfg)
    (car cfg))
  (defun my/cmake--config-flags (cfg)
    (concat (cdr cfg) " -DCMAKE_EXPORT_COMPILE_COMMANDS=1"))
  (defun my/cmake--config-build-dir (proj-root cfg)
    (f-join proj-root (format "cmake-build-%s" (my/cmake--config-name cfg))))
  (defun my/cmake--config-command (proj-root cfg)
    (let ((build-dir (my/cmake--config-build-dir proj-root cfg))
          (flags (my/cmake--config-flags cfg)))
      (concat "mkdir -p " build-dir
              " && cd " build-dir
              " && cmake " flags " ..")))
  (defun my/cmake--config-build-command (proj-root cfg)
    (let ((build-dir (my/cmake--config-build-dir proj-root cfg)))
      (concat "cmake --build " build-dir)))
  (defun my/cmake--select-config (proj-root cfg)
    (shell-command (my/cmake--config-command proj-root cfg))
    (setq compile-command (my/cmake--config-build-command proj-root cfg)))
  (defun my/cmake--add-or-subs-config (proj-root cfg)
    (let* ((configs (my/cmake--configs proj-root))
           (new-configs (cons cfg (--select (not (equal name (car it))) configs))))
      (my/cmake--set-configs proj-root new-configs)))
  (defun my/cmake--rm-config-by-name (proj-root name)
    (let* ((configs (my/cmake--configs proj-root))
           (new-configs (--select (not (equal name (car it))) configs)))
      (my/cmake--set-configs proj-root new-configs)))
  (defun my/cmake-make-config (name flags)
    "Make a new or substitute a config."
    (interactive (list
                  (read-string "Name: ")
                  (read-string "Flags: " "-DCMAKE_BUILD_TYPE=Debug")))
    (when (equal (projectile-project-type) 'cmake)
      (let ((proot (projectile-project-root))
            (cfg (cons name flags)))
        (my/cmake--add-or-subs-config proot cfg)
        ;; delete build directory because the flags may be changed
        (let ((build-dir (my/cmake--config-build-dir proot cfg)))
          (when (file-directory-p build-dir)
            (delete-directory build-dir t)))
        (my/cmake--select-config proot cfg))))
  (defun my/cmake-remove-config ()
    "Remove a config"
    (interactive)
    (when (equal (projectile-project-type) 'cmake)
      (let* ((proot (projectile-project-root)))
        (ivy-read "Remove config: "
                  (--map (car it) (my/cmake--configs proot))
                  :require-match t
                  :action (lambda (name)
                            (my/cmake--rm-config-by-name proot name))
                  :caller 'my/cmake-remove-config))))
  (defun my/cmake-clear-config ()
    "Clear all configs"
    (interactive)
    (when (equal (projectile-project-type) 'cmake)
      (my/cmake--clear-configs (projectile-project-root))))
  (defun my/cmake-select-config ()
    "Select a config."
    (interactive)
    (when (equal (projectile-project-type) 'cmake)
      (let* ((proot (projectile-project-root))
             (configs (my/cmake--configs proot))
             (config-names (--map (car it) configs)))
        (ivy-read "Select config: "
                  config-names
                  :require-match t
                  :action (lambda (name)
                            (my/cmake--select-config proot (assoc name configs)))
                  :caller 'my/cmake-remove-config))))
  (defun my/cmake-open-config-file ()
    "Open config file for edit."
    (interactive)
    (when (equal (projectile-project-type) 'cmake)
      (find-file (my/cmake--configs-path (projectile-project-root)))))
  (defun my/cmake--project-setup (proj-root)
    (let ((configs (my/cmake--configs proj-root)))
      (unless (null configs)
        (my/cmake--select-config proj-root (car configs)))))
  (defun my/projectile-project-setup ()
    (let ((ptype (projectile-project-type))
          (proot (projectile-project-root)))
      (message "Entered a %s project" (symbol-name ptype))
      (cond
       ((equal ptype 'kernel)
        (message "Linux kernel found at %s" proot)
        (my/projectile--kbuild-select-default-build-dir proot (car my/projectile--kernel-includes) (cdr my/projectile--kernel-includes))
        )
       ((equal ptype 'u-boot)
        (message "U-boot found at %s" proot)
        (my/projectile--kbuild-select-default-build-dir proot (car my/projectile--u-boot-includes) (cdr my/projectile--u-boot-includes))
        )
       ((equal ptype 'kernel-module)
        (message "Kernel module found at %s" proot)
        (my/projectile--kernel-module-setup proot)
        )
       ((equal ptype 'cmake)
        (message "CMake project found at %s" proot)
        (my/cmake--project-setup proot)
        ))))
  (add-hook 'projectile-after-switch-project-hook #'my/projectile-project-setup)
  (defun my/projectile-project-teardown ()
    (let ((ptype (projectile-project-type)))
      (message "Leaving a %s project" (symbol-name ptype))
      (cond
       ((equal ptype 'kernel) (my/projectile--kbuild-teardown))
       ((equal ptype 'u-boot) (my/projectile--kbuild-teardown))
       ((equal ptype 'kernel-module) (my/projectile--kbuild-teardown))
       ((equal ptype 'cmake) nil))))
  (add-hook 'projectile-before-switch-project-hook #'my/projectile-project-teardown))

(use-package counsel-projectile
  :ensure t
  :bind-keymap ("C-c p" . counsel-projectile-command-map)
  :config (counsel-projectile-mode))

(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status)
  :config (setq magit-completing-read-function 'ivy-completing-read))

(use-package gitignore-mode
  :ensure t
  :mode "\\.gitignore\\'")

(use-package git-timemachine
  :ensure t
  :defer t)

(use-package multi-term
  :ensure t
  :defer t)

(use-package vlf
  :ensure t
  :defer t)

(use-package vlf-setup
  :commands (vlf))

;; programming
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package smartparens
  :ensure t
  :diminish smartparens-mode
  :hook (prog-mode . smartparens-mode)
  :bind (("M-p" . 'sp-forward-parallel-sexp)
         ("C-M-p" . 'sp-backward-parallel-sexp)))

(use-package smartparens-config
  :after (smartparens))

(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :after (company)
  :config (yas-global-mode))

(use-package yasnippet-snippets
  :ensure t
  :after (yasnippet))

(use-package company-yasnippet
  :after (yasnippet)
  :bind ("C-M-y" . company-yasnippet))

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

(use-package iedit
  :ensure t
  :bind ("C-;" . iedit-mode))
(put 'narrow-to-region 'disabled nil)

(use-package sr-speedbar
  :ensure t
  :bind ("<f6>" . sr-speedbar-toggle)
  :config (setq speedbar-show-unknown-files t))

(use-package aggressive-indent
  :ensure t
  :hook (emacs-lisp-mode . aggressive-indent-mode))

(use-package highlight-indentation
  :ensure t
  :commands (highlight-indentation-mode))

(use-package hungry-delete
  :ensure t
  :diminish hungry-delete-mode
  :hook (prog-mode . hungry-delete-mode))

(use-package tdd
  :ensure-contrib "https://raw.githubusercontent.com/jorgenschaefer/emacs-tdd/master/tdd.el"
  :commands (tdd-mode))

;; backtrace buffer: realgud:window-bt
;; enable short key debugging: use short-key mode (C-x C-a C-q)
;; clear mark: loc-change-clear-buffer
(use-package realgud
  :ensure t
  :commands (realgud:gdb realgud:pdb)
  :config
  (setq-default realgud:pdb-command-name "python -m pdb")
  (setq-default gdb-many-windows t
                gdb-show-main t))

(use-package my-indent-whole-buffer
  :commands (my/indent-whole-buffer)
  :init (defalias 'iwb 'my/indent-whole-buffer))

(use-package my-prog-mode-config
  :hook (prog-mode . my/prog-mode-config))

(use-package my-semantic
  :commands (my/semantic-enabled? my/semantic-enable my/semantic-disable my/semantic-reset-cc-includes my/semantic-add-cc-include))

(use-package srefactor
  :ensure t
  :after (my-enable-semantic)
  :bind (:map c-mode-base-map
              ("M-RET" . srefactor-refactor-at-point)))

(use-package flycheck
  :ensure t
  :diminish flycheck-mode
  :hook (prog-mode . flycheck-mode)
  :config
  (add-to-list 'display-buffer-alist
               `(,(rx bos "*Flycheck errors" eos)
                 (display-buffer-reuse-window display-buffer-in-side-window)
                 (side . bottom)
                 (reuseable-frames . visible)
                 (window-height . 0.23)))
  (setq flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list))

;; lua
(use-package lua-mode
  :ensure t
  :mode "\\.lua\\'")

;; python
(use-package elpy
  :ensure t
  :after (python)
  :config
  (elpy-enable)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (elpy-set-test-runner 'elpy-test-pytest-runner)
  (setq-default python-shell-interpreter "jupyter-console"
                python-shell-interpreter-args "--simple-prompt"))

(use-package py-autopep8
  :ensure t
  :hook (elpy-mode . py-autopep8-enable-on-save))

(use-package live-py-mode
  :ensure t
  :defer t)

(use-package hy-mode
  :ensure t
  :mode "\\.hy\\'")

(use-package ob-hy
  :ensure t
  :defer t)

(use-package org
  :ensure t
  :mode "\\.org\\'"
  :config
  (setq org-src-fontify-natively t)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((sh . t)
     (python . t)
     (dot . t)
     (octave . t)
     (sqlite . t)
     (C . t)))
  (setq org-latex-pdf-process '("xelatex -interaction nonstopmode -output-directory %o %f"
                                "xelatex -interaction nonstopmode -output-directory %o %f"
                                "xelatex -interaction nonstopmode -output-directory %o %f"))
  (defun my/org-pandoc-to-rst ()
    "Convert org to rst using pandoc."
    (interactive)
    (set (make-local-variable 'compile-command)
         (format "pandoc -o %s.rst %s"
                 (file-name-sans-extension (file-name-nondirectory (buffer-file-name)))
                 (file-name-nondirectory (buffer-file-name))))
    (call-interactively 'compile)))

(use-package my-rest
  :after (rst)
  :bind (:map rst-mode-map
              ("<f5> p" . my/rest-pandoc-to-pdf)
              ("<f5> d" . my/rest-pandoc-to-docx)
              ("<f5> f" . my/rest-rst2pdf-to-pdf)
              ("C-c C-p" . my/rest-restview-preview)))

;; device tree
(use-package dts-mode
  :ensure t
  :mode "\\.dtsi?\\'")

;; Kconfig
;; NOTE: full path is used for compare, '/' restrict to last component
(use-package kconfig-mode
  :ensure-contrib "https://raw.githubusercontent.com/delaanthonio/kconfig-mode/master/kconfig-mode.el"
  :mode "/Kconfig\\'")

;; Makefile.host Makefile.include Kbuild Kbuild.include
(add-to-list 'auto-mode-alist '("/Kbuild\\'\\|Kbuild\\.\\|[Mm]akefile\\." . makefile-gmake-mode))

;; CMakefileLists.txt
(use-package cmake-mode
  :ensure t
  :mode "/CMakeFileLists\\.txt\\'\\|\\.cmake\\'"
  :bind (:map cmake-mode-map
              ("<f1> d" . cmake-help)
              ("C-c C-d" . cmake-help)))

(use-package counsel-gtags
  :ensure t
  :init
  (setq tags-revert-without-query t
        large-file-warning-threshold nil)
  :bind (("C-c g d" . counsel-gtags-find-definition)
         ("C-c g r" . counsel-gtags-find-reference)
         ("C-c g s" . counsel-gtags-find-symbol)
         ("C-c g f" . counsel-gtags-find-file)
         ("C-c g ." . counsel-gtags-go-forward)
         ("C-c g ," . counsel-gtags-go-backward)
         ("C-c g m" . counsel-gtags-dwim)))

(use-package xcscope
  :ensure t
  :commands (cscope-unset-initial-directory cscope-set-initial-directory)
  :hook ((c-mode-common . cscope-minor-mode)
         (dired-mode . cscope-minor-mode))
  :config
  (defadvice cscope-set-initial-directory (around cscope-set-initial-directory-from-old activate)
    "set cscope initial directory starting from old one"
    (interactive (list
                  (read-directory-name "cscope initial directory: "
                                       (or cscope-initial-directory default-directory))))
    ad-do-it))

(use-package clang-format
  :ensure t
  :after (cc-mode)
  :config (setq-default clang-format-style "Google")
  :bind (:map c-mode-base-map
              ("C-c c f" . clang-format)))

;;; rtags system packages(rdm & rc) install commands
;;; - git clone --recursive https://github.com/Andersbakken/rtags.git
;;; - cd rtags
;;; - cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=1 .
;;; make
;;; make install
(use-package rtags
  :ensure t
  :hook ((c-mode-common . rtags-start-process-unless-running)
         (c-mode-common . rtags-enable-standard-keybindings))
  :bind (:map c-mode-base-map
              ("M-." . rtags-find-symbol-at-point)
              ("M-?" . rtags-find-references-at-point)
              ("M-," . rtags-location-stack-back)))

(use-package company-rtags
  :ensure t
  :after (rtags company)
  :init
  (setq rtags-autostart-diagnostics t
        rtags-completions-enabled t)
  :config
  (add-to-list 'company-backends 'company-rtags))

(use-package flycheck-rtags
  :ensure t
  :after (flycheck rtags)
  :config
  (defun my/flycheck-rtags-setup ()
    (flycheck-select-checker 'rtags)
    (setq-local flycheck-highlighting-mode nil) ;; RTags creates more accurate overlays.
    (setq-local flycheck-check-syntax-automatically nil))
  (add-hook 'c-mode-common-hook #'my/flycheck-rtags-setup t))

(use-package ivy-rtags
  :ensure t
  :after (ivy rtags)
  :config (setq rtags-display-result-backend 'ivy))

(use-package modern-cpp-font-lock
  :ensure t
  :hook (c++-mode . modern-c++-font-lock-mode))

(use-package company-c-headers
  :ensure t
  :after (company)
  :config
  (add-to-list 'company-backends 'company-c-headers)
  (defun company-c-headers-path-system ()
    (if (boundp 'semantic-c-dependency-system-include-path)
        semantic-c-dependency-system-include-path
      (symbol-value 'company-c-headers-path-system))))

(use-package hideshow
  :diminish hs-minor-mode
  :init
  (defun my/set-hideshow-binding ()
    (local-set-key (kbd "M-s h t") #'hs-toggle-hiding))
  :hook ((c-mode-common . hs-minor-mode)
         (c-mode-common . my/set-hideshow-binding)))

(use-package google-c-style
  :ensure t
  :commands (google-set-c-style google-make-newline-indent)
  :init
  (defun my/set-google-cc-style ()
    "Set google cc style"
    (interactive)
    (google-set-c-style)
    (google-make-newline-indent)))

(defun my/set-cc-style ()
  "Set my custom cc style."
  (interactive)
  (c-set-style "linux")
  (setq-local tab-width 8)
  (setq-local indent-tabs-mode nil)
  (setq-local c-basic-offset 8))

(defun my/set-cc-key ()
  "Set my custom cc key binding."
  (local-set-key (kbd "<f1> d") 'man)
  (local-set-key (kbd "C-c C-d") 'man))

(add-hook 'c-mode-common-hook #'my/set-cc-style)
(add-hook 'c-mode-common-hook #'my/set-cc-key)

(message "Emacs init time %s" (emacs-init-time))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (company-yasnippet diminish clang-format company-c-headers modern-cpp-font-lock ivy-rtags counsel-rtags flycheck-rtags company-rtags rtags hl-todo volatile-highlights dts-mode iedit ace-link spaceline-all-the-icons use-package spaceline danneskjold-theme))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(provide 'init)
;;; init ends here
