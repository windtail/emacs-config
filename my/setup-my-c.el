
(require 'setup-my-auto-install)

(auto-install '(xcscope company-c-headers dts-mode cmake-mode cmake-project))
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

(load "kconfig-mode")
(load "makefile-mode-ext")
(add-to-list 'auto-mode-alist '("[Mm]akefile\\..*" . makefile-gmake-mode)) ; like Makefile.host
(add-to-list 'auto-mode-alist '("/Kbuild\\..*" . makefile-gmake-mode))     ; like Kbuild.include

(require 'cmake-mode)

(defun my-cmake-help ()
  "CMake command help should be in rst-mode rather than cmake-mode"
  (interactive)
  (call-interactively 'cmake-help)
  (let ((buffer (get-buffer "*CMake Help*")))
    (if buffer (save-selected-window
      (select-window (display-buffer buffer 'not-this-window))
      (cmake-mode)
      (rst-mode)))))

(defun my-cmake-keys ()
  (local-set-key (kbd "<f1> d") 'my-cmake-help)
  (local-set-key (kbd "C-c C-d") 'my-cmake-help))

(add-hook 'cmake-mode-hook 'my-cmake-keys)

(require 'cmake-project)

(defun maybe-cmake-project-hook ()
  (if (file-exists-p "CMakeLists.txt") (cmake-project-mode)))
(add-hook 'c-mode-hook 'maybe-cmake-project-hook)
(add-hook 'c++-mode-hook 'maybe-cmake-project-hook)

(projectile-register-project-type 'cmake '("CMakeLists.txt")
                                  :compile "cmake --build build"
                                  :configure "mkdir -p build && cd build && cmake ..")

(setq-default gdb-many-windows t gdb-show-main t)

(defvar my-gdb--last-exe nil "Last executable path for my-gdb command")

(defun my-gdb--do (exepath)
  "GDB debug a executable.
If CROSS_COMPILE is detected, a initial gdb script EXEPATH-gdb.gdb will be created.
The script will connect to gdbserver at port 2331 which is default for jlink gdbserver.
Please add current directory to safe auto-load path or completed disable safe auto-load feature
by adding `set auto-load safe-path /' to ~/.gdbinit"
  (let* ((cross-compile (getenv "CROSS_COMPILE"))
         (cmd-line (concat cross-compile "gdb -i=mi " exepath))
         (obj-init (concat exepath "-gdb.gdb")))
    (if cross-compile
        (if (file-exists-p obj-init)
            nil
          (write-region "target remote localhost:2331\nload\n" nil obj-init)))
    (setq my-gdb--last-exe exepath)
    (gdb cmd-line)))

(defun my-start-gdb (exepath)
  "Choose a file to debug"
  (interactive "fexecutable to debug:")
  (my-gdb--do exepath))

(defun my-gdb (with-prefix)
  "Choose a file to debug or debug last chosen file"
  (interactive "P")
  (if (or with-prefix (null my-gdb--last-exe))
      (call-interactively 'my-start-gdb)
    (my-gdb--do my-gdb--last-exe)))

(defun linux-c-indent ()
  "adjusted defaults for C/C++ mode use"
  (interactive)
  (c-set-style "linux")
  (setq tab-width 4)
  (setq indent-tabs-mode nil)
  (setq c-basic-offset 4))

(defun my-on-c-mode ()
  (hs-minor-mode)

  (linux-c-indent)

  ;; Don't ask before rereading the TAGS files if they have changed
  (setq tags-revert-without-query t)
  ;; Don't warn when TAGS files are large
  (setq large-file-warning-threshold nil)

  (add-to-list (make-local-variable 'company-backends) 'company-c-headers)
  (setq company-c-headers-path-system my-cc-include-dirs)

  (local-set-key (kbd "<f1> d") 'man)
  (local-set-key (kbd "C-c s M") 'my-projectile-auto-cscope-init-dirs)
  (define-key c-mode-map (kbd "<f5>") 'my-gdb))

(add-hook 'c-mode-hook 'my-on-c-mode)
(add-hook 'c++-mode-hook 'my-on-c-mode)

(cscope-setup)

;; set default cscope initial directory to project root
(add-hook 'projectile-after-switch-project-hook
          #'(lambda ()
              (cscope-set-initial-directory (projectile-project-root))))

;; set initial directory starting from old one
(defun my-cscope-set-initial-directory (cs-id)
  (interactive (list
                (read-directory-name "cscope initial directory: "
                                     (or cscope-initial-directory default-directory))))
  (cscope-set-initial-directory cs-id))
(define-key cscope-minor-mode-keymap (kbd "C-c s a") 'my-cscope-set-initial-directory)

(provide 'setup-my-c)
