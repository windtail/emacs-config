(require 'setup-my-c)

(defun cross-compile--get-sysroot (gcc-prefix)
  (s-trim
   (shell-command-to-string (concat gcc-prefix "gcc" " -print-sysroot"))))

(defvar cross-compile-prefix nil "cross compile prefix, such as arm-linux-gnueabihf-")
(defvar cross-compile-arch nil "architecture for cross compiling")
(defvar cross-compile-sysroot nil "sysroot for cross compiling")

(defun cross-compile-setup(toolchain-prefix arch &optional overwrite-env)
  "Setup cross compile environment"
  (interactive (list
				(read-string "Prefix of toolchain: " (or cross-compile-prefix "arm-linux-gnueabihf-"))
				(read-string "Architecture of target: " (or cross-compile-arch "arm"))))
  (setq-default cross-compile-prefix toolchain-prefix)
  (setq-default cross-compile-arch arch)
  (setq-default cross-compile-sysroot (cross-compile--get-sysroot toolchain-prefix))
  (when overwrite-env
	(setenv "CROSS_COMPILE" cross-compile-prefix)
	(setenv "ARCH" cross-compile-arch)))

(defun cross-compile-reset ()
  (setq-default cross-compile-prefix (getenv "CROSS_COMPILE"))
  (setq-default cross-compile-arch (getenv "ARCH"))
  (setq-default cross-compile-sysroot (cross-compile--get-sysroot cross-compile-prefix)))

(defun cross-compile-gdb ()
  (interactive)
  (gdb (concat "gdb-multiarch" " -i=mi ")))

;; reset on load first time
(cross-compile-reset)

(defun cross-compile--add-include-relatively (base-directory relative-directory)
  (let ((include-directory (concat (file-name-as-directory base-directory) relative-directory)))
	(semantic-add-system-include include-directory 'c-mode)
	(semantic-add-system-include include-directory 'c++-mode)))

(defun cross-compile--clear-includes ()
  (semantic-reset-system-include 'c-mode)
  (semantic-reset-system-include 'c++-mode))

(defun cross-compile-setup-baremetal-project (project-directory &optional build-directory
																project-relative-include-directories
																build-relative-include-directories)
  (let ((build-directory (or build-directory project-directory)))
	(cscope-set-initial-directory build-directory)
	(cross-compile--clear-includes)
	(semantic-add-system-include (f-join cross-compile-sysroot "usr/include") 'c-mode)
	(dolist (inc project-relative-include-directories)
	  (cross-compile--add-include-relatively project-directory inc))
	(dolist (inc build-relative-include-directories)
	  (cross-compile--add-include-relatively build-directory inc))))

(defun cross-compile-teardown-baremetal-project ()
  (cross-compile--clear-includes)
  (semantic-gcc-setup))

(defun cross-compile-setup-arm-linux (project-directory &optional build-directory)
  (cross-compile-setup "arm-linux-gnueabihf-" "arm")
  (cross-compile-setup-baremetal-project project-directory
										 build-directory
										 '("include" "arch/arm/include" "arch/arm/include/uapi")
										 '("include" "include/uapi" "include/generated" "include/generated/uapi" "arch/arm/include/generated" "arch/arm/include/generated/uapi")))

(defun cross-compile-teardown-arm-linux ()
  (cross-compile-reset)
  (cross-compile-teardown-baremetal-project))

(defun cross-compile-setup-arm-u-boot (project-directory &optional build-directory)
  (cross-compile-setup "arm-linux-gnueabihf-" "arm")
  (cross-compile-setup-baremetal-project project-directory
										 build-directory
										 '("include" "arch/arm/include")
										 '("include" "include/generated" "arch/arm/include/generated")))
(defun cross-compile-teardown-arm-u-boot ()
  (cross-compile-reset)
  (cross-compile-teardown-baremetal-project))

(provide 'setup-my-cross-compile)
