
(add-to-list 'load-path "~/.emacs.d/my/")

(require 'setup-my-better-defaults)

(require 'setup-my-hack-emacs)

(when (eq system-type 'windows-nt)
  (require 'setup-my-windows-nt))

(require 'setup-my-c)
(require 'setup-my-python)
(require 'setup-my-text)
(require 'setup-my-misc)

;; 放在最后面，以便在恢复时，所有之前的设置都已生效
(workgroups-mode 1)
