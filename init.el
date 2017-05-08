
(add-to-list 'load-path "~/.emacs.d/my/")

(require 'setup-my-better-defaults)

(require 'setup-my-hack-emacs)

(when (eq system-type 'windows-nt)
  (require 'setup-my-windows-nt))

(require 'setup-my-c)
(require 'setup-my-python)
(require 'setup-my-org)
