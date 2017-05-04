
(add-to-list 'load-path "~/.emacs.d/my/")

(require 'setup-my-better-defaults)

(require 'setup-my-hack-emacs)

(when (eq system-type 'windows-nt)
  (require 'setup-my-windows-nt))

(require 'setup-my-c)
(require 'setup-my-python)
(require 'setup-my-org)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("5dc0ae2d193460de979a463b907b4b2c6d2c9c4657b2e9e66b8898d2592e3de5" default)))
 '(package-selected-packages
   (quote
    (clean-aindent-mode workgroups2 material-theme org ggtags helm helm-git magit window-numbering better-defaults ein elpy flycheck monokai-theme py-autopep8))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
