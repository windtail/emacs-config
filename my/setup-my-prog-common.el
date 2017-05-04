
(require 'setup-my-auto-install)

(auto-install '(yasnippet clean-aindent-mode smartparens))

(add-hook 'prog-mode-hook 'clean-aindent-mode)

(require 'smartparens-config)
(add-hook 'prog-mode-hook 'smartparens-mode)

(add-hook 'prog-mode-hook 'yas-minor-mode)

(provide 'setup-my-prog-common)

