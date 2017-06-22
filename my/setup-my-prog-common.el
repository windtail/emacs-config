
(require 'setup-my-auto-install)

(auto-install '(yasnippet hungry-delete clean-aindent-mode smartparens
                          dtrt-indent ws-butler expand-region iedit
                          realgud))

(require 'smartparens-config)

(smartparens-global-mode)

(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

(global-set-key (kbd "M-s e") 'iedit-mode)

(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "<f7>") (lambda ()
                               (interactive)
                               (setq-default compilation-read-command nil)
                               (call-interactively 'compile)))
(defun my-on-prog-mode ()
  (clean-aindent-mode 1)
  (dtrt-indent-mode 1)
  (setq dtrt-indent-verbosity 0)
  (ws-butler-mode)
  (setq truncate-lines t))

(add-hook 'prog-mode-hook 'my-on-prog-mode)

(provide 'setup-my-prog-common)

