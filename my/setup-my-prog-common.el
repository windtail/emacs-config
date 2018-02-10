
(require 'setup-my-auto-install)

(auto-install '(yasnippet hungry-delete clean-aindent-mode smartparens
                          dtrt-indent ws-butler expand-region iedit
                          realgud jinja2-mode highlight-parentheses))
(auto-download-contrib "https://raw.githubusercontent.com/jorgenschaefer/emacs-tdd/master/tdd.el" "tdd.el")

(require 'smartparens-config)
(smartparens-global-mode)

(require 'highlight-parentheses)
(define-globalized-minor-mode global-highlight-parentheses-mode
  highlight-parentheses-mode
  (lambda ()
	(highlight-parentheses-mode t)))
(global-highlight-parentheses-mode t)

(yas-global-mode)

(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

(global-set-key (kbd "M-s e") 'iedit-mode)

(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "<f7>") (lambda ()
                               (interactive)
                               (setq-default compilation-read-command nil)
                               (call-interactively 'compile)))

(defun iwb ()
  "indent whole buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))

(defun my-on-prog-mode ()
  (clean-aindent-mode 1)
  (dtrt-indent-mode 1)
  (setq dtrt-indent-verbosity 0)
  (ws-butler-mode)
  (setq truncate-lines t)
  (setq speedbar-show-unknown-files t)
  (require 'tdd))

(add-hook 'prog-mode-hook 'my-on-prog-mode)

(provide 'setup-my-prog-common)

