
(require 'setup-my-auto-install)

(auto-install '(yasnippet yasnippet-snippets hungry-delete clean-aindent-mode smartparens
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

;; iedit should be company with narrow-to-defun/region
;; or mark a region in iedit-mode and exec iedit-mode again
(global-set-key (kbd "C-;") 'iedit-mode)
(put 'narrow-to-region 'disabled nil)

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
  (require 'tdd)
  (require 'semantic/sb)
  (add-to-list 'write-file-functions 'delete-trailing-whitespace))

(add-hook 'prog-mode-hook 'my-on-prog-mode)

(provide 'setup-my-prog-common)

