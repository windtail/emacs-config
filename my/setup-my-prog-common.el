
(require 'setup-my-auto-install)

(auto-install '(yasnippet yasnippet-snippets hungry-delete clean-aindent-mode smartparens
                          dtrt-indent ws-butler expand-region iedit
                          realgud jinja2-mode sr-speedbar srefactor
                          rainbow-delimiters highlight-indentation aggressive-indent
                          multi-compile))
(auto-download-contrib "https://raw.githubusercontent.com/jorgenschaefer/emacs-tdd/master/tdd.el" "tdd.el")

(require 'smartparens-config)
(smartparens-global-mode)

(yas-global-mode)

(setq semantic-default-submodes
      '(global-semanticdb-minor-mode
        global-semantic-idle-scheduler-mode
        global-semantic-idle-summary-mode
        global-semantic-decoration-mode
        global-semantic-highlight-func-mode
        global-semantic-stickyfunc-mode
        global-semantic-mru-bookmark-mode
        global-semantic-idle-local-symbol-highlight-mode
        global-semantic-highlight-edits-mode
        global-semantic-show-unmatched-syntax-mode))
(semantic-mode)
(global-set-key (kbd "<f12>") 'semantic-ia-fast-jump)

(defadvice semantic-add-system-include (around semantic-try-add-system-include activate)
  "add system include if path exists"
  (interactive "DNew Include Directory: ")
  (if (file-directory-p dir)
	  ad-do-it))

(global-set-key (kbd "<f6>") 'sr-speedbar-toggle)

(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;; iedit should be company with narrow-to-defun/region
;; or mark a region in iedit-mode and exec iedit-mode again
(global-set-key (kbd "C-;") 'iedit-mode)
(put 'narrow-to-region 'disabled nil)

(global-aggressive-indent-mode)
;; (add-to-list 'aggressive-indent-excluded-modes 'xxx-mode)

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
  (add-to-list 'write-file-functions 'delete-trailing-whitespace)
  (hungry-delete-mode)
  (rainbow-delimiters-mode)
  (aggressive-indent-mode)
  (highlight-indentation-mode))

(add-hook 'prog-mode-hook 'my-on-prog-mode)

(provide 'setup-my-prog-common)
