
(require 'setup-my-auto-install)

(auto-install '(better-defaults
                material-theme
                workgroups2
                volatile-highlights
                company
                window-numbering
                helm
                swiper
                counsel
                magit))

(setq inhibit-startup-message t)
(global-auto-revert-mode)
(global-hl-line-mode)
(setq mode-require-final-newline t)
(setq-default tab-width 4)
(global-linum-mode t)
(set-language-environment "utf-8")      ; default utf-8 encoding
(setq ibuffer-use-other-window t)
(window-numbering-mode t)

(global-company-mode t)
(setq-default company-idle-begin 0.1)
(setq-default company-minimum-prefix-length 2)

(global-set-key (kbd "C-c w") 'whitespace-mode)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x C-r") 'recentf-open-files)

;; theme
(load-theme 'material-light t)

(require 'helm-config)

(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
(global-set-key (kbd "C-s") 'swiper)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "<f6>") 'ivy-resume)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-c k") 'counsel-ag)
(global-set-key (kbd "C-x l") 'counsel-locate)

(workgroups-mode 1)
(global-set-key (kbd "s-z") 'wg-switch-to-workgroup)
(global-set-key (kbd "s-/") 'wg-switch-to-previous-workgroup)

(global-set-key (kbd "s-x") 'clipboard-kill-region)
(global-set-key (kbd "s-c") 'clipboard-kill-ring-save)
(global-set-key (kbd "s-v") 'clipboard-yank)

(volatile-highlights-mode t)

(provide 'setup-my-better-defaults)
