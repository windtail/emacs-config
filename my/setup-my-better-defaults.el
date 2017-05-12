
(require 'setup-my-auto-install)

(auto-install '(better-defaults
                material-theme
                workgroups2
                volatile-highlights
                company
                window-numbering
                projectile
                swiper
                counsel
                counsel-projectile
                magit))

(setq inhibit-startup-message t)
(global-auto-revert-mode)
(global-hl-line-mode)
(setq mode-require-final-newline t)
(setq-default tab-width 4)
(global-linum-mode t)
(setq column-number-mode t)
(setq ibuffer-use-other-window t)
(window-numbering-mode t)

;; 使用本地编码，避免Windows上各种中文问题
(set-locale-environment)

;; 创建文件时总是用utf-8编码，更于Windows和Linux互通
(defun my-set-new-file-to-utf-8 ()
  (with-current-buffer (current-buffer)
    (setq buffer-file-coding-system 'utf-8))
  t)

(add-to-list 'find-file-not-found-functions #'my-set-new-file-to-utf-8)

(global-company-mode t)
(setq-default company-idle-delay 0)
(setq-default company-minimum-prefix-length 2)

(global-set-key (kbd "C-c w") 'whitespace-mode)
(global-set-key (kbd "C-x g") 'magit-status)

;; theme
(load-theme 'material-light t)

;; 根目录创建一个 .projectile 文件即可被识别为工程根目录（另外，git根目录就自动会被识别）
(projectile-mode 1)

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

(counsel-projectile-on)

(workgroups-mode 1)
(global-set-key (kbd "s-z") 'wg-switch-to-workgroup)
(global-set-key (kbd "s-/") 'wg-switch-to-previous-workgroup)

(volatile-highlights-mode t)

(provide 'setup-my-better-defaults)
