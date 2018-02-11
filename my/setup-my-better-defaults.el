
(require 'setup-my-auto-install)

(auto-install '(better-defaults
                material-theme
                workgroups2
                which-key
                volatile-highlights
                company
                window-numbering
                projectile
                swiper
                counsel
                counsel-projectile
                magit
                multi-term
                neotree))

(setq inhibit-startup-message t)
(menu-bar-mode 0)
(tool-bar-mode 0)
; (scroll-bar-mode 0)
(global-auto-revert-mode)
(global-hl-line-mode)
(setq mode-require-final-newline t)
(setq-default tab-width 4)
(global-linum-mode t)
(setq column-number-mode t)
(setq ibuffer-use-other-window t)
(window-numbering-mode t)

;; maximize initial frame
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;; 使用本地编码，避免Windows上各种中文问题
(set-locale-environment)

;; 创建文件时总是用utf-8编码，更于Windows和Linux互通
(defun my-set-new-file-to-utf-8 ()
  (with-current-buffer (current-buffer)
    (setq buffer-file-coding-system 'utf-8))
  t)

(add-to-list 'find-file-not-found-functions #'my-set-new-file-to-utf-8)

(global-company-mode t)
(setq-default company-idle-delay 0.05)  ;; 0 will cause company-clang error on windows-nt
(setq-default company-async-timeout 5)  ;; company-clang very slow on windows-nt
(setq-default company-minimum-prefix-length 2)

(global-set-key (kbd "C-c w") 'whitespace-mode)
(global-set-key (kbd "C-x g") 'magit-status)

;; theme
(load-theme 'material-light t)

;; 根目录创建一个 .projectile 文件即可被识别为工程根目录（另外，git根目录就自动会被识别）
(counsel-projectile-mode)
(setq-default projectile-globally-ignored-files '("TAGS" "cscope.*"))

(require 'neotree)
(setq neo-smart-open t)
(add-hook 'projectile-after-switch-project-hook 'neotree-projectile-action)

(global-set-key (kbd "<f8>") 'neotree-toggle)
(global-set-key (kbd "C-c p n") 'neotree-projectile-action)

;; turn off ido mode, cancel operation in better-defaults
(add-hook 'after-init-hook
          '(lambda ()
             (ido-mode -1))
          )

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
(global-set-key (kbd "C-c i") 'counsel-imenu)

;; (require 'workgroups2)
;; (global-set-key (kbd "s-z") 'wg-switch-to-workgroup)
;; (global-set-key (kbd "s-/") 'wg-switch-to-previous-workgroup)

(volatile-highlights-mode t)

(require 'which-key)
(which-key-mode)

(defalias 'yes-or-no-p 'y-or-n-p)

(provide 'setup-my-better-defaults)
