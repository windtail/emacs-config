
(require 'setup-my-auto-install)

(auto-install '(better-defaults
                danneskjold-theme
                zenburn-theme
                nimbus-theme
                leuven-theme
                which-key
                volatile-highlights
                company
                ace-window
                ace-link
                ace-jump-mode
                projectile
                ivy
                ivy-hydra
                avy
                swiper
                counsel
                wgrep
                counsel-projectile
                magit
                gitignore-mode
                git-timemachine
                multi-term
                bm
                helpful
                hl-todo
                highlight-thing
                spaceline
                spaceline-all-the-icons
                diminish))

(setq inhibit-startup-message t)
(menu-bar-mode 0)
;; (tool-bar-mode 0)
;; (scroll-bar-mode 0)
(global-auto-revert-mode)
(global-hl-line-mode)
(setq mode-require-final-newline t)
(setq-default tab-width 4)
(global-linum-mode t)
(setq column-number-mode t)
(setq ibuffer-use-other-window t)

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

;; whitespace can be insert when finding completes
(define-key company-mode-map (kbd "C-c c") 'counsel-company)

(global-set-key (kbd "C-c w") 'whitespace-mode)
(global-set-key (kbd "C-x g") 'magit-status)

;; theme
(load-theme 'danneskjold t)

(require 'spaceline-config)
(spaceline-emacs-theme)

(require 'spaceline-all-the-icons)
(spaceline-all-the-icons-theme)

;; (diminish 'company-mode)

;; 根目录创建一个 .projectile 文件即可被识别为工程根目录（另外，git根目录就自动会被识别）
(counsel-projectile-mode)
(setq-default projectile-globally-ignored-files '("TAGS" "cscope.*"))

(defun my-projectile-load-project-script (name)
  (if (projectile-project-p)
      (let ((script-path (concat (projectile-project-root) name)))
        (if (file-exists-p script-path)
            (load-file script-path)))))

(defun my-projectile-run-setup ()
  (interactive)
  (my-projectile-load-project-script ".project-setup.el"))
(defun my-projectile-run-teardown ()
  (interactive)
  (my-projectile-load-project-script ".project-teardown.el"))
(add-hook 'projectile-before-switch-project-hook 'my-projectile-run-teardown)
(add-hook 'projectile-after-switch-project-hook 'my-projectile-run-setup)

;; turn off ido mode, cancel operation in better-defaults
(add-hook 'after-init-hook
          '(lambda ()
             (ido-mode -1))
          )

(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
(global-set-key (kbd "C-s") 'swiper)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "M-y") 'counsel-yank-pop)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "C-x l") 'counsel-locate)
(global-set-key (kbd "C-c i") 'counsel-semantic-or-imenu)

(global-set-key (kbd "C-h f") #'helpful-callable)
(global-set-key (kbd "C-h v") #'helpful-variable)
(global-set-key (kbd "C-h k") #'helpful-key)
(global-set-key (kbd "C-h F") #'helpful-function)
(global-set-key (kbd "C-h C") #'helpful-command)
(global-set-key (kbd "C-h l") #'counsel-find-library)

(require 'wgrep)

(global-set-key (kbd "C-x C-b") 'ibuffer)

(volatile-highlights-mode t)

(require 'which-key)
(which-key-mode)

;; y for yes, n for no, always
(defalias 'yes-or-no-p 'y-or-n-p)

(global-set-key (kbd "<f2>") 'bm-toggle)
(global-set-key (kbd "C-<f2>") 'bm-next)
(global-set-key (kbd "S-<f2>") 'bm-previous)

(global-set-key (kbd "C-c SPC") 'ace-jump-mode)
(global-set-key (kbd "M-o") 'ace-window)
(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
(ace-link-setup-default)

(global-hl-todo-mode)

(provide 'setup-my-better-defaults)
