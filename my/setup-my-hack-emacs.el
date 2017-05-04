
;; 快速打开配置文件
(defun my-edit-init-file ()
  "Open init.el file for edit"
  (interactive)
  (find-file user-init-file))

(defun my-open-custom-dir ()
  "open custom directory"
  (interactive)
  (dired (concat user-emacs-directory "my/")))

(global-set-key (kbd "<f2> f") 'my-edit-init-file)
(global-set-key (kbd "<f2> d") 'my-open-custom-dir)

(provide 'setup-my-hack-emacs)
