
;; 快速打开配置文件
(defun my-edit-init-file ()
  "Open init.el file for edit"
  (interactive)
  (find-file user-init-file))

(defun my-open-custom-dir ()
  "open custom directory"
  (interactive)
  (dired (concat user-emacs-directory "my/")))

(global-set-key (kbd "<f6>") 'my-edit-init-file)
(global-set-key (kbd "C-c C-f") 'find-file-at-point)

(provide 'setup-my-hack-emacs)
