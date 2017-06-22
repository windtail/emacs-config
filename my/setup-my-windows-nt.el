
(defun w32-restore-frame ()
  "Restore a minimized frame"
  (interactive)
  (w32-send-sys-command 61728))

(defun w32-maximize-frame ()
  "Maximize the current frame"
  (interactive)
  (w32-send-sys-command 61488))

;; 设置垃圾回收，在Windows下，emacs25版本会频繁出发垃圾回收，所以需要设置
(setq gc-cons-threshold (* 1024 1024 1024))
(setq gc-cons-percentage 0.5)
(run-with-idle-timer 5 t #'garbage-collect)

;; 避免打开多个实例，需要注册表支持
;; Linux中应在需要时直接运行 server-start 命令，一般是在term中打开某个文件
;; 如果使用eshell，则可以直接运行emacs命令 find-file
(server-mode 1)

;; 字体设置
(set-frame-font (font-spec :family "Consolas" :size 24))
(set-fontset-font t 'gb18030 '("微软雅黑" . "unicode-bmp"))

;; 打开时最大化
(w32-maximize-frame)

;; 鼠标滚动放大缩小字体
(global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)
(global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease)

(defun show-in-explorer ()
  (interactive)
   (w32-shell-execute "explore" (replace-regexp-in-string "/" "\\" default-directory t t)))

(provide 'setup-my-windows-nt)

