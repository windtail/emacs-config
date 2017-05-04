
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

;; 字体设置
(set-default-font (font-spec :family "Microsoft Yahei" :size 24))
(set-fontset-font "fontset-default" 'gb18030' ("微软雅黑" . "unicode-bmp"))

;; 打开时最大化
(w32-maximize-frame)

(provide 'setup-my-windows-nt)

