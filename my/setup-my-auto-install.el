
(require 'package)

(defun elpa-mirror (name)
  (cons name (concat "/home/ljj/Downloads/elpa/" name "/")))

(setq package-archives (list (elpa-mirror "gnu")
                             (elpa-mirror "melpa")))
(package-initialize)

(defun auto-install (packages)
  "automatic install needed packages"
  (when (not package-archive-contents)
    (package-refresh-contents))
  (mapc #'(lambda (package)
    (unless (package-installed-p package)
      (package-install package)))
      packages))

(provide 'setup-my-auto-install)
