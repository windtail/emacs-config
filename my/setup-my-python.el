
(require 'setup-my-auto-install)

(auto-install '(ein
                elpy
                flycheck
                py-autopep8))

(require 'setup-my-prog-common)

(require 'elpy)
(add-hook 'python-mode-hook #'elpy-enable)
(add-hook 'python-mode-hook #'elpy-mode)
(elpy-use-ipython)

;; use flycheck not flymake with elpy
(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

;; enable autopep8 formatting on save
(require 'py-autopep8)
(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)

(defun my-pdb-config
  (require 'realgud)
  (setq-default realgud:pdb-command-name "python -m pdb"))

;; for pyvenv-workon
(if (eq system-type 'windows-nt)
  (setenv "WORKON_HOME" (getenv "USERPROFILE"))
  (setenv "WORKON_HOME" (getenv "HOME")))

(provide 'setup-my-python)
