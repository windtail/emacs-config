
(require 'setup-my-auto-install)

(auto-install '(ein
                elpy
                flycheck
                py-autopep8))

(require 'setup-my-prog-common)

(require 'elpy)
(add-hook 'python-mode-hook #'elpy-enable)
(add-hook 'python-mode-hook #'elpy-mode)

;; use flycheck not flymake with elpy
(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

;; enable autopep8 formatting on save
(require 'py-autopep8)
(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)

;; set py.test as test runner
(add-hook 'elpy-mode-hook (lambda () (elpy-set-test-runner 'elpy-test-pytest-runner)))

(defun my-pdb-config
  (require 'realgud)
  (setq-default realgud:pdb-command-name "python -m pdb"))

(provide 'setup-my-python)
