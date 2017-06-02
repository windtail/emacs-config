
(require 'setup-my-auto-install)

(auto-install '(restclient company-restclient))

(add-hook 'restclient-mode-hook
          (lambda () (setq company-backends '(company-restclient company-dabbrev))))

(provide 'setup-my-misc)
