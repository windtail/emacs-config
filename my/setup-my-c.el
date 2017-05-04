
(require 'setup-my-auto-install)

(auto-install '(ggtags))

(require 'setup-my-prog-common)

(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
              (ggtags-mode 1))))

(provide 'setup-my-c)
