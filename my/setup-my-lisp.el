
(require 'setup-my-auto-install)

(auto-install '(geiser))

(setq scheme-program-name "csi -:c")

(require 'geiser)
(setq-default geiser-implementations-alist '(
                                             ((regexp "\\.scm$") chicken)
                                             ((regexp "\\.ss$") racket)
                                             ))
(setq-default geiser-mode-start-repl-p t)

(provide 'setup-my-lisp)
