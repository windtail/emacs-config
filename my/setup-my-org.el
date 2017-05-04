
(require 'setup-my-auto-install)

(auto-install '(org))

(require 'org)
(setq org-src-fontify-native t)

(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (sh . t)
   (python . t)
   (dot . t)
   (octave . t)
   (sqlite . t)   
   (C . t)
   ))

(provide 'setup-my-org)
