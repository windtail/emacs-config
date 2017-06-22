
(require 'setup-my-auto-install)

(auto-install '(org markdown-mode markdown-mode+))

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

;; 3 runs to get ref correct (pdflatext => xelatex)
(setq org-latex-pdf-process '("xelatex -interaction nonstopmode -output-directory %o %f"
                              "xelatex -interaction nonstopmode -output-directory %o %f"
                              "xelatex -interaction nonstopmode -output-directory %o %f"))

(add-hook 'org-mode-hook 'yas-minor-mode)

(provide 'setup-my-text)
