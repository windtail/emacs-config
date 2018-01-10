
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

(if (eq system-type 'windows-nt)
    (setq rst-normal-font "SimSun" rst-bold-font "SimHei")
  (setq rst-normal-font "AR PL UMing CN" rst-bold-font "文泉驿正黑"))

(defun my-pandoc-rst2pdf ()
  "convert rst to pdf using pandoc(xelatex)"
  (interactive)
  (compile (format "pandoc -t latex --latex-engine=xelatex -s -VCJKoptions=BoldFont=\"%s\" -VCJKmainfont=\"%s\" -o %s.pdf %s"
                   rst-bold-font rst-normal-font (file-name-base (buffer-name)) (buffer-name))))

(provide 'setup-my-text)
