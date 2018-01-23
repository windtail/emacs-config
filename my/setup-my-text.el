
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

(defun my-pandoc-setup-pdf ()
  (if (find-font (font-spec :family="宋体"))
	  (setq pandoc-rst-cjk-main-font "宋体")
	(setq pandoc-rst-cjk-main-font "AR PL UMing CN"))

  (if (find-font (font-spec :family="黑体"))
	  (setq pandoc-rst-cjk-main-bold-font "黑体")
	(setq pandoc-rst-cjk-main-bold-font "文泉驿正黑"))

  (defun my-pandoc-rst2pdf ()
	"convert reST to pdf using pandoc(xelatex)"
	(interactive)
	(set (make-local-variable 'compile-command)
		 (format "pandoc -t latex --latex-engine=xelatex -s -VCJKoptions=BoldFont=\"%s\" -VCJKmainfont=\"%s\" -o %s.pdf %s"
				 pandoc-rst-cjk-main-bold-font pandoc-rst-cjk-main-font
				 (file-name-sans-extension (file-name-nondirectory (buffer-file-name)))
				 (file-name-nondirectory (buffer-file-name))))
	(call-interactively 'compile))

  (defun my-pandoc-rst2docx ()
	"convert reST to docx using pandoc"
	(interactive)
	(set (make-local-variable 'compile-command)
		 (format "pandoc -t docx -s -o %s.docx %s"
				 (file-name-sans-extension (file-name-nondirectory (buffer-file-name)))
				 (file-name-nondirectory (buffer-file-name))))
	(call-interactively 'compile))

  (define-key rst-mode-map (kbd "<f5> p") 'my-pandoc-rst2pdf)
  (define-key rst-mode-map (kbd "<f5> d") 'my-pandoc-rst2docx))

(defun my-setup-rst2pdf ()
  (defun my-rst2pdf ()
	"convert reST to pdf using rst2pdf + custom config and style"
	(interactive)
	(set (make-local-variable 'compile-command)
		 (format "rst2pdf --config ~/.emacs.d/rst2pdf/config -o %s.pdf %s"
				 (file-name-sans-extension (file-name-nondirectory (buffer-file-name)))
				 (file-name-nondirectory (buffer-file-name))))
	(call-interactively 'compile))

  (define-key rst-mode-map (kbd "<f5> f") 'my-rst2pdf))

(defun my-restview-preview (filepath)
  "Preview reST in browser and updated on saving"
  (interactive (list (buffer-file-name)))
  (start-process "restview" nil "restview" filepath))

(add-hook 'rst-mode-hook 'my-pandoc-setup-pdf)
(add-hook 'rst-mode-hook 'my-setup-rst2pdf)
(add-hook 'rst-mode-hook (lambda () (require 'table)))
(add-hook 'rst-mode-hook (lambda () (define-key rst-mode-map (kbd "C-c C-p") 'my-restview-preview)))

(provide 'setup-my-text)
