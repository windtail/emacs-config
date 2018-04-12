;;; my-rest --- reST configurations
;;; Commentary:
;;; Code:

(require 'table)

(defvar my/rest-pdf-cjk-main-font (if (find-font (font-spec :family="宋体"))
                                      "宋体"
                                    "AR PL UMing CN")
  "CJK main font.")

(defvar my/rest-pdf-cjk-main-bold-font (if (find-font (font-spec :family="宋体"))
                                           "黑体"
                                         "文泉驿正黑")
  "CJK main bold font.")

;;;###autoload
(defun my/rest-pandoc-to-pdf ()
  "Convert reST to pdf using pandoc (xelatex)."
  (interactive)
  (set (make-local-variable 'compile-command)
       (format "pandoc -t latex --latex-engine=xelatex -s -VCJKoptions=BoldFont=\"%s\" -VCJKmainfont=\"%s\" -o %s.pdf %s"
               my/rest-pdf-cjk-main-bold-font my/rest-pdf-cjk-main-font
               (file-name-sans-extension (file-name-nondirectory (buffer-file-name)))
               (file-name-nondirectory (buffer-file-name))))
  (call-interactively 'compile))

;;;###autoload
(defun my/rest-pandoc-to-docx ()
  "Convert reST to docx using pandoc."
  (interactive)
  (set (make-local-variable 'compile-command)
       (format "pandoc -t docx -s -o %s.docx %s"
               (file-name-sans-extension (file-name-nondirectory (buffer-file-name)))
               (file-name-nondirectory (buffer-file-name))))
  (call-interactively 'compile))

;;;###autoload
(defun my/rest-rst2pdf-to-pdf ()
  "Convert reST to pdf using rst2pdf + custom config and style."
  (interactive)
  (set (make-local-variable 'compile-command)
       (format "rst2pdf --config ~/.emacs.d/rst2pdf/config -o %s.pdf %s"
               (file-name-sans-extension (file-name-nondirectory (buffer-file-name)))
               (file-name-nondirectory (buffer-file-name))))
  (call-interactively 'compile))

;;;###autoload
(defun my/rest-restview-preview (rest-file-path)
  "Preview reST file `REST-FILE-PATH' in browser and updated on saving."
  (interactive (list (buffer-file-name)))
  (start-process "restview" nil "restview" rest-file-path))

(provide 'my-rest)
;;; my-rest.el ends here
