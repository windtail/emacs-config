;;; my-use-package-ensure-contrib --- :ensure-contrib keyword for use-package
;;; Commentary:
;;; Code:

(require 'use-package)

(add-to-list 'use-package-keywords :ensure-contrib)

(defun my/ensure-contrib-package (link name)
  "Download contrib package from `LINK' and save to `NAME'.el if not found locally."
  (let* ((contrib-package-directory (concat user-emacs-directory "contrib/"))
         (package-file-name (concat contrib-package-directory (symbol-name name) ".el")))
    (make-directory contrib-package-directory t)
    (unless (file-exists-p package-file-name)
      (require 'url)
      (url-copy-file link package-file-name))))

(defun use-package-normalize/:ensure-contrib (name keyword args)
  "Check argument is a string."
  (use-package-only-one (symbol-name keyword) args
    (lambda (label arg)
      (cond
       ((stringp arg) arg)
       (t (use-package-error ":ensure-contrib wants an link"))))))

(defun use-package-handler/:ensure-contrib (name keyword link rest state)
  "Insert or execute package installing commands."
  (let ((body (use-package-process-keywords name rest state)))
    (if (bound-and-true-p byte-compile-current-file)
        (my/ensure-contrib-package link name)
      (push `(my/ensure-contrib-package ,link ',name)
            body))
    body))

(provide 'my-use-package-ensure-contrib)
;;; my-use-package-ensure-contrib ends here
