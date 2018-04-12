;;; my-prog-mode-config --- misc prog-mode config
;;; Commentary:
;;; Code:

(require 'f)

;;;###autoload
(defun my/compile-emacs-lisp (arg)
  "Byte compile current Lisp file if needed.

Remove elc file if prefix"
  (interactive "P")
  (let* ((name (buffer-file-name))
         (ext (and name (f-ext name))))
    (when (equal "el" ext)
      (let ((namec (concat name "c")))
        (if (f-exists? namec)
            (if arg
                (f-delete namec)
              (byte-recompile-file name))
          (if arg
              nil
            (byte-compile-file name)))))))

;;;###autoload
(defun my/prog-mode-config ()
  "Misc `prog-mode' config."
  (if (derived-mode-p 'emacs-lisp-mode)
      (local-set-key (kbd "<f7>") #'my/compile-emacs-lisp)
    (local-set-key (kbd "<f7>") #'recompile))
  (setq truncate-lines t)
  (add-to-list 'write-file-functions 'delete-trailing-whitespace))

(provide 'my-prog-mode-config)
;;; my-prog-mode-config ends here
