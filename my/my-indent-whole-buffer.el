;;; my-indent-whole-buffer --- indent whole buffer
;;; Commentary:
;;; Code:

;;;###autoload
(defun my/indent-whole-buffer ()
  "Indent whole buffer."
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))

(provide 'my-indent-whole-buffer)
;;; my-indent-whole-buffer ends here
