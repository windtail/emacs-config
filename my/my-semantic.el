;;; my-semantic --- my semantic utils
;;; Commentary:
;;; Code:

(defvar my/semantic--initialize-status -1)

(defun my/semantic-clear-cc-includes ()
  (require 'cc-mode) ; make sure cc-mode required
  (semantic-reset-system-include 'c-mode)
  (semantic-reset-system-include 'c++-mode))

;;;###autoload
(defun my/semantic-reset-cc-includes ()
  "Reset system includes for `c-mode' and `c++-mode'.

MUST be called after `my/semantic-enable'"
  (my/semantic-clear-cc-includes)
  (semantic-gcc-setup))

;;;###autoload
(defun my/semantic-add-cc-include (dir)
  "Add `DIR' as system include for `c-mode' and `c++-mode'.

MUST be called after `my/semantic-enable'"
  (require 'cc-mode)
  (semantic-add-system-include dir 'c-mode)
  (semantic-add-system-include dir 'c++-mode))

(defun my/semantic--enable-first-time ()
  (setq semantic-default-submodes
        '(global-semanticdb-minor-mode
          global-semantic-idle-scheduler-mode
          global-semantic-idle-summary-mode
          global-semantic-decoration-mode
          global-semantic-highlight-func-mode
          global-semantic-stickyfunc-mode
          global-semantic-mru-bookmark-mode
          global-semantic-idle-local-symbol-highlight-mode
          global-semantic-highlight-edits-mode
          global-semantic-show-unmatched-syntax-mode))
  (semantic-mode)
  (global-set-key (kbd "<f12>") 'semantic-ia-fast-jump)
  (require 'semantic/sb)
  (defadvice semantic-add-system-include (around semantic-try-add-system-include activate)
    "Add system include if path exists"
    (interactive "DNew Include Directory: ")
    (if (file-directory-p dir)
        ad-do-it)))

;;;###autoload
(defun my/semantic-enable ()
  "Enable semantic."
  (interactive)
  (let ((status my/semantic--initialize-status))
    (cond
     ((< status 0) (my/semantic--enable-first-time))
     ((= status 0) (semantic-mode)))
    (setq my/semantic--initialize-status 1)))

;;;###autoload
(defun my/semantic-disable ()
  "Disable semantic."
  (interactive)
  (semantic-mode 0)
  (setq my/semantic--initialize-status 0))

;;;###autoload
(defun my/semantic-enabled? ()
  (= my/semantic--initialize-status 1))

(provide 'my-semantic)
;;; my-semantic ends here
