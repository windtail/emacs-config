
(require 'package)

(defvar elpa-root (or (getenv "ELPA_OFFLINE_ROOT") "http://elpa.emacs-china.org/"))

(defun elpa-mirror (name)
  (cons name (concat elpa-root name "/")))

(setq package-archives (list (elpa-mirror "gnu")
                             (elpa-mirror "melpa")))
(package-initialize)

(defun auto-install (packages)
  "automatic install needed packages"
  (when (not package-archive-contents)
    (package-refresh-contents))
  (mapc #'(lambda (package)
    (unless (package-installed-p package)
      (package-install package)))
      packages))

(defun auto-download-contrib (link name)
  "automatic download third-party package not listed in melpa,
note: downloading https resources needs gnutls support, to test
whether gnutls support is compiled with, eval (gnutls-available-p),
if you compiled emacs from source, make sure gnutls development library
is installed.
If you really have problem to get gnutls support, please download
the resources using browser and put it in ~/.emacs.d/contrib/ directory"
  (let* ((contrib-dir-name "~/.emacs.d/contrib")
    (full-name (format "%s/%s" contrib-dir-name name)))
    (if (not (file-exists-p contrib-dir-name))
        (make-directory contrib-dir-name))
    (require 'url)
    (if (not (file-exists-p full-name))
        (url-copy-file link full-name))))

(provide 'setup-my-auto-install)
