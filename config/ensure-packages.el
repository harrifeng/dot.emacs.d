(require 'package)
(require 'cl)
(package-initialize)

(setq url-http-attempt-keepalives nil)

(defvar ensure-packages
  (anything-config
   anything
   auto-complete
   bm
   grep-a-lot
   highlight-symbol
   highline
   htmlize
   jedi
   multi-web-mode
   magit
   rainbow-mode
   sublime-themes
   yasnippet
   dropdown-list)
  "A list of packages to ensure are installed at launch.")

(defun ensure-packages-package-installed-p (p)
  (cond ((package-installed-p p) t)
        (t nil)))

(defun ensure-packages-installed-p ()
  (mapcar 'ensure-packages-package-installed-p ensure-packages))

(defun ensure-packages-install-missing ()
  (interactive)
  (unless (every 'identity (ensure-packages-installed-p))
  ;; check for new packages (package versions)
  (message "%s" "Emacs is now refreshing its package database...")
  (package-refresh-contents)
  (message "%s" " done.")
  ;; install the missing packages
  (dolist (p ensure-packages)
    (when (not (package-installed-p p))
      (package-install p)))))

(provide 'ensure-packages)
