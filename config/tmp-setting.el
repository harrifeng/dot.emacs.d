(provide 'tmp-setting)

(defvar my-ansi-escape-re
  (rx (or ?\233 (and ?\e ?\[))
      (zero-or-more (char (?0 . ?\?)))
      (zero-or-more (char ?\s ?- ?\/))
      (char (?@ . ?~))))

(defun my-nuke-ansi-escapes (beg end)
  (save-excursion
    (goto-char beg)
    (while (re-search-forward my-ansi-escape-re end t)
      (replace-match ""))))

(defun my-eshell-nuke-ansi-escapes ()
  (my-nuke-ansi-escapes eshell-last-output-start eshell-last-output-end))

(add-hook 'eshell-output-filter-functions 'my-eshell-nuke-ansi-escapes t)
(add-hook 'shell-output-filter-functions 'my-eshell-nuke-ansi-escapes t)


(defun java-save-compile-and-run()
  (interactive)
  (save-buffer)
  (if (eq system-type 'windows-nt)
      (setq java-run-command "cd %s && gradlew.bat run -DmainClass=%s")
    (setq java-run-command "cd %s && export TERM=dumb && ./gradlew run -DmainClass=%s"))
  (compile
   (format java-run-command
           (substring (buffer-file-name) 0  (string-match "src" (buffer-file-name)) )
           (mapconcat 'identity
                      (split-string
                       (substring (file-name-sans-extension (buffer-file-name))
                                  (string-match "org" (file-name-sans-extension
                                                       (buffer-file-name)))
                                  (string-width (file-name-sans-extension
                                                 (buffer-file-name)))) "/") ".")))
  (my-make-room-for-new-compilation-buffer)
  )

(defun java-save-compile-and-test()
  (interactive)
  (save-buffer)
  (if (eq system-type 'windows-nt)
      (setq java-run-command "cd %s && gradlew.bat test --tests %sTest")
    (setq java-run-command "cd %s && export TERM=dumb && ./gradlew test --tests %sTest"))
  (compile
   (format java-run-command
           (substring (buffer-file-name) 0  (string-match "src" (buffer-file-name)) )
           (mapconcat 'identity
                      (split-string
                       (substring (file-name-sans-extension (buffer-file-name))
                                  (string-match "org" (file-name-sans-extension
                                                       (buffer-file-name)))
                                  (string-width (file-name-sans-extension
                                                 (buffer-file-name)))) "/") "."))))

(add-hook 'java-mode-hook
          (lambda ()
            (define-key java-mode-map (kbd "<f9>") 'java-save-compile-and-run)
            (define-key java-mode-map (kbd "C-m") 'java-save-compile-and-run)
            (define-key java-mode-map (kbd "<f8>") 'java-save-compile-and-test)
            ))

(defun scheme-hfeng-run()
  (interactive)
  (save-buffer)

  (if (eq system-type 'windows-nt)
      (setq rust-run-command "scheme.exe load < %s")
    (setq rust-run-command "scheme load < %s"))
  (compile
   (format rust-run-command
           (buffer-file-name))))


(add-hook 'scheme-mode-hook
          (lambda ()
            (define-key scheme-mode-map (kbd "<f9>") 'scheme-hfeng-run)
            ))

(defun sh-hfeng-run()
  (interactive)
  (save-buffer)

  (if (eq system-type 'windows-nt)
      (setq hfeng-run-command "bash.exe  %s")
    (setq hfeng-run-command "bash  %s"))
  (compile
   (format hfeng-run-command
           (buffer-file-name))))

(add-hook 'sh-mode-hook
          (lambda ()
            (define-key sh-mode-map (kbd "<f9>") 'sh-hfeng-run)
            ))

(defun objc-save-compile-and-run ()
  (interactive)
  (save-buffer)

  (if (locate-dominating-file (buffer-file-name) "Makefile")
      (compile "make run && make clean")
    ;; (setq objc-run-command "objcc %s && %s && rm %s")
    (setq objc-run-command "clang -fobjc-arc -framework Foundation %s -o %s.exe && %s.exe && rm %s.exe")
    (compile
     (format objc-run-command
             (buffer-file-name)
             (file-name-sans-extension (buffer-file-name))
             (file-name-sans-extension (buffer-file-name))
             (file-name-sans-extension (buffer-file-name))
             ))))

(add-hook 'objc-mode-hook
          (lambda ()
            (define-key objc-mode-map (kbd "<f9>") 'objc-save-compile-and-run)
            ))

;; objc-mode
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region compilation-filter-start (point))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

(require 'virtualenvwrapper)
(venv-initialize-interactive-shells) ;; if you want interactive shell support
(venv-initialize-eshell) ;; if you want eshell support

(defun folder-under-folder (path)
  (mapcar (lambda (x) (concat path x))
          (nthcdr 2 (directory-files path))))

(defconst my-venv-path (concat (getenv "HOME") "/venv/"))

(if (file-exists-p my-venv-path)
    (progn
      (setq venv-location
            (folder-under-folder my-venv-path))
      ;; we often had two envs, the 2ENV and 3ENV, comment out if you do not use env
      ;; (venv-workon "3ENV")
      ))

(add-to-list 'load-path (concat my-lisps-path "nopackage"))
(require 'livedown)
(require 'redis-cli)
(require 'visible-mark)
(require 'highlight)
;; (global-visible-mark-mode 1) ;; or add (visible-mark-mode) to specific hooks
;; (defface visible-mark-active ;; put this before (require 'visible-mark)
;;   '((((type tty) (class mono)))
;;     (t (:background "magenta"))) "")
;; (setq visible-mark-max 2)
;; (setq visible-mark-faces `(visible-mark-face1 visible-mark-face2))

(defun refresh-file ()
  "Revert buffer Only when it's really changed, maybe usefull"
  (interactive)
  ;;(revert-buffer t t t)
  (revert-buffer t (not (buffer-modified-p)) t)
  )


(defun get-string-from-file (filePath)
  "Return filePath's file content"
  (if (file-exists-p filePath)
      (with-temp-buffer
        (insert-file-contents filePath)
        (buffer-string))
    (format "%s" 0)
    ))


(defun regexp-alternatives (regexps)
  "Return the alternation of a list of regexps."
  (mapconcat (lambda (regexp)
               (concat "\\(?:" regexp "\\)"))
             regexps "\\|"))

(defvar non-sgr-control-sequence-regexp nil
  "Regexp that matches non-SGR control sequences.")

(setq non-sgr-control-sequence-regexp
      (regexp-alternatives
       '(;; icon name escape sequences
         "\033\\][0-2];.*?\007"
         ;; non-SGR CSI escape sequences
         "\033\\[\\??[0-9;]*[^0-9;m]"
         ;; noop
         "\012\033\\[2K\033\\[1F"
         )))

(defun filter-non-sgr-control-sequences-in-region (begin end)
  (save-excursion
    (goto-char begin)
    (while (re-search-forward
            non-sgr-control-sequence-regexp end t)
      (replace-match ""))))

(defun filter-non-sgr-control-sequences-in-output (ignored)
  (let ((start-marker
         (or comint-last-output-start
             (point-min-marker)))
        (end-marker
         (process-mark
          (get-buffer-process (current-buffer)))))
    (filter-non-sgr-control-sequences-in-region
     start-marker
     end-marker)))

(add-hook 'comint-output-filter-functions
          'filter-non-sgr-control-sequences-in-output)

(defun lunaryorn-use-js-executables-from-node-modules ()
  "Set executables of JS checkers from local node modules."
  (-when-let* ((file-name (buffer-file-name))
               (root (locate-dominating-file file-name "node_modules"))
               (module-directory (expand-file-name "node_modules" root)))
    (pcase-dolist (`(,checker . ,module) '((javascript-jshint . "jshint")
                                           (javascript-eslint . "eslint")
                                           (javascript-jscs   . "jscs")))
      (let ((package-directory (expand-file-name module module-directory))
            (executable-var (flycheck-checker-executable-variable checker)))
        (when (file-directory-p package-directory)
          (set (make-local-variable executable-var)
               (expand-file-name (concat "bin/" module ".js")
                                 package-directory)))))))

(add-hook 'flycheck-mode-hook 'lunaryorn-use-js-executables-from-node-modules)

;;========================================
(global-flycheck-mode 1)

(setq flycheck-highlighting-mode nil)
(setq flycheck-indication-mode nil)

(defun toggle-flycheck-error-buffer ()
  "toggle a flycheck error buffer."
  (interactive)
  (if (string-match-p "Flycheck errors" (format "%s" (window-list)))
      (dolist (w (window-list))
        (when (string-match-p "*Flycheck errors*" (buffer-name (window-buffer w)))
          (delete-window w)
          ))
    (flycheck-list-errors)
    )
  )

(setq-default resize-mini-windows nil
              enable-recursive-minibuffers t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (require 'flycheck-tip)                                         ;;
;; (define-key your-prog-mode (kbd "C-c C-n") 'flycheck-tip-cycle) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; To avoid echoing error message on minibuffer (optional)
(setq flycheck-display-errors-function 'ignore)


;; tmp solution for ruby 1.9, as it does not support rubocop
(add-hook 'ruby-mode-hook
          '(lambda ()
             (setq flycheck-checker 'ruby-rubylint)
             (flycheck-mode 1)))

;; This speed up the tramp connection
(setq projectile-mode-line "Projectile")

;; sample way to unset key for minor mode
(defun my-elpy-hook ()
  (define-key elpy-mode-map (kbd "<C-return>") nil))
(add-hook 'elpy-mode-hook 'my-elpy-hook)


(add-hook 'comint-exec-hook
          (lambda () (set-process-query-on-exit-flag (get-buffer-process (current-buffer)) nil)))

(add-hook 'prog-mode-hook #'hs-minor-mode)




;; ========
;; savehist
;; ========
(setq savehist-additional-variables
      '(search-ring regexp-search-ring kill-ring register-alist)
      savehist-file "~/.emacs.d/savehist")

(savehist-mode 1)

;; =======
;; recentf
;; =======
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-saved-items 300)
(setq recent-save-file "~/.emacs.d/recentf")

;; =========
;; saveplace
;; =========
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file "~/.emacs.d/saved-places")



(defun shell-log-hook ()
  (add-hook 'kill-buffer-hook 'hfeng-clear-screen-to-another-buffer nil t)
  (add-hook 'kill-emacs-hook 'hfeng-clear-screen-to-another-buffer nil t)
  )

(add-hook 'shell-mode-hook 'shell-log-hook)
(add-hook 'eshell-mode-hook 'shell-log-hook)

(if (file-exists-p "/usr/local/bin/git")
    (setq magit-git-executable "/usr/local/bin/git"))

;; python3
(add-to-list 'auto-mode-alist '("\\.py3\\'" . python-mode))


;; total commander function
(defun total-commander-copy-files ()
  "total commander copy method. Copy the marked files to the next panel."
  (interactive)
  (progn (switch-window)
         (let ((target-directory (expand-file-name default-directory)))
           (progn (switch-window)
                  (mapc (lambda (marked-file)
                          (shell-command (concat (format "cp -r \"%s\""
                                                         (file-name-nondirectory marked-file))
                                                 " "
                                                 target-directory)))
                        (dired-get-marked-files))
                  (switch-window)
                  (revert-buffer)))))
(advice-add 'yas--auto-fill-wrapper :override #'ignore)


(setq helm-dash-browser-func 'eww)

(setq helm-dash-common-docsets '("Go"
                                 "Bash"
                                 "Python 2"
                                 "Python 3"))



(defun go-doc ()
  (interactive)
  (setq-local helm-dash-docsets '("Go")))

(add-hook 'go-mode-hook 'go-doc)


(defun python-doc ()
  (interactive)
  (setq-local helm-dash-docsets '("Python 2")))

(add-hook 'python-mode-hook 'python-doc)


(defun go-doc ()
  (interactive)
  (setq-local helm-dash-docsets '("Go")))

(add-hook 'go-mode-hook 'go-doc)


(defun bash-doc ()
  (interactive)
  (setq-local helm-dash-docsets '("Bash")))

(add-hook 'sh-mode-hook 'bash-doc)
