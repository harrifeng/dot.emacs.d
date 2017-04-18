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

;; (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
;; (add-to-list 'comint-output-filter-functions 'ansi-color-process-output)
(mapc
 (lambda (package)
   (or (package-installed-p package)
       (package-install package)))
 '(
   multi-term
   helm-mt
   ))

(require 'multi-term)
(setq multi-term-program "/bin/bash")
;; Use Emacs terminfo, not system terminfo
(setq system-uses-terminfo nil)

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
                                                 (buffer-file-name)))) "/") "."))))

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

;; Different theme
(defun random-color-theme () (interactive)
       (let ((chosen-theme
              (nth
               (random
                (length (mapcar 'symbol-name (custom-available-themes))))
               (custom-available-themes))))
         (message "Theme: %s" chosen-theme)
         (load-theme chosen-theme t nil)))

(defun show-me-the-colors ()  (interactive) (loop do (random-color-theme) (sit-for 3)))
(setq color-theme-is-cumulative 'false)


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
      (venv-workon "3ENV")))

(add-to-list 'load-path (concat my-lisps-path "nopackage"))
(require 'livedown)
(require 'redis-cli)
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

(let* ((dot-theme-file (concat my-emacs-path ".theme"))
       (files '(
                ;; darcula
                material
                ;; spacemacs-dark
                ))
       (tmp (string-to-number (get-string-from-file dot-theme-file)))
       (idx (% (abs tmp) (length files )))
       (selected-filename (nth idx files))
       )
  (load-theme selected-filename t)
  (with-temp-file dot-theme-file
    (insert (format "%s" (+ tmp 1)))
    )
  )

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


(defun open-shell-pwd ()
  "Open a shell on pwd"
  (interactive)
  (setq shell-pwd-name  (format "SHELL-%s" (current-buffer)))
  (if (not (get-buffer shell-pwd-name))
      (progn
        (split-window-below)
        (shell (format "SHELL-%s" (current-buffer)))
        )
    (switch-to-buffer-other-window shell-pwd-name)))

(global-set-key (kbd "C-c v")   'open-shell-pwd)
(global-set-key (kbd "C-c C-v") 'open-shell-pwd)


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
(global-flycheck-mode)

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
(global-set-key (kbd "<f8>") 'toggle-flycheck-error-buffer)

(setq-default resize-mini-windows nil
              enable-recursive-minibuffers t)
