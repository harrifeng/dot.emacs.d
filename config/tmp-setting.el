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
      ;; (venv-workon "3ENV")
      ))

(add-to-list 'load-path (concat my-lisps-path "nopackage"))
(require 'livedown)
(require 'redis-cli)
(require 'visible-mark)
(global-visible-mark-mode 1) ;; or add (visible-mark-mode) to specific hooks
(defface visible-mark-active ;; put this before (require 'visible-mark)
  '((((type tty) (class mono)))
    (t (:background "magenta"))) "")
(setq visible-mark-max 2)
(setq visible-mark-faces `(visible-mark-face1 visible-mark-face2))

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
                darcula
                ;; material
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
        (shell shell-pwd-name)
        )
    (switch-to-buffer-other-window shell-pwd-name)))

(defun open-eshell-pwd ()
  "Open a eshell on pwd"
  (interactive)
  (setq eshell-pwd-name  (format "ESHELL-%s" (current-buffer)))
  (if (not (get-buffer eshell-pwd-name))
      (progn
        (split-window-below)
        (eshell "new")
        (rename-buffer eshell-pwd-name)
        )
    (switch-to-buffer-other-window eshell-pwd-name)))


(if (eq system-type 'windows-nt)
    (progn
      (global-set-key (kbd "C-c v")   'open-eshell-pwd)
      (global-set-key (kbd "C-c C-v") 'open-eshell-pwd)
      )
  (progn
    (global-set-key (kbd "C-c v")   'open-shell-pwd)
    (global-set-key (kbd "C-c C-v") 'open-shell-pwd)
    ))

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

(add-hook 'yaml-mode-hook 'flymake-yaml-load)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(require 'general)
;; define C-. as prefix key, otherwise general will complain
(define-prefix-command 'ctl-space-map)
(global-set-key (kbd "C-SPC") 'ctl-space-map)
(global-set-key (kbd "C-c a") 'ctl-space-map) ;mac key bind in Iterm2
(setq my-leader1 "C-SPC")

(general-define-key :prefix my-leader1
                    "."   'open-shell-pwd
                    "0"   'delete-window
                    "1"   'delete-other-windows
                    "2"   'split-window-below
                    "3"   'split-window-right
                    "a"   'helm-projectile-ag ;Ag
                    "d"   'delete-window
                    "e"   'toggle-flycheck-error-buffer ;Error
                    "h"   'split-window-horizontal ;Horizontal
                    "i"   'highlight-symbol        ;hIghlithg-symbol
                    "k"   'kill-region             ;Kill(cut)
                    "l"   'hfeng-clear-screen-to-another-buffer ;cLear
                    "n"   'highlight-symbol-next   ;Next      highlight
                    "o"   'kill-ring-save          ;Opposite of kill-region
                    "p"   'highlight-symbol-prev   ;Prev      highlight
                    "x"   'helm-all-mark-rings                    ;rings
                    ;; "t"   'bm-toggle                ;bm-Toggle
                    "u"   'cua-set-rectangle-mark
                    "v"   'split-window-vertical   ;Vertical
                    "y"   'kill-ring-save          ;copY
                    "SPC" 'helm-M-x
                    ;; More tolerant
                    "C-."   'open-shell-pwd
                    "C-1"   'delete-other-windows
                    "C-2"   'split-window-below
                    "C-3"   'split-window-right
                    "C-a"   'helm-projectile-ag ;Ag
                    "C-d"   'delete-other-windows ;Delete
                    "C-e"   'toggle-flycheck-error-buffer ;Error
                    "C-h"   'split-window-horizontal ;Horizontal
                    "C-i"   'highlight-symbol        ;hIghlithg-symbol
                    "C-k"   'kill-region             ;Kill(cut)
                    "C-l"   'hfeng-clear-screen-to-another-buffer ;cLear
                    "C-n"   'highlight-symbol-next   ;Next      highlight
                    "C-o"   'other-window
                    "C-p"   'highlight-symbol-prev   ;Prev      highlight
                    "C-x"   'helm-all-mark-rings ;all mark ring
                    "C-t"   'bm-toggle                ;bm-Toggle
                    "C-v"   'split-window-vertical   ;Vertical
                    "C-y"   'kill-ring-save          ;copY
                    "C-SPC" 'helm-M-x
                    )



(setq my-leader2 "C-SPC b")

(general-define-key :prefix my-leader2
                    :prefix-command 'Buffer
                    "b" 'helm-mini
                    "i" 'ibuffer
                    "k" 'kill-buffer
                    "n" 'next-buffer
                    "p" 'previous-buffer
                    "r" 'revert-buffer-no-confirm ;Revert
                    "s" 'save-buffer
                    "w" 'mark-whole-buffer
                    )

(setq my-leader3 "C-SPC f")

(general-define-key :prefix my-leader3
                    :prefix-command 'File
                    "f" 'find-file
                    "h" 'helm-find-files
                    "o" 'find-file-other-window
                    "p" 'helm-projectile ; opened files called buffer
                    "m" 'helm-bookmarks
                    )


(setq my-leader4 "C-SPC g")

(general-define-key :prefix my-leader4
                    :prefix-command 'Git
                    "g" 'magit-status
                    "a" 'git-add-current-buffer-to-git ;add & stash
                    "d" 'vc-diff
                    "c" 'with-editor-finish ;commit
                    "r" 'vc-revert
                    "s" 'git-add-current-buffer-to-git ;add & stash
                    )


(setq my-leader5 "C-SPC s")

(general-define-key :prefix my-leader5
                    :prefix-command 'Search
                    "s" 'helm-swoop
                    "a" 'helm-do-ag
                    "b" 'helm-do-ag-buffers
                    "f" 'helm-do-ag-this-file
                    "p" 'helm-projectile-ag
                    )

(setq my-leader6 "C-SPC q")

(general-define-key :prefix my-leader6
                    :prefix-command 'Quit
                    "q" 'save-buffers-kill-terminal   ;Exit
                    "r" 'restart-emacs
                    )

(setq my-leader7 "C-SPC r")

(general-define-key :prefix my-leader7
                    :prefix-command 'Replace
                    "q" 'query-replace
                    "r" 'replace-string
                    )

(setq my-leader8 "C-SPC m")

(general-define-key :prefix my-leader8
                    :prefix-command 'MultipleCursors
                    ;; here the region start from coursor to the end
                    ;; you can use `Mark set` again to select region!!!
                    "m" 'mc/edit-lines              ; Most useful one, can cursor in the middle of the line
                    "b" 'mc/edit-beginnings-of-lines ; like 'm', except the can only cursor at beginning
                    "e" 'mc/edit-ends-of-lines       ; like 'm', except the can only cursor at end
                    ;; had region concept
                    "a" 'mc/mark-all-in-region  ;you can also use `C-g` to cancle region, only the curors!!!
                    "n" 'mc/mark-next-like-this ;region select next
                    "p" 'mc/mark-previous-like-this ;resiong select previous
                    "w" 'mc/mark-all-like-this ; mark active region in all in whole buffer, not very accurate
                    "d" 'mc/insert-numbers          ;digits
                    "l" 'mc/insert-letters
                    "s" 'mc/sort-regions
                    "r" 'mc/reverse-regions
                    "o" 'mc/mark-pop
                    ; mark-next-like-this is two phases
                    ; 1.mark this region, and 2.move to next line
                    ; we use skip-to-next-lik-this means
                    ; 1.skip current line,and 2.move to next line
                    ; Also, you can use C-u <num> to mark-like-this quickly!
                    "k" 'mc/skip-to-next-like-this
                    "x" 'mc/skip-to-previous-like-this; not use often
                    )

(setq my-leader9 "C-SPC w")

(general-define-key :prefix my-leader9
                    :prefix-command 'Window
                    "w" 'delete-other-windows
                    "b" 'balance-windows
                    "d" 'delete-window
                    "o" 'delete-other-windows
                    "s" 'switch-window
                    "h" 'windmove-left
                    "l" 'windmove-right
                    "k" 'windmove-up
                    "j" 'windmove-down
                    )

(setq my-leader10 "C-SPC t")

(general-define-key :prefix my-leader10
                    :prefix-command 'Text
                    "t" 'delete-trailing-whitespace
                    "a" 'align-whitespace
                    "b" 'org-edit-special ;begin src
                    "d" 'text-scale-decrease
                    "e" 'org-edit-src-exit ;end src
                    "i" 'text-scale-increase
                    "u" 'untabify
                    "w" 'whitespace-mode
                    "r" 'replace-string
                    "q" 'query-replace
                    )




;; Mac iterm set `ctrl-.` to `0x03 0x64`, so the my-leader2 is
;; solely for iterm usage, and thus the code will be copied from
;; my-leader2

;; (setq my-leader11 "C-c d")
;;
;; (general-define-key :prefix my-leader11
;;                     )
