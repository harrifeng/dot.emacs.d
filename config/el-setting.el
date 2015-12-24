(provide 'el-setting)

(package-initialize)
;; check if the packages is installed; if not, install it.
(mapc
 (lambda (package)
   (or (package-installed-p package)
       (package-install package)))
 '(
   ag
   avy
   bm
   company
   company-go
   column-marker
   dockerfile-mode
   exec-path-from-shell
   go-mode
   gotest
   grep-a-lot
   groovy-mode
   git-gutter
   helm
   helm-ag
   helm-swoop
   helm-projectile
   highlight-indentation
   highlight-symbol
   htmlize
   inf-ruby
   js2-mode
   magit
   markdown-mode
   multi-term
   multiple-cursors
   projectile
   restclient
   racer
   rust-mode
   scss-mode
   simple-httpd
   skewer-mode
   slime
   smart-mode-line-powerline-theme
   solarized-theme
   spacemacs-theme
   switch-window
   toml-mode
   web-mode
   yaml-mode
   yasnippet dropdown-list
   ))

;; [A]g------------------------------------------------------------------->>
(setq ag-highlight-search t)

;; [A]uto-complete-------------------------------------------------------->>
;; (require 'auto-complete-config)
;; (ac-config-default)

;; [A]vy------------------------------------------------------------------>>
(global-set-key (kbd "C-t")          'avy-goto-char)

;; [B]m-toogle------------------------------------------------------------>>
(require 'bm)
(global-set-key (kbd "C-x m")        'bm-toggle)

;; [G]rep-a-lot----------------------------------------------------------->>
(require 'grep-a-lot)
(grep-a-lot-setup-keys)

;; [G]it-gutter----------------------------------------------------------->>
(require 'git-gutter)
(global-git-gutter-mode +1)

;; [H]elm-alike-plugins--------------------------------------------------->>
(require 'helm)
(require 'helm-projectile)
(helm-projectile-on)

(global-set-key (kbd "C-;")          'helm-toggle-visible-mark)
(global-set-key (kbd "C-x C-r")      'helm-recentf)
(global-set-key (kbd "C-x b")        'helm-mini)
(global-set-key (kbd "C-M-;")        'helm-projectile)
(global-set-key (kbd "C-M-y")        'helm-show-kill-ring)
(global-set-key (kbd "C-c C-m")      'helm-M-x)
(global-set-key (kbd "C-x C-m")      'helm-M-x)
(global-set-key (kbd "C-c s")        'helm-swoop)
(global-set-key (kbd "C-c C-s")      'helm-swoop)

;; default is vertical
;; (setq helm-swoop-split-direction 'split-window-horizontally)

;; disable pre-input
(setq helm-swoop-pre-input-function
      (lambda () ""))

;; Only works for helm-ag, not for helm-do-ag
(setq helm-ag-command-option "--all-text")
(setq helm-ag-source-type 'file-line)

;; [G]olang relative------------------------------------------------------>>
(setq gofmt-command "goimports")
(add-hook 'before-save-hook #'gofmt-before-save)
(require 'gotest)
(define-key go-mode-map (kbd "C-x f") 'go-test-current-file)
(define-key go-mode-map (kbd "C-x t") 'go-test-current-test)
(define-key go-mode-map (kbd "C-x p") 'go-test-current-project)
(define-key go-mode-map (kbd "C-x r") 'go-run)
(define-key go-mode-map (kbd "C-c r") 'go-run)

(add-hook 'go-mode-hook 'company-mode)
(add-hook 'go-mode-hook (lambda ()
  (set (make-local-variable 'company-backends) '(company-go))
  (company-mode)))

(defun node-hfeng-run ()
  (interactive)
  (save-buffer)

  (if (eq system-type 'windows-nt)
      (setq go-run-command "node.exe %s")
    (setq go-run-command "node %s"))
  (compile
   (format go-run-command
           (buffer-file-name))))

(add-hook 'js2-mode-hook
      (lambda ()
        (define-key js2-mode-map (kbd "<f9>") 'node-hfeng-run)
        ))

(defun go-hfeng-run ()
  (interactive)
  (save-buffer)

  (if (eq system-type 'windows-nt)
      (setq go-run-command "go run %s")
    (setq go-run-command "go run %s"))
  (compile
   (format go-run-command
           (buffer-file-name))))


(defun go-more-hfeng-run ()
  (interactive)
  (save-buffer)

  (if (eq system-type 'windows-nt)
      (setq go-run-command "go run *.go")
    (setq go-run-command "go run *.go"))
  (compile
   (format go-run-command
           (buffer-file-name))))


(add-hook 'go-mode-hook
      (lambda ()
        (define-key go-mode-map (kbd "<f9>") 'go-hfeng-run)
        (define-key go-mode-map (kbd "<f11>") 'go-more-hfeng-run)
        ;; (define-key go-mode-map (kbd "TAB") #'company-indent-or-complete-common)
        ))

(defun ruby-hfeng-run ()
  (interactive)
  (save-buffer)

  (if (eq system-type 'windows-nt)
      (setq ruby-run-command "ruby *.rb")
    (setq ruby-run-command "ruby *.rb"))
  (compile
   (format ruby-run-command
           (buffer-file-name))))

(add-hook 'ruby-mode-hook
          (lambda ()
            (define-key ruby-mode-map (kbd "<f9>") 'ruby-hfeng-run)))

;; [H]ighlight-indentation------------------------------------------------>>
(require 'highlight-indentation)

(add-hook 'ruby-mode-hook
          (lambda () (highlight-indentation-current-column-mode)))

(add-hook 'emacs-lisp-mode-hook
          (lambda () (highlight-indentation-current-column-mode)))

(add-hook 'python-mode-hook
          (lambda () (highlight-indentation-current-column-mode)))

;; [H]ighlight-symbol----------------------------------------------------->>
(require 'highlight-symbol)
(global-set-key [(control f3)] 'highlight-symbol)
(global-set-key [f4] 'highlight-symbol)
(global-set-key [f3] 'highlight-symbol-next)
(global-set-key [(shift f3)] 'highlight-symbol-prev)
(global-set-key [(meta f3)] 'highlight-symbol-query-replace)

;; To customize the background color
;(set-face-background 'highline-face "#333")

;; [I]nf-ruby------------------------------------------------------------->>
;; ruby shell mode

;; [J]s2-mode------------------------------------------------------------->>
(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;; [M]agit---------------------------------------------------------------->>
(global-set-key (kbd "C-M-i")        'magit-status)

;; [M]ulti-term----------------------------------------------------------->>
(require 'multi-term)

;; [P]rojectile----------------------------------------------------------->>
(projectile-global-mode)

;; to enable caching unconditionally
;; (setq projectile-enable-caching t)

;; To disable remote file exists cache that use this snippet of code:
;; (setq projectile-file-exists-remote-cache-expire nil)

;; [R]estclient]---------------------------------------------------------->>
(require 'restclient)
(add-to-list 'auto-mode-alist '("\\.http?\\'" . restclient-mode))


;; [R]ust----------------------------------------------------------------->>
(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'company-mode)


(setq company-tooltip-align-annotations t)

(defun rust-save-compile-and-run ()
  (interactive)
  (save-buffer)

  (if (locate-dominating-file (buffer-file-name) "Cargo.toml")

      (compile "cargo run")
    (if (eq system-type 'windows-nt)
        (setq rust-run-command "rustc %s && %s && rm %s.exe")
      (setq rust-run-command "rustc %s && %s && rm %s"))
    (compile
     (format rust-run-command
             (buffer-file-name)
             (file-name-sans-extension (buffer-file-name))
             (file-name-sans-extension (buffer-file-name))
             ))))

(add-hook 'rust-mode-hook
      (lambda ()
        (define-key rust-mode-map (kbd "<f9>") 'rust-save-compile-and-run)
        (define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
        ))
;; [S]kewer-mode---------------------------------------------------------->>
;; use simple-httpd inside
(add-hook 'js2-mode-hook 'skewer-mode)
(add-hook 'css-mode-hook 'skewer-css-mode)
(add-hook 'html-mode-hook 'skewer-html-mode)

;; [S]lime---------------------------------------------------------------->>
(setq inferior-lisp-program (executable-find "sbcl"))
(setq slime-contribs '(slime-fancy))


;; [S]mart-mode-line-powerline-theme-------------------------------------->>
(require 'smart-mode-line)
(setq sml/no-confirm-load-theme t)
(sml/setup)
(sml/apply-theme 'dark)
;; (sml/apply-theme 'powerline)

;; [S]witch-windows
(global-set-key (kbd "C-x o") 'switch-window)

;; [T]heme---------s------------------------------------------------------>>
(load-theme 'spacemacs-dark t)
;; (load-theme 'solarized-dark t)
;; (load-theme 'solarized-light t)
;; (load-theme 'hickey t)
;; (load-theme 'wheatgrass t)
;; (load-theme 'cyberpunk t)
;; (load-theme 'zen-and-art t)

;; [W]eb-mode------------------------------------------------------------->>
(require 'web-mode)
;; (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.[gj]sp\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))

;; [Y]asnippet------------------------------------------------------------>>
(require 'yasnippet)
(yas-global-mode 1)

(setq yas-snippet-dirs
      '(
        (concat my-emacs-path "snippets")
        ))
;; dropdown-list is needed by yasnippet
(require 'dropdown-list)
(setq yas-prompt-functions
      '(yas/dropdown-prompt
        yas/ido-prompt
        yas/x-prompt
        yas/completing-prompt
        yas/no-prompt))

;; Misc------------------------------------------------------------------->>
(require 'scss-mode)
(autoload 'scss-mode "scss-mode")
(add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))

(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)


(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; From now on, we use Ctrl + ; to expand the yasnippet
(define-key yas-minor-mode-map (kbd "<tab>") nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)
(define-key yas-minor-mode-map (kbd "C-o") 'yas-expand)
