(provide 'el-setting)

;; [A]g------------------------------------------------------------------->>
(setq ag-highlight-search t)

;; [A]uto-highlight-symbol------------------------------------------------>>
(global-auto-highlight-symbol-mode t)
(ahs-chrange-display)

;; [A]vy------------------------------------------------------------------>>
(global-set-key (kbd "C-t")          'avy-goto-char)

;; [B]ack-button---------------------------------------------------------->>
;; (back-button-mode)

;; [B]m-toogle------------------------------------------------------------>>
(setq-default bm-repository-file "~/.emacs.d/.bm-repository")
(setq bm-restore-repository-on-load t)

(require 'bm)

(setq-default bm-repository-size 1000)
(setq-default bm-persistent-face
  '(:foreground "#2e3043" :background "#f9b529"))
(setq-default bm-buffer-persistence t)
(setq-default bm-cycle-all-buffers nil)
(setq-default bm-recenter t)

(defun bm--turn-on-cycle-buffers ()
  (interactive)
  (message "bookmark cycle buffers: (on)")
  (setq-default bm-cycle-all-buffers t))

(defun bm--turn-off-cycle-buffers ()
  (interactive)
  (message "bookmark cycle buffers: (off)")
  (setq-default bm-cycle-all-buffers nil))

(add-hook 'after-init-hook 'bm-repository-load)
(add-hook 'find-file-hooks 'bm-buffer-restore)
(add-hook 'kill-buffer-hook 'bm-buffer-save)
(add-hook 'kill-emacs-hook '(lambda nil
                              (bm-buffer-save-all)
                              (bm-repository-save)))
(add-hook 'after-save-hook 'bm-buffer-save)
(add-hook 'after-revert-hook 'bm-buffer-restore)
(add-hook 'vc-before-checkin-hook 'bm-buffer-save)

;; [E]lpy----------------------------------------------------------------->>
(elpy-enable)
(remove-hook 'elpy-modules 'elpy-module-flymake)
;; [G]it-gutter----------------------------------------------------------->>
(require 'git-gutter)
(global-git-gutter-mode +1)

;; [H]elm-alike-plugins--------------------------------------------------->>
(require 'helm-config)

(setq helm-prevent-escaping-from-minibuffer t
      helm-bookmark-show-location t
      helm-display-header-line nil
      helm-split-window-in-side-p t
      helm-always-two-windows t
      helm-echo-input-in-header-line t
      helm-imenu-execute-action-at-once-if-one nil
      helm-ag-insert-at-point 'symbol
      helm-org-format-outline-path t)

(require 'helm-projectile)
(helm-projectile-on)

(require 'helm-swoop)
(setq helm-swoop-split-direction 'split-window-vertically)
(setq helm-swoop-split-with-multiple-windows t)
(setq helm-swoop-move-to-line-cycle t)
;; Only works for helm-ag, not for helm-do-ag
;;Three setting should all together to config, otherwise it will fail
(setq helm-ag-base-command "ag --nocolor --nogroup --ignore-case")
(setq helm-ag-command-option "--all-text")
(setq helm-ag-source-type 'file-line)

;; [G]olang relative------------------------------------------------------>>
(setq gofmt-command "goimports")
(require 'gotest)
(define-key go-mode-map (kbd "C-c r") 'go-run)

(add-hook 'go-mode-hook 'company-mode)
(add-hook 'go-mode-hook
          (lambda ()
            (add-hook 'before-save-hook 'gofmt-before-save)
            (setq tab-width 8)
            (setq indent-tabs-mode 1)
            (set (make-local-variable 'company-backends) '(company-go))
            (company-mode)))

(defun my-make-room-for-new-compilation-buffer ()
  "Renames existing *compilation* buffer to something unique so
      that a new compilation job can be run."
  (interactive)
  (let ((cbuf (get-buffer "*compilation*"))
        (more-cbufs t)
        (n 1)
        (new-cbuf-name ""))
    (when cbuf
      (while more-cbufs
        (setq new-cbuf-name (format "*compilation%d*" n))
        (setq n (1+ n))
        (setq more-cbufs (get-buffer new-cbuf-name)))
      (with-current-buffer cbuf
        (rename-buffer new-cbuf-name)))))

(defun node-hfeng-run ()
  (interactive)
  (save-buffer)

  (if (eq system-type 'windows-nt)
      (setq go-run-command "node.exe %s")
    (setq go-run-command "node %s"))
  (compile
   (format go-run-command
           (buffer-file-name)))
  (my-make-room-for-new-compilation-buffer)
  )


(defun node-hfeng-test ()
  (interactive)
  (save-buffer)

  (if (eq system-type 'windows-nt)
      (setq go-run-command "mocha.exe %s")
    (setq go-run-command "mocha %s"))
  (compile
   (format go-run-command
           (buffer-file-name)))
  (my-make-room-for-new-compilation-buffer)
  )

(add-hook 'js2-mode-hook 'company-mode)
(autoload 'tern-mode "tern.el" nil t)
(add-hook 'js2-mode-hook
          (lambda ()
            (tern-mode t)
            ))
(add-to-list 'company-backends 'company-tern)


(add-hook 'js2-mode-hook
          (lambda ()
            ;; (define-key js2-mode-map (kbd "<f8>") 'node-hfeng-test)
            (define-key js2-mode-map (kbd "<f9>") 'node-hfeng-run)
            (define-key js2-mode-map (kbd "C-m") 'node-hfeng-run)
            ))

(setq js2-strict-missing-semi-warning nil)

(defun go-hfeng-run ()
  (interactive)
  (save-buffer)

  (if (eq system-type 'windows-nt)
      (setq go-run-command "go run %s")
    (setq go-run-command "go run %s"))
  (compile
   (format go-run-command
           (buffer-file-name)))
  (my-make-room-for-new-compilation-buffer)
  )


(defun go-more-hfeng-run ()
  (interactive)
  (save-buffer)

  (if (eq system-type 'windows-nt)
      (setq go-run-command "go run *.go")
    (setq go-run-command "go run *.go"))
  (compile
   (format go-run-command
           (buffer-file-name)))
  (my-make-room-for-new-compilation-buffer)
  )


(add-hook 'go-mode-hook
          (lambda ()
            (define-key go-mode-map (kbd "<f9>") 'go-hfeng-run)
            (define-key go-mode-map (kbd "C-m") 'go-hfeng-run)
            ;; (define-key go-mode-map (kbd "TAB") #'company-indent-or-complete-common)
            ))

(defun ruby-hfeng-run ()
  (interactive)
  (save-buffer)

  (if (eq system-type 'windows-nt)
      (setq ruby-run-command "ruby %s")
    (setq ruby-run-command "ruby %s"))
  (compile
   (format ruby-run-command
           (buffer-file-name)))
  (my-make-room-for-new-compilation-buffer)
  )

(add-hook 'ruby-mode-hook
          (lambda ()
            (define-key ruby-mode-map (kbd "<f9>") 'ruby-hfeng-run)
            (define-key ruby-mode-map (kbd "C-m") 'ruby-hfeng-run)
            ))


(defun python-hfeng-run ()
  (interactive)
  (save-buffer)

  (if (eq system-type 'windows-nt)
      (setq python-run-command "python %s")
    (setq python-run-command "python %s"))
  (compile
   (format python-run-command
           (buffer-file-name)))
  (my-make-room-for-new-compilation-buffer)
  )

(defun python-hfeng-test ()
  (interactive)
  (save-buffer)

  (if (eq system-type 'windows-nt)
      (setq python-run-command "python -m unittest %s")
    (setq python-run-command "python -m unittest %s"))
  (compile
   (format python-run-command
           (file-name-base (buffer-file-name))))
  (my-make-room-for-new-compilation-buffer)
  )

(add-hook 'python-mode-hook
          (lambda ()
            ;; (define-key python-mode-map (kbd "<f8>") 'python-hfeng-test)
            (define-key python-mode-map (kbd "<f9>") 'python-hfeng-run)
            (define-key python-mode-map (kbd "C-m") 'python-hfeng-run)
            ))


;; [H]ighlight-indentation------------------------------------------------>>
(add-hook 'ruby-mode-hook
          (lambda () (highlight-indentation-current-column-mode)))

(add-hook 'emacs-lisp-mode-hook
          (lambda () (highlight-indentation-current-column-mode)))

(add-hook 'python-mode-hook
          (lambda () (highlight-indentation-current-column-mode)))

;; [H]ighlight-symbol----------------------------------------------------->>
(global-set-key [(control f3)] 'highlight-symbol)
(global-set-key [f4] 'highlight-symbol)
(global-set-key [f3] 'highlight-symbol-next)
(global-set-key [(shift f3)] 'highlight-symbol-prev)
(global-set-key [(meta f3)] 'highlight-symbol-query-replace)

;; [J]s2-mode------------------------------------------------------------->>
;; (autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . rjsx-mode))
(setq js-indent-level 2)

;; [M]agit---------------------------------------------------------------->>
(cond ((file-readable-p "/usr/local/git/bin/git")
       (setq magit-git-executable "/usr/local/git/bin/git"))
      ((file-readable-p "/usr/local/bin/git")
       (setq magit-git-executable "/usr/local/bin/git"))
      ((file-readable-p "/usr/bin/git")
       (setq magit-git-executable "/usr/bin/git"))
      (t (setq magit-git-executable "git")))


;; [M]aterial theme------------------------------------------------------->>
(load-theme 'monokai t)

;; [P]rojectile----------------------------------------------------------->>
(projectile-global-mode)

;; to enable caching unconditionally
(setq projectile-enable-caching t)

;; To disable remote file exists cache that use this snippet of code:
(setq projectile-file-exists-remote-cache-expire nil)

;; [R]ainbow-delimiters--------------------------------------------------->>
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; [R]estclient]---------------------------------------------------------->>
(require 'restclient)
(add-to-list 'auto-mode-alist '("\\.http?\\'" . restclient-mode))

;; cpp
(defun cpp-save-compile-and-run ()
  (interactive)
  (save-buffer)
  (if (eq system-type 'windows-nt)
      (setq cpp-run-command "g++ -o %s %s && %s && rm %s.exe")
    (setq cpp-run-command "g++ -o %s %s && %s && rm %s"))
  (compile
   (format cpp-run-command
           (file-name-sans-extension (buffer-file-name))
           (buffer-file-name)
           (file-name-sans-extension (buffer-file-name))
           (file-name-sans-extension (buffer-file-name))
           ))
  (my-make-room-for-new-compilation-buffer)
  )

(add-hook 'c++-mode-hook
          (lambda ()
            (define-key c++-mode-map (kbd "<f9>") 'cpp-save-compile-and-run)
            (define-key c++-mode-map (kbd "C-m") 'cpp-save-compile-and-run)
            (define-key c++-mode-map (kbd "TAB") #'company-indent-or-complete-common)
            ))

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
             ))
    (my-make-room-for-new-compilation-buffer)
    ))

(add-hook 'rust-mode-hook
          (lambda ()
            (define-key rust-mode-map (kbd "<f9>") 'rust-save-compile-and-run)
            (define-key rust-mode-map (kbd "C-m") 'rust-save-compile-and-run)
            (define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
            ))

;; [S]mart-mode-line------------------------------------------------------>>
(setq sml/no-confirm-load-theme t)
(sml/setup)
(sml/apply-theme 'dark)

;; [S]imple-httpd--------------------------------------------------------->>
(setq httpd-port 1234)
;; [S]witch-windows

;; [V]isual-regexp-------------------------------------------------------->>
(define-key global-map (kbd "C-c C-r") 'vr/replace)
(define-key global-map (kbd "C-c C-q") 'vr/query-replace) ;awesome
;; if you use multiple-cursors, this is for you:
(define-key global-map (kbd "C-c m") 'vr/mc-mark)

;; [W]eb-mode------------------------------------------------------------->>
(require 'web-mode)

(setq web-mode-markup-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq web-mode-code-indent-offset 2)


(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[gj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . rjsx-mode))

;; (setq web-mode-content-types-alist
;;       '(("jsx" . "\\.js[x]?\\'")))

;; [W]grep---------------------------------------------------------------->>
(setq wgrep-auto-save-buffer t)
(setq wgrep-change-readonly-file t)
;; [W]ich-key-mode-------------------------------------------------------->>
(which-key-mode)

;; Set the time delay (in seconds) for the which-key popup to appear. A value of
;; zero might cause issues so a non-zero value is recommended.
(setq which-key-idle-delay 2.0)

;; [Y]asnippet------------------------------------------------------------>>
(defconst my-snippet-path (concat my-emacs-path "snippets"))
(setq yas-snippet-dirs
      '(my-snippet-path)
      )
(yas-global-mode 1)

;; dropdown-list is needed by yasnippet
(setq yas-prompt-functions
      '(yas/dropdown-prompt
        yas/ido-prompt
        yas/x-prompt
        yas/completing-prompt
        yas/no-prompt))

;; Misc------------------------------------------------------------------->>
(autoload 'scss-mode "scss-mode")
(add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))

(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
