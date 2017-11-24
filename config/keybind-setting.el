(provide 'keybind-setting)
;; git add current buffer to version control!
(global-set-key (kbd "<f2>")              'helm-all-mark-rings)
(global-set-key (kbd "<f5>")              'revert-buffer-no-confirm)
(global-set-key (kbd "<f7>")              'indent-whole)
(global-set-key (kbd "<f8>")              'toggle-flycheck-error-buffer)
(global-set-key (kbd "<f12>")             'whitespace-mode)
(global-set-key (kbd "M-;")               'qiang-comment-dwim-line)
(global-set-key (kbd "M-/")               'hippie-expand)
(global-set-key (kbd "C-j")               'newline-and-indent)
(global-set-key (kbd "M-p")               'query-replace)

(define-key helm-find-files-map (kbd "C-h") nil)
(global-set-key (kbd "C-h")               'backward-delete-char-untabify)
(global-set-key (kbd "C-o")               'copy-to-end-of-line)
(global-set-key (kbd "C-r")               'cua-scroll-down)


;; Start Iterm2 Key region----------------------------->

;; Map iterm2 may corresponding key to following code hex
;; ctrl-c-a is 0x03(ctrl) 0x61 (a) -> C-SPC
;; ctrl-c-b is 0x03(ctrl) 0x62 (b) -> C-,
;; ctrl-c-c is 0x03(ctrl) 0x63 (c) -> C-.
;; ctrl-c-d is 0x03(ctrl) 0x64 (d) -> C-;
;; ctrl-c-e is 0x03(ctrl) 0x65 (e) -> C-'
(global-set-key (kbd "C-c b")             'set-mark-command)
(global-set-key (kbd "C-,")               'set-mark-command)

;; org-mode bind C-, to another use, in such case, we can
;; use C-' as set-mark-command
(global-set-key (kbd "C-c c")             'set-mark-command)
(global-set-key (kbd "C-.")               'set-mark-command)

(global-set-key (kbd "C-c e")             'avy-goto-char)
(global-set-key (kbd "C-'")               'avy-goto-char)

(global-set-key (kbd "C-c d")             'other-window)
(global-set-key (kbd "C-;")               'other-window)


;; End Iterm2 Key region------------------------------->
(if (display-graphic-p)
    (progn
      ;; if graphic
      (setq hfeng-prefix "C-SPC")
      )
  ;; else (optional)
  (setq hfeng-prefix "C-c a")
  )


;; spacemacs-like keybinding use C-SPC as leader
(require 'general)
;; define C-. as prefix key, otherwise general will complain
(define-prefix-command 'ctl-space-map)
(global-set-key (kbd "C-SPC") 'ctl-space-map)
(global-set-key (kbd "C-c a") 'ctl-space-map) ;mac key bind in Iterm2
                                        ;(setq my-leader1 "C-c a")
(setq my-leader1 hfeng-prefix)

(general-define-key :prefix my-leader1
                    ";"   'open-shell-pwd ;Most frequently used
                    "'"   'open-eshell-pwd ;mainly used in windows
                    "RET" 'split-window-right
                    ","   'split-window-below
                    "."   'split-window-below
                    "0"   'delete-window
                    "-"   'delete-window
                    "="   'delete-window
                    "1"   'delete-other-windows
                    "2"   'split-window-below
                    "3"   'split-window-right
                    "/"   'helm-projectile-ag ; Vim use / as search
                    "d"   'delete-window
                    "k"   'kill-region             ;Kill(cut)
                    "l"   'hfeng-clear-screen-to-another-buffer ;cLear
                    "o"   'kill-ring-save          ;Opposite of kill-region
                    "u"   'cua-set-rectangle-mark
                    "y"   'helm-show-kill-ring ;detailed paste
                    "SPC" 'helm-M-x
                    ;; More tolerant
                    "C-SPC" 'helm-M-x
                    )



(setq my-leader2 (concat hfeng-prefix " b"))

(general-define-key :prefix my-leader2
                    :prefix-command 'Buffer
                    "a" 'beginning-of-buffer
                    "b" 'helm-mini
                    "d" 'kill-buffer
                    "e" 'end-of-buffer
                    "f" 'rename-buffer  ; refresh
                    "i" 'indent-whole
                    "k" 'hfeng/kill-all-region
                    "n" 'next-buffer
                    "o" 'copy-whole-buffer
                    "p" 'previous-buffer
                    "r" 'revert-buffer-no-confirm ;Revert
                    "s" 'save-buffer
                    "t" 'load-theme-buffer-local
                    "u" 'ibuffer
                    "w" 'mark-whole-buffer
                    "x" 'sudo-edit
                    )

(setq my-leader3 (concat hfeng-prefix " f"))

(general-define-key :prefix my-leader3
                    :prefix-command 'File
                    "f" 'find-file
                    "h" 'helm-find-files
                    "m" 'helm-bookmarks
                    "o" 'find-file-other-window
                    "p" 'helm-projectile ; opened files called buffer
                    )


(setq my-leader4 (concat hfeng-prefix " g"))

(general-define-key :prefix my-leader4
                    :prefix-command 'Git
                    "g" 'magit-status
                    "a" 'git-add-current-buffer-to-git ;add & stash
                    "b" 'vc-annotate                   ;blame
                    "c" 'with-editor-finish ;Commit it
                    "d" 'vc-diff
                    "e" 'with-editor-cancel ;Exit commit
                    "r" 'vc-revert
                    "s" 'git-add-current-buffer-to-git ;add & stash
                    )


(setq my-leader5 (concat hfeng-prefix " s"))

(general-define-key :prefix my-leader5
                    :prefix-command 'Search
                    "s" 'helm-swoop
                    "a" 'helm-do-ag
                    "b" 'helm-do-ag-buffers
                    "f" 'helm-do-ag-this-file
                    "p" 'helm-projectile-ag
                    "x" 'ag
                    )

(setq my-leader6 (concat hfeng-prefix " q"))

(general-define-key :prefix my-leader6
                    :prefix-command 'Query
                    "q" 'query-replace
                    "r" 'query-replace-from-region ;Only query replace can use this region as first parameter
                    )

(setq my-leader7 (concat hfeng-prefix " h"))

(general-define-key :prefix my-leader7
                    :prefix-command 'Highlight
                    ;; 1st
                    "h" 'highlight-regexp
                    "u" 'unhighlight-regexp
                    ;; 2nd
                    "s" 'highlight-symbol-at-point
                    "n" 'highlight-symbol-next
                    "p" 'highlight-symbol-prev
                    ;; 3rd
                    "r" 'hlt-highlight-region
                    "c" 'hlt-unhighlight-region ;region will clear all if no active space is set
                    "l" 'hfeng-line
                    )

(setq my-leader8 (concat hfeng-prefix " m"))

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

(setq my-leader9 (concat hfeng-prefix " w"))

(general-define-key :prefix my-leader9
                    :prefix-command 'Window
                    "w" 'delete-other-windows
                    "a" 'balance-windows
                    "b" 'split-window-and-cursor-below
                    "d" 'delete-window
                    "h" 'windmove-left
                    "j" 'windmove-down
                    "k" 'windmove-up
                    "l" 'windmove-right
                    "o" 'other-window
                    "r" 'split-window-and-cursor-right
                    "s" 'switch-window
                    )

(setq my-leader10 (concat hfeng-prefix " x"))

(general-define-key :prefix my-leader10
                    :prefix-command 'X
                    "b" 'bookmark-set
                    "c" 'restart-a-clean-emacs ; Clean Reatart
                    "d" 'text-scale-decrease   ;no work under console
                    "i" 'text-scale-increase   ;no work under console
                    "q" 'save-buffers-kill-terminal   ; Close
                    "r" 'recompile-emacs
                    "u" 'untabify
                    "x" 'regex-tool
                    "z" 'restart-emacs  ;no work under console
                    )
(setq my-leader11 (concat hfeng-prefix " a"))

(general-define-key :prefix my-leader11
                    :prefix-command 'Avy
                    ;; go to position
                    "a" 'avy-goto-char
                    "l" 'avy-goto-line
                    ;; handle copy & paste
                    "m" 'avy-move-region
                    "o" 'avy-kill-ring-save-region
                    "k" 'avy-kill-region
                    )

(setq my-leader12 (concat hfeng-prefix " v"))

(general-define-key :prefix my-leader12
                    :prefix-command 'VisualBM
                    ;; go to position
                    "v" 'bm-toggle
                    "l" 'helm-bm
                    "n" 'bm-next
                    "p" 'bm-previous
                    )

(setq my-leader13 (concat hfeng-prefix " r"))

(general-define-key :prefix my-leader13
                    :prefix-command 'Register
                    "r" 'copy-to-register ;new register content
                    "a" 'append-to-register ;back keep old
                    "i" 'insert-register
                    "l" 'helm-register  ;list register
                    "n" 'copy-to-register ;new register content
                    "p" 'prepend-to-register ;front keep old
                    )

(setq my-leader14 (concat hfeng-prefix " e"))

(general-define-key :prefix my-leader14
                    :prefix-command 'Edit
                    "e" 'query-replace
                    "a" 'align-whitespace
                    "b" 'backward-kill-line
                    "f" 'wgrep-finish-edit
                    "k" 'wgrep-abort-changes
                    "l" 'downcase-region ;M-l downcase-word
                    "m" 'wgrep-change-to-wgrep-mode ;multiple modify
                    "q" 'query-replace-from-region ;Only query replace can use this region as first parameter
                    "r" 'replace-string
                    "s" 'sort-lines
                    "t" 'delete-trailing-whitespace
                    "u" 'upcase-region  ;M-u upcase-word
                    "w" 'whack-whitespace
                    )


(setq my-leader15 (concat hfeng-prefix " j"))

(general-define-key :prefix my-leader15
                    :prefix-command 'Jump      ;jump will go to definite one buffer
                    ; these two for the jump around: godef-jump to somewhere, and j-back
                    "b" 'pop-tag-mark
                    "l" 'helm-all-mark-rings

                    "t" 'hfeng/jump-to-scratch ;tmp file
                    "m" 'hfeng/jump-to-main-go
                    "e" 'hfeng/jump-to-emacsd  ;emacs.d folder
                    "g" 'hfeng/jump-to-go      ;go folder

                    "s" 'shell
                    "n" 'goto-line      ;line number
                    "p" 'hfeng/go-to-previous-mark-in-current-buffer
                    "r" 'inf-ruby       ;ruby interpreter
                    "y" 'hfeng/jump-to-python ; python interpreter
                    )


(add-hook 'go-mode-hook
    '(lambda ()
       (local-set-key (kbd (concat hfeng-prefix " j j")) 'godef-jump)))

(add-hook 'org-mode-hook
    '(lambda ()
       (local-set-key (kbd (concat hfeng-prefix " j j")) 'org-edit-special)
       ))

(add-hook 'org-src-mode-hook ;; guessing
    '(lambda ()
       (local-set-key (kbd (concat hfeng-prefix " j f")) 'org-edit-src-exit)
       ))

(add-hook 'picture-mode-hook ;; artist mode use picture-mode as major mode
    '(lambda ()
       (local-set-key (kbd (concat hfeng-prefix " j f")) 'org-edit-src-exit)
       ))

(setq my-leader16 (concat hfeng-prefix " t"))

(general-define-key :prefix my-leader16
                    :prefix-command 'Toggle
                    "t" 'hs-toggle-hiding
                    "a" 'artist-mode
                    "e" 'toggle-flycheck-error-buffer
                    "f" 'toggle-frame-fullscreen
                    "h" 'hs-hide-all
                    "i" 'image-toggle-display
                    "l" 'toggle-truncate-lines
                    "r" 'read-only-mode
                    "s" 'hs-show-all
                    "w" 'whitespace-mode
                    )


(setq my-leader17 (concat hfeng-prefix " i"))

(general-define-key :prefix my-leader17
                    :prefix-command 'Info
                    "i" 'shell-command
                    "e" 'eval-last-sexp
                    "k" 'describe-key
                    "s" 'yas/describe-tables ;short key
                    "v" 'describe-variable
                    "y" 'hfeng/shell-command-to-current-pos
                    )

(setq my-leader18 (concat hfeng-prefix " n"))

(general-define-key :prefix my-leader18
                    :prefix-command 'Not-open
                    "n" 'hfeng/bk-kill-buffers
                    "c" 'hfeng/bk-kill-buffers-compilation
                    "s" 'hfeng/bk-kill-buffers-shell-pwd
                    )

(setq my-leader19 (concat hfeng-prefix " p"))

(general-define-key :prefix my-leader19
                    :prefix-command 'PerMode
                    "p" 'hfeng/previous-buffer-same-mode
                    "n" 'hfeng/next-buffer-same-mode
                    )

(add-hook 'go-mode-hook
    '(lambda ()
       (local-set-key (kbd (concat hfeng-prefix " p a")) 'helm-go-package))) ;add package

(add-hook 'go-mode-hook
    '(lambda ()
       (local-set-key (kbd (concat hfeng-prefix " p d")) 'godoc-at-point)))

(add-hook 'go-mode-hook
    '(lambda ()
       (local-set-key (kbd (concat hfeng-prefix " p g")) 'go-guru-describe)))

(add-hook 'python-mode-hook
    '(lambda ()
       (local-set-key (kbd (concat hfeng-prefix " p f")) 'elpy-autopep8-fix-code)))

(add-hook 'python-mode-hook
    '(lambda ()
       (local-set-key (kbd (concat hfeng-prefix " p v")) 'venv-workon))) ;choose python version

(add-hook 'python-mode-hook
    '(lambda ()
       (local-set-key (kbd (concat hfeng-prefix " p r")) 'run-python))) ;repl

(add-hook 'ess-mode-hook
    '(lambda ()
       (local-set-key (kbd (concat hfeng-prefix " p l")) 'ess-load-file)))
(add-hook 'inferior-python-mode-hook
    '(lambda ()
       (local-set-key (kbd (concat hfeng-prefix " p v")) 'venv-workon))) ;choose python version

(defun plantuml-preview-with-prefix-arg ()
  (interactive)
  (setq current-prefix-arg '(4)) ; C-u
  (call-interactively 'plantuml-preview))

(defun hfeng/go-to-previous-mark-in-current-buffer ()
  (interactive)
  (setq current-prefix-arg '(4)) ; C-u
  (call-interactively 'cua-set-mark))

(add-hook 'plantuml-mode-hook
    '(lambda ()
       (local-set-key (kbd (concat hfeng-prefix " p v")) 'plantuml-preview-with-prefix-arg)))

(defun hfeng/shell-command-to-current-pos ()
  (interactive)
  (setq current-prefix-arg '(4)) ; C-u
  (call-interactively 'shell-command))
