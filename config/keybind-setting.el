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


;; spacemacs-like keybinding use C-SPC as leader
(require 'general)
;; define C-. as prefix key, otherwise general will complain
(define-prefix-command 'ctl-space-map)
(global-set-key (kbd "C-SPC") 'ctl-space-map)
(global-set-key (kbd "C-c a") 'ctl-space-map) ;mac key bind in Iterm2
(setq my-leader1 "C-SPC")

(general-define-key :prefix my-leader1
                    ";"   'open-shell-pwd ;Most frequently used
                    "'"   'open-eshell-pwd ;mainly used in windows
                    "<return>" 'open-shell-pwd
                    ","   'split-window-and-cursor-below
                    "."   'split-window-and-cursor-right ; `.` is right of `,`
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



(setq my-leader2 "C-SPC b")

(general-define-key :prefix my-leader2
                    :prefix-command 'Buffer
                    "b" 'helm-mini
                    "a" 'beginning-of-buffer
                    "e" 'end-of-buffer
                    "i" 'indent-whole
                    "k" 'kill-buffer
                    "n" 'next-buffer
                    "o" 'copy-whole-buffer
                    "p" 'previous-buffer
                    "r" 'revert-buffer-no-confirm ;Revert
                    "s" 'save-buffer
                    "u" 'ibuffer
                    "w" 'mark-whole-buffer
                    "t" 'load-theme-buffer-local
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
                    "b" 'vc-annotate                   ;blame
                    "d" 'vc-diff
                    "c" 'with-editor-finish ;Commit it
                    "e" 'with-editor-cancel ;Exit commit
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
                    "x" 'ag
                    )

(setq my-leader6 "C-SPC q")

(general-define-key :prefix my-leader6
                    :prefix-command 'Quit
                    "q" 'save-buffers-kill-terminal   ;Exit
                    "r" 'restart-emacs
                    "c" 'restart-a-clean-emacs ;no desktop buffer saved
                    )

(setq my-leader7 "C-SPC h")

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
                    "a" 'balance-windows
                    "b" 'split-window-and-cursor-below
                    "r" 'split-window-and-cursor-right
                    "d" 'delete-window
                    "o" 'other-window
                    "s" 'switch-window
                    "h" 'windmove-left
                    "l" 'windmove-right
                    "k" 'windmove-up
                    "j" 'windmove-down
                    )

(setq my-leader10 "C-SPC x")

(general-define-key :prefix my-leader10
                    :prefix-command 'X
                    "b" 'bookmark-set
                    "d" 'text-scale-decrease
                    "i" 'text-scale-increase
                    "u" 'untabify
                    "r" 'recompile-emacs
                    )
(setq my-leader11 "C-SPC a")

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

(setq my-leader12 "C-SPC v")

(general-define-key :prefix my-leader12
                    :prefix-command 'VisualBM
                    ;; go to position
                    "v" 'bm-toggle
                    "l" 'helm-bm
                    "n" 'bm-next
                    "p" 'bm-previous
                    )

(setq my-leader13 "C-SPC r")

(general-define-key :prefix my-leader13
                    :prefix-command 'Register
                    "r" 'append-to-register
                    "l" 'helm-register  ;list register
                    "n" 'copy-to-register ;new register content
                    "a" 'append-to-register
                    "p" 'prepend-to-register
                    "i" 'insert-register
                    )

(setq my-leader14 "C-SPC e")

(general-define-key :prefix my-leader14
                    :prefix-command 'Edit
                    "a" 'align-whitespace
                    "b" 'backward-kill-line
                    "u" 'upcase-region  ;M-u upcase-word
                    "l" 'downcase-region ;M-l downcase-word
                    "w" 'whack-whitespace
                    "r" 'replace-string
                    "t" 'delete-trailing-whitespace
                    "q" 'query-replace-from-region ;Only query replace can use this region as first parameter
                    "s" 'sudo-edit
                    "e" 'sudo-edit
                    "m" 'wgrep-change-to-wgrep-mode ;multiple modify
                    "f" 'wgrep-finish-edit
                    "k" 'wgrep-abort-changes
                    )


(setq my-leader15 "C-SPC j")

(general-define-key :prefix my-leader15
                    :prefix-command 'Jump      ;jump will go to definite one buffer
                    ; these two for the jump around: godef-jump to somewhere, and j-back
                    "b" 'pop-tag-mark
                    "l" 'helm-all-mark-rings

                    "t" 'hfeng/jump-to-scratch ;tmp file
                    "m" 'hfeng/jump-to-message ;message file
                    "s" 'shell
                    "g" 'goto-line      ;line number
                    "n" 'cua-set-mark
                    "p" 'hfeng/go-to-previous-mark-in-current-buffer
                    "r" 'inf-ruby       ;ruby interpreter
                    "y" 'hfeng/jump-to-python ; python interpreter
                    )


(add-hook 'go-mode-hook
    '(lambda ()
       (local-set-key (kbd "C-SPC j j") 'godef-jump)))

(add-hook 'org-mode-hook
    '(lambda ()
       (local-set-key (kbd "C-SPC j j") 'org-edit-special)
       ))

(add-hook 'org-src-mode-hook ;; guessing
    '(lambda ()
       (local-set-key (kbd "C-SPC j e") 'org-edit-src-exit)
       ))




(setq my-leader16 "C-SPC t")

(general-define-key :prefix my-leader16
                    :prefix-command 'Toggle
                    "t" 'hs-toggle-hiding
                    "h" 'hs-hide-all
                    "i" 'image-toggle-display
                    "s" 'hs-show-all
                    "a" 'artist-mode
                    "r" 'read-only-mode
                    "l" 'toggle-truncate-lines
                    "f" 'toggle-frame-fullscreen
                    "e" 'toggle-flycheck-error-buffer
                    "w" 'whitespace-mode
                    )


(setq my-leader17 "C-SPC i")

(general-define-key :prefix my-leader17
                    :prefix-command 'Info
                    "i" 'shell-command
                    "v" 'describe-variable
                    "k" 'yas/describe-tables
                    "e" 'eval-last-sexp
                    )


(setq my-leader18 "C-SPC p")

(general-define-key :prefix my-leader18
                    :prefix-command 'PerMode
                    "p" 'hfeng/previous-buffer-same-mode
                    "n" 'hfeng/next-buffer-same-mode
                    )

(add-hook 'go-mode-hook
    '(lambda ()
       (local-set-key (kbd "C-SPC p a") 'helm-go-package))) ;add package

(add-hook 'go-mode-hook
    '(lambda ()
       (local-set-key (kbd "C-SPC p d") 'godoc-at-point)))


(add-hook 'python-mode-hook
    '(lambda ()
       (local-set-key (kbd "C-SPC p f") 'elpy-autopep8-fix-code)))

(add-hook 'python-mode-hook
    '(lambda ()
       (local-set-key (kbd "C-SPC p v") 'venv-workon))) ;choose python version

(add-hook 'python-mode-hook
    '(lambda ()
       (local-set-key (kbd "C-SPC p r") 'run-python))) ;repl

(add-hook 'ess-mode-hook
    '(lambda ()
       (local-set-key (kbd "C-SPC p l") 'ess-load-file)))
(add-hook 'inferior-python-mode-hook
    '(lambda ()
       (local-set-key (kbd "C-SPC p v") 'venv-workon))) ;choose python version

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
       (local-set-key (kbd "C-SPC p v") 'plantuml-preview-with-prefix-arg)))
