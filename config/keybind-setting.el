(provide 'keybind-setting)
;; git add current buffer to version control!
(global-set-key (kbd "<f2>")              'helm-global-mark-ring)
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

(define-key yas-minor-mode-map (kbd "C-o") 'copy-line)

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
(global-set-key (kbd "C-c e")             'set-mark-command)
(global-set-key (kbd "C-'")               'set-mark-command)

(global-set-key (kbd "C-c c")             'avy-goto-char)
(global-set-key (kbd "C-.")               'avy-goto-char)

(global-set-key (kbd "C-c d")             'other-window)
(global-set-key (kbd "C-;")               'other-window)


;; End Iterm2 Key region------------------------------->
