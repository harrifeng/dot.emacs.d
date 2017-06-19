(provide 'keybind-setting)

(global-set-key (kbd "C-x o")             'switch-window)
(global-set-key (kbd "C-c C-c")           'comment-box)
(global-set-key (kbd "C-x C-k")           'kill-region)
(global-set-key (kbd "C-c C-k")           'kill-region)

;; Group key bind-------------------------------->
;; Mac iterm set `ctrl-;` to `0x03 0x6f`
(global-set-key (kbd "C-c o")             'other-window)
;; Windows set `ctrl-;` to `ctrl-1`
(global-set-key (kbd "C-1")               'other-window)
(global-set-key (kbd "C-;")               'other-window)

;; Group key bind-------------------------------->
;; iterm set `ctrl-=` to `0x03 0x61`
;; C-= can be used later
(global-set-key (kbd "C-x C-y")           'kill-ring-save)
(global-set-key (kbd "C-c C-y")           'kill-ring-save)
(global-set-key (kbd "C-x C-u")           'backward-kill-line)
(global-set-key (kbd "C-x C-b")           'ibuffer)
(global-set-key (kbd "C-x j")             'join-line)
;; git add current buffer to version control!
(global-set-key (kbd "C-c i")             'git-add-current-buffer)
(global-set-key (kbd "C-c C-i")           'git-add-current-buffer)
;;(global-set-key (kbd "C-w")               'align-whitespace)
(global-set-key (kbd "C-c r")             'revert-buffer)
(global-set-key (kbd "<f5>")              'revert-buffer-no-confirm)
(global-set-key (kbd "<f2>")              'helm-global-mark-ring)
(global-set-key (kbd "<f7>")              'indent-whole)
(global-set-key (kbd "<f12>")             'whitespace-mode)
(global-set-key (kbd "<C-f11>")           'toggle-tool-bar-mode-from-frame)
(global-set-key (kbd "M-;")               'qiang-comment-dwim-line)
(global-set-key (kbd "M-/")               'hippie-expand)
(global-set-key (kbd "C-j")               'newline-and-indent)
(global-set-key (kbd "M-p")               'query-replace)
(global-set-key (kbd "C-x C-g")           'helm-global-mark-ring)
(global-set-key (kbd "C-c C-g")           'helm-global-mark-ring)
;; iterm set `ctrl-=` to `0x03 0x0d`
(global-set-key (kbd "C-c RET")           'mark-whole-buffer)
(global-set-key (kbd "<C-return>")        'mark-whole-buffer)
(global-set-key (kbd "C-2")               'set-mark-command)
(global-set-key (kbd "C-,")               'set-mark-command)
(global-set-key (kbd "C-c h")             'windmove-left)
(global-set-key (kbd "C-c l")             'windmove-right)
(global-set-key (kbd "C-c j")             'windmove-down)
(global-set-key (kbd "C-c k")             'windmove-up)
;; ctrl-c-ctrl-j is go to definition, so ctrl-c-ctrl-space is handy
(global-set-key (kbd "C-c C-SPC")         'pop-global-mark)
;; rename the buffer, the start another shell
(global-set-key (kbd "C-c c")             'rename-buffer)

(keyboard-translate ?\C-t ?\C-x)
(keyboard-translate ?\C-x ?\C-t)

;; we still need this
;;(global-set-key (kbd "C-h")               'backward-delete-char-untabify)
