(provide 'helpfunc-setting)

(require 'cl)
(defun hfeng/bk-kill-buffers (regexp)
  "Kill buffers matching REGEXP without asking for confirmation."
  (interactive "sKill buffers matching this regular expression: ")
  (flet ((kill-buffer-ask (buffer) (kill-buffer buffer)))
    (kill-matching-buffers regexp)))

(defun hfeng/bk-kill-buffers-compilation ()
  (interactive)
  (hfeng/bk-kill-buffers "compilation*")
  )

(defun hfeng/bk-kill-buffers-shell-pwd ()
  (interactive)
  (hfeng/bk-kill-buffers "SHELL-")
  )


(defun buffer-same-mode (change-buffer-fun)
  (let ((current-mode major-mode)
        (next-mode nil))
    (while (not (eq next-mode current-mode))
      (funcall change-buffer-fun)
      (setq next-mode major-mode))))

(defun hfeng/previous-buffer-same-mode ()
  (interactive)
  (buffer-same-mode #'previous-buffer))

(defun hfeng/next-buffer-same-mode ()
  (interactive)
  (buffer-same-mode #'next-buffer))

(defun hfeng/jump-to-python()
  "switch to *python*"
  (interactive)
  (run-python)
  (switch-to-buffer-other-window "*Python*")
  )

(defun hfeng/jump-to-scratch ()
  "switch to *scratch* buffer even it doesn't exist "
  (interactive)
  (switch-to-buffer-other-window "*scratch*")
  )

(defun hfeng/jump-to-message ()
  (interactive)
  (switch-to-buffer-other-window "*Messages*")
  )

(defun hfeng/jump-to-emacsd ()
  (interactive)
  (find-file "~/.emacs.d")
  )

(defun hfeng/jump-to-go ()
  (interactive)
  (find-file "~/go/src/github.com/harrifeng/playground/")
  )

(defun copy-whole-buffer ()
  "kill the whole buffer"
  (interactive)
  (kill-ring-save (point-min)
                  (point-max))
  )

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

(defun hfeng-line (arg)
  "Copy lines (as many as prefix argument) in the kill ring.
      Ease of use features:
      - Move to start of next line.
      - Appends the copy on sequential calls.
      - Use newline as last char even on the last line of the buffer.
      - If region is active, copy its lines."
  (interactive "p")

  (let ((beg (line-beginning-position))
        (end (line-end-position arg)))
    (when mark-active
      (if (> (point) (mark))
          (setq beg (save-excursion (goto-char (mark)) (line-beginning-position)))
        (setq end (save-excursion (goto-char (mark)) (line-end-position)))))
    (setq face
          `((background-color . ,"yellow") (foreground-color . ,hlt-auto-face-foreground)))
    (hlt-highlight-lines beg end face "Haha"))
  (if (and arg (not (= 1 arg))) (message "%d lines copied" arg)))



(defun split-window-and-cursor-right ()
  "Open a windows and cursor on the right"
  (interactive)
  (split-window-right)
  (other-window 1)
  (helm-mini)
  )

(defun split-window-and-cursor-below ()
  "Open a windows and cursor on the below"
  (interactive)
  (split-window-below)
  (other-window 1)
  (helm-mini)
  )

(defun revert-buffer-no-confirm ()
  "Revert buffer without confirmation."
  (interactive)
  (revert-buffer :ignore-auto :noconfirm))

(defun git-add-current-buffer-to-git ()
  "call 'git add [current-buffer]' revert-buffer afterwards, no gitgutter changes show"
  (interactive)
  (delete-trailing-whitespace)
  (save-buffer)
  (let* ((buffile (buffer-file-name))
         (output (shell-command-to-string
                  (concat "git add " (buffer-file-name))))
         )
    (message (if (not (string= output ""))
                 output
               (concat "Added " buffile))))

  (sleep-for 0 200)                     ;sleep for 0.2 seconds,  `git add` need time
  (revert-buffer :ignore-auto :noconfirm) ; revert buffer, no git gutter show, as they're indexed
  )

(defun set-china-mirror()
  (interactive)
  (setq package-archives '(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                           ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
                           ("elpy" . "https://jorgenschaefer.github.io/packages/")
                           ))
  (package-initialize)
  )

(defun set-no-mirror()
  (interactive)
  (setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                           ("melpa" . "http://melpa.milkbox.net/packages/")
                           ("elpy" . "https://jorgenschaefer.github.io/packages/")
                           ))
  (package-initialize)
  )

(defun install-my-packages()
  (interactive)
  ;; check if the packages is installed; if not, install it.
  (mapc
   (lambda (package)
     (or (package-installed-p package)
         (package-install package)))
   '(
     ag
     auto-highlight-symbol
     avy
     bm
     company
     company-go
     company-tern
     column-marker
     darcula-theme
     dockerfile-mode
     docker-tramp
     elnode
     elpy
     ess
     exec-path-from-shell
     flycheck
     general
     auto-highlight-symbol
     go-mode
     go-guru
     gotest
     groovy-mode
     gist
     git-gutter
     haml-mode
     helm
     helm-ag
     helm-bm
     helm-go-package
     helm-projectile
     helm-swoop
     highlight-indentation
     highlight-symbol
     htmlize
     inf-ruby
     js2-mode
     load-theme-buffer-local
     magit
     markdown-mode
     wgrep-ag
     material-theme
     multiple-cursors
     nodejs-repl
     plantuml-mode
     projectile
     replace-from-region
     regex-tool
     restart-emacs
     rjsx-mode
     rubocop
     scss-mode
     simple-httpd
     smart-mode-line
     solarized-theme
     sudo-edit
     switch-window
     tern
     toml-mode
     virtualenvwrapper
     visual-regexp
     web-beautify
     web-mode
     which-key
     yaml-mode
     yasnippet
     )))

(defun local-proxy()
  (interactive)
  (setq url-proxy-services '(("http" . "127.0.0.1:8123")))
  )
(defun no-proxy()
  (interactive)
  (setq url-proxy-services nil)
  )

(defun whack-whitespace (arg)
  "Delete all white space from point to the next word.  With prefix ARG
    delete across newlines as well.  The only danger in this is that you
    don't have to actually be at the end of a word to make it work.  It
    skips over to the next whitespace and then whacks it all to the next
    word."
  (interactive "P")
  (let ((regexp (if arg "[ \t\n]+" "[ \t]+")))
    (re-search-forward regexp nil t)
    (replace-match "" nil nil)))

;; Translate the problematic keys to the function key Hyper,
;; then bind this to the desired ctrl-i behavior

(defun shell-mode-auto-rename-buffer (text)
  (if (eq major-mode 'shell-mode)
      (rename-buffer  (concat "*Shell: "
                              (concat default-directory "*")) t)))

(defun hfeng-clear-screen-to-another-buffer ()
  "This function delete content to another bufer"
  (interactive)

  (if (file-exists-p (concat (getenv "HOME") "/github/shell-log/"))
      (defconst my-shell-log-path
        (concat
         (getenv "HOME")
         "/github/shell-log/"
         (format-time-string "%Y-%m/")
         ))
    (defconst my-shell-log-path
      (concat (getenv "HOME")
              (format-time-string "/%Y-%m/")
              )))
  (unless (file-exists-p my-shell-log-path)
    (make-directory my-shell-log-path))

  (setq log-file-name
        (concat my-shell-log-path
                (format-time-string "%Y-%m-%d-")
                (replace-regexp-in-string "/" "-" (symbol-name system-type))
                ".txt"
                ))
  (save-excursion
    (mark-whole-buffer)
    (write-region (format-time-string
                   "\nStart From %Y-%m-%d %H:%M:%S------------------------------------------>\n")
                  nil log-file-name 'append)
    (append-to-file (mark) (point) log-file-name)
    (write-region (format-time-string
                   "\nFinish At %Y-%m-%d %H:%M:%S-------------------------------------------<\n")
                  nil log-file-name 'append)
    ))

(defun my-clear ()
  (interactive)
  (let ((comint-buffer-maximum-size 0))
    (comint-truncate-buffer)))

(defun hfeng-clear (&optional prefix)
  "Move the line containing point to the top of the window.
   With PREFIX, move the line containing point to line PREFIX of the window."
  (interactive "P")
  (recenter (or prefix 0)))

(defun my-shell-mode-hook ()
  (local-set-key (kbd "C-SPC l")
                 (lambda nil
                   (interactive)
                   (hfeng-clear-screen-to-another-buffer)
                   (my-clear))))

(defun my-shell-mode-hook-term ()
  (local-set-key (kbd "C-c a l")
                 (lambda nil
                   (interactive)
                   (hfeng-clear-screen-to-another-buffer)
                   (my-clear))))


;;clean all the buffer content
(add-hook 'shell-mode-hook 'my-shell-mode-hook)
(add-hook 'shell-mode-hook 'my-shell-mode-hook-term)


;;eshell clear the screen
(defun eshell/cls ()
  "Clears the shell buffer ala Unix's clear or DOS' cls"
  (interactive)
  (hfeng-clear-screen-to-another-buffer)
  ;; the shell prompts are read-only, so clear that for the duration
  (let ((inhibit-read-only t))
    ;; simply delete the region
    (delete-region (point-min) (point-max))))

;; Alt+; as comment advanced;
(defun qiang-comment-dwim-line (&optional arg)
  (interactive "*P")
  (comment-normalize-vars)
  (if (and (not (region-active-p))
           (not (looking-at "[ \t]*$")))
      (comment-or-uncomment-region (line-beginning-position)
                                   (line-end-position))
    (comment-dwim arg)))


;; Copy line from point to the end, also, exclude the line break,
;; it will looks more naturely
(defadvice kill-ring-save
    (before slick-copy activate compile)
  "When called interactively with no active region,
   copy a single line instead."
  (interactive (if mark-active (list (region-beginning)
                                     (region-end))
                 (message "Copied line")
                 (list (line-beginning-position)
                       (line-beginning-position 2)))))

;; (defadvice kill-line (before check-position activate)
;;   (if (member major-mode
;;               '(emacs-lisp-mode scheme-mode lisp-mode
;;                                 c-mode c++-mode objc-mode js-mode
;;                                 latex-mode plain-tex-mode))
;;       (if (and (eolp) (not (bolp)))
;;           (progn (forward-char 1)
;;                  (just-one-space 0)
;;                  (backward-char 1)))))

;; kill to the begin of the line
(defun backward-kill-line (arg)
  (interactive "p") (kill-line 0) )

(defun copy-to-end-of-line ()
  (interactive)
  (kill-ring-save (point)
                  (line-end-position))
  (message "Copied to end of line"))

(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
           (line-beginning-position 2)))))

;;set transparent and use f1 to control it
(global-set-key (kbd "<f1>")         'loop-alpha)
(setq alpha-list '((100 100) (80 70) (60 40)))
(defun loop-alpha ()
  (interactive)
  (let ((h (car alpha-list)))                ;; head value will set to
    ((lambda (a ab)
       (set-frame-parameter (selected-frame) 'alpha (list a ab))
       (add-to-list 'default-frame-alist (cons 'alpha (list a ab)))
       ) (car h) (car (cdr h)))
    (setq alpha-list (cdr (append alpha-list (list h))))
    )
  )

;; font setting functions
(defun qiang-font-existsp (font)
  (if (null (x-list-fonts font))
      nil t))

(defun qiang-make-font-string (font-name font-size)
  (if (and (stringp font-size)
           (equal ":" (string (elt font-size 0))))
      (format "%s%s" font-name font-size)
    (format "%s %s" font-name font-size)))

(defun qiang-set-font (english-fonts
                       english-font-size
                       chinese-fonts
                       &optional chinese-font-size)
  (require 'cl)                         ; for find if
  (let ((en-font (qiang-make-font-string
                  (find-if #'qiang-font-existsp english-fonts)
                  english-font-size))
        (zh-font (font-spec :family (find-if #'qiang-font-existsp chinese-fonts)
                            :size chinese-font-size)))
    ;; Set the default English font
    ;;
    ;; The following 2 method cannot make the font settig work in new frames.
    ;; (set-default-font "Consolas:pixelsize=18")
    ;; (add-to-list 'default-frame-alist '(font . "Consolas:pixelsize=18"))
    ;; We have to use set-face-attribute
    (message "Set English Font to %s" en-font)
    (set-face-attribute
     'default nil :font en-font)

    ;; Set Chinese font
    ;; Do not use 'unicode charset, it will cause the english font setting invalid
    (message "Set Chinese Font to %s" zh-font)
    (dolist (charset '(kana han symbol cjk-misc bopomofo))
      (set-fontset-font (frame-parameter nil 'font)
                        charset
                        zh-font))))

;; chinese-gbk shell
(defun gshell()
  (interactive)
  (let ((coding-system-for-read 'chinese-gbk)
        (coding-system-for-write 'chinese-gbk))
    (call-interactively (shell))))
;; utf-8-unix shell
(defun ushell()
  (interactive)
  (let ((coding-system-for-read 'utf-8-unix)
        (coding-system-for-write 'utf-8-unix))
    (call-interactively (shell))))

;;needed in insert-word-or-file-name
(defun backward-to-non-blank () "go to 1st non blank (after blank) to left"
       (interactive)
       (if (re-search-backward "[ \t\n][^ \t\n]" (point-min) t)
           (forward-char 1)
         (if (string-match "[^ \t\n]" (buffer-substring 1 2))
             (goto-char (point-min)))))


;;needed in insert-buffer/file/dir-name functions
(defun buffer-name-not-mini ()
  "Return the name of current buffer, as a string.
If current buffer is the *mini-buffer* return name of previous-window."
  (buffer-name (if (window-minibuffer-p)
                   (if (eq (get-lru-window) (next-window))
                       (window-buffer (previous-window))
                     (window-buffer (next-window)))
                 nil)))

(defun insert-word-or-file-name ()
  "copy word cursor is on or file name to minibuff input"
  (interactive)
  (let* ((bfl (current-buffer))
         (str ""))
    (set-buffer (buffer-name-not-mini))
    (cond
     ((eq major-mode 'dired-mode)
      (setq str (dired-get-filename t t)))
     (t
      (let (bch ech)
        (forward-char 1)
        (backward-to-non-blank)
        (setq bch (point))
        (re-search-forward "[^ \t\n][ \t\n]" (point-max) t)
        (setq ech (1- (point)))
        (setq str (buffer-substring bch ech)))))
    (set-buffer bfl)
    (insert str)))

;; Insert buffer name
(defun ifn ()
  "insert buffer name of current buffer or most recent buffer when in
minibuffer"
  (interactive)
  (insert (buffer-file-name)))

(defun insert-full-name ()
  "insert buffer name of current buffer or most recent buffer when in
minibuffer"
  (interactive)
  (insert (buffer-file-name)))

(defun insert-buffer-name ()
  "insert buffer name of current buffer or most recent buffer when in
minibuffer"
  (interactive)
  (insert (buffer-name-not-mini)))


(defun insert-buffer-dir-name ()
  "insert dir name of current buffer or most recent buffer when in minibuffer"
  (interactive)
  (let* ((bfn (buffer-file-name (get-buffer (buffer-name-not-mini)))))
    (if bfn
        (insert (file-name-directory bfn)))))


(defun insert-buffer-file-name ()
  "insert file name of current buffer or most recent buffer when in minibuffer"
  (interactive)
  (let* ((bfn (buffer-file-name (get-buffer (buffer-name-not-mini)))))
    (if bfn
        (insert (file-name-nondirectory bfn)))))


(defun complete-from-minibuffer-history ()
  "Take the history list and make it available as a `completions' buffer"
  (interactive)
  (with-output-to-temp-buffer "*Completions*"
    (display-completion-list (symbol-value minibuffer-history-variable))
    (save-excursion
      (set-buffer standard-output)
      (setq completion-base-size 0))))


(defun insert-current-date-time-minibuf ()
  "insert the current date and time into mini-buffer."
  (interactive)
  (insert (format-time-string "%y%m%d_%H%M%S" (current-time))))

(defun indent-whole ()
  (interactive)
  (indent-region (point-min) (point-max))
  (message "format successfully"))

;; find none-ascii character
(defun occur-non-ascii ()
  "Find any non-ascii characters in the current buffer."
  (interactive)
  (occur "[^[:ascii:]]"))


(defun align-whitespace (start end)
  "Align columns by whitespace"
  (interactive "r")
  (align-regexp start end
                "\\(\\s-*\\)\\s-" 1 0 t)
  (indent-region start end))

(defun toggle-shell-buffer ()
  "Create or visit a shell buffer."
  (interactive)
  (if (eq (current-buffer) (get-buffer "*shell*"))
      (delete-window)
    (if (not (get-buffer "*shell*"))
        (progn
          (split-window-below)
          (other-window 1)
          (shell))
      (switch-to-buffer-other-window "*shell*"))
    )
  )

(defun toggle-eshell-buffer ()
  "Create or visit a eshell buffer."
  (interactive)
  (if (eq (current-buffer) (get-buffer "*eshell*"))
      (delete-window)
    (if (not (get-buffer "*eshell*"))
        (progn
          (split-window-below)
          (other-window 1)
          (eshell))
      (switch-to-buffer-other-window "*eshell*"))
    )
  )


(defun nuke-all-buffers ()
  (interactive)
  (mapcar 'kill-buffer (buffer-list))
  (delete-other-windows))

;; we use desktop to save all the buffers, if you do not want these buffers
;; use this function
(defun restart-a-clean-emacs ()
  (interactive)
  (nuke-all-buffers)
  (restart-emacs)
  )

(defun recompile-emacs ()
  (interactive)
  (byte-recompile-directory "~/.emacs.d/config" 0)
  )
