(provide 'basic-setting)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; =============Quantified Habits================ ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq user-mail-address "harrifeng@gmail.com")
(setq user-full-name    "harrifeng")

(setq kill-ring-max 200)

(setq-default tab-width 4)

(setq scroll-step 0
      scroll-margin 0
      scroll-conservatively 10000)

(fset 'yes-or-no-p 'y-or-n-p)

(setq ring-bell-function 'ignore)


;;auto expand
(setq hippie-expand-try-functions-list
      '(try-expand-line ;; whole line first
        try-expand-line-all-buffers
        try-expand-list
        try-expand-list-all-buffers
        try-expand-dabbrev
        try-expand-dabbrev-visible
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-file-name
        try-complete-file-name-partially
        try-complete-lisp-symbol
        try-complete-lisp-symbol-partially
        try-expand-whole-kill))

;;Custom setting saved file location
(setq abbrev-file-name (concat my-lisps-path ".abbrev_defs"))
(setq custom-file (concat my-lisps-path ".emacs_custom.el"))

;;auto compile elc file when saved
(add-hook 'after-save-hook
          (lambda ()
            (if (eq major-mode 'emacs-lisp-mode)
                (save-excursion (byte-compile-file buffer-file-name)))))

;;faster compile & accurate warning.
(setq byte-compile-verbose nil)
(setq font-lock-verbose t)

;;encoding from stackoverflow
;;http://stackoverflow.com/questions/2901541/which-coding-system-should-i-use-in-emacs
(setq utf-translate-cjk-mode 1) ; enable CJK coding/encoding (Chinese/Japanese/Korean characters)
(set-language-environment 'utf-8)
(set-keyboard-coding-system 'utf-8-mac) ; For old Carbon emacs on OS X only
(setq locale-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(unless (eq system-type 'windows-nt)
  (set-selection-coding-system 'utf-8))
(prefer-coding-system 'utf-8)


;; grep-windows-height
(setq compilation-window-height 12)
(setq grep-window-height 12)

(setenv "PAGER" "cat")
(setenv "TERM"  "dumb")

;; packages setting, this will work on all platforms, although unix-link system
;; can use el-get, package still exist as supplementary
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

(package-initialize)

;; different font size for different hosts
(defconst my-font-size
  (cond
   ((string-match system-name "hfeng-desktop")
    ":pixelsize=28")
   ((string-match system-name "hfeng-laptop")
    ":pixelsize=18")
   ((string-match system-name "vagrant-ubuntu-vivid-64")
    ":pixelsize=15")
   ((string-match system-name "pvgm50860487a.dhcp.pvgl.sap.corp")
    ":pixelsize=12")
   (":pixelsize=18")))

;; red some color
(set-cursor-color "red")

(setq default-frame-alist
      '((cursor-color . "red")))

;; mysql config, not including password
(setq sql-user "root")
(setq sql-password "")
(setq sql-database "mysql")
(setq sql-server "localhost")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ==============Boolean Habits=================== ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; use spaces only!
(setq-default indent-tabs-mode nil)

(setq inhibit-startup-message t)
(setq initial-scratch-message "")
(setq column-number-mode t)
(customize-set-variable 'scroll-bar-mode nil)
(blink-cursor-mode nil)
(tool-bar-mode -1)
;; Delete the CR and the end of the line when Ctrl + K at beginning of the line
(setq-default kill-whole-line t)
;; if kill content are the same, ignore them
(setq kill-do-not-dave-duplicates t)
;; Stop scroll-bar, it's very important to make emacs looks move more smoothly
(setq comment-empty-lines t)

;; up-down case
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; always new find windows on horizontally
(setq split-width-threshold nil)

;; disable backup
(setq backup-inhibited t)
;; disable auto save
(setq auto-save-default nil)
;; Enable copy and pasting from clipboard
(setq x-select-enable-clipboard t)
