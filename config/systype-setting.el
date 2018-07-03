(provide 'systype-setting)

(add-to-list 'load-path (concat my-lisps-path "sub"))

(cond
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; OS X system specific test on MAC OS 10.8    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ((eq system-type 'darwin)
  ;; unix-like path setting------------------------------------>>
  (require 'sub-mac-path)
  (require 'sub-mac-mode)
  (require 'sub-mac-font)
  ;; mac use bash------------------------------------------->>
  (setq explicit-shell-file-name "/bin/bash")
  (setq explicit-bash-args '("--noediting" "--login" "-i"))
  (load-theme 'solarized-light t)

  ;; if you want to the emacs mac open to maximized size, setting this
  (add-to-list 'default-frame-alist '(fullscreen . maximized))

  ;; menu-bar-mode is useful in mac---------------------------->>
  (menu-bar-mode -1))

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;; Linux System specific test on Ubuntu 12.04  ;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ((eq system-type 'gnu/linux)
  (require 'sub-linux-path)
  (require 'sub-linux-mode)
  (require 'sub-linux-font)
  ;; Linux use bash also
  (setq explicit-shell-file-name "/bin/bash")
  (load-theme 'monokai t)
  (menu-bar-mode -1))

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;; Cygwin System specific test on 1.7.1        ;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 ((eq system-type 'cygwin)
  (require 'sub-linux-path)
  (require 'sub-linux-mode)
  (require 'sub-nt-font)
  ;; max windows size on start up------------------------------>>
  (run-with-idle-timer 1 nil 'w32-send-sys-command 61488)
  ;; cygwin use bash------------------------------------------->>
  (setq explicit-shell-file-name "/bin/bash")
  (menu-bar-mode -1))

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;; Windows NT System specific test on Windows 7 ;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ((eq system-type 'windows-nt)


  ;; win 10 GC
  (setq gc-cons-threshold (* 512 1024 1024))
  (setq gc-cons-percentage 0.5)
  (run-with-idle-timer 5 t #'garbage-collect)
  ;; (setq garbage-collection-messages t)

  ;; nt-like path setting------------------------------------>>
  (require 'sub-nt-path)
  (require 'sub-nt-mode)
  (require 'sub-nt-font)

  ;; max windows size on start up------------------------------>>
  (run-with-idle-timer 1 nil 'w32-send-sys-command 61488)
  ;; no menu bar----------------------------------------------->>
  (menu-bar-mode -1))
 )
