
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(defconst my-emacs-path "~/.emacs.d/")

(defconst my-lisps-path  (concat my-emacs-path "config/")
  "Path for configuration")
(add-to-list 'load-path my-lisps-path)

(require 'basic-setting)
(require 'helpfunc-setting)
(require 'systype-setting)
(require 'path-setting)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; path is used by others ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'build-in-setting)
(require 'mode-setting)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; el-get is used by mac and linux,package is used by all.  ;;
;; Enable specific plugin in the el-setting.el              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'el-setting)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; keybind is the last to load ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'keybind-setting)
(require 'tmp-setting)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sql-setting should not be included in the git repo ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(if (file-exists-p
     (concat my-lisps-path "sql-setting.el"))
    (require 'sql-setting))
