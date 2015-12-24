(provide 'path-setting)

;; PATH is setting in sub-[SYS]-path.el, should load before this file
;; exec-path based on path, can work on all system if path is
;; set successfully
(setq exec-path (split-string (getenv "PATH") path-separator))
