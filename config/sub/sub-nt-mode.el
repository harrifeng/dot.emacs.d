(provide 'sub-nt-mode)

;; python mode------------>>
(require 'python)

(setq
 python-shell-interpreter "python.exe")

;; spell checking
(setq ispell-program-name "aspell.exe")

;; sql-mysql
(setq sql-mysql-options '("-C" "-t" "-f" "-n"))
