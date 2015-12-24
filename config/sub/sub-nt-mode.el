(provide 'sub-nt-mode)

;; python mode------------>>
(require 'python)

(setq
 ;; WinPython has ipython.exe with it
 python-shell-interpreter "ipython.exe")

;; spell checking
(setq ispell-program-name "aspell.exe")

;; sql-mysql
(setq sql-mysql-options '("-C" "-t" "-f" "-n"))
