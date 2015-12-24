(provide 'sub-linux-path)

;;For Macos build the emacs by
;; > brew install emacs --cocoa --srge
(setenv "PATH"
	(concat
	 "/usr/local/bin" ":"
	 "/bin"           ":"
	 "/sbin"          ":"
	 "/usr"           ":"
	 "/usr/bin"       ":"
	 "/usr/sbin"      ":"
	 (getenv "PATH")
	 ))
