(provide 'sub-nt-path)


(defconst my-go-space
  (concat (getenv "HOME")
          "/go/"))
(setenv "GOPATH" my-go-space)

(defconst my-go-space-path
  (concat my-go-space "bin/"))

(setenv "PATH"
        (concat
         my-go-space-path ";"
         (getenv "PATH")))
