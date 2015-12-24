(provide 'sub-nt-path)

(if (file-exists-p "c:/Devbox/")
    (defconst my-dev "c:/Devbox/")
  (defconst my-dev "d:/Devbox/"))

(defconst my-util-path
  (concat my-dev "Util/"))

(defconst my-rust-home
  (concat my-dev "Rust/"))
(defconst my-rust-path
  (concat my-rust-home "bin/"))

(setq racer-rust-src-path
      (concat my-rust-home "ForRacer/src"))

(defconst my-go-path
  (concat my-dev "Go/bin/"))

(defconst my-python-path
  (concat my-dev "Python2710/"))

(defconst my-python-script-path
  (concat my-python-path "Scripts/"))

(defconst my-git-path
  (concat my-dev "Git/bin/"))

(defconst my-mingw-path
  (concat my-dev "MinGW/bin/"))

(defconst my-java-home
  (concat my-dev "Java/"))

(defconst my-go-home
  (concat my-dev "Go/"))
(setenv "GOROOT" my-go-home)

(defconst my-java-path
  (concat my-dev "Java/bin/"))

(defconst my-graphviz-path
  (concat my-dev "Graphviz/bin/"))


(defconst my-go-space
  (concat my-dev "Code/golang-space"))
(setenv "GOPATH" my-go-space)

(defconst my-go-space-path
  (concat my-go-space "/bin"))


(setenv "PATH"
        (concat
         my-git-path ";"
         my-python-path ";"
         my-rust-path ";"
         my-go-path ";"
         my-go-space-path ";"
         my-python-script-path ";"
         my-java-path ";"
         my-mingw-path ";"
         my-graphviz-path ";"
         my-util-path ";"
         (getenv "PATH")))

(setenv "JAVA_HOME"
        my-java-home)
