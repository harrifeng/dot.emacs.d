# Emacs configuration ReadMe #

## mac version emacs with svg support

```
brew install emacs --HEAD --use-git-head --cocoa --with-gnutls --with-librsvg --with-imagemagick
```

## dotemacsd
dotemacsd is from old .emacs.d, keep this only have one branch, so that it can spread quickly

## Daily usage
Use following elisp to clean the screen in any repl buffer

```
    $ erase-buffer
```

## Sql setting
sql-setting.el is need if you use sql very often, one example el file looks like following

```
    (provide 'sql-setting)

    (setq sql-connection-alist
          '((server1 (sql-product 'mysql)
                     (sql-port 3306)
                     (sql-server "db")
                     (sql-user "root")
                     (sql-password "root")
                     (sql-database "mysql"))
            (server2 (sql-product 'mysql)
                     (sql-port 3306)
                     (sql-server "localhost")
                     (sql-user "root")
                     (sql-password "password")
                     (sql-database "db2"))))

    (defun my-sql-server1 ()
      (interactive)
      (my-sql-connect 'mysql 'server1))

    (defun my-sql-server2 ()
      (interactive)
      (my-sql-connect 'postgres 'server2))

    (defun my-sql-connect (product connection)
      ;; remember to set the sql-product, otherwise, it will fail for the first time
      ;; you call the function
      (setq sql-product product)
      (sql-connect connection))
```
## golang install


```
go get -u -v github.com/nsf/gocode  golang.org/x/tools/cmd/goimports github.com/tpng/gopkgs  github.com/ramya-rao-a/go-outline  github.com/acroca/go-symbols  golang.org/x/tools/cmd/guru  golang.org/x/tools/cmd/gorename  github.com/fatih/gomodifytags  github.com/josharian/impl  github.com/rogpeppe/godef  sourcegraph.com/sqs/goreturns  github.com/golang/lint/golint  github.com/cweill/gotests/...  github.com/derekparker/delve/cmd/dlv
```

## nodejs install

    $ npm install -g js-beautify webpack tern livedown yaml-jsyaml

## python install

    $ pip install rope flake8 importmagic autopep8 yapf powerline-status psutil

## flycheck
+ for shell flycheck,you need

```
brew install shellcheck
```


## Command setup (May not work for Emacs 25.1)
+ First usage use following command to first initialize the environment on shell you can go to .emacs.d folder and run

```
    $(tail -n1 README.md
    ```
+ Actual-work script

```
     emacs --batch -l ~/.emacs.d/config/helpfunc-setting.el -f install-my-packages
```

## Choco install
```
     choco install -y git diffutils nodejs python2 ag
```

## Docker install
```
docker run --name cow --net host -e listen=http://0.0.0.0:7777 -e proxy=ss://aes-256-cfb:pwd@harrifeng.club:1984 -d fzinfz/cow
```
