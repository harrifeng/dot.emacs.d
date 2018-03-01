# Emacs configuration ReadMe #

## Configure Git

```
git config --global credential.helper store
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
branch="release-branch.go1.8"
mkdir -p $GOPATH/src/golang.org/x

git clone -b $branch git@github.com:golang/tools.git $GOPATH/src/golang.org/x/tools
git clone git@github.com:golang/net.git $GOPATH/src/golang.org/x/net

cd $GOPATH
go get -v github.com/FiloSottile/gvt github.com/nsf/gocode golang.org/x/tools/cmd/goimports github.com/tpng/gopkgs github.com/ramya-rao-a/go-outline github.com/acroca/go-symbols golang.org/x/tools/cmd/guru golang.org/x/tools/cmd/gorename github.com/fatih/gomodifytags github.com/josharian/impl github.com/rogpeppe/godef sourcegraph.com/sqs/goreturns github.com/golang/lint/golint github.com/cweill/gotests/... github.com/derekparker/delve/cmd/dlv
```

## some problem

```
mkdir -p $GOPATH/src/golang.org/x
cd $GOPATH/src/golang.org/x
git clone https://github.com/golang/tools.git
```

## cnpm install first

    $ npm install -g cnpm --registry=https://registry.npm.taobao.org

## nodejs install global package

    $ cnpm install -g js-beautify webpack tern livedown npm-check-updates nodeppt tldr

## python install

    $ pip install -U pip rope flake8 importmagic autopep8 yapf -i http://mirrors.aliyun.com/pypi/simple --trusted-host=mirrors.aliyun.com

## flycheck
+ for shell flycheck,you need

```
brew install shellcheck
```


## Command setup (May not work for Emacs 25.1)
+ First usage use following command to first initialize the environment on shell you can go to .emacs.d folder and run

```
    $tail -n1 README.md
```
+ Actual-work script

```
     emacs --batch -l ~/.emacs.d/config/helpfunc-setting.el -f install-my-packages
```

## Choco install
```
choco install -y sqlyog ag curl wget diffutils git emacs64 nodejs python3 golang jdk8 virtualbox vagrant cmder --ignorechecksum
```

## Docker install
```
docker run --name cow -p 7777:7777 -e listen=http://0.0.0.0:7777 -e proxy=ss://aes-256-cfb:pwd@harrifeng.club:1984 -d fzinfz/cow
```

## xterm
```
xterm -xrm '*.VT100.translations: #override Ctrl <Key>Return: string(0x1c)'
```

## linux often
```
sudo unlink /etc/localtime
sudo ln -s /usr/share/zoneinfo/Asia/Shanghai /etc/localtime
```

## YUM package
```
sudo yum install gnutls-utils
```

## Helm docs
+ Use `TAB` if you don't konw how to swith to edit mode
