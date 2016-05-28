# emacs.d
emacs.d is from old .emacs.d, keep this only have one branch, so that it can spread quickly

# golang install
```
go get -u -v github.com/nsf/gocode github.com/golang/lint/golint github.com/lukehoban/go-find-references github.com/lukehoban/go-outline sourcegraph.com/sqs/goreturns golang.org/x/tools/cmd/gorename github.com/tpng/gopkgs github.com/newhook/go-symbols github.com/rogpeppe/godef golang.org/x/tools/cmd/goimports
```

# first usage
Use following command to first initialize the environment(on shell you can go to .emacs.d folder and run > $(tail -n1 README.md):
emacs --batch -l ~/.emacs.d/config/helpfunc-setting.el -f install-my-packages
