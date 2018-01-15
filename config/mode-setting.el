(provide 'mode-setting)
;; sql-interactive-mode--->>
(add-hook 'sql-interactive-mode-hook
          (lambda ()
            (toggle-truncate-lines t)))
;; cc mode---------------->>
(setq c-default-style
      (quote ((java-mode . "java")
              (awk-mode . "awk")
              (c-mode . "bsd")
              (other . "stroustrup")))
      c-basic-offset 4)

;; nXML mode-------------->>
(setq nxml-child-indent 4)

;; makefile mode---------->>
(add-to-list 'auto-mode-alist '("\\.mak\\'" . makefile-mode))

;; python mode(from Emacs 24.3, new python.el is introduced)--->>
(require 'python)
(setq
 python-indent-offset 4
 python-shell-prompt-regexp "In \\[[0-9]+\\]: "
 python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
 python-shell-completion-native-enable nil
 )

;; ruby mode (from Emacs 24, new function introduced for auto-indent)--->>
(add-hook 'ruby-mode-hook
          (lambda ()
            (electric-indent-mode t) ))
(add-to-list 'auto-mode-alist '("\\Gemfile\\'" . ruby-mode))
