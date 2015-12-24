(provide 'sub-mac-font)

(if (eq window-system 'ns)
    (qiang-set-font
     '("Fira Mono for Powerline" "Menlo" "Consolas" "Courier New" "Andale Mono" "Monaco" "Courier10 BT" "PT Mono"
        "DejaVu Sans Mono" "Monospace")
     my-font-size
     '("Yuanti SC" "楷体" "kai" "Microsoft Yahei" "文泉驿等宽微米黑"
       "黑体" "新宋体" )))
