(provide 'sub-mac-font)

(if (eq window-system 'ns)
    (qiang-set-font
     '("Source Code Pro" "SF Mono" "Courier" "Fira Mono for Powerline" "Menlo" "Consolas" "Courier New" "Andale Mono" "Monaco" "Courier10 BT" "PT Mono"
        "DejaVu Sans Mono" "Monospace")
     my-font-size
     '("Yuppy SC" "HanziPen SC" "Yuanti SC" "楷体" "kai" "Microsoft Yahei" "文泉驿等宽微米黑"
       "黑体" "新宋体" )))
