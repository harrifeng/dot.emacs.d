(provide 'sub-mac-font)

(if (eq window-system 'ns)
    (qiang-set-font
     '("Monaco" "Source Code Pro" "SF Mono" "Fira Mono for Powerline" "Menlo" "Consolas" "Courier New" "Andale Mono" "Courier10 BT" "PT Mono" "Courier" "DejaVu Sans Mono" "Monospace")
     my-english-font-size
     '("Yuppy SC" "HanziPen SC" "Yuanti SC" "楷体" "kai" "Microsoft Yahei" "文泉驿等宽微米黑"
       "黑体" "新宋体" )))
