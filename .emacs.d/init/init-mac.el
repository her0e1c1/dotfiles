(if (or (eq window-system 'mac)
        (eq window-system 'ns))
    (set-frame-font "Hiragino-12")
    (let ()
	  ;フォントサイズを変更する
      ;(require 'carbon-font)
      ;(fixed-width-set-fontset "hirakaku_w3" 12)
      ))
(provide 'init-mac)
