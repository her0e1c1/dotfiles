; バージョン指定
(let ((minver 23))
  (unless (>= emacs-major-version minver)
    (error "Your Emacs is too old. this requires v%s or higher" minver)))

; 初期化ファイルを読み込むディレクトリを設定
(add-to-list 'load-path (expand-file-name "init" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "config" user-emacs-directory))

;;; 始めに読み込む必要があるファイル
(require 'my-packages)
(require 'init-env)
(require 'init-global-set-key)
(require 'init-keyjack)
(require 'init-elpa)
(require 'init-recentf)
;;; init以外で読み込むライブラリ
(require 'cl)
(require 'dash)
(require 'f)
(require 's)
(require 'ht)
;(require 'flymake)

;;; osごとの設定
(require 'init-mac)

;;; 読み込むパッケージはelpa以下に記述
(require 'init-helm)
(require 'init-auto-complete)
(require 'init-flycheck)
(require 'init-evil)
(require 'init-python)
(require 'init-javascript)
(require 'init-multiple-cursors)
(require 'init-smartparents)
(require 'init-yasnippet)
(require 'init-rainbow-delimiters)
(require 'init-expand-region)

;;; サーバー起動
(require 'server)
(unless (server-running-p)
  (server-start))

(provide 'init)
