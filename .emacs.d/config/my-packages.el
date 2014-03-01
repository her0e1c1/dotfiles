(defvar my/installing-package-list
  '(
    ;w3m  ; w3mコマンドが必須
    ;icalendar
    php-mode
    ;scala-mode
    ;markdown-mode
    scss-mode
    haskell-mode
    ;google-c-style
    yaml-mode
    ;open-junk-file
    zenburn-theme
    rainbow-delimiters
    flycheck
    yasnippet
    dash
    s
    f
    ht
    helm
    smartparens
    auto-complete
    bookmark+
    recentf-ext
    dired+
    js2-mode
    evil
    key-chord
    ))

(defvar my/package-archives 
  '(("melpa" . "http://melpa.milkbox.net/packages/")
    ("marmalade" . "http://marmalade-repo.org/packages/")))

(provide 'my-packages)
