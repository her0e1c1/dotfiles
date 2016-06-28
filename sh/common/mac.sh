# 隠しファイル表示
defaults write com.apple.finder AppleShowAllFiles TRUE

# Dockを再起動(killall Dock)すると、どれだけCommand-Tabしても、ワークスペースが移動することを防ぐことができる。もとに戻す場合は上記コマンドのNOをYESとすればよい。
defaults write com.apple.Dock workspaces-auto-swoosh -bool NO


# FTP Server
# start
#  sudo -s launchctl load -w /System/Library/LaunchDaemons/ftp.plist
# stop::
# sudo -s launchctl unload -w /System/Library/LaunchDaemons/ftp.plist
# /usr/libexec/ftpd -D

# cat id_rsa.pub |pbcopy

# .DS_Storeを自動生成させない
defaults write com.apple.desktopservices DSDontWriteNetworkStores true

# mac独自のコマンドで便利なもの open say 

# ``/etc/exports``
#  #mount_point option ip
#  / -maproot=USE_NAME 192.168.56.100
