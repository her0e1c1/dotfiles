# -*- coding: utf-8 -*-
from fabric.api import *

env.projects = {
    "dccj": "~/projects/dccj/dccj",
    "kencorp": "~/projects/kencorp/kencorp"
}


@task
def up(project_name):
    """開発環境のセットアップをする。

    :project_name: プロジェクトの名前。これを使ってプロジェクトルートパスへ移動する

    実行すること:
        - emacs server起動
        - tmuxの設定
        - ローカルサーバーを立ち上げる
    """

    # emacs serverが起動していない場合はスタート
    with settings(warn_only=True):
        ret = local("emacsclient -e '(server-running-p)'")
        if ret.failed:
            local("emacs --daemon")

    # tmuxの設定
    local("tmux split-window -l 50")
    local("tmux new-window")

    # server起動
    path = env.projects[project_name]
    with lcd(path), prefix("source ../bin/activate"):
        local("paster serve --reload my_development.ini", shell="/bin/bash")
