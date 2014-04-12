# -*- coding: utf-8 -*-
from fabric.api import *
from fabric import contrib


@roles("localhost")
def localhost():
    env.run = local


@roles("prisoners")
def prisoners():
    env.shell = "/usr/local/bin/zsh -l -c"


@roles("jailer")
def jailer():
    env.shell = "/usr/local/bin/zsh -l -c"


def init():
    prisoners()
    jailer()

# コマンドラインのオプション-Rに応じて、envの設定を動的に変更させる
init()
