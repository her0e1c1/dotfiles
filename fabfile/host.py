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


@roles("win")
def win():
    env.run = local
    env.sphinx_dir = [
        ("~/github/sphinx_document", "/media/sf_share/sphinx_document"),
        ("~/Dropbox/sphinx", "/media/sf_share/work")
    ]


def init():
    localhost()
    prisoners()
    jailer()
    win()


# コマンドラインのオプション-Rに応じて、envの設定を動的に変更させる
init()
