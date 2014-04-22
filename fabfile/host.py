# -*- coding: utf-8 -*-
from fabric.api import *
from fabric import contrib


def mac():
    env.run = local
    env.sphinx_dir = [
        ("~/github/sphinx_document", "~/sphinx_build/sphinx_document"),
        ("~/Dropbox/sphinx", "~/sphinx_build/mine")
    ]

def prisoners():
    env.shell = "/usr/local/bin/zsh -l -c"


def jailer():
    env.shell = "/usr/local/bin/zsh -l -c"


def win():
    env.run = local
    env.sphinx_dir = [
        ("~/github/sphinx_document", "/media/sf_share/sphinx_document"),
        ("~/Dropbox/sphinx", "/media/sf_share/work")
    ]


# コマンドラインのオプション-Rに応じて、envの設定を動的に変更させる
def init():
    if "mac" in env.roles:
        mac()
    elif "prisoners" in env.roles:
        prisoners()
    elif "jailer" in env.roles:
        jailer()
    elif "win"in env.roles:
        win()

init()
