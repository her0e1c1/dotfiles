# -*- coding: utf-8 -*-
import os
from fabric.api import *
from fabric import contrib
from fabric.colors import magenta


@task
def setup(replace=False):
    """gaeの環境を構築する。

    1. zipファイルを置くディレクトリを生成
    2. google_appengineのSDKのzipファイルをダウンロード
    3. 取得したzipファイルを解凍する

    :replace: Trueの場合は、取得したzipファイルを解凍すると生成されるgoogle_appengine
              ディレクトリを一度削除して、新しいSDKに置き換える
    """
    require("gae_download_url", "gae_lib_dir")

    if not contrib.files.exists(env.gae_lib_dir):
        run("mkdir %s" % env.gae_lib_dir)

    with cd(env.gae_lib_dir):
        base = os.path.basename(env.gae_download_url)
        if contrib.files.exists(base):
            print(magenta("you already downloaded %s." % base))
        else:
            run("wget %s" % env.gae_download_url)

        if replace:
            run("rm -fr google_appengine")

        if contrib.files.exists("google_appengine"):
            print(magenta("google_appengine already exists."))
        else:
            run("unzip %s" % base)
