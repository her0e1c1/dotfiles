# -*- coding: utf-8 -*-
from fabric.api import *
from . import host  # 設定ファイルなので始めに読み込ませること
from . import utils as u
from . import hg
from . import start 
from . import gae
from . import jail
from . import update


env.user = "root"
env.roledefs = {
    "localhost": ["localhost"],
    "win": ["localhost"],
    "jailer": ["192.168.1.100"],
    "prisoners": ["192.168.1.101"],
}

env.github_dir = [
    "~/github/home",
    "~/github/sphinx_document",
]

env.ansible = {
    "playbook": "~/github/home/ansible/playbook.yml",
    "hosts": "~/github/home/ansible/hosts",
}
