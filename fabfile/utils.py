# -*- coding: utf-8 -*-
import os
import crypt
from getpass import getpass
from fabric.api import *
from fabric import contrib


@task
def encrypt():
    """文字列を入力するとハッシュした文字列を返す
    """
    p = getpass()
    print(crypt.crypt(p, "somesolt"))
