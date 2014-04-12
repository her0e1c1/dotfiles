# -*- coding: utf-8 -*-
from fabric.api import *
from fabric import contrib


@roles("prisoners")
def prisoners():
    env.shell = "/usr/local/bin/zsh -l -c"


def init():
    prisoners()

init()
