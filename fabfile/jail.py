# -*- coding: utf-8 -*-
import os
from StringIO import StringIO
from fabric.api import *
from fabric import contrib


@task
def new(name, ip_addr, number, root="/usr/jails/", nameserver="192.168.1.1"):
    """新しいprisonerを作成し、 sshでログインできるようにする。

    1. ezjail-adminを使って新しいprisonerを作成

    :number: aliasの後につける数字
    :nameserver: /etc/resolv.confに設定するgatewayのipアドレス
    """
    path = os.path.join(root, name)
    rcconf = os.path.join(path, "etc/rc.conf")
    resolv = os.path.join(path, "etc/resolv.conf")
    kw = {
        "name": name, "ip_addr": ip_addr, "number": number, "path": path,
        "rcconf": rcconf, "resolv": resolv, "nameserver": nameserver,
    }

    jailer_rcconf = """
jail_{name}_rootdir="{path}"
jail_{name}_ip="{ip_addr}"
ifconfig_re0_alias{number}="inet {ip_addr} netmask 255.255.255.0"\
""".format(**kw)

    prisoner_rcconf = "sshd_enable='YES'"
    prisoner_resolv = "nameserver {nameserver}".format(**kw)

    run("ezjail-admin create {name} {ip_addr}".format(**kw))
    run("cp -f /etc/ssh/sshd_config %s" % os.path.join(path, "etc/ssh/sshd_config"))
    run("mkdir %s" % os.path.join(path, "root/.ssh"))
    run("cp ~/.ssh/authorized_keys %s" % os.path.join(path, "root/.ssh/authorized_keys"))
    contrib.files.append("/etc/rc.conf",jailer_rcconf)
    put(StringIO(prisoner_rcconf), kw["rcconf"])
    put(StringIO(prisoner_resolv), kw["resolv"])
    #run("reboot")  # 設定ファイルを反映するのに再起動は不要
    run("ezjail-admin start {name}".format(**kw))


@task
def delete(name, number):
    kw = {"name": name, "number": number}
    rcconf = [
        r'^jail_{name}_rootdir.*'.format(**kw),
        r'^jail_{name}_ip.*'.format(**kw),
        r'^ifconfig_re0_alias{number}.*'.format(**kw)
    ]
    msg = "Are you sure delete %s?" % name
    if contrib.console.confirm(msg, default=False):
        run("ezjail-admin delete -wf %s" % name)
        for r in rcconf:
            contrib.files.comment("/etc/rc.conf", r)
