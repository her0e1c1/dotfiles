from fabric.api import *

@task
def pull():
    print "hg pull"
    with hide("running"):
        local("hg pull --insecure --config auth.x.prefix=* --config auth.x.username={hg_user} --config auth.x.password={hg_pass}".format(**env))
    

@task
def push():
    print "hg push"
    with hide("running"):
        local("hg push --new-branch --insecure --config auth.x.prefix=* --config auth.x.username={hg_user} --config auth.x.password={hg_pass}".format(**env))
