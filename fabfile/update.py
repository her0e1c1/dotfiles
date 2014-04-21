from fabric.api import *


@task(default=True)
def update():
    require("github_dir")
    for d in env.github_dir:
        with lcd(d):
            print("git pull %s" % d)
            local("git pull")

    require("sphinx_dir")
    for src, dst in env.sphinx_dir:
        local("sphinx-build -b html %s %s" % (src, dst))  

@task
def push():
    require("github_dir")
    for d in env.github_dir:
        with lcd(d):
            print("git auto commit and push %s" % d)
            local("git add .")
            local("git ci -m 'auto commit by fabric'")
            local("git push")
