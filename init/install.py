#!/usr/bin/env python3
import os
import sys
import argparse
import subprocess
import tarfile
import kommon as k
from http import client
import tempfile

#packages you need fisrt
packages = [
    "xpdf",
    "py27-setuptools",
    "bash",
    "ImageMagick",
    "git",
    "fluxbox",
    "ja-scim-anthy",
    "rsync",
    "firefox",
    "opera",
    "vim",
    "gimp",
    "virtualbox-ose-additions",
    "ja-font-std",
    "mplayer",
    "ja-font-mplus-ipa",
    "wget",
    "emacs",
    #"ffmpeg",
    "thunderbird",
    "ghc",
    "ccl",
    "fusefs-kmod",
    "fusefs-exfat",
    "xorg",
]

#common files at home dirctory
home = [
    ".bashrc",
    ".emacs",
    ".emacs.d",
    ".vimrc",
    ".xmodmap",
]


PYTHON_PACKAGES = [
    "ipython",
    "pip",
    "sphinx",
    "flask",
    "requests",
    "django",
    "watchdog",
    "numpy",
    "scipy",
    "matplotlib",
    
]

bsd = {
    os.environ.get("HOME"): [
        ".Xresources",
        ".xinitrc",
    ],

    "/etc/": [
        "rc.conf",
    ]
}
    
ssh_key = {"public_key": "./id_me_rsa.pub"}

class addusers:
    def __init__(self):
        pass

        

def execute(cmd):
    print(cmd)
    subprocess.call(cmd, shell=True)


def ssh():
    global ssh_key
    pkey = ssh_key["public_key"]
    dirc = os.path.expanduser("~/.ssh")
    if not os.path.isdir(dirc):
        os.mkdir(dirc)
    cmd = "cp {0} ~/.ssh/authorized_keys"
    cmdf = cmd.format(pkey)
    if os.path.isfile(pkey):
        execute(cmdf)


def pkg_exists(name):    

    cmd = "pkg_info | grep {0}".format(name)
    print(cmd)
    try:
        output = subprocess.check_output(cmd, shell=True)
    except subprocess.CalledProcessError:
        return False
    return True


def pkg_add(packages):
    """this is a bsd package auto installer"""

    for p in packages:
        if not pkg_exists(p):
            cmd = "pkg_add -r {0}".format(p)
            execute(cmd)


def link():

    if "bsd" in sys.platform:
        global bsd
        platform = bsd 
    else: return
    
    platform[os.environ["HOME"]] += home
    for dirc, files in platform.items():
        for f in files:
            cmd = "ln -fs {src} {dst}"
            src = os.path.abspath(f)
            dst = os.path.join(dirc, f)
            cmdf = cmd.format(
                src=src,
                dst=dst
            )
            if os.path.isfile(src):
                execute(cmdf)


def link_home(names):

    if "bsd" in sys.platform:
        global bsd
        for b in bsd:
            names.append(b)

    for name in names:
        home = os.path.join("~/", name)
        dst = os.path.expanduser(home)
        abspath = os.path.abspath(name)
        if os.path.isfile(abspath):
            cmd = "ln -sf {src} {dst}"
            cmdf = cmd.format(
                src=abspath,
                dst=dst
            )
            execute(cmdf)


@k.arg("--install", action="store_true")
@k.arg("--version", default="3.3.0")
@k.arg("--prefix", default=None)
@k.arg("--distribute", nargs="?", default=None, const="0.6.31")
@k.arg("--packages", action="store_true")
def python(args):
    """this is a dispach function"""

    if args.packages:
        for p in PYTHON_PACKAGES:
            cmd = "easy_install-3.3 {}".format(p)
            execute(cmd)

    if args.distribute:
        _distribute(args.distribute)

    if args.install:
        _install(args.version, args.prefix)


def _install(version, prefix=None):

    filename = "Python-{0}.tgz".format(version)
    extracted_dir = k.splitext(filename)[0]
    host = "http://www.python.org/ftp/python/{0}/"
    host = host.format(version) + filename
    p = os.path.join(os.environ.get("HOME"), "python", version)
    installed_dir = prefix or p
    bin = os.path.join(installed_dir, "bin")

    if not os.path.isdir(installed_dir):
        print("makedirs {}".format(installed_dir))
        os.makedirs(installed_dir)
    
    with tempfile.NamedTemporaryFile() as fp:
        fp.write(k.get(host))
        with tarfile.open(fp.name) as tf:
            tf.extractall(tempfile.gettempdir())

    if not os.path.isdir(bin):
        c_dir = os.path.join(tempfile.gettempdir(), extracted_dir)
        with k.chdir(c_dir):
            sh = """
            ./configure --prefix={0}
            make
            make install
            """.format(installed_dir)
            execute(sh)
    else:
        print("python is already installed")


def _distribute(version):

    filename = "distribute-{0}.tar.gz".format(version)
    host = "http://pypi.python.org/packages/source/d/distribute/"
    host += filename
    extracted_dir = k.splitext(filename)[0]

    major_ver = sys.version_info[0]
    if major_ver == 3:
        cmd = "python3"
    else:
        cmd = "python"

    if k.which("easy_install-3.3"):
        print("easy_install-3.3 is already installed")
    else:
        with tempfile.NamedTemporaryFile() as tf:
            tf.write(k.get(host))
            with tarfile.open(tf.name) as tr:
                tr.extractall(tempfile.gettempdir())

            c_dir = os.path.join(tempfile.gettempdir(), extracted_dir)
            with k.chdir(c_dir, debug=True):
                sh = "{cmd} setup.py install".format(cmd=cmd)
                execute(sh)


if __name__ == "__main__":
    k.dispatch([python])
