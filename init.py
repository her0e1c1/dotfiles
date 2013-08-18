#!/usr/bin/env python3
import os
import sys
import argparse
import configparser

usage = \
"""
type:
python3 init.py [option]

you must execute the command in the same directory
as the one having init.py
"""

abspath = os.path.abspath(os.path.dirname(__file__))
assert os.getcwd() == abspath, usage

def execute(cmd):
    print(cmd)
    os.system(cmd)
    

def home():
    """python init.py --home"""

    files = [os.path.abspath(f) for f in IFR.values("home")]
    for f in files:
        cmd = "ln -sf {0} ~/".format(f)
        print(cmd)
        os.system(cmd)


def easy_install(packages):
    cmd = "sudo easy_install3 {package}"
    for p in packages:
        execute(cmd.format(package=p))


def main():
    p = argparse.ArgumentParser()
    p.add_argument("--home", action="store_true")
    p.add_argument("--easy-install", action="store_true")
    args = p.parse_args()

    if args.home:
        home()
    if args.easy_install:
        easy_install(IFR.values("easy_install3"))


if __name__ == "__main__":
    main()
