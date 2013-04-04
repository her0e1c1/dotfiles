#!/usr/bin/env python3
import os
import sys
import argparse

usage = \
"""
type:
python3 init.py [option]

you must execute the command in the same directory
as the one having init.py
"""

abspath = os.path.abspath(os.path.dirname(__file__))
assert os.getcwd() == abspath, usage

def home():
    """python init.py --home"""

    files = [os.path.abspath(f) for f in os.listdir(".")]
    for f in files:
        cmd = "ln -sf {0} ~/".format(f)
        print(cmd)
        os.system(cmd)
        
def main():
    p = argparse.ArgumentParser()
    p.add_argument("--home", action="store_true")
    args = p.parse_args()

    if args.home:
        home()

if __name__ == "__main__":
    main()
