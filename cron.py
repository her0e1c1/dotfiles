#!/usr/bin/env python3
import sched
import subprocess


def execute(cmd):
    print(cmd)
    subprocess.call(cmd, shell=True)

def repeat():
    s = sched.scheduler()
    while True:
        try:
            cmd = "docomo.py --new --send"
            s.enter(60 * 15, 1, execute, argument=(cmd,))  #15分毎
            s.run()
        except KeyboardInterrupt:
            print("terminated ...")
            break

repeat()
