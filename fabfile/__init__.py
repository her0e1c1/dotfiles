from fabric.api import *
from . import host
from . import hg
from . import start 
from . import gae


env.user = "root"
env.roledefs = {
    "prisoners": ["192.168.1.101"]
}
