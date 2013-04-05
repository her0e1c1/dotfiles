import os

print("this is executed only once when user has logined")

def exec(cmd):
    print(cmd)
    os.system(cmd)

watchmedo_dir = os.path.expanduser("~/git/sphinx_document")
os.chdir(watchmedo_dir)
cmd = ('watchmedo shell-command --patterns="*.rst" '
       ' --recursive --wait --command="make html"')
exec(cmd)
