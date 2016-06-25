
heroku_install() { wget -O- https://toolbelt.heroku.com/install-ubuntu.sh | sh }

heroku_login() {heroku auth:login --app $1 }

heroku_db() { heroku pg:psql --app $1 }  # use redis-cli instead

heroku_env() { heroku run env --app $1 }

heroku_run() { local app=$1; shift; heroku run --app $app $@ }  # python -m MODULE cmd

heroku_log() { heroku logs --app $1 --tail }

heroku_config() { heroku config --app $1 }

heroku_info() { heroku apps:info --app $1 }
