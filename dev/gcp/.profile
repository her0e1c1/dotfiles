# Required so `make bash` login shells enable direnv automatically inside the dev container.
if command -v direnv >/dev/null 2>&1; then
  case $- in
    *i*) eval "$(direnv hook bash)" ;;
  esac
fi
