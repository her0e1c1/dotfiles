echo "try to run install.sh"

if [ "$CODESPACES" ==  true ]; then
  #apt update -y
  apt install tig;
  apt install peco;
fi
