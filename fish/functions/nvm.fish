function nvm
  NVM_DIR=~/.nvm bash -c 'source $NVM_DIR/nvm.sh --no-use && nvm "$@"' (status filename) $argv
end
