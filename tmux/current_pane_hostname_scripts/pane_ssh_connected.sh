#!/usr/bin/sh

SCRIPT_DIR="/home/ross/.dotphiles/tmux/current_pane_hostname_scripts"

source $SCRIPT_DIR/shared.sh

main() {
  if ssh_connected; then
      echo 1
  else
      echo 0
  fi
}

main
