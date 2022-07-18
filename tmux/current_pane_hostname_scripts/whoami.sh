#!/usr/bin/sh

SCRIPT_DIR="/home/ross/.dotphiles/tmux/current_pane_hostname_scripts"

source $SCRIPT_DIR/shared.sh

main() {
  get_info "whoami"
}

main
