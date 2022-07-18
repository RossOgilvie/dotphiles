#!/usr/bin/sh

### https://github.com/soyuka/tmux-current-pane-hostname

SCRIPT_DIR="/home/ross/.dotphiles/tmux/current_pane_hostname_scripts"

interpolation=('\#H' '\#{hostname_short}' '\#U' '\#\{pane_ssh_port\}' '\#\{pane_ssh_connected\}')
script=("#($SCRIPT_DIR/hostname.sh)" "#($SCRIPT_DIR/hostname_short.sh)" "#($SCRIPT_DIR/whoami.sh)" "#($SCRIPT_DIR/port.sh)" "#($SCRIPT_DIR/pane_ssh_connected.sh)")


source $SCRIPT_DIR/shared.sh

do_interpolation() {
    local interpolated=$1
    local j=0

    for i in "${interpolation[@]}"; do
        local s=${script[$j]}
        local interpolated=${interpolated//$i/$s}
        ((j+=1))
    done
    echo "$interpolated"
}

update_tmux_option() {
	local option=$1
	local option_value=$(get_tmux_option "$option")
	local new_option_value=$(do_interpolation "$option_value")
	set_tmux_option "$option" "$new_option_value"
}

main() {
	update_tmux_option "status-right"
	update_tmux_option "status-left"
}

main
