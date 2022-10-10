# Return if requirements are not found.
if (( ! $+commands[tmux] )); then
  return 1
fi

#
# Aliases
#

# alias ta='tmux attach -t'
# alias ts='tmux new-session -s'
# alias tl='tmux list-sessions'
# alias tksv='tmux kill-server'
# alias tkss='tmux kill-session -t'

# Configuration variables
#
# Automatically start tmux
[[ -n "$ZSH_TMUX_AUTOSTART" ]] || ZSH_TMUX_AUTOSTART=false
# Only autostart once. If set to false, tmux will attempt to
# autostart every time your zsh configs are reloaded.
[[ -n "$ZSH_TMUX_AUTOSTART_ONCE" ]] || ZSH_TMUX_AUTOSTART_ONCE=true
# Automatically connect to a previous session if it exists
[[ -n "$ZSH_TMUX_AUTOCONNECT" ]] || ZSH_TMUX_AUTOCONNECT=true
# Automatically close the terminal when tmux exits
[[ -n "$ZSH_TMUX_AUTOQUIT" ]] || ZSH_TMUX_AUTOQUIT=$ZSH_TMUX_AUTOSTART
# The TERM to use for non-256 color terminals.
# Tmux states this should be screen, but you may need to change it on
# systems without the proper terminfo
[[ -n "$ZSH_TMUX_FIXTERM_WITHOUT_256COLOR" ]] || ZSH_TMUX_FIXTERM_WITHOUT_256COLOR="screen"
# The TERM to use for 256 color terminals.
# Tmux states this should be screen-256color, but you may need to change it on
# systems without the proper terminfo
[[ -n "$ZSH_TMUX_FIXTERM_WITH_256COLOR" ]] || ZSH_TMUX_FIXTERM_WITH_256COLOR="screen-256color"


# Get the absolute path to the current directory
# local zsh_tmux_abs_path="$(cd "$(dirname "$0")" && pwd)"

# Determine if the terminal supports 256 colors
if [[ `tput colors` == "256" ]]
then
	export ZSH_TMUX_TERM=$ZSH_TMUX_FIXTERM_WITH_256COLOR
else
	export ZSH_TMUX_TERM=$ZSH_TMUX_FIXTERM_WITHOUT_256COLOR
fi

local zsh_tmux_ross_session_name=`hostnamectl hostname`
# Wrapper function for tmux.
function _zsh_tmux_plugin_run()
{
	# We have other arguments, just run them
	if [[ -n "$@" ]]
	then
		\tmux $@
	# Try to connect to an existing session.
	elif [[ "$ZSH_TMUX_AUTOCONNECT" == "true" ]]
	then
		#How many sessions with this name are already running?
		tmux_nb=$(\tmux ls | grep "^$zsh_tmux_ross_session_name" | wc -l)

		if [[ "$tmux_nb" == "0" ]]; then
		    echo "Launching tmux base session $zsh_tmux_ross_session_name ..."
		    \tmux new-session -s $zsh_tmux_ross_session_name
		else
		    # Make sure we are not already in a tmux session
		    if [[ -z "$TMUX" ]]; then
		        # Kill defunct sessions first
		        old_sessions=$(\tmux ls 2>/dev/null | grep "^$zsh_tmux_ross_session_name""[0-9]\{14\}" | cut -f 1 -d:)
		        for old_session_id in $old_sessions; do
		            \tmux kill-session -t $old_session_id
		        done

		        echo "Launching copy of base session $zsh_tmux_ross_session_name ..."
		        # Session is is date and time to prevent conflict
		        session_id="$zsh_tmux_ross_session_name`date +%Y%m%d%H%M%S`"
                echo $session_id
		        # Create a new session (without attaching it) and link to base session
		        # to share windows
		        \tmux new-session -d -t $zsh_tmux_ross_session_name -s $session_id
		        # Attach to the new session
		        \tmux attach-session -t $session_id
		        # When we detach from it, kill the session
		        \tmux kill-session -t $session_id
		    fi
		fi
		[[ "$ZSH_TMUX_AUTOQUIT" == "true" ]] && exit
	# Just run tmux, fixing the TERM variable if requested.
	else
		\tmux `[[ "$ZSH_TMUX_ITERM2" == "true" ]] && echo '-CC '`
		[[ "$ZSH_TMUX_AUTOQUIT" == "true" ]] && exit
	fi
}

# Use the completions for tmux for our function
# compdef _tmux _zsh_tmux_plugin_run

# Alias tmux to our wrapper function.
alias tmux=_zsh_tmux_plugin_run

# Autostart if not already in tmux and enabled.
if [[ ! -n "$TMUX" && "$ZSH_TMUX_AUTOSTART" == "true" ]]
then
	# Actually don't autostart if we already did and multiple autostarts are disabled.
	if [[ "$ZSH_TMUX_AUTOSTART_ONCE" == "false" || "$ZSH_TMUX_AUTOSTARTED" != "true" ]]
	then
		export ZSH_TMUX_AUTOSTARTED=true
		_zsh_tmux_plugin_run
	fi
fi
