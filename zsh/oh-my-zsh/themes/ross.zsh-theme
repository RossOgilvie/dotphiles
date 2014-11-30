function get_user_colour {
case $(whoami) in
	ross)
	ZSH_THEME_USER_COLOUR="magenta";;
	root)
	ZSH_THEME_USER_COLOUR="red";;
	*)
	ZSH_THEME_USER_COLOUR="green";;
esac
}

function get_host_colour() {
case $(hostname) in
	nyx)
	ZSH_THEME_HOST_COLOUR="blue";;
	eris)
	ZSH_THEME_HOST_COLOUR="cyan";;
	*)
	ZSH_THEME_HOST_COLOUR="yellow";;
esac
}

get_user_colour
get_host_colour

ZSH_THEME_GIT_PROMPT_PREFIX=" on "
ZSH_THEME_GIT_PROMPT_SUFFIX=""
ZSH_THEME_GIT_PROMPT_DIRTY=" ✗"
ZSH_THEME_GIT_PROMPT_UNTRACKED="?"
ZSH_THEME_GIT_PROMPT_CLEAN=" ✔"

precmd() { print "" }
PROMPT="%{$fg[$ZSH_THEME_USER_COLOUR]%}%n\
%{$fg[$ZSH_THEME_HOST_COLOUR]%}@%m:\
%{$reset_color%}%2~\
$(git_prompt_info)%{$reset_color%}   "
