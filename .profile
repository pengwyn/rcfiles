export PATH="$PATH:$HOME/bin:$HOME/.local/bin"

export EDITOR="vim"
if [[ $(hostname) != "mixologist" ]] ; then
    export ALTERNATE_EDITOR="emacs"
	export VISUAL="emacsclient -c -n"
fi
export GIT_EDITOR="vim"

export TERMCMD="xfce4-terminal"




export PYTHONPATH="$HOME/work4/python:$HOME/work_helium34/python:$HOME/work3/python"

export LESS=-R

# Less colours for man pages
export LESS_TERMCAP_mb=$(printf "\e[1;37m")
export LESS_TERMCAP_md=$(printf "\e[1;37m")
export LESS_TERMCAP_me=$(printf "\e[0m")
export LESS_TERMCAP_se=$(printf "\e[0m")
export LESS_TERMCAP_so=$(printf "\e[1;47;30m")
export LESS_TERMCAP_ue=$(printf "\e[0m")
export LESS_TERMCAP_us=$(printf "\e[0;36m")

export PLOTS_DEFAULT_BACKEND=PyPlot

