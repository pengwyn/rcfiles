# The following lines were added by compinstall

fpath=(~/.zsh/completion $fpath)

zstyle ':completion:*' completer _expand _complete _ignored _approximate _prefix
#zstyle ':completion:*' completer _expand _complete
#zstyle ':completion:*' completions 1
zstyle ':completion:*' glob 1
#zstyle ':completion:*' matcher-list '' 'm:{[:lower:][:upper:]}={[:upper:][:lower:]}' 'r:|[._-]=* r:|=*' 'l:|=* r:|=*'
#zstyle ':completion:*' matcher-list '' 'm:{[:lower:][:upper:]}={[:upper:][:lower:]}' 'r:|[._-]=*'
#zstyle ':completion:*' matcher-list '' 'm:{[:lower:][:upper:]}={[:upper:][:lower:]}' '+b:=* e:=*'
zstyle ':completion:*' matcher-list '' 'm:{[:lower:][:upper:]}={[:upper:][:lower:]}' '+b:=* e:=*'
zstyle ':completion:*' max-errors 2
zstyle ':completion:*' substitute 1
zstyle :compinstall filename '/home/pengwyn/.zshrc'

## Stolen from grml-zshrc
	# start menu completion only if it could find no unambiguous initial string
	zstyle ':completion:*:correct:*'       insert-unambiguous true
	zstyle ':completion:*:corrections'     format $'%{\e[0;31m%}%d (errors: %e)%{\e[0m%}'
	zstyle ':completion:*:correct:*'       original true


	eval "`dircolors -b`"
	# activate color-completion
	zstyle ':completion:*:default'         list-colors ${(s.:.)LS_COLORS}

	# format on completion
	zstyle ':completion:*:descriptions'    format $'%{\e[0;31m%}completing %B%d%b%{\e[0m%}'

	# insert all expansions for expand completer
	zstyle ':completion:*:expand:*'        tag-order all-expansions
	zstyle ':completion:*:history-words'   list false

    # activate menu
    zstyle ':completion:*:history-words'   menu yes

    # ignore duplicate entries
    zstyle ':completion:*:history-words'   remove-all-dups yes
    zstyle ':completion:*:history-words'   stop yes

    # separate matches into groups
    zstyle ':completion:*:matches'         group 'yes'
    zstyle ':completion:*'                 group-name ''

    zstyle ':completion:*:messages'        format '%d'
    zstyle ':completion:*:options'         auto-description '%d'

    # describe options in full
    zstyle ':completion:*:options'         description 'yes'

    # on processes completion complete all user processes
    zstyle ':completion:*:processes'       command 'ps -au$USER'

    # provide verbose completion information
    zstyle ':completion:*'                 verbose true

    # set format for warnings
    zstyle ':completion:*:warnings'        format $'%{\e[0;31m%}No matches for:%{\e[0m%} %d'

    # Provide more processes in completion of programs like killall:
    zstyle ':completion:*:processes-names' command 'ps c -u ${USER} -o command | uniq'

    # complete manual by their section
    zstyle ':completion:*:manuals'    separate-sections true
    zstyle ':completion:*:manuals.*'  insert-sections   true
    zstyle ':completion:*:man:*'      menu yes select

    # provide .. as a completion
    zstyle ':completion:*' special-dirs ..

autoload -Uz compinit
compinit
# End of lines added by compinstall
# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=10000
SAVEHIST=10000
setopt appendhistory hist_expire_dups_first hist_ignore_space inc_append_history
#setopt glob_complete
#setopt menu_complete auto_list
setopt auto_list
setopt list_packed list_types
setopt noauto_remove_slash
setopt autocd extendedglob notify
#setopt completealiases
setopt longlistjobs
setopt auto_pushd pushd_ignore_dups
setopt noglobdots
#unsetopt nomatch

bindkey -e
# End of lines configured by zsh-newuser-install


# Stolen from graysky2 config
pagrep() {
	[[ -z "$1" ]] && echo 'Define a grep string and try again' && return 1
	find . -type f | parallel -k -j150% -n 1000 -m grep -H -n "$1" {}
}

tailc() { tail -n 40 "$1" | column -t; }

# My stuff
eval "`dircolors -b`"

autoload -U history-search-end

zle -N history-beginning-search-backward-end history-search-end
zle -N history-beginning-search-forward-end history-search-end

bindkey "\e[B" history-beginning-search-forward-end
bindkey "\e[A" history-beginning-search-backward-end

bindkey "^[[3~" delete-char
bindkey "^[3;5~" delete-char

function add-sudo-in-front()
{
[[ -z $BUFFER ]] && zle up-history
[[ $BUFFER != sudo\ * ]] && BUFFER="sudo -E $BUFFER"
zle end-of-line
}

# Don't delete whole paths
WORDCHARS=${WORDCHARS/\//}

zle -N add-sudo-in-front
bindkey "\C-o" add-sudo-in-front

function quote-word-on-cursor()
{
	zle set-mark-command
	zle backward-word
	zle quote-region
}

zle -N quote-word-on-cursor
bindkey "\C-p" quote-word-on-cursor

bindkey '^[[Z' reverse-menu-complete
zstyle ':completion:*' menu select

alias ls='ls --color=auto'
alias la='ls -lAh'
alias ipython='ipython --pylab --profile math'
alias ipython2='ipython2 --pylab --profile math'
alias ipython2noplot='"ipython2" --profile noplot'
alias ipython2r='MPLCONFIGDIR=$HOME/.config/matplotlib/nodisplay ipython2'
#alias pacman='pacman-color'
#alias netstat='netstat --numeric-hosts --inet -a'
alias ..='cd ..'

function gitpullall() {
	if [[ $# > 0 && $1 != "--rebase" ]]
	then
		echo "Takes only --rebase as an argument"
		return 2
	fi
	arg=$1

	echo "Fetching..."
	git fetch --all # || return 1
	if [[ $? != 0 ]]
	then
		echo "!!!!!! Errors in git fetch --all !!!!!!!!!"
	fi

	curbranch=$(git rev-parse --abbrev-ref HEAD)
	[[ -n "$curbranch" ]] || { echo "Problem with curbranch!"; return 1}

	for name in $(git remote show)
	do
		echo "Pulling ($arg) from ${name}..."
		git pull $arg $name $curbranch || return 1
	done
}

alias gs="git status"
alias gd="git diff"
alias ga="git add"
alias gc="git commit"
alias gca="git commit -a"
alias gpl="git pull"
alias gpla="gitpullall"
alias gps="git push"
alias gr="git remote"
alias g="git"

autoload -U colors && colors
function precmd() {
	RET="$?"
}
function retrprompt() {
	if [[ $RET = 0 ]]
	then 
		echo -n "%{$fg_bold[green]%}"
   	else
		echo -n "%{$fg_bold[red]%}"
	fi
	echo -n "[$RET] %{$fg[yellow]%}%*%{$reset_color%}"
}
function retlprompt() {
	echo -n "%n@%m %U%{$fg[yellow]%}%~%{$reset_color%}%u$ "
}

setopt prompt_subst
export PROMPT='$(retlprompt)'
export RPROMPT='$(retrprompt)'

export PYTHONPATH="$HOME/work4/python:$HOME/work_helium34/python:$HOME/work3/python"
#alias changepython2="export PYTHONPATH=$HOME/work2/python"

#export PYTHONDOCS='/usr/share/doc/python/html/'

export PATH="$PATH:$HOME/bin"

export EDITOR="vim"

export LESS=-R

# Less colours for man pages
export LESS_TERMCAP_mb=$(printf "\e[1;37m")
export LESS_TERMCAP_md=$(printf "\e[1;37m")
export LESS_TERMCAP_me=$(printf "\e[0m")
export LESS_TERMCAP_se=$(printf "\e[0m")
export LESS_TERMCAP_so=$(printf "\e[1;47;30m")
export LESS_TERMCAP_ue=$(printf "\e[0m")
export LESS_TERMCAP_us=$(printf "\e[0;36m")

# Quickfind
function qfind {
	find -iname "*$1*"
}

#alias qme='ssh zodiac bash -ic qme'

unalias run-help
autoload run-help

source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

. $HOME/z.sh

#eval $(gpg-agent --daemon --enable-ssh-support --disable-scdaemon)
# Doing this with manual setting because gpg-agent doesn't return the environment variables a second time.
evalstr=$(gpg-agent --daemon --enable-ssh-support --disable-scdaemon --log-file="$HOME/.gnupg/logfile" 2>/dev/null)
gpgret=$?
if [[ $gpgret == 0 ]]
then
	echo $evalstr > ~/.gnupg/evalstr
fi
if [[ ( $gpgret == 0 || $gpgret == 2 ) && -f ~/.gnupg/evalstr ]]
then
	eval $(cat ~/.gnupg/evalstr)
fi
#eval $(cat ~/.gnupg/evalstr)
#export SSH_AUTH_SOCK="$HOME/.gnupg/S.gpg-agent.ssh"

#export GPG_TTY=$(tty)
#echo "UPDATESTARTUPTTY" | gpg-connect-agent

# Force an update for every ssh command
alias ssh='gpg-connect-agent updatestartuptty /bye;ssh'

if [[ $(hostname) == "mixologist" ]]
then
	alias sq=squeue -l

	export PATH="$PATH:/cluster/admins/scripts"

	function cdjob() {
		# First try to serach for existing jobs in the queue
		#temp=`scontrol show job "$1" | grep WorkDir | cut -f2 -d'='`
		temp=`scontrol -o show job "$1" | pcregrep -o '(?<=WorkDir=)(.)*?(?= )'`
		#temp=`qstat -f1 "$1" | pcregrep -o '(?<=PBS_O_WORKDIR=)(.)*?(?=,)'`
		#temp=`qstat -f1 "$1" | grep PBS_O_WORKDIR | cut -d'PBS_O_WORKDIR=' -f2 | cut -d',' -f1`
		if [ -n "$temp" ] ; then
			cd $temp
			return
		fi

		echo "Couldn't find job in queue, looking for log file."

		temp=`find -name "$1.stdout" -print -quit`
		if [ -z "$temp" ] ; then
			echo "Cannot find job"
			return
		fi
		cd `dirname $temp`
	}

	function catjob() {
		temp=$(scontrol -o show job "$1" | sed -e 's:^.*StdOut=\([^[:space:]]*\).*$:\1:')
		if [[ -n "$temp" ]]
		then
			cat $temp
		fi
	}
fi
