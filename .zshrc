[[ $TERM == "dumb" ]] && unsetopt zle && PS1='$ ' && return

############################
# * COMPLETION

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

bindkey '^[[Z' reverse-menu-complete
zstyle ':completion:*' menu select

# End of lines added by compinstall

############################
# * Basic options

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
setopt autocontinue

bindkey -e
# End of lines configured by zsh-newuser-install


# My stuff
eval "`dircolors -b`"

# Stop coredumps
ulimit -c 0

############################
# * History keys

autoload -U history-search-end

zle -N history-beginning-search-backward-end history-search-end
zle -N history-beginning-search-forward-end history-search-end

bindkey "\e[B" history-beginning-search-forward-end
bindkey "\e[A" history-beginning-search-backward-end

bindkey "^[[3~" delete-char
bindkey "^[3;5~" delete-char


############################
# * PROMPT

_git_repo_name() { 
    gittopdir=$(git rev-parse --git-dir 2> /dev/null)
    if [[ "foo$gittopdir" == "foo.git" ]]; then
        echo `basename $(pwd)`
    elif [[ "foo$gittopdir" != "foo" ]]; then
        echo `dirname $gittopdir | xargs basename`
    fi
}
_git_branch_name() {    
    #git branch 2>/dev/null | awk '/^\*/ { print $2 }'
    git branch 2>/dev/null | awk '/^\*/ { print }' | cut -d' ' -f2-
}    
 _git_is_dirty() { 
   git diff --quiet 2> /dev/null || echo '*'
 }

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
	#echo -n "[$RET] %{$fg[yellow]%}%*%{$reset_color%}"
	echo -n "[$RET] %{$fg[yellow]%}%T%{$reset_color%}"
}
function retlprompt() {
	dirty=$(_git_is_dirty)
	if [[ "$dirty" == "*" ]] ; then
		gitcolour="%{$fg[magenta]%}"
	else
		gitcolour="%{$fg[cyan]%}"
	fi
	branchname=$(_git_branch_name)
	if [[ -z "$branchname" ]] ; then
		gitstuff=""
	else
		gitstuff="${gitcolour}${branchname}%{$reset_color%} "
	fi

	#echo -n "%n@%m %U%{$fg[yellow]%}%~%{$reset_color%}%u$ "
	echo -n "%m ${gitstuff}%U%{$fg[yellow]%}%~%{$reset_color%}%u$ "
}

setopt prompt_subst
export PROMPT='$(retlprompt)'
export RPROMPT='$(retrprompt)'


unalias run-help
autoload run-help

############################
# * Highlighting

source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

############################
# * FZF
source /usr/share/fzf/completion.zsh
source /usr/share/fzf/key-bindings.zsh

. $HOME/z.sh

############################
# * gpg-agent

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

export GPG_TTY=$(tty)
#echo "UPDATESTARTUPTTY" | gpg-connect-agent

# Force an update for every ssh command
alias ssh='gpg-connect-agent updatestartuptty /bye;ssh'

alias rs="gpg-connect-agent updatestartuptty /bye;rsync -avzziu --info=progress2"

############################
# * Utils

function prompt_confirm() {
  while true; do
    read -r "REPLY?${1:-Continue?} [yes/no]: "
    case $REPLY in
      yes|YES) echo ; return 0 ;;
      no|NO) echo ; return 1 ;;
      *) printf " \033[31m %s \n\033[0m" "invalid input"
    esac 
  done  
}


#if [[ -f ~/.local/lib/python2.7/site-packages/Jug-1.2.2-py2.7.egg/EGG-INFO/scripts/jug ]]
#then
#	PATH="$PATH:$HOME/.local/lib/python2.7/site-packages/Jug-1.2.2-py2.7.egg/EGG-INFO/scripts"
#fi

############################
# * SUDO key

function add-sudo-in-front()
{
[[ -z $BUFFER ]] && zle up-history
#[[ $BUFFER != sudo\ * ]] && BUFFER="sudo -E $BUFFER"
[[ $BUFFER != s\ * && $BUFFER != sudo\ * ]] && BUFFER="s $BUFFER"
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

__remote_commands=(scp rsync rs)
autoload -U url-quote-magic
zle -N self-insert url-quote-magic
zstyle -e :urlglobber url-other-schema '[[ $__remote_commands[(i)$words[1]] -le ${#__remote_commands} ]] && reply=("*") || reply=(http https ftp)'
