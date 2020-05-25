
source ~/.profile

autoload -Uz compinit
compinit

fpath=( $fpath "$HOME/bin/completions" )

source /usr/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh

########################################
# * ALIASES

alias ls='ls --color=auto'
alias la='ls -lAh'
alias ipython='ipython --pylab --profile math'
alias ipython2='ipython2 --pylab --profile math'
alias ipython2noplot='"ipython2" --profile noplot'
alias ipython2r='MPLCONFIGDIR=$HOME/.config/matplotlib/nodisplay ipython2'
alias ..='cd ..'

alias s="sudo -E"
alias d="disown %%"

# Quickfind
function qfind {
	find -iname "*$1*"
}


function ssht () {
	#gpg-connect-agent updatestartuptty /bye
	ssh -t $@ "tmux new -A -s main"
}
compdef _ssh ssht=ssh
compdef _pacman aurman=pacman

############################
# * i3 things

function _danny-pwd-term {
  echo -ne "\033]0;$(pwd)\007"
}
add-zsh-hook chpwd _danny-pwd-term

if [[ $(pgrep -lx i3) ]] ; then
	function replacei3() {
		i3-msg 'split vertical ; layout tabbed' >/dev/null
		$*
		i3-msg 'move left' >/dev/null
	}
	# alias e3='replacei3 emacsclient -c -n'
	function e3() {
        emacsclient -c -n "$@"
        exit
    }
fi




############################
# * Emacs

alias et='emacsclient -c -t'

function e() {
    emacsclient -e "(my/open-file-maybe \"$1\")"
}

function ediff() {
	if [[ -z "${2}" ]]; then
		echo "USAGE: ediff <FILE 1> <FILE 2>"
	else
		# The --eval flag takes lisp code and evaluates it with EMACS
		emacs --eval "(ediff-files \"$1\" \"$2\")"
	fi
}



############################
# * Git

function gitpullall() {
	gpg-connect-agent updatestartuptty /bye

	arg=$1
	if [[ $# > 0 && $1 != "--rebase" ]]
	then
		echo "Takes only --rebase as an argument"
		return 2
	fi

	echo "Fetching..."
	git fetch --all # || return 1
	if [[ $? != 0 ]]
	then
		echo "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
		echo "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
		echo "!!!!!! Errors in git fetch --all !!!!!!!!!"
		echo "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
		echo "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
	fi

	curbranch=$(git rev-parse --abbrev-ref HEAD)
	[[ -n "$curbranch" ]] || { echo "Problem with curbranch!"; return 1}

	for name in $(git remote show)
	do
		echo "Pulling ($arg) from ${name}..."
		#git pull $arg $name $curbranch || return 1
		if [[ -z "$arg" ]]; then
			git merge --ff-only $name/$curbranch || return 1
		elif [[ "$arg" == "--rebase" ]]; then
			git rebase $name/$curbranch || return 1
		fi
	done
}

alias gs="git status"
alias gd="git diff"
alias ga="git add"
alias gc="git commit"
alias gca="git commit -a"
alias gpl="gpg-connect-agent updatestartuptty /bye; git pull"
alias gpla="gitpullall"
alias gps="git push"
#alias gr="git remote"
alias gl="git log --oneline --graph --branches --remotes --decorate"
alias gb="git branch"
alias gf="git fetch"
alias g="git"

alias grf="gr git fetch ; gr status"
function grpl() {
    gpg-connect-agent updatestartuptty /bye
    if (( $# < 1 )) ; then
        gr git pull
    else
        gr git pullthis $@
    fi
    gr status
}
function grs() {
    (( $# >= 1 )) && gr git setupto $@
    gr status
}

############################
# * Julia stuff

alias jupy="tmux new -s Jupy -d env JUPYTER_CONFIG_DIR=${HOME}/Dropbox/Physics/MyCalcs/JupyterNotebooks/.jupyter jupyter notebook --notebook-dir=${HOME}/Dropbox/Physics/MyCalcs/JupyterNotebooks"
alias jupyat="tmux attach -t Jupy"

export LD_LIBRARY_PATH="$LD_LIBRARY_PATH:/home/pengwyn/.julia/artifacts/e1bc7d06ffa3fc6dc77112d1bec9f3ffe0d0dfd5/lib"

sysimage="ijulia_sysimage.so"
alias j="'julia' --startup-file=no"
alias jl="'julia' --startup-file=no --load ${HOME}/.julia/config/reduced_startup.jl"
alias jul="'julia' --startup-file=no --sysimage ${HOME}/.julia/config/$sysimage"
alias juli="'julia' --startup-file=no --sysimage ${HOME}/.julia/config/$sysimage --load ${HOME}/.julia/config/reduced_startup.jl"
alias julia="julia --sysimage ${HOME}/.julia/config/$sysimage"

############################
# * Ranger

function ranger-cd {
    tempfile="$(mktemp -t tmp.XXXXXX)"
    ranger --choosedir="$tempfile" "${@:-$(pwd)}"
    test -f "$tempfile" &&
    if [ "$(cat -- "$tempfile")" != "$(echo -n `pwd`)" ]; then
        cd -- "$(cat "$tempfile")"
    fi
    rm -f -- "$tempfile"
}


alias r="ranger-cd"

############################
# * Full updates

alias pacupdatekernel="s pacman -Syu --needed linux linux-firmware"

function pacfull()
{
	ignored=( $(cat /etc/pacman.conf | awk '/^IgnorePkg/ {split($0,a,"=");print a[2]}') )
	# sudo pacman -Syu --needed "${ignored[@]}"
	# sudo pacman -Syu --needed $ignored
	#yay -Syu --needed $ignored
	/bin/yes | sudo pacman -Syu --needed $ignored
	yay -Syu --nocleanmenu --nodiffmenu --noeditmenu --noupgrademenu --noremovemake
}

############################
# * Cluster specific
#if [[ $(hostname) == "mixologist" ]]
# Need to do this so it works on all nodes too
if (which slurmd >/dev/null)
then
	function sq () {
		output=$(squeue -O jobarrayid,name,username,partition,state,timeused,batchhost,reason,starttime $* )
		print $output

awkcount='
BEGIN { cnt=0 }
$1 == "JOBID" { next }
match($1, /[[:digit:]]+_\[([[:digit:]]+)-([[:digit:]]+)\]/ , vals) { cnt = cnt + vals[2] - vals[1] + 1 ; next }
{ cnt++ }
END { print cnt }
'
		#jobnum=$(print $output | wc | awk '{print ($1 - 1)}')
		#myjobnum=$(print $output | grep pengwyn | wc | awk '{print $1}')
		jobnum=$(print $output | awk "$awkcount")
		myjobnum=$(print $output | grep pengwyn | awk "$awkcount")
		print "Number of jobs: $jobnum, number of my jobs: $myjobnum"
	}

	alias -g sacct='sacct --format=jobid,start,elapsed,user,jobname,partition,maxvmsize,avevmsize,maxrss,averss,mincpu,avecpu,ntasks,alloccpus,state,exitcode'

	function requeueallon () {
		if [[ $# != 1 ]] ; then
			echo "Need one argument"
			return 1
		fi
		joblist=( $(sq -u pengwyn -t running | grep $1 | awk '{print $1}') )
		if [[ ${#joblist} == 0 ]] ; then
			echo "No jobs running on $1 to requeue"
			return 1
		fi
		echo "Requeueing $joblist"
	   	scontrol requeue "$joblist"
	}

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

	function SLURMsetminenice() {
		for jobid in $(squeue -h -u pengwyn -O jobid)
		do
			scontrol update jobid=$jobid nice=10000
		done
	}

	function SLURMpausepending() {
		for jobid in $(squeue -h -u pengwyn -t pending -O jobid)
		do
			scontrol holdu $jobid
		done
	}

	function SLURMreleaseall() {
		for jobid in $(squeue -h -u pengwyn -O jobid)
		do
			scontrol release $jobid
		done
	}

	function SLURMcanceljobgrep() {
		if [[ $# != 1 ]]
		then
			echo "Need one argument!"
			return 1
		fi

		filteredlist=$(squeue -h -O "jobid,name" | awk '$2 ~ /'$1'/')

		if [[ -z "$filteredlist" ]]
		then
			echo "No jobs to cancel"
			return 0
		fi

		echo "Going to cancel the following:"
		echo $filteredlist
		
		prompt_confirm || return 0

		scancel $(echo $filteredlist | awk '{print $1}')
	}

	function rootssh() {
		ssh -i ~/.ssh/id_cluster_root "root@$1"
	}
	compdef _ssh rootssh=ssh

	alias emacs='emacs -nw'

	export NO_JULIA_PACKAGE_CHECK=nocheck

	function mkmachinefile() {
		#filename=$(mktemp)
		#srun zsh -c 'echo hostname $(hostname -s)' | grep hostname | awk '{ print $2 }' | sort > $filename
		#echo $filename
		
		result=$(srun zsh -c 'echo hostname $(hostname -s)' | grep hostname | awk '{ print $2 }' | sort)
		echo $result
	}
fi

