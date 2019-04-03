
autoload -Uz compinit
compinit

########################################
# * ALIASES

alias ls='ls --color=auto'
alias la='ls -lAh'
alias ipython='ipython --pylab --profile math'
alias ipython2='ipython2 --pylab --profile math'
alias ipython2noplot='"ipython2" --profile noplot'
alias ipython2r='MPLCONFIGDIR=$HOME/.config/matplotlib/nodisplay ipython2'
#alias pacman='pacman-color'
#alias netstat='netstat --numeric-hosts --inet -a'
alias ..='cd ..'

if [[ $(hostname) != "mixologist" ]]
then
	alias julia='julia -J /home/pengwyn/.julia/dev/PackageCompiler/sysimg/sys.so'
fi

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
alias gr="git remote"
alias gl="git log --oneline --graph --all --decorate"
alias gb="git branch"
alias gf="git fetch"
alias g="git"

# Quickfind
function qfind {
	find -iname "*$1*"
}

function ediff() {
	if [[ -z "${2}" ]]; then
		echo "USAGE: ediff <FILE 1> <FILE 2>"
	else
		# The --eval flag takes lisp code and evaluates it with EMACS
		emacs --eval "(ediff-files \"$1\" \"$2\")"
	fi
}

function ssht () {
	#gpg-connect-agent updatestartuptty /bye
	ssh -t $@ "tmux new -A -s main"
}
compdef _ssh ssht=ssh
compdef _pacman aurman=pacman

alias s="sudo -E"
alias d="disown %%"

#alias jupy="jupyter notebook --notebook-dir=${HOME}/Dropbox/Physics/MyCalcs/JupyterNotebooks"
#alias jupy="tmux new -s Jupy -d jupyter notebook --notebook-dir=${HOME}/Dropbox/Physics/MyCalcs/JupyterNotebooks"
alias jupy="tmux new -s Jupy -d env JUPYTER_CONFIG_DIR=${HOME}/Dropbox/Physics/MyCalcs/JupyterNotebooks/.jupyter jupyter notebook --notebook-dir=${HOME}/Dropbox/Physics/MyCalcs/JupyterNotebooks"
alias jupyat="tmux attach -t Jupy"

alias pacupdatekernel="s pacman -Syu --needed linux linux-firmware"

############################
# * Common ENV vars

export PYTHONPATH="$HOME/work4/python:$HOME/work_helium34/python:$HOME/work3/python"
#alias changepython2="export PYTHONPATH=$HOME/work2/python"

#export PYTHONDOCS='/usr/share/doc/python/html/'

export PATH="$PATH:$HOME/bin:$HOME/.local/bin"

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

export PLOTS_DEFAULT_BACKEND=PyPlot

############################
# * Full updates

function pacfull()
{
	ignored=( $(cat /etc/pacman.conf | awk '/^IgnorePkg/ {split($0,a,"=");print a[2]}') )
	# sudo pacman -Syu --needed "${ignored[@]}"
	# sudo pacman -Syu --needed $ignored
	#yay -Syu --needed $ignored
	/bin/yes | sudo pacman -Syu --needed $ignored
	yay -Syu
}

############################
# * Cluster specific
if [[ $(hostname) == "mixologist" ]]
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
fi

