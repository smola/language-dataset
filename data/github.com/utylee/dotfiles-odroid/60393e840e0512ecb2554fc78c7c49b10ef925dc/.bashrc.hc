# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples
# If not running interactively, don't do anything
case $- in
    *i*) ;;
      *) return;;
esac

# :0 은 왜그런지 모르겠는데 버전이 최신이라 그런지 절대 안되고 계속 no display 뜨고..
# sshd config에서 x11forward 부분 offset이 10부터 시작하고 xauth list로 봐도 :10 이길래 그렇게 해봤더니 되네
export DISPLAY=:0
export EDITOR=/usr/local/bin/vim
# git editor를 vim으로 바꾸는 환경변수 차원의 방법이랍니다
export GIT_EDITOR=vim


# don't put duplicate lines or lines starting with space in the history.
# See bash(1) for more options
HISTCONTROL=ignoreboth

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=1000
HISTFILESIZE=2000

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# If set, the pattern "**" used in a pathname expansion context will
# match all files and zero or more directories and subdirectories.
#shopt -s globstar

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "${debian_chroot:-}" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

# set a fancy prompt (non-color, unless we know we "want" color)
case "$TERM" in
    xterm-color) color_prompt=yes;;
	xterm-256color) color_prompt=yes;;
	screen-256color) color_prompt=yes;;
	xterm-256color-italic) color_prompt=yes;;
	screen-256color-italic) color_prompt=yes;;
esac

# uncomment for a colored prompt, if the terminal has the capability; turned
# off by default to not distract the user: the focus in a terminal window
# should be on the output of commands, not on the prompt

#force_color_prompt=yes

if [ -n "$force_color_prompt" ]; then
    if [ -x /usr/bin/tput ] && tput setaf 1 >&/dev/null; then
	# We have color support; assume it's compliant with Ecma-48
	# (ISO/IEC-6429). (Lack of such support is extremely rare, and such
	# a case would tend to support setf rather than setaf.)
	color_prompt=yes
    else
	color_prompt=
    fi
fi

# utylee's addition
#----
#export TERM=screen-256color-italic
#export TERM=xterm-256color-itali
#export TERM=xterm-256color-italic
#color_prompt=yes

alias ll='ls -lhF'
set CLICOLOR=1
parse_git_branch() {
   git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/(\1)/'
}
#-----
if [ "$color_prompt" = yes ]; then
    #PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '
    PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[01;31m\]$(parse_git_branch)\[\033[00m\]\$ '
else
    PS1='${debian_chroot:+($debian_chroot)}\u@\h:\w\$ '
fi
unset color_prompt force_color_prompt

# If this is an xterm set the title to user@host:dir
case "$TERM" in
xterm*|rxvt*)
    PS1="\[\e]0;${debian_chroot:+($debian_chroot)}\u@\h: \w\a\]$PS1"
    ;;
*)
    ;;
esac

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    #alias dir='dir --color=auto'
    #alias vdir='vdir --color=auto'

    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

# some more ls aliases
alias la='ls -Ah'
alias l='ls -CF'
alias scn='screen -h 10000'
alias lsb='lsb_release -a'
alias dt='tmux detach -a'
alias un='uname -a'
alias mount-MacBook='sudo mount -t cifs //192.168.0.103/Downloads /home/odroid/media/MacBook -o user=utylee,pass=sksmsqnwk11,uid=1001,gid=1001,dir_mode=0777,file_mode=0777,iocharset=utf8,nounix,sec=ntlmssp'
alias mount-seoruPC='sudo mount -t cifs //192.168.0.101/down /home/odroid/media/seoruPC -o user=seoru,pass=sksmsqnwk11,uid=1001,gid=1001,dir_mode=0777,file_mode=0777,iocharset=utf8'

alias mm='mount-MacBook'
alias ms='mount-seoruPC' 

# Add an "alert" alias for long running commands.  Use like so:
#   sleep 10; alert
alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

# Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.

if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if ! shopt -oq posix; then
  if [ -f /usr/share/bash-completion/bash_completion ]; then
    . /usr/share/bash-completion/bash_completion
  elif [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
  fi
fi

v() {
	sh ~/v.sh $@
}

vi0() {
	filename=$PWD/$1
	tmux send-keys -t vBLOG.0 ":e $filename" C-m
	tmux select-window -t vBLOG
	tmux select-pane -t vBLOG.0
}
vi1() {
	filename=$PWD/$1
	tmux send-keys -t vMISC.0 ":e $filename" C-m
	tmux select-window -t vMISC
	tmux select-pane -t vMISC.0
}

m() {
#echo $2 | mutt -s "$1" utylee@gmail.com -a "$3"
echo $2 | mutt -s "$1" utylee@gmail.com
}

ma() {
echo $2 | mutt -s "$1" utylee@gmail.com -a "$3"
#echo $2 | mutt -s "$1" utylee@gmail.com
}

# echo -ne   '\eP\e]12;#2AA198\a'  # Cursor       -> red
echo -ne   '\eP\e]12;#5F5FAF\a'  # Cursor       -> purple

export PYENV_ROOT="$HOME/.pyenv"
export PATH="$PYENV_ROOT/bin:$PATH"
export PATH="$HOME/.cargo/bin:$PATH"
eval "$(pyenv init -)"
pyenv virtualenvwrapper_lazy

#alias vi1='vim --servername misc --remote '
#alias vi0='vim --servername blog --remote '

alias t0='source ~/.tmuxset-blog'
alias tr0='source ~/.tmuxset-rust'
alias t1='source ~/.tmuxset-misc'
alias t2='source ~/.tmuxset-flask'
alias smi-sync='python ~/.virtualenvs/misc/src/smi-sync.py '
#eval "$(pyenv virtualenv-init -)"



[ -f ~/.fzf.bash ] && source ~/.fzf.bash
# glob 옵션은 ignore에 none을 줘서 ignore없이선행하게 합니다 문자열이 아닌 파일찾기이기 때문입니다
export FZF_DEFAULT_COMMAND='rg --files --hidden --follow --no-ignore'
export FZF_CTRL_T_COMMAND='rg --files --hidden --follow --no-ignore'
#export FZF_DEFAULT_COMMAND='rg --files --hidden --follow --glob ""'
#export FZF_DEFAULT_COMMAND='ag --hidden -g ""'
#export FZF_DEFAULT_COMMAND='ag --hidden --path-to-ignore ~/.ignore -g ""'
#alias ag='ag --path-to-ignore /home/odroid/.ignore'
#export FZF_DEFAULT_COMMAND='ag --hidden --ignore={"*css","*min.css","*min.js"} -g ""'
#export FZF_DEFAULT_COMMAND='ag --hidden --path-to-ignore ~/.ignore -g ""'
#export FZF_DEFAULT_COMMAND='ag -l --path-to-ignore ~/.ignore --nocolor --hidden -g ""'

#export FZF_DEFAULT_COMMAND='ag --ignore={"*json","*.min.css","*.min.js"}'
