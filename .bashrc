# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# If not running interactively, don't do anything
case $- in
    *i*) ;;
      *) return;;
esac

# don't put duplicate lines in the history. See bash(1) for more options
# don't overwrite GNU Midnight Commander's setting of `ignorespace'.
# HISTCONTROL=$HISTCONTROL${HISTCONTROL+:}ignoredups
# ... or force ignoredups and ignorespace
HISTCONTROL=ignoreboth

# More history
HISTFILESIZE=
HISTSIZE=

# append to the history file, don't overwrite it
shopt -s histappend

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# lesspipe is a security risk
# http://seclists.org/fulldisclosure/2014/Nov/74
unset LESSOPEN LESSCLOSE

osc7() {
    local strlen=${#PWD}
    local encoded=""
    local pos c o
    for (( pos=0; pos<strlen; pos++ )); do
        c=${PWD:$pos:1}
        case "$c" in
            [-/:_.!\'\(\)~[:alnum:]] ) o="${c}" ;;
            * ) printf -v o '%%%02X' "'${c}" ;;
        esac
        encoded+="${o}"
    done
    printf '\e]7;file://%s%s\e\\' "${HOSTNAME}" "${encoded}"
}

osc33() {
    printf '\e]133;A\e\\'
}

prompt_command() {
    local RET=$?

    # flush history
    history -a

    # set the prefix to reflect the previous command's return code
    if [ ! "$RET" -eq 0 ]; then
        ps1_prefix_color="31"
    else
        ps1_prefix_color="32"
    fi
    ps1_prefix="\[\033[${ps1_prefix_color}m\][$RET]\[\033[00m\]"

    # set variable identifying the chroot you work in (used in the prompt below)
    if [ -r /etc/debian_chroot ]; then
        debian_chroot=":debian:$(cat /etc/debian_chroot)"
    fi

    # git branch of current directory
    git_branch=$(git rev-parse --abbrev-ref HEAD 2> /dev/null | awk '{ print ":git:" $1 }')

    ps1_path_suffix="${debian_chroot}${git_branch}"

    PS1="${ps1_prefix} \u@\h: \[\033[34m\]\w$ps1_path_suffix\[\033[00m\]\n\$ "

    # If this is an xterm set the title to user@host:dir
    case "$TERM" in
    xterm*|rxvt*|alacritty|foot)
        PS1="\[\e]0;\u@\h: \w$ps1_path_suffix\a\]$PS1"
        ;;
    *)
        ;;
    esac

    # If this is foot, emit OSC-7 and OSC-133
    if [ "$TERM" = "foot" ]; then
        osc7
        osc33
    fi
}
PROMPT_COMMAND=prompt_command

if [ -f "$HOME/.asdf/asdf.sh" ]; then
    . $HOME/.asdf/asdf.sh
fi

# direnv manipulates PROMPT_COMMAND after ours
if command -v direnv 1>/dev/null 2>&1; then
  eval "$(direnv hook bash)"
fi

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    alias dir='dir --color=auto'
    alias vdir='vdir --color=auto'

    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

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

# Extra completions
if command -v kubectl 1>/dev/null 2>&1; then
  source <(kubectl completion bash)
fi
if command -v minikube 1>/dev/null 2>&1; then
  source <(minikube completion bash)
fi
if command -v helm 1>/dev/null 2>&1; then
  source <(helm completion bash)
fi
if command -v k9s 1>/dev/null 2>&1; then
  source <(k9s completion bash)
fi
if command -v pipenv 1>/dev/null 2>&1; then
  source <(pipenv --completion)
fi
if command -v poetry 1>/dev/null 2>&1; then
  source <(poetry completions bash)
fi
if [ -f "$HOME/.asdf/completions/asdf.bash" ]; then
  . $HOME/.asdf/completions/asdf.bash
fi
