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

# bash completion with cache
cached_completion() {
    if ! command -v "$1" 1>/dev/null 2>&1; then
        return
    fi
    if [ -z "$XDG_RUNTIME_DIR" ]; then
        return
    fi

    if [ ! -d "$XDG_RUNTIME_DIR/completions" ]; then
        mkdir "$XDG_RUNTIME_DIR/completions"
    fi

    if [ ! -f "$XDG_RUNTIME_DIR/completions/$1" ]; then
        $@ | tee "$XDG_RUNTIME_DIR/completions/$1" > /dev/null
    fi

    source "$XDG_RUNTIME_DIR/completions/$1"
}

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
cached_completion kubectl completion bash
cached_completion minikube completion bash
cached_completion helm completion bash
cached_completion helmfile completion bash
cached_completion k9s completion bash
cached_completion pipenv --completion
cached_completion poetry completions bash
cached_completion just --completions bash
if [ -f "$HOME/.asdf/completions/asdf.bash" ]; then
  . $HOME/.asdf/completions/asdf.bash
fi
