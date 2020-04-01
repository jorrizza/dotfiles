# ~/.profile: executed by the command interpreter for login shells.
# This file is not read by bash(1), if ~/.bash_profile or ~/.bash_login
# exists.
# see /usr/share/doc/bash/examples/startup-files for examples.
# the files are located in the bash-doc package.

# the default umask is set in /etc/profile; for setting the umask
# for ssh logins, install and configure the libpam-umask package.
#umask 022

# if running bash
if [ -n "$BASH_VERSION" ]; then
    # include .bashrc if it exists
    if [ -f "$HOME/.bashrc" ]; then
	. "$HOME/.bashrc"
    fi
fi

# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/bin" ] ; then
    PATH="$HOME/bin:$PATH"
fi

# GCC stuff
export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'

# Go stuff
export GOPATH="$HOME/go"
export PATH="$GOPATH/bin:$PATH"

# Rust stuff
export PATH="$HOME/.cargo/bin:$PATH"

# Chruby stuff
[[ -s /usr/local/share/chruby/chruby.sh ]] && source /usr/local/share/chruby/chruby.sh
[[ -s /usr/local/share/chruby/auto.sh ]] && source /usr/local/share/chruby/auto.sh

# Pyenv stuff
[[ -d "$HOME/.pyenv" ]] && export PYENV_ROOT="$HOME/.pyenv"
[[ -d "$HOME/.pyenv" ]] && export PATH="$PYENV_ROOT/bin:$PATH"
export WORKON_HOME=$HOME/.local/share/virtualenvs
if command -v pyenv 1>/dev/null 2>&1; then
  eval "$(pyenv init -)"
fi

# Poetry stuff
[[ -d "$HOME/.poetry" ]] && export PATH="$HOME/.poetry/bin:$PATH"

# sdkman (Java etc.) stuff
export SDKMAN_DIR="$HOME/.sdkman"
[[ -s "$HOME/.sdkman/bin/sdkman-init.sh" ]] && source "$HOME/.sdkman/bin/sdkman-init.sh"

# Wayland stuff
export MOZ_ENABLE_WAYLAND=1
#export GDK_BACKEND=wayland
export QT_QPA_PLATFORM=wayland-egl
export QT_WAYLAND_DISABLE_WINDOWDECORATION="1"
export _JAVA_AWT_WM_NONREPARENTING=1
export SDL_VIDEODRIVER=wayland

# GPG needs to know this stuff
echo "UPDATESTARTUPTTY" | gpg-connect-agent
export SSH_AUTH_SOCK="${XDG_RUNTIME_DIR}/gnupg/S.gpg-agent.ssh"
