# ~/.profile: executed by the command interpreter for login shells.
# This file is not read by bash(1), if ~/.bash_profile or ~/.bash_login
# exists.
# see /usr/share/doc/bash/examples/startup-files for examples.
# the files are located in the bash-doc package.

# the default umask is set in /etc/profile; for setting the umask
# for ssh logins, install and configure the libpam-umask package.
#umask 022

# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/bin" ] ; then
    PATH="$HOME/bin:$PATH"
fi
if [ -d "$HOME/.local/bin" ] ; then
    PATH="$HOME/.local/bin:$PATH"
fi

# if running bash
if [ -n "$BASH_VERSION" ]; then
    # include .bashrc if it exists
    if [ -f "$HOME/.bashrc" ]; then
	. "$HOME/.bashrc"
    fi
fi

# GCC stuff
export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'

# Go stuff
export GOPATH="$HOME/go"
export PATH="$GOPATH/bin:$PATH"

# Rust stuff
export PATH="$HOME/.cargo/bin:$PATH"

# JS stuff
if command -v yarn 1>/dev/null 2>&1; then
  export PATH="$(yarn global bin):$PATH"
fi

# Wayland stuff
export MOZ_ENABLE_WAYLAND=1
export MOZ_DBUS_REMOTE=1
#export GDK_BACKEND=wayland
export QT_QPA_PLATFORM=wayland-egl
export QT_QPA_PLATFORMTHEME="qt5ct"
export QT_WAYLAND_DISABLE_WINDOWDECORATION="1"
export _JAVA_AWT_WM_NONREPARENTING=1
export SDL_VIDEODRIVER=wayland
export XDG_CURRENT_DESKTOP=sway
export XDG_SESSION_TYPE=wayland

# GPG needs to know this stuff
echo "UPDATESTARTUPTTY" | gpg-connect-agent
export SSH_AUTH_SOCK="${XDG_RUNTIME_DIR}/gnupg/S.gpg-agent.ssh"
