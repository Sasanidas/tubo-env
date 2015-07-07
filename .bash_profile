#!/bin/bash
[ -e /etc/profile.d/autojump.bash ] && . /etc/profile.d/autojump.bash

function try_load ()
{
    for fn in $*; do
        [ -e $fn ] && . $fn
    done
}
source ~/.bashrc
# Import some shared setting.
try_load ~/.zshrc.d/01_env.zsh  ~/.zshrc.d/02_functions.zsh

test -e "${HOME}/.iterm2_shell_integration.bash" && source "${HOME}/.iterm2_shell_integration.bash"

