#!/usr/bin/env bash
BASE_DIR=$(cd "$(dirname "$0")"; pwd)
if [ ! -f "$HOME/bin/cask" ]
then
    curl -fsSkL https://raw.github.com/cask/cask/master/go | python
fi
if [ ! -d  "$HOME/bin" ]
then
    mkdir $HOME/bin
fi
ln -sf ~/.cask/bin/cask ~/bin
if [ -d "$HOME/.emacs.d" ]
then
    echo "rename existing .emacs.d to .emacs.d.old"
    mv $HOME/.emacs.d $HOME/.emacs.d.old
fi
ln -sf $BASE_DIR $HOME/.emacs.d
