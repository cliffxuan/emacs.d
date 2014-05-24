#!/usr/bin/env bash
curl -fsSkL https://raw.github.com/cask/cask/master/go | python
if [ ! -d  "$HOME/bin" ]
then
    mkdir $HOME/bin
fi
ln -s ~/.cask/bin/cask ~/bin
