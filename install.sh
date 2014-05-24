#!/usr/bin/env bash
curl -fsSkL https://raw.github.com/cask/cask/master/go | python
ln -s ~/.cask/bin/cask ~/bin
