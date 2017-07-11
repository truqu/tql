#!/bin/bash

if [ ! -e $HOME/erlang/19.3 ]; then
    kerl update releases
    kerl build 19.3 19.3
    kerl install 19.3 ~/erlang/19.3
fi
