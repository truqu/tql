#!/bin/bash

KERL_VERSION=20.2

if [ ! -e $HOME/erlang/$KERL_VERSION ]; then
    kerl update releases
    kerl build $KERL_VERSION $KERL_VERSION
    kerl install $KERL_VERSION ~/erlang/$KERL_VERSION
fi
