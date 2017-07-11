#!/bin/bash

if [ ! -e $HOME/bin/rebar3 ]; then
   cd $HOME/bin
   wget https://s3.amazonaws.com/rebar3/rebar3
   chmod a+x rebar3;
fi
