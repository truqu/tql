#!/bin/bash

if [ ! -e $HOME/bin/kerl ]; then
   cd $HOME/bin
   wget https://raw.githubusercontent.com/kerl/kerl/master/kerl
   chmod a+x kerl;
fi
