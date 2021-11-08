#!/bin/sh

# a more portable way to kill a process
# killall polybar does not work on NixOS
kill -9 $(ps ax | grep $(which polybar) | awk '{print $1}')

polybar example
