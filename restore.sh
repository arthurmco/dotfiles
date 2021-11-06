#!/bin/sh

# This command will restore your files
#
# Note that no directory should have a dot, it will think that
# the directory is not a package and will not restore it.

stow $(ls | sed 's/.*\..*//g') && echo "Restored"
