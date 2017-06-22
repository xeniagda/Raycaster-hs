#!/bin/bash
stack build
if [[ $? != 0 ]] ; then
    echo "Build failed"
    exit 1
fi

STACK_PATH=$(stack path --local-install-root)
cp "$STACK_PATH/bin/Raycaster-exe" .

if [[ "$*" != "-R" ]] ; then
    ./Raycaster-exe $*
fi