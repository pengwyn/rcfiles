#!/bin/zsh

if [ -z "$1" ] ; then
    echo "Need one argument"
    exit 1
fi

target_dir=$1
[ -d $target_dir ] || exit 1

source=$(dragon --target --and-exit --print-path "$target_dir")

[ $? = 0 ] || exit 1
[ -n $source ] || exit 1
[ ! -d $source ] || exit 1

cp $source $target_dir
