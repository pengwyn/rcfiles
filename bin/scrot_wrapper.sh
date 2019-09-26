#!/bin/zsh

common=( --exec 'dragon $f' '/tmp/screenshot_%Y-%m-%d.png' )
opt=""
case "$1" in
     region)
            opt="--select"
            ;;
     window)
         opt="--focused"
         ;;
     *)
         echo "Unknown option $1"
         exit 1
esac
           
scrot $opt $common
