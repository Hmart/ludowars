#!/bin/sh
set -e

if [ -e /.installed ]; then
  echo 'Already installed.'
else
  echo ''
  echo 'INSTALLING'
  echo '----------'

  apt-get update
  apt-get -y install tmux erlang

  touch /.installed
fi

