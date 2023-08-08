#!/bin/env bash

set -o errexit -o nounset

if [[ "${TRACE-0}" == "1" ]]
then
  set -o xtrace
fi

if [ ! -f /etc/gentoo-release ]
then
  echo This script can only be executed on Gentoo Linux 1>&2
  exit 1
fi

rsync --archive --verbose --recursive --delete --files-from=./syncfiles / .
