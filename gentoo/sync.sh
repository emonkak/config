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

rsync --archive --verbose --recursive --delete --files-from=<(cat <<EOF
/etc/X11/xorg.conf
/etc/default/grub
/etc/fstab
/etc/portage/
/etc/udev/rules.d/
/etc/udev/update-keymap.sh
/usr/src/linux/.config
/var/lib/portage/world
EOF) / .
