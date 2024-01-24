#!/bin/bash

export DISPLAY=:0.0
export HOME=/home/emonkak
export XAUTHORITY=$HOME/.Xauthority

match_line() {
  expected_line="$1"

  while read -r line
  do
    if [ "${line}" = "${expected_line}" ]
    then
      return 0
    fi
  done

  return 1
}

update_keymap() {
  name="$1"
  attempts=0

  while [ $attempts -lt 3 ]
  do
    if xinput list --name-only | match_line "${name}"
    then
      xkbcomp -w 0 -I"${HOME}/.xkb" "${HOME}/.xkb/keymap/my_keymap" "${DISPLAY}"
      notify-send -i dialog-information "Keyboard '$name' is plugged in and ready for use now."
      break
    fi

    sleep 1

    attempts=$(( $attempts + 1 ))
  done
}

update_keymap "$@" &
