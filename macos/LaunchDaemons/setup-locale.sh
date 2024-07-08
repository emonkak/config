#!/usr/bin/env bash

set -o errexit -o nounset

if [[ "${TRACE:-0}" -ne 0 ]]
then
  set -o xtrace
fi

TARGET_UID=501
RETRY_INTERVAL=1
TIMEOUT=60

retry_seconds=0

while [[ ${retry_seconds} -lt ${TIMEOUT} ]]
do
  if launchctl print "gui/$TARGET_UID" > /dev/null
  then
    user_name="$(id -n -u ${TARGET_UID})"
    launchctl asuser ${TARGET_UID} launchctl setenv PATH_LOCALE "/Users/${user_name}/.locale"
    break
  fi
  sleep ${RETRY_INTERVAL}
  retry_seconds=$(( ${retry_seconds} + ${RETRY_INTERVAL} ))
done
