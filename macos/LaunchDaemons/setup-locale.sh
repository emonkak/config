#!/usr/bin/env bash

set -o errexit -o nounset

if [[ "${TRACE-0}" == "1" ]]
then
  set -o xtrace
fi

TARGET_UID=501
RETRY_INTERVAL=1
MAX_RETRIES=10

retry_count=0
domain_created=0

while [[ ${retry_count} -lt ${MAX_RETRIES} ]]
do
  if launchctl print "gui/$TARGET_UID" > /dev/null
  then
    domain_created=1
    break
  fi
  sleep ${RETRY_INTERVAL}
  retry_count=$(( ${retry_count} + 1 ))
done

if [[ ${domain_created} -eq 1 ]]
then
  launchctl asuser ${TARGET_UID} launchctl setenv PATH_LOCALE /Users/emonkak/.locale
fi
