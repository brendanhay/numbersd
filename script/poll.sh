#!/bin/bash

RANDOM=$$;

usage() {
    echo "Usage: -c [port]"
    exit 1
}

while getopts ":c:" opt
do
    case $opt in
        c) CONNECT=$OPTARG;;
        *) usage;;
    esac
done

echo "poll.sh -c $CONNECT"

while true
do
    url="http://127.0.0.1:${CONNECT-8126}/numbersd.whisper"
    echo $url
    curl -f -m 1 $url > /dev/null
    sleep "5.${RANDOM:0:1}"
done
