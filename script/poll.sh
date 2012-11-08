#!/bin/bash

RANDOM=$$;

usage() {
    echo "Usage: -a [addr]"
    exit 1
}

while getopts ":a:" opt
do
    case $opt in
        a) ADDR=$OPTARG;;
        *) usage;;
    esac
done

echo "poll.sh -a $ADDR"

while true
do
    url="http://${ADDR-0.0.0.0:8126}/numbers.json"
    echo $url
    curl -f -s -m 1 $url > /dev/null
    sleep "5.${RANDOM:0:1}"
done
