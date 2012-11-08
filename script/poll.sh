#!/bin/bash

RANDOM=$$;

usage() {
    echo "Usage: -h [host] -p [port]"
    exit 1
}

while getopts ":h:p" opt
do
    case $opt in
        h) HOST=$OPTARG;;
        p) PORT=$OPTARG;;
        *) usage;;
    esac
done

while true
do
    url="http://${HOST-0.0.0.0}:${PORT-8126}/numbers.json"
    curl -f -m 1 $url
    sleep "5.${RANDOM:0:1}"
done
