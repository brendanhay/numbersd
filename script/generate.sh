#!/bin/bash

RANDOM=$$;
WORDFILE=/usr/share/dict/words
LINES=10000

word() {
    rnum=$((RANDOM*RANDOM%$LINES+5))
    printf $(sed -n "$rnum p" $WORDFILE)
}


usage() {
    echo "Usage: -c [port]"
    exit 1
}

rand() {
    printf $(($1 *  RANDOM  / 32767))
}

sample() {
    declare -a ary=("${!1}")
    unset ary[0]
    printf $'%s\n' "${ary[$(($(rand "${#ary[*]}")+1))]}"
}

while getopts ":c:" opt
do
    case $opt in
        c) CONNECT=$OPTARG;;
        *) usage;;
    esac
done

echo "generate.sh -c $CONNECT"

TYPES=('' g c s ms)

emit() {
    local samples rate metric

    values=(1 $(rand 10) $(rand 10000))
    samples=('' "|@0.${RANDOM:0:2}" "")
    rate=$(sample samples[@])
    metric="$(word):$(sample values[@])|$(sample TYPES[@])$rate"

    echo $metric
    echo -n $metric | nc -u ${HOST-"127.0.0.1"} ${CONNECT-"8125"} -c
}

while true
do
    emit
    sleep "0.${RANDOM:0:1}"
done
