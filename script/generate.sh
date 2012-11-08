#!/bin/bash

RANDOM=$$;
WORDFILE=/usr/share/dict/words
LINES=30 # $(cat $WORDFILE | wc -l)

word() {
    rnum=$((RANDOM*RANDOM%$LINES+1))
    printf `sed -n "$rnum p" $WORDFILE`
}

TYPES=(g c s ms)

usage() {
    echo "Usage: -l [port]"
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

while getopts ":l:" opt
do
    case $opt in
        l) PORT=$OPTARG;;
        *) usage;;
    esac
done

echo "generate.sh -l $PORT"

emit() {
    local samples rate metric

    values=(1 $(rand 10) $(rand 10000))
    samples=('' "|@0.${RANDOM:0:2}" "")
    rate=$(sample samples[@])
    metric="$(word):$(sample values[@])|$(sample TYPES[@])$rate"

    echo $metric
    echo -n $metric | nc -u ${HOST-"127.0.0.1"} ${PORT-"8125"} -c
}

while true
do
    emit
    sleep "0.${RANDOM:0:1}"
done
