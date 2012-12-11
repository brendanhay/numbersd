#!/bin/bash

RANDOM=$$;
WORDS=(brood bucolic bungalow chatoyant comely conflate cynosure dalliance demesne demure denouement desuetude elixir eloquence ephemeral epiphany)
# WORDS=(desultory diaphanous dissemble dulcet ebullience effervescent efflorescence elision elixir eloquence embrocation emollient ephemeral epiphany erstwhile ethereal evanescent evocative fetching felicity forbearance fugacious furtive gambol glamour gossamer halcyon harbinger imbrication imbroglio imbue incipient ineffable ingénue inglenook insouciance inure labyrinthine lagniappe lagoon languor lassitude leisure lilt lissome lithe love mellifluous moiety mondegreen murmurous nemesis offing onomatopoeia opulent palimpsest panacea panoply pastiche penumbra petrichor plethora propinquity pyrrhic quintessential ratatouille ravel redolent riparian ripple scintilla sempiternal seraglio serendipity summery sumptuous surreptitious susquehanna susurrous talisman tintinnabulation umbrella untoward vestigial wafture wherewithal woebegone)

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
    metric="$(sample WORDS[@]):$(sample values[@])|$(sample TYPES[@])$rate"

    echo $metric
    echo -n $metric | nc -u ${HOST-"127.0.0.1"} ${CONNECT-"8125"} -c
}

while true
do
    emit
    sleep "0.1"
done
