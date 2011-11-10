#!/bin/bash

# Use this script to automatically produce several graph pdfs

BASE_NAME=ceal_batch
declare -a EXP_NAMES=( \
    exptree_eval \
    list_quicksort \
    list_mergesort \
);
declare -a EXP_SHORT_NAMES=( \
    "exptree" \
    "qs" \
    "ms" \
);
TESTPOWERS="cp verf"
CHANGE_SIZES="1 4 8 64 128 512 1024 4096"
RUNS=5

for ((exp_index=0; exp_index<${#EXP_NAMES[@]}; exp_index++))
do
    exp_name=${EXP_NAMES[$exp_index]}
    exp_short_name=${EXP_SHORT_NAMES[$exp_index]}
    folders=""
    legends=""
    for testpower in TESTPOWERS
    do
        for change_size in CHANGE_SIZES
        do
            folder="${BASE_NAME}_${exp_name}_${testpower}_cs${change_size}_${RUNS}runs"
            if [ -d "../data/${folder}" ]
            then
                folders="${folders} ${folder}"
                legends="${legends} ${exp_short_name}_${testpower}_cs${change_size}"
            fi
        done
    done
    ./generate_graph_multi.sh "${folders}" "${legends}"
    mkdir ../data/output
    cp ../data/out.pdf "../data/output/${exp_name}.pdf"
    rm ../data/*.eps
    rm ../data/*.pdf
    chmod -R 775 ../data/output
done