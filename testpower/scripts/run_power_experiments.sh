#!/bin/bash

# BINARIES: binaries to run the experiments with; format: {"/binary/path binary_name","/binary/path binary_name",...}
# INPUT_SIZES: input sizes for experiments
# runs: number of runs to do each combination of binary and input size
# runtime: time a single run should take in micro seconds
# moretime: overprovisioning time, measurements should run for (get killed if the run finishes first) in seconds
# file: output filepath/name

declare -a BINARIES=( \
#'apps/exptree_eval/verf exptree_eval' \
#'apps/list_map/verf list_map' \
#'apps/list_filter/verf list_filter' \
#'apps/list_reverse/verf list_reverse' \
#'apps/list_sum/verf list_sum' \
#'apps/list_minimum/verf list_minimum' \
#'apps/list_quicksort/verf list_quicksort' \
#'apps/list_mergesort/verf list_mergesort' \
#'apps/geom2d_quickhull/verf geom2d_quickhull' \
#'apps/geom2d_diameter/verf geom2d_diameter' \
#'apps/geom2d_distance/verf geom2d_distance' \
'apps/exptree_eval/foreign-ring exptree_eval' \
'apps/list_map/foreign-ring list_map' \
'apps/list_filter/foreign-ring list_filter' \
'apps/list_reverse/foreign-ring list_reverse' \
'apps/list_sum/foreign-ring list_sum' \
'apps/list_minimum/foreign-ring list_minimum' \
'apps/list_quicksort/foreign-ring list_quicksort' \
'apps/list_mergesort/foreign-ring list_mergesort' \
'apps/geom2d_quickhull/foreign-ring geom2d_quickhull' \
'apps/geom2d_diameter/foreign-ring geom2d_diameter' \
'apps/geom2d_distance/foreign-ring geom2d_distance' \
);
declare -a INPUT_SIZES=( \
'10 100 1000 10000 20000 50000 80000 100000 200000 400000 500000 800000 1000000' \
'10 100 1000 10000 20000 50000 80000 100000 200000 400000 500000 800000 1000000' \
'10 100 1000 10000 20000 50000 80000 100000 200000 400000 500000 800000 1000000' \
'10 100 1000 10000 20000 50000 80000 100000 200000 400000 500000 800000 1000000' \
'10 100 1000 10000 20000 50000 80000 100000 200000 400000 500000 800000 1000000' \
'10 100 1000 10000 20000 50000 80000 100000 200000 400000 500000 800000 1000000' \
'10 100 1000 10000 20000 50000 80000 100000' \
'10 100 1000 10000 20000 50000 80000 100000' \
'10 100 1000 10000 20000 50000 80000 100000' \
'10 100 1000 10000 20000 50000 80000 100000' \
'10 100 1000 10000 20000 50000 80000 100000' \
);
declare -a CHANGE_SIZES=( \
'1 4 8 64 128 512 1024 4096' \
'1 4 8 64 128 512 1024 4096' \
'1 4 8 64 128 512 1024 4096' \
'1 4 8 64 128 512 1024 4096' \
'1 4 8 64 128 512 1024 4096' \
'1 4 8 64 128 512 1024 4096' \
'1 4 8 64 128 512 1024 4096' \
'1 4 8 64 128 512 1024 4096' \
'1 4 8 64 128 512 1024 4096' \
'1 4 8 64 128 512 1024 4096' \
'1 4 8 64 128 512 1024 4096' \
);
declare -a TESTPOWERS=( \
'cp' \
'cp' \
'cp' \
'cp' \
'cp' \
'cp' \
'cp' \
'cp' \
'cp' \
'cp' \
'cp' \
);
RUNS=5
RUNTIME=30000000
WATTSUP_TIME=120
FILE="../data/tmp/current_output"

function prep_files ()
{
    echo -n > "$FILE.nano.txt"
    echo -n > "$FILE.ops.txt"
    echo -n > "$FILE.av_bef.txt"
    echo -n > "$FILE.av_dur.txt"
    echo -n > "$FILE.av_diff.txt"
    echo -n > "$FILE.av_time.txt"
}

# $1: input_size
function write_zero ()
{
    echo "$1, 0" >> "$FILE.nano.txt"
    echo "$1, 0" >> "$FILE.ops.txt"
    echo "$1, 0" >> "$FILE.av_bef.txt"
    echo "$1, 0" >> "$FILE.av_dur.txt"
    echo "$1, 0" >> "$FILE.av_diff.txt"
    echo "$1, 0" >> "$FILE.av_time.txt"
}

# $1: binary
# $2: input_size
# $3: run
# $4: testpower
# $5: change_size
function single_run ()
{
    echo "========================= RUN ${3} for input size ${2}"
    echo "DEBUG: bin:$1; in_siz:$2; run:$3; tp:$4; c_siz:$5"
    ./measure_watts.sh $WATTSUP_TIME 0 "./run_power_experiments_wks-51-03.sh $1 $2 $RUNTIME $4 $5" \
	"${FILE}"
    ret=$?
    cp -r ../data/tmp "../data/tmp-$3-$2"
    chmod 775 "../data/tmp-$3-$2"
    rm -rf "../data/tmp-$3-$2/.svn"
    echo "==============================================================================="
    if [ $(echo "$ret==1" | bc) -eq 1 ]; then
        return 1
    fi
    return 0
}

echo "Type the name of the experiment (no whitespaces), followed by [ENTER]:"
read exname

for ((bin=0; bin<${#BINARIES[@]}; bin++))
do
    for testpower in `echo ${TESTPOWERS[$bin]}`
    do
        for change_size in `echo ${CHANGE_SIZES[$bin]}`
        do
            prep_files

            for input_size in `echo ${INPUT_SIZES[$bin]}`
            do
                write_zero "$input_size" "${FILE}.change${change_size}"
            done

            for ((run=1; run<=$RUNS; run++))
            do
                for input_size in `echo ${INPUT_SIZES[$bin]}`
                do
                    if [ $(echo "${change_size}<(${input_size}/4)" | bc) -eq 1 ]; then
                        single_run "${BINARIES[$bin]}" "$input_size" "$run" "$testpower" "$change_size"
                        if [ $(echo "$?==1" | bc) -eq 1 ]; then
                            break
                        fi
                    fi
                done
            done

            set -- ${BINARIES[$bin]}

            mkdir "../data/${exname}_${2}_${testpower}_cs${change_size}_${RUNS}runs"
            chmod 775 "../data/${exname}_${2}_${testpower}_cs${change_size}_${RUNS}runs"
            mv ../data/tmp* "../data/${exname}_${2}_${testpower}_cs${change_size}_${RUNS}runs/"
            mkdir "../data/tmp"
            chmod 775 "../data/tmp"
            cp "../data/${exname}_${2}_${testpower}_cs${change_size}_${RUNS}runs/tmp/current_ssh-agent.txt" "../data/tmp/"
        done
    done
done

rm -rf "../data/tmp"
