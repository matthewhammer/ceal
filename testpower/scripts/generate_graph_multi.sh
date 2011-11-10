#!/bin/bash

# Use this script to generate graphs for different runs of run_power_experiments.sh
# Usage: $ generate_graph_multi.sh folders legends

# graph: points connected with lines
# bar: bars no x axis zoom or log scale
# ratio_graph: same as for graph but showing ratio of right_neighbor/value
# ratio_bar: same as for bar but showing ratio of right_neighbor/value
GRAPH_TYPE="graph"
declare -a TYPES=( \
    nano \
    av_time \
    ops \
#    av_diff \
#    av_dur \
#    av_bef \
)
declare -a TITLES=( \
    'Energy used per update (for change propagation / rerun verifier)' \
    'Time used per update (for change propagation / rerun verifier)' \
    'Total number of updates' \
#    'Difference in average energy consumption before and during the experiment' \
#    'Average energy consumption during the experiment' \
#    'Average energy consumption before the experiment' \
    )
declare -a XLABELS=( \
    'Input size in number of elements [numElem]' \
    'Input size in number of elements [numElem]' \
    'Input size in number of elements [numElem]' \
#    'Input size in number of elements [numElem]' \
#    'Input size in number of elements [numElem]' \
#    'Input size in number of elements [numElem]' \
    )
declare -a YLABELS=( \
    'Energy per update [nanoJoule/up]' \
    'Time per update [microSec/up]' \
    'Total number of updates [up]' \
#    'Difference in average energy consumption before/during experiment [watts]' \
#    'Average energy consumption during experiment [watts]' \
#    'Average energy consumption before experiment [watts]' \
    )
# 0: no zoom
# 1: no zoom; log scale
declare -a ZOOM_Y_LEVELS=( \
    '0 1' \
    '0 1' \
    '0 1' \
#    '0' \
#    '0' \
#    '0' \
    )
# 0: no zoom
# 1: no zoom; log scale
declare -a ZOOM_X_LEVELS=( \
    '0 1' \
    '0 1' \
    '0 1' \
#    '0' \
#    '0' \
#    '0' \
    )

eval "declare -a FOLDERS=( $1 )"
eval "declare -a LEGENDS=( $2 )"

function colorize_eps_soft ()
{
    sed -i 's/LC1 {0 1 0} def/LC1 {0.45 0.45 0.45} def/g' ../data/tmp/imageOutput.eps
    sed -i 's/LC2 {0 0 1} def/LC2 {0.95 0.35 0.38} def/g' ../data/tmp/imageOutput.eps
    sed -i 's/LC3 {1 0 1} def/LC3 {0.48 0.76 0.42} def/g' ../data/tmp/imageOutput.eps
    sed -i 's/LC4 {0 1 1} def/LC4 {0.35 0.61 0.83} def/g' ../data/tmp/imageOutput.eps
    sed -i 's/LC5 {1 1 0} def/LC5 {0.98 0.65 0.36} def/g' ../data/tmp/imageOutput.eps
    sed -i 's/LC6 {0 0 0} def/LC6 {0.62 0.40 0.67} def/g' ../data/tmp/imageOutput.eps
    sed -i 's/LC7 {1 0.3 0} def/LC7 {0.81 0.44 0.35} def/g' ../data/tmp/imageOutput.eps
    sed -i 's/LC8 {0.5 0.5 0.5} def/LC8 {0.84 0.50 0.71} def/g' ../data/tmp/imageOutput.eps
}

function colorize_eps_bright ()
{
    sed -i 's/LC1 {0 1 0} def/LC1 {0 0.01 0.01} def/g' ../data/tmp/imageOutput.eps
    sed -i 's/LC2 {0 0 1} def/LC2 {0.93 0.18 0.18} def/g' ../data/tmp/imageOutput.eps
    sed -i 's/LC3 {1 0 1} def/LC3 {0 0.55 0.28} def/g' ../data/tmp/imageOutput.eps
    sed -i 's/LC4 {0 1 1} def/LC4 {0.09 0.35 0.66} def/g' ../data/tmp/imageOutput.eps
    sed -i 's/LC5 {1 1 0} def/LC5 {0.96 0.49 0.14} def/g' ../data/tmp/imageOutput.eps
    sed -i 's/LC6 {0 0 0} def/LC6 {0.40 0.17 0.57} def/g' ../data/tmp/imageOutput.eps
    sed -i 's/LC7 {1 0.3 0} def/LC7 {0.64 0.11 0.13} def/g' ../data/tmp/imageOutput.eps
    sed -i 's/LC8 {0.5 0.5 0.5} def/LC8 {0.71 0.22 0.58} def/g' ../data/tmp/imageOutput.eps
}

# $1: type
function gen_bar_plot_cmd ()
{
    echo -n "plot \"../data/tmp/current_archive_output.${1}.txt\" using 2:xtic(1) title \"${LEGENDS[0]}\" fs pattern 3 lt -1 lc 2" >> ../data/tmp/plot.gnu
    for ((folder_index=1; folder_index<${#FOLDERS[@]}; folder_index++))
    do
        folder=${FOLDERS[$folder_index]}
        color=$(echo "(${folder_index}%8)+2" | bc)
        column=$(echo "${folder_index}+2" | bc)
	echo -n ", \"\" using ${column} title \"${LEGENDS[$folder_index]}\" fs pattern 3 lt -1 lc ${color}" >> ../data/tmp/plot.gnu
    done
    echo >> ../data/tmp/plot.gnu
}

# $1: type
function gen_graph_plot_cmd ()
{
    echo -n "plot \"../data/tmp/current_archive_output.${1}.txt\" using 1:2 title \"${LEGENDS[0]}\" with linespoints lt -1 lc 2" >> ../data/tmp/plot.gnu
    for ((folder_index=1; folder_index<${#FOLDERS[@]}; folder_index++))
    do
        folder=${FOLDERS[$folder_index]}
        color=$(echo "(${folder_index}%8)+2" | bc)
        column=$(echo "${folder_index}+2" | bc)
	echo -n ", \"\" using 1:${column} title \"${LEGENDS[$folder_index]}\" with linespoints lt -1 lc ${color}" >> ../data/tmp/plot.gnu
    done
    echo >> ../data/tmp/plot.gnu
}

# $1: type index
# $2: filename addition
function plot ()
{
    gnuplot ../data/tmp/plot.gnu
    if [ "$GRAPH_TYPE" == "bar" ] || [ "$GRAPH_TYPE" == "ratio_bar" ]
    then
        colorize_eps_soft
    else
        colorize_eps_bright
    fi
    ty=${TYPES[$1]}
    cp "../data/tmp/imageOutput.eps" "../data/${ty}${2}.eps"
    epstopdf "--outfile=../data/${ty}${2}.pdf" "../data/${ty}${2}.eps"
    chmod 775 "../data/${ty}${2}.eps"
    chmod 775 "../data/${ty}${2}.pdf"
    if [ -w "../data/out.pdf" ]; then
        pdftk "../data/out.pdf" "../data/${ty}${2}.pdf" cat output "../data/newout.pdf"
	rm ../data/out.pdf
	mv ../data/newout.pdf ../data/out.pdf
    else
	mv "../data/${ty}${2}.pdf" ../data/out.pdf
    fi
}

# $1: type index
# $2: zoom index
function print ()
{
    title_add=""
    file_add=""
# setup x axis
    if [ "$GRAPH_TYPE" != "bar" ] && [ "$GRAPH_TYPE" != "ratio_bar" ]
    then
        if [ "${x_zoom[$zoom_index]}" == "1" ]
        then
            sed -i 's/##set logscale x/set logscale x/g' ../data/tmp/plot.gnu
            sed -i 's/#set logscale x/set logscale x/g' ../data/tmp/plot.gnu
            title_add=`echo "${title_add} | x axis: log scale, no zoom"`
            file_add=`echo "${file_add}_xLogNoZoom"`
        elif [ "${x_zoom[$zoom_index]}" != "0" ]
        then
            sed -i 's/##unset autoscale x/unset autoscale x/g' ../data/tmp/plot.gnu
            sed -i 's/#unset autoscale x/unset autoscale x/g' ../data/tmp/plot.gnu
            sed -i "s/set xrange \[1:\*\]/set xrange \[0:${x_zoom[$zoom_index]}\]/g" ../data/tmp/plot.gnu
            title_add=`echo "${title_add} | x axis: lin scale, zoom ${x_zoom[$zoom_index]}"`
            file_add=`echo "${file_add}_xLinZoom${x_zoom[$zoom_index]}"`
        else
            sed -i "s/set xrange \[1:\*\]/set xrange \[0:\*\]/g" ../data/tmp/plot.gnu
            title_add=`echo "${title_add} | x axis: lin scale, no zoom"`
            file_add=`echo "${file_add}_xLinNoZoom"`
        fi
    fi
# setup y axis
    if [ "${y_zoom[$zoom_index]}" == "1" ]
    then
        sed -i 's/##set logscale y/set logscale y/g' ../data/tmp/plot.gnu
        sed -i 's/#set logscale y/set logscale y/g' ../data/tmp/plot.gnu
        title_add=`echo "${title_add} | y axis: log scale, no zoom"`
        file_add=`echo "${file_add}_yLogNoZoom"`
    elif [ "${y_zoom[$zoom_index]}" != "0" ]
    then
        sed -i 's/##unset autoscale y/unset autoscale y/g' ../data/tmp/plot.gnu
        sed -i 's/#unset autoscale y/unset autoscale y/g' ../data/tmp/plot.gnu
        sed -i "s/set yrange \[1:\*\]/set yrange \[0:${y_zoom[$zoom_index]}\]/g" ../data/tmp/plot.gnu
        title_add=`echo "${title_add} | y axis: lin scale, zoom ${y_zoom[$zoom_index]}"`
        file_add=`echo "${file_add}_yLinZoom${y_zoom[$zoom_index]}"`
    else
        sed -i "s/set yrange \[1:\*\]/set yrange \[0:\*\]/g" ../data/tmp/plot.gnu
        title_add=`echo "${title_add} | y axis: lin scale, no zoom"`
        file_add=`echo "${file_add}_yLinNoZoom"`
    fi
# set title and labels
    ti=`echo "${TITLES[${1}]}${title_add}"`
    xl=`echo "${XLABELS[${1}]}"`
    yl=`echo "${YLABELS[${1}]}"`
    sed -i "/set title/ c\set title \"${ti}\"" ../data/tmp/plot.gnu
    sed -i "/set xlabel/ c\set xlabel \"${xl}\"" ../data/tmp/plot.gnu
    sed -i "/set ylabel/ c\set ylabel \"${yl}\"" ../data/tmp/plot.gnu

# plot
    plot "$1" "$file_add"

# reverse title and labels
    sed -i "/set title/ c\set title" ../data/tmp/plot.gnu
    sed -i "/set xlabel/ c\set xlabel" ../data/tmp/plot.gnu
    sed -i "/set ylabel/ c\set ylabel" ../data/tmp/plot.gnu
# reverse x axis setup
    if [ "$GRAPH_TYPE" != "bar" ]
    then
        if [ "${x_zoom[$zoom_index]}" == "1" ]
        then
            sed -i 's/set logscale x/#set logscale x/g' ../data/tmp/plot.gnu
        elif [ "${x_zoom[$zoom_index]}" != "0" ]
        then
            sed -i "s/set xrange \[0:${x_zoom[$zoom_index]}\]/set xrange \[1:\*\]/g" ../data/tmp/plot.gnu
            sed -i 's/unset autoscale x/#unset autoscale x/g' ../data/tmp/plot.gnu
        else
            sed -i "s/set xrange \[0:\*\]/set xrange \[1:\*\]/g" ../data/tmp/plot.gnu
        fi
    fi
# reverse y axis setup
    if [ "${y_zoom[$zoom_index]}" == "1" ]
    then
        sed -i 's/set logscale y/#set logscale y/g' ../data/tmp/plot.gnu
    elif [ "${y_zoom[$zoom_index]}" != "0" ]
    then
        sed -i "s/set yrange \[0:${y_zoom[$zoom_index]}\]/set yrange \[1:\*\]/g" ../data/tmp/plot.gnu
        sed -i 's/unset autoscale y/#unset autoscale y/g' ../data/tmp/plot.gnu
    else
        sed -i "s/set yrange \[0:\*\]/set yrange \[1:\*\]/g" ../data/tmp/plot.gnu
    fi
}

# $1: type
# $2: folder
function read_vals ()
{
    while read line
    do
	if [ ! -z "$line" ]; then
            position=`expr index "$line" ,`
            let "position-=1"
            x=${line:0:$position}
            let "position+=2"
	    y=${line:$position}
            if [ $(echo "${y}>0" | bc) -eq 1 ]
            then
                if [ "${X_VAL_IS_SET[$x]}" == "TRUE" ]
                then
                    Y_SUM[$x]=$(echo "${Y_SUM[$x]}+$y" | bc)
                    Y_COUNT[$x]=$(echo "${Y_COUNT[$x]}+1" | bc)
                else
                    X_VAL_IS_SET[$x]="TRUE"
                    X_VALS[${#X_VALS[@]}]=$x
                    Y_SUM[$x]=$y
                    Y_COUNT[$x]=1
                fi
            fi
        fi
    done < "../data/${2}/tmp/current_output.${1}.txt"
}

function calc_averages ()
{
    for index in `echo ${X_VALS[@]}`
    do
        av=$(echo "scale=6; ${Y_SUM[$index]}/${Y_COUNT[$index]}" | bc)
        eval "AVERAGES_${folder}[$index]=$av"
        if [ "${GLOBAL_X_VAL_IS_SET[$index]}" != "TRUE" ]
        then
            GLOBAL_X_VAL_IS_SET[$index]="TRUE"
            GLOBAL_X_VALS[${#GLOBAL_X_VALS[@]}]=$index
        fi
        unset X_VAL_IS_SET[$index]
        unset Y_SUM[$index]
        unset Y_COUNT[$index]
    done
}

# $1: type
function compose_output ()
{
    for index in `echo "${GLOBAL_X_VALS[@]}" | tr " " "\n" | sort -n | tr "\n" " "`
    do
        if [ "$GRAPH_TYPE" == "bar" ]
        then
            echo -n '"' >> "../data/tmp/current_archive_output.${1}.txt"
        fi
        echo -n "$index" >> "../data/tmp/current_archive_output.${1}.txt"
        if [ "$GRAPH_TYPE" == "bar" ]
        then
            echo -n '"' >> "../data/tmp/current_archive_output.${1}.txt"
        fi
        for ((folder_index=0; folder_index<${#FOLDERS[@]}; folder_index++))
        do
            folder=${FOLDERS[$folder_index]}
            echo -n ', ' >> "../data/tmp/current_archive_output.${1}.txt"
            eval "val=\${AVERAGES_${folder}[$index]}"
            if [ ! -z "$val" ]
            then
                echo -n "$val" >> "../data/tmp/current_archive_output.${1}.txt"
                eval "unset AVERAGES_${folder}[$index]"
            elif [ "$GRAPH_TYPE" == "bar" ]
            then
                echo -n "0" >> "../data/tmp/current_archive_output.${1}.txt"
            fi
        done
	echo >> "../data/tmp/current_archive_output.${1}.txt"
        unset GLOBAL_X_VAL_IS_SET[$index]
    done

}

# $1: type
function compose_ratio_output ()
{
    first="TRUE"
    unset prev_index
    for index in `echo "${GLOBAL_X_VALS[@]}" | tr " " "\n" | sort -n | tr "\n" " "`
    do
        if [ "$first" == "TRUE" ]
        then
            first="FALSE"
            prev_index=$index
        else
            if [ "$GRAPH_TYPE" == "ratio_bar" ]
            then
                echo -n '"' >> "../data/tmp/current_archive_output.${1}.txt"
                echo -n "${prev_index}-${index}" >> "../data/tmp/current_archive_output.${1}.txt"
                echo -n '"' >> "../data/tmp/current_archive_output.${1}.txt"
            else
                echo -n "${prev_index}" >> "../data/tmp/current_archive_output.${1}.txt"
            fi
            for ((folder_index=0; folder_index<${#FOLDERS[@]}; folder_index++))
            do
                folder=${FOLDERS[$folder_index]}
                echo -n ', ' >> "../data/tmp/current_archive_output.${1}.txt"
                eval "prev_val=\${AVERAGES_${folder}[$prev_index]}"
                eval "val=\${AVERAGES_${folder}[$index]}"
                if [ ! -z "$val" ] && [ ! -z "$prev_val" ]
                then
                    ratio=$(echo "scale=6; $val/$prev_val" | bc)
                    echo -n "$ratio" >> "../data/tmp/current_archive_output.${1}.txt"
                    eval "unset AVERAGES_${folder}[$prev_index]"
                elif [ "$GRAPH_TYPE" == "ratio_bar" ]
                then
                    echo -n "0" >> "../data/tmp/current_archive_output.${1}.txt"
                fi
            done
	    echo >> "../data/tmp/current_archive_output.${1}.txt"
            unset GLOBAL_X_VAL_IS_SET[$prev_index]
            prev_index=$index
        fi
    done
    for ((folder_index=0; folder_index<${#FOLDERS[@]}; folder_index++))
    do
        folder=${FOLDERS[$folder_index]}
        eval "unset AVERAGES_${folder}[$prev_index]"
    done
    unset GLOBAL_X_VAL_IS_SET[$prev_index]
}

function unset_X_VALS ()
{
    len=${#X_VALS[@]}
    for ((i=0; i<len; i++))
    do
        unset X_VALS[$i]
    done
}

function unset_GLOBAL_X_VALS ()
{
    len=${#GLOBAL_X_VALS[@]}
    for ((i=0; i<len; i++))
    do
        unset GLOBAL_X_VALS[$i]
    done
}

# X_VALS: array of available x variables
# GLOBAL_X_VALS: array of available x variables
# X_VAL_IS_SET: array that is TRUE at index x_val
# GLOBAL_X_VAL_IS_SET: array that is TRUE at index x_val
# Y_SUM: sum for average
# Y_COUNT: count for average
# AVERAGES: current average of each folder

mkdir ../data/tmp
chmod 775 ../data/tmp
rm ../data/out.pdf

echo 'Start generating ...'

for ((type_index=0; type_index<${#TYPES[@]}; type_index++))
do
# Generate plot data file
    type=${TYPES[$type_index]}
    echo -n > "../data/tmp/current_archive_output.${type}.txt"
    for ((folder_index=0; folder_index<${#FOLDERS[@]}; folder_index++))
    do
        folder=${FOLDERS[$folder_index]}
	read_vals "$type" "$folder"
        calc_averages
        unset_X_VALS
    done
    if [ "$GRAPH_TYPE" == "ratio_graph" ] || [ "$GRAPH_TYPE" == "ratio_bar" ]
    then
        compose_ratio_output "$type"
    else
        compose_output "$type"
    fi
    unset_GLOBAL_X_VALS

# Plot it
    echo -n > ../data/tmp/plot.gnu
    if [ "$GRAPH_TYPE" == "bar" ] || [ "$GRAPH_TYPE" == "ratio_bar" ]
    then
        cat partial_bar_plot_data.gnu >> ../data/tmp/plot.gnu
        gen_bar_plot_cmd "$type"
    elif [ "$GRAPH_TYPE" == "graph" ] || [ "$GRAPH_TYPE" == "ratio_graph" ]
    then
        cat partial_graph_plot_data.gnu >> ../data/tmp/plot.gnu
        gen_graph_plot_cmd "$type"
    else
        echo "Fatal error! Unknown graph type!"
        exit 1
    fi
    count=0
    for x_zoom_level in `echo ${ZOOM_X_LEVELS[$type_index]}`
    do
        eval "x_zoom[$count]=$x_zoom_level"
        let "count+=1"
    done
    count=0
    for y_zoom_level in `echo ${ZOOM_Y_LEVELS[$type_index]}`
    do
        eval "y_zoom[$count]=$y_zoom_level"
        let "count+=1"
    done
    count=${#x_zoom[@]}
    for ((zoom_index=0; zoom_index<$count; zoom_index++))
    do
        print "$type_index" "$zoom_index"
        unset x_zoom[$zoom_index]
        unset y_zoom[$zoom_index]
    done
done

chmod 775 ../data/out.pdf

echo "... Finished generating graphs."
