#!/bin/bash

# Use this script to record energy consumption
# It will start recording, execute the given command, wait for 10 secs,
# read the recorded values, use the threshhold to determine energy consumption before and
# during execution, calculate and return the difference between the average over three pre-
# ceding seconds and the average during execution. (Assuming consumption won't fall below
# threshold during execution.)
#
# Usage: $ measure_watts.sh measure_for_seconds break_ops command output_file
#

success=0
exp_count=0

while [ $(echo "($success==0&&$exp_count<15)" | bc) -eq 1 ]; do

	echo 'Start recording energy consumption ...'
	echo "DEBUG: wattsup count is: $1"
	../../bin/testpower/wattsup_monitor -c $1 ttyUSB0 watts 2> ../data/tmp/current_wattsup_monitor_err.txt 1> ../data/tmp/current_wattsup_monitor_recording.txt &

	echo 'Executing command ...'
	echo "DEBUG: command is: $3"
	$3

	echo -n "Wait for 2 sec to overlap measurements ..."
	sleep 1
	echo -n " 1"
        sleep 1
        echo " 2"

        echo "Killing measurements ..."
	kill -9 $!

	echo 'Calculating ...'

	ops=0
	bytes=0
	start_time=0
	finish_time=0
	run_time=0
	while read line
        do
                if [ "${line:0:11}" == "Total ops: " ]; then
                        ops=${line:11}
                elif [ "${line:0:21}" == "Array size in bytes: " ]; then
                        bytes=${line:21}
                elif [ "${line:0:12}" == "Start time: " ]; then
                        start_time=${line:12}
                elif [ "${line:0:13}" == "Finish time: " ]; then
                        finish_time=${line:13}
                elif [ "${line:0:15}" == "Run time [us]: " ]; then
                        run_time=${line:15}
                fi
        done < "../data/tmp/current_run_power_experiments_cmd.txt"

	sum_before=0
	sum_during=0
	count_before=0
	count_during=0
	start_millis=$(echo "${start_time:1:2}*3600000+${start_time:4:2}*60000+${start_time:7:2}*1000+${start_time:10:3}" | bc)
	finish_millis=$(echo "${finish_time:1:2}*3600000+${finish_time:4:2}*60000+${finish_time:7:2}*1000+${finish_time:10:3}" | bc)
	first=1
	last_measure_millis=0
	success=1
	while read line
	do
		measure_millis=$(echo "${line:1:2}*3600000+${line:4:2}*60000+${line:7:2}*1000+${line:10:3}" | bc)
		if [ $(echo "$last_measure_millis>=($start_millis-6000) && ($measure_millis-$last_measure_millis)>1500" | bc) -eq 1 ]; then
			measure_dist=$(echo "$measure_millis-$last_measure_millis" | bc)
			echo "WARNING: Two measure time points are more than 1050ms apart, actual distance is [ms]: $measure_dist"
			success=0
		fi
		last_measure_millis=$measure_millis
		if [ $(echo "$measure_millis>=($start_millis-4000) && $measure_millis<=$start_millis" | bc) -eq 1 ]; then
			sum_before=$(echo "$sum_before+${line:15:4}" | bc)
			let "count_before+=1"
		elif [ $(echo "$measure_millis>$start_millis && $measure_millis<=$finish_millis && $first==1" | bc) -eq 1 ]; then
			first=0
		elif [ $(echo "$measure_millis>$start_millis && $measure_millis<=$finish_millis" | bc) -eq 1 ]; then
                        sum_during=$(echo "$sum_during+${line:15:4}" | bc)
                        let "count_during+=1"
		fi
	done < "../data/tmp/current_wattsup_monitor_recording.txt"
	let "exp_count+=1"
done

if [ $count_before -eq 0 ]; then
        let "count_before+=1"
elif [ $count_during -eq 0 ]; then
        let "count_during+=1"
fi

average_before=$(echo "scale=6; $sum_before/$count_before" | bc)
average_during=$(echo "scale=6; $sum_during/$count_during" | bc)
average_diff=$(echo "scale=6; $average_during-$average_before" | bc)

echo "Average before consumption [Watt]: $average_before"
echo "Average during consumption [Watt]: $average_during"
echo "Difference between average before and during consumption [Watt]: $average_diff"

nanojoule_per_op=$(echo "scale=6; ($average_diff*1000*$run_time)/$ops" | bc)
average_time_per_op=$(echo "scale=6; $run_time/$ops" | bc)

echo "Average energy consumption [nanoJoule/Operation]: $nanojoule_per_op"
echo "Runtime [us]: $run_time"
echo "Operations [ops]: $ops"
echo "Average time per operation [us/Operation]: $average_time_per_op"

if [ $bytes -ge 1 ]; then
	echo "$bytes, $nanojoule_per_op" >> "$4.nano.txt"
	echo "$bytes, $ops" >> "$4.ops.txt"
	echo "$bytes, $average_before" >> "$4.av_bef.txt"
	echo "$bytes, $average_during" >> "$4.av_dur.txt"
	echo "$bytes, $average_diff" >> "$4.av_diff.txt"
	echo "$bytes, $average_time_per_op" >> "$4.av_time.txt"
else
	echo "ERROR: No output as bytes field is 0!"
fi

if [ $(echo "$ops<=$2" | bc) -eq 1 ]; then
    exit 1
fi
exit 0
