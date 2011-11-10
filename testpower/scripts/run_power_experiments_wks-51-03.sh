#!/bin/bash

# Usage: $ ./run_power_experiments_wks-51-03.sh binary_path binary_name input_size run_time \
#                                               testpower change_size

echo "Running on wks-51-03"

eval `cat ../data/tmp/current_ssh-agent.txt`

echo -n > ../data/tmp/current_remote_script.sh
echo "echo -n  > /mnt/ramdisk/output.txt" >> ../data/tmp/current_remote_script.sh
echo "echo \"Starting script remotely\" | tee -a /mnt/ramdisk/output.txt" >> ../data/tmp/current_remote_script.sh
echo "./${2} -input-size $3 -runtime $4 -testpower $5 -change-size $6 2>&1 | tee -a /mnt/ramdisk/output.txt" >> ../data/tmp/current_remote_script.sh

ssh root@wks-51-03.mpi-sws.org "killall power_experiments"

scp ../data/tmp/current_remote_script.sh root@wks-51-03.mpi-sws.org:~/current_remote_script.sh
scp ../../bin/${1}/${2} root@wks-51-03.mpi-sws.org:~/${2}

ssh root@wks-51-03.mpi-sws.org "chmod +x current_remote_script.sh && ./current_remote_script.sh"

scp root@wks-51-03.mpi-sws.org:/mnt/ramdisk/output.txt ../data/tmp/current_run_power_experiments_cmd.txt

ssh root@wks-51-03.mpi-sws.org "rm -f ${2} current_remote_script.sh /mnt/ramdisk/output.txt"
