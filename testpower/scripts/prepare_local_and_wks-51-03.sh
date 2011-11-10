#!/bin/bash

echo "Preparing local"
echo "Creating data if needed"
mkdir ../data
chmod 775 ../data

echo "Creating tmp if needed"
mkdir ../data/tmp
chmod 775 ../data/tmp

echo "Cleaning tmp folder"
rm -rf ../data/tmp/*

echo "Setting up ssh-agent"
killall ssh-agent
ssh-agent > ../data/tmp/current_ssh-agent.txt
chmod 664 ../data/tmp/current_ssh-agent.txt
eval `cat ../data/tmp/current_ssh-agent.txt`
ssh-add

echo "Forcing NTP sync"
ssh root@127.0.0.1 "ntpd -q"

echo "Finished preparing local"


echo "Preparing wks-51-03"
echo "Setting up ramdisk"
ssh root@wks-51-03.mpi-sws.org "mknod -m 660 /dev/ram b 1 1 && dd if=/dev/zero of=/dev/ram bs=1K count=4K && mke2fs -m 0 /dev/ram0 4096 && mkdir /mnt/ramdisk && mount -v -t ext2 /dev/ram0 /mnt/ramdisk"

echo "Cleaning ramdisk"
ssh root@wks-51-03.mpi-sws.org "rm -rf /mnt/ramdisk/*"

echo "Setting hard disk parameters"
ssh root@wks-51-03.mpi-sws.org "hdparm -S251 /dev/sda"

governor=$1
if [ -z "$governor" ]; then
	governor=ondemand
fi
echo "Setting CPU governor $governor"
for i in {0..3}
do
	ssh root@wks-51-03.mpi-sws.org "echo $governor > /sys/devices/system/cpu/cpu$i/cpufreq/scaling_governor"
done

echo "Checking CPU frequency"
for i in {0..3}
do
	sleep 1s
	echo -n "Frequency CPU#${i}: "
        ssh root@wks-51-03.mpi-sws.org "cpufreq-info -c $i -f"
done

echo "Forcing NTP sync"
ssh root@wks-51-03.mpi-sws.org "ntpd -q"

echo "Finished preparing wks-51-03"
