all : wattsup_monitor

wattsup_monitor : wattsup_monitor.c
	gcc -Wall -o $@ wattsup_monitor.c

clean :
	rm -f wattsup_monitor

install :
	cp wattsup_monitor ../../bin/testpower/wattsup_monitor
