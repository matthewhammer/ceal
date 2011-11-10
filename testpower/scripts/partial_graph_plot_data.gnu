set terminal postscript eps color font "Tahoma" 16 size 40cm,15cm
set output '../data/tmp/imageOutput.eps'
set key outside top left
#set logscale x
#set logscale y
set xtic rotate by -50
set xtics nomirror out
set ytics nomirror out
set mytics 4
set grid noxtics nomxtics ytics mytics
set rmargin at screen 0.95
set xrange [1:*]
set yrange [1:*]
#unset autoscale x
#unset autoscale y
set title
set xlabel
set ylabel
