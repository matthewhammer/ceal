set terminal postscript eps color font "Tahoma" 16 size 40cm,15cm
set output '../data/tmp/imageOutput.eps'
set style data histograms
set style histogram clustered gap 1
set key outside top left
set boxwidth 0.8
#set logscale y
set xtic rotate by -50
set xtics nomirror scale 0
set ytics nomirror out
set mytics 4
set grid noxtics nomxtics ytics mytics
set rmargin at screen 0.95
set yrange [1:*]
#unset autoscale y
set title
set xlabel
set ylabel
