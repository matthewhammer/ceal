#!/bin/bash

DIR=$1;

if [ -d $DIR ]; then
    for i in `find $DIR -name "*.ps"`; do 
        pushd `dirname $i`; 
        echo `pwd` : epstopdf `basename $i`; 
        epstopdf `basename $i`; 
        popd; 
    done
else
    echo "can't find directory: $1\n";
    exit -1;
fi


