#!/bin/sh

DIR=$1;
OUT=$DIR/plots.pdf

function create_a_page {
    local DIR=$1
    local OUT=$2
    
    if [ -f $OUT ]; then
        rm $OUT;
    fi

    srcpdfs=`find $DIR -name "*.pdf" | sort`
    joined=$(tempfile).pdf
    pdfjoin --outfile $joined $srcpdfs || exit -1;
    #pdfnup --outfile $OUT --nup 2x3 $joined || exit -1;
    pdfnup --outfile $OUT --nup 2x2 $joined || exit -1;
    rm $joined
}

function create_document {
    local DIR=$1
    local OUT=$2
    
    plotdirs=`find $DIR -name "plots" -type d`

    tempdir=$(tempfile).temp_pdfs
    
    mkdir -p $tempdir || exit -1;

    for plotdir in $plotdirs; do
        tempfile=$(tempfile).pdf
        create_a_page $plotdir $tempfile
        mv $tempfile $tempdir
    done
    
    pdfjoin --outfile $OUT $tempdir/*.pdf || exit -1;
    
    rm -rf $tempdir
}

if [ -d $DIR ]; then

    if [ -f $OUT ]; then
        rm $OUT;
    fi
    
    create_document $DIR $OUT
else
    echo "can't find directory: $1\n";
    exit -1;
fi


