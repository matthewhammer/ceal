#!/bin/sh
if [ ! -f COPYRIGHT ]; then 
    echo "couldn't find COPYRIGHT"
    exit 1
fi

for f in $* ; do
    cp $f $f~
    grep 'Copyright 2008-2011' $f > /dev/null
    if [ $? = 0 ]; then
      echo $f: has copyright. Changing it.
      basename=`basename $f`
      cat COPYRIGHT | sed -e "s:FILENAME:$basename:g" > /tmp/COPYRIGHT
      sed -e '/^ *Copyright 2008-2011 *$/,/^ *along with CEAL\.  If not, see/ {
      d
  }' $f | sed -e '1 r /tmp/COPYRIGHT' > $f.sedtmp && mv $f.sedtmp $f
    else
      echo $f: no copyright.
    fi
done
