#!/bin/bash

LIB_SRC_PATH=../src/lib/
APPS_SRC_PATH=../src/apps/
INCLUDE_PATHS="-I../src/apps/common -I../src/apps/functors -I../src/apps/foreign_c/ -I ../src/lib/runtime"

CILLY_FLAGS=" -O0 -g -pg"
#CILLY_FLAGS=" -O3"
#CILLY_FLAGS=" -O3 -g -pg --noPrintLn"
#CILLY_FLAGS=" -O1 -g -pg --noPrintLn"
#CILLY_FLAGS=" -O3 -g -pg --noPrintLn"
#CILLY_FLAGS=" -O0 -g     --noPrintLn"

CEAL_FLAGS=" --ceal-hobble"
#CEAL_FLAGS=" --ceal-db-passes --ceal-hobble"
#CEAL_FLAGS=" --ceal-db-passes --ceal-hobble"
#CEAL_FLAGS=" --ceal-db-passes --ceal-hobble --ceal-db-entrances --ceal-db-dom-trees --ceal-db-cil-dom-trees"
#CEAL_FLAGS=" --ceal-db-passes --ceal-hobble  --ceal-db-entrances --ceal-db-dom-trees"

# Try to avoid this one:
# MORE_EXTRA_FLAGS="--keepunused"

# Fix the ac OS "blocks" support.
# CIL does not suppor this syntax (yet?).
FIX_MAC_BLOCKS="-U__BLOCKS__"

function compile_binary {
    local APP=$1
    local SRC_PATH=$2
    local BIN_PATH=$3
    local CEAL_FLAGS=`echo $CEAL_FLAGS`
    local CILLY_FLAGS=`echo $CILLY_FLAGS`
    local EXTRA_CILLY_FLAGS=`echo $4`
    local REGRESSION_SCRIPT=$BIN_PATH/$APP.regression-test.sh
    local REGRESSION_PHASES=$5
    local REGRESSION_OPTIONS="-phases $REGRESSION_PHASES -verf-all-updates"

    mkdir -p $BIN_PATH  || exit -1;
    
    # Create a C program that includes the runtime library.
    echo "#include \"../../../bin/lib/libceal.h\"" > $SRC_PATH/.$APP.c
    echo "#include \"$APP.c\""                    >> $SRC_PATH/.$APP.c

    ./bin/cilly $CEAL_FLAGS \
        $CILLY_FLAGS \
        $FIX_MAC_BLOCKS \
        $EXTRA_CILLY_FLAGS \
        \
        ../bin/lib/libceal.o \
        $INCLUDE_PATHS \
        $SRC_PATH/.$APP.c \
        -lm \
        --save-temps=$BIN_PATH \
        -o $BIN_PATH/$APP \
        || exit -1;    
    
    ## This is a sanity check.  Should never reach the 'else' branch.
    if [ -f $BIN_PATH/$APP ]; then
        
        echo -e "#!/bin/sh\n$BIN_PATH/$APP $REGRESSION_OPTIONS \$@" > $REGRESSION_SCRIPT
        chmod +x $REGRESSION_SCRIPT
        echo
        echo "Succesfully compiled $BIN_PATH/$APP."
        echo "  Used: $CILLY_FLAGS $EXTRA_CILLY_FLAGS"
        echo
    else
        echo Compilation Failed.
    fi
}

function compile_app_binaries {    
    local APP=$1
    local APP_SRC_PATH=$APPS_SRC_PATH/$APP
    
    if [[ -d $APP_SRC_PATH ]]
    then
    #        0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0
        SEP="- - - - - - - - - - - - - - - - - - - - -"
        echo -e $SEP $SEP $SEP $SEP
        echo -e "$0: compiling app: $APP ..."
        echo -e $SEP $SEP $SEP $SEP

        
        ## Splay-based modrefs for mutables.
        FLAGS_SPLAY=" \
                 --ceal-dest-qual=awar \
                 --ceal-implicit-qual=awar";
        
        ## Ring-based modrefs for mutables.
        FLAGS_RING=" \
                 --ceal-dest-qual=ring \
                 --ceal-implicit-qual=ring";
        
        ## Foreign immutables; Splay-based modrefs for mutables.
        FLAGS_FOREIGN_SPLAY=" \
                 -DCEAL_FOREIGN_IMMUTABLES \
                 --ceal-ignore-qual=zwzr \
                 --ceal-ignore-qual=owcr \
                 --ceal-ignore-qual=awar \
                 --ceal-dest-qual=awar \
                 --ceal-implicit-qual=awar";
        
        ## Foreign immutables; Ring-based modrefs for mutables.
        FLAGS_FOREIGN_RING=" \
                 -DCEAL_FOREIGN_IMMUTABLES \
                 --ceal-ignore-qual=zwzr \
                 --ceal-ignore-qual=owcr \
                 --ceal-ignore-qual=awar \
                 --ceal-dest-qual=ring \
                 --ceal-implicit-qual=ring";

        ## Implicit modrefs everywhere (all are Splay).
        FLAGS_IMPLICIT=" \
                 --ceal-ignore-qual=zwzr \
                 --ceal-ignore-qual=owcr \
                 --ceal-ignore-qual=awar \
                 --ceal-dest-qual=ring \
                 --ceal-implicit-qual=awar";
        
        ## Generate TV signals
        FLAGS_TV="--ceal-tv-signals"

#          compile_binary $APP $APP_SRC_PATH "../bin/apps/$APP/ring" \
#              "$FLAGS_RING" \
#              "ipvV";

#          compile_binary $APP $APP_SRC_PATH "../bin/apps/$APP/ring-tv" \
#              "$FLAGS_RING $FLAGS_TV" \
#              "ipvV";

#          compile_binary $APP $APP_SRC_PATH "../bin/apps/$APP/splay" \
#              "$FLAGS_SPLAY" \
#              "ipvV";

          compile_binary $APP $APP_SRC_PATH "../bin/apps/$APP/foreign-ring" \
              "$FLAGS_FOREIGN_RING" \
              "ipvV";

          compile_binary $APP $APP_SRC_PATH "../bin/apps/$APP/foreign-ring-tv" \
              "$FLAGS_FOREIGN_RING $FLAGS_TV" \
              "ipvV";

#          compile_binary $APP $APP_SRC_PATH "../bin/apps/$APP/foreign-splay" \
#              "$FLAGS_FOREIGN_SPLAY" \
#              "ipvV";

         compile_binary $APP $APP_SRC_PATH "../bin/apps/$APP/verf" \
             "--ceal-verifier" "i"

#          compile_binary $APP $APP_SRC_PATH "../bin/apps/$APP/foreign-splay-tv" \
#              "$FLAGS_FOREIGN_SPLAY $FLAGS_TV" \
#              "ipvV";

          # compile_binary $APP $APP_SRC_PATH "../bin/apps/$APP/foreign-ring" \
          #     "$FLAGS_FOREIGN_RING" \
          #     "ipvV";

#           compile_binary $APP $APP_SRC_PATH "../bin/apps/$APP/foreign-ring" \
#               "$FLAGS_FOREIGN_RING" \
#               "ipvV";

#           compile_binary $APP $APP_SRC_PATH "../bin/apps/$APP/foreign-ring-tv" \
#               "$FLAGS_FOREIGN_RING $FLAGS_TV" \
#               "ipvV";

#        - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
#        OOPSLA 2011 Bargraphs.

#          compile_binary $APP $APP_SRC_PATH "../bin/apps/$APP/foreign-ring" \
#              "$FLAGS_FOREIGN_RING" \
#              "ipvV";

#          compile_binary $APP $APP_SRC_PATH "../bin/apps/$APP/foreign-ring-no-seldps" \
#              "$FLAGS_FOREIGN_RING \
#              --ceal-dps-everything" \
#              "ipvV";

#          compile_binary $APP $APP_SRC_PATH "../bin/apps/$APP/foreign-ring-no-share" \
#              "$FLAGS_FOREIGN_RING \
#               --ceal-singleton-trnodes" \
#              "ipvV";

#          compile_binary $APP $APP_SRC_PATH "../bin/apps/$APP/foreign-ring-no-opt" \
#              "$FLAGS_FOREIGN_RING \
#               --ceal-singleton-trnodes --ceal-dps-everything" \
#              "ipvV";

#        - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
#        Do not edit the stuff below; just copy it and uncomment it.
#        - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
        
#         compile_binary $APP $APP_SRC_PATH "../bin/apps/$APP/verf" \
#             "--ceal-verifier" "i"

#        - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

#         compile_binary $APP $APP_SRC_PATH "../bin/apps/$APP/foreign-splay" \
#             "$FLAGS_FOREIGN_SPLAY" \
#             "ipvV";

#         compile_binary $APP $APP_SRC_PATH "../bin/apps/$APP/foreign-splay-no-seldps" \
#             "$FLAGS_FOREIGN_SPLAY \
#             --ceal-dps-everything" \
#             "ipvV";

#         compile_binary $APP $APP_SRC_PATH "../bin/apps/$APP/foreign-splay-no-share" \
#             "$FLAGS_FOREIGN_SPLAY \
#              --ceal-singleton-trnodes" \
#             "ipvV";

#        - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

#         compile_binary $APP $APP_SRC_PATH "../bin/apps/$APP/foreign-ring" \
#             "$FLAGS_FOREIGN_RING" \
#             "ipvV";

#         compile_binary $APP $APP_SRC_PATH "../bin/apps/$APP/foreign-ring-no-seldps" \
#             "$FLAGS_FOREIGN_RING \
#             --ceal-dps-everything" \
#             "ipvV";

#         compile_binary $APP $APP_SRC_PATH "../bin/apps/$APP/foreign-ring-no-share" \
#             "$FLAGS_FOREIGN_RING \
#              --ceal-singleton-trnodes" \
#             "ipvV";

#        - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

#         compile_binary $APP $APP_SRC_PATH "../bin/apps/$APP/implicit" \
#             "$FLAGS_IMPLICIT" "ipvV";

#         compile_binary $APP $APP_SRC_PATH "../bin/apps/$APP/implicit-no-seldps" \
#             "$FLAGS_IMPLICIT \
#              --ceal-dps-everything" \
#             "ipvV";
        
#         compile_binary $APP $APP_SRC_PATH "../bin/apps/$APP/implicit-no-share" \
#             "$FLAGS_IMPLICIT \
#              --ceal-singleton-trnodes" \
#             "ipvV";

#        - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
        
#        compile_binary $APP $APP_SRC_PATH "../bin/apps/$APP/implicit-no-opt" \
#            "$FLAGS_IMPLICIT \
#             --ceal-singleton-trnodes \
#             --ceal-dps-everything" \
#            "ipvV";

    else
        echo "Error: source path does not exist: $APP_SRC_PATH"
        exit -1
    fi
}

if [ "$1" != "" ] ; then
    pushd ../cil && \
    make NOOPT=1 cilly && \
    \
    pushd $LIB_SRC_PATH && \
    make && \
    popd && \
    \
    compile_app_binaries $1
else
    echo Error: required argument: application to compile
    echo Hint: try one of these:
    ls $APPS_SRC_PATH
    exit -1
fi
