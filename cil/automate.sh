#TEST=../tests/array_max.c

APP=list_quicksort

APP_BIN_PATH=../bin/apps/$APP
APP_SRC_PATH=../src/apps/$APP
INCLUDE_PATH=../src/apps/common

make NOOPT=1 cilly && \
    \
    pushd .. && \
    make && \
    popd && \
    \
    mkdir -p $APP_BIN_PATH \
    && \
    ./bin/cilly \
    --ceal-db-passes \
    -O0 -g -pg --noPrintLn \
    ../bin/libceal.a \
    -I$INCLUDE_PATH \
    $APP_SRC_PATH/$APP.c \
    --save-temps=$APP_BIN_PATH \
    -o $APP_BIN_PATH/$APP \
    && \
    $APP_BIN_PATH/$APP -input-size 10000

#    valgrind --db-attach=yes $APP_BIN_PATH/$APP -input-size 10 -print-inout
