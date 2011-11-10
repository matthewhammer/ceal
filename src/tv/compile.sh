#!/bin/bash

PKGS="biniou,easy-format,yojson,atdgen,unix,graphics,lablgtk2"

atdgen -json tv_signal.atd \
    || exit -1

# ocamlfind ocamlc -g -o tv_echo tv_signal.mli tv_signal.ml \
#     tv_echo.ml -package $PKGS -linkpkg \
#     || exit -1

# ocamlfind ocamlc -g -o tv_examples tv_signal.mli tv_signal.ml \
#     tv_examples.ml -package $PKGS -linkpkg \
#     || exit -1

# ocamlfind ocamlc -g -o tv_dump tv_signal.mli tv_signal.ml \
#     tv_pretty.ml tv_dump.ml -package $PKGS -linkpkg \
#     || exit -1

#ocamlfind ocamlc -g -o tv_mirror tv_signal.mli tv_signal.ml \
#    tv_pretty.ml tv_trace.ml tv_mirror.ml -package $PKGS -linkpkg \
#    || exit -1

# ocamlfind ocamlopt -o cealtv_vt100 \
#     tv_signal.mli tv_signal.ml \
#     hammer_util.ml \
#     tv_pretty.ml tv_signal_util.ml \
#     tv_trace_zipper.ml \
#     tv_mirror.ml \
#     tv_surface.ml \
#     tv_trace_surface.ml \
#     cealtv_vt100.ml \
#     -package $PKGS -linkpkg \
#         || exit -1

# ocamlfind ocamlc -o cealtv_tk -I +labltk labltk.cma \
#     tv_signal.mli tv_signal.ml \
#     hammer_util.ml \
#     tv_pretty.ml tv_signal_util.ml \
#     cealtv_tk.ml \
#     -package $PKGS -linkpkg \
#         || exit -1


ocamlfind ocamlc -g -o cealtv_gtk \
    tv_signal.mli tv_signal.ml \
    hammer_util.ml \
    tv_pretty.ml tv_signal_util.ml \
    tv_trace_zipper.ml \
    tv_mirror.ml \
    cealtv_gtk.ml \
    -package $PKGS -linkpkg \
        || exit -1

## An old attempt at compiling tv tools: 
##
## The naming of the library files for yojson and easy-format was
## causing problems: ocamlbuild assumes a particular naming convention
## that is (apparently) not always followed by the GODI-built
## libraries.
#
#IFLAGS="-I,`ocamlfind query biniou`,-I,`ocamlfind query easy-format`,-I,`ocamlfind query yojson`,-I,`ocamlfind query atdgen`"
#CFLAGS=$IFLAGS
#LFLAGS=$IFLAGS
#LIBS="biniou,easy_format,atdgen"
#ocamlbuild -cflags $CFLAGS -lflags $LFLAGS -libs $LIBS tv_echo.native || exit -1
#ocamlbuild -cflags $CFLAGS -lflags $LFLAGS -libs $LIBS tv_examples.native || exit -1

