cmake_minimum_required(VERSION 3.12)
project(PA4)

set(CMAKE_CXX_STANDARD 14)

include_directories(cool-support/include)
include_directories(src)

add_executable(PA4
        cool-support/include/ast-parse.h
        cool-support/include/cgen_gc.h
        cool-support/include/cool-io.h
        cool-support/include/cool-parse.h
        cool-support/include/cool-tree.h
        cool-support/include/cool-tree.handcode.h
        cool-support/include/cool.h
        cool-support/include/copyright.h
        cool-support/include/list.h
        cool-support/include/str_aux.h
        cool-support/include/stringtab.h
        cool-support/include/stringtab.handcode.h
        cool-support/include/stringtab_functions.h
        cool-support/include/symtab.h
        cool-support/include/tree.h
        cool-support/include/utilities.h
        cool-support/src/ast-lex.cc
        cool-support/src/ast-parse.cc
        cool-support/src/cgen-phase.cc
        cool-support/src/cool-tree.cc
        cool-support/src/dumptype.cc
        cool-support/src/handle_flags.cc
        cool-support/src/lextest.cc
        cool-support/src/parser-phase.cc
        cool-support/src/semant-phase.cc
        cool-support/src/str_aux.cc
        cool-support/src/stringtab.cc
        cool-support/src/symtab_example.cc
        cool-support/src/tokens-lex.cc
        cool-support/src/tree.cc
        cool-support/src/utilities.cc
        src/cgen.cc
        src/cgen.h
        src/cgen_supp.cc
        src/cool-tree.h
        src/emit.h)
