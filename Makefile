# Lexer
LIB= -lfl

CSRC= lextest.cc utilities.cc stringtab.cc handle_flags.cc
CGEN= cool-lex.cc
CFIL= ${CSRC} ${CGEN}
OBJS= ${CFIL:.cc=.o}

SRCDIR= src
CPPINCLUDE= -I. -Iinclude -I${SRCDIR}

CC= g++
CFLAGS=-g -Wall -Wno-unused -Wno-write-strings ${CPPINCLUDE}

FFLAGS= -d -o${SRCDIR}/cool-lex.cc

FLEX=flex ${FFLAGS}

cool-lex.cc: ${SRCDIR}/cool.flex
	${FLEX} ${SRCDIR}/cool.flex

.cc.o:
	${CC} ${CFLAGS} -c $<

lexer: ${OBJS:%.o=${SRCDIR}/%.o}
	${CC} ${CFLAGS} ${OBJS} ${LIB} -o lexer

# Parser
CSRC1= parser-phase.cc utilities.cc stringtab.cc dumptype.cc \
       tree.cc cool-tree.cc cool-lex.cc handle_flags.cc
CGEN1= cool-parse.cc
CFIL1 = ${CSRC1} ${CGEN1}
OBJS1 = ${CFIL1:.cc=.o}

BFLAGS = -d -v -y -b cool --debug -p cool_yy

cool-parse.cc cool-parse.h: ${SRCDIR}/cool.y
	bison ${BFLAGS} ${SRCDIR}/cool.y
	mv -f cool.tab.c ${SRCDIR}/cool-parse.cc

parser: ${OBJS1:%.o=${SRCDIR}/%.o}
	${CC} ${CFLAGS} ${OBJS1} ${LIB} -o parser

clean:
	rm *.o
