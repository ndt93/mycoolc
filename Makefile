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
