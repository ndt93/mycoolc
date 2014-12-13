CGEN= cool-lex.cc

FFLAGS= -d -ocool-lex.cc

FLEX=flex ${FFLAGS}

cool-lex.cc: cool.flex
	${FLEX} cool.flex
