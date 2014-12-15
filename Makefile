CGEN= cool-lex.cc

FFLAGS= -d -osrc/cool-lex.cc

FLEX=flex ${FFLAGS}

cool-lex.cc: src/cool.flex
	${FLEX} src/cool.flex
