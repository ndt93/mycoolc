#include <stdio.h>
#include "cool-tree.h"

extern Program ast_root;      // root of the abstract syntax tree
FILE *token_file = stdin;       // we read the AST from standard input
extern int cool_yyparse(); // entry point to the AST parser

extern int cool_yydebug;     // not used, but needed to link with handle_flags
extern int curr_lineno;
char *curr_filename = "<stdin>";

void handle_flags(int argc, char *argv[]);

int main(int argc, char *argv[]) {
  handle_flags(argc,argv);
  cool_yyparse();
  ast_root->semant();
  ast_root->dump_with_types(cout,0);
}
