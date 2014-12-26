/*
 *  The scanner definition for COOL.
 */

/*
 *  Stuff enclosed in %{ %} in the first section is copied verbatim to the
 *  output, so headers and global definitions are placed here to be visible
 * to the code in the file.  Don't remove anything that was here initially
 */
%{
#include <cool-parse.h>
#include <stringtab.h>
#include <utilities.h>

/* The compiler assumes these identifiers. */
#define yylval cool_yylval
#define yylex  cool_yylex

/* Max size of string constants */
#define MAX_STR_CONST 1025
#define YY_NO_UNPUT   /* keep g++ happy */

extern FILE *token_file; /* we read from this file */

/* define YY_INPUT so we read from the FILE fin:
 * This change makes it possible to use this scanner in
 * the Cool compiler.
 */
#undef YY_INPUT
#define YY_INPUT(buf,result,max_size) \
	if ( (result = fread( (char*)buf, sizeof(char), max_size, token_file)) < 0) \
		YY_FATAL_ERROR( "read() in flex scanner failed");

char string_buf[MAX_STR_CONST]; /* to assemble string constants */
char *string_buf_ptr;

extern int curr_lineno;
extern int verbose_flag;

extern YYSTYPE cool_yylval;

/*
 *  Add Your own definitions here
 */

int nested_comments = 0;
int report_long_str();

%}

%option yylineno
%s NCOMMENT LCOMMENT STRING_CONST STR_ERR

/*
 * Define names for regular expressions here.
 */

DARROW          =>
ASSIGN          <-

CLASS           [Cc][Ll][Aa][Ss][Ss]
ELSE            [Ee][Ll][Ss][Ee]
FALSE           f[Aa][Ll][Ss][Ee]
FI              [Ff][Ii]
IF              [Ii][Ff]
IN              [Ii][Nn]
INHERITS        [Ii][Nn][Hh][Ee][Rr][Ii][Tt][Ss]
ISVOID          [Ii][Ss][Vv][Oo][Ii][Dd]
LET             [Ll][Ee][Tt]
LOOP            [Ll][Oo][Oo][Pp]
POOL            [Pp][Oo][Oo][Ll]
THEN            [Tt][Hh][Ee][Nn]
WHILE           [Ww][Hh][Ii][Ll][Ee]
CASE            [Cc][Aa][Ss][Ee]
ESAC            [Ee][Ss][Aa][Cc]
NEW             [Nn][Ee][Ww]
OF              [Oo][Ff]
NOT             [Nn][Oo][Tt]
TRUE            t[Rr][Uu][Ee]

DIGIT           [0-9]
WHITESPACE      [ \n\f\r\t\v]

%%
  char string_buffer[MAX_STR_CONST];
  char *string_buf_ptr;
  int str_const_len;

<INITIAL>"--"       BEGIN(LCOMMENT); curr_lineno = yylineno;
<LCOMMENT>{
    \n          BEGIN(INITIAL); curr_lineno = yylineno;
    [^\n]*      /* Ignore everything other than new line */
    <<EOF>>     BEGIN(INITIAL); curr_lineno = yylineno; yyterminate();
}

 /*
  *  Nested comments
  */
<NCOMMENT>"(*"  nested_comments++; curr_lineno = yylineno;
<INITIAL>"(*" {
    BEGIN(NCOMMENT);
    nested_comments = 1;
    curr_lineno = yylineno;
}
<INITIAL>"*)" {
    curr_lineno = yylineno;
    cool_yylval.error_msg = (stringtable.add_string("Unmatched *)"))
                            ->get_string();
    return (ERROR);
}
<NCOMMENT>{
[^*()]*   /* Ignore anything not * and ) */
"*)"     if (--nested_comments == 0) BEGIN(INITIAL); curr_lineno = yylineno;
[*()]      /* Ignore any * and ) not forming a comment token */
<<EOF>> {
        curr_lineno = yylineno;
        BEGIN(INITIAL);
        cool_yylval.error_msg = (stringtable.add_string("EOF in comment"))->
                                 get_string();
        return (ERROR);
    }
}


 /*
  *  The multiple-character operators.
  */
<INITIAL>{
{DARROW}		{ curr_lineno = yylineno; return (DARROW); }
{ASSIGN}                { curr_lineno = yylineno; return (ASSIGN); }
\<=                     { curr_lineno = yylineno; return (LE); }
}

 /*
  * Keywords are case-insensitive except for the values true and false,
  * which must begin with a lower-case letter.
  */
<INITIAL>{
{CLASS}     curr_lineno = yylineno; return (CLASS);
{ELSE}      curr_lineno = yylineno; return (ELSE);
{FALSE} {
    curr_lineno = yylineno;
    cool_yylval.boolean = false;
    return (BOOL_CONST);
}
{FI}        curr_lineno = yylineno; return (FI);
{IF}        curr_lineno = yylineno; return (IF);
{IN}        curr_lineno = yylineno; return (IN);
{INHERITS}  curr_lineno = yylineno; return (INHERITS);
{ISVOID}    curr_lineno = yylineno; return (ISVOID);
{LET}       curr_lineno = yylineno; return (LET);
{LOOP}      curr_lineno = yylineno; return (LOOP);
{POOL}      curr_lineno = yylineno; return (POOL);
{THEN}      curr_lineno = yylineno; return (THEN);
{WHILE}     curr_lineno = yylineno; return (WHILE);
{CASE}      curr_lineno = yylineno; return (CASE);
{ESAC}      curr_lineno = yylineno; return (ESAC);
{NEW}       curr_lineno = yylineno; return (NEW);
{OF}        curr_lineno = yylineno; return (OF);
{NOT}       curr_lineno = yylineno; return (NOT);
{TRUE} {
    curr_lineno = yylineno;
    cool_yylval.boolean = true;
    return (BOOL_CONST);
}
}

 /*
  *  String constants (C syntax)
  *  Escape sequence \c is accepted for all characters c. Except for
  *  \n \t \b \f, the result is c.
  *
  */
<INITIAL>\" {
    curr_lineno = yylineno;
    string_buf_ptr = string_buffer;
    str_const_len = 0;
    BEGIN(STRING_CONST);
}
<STRING_CONST>{
    \" {
        curr_lineno = yylineno;
        *string_buf_ptr = '\0';
        cool_yylval.symbol = stringtable.add_string(string_buffer);
        BEGIN(INITIAL);
        return (STR_CONST);
    }

    \n {
        curr_lineno = yylineno;
        cool_yylval.error_msg = (stringtable.
                                 add_string("Unterminated string constant"))
                                ->get_string();
        BEGIN(INITIAL);
        return (ERROR);
    }
    (\\\0|\0) {
        curr_lineno = yylineno;
        cool_yylval.error_msg = (stringtable.
                               add_string("String contains null character"))
                               ->get_string();
        BEGIN(STR_ERR);
        return (ERROR);
    }
    <<EOF>> {
        curr_lineno = yylineno;
        BEGIN(INITIAL);
        cool_yylval.error_msg = (stringtable.
                                 add_string("EOF in string constant"))
                                ->get_string();
        return (ERROR);
    }

    \\b {
        curr_lineno = yylineno;
        *(string_buf_ptr++) = '\b';
        if (++str_const_len >= MAX_STR_CONST) return report_long_str();
    }
    \\t {
        curr_lineno = yylineno;
        *(string_buf_ptr++) = '\t';
        if (++str_const_len >= MAX_STR_CONST) return report_long_str();
    }
    \\n {
        curr_lineno = yylineno;
        *(string_buf_ptr++) = '\n';
        if (++str_const_len >= MAX_STR_CONST) return report_long_str();
    }
    \\f {
        curr_lineno = yylineno;
        *(string_buf_ptr++) = '\f';
        if (++str_const_len >= MAX_STR_CONST) return report_long_str();
    }
    \\[^\0] {
        curr_lineno = yylineno;
        *(string_buf_ptr++) = yytext[1];
        if (++str_const_len >= MAX_STR_CONST) return report_long_str();
    }

    [^\\\n\0"]+ {
        curr_lineno = yylineno;
        char* yptr = yytext;
        while (*yptr) {
            *(string_buf_ptr++) = *(yptr++);
            if (++str_const_len >= MAX_STR_CONST) {
                return report_long_str();
            }
        }
    }
}
<STR_ERR>\n     |
<STR_ERR>[^"\n]*[^\\]\n BEGIN(INITIAL);
<STR_ERR>[^"\n]*\"      BEGIN(INITIAL);/* Ignore the rest of quoted string */
<STR_ERR><<EOF>>        BEGIN(INITIAL); yyterminate();

    /* Integers constant: non-empty sequence of digits 0 to 9 */
<INITIAL>{DIGIT}+ {
    curr_lineno = yylineno;
    cool_yylval.symbol = inttable.add_string(yytext);
    return (INT_CONST);
}

    /* Identifiers consist of digits, letters and underscore
     * Type identifiers begin with a capital letter
     * Object identifiers begin with a lower case letter
     */
<INITIAL>{
[A-Z][a-zA-Z0-9_]* {
    curr_lineno = yylineno;
    cool_yylval.symbol = idtable.add_string(yytext);
    return (TYPEID);
}
[a-z][a-zA-Z0-9_]* {
    curr_lineno = yylineno;
    cool_yylval.symbol = idtable.add_string(yytext);
    return (OBJECTID);
}
}

    /* Special Syntatic Notations */
<INITIAL>[+\-*/~<=();,:{}.@] curr_lineno = yylineno; return (yytext[0]);

<INITIAL>{WHITESPACE} /* Ignore white spaces */

    /* Invalid characters */
<INITIAL>.  {
    curr_lineno = yylineno;
    cool_yylval.error_msg = (stringtable.add_string(yytext))->get_string();
    return (ERROR);
}
%%

int report_long_str() {
    cool_yylval.error_msg = (stringtable.
                             add_string("String constant too long"))
                            ->get_string();
    BEGIN(STR_ERR);
    return (ERROR);
}
