//
// The following include files must come first.

#ifndef COOL_TREE_HANDCODE_H
#define COOL_TREE_HANDCODE_H

#include <iostream>
#include "tree.h"
#include "cool.h"
#include "stringtab.h"

#define yylineno curr_lineno;

#define ATTR_FEATURE 0
#define METHOD_FEATURE 1

extern int yylineno;

inline Boolean copy_Boolean(Boolean b) {return b; }
inline void assert_Boolean(Boolean) {}
inline void dump_Boolean(ostream& stream, int padding, Boolean b)
	{ stream << pad(padding) << (int) b << "\n"; }

void dump_Symbol(ostream& stream, int padding, Symbol b);
void assert_Symbol(Symbol b);
Symbol copy_Symbol(Symbol b);

class Program_class;
typedef Program_class *Program;
class Class__class;
typedef Class__class *Class_;
class Feature_class;
typedef Feature_class *Feature;
class Formal_class;
typedef Formal_class *Formal;
class Expression_class;
typedef Expression_class *Expression;
class Case_class;
typedef Case_class *Case;

typedef list_node<Class_> Classes_class;
typedef Classes_class *Classes;
typedef list_node<Feature> Features_class;
typedef Features_class *Features;
typedef list_node<Formal> Formals_class;
typedef Formals_class *Formals;
typedef list_node<Expression> Expressions_class;
typedef Expressions_class *Expressions;
typedef list_node<Case> Cases_class;
typedef Cases_class *Cases;

#define Program_EXTRAS                           \
virtual void dump_with_types(ostream&, int) = 0; \
virtual void semant() = 0;

#define program_EXTRAS                          \
void dump_with_types(ostream&, int);            \
void semant();

#define Class__EXTRAS                           \
virtual Symbol get_filename() = 0;              \
virtual void dump_with_types(ostream&,int) = 0; \
virtual Symbol get_name() = 0;                  \
virtual Symbol get_parent() = 0;                \
virtual Features get_features() = 0;            \
virtual void semant() = 0;


#define class__EXTRAS                               \
Symbol get_filename() { return filename; }          \
void dump_with_types(ostream&,int);                 \
Symbol get_name();                                  \
Symbol get_parent();                                \
Features get_features();                            \
void semant();


#define Feature_EXTRAS                              \
virtual void dump_with_types(ostream&,int) = 0;     \
virtual int get_feature_type() = 0;                 \
virtual Symbol get_name() = 0;                      \
virtual Formals get_formals();                      \
virtual Symbol get_return_type();                   \
virtual Symbol get_type();                          \
virtual void semant() = 0;


#define Feature_SHARED_EXTRAS                                       \
void dump_with_types(ostream&,int);


#define attr_EXTRAS           \
int get_feature_type();       \
Symbol get_name();            \
Symbol get_type();            \
void semant();


#define method_EXTRAS         \
int get_feature_type();       \
Symbol get_name();            \
Formals get_formals();        \
Symbol get_return_type();     \
void semant();


#define Formal_EXTRAS                              \
virtual void dump_with_types(ostream&,int) = 0;    \
virtual Symbol get_type() = 0;


#define formal_EXTRAS                           \
void dump_with_types(ostream&,int);             \
Symbol get_type();


#define Case_EXTRAS                             \
virtual void dump_with_types(ostream& ,int) = 0;


#define branch_EXTRAS                                   \
void dump_with_types(ostream& ,int);


#define Expression_EXTRAS                    \
Symbol type;                                 \
Symbol get_type() { return type; }           \
Expression set_type(Symbol s) { type = s; return this; } \
virtual void dump_with_types(ostream&,int) = 0;  \
void dump_type(ostream&, int);               \
Expression_class() { type = (Symbol) NULL; } \
virtual bool is_no_expr() { return false; }  \
virtual void semant() = 0;



#define Expression_SHARED_EXTRAS           \
void dump_with_types(ostream&,int);


#define no_expr_EXTRAS      \
bool is_no_expr() { return true; }  \
void semant();

/* Extra definitions for classes derived from Expression phylum */

#define assign_EXTRAS   \
void semant();

#define static_dispatch_EXTRAS  \
void semant();

#define dispatch_EXTRAS     \
void semant();

#define cond_EXTRAS     \
void semant();

#define loop_EXTRAS     \
void semant();

#define typcase_EXTRAS  \
void semant(); 

#define block_EXTRAS    \
void semant(); 

#define let_EXTRAS  \
void semant(); 

#define plus_EXTRAS     \
void semant(); 

#define sub_EXTRAS   \
void semant(); 

#define mul_EXTRAS  \
void semant(); 

#define divide_EXTRAS   \
void semant(); 

#define neg_EXTRAS  \
void semant(); 

#define lt_EXTRAS   \
void semant(); 

#define eq_EXTRAS   \
void semant(); 

#define leq_EXTRAS  \
void semant(); 

#define comp_EXTRAS     \
void semant(); 

#define int_const_EXTRAS    \
void semant();

#define bool_const_EXTRAS   \
void semant();

#define string_const_EXTRAS     \
void semant();

#define new__EXTRAS     \
void semant();

#define isvoid_EXTRAS   \
void semant();

#define object_EXTRAS   \
void semant();

#endif
