#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include "semant.h"
#include "utilities.h"

#define OBJ_CLASS_INDEX 0
#define NO_CLASS_INDEX -1
#define NUM_BASIC_CLASSES 5

#define GET_SELFTYPE(id) ((id) == SELF_TYPE ? selftype_env.lookup((id)) : NULL)
#define SET_TYPE(type, expr) \
    do { \
        set_type((type)); \
        if ((type) == SELF_TYPE) self_type_class = (expr)->self_type_class; \
    } while (false); \

extern int semant_debug;
extern char *curr_filename;

//////////////////////////////////////////////////////////////////////
//
// Symbols
//
// For convenience, a large number of symbols are predefined here.
// These symbols include the primitive type and method names, as well
// as fixed names used by the runtime system.
//
//////////////////////////////////////////////////////////////////////
static Symbol
    arg,
    arg2,
    Bool,
    concat,
    cool_abort,
    copy,
    Int,
    in_int,
    in_string,
    IO,
    length,
    Main,
    main_meth,
    No_class,
    No_type,
    Object,
    out_int,
    out_string,
    prim_slot,
    self,
    SELF_TYPE,
    Str,
    str_field,
    substr,
    type_name,
    val;
//
// Initializing the predefined symbols.
//
static void initialize_constants(void)
{
    arg         = idtable.add_string("arg");
    arg2        = idtable.add_string("arg2");
    Bool        = idtable.add_string("Bool");
    concat      = idtable.add_string("concat");
    cool_abort  = idtable.add_string("abort");
    copy        = idtable.add_string("copy");
    Int         = idtable.add_string("Int");
    in_int      = idtable.add_string("in_int");
    in_string   = idtable.add_string("in_string");
    IO          = idtable.add_string("IO");
    length      = idtable.add_string("length");
    Main        = idtable.add_string("Main");
    main_meth   = idtable.add_string("main");
    //   _no_class is a symbol that can't be the name of any
    //   user-defined class.
    No_class    = idtable.add_string("_no_class");
    No_type     = idtable.add_string("_no_type");
    Object      = idtable.add_string("Object");
    out_int     = idtable.add_string("out_int");
    out_string  = idtable.add_string("out_string");
    prim_slot   = idtable.add_string("_prim_slot");
    self        = idtable.add_string("self");
    SELF_TYPE   = idtable.add_string("SELF_TYPE");
    Str         = idtable.add_string("String");
    str_field   = idtable.add_string("_str_field");
    substr      = idtable.add_string("substr");
    type_name   = idtable.add_string("type_name");
    val         = idtable.add_string("_val");
}

static SymbolTable<Symbol, Entry> vars_env;
/* Used to lookup the class of a SELF_TYPE id */
static SymbolTable<Symbol, Entry> selftype_env;
static ClassTable* classtable;
static Class_ curr_class;
static Symbol cur_filename;

ClassTable::ClassTable(Classes classes) : semant_errors(0) , error_stream(cerr)
{
    if (classes->len() == 0) {
        semant_error() << "No class definition found\n";
        return;
    }

    /* Allocate memory for the inheritence table(graph) */
    num_classes = classes->len() + NUM_BASIC_CLASSES;
    table = new TableEntry[num_classes];

    install_basic_classes();

    for (int i=0, index=5; classes->more(i); i = classes->next(i), index++) {
        table[index].class_ = classes->nth(i);
        table[index].parent = NO_CLASS_INDEX;
    }

    if (validate_names())
        validate_inheritance();
}

ClassTable::~ClassTable() {
    delete table;
}

bool ClassTable::validate_names() {
    if (semant_debug) {
        printf("Validating class names...\n");
    }

    for (int i = 0; i < num_classes; i++) {
        if (table[i].class_->get_name() == SELF_TYPE) {
            semant_error(table[i].class_)
                << "Cannot define class with name SELF_TYPE\n";
            return false;
        }

        for (int j = i + 1; j < num_classes; j++) {
            if (table[i].class_->get_name() == table[j].class_->get_name()) {
                if (i < NUM_BASIC_CLASSES)
                    semant_error(table[i].class_)
                        << "Cannot redefine built-in classes\n";
                else
                    semant_error(table[i].class_) << "Repeated class name\n";
                return false;
            }
        }
    }

    if (semant_debug) {
        printf("Class names ok\n");
    }

    return true;
}

bool ClassTable::validate_inheritance() {
    if (semant_debug) {
        printf("Building inheritance graph...\n");
    }

    /* Build inheritance class */
    for (int i = NUM_BASIC_CLASSES; i < num_classes; i++) {
        for (int j = 0; j < num_classes; j++) {
            if (i != j &&
                table[j].class_->get_name() == table[i].class_->get_parent()) {
                table[i].parent = j;
                break;
            }
        }

        if (table[i].parent == NO_CLASS_INDEX) {
            semant_error(table[i].class_) << "Invalid parent class\n";
            return false;
        }
    }

    if (semant_debug) {
        printf("Validating inheritance graph...\n");
    }

    /* Check for cycle */
    bool* checked = new bool[num_classes]();
    bool* visited = new bool[num_classes];

    for (int i = NUM_BASIC_CLASSES; i < num_classes; i++) {
        memset(visited, 0, num_classes);

        if (!checked[i] && has_cycle(checked, visited, i)) {
            return false;
        }
    }

    if (semant_debug) {
        printf("Inheritance graph ok...\n");
    }

    delete checked;
    delete visited;

    return true;
}

bool ClassTable::has_cycle(bool* checked, bool* visited, int index) {
    if (index == -1) {
        return false;
    } else if (visited[index]) {
        semant_error(table[index].class_) <<
            "Cycle detected in inheritance graph\n";
        return true;
    }

    visited[index] = true;
    bool rval = has_cycle(checked, visited, table[index].parent);
    checked[index] = true;
    return rval;
}

int ClassTable::get_class_index(Symbol class_name)
{
    if (class_name == NULL) return NO_CLASS_INDEX;

    for (int i = 0; i < num_classes; i++) {
        if (table[i].class_->get_name() == class_name)
            return i;
    }

    return NO_CLASS_INDEX;
}

void ClassTable::install_basic_classes() {

    // The tree package uses these globals to annotate the classes built below.
   // curr_lineno  = 0;
    Symbol filename = stringtable.add_string("<basic class>");

    // The following demonstrates how to create dummy parse trees to
    // refer to basic Cool classes.  There's no need for method
    // bodies -- these are already built into the runtime system.

    // IMPORTANT: The results of the following expressions are
    // stored in local variables.  You will want to do something
    // with those variables at the end of this method to make this
    // code meaningful.

    //
    // The Object class has no parent class. Its methods are
    //        abort() : Object    aborts the program
    //        type_name() : Str   returns a string representation of class name
    //        copy() : SELF_TYPE  returns a copy of the object
    //
    // There is no need for method bodies in the basic classes---these
    // are already built in to the runtime system.

    Class_ Object_class =
	class_(Object,
	       No_class,
	       append_Features(
			       append_Features(
					       single_Features(method(cool_abort, nil_Formals(), Object, no_expr())),
					       single_Features(method(type_name, nil_Formals(), Str, no_expr()))),
			       single_Features(method(copy, nil_Formals(), SELF_TYPE, no_expr()))),
	       filename);
    table[OBJ_CLASS_INDEX].class_ = Object_class;
    table[OBJ_CLASS_INDEX].parent = NO_CLASS_INDEX;

    //
    // The IO class inherits from Object. Its methods are
    //        out_string(Str) : SELF_TYPE       writes a string to the output
    //        out_int(Int) : SELF_TYPE            "    an int    "  "     "
    //        in_string() : Str                 reads a string from the input
    //        in_int() : Int                      "   an int     "  "     "
    //
    Class_ IO_class =
	class_(IO,
	       Object,
	       append_Features(
			       append_Features(
					       append_Features(
							       single_Features(method(out_string, single_Formals(formal(arg, Str)),
										      SELF_TYPE, no_expr())),
							       single_Features(method(out_int, single_Formals(formal(arg, Int)),
										      SELF_TYPE, no_expr()))),
					       single_Features(method(in_string, nil_Formals(), Str, no_expr()))),
			       single_Features(method(in_int, nil_Formals(), Int, no_expr()))),
	       filename);
    table[1].class_ = IO_class;
    table[1].parent = OBJ_CLASS_INDEX;

    //
    // The Int class has no methods and only a single attribute, the
    // "val" for the integer.
    //
    Class_ Int_class =
	class_(Int,
	       Object,
	       single_Features(attr(val, prim_slot, no_expr())),
	       filename);
    table[2].class_ = Int_class;
    table[2].parent = OBJ_CLASS_INDEX;

    //
    // Bool also has only the "val" slot.
    //
    Class_ Bool_class =
	class_(Bool, Object, single_Features(attr(val, prim_slot, no_expr())),filename);
    table[3].class_ = Bool_class;
    table[3].parent = OBJ_CLASS_INDEX;

    //
    // The class Str has a number of slots and operations:
    //       val                                  the length of the string
    //       str_field                            the string itself
    //       length() : Int                       returns length of the string
    //       concat(arg: Str) : Str               performs string concatenation
    //       substr(arg: Int, arg2: Int): Str     substring selection
    //
    Class_ Str_class =
	class_(Str,
	       Object,
	       append_Features(
			       append_Features(
					       append_Features(
							       append_Features(
									       single_Features(attr(val, Int, no_expr())),
									       single_Features(attr(str_field, prim_slot, no_expr()))),
							       single_Features(method(length, nil_Formals(), Int, no_expr()))),
					       single_Features(method(concat,
								      single_Formals(formal(arg, Str)),
								      Str,
								      no_expr()))),
			       single_Features(method(substr,
						      append_Formals(single_Formals(formal(arg, Int)),
								     single_Formals(formal(arg2, Int))),
						      Str,
						      no_expr()))),
	       filename);
    table[4].class_ = Str_class;
    table[4].parent = OBJ_CLASS_INDEX;
}

////////////////////////////////////////////////////////////////////
//
// semant_error is an overloaded function for reporting errors
// during semantic analysis.  There are three versions:
//
//    ostream& ClassTable::semant_error()
//
//    ostream& ClassTable::semant_error(Class_ c)
//       print line number and filename for `c'
//
//    ostream& ClassTable::semant_error(Symbol filename, tree_node *t)
//       print a line number and filename
//
///////////////////////////////////////////////////////////////////

ostream& ClassTable::semant_error(Class_ c)
{
    return semant_error(c->get_filename(),c);
}

ostream& ClassTable::semant_error(Symbol filename, tree_node *t)
{
    error_stream << filename << ":" << t->get_line_number() << ": ";
    return semant_error();
}

ostream& ClassTable::semant_error()
{
    semant_errors++;
    return error_stream;
}

/*
 * Look up the feature <feature_name> in the class <class_name> and its parents
 * return a Feature containing the method data
 */
Feature ClassTable::lookup_feature(Symbol class_name, Symbol feature_name,
                                   int feature_type)
{
    int class_index = get_class_index(class_name);
    Features features;
    Feature rval;

    while (class_index != NO_CLASS_INDEX) {
        features = table[class_index].class_->get_features();

        for (int i = 0; features->more(i); i = features->next(i)) {
            rval = features->nth(i);
            if (rval->get_feature_type() == feature_type &&
                rval->get_name() == feature_name)
                return rval;
        }

        class_index = table[class_index].parent;
    }

    return NULL;
}

/*
 * Look up the method <method_name> in the class <class_name>
 * return a Feature containing the method data
 */
void ClassTable::add_attrs_to_scope(Symbol class_name)
{
    int class_index = get_class_index(class_name);
    if (class_index == NO_CLASS_INDEX) return;

    while (class_index != NO_CLASS_INDEX) {
        Features features = table[class_index].class_->get_features();
        Feature cur_feature;

        for (int i = 0; features->more(i); i = features->next(i)) {
            cur_feature = features->nth(i);
            if (cur_feature->get_feature_type() == ATTR_FEATURE) {
                vars_env.addid(cur_feature->get_name(),
                               cur_feature->get_type());
                if (cur_feature->get_type() == SELF_TYPE) {
                    selftype_env.addid(cur_feature->get_name(), class_name);
                }
            }
        }

        class_index = table[class_index].parent;
    }

    vars_env.addid(self, SELF_TYPE);
    selftype_env.addid(self, class_name);
}

/*
 * Check if the method <method_name> has a matching signature if it overrides
 * a parent class's method
 * return a true if it's valid and false otherwise
 */
bool ClassTable::validate_method(Symbol class_name, Feature method)
{
    Class_ cur_class = table[get_class_index(class_name)].class_;
    Feature p_feature = lookup_feature(cur_class->get_parent(),
                                       method->get_name(), METHOD_FEATURE);

    bool rval = true;

    if (p_feature != NULL) {
        if (p_feature->get_return_type() != method->get_return_type()) {
            semant_error(cur_class->get_filename(), method) <<
                "Return type of overriding method does not match parents'\n";
            rval = false;
        }

        Formals p_formals = p_feature->get_formals();
        Formals cur_formals = method->get_formals();

        if (p_formals->len() != cur_formals->len()) {
            semant_error(cur_class->get_filename(), method) <<
                "Formal parameters list of overriding method does not match parents'\n";
            rval = false;
        } else {
            for (int i = 0, j = 0;
                 i = cur_formals->more(i), j = p_formals->more(j);
                 i = cur_formals->next(i), j = p_formals->next(j)) {
                if (cur_formals->nth(i)->get_type() !=
                    p_formals->nth(j)->get_type()) {
                    semant_error(cur_class->get_filename(), method) <<
                        "Formal parameters list of overriding method does not match parents'\n";
                    rval = false;
                }
            }
        }
    }

    return rval;
}

/*
 * Check if the attribute <attr_name> is unique along the inheritance path
 * return a true if it's valid and false otherwise
 */
bool ClassTable::validate_attr(Symbol class_name, Feature attr)
{
    Class_ cur_class = table[get_class_index(class_name)].class_;
    Feature p_feature = lookup_feature(cur_class->get_parent(),
                                       attr->get_name(), ATTR_FEATURE);
    if (p_feature != NULL) {
        semant_error(cur_class->get_filename(), attr) <<
           "Overriding of attributes in parents' classes is illegal\n";
        return false;
    }

    return true;
}

bool ClassTable::is_subclass(Symbol subclass, Symbol superclass)
{
    if (subclass == SELF_TYPE || superclass == SELF_TYPE) {
        printf("Fatal: SELF_TYPE subclass should not be handled here!\n");
        exit(1);
    }

    if (subclass == NULL || superclass == NULL) {
        if (semant_debug) { printf("NULL class!"); }
        return false;
    }

    if (subclass == No_type) return true;

    int subclass_index = get_class_index(subclass);
    int superclass_index = get_class_index(superclass);

    while (subclass_index != NO_CLASS_INDEX) {
        if (subclass_index == superclass_index) return true;
        subclass_index = table[subclass_index].parent;
    }

    return false;
}

/*
 * Helper functions
 */

bool conform(Symbol subtype, Symbol sub_selftype,
             Symbol supertype, Symbol super_selftype)
{
    if (subtype == No_type) return true;

    if (subtype == SELF_TYPE && supertype == SELF_TYPE) {
        if (sub_selftype == NULL || super_selftype == NULL) {
            printf("Fatal: SELF_TYPE class not assigned somewhere\n");
            exit(1);
        }

        if (sub_selftype == super_selftype) {
            return true;
        } else {
            return false;
        }
    } else if (subtype != SELF_TYPE && supertype != SELF_TYPE) {
        return classtable->is_subclass(subtype, supertype);
    } else if (supertype == SELF_TYPE) {
        return false;
    } else {
        return classtable->is_subclass(sub_selftype, supertype);
    }
}

/*   This is the entry point to the semantic checker.

     Your checker should do the following two things:

     1) Check that the program is semantically correct
     2) Decorate the abstract syntax tree with type information
        by setting the `type' field in each Expression node.
        (see `tree.h')

     You are free to first do 1), make sure you catch all semantic
     errors. Part 2) can be done in a second stage, when you want
     to build mycoolc.
 */

void program_class::semant()
{
    initialize_constants();

    /* ClassTable constructor may do some semantic analysis */
    classtable = new ClassTable(classes);

    /* some semantic analysis code may go here */

    if (classtable->errors()) {
	cerr << "Compilation halted due to static semantic errors." << endl;
	exit(1);
    }

    /* Semantic analysis for each class */
    for (int i = 0; classes->more(i); i = classes->next(i)) {
        classes->nth(i)->semant();
    }
}

/*
 * Extra definitions for the Class_ phylum
 */

void class__class::semant()
{
    cur_filename = get_filename();
    curr_class = this;

    vars_env.enterscope();
    selftype_env.enterscope();

    Feature cur_feature;

    for (int i = 0; features->more(i); i = features->next(i)) {
        cur_feature = features->nth(i);

        if (cur_feature->get_feature_type() == METHOD_FEATURE) {
            classtable->validate_method(name, cur_feature);
        } else {
            classtable->validate_attr(name, cur_feature);
        }
    }

    classtable->add_attrs_to_scope(name);

    for (int i = 0; features->more(i); i = features->next(i)) {
        features->nth(i)->semant();
    }

    selftype_env.exitscope();
    vars_env.exitscope();
}

Symbol class__class::get_name() { return name; }

Symbol class__class::get_parent() { return parent; }

Features class__class::get_features() { return features; }

/*
 * Extra definitions for Feature phylum
 */

void attr_class::semant() {
    init->semant();

    if (!conform(init->get_type(), init->self_type_class,
                 type_decl, selftype_env.lookup(name))) {
        classtable->semant_error(cur_filename, this) <<
            "Initial expression of type " << init->get_type()
            << " is not a subclass of " << type_decl << "\n";
    }
}

void method_class:: semant() {
    vars_env.enterscope();
    selftype_env.enterscope();

    for (int i = 0; formals->more(i); i = formals->next(i)) {
        formals->nth(i)->semant();
    }

    expr->semant();
    Symbol T_0 = expr->get_type();

    if (!conform(T_0, expr->self_type_class,
                 return_type, curr_class->get_name())) {
        classtable->semant_error(cur_filename, this) <<
            "Infered type from method body " << T_0
            << " is not a subclass of " << return_type << "\n";
    }

    vars_env.exitscope();
    selftype_env.exitscope();
}

int method_class::get_feature_type() { return METHOD_FEATURE; }

int attr_class::get_feature_type() { return ATTR_FEATURE; }

Symbol method_class::get_name() { return name; }

Symbol attr_class::get_name() { return name; }

Formals Feature_class::get_formals() { return NULL; }

Formals method_class::get_formals() { return formals; }

Symbol Feature_class::get_return_type() { return NULL; }

Symbol method_class::get_return_type() { return return_type; }

Symbol Feature_class::get_type() { return NULL; }

Symbol attr_class::get_type() { return type_decl; }


/*
 * Extra definitions for Formal phylum
 */

Symbol formal_class::get_type()
{
    return type_decl;
}

void formal_class::semant() {
    if (type_decl == SELF_TYPE) {
        classtable->semant_error(cur_filename, this) <<
            "Declared type of formal parameter cannot be SELF_TYPE\n";
        vars_env.addid(name, No_type);
    } else {
        vars_env.addid(name, type_decl);
    }
}

/*
 * Extra definitions for the Case phylum
 */

void branch_class::semant() {

}

Expression branch_class::get_expr() {
    return expr;
}

/*
 * Extra definitions for the Expression phylum
 */

void no_expr_class::semant()
{
    set_type(No_type);
}

void assign_class::semant()
{
    expr->semant();

    Symbol expr_t = expr->get_type();
    Symbol id_type = vars_env.lookup(name);

    if (id_type != NULL) {
        if (conform(expr_t, expr->self_type_class,
                    id_type, GET_SELFTYPE(name))) {
            SET_TYPE(expr_t, expr);
        } else {
            classtable->semant_error(cur_filename, this) <<
                expr_t << " is not a subclass of " << id_type << "\n";
            set_type(No_type);
        }
    } else {
        classtable->semant_error(cur_filename, this) <<
            name << " has not been declared in this scope\n";
        set_type(No_type);
    }
}

void static_dispatch_class::semant()
{
    expr->semant();

    for (int i = 0; actual->more(i); i = actual->next(i)) {
        actual->nth(i)->semant();
    }

    if (type_name == SELF_TYPE) {
        classtable->semant_error(cur_filename, this) <<
            "Type name for static dispatch cannot be SELF_TYPE\n";
        set_type(No_type);
    }

    Symbol T0 = expr->get_type();

    if (!conform(T0, expr->self_type_class, type_name, NULL)) {
        classtable->semant_error(cur_filename, this) <<
            "Expression's type " << T0 << " is not a subclass of type name "
            << type_name << "\n";
        set_type(No_type);
        return;
    }

    Feature M = classtable->lookup_feature(type_name, name, METHOD_FEATURE);
    if (M == NULL) {
        classtable->semant_error(cur_filename, this) <<
            "Method " << name << " has not been declared in this scope\n";
        set_type(No_type);
    } else {
        Formals formals = M->get_formals();

        for (int i = 0, j = 0; actual->more(i), formals->more(j);
             i = actual->next(i), j = formals->next(j)) {
            Expression cur_expr = actual->nth(i);
            if (!conform(cur_expr->get_type(), cur_expr->self_type_class,
                         formals->nth(i)->get_type(), NULL)) {
                classtable->semant_error(cur_filename, this) <<
                    "Argument's type at " << i + 1 << " is not a subclass of "
                    << formals->nth(i)->get_type() << "\n";
                set_type(No_type);
                return;
            }
        }

        if (M->get_return_type() == SELF_TYPE) {
            SET_TYPE(T0, expr);
        } else {
            set_type(M->get_return_type());
        }
    }
}

void dispatch_class::semant()
{
    expr->semant();

    for (int i = 0; actual->more(i); i = actual->next(i)) {
        actual->nth(i)->semant();
    }

    Symbol T0 = expr->get_type();
    Symbol T_0 = T0 == SELF_TYPE ? expr->self_type_class : T0;

    Feature M = classtable->lookup_feature(T_0, name, METHOD_FEATURE);
    if (M == NULL) {
        classtable->semant_error(cur_filename, this) <<
            "Method " << name << " has not been declared in this scope\n";
        set_type(No_type);
    } else {
        Formals formals = M->get_formals();

        for (int i = 0, j = 0; actual->more(i), formals->more(j);
             i = actual->next(i), j = formals->next(j)) {
            Expression cur_expr = actual->nth(i);
            if (!conform(cur_expr->get_type(), cur_expr->self_type_class,
                         formals->nth(i)->get_type(), NULL)) {
                classtable->semant_error(cur_filename, this) <<
                    "Argument's type at " << i + 1 << " is not a subclass of "
                    << formals->nth(i)->get_type() << "\n";
                set_type(No_type);
                return;
            }
        }

        if (M->get_return_type() == SELF_TYPE) {
            SET_TYPE(T0, expr);
        } else {
            set_type(M->get_return_type());
        }
    }
}

void cond_class::semant()
{
    pred->semant();
    then_exp->semant();
    else_exp->semant();

    if (pred->get_type() != Bool) {
        classtable->semant_error(cur_filename, this) <<
            "If expression's predicate must evaluate to type Bool\n";
        set_type(No_type);
        return;
    }
}

void loop_class::semant()
{
}

void typcase_class::semant()
{
}

void block_class::semant()
{
}

void let_class::semant()
{
}

void plus_class::semant()
{
}

void sub_class::semant()
{
}

void mul_class::semant()
{
}

void divide_class::semant()
{
}

void neg_class::semant()
{
}

void lt_class::semant()
{
}

void eq_class::semant()
{
}

void leq_class::semant()
{
}

void comp_class::semant()
{
}

void int_const_class::semant()
{
    set_type(Int);
}

void bool_const_class::semant()
{
    set_type(Bool);
}

void string_const_class::semant()
{
    set_type(Str);
}

void new__class::semant()
{
    set_type(type_name);

    if (type_name == SELF_TYPE) {
        self_type_class = curr_class->get_name();
    }
}

void isvoid_class::semant()
{
}

void object_class::semant()
{
    Symbol object_t = vars_env.lookup(name);

    if (object_t == NULL) {
        classtable->semant_error(cur_filename, this) <<
            "Object " << name << " has not been declared in this scope\n";
        set_type(No_type);
    } else {
        set_type(object_t);
        if (object_t == SELF_TYPE) {
            self_type_class = selftype_env.lookup(name);
        }
    }
}
