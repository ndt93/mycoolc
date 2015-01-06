#ifndef SEMANT_H_
#define SEMANT_H_

#include <assert.h>
#include <iostream>
#include "cool-tree.h"
#include "stringtab.h"
#include "symtab.h"
#include "list.h"

#define TRUE 1
#define FALSE 0

class ClassTable;
typedef ClassTable *ClassTableP;

struct TableEntry {
    Class_ class_;
    int parent;
};

// This is a structure that may be used to contain the semantic
// information such as the inheritance graph.  You may use it or not as
// you like: it is only here to provide a container for the supplied
// methods.
// ** All methods in ClassTable are not aware of the special SELF_TYPE type

class ClassTable {
private:
  int semant_errors;
  void install_basic_classes();
  ostream& error_stream;

  /* Inheritance table */
  TableEntry* table;
  int num_classes;

  bool validate_names(); // Check if all classes have different names
  bool validate_inheritance(); // Check if inheritance graph is valid
  bool has_cycle(bool*, bool*, int); // Detect cycle in inheritance graph

  int get_class_index(Symbol class_name);
public:
  ClassTable(Classes);
  ~ClassTable();
  int errors() { return semant_errors; }
  ostream& semant_error();
  ostream& semant_error(Class_ c);
  ostream& semant_error(Symbol filename, tree_node *t);

  /* Methods for querying class attritutes and methods */
  Feature lookup_feature(Symbol class_name, Symbol feature_name, int);
  bool validate_method(Symbol class_name, Feature method);
  bool validate_attr(Symbol class_name, Feature attr);

  /* Methods for type ordering query */
  bool is_subclass(Symbol subclass, Symbol superclass);
  Symbol lub(Symbol class1, Symbol class2);
};


#endif
