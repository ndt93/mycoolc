#include <assert.h>
#include <stdio.h>
#include "emit.h"
#include "cool-tree.h"
#include "symtab.h"

enum Basicness     {Basic, NotBasic};

#define TRUE 1
#define FALSE 0

class CgenClassTable;
typedef CgenClassTable *CgenClassTableP;

class CgenNode;
typedef CgenNode *CgenNodeP;

class CgenClassTable : public SymbolTable<Symbol,CgenNode> {
private:
   List<CgenNode> *nds;
   ostream& str;
   int stringclasstag;
   int intclasstag;
   int boolclasstag;


// The following methods emit code for
// constants and global declarations.

   void code_global_data();
   void code_global_text();
   void code_bools(int);
   void code_select_gc();
   void code_constants();

// The following creates an inheritance graph from
// a list of classes.  The graph is implemented as
// a tree of `CgenNode', and class names are placed
// in the base class symbol table.

   void install_basic_classes();
   void install_class(CgenNodeP nd);
   void install_classes(Classes cs);
   void build_inheritance_tree();
   void set_relations(CgenNodeP nd);

   /* Method to generate the class_nameTab */
   void fill_classname_tab(List<CgenNode>* l);
public:
   CgenClassTable(Classes, ostream& str);
   void code();
   CgenNodeP root();

   StringEntryP empty_string;
   IntEntryP zero;
};

class CgenNode : public class__class {
private: 
   CgenNodeP parentnd;                        // Parent of class
   List<CgenNode> *children;                  // Children of class
   Basicness basic_status;                    // `Basic' if class is basic
   CgenClassTableP classtable;

protected:
   int class_tag;
   
/* Fields used to layout object attributes in memory */
   int num_attr;
   SymbolTable<Symbol, int> attr_offset;
   SymbolTable<int, Entry> offset_type; // offset to attribute type mapping

/* Fields used to layout method in dispatch table */
   struct method_name_t {
        Symbol class_name;
        Symbol method_name;
   };

   int num_methods;
   SymbolTable<Symbol, int> method_offset;
   SymbolTable<int, method_name_t>  offset_method; // offset to method name (with class)
public:
   CgenNode(Class_ c,
            int tag,
            Basicness bstatus,
            CgenClassTableP class_table);

   void add_child(CgenNodeP child);
   List<CgenNode> *get_children() { return children; }
   void set_parentnd(CgenNodeP p);
   CgenNodeP get_parentnd() { return parentnd; }
   int basic() { return (basic_status == Basic); }

   /* Methods to generate code for this class */
   void generate_proto(ostream& s); // Generate prototype object
   void generate_disptab(ostream& s); // Generate dispatch table
   void generate_init(ostream& s); // Generate init procedure
   void generate_methods(ostream& s); // Generate rest of methods
};

class BoolConst 
{
 private: 
  int val;
 public:
  BoolConst(int);
  void code_def(ostream&, int boolclasstag);
  void code_ref(ostream&) const;
};
