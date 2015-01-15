//**************************************************************
//
// Code generator SKELETON
//
// Read the comments carefully. Make sure to
//    initialize the base class tags in
//       `CgenClassTable::CgenClassTable'
//
//    Add the label for the dispatch tables to
//       `IntEntry::code_def'
//       `StringEntry::code_def'
//       `BoolConst::code_def'
//
//    Add code to emit everyting else that is needed
//       in `CgenClassTable::code'
//
//
// The files as provided will produce code to begin the code
// segments, declare globals, and emit constants.  You must
// fill in the rest.
//
//**************************************************************

#include "cgen.h"
#include "cgen_gc.h"

#define DUMMY_TAG -1
#define OBJ_CLASS_TAG 0
#define IO_CLASS_TAG 1
#define NONBASIC_OFFSET 5

#define VOID_REF 0

/*
 * Global variables used to generate code for methods
 */
static CgenNodeP cur_class; // class in which code are being generated
// Offset of a formal parameter from FP
static SymbolTable<Symbol, int> id_offset;
// Current SP offset from FP
static int sp_offset;

extern void emit_string_constant(ostream& str, char *s);
extern int cgen_debug;

static CgenClassTableP codegen_classtable;

//
// Three symbols from the semantic analyzer (semant.cc) are used.
// If e : No_type, then no code is generated for e.
// Special code is generated for new SELF_TYPE.
// The name "self" also generates code different from other references.
//
//////////////////////////////////////////////////////////////////////
//
// Symbols
//
// For convenience, a large number of symbols are predefined here.
// These symbols include the primitive type and method names, as well
// as fixed names used by the runtime system.
//
//////////////////////////////////////////////////////////////////////
Symbol
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

static char *gc_init_names[] =
{ "_NoGC_Init", "_GenGC_Init", "_ScnGC_Init" };
static char *gc_collect_names[] =
{ "_NoGC_Collect", "_GenGC_Collect", "_ScnGC_Collect" };


//  BoolConst is a class that implements code generation for operations
//  on the two booleans, which are given global names here.
BoolConst falsebool(FALSE);
BoolConst truebool(TRUE);

//*********************************************************
//
// Define method for code generation
//
// This is the method called by the compiler driver
// `cgtest.cc'. cgen takes an `ostream' to which the assembly will be
// emmitted, and it passes this and the class list of the
// code generator tree to the constructor for `CgenClassTable'.
// That constructor performs all of the work of the code
// generator.
//
//*********************************************************

void program_class::cgen(ostream &os)
{
    // spim wants comments to start with '#'
    os << "# start of generated code\n";

    initialize_constants();
    codegen_classtable = new CgenClassTable(classes, os);

    // Object initializer
    CgenNode* obj_class = codegen_classtable->root();
    obj_class->generate_init(os);
    // The class methods
    obj_class->generate_methods(os);

    os << "\n# end of generated code\n";
}


//////////////////////////////////////////////////////////////////////////////
//
//  emit_* procedures
//
//  emit_X  writes code for operation "X" to the output stream.
//  There is an emit_X for each opcode X, as well as emit_ functions
//  for generating names according to the naming conventions (see emit.h)
//  and calls to support functions defined in the trap handler.
//
//  Register names and addresses are passed as strings.  See `emit.h'
//  for symbolic names you can use to refer to the strings.
//
//////////////////////////////////////////////////////////////////////////////

static void emit_load(char *dest_reg, int offset, char *source_reg, ostream& s)
{
    s << LW << dest_reg << " " << offset * WORD_SIZE << "(" << source_reg << ")"
        << endl;
}

static void emit_store(char *source_reg, int offset, char *dest_reg, ostream& s)
{
    s << SW << source_reg << " " << offset * WORD_SIZE << "(" << dest_reg << ")"
        << endl;
}

static void emit_load_imm(char *dest_reg, int val, ostream& s)
{ s << LI << dest_reg << " " << val << endl; }

static void emit_load_address(char *dest_reg, char *address, ostream& s)
{ s << LA << dest_reg << " " << address << endl; }

static void emit_partial_load_address(char *dest_reg, ostream& s)
{ s << LA << dest_reg << " "; }

static void emit_load_bool(char *dest, const BoolConst& b, ostream& s)
{
    emit_partial_load_address(dest,s);
    b.code_ref(s);
    s << endl;
}

static void emit_load_string(char *dest, StringEntry *str, ostream& s)
{
    emit_partial_load_address(dest,s);
    str->code_ref(s);
    s << endl;
}

static void emit_load_int(char *dest, IntEntry *i, ostream& s)
{
    emit_partial_load_address(dest,s);
    i->code_ref(s);
    s << endl;
}

static void emit_move(char *dest_reg, char *source_reg, ostream& s)
{ s << MOVE << dest_reg << " " << source_reg << endl; }

static void emit_neg(char *dest, char *src1, ostream& s)
{ s << NEG << dest << " " << src1 << endl; }

static void emit_add(char *dest, char *src1, char *src2, ostream& s)
{ s << ADD << dest << " " << src1 << " " << src2 << endl; }

static void emit_addu(char *dest, char *src1, char *src2, ostream& s)
{ s << ADDU << dest << " " << src1 << " " << src2 << endl; }

static void emit_addiu(char *dest, char *src1, int imm, ostream& s)
{ s << ADDIU << dest << " " << src1 << " " << imm << endl; }

static void emit_div(char *dest, char *src1, char *src2, ostream& s)
{ s << DIV << dest << " " << src1 << " " << src2 << endl; }

static void emit_mul(char *dest, char *src1, char *src2, ostream& s)
{ s << MUL << dest << " " << src1 << " " << src2 << endl; }

static void emit_sub(char *dest, char *src1, char *src2, ostream& s)
{ s << SUB << dest << " " << src1 << " " << src2 << endl; }

static void emit_sll(char *dest, char *src1, int num, ostream& s)
{ s << SLL << dest << " " << src1 << " " << num << endl; }

static void emit_jalr(char *dest, ostream& s)
{ s << JALR << "\t" << dest << endl; }

static void emit_jal(char *address, ostream &s)
{ s << JAL << address << endl; }

static void emit_return(ostream& s)
{ s << RET << endl; }

static void emit_gc_assign(ostream& s)
{ s << JAL << "_GenGC_Assign" << endl; }

static void emit_disptable_ref(Symbol sym, ostream& s)
{  s << sym << DISPTAB_SUFFIX; }

static void emit_init_ref(Symbol sym, ostream& s)
{ s << sym << CLASSINIT_SUFFIX; }

static void emit_label_ref(int l, ostream &s)
{ s << "label" << l; }

static void emit_protobj_ref(Symbol sym, ostream& s)
{ s << sym << PROTOBJ_SUFFIX; }

static void emit_method_ref(Symbol classname, Symbol methodname, ostream& s)
{ s << classname << METHOD_SEP << methodname; }

static void emit_label_def(int l, ostream &s)
{
    emit_label_ref(l,s);
    s << ":" << endl;
}

static void emit_beqz(char *source, int label, ostream &s)
{
    s << BEQZ << source << " ";
    emit_label_ref(label,s);
    s << endl;
}

static void emit_beq(char *src1, char *src2, int label, ostream &s)
{
    s << BEQ << src1 << " " << src2 << " ";
    emit_label_ref(label,s);
    s << endl;
}

static void emit_bne(char *src1, char *src2, int label, ostream &s)
{
    s << BNE << src1 << " " << src2 << " ";
    emit_label_ref(label,s);
    s << endl;
}

static void emit_bleq(char *src1, char *src2, int label, ostream &s)
{
    s << BLEQ << src1 << " " << src2 << " ";
    emit_label_ref(label,s);
    s << endl;
}

static void emit_blt(char *src1, char *src2, int label, ostream &s)
{
    s << BLT << src1 << " " << src2 << " ";
    emit_label_ref(label,s);
    s << endl;
}

static void emit_blti(char *src1, int imm, int label, ostream &s)
{
    s << BLT << src1 << " " << imm << " ";
    emit_label_ref(label,s);
    s << endl;
}

static void emit_bgti(char *src1, int imm, int label, ostream &s)
{
    s << BGT << src1 << " " << imm << " ";
    emit_label_ref(label,s);
    s << endl;
}

static void emit_branch(int l, ostream& s)
{
    s << BRANCH;
    emit_label_ref(l,s);
    s << endl;
}

//
// Push a register on the stack. The stack grows towards smaller addresses.
//
static void emit_push(char *reg, ostream& str)
{
    emit_store(reg, 0, SP, str);
    emit_addiu(SP, SP, -4, str);
}

//
// Push a register on the stack. The stack grows towards smaller addresses.
//
static void emit_pop(ostream& str)
{
    emit_addiu(SP, SP, 4, str);
}

//
// Fetch the integer value in an Int object.
// Emits code to fetch the integer value of the Integer object pointed
// to by register source into the register dest
//
static void emit_fetch_int(char *dest, char *source, ostream& s)
{ emit_load(dest, DEFAULT_OBJFIELDS, source, s); }

//
// Emits code to store the integer value contained in register source
// into the Integer object pointed to by dest.
//
static void emit_store_int(char *source, char *dest, ostream& s)
{ emit_store(source, DEFAULT_OBJFIELDS, dest, s); }


static void emit_test_collector(ostream &s)
{
    emit_push(ACC, s);
    emit_move(ACC, SP, s); // stack end
    emit_move(A1, ZERO, s); // allocate nothing
    s << JAL << gc_collect_names[cgen_Memmgr] << endl;
    emit_addiu(SP, SP, 4, s);
    emit_load(ACC,0,SP,s);
}

static void emit_gc_check(char *source, ostream &s)
{
    if (source != (char*) A1) emit_move(A1, source, s);
    s << JAL << "_gc_check" << endl;
}

///////////////////////////////////////////////////////////////////////////////
//
// coding strings, ints, and booleans
//
// Cool has three kinds of constants: strings, ints, and booleans.
// This section defines code generation for each type.
//
// All string constants are listed in the global "stringtable" and have
// type StringEntry.  StringEntry methods are defined both for String
// constant definitions and references.
//
// All integer constants are listed in the global "inttable" and have
// type IntEntry.  IntEntry methods are defined for Int
// constant definitions and references.
//
// Since there are only two Bool values, there is no need for a table.
// The two booleans are represented by instances of the class BoolConst,
// which defines the definition and reference methods for Bools.
//
///////////////////////////////////////////////////////////////////////////////

//
// Strings
//
void StringEntry::code_ref(ostream& s)
{
    s << STRCONST_PREFIX << index;
}

//
// Emit code for a constant String.
// You should fill in the code naming the dispatch table.
//

void StringEntry::code_def(ostream& s, int stringclasstag)
{
    IntEntryP lensym = inttable.add_int(len);

    // Add -1 eye catcher
    s << WORD << "-1" << endl;

    code_ref(s);  s  << LABEL                                             // label
        << WORD << stringclasstag << endl                                 // tag
        << WORD << (DEFAULT_OBJFIELDS + STRING_SLOTS + (len+4)/4) << endl;// size

    /* Dispatch table */
    s << WORD;
    emit_disptable_ref(Str, s);
    s << endl;

    s << WORD;  lensym->code_ref(s);  s << endl;           // string length
    emit_string_constant(s, str);                          // ascii string
    s << ALIGN;                                            // align to word
}

//
// StrTable::code_string
// Generate a string object definition for every string constant in the
// stringtable.
//
void StrTable::code_string_table(ostream& s, int stringclasstag)
{
    for (List<StringEntry> *l = tbl; l; l = l->tl())
        l->hd()->code_def(s, stringclasstag);
}

//
// Ints
//
void IntEntry::code_ref(ostream &s)
{
    s << INTCONST_PREFIX << index;
}

//
// Emit code for a constant Integer.
// You should fill in the code naming the dispatch table.
//

void IntEntry::code_def(ostream &s, int intclasstag)
{
    // Add -1 eye catcher
    s << WORD << "-1" << endl;

    code_ref(s);  s << LABEL                                // label
        << WORD << intclasstag << endl                      // class tag
        << WORD << (DEFAULT_OBJFIELDS + INT_SLOTS) << endl;  // object size

    s << WORD;
    emit_disptable_ref(Int, s);                 // dispatch table
    s << endl;

    s << WORD << str << endl;                           // integer value
}


//
// IntTable::code_string_table
// Generate an Int object definition for every Int constant in the
// inttable.
//
void IntTable::code_string_table(ostream &s, int intclasstag)
{
    for (List<IntEntry> *l = tbl; l; l = l->tl())
        l->hd()->code_def(s, intclasstag);
}


//
// Bools
//
BoolConst::BoolConst(int i) : val(i) { assert(i == 0 || i == 1); }

void BoolConst::code_ref(ostream& s) const
{
    s << BOOLCONST_PREFIX << val;
}

//
// Emit code for a constant Bool.
// You should fill in the code naming the dispatch table.
//

void BoolConst::code_def(ostream& s, int boolclasstag)
{
    // Add -1 eye catcher
    s << WORD << "-1" << endl;

    code_ref(s);  s << LABEL                                  // label
        << WORD << boolclasstag << endl                       // class tag
        << WORD << (DEFAULT_OBJFIELDS + BOOL_SLOTS) << endl;   // object size

    s << WORD;
    emit_disptable_ref(Bool, s);                // dispatch table
    s << endl;

    s << WORD << val << endl;                             // value (0 or 1)
}

//////////////////////////////////////////////////////////////////////////////
//
//  CgenClassTable methods
//
//////////////////////////////////////////////////////////////////////////////

//***************************************************
//
//  Emit code to start the .data segment and to
//  declare the global names.
//
//***************************************************

void CgenClassTable::code_global_data()
{
    Symbol main    = idtable.lookup_string(MAINNAME);
    Symbol string  = idtable.lookup_string(STRINGNAME);
    Symbol integer = idtable.lookup_string(INTNAME);
    Symbol boolc   = idtable.lookup_string(BOOLNAME);

    str << "\t.data\n" << ALIGN;
    //
    // The following global names must be defined first.
    //
    str << GLOBAL << CLASSNAMETAB << endl;
    str << GLOBAL; emit_protobj_ref(main, str);    str << endl;
    str << GLOBAL; emit_protobj_ref(integer, str); str << endl;
    str << GLOBAL; emit_protobj_ref(string, str);  str << endl;
    str << GLOBAL; falsebool.code_ref(str);  str << endl;
    str << GLOBAL; truebool.code_ref(str);   str << endl;
    str << GLOBAL << INTTAG << endl;
    str << GLOBAL << BOOLTAG << endl;
    str << GLOBAL << STRINGTAG << endl;

    //
    // We also need to know the tag of the Int, String, and Bool classes
    // during code generation.
    //
    str << INTTAG << LABEL
        << WORD << intclasstag << endl;
    str << BOOLTAG << LABEL
        << WORD << boolclasstag << endl;
    str << STRINGTAG << LABEL
        << WORD << stringclasstag << endl;
}


//***************************************************
//
//  Emit code to start the .text segment and to
//  declare the global names.
//
//***************************************************

void CgenClassTable::code_global_text()
{
    str << GLOBAL << HEAP_START << endl
        << HEAP_START << LABEL
        << WORD << 0 << endl
        << "\t.text" << endl
        << GLOBAL;
    emit_init_ref(idtable.add_string("Main"), str);
    str << endl << GLOBAL;
    emit_init_ref(idtable.add_string("Int"), str);
    str << endl << GLOBAL;
    emit_init_ref(idtable.add_string("String"), str);
    str << endl << GLOBAL;
    emit_init_ref(idtable.add_string("Bool"), str);
    str << endl << GLOBAL;
    emit_method_ref(idtable.add_string("Main"), idtable.add_string("main"), str);
    str << endl;
}

void CgenClassTable::code_bools(int boolclasstag)
{
    falsebool.code_def(str,boolclasstag);
    truebool.code_def(str,boolclasstag);
}

void CgenClassTable::code_select_gc()
{
    //
    // Generate GC choice constants (pointers to GC functions)
    //
    str << GLOBAL << "_MemMgr_INITIALIZER" << endl;
    str << "_MemMgr_INITIALIZER:" << endl;
    str << WORD << gc_init_names[cgen_Memmgr] << endl;
    str << GLOBAL << "_MemMgr_COLLECTOR" << endl;
    str << "_MemMgr_COLLECTOR:" << endl;
    str << WORD << gc_collect_names[cgen_Memmgr] << endl;
    str << GLOBAL << "_MemMgr_TEST" << endl;
    str << "_MemMgr_TEST:" << endl;
    str << WORD << (cgen_Memmgr_Test == GC_TEST) << endl;
}


//********************************************************
//
// Emit code to reserve space for and initialize all of
// the constants.  Class names should have been added to
// the string table (in the supplied code, is is done
// during the construction of the inheritance graph), and
// code for emitting string constants as a side effect adds
// the string's length to the integer table.  The constants
// are emmitted by running through the stringtable and inttable
// and producing code for each entry.
//
//********************************************************

void CgenClassTable::code_constants()
{
    //
    // Add constants that are required by the code generator.
    //
    empty_string = stringtable.add_string("");
    zero = inttable.add_string("0");

    stringtable.code_string_table(str, stringclasstag);
    inttable.code_string_table(str, intclasstag);
    code_bools(boolclasstag);
}


CgenClassTable::CgenClassTable(Classes classes, ostream& s) : nds(NULL) , str(s)
{
    stringclasstag = 4 /* Change to your String class tag here */;
    intclasstag =    2 /* Change to your Int class tag here */;
    boolclasstag =   3 /* Change to your Bool class tag here */;

    enterscope();
    if (cgen_debug) cout << "Building CgenClassTable" << endl;
    install_basic_classes();
    install_classes(classes);
    build_inheritance_tree();

    code();
    //exitscope();
}

void CgenClassTable::install_basic_classes()
{

// The tree package uses these globals to annotate the classes built below.
  //curr_lineno  = 0;
  Symbol filename = stringtable.add_string("<basic class>");

//
// A few special class names are installed in the lookup table but not
// the class list.  Thus, these classes exist, but are not part of the
// inheritance hierarchy.
// No_class serves as the parent of Object and the other special classes.
// SELF_TYPE is the self class; it cannot be redefined or inherited.
// prim_slot is a class known to the code generator.
//
  addid(No_class,
	new CgenNode(class_(No_class, No_class, nil_Features(), filename),
			    DUMMY_TAG, Basic, this));
  addid(SELF_TYPE,
	new CgenNode(class_(SELF_TYPE, No_class, nil_Features(), filename),
			    DUMMY_TAG, Basic, this));
  addid(prim_slot,
	new CgenNode(class_(prim_slot, No_class, nil_Features(), filename),
			    DUMMY_TAG, Basic, this));

//
// The Object class has no parent class. Its methods are
//        cool_abort() : Object    aborts the program
//        type_name() : Str        returns a string representation of class name
//        copy() : SELF_TYPE       returns a copy of the object
//
// There is no need for method bodies in the basic classes---these
// are already built in to the runtime system.
//
  install_class(
   new CgenNode(
    class_(Object,
	   No_class,
	   append_Features(
           append_Features(
           single_Features(method(cool_abort, nil_Formals(), Object, no_expr())),
           single_Features(method(type_name, nil_Formals(), Str, no_expr()))),
           single_Features(method(copy, nil_Formals(), SELF_TYPE, no_expr()))),
	   filename),
    OBJ_CLASS_TAG, Basic, this));

//
// The IO class inherits from Object. Its methods are
//        out_string(Str) : SELF_TYPE          writes a string to the output
//        out_int(Int) : SELF_TYPE               "    an int    "  "     "
//        in_string() : Str                    reads a string from the input
//        in_int() : Int                         "   an int     "  "     "
//
   install_class(
    new CgenNode(
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
	   filename),
    IO_CLASS_TAG, Basic, this));

//
// The Int class has no methods and only a single attribute, the
// "val" for the integer.
//
   install_class(
    new CgenNode(
     class_(Int,
	    Object,
            single_Features(attr(val, prim_slot, no_expr())),
	    filename),
     intclasstag, Basic, this));

//
// Bool also has only the "val" slot.
//
    install_class(
     new CgenNode(
      class_(Bool, Object, single_Features(attr(val, prim_slot, no_expr())),filename),
      boolclasstag, Basic, this));

//
// The class Str has a number of slots and operations:
//       val                                  ???
//       str_field                            the string itself
//       length() : Int                       length of the string
//       concat(arg: Str) : Str               string concatenation
//       substr(arg: Int, arg2: Int): Str     substring
//
   install_class(
    new CgenNode(
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
	     filename),
        stringclasstag, Basic, this));

}

// CgenClassTable::install_class
// CgenClassTable::install_classes
//
// install_classes enters a list of classes in the symbol table.
//
void CgenClassTable::install_class(CgenNodeP nd)
{
    Symbol name = nd->get_name();

    if (probe(name))
    {
        return;
    }

    // The class name is legal, so add it to the list of classes
    // and the symbol table.
    nds = new List<CgenNode>(nd, nds);
    addid(name, nd);
}

void CgenClassTable::install_classes(Classes cs)
{
    for(int i = cs->first(), tag = NONBASIC_OFFSET;
        cs->more(i);
        i = cs->next(i), tag++)
        install_class(new CgenNode(cs->nth(i), tag,
                                   NotBasic, this));
}

//
// CgenClassTable::build_inheritance_tree
//
void CgenClassTable::build_inheritance_tree()
{
    for(List<CgenNode> *l = nds; l; l = l->tl())
        set_relations(l->hd());
}

//
// CgenClassTable::set_relations
//
// Takes a CgenNode and locates its, and its parent's, inheritance nodes
// via the class table.  Parent and child pointers are added as appropriate.
//
void CgenClassTable::set_relations(CgenNodeP nd)
{
    CgenNode *parent_node = probe(nd->get_parent());
    nd->set_parentnd(parent_node);
    parent_node->add_child(nd);
}

void CgenNode::add_child(CgenNodeP n)
{
    children = new List<CgenNode>(n,children);
}

void CgenNode::set_parentnd(CgenNodeP p)
{
    assert(parentnd == NULL);
    assert(p != NULL);
    parentnd = p;
}



void CgenClassTable::code()
{
    if (cgen_debug) cout << "coding global data" << endl;
    code_global_data();

    if (cgen_debug) cout << "choosing gc" << endl;
    code_select_gc();

    if (cgen_debug) cout << "coding constants" << endl;
    code_constants();

    CgenNodeP obj_class = root();
    /* Prototype objects */
    obj_class->generate_proto(str);
    /* Class_nameTab */
    str << CLASSNAMETAB << LABEL;
    fill_classname_tab(nds);
    /* Dispatch tables */
    obj_class->generate_disptab(str);

    if (cgen_debug) cout << "coding global text" << endl;
    code_global_text();

}


CgenNodeP CgenClassTable::root()
{
    return probe(Object);
}


void CgenClassTable::fill_classname_tab(List<CgenNode>* l)
{
    if (l == NULL) return;

    fill_classname_tab(l->tl());

    str << WORD;
    stringtable.lookup_string(l->hd()->get_name()->get_string())->code_ref(str);
    str << endl;
}

///////////////////////////////////////////////////////////////////////
//
// CgenNode methods
//
///////////////////////////////////////////////////////////////////////

CgenNode::CgenNode(Class_ nd, int tag, Basicness bstatus, CgenClassTableP ct) :
    class__class((const class__class &) *nd),
    parentnd(NULL),
    children(NULL),
    basic_status(bstatus),
    class_tag(tag)
{
    // Add class name to string table
    classtable = ct;
    stringtable.add_string(name->get_string());
}

void CgenNode::generate_proto(ostream& s)
{
    if (cgen_debug) {
        cout << "Creating prototype object for " << name << endl;
    }

    if (parentnd->name != No_class) {
        attr_offset = parentnd->attr_offset;
        offset_type = parentnd->offset_type;
        num_attr = parentnd->num_attr;
    } else {
        num_attr = 0;
    }

    attr_offset.enterscope();
    offset_type.enterscope();

    Feature f;

    for (int i = 0; features->more(i); i = features->next(i)) {
        f = features->nth(i);
        if (f->get_feature_type() == ATTR_FEATURE) {
            attr_offset.addid(f->get_name(), new int(num_attr));
            offset_type.addid(num_attr, f->get_type());
            num_attr++;
        }
    }

    s << WORD << -1 << endl; // Garbage collection eye-catcher

    emit_protobj_ref(name, s); s << LABEL;
    s << WORD << class_tag << endl;

    if (name == Str) {
        s << WORD << 5 << endl;
        s << WORD; emit_disptable_ref(name, s); s << endl; // dispatch table
        s << WORD; classtable->zero->code_ref(s); s << endl;
        emit_string_constant(s, "");
        s << ALIGN;
    } else {
        s << WORD << DEFAULT_OBJFIELDS + num_attr << endl; // object size

        s << WORD;
        emit_disptable_ref(name, s); // dispatch table pointer
        s << endl;

        Symbol attr_type;

        // Default slots for all attributes
        for (int i = 0; i < num_attr; i++) {
            attr_type = offset_type.lookup(i);

            s << WORD;
            if (attr_type == Int) {
                classtable->zero->code_ref(s);
            } else if (attr_type == Str) {
                classtable->empty_string->code_ref(s);
            } else if (attr_type == Bool) {
                falsebool.code_ref(s);
            } else {
                s << VOID_REF;
            }
            if (i < num_attr - 1) s << endl;
        }
        if (num_attr > 0) s << endl;
        s << ALIGN;
    }

    /* Generate protypes of children classes */
    for (List<CgenNode>* l = children; l; l = l->tl()) {
        l->hd()->generate_proto(s);
    }
}

void CgenNode::generate_disptab(ostream& s)
{
    if (cgen_debug) {
        cout << "Creating prototype object for " << name << endl;
    }

    emit_disptable_ref(name, s); s << LABEL;

    if (parentnd->name != No_class) {
        method_offset = parentnd->method_offset;
        offset_method = parentnd->offset_method;
        num_methods = parentnd->num_methods;
    } else {
        num_methods = 0;
    }

    method_offset.enterscope();
    offset_method.enterscope();

    Feature f;
    int* offset;
    method_name_t* med_name;

    for (int i = 0; features->more(i); i = features->next(i)) {
        f = features->nth(i);
        if (f->get_feature_type() == METHOD_FEATURE) {
            offset = parentnd->method_offset.lookup(f->get_name());

            med_name = new method_name_t();
            med_name->class_name = name;
            med_name->method_name = f->get_name();

            if (offset == NULL) {
                method_offset.addid(f->get_name(), new int(num_methods));
                offset_method.addid(num_methods, med_name);
                num_methods++;
            } else {
                offset_method.addid(*offset, med_name);
            }
        }
    }

    for (int i = 0; i < num_methods; i++) {
        method_name_t* method_name = offset_method.lookup(i);

        s << WORD;
        emit_method_ref(method_name->class_name, method_name->method_name, s);
        s << endl;
    }

    for (List<CgenNode>* l = children; l; l = l->tl()) {
        l->hd()->generate_disptab(s);
    }
}

void CgenNode::generate_init(ostream& s) {
    id_offset.enterscope();

    emit_init_ref(name, s); s << LABEL;

    /* If this is a basic class then no need to do anything */
    if (basic_status == Basic) {
        /* Return to the caller, new object already in ACC */
        emit_return(s);
    } else {
        /* Complete the AR for this init call */
        emit_push(FP, s); // push the old frame pointer onto the stack
        emit_push(ACC, s); // push the self object onto the stack
        emit_move(FP, SP, s); // update the new frame pointer
        emit_push(RA, s); // push the return address onto the stack

        id_offset.addid(self, new int(1));

        /* Calling parent's init */
        if (parentnd->get_name() != No_class) {
            s << JAL; emit_init_ref(parentnd->get_name(), s); s << endl;
        }

        /* Initialize own attributes */
        Feature f;
        Expression init;
        int offset;

        for (int i = features->first(); features->more(i); i = features->next(i)) {
            f = features->nth(i);

            if (f->get_feature_type() == ATTR_FEATURE) {
                init = f->get_init();

                if (!init->is_no_expr()) {
                    init->code(s);

                    emit_load(T1, 1, FP, s);
                    offset = *(attr_offset.lookup(f->get_name())) +
                                DEFAULT_OBJFIELDS;
                    emit_store(ACC, offset, T1, s);
                }
            }
        }

        emit_load(RA, 0, FP, s); // Put the return address to $ra

        /* Put the object back to ACC and clean up AR */
        emit_load(ACC, 1, FP, s);
        // AR = ra + self + old_fp = 12
        emit_addiu(SP, SP, 12, s);

        emit_return(s);
    }

    id_offset.exitscope();

    for (List<CgenNode>* l = children; l; l = l->tl())
        l->hd()->generate_init(s);
}

/*
 * AR structure:
 * (high) |old fp| arg0 | arg1 | ... | argN-1 | self | ra | <- fp (low)
 */
void CgenNode::generate_methods(ostream& s)
{
    if (basic_status != Basic) {
        cur_class = this;

        Feature f;
        for (int i = features->first(); features->more(i); i = features->next(i)) {
            f = features->nth(i);
            if (f->get_feature_type() == METHOD_FEATURE)
                f->code(s);
        }
    }

    for (List<CgenNode>* l = children; l; l = l->tl())
        l->hd()->generate_methods(s);
}

void method_class::code(ostream& s)
{
    id_offset.enterscope();

    emit_method_ref(cur_class->get_name(), name, s);
    s << LABEL;

    /* Set up the rest of AR after the caller */
    emit_move(FP, SP, s); // Update FP to end of AR
    emit_push(RA, s); // push return address onto the stack
    sp_offset = -1; // SP is now 1 Word lower than FP

    /* Find the offset of a parameter from the FP in the AR */
    int num_formals = formals->len();

    for (int i = formals->first(), j = 0; formals->more(i);
         i = formals->next(i), j++) {
        id_offset.addid(formals->nth(i)->get_name(),
                             new int(num_formals - j));
    }

    id_offset.addid(self, new int(num_formals + 1));

    /* Generate code for method's body */
    expr->code(s); // Preserve stack and result is in ACC

    emit_load(RA, 0, FP, s); // Put the return address to $ra

    // Pop AR off the stack
    emit_addiu(SP, SP, 4 * (1 + num_formals), s);

    emit_return(s);

    id_offset.exitscope();
}

//******************************************************************
//
//   Fill in the following methods to produce code for the
//   appropriate expression.  You may add or remove parameters
//   as you wish, but if you do, remember to change the parameters
//   of the declarations in `cool-tree.h'  Sample code for
//   constant integers, strings, and booleans are provided.
//
//*****************************************************************

/*
 * Emit code that load the location of the address
 * of an object to which an id refers into <dest_reg>
 */
static void load_id_loc(char* dest_reg, Symbol id, ostream& s)
{
    int* offset;

    if ((offset = id_offset.lookup(id)) != NULL) {
        emit_addiu(dest_reg, FP, WORD_SIZE * (*offset), s);
    } else if ((offset = cur_class->attr_offset.lookup(id)) != NULL) {
        int* self_offset = id_offset.lookup(self);

        if (self_offset != NULL) {
            emit_load(dest_reg, *self_offset, FP, s);
            emit_addiu(dest_reg, dest_reg,
                       WORD_SIZE * (DEFAULT_OBJFIELDS + (*offset)), s);
        } else {
            printf("Cannot locate self object!\n");
        }
    }
}

void assign_class::code(ostream &s) {
    expr->code(s);
    load_id_loc(T1, name, s);
    emit_store(ACC, 0, T1, s);
}

void static_dispatch_class::code(ostream &s) {
}

void dispatch_class::code(ostream &s) {
    expr->code(s); // Evaluate expression, self will be in ACC

    CgenNode* disp_class = expr->get_type() == SELF_TYPE ?
        cur_class : codegen_classtable->lookup(expr->get_type());

    emit_push(FP, s); // push the old frame pointer
    emit_push(ACC, s); // push the self object

    int* offset = disp_class->method_offset.lookup(name);

    for (int i = actual->first(); actual->more(i); i = actual->next(i)) {
        actual->nth(i)->code(s);
        emit_push(ACC, s);
    }

    emit_load(ACC, actual->len() + 1, SP, s); // load self's heap addr. into ACC
    emit_load(T1, DISPTABLE_OFFSET, ACC, s); // load disptab's addr. into T1
    emit_load(T1, *offset, T1, s);
    emit_jalr(T1, s);

    emit_addiu(SP, SP, 8, s);
    emit_load(FP, 0, SP, s);
}

void cond_class::code(ostream &s) {
}

void loop_class::code(ostream &s) {
}

void typcase_class::code(ostream &s) {
}

void block_class::code(ostream &s) {
}

void let_class::code(ostream &s) {
}

void plus_class::code(ostream &s) {
}

void sub_class::code(ostream &s) {
}

void mul_class::code(ostream &s) {
}

void divide_class::code(ostream &s) {
}

void neg_class::code(ostream &s) {
}

void lt_class::code(ostream &s) {
}

void eq_class::code(ostream &s) {
}

void leq_class::code(ostream &s) {
}

void comp_class::code(ostream &s) {
}

void int_const_class::code(ostream& s)
{
    //
    // Need to be sure we have an IntEntry *, not an arbitrary Symbol
    //
    emit_load_int(ACC, inttable.lookup_string(token->get_string()), s);
}

void string_const_class::code(ostream& s)
{
    emit_load_string(ACC, stringtable.lookup_string(token->get_string()), s);
}

void bool_const_class::code(ostream& s)
{
    emit_load_bool(ACC, BoolConst(val), s);
}

void new__class::code(ostream &s) {
}

void isvoid_class::code(ostream &s) {
}

void no_expr_class::code(ostream &s) {
}

void object_class::code(ostream &s) {
    load_id_loc(T1, name, s);
    emit_load(ACC, 0, T1, s);
}


