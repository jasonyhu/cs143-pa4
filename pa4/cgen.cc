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
//    Add code to emit everything else that is needed
//       in `CgenClassTable::code'
//
//
// The files as provided will produce code to begin the code
// segments, declare globals, and emit constants.  You must
// fill in the rest.
//
//**************************************************************

#include "cgen.h"
#include "cgen_supp.h"
#include "handle_flags.h"

int label = 0;
int method_tag_counter = 0;
int let_counter = 0;
int internal_let_counter = 0;
int arg_count = 0;

//
// Two symbols from the semantic analyzer (semant.cc) are used.
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
  ::copy      = idtable.add_string("copy");
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

static const char *gc_init_names[] =
  { "_NoGC_Init", "_GenGC_Init", "_ScnGC_Init" };
static const char *gc_collect_names[] =
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
//
// Note that Spim wants comments to start with '#'. For example:
// os << "# start of generated code\n";
// os << "\n# end of generated code\n";
//
//*********************************************************
void program_class::cgen(ostream &os) {
   initialize_constants();
   CgenClassTable *codegen_classtable = new CgenClassTable(classes,os);
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

// Added for consistent label support.
static std::string get_label_ref(int l)
{ std::stringstream ss;
  ss << l;
  std::string lbl = "label" + ss.str();
  return lbl;
}

static void emit_load(const char *dest_reg, int offset, const char *source_reg, ostream& s)
{
  s << LW << dest_reg << " " << offset * WORD_SIZE << "(" << source_reg << ")"
    << std::endl;
}

static void emit_store(const char *source_reg, int offset, const char *dest_reg, ostream& s)
{
  s << SW << source_reg << " " << offset * WORD_SIZE << "(" << dest_reg << ")"
      << std::endl;
}

static void emit_load_imm(const char *dest_reg, int val, ostream& s)
{ s << LI << dest_reg << " " << val << std::endl; }

static void emit_load_address(const char *dest_reg, const char *address, ostream& s)
{ s << LA << dest_reg << " " << address << std::endl; }

static void emit_partial_load_address(const char *dest_reg, ostream& s)
{ s << LA << dest_reg << " "; }

static void emit_load_bool(const char *dest, const BoolConst& b, ostream& s)
{
  emit_partial_load_address(dest,s);
  b.code_ref(s);
  s << std::endl;
}

static void emit_load_string(const char *dest, StringEntry *str, ostream& s)
{
  emit_partial_load_address(dest,s);
  str->code_ref(s);
  s << std::endl;
}

static void emit_load_int(const char *dest, IntEntry *i, ostream& s)
{
  emit_partial_load_address(dest,s);
  i->code_ref(s);
  s << std::endl;
}

static void emit_move(const char *dest_reg, const char *source_reg, ostream& s)
{
  s << MOVE << dest_reg << " " << source_reg << std::endl;
}

static void emit_neg(const char *dest, const char *src1, ostream& s)
{ s << NEG << dest << " " << src1 << std::endl; }

static void emit_add(const char *dest, const char *src1, const char *src2, ostream& s)
{ s << ADD << dest << " " << src1 << " " << src2 << std::endl; }

static void emit_addu(const char *dest, const char *src1, const char *src2, ostream& s)
{ s << ADDU << dest << " " << src1 << " " << src2 << std::endl; }

static void emit_addiu(const char *dest, const char *src1, int imm, ostream& s)
{ s << ADDIU << dest << " " << src1 << " " << imm << std::endl; }

static void emit_div(const char *dest, const char *src1, const char *src2, ostream& s)
{ s << DIV << dest << " " << src1 << " " << src2 << std::endl; }

static void emit_mul(const char *dest, const char *src1, const char *src2, ostream& s)
{ s << MUL << dest << " " << src1 << " " << src2 << std::endl; }

static void emit_sub(const char *dest, const char *src1, const char *src2, ostream& s)
{ s << SUB << dest << " " << src1 << " " << src2 << std::endl; }

static void emit_sll(const char *dest, const char *src1, int num, ostream& s)
{ s << SLL << dest << " " << src1 << " " << num << std::endl; }

static void emit_jalr(const char *dest, ostream& s)
{ s << JALR << "\t" << dest << std::endl; }

static void emit_jal(char *address,ostream &s)
{ s << JAL << address << endl; }

static void emit_return(ostream& s)
{ s << RET << std::endl; }

static void emit_gc_assign(ostream& s)
{ s << JAL << "_GenGC_Assign" << std::endl; }

static void emit_disptable_ref(Symbol sym, ostream& s)
{  s << sym << DISPTAB_SUFFIX; }

static void emit_init_ref(Symbol sym, ostream& s)
{ s << sym << CLASSINIT_SUFFIX; }

static void emit_label_ref(int l, ostream &s)
{ s << get_label_ref(l); }

static void emit_protobj_ref(Symbol sym, ostream& s)
{ s << sym << PROTOBJ_SUFFIX; }

static void emit_method_ref(Symbol classname, Symbol methodname, ostream& s)
{ s << classname << METHOD_SEP << methodname; }

static void emit_label_def(int l, ostream &s)
{
  emit_label_ref(l,s);
  s << ":" << std::endl;
}

static void emit_beqz(const char *source, int label, ostream &s)
{
  s << BEQZ << source << " ";
  emit_label_ref(label,s);
  s << std::endl;
}

static void emit_beq(const char *src1, const char *src2, int label, ostream &s)
{
  s << BEQ << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << std::endl;
}

static void emit_bne(const char *src1, const char *src2, int label, ostream &s)
{
  s << BNE << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << std::endl;
}

static void emit_bleq(const char *src1, const char *src2, int label, ostream &s)
{
  s << BLEQ << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << std::endl;
}

static void emit_blt(const char *src1, const char *src2, int label, ostream &s)
{
  s << BLT << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << std::endl;
}

static void emit_blti(const char *src1, int imm, int label, ostream &s)
{
  s << BLT << src1 << " " << imm << " ";
  emit_label_ref(label,s);
  s << std::endl;
}

static void emit_bgti(const char *src1, int imm, int label, ostream &s)
{
  s << BGT << src1 << " " << imm << " ";
  emit_label_ref(label,s);
  s << std::endl;
}

static void emit_branch(int l, ostream& s)
{
  s << BRANCH;
  emit_label_ref(l,s);
  s << std::endl;
}

//
// Push a register on the stack. The stack grows towards smaller addresses.
//
static void emit_push(const char *reg, ostream& str)
{
  emit_store(reg,0,SP,str);
  emit_addiu(SP,SP,-4,str);
}

//
// Fetch the integer value in an Int object. Emits code to fetch the integer
// value of the Integer object pointed to by register source into the register dest
//
static void emit_fetch_int(const char *dest, const char *source, ostream& s)
{ emit_load(dest, DEFAULT_OBJFIELDS, source, s); }

//
// Emits code to store the integer value contained in register source
// into the Integer object pointed to by dest.
//
static void emit_store_int(const char *source, const char *dest, ostream& s)
{ emit_store(source, DEFAULT_OBJFIELDS, dest, s); }

static void emit_test_collector(ostream &s)
{
  emit_push(ACC, s);
  emit_move(ACC, SP, s); // stack end
  emit_move(A1, ZERO, s); // allocate nothing
  s << JAL << gc_collect_names[cgen_Memmgr] << endl;
  emit_addiu(SP,SP,4,s);
  emit_load(ACC,0,SP,s);
}

static void emit_gc_check(const char *source, ostream &s)
{
  if (strcmp(source, A1)) emit_move(A1, source, s);
  s << JAL << "_gc_check" << std::endl;
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
  s << WORD << "-1" << std::endl;

  code_ref(s);
  s  << LABEL                                                               // label
     << WORD << stringclasstag << std::endl                                 // tag
     << WORD << (DEFAULT_OBJFIELDS + STRING_SLOTS + (len+4)/4) << std::endl // size
     << WORD;

  /***** Add dispatch information for class String ******/
  s << Str << DISPTAB_SUFFIX << std::endl;                                              // dispatch table
  s << WORD;  lensym->code_ref(s);  s << std::endl;            // string length
  emit_string_constant(s,str);                                // ascii string
  s << ALIGN;                                                 // align to word
}

//
// StrTable::code_string
// Generate a string object definition for every string constant in the
// stringtable.
//
void StrTable::code_string_table(ostream& s, int stringclasstag) {
  for (auto entry : tbl) {
    entry.code_def(s, stringclasstag);
  }
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
  s << WORD << "-1" << std::endl;

  code_ref(s);
  s << LABEL                                // label
    << WORD << intclasstag << std::endl                      // class tag
    << WORD << (DEFAULT_OBJFIELDS + INT_SLOTS) << std::endl  // object size
    << WORD;

  /***** Add dispatch information for class Int ******/

  s << Int << DISPTAB_SUFFIX << std::endl;                                          // dispatch table
  s << WORD << str << std::endl;                           // integer value
}

//
// IntTable::code_string_table
// Generate an Int object definition for every Int constant in the
// inttable.
//
void IntTable::code_string_table(ostream &s, int intclasstag) {
  for (auto entry : tbl) {
    entry.code_def(s,intclasstag);
  }
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
  s << WORD << "-1" << std::endl;

  code_ref(s);
  s << LABEL                                  // label
    << WORD << boolclasstag << std::endl                       // class tag
    << WORD << (DEFAULT_OBJFIELDS + BOOL_SLOTS) << std::endl   // object size
    << WORD;

  /***** Add dispatch information for class Bool ******/

  s << Bool << DISPTAB_SUFFIX << std::endl;                                            // dispatch table
  s << WORD << val << std::endl;                             // value (0 or 1)
}

//////////////////////////////////////////////////////////////////////////////
//
//  CgenClassTable methods
//
//////////////////////////////////////////////////////////////////////////////

//
// Define global names for some of basic classes and their tags.
//
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
  str << GLOBAL << CLASSNAMETAB << std::endl;
  str << GLOBAL; emit_protobj_ref(main,str);    str << std::endl;
  str << GLOBAL; emit_protobj_ref(integer,str); str << std::endl;
  str << GLOBAL; emit_protobj_ref(string,str);  str << std::endl;
  str << GLOBAL; falsebool.code_ref(str);  str << std::endl;
  str << GLOBAL; truebool.code_ref(str);   str << std::endl;
  str << GLOBAL << INTTAG << std::endl;
  str << GLOBAL << BOOLTAG << std::endl;
  str << GLOBAL << STRINGTAG << std::endl;
  str << GLOBAL << CLASSOBJTAB << std::endl;
    str << GLOBAL << CLASSPARENTTAB << std::endl;
  for (CgenNodeP nd : nds) {
    str << GLOBAL << nd->get_name() << PROTOBJ_SUFFIX << std::endl;
    str << GLOBAL << nd->get_name() << "_init" << std::endl;
  }

  //
  // We also need to know the tag of the Int, String, and Bool classes
  // during code generation.
  //

  int stringclasstag = *class_to_tag_table.lookup(string);
  int intclasstag = *class_to_tag_table.lookup(integer);
  int boolclasstag = *class_to_tag_table.lookup(boolc);

  str << INTTAG << LABEL
      << WORD << intclasstag << std::endl;
  str << BOOLTAG << LABEL
      << WORD << boolclasstag << std::endl;
  str << STRINGTAG << LABEL
      << WORD <<  stringclasstag
      << std::endl;
}

//***************************************************
//
//  Emit code to start the .text segment and to
//  declare the global names.
//
//***************************************************

void CgenClassTable::code_global_text()
{
  str << GLOBAL << HEAP_START << std::endl
      << HEAP_START << LABEL
      << WORD << 0 << std::endl
      << "\t.text" << std::endl
      << GLOBAL;
  emit_init_ref(idtable.add_string("Main"), str);
  str << std::endl << GLOBAL;
  emit_init_ref(idtable.add_string("Int"),str);
  str << std::endl << GLOBAL;
  emit_init_ref(idtable.add_string("String"),str);
  str << std::endl << GLOBAL;
  emit_init_ref(idtable.add_string("Bool"),str);
  str << std::endl << GLOBAL;
  emit_method_ref(idtable.add_string("Main"), idtable.add_string("main"), str);
  str << std::endl;
}

void CgenClassTable::code_bools()
{
  int boolclasstag = *class_to_tag_table.lookup(idtable.add_string(BOOLNAME));
  falsebool.code_def(str,boolclasstag);
  truebool.code_def(str,boolclasstag);
}

//
// Generate GC choice constants (pointers to GC functions)
//
void CgenClassTable::code_select_gc()
{
  str << GLOBAL << "_MemMgr_INITIALIZER" << std::endl;
  str << "_MemMgr_INITIALIZER:" << std::endl;
  str << WORD << gc_init_names[cgen_Memmgr] << std::endl;
  str << GLOBAL << "_MemMgr_COLLECTOR" << std::endl;
  str << "_MemMgr_COLLECTOR:" << std::endl;
  str << WORD << gc_collect_names[cgen_Memmgr] << std::endl;
  str << GLOBAL << "_MemMgr_TEST" << std::endl;
  str << "_MemMgr_TEST:" << std::endl;
  str << WORD << (cgen_Memmgr_Test == GC_TEST) << std::endl;
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
//********************************************************
void CgenClassTable::code_constants()
{
  //
  // Add constants that are required by the code generator.
  //
  stringtable.add_string("");
  inttable.add_string("0");

  int stringclasstag = *class_to_tag_table.lookup(idtable.lookup_string(STRINGNAME));
  int intclasstag = *class_to_tag_table.lookup(idtable.lookup_string(INTNAME));

  stringtable.code_string_table(str,stringclasstag);
  inttable.code_string_table(str,intclasstag);
  code_bools();
}

void CgenClassTable::code_class_name_table() {
  str << CLASSNAMETAB << LABEL;
  nds.reverse();
  for (CgenNodeP nd : nds) {
    Symbol class_ = nd->get_name();
    StringEntryP str_entry = stringtable.lookup_string(class_->get_string());
    str << WORD;
    str_entry->code_ref(str);
    str << std::endl;
  }
}

void CgenClassTable::code_class_obj_table() {
  str << CLASSOBJTAB << LABEL;
  for (CgenNodeP nd : nds) {
    Symbol class_ = nd->get_name();
    str << WORD << class_->get_string() << PROTOBJ_SUFFIX << std::endl;
    str << WORD << class_->get_string() << "_init" << std::endl;
  }
}

void CgenClassTable::code_class_parent_table() {
  str << CLASSPARENTTAB << LABEL;
  for (CgenNodeP nd : nds) {
    if (nd->get_name() == Object) {
      str << WORD << NOPARENTTAG << std::endl;
    } else {
      str << WORD << *class_to_tag_table.lookup(nd->get_parent()) << std::endl;
    }
  }
}

std::map<Symbol, Symbol> CgenNode::get_all_methods() {
  if (all_methods.empty()) {
    std::list<CgenNode*> parents;
    CgenNode* cn = this;
    while (cn->name != No_class) {
      parents.push_back(cn);
      cn = cn->get_parentnd();
    }
    parents.reverse();
    for (CgenNode* nd : parents) {
      Features parent_features = nd->features;
      for (int i = parent_features->first(); parent_features->more(i); i = parent_features->next(i)) {
        Feature f = parent_features->nth(i);
        if (f->is_method()) {
          all_methods[((method_class*)f)->get_name()] = nd->get_name();
        }
      }
    }
    int i = 0;
    for (auto it = all_methods.begin(); it != all_methods.end(); it++) {
      // method_ids[it->first] = i;
      i++;
    }
  }
  return all_methods;
}

void CgenNode::disp_traversal(Symbol dispTabClass, ostream& str, std::map<Symbol, std::map<Symbol, int>>& method_ids) {
  if (get_parentnd() != NULL) {
    get_parentnd()->disp_traversal(dispTabClass, str, method_ids);
  }
  for (int i = features->first(); features->more(i); i = features->next(i)) {
    if (features->nth(i)->is_method()) {
      str << WORD << name->get_string() << "." << features->nth(i)->get_name()->get_string() << std::endl;
      method_ids[dispTabClass][features->nth(i)->get_name()] = method_tag_counter;
      method_tag_counter++;
    }
  }
}


std::map<Symbol, std::map<Symbol, int>> CgenClassTable::code_disp_tables() {
  std::map<Symbol, std::map<Symbol, int>> method_ids;
  for (Symbol class_ : classes) {
    emit_disptable_ref(class_, str);
    str << LABEL;
    lookup(class_)->disp_traversal(class_, str, method_ids);
    method_tag_counter = 0;
    for (int i = lookup(class_)->get_features()->first(); lookup(class_)->get_features()->more(i); i = lookup(class_)->get_features()->next(i)) {
      if (lookup(class_)->get_features()->nth(i)->is_method()) {
        // let_traverse through the method and get the let_counter
        lookup(class_)->get_features()->nth(i)->get_expr()->let_traverse();
        method_let_vars_table[class_][lookup(class_)->get_features()->nth(i)->get_name()] = let_counter;
        let_counter = 0;
      }
    }
  }
  return method_ids;
}

std::vector<attr_class*> CgenNode::get_all_attrs() {
  if (all_attrs.empty()) {
    std::vector<CgenNode*> parents;
    CgenNode* cn = this;
    while (cn->name != No_class) {
      parents.push_back(cn);
      cn = cn->get_parentnd();
    }
    std::reverse(parents.begin(), parents.end());
    for (CgenNode* parent : parents) {
      Features parent_features = parent->features;
      for (int i = parent_features->first(); parent_features->more(i); i = parent_features->next(i)) {
        // todo: make a separate pass to gather the let variables for later AR allocation
        Feature f = parent_features->nth(i);
        if (!f->is_method()) {
          all_attrs.push_back((attr_class*)f);
        }
      }
    }
    for (size_t i = 0; i < all_attrs.size(); i++) {
      attr_ids[all_attrs[i]->get_name()] = i;
    }
  }
  return all_attrs;
}

void CgenClassTable::code_prot_objs() {
  for (CgenNodeP nd : nds) {
    std::vector<attr_class*> attribs = nd->get_all_attrs();
    str << WORD << -1 << std::endl;   // garbage collector tag
    Symbol class_ = nd->get_name();
    emit_protobj_ref(class_, str);
    str << LABEL;
    int classTag = *class_to_tag_table.lookup(class_);
    str << WORD << classTag << std::endl;
    // object size
    str << WORD << (DEFAULT_OBJFIELDS + attribs.size()) << std::endl;
    // dispatch ptr
    str << WORD << class_ << DISPTAB_SUFFIX << std::endl;
    for (attr_class* attrib : attribs) {
      if (attrib->get_name() == val) {
        if (nd->get_name() == Str) {
          str << WORD;
          inttable.lookup_string("0")->code_ref(str);
          str << std::endl;
        } else {
          str << WORD << "0" << endl;
        }
      } else if (attrib->get_name() == str_field) {
        str << WORD << "0" << endl;
      } else {
        Symbol type = attrib->get_type();
        if (type == Int) {
          str << WORD;
          inttable.lookup_string("0")->code_ref(str);
          str << endl;
        } else if (type == Str) {
          str << WORD;
          stringtable.lookup_string("")->code_ref(str);
          str << endl;
        } else if (type == Bool) {
          str << WORD;
          falsebool.code_ref(str);
          str << endl;
        } else {
          str << WORD << "0" << endl;
        }
      }
    }
  }
}

Environment CgenClassTable::code_inits() {
  Environment env(nds);
  env.enterscope();
  for (CgenNode* nd : nds) {
    env.class_tags[nd->get_name()] = *(class_to_tag_table.lookup(nd->get_name()));
    str << nd->get_name() << CLASSINIT_SUFFIX << LABEL;
    emit_addiu(SP, SP, -12, str);
    emit_store(FP, 3, SP, str);
    emit_store(SELF, 2, SP, str);
    emit_store(RA, 1, SP, str);
    // ALEX: chanaged this to 16 to conform to reference
    emit_addiu(FP, SP, 16, str);
    emit_move(SELF, ACC, str);
    Symbol parent = nd->get_parent();
    env.so = nd;
    if (parent != No_class) {
      str << JAL;
      emit_init_ref(parent, str);
      str << std::endl;
    }
    std::vector<attr_class*> attribs = nd->get_all_attrs();
    
    for (attr_class* attrib : attribs) {
      int id = nd->get_attr_ids().at(attrib->get_name());
      // ALEX: modified, see github for more info
      std::pair<std::string, int>* value = new std::pair<std::string, int>("attr", id);
      env.addid(attrib->get_name(), value);
      attrib->get_init()->code(str, &env);
      if (attrib->get_type() == Str) {
        emit_store(ACC, id + 3, SELF, str);
      } else if (attrib->get_type() == Int) {
        emit_store(ACC, id + 3, SELF, str);
      } else if (attrib->get_type() == Bool) {
        emit_store(ACC, id + 3, SELF, str);
      }
    }
    emit_move(ACC, SELF, str);
    emit_load(FP, 3, SP, str);
    emit_load(SELF, 2, SP, str);
    emit_load(RA, 1, SP, str);
    emit_addiu(SP, SP, 12, str);
    emit_return(str);
  }
  return env;
}

CgenClassTable::CgenClassTable(Classes classes, ostream& s) : str(s) {

  // make sure the various tables have a scope
  class_to_tag_table.enterscope();

  enterscope();
  if (cgen_debug) std::cerr << "Building CgenClassTable" << std::endl;
  install_basic_classes();
  install_classes(classes);
  build_inheritance_tree();

  code();
  exitscope();
}


void CgenClassTable::install_basic_classes() {
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
	new CgenNode(class_(No_class,No_class,nil_Features(),filename),
		     Basic,this));
  addid(SELF_TYPE,
	new CgenNode(class_(SELF_TYPE,No_class,nil_Features(),filename),
		     Basic,this));
  addid(prim_slot,
	new CgenNode(class_(prim_slot,No_class,nil_Features(),filename),
		     Basic,this));

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
           single_Features(method(::copy, nil_Formals(), SELF_TYPE, no_expr()))),
	   filename),
    Basic,this));

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
    Basic,this));

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
     Basic,this));

//
// Bool also has only the "val" slot.
//
    install_class(
     new CgenNode(
      class_(Bool, Object, single_Features(attr(val, prim_slot, no_expr())),filename),
      Basic,this));

//
// The class Str has a number of slots and operations:
//       val                                  the string's length
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
        Basic,this));
}

// CgenClassTable::install_class
// CgenClassTable::install_classes
//
// install_classes enters a list of classes in the symbol table.
// The following possible errors are checked:
//       - a class called SELF_TYPE
//       - redefinition of a basic class
//       - redefinition of another previously defined class
//
void CgenClassTable::install_class(CgenNodeP nd) {
  Symbol name = nd->get_name();

  if (probe(name)) {
    return;
  }
  // The class name is legal, so add it to the list of classes
  // and the symbol table.
  class_to_tag_table.addid(name, new int(tag_counter));
  tag_counter++;
  classes.push_back(name);
  nds.push_front(nd);
  addid(name, nd);
}

void CgenClassTable::install_classes(Classes cs) {
  for(int i = cs->first(); cs->more(i); i = cs->next(i))
    install_class(new CgenNode(cs->nth(i), NotBasic, this));
}

//
// CgenClassTable::build_inheritance_tree
//
void CgenClassTable::build_inheritance_tree() {
  for (auto nd : nds) {
    set_relations(nd);
  }
}

//
// CgenClassTable::set_relations
//
// Takes a CgenNode and locates its, and its parent's, inheritance nodes
// via the class table.  Parent and child pointers are added as appropriate.
//
void CgenClassTable::set_relations(CgenNodeP nd) {
  CgenNode *parent_node = probe(nd->get_parent());
  nd->set_parentnd(parent_node);
  parent_node->add_child(nd);
}

void CgenNode::add_child(CgenNodeP n) {
  children.push_front(n);
}

void CgenNode::set_parentnd(CgenNodeP p)
{
  assert(parentnd == NULL);
  assert(p != NULL);
  parentnd = p;
}

CgenNodeP CgenNode::get_parentnd()
{
  return parentnd;
}

void CgenClassTable::code()
{
    if (cgen_debug) std::cerr << "coding global data" << std::endl;
    code_global_data();

    if (cgen_debug) std::cerr << "choosing gc" << std::endl;
    code_select_gc();

    if (cgen_debug) std::cerr << "coding constants" << std::endl;
    code_constants();

    if (cgen_debug) std::cerr << "coding class name table" << std::endl;
    code_class_name_table();

    if (cgen_debug) std::cerr << "coding class object table" << std::endl;
    code_class_obj_table();

    if (cgen_debug) std::cerr << "coding class parent table" << std::endl;
    code_class_parent_table();

    if (cgen_debug) std::cerr << "coding dispatch tables" << std::endl;
    std::map<Symbol, std::map<Symbol, int>>  method_ids = code_disp_tables();

    if (cgen_debug) std::cerr << "coding prototype objects" << std::endl;
    code_prot_objs();

    if (cgen_debug) std::cerr << "coding global text" << std::endl;
    code_global_text();

    if (cgen_debug) std::cerr << "coding initializers" << std::endl;
    Environment env = code_inits();

    env.method_let_vars_table = method_let_vars_table;
    env.method_ids = method_ids;
    for (CgenNodeP nd : nds) {
      env.so = nd;
      Symbol name = nd->get_name();
      if (name == Object || name == Str || name == IO || name == Bool || name == Int) {
        continue;
      }
      Symbol class_ = nd->get_name();
      Features features = lookup(class_)->get_features();
      for (int j = features->first(); features->more(j); j = features->next(j)) {
        features->nth(j)->code(str, nd, &env);
      }
    }

}


CgenNodeP CgenClassTable::root()
{
   return probe(Object);
}

CgenNodeP CgenClassTable::get_class_node(Symbol name)  {
    for (CgenNodeP nd : nds) {
      if (name == nd->get_name()) {
        return nd;
      }
    }
    cout << "class name not found!" << endl;
    return NULL;
  }


///////////////////////////////////////////////////////////////////////
//
// CgenNode methods
//
///////////////////////////////////////////////////////////////////////

CgenNode::CgenNode(Class_ nd,Basicness bstatus, CgenClassTableP ct) :
   class__class((const class__class &) *nd),
   parentnd(NULL),
   basic_status(bstatus)
{
  stringtable.add_string(name->get_string());          // Add class name to string table
}

// **
// Let traversal methods
// **

void assign_class::let_traverse() {
  expr->let_traverse();
}

void static_dispatch_class::let_traverse() {
  expr->let_traverse();
  for (int i = actual->first(); actual->more(i); i = actual->next(i)) { 
    actual->nth(i)->let_traverse();
  }
}

void dispatch_class::let_traverse() {
  expr->let_traverse();
  for (int i = actual->first(); actual->more(i); i = actual->next(i)) { 
    actual->nth(i)->let_traverse();
  }
  
}

void cond_class::let_traverse() {
  pred->let_traverse();
  then_exp->let_traverse();
  else_exp->let_traverse();
}

void loop_class::let_traverse() {
  pred->let_traverse();
  body->let_traverse();
}

void typcase_class::let_traverse() {
  expr->let_traverse();
  for (int i = cases->first(); cases->more(i); i = cases->next(i)) { 
    cases->nth(i)->let_traverse();
  }
}

void branch_class::let_traverse() {
  expr->let_traverse();
}

void block_class::let_traverse() {
  for (int i = body->first(); body->more(i); i = body->next(i)) { 
    body->nth(i)->let_traverse();
  }
}

void let_class::let_traverse() {
// todo: increment int counter
  let_counter++;
  init->let_traverse();
  body->let_traverse();
}

void plus_class::let_traverse() {
  e1->let_traverse();
  e2->let_traverse();
}
void sub_class::let_traverse() {
  e1->let_traverse();
  e2->let_traverse();
}
void mul_class::let_traverse() {
  e1->let_traverse();
  e2->let_traverse();
}
void divide_class::let_traverse() {
  e1->let_traverse();
  e2->let_traverse();
}

void neg_class::let_traverse() {
  e1->let_traverse();
}

void lt_class::let_traverse() {
  e1->let_traverse();
  e2->let_traverse();
}

void eq_class::let_traverse() {
  e1->let_traverse();
  e2->let_traverse();
}

void leq_class::let_traverse() {
  e1->let_traverse();
  e2->let_traverse();
}

void comp_class::let_traverse() {
  e1->let_traverse();
}

void int_const_class::let_traverse() {
  
}

void bool_const_class::let_traverse() {
  
}

void string_const_class::let_traverse() {
  
}

void new__class::let_traverse() {
  
}

void isvoid_class::let_traverse() {
  e1->let_traverse();
}

void no_expr_class::let_traverse() {
  
}

void object_class::let_traverse() {
  
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

void method_class::code(ostream &s, CgenNodeP nd, Environment* env) {
  emit_method_ref(nd->get_name(), name, s);
  s << LABEL;
  arg_count = 0;
  for (int i = formals->first(); formals->more(i); i = formals->next(i)) {
    // env.vars.addid(nd->get_name(), formals->nth(i)->get_name());
    // todo: idk how method formals are set
    std::pair<std::string, int>* value = new std::pair<std::string, int>("param", arg_count);
    env->addid(formals->nth(i)->get_name(), value);
    arg_count++;
  }
  // todo: seems like we also need to increment -12 by however many local variables + arguments there are. multiply 8 * let_counter
  // LET HANDLING
  int let_count = env->method_let_vars_table[env->so->get_name()][name];
  emit_addiu(SP, SP, -12 - let_count * 8, s);
  emit_store(FP, 3, SP, s);
  emit_store(SELF, 2, SP, s);
  emit_store(RA, 1, SP ,s);
  emit_addiu(FP, SP, 16, s);
  emit_move(SELF, ACC, s);
  let_counter = 0;
  env->cur_method = name;
  env->enterscope();  
  expr->code(s, env);
  emit_load(FP, 3, SP, s);
  emit_load(SELF, 2, SP, s);
  emit_load(RA, 1, SP, s);
  emit_addiu(SP, SP, 12 + arg_count * 4 + let_count * 8, s);
  emit_return(s);
  env->exitscope();
}

// do we need to do anything here? i don't think so -- jason
void attr_class::code(ostream &s, CgenNodeP nd, Environment* env) {
  init->code(s, env);
}

// TODO
void branch_class::code(ostream &s, Environment* env) {
  expr->code(s, env);
}

void assign_class::code(ostream &s, Environment* env) {
  expr->code(s, env);

  int id;
  id = env->lookup(name)->second;
  if (env->lookup(name)->first == "param") {
    emit_store(ACC, id, FP, s);
  } else if (env->lookup(name)->first == "attr") {
    emit_store(ACC, id + 3, SELF, s);
    if (cgen_Memmgr > 0) {
      // todo: add addiu operaiton
      emit_gc_assign(s);
    }
  } else if (env->lookup(name)->first == "letvar") {
    emit_load(ACC, id, SP, s);
  } 
}

void static_dispatch_class::code(ostream &s, Environment* env) {
  for (int i = actual->first(); actual->more(i); i = actual->next(i)) {
    actual->nth(i)->code(s, env);
    emit_push(ACC, s);
  }

  expr->code(s, env);

  // dispatch on void
  emit_bne(ACC, ZERO, label, s);
  s << LA << ACC << " str_const0" << endl;
  emit_load_imm(T1, line_number, s);
  emit_jal("_dispatch_abort", s);

  emit_label_def(label, s);
  label++;

  // get dispatch table for the statically dispatched class
  CgenNodeP cur_class_node; 
  for (CgenNodeP nd : env->nds) {
      if (type_name == nd->get_name()) {
        cur_class_node = nd;
        break;
      }
  }

  // load dispatch table
  std::string disp_tab = type_name->get_string();
  disp_tab.append(DISPTAB_SUFFIX);
  emit_load_address(T1, disp_tab.c_str(), s);

  // get method from dispatch table
  int id = env->get_method_ids().at(cur_class_node->get_name()).at(name);
  emit_load(T1, id, T1, s);
  emit_jalr(T1, s);
}

void dispatch_class::code(ostream &s, Environment* env) {
  env->enterscope();
  for (int i = actual->first(); actual->more(i); i = actual->next(i)) {
    actual->nth(i)->code(s, env);
    // env->addid(formals->nth(i)->get_name(), &std::make_pair("param", arg_count + 2));
    emit_push(ACC, s);
  }
  expr->code(s, env);

  // added to conform with code, also seems like it's necessary for BNE to work?
  // emit_load(ACC, 3, SELF, s);

  // dispatch on void
  emit_bne(ACC, ZERO, label, s);
  s << LA << ACC << " str_const0" << std::endl;
  emit_load_imm(T1, line_number, s);
  emit_jal("_dispatch_abort", s);


  emit_label_def(label, s);
  label++;

  Symbol cur_class = env->get_so()->get_name();
  if (expr->get_type() != SELF_TYPE) {
    cur_class = expr->get_type();
  }

  // seg faults here
  CgenNodeP cur_class_node; 
  // codegen_classtable->get_class_node(cur_class);
  for (CgenNodeP nd : env->nds) {
      if (cur_class == nd->get_name()) {
        cur_class_node = nd;
        break;
      }
  }
  emit_load(T1, 2, ACC, s);
  int id = env->get_method_ids().at(cur_class_node->get_name()).at(name);
  emit_load(T1, id, T1, s);
  emit_jalr(T1, s);
}

void cond_class::code(ostream &s, Environment* env) {
  pred->code(s, env);
  emit_fetch_int(T1, ACC, s);
  int else_label = label;
  label++;
  int then_label = label;
  label++;
  emit_beqz(T1, else_label, s);
  then_exp->code(s, env);
  emit_branch(then_label, s);
  emit_label_def(else_label, s);
  else_exp->code(s, env);
  emit_label_def(then_label, s);
}

void loop_class::code(ostream &s, Environment* env) {
  int start = label;
  label++;
  emit_label_def(label, s);
  pred->code(s, env);
  emit_fetch_int(ACC, ACC, s);
  int exit = label;
  label++;
  emit_beqz(ACC, exit, s);
  body->code(s, env);
  emit_branch(start, s);
  emit_label_def(exit, s);
  emit_move(ACC, ZERO, s);
}

// TODO
void typcase_class::code(ostream &s, Environment* env) {
  expr->code(s, env);
  emit_bne(ACC, ZERO, label, s);
  emit_load_address(ACC, "str_const0", s);
  emit_load_imm(T1, line_number, s);
  emit_jal("_case_abort2", s);

  std::map<Symbol, int> class_tags = env->class_tags;
  CgenNodeP so = env->get_so();

  std::vector<Symbol> branch_types;
  std::map<Symbol, branch_class*> branches;
  std::map<Symbol, int> branch_labels;

  int expr_check = label;
  label++;
  int parent_loop = label;
  label++;
  int branch_success = label;
  label++;
  int branch_fail = label;
  label++;

  for (int i = cases->first(); cases->more(i); i = cases->next(i)) {
    branch_class* branch = (branch_class*)(cases->nth(i));
    Symbol branch_type = branch->get_type();
    branches[branch_type] = branch;
    branch_labels[branch_type] = label;
    label++;
    branch_types.push_back(branch_type);
  }

  std::sort(branch_types.begin(), branch_types.end(), [&](auto const &a, auto const &b) {
    return class_tags[a] > class_tags[b];
  });

  emit_label_def(expr_check, s);
  emit_load(T1, 0, ACC, s);
  emit_move("$t3", T1, s);
  emit_load_address("$t4", CLASSPARENTTAB, s);
  emit_load_imm("$t8", NOPARENTTAG, s);
  emit_load_imm("$t9", 4, s);
  emit_label_def(parent_loop, s);
  emit_beq("$t3", "$t8", branch_fail, s);
  for (Symbol branch_type : branch_types) {
    int l = branch_labels[branch_type];
    int tag = class_tags[branch_type];
    emit_load_imm(T2, tag, s);
    emit_beq("$t3", T2, l, s);
  }

  emit_mul("$t3", "$t3", "$t9", s);
  emit_add("$t3", "$t3", "$t4", s);
  emit_load("$t3", 0, "$t3", s);

  emit_beq(T1, T1, parent_loop, s);

  for (Symbol branch_type : branch_types) {
    int l = branch_labels[branch_type];
    int tag = class_tags[branch_type];
    branch_class* branch = branches[branch_type];

    emit_label_def(l, s);
    emit_push(ACC, s);
    env->enterscope();

    // can we can treat the branch as a let variable??
    int id = internal_let_counter + env->method_let_vars_table.at(env->so->get_name()).at(env->cur_method);
    env->addid(branch->get_name(), new std::pair<std::string, int>("let", id));
    branch->code(s, env);
    emit_addiu(SP, SP, 4, s);
    env->exitscope();
    emit_beq(SP, SP, branch_success, s);
  }

  emit_label_def(branch_fail, s);
  emit_load_address("$t3", CLASSNAMETAB, s);
  emit_load_imm(T2, 4, s);
  emit_mul(T2, T1, T2, s);
  emit_add(A1, "$t3", T2, s);
  emit_jal("_case_abort", s);
  emit_label_def(branch_success, s);
}

void block_class::code(ostream &s, Environment* env) {
  for (int i = body->first(); body->more(i); i = body->next(i)) {
    body->nth(i)->code(s, env);
  }
}

// TODO
void let_class::code(ostream &s, Environment* env) {
  // add single let to scope
  env->enterscope();
  int id = internal_let_counter + env->method_let_vars_table.at(env->so->get_name()).at(env->cur_method);
  std::pair<std::string, int>* val = new std::pair<std::string, int>("letvar", id);
  env->addid(identifier, val);
  internal_let_counter++;

  init->code(s, env);
  // emit_load_address("$s1", ACC, s);
  body->code(s, env);
  // emit_addiu(SP, SP, 4, s);
  // init->code(s, env);
  
  internal_let_counter = 0;
  env->exitscope();
  // // todo: enter a new scope into the environment, then add all of the new offsets for the formal parameters
  // env->enterscope();
  // // todo: not sure if let_counter + arg_count is right
  // emit_push(ACC, s);
  // int id = let_counter // + total_lets;
  // std::pair<std::string, int>* val = new std::pair<std::string, int>("letvar", id + 1);
  // env->addid(identifier, val);
  // let_counter++;
  // emit_store("s1", id, s);
  // body->code(s, env);
  // emit_load(s1, id, s);
  // env->exitscope();
}

void plus_class::code(ostream &s, Environment* env) {
  e1->code(s, env);
  emit_push(ACC, s);
  e2->code(s, env);
  emit_jal("Object.copy", s);
  emit_addiu(SP, SP, 4, s);
  emit_load(T1, 0, SP, s);
  emit_fetch_int(T1, T1, s);
  emit_fetch_int(T2, ACC, s);
  emit_add(T1, T1, T2, s);
  emit_store_int(T1, ACC, s);
}

void sub_class::code(ostream &s, Environment* env) {
  e1->code(s, env);
  emit_push(ACC, s);
  e2->code(s, env);
  emit_jal("Object.copy", s);
  emit_addiu(SP, SP, 4, s);
  emit_load(T1, 0, SP, s);
  emit_fetch_int(T1, T1, s);
  emit_fetch_int(T2, ACC, s);
  emit_sub(T1, T1, T2, s);
  emit_store_int(T1, ACC, s);
}

void mul_class::code(ostream &s, Environment* env) {
  e1->code(s, env);
  emit_push(ACC, s);
  e2->code(s, env);
  emit_jal("Object.copy", s);
  emit_addiu(SP, SP, 4, s);
  emit_load(T1, 0, SP, s);
  emit_fetch_int(T1, T1, s);
  emit_fetch_int(T2, ACC, s);
  emit_mul(T1, T1, T2, s);
  emit_store_int(T1, ACC, s);
}

void divide_class::code(ostream &s, Environment* env) {
  e1->code(s, env);
  emit_push(ACC, s);
  e2->code(s, env);
  emit_jal("Object.copy", s);
  emit_addiu(SP, SP, 4, s);
  emit_load(T1, 0, SP, s);
  emit_fetch_int(T1, T1, s);
  emit_fetch_int(T2, ACC, s);
  emit_div(T1, T1, T2, s);
  emit_store_int(T1, ACC, s);
}

void neg_class::code(ostream &s, Environment* env) {
  e1->code(s, env);
  emit_jal("Object.copy", s);
  emit_fetch_int(T1, ACC, s);
  emit_neg(T1, T1, s);
  emit_store_int(T1, ACC, s);
}

void lt_class::code(ostream &s, Environment* env) {
  e1->code(s, env);
  emit_push(ACC, s);
  e2->code(s, env);
  emit_load(T1, 1, SP, s);
  emit_fetch_int(T1, T1, s);
  emit_fetch_int(T2, ACC, s);

  emit_load_bool(ACC, truebool, s);
  emit_blt(T1, T2, label, s);
  emit_load_bool(ACC, falsebool, s);
  emit_label_def(label, s);
  label++;

  emit_addiu(SP, SP, 4, s);
}

void eq_class::code(ostream &s, Environment* env) {
  e1->code(s, env);
  emit_push(ACC, s);
  e2->code(s, env);
  emit_move(T2, ACC, s);
  emit_load(T1, 1, SP, s);
  emit_addiu(SP, SP, 4, s);
  emit_load_bool(ACC, truebool, s);
  emit_load_bool(A1, falsebool, s);

  if (e1->get_type() == Int || e1->get_type() == Str || e1->get_type() == Bool) {
    emit_jal("equality_test", s);
  } else {
    emit_beq(T1, T2, label, s);
    emit_load_bool(ACC, falsebool, s);
    emit_label_def(label, s);
    label++;
  }
}

void leq_class::code(ostream &s, Environment* env) {
  e1->code(s, env);
  emit_push(ACC, s);
  e2->code(s, env);
  emit_load(T1, 1, SP, s);
  emit_fetch_int(T1, T1, s);
  emit_fetch_int(T2, ACC, s);

  emit_load_bool(ACC, truebool, s);
  emit_bleq(T1, T2, label, s);
  emit_load_bool(ACC, falsebool, s);
  emit_label_def(label, s);
  label++;
  
  emit_addiu(SP, SP, 4, s);
}

void comp_class::code(ostream &s, Environment* env) {
  e1->code(s, env);
  emit_load(T1, 3, ACC, s);
  emit_load_bool(ACC, truebool, s);
  emit_beqz(T1, label, s);
  emit_load_bool(ACC, falsebool, s);
  emit_label_def(label, s);
  label++;
}

void int_const_class::code(ostream& s, Environment* env)
{
  //
  // Need to be sure we have an IntEntry *, not an arbitrary Symbol
  //
  emit_load_int(ACC,inttable.lookup_string(token->get_string()),s);
}

void string_const_class::code(ostream& s, Environment* env)
{
  emit_load_string(ACC,stringtable.lookup_string(token->get_string()),s);
}

void bool_const_class::code(ostream& s, Environment* env)
{
  emit_load_bool(ACC, BoolConst(val), s);
}

void new__class::code(ostream &s, Environment* env) {
  Symbol type = type_name;
  if (type == SELF_TYPE) {
    emit_store("$s1", 1, FP, s);
    emit_load_address(T1, "class_objTab", s);
    emit_load(T2, 0, SELF, s);
    emit_sll(T2, T2, 3, s);
    emit_addu(T1, T1, T2, s);
    emit_move("$s1", T1, s);
    emit_load(ACC, 0, T1, s);
    emit_jal("Object.copy", s);
    emit_load(T1, 1, "$s1", s);
    emit_jalr(T1, s);
    emit_load("$s1", 1, FP, s);
  } else {
    emit_partial_load_address(ACC, s);
    emit_protobj_ref(type_name, s);
    s << std::endl;
    emit_jal("Object.copy", s);
    s << JAL << type->get_string() << CLASSINIT_SUFFIX << std::endl;
  }
}

void isvoid_class::code(ostream &s, Environment* env) {
  e1->code(s, env);
  emit_move(T1, ACC, s);
  emit_load_bool(ACC, truebool, s);
  emit_beqz(T1, label, s);
  emit_load_bool(ACC, falsebool, s);
  emit_label_def(label, s);
  label++;
}

void no_expr_class::code(ostream &s, Environment* env) {
  emit_move(ACC, ZERO, s);
}

void object_class::code(ostream &s, Environment* env) {
  // TODO: map object identifier to environment?
  CgenNodeP cur_class_node; 
  Symbol cur_class = env->get_so()->get_name();
  for (CgenNodeP nd : env->nds) {
      if (cur_class == nd->get_name()) {
        cur_class_node = nd;
        break;
      }
  }
  int id;
  if (name == self) {
    emit_move(ACC, SELF, s);
    return;
  } else {
    id = env->lookup(name)->second;
  }
  if (env->lookup(name)->first == "param") {
    emit_load(ACC, id, FP, s);
  } else if (env->lookup(name)->first == "attr") {
    emit_load(ACC, id + 3, SELF, s);
    if (cgen_Memmgr > 0) {
      // todo: add addiu operation and do this for param and letvar
      emit_gc_assign(s);
    }
    emit_load(ACC, id + 3, SELF, s);
  } else if (env->lookup(name)->first == "letvar") {
     emit_load(ACC, id + 1, SP, s);
  } 
}

