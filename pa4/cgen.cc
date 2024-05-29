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
#include "cgen_supp.h"
#include "handle_flags.h"

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
  // TODO: additional symobls
  // Symbol class_   = idtable.lookup_string(BOOLNAME);
  // Symbol class_   = idtable.lookup_string(BOOLNAME);
  // Symbol class_   = idtable.lookup_string(BOOLNAME);

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

std::map<Symbol, Symbol> CgenNode::get_all_methods() {
  if (all_methods.empty()) {
    std::vector<CgenNode*> parents;
    CgenNode* cn = this;
    while (cn->name != No_class) {
      parents.push_back(cn);
      cn = cn->get_parentnd();
    }
    std::reverse(parents.begin(), parents.end());
    for (CgenNode* nd : parents) {
      Features parent_features = nd->features;
      for (int j = parent_features->first(); parent_features->more(j); j = parent_features->next(j)) {
        Feature f = parent_features->nth(j);
        if (f->is_method()) {
          all_methods[((method_class*)f)->get_name()] = nd->get_name();
        }
      }
    }
  }
  return all_methods;
}

void CgenClassTable::code_disp_tables() {
  for (CgenNodeP nd : nds) {
    Symbol class_ = nd->get_name();
    emit_disptable_ref(class_, str);
    str << LABEL;
    std::map<Symbol, Symbol> all_methods = nd->get_all_methods();
    for (auto method = all_methods.begin(); method != all_methods.end(); method++) {
      str << WORD << method->second << "." << method->first << std::endl;
    }
  }
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
    for (CgenNode* nd : parents) {
      Features parent_features = nd->features;
      for (int j = parent_features->first(); parent_features->more(j); j = parent_features->next(j)) {
        Feature f = parent_features->nth(j);
        if (!f->is_method()) {
          all_attrs.push_back((attr_class*)f);
        }
      }
    }
  }
  return all_attrs;
}

void CgenClassTable::code_prot_objs() {
  for (CgenNodeP nd : nds) {
    std::vector<attr_class*> attribs = nd->get_all_attrs();
    // garbage collector tag
    str << WORD << -1 << std::endl;
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
          stringtable.lookup_string("0")->code_ref(str);
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

void CgenClassTable::code_inits() {
  for (CgenNode* nd : nds) {
    str << nd->get_name() << CLASSINIT_SUFFIX << LABEL;
    emit_addiu(SP, SP, -12, str);
    emit_store(FP, 3, SP, str);
    emit_store(SELF, 2, SP, str);
    emit_store(RA, 1, SP, str);
    emit_addiu(FP, SP, 4, str);
    emit_move(SELF, ACC, str);
    Symbol parent = nd->get_parent();
    if (parent != No_class) {
      str << JAL;
      emit_init_ref(parent, str);
      str << std::endl;
    }
    std::vector<attr_class*> attribs = nd->get_all_attrs();
    for (auto attrib : attribs) {
      if (attrib != nullptr) {
        if (attrib->get_init()->get_type()) {
          attrib->get_init()->code(str);
          
        }
      }
    }
    // TODO: finish init functions
  }
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

// void method_class::disPrint(Symbol parent, ostream& str) {
//   str << WORD << parent->get_string() << "." << name->get_string() << std::endl;
// }

// // deprecated
// void attr_class::disPrint(Symbol parent, ostream& str) {
//   return;
// }

// // unused
// void method_class::attrPrint(Symbol parent, ostream& str) {
//   return;
// }

// // unused
// void attr_class::attrPrint(Symbol parent, ostream& str) {
//   // TODO: what is the difference between Int and void?
//   if (name == Int) {
//     str << WORD << 0 << std::endl;
//   } else if (name == Bool) {
//     str << WORD << "false" << std::endl;
//   } else if (name == Str) {
//     str << WORD << "" << std::endl;
//   } else {
//     str << WORD << 0 << std::endl;
//   }
// }

// void CgenNode::disp_traversal(ostream& str) {
//   if (get_parentnd() != NULL) {
//     get_parentnd()->disp_traversal(str);
//   }
//   for (int i = features->first(); features->more(i); i = features->next(i)) {
//     Feature f = features->nth(i);
//     if (f->is_method()) {
//       f->disPrint(name, str);
//     }
//   }
// }

// void CgenNode::attr_traversal(ostream& str) {
//   if (get_parentnd() != NULL) {
//     get_parentnd()->attr_traversal(str);
//   }
//   if (name == Int) {
//     str << WORD << 0 << std::endl;
//   } else if (name == Bool) {
//     str << WORD << 0 << std::endl;
//   // TODO: what do i even do with string
//   } else if (name == Str) {
//     str << WORD << 0 << std::endl;
//     str << WORD << "" << std::endl;
//   } else {
//     for (int i = features->first(); features->more(i); i = features->next(i)) {
//       features->nth(i)->attrPrint(name, str);
//     }
//   }
// }

void CgenClassTable::code()
{
    if (cgen_debug) std::cerr << "coding global data" << std::endl;
    code_global_data();

    if (cgen_debug) std::cerr << "choosing gc" << std::endl;
    code_select_gc();

    if (cgen_debug) std::cerr << "coding constants" << std::endl;
    code_constants();

    //                 Add your code to emit
    //                   - prototype objects
    //                   - class_nameTab
    //                   - dispatch tables
    //

    /*
    For each table, you should insert a label for the table (which matches the label expected by the runtime.) A label is a location 
    in the program code. In other parts of the program, you can use the label as you would a constant value, and the assembler/linker 
    will insert the actual address that code was loaded into memory. The runtime can also use these labels if you declare them to be .globl 
    (see code_global_data for some example.)
    */
   // TODO: incomplete

    if (cgen_debug) std::cerr << "coding class name table" << std::endl;
    code_class_name_table();

    if (cgen_debug) std::cerr << "coding class object table" << std::endl;
    code_class_obj_table();

    if (cgen_debug) std::cerr << "coding dispatch tables" << std::endl;
    code_disp_tables();

    if (cgen_debug) std::cerr << "coding prototype objects" << std::endl;
    code_prot_objs();

    if (cgen_debug) std::cerr << "coding global text" << std::endl;
    code_global_text();

    if (cgen_debug) std::cerr << "coding initializers" << std::endl;
    code_inits();
    //                 Add your code to emit
    //                   - object initializer
    //                   - the class methods
    //                   - etc...


     // set up the recursive code generation
  /* In your code() method, recurse over each class.

  For each class, emit code to generate the init function. (Also, you should generate the methods, but for simplicity we'll focus on the attributes.)

  The init function generates code to initialize each attribute using its initializer.

  If the attribute's initializer is an integer constant, it will call int_const_class::code.
  */
  // TODO: add more passes depending on what we need
  for (CgenNodeP nd : nds) {
    Symbol class_ = nd->get_name();
    Features features = lookup(class_)->get_features();
    for (int j = features->first(); features->more(j); j = features->next(j)) {
      // TODO: what does os mean in this context, am i using it wrong
      // TODO: do i even need traverse? "emit code to generate the init function"
      features->nth(j)->traverse(str);
    }
  }

}


CgenNodeP CgenClassTable::root()
{
   return probe(Object);
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


//******************************************************************
//
//   Fill in the following methods to produce code for the
//   appropriate expression.  You may add or remove parameters
//   as you wish, but if you do, remember to change the parameters
//   of the declarations in `cool-tree.h'  Sample code for
//   constant integers, strings, and booleans are provided.
//
//*****************************************************************

void method_class::traverse(ostream &s) {
  // TODO: handle formals?
  expr->code(s);
}

void attr_class::traverse(ostream &s) {
  init->code(s);
}

void branch_class::code(ostream &s) {
  expr->code(s);
}

void assign_class::code(ostream &s) {
  expr->code(s);
}

void static_dispatch_class::code(ostream &s) {
  for (int i = actual->first(); actual->more(i); i = actual->next(i)) {
    actual->nth(i)->code(s);
  }
  expr->code(s);
}

void dispatch_class::code(ostream &s) {
  for (int i = actual->first(); actual->more(i); i = actual->next(i)) {
    actual->nth(i)->code(s);
  }
  expr->code(s);
}

void cond_class::code(ostream &s) {
  pred->code(s);
  then_exp->code(s);
  else_exp->code(s);
}

void loop_class::code(ostream &s) {
  pred->code(s);
  body->code(s);
}

void typcase_class::code(ostream &s) {
  expr->code(s);
  for (int i = cases->first(); cases->more(i); i = cases->next(i)) {
    cases->nth(i)->code(s);
  }
}

void block_class::code(ostream &s) {
  for (int i = body->first(); body->more(i); i = body->next(i)) {
    Expression exp = body->nth(i);
    exp->code(s);
  }
}

void let_class::code(ostream &s) {
  init->code(s);
  body->code(s);
}

void plus_class::code(ostream &s) {
  e1->code(s);
  emit_push(ACC, s);
  e2->code(s);
  emit_load(T1, 1, SP, s);
  emit_add(ACC, T1, ACC, s);
  emit_addiu(SP, SP, 4, s);
}

void sub_class::code(ostream &s) {
  e1->code(s);
  e2->code(s);
}

void mul_class::code(ostream &s) {
  e1->code(s);
  e2->code(s);
}

void divide_class::code(ostream &s) {
  e1->code(s);
  e2->code(s);
}

void neg_class::code(ostream &s) {
  e1->code(s);
}

void lt_class::code(ostream &s) {
  e1->code(s);
  e2->code(s);
}

void eq_class::code(ostream &s) {
  e1->code(s);
  e2->code(s);
}

void leq_class::code(ostream &s) {
  e1->code(s);
  e2->code(s);
}

void comp_class::code(ostream &s) {
  e1->code(s);
}

void int_const_class::code(ostream& s)
{
  //
  // Need to be sure we have an IntEntry *, not an arbitrary Symbol
  //
  emit_load_int(ACC,inttable.lookup_string(token->get_string()),s);
}

void string_const_class::code(ostream& s)
{
  emit_load_string(ACC,stringtable.lookup_string(token->get_string()),s);
}

void bool_const_class::code(ostream& s)
{
  emit_load_bool(ACC, BoolConst(val), s);
}

void new__class::code(ostream &s) {

}

void isvoid_class::code(ostream &s) {
  e1->code(s);
}

void no_expr_class::code(ostream &s) {
  // emit_move(ACC, ZERO, s);
}

void object_class::code(ostream &s) {

}

