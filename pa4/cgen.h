#include <assert.h>
#include <stdio.h>
#include <string.h>
#include <list>
#include "cool-tree.h"
#include "emit.h"
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
  std::list<CgenNodeP> nds;
  std::ostream& str;
  std::list<Symbol> classes;
  SymbolTable<Symbol,int> class_to_tag_table;
  int* tagCounter = (int *)malloc(sizeof(int));


  // The following methods emit code for constants and global declarations.
  void code_global_data();
  void code_global_text();
  void code_bools();
  void code_select_gc();
  void code_constants();

  // The following creates an inheritance graph from a list of classes. The
  // graph is implemented as  a tree of `CgenNode', and class names are placed
  // in the base class symbol table.
  void install_basic_classes();
  void install_class(CgenNodeP nd);
  void install_classes(Classes cs);
  void build_inheritance_tree();
  void set_relations(CgenNodeP nd);
public:
  CgenClassTable(Classes, std::ostream& str);
  void code();
  CgenNodeP root();
};

class CgenNode : public class__class {
private:

  CgenNodeP parentnd;
  std::list<CgenNodeP> children;
  Basicness basic_status;

public:
  CgenNode(Class_ c,
	   Basicness bstatus,
	   CgenClassTableP class_table);

  void add_child(CgenNodeP child);
  std::list<CgenNodeP>& get_children() { return children; }
  void set_parentnd(CgenNodeP p);
  CgenNodeP get_parentnd();
  int basic() { return (basic_status == Basic); }
  void disp_traversal(ostream& str);
  void attr_traversal(ostream& str);
};

class BoolConst {
 private:
  int val;
 public:
  BoolConst(int);
  void code_def(std::ostream&, int boolclasstag);
  void code_ref(std::ostream&) const;
};

