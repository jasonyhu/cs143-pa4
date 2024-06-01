#include <assert.h>
#include <stdio.h>
#include <string.h>
#include <list>
#include <vector>
#include <algorithm>
#include <utility>
#include <map>
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
  std::list<CgenNodeP> nds;  // nodes
  std::ostream& str;
  SymbolTable<Symbol,int> class_to_tag_table;
  int tag_counter = 0;


  // The following methods emit code for constants and global declarations.
  void code_global_data();
  void code_global_text();
  void code_bools();
  void code_select_gc();
  void code_constants();
  void code_class_name_table();
  void code_class_obj_table();
  void  code_disp_tables();
  void code_prot_objs();
  void code_inits();

  // The following creates an inheritance graph from a list of classes. The
  // graph is implemented as a tree of `CgenNode', and class names are placed
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
  CgenNodeP get_class_node(Symbol name);
};

class CgenNode : public class__class {
private:

  CgenNodeP parentnd;
  std::list<CgenNodeP> children;
  Basicness basic_status;
  std::vector<attr_class*> all_attrs;
  std::map<Symbol, Symbol> all_methods;
  std::map<Symbol, int> attr_ids;
  std::map<Symbol, int> method_ids;

public:
  CgenNode(Class_ c,
	   Basicness bstatus,
	   CgenClassTableP class_table);

  void add_child(CgenNodeP child);
  std::list<CgenNodeP>& get_children() { return children; }
  void set_parentnd(CgenNodeP p);
  CgenNodeP get_parentnd();
  int basic() { return (basic_status == Basic); }
  // void disp_traversal(ostream& str);
  // void attr_traversal(ostream& str);

  std::vector<attr_class*> get_all_attrs();
  std::map<Symbol, int> get_attr_ids() { return attr_ids; };
  std::map<Symbol, Symbol> get_all_methods();
  std::map<Symbol, int> get_method_ids() { return method_ids; };
  // std::map<Symbol, std::map<Symbol, int>> get_method_ids() { return method_ids; };
  // void disp_traversal(Symbol dispTabClass, ostream& str, std::map<Symbol, std::map<Symbol, int>>& method_ids);
};

class BoolConst {
 private:
  int val;
 public:
  BoolConst(int);
  void code_def(std::ostream&, int boolclasffstag);
  void code_ref(std::ostream&) const;
};

class Environment {
  public:
    Environment(CgenNodeP so) : so(so) {};
    std::list<CgenNodeP> nds;
    int lookup_param(Symbol name);
    int lookup_attr(Symbol name) {
      std::map<Symbol, int> attr_ids = so->get_attr_ids();
      return attr_ids.at(name);
    };
    int lookup_var(Symbol name);
    int add_let(Symbol name) {
      let_vars.push_back(name);
      return let_vars.size() - 1;
    }
    int add_param(Symbol name) {
      params.push_back(name);
      return params.size() - 1;
    }
    CgenNodeP get_so() const { return so; }
  private:
    CgenNodeP so;
    std::vector<Symbol> let_vars;
    std::vector<Symbol> params;
};

// ALEX: string is kind of funky so i'm open to changing it to something cleaner, but we can just use "attr", "let", "case", "newO" (newO), and "param" to standardize

