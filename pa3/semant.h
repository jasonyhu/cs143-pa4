#ifndef SEMANT_H_
#define SEMANT_H_

#include <assert.h>
#include "cool-tree.h"
#include "stringtab.h"
#include "symtab.h"
#include <list>

#define TRUE 1
#define FALSE 0

class InheritanceNode;
typedef InheritanceNode *InheritanceNodeP;
class ClassTable;
typedef ClassTable *ClassTableP;


class InheritanceNode {
  private:
    Symbol parent;
  public:
    InheritanceNode(Class_ class_) {
      parent = class_->get_parent();
    };
    bool operator<(const InheritanceNode& other) const {
      return this->parent < other.parent;
    }
};

// This is a structure that may be used to contain the semantic
// information such as the inheritance graph.  You may use it or not as
// you like: it is only here to provide a container for the supplied
// methods.
class ClassTable : public SymbolTable<Symbol, InheritanceNode> {
private:
  int semant_errors;           // counts the number of semantic errors
  void install_basic_classes();
  std::ostream& error_stream;

public:
  ClassTable(Classes);
  Classes basic_classes;
  SymbolTable<Symbol, Class_> *map = new SymbolTable<Symbol, Class_>();
  int errors() { return semant_errors; }
  std::ostream& semant_error();
  std::ostream& semant_error(Class_ c);
  std::ostream& semant_error(Symbol filename, tree_node *t);
};


#endif
