#ifndef SEMANT_H_
#define SEMANT_H_

#include <assert.h>
#include <iostream>
#include <list>
#include "tree.h"
#include "stringtab.h"
#include "symtab.h"
#include "cool-tree.handcode.h"
#include "cool-tree.h"


#define TRUE 1
#define FALSE 0

// class Program_class;
// typedef Program_class *Program;
// class Class__class;
// typedef Class__class *Class_;
// class Feature_class;
// typedef Feature_class *Feature;
// class Formal_class;
// typedef Formal_class *Formal;
// class Expression_class;
// typedef Expression_class *Expression;
// class Case_class;
// typedef Case_class *Case;

// typedef list_node<Class_> Classes_class;
// typedef Classes_class *Classes;
// typedef list_node<Feature> Features_class;
// typedef Features_class *Features;
// typedef list_node<Formal> Formals_class;
// typedef Formals_class *Formals;
// typedef list_node<Expression> Expressions_class;
// typedef Expressions_class *Expressions;
// typedef list_node<Case> Cases_class;
// typedef Cases_class *Cases;

class InheritanceNode;
typedef InheritanceNode *InheritanceNodeP;
class ClassTable;
typedef ClassTable *ClassTableP;

class InheritanceNode {
private:
  Symbol parent;
  Features features;
  Class_ thisclass_;
public:
  InheritanceNode(Class_ class_);
  Symbol get_parent();
  Features get_features();
  Class_ get_class();
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
  int errors() { return semant_errors; };
  std::ostream& semant_error();
  std::ostream& semant_error(Class_ c);
  std::ostream& semant_error(Symbol filename, tree_node *t);
};


#endif
