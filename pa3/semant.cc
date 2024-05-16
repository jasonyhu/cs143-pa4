

#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <set>
#include <vector>
#include "semant.h"
#include "cool-tree.h"
#include "cool-tree.handcode.h"
#include "utilities.h"

extern int semant_debug;
extern char *curr_filename;
extern int node_lineno;

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
       isProto,
       length,
       Main,
       main_meth,
       No_class,
       No_type,
       _BOTTOM_,
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
static void initialize_constants(void) {
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
  isProto     = idtable.add_string("isProto");
  length      = idtable.add_string("length");
  Main        = idtable.add_string("Main");
  main_meth   = idtable.add_string("main");
  //   _no_class is a symbol that can't be the name of any
  //   user-defined class.
  No_class    = idtable.add_string("_no_class");
  No_type     = idtable.add_string("_no_type");
  // _BOTTOM_ is the symbol for the bottom of the lattice of types
  _BOTTOM_    = idtable.add_string("_bottom");
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

/* Inheritance Node: basic functions */
Symbol InheritanceNode::get_parent() { return parent; };
Features InheritanceNode::get_features() { return features; };
Class_ InheritanceNode::get_class() { return thisclass_; };

ClassTable::ClassTable(Classes classes) : semant_errors(0), error_stream(cerr) {
  
  enterscope();
  install_basic_classes();

  /* 
   * Loops through all classes in the class tree and adds them to a class table.

   * Throws an error if a class redefines a basic class, 
   * redefines an existing class,
   * or inherits an uninheritable class.
  */
  for (int i = classes->first(); classes->more(i); i = classes->next(i)) {
    Class_ class_ = classes->nth(i); 
    Symbol name = class_->get_name();
    if (name == IO || name == Int || name == Str || name == Bool || name == Object || name == SELF_TYPE) {
      semant_error(class_) << "Redefinition of basic class " << name->get_string() << ".\n";
    } else if (lookup(name) != NULL) {
      semant_error(class_) << "Class " << name->get_string() << " was previously defined.\n";
    } 
    Symbol parent = class_->get_parent();
    if (parent == Int || parent == Str || parent == Bool || parent == SELF_TYPE) {
      semant_error(class_) << "Class " << name->get_string() << " cannot inherit class " << parent->get_string() << ".\n";
    }

    addid(name, new InheritanceNode(class_));
  }
  
  // Throws an error for inheritance from undefined classes.
  for (int i = classes->first(); classes->more(i); i = classes->next(i)) {
    Class_ class_ = classes->nth(i); 
    Symbol name = class_->get_name();
    Symbol parent = class_->get_parent();
    if (lookup(parent) == NULL) {
      semant_error(class_) << "Class " << name->get_string() << " inherits from an undefined class " << parent->get_string() << ".\n";
    }
  }

  // All Cool programs must define a Main class.
  if (lookup(Main) == NULL) {
    semant_error() << "Class Main is not defined.\n";
  }

  if (errors() > 0) {
    return;
  }
  
  
  // iterates through each node, then traverses the recursive parents of that node to look for inheritance cycles
  for (int i = classes->first(); classes->more(i); i = classes->next(i)) {
    Class_ cur = classes->nth(i);
    Symbol parent = cur->get_parent();
    while (parent != Object && parent != classes->nth(i)->get_name()) {
      cur = lookup(parent)->get_class();
      parent = cur->get_parent();
    }
    if (parent != Object) {
      Class_ loop_start = cur;
      while (true) {
        semant_error(cur) << "Class " << cur->get_name()->get_string() << ", or an ancestor of " << cur->get_name()->get_string() << ", is involved in an inheritance cycle.\n";
        cur = lookup(cur->get_parent())->get_class();
        if (cur == loop_start) {
          break;
        }
      }
    }
  }
}

Class_ lub(ClassTable* classes, Class_ x, Class_ y) {
  // least common ancestor in the inheritance tree
  std::set<Class_> xSet;
  xSet.insert(classes->lookup(Object)->get_class());
  // add x's inheritance tree to the set
  Class_ cur = x;
  while (cur != classes->lookup(Object)->get_class()) {
    cur = classes->lookup(x->get_parent())->get_class();
    xSet.insert(cur);
  }
  cur = y;
  // check for collision
  while (cur != classes->lookup(Object)->get_class()) {
    cur = classes->lookup(y->get_parent())->get_class();
    if (xSet.find(cur) != xSet.end()) {
      return cur;
    }
  }
  return classes->lookup(Object)->get_class();
}

void ClassTable::install_basic_classes() {
  // The tree package uses these globals to annotate the classes built below.
  node_lineno  = 0;
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
  //        cool_abort() : Object    aborts the program
  //        type_name() : Str        returns a string representation of class name
  //        copy() : SELF_TYPE       returns a copy of the object
  //
  // There is no need for method bodies in the basic classes---these
  // are already built in to the runtime system.
  //

  Class_ Object_class =
    class_(Object,
	   No_class,
	   append_Features(
           append_Features(
           single_Features(method(cool_abort, nil_Formals(), Object, no_expr())),
           single_Features(method(type_name, nil_Formals(), Str, no_expr()))),
           single_Features(method(::copy, nil_Formals(), SELF_TYPE, no_expr()))),
	   filename);

  //
  // The IO class inherits from Object. Its methods are
  //        out_string(Str) : SELF_TYPE          writes a string to the output
  //        out_int(Int) : SELF_TYPE               "    an int    "  "     "
  //        in_string() : Str                    reads a string from the input
  //        in_int() : Int                         "   an int     "  "     "
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

  //
  // The Int class has no methods and only a single attribute, the
  // "val" for the integer.
  //

  Class_ Int_class =
      class_(Int,
	     Object,
	     single_Features(attr(val, prim_slot, no_expr())),
	     filename);

  //
  // Bool also has only the "val" slot.
  //

  Class_ Bool_class =
      class_(Bool, Object, single_Features(attr(val, prim_slot, no_expr())),filename);

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
  addid(Str, new InheritanceNode(Str_class));
  addid(Bool, new InheritanceNode(Bool_class));
  addid(Int, new InheritanceNode(Int_class));
  addid(IO, new InheritanceNode(IO_class));
  addid(Object, new InheritanceNode(Object_class));

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
//       (line number is extracted from tree_node)
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

void class__class::traverse(ClassTable* table, Class_ cur) {
  // TODO: "a method need not be defined in the class in which it is used, but in some parent class"
  SymbolTable<Symbol, std::map<Symbol, Classes>> methods = SymbolTable<Symbol, std::map<Symbol, Classes>>();
  SymbolTable<Symbol, Class__class> objects = SymbolTable<Symbol, Class__class>();

  for (int i = features->first(); features->more(i); i = features->next(i)) {
    // TODO: pass attribute (objects) table to further recursive calls
    features->nth(i)->traverse(table, methods, objects, cur);
  }
}

// void traverse_feature(Feature_ feature) {
//   Features_ features = cur->get_features();
//   for (int i = features->first(); features->more(i); i = features->next(i)) {
//     return;
//   }
// }

void method_class::traverse(ClassTable* classes, SymbolTable<Symbol, std::map<Symbol, Classes>>& methods, SymbolTable<Symbol, Class__class>& objects, Class_ errClass) {
  // ADD OBJECT NAME TO TABLE
  // ADD METHOD NAME
  // add method to signature
  std::map<Symbol, Classes> methodMap;
  
  // methods.addid(type_name, )

  for (int i = formals->first(); formals->more(i); i = formals->next(i)) {
    formals->nth(i)->traverse(classes, methods, objects, errClass);
  }
}

void attr_class::traverse(ClassTable* classes, SymbolTable<Symbol, std::map<Symbol, Classes>>& methods, SymbolTable<Symbol, Class__class>& objects, Class_ errClass) {
  // ADD OBJECT NAME TO TABLE
  init->traverse(classes, methods, objects, errClass);
  // TODO: CHANGE THIS
}

void formal_class::traverse(ClassTable* classes, SymbolTable<Symbol, std::map<Symbol, Classes>>& methods, SymbolTable<Symbol, Class__class>& objects, Class_ errClass) {
  // TODO:
  return;
}

Symbol branch_class::traverse(ClassTable* classes, SymbolTable<Symbol, std::map<Symbol, Classes>>& methods, SymbolTable<Symbol, Class__class>& objects, Class_ errClass) {
  // ADD OBJECT NAME TO TABLE
  objects.addid(name, classes->lookup(type_decl)->get_class());
  expr->traverse(classes, methods, objects, errClass);
  return expr->get_type();
}

Symbol Expression_class::traverse(ClassTable* classes, SymbolTable<Symbol, std::map<Symbol, Classes>>& methods, SymbolTable<Symbol, Class__class>& objects, Class_ errClass) {
  // TODO: is there really anything to do
  return No_type;
}

Symbol assign_class::traverse(ClassTable* classes, SymbolTable<Symbol, std::map<Symbol, Classes>>& methods, SymbolTable<Symbol, Class__class>& objects, Class_ errClass) {
  expr->traverse(classes, methods, objects, errClass);
  // check if 
  if (objects.lookup(name) == NULL) {
    classes->semant_error(errClass) << ": " << "Identifier does not refer to an object.\n";
    set_type(Object);
    return Object;
  }

  std::set<Class_> xSet;
  xSet.insert(classes->lookup(Object)->get_class());

  // add x's inheritance tree to the set
  Class_ cur = classes->lookup(expr->get_type())->get_class();
  while (cur != classes->lookup(Object)->get_class()) {
    cur = classes->lookup(cur->get_parent())->get_class();
    xSet.insert(cur);
    if (xSet.find(classes->lookup(name)->get_class()) != xSet.end()) {
      set_type(expr->get_type());
      return expr->get_type();
    }
  }
  // AFTER PROCESSING E, REMOVE DEFINITION OF X AND RESTORE OLD DEFINITION OF X

  // throw error
  classes->semant_error(errClass) << ": " << "Expression type does not conform to identifier type.\n";
  set_type(Object);
  return Object;
}

Symbol static_dispatch_class::traverse(ClassTable* classes, SymbolTable<Symbol, std::map<Symbol, Classes>>& methods, SymbolTable<Symbol, Class__class>& objects, Class_ errClass) {
  expr->traverse(classes, methods, objects, errClass);
  std::vector<Symbol> formals;
  for (int i = actual->first(); actual->more(i); i = actual->next(i)) {
    actual->nth(i)->traverse(classes, methods, objects, errClass);
    formals.push_back(actual->nth(i)->get_type());
  }
  // check if T_0 is subtype of T
  bool isInherit = false;
  Class_ cur = classes->lookup(actual->nth(0)->get_type())->get_class();
  std::set<Class_> xSet;
  while (cur != classes->lookup(Object)->get_class()) {
    cur = classes->lookup(cur->get_parent())->get_class();
    xSet.insert(cur);
    if (xSet.find(classes->lookup(type_name)->get_class()) != xSet.end()) {
      isInherit = true;
    }
  }
  if (!isInherit) {
    classes->semant_error(errClass) << ": " << "Expression type does not conform to identifier type.\n";
    set_type(Object);
    return Object;
  }

  // add method to signature
  if (methods.lookup(name) == NULL) {
    classes->semant_error(errClass) << ": " << "Invalid method.\n";
    set_type(Object);
    return Object;
  }
  
  Classes methodFormals = methods.lookup(type_name)->find(name)->second;
  // check if each parameter in dispatch call inherits declared parameter
  // TODO: make sure method table stores parameters AND return types too
  for (size_t i = 0; i < formals.size() - 1; i++) {
    // represents return type, do not check inheritance
    bool isInherit = false;
    Class_ cur = classes->lookup(formals[i])->get_class();
    std::set<Class_> xSet;
    while (cur != classes->lookup(Object)->get_class()) {
      cur = classes->lookup(cur->get_parent())->get_class();
      xSet.insert(cur);
      if (xSet.find(methodFormals->nth(i)) != xSet.end()) {
        isInherit = true;
      }
    }
    if (!isInherit) {
      // throw error, subtyping doesn't exist for a parameter
      classes->semant_error(errClass) << ": " << "Expression type does not conform to identifier type.\n";
      set_type(Object);
      return Object;
    }
  }
  set_type(methods.lookup(type_name)->find(name)->second->nth(formals.size() - 1)->get_name());
  return methods.lookup(type_name)->find(name)->second->nth(formals.size() - 1)->get_name();
}

Symbol dispatch_class::traverse(ClassTable* classes, SymbolTable<Symbol, std::map<Symbol, Classes>>& methods, SymbolTable<Symbol, Class__class>& objects, Class_ errClass) {
  expr->traverse(classes, methods, objects, errClass);
  std::vector<Symbol> formals;
  for (int i = actual->first(); actual->more(i); i = actual->next(i)) {
    actual->nth(i)->traverse(classes, methods, objects, errClass);
    formals.push_back(actual->nth(i)->get_type());
  }
  // check for dispatch caller type
  Symbol caller_type = classes->lookup(self)->get_class()->get_name();
  if (actual->nth(0)->get_type() != SELF_TYPE) {
    caller_type = actual->nth(0)->get_type();
  }

  // add method to signature
  if (methods.lookup(name) == NULL) {
    classes->semant_error(errClass) << ": " << "Invalid method.\n";
    set_type(Object);
    return Object;
  }

  Classes methodFormals = methods.lookup(caller_type)->find(name)->second;
  // check if each parameter in dispatch call inherits declared parameter
  // TODO: make sure method table stores parameters AND return types too
  for (size_t i = 0; i < formals.size() - 1; i++) {
    // represents return type, do not check inheritance
    bool isInherit = false;
    Class_ cur = classes->lookup(formals[i])->get_class();
    std::set<Class_> xSet;
    while (cur != classes->lookup(Object)->get_class()) {
      cur = classes->lookup(cur->get_parent())->get_class();
      xSet.insert(cur);
      if (xSet.find(methodFormals->nth(i)) != xSet.end()) {
        isInherit = true;
      }
    }
    if (!isInherit) {
      // throw error, subtyping doesn't exist for a parameter
      classes->semant_error(errClass) << ": " << "Expression type does not conform to identifier type.\n";
      set_type(Object);
      return Object;
    }
  }

  set_type(methods.lookup(caller_type)->find(name)->second->nth(formals.size() - 1)->get_name());
  return methods.lookup(caller_type)->find(name)->second->nth(formals.size() - 1)->get_name();
}

Symbol cond_class::traverse(ClassTable* classes, SymbolTable<Symbol, std::map<Symbol, Classes>>& methods, SymbolTable<Symbol, Class__class>& objects, Class_ errClass) {
  pred->traverse(classes, methods, objects, errClass);
  then_exp->traverse(classes, methods, objects, errClass);
  else_exp->traverse(classes, methods, objects, errClass);
  if (pred->get_type() != Bool) {
    classes->semant_error(errClass) << ": " << "First expression is not a boolean. \n";
    set_type(Object);
    return Object;
  }
  set_type(lub(classes, classes->lookup(then_exp->get_type())->get_class(), classes->lookup(else_exp->get_type())->get_class())->get_name());
  return lub(classes, classes->lookup(then_exp->get_type())->get_class(), classes->lookup(else_exp->get_type())->get_class())->get_name();
}

Symbol loop_class::traverse(ClassTable* classes, SymbolTable<Symbol, std::map<Symbol, Classes>>& methods, SymbolTable<Symbol, Class__class>& objects, Class_ errClass) {
  pred->traverse(classes, methods, objects, errClass);
  body->traverse(classes, methods, objects, errClass);
  if (pred->get_type() != Bool) {
    classes->semant_error(errClass) << ": " << "First expression is not a boolean.\n";
    set_type(Object);
    return Object;
  }
  set_type(Object);
  return Object;
}

Symbol typcase_class::traverse(ClassTable* classes, SymbolTable<Symbol, std::map<Symbol, Classes>>& methods, SymbolTable<Symbol, Class__class>& objects, Class_ errClass) {
  expr->traverse(classes, methods, objects, errClass);
  std::set<Class_> types;
  Symbol return_type = NULL;
  for (int i = cases->first(); cases->more(i); i = cases->next(i)) {
    objects.enterscope();
    cases->nth(i)->traverse(classes, methods, objects, errClass);
    if (types.find(objects.lookup(cases->nth(i)->get_name())) != types.end()) {
      classes->semant_error(errClass) << ": " << "Variables do not have distinct types.\n";
      set_type(Object);
      return Object;
    } else {
      types.insert(objects.lookup(cases->nth(i)->get_name()));
    }
    if (return_type == NULL) {
      return_type = classes->lookup(cases->nth(i)->get_name())->get_class()->get_name();
    } else {
      return_type = lub(classes, classes->lookup(return_type)->get_class(), objects.lookup(cases->nth(i)->get_name()))->get_name();
    }
    objects.exitscope();
  }
  set_type(return_type);
  return return_type;
}

Symbol block_class::traverse(ClassTable* classes, SymbolTable<Symbol, std::map<Symbol, Classes>>& methods, SymbolTable<Symbol, Class__class>& objects, Class_ errClass) {
  for (int i = body->first(); body->more(i); i = body->next(i)) {
    Expression exp = body->nth(i);
    exp->traverse(classes, methods, objects, errClass);
    if (i == body->len() - 1) {
      set_type(exp->get_type());
      return exp->get_type();
    }
  }
  return Object;
}

Symbol let_class::traverse(ClassTable* classes, SymbolTable<Symbol, std::map<Symbol, Classes>>& methods, SymbolTable<Symbol, Class__class>& objects, Class_ errClass) {
  // ADD OBJECT NAME TO TABLE
  // note: in new scope, match self with type of self in objects table (class)
  init->traverse(classes, methods, objects, errClass);
  body->traverse(classes, methods, objects, errClass);

  objects.enterscope();
  // init's type must inherit x
  Symbol x_type = type_decl;
  if (type_decl == SELF_TYPE) {
    x_type = classes->lookup(self)->get_class()->get_name();
  }
  objects.addid(identifier, classes->lookup(x_type)->get_class());
  std::set<Class_> xSet;
  xSet.insert(classes->lookup(Object)->get_class());

  Class_ cur = classes->lookup(init->get_type())->get_class();
  // IF INIT EXISTS
  if (init->get_type() != No_type) {
    bool isInherit = false;
    while (cur != classes->lookup(Object)->get_class()) {
      cur = classes->lookup(cur->get_parent())->get_class();
      xSet.insert(cur);
      if (xSet.find(classes->lookup(x_type)->get_class()) != xSet.end()) {
        isInherit = true;
      }
    }
    if (!isInherit) {
      classes->semant_error(errClass) << ": " << "Expression type does not conform to identifier type.\n";
      set_type(Object);
      return Object;
    }
  }
  set_type(body->get_type());
  return body->get_type();

}

Symbol plus_class::traverse(ClassTable* classes, SymbolTable<Symbol, std::map<Symbol, Classes>>& methods, SymbolTable<Symbol, Class__class>& objects, Class_ errClass) {
  e1->traverse(classes, methods, objects, errClass);
  e2->traverse(classes, methods, objects, errClass);
  if (e1->get_type() != Int || e2->get_type() != Int) {
    // throw error
    classes->semant_error(errClass) << ": " << "One or both expressions is not an integer.\n";
    set_type(Object);
    return Object;
  }
  set_type(Int);
  return Int;
}

Symbol sub_class::traverse(ClassTable* classes, SymbolTable<Symbol, std::map<Symbol, Classes>>& methods, SymbolTable<Symbol, Class__class>& objects, Class_ errClass) {
  e1->traverse(classes, methods, objects, errClass);
  e2->traverse(classes, methods, objects, errClass);
  if (e1->get_type() != Int || e2->get_type() != Int) {
    classes->semant_error(errClass) << ": " << "One or both expressions is not an integer.\n";
    set_type(Object);
    return Object;
  }
  set_type(Int);
  return Int;
}

Symbol mul_class::traverse(ClassTable* classes, SymbolTable<Symbol, std::map<Symbol, Classes>>& methods, SymbolTable<Symbol, Class__class>& objects, Class_ errClass) {
  e1->traverse(classes, methods, objects, errClass);
  e2->traverse(classes, methods, objects, errClass);
  if (e1->get_type() != Int || e2->get_type() != Int) {
    classes->semant_error(errClass) << ": " << "One or both expressions is not an integer.\n";
    set_type(Object);
    return Object;
  }
  set_type(Int);
  return Int;
}

Symbol divide_class::traverse(ClassTable* classes, SymbolTable<Symbol, std::map<Symbol, Classes>>& methods, SymbolTable<Symbol, Class__class>& objects, Class_ errClass) {
  e1->traverse(classes, methods, objects, errClass);
  e2->traverse(classes, methods, objects, errClass);
  if (e1->get_type() != Int || e2->get_type() != Int) {
    classes->semant_error(errClass) << ": " << "One or both expressions is not an integer.\n";
    set_type(Object);
    return Object;
  }
  set_type(Int);
  return Int;
}

Symbol neg_class::traverse(ClassTable* classes, SymbolTable<Symbol, std::map<Symbol, Classes>>& methods, SymbolTable<Symbol, Class__class>& objects, Class_ errClass) {
  e1->traverse(classes, methods, objects, errClass);
  if (e1->get_type() != Int) {
    classes->semant_error(errClass) << ": " << "Expression is not an integer.\n";
    set_type(Object);
    return Object;
  }
  set_type(Int);
  return Int;
}

Symbol lt_class::traverse(ClassTable* classes, SymbolTable<Symbol, std::map<Symbol, Classes>>& methods, SymbolTable<Symbol, Class__class>& objects, Class_ errClass) {
  e1->traverse(classes, methods, objects, errClass);
  e2->traverse(classes, methods, objects, errClass);
  if (e1->get_type() != Int || e2->get_type() != Int) {
    classes->semant_error(errClass) << ": " << "One or both expressions is not an integer.\n";
    set_type(Object);
    return Object;
  }
  set_type(Bool);
  return Bool;
}

Symbol eq_class::traverse(ClassTable* classes, SymbolTable<Symbol, std::map<Symbol, Classes>>& methods, SymbolTable<Symbol, Class__class>& objects, Class_ errClass) {
  e1->traverse(classes, methods, objects, errClass);
  e2->traverse(classes, methods, objects, errClass);
  if ( (e1->get_type() == Int && e2->get_type() != Int) && 
    (e1->get_type() == Bool && e2->get_type() != Bool) && 
    (e1->get_type() == Str && e2->get_type() != Str) && 
    (e2->get_type() == Int && e1->get_type() != Int) && 
    (e2->get_type() == Bool && e1->get_type() != Bool) && 
    (e2->get_type() == Str && e1->get_type() != Str)) {
      classes->semant_error(errClass) << ": " << "Expressions do not match each other.\n";
      set_type(Object);
      return Object;
    }
  set_type(Bool);
  return Bool;
}

Symbol leq_class::traverse(ClassTable* classes, SymbolTable<Symbol, std::map<Symbol, Classes>>& methods, SymbolTable<Symbol, Class__class>& objects, Class_ errClass) {
  e1->traverse(classes, methods, objects, errClass);
  e2->traverse(classes, methods, objects, errClass);
  if (e1->get_type() != Int || e2->get_type() != Int) {
    classes->semant_error(errClass) << ": " << "One or both expressions is not an integer.\n";
    set_type(Object);
    return Object;
  }
  set_type(Bool);
  return Bool;
}

Symbol comp_class::traverse(ClassTable* classes, SymbolTable<Symbol, std::map<Symbol, Classes>>& methods, SymbolTable<Symbol, Class__class>& objects, Class_ errClass) {
  e1->traverse(classes, methods, objects, errClass);
  if (e1->get_type() != Bool) {
    classes->semant_error(errClass) << ": " << "Expression is not an integer.\n";
    set_type(Object);
    return Object;
  }
  set_type(Bool);
  return Bool;
}

Symbol int_const_class::traverse(ClassTable* classes, SymbolTable<Symbol, std::map<Symbol, Classes>>& methods, SymbolTable<Symbol, Class__class>& objects, Class_ errClass) {
  set_type(Int);
  return Int;
}

Symbol bool_const_class::traverse(ClassTable* classes, SymbolTable<Symbol, std::map<Symbol, Classes>>& methods, SymbolTable<Symbol, Class__class>& objects, Class_ errClass) {
  set_type(Bool);
  return Bool;
}

Symbol string_const_class::traverse(ClassTable* classes, SymbolTable<Symbol, std::map<Symbol, Classes>>& methods, SymbolTable<Symbol, Class__class>& objects, Class_ errClass) {
  set_type(Str);
  return Str;
}

Symbol new__class::traverse(ClassTable* classes, SymbolTable<Symbol, std::map<Symbol, Classes>>& methods, SymbolTable<Symbol, Class__class>& objects, Class_ errClass) {
  if (type_name == SELF_TYPE) {
    set_type(classes->lookup(self)->get_class()->get_name());
    return classes->lookup(self)->get_class()->get_name();
  }
  set_type(type_name);
  return type_name;
}

Symbol isvoid_class::traverse(ClassTable* classes, SymbolTable<Symbol, std::map<Symbol, Classes>>& methods, SymbolTable<Symbol, Class__class>& objects, Class_ errClass) {
  set_type(Bool);
  return Bool;
}

Symbol no_expr_class::traverse(ClassTable* classes, SymbolTable<Symbol, std::map<Symbol, Classes>>& methods, SymbolTable<Symbol, Class__class>& objects, Class_ errClass) {
  set_type(No_type);
  return No_type;
}


Symbol object_class::traverse(ClassTable* classes, SymbolTable<Symbol, std::map<Symbol, Classes>>& methods, SymbolTable<Symbol, Class__class>& objects, Class_ errClass) {
  if (objects.lookup(name) == NULL) {
    classes->semant_error(errClass) << ": " << "Identifier does not refer to object.\n";
    set_type(Object);
    return Object;
  }
  set_type(name);
  return name;
}


/*
 * This is the entry point to the semantic checker.
 *
 * Your checker should do the following two things:
 *
 *   1) Check that the program is semantically correct
 *   2) Decorate the abstract syntax tree with type information
 *      by setting the `type' field in each Expression node.
 *      (see `tree.h')
 *
 *   You are free to first do 1), make sure you catch all semantic
 *   errors. Part 2) can be done in a second stage, when you want
 *   to build mycoolc.
 */
void program_class::semant() {
  initialize_constants();

    /* ClassTable constructor may do some semantic analysis */

  ClassTableP classtable = new ClassTable(classes);

  if (classtable->errors()) {
    cerr << "Compilation halted due to static semantic errors." << endl;
    exit(1);
  }

  // use dfs/bfs to go through each class + have info that we need (ex: attributes)
  // traverse through the AST of each class and do the type checking
  // type checking : assign types to each node of the AST
  // figure out the type of the expression/feature in the AST
  // 
  for (int i = classes->first(); classes->more(i); i = classes->next(i)) {
    classes->nth(i)->traverse(classtable, classes->nth(i));
  }

}
