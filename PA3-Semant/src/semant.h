#ifndef SEMANT_H_
#define SEMANT_H_

#include <assert.h>
#include <iostream> 
#include <map>
#include <set>
#include "cool-tree.h"
#include "stringtab.h"
#include "symtab.h"
#include "list.h"

#define TRUE 1
#define FALSE 0

class ClassTable;
typedef ClassTable *ClassTableP;

// This is a structure that may be used to contain the semantic
// information such as the inheritance graph.  You may use it or not as
// you like: it is only here to provide a container for the supplied
// methods.
class ClassTable {
private:
  int semant_errors;
  void install_basic_classes();
  ostream& error_stream;

  std::map<Symbol,Symbol> graph;

public:
  explicit ClassTable(Classes);
  int errors() { return semant_errors; }
  ostream& semant_error();
  ostream& semant_error(Symbol filename, tree_node *t);

  void add_edge(const Symbol& a,const Symbol& b);//a inherits b
  void topo_sort();
  bool cmp_type(Symbol a,Symbol b);//compair type a<=b
  Symbol least_upper(Symbol a,Symbol b); // return the least upper bound of a and b;
  void check_halt();

  Symbol get_parent(const Symbol& symbol);
};

#endif
