#include <assert.h>
#include <stdio.h>
#include "emit.h"
#include "cool-tree.h"
#include "symtab.h"
#include <map>
#include <vector>
#include <deque>
#include <set>
#include <stack>

enum Basicness     {Basic, NotBasic};
#define TRUE 1
#define FALSE 0

class CgenClassTable;
typedef CgenClassTable *CgenClassTableP;

class CgenNode;
typedef CgenNode *CgenNodeP;

std::map<int, CgenNodeP> tag_to_class;
std::map<Symbol, CgenNodeP> symbol_to_class;

int _label_=0;
int get_label(){
   return _label_++;
}

CgenNodeP curr_class;
SymbolTable<Symbol, int> identifiers;
int offset_fp = 0;
/* int represent the offset of the id of  formals */

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
   void code_name_tab();
   void code_objTab();
   void code_dispTab();
   void code_prototype();
   void code_object_init();
   void code_class_method();

// The following creates an inheritance graph from
// a list of classes.  The graph is implemented as
// a tree of `CgenNode', and class names are placed
// in the base class symbol table.

   void install_basic_classes();
   void install_class(CgenNodeP nd);
   void install_classes(Classes cs);
   void build_inheritance_tree();
   void allocate_tag();
   void set_relations(CgenNodeP nd);
public:
   CgenClassTable(Classes, ostream& str);
   void code();
   CgenNodeP root();
};


class CgenNode : public class__class {
private: 
   CgenNodeP parentnd;                        // Parent of class
   List<CgenNode> *children;                  // Children of class
   Basicness basic_status;                    // `Basic' if class is basic
                                              // `NotBasic' otherwise
   std::vector<Feature> attr_list;
   std::vector<Feature> method_list;
   int tag;                                   // Tag number of the class
   int size;                                  // Size of the class
   int max_child;

public:
   CgenNode(Class_ c,
            Basicness bstatus,
            CgenClassTableP class_table);

   void add_child(CgenNodeP child);
   List<CgenNode> *get_children() { return children; }
   void set_parentnd(CgenNodeP p);
   CgenNodeP get_parentnd() { return parentnd; }
   int basic() { return (basic_status == Basic); }
   void calc_size();
   void set_tag(int tag_) { tag = tag_; max_child = tag; }
   int get_tag() { return tag; }
   int get_size() { return size; }
   void code_prototype_ref(ostream &s)
   { s << name << PROTOBJ_SUFFIX; }
   void code_prototype_def(ostream &s);
   void code_dispTab(ostream &s);
   void code_init_ref(ostream &s);
   void code_init_def(ostream &s);
   void code_methods(ostream &s, std::set<Feature> &coded_method);
   void set_max_child(int tag);
   int get_max_child() { return max_child; }
   int get_attr_offset(Symbol attr)
   {
     std::vector<Feature>::iterator it;
     int n = 0;
     for (it = attr_list.begin(); it != attr_list.end(); it++)
     {
       if ((*it)->get_name() == attr)
         return n + 3;
       n++;
     }
     return -1;
   }
   int get_method_offset(Symbol method)
   {
     std::vector<Feature>::iterator it;
     int n = 0;
     for (it = method_list.begin(); it != method_list.end(); it++)
     {
       if ((*it)->get_name() == method)
         return n;
       n++;
     }
     return -1;
   }
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


