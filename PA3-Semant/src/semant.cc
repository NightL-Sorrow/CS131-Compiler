#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include "semant.h"
#include "utilities.h"
#include "symtab.h"
#include "cool-tree.h"

SymbolTable<Symbol ,Class__class> ctable;
SymbolTable<Symbol ,Entry> etable;
SymbolTable<Symbol ,Entry> mtable;
Class__class *curr;

extern int semant_debug;
extern char *curr_filename;

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



ClassTable::ClassTable(Classes classes) : semant_errors(0) , error_stream(cerr) {
    /* Fill this in */
    install_basic_classes();
}

void ClassTable::install_basic_classes() {

    // The tree package uses these globals to annotate the classes built below.
   // curr_lineno  = 0;
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
    //        abort() : Object    aborts the program
    //        type_name() : Str   returns a string representation of class name
    //        copy() : SELF_TYPE  returns a copy of the object
    //
    // There is no need for method bodies in the basic classes---these
    // are already built in to the runtime system.

    Class_ Object_class =
	class_(Object, 
	       No_class,
	       append_Features(
			       append_Features(
					       single_Features(method(cool_abort, nil_Formals(), Object, no_expr())),
					       single_Features(method(type_name, nil_Formals(), Str, no_expr()))),
			       single_Features(method(copy, nil_Formals(), SELF_TYPE, no_expr()))),
	       filename);

    // 
    // The IO class inherits from Object. Its methods are
    //        out_string(Str) : SELF_TYPE       writes a string to the output
    //        out_int(Int) : SELF_TYPE            "    an int    "  "     "
    //        in_string() : Str                 reads a string from the input
    //        in_int() : Int                      "   an int     "  "     "
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

    //1 add basic class from cool definition
    ctable.addid(Object,Object_class);
    ctable.addid(Str,Str_class);
    ctable.addid(IO,IO_class);
    ctable.addid(Bool,Bool_class);
    ctable.addid(Int,Int_class);
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
//
///////////////////////////////////////////////////////////////////

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

Symbol ClassTable::get_parent(const Symbol& symbol) {
    if(symbol == Object) return No_class;
    if(graph.count(symbol)==0) return Object;
    else return graph[symbol];
}

void ClassTable::check_halt(){
    if (errors()) {
        cerr << "Compilation halted due to static semantic errors." << endl;
        exit(1);
    }
}

Symbol ClassTable::least_upper(Symbol a, Symbol b)
{
    Symbol a_type = a, b_type = b;
    std::set<Symbol> found;
    if (a == SELF_TYPE)
        a_type = curr->get_name();
    if (b == SELF_TYPE)
        b_type = curr->get_name();

    while (a_type != No_class && b_type!= No_class)
    {
        if (found.count(a_type))
            return a_type;
        found.insert(a_type);
        a_type = graph[a_type];
        if (found.count(b_type))
            return b_type;
        found.insert(b_type);
        b_type = graph[b_type];
    }
    return No_class;
}

//true for Type a <= Type b
bool ClassTable::cmp_type(Symbol a,Symbol b){
    Symbol a_type = a, b_type = b;
    //cerr << a << " " << b << endl;
    if (a == SELF_TYPE && b == SELF_TYPE)
        return true;
    if (a != SELF_TYPE && b == SELF_TYPE)
        return false;
    if (a == SELF_TYPE)
        a_type = curr->get_name();

    while (a_type != Object){
        if (a_type == b_type) return true;
        a_type = graph[a_type];
    }

    return a_type==b_type;
}

/*   This is the entry point to the semantic checker.

     Your checker should do the following two things:

     1) Check that the program is semantically correct
     2) Decorate the abstract syntax tree with type information
        by setting the `type' field in each Expression node.
        (see `tree.h')

     You are free to first do 1), make sure you catch all semantic
     errors. Part 2) can be done in a second stage, when you want
     to build mycoolc.
 */
void ClassTable::add_edge(const Symbol& a,const Symbol& b){
  if(graph.count(a)){
    throw "error";
  }
//Object parent No_class
//   printf("parent%s\n",b->get_string());
  graph[a]=b;
}

void ClassTable::topo_sort(){
  //check for inheritance cycle
  bool last;
  Symbol a, b;
    for(std::map<Symbol,Symbol>::reverse_iterator i=graph.rbegin();i!=graph.rend();i++){
        a = i->first;
        const char* name = a->get_string();
        b = i->second;
        last = false;
        for( ; graph.count(b)!=0 ; b=graph[b]){
            if(a==b || b==graph[b]){
                last = true;
                semant_error(ctable.lookup(a)->get_filename(), ctable.lookup(a)) << "Class " << name <<\
                ", or an ancestor of " << name << ", is involved in an inheritance cycle." <<endl;
                break;
            }
        }
        if(!last){
            if(a==b){
                semant_error(ctable.lookup(a)->get_filename(), ctable.lookup(a)) << "Class " << name <<\
                ", or an ancestor of " << name << ", is involved in an inheritance cycle." <<endl;
                break;
            }
        }
    }
    //check halt
    check_halt();
}

void program_class::check_inherit(ClassTable &g){
    //add edge of classes
    g.add_edge(IO,Object);
    g.add_edge(Int,Object);
    g.add_edge(Str,Object);
    g.add_edge(Bool,Object);

    bool main_exsit = false;
    for (int i = classes->first(); classes->more(i); i = classes->next(i)){
        curr = classes->nth(i);
        const char* name = curr->get_name()->get_string();
        Symbol parent = curr->get_parent();
        //check for main exsitance
        if( !strcmp(name,"Main") )
            main_exsit = true;

        if( !strcmp(name,"SELF_TYPE") || !strcmp(name,"Int")\
            ||!strcmp(name,"String") || !strcmp(name,"Bool")\
            ||!strcmp(name,"Object") || !strcmp(name,"IO")
        ){
            g.semant_error(curr->get_filename(), curr) << "Redefinition of basic class " << name << "." << endl;
            continue;
        }

        //add edges; add ctable for classes;
        try{
            //cerr << curr->get_name() << endl;
            g.add_edge(curr->get_name(), curr->get_parent());
            ctable.addid(curr->get_name() ,curr);
        }catch(const char* msg){
            g.semant_error(curr->get_filename(), curr) << "Class " << name << " was previously defined." << endl;
        }
    }
    
    //check halt
    g.check_halt();

    //check for inherits nondefinition
    for (int i = classes->first(); classes->more(i); i = classes->next(i)){
        curr = classes->nth(i);
        Symbol name = curr->get_name();
        Symbol parent = curr->get_parent();

        //inherit from SELF_TYPE
        if(parent == SELF_TYPE || parent == Bool || parent == Str || parent == Int){
            g.semant_error(curr->get_filename(), curr) << "Class " << name << " cannot inherit class "<< parent << "." << endl;
            continue;
        }

        //parent doesn't exsit
        if(curr->get_parent() != Object && !ctable.lookup(parent)){
            g.semant_error(curr->get_filename(), curr) << "Class " << name << \
            " inherits from an undefined class "<< parent << "." << endl;
            continue;
        }
    }

    //halt
    g.check_halt();
    //inheritance cycle
    g.topo_sort();
    //check for main class
    if(!main_exsit){
       g.semant_error() << "Class Main is not defined." << endl;
       g.check_halt();
    } 
}

void program_class::semant()
{
    initialize_constants();
    ctable.enterscope();
    /* ClassTable constructor may do some semantic analysis */
    ClassTable classtable (classes);
    /* some semantic analysis code may go here */
    try{
        check_inherit(classtable);
        check_type(classtable);
    }
    catch(const char* msg){
        classtable.semant_error(curr->get_filename(), this) << msg << endl;
    }
    //halt
    classtable.check_halt();
}

void program_class::check_type(ClassTable &g)
{
    for (int i = classes->first(); classes->more(i); i = classes->next(i))
    {
        curr = classes->nth(i);
        etable.enterscope();
        curr->check_type(g);
        etable.exitscope();
    }
}

Feature class__class::find_attr(const char *attr)
{
    for (int i = features->first(); features->more(i); i = features->next(i))
    {
        Feature feature = features->nth(i);
        if (feature->equal_attr(attr))
            return feature;
    }
    return NULL;
}

Feature class__class::find_method(const char *method)
{
    for (int i = features->first(); features->more(i); i = features->next(i))
    {
        Feature feature = features->nth(i);
        if (feature->equal_method(method))
            return feature;
    }
    return NULL;
}

void class__class::check_type(ClassTable &g)
{
  mtable.enterscope();
  for (int i = features->first(); features->more(i); i = features->next(i))
  {
      Feature feature = features->nth(i);
      feature->check_type(g);
  }
  if(curr->get_name() == Main && find_method("main") == NULL ){
      g.semant_error(curr->get_filename(), this)<< "No 'main' method in class Main."<<endl;
  }
  mtable.exitscope();
  g.check_halt();
}

void attr_class::check_type(ClassTable &g)
{   //cerr << name << endl;

    if (type_decl != SELF_TYPE && !ctable.lookup(type_decl))
        g.semant_error(curr->get_filename(), this)<< "Class " << type_decl << " of attribute "<< name << " is undefined."<<endl;
    if (name->equal_string("self", 4))
        g.semant_error(curr->get_filename(), this)<< "\'self\' cannot be the name of an attribute." <<endl;

    Class_ cls;
    Symbol parent = curr->get_parent();

    while (parent != No_class)
    {
        //cout << parent->get_string() << endl;
        cls = ctable.lookup(parent);
        if (cls->find_attr(name->get_string())){
            g.semant_error(curr->get_filename(), this)<< "Attribute " << name << \
            " is an attribute of an inherited class." <<endl;
        }
        parent = cls->get_parent();
    }

    init->check_type(g);

    if (init->get_type() != No_type && !g.cmp_type(init->get_type(), type_decl))
        g.semant_error(curr->get_filename(), this)<< "Inferred type " << init->get_type() << \
        " of initialization of attribute " << name << \
        " does not conform to declared type " << type_decl <<"."<<endl;

    g.check_halt();
    etable.addid(name, type_decl);
}

void method_class::check_type(ClassTable &g)
{
    etable.enterscope();
    //cerr << name << "()" << endl;
    Class_ cls;
    Feature method = NULL;
    Symbol parent = curr->get_parent();

    if (mtable.lookup(name))
        g.semant_error(curr->get_filename(), this)<< "Method "<< name <<" is multiply defined." << endl;
    while (parent != No_class)
    {
        cls = ctable.lookup(parent);
        if (method = cls->find_method(name->get_string()))
            break;
        parent = cls->get_parent();
    }

    if (method)
    {
        if (formals->len() != method->get_formals()->len())
            g.semant_error(curr->get_filename(), this)<< "Incompatible number of formal parameters in redefined method "<< \
            method->get_method_name() << "." <<endl;
        int length = std::min(method->get_formals()->len(), formals->len());
        for (int i = 0; i < length; i++)
        {
            Formal formal = formals->nth(i);
            Formal parent_formal = method->get_formals()->nth(i);
            if (formal->get_type_decl() != parent_formal->get_type_decl())
                //std forgets a \. here at the end of error msg
                g.semant_error(curr->get_filename(), this)<< "In redefined method "<< method->get_method_name()<<\
                ", parameter type "<< formal->get_type_decl()<< " is different from original type " <<\
                parent_formal->get_type_decl() << endl;
            formal->check_type(g);
        }
        if (method->get_return_type() != return_type)
            g.semant_error(curr->get_filename(), this)<<"In redefined method "<<method->get_method_name()<<\
            ", return type "<<return_type<<" is different from original return type "<<method->get_return_type()<<"."<<endl;
    }
    else
        for (int i = formals->first(); formals->more(i); i = formals->next(i))
            formals->nth(i)->check_type(g);
        
    expr->check_type(g);

    if (return_type != SELF_TYPE && !ctable.lookup(return_type))
        g.semant_error(curr->get_filename(), this)<< "Undefined return type " << return_type <<\
        " in method "<< get_method_name() << "." <<endl;

    if (!g.cmp_type(expr->get_type(), return_type))
        g.semant_error(curr->get_filename(), this)<< "Inferred return type "<< expr->get_type() <<\
        " of method "<< name << " does not conform to declared return type "<<\
        return_type << "." << endl;

    g.check_halt();
    mtable.addid(name,return_type);
    etable.exitscope();
}

void formal_class::check_type(ClassTable &g)
{
    if (etable.probe(name))
        g.semant_error(curr->get_filename(), this) << "Formal parameter " << name << " is multiply defined." <<endl;
    if (name == self)
        g.semant_error(curr->get_filename(), this) << "\'self\' cannot be the name of a formal parameter." <<endl;
    if (type_decl == SELF_TYPE)
        g.semant_error(curr->get_filename(), this) << "Formal parameter " << name << " cannot have type SELF_TYPE." <<endl;
    
    // g.check_halt();
    // if(!g.errors())
    etable.addid(name, type_decl);
}

void branch_class::check_type(ClassTable &g)
{
    etable.addid(name,type_decl);
    expr->check_type(g);
}

void assign_class::check_type(ClassTable &g)
{
    expr->check_type(g);
    Symbol expr_type = expr->get_type();
    if (name == self)
        g.semant_error(curr->get_filename(), this) << "Cannot assign to \'self\'." << endl;

    Symbol id_type = name == self ? SELF_TYPE :etable.lookup(name);
    if (!id_type)
    {
        Class_ cls;
        Symbol parent = curr->get_name();
        id_type = NULL;

        do {
            cls = ctable.lookup(parent);
            Feature attr = cls->find_attr(name->get_string());
            if (attr)
            {
                id_type = attr->get_attr_type();
                break;
            }
            parent = cls->get_parent();
        } while (parent != No_class);
    }
    if (!id_type)
        g.semant_error(curr->get_filename(), this) << "Assignment to undeclared variable "
        << name << "." << endl;

    if (id_type)
    {
        Symbol lvalue_type = id_type;
        //cerr << expr_type << " ||| " << lvalue_type << endl;
        if (!lvalue_type || !g.cmp_type(expr_type, lvalue_type))
            g.semant_error(curr->get_filename(), this)<< "Type " << expr_type << \
            " of assigned expression does not conform to declared type " << lvalue_type
            <<" of identifier " << name  << "." <<endl;
    }
    type = expr_type;
}

//we check all the dispatch errors together and do not exsit
void static_dispatch_class::check_type(ClassTable &g)
{
    expr->check_type(g);
    Symbol expr_type = expr->get_type();
    Symbol parent;

    if(!ctable.lookup(type_name))
    {
        g.semant_error(curr->get_filename(), this)<< "Static dispatch to undefined class " << type_name << "."<<endl;
        parent = Object;
        type = Object;
        return;
    }
    else if (!g.cmp_type(expr_type, type_name))
    {
        g.semant_error(curr->get_filename(), this)<< "Expression type " << expr_type <<\
        " does not conform to declared static dispatch type " << type_name << "." <<endl;
        parent = type_name;
    }

//    g.check_halt();
    Feature method = NULL;

    do {
        Class_ cls = ctable.lookup(parent);
        method = cls->find_method(name->get_string());
        if (method)
            break;
        parent = cls->get_parent();
    } while (parent != No_class);

    if (!method)
        g.semant_error(curr->get_filename(), this)<< "Static dispatch to undefined method "<<\
        name << "." <<endl;
    else
    {
        Formals formals = method->get_formals();
        if (method->get_return_type() == SELF_TYPE)
            type = expr_type;
        else
            type = method->get_return_type();

        if (actual->len() != formals->len())
            g.semant_error(curr->get_filename(), this)<< "Method " << method->get_method_name() <<\
        " invoked with wrong number of arguments."<< endl;

        for (int i = actual->first(); actual->more(i); i = actual->next(i)) {
            Expression act = actual->nth(i);
            act->check_type(g);
            Symbol nth_formal_type = method->get_formals()->nth(i)->get_type_decl();

            if (!g.cmp_type(act->get_type(), nth_formal_type))
                g.semant_error(curr->get_filename(), this)<< "In call of method " << method->get_method_name()<<\
                    ", type "<< act->get_type() << " of parameter " <<
                    method->get_formals()->nth(i)->get_formal_name()<<\
                    " does not conform to declared type "<< nth_formal_type<< "." << endl;
        }
    }
}

void dispatch_class::check_type(ClassTable &g)
{
    expr->check_type(g);
    Symbol expr_type = expr->get_type();
//    if(expr == self)
    Symbol parent = expr_type == SELF_TYPE? curr->get_name(): expr_type;
    Feature method = NULL;

    do {
        Class_ cls = ctable.lookup(parent);
        method = cls->find_method(name->get_string());
        if (method)
            break;
        parent = cls->get_parent();
    } while (parent != No_class);

    if (!method)
    {
        g.semant_error(curr->get_filename(), this)<< "Dispatch to undefined method "<< name << "." <<endl;
        type = Object;
    }
    else
    {
        Formals formals = method->get_formals();
        Symbol method_return_type = method->get_return_type();
        type = method_return_type == SELF_TYPE? expr_type : method_return_type;

        if (actual->len() != formals->len())
            g.semant_error(curr->get_filename(), this)<< "Method " << method->get_method_name() <<\
            " called with wrong number of arguments."<< endl;

        for (int i = actual->first(); actual->more(i); i = actual->next(i)) {
            Expression act = actual->nth(i);
            act->check_type(g);
            Symbol nth_formal_type = method->get_formals()->nth(i)->get_type_decl();

            if (!g.cmp_type(act->get_type(), nth_formal_type))
                g.semant_error(curr->get_filename(), this)<< "In call of method " << method->get_method_name()<<\
                ", type "<< act->get_type() << " of parameter "\
                << method->get_formals()->nth(i)->get_formal_name() <<\
                " does not conform to declared type "<< nth_formal_type << "." << endl;
        }
    }

    // g.check_halt();
}

void cond_class::check_type(ClassTable &g)
{
    pred->check_type(g);
    if (pred->get_type() != Bool)
        g.semant_error(curr->get_filename(), this)<< "Predicate of 'if' does not have type Bool." <<endl;

    then_exp->check_type(g);
    else_exp->check_type(g);
    type = g.least_upper(then_exp->get_type(), else_exp->get_type());
}

void loop_class::check_type(ClassTable &g)
{
    pred->check_type(g);
    if (pred->get_type() != Bool)
        g.semant_error(curr->get_filename(), this)<< "Loop condition does not have type Bool." <<endl;

    body->check_type(g);
    type = Object;
}

void typcase_class::check_type(ClassTable &g)
{
    expr->check_type(g);

    std::set<Symbol> caseset;
    Symbol cases_type = NULL;

    for (int i = cases->first(); cases->more(i); i = cases->next(i))
    {
        etable.enterscope();
        Case c = cases->nth(i);

        c->check_type(g);
        if (caseset.count(c->get_type()))
            g.semant_error(curr->get_filename(), this)<< "Duplicate branch "
            << c->get_type() << " in case statement." <<endl;
        caseset.insert(c->get_type());
        cases_type = !cases_type?
                c->get_expr_type() : g.least_upper(cases_type, c->get_expr_type());
        etable.exitscope();
    }

    type = cases_type;
}

void block_class::check_type(ClassTable &g)
{
    Expression e;
    for (int i = body->first(); body->more(i); i = body->next(i)) {
        e = body->nth(i);
        e->check_type(g);
    }
    type = e->get_type();
}

void let_class::check_type(ClassTable &g)
{ //cerr << identifier << endl;
    if (identifier == self)
        g.semant_error(curr->get_filename(), this)<<"'self' cannot be bound in a 'let' expression."<<endl;
    init->check_type(g);
    if (init->get_type() != No_type && !g.cmp_type(init->get_type(), type_decl))
        g.semant_error(curr->get_filename(), this)<< "Inferred type " << init->get_type()  << \
        " of initialization of " << identifier  << \
        " does not conform to identifier's declared type " << type_decl <<"."<<endl;
    etable.enterscope();
    etable.addid(identifier, type_decl);
    //etable.dump();
    body->check_type(g);
    type = body->get_type();
    etable.exitscope();
    //etable.dump();
}

void plus_class::check_type(ClassTable &g)
{
    e1->check_type(g);
    e2->check_type(g);
    if (e1->get_type() != Int || e2->get_type() != Int)
        g.semant_error(curr->get_filename(), this)<< "non-Int arguments: " <<e1->get_type() <<" + "<<e2->get_type() <<endl;
    type = Int;
}

void sub_class::check_type(ClassTable &g)
{
    e1->check_type(g);
    e2->check_type(g);
    if (e1->get_type() != Int || e2->get_type() != Int)
        g.semant_error(curr->get_filename(), this)<< "non-Int arguments: " <<e1->get_type() <<" - "<<e2->get_type() <<endl;
    type = Int;
}

void mul_class::check_type(ClassTable &g)
{
    e1->check_type(g);
    e2->check_type(g);
    if (e1->get_type() != Int || e2->get_type() != Int)
        g.semant_error(curr->get_filename(), this)<< "non-Int arguments: " <<e1->get_type() <<" * "<<e2->get_type() <<endl;
    type = Int;
}

void divide_class::check_type(ClassTable &g)
{
    e1->check_type(g);
    e2->check_type(g);
    if (e1->get_type() != Int || e2->get_type() != Int)
        g.semant_error(curr->get_filename(), this)<< "non-Int arguments: " << e1->get_type() <<" / "<<e2->get_type() <<endl;
    type = Int;
}

void neg_class::check_type(ClassTable &g)
{
    e1->check_type(g);
    if (e1->get_type() != Int)
        g.semant_error(curr->get_filename(), this)<< "Argument of '~' haas type " << e1->get_type() <<" instead of Int."<<endl;
    type = Int;
}

void lt_class::check_type(ClassTable &g){
    e1->check_type(g);
    e2->check_type(g);
    if (e1->get_type() != Int || e2->get_type() != Int){
        g.semant_error(curr->get_filename(), this)<< "non-Int arguments: " << e1->get_type() << " < " << e2->get_type() << endl;
    }
    type = Bool;

    // g.check_halt();
}

void eq_class::check_type(ClassTable &g){
    e1->check_type(g);
    e2->check_type(g);
    if (e1->get_type() == Int || e1->get_type() == Bool || e1->get_type() == Str){
        if(e1->get_type() != e2->get_type()){
            g.semant_error(curr->get_filename(), this)<< "Illegal comparison with a basic type." << endl;
        }else type = Bool;
    }
    type = Bool;

    // g.check_halt();
}

void leq_class::check_type(ClassTable &g){
    e1->check_type(g);
    e2->check_type(g);
    if (e1->get_type() != Int || e2->get_type() != Int) {
        g.semant_error(curr->get_filename(), this) << "non-Int arguments: " << e1->get_type() << " <= "
                                                   << e2->get_type() << endl;
    }
    type = Bool;

    // g.check_halt();
}

void comp_class::check_type(ClassTable &g){
    e1->check_type(g);
    if (e1->get_type() != Bool){
        g.semant_error(curr->get_filename(), this)<< "Argument of 'not' has type " << e1->get_type() << " instead of Bool." <<endl;
    }
    type = Bool;

    // g.check_halt();
}

void int_const_class::check_type(ClassTable &g){
    type = Int;
}

void bool_const_class::check_type(ClassTable &g){
    type = Bool;
}

void string_const_class::check_type(ClassTable &g){
    type = Str;
}

void new__class::check_type(ClassTable &g){
    if (type_name != SELF_TYPE && !ctable.lookup(type_name))
        g.semant_error(curr->get_filename(), this) << "\'new\' used with undefined class "
        << type_name << "." << endl;
    type = type_name;
}

void isvoid_class::check_type(ClassTable &g){
    e1->check_type(g);
    type = Bool;
}

void no_expr_class::check_type(ClassTable &g){
    type = No_type;
}

void object_class::check_type(ClassTable &g){ //cerr << name << endl;
    if (name == self) {
        type = SELF_TYPE;
        return;
    }
    type = etable.lookup(name);

    if (!type)
    {
        Class_ cls;
        Symbol parent = curr->get_name();
        type = NULL;

        do {
            cls = ctable.lookup(parent);
            Feature attr = cls->find_attr(name->get_string());
            if (attr)
            {
                type = attr->get_attr_type();
                break;
            }
                    parent = cls->get_parent();
        } while (parent != No_class);
    }

    if (!type)
    {
        g.semant_error(curr->get_filename(), this)<< "Undeclared identifier "<< name << "." << endl;
        type = Object;
    }
}
