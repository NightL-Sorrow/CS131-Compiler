
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
#include "cgen_gc.h"

extern void emit_string_constant(ostream& str, const char *s);
extern int cgen_debug;

//
// Three symbols from the semantic analyzer (semant.cc) are used.
// If e : No_type, then no code is generated for e.
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
Symbol 
       arg,
       arg2,
       Bool,
       concat,
       cool_abort,
       cool_copy,
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
static void initialize_constants()
{
  arg         = idtable.add_string("arg");
  arg2        = idtable.add_string("arg2");
  Bool        = idtable.add_string("Bool");
  concat      = idtable.add_string("concat");
  cool_abort  = idtable.add_string("abort");
  cool_copy   = idtable.add_string("copy");
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
//*********************************************************

void program_class::cgen(ostream &os) 
{
  // spim wants comments to start with '#'
  os << "# start of generated code\n";

  initialize_constants();
  CgenClassTable *codegen_classtable = new CgenClassTable(classes,os);

  os << "\n# end of generated code\n";
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

static void emit_load(const char *dest_reg, int offset, const char *source_reg, ostream& s)
{
  s << LW << dest_reg << " " << offset * WORD_SIZE << "(" << source_reg << ")" 
    << endl;
}

static void emit_store(const char *source_reg, int offset, const char *dest_reg, ostream& s)
{
  s << SW << source_reg << " " << offset * WORD_SIZE << "(" << dest_reg << ")"
      << endl;
}

static void emit_load_imm(const char *dest_reg, int val, ostream& s)
{ s << LI << dest_reg << " " << val << endl; }

static void emit_load_address(const char *dest_reg, const char *address, ostream& s)
{ s << LA << dest_reg << " " << address << endl; }

static void emit_partial_load_address(const char *dest_reg, ostream& s)
{ s << LA << dest_reg << " "; }

static void emit_load_bool(const char *dest, const BoolConst& b, ostream& s)
{
  emit_partial_load_address(dest,s);
  b.code_ref(s);
  s << endl;
}

static void emit_load_string(const char *dest, StringEntry *str, ostream& s)
{
  emit_partial_load_address(dest,s);
  str->code_ref(s);
  s << endl;
}

static void emit_load_int(const char *dest, IntEntry *i, ostream& s)
{
  emit_partial_load_address(dest,s);
  i->code_ref(s);
  s << endl;
}

static void emit_move(const char *dest_reg, const char *source_reg, ostream& s)
{ s << MOVE << dest_reg << " " << source_reg << endl; }

static void emit_neg(const char *dest, const char *src1, ostream& s)
{ s << NEG << dest << " " << src1 << endl; }

static void emit_add(const char *dest, const char *src1, const char *src2, ostream& s)
{ s << ADD << dest << " " << src1 << " " << src2 << endl; }

static void emit_addu(const char *dest, const char *src1, const char *src2, ostream& s)
{ s << ADDU << dest << " " << src1 << " " << src2 << endl; }

static void emit_addiu(const char *dest, const char *src1, int imm, ostream& s)
{ s << ADDIU << dest << " " << src1 << " " << imm << endl; }

static void emit_div(const char *dest, const char *src1, const char *src2, ostream& s)
{ s << DIV << dest << " " << src1 << " " << src2 << endl; }

static void emit_mul(const char *dest, const char *src1, const char *src2, ostream& s)
{ s << MUL << dest << " " << src1 << " " << src2 << endl; }

static void emit_sub(const char *dest, const char *src1, const char *src2, ostream& s)
{ s << SUB << dest << " " << src1 << " " << src2 << endl; }

static void emit_sll(const char *dest, const char *src1, int num, ostream& s)
{ s << SLL << dest << " " << src1 << " " << num << endl; }

static void emit_jalr(const char *dest, ostream& s)
{ s << JALR << "\t" << dest << endl; }

static void emit_jal(const char *address,ostream &s)
{ s << JAL << address << endl; }

static void emit_return(ostream& s)
{ s << RET << endl; }

static void emit_gc_assign(ostream& s)
{ s << JAL << "_GenGC_Assign" << endl; }

static void emit_disptable_ref(Symbol sym, ostream& s)
{  s << sym << DISPTAB_SUFFIX; }

static void emit_init_ref(Symbol sym, ostream& s)
{ s << sym << CLASSINIT_SUFFIX; }

static void emit_label_ref(int l, ostream &s)
{ s << "label" << l; }

static void emit_protobj_ref(Symbol sym, ostream& s)
{ s << sym << PROTOBJ_SUFFIX; }

static void emit_method_ref(Symbol classname, Symbol methodname, ostream& s)
{ s << classname << METHOD_SEP << methodname; }

static void emit_label_def(int l, ostream &s)
{
  emit_label_ref(l,s);
  s << ":" << endl;
}

static void emit_beqz(const char *source, int label, ostream &s)
{
  s << BEQZ << source << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_beq(const char *src1, const char *src2, int label, ostream &s)
{
  s << BEQ << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_bne(const char *src1, const char *src2, int label, ostream &s)
{
  s << BNE << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_bleq(const char *src1, const char *src2, int label, ostream &s)
{
  s << BLEQ << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_blt(const char *src1, const char *src2, int label, ostream &s)
{
  s << BLT << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_blti(const char *src1, int imm, int label, ostream &s)
{
  s << BLT << src1 << " " << imm << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_bgti(const char *src1, int imm, int label, ostream &s)
{
  s << BGT << src1 << " " << imm << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_branch(int l, ostream& s)
{
  s << BRANCH;
  emit_label_ref(l,s);
  s << endl;
}

//
// Push a register on the stack. The stack grows towards smaller addresses.
//
static void emit_push(const char *reg, ostream& str)
{
  emit_store(reg,0,SP,str);
  emit_addiu(SP,SP,-4,str);
}

//load from stack top and store value in reg
static void emit_top(const char *reg, ostream &str){
  emit_load(reg,1,SP,str);
}

//store value in reg and pop stack top.
static void emit_pop(const char *reg, ostream &str){
  emit_load(reg,1,SP,str);
  emit_addiu(SP,SP,4,str);
}

//
// Fetch the integer value in an Int object.
// Emits code to fetch the integer value of the Integer object pointed
// to by register source into the register dest
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
  if (source != (char*)A1) emit_move(A1, source, s);
  s << JAL << "_gc_check" << endl;
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
  s << WORD << "-1" << endl;

  code_ref(s);  s  << LABEL                                             // label
      << WORD << stringclasstag << endl                                 // tag
      << WORD << (DEFAULT_OBJFIELDS + STRING_SLOTS + (len+4)/4) << endl;// size


 /***** Add dispatch information for class String ******/

      s << WORD;  emit_disptable_ref(Str, s); s << endl;      // dispatch table
      s << WORD;  lensym->code_ref(s);  s << endl;            // string length
  emit_string_constant(s,str);                                // ascii string
  s << ALIGN;                                                 // align to word
}

//
// StrTable::code_string
// Generate a string object definition for every string constant in the 
// stringtable.
//
void StrTable::code_string_table(ostream& s, int stringclasstag)
{  
  for (List<StringEntry> *l = tbl; l; l = l->tl())
    l->hd()->code_def(s,stringclasstag);
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
  s << WORD << "-1" << endl;

  code_ref(s);  s << LABEL                                // label
      << WORD << intclasstag << endl                      // class tag
      << WORD << (DEFAULT_OBJFIELDS + INT_SLOTS) << endl; // object size

 /***** Add dispatch information for class Int ******/

      s << WORD;  emit_disptable_ref(Int, s); s << endl;  // dispatch table
      s << WORD << str << endl;                           // integer value
}


//
// IntTable::code_string_table
// Generate an Int object definition for every Int constant in the
// inttable.
//
void IntTable::code_string_table(ostream &s, int intclasstag)
{
  for (List<IntEntry> *l = tbl; l; l = l->tl())
    l->hd()->code_def(s,intclasstag);
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
  s << WORD << "-1" << endl;

  code_ref(s);  s << LABEL                                  // label
      << WORD << boolclasstag << endl                       // class tag
      << WORD << (DEFAULT_OBJFIELDS + BOOL_SLOTS) << endl;  // object size

 /***** Add dispatch information for class Bool ******/

      s << WORD;  emit_disptable_ref(Bool, s);   s << endl; // dispatch table
      s << WORD << val << endl;                             // value (0 or 1)
}

//////////////////////////////////////////////////////////////////////////////
//
//  CgenClassTable methods
//
//////////////////////////////////////////////////////////////////////////////

//***************************************************
//
//  Emit code to start the .data segment and to
//  declare the global names.
//
//***************************************************

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
  str << GLOBAL << CLASSNAMETAB << endl;
  str << GLOBAL; emit_protobj_ref(main,str);    str << endl;
  str << GLOBAL; emit_protobj_ref(integer,str); str << endl;
  str << GLOBAL; emit_protobj_ref(string,str);  str << endl;
  str << GLOBAL; falsebool.code_ref(str);  str << endl;
  str << GLOBAL; truebool.code_ref(str);   str << endl;
  str << GLOBAL << INTTAG << endl;
  str << GLOBAL << BOOLTAG << endl;
  str << GLOBAL << STRINGTAG << endl;

  //
  // We also need to know the tag of the Int, String, and Bool classes
  // during code generation.
  //
  str << INTTAG << LABEL
      << WORD << intclasstag << endl;
  str << BOOLTAG << LABEL 
      << WORD << boolclasstag << endl;
  str << STRINGTAG << LABEL 
      << WORD << stringclasstag << endl;    
}


//***************************************************
//
//  Emit code to start the .text segment and to
//  declare the global names.
//
//***************************************************

void CgenClassTable::code_global_text()
{
  str << GLOBAL << HEAP_START << endl
      << HEAP_START << LABEL 
      << WORD << 0 << endl
      << "\t.text" << endl
      << GLOBAL;
  emit_init_ref(idtable.add_string("Main"), str);
  str << endl << GLOBAL;
  emit_init_ref(idtable.add_string("Int"),str);
  str << endl << GLOBAL;
  emit_init_ref(idtable.add_string("String"),str);
  str << endl << GLOBAL;
  emit_init_ref(idtable.add_string("Bool"),str);
  str << endl << GLOBAL;
  emit_method_ref(idtable.add_string("Main"), idtable.add_string("main"), str);
  str << endl;
}

void CgenClassTable::code_bools(int boolclasstag)
{
  falsebool.code_def(str,boolclasstag);
  truebool.code_def(str,boolclasstag);
}

void CgenClassTable::code_select_gc()
{
  //
  // Generate GC choice constants (pointers to GC functions)
  //
  str << GLOBAL << "_MemMgr_INITIALIZER" << endl;
  str << "_MemMgr_INITIALIZER:" << endl;
  str << WORD << gc_init_names[cgen_Memmgr] << endl;
  str << GLOBAL << "_MemMgr_COLLECTOR" << endl;
  str << "_MemMgr_COLLECTOR:" << endl;
  str << WORD << gc_collect_names[cgen_Memmgr] << endl;
  str << GLOBAL << "_MemMgr_TEST" << endl;
  str << "_MemMgr_TEST:" << endl;
  str << WORD << (cgen_Memmgr_Test == GC_TEST) << endl;
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
//
//********************************************************

void CgenClassTable::code_constants()
{
  //
  // Add constants that are required by the code generator.
  //
  stringtable.add_string("");
  inttable.add_string("0");

  stringtable.code_string_table(str,stringclasstag);
  inttable.code_string_table(str,intclasstag);
  code_bools(boolclasstag);
}

void CgenClassTable::code_name_tab()
{
  str << CLASSNAMETAB << LABEL;
  for (int i = 0; i < tag_to_class.size(); ++i)
  {
    str << WORD; //tag_to_class[i]->get_name()->code_ref(str);
    stringtable.lookup_string(tag_to_class[i]->get_name()->get_string())->code_ref(str);
            str << endl;
  }
}

void CgenClassTable::code_objTab()
{
  str << CLASSOBJTAB << LABEL;
  int i = 0;
  while (tag_to_class.count(i))
  {
    str << WORD; emit_protobj_ref(tag_to_class[i]->get_name(), str); str << endl
        << WORD; emit_init_ref(tag_to_class[i]->get_name(), str); str << endl;
    i++;
  }
}

void CgenClassTable::code_dispTab()
{
  List<CgenNode> *l;
  for (l = nds; l; l = l->tl())
    l->hd()->code_dispTab(str);
}

void CgenClassTable::code_prototype()
{
  List<CgenNode> *l;
  for (l = nds; l; l = l->tl())
    l->hd()->code_prototype_def(str);
}

void CgenClassTable::code_object_init()
{
  List<CgenNode> *l;
  //cout<<"#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" << tag_to_class[6]->get_name()<<endl;
  for (int i = 0; i < tag_to_class.size(); ++i)
  {
    //cout<< "#" << i << " " << endl;
    //cout << "#" << tag_to_class[i]->get_name() << endl;
    curr_class = tag_to_class[i];
    tag_to_class[i]->code_init_def(str);
  }
}

void CgenClassTable::code_class_method()
{
  std::set<Feature> coded_method;
  for (int i = 0; i < tag_to_class.size(); ++i)
  {
    curr_class = tag_to_class[i];
    curr_class->code_methods(str, coded_method);
  }
}

CgenClassTable::CgenClassTable(Classes classes, ostream& s) : nds(NULL) , str(s)
{
  /*tag number from example.s */
   stringclasstag = 0 /* Change to your String class tag here */;
   intclasstag =    0 /* Change to your Int class tag here */;
   boolclasstag =   0 /* Change to your Bool class tag here */;

   enterscope();
   if (cgen_debug) cout << "Building CgenClassTable" << endl;
   install_basic_classes();
   install_classes(classes);
   build_inheritance_tree();
   allocate_tag();

   code();
   exitscope();
}

void CgenClassTable::install_basic_classes()
{

// The tree package uses these globals to annotate the classes built below.
  //curr_lineno  = 0;
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
           single_Features(method(cool_copy, nil_Formals(), SELF_TYPE, no_expr()))),
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
//       val                                  ???
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
//
void CgenClassTable::install_class(CgenNodeP nd)
{
  Symbol name = nd->get_name();

  if (probe(name))
    {
      return;
    }

  symbol_to_class[nd->get_name()] = nd;
  nds = new List<CgenNode>(nd,nds);
  addid(name,nd);
}

void CgenClassTable::install_classes(Classes cs)
{
  for(int i = cs->first(); cs->more(i); i = cs->next(i))
    install_class(new CgenNode(cs->nth(i),NotBasic,this));
}

//
// CgenClassTable::build_inheritance_tree
//
void CgenClassTable::build_inheritance_tree()
{
  for(List<CgenNode> *l = nds; l; l = l->tl())
      set_relations(l->hd());
}

void CgenClassTable::allocate_tag()
{
  CgenNodeP object_node = NULL;
  for(List<CgenNode> *l = nds; l; l = l->tl())
  {
    if (l->hd()->get_name() == Object) {
      object_node = l->hd();
      break;
    }
  }
  std::stack<CgenNodeP> inallocated;
  inallocated.push(object_node);
  int nexttag = 0;
  while (!inallocated.empty())
  {
    // The class name is legal, so add it to the list of classes
    // and the symbol table.
    CgenNodeP nd = inallocated.top();
    inallocated.pop();
    nd->set_tag(nexttag++);
    Symbol className = nd->get_name();
    if (className == Str)
      stringclasstag = nd->get_tag();
    else if (className == Int)
      intclasstag = nd->get_tag();
    else if (className == Bool)
      boolclasstag = nd->get_tag();

    CgenNodeP parentnd = nd->get_parentnd();
    if (parentnd->get_name() != No_class)
      parentnd->set_max_child(nd->get_tag());
    tag_to_class[nd->get_tag()] = nd;
    cout <<"#Size: " << tag_to_class.size() << " " << nd->get_name() << " " << nd->get_tag() << endl;

    List<CgenNode> *l;
    for (l = nd->get_children(); l; l = l->tl())
      inallocated.push(l->hd());
  }
}

//
// CgenClassTable::set_relations
//
// Takes a CgenNode and locates its, and its parent's, inheritance nodes
// via the class table.  Parent and child pointers are added as appropriate.
//
void CgenClassTable::set_relations(CgenNodeP nd)
{
  CgenNode *parent_node = probe(nd->get_parent());
  nd->set_parentnd(parent_node);
  parent_node->add_child(nd);
}

void CgenNode::add_child(CgenNodeP n)
{
  children = new List<CgenNode>(n,children);
}

void CgenNode::set_parentnd(CgenNodeP p)
{
  assert(parentnd == NULL);
  assert(p != NULL);
  parentnd = p;
}

void CgenNode::calc_size()
{
  size = 3 + attr_list.size();
}

void CgenNode::code_prototype_def(ostream &s)
{
  std::deque<CgenNodeP> path;
  CgenNodeP c = this;
  while (c->get_name() != No_class)
  {
    path.push_front(c);
    c = c->parentnd;
  }

  std::deque<CgenNodeP>::iterator it;
  for (it = path.begin(); it != path.end(); it++)
  {
    c = *it;
    for(int i = c->features->first(); c->features->more(i); i = c->features->next(i))
    {
      Feature feature = c->features->nth(i);
      if (feature->is_attr())
        attr_list.push_back(feature);
    }
  }

  calc_size();
  code_prototype_ref(s); s << LABEL;
  s << WORD << tag << endl;
  s << WORD << size << endl;
  s << WORD << name << DISPTAB_SUFFIX << endl;
  std::vector<Feature>::iterator it2;
  for(it2 = attr_list.begin(); it2 != attr_list.end(); it2++)
  {
    Feature feature = *it2;
    s << WORD;
    if (feature->get_type() == Int)
    {
      inttable.lookup_string("0")->code_ref(s);
      s << endl;
    }
    else if (feature->get_type() == Bool)
      s << BOOLCONST_PREFIX << 0 << endl;
    else if (feature->get_type() == Str)
    {
      stringtable.lookup_string("")->code_ref(s);
      s << endl;
    }
    else
      s << 0 << endl;
  }
  s << WORD << -1 << endl;
}

void CgenNode::code_dispTab(ostream &s)
{
  s << name << DISPTAB_SUFFIX << LABEL;
  std::map<Symbol, Feature> method_used;
  std::map<Symbol, CgenNodeP> method_used_class;
  std::map<Symbol, CgenNodeP> method_earliest;
  std::deque<CgenNodeP> path;
  CgenNodeP c = this;
  while (c->get_name() != No_class)
  {
    path.push_front(c);
    for(int i = c->features->first(); c->features->more(i); i = c->features->next(i))
    {
      Feature feature = c->features->nth(i);
      if (feature->is_method())
      {
        if (!method_used.count(feature->get_name()))
        {
          method_used[feature->get_name()] = feature;
          method_used_class[feature->get_name()] = c;
        }
        method_earliest[feature->get_name()] = c;
      }
    }
    c = c->parentnd;
  }
  //cout << "#" << method_earliest.size() << " " << method_used.size() << endl;
  std::deque<CgenNodeP>::iterator it;
  for (it = path.begin(); it != path.end(); it++)
  {
    c = *it;
    for(int i = c->features->first(); c->features->more(i); i = c->features->next(i))
    {
      Feature feature = c->features->nth(i);
      if (feature->is_method() && method_earliest[feature->get_name()] == c)
        method_list.push_back(method_used[feature->get_name()]);
    }
  }
  std::vector<Feature>::iterator it2;
  for(it2 = method_list.begin(); it2 != method_list.end(); it2++)
  {
    Feature feature = *it2;
    s << WORD << method_used_class[feature->get_name()]->get_name() << "."
      << feature->get_name() << endl;
  }
}

void CgenNode::code_init_ref(ostream &s)
{
  s << name << CLASSINIT_SUFFIX;
}

void CgenNode::code_init_def(ostream &s)
{
  emit_init_ref(name, s); s << LABEL;
  emit_addiu(SP, SP, -12, s);
  emit_store(FP, 3, SP, s);
  emit_store(SELF, 2, SP, s);
  emit_store(RA, 1, SP, s);
  emit_addiu(FP, SP, 4, s);
  emit_move(SELF, ACC, s);
  if (parentnd->get_name() != No_class)
  {
    s << JAL; parentnd->code_init_ref(s); s << endl;
  }
  for (int i = features->first(); features->more(i); i = features->next(i))
  {
    Feature attr = features->nth(i);
    if (attr->is_attr())
    {
      attr->get_init()->code(s);
      if (typeid(*(attr->get_init())) != typeid(no_expr_class))
        emit_store(ACC, get_attr_offset(attr->get_name()), SELF, s);
    }
  }
  emit_move(ACC,SELF,s);
  emit_load(FP, 3, SP, s);
  emit_load(SELF, 2, SP, s);
  emit_load(RA, 1, SP, s);
  emit_addiu(SP, SP, 12, s);
  emit_return(s);
}

void CgenNode::code_methods(ostream &s, std::set<Feature> &coded_method)
{
  for (int i = 0; i < method_list.size(); ++i)
  {
    Feature method = method_list[i];
    Symbol method_name = method->get_name();
    if (method_name != type_name && method_name != cool_copy
        && method_name != cool_abort && !coded_method.count(method))
    {
      coded_method.insert(method);
      if (name != IO && name != Str)
      {
        emit_method_ref(name, method->get_name(), s); s << LABEL;
        emit_addiu(SP, SP, -12, s);
        emit_store(FP, 3, SP, s);
        emit_store(SELF, 2, SP, s);
        emit_store(RA, 1, SP, s);
        emit_addiu(FP, SP, 4, s);
        emit_move(SELF, ACC, s);
        Formals formals = method->get_formals();
        identifiers.enterscope();
        int n = 0;
        for (int i = formals->first(); formals->more(i); i = formals->next(i))
          n++;
        for (int i = formals->first(); formals->more(i); i = formals->next(i))
          identifiers.addid(formals->nth(i)->get_name(), new int(3 + n - i - 1));
        method->get_expr()->code(s);
        identifiers.exitscope();
        emit_load(FP, 3, SP, s);
        emit_load(SELF, 2, SP, s);
        emit_load(RA, 1, SP, s);
        emit_addiu(SP, SP, 12 + n*4, s);
        emit_return(s);
      }
    }
  }
}

void CgenNode::set_max_child(int tag)
{
  max_child = tag;
  if (parentnd->get_name() != No_class)
    parentnd->set_max_child(tag);
}

void CgenClassTable::code()
{
  if (cgen_debug) cout << "coding global data" << endl;
  code_global_data();

  if (cgen_debug) cout << "choosing gc" << endl;
  code_select_gc();

  if (cgen_debug) cout << "coding constants" << endl;
  code_constants();
//                 Add your code to emit
  code_name_tab();
  code_objTab();
  code_dispTab();
  code_prototype();
//                   - prototype objects
//                   - class_nameTab
//                   - dispatch tables
//

  if (cgen_debug) cout << "coding global text" << endl;
  code_global_text();
  code_object_init();
  code_class_method();
//                 Add your code to emit
//                   - object initializer
//                   - the class methods
//                   - etc...

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

CgenNode::CgenNode(Class_ nd, Basicness bstatus, CgenClassTableP ct) :
   class__class((const class__class &) *nd),
   parentnd(NULL),
   children(NULL),
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

void assign_class::code(ostream &s) {
  s<<"#assign begin"<<endl;
  int* offset_to_fp=identifiers.lookup(name);
  expr->code(s);
  if(offset_to_fp==NULL){
    emit_store(ACC,curr_class->get_attr_offset(name),SELF,s);
  }else{
    emit_store(ACC,*offset_to_fp,FP,s);
  }
  s<<"#assign end"<<endl;
}

void static_dispatch_class::code(ostream &s) {
  s<<"#static dispatch begin"<<endl;
  CgenNodeP curr=symbol_to_class[name];
  int n = 0;
  for(int i=actual->first();actual->more(i);i=actual->next(i)){
    Expression e=actual->nth(i);
    e->code(s);
    emit_push(ACC,s);
    n++;
  }

  expr->code(s);
  //check void dispatch
  int not_void_label=get_label();
  emit_bne(ACC,ZERO,not_void_label,s);
  emit_load_string(ACC, stringtable.lookup_string(curr_class->filename->get_string()),s);
  emit_load_imm(T1, line_number, s);
  emit_jal("_dispatch_abort", s);

  emit_label_def(not_void_label, s);

  Symbol class_name= type_name==SELF_TYPE? curr->name : type_name;
  CgenNodeP class_of_static_type=symbol_to_class[class_name];
  //T1 <- static_type dispatch table
  emit_partial_load_address(T1, s);emit_disptable_ref(type_name, s);s << endl;
  emit_load(T1,class_of_static_type->get_method_offset(name),T1,s);
  emit_jalr(T1,s);

  s<<"#static dispatch end"<<endl;
}

void dispatch_class::code(ostream &s) {
  s<<"#dispatch begin"<<endl;

  int n = 0;
  for(int i=actual->first();actual->more(i);i=actual->next(i)){
    Expression e=actual->nth(i);
    e->code(s);
    emit_push(ACC,s);
    n++;
  }

  expr->code(s);
  //check void dispatch
  int not_void_label=get_label();
  emit_bne(ACC,ZERO,not_void_label,s);
  emit_load_string(ACC, stringtable.lookup_string(curr_class->filename->get_string()),s);
  emit_load_imm(T1, line_number, s);
  emit_jal("_dispatch_abort", s);

  emit_label_def(not_void_label, s);
  emit_load(T1,2,ACC,s);
  cout << "#" << curr_class->get_name() << endl;
  if(expr->get_type()==SELF_TYPE){
    emit_load(T1,curr_class->get_method_offset(name),T1,s);
  }else{
    emit_load(T1,symbol_to_class[expr->get_type()]->get_method_offset(name),T1,s);
  }

  emit_jalr(T1,s);
  s<<"#dispatch end"<<endl;
}

void cond_class::code(ostream &s) {
  s<<"#cond begin"<<endl;
  int false_label=get_label(),end_label=get_label();
  pred->code(s);
  emit_fetch_int(T1,ACC,s);
  emit_beq(T1,ZERO, false_label, s);
  then_exp->code(s);
  emit_branch(end_label, s);
  emit_label_def(false_label, s);
  else_exp->code(s);
  emit_label_def(end_label, s);
  s<<"#cond end"<<endl;
}

void loop_class::code(ostream &s) {
  s<<"#loop begin"<<endl;
  int loop_label=get_label(),end_label=get_label();
  emit_label_def(loop_label,s);
  pred->code(s);
  emit_fetch_int(T1,ACC,s);
  emit_beq(T1,ZERO, end_label, s);
  body->code(s);
  emit_branch(loop_label, s);
  emit_label_def(end_label, s);
  emit_move(ACC,ZERO,s);//while expr: void
  s<<"#loop end"<<endl;
}

void typcase_class::code(ostream &s) {
  s<<"#typecase code begin"<<endl;
  expr->code(s);
  emit_push(ACC,s);
  int not_void_label=get_label();
  emit_bne(ACC,ZERO,not_void_label,s);
  //void
  emit_load_string(ACC, stringtable.lookup_string(curr_class->filename->get_string()),s);
  emit_load_imm(T1, line_number, s);
  emit_jal("_case_abort2", s);

  emit_label_def(not_void_label,s);
  //not void
  //T2 <= ACC class tag
  emit_load(T2,0,ACC,s);
  int end_label=get_label();
  std::map<int,branch_class*> type_sort;
  for(int i = cases->first(); cases->more(i); i=cases->next(i)) 
  {
    branch_class* b = (branch_class*)cases->nth(i);
    CgenNodeP curr=symbol_to_class[b->type_decl];
    type_sort[curr->get_tag()]=b;
  }

  for(std::map<int,branch_class*>::reverse_iterator it=type_sort.rbegin();it!=type_sort.rend();it++)
  {
    branch_class* b = it->second;
    identifiers.enterscope();
    CgenNodeP curr=symbol_to_class[b->type_decl];
    cout<<"# fuck off"<<endl;
    int tag_of_this=curr->get_tag();
    int max_tag_of_child=curr->get_max_child();
    int next_label=get_label();

    emit_blti(T2,tag_of_this,next_label,s);
    emit_bgti(T2,max_tag_of_child,next_label,s);
    //when it doesn't jump, this type class is the parent of expr
    identifiers.addid(b->name,new int(-(1+offset_fp++)));
    b->expr->code(s);
    offset_fp--;
    emit_branch(end_label,s);

    emit_label_def(next_label,s);
    identifiers.exitscope();
  }
  //reaching here means no matching branch
  emit_jal("_case_abort",s);

  emit_label_def(end_label,s);
  emit_pop(T1,s);
  s<<"#typecase code end"<<endl;
}

void block_class::code(ostream &s) {
  for (int i = body->first(); body->more(i); i = body->next(i)) {
		Expression e = body->nth(i);
		e->code(s);
	}
}

void let_class::code(ostream &s) {
  s<<"#let code begin"<<endl;
  if(typeid(*init)==typeid(no_expr_class)){
    if(type_decl==Int||type_decl==Bool||type_decl==Str||type_decl==IO || type_decl==Object){
      emit_partial_load_address(ACC,s);
      CgenNodeP type_class=symbol_to_class[type_decl];
      type_class->code_prototype_ref(s);s<<endl;
      emit_jal("Object.copy",s);
      s<<JAL;emit_init_ref(type_decl,s);s<<endl;
    }else emit_add(ACC,ZERO,ZERO,s);
  }else init->code(s);
  emit_push(ACC,s);

  identifiers.enterscope();
  identifiers.addid(identifier,new int(-(1+offset_fp++)));
  body->code(s);
  emit_pop(T1,s);
  offset_fp--;
  identifiers.exitscope();
  s<<"#let code end"<<endl;
}

void plus_class::code(ostream &s) {
  e1->code(s);
  emit_push(ACC, s);
  e2->code(s);
  emit_jal("Object.copy", s);
  emit_fetch_int(T1, ACC, s);
  emit_pop(T2, s);
  emit_fetch_int(T2, T2, s);
  emit_add(T1, T2, T1, s);
  emit_store_int(T1, ACC, s);
}

void sub_class::code(ostream &s) {
  e1->code(s);
  emit_push(ACC, s);
  e2->code(s);
  emit_jal("Object.copy", s);
  emit_fetch_int(T1, ACC, s);
  emit_pop(T2, s);
  emit_fetch_int(T2, T2, s);
  emit_sub(T1, T2, T1, s);
  emit_store_int(T1, ACC, s);
}

void mul_class::code(ostream &s) {
  e1->code(s);
  emit_push(ACC, s);
  e2->code(s);
  emit_jal("Object.copy", s);
  emit_fetch_int(T1, ACC, s);
  emit_pop(T2, s);
  emit_fetch_int(T2, T2, s);
  emit_mul(T1, T2, T1, s);
  emit_store_int(T1, ACC, s);
}

void divide_class::code(ostream &s) {
  e1->code(s);
  emit_push(ACC, s);
  e2->code(s);
  emit_jal("Object.copy", s);
  emit_fetch_int(T1, ACC, s);
  emit_pop(T2, s);
  emit_fetch_int(T2, T2, s);
  emit_div(T1, T2, T1, s);
  emit_store_int(T1, ACC, s);
}

void neg_class::code(ostream &s) {
  e1->code(s);
  emit_jal("Object.copy", s);
  emit_fetch_int(T1, ACC, s);
  emit_neg(T1, T1, s);
  emit_store_int(T1, ACC, s);
}

void lt_class::code(ostream &s) {
  s<<"#lt begin"<<endl;
  int true_label=get_label(),end_label=get_label();
  e1->code(s);
  emit_push(ACC,s);
  e2->code(s);
  emit_move(T2,ACC,s);
  emit_pop(T1,s);
  emit_fetch_int(T1,T1,s);
  emit_fetch_int(T2,T2,s);
  emit_blt(T1,T2,true_label,s);
  emit_load_bool(ACC,falsebool,s);
  emit_branch(end_label,s);
  emit_label_def(true_label,s);
  emit_load_bool(ACC,truebool,s);
  emit_label_def(end_label,s);
  s<<"#lt end"<<endl;
}

void eq_class::code(ostream &s) {
  s<<"#eq begin"<<endl;
  int true_label=get_label(),end_label=get_label();
  e1->code(s);
  emit_push(ACC,s);
  e2->code(s);
  emit_pop(T1,s);
  emit_move(T2,ACC,s);
  emit_load_bool(ACC,truebool,s);
  emit_beq(T1,T2,end_label,s);
  emit_load_bool(A1,falsebool,s);
  emit_jal("equality_test",s);
  emit_label_def(end_label,s);
  s<<"#eq end"<<endl;
}

void leq_class::code(ostream &s) {
  s<<"#leq begin"<<endl;
  int true_label=get_label(),end_label=get_label();
  e1->code(s);
  emit_push(ACC,s);
  e2->code(s);
  emit_move(T2,ACC,s);
  emit_pop(T1,s);
  emit_fetch_int(T1,T1,s);
  emit_fetch_int(T2,T2,s);
  emit_bleq(T1,T2,true_label,s);
  emit_load_bool(ACC,falsebool,s);
  emit_branch(end_label,s);
  emit_label_def(true_label,s);
  emit_load_bool(ACC,truebool,s);
  emit_label_def(end_label,s);
  s<<"#leq end"<<endl;
}

void comp_class::code(ostream &s) {
  s<<"#comp begin"<<endl;
  int true_label=get_label(),end_label=get_label();
  e1->code(s);
  emit_fetch_int(T1,ACC,s);
  emit_beq(T1,ZERO,true_label,s);
  emit_load_bool(ACC,falsebool,s);
  emit_branch(end_label,s);
  emit_label_def(true_label,s);
  emit_load_bool(ACC,truebool,s);
  emit_label_def(end_label,s);
  s<<"#comp end"<<endl;
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
  s<<"#new code begin"<<endl;
  if(type_name==SELF_TYPE){
    emit_load_address(T1,CLASSOBJTAB,s);
    emit_load(T2,0,SELF,s);
    //2*tag words -> 8*tag bytes
    emit_sll(T2,T2,3,s);
    emit_addu(T2,T1,T2,s);
    emit_load(ACC,0,T2,s);
    emit_push(ACC,s);
    emit_jal("Object.copy",s);
    emit_load_address(T1,CLASSOBJTAB,s);
    emit_load(T2,0,SELF,s);
    //2*tag words -> 8*tag bytes
    emit_sll(T2,T2,3,s);
    emit_addu(T2,T1,T2,s);
    emit_load(T1,1,T2,s);
    emit_store(ACC,1,SP,s);
    emit_jalr(T1,s);
    emit_addiu(SP,SP,4,s);
  }else{
    emit_partial_load_address(ACC,s);symbol_to_class[type_name]->code_prototype_ref(s);s<<endl;
    emit_jal("Object.copy",s);
    s<<JAL;emit_init_ref(type_name,s);s<<endl;
  }
  s<<"#new code end"<<endl;
}

void isvoid_class::code(ostream &s) {
  s<<"#isvoid code begin"<<endl;
  int true_label=get_label(),end_label=get_label();
  e1->code(s);
  emit_beq(ACC,ZERO,true_label,s);
  emit_load_bool(ACC,falsebool,s);
  emit_branch(end_label,s);
  emit_label_def(true_label,s);
  emit_load_bool(ACC,truebool,s);
  emit_label_def(end_label,s);
  s<<"#isvoid code end"<<endl;
}

void no_expr_class::code(ostream &s) {
  //li ACC 0 
  s<<"#No expression"<<endl;
}

void object_class::code(ostream &s) {
  s<<"#object code begin"<<endl;
  if(name==self){
    emit_move(ACC,SELF,s);
  }else{
    int* offset_to_fp=identifiers.lookup(name);
    if(offset_to_fp==NULL){
      emit_load(ACC,curr_class->get_attr_offset(name),SELF,s);
    }else{
      emit_load(ACC,*offset_to_fp,FP,s);
    }
  }
  s<<"#object code end"<<endl;
}

