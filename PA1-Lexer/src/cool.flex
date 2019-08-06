  /*
  * Define regular expressions for the tokens of COOL here. Make sure, you
  * handle correctly special cases, like:
  *   - Nested comments
  *   - String constants: They use C like systax and can contain escape
  *     sequences. Escape sequence \c is accepted for all characters c. Except
  *     for \n \t \b \f, the result is c.
  *   - Keywords: They are case-insensitive except for the values true and
  *     false, which must begin with a lower-case letter.
  *   - Multiple-character operators (like <-): The scanner should produce a
  *     single token for every such operator.
  *   - Line counting: You should keep the global variable curr_lineno updated
  *     with the correct line number
  */
  /*Defination Section*/
%{
  /*C code to be copied verbatim*/
#include <cool-parse.h>
#include <stringtab.h>
#include <utilities.h>

/* The compiler assumes these identifiers. */
#define yylval cool_yylval
#define yylex  cool_yylex

/* Max size of string constants */
#define MAX_STR_CONST 1025
#define YY_NO_UNPUT   /* keep g++ happy */

extern FILE *fin; /* we read from this file */
/* define YY_INPUT so we read from the FILE fin:*/
#undef YY_INPUT
#define YY_INPUT(buf,result,max_size) \
  if ( (result = fread( (char*)buf, sizeof(char), max_size, fin)) < 0) \
    YY_FATAL_ERROR( "read() in flex scanner failed");

char string_buf[10000]; /* to assemble string constants */
char *string_buf_ptr;

extern int curr_lineno;
extern YYSTYPE cool_yylval;

/* Add Your own definitions here*/  
#include <math.h>
#include <string.h>
int len=0;
%}
%option noyywrap

%x  STRERROR
%x  COMMENT
%x  LINECOM
%x  STR
/*Define names for regular expressions here.*/
INTEGER [0-9]+
OBJECTID  [a-z][a-zA-Z0-9_]*
TYPEID  [A-Z][a-zA-Z0-9_]*
WHITESPACE  [ \f\r\t\v]
NEWLINE [\n]
NULL  [\\]0
CLASS [Cc][Ll][Aa][Ss][Ss]
ELSE  [Ee][Ll][Ss][Ee]
IF  [Ii][Ff]
FI  [Ff][Ii]
IN  [Ii][Nn]
INHERITS  [Ii][Nn][Hh][Ee][Rr][Ii][Tt][Ss]
ISVOID  [Ii][Ss][Vv][Oo][Ii][Dd]
LET [Ll][Ee][Tt]
LOOP  [Ll][Oo][Oo][Pp]
POOL  [Pp][Oo][Oo][Ll]
THEN  [Tt][Hh][Ee][Nn]
WHILE [Ww][Hh][Ii][Ll][Ee]
CASE  [Cc][Aa][Ss][Ee]
ESAC  [Ee][Ss][Aa][Cc]
NEW [Nn][Ee][Ww]
OF [Oo][Ff]
NOT [Nn][Oo][Tt]
TRUE [t][Rr][Uu][Ee]
FALSE [f][Aa][Ll][Ss][Ee]
DARROW  [=][>]
ASSIGN  [<][-]
LE  [<][=]

ANYCHAR .

%%
  /*comment token*/
[-][-] {BEGIN(LINECOM);}
<LINECOM>{NEWLINE}  {curr_lineno++;BEGIN(INITIAL);}
<LINECOM>{ANYCHAR}  {;}

[(][*]  {BEGIN(COMMENT);}
<COMMENT>{NEWLINE}  {curr_lineno++;}
<COMMENT>"*)" {BEGIN(INITIAL);}
<COMMENT>{ANYCHAR}  {;}
  /*keyword token*/
[*][)]  {yylval.error_msg="Unmatched *)";return ERROR;}
{TRUE}  {yylval.boolean=true;return BOOL_CONST;}
{FALSE} {yylval.boolean=false;return BOOL_CONST;}
{CLASS} {return CLASS;}
{IN}  {return IN;}
{IF}  {return IF;}
{FI}  {return FI;}
{THEN}  {return THEN;}
{ELSE}  {return ELSE;}
{WHILE} {return WHILE;}
{INHERITS}  {return INHERITS;}
{ISVOID}  {return ISVOID;}
{LET} {return LET;}
{NEW} {return NEW;}
{OF}  {return OF;}
{NOT} {return NOT;}
{LOOP}  {return LOOP;}
{POOL}  {return POOL;}
{CASE}  {return CASE;}
{ESAC}  {return ESAC;}
{ASSIGN}  {return ASSIGN;}
{LE}  {return LE;}
{DARROW}  {return DARROW;}
[\+\-\*\/\;\,\=\{\}\(\)\@\.\~\:\<]  {
  return  yytext[0];}

  /*TypeID and Interger*/
{INTEGER} {yylval.symbol=inttable.add_int(atoi(yytext));return INT_CONST;}
{OBJECTID}  {yylval.symbol=idtable.add_string(yytext);return OBJECTID;}
{TYPEID}  {yylval.symbol=idtable.add_string(yytext);return TYPEID;}
  /*string-------------------*/
[\"]  {memset(string_buf,0,10000);len=0;BEGIN(STR);}
<STR>\0 {
  yylval.error_msg="String contains null character";
  BEGIN(STRERROR);
  return ERROR;}
<STR>[\\]f  {strcat(string_buf,"\f");len+=2;}
<STR>[\\]t  {strcat(string_buf,"\t");len+=2;}
<STR>[\\]b  {strcat(string_buf,"\b");len+=2;}
<STR>[\\][\\]n  {strcat(string_buf,"\\n");len+=3;}
<STR>[\\]n  {strcat(string_buf,"\n");len+=2;}
  /*Magic here to match \\n */
<STR>\\\n {strcat(string_buf,"\n");len+=2;curr_lineno++;}
<STR>[\\][\\] {strcat(string_buf,"\\");len+=2;}
<STR>[\\][\"] {strcat(string_buf,"\"");len+=2;}
<STR>{NEWLINE}  {
  curr_lineno++;
  yylval.error_msg="Unterminated string constant";
  BEGIN(INITIAL);
  return ERROR;}
<STR>\" { 
  BEGIN(INITIAL);
  if(len>MAX_STR_CONST){
    yylval.error_msg="String constant too long";
    return ERROR;
  }else{
    yylval.symbol=stringtable.add_string(string_buf,len);
    return STR_CONST;
  }}
<STR>[^\"\\\n\0]  {strcat(string_buf,yytext);len+=1;}  
<STR>[\\] {;}
  /*whitespace and errors*/
      /*Filter of null string*/
<STRERROR>{NEWLINE}|[\"]  {BEGIN(INITIAL);}
<STRERROR>{ANYCHAR}  {;}
<STR><<EOF>>  {
  BEGIN(INITIAL);
  yylval.error_msg="EOF in string constant";
  return ERROR;}
<COMMENT><<EOF>>  {
  yylval.error_msg="EOF in comment";
  BEGIN(INITIAL);
  return ERROR;}
{WHITESPACE}  {;}
{NEWLINE} {curr_lineno++;}
{ANYCHAR} {yylval.error_msg=yytext;return ERROR;}
%%
