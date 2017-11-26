-----------------------------------------------------------------------------
-- Module      :  Parser.y
-- Copyright   :  (c) 2005-2007 Duncan Coutts
--                (c) 2008 Benedikt Huber
--                (c) [1999..2004] Manuel M T Chakravarty
--                Portions copyright 1989, 1990 James A. Roskind
-- License     :  BSD-style
-- Maintainer  :  benedikt.huber@gmail.com
-- Portability :  portable
--
--  Parser for C translation units, which have already been run through the C
--  preprocessor. It is recommended to use the `strict' flag for happy.
--
--  The parser recognizes all of ISO C 99 and most GNU C extensions.
--
--  With C99 we refer to the ISO C99 standard, specifically the section numbers
--  used below refer to this report:
--
--    <http://www.open-std.org/jtc1/sc22/wg14/www/docs/n1124.pdf>
--
-- GNU extensions are documented in the gcc parser
--
--    <http://gcc.gnu.org/viewcvs/trunk/gcc/c-parser.c>
--
-- and in: <http://gcc.gnu.org/onlinedocs/gcc/C-Extensions.html>
--
-- The set of supported extensions is documented in
--
--    <http://www.sivity.net/projects/language.c/wiki/Cee>
------------------------------------------------------------------

{
// action_4 is unused...
#![allow(dead_code)]

use std::rc::Rc;
use std::boxed::FnBox;
use either::Either::*;

use data::position::{Located, Pos};
use data::node::NodeInfo;
use data::ident::Ident;
use parser::tokens::*;
use parser::lexer::{lexC, parseError};
use parser::{ParseError, PState, CDeclrR};
use syntax::ops::*;
use syntax::ast::*;
use syntax::constants::*;

type Error = ParseError;
type State = PState;
type Token = CToken;

macro_rules! with_pos {
    ($parser:expr, $infonode:expr, $closure:expr) => {{
        let pos1 = $infonode.pos();
        let (pos2, len) = $parser.getSavedToken().into_pos_len();
        Ok($closure(NodeInfo::new(pos1, pos2, len, $parser.getNewName())))
    }};
}

}

-- Relevant C99 sections:
--
-- 6.5 Expressions .1 - .17 and 6.6 (almost literally)
--  Supported GNU extensions:
--     - Allow a compound statement as an expression
--     - Various __builtin_* forms that take type parameters
--     - `alignof' expression or type
--     - `__extension__' to suppress warnings about extensions
--     - Allow taking address of a label with: && label
--     - Omitting the `then' part of conditional expressions
--     - complex numbers
--
-- 6.7 C Declarations .1 -.8
--  Supported GNU extensions:
--     - '__thread' thread local storage (6.7.1)
--
-- 6.8 Statements .1 - .8
--  Supported GNU extensions:
--    - case ranges (C99 6.8.1)
--    - '__label__ ident;' declarations (C99 6.8.2)
--    - computed gotos (C99 6.8.6)
--
-- 6.9 Translation unit
--  Supported GNU extensions:
--     - allow empty translation_unit
--     - allow redundant ';'
--     - allow extension keyword before external declaration
--     - asm definitions
--
--  Since some of the grammar productions are quite difficult to read,
--  (especially those involved with the decleration syntax) we document them
--  with an extended syntax that allows a more consise representation:
--
--  Ordinary rules
--
--   foo      named terminal or non-terminal
--
--   'c'      terminal, literal character token
--
--   A B      concatenation
--
--   A | B    alternation
--
--   (A)      grouping
--
--  Extended rules
--
--   A?       optional, short hand for (A|) or [A]{ 0==A || 1==A }
--
--   ...      stands for some part of the grammar omitted for clarity
--
--   {A}      represents sequences, 0 or more.
--
--   <permute> modifier which states that any permutation of the immediate subterms is valid
--
--
--- TODO ----------------------------------------------------------------------
--
--  !* We ignore C11 _Atomic type annotations
--  !* We ignore the C99 static keyword (see C99 6.7.5.3)
--  !* We do not distinguish in the AST between incomplete array types and
--      complete variable length arrays ([ '*' ] means the latter). (see C99 6.7.5.2)
--  !* The AST doesn't allow recording __attribute__ of unnamed struct field
--     (see , struct_default_declaring_list, struct_identifier_declarator)
--  !* see `We're being far to liberal here' (... struct definition within structs)
--  * Documentation isn't complete and consistent yet.

-- in order to document the parsers, we have to alias them
%name translation_unit translation_unit
%name external_declaration external_declaration
%name statement statement
%name expression expression

%tokentype { CToken }

%monad { Res } { >>= } { return }
%lexer { lexC } { CTokEof }

%expect 1

%token

'('             { CTokLParen(_) }
')'             { CTokRParen(_) }
'['             { CTokLBracket(_) }
']'             { CTokRBracket(_) }
"->"            { CTokArrow(_) }
'.'             { CTokDot(_) }
'!'             { CTokExclam(_) }
'~'             { CTokTilde(_) }
"++"            { CTokInc(_) }
"--"            { CTokDec(_) }
'+'             { CTokPlus(_) }
'-'             { CTokMinus(_) }
'*'             { CTokStar(_) }
'/'             { CTokSlash(_) }
'%'             { CTokPercent(_) }
'&'             { CTokAmper(_) }
"<<"            { CTokShiftL(_) }
">>"            { CTokShiftR(_) }
'<'             { CTokLess(_) }
"<="            { CTokLessEq(_) }
'>'             { CTokHigh(_) }
">="            { CTokHighEq(_) }
"=="            { CTokEqual(_) }
"!="            { CTokUnequal(_) }
'^'             { CTokHat(_) }
'|'             { CTokBar(_) }
"&&"            { CTokAnd(_) }
"||"            { CTokOr(_) }
'?'             { CTokQuest(_) }
':'             { CTokColon(_) }
'='             { CTokAssign(_) }
"+="            { CTokPlusAss(_) }
"-="            { CTokMinusAss(_) }
"*="            { CTokStarAss(_) }
"/="            { CTokSlashAss(_) }
"%="            { CTokPercAss(_) }
"&="            { CTokAmpAss(_) }
"^="            { CTokHatAss(_) }
"|="            { CTokBarAss(_) }
"<<="           { CTokSLAss(_) }
">>="           { CTokSRAss(_) }
','             { CTokComma(_) }
';'             { CTokSemic(_) }
'{'             { CTokLBrace(_) }
'}'             { CTokRBrace(_) }
"..."           { CTokEllipsis(_) }
alignof         { CTokAlignof(_) }
alignas         { CTokAlignas(_) }
"_Atomic"       { CTokAtomic(_) }
asm             { CTokAsm(_) }
auto            { CTokAuto(_) }
break           { CTokBreak(_) }
"_Bool"         { CTokBool(_) }
case            { CTokCase(_) }
char            { CTokChar(_) }
const           { CTokConst(_) }
continue        { CTokContinue(_) }
"_Complex"      { CTokComplex(_) }
default         { CTokDefault(_) }
do              { CTokDo(_) }
double          { CTokDouble(_) }
else            { CTokElse(_) }
enum            { CTokEnum(_) }
extern          { CTokExtern(_) }
float           { CTokFloat(_) }
"__float128"    { CTokFloat128(_) }
for             { CTokFor(_) }
"_Generic"      { CTokGeneric(_) }
goto            { CTokGoto(_) }
if              { CTokIf(_) }
inline          { CTokInline(_) }
int             { CTokInt(_) }
"__int128"      { CTokInt128(_) }
long            { CTokLong(_) }
"__label__"     { CTokLabel(_) }
"_Noreturn"     { CTokNoreturn(_) }
"_Nullable"     { CTokNullable(_) }
"_Nonnull"      { CTokNonnull(_) }
register        { CTokRegister(_) }
restrict        { CTokRestrict(_) }
return          { CTokReturn(_) }
short           { CTokShort(_) }
signed          { CTokSigned(_) }
sizeof          { CTokSizeof(_) }
static          { CTokStatic(_) }
"_Static_assert"{ CTokStaticAssert(_) }
struct          { CTokStruct(_) }
switch          { CTokSwitch(_) }
typedef         { CTokTypedef(_) }
typeof          { CTokTypeof(_) }
"__thread"      { CTokThread(_) }
union           { CTokUnion(_) }
unsigned        { CTokUnsigned(_) }
void            { CTokVoid(_) }
volatile        { CTokVolatile(_) }
while           { CTokWhile(_) }
cchar           { CTokCLit(_, _) }              -- character constant
cint            { CTokILit(_, _) }              -- integer constant
cfloat          { CTokFLit(_, _) }              -- float constant
cstr            { CTokSLit(_, _) }              -- string constant (no escapes)
ident           { CTokIdent(_, $$) }            -- identifier
tyident         { CTokTyIdent(_, $$) }          -- `typedef-name' identifier
"__attribute__" { CTokGnuC(_, GnuCTok::Attr) }    -- special GNU C tokens
"__extension__" { CTokGnuC(_, GnuCTok::Ext) }     -- special GNU C tokens
"__real__"      { CTokGnuC(_, GnuCTok::ComplexReal) }
"__imag__"      { CTokGnuC(_, GnuCTok::ComplexImag) }
-- special GNU C builtin 'functions' that actually take types as parameters:
"__builtin_va_arg"              { CTokGnuC(_, GnuCTok::VaArg) }
"__builtin_offsetof"            { CTokGnuC(_, GnuCTok::Offsetof) }
"__builtin_types_compatible_p"  { CTokGnuC(_, GnuCTok::TyCompat) }
"__builtin_convertvector"       { CTokClangC(_, ClangCTok::ConvertVector) }
clangcversion   { CTokClangC(_, ClangCTok::CVersion($$)) } -- Clang version literal

%%


-- parse a complete C translation unit
-- we have to take special care of empty translation units
translation_unit :: { Box<CTranslUnit> }
translation_unit
  : ext_decl_list {%
                      let decls = $1;
                      if decls.len() == 0 {
                          let name = p.getNewName();
                          let pos = Rc::new(p.getPosClone());
                          let nodeinfo = NodeInfo::new(pos.clone(), pos, 0, name);
                          Ok(box CTranslationUnit(decls, nodeinfo))
                      } else {
                          with_pos!(p, decls[0], |at| box CTranslationUnit(decls, at))
                      }
                  }


-- parse a list of external declarations, making up a C translation unit (C99 6.9)
--
-- * GNU extensions:
--     allow empty translation_unit
--     allow redundant ';'
ext_decl_list :: { Vec<CExtDecl> }
ext_decl_list
  : {- empty -}                         { vec![] }
  | ext_decl_list ';'                   { $1 }
  | ext_decl_list external_declaration  { appended($1, *$2) }


-- parse external C declaration (C99 6.9)
--
-- * GNU extensions:
--     allow extension keyword before external declaration (TODO: discarded)
--     asm definitions
external_declaration :: { Box<CExtDecl> }
external_declaration
  : function_definition                           { box CFDefExt(*$1) }
  | declaration                                   { box CDeclExt(*$1) }
  | "__extension__" external_declaration          { $2 }
  | asm '(' string_literal ')' ';'                {% with_pos!(p, $1, |at| box CAsmExt(*$3, at)) }


-- parse C function definition (C99 6.9.1)
--
-- function_definition :- specifiers? fun-declarator compound-statement
--                        specifiers? old-fun-declarator  declaration-list compound-statement
--
-- The specifiers are a list consisting of type-names (int, struct foo, ...),
-- storage-class specifiers (extern, static,...) and type qualifiers (const, volatile, ...).
--
--   declaration_specifier      :- <permute> type-qualifier* storage-class+ typename+    "extern unsigned static volatile int f()"
--   type_specifier             :- <permute> type-qualifier* typename+                   "const int f()", "long int f()"
--   declaration_qualifier_list :- <permute> type_qualifier* storage-class+              "extern static const f()"
--   type_qualifier_list        :- type-qualifier+                                       "const f()"
--
-- * GNU extension:
--    __attribute__ annotations
--
function_definition :: { Box<CFunDef> }
function_definition
  :                            function_declarator compound_statement
        {% p.leaveScope(); with_pos!(p, $1, |at| box CFunctionDef(vec![], $1, vec![], $2, at)) }

  | attrs                      function_declarator compound_statement
        {% p.leaveScope(); with_pos!(p, $1, |at| box CFunctionDef(liftCAttrs($1), $2, vec![], $3, at)) }

  | declaration_specifier      function_declarator compound_statement
        {% p.leaveScope(); with_pos!(p, $1, |at| box CFunctionDef($1, $2, vec![], $3, at)) }

  | type_specifier             function_declarator compound_statement
        {% p.leaveScope(); with_pos!(p, $1, |at| box CFunctionDef($1, $2, vec![], $3, at)) }

  | declaration_qualifier_list function_declarator compound_statement
        {% p.leaveScope(); with_pos!(p, $1, |at| box CFunctionDef($1, $2, vec![], $3, at)) }

  | type_qualifier_list        function_declarator compound_statement
        {% p.leaveScope(); with_pos!(p, $1, |at| box CFunctionDef(liftTypeQuals($1), $2, vec![], $3, at)) }

  | type_qualifier_list  attrs function_declarator compound_statement
        {% p.leaveScope(); with_pos!(p, $1, |at| box CFunctionDef(addVecs(liftTypeQuals($1), liftCAttrs($2)), $3, vec![], $4, at)) }

  -- old function declarators

  |                            function_declarator_old declaration_list compound_statement
        {% with_pos!(p, $1, |at| box CFunctionDef(vec![], $1, $2, $3, at)) }

  |                      attrs function_declarator_old declaration_list compound_statement
        {% with_pos!(p, $2, |at| box CFunctionDef(liftCAttrs($1), $2, $3, $4, at)) }

  | declaration_specifier      function_declarator_old declaration_list compound_statement
        {% with_pos!(p, $1, |at| box CFunctionDef($1, $2, $3, $4, at)) }

  | type_specifier             function_declarator_old declaration_list compound_statement
        {% with_pos!(p, $1, |at| box CFunctionDef($1, $2, $3, $4, at)) }

  | declaration_qualifier_list function_declarator_old declaration_list compound_statement
        {% with_pos!(p, $1, |at| box CFunctionDef($1, $2, $3, $4, at)) }

  | type_qualifier_list   function_declarator_old declaration_list compound_statement
        {% with_pos!(p, $1, |at| box CFunctionDef(liftTypeQuals($1), $2, $3, $4, at)) }

  | type_qualifier_list attrs  function_declarator_old declaration_list compound_statement
        {% with_pos!(p, $1, |at| box CFunctionDef(addVecs(liftTypeQuals($1), liftCAttrs($2)), $3, $4, $5, at)) }

-- Read declarator and put function
function_declarator :: { Box<CDeclr> }
function_declarator
  : identifier_declarator
        {%
            let declr = $1.reverse();
            p.enterScope();
            p.doFuncParamDeclIdent(&declr);
            Ok(declr)
        }


-- parse C statement (C99 6.8)
--
-- * GNU extension: ' __asm__ (...); ' statements
--
statement :: { Box<CStat> }
statement
  : labeled_statement           { $1 }
  | compound_statement          { $1 }
  | expression_statement        { $1 }
  | selection_statement         { $1 }
  | iteration_statement         { $1 }
  | jump_statement              { $1 }
  | asm_statement               {% with_pos!(p, $1, |at| box CAsm($1, at)) }


-- parse C labeled statement (C99 6.8.1)
--
-- * GNU extension: case ranges
--
labeled_statement :: { Box<CStat> }
labeled_statement
  : identifier ':' attrs_opt statement          {% with_pos!(p, $1, |at| box CLabel($1, $4, $3, at)) }
  | case constant_expression ':' statement      {% with_pos!(p, $1, |at| box CCase($2, $4, at)) }
  | default ':' statement                       {% with_pos!(p, $1, |at| box CDefault($3, at)) }
  | case constant_expression "..." constant_expression ':' statement
        {% with_pos!(p, $1, |at| box CCases($2, $4, $6, at)) }


-- parse C compound statement (C99 6.8.2)
--
-- * GNU extension: '__label__ ident;' declarations
--
compound_statement :: { Box<CStat> }
compound_statement
  : '{' enter_scope block_item_list leave_scope '}'
        {% with_pos!(p, $1, |at| box CCompound(vec![], $3, at)) }

  | '{' enter_scope label_declarations block_item_list leave_scope '}'
        {% with_pos!(p, $1, |at| box CCompound($3, $4, at)) }


-- No syntax for these, just side effecting semantic actions.
--
enter_scope :: { () }
enter_scope : {% Ok(p.enterScope()) }
leave_scope :: { () }
leave_scope : {% Ok(p.leaveScope()) }


block_item_list :: { Vec<CBlockItem> }
block_item_list
  : {- empty -}                 { vec![] }
  | block_item_list block_item  { appended($1, *$2) }

block_item :: { Box<CBlockItem> }
block_item
  : statement                   { box CBlockStmt(*$1) }
  | nested_declaration          { $1 }

nested_declaration :: { Box<CBlockItem> }
nested_declaration
  : declaration                         { box CBlockDecl(*$1) }
  | nested_function_definition          { box CNestedFunDef(*$1) }
  | "__extension__" nested_declaration  { $2 }

nested_function_definition :: { Box<CFunDef> }
nested_function_definition
  : declaration_specifier      function_declarator compound_statement
        {% p.leaveScope(); with_pos!(p, $1, |at| box CFunctionDef($1, $2, vec![], $3, at)) }

  | type_specifier             function_declarator compound_statement
        {% p.leaveScope(); with_pos!(p, $1, |at| box CFunctionDef($1, $2, vec![], $3, at)) }

  | declaration_qualifier_list function_declarator compound_statement
        {% p.leaveScope(); with_pos!(p, $1, |at| box CFunctionDef($1, $2, vec![], $3, at)) }

  | type_qualifier_list   function_declarator compound_statement
        {% p.leaveScope(); with_pos!(p, $1, |at| box CFunctionDef(liftTypeQuals($1), $2, vec![], $3, at)) }

  | type_qualifier_list   attrs function_declarator compound_statement
        {% p.leaveScope(); with_pos!(p, $1, |at| box CFunctionDef(addVecs(liftTypeQuals($1), liftCAttrs($2)),
                                                                  $3, vec![], $4, at)) }


label_declarations :: { Vec<Ident> }
label_declarations
  : "__label__" identifier_list ';'                     { $2 }
  | label_declarations "__label__" identifier_list ';'  { addVecs($1, $3) }


-- parse C expression statement (C99 6.8.3)
--
expression_statement :: { Box<CStat> }
expression_statement
  : ';'                         {% with_pos!(p, $1, |at| box CExpr(None, at)) }
  | expression ';'              {% with_pos!(p, $1, |at| box CExpr(Some($1), at)) }


-- parse C selection statement (C99 6.8.4)
--
selection_statement :: { Box<CStat> }
selection_statement
  : if '(' expression ')' statement
        {% with_pos!(p, $1, |at| box CIf($3, $5, None, at)) }

  | if '(' expression ')' statement else statement
        {% with_pos!(p, $1, |at| box CIf($3, $5, Some($7), at)) }

  | switch '(' expression ')' statement
        {% with_pos!(p, $1, |at| box CSwitch($3, $5, at)) }


-- parse C iteration statement (C99 6.8.5)
--
iteration_statement :: { Box<CStat> }
iteration_statement
  : while '(' expression ')' statement
        {% with_pos!(p, $1, |at| box CWhile($3, $5, false, at)) }

  | do statement while '(' expression ')' ';'
        {% with_pos!(p, $1, |at| box CWhile($5, $2, true, at)) }

  | for '(' expression_opt ';' expression_opt ';' expression_opt ')' statement
        {% with_pos!(p, $1, |at| box CFor(Left($3), $5, $7, $9, at)) }

  | for '(' enter_scope declaration expression_opt ';' expression_opt ')' statement leave_scope
        {% with_pos!(p, $1, |at| box CFor(Right($4), $5, $7, $9, at)) }


-- parse C jump statement (C99 6.8.6)
--
-- * GNU extension: computed gotos
--
jump_statement :: { Box<CStat> }
jump_statement
  : goto identifier ';'                 {% with_pos!(p, $1, |at| box CGoto($2, at)) }
  | goto '*' expression ';'             {% with_pos!(p, $1, |at| box CGotoPtr($3, at)) }
  | continue ';'                        {% with_pos!(p, $1, |at| box CCont(at)) }
  | break ';'                           {% with_pos!(p, $1, |at| box CBreak(at)) }
  | return expression_opt ';'           {% with_pos!(p, $1, |at| box CReturn($2, at)) }


-- parse GNU C __asm__ statement (compatible with C99: J.5.10)
--
-- asm_stmt    :- asm volatile? ( "asm..." : output-operands : input-operands : asm-clobbers )
-- asm_operand :- [operand-name] "constraint" ( expr )
-- asm_clobber :- "r1", "r2", ...
--
asm_statement :: { Box<CAsmStmt> }
asm_statement
  : asm maybe_type_qualifier '(' string_literal ')' ';'
        {% with_pos!(p, $1, |at| box CAssemblyStatement($2, $4, vec![], vec![], vec![], at)) }

  | asm maybe_type_qualifier '(' string_literal ':' asm_operands ')' ';'
        {% with_pos!(p, $1, |at| box CAssemblyStatement($2, $4, $6, vec![], vec![], at)) }

  | asm maybe_type_qualifier '(' string_literal ':' asm_operands ':' asm_operands ')' ';'
        {% with_pos!(p, $1, |at| box CAssemblyStatement($2, $4, $6, $8, vec![], at)) }

  | asm maybe_type_qualifier '(' string_literal ':' asm_operands ':' asm_operands ':' asm_clobbers ')' ';'
        {% with_pos!(p, $1, |at| box CAssemblyStatement($2, $4, $6, $8, $10, at)) }


maybe_type_qualifier :: { Option<Box<CTypeQual>> }
maybe_type_qualifier
  : {- empty -}           { None }
  | type_qualifier        { Some($1) }

asm_operands :: { Vec<CAsmOperand> }
asm_operands
  : {- empty -}                         { vec![] }
  | nonnull_asm_operands                { $1 }

nonnull_asm_operands :: { Vec<CAsmOperand> }
nonnull_asm_operands
  : asm_operand                           { vec![*$1] }
  | nonnull_asm_operands ',' asm_operand  { appended($1, *$3) }

asm_operand :: { Box<CAsmOperand> }
asm_operand
  : string_literal '(' expression ')'
        {% with_pos!(p, $1, |at| box CAssemblyOperand(None, $1, $3, at)) }
  | '[' ident ']' string_literal '(' expression ')'
        {% with_pos!(p, $1, |at| box CAssemblyOperand(Some($2), $4, $6, at)) }
  | '[' tyident ']' string_literal '(' expression ')'
        {% with_pos!(p, $1, |at| box CAssemblyOperand(Some($2), $4, $6, at)) }


asm_clobbers :: { Vec<CStrLit> }
asm_clobbers
  : string_literal                      { vec![*$1] }
  | asm_clobbers ',' string_literal     { appended($1, *$3) }

{-
---------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------
-- Declarations
---------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------

Declarations are the most complicated part of the grammar, and shall be summarized here.
To allow a lightweight notation, we will use the modifier <permute> to indicate that the order of the immidieate right-hand sides doesn't matter.
 - <permute> a* b+ c   === any sequence of a's, b's and c's, which contains exactly 1 'c' and at least one 'b'

-- storage class and type qualifier
---------------------------------------------------------------------------------------------------------------
attr                       :-   __attribute__((..))
storage_class              :-   typedef | extern | static | auto | register | _Thread_local
function_specifier         :-   inline | _Noreturn
alignment_specifier        :-   _Alignas (type_name) | _Alignas (constant_expr)

type_qualifier             :-   const | volatile | restrict | _Atomic | _Nullable | _Nonnull
type_qualifier_list        :-   type_qualifier+

declaration_qualifier      :-   storage_class | type_qualifier | function_specifier | alginment_specifier
declaration_qualifier_list :-   <permute> type_qualifier* storage_class+

qualifiers                 :-   declaration_qualifier_list | type_qualifier_list
                           :=   <permute> (type_qualifier|storage_class)+

-- type names
---------------------------------------------------------------------------------------------------------------
declaration_specifier      :- <permute> type_qualifier* storage_class+ (basic_type_name+ | elaborated_type_name | tyident )
type_specifier             :- <permute> type_qualifier* (basic_type_name+ | elaborated_type_name | tyident)

specifiers                 :- declaration_specifier | type_specifier
                           := <permute> type_qualifier* storage_class* (basic_type_name+ | elaborated_type_name | tyident )

-- struct/union/enum declarations
---------------------------------------------------------------------------------------------------------------
sue_declaration_specifier :- <permute> type_qualifier* storage_class+ elaborated_type_name
sue_type_specifier        :- <permute> type_qualifier* elaborated_type_name

sue_declaration           := sue_declaration_specifier | sue_type_specifier
                          :- <permute> type_qualifier* storage_class* elaborated_type_name

-- declarators
---------------------------------------------------------------------------------------------------------------
identifier_declarator :- ( '*' (type_qualifier | attr)* ) * ident     [ array_decl | "(" parameter-list ")" ]
                               plus additional parenthesis' ending ^^ here
typedef_declartor     :-
declarator            :- identifier_declarator | typedef_declarator

-- Declaration lists
---------------------------------------------------------------------------------------------------------------
default_declaring_list :- qualifiers ( identifier_declarator asm*attrs* initializer? )_comma_list

declaring_list         :- specifiers ( declarator asm*attrs* initializer? )_comma_list

declaration_list := default_declaring_list | declaring_list

-- Declaration
---------------------------------------------------------------------------------------------------------------
declaration = sue_declaration | declaration_list | _Static_assert(..)

---------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------
-- Attributes
-- (citing http://gcc.gnu.org/onlinedocs/gcc/Attribute-Syntax.html)
---------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------

"Attributes may appear after the colon following a label (expect case and default)"

labeled_statement :- identifier ':' attrs_opt statement

"Attributes may go either immediately after the struct/union/enum keyword or after the closing brace"

struct attrs_opt ...
struct ... { } attrs_opt

"In general: Attributes appear as part of declarations, either belonging to a declaration or declarator"

"Any list of specifiers and qualifiers at the start of a declaration may contain attribute specifiers"
"An attribute list may appear immediately before the comma, = or semicolon terminating a declaration of an identifier"

---------------------------------------------------------------------------------------------------------------
For the parser, we modified the following rules to be interleaved with attributes:

default_declaring_list' :-  (declaration_qualifier_list' | type_qualifier_list' attr*)
                                             identifier_declarator asm*attr* initializer?
                                 { ',' attr* identifier_declarator asm*attr* initializer? }
declaring_list' :-          specifier' declarator asm*attr* initializer?
                                 { ',' attr* declarator asm*attr* initializer? }


type_qualifier_list' is like type_qualifier_list, but with preceeding and/or interleaving (but not terminating) __attribute__ annotations.
declaration_qualifier_list', declaration_specifier' and type_specifier' are like their unprimed variants, but with arbitrary preceeding, interleaving and/or terminating __attribute__ annotations.

"An attribute list may appear immediately before a declarator other than the first in a comma seperated list of declarators"

"The attribute specifiers may be the only specifiers present (implicit int)" [not supported]

"Attribute specifiers may be mixed with type qualifiers appearing inside the [] of an parameter array declarator"

tbc.
-}



-- parse C declaration (C11 6.7)
--
-- * new form in C11
--     _Static_assert(expr,string_literal)
declaration :: { Box<CDecl> }
declaration
  : sue_declaration_specifier ';'
        {% with_pos!(p, $1, |at| box CDecl($1, vec![], at)) }

  | sue_type_specifier ';'
        {% with_pos!(p, $1, |at| box CDecl($1, vec![], at)) }

  | declaring_list ';'
        {%
            if let CDecl(declspecs, dies, at) = { *$1 } {
                p.withLength(at, |at| box CDecl(declspecs, revVec(dies), at))
            } else {
                panic!("irrefutable pattern")
            }
        }

  | default_declaring_list ';'
        {%
            if let CDecl(declspecs, dies, at) = { *$1 } {
                p.withLength(at, |at| box CDecl(declspecs, revVec(dies), at))
            } else {
                panic!("irrefutable pattern")
            }
        }
  | "_Static_assert" '(' constant_expression ',' string_literal ')' ';'
        {% with_pos!(p, $1, |at| box CStaticAssert($3, *$5, at)) }

declaration_list :: { Vec<CDecl> }
declaration_list
  : {- empty -}                       { vec![] }
  | declaration_list declaration      { appended($1, *$2) }


-- * SUMMARY: default_declaring_list :- qualifier* identifier_declarator asm_attrs initializer?
--                                                 { ',' identifier_declarator asm_attrs initializer? }
--
-- * GNU extensions
--   __attribute__ annotations imm. before an declarator (see Attribute Syntax, paragraph 11)
--   asm + __attribute__ annotations (end of declarations, see Attribute Syntax, paragraph 12)
--   The assembler annotation is used to specifiy an assembler name for the declarator.
--
default_declaring_list :: { Box<CDecl> }
default_declaring_list
  : declaration_qualifier_list identifier_declarator asm_attrs_opt {-{}-} initializer_opt
        {%
            let declspecs = $1;
            let declr = $2.withAsmNameAttrs(*$3)?;
            p.doDeclIdent(&declspecs, &declr);
            with_pos!(p, declspecs, |at| box CDecl(declspecs, vec![(Some(declr.reverse()), $4, None)], at))
        }

  | type_qualifier_list identifier_declarator asm_attrs_opt {-{}-} initializer_opt
        {%
            let declspecs = liftTypeQuals($1);
            let declr = $2.withAsmNameAttrs(*$3)?;
            p.doDeclIdent(&declspecs, &declr);
            with_pos!(p, declspecs, |at| box CDecl(declspecs, vec![(Some(declr.reverse()), $4, None)], at))
        }

  | type_qualifier_list attrs identifier_declarator asm_attrs_opt {-{}-} initializer_opt -- FIX 1600
        {%
            let declspecs = liftTypeQuals($1);
            let declr = $3.withAsmNameAttrs(*$4)?;
            p.doDeclIdent(&declspecs, &declr);
            with_pos!(p, declspecs, |at| box CDecl(addVecs(declspecs, liftCAttrs($2)),
                                                   vec![(Some(declr.reverse()), $5, None)], at))
        }

  -- GNU extension: __attribute__ as the only qualifier
  | attrs identifier_declarator asm_attrs_opt {-{}-} initializer_opt
        {%
            let declspecs = liftCAttrs($1);
            let declr = $2.withAsmNameAttrs(*$3)?;
            p.doDeclIdent(&declspecs, &declr);
            with_pos!(p, declspecs, |at| box CDecl(declspecs, vec![(Some(declr.reverse()), $4, None)], at))
        }

  | default_declaring_list ',' attrs_opt identifier_declarator asm_attrs_opt {-{}-} initializer_opt
        {%
            if let CDecl(declspecs, dies, at) = { *$1 } {
                let (f, s) = { *$5 };
                let declr = $4.withAsmNameAttrs((f, addVecs(s, $3)))?;
                p.doDeclIdent(&declspecs, &declr);
                p.withLength(at, |at| box CDecl(declspecs, prepend((Some(declr.reverse()), $6, None), dies), at))
            } else {
                panic!("irrefutable pattern")
            }
        }

-- assembler, followed by attribute annotation
asm_attrs_opt :: { Box<(Option<Box<CStrLit>>, Vec<CAttr>)> }
asm_attrs_opt
  : asm_opt attrs_opt    { box ($1, $2) }

--
-- SUMMARY: declaring_list :- specifier* declarator asm_attrs initializer?
--                                 { ',' declarator asm_attrs initializer? }
--
-- GNU extensions:
--      __attribute__ annotations imm. before an declarator (see Attribute Syntax, paragraph 11)
--      asm + __attribute__ annotations (end of declarations, see Attribute Syntax, paragraph 12)
--
declaring_list :: { Box<CDecl> }
declaring_list
  : declaration_specifier declarator asm_attrs_opt initializer_opt
        {%
            let declr = $2.withAsmNameAttrs(*$3)?;
            p.doDeclIdent(&$1, &declr);
            with_pos!(p, $1, |at| box CDecl($1, vec![(Some(declr.reverse()), $4, None)], at))
        }

  | type_specifier declarator asm_attrs_opt initializer_opt
        {%
            let declr = $2.withAsmNameAttrs(*$3)?;
            p.doDeclIdent(&$1, &declr);
            with_pos!(p, $1, |at| box CDecl($1, vec![(Some(declr.reverse()), $4, None)], at))
        }

  | declaring_list ',' attrs_opt declarator asm_attrs_opt initializer_opt
        {%
            if let CDecl(declspecs, dies, at) = { *$1 } {
                let (f, s) = { *$5 };
                let declr = $4.withAsmNameAttrs((f, addVecs(s, $3)))?;
                p.doDeclIdent(&declspecs, &declr);
                Ok(box CDecl(declspecs, prepend((Some(declr.reverse()), $6, None), dies), at))
            } else {
                panic!("irrefutable pattern")
            }
        }


-- parse C declaration specifiers (C99 6.7)
--
-- * <permute> type_qualifier* storage_class+ (basic_type_name+ | elaborated_type_name | tyident )
--
declaration_specifier :: { Vec<CDeclSpec> }
declaration_specifier
  : basic_declaration_specifier         { $1 }  -- Arithmetic or void
  | sue_declaration_specifier           { $1 }  -- Struct/Union/Enum
  | typedef_declaration_specifier       { $1 }  -- Typedef


-- A mixture of type qualifiers (const, volatile, restrict, _Atomic, _Nonnull, _Nullable),
-- function specifiers (inline, _Noreturn),
-- alignment specifiers (_Alignas) and
-- storage class specifiers (extern, static, auto, register, _Thread_local),
-- in any order, but containing at least one storage class specifier.
--
-- declaration_qualifier_list :- <permute> type_qualifier* alignment_specifier* function_specifier* storage_class+
--
-- GNU extensions
--   * arbitrary interleaved __attribute__ annotations
--
declaration_qualifier_list :: { Vec<CDeclSpec> }
declaration_qualifier_list
  : declaration_qualifier_without_types
        { vec![*$1] }

  | attrs declaration_qualifier_without_types
        { appended(liftCAttrs($1), *$2) }

  | type_qualifier_list declaration_qualifier_without_types
        { appended(map(CTypeQual, $1), *$2) }

  | type_qualifier_list attrs declaration_qualifier_without_types
        { appended(addVecs(map(CTypeQual, $1), liftCAttrs($2)), *$3) }

  | declaration_qualifier_list declaration_qualifier
        { appended($1, *$2) }

  | declaration_qualifier_list attr
        { addTrailingAttrs($1, $2) }

--
-- declaration_qualifier :- storage_class | type_qualifier | function_specifier | alignment_specifier
--
declaration_qualifier :: { Box<CDeclSpec> }
declaration_qualifier
  : storage_class                    { box CStorageSpec(*$1) }
  | type_qualifier                   { box CTypeQual(*$1) }
  | function_specifier               { box CFunSpec(*$1) }
  | alignment_specifier              { box CAlignSpec(*$1) }


declaration_qualifier_without_types :: { Box<CDeclSpec> }
  : storage_class                    { box CStorageSpec(*$1) }
  | function_specifier               { box CFunSpec(*$1) }
  | alignment_specifier              { box CAlignSpec(*$1) }


-- parse C storage class specifier (C99 6.7.1)
--
-- * GNU extensions: '__thread' thread local storage
--
storage_class :: { Box<CStorageSpec> }
storage_class
  : typedef                     {% with_pos!(p, $1, |at| box CTypedef(at)) }
  | extern                      {% with_pos!(p, $1, |at| box CExtern(at)) }
  | static                      {% with_pos!(p, $1, |at| box CStatic(at)) }
  | auto                        {% with_pos!(p, $1, |at| box CAuto(at)) }
  | register                    {% with_pos!(p, $1, |at| box CRegister(at)) }
  | "__thread"                  {% with_pos!(p, $1, |at| box CThread(at)) }

-- parse C function specifier (C11 6.7.4)
function_specifier :: { Box<CFunSpec> }
function_specifier
  : inline              {% with_pos!(p, $1, |at| box CInlineQual(at)) }
  | "_Noreturn"         {% with_pos!(p, $1, |at| box CNoreturnQual(at)) }

-- parse C alignment specifier (C11 6.7.5)
alignment_specifier :: { Box<CAlignSpec> }
alignment_specifier
  : alignas '(' type_name ')'           {% with_pos!(p, $1, |at| box CAlignAsType($3, at)) }
  | alignas '(' constant_expression ')' {% with_pos!(p, $1, |at| box CAlignAsExpr($3, at)) }

-- parse C type specifier (C99 6.7.2)
--
-- This recignises a whole list of type specifiers rather than just one
-- as in the C99 grammar.
--
-- type_specifier :- <permute> type_qualifier* (basic_type_name+ | elaborated_type_name | g)
--
-- Type specifier _Atomic(type) is not yet supported because of conflicts with type qualifier _Atomic
type_specifier :: { Vec<CDeclSpec> }
type_specifier
  : basic_type_specifier                { $1 }  -- Arithmetic or void
  | sue_type_specifier                  { $1 }  -- Struct/Union/Enum
  | typedef_type_specifier              { $1 }  -- Typedef
--  | "_Atomic" '(' type_name ')'                         -- _Atomic(type)
--        {% withNodeInfo $1 $ \at -> [CTypeSpec (CAtomicType $3 at)] }

basic_type_name :: { Box<CTypeSpec> }
basic_type_name
  : void                        {% with_pos!(p, $1, |at| box CVoidType(at)) }
  | char                        {% with_pos!(p, $1, |at| box CCharType(at)) }
  | short                       {% with_pos!(p, $1, |at| box CShortType(at)) }
  | int                         {% with_pos!(p, $1, |at| box CIntType(at)) }
  | long                        {% with_pos!(p, $1, |at| box CLongType(at)) }
  | float                       {% with_pos!(p, $1, |at| box CFloatType(at)) }
  | double                      {% with_pos!(p, $1, |at| box CDoubleType(at)) }
  | signed                      {% with_pos!(p, $1, |at| box CSignedType(at)) }
  | unsigned                    {% with_pos!(p, $1, |at| box CUnsigType(at)) }
  | "_Bool"                     {% with_pos!(p, $1, |at| box CBoolType(at)) }
  | "_Complex"                  {% with_pos!(p, $1, |at| box CComplexType(at)) }
  | "__int128"                  {% with_pos!(p, $1, |at| box CInt128Type(at)) }
  | "__float128"                {% with_pos!(p, $1, |at| box CFloat128Type(at)) }


-- A mixture of type qualifiers, storage class and basic type names in any
-- order, but containing at least one basic type name and at least one storage
-- class specifier.
--
-- basic_declaration_specifier :- <permute> type_qualifier* storage_class+ basic_type_name+
--
--   GNU extensions
--     arbitrary interleaved __attribute__ annotations
--
basic_declaration_specifier :: { Vec<CDeclSpec> }
basic_declaration_specifier
  : declaration_qualifier_list basic_type_name
        { appended($1, CTypeSpec(*$2)) }

  | basic_type_specifier storage_class
        { appended($1, CStorageSpec(*$2)) }

  | basic_declaration_specifier declaration_qualifier
        { appended($1, *$2) }

  | basic_declaration_specifier basic_type_name
        { appended($1, CTypeSpec(*$2)) }

  | basic_declaration_specifier attr
        { addTrailingAttrs($1, $2) }


-- A mixture of type qualifiers and basic type names in any order, but
-- containing at least one basic type name.
--
-- basic_type_specifier :- <permute> type_qualifier* basic_type_name+
--
--   GNU extensions
--     arbitrary interleaved __attribute__ annotations
--
basic_type_specifier :: { Vec<CDeclSpec> }
basic_type_specifier
  -- Arithmetic or void
  : basic_type_name
        { vec![CTypeSpec(*$1)] }

  | attrs basic_type_name
        { appended(liftCAttrs($1), CTypeSpec(*$2)) }

  | type_qualifier_list basic_type_name
        { appended(map(CTypeQual, $1), CTypeSpec(*$2)) }

  | type_qualifier_list attrs basic_type_name
        { appended(addVecs(map(CTypeQual, $1), liftCAttrs($2)), CTypeSpec(*$3)) }

  | basic_type_specifier type_qualifier
        { appended($1, CTypeQual(*$2)) }

  | basic_type_specifier basic_type_name
        { appended($1, CTypeSpec(*$2)) }

  | basic_type_specifier attr
        { addTrailingAttrs($1, $2) }


-- A named or anonymous struct, union or enum type along with at least one
-- storage class and any mix of type qualifiers.
--
-- * Summary:
--   sue_declaration_specifier :- <permute> type_qualifier* storage_class+ elaborated_type_name
--
sue_declaration_specifier :: { Vec<CDeclSpec> }
sue_declaration_specifier
  : declaration_qualifier_list elaborated_type_name
        { appended($1, CTypeSpec(*$2)) }

  | sue_type_specifier storage_class
        { appended($1, CStorageSpec(*$2)) }

  | sue_declaration_specifier declaration_qualifier
        { appended($1, *$2) }

  | sue_declaration_specifier attr
        { addTrailingAttrs($1, $2) }


-- A struct, union or enum type (named or anonymous) with optional leading and
-- trailing type qualifiers.
--
-- * Summary:
--   sue_type_specifier :- <permute> type_qualifier* elaborated_type_name
--
-- * GNU Extensions: records __attribute__ annotations
--
sue_type_specifier :: { Vec<CDeclSpec> }
sue_type_specifier
  -- struct/union/enum
  : elaborated_type_name
        { vec![CTypeSpec(*$1)] }

  | attrs elaborated_type_name
        { appended(liftCAttrs($1), CTypeSpec(*$2)) }

  | type_qualifier_list elaborated_type_name
        { appended(map(CTypeQual, $1), CTypeSpec(*$2)) }

  | type_qualifier_list attrs elaborated_type_name
        { appended(addVecs(map(CTypeQual, $1), liftCAttrs($2)), CTypeSpec(*$3)) }

  | sue_type_specifier type_qualifier
        { appended($1, CTypeQual(*$2)) }

  | sue_type_specifier attr
        { addTrailingAttrs($1, $2) }

-- A typedef'ed type identifier with at least one storage qualifier and any
-- number of type qualifiers
--
-- * Summary:
--   typedef_declaration_specifier :- <permute> type_qualifier* storage_class+ tyident
--
-- * Note:
--   the tyident can also be a: typeof '(' ... ')'
--
typedef_declaration_specifier :: { Vec<CDeclSpec> }
typedef_declaration_specifier
  : typedef_type_specifier storage_class
        { appended($1, CStorageSpec(*$2)) }

  | declaration_qualifier_list tyident
        {% with_pos!(p, $2, |at| appended($1, CTypeSpec(CTypeDef($2, at)))) }

  | declaration_qualifier_list typeof '(' expression ')'
        {% with_pos!(p, $2, |at| appended($1, CTypeSpec(CTypeOfExpr($4, at)))) }

  | declaration_qualifier_list typeof '(' type_name ')'
        {% with_pos!(p, $2, |at| appended($1, CTypeSpec(CTypeOfType($4, at)))) }

  | typedef_declaration_specifier declaration_qualifier
        { appended($1, *$2) }

  | typedef_declaration_specifier attr
        { addTrailingAttrs($1, $2) }


-- typedef'ed type identifier with optional leading and trailing type qualifiers
--
-- * Summary:
--   type_qualifier* ( tyident | typeof '('...')' ) type_qualifier*
--
typedef_type_specifier :: { Vec<CDeclSpec> }
typedef_type_specifier
  : tyident
        {% with_pos!(p, $1, |at| vec![CTypeSpec(CTypeDef($1, at))]) }

  | typeof '(' expression ')'
        {% with_pos!(p, $1, |at| vec![CTypeSpec(CTypeOfExpr($3, at))]) }

  | typeof '(' type_name ')'
        {% with_pos!(p, $1, |at| vec![CTypeSpec(CTypeOfType($3, at))]) }

  | type_qualifier_list tyident
        {% with_pos!(p, $2, |at| appended(map(CTypeQual, $1), CTypeSpec(CTypeDef($2, at)))) }

  | type_qualifier_list typeof '(' expression ')'
        {% with_pos!(p, $2, |at| appended(map(CTypeQual, $1), CTypeSpec(CTypeOfExpr($4, at)))) }

  | type_qualifier_list typeof '(' type_name ')'
        {% with_pos!(p, $2, |at| appended(map(CTypeQual, $1), CTypeSpec(CTypeOfType($4, at)))) }

  -- repeat with attrs (this could be easier if type qualifier list wouldn't allow leading attributes)
  | attrs tyident
        {% with_pos!(p, $2, |at| appended(liftCAttrs($1), CTypeSpec(CTypeDef($2, at)))) }

  | attrs typeof '(' expression ')'
        {% with_pos!(p, $1, |at| appended(liftCAttrs($1), CTypeSpec(CTypeOfExpr($4, at)))) }

  | attrs typeof '(' type_name ')'
        {% with_pos!(p, $2, |at| appended(liftCAttrs($1), CTypeSpec(CTypeOfType($4, at)))) }

  | type_qualifier_list attrs tyident
        {% with_pos!(p, $3, |at| appended(addVecs(map(CTypeQual, $1), liftCAttrs($2)),
                                          CTypeSpec(CTypeDef($3, at)))) }

  | type_qualifier_list attrs typeof '(' expression ')'
        {% with_pos!(p, $3, |at| appended(addVecs(map(CTypeQual, $1), liftCAttrs($2)),
                                          CTypeSpec(CTypeOfExpr($5, at)))) }

  | type_qualifier_list attrs typeof '(' type_name ')'
        {% with_pos!(p, $3, |at| appended(addVecs(map(CTypeQual, $1), liftCAttrs($2)),
                                          CTypeSpec(CTypeOfType($5, at)))) }

  | typedef_type_specifier type_qualifier
        { appended($1, CTypeQual(*$2)) }

  | typedef_type_specifier attr
        { addTrailingAttrs($1, $2) }


-- A named or anonymous struct, union or enum type.
--
-- * Summary:
--   (struct|union|enum) (identifier? '{' ... '}' | identifier)
--
elaborated_type_name :: { Box<CTypeSpec> }
elaborated_type_name
  : struct_or_union_specifier   {% with_pos!(p, $1, |at| box CSUType($1, at)) }
  | enum_specifier              {% with_pos!(p, $1, |at| box CEnumType($1, at)) }


-- parse C structure or union declaration (C99 6.7.2.1)
--
-- * Summary:
--    (struct|union) (identifier? '{' ... '}' | identifier)
--
struct_or_union_specifier :: { Box<CStructUnion> }
struct_or_union_specifier
  : struct_or_union attrs_opt identifier '{' struct_declaration_list  '}'
        {% with_pos!(p, $1, |at| box CStructureUnion($1.into_inner(), Some($3), Some($5), $2, at)) }

  | struct_or_union attrs_opt '{' struct_declaration_list  '}'
        {% with_pos!(p, $1, |at| box CStructureUnion($1.into_inner(), None,     Some($4), $2, at)) }

  | struct_or_union attrs_opt identifier
        {% with_pos!(p, $1, |at| box CStructureUnion($1.into_inner(), Some($3), None,     $2, at)) }


struct_or_union :: { Located<CStructTag> }
struct_or_union
  : struct                      { Located::new(CStructTag, $1) }
  | union                       { Located::new(CUnionTag, $1) }


struct_declaration_list :: { Vec<CDecl> }
struct_declaration_list
  : {- empty -}                                         { vec![] }
  | struct_declaration_list ';'                         { $1 }
  | struct_declaration_list struct_declaration          { appended($1, *$2) }


-- parse C structure declaration (C99 6.7.2.1)
--
struct_declaration :: { Box<CDecl> }
struct_declaration
  : struct_declaring_list ';'
        {
            if let CDecl(declspecs, dies, at) = { *$1 } {
                box CDecl(declspecs, revVec(dies), at)
            } else {
                panic!("irrefutable pattern");
            }
        }

  | struct_default_declaring_list';'
        {
            if let CDecl(declspecs, dies, at) = { *$1 } {
                box CDecl(declspecs, revVec(dies), at)
            } else {
                panic!("irrefutable pattern");
            }
        }

  | "__extension__" struct_declaration  { $2 }


--
--  * Note: doesn't redeclare typedef
--
--  TODO: FIXME: AST doesn't allow recording attributes of unnamed struct members
struct_default_declaring_list :: { Box<CDecl> }
struct_default_declaring_list
  : type_qualifier_list attrs_opt struct_identifier_declarator
        {%
            with_pos!(p, $1, match $3 {
                (d, s) => |at| box CDecl(addVecs(liftTypeQuals($1), liftCAttrs($2)), vec![(d, None, s)], at)
            })
        }

  -- GNU extension: __attribute__ as only type qualifier
  | attrs struct_identifier_declarator
        {%
            with_pos!(p, $1, match $2 {
                (d, s) => |at| box CDecl(liftCAttrs($1), vec![(d, None, s)], at),
            })
        }

  -- attrs_opt apply to the declared object
  | struct_default_declaring_list ',' attrs_opt struct_identifier_declarator
        {
            if let CDecl(declspecs, dies, at) = { *$1 } {
                box match $4 {
                    (Some(d), s) => {
                        CDecl(declspecs, prepend((Some(appendObjAttrs($3, d)), None, s), dies), at)
                    },
                    (None, s) => {
                        CDecl(declspecs, prepend((None, None, s), dies), at)
                    },
                }
            } else {
                panic!("irrefutable pattern")
            }
        } -- FIXME

-- * GNU extensions:
--     allow anonymous nested structures and unions
--     FIXME: cannot record attribute of unnamed field
struct_declaring_list :: { Box<CDecl> }
struct_declaring_list
  : type_specifier struct_declarator attrs_opt
        {%
            with_pos!(p, $1, move |at| box match $2 {
                (Some(d), s) => {
                    CDecl($1, vec![(Some(appendObjAttrs($3, d)), None, s)], at)
                },
                (None, s) => {
                    CDecl($1, vec![(None, None, s)], at)
                },
            })
        } {- DO FIXME -}

  | struct_declaring_list ',' attrs_opt struct_declarator attrs_opt
        {
            if let CDecl(declspecs, dies, at) = { *$1 } {
                box match $4 {
                    (Some(d), s) => {
                        CDecl(declspecs, prepend((Some(
                            appendObjAttrs(addVecs($3, $5), d)), None, s), dies), at)
                    },
                    (None, s) => {
                        CDecl(declspecs, prepend((None, None, s), dies), at)
                    },
                }
            } else {
                panic!("irrefutable pattern");
            }
        }

  -- FIXME: We're being far too liberal in the parsing here, we really want to just
  -- allow unnamed struct and union fields but we're actually allowing any
  -- unnamed struct member. Making it allow only unnamed structs or unions in
  -- the parser is far too tricky, it makes things ambiguous. So we'll have to
  -- diagnose unnamed fields that are not structs/unions in a later stage.

  -- Note that a plain type specifier can have a trailing attribute

  | type_specifier
        {% with_pos!(p, $1, |at| box CDecl($1, vec![], at)) }


-- parse C structure declarator (C99 6.7.2.1)
--
struct_declarator :: { (Option<Box<CDeclr>>, Option<Box<CExpr>>) }
struct_declarator
  : declarator                                  { (Some($1.reverse()), None) }
  | ':' constant_expression                     { (None, Some($2)) }
  | declarator ':' constant_expression          { (Some($1.reverse()), Some($3)) }

-- FIXME: anonymous bitfield doesn't allow recording of attributes
struct_identifier_declarator :: { (Option<Box<CDeclr>>, Option<Box<CExpr>>) }
struct_identifier_declarator
  : identifier_declarator                               { (Some($1.reverse()), None) }
  | ':' constant_expression                             { (None, Some($2)) }
  | identifier_declarator ':' constant_expression       { (Some($1.reverse()), Some($3)) }
  | struct_identifier_declarator attr
        {
            match $1 {
                (None, expr) => (None, expr),
                (Some(decl), bsz) => {
                    let CDeclarator(name, derived, asmname, attrs, node) = { *decl };
                    (Some(box CDeclarator(name, derived, asmname, addVecs(attrs, $2), node)), bsz)
                }
            }
        }

-- parse C enumeration declaration (C99 6.7.2.2)
--
-- * Summary:
--   enum (identifier? '{' ... '}' | identifier)
--
enum_specifier :: { Box<CEnum> }
enum_specifier
  : enum attrs_opt '{' enumerator_list '}'
        {% with_pos!(p, $1, |at| box CEnumeration(None,     Some($4), $2, at)) }

  | enum attrs_opt '{' enumerator_list ',' '}'
        {% with_pos!(p, $1, |at| box CEnumeration(None,     Some($4), $2, at)) }

  | enum attrs_opt identifier '{' enumerator_list '}'
        {% with_pos!(p, $1, |at| box CEnumeration(Some($3), Some($5), $2, at)) }

  | enum attrs_opt identifier '{' enumerator_list ',' '}'
        {% with_pos!(p, $1, |at| box CEnumeration(Some($3), Some($5), $2, at)) }

  | enum attrs_opt identifier
        {% with_pos!(p, $1, |at| box CEnumeration(Some($3), None, $2, at)) }

enumerator_list :: { Vec<(Ident, Option<Box<CExpr>>)> }
enumerator_list
  : enumerator                                  { vec![$1] }
  | enumerator_list ',' enumerator              { appended($1, $3) }


enumerator :: { (Ident, Option<Box<CExpr>>) }
enumerator
  : identifier                               { ($1, None) }
  | identifier attrs                         { ($1, None) }
  | identifier attrs '=' constant_expression { ($1, Some($4)) }
  | identifier '=' constant_expression       { ($1, Some($3)) }


-- parse C type qualifier (C11 6.7.3)
--
-- concerning atomic, note:  If the _Atomic keyword is immediately followed by a left
-- parenthesis, it should be interpreted as a type specifier (with a type name), not as a type qualifier
type_qualifier :: { Box<CTypeQual> }
type_qualifier
  : const               {% with_pos!(p, $1, |at| box CConstQual(at)) }
  | volatile            {% with_pos!(p, $1, |at| box CVolatQual(at)) }
  | restrict            {% with_pos!(p, $1, |at| box CRestrQual(at)) }
  | "_Nullable"         {% with_pos!(p, $1, |at| box CNullableQual(at)) }
  | "_Nonnull"          {% with_pos!(p, $1, |at| box CNonnullQual(at)) }
  | "_Atomic"           {% with_pos!(p, $1, |at| box CAtomicQual(at)) }

-- a list containing at least one type_qualifier (const, volatile, restrict, inline, _Noreturn)
--    and additionally CAttrs
type_qualifier_list :: { Vec<CTypeQual> }
type_qualifier_list
  : attrs_opt type_qualifier                 { appended(map(|q| CAttrQual(box q), $1), *$2) }
  | type_qualifier_list type_qualifier       { appended($1, *$2) }
  | type_qualifier_list attrs type_qualifier { appended(addVecs($1, map(|q| CAttrQual(box q), $2)), *$3) }

-- parse C declarator (C99 6.7.5)
--
declarator :: { Box<CDeclrR> }
declarator
  : identifier_declarator               { $1 }
  | typedef_declarator                  { $1 }


-- Parse GNU C's asm annotations
--
-- Those annotations allow to give an assembler name to a function or identifier.
asm_opt :: { Option<Box<CStrLit>> }
asm_opt
  : {- empty -}                         { None }
  | asm '(' string_literal ')'          { Some($3) }

--
-- typedef_declarator :-

typedef_declarator :: { Box<CDeclrR> }
typedef_declarator
  -- would be ambiguous as parameter
  : paren_typedef_declarator            { $1 }

  -- not ambiguous as param
  | parameter_typedef_declarator        { $1 }


-- parameter_typedef_declarator :- tyident declarator_postfix?
--                              | '(' attrs? clean_typedef_declarator ')' declarator_postfix?
--                              |  '*' attrs? type_qualifier_list? parameter_typedef_declarator
--
parameter_typedef_declarator :: { Box<CDeclrR> }
parameter_typedef_declarator
  : tyident
        {% with_pos!(p, $1, |at| CDeclrR::from_var($1, at)) }

  | tyident postfixing_abstract_declarator
        {% with_pos!(p, $1, |at| { $2(CDeclrR::from_var($1, at)) }) }

  | clean_typedef_declarator
        { $1 }


-- The  following have at least one '*'.
-- There is no (redundant) '(' between the '*' and the tyident.
--
-- clean_typedef_declarator :-  '(' attrs? clean_typedef_declarator ')' declarator_postfix?
--                            | '*' attrs? type_qualifier_list? parameter_typedef_declarator
--
clean_typedef_declarator :: { Box<CDeclrR> }
clean_typedef_declarator
  : clean_postfix_typedef_declarator
        { $1 }

  | '*' parameter_typedef_declarator
        {% with_pos!(p, $1, |at| $2.ptrDeclr(vec![], at)) }

  | '*' attrs parameter_typedef_declarator
        {% p.withAttribute($1, $2, |at| $3.ptrDeclr(vec![], at)) }

  | '*' type_qualifier_list  parameter_typedef_declarator
        {% with_pos!(p, $1, |at| $3.ptrDeclr($2, at)) }

  | '*' type_qualifier_list attrs parameter_typedef_declarator
        {% p.withAttribute($1, $3, |at| $4.ptrDeclr($2, at)) }

-- clean_postfix_typedef_declarator :- ( attrs? clean_typedef_declarator ) declarator_postfix?
--
clean_postfix_typedef_declarator :: { Box<CDeclrR> }
clean_postfix_typedef_declarator
  : '(' clean_typedef_declarator ')'                                        { $2 }
  | '(' clean_typedef_declarator ')' postfixing_abstract_declarator         { $4($2) }
  | '(' attrs clean_typedef_declarator ')'                                  { $3.appendAttrs($2) }
  | '(' attrs clean_typedef_declarator ')' postfixing_abstract_declarator   { $5($3).appendAttrs($2) }


-- The following have a redundant '(' placed
-- immediately to the left of the tyident
--
paren_typedef_declarator :: { Box<CDeclrR> }
paren_typedef_declarator
  : paren_postfix_typedef_declarator
        { $1 }

  -- redundant paren
  | '*' '(' simple_paren_typedef_declarator ')'
        {% with_pos!(p, $1, |at| $3.ptrDeclr(vec![], at)) }

  | '*' type_qualifier_list '(' simple_paren_typedef_declarator ')'
        {% with_pos!(p, $1, |at| $4.ptrDeclr($2, at)) }
  | '*' type_qualifier_list attrs '(' simple_paren_typedef_declarator ')'
        {% p.withAttribute($1, $3, |at| $5.ptrDeclr($2, at))  }

  | '*' paren_typedef_declarator
        {% with_pos!(p, $1, |at| $2.ptrDeclr(vec![], at)) }

  | '*' type_qualifier_list paren_typedef_declarator
        {% with_pos!(p, $1, |at| $3.ptrDeclr($2, at)) }
  | '*' type_qualifier_list attrs paren_typedef_declarator
        {% p.withAttribute($1, $3, |at| $4.ptrDeclr($2, at)) }

-- redundant paren to left of tname
paren_postfix_typedef_declarator :: { Box<CDeclrR> }
paren_postfix_typedef_declarator
  : '(' paren_typedef_declarator ')'
        { $2 }

  -- redundant paren
  | '(' simple_paren_typedef_declarator postfixing_abstract_declarator ')'
        { $3($2) }

  | '(' paren_typedef_declarator ')' postfixing_abstract_declarator
        { $4($2) }


-- Just a type name in any number of nested brackets
--
simple_paren_typedef_declarator :: { Box<CDeclrR> }
simple_paren_typedef_declarator
  : tyident
        {% with_pos!(p, &$1, |at| CDeclrR::from_var($1, at)) }

  | '(' simple_paren_typedef_declarator ')'
        { $2 }

--
-- Declarators
-- * Summary
--   declarator :- ( '*' (type_qualifier | attr)* )* ident ( array_decl | "(" parameter-list ")" )?
--      + additional parenthesis
--
identifier_declarator :: { Box<CDeclrR> }
identifier_declarator
  : unary_identifier_declarator                 { $1 }
  | paren_identifier_declarator                 { $1 }


unary_identifier_declarator :: { Box<CDeclrR> }
unary_identifier_declarator
  : postfix_identifier_declarator
        { $1 }

  | '*' identifier_declarator
        {% with_pos!(p, $1, |at| $2.ptrDeclr(vec![], at)) }

  | '*' attrs identifier_declarator
        {% p.withAttribute($1, $2, |at| $3.ptrDeclr(vec![], at)) }

  | '*' type_qualifier_list identifier_declarator
        {% with_pos!(p, $1, |at| $3.ptrDeclr($2, at)) }

  | '*' type_qualifier_list attrs identifier_declarator
        {% p.withAttribute($1, $3, |at| $4.ptrDeclr($2, at)) }

postfix_identifier_declarator :: { Box<CDeclrR> }
postfix_identifier_declarator
  : paren_identifier_declarator postfixing_abstract_declarator
        { $2($1) }

   | '('  unary_identifier_declarator ')'
        { $2 }

   | '(' unary_identifier_declarator ')' postfixing_abstract_declarator
        { $4($2) }

   | '(' attrs unary_identifier_declarator ')'
        { $3.appendAttrs($2) }

   | '(' attrs unary_identifier_declarator ')' postfixing_abstract_declarator
        { $5($3).appendAttrs($2) }


-- just an identifier in any number of nested parenthesis
paren_identifier_declarator :: { Box<CDeclrR> }
paren_identifier_declarator
  : ident
        {% with_pos!(p, $1, |at| CDeclrR::from_var($1, at)) }

  | '(' paren_identifier_declarator ')'
        { $2 }

  | '(' attrs paren_identifier_declarator ')'
        { $3.appendAttrs($2) }

function_declarator_old :: { Box<CDeclr> }
function_declarator_old
  : old_function_declarator
        { $1.reverse() }

old_function_declarator :: { Box<CDeclrR> }
old_function_declarator
  : postfix_old_function_declarator
        { $1 }

  | '*' old_function_declarator
        {% with_pos!(p, $1, |at| $2.ptrDeclr(vec![], at)) } -- FIXME: no attr possible here ???

  | '*' type_qualifier_list old_function_declarator
        {% with_pos!(p, $1, |at| $3.ptrDeclr($2, at)) }

postfix_old_function_declarator :: { Box<CDeclrR> }
postfix_old_function_declarator
  : paren_identifier_declarator '(' identifier_list ')'
        {% with_pos!(p, $1, |at| $1.funDeclr(Left($3), vec![], at)) }

  | '(' old_function_declarator ')'
        { $2 }

  | '(' old_function_declarator ')' postfixing_abstract_declarator
        { $4($2) }


-- parse C parameter type list (C99 6.7.5)
--
parameter_type_list :: { (Vec<CDecl>, bool) }
parameter_type_list
  : {- empty -}                         { (vec![], false) }
  | parameter_list                      { ($1, false) }
  | parameter_list ',' "..."            { ($1, true) }

parameter_list :: { Vec<CDecl> }
parameter_list
  : parameter_declaration                       { vec![*$1] }
  | parameter_list ',' parameter_declaration    { appended($1, *$3) }

parameter_declaration :: { Box<CDecl> }
parameter_declaration
  : declaration_specifier
        {% with_pos!(p, $1, |at| box CDecl($1, vec![], at)) }

  | declaration_specifier abstract_declarator
        {% with_pos!(p, $1, |at| box CDecl($1, vec![(Some($2.reverse()), None, None)], at)) }

  | declaration_specifier identifier_declarator attrs_opt
        {% with_pos!(p, $1, |at| box CDecl($1, vec![(Some($2.appendAttrs($3).reverse()), None, None)], at)) }

  | declaration_specifier parameter_typedef_declarator attrs_opt
        {% with_pos!(p, $1, |at| box CDecl($1, vec![(Some($2.appendAttrs($3).reverse()), None, None)], at)) }

  | declaration_qualifier_list
        {% with_pos!(p, $1, |at| box CDecl($1, vec![], at)) }

  | declaration_qualifier_list abstract_declarator
        {% with_pos!(p, $1, |at| box CDecl($1, vec![(Some($2.reverse()), None, None)], at)) }

  | declaration_qualifier_list identifier_declarator attrs_opt
        {% with_pos!(p, $1, |at| box CDecl($1, vec![(Some($2.appendAttrs($3).reverse()), None, None)], at)) }

  | type_specifier
        {% with_pos!(p, $1, |at| box CDecl($1, vec![], at)) }

  | type_specifier abstract_declarator
        {% with_pos!(p, $1, |at| box CDecl($1, vec![(Some($2.reverse()), None, None)], at)) }

  | type_specifier identifier_declarator attrs_opt
        {% with_pos!(p, $1, |at| box CDecl($1, vec![(Some($2.appendAttrs($3).reverse()), None, None)], at)) }

  | type_specifier parameter_typedef_declarator attrs_opt
        {% with_pos!(p, $1, |at| box CDecl($1, vec![(Some($2.appendAttrs($3).reverse()), None, None)], at)) }

  | type_qualifier_list
        {% with_pos!(p, $1, |at| box CDecl(liftTypeQuals($1), vec![], at)) }
  | type_qualifier_list attr
        {% with_pos!(p, $1, |at| box CDecl(addVecs(liftTypeQuals($1), liftCAttrs($2)), vec![], at)) }

  | type_qualifier_list abstract_declarator
        {% with_pos!(p, $1, |at| box CDecl(liftTypeQuals($1), vec![(Some($2.reverse()), None, None)], at)) }

  | type_qualifier_list identifier_declarator attrs_opt
        {% with_pos!(p, $1, |at| box CDecl(liftTypeQuals($1),
                                           vec![(Some($2.appendAttrs($3).reverse()), None, None)], at)) }


identifier_list :: { Vec<Ident> }
identifier_list
  : ident                               { vec![$1] }
  | identifier_list ',' ident           { appended($1, $3) }


-- parse C type name (C99 6.7.6)
--
type_name :: { Box<CDecl> }
type_name
  :  type_specifier
        {% with_pos!(p, $1, |at| box CDecl($1, vec![], at)) }

  |  type_specifier abstract_declarator
        {% with_pos!(p, $1, |at| box CDecl($1, vec![(Some($2.reverse()), None, None)], at)) }

  |  type_qualifier_list attr
        {% with_pos!(p, $1, |at| box CDecl(addVecs(liftTypeQuals($1), liftCAttrs($2)), vec![], at)) }

  |  type_qualifier_list abstract_declarator
        {% with_pos!(p, $1, |at| box CDecl(liftTypeQuals($1), vec![(Some($2.reverse()), None, None)], at)) }

-- parse C abstract declarator (C99 6.7.6)
--
-- postfix starts with '('
-- postfixing starts with '(' or '['
-- unary start with '*'
abstract_declarator :: { Box<CDeclrR> }
abstract_declarator
  : unary_abstract_declarator       { $1 }
  | postfix_abstract_declarator     { $1 }
  | postfixing_abstract_declarator  { $1(CDeclrR::empty()) }

--
-- FIXME
--  | postfixing_abstract_declarator attrs_opt  { $1(CDeclrR::empty()) }


postfixing_abstract_declarator :: { Box<FnBox(Box<CDeclrR>) -> Box<CDeclrR>> }
postfixing_abstract_declarator
  : array_abstract_declarator
        { $1 }

  | '(' parameter_type_list ')'
        {%
            with_pos!(p, $1, |at| {
                let a: Box<FnBox(Box<CDeclrR>) -> Box<CDeclrR>> = box move |declr: Box<CDeclrR>| {
                    let (params, variadic) = $2;
                    declr.funDeclr(Right((params, variadic)), vec![], at)
                };
                a
            })
        }


-- * TODO: Note that we recognise but ignore the C99 static keyword (see C99 6.7.5.3)
--
-- * TODO: We do not distinguish in the AST between incomplete array types and
-- complete variable length arrays ([ '*' ] means the latter). (see C99 6.7.5.2)
--
array_abstract_declarator :: { Box<FnBox(Box<CDeclrR>) -> Box<CDeclrR>> }
array_abstract_declarator
  : postfix_array_abstract_declarator
        { $1 }

  | array_abstract_declarator postfix_array_abstract_declarator
        { box |decl| { $2($1(decl)) } }

--
-- TODO: record static
postfix_array_abstract_declarator :: { Box<FnBox(Box<CDeclrR>) -> Box<CDeclrR>> }
postfix_array_abstract_declarator
  : '[' assignment_expression_opt ']'
        {%
            with_pos!(p, $1, |at| {
                let a: Box<FnBox(Box<CDeclrR>) -> Box<CDeclrR>> = box |declr: Box<CDeclrR>| {
                    declr.arrDeclr(vec![], false, false, $2, at)
                };
                a
            })
        }

  | '[' attrs assignment_expression_opt ']'
        {% p.withAttributePF($1, $2, |at, declr| declr.arrDeclr(vec![], false, false, $3, at)) }

  | '[' type_qualifier_list assignment_expression_opt ']'
        {%
            with_pos!(p, $1, |at| {
                let a: Box<FnBox(Box<CDeclrR>) -> Box<CDeclrR>> =
                    box |declr: Box<CDeclrR>| declr.arrDeclr($2, false, false, $3, at);
                a
            })
        }

  | '[' type_qualifier_list attrs assignment_expression_opt ']'
        {% p.withAttributePF($1, $3, |at, declr| declr.arrDeclr($2, false, false, $4, at)) }

  | '[' static attrs_opt assignment_expression ']'
        {% p.withAttributePF($1, $3, |at, declr| declr.arrDeclr(vec![], false, true, Some($4), at)) }

  | '[' static type_qualifier_list attrs_opt assignment_expression ']'
        {% p.withAttributePF($1, $4, |at, declr| declr.arrDeclr($3, false, true, Some($5), at)) }

  | '[' type_qualifier_list attrs_opt static attrs_opt assignment_expression ']'
        {% p.withAttributePF($1, addVecs($3, $5), |at, declr| declr.arrDeclr($2, false, true, Some($6), at)) }

  | '[' '*' attrs_opt ']'
        {% p.withAttributePF($1, $3, |at, declr| declr.arrDeclr(vec![], true, false, None, at)) }
  | '[' attrs '*' attrs_opt ']'
        {% p.withAttributePF($1, addVecs($2, $4), |at, declr|
                             declr.arrDeclr(vec![], true, false, None, at)) }

  | '[' type_qualifier_list '*' attrs_opt ']'
        {% p.withAttributePF($1, $4, |at, declr| declr.arrDeclr($2, true, false, None, at)) }
  | '[' type_qualifier_list attrs '*' attrs_opt ']'
        {% p.withAttributePF($1, addVecs($3, $5), |at, declr| declr.arrDeclr($2, true, false, None, at)) }

unary_abstract_declarator :: { Box<CDeclrR> }
unary_abstract_declarator
  : '*'
        {% with_pos!(p, $1, |at| CDeclrR::empty().ptrDeclr(vec![], at)) }

  | '*' type_qualifier_list attrs_opt
        {% p.withAttribute($1, $3, |at| CDeclrR::empty().ptrDeclr($2, at)) }

  | '*' abstract_declarator
        {% with_pos!(p, $1, |at| $2.ptrDeclr(vec![], at)) }

  | '*' type_qualifier_list abstract_declarator
        {% with_pos!(p, $1, |at| $3.ptrDeclr($2, at)) }

  | '*' attrs
        {% p.withAttribute($1, $2, |at| CDeclrR::empty().ptrDeclr(vec![], at)) }
  | '*' attrs abstract_declarator
        {% p.withAttribute($1, $2, |at| $3.ptrDeclr(vec![], at)) }

-- postfix_ad starts with '(', postfixing with '(' or '[', unary_abstract starts with '*'
postfix_abstract_declarator :: { Box<CDeclrR> }
postfix_abstract_declarator
  : '(' unary_abstract_declarator ')'                                   { $2 }
  | '(' postfix_abstract_declarator ')'                                 { $2 }
  | '(' postfixing_abstract_declarator ')'                              { $2(CDeclrR::empty()) }
  | '(' unary_abstract_declarator ')' postfixing_abstract_declarator    { $4($2) }

-- FIX 0700
  | '(' attrs unary_abstract_declarator ')'                                 { $3.appendAttrs($2) }
  | '(' attrs postfix_abstract_declarator ')'                               { $3.appendAttrs($2) }
  | '(' attrs postfixing_abstract_declarator ')'                            { $3(CDeclrR::empty()).appendAttrs($2) }
  | '(' attrs unary_abstract_declarator ')' postfixing_abstract_declarator  { $5($3).appendAttrs($2) }
  | postfix_abstract_declarator attr                                        { $1.appendAttrs($2) }


-- parse C initializer (C99 6.7.8)
--
initializer :: { Box<CInit> }
initializer
  : assignment_expression               {% with_pos!(p, $1, |at| box CInitExpr($1, at)) }
  | '{' initializer_list '}'            {% with_pos!(p, $1, |at| box CInitList($2, at)) }
  | '{' initializer_list ',' '}'        {% with_pos!(p, $1, |at| box CInitList($2, at)) }


initializer_opt :: { Option<Box<CInit>> }
initializer_opt
  : {- empty -}                 { None }
  | '=' initializer             { Some($2) }


initializer_list :: { CInitList }
initializer_list
  : {- empty -}                                         { vec![] }
  | initializer                                         { vec![(vec![], $1)] }
  | designation initializer                             { vec![($1, $2)] }
  | initializer_list ',' initializer                    { appended($1, (vec![], $3)) }
  | initializer_list ',' designation initializer        { appended($1, ($3, $4)) }


-- designation
--
-- * GNU extensions:
--     old style member designation: 'ident :'
--     array range designation
--
designation :: { Vec<CDesignator> }
designation
  : designator_list '='         { $1 }
  | identifier ':'              {% with_pos!(p, $1, |at| vec![CMemberDesig($1, at)]) }
  | array_designator            { vec![*$1] }


designator_list :: { Vec<CDesignator> }
designator_list
 : designator                           { vec![*$1] }
 | designator_list designator           { appended($1, *$2) }


designator :: { Box<CDesignator> }
designator
  : '[' constant_expression ']'         {% with_pos!(p, $1, |at| box CArrDesig($2, at)) }
  | '.' identifier                      {% with_pos!(p, $1, |at| box CMemberDesig($2, at)) }
  | array_designator                    { $1 }


array_designator :: { Box<CDesignator> }
array_designator
  : '[' constant_expression "..." constant_expression ']'
        {% with_pos!(p, $1, |at| box CRangeDesig($2, $4, at)) }


-- parse C primary expression (C11 6.5.1)
--
-- We cannot use a typedef name as a variable
--
-- * C11: generic selection
-- * GNU extensions:
--     allow a compound statement as an expression
--     __builtin_va_arg
--     __builtin_offsetof
--     __builtin_types_compatible_p
-- * Clang extensions:
--     __builtin_convertvector
--
primary_expression :: { Box<CExpr> }
primary_expression
  : ident                {% with_pos!(p, $1, |at| box CVar($1, at)) }
  | constant             { box CConst($1) }
  | string_literal       { box CConst(box liftStrLit(*$1)) }
  | '(' expression ')'   { $2 }
  | "_Generic" '(' assignment_expression ',' generic_assoc_list ')'
        {% with_pos!(p, $1, |at| box CGenericSelection($3, $5, at)) }
  -- GNU extensions
  | '(' compound_statement ')'
        {% with_pos!(p, $1, |at| box CStatExpr($2, at)) }

  | "__builtin_va_arg" '(' assignment_expression ',' type_name ')'
        {% with_pos!(p, $1, |at| box CBuiltinExpr(box CBuiltinVaArg($3, $5, at))) }

  | "__builtin_offsetof" '(' type_name ',' offsetof_member_designator ')'
        {% with_pos!(p, $1, |at| box CBuiltinExpr(box CBuiltinOffsetOf($3, $5, at))) }

  | "__builtin_types_compatible_p" '(' type_name ',' type_name ')'
        {% with_pos!(p, $1, |at| box CBuiltinExpr(box CBuiltinTypesCompatible($3, $5, at))) }

  | "__builtin_convertvector" '(' assignment_expression ',' type_name ')'
        {% with_pos!(p, $1, |at| box CBuiltinExpr(box CBuiltinConvertVector($3, $5, at))) }

-- Generic Selection association list (C11 6.5.1.1)
--
-- TODO: introduce AST type for generic association
generic_assoc_list :: { Vec<(Option<Box<CDecl>>, Box<CExpr>)> }
  : generic_assoc_list ',' generic_assoc { appended($1, $3) }
  | generic_assoc                        { vec![$1] }
generic_assoc :: { (Option<Box<CDecl>>, Box<CExpr>) }
generic_assoc
  : type_name ':' assignment_expression { (Some($1), $3) }
  | default   ':' assignment_expression { (None, $3) }

offsetof_member_designator :: { Vec<CDesignator> }
offsetof_member_designator
  : identifier
        {% with_pos!(p, $1, |at| vec![CMemberDesig($1, at)]) }
  | offsetof_member_designator '.' identifier
        {% with_pos!(p, $3, |at| appended($1, CMemberDesig($3, at))) }
  | offsetof_member_designator '[' expression ']'
        {% with_pos!(p, $3, |at| appended($1, CArrDesig($3, at))) }


-- parse C postfix expression (C99 6.5.2)
--
postfix_expression :: { Box<CExpr> }
postfix_expression
  : primary_expression
        { $1 }

  | postfix_expression '[' expression ']'
        {% with_pos!(p, $1, |at| box CIndex($1, $3, at)) }

  | postfix_expression '(' ')'
        {% with_pos!(p, $1, |at| box CCall($1, vec![], at)) }

  | postfix_expression '(' argument_expression_list ')'
        {% with_pos!(p, $1, |at| box CCall($1, $3, at)) }

  | postfix_expression '.' identifier
        {% with_pos!(p, $1, |at| box CMember($1, $3, false, at)) }

  | postfix_expression "->" identifier
        {% with_pos!(p, $1, |at| box CMember($1, $3, true, at)) }

  | postfix_expression "++"
        {% with_pos!(p, $1, |at| box CUnary(CPostIncOp, $1, at)) }

  | postfix_expression "--"
        {% with_pos!(p, $1, |at| box CUnary(CPostDecOp, $1, at)) }

  | '(' type_name ')' '{' initializer_list '}'
        {% with_pos!(p, $1, |at| box CCompoundLit($2, $5, at)) }

  | '(' type_name ')' '{' initializer_list ',' '}'
        {% with_pos!(p, $1, |at| box CCompoundLit($2, $5, at)) }


argument_expression_list :: { Vec<CExpr> }
argument_expression_list
  : assignment_expression                               { vec![*$1] }
  | argument_expression_list ',' assignment_expression  { appended($1, *$3) }


-- parse C unary expression (C99 6.5.3)
--
-- * GNU extensions:
--     'alignof' expression or type
--     '__real' and '__imag' expression
--     '__extension__' to suppress warnings about extensions
--     allow taking address of a label with: && label
--
unary_expression :: { Box<CExpr> }
unary_expression
  : postfix_expression                  { $1 }
  | "++" unary_expression               {% with_pos!(p, $1, |at| box CUnary(CPreIncOp, $2, at)) }
  | "--" unary_expression               {% with_pos!(p, $1, |at| box CUnary(CPreDecOp, $2, at)) }
  | "__extension__" cast_expression     { $2 }
  | unary_operator cast_expression      {% with_pos!(p, $1, |at| box CUnary($1.into_inner(), $2, at)) }
  | sizeof unary_expression             {% with_pos!(p, $1, |at| box CSizeofExpr($2, at)) }
  | sizeof '(' type_name ')'            {% with_pos!(p, $1, |at| box CSizeofType($3, at)) }
  -- GNU: alignof, complex and && extension
  | alignof unary_expression            {% with_pos!(p, $1, |at| box CAlignofExpr($2, at)) }
  | alignof '(' type_name ')'           {% with_pos!(p, $1, |at| box CAlignofType($3, at)) }
  | "__real__" unary_expression         {% with_pos!(p, $1, |at| box CComplexReal($2, at)) }
  | "__imag__" unary_expression         {% with_pos!(p, $1, |at| box CComplexImag($2, at)) }
  | "&&" identifier                     {% with_pos!(p, $1, |at| box CLabAddrExpr($2, at)) }


unary_operator :: { Located<CUnaryOp> }
unary_operator
  : '&'         { Located::new(CAdrOp,  $1) }
  | '*'         { Located::new(CIndOp,  $1) }
  | '+'         { Located::new(CPlusOp, $1) }
  | '-'         { Located::new(CMinOp,  $1) }
  | '~'         { Located::new(CCompOp, $1) }
  | '!'         { Located::new(CNegOp,  $1) }


-- parse C cast expression (C99 6.5.4)
--
cast_expression :: { Box<CExpr> }
cast_expression
  : unary_expression                    { $1 }
  | '(' type_name ')' cast_expression   {% with_pos!(p, $1, |at| box CCast($2, $4, at)) }


-- parse C multiplicative expression (C99 6.5.5)
--
multiplicative_expression :: { Box<CExpr> }
multiplicative_expression
  : cast_expression
        { $1 }

  | multiplicative_expression '*' cast_expression
        {% with_pos!(p, $1, |at| box CBinary(CMulOp, $1, $3, at)) }

  | multiplicative_expression '/' cast_expression
        {% with_pos!(p, $1, |at| box CBinary(CDivOp, $1, $3, at)) }

  | multiplicative_expression '%' cast_expression
        {% with_pos!(p, $1, |at| box CBinary(CRmdOp, $1, $3, at)) }


-- parse C additive expression (C99 6.5.6)
--
additive_expression :: { Box<CExpr> }
additive_expression
  : multiplicative_expression
        { $1 }

  | additive_expression '+' multiplicative_expression
        {% with_pos!(p, $1, |at| box CBinary(CAddOp, $1, $3, at)) }

  | additive_expression '-' multiplicative_expression
        {% with_pos!(p, $1, |at| box CBinary(CSubOp, $1, $3, at)) }


-- parse C shift expression (C99 6.5.7)
--
shift_expression :: { Box<CExpr> }
shift_expression
  : additive_expression
        { $1 }

  | shift_expression "<<" additive_expression
        {% with_pos!(p, $1, |at| box CBinary(CShlOp, $1, $3, at)) }

  | shift_expression ">>" additive_expression
        {% with_pos!(p, $1, |at| box CBinary(CShrOp, $1, $3, at)) }


-- parse C relational expression (C99 6.5.8)
--
relational_expression :: { Box<CExpr> }
relational_expression
  : shift_expression
        { $1 }

  | relational_expression '<' shift_expression
        {% with_pos!(p, $1, |at| box CBinary(CLeOp, $1, $3, at)) }

  | relational_expression '>' shift_expression
        {% with_pos!(p, $1, |at| box CBinary(CGrOp, $1, $3, at)) }

  | relational_expression "<=" shift_expression
        {% with_pos!(p, $1, |at| box CBinary(CLeqOp, $1, $3, at)) }

  | relational_expression ">=" shift_expression
        {% with_pos!(p, $1, |at| box CBinary(CGeqOp, $1, $3, at)) }


-- parse C equality expression (C99 6.5.9)
--
equality_expression :: { Box<CExpr> }
equality_expression
  : relational_expression
        { $1 }

  | equality_expression "==" relational_expression
        {% with_pos!(p, $1, |at| box CBinary(CEqOp, $1, $3, at)) }

  | equality_expression "!=" relational_expression
        {% with_pos!(p, $1, |at| box CBinary(CNeqOp, $1, $3, at)) }


-- parse C bitwise and expression (C99 6.5.10)
--
and_expression :: { Box<CExpr> }
and_expression
  : equality_expression
        { $1 }

  | and_expression '&' equality_expression
        {% with_pos!(p, $1, |at| box CBinary(CAndOp, $1, $3, at)) }


-- parse C bitwise exclusive or expression (C99 6.5.11)
--
exclusive_or_expression :: { Box<CExpr> }
exclusive_or_expression
  : and_expression
        { $1 }

  | exclusive_or_expression '^' and_expression
        {% with_pos!(p, $1, |at| box CBinary(CXorOp, $1, $3, at)) }


-- parse C bitwise or expression (C99 6.5.12)
--
inclusive_or_expression :: { Box<CExpr> }
inclusive_or_expression
  : exclusive_or_expression
        { $1 }

  | inclusive_or_expression '|' exclusive_or_expression
        {% with_pos!(p, $1, |at| box CBinary(COrOp, $1, $3, at)) }


-- parse C logical and expression (C99 6.5.13)
--
logical_and_expression :: { Box<CExpr> }
logical_and_expression
  : inclusive_or_expression
        { $1 }

  | logical_and_expression "&&" inclusive_or_expression
        {% with_pos!(p, $1, |at| box CBinary(CLndOp, $1, $3, at)) }


-- parse C logical or expression (C99 6.5.14)
--
logical_or_expression :: { Box<CExpr> }
logical_or_expression
  : logical_and_expression
        { $1 }

  | logical_or_expression "||" logical_and_expression
        {% with_pos!(p, $1, |at| box CBinary(CLorOp, $1, $3, at)) }


-- parse C conditional expression (C99 6.5.15)
--
-- * GNU extensions:
--     omitting the `then' part
conditional_expression :: { Box<CExpr> }
conditional_expression
  : logical_or_expression
        { $1 }

  | logical_or_expression '?' expression ':' conditional_expression
        {% with_pos!(p, $1, |at| box CCond($1, Some($3), $5, at)) }

  | logical_or_expression '?' ':' conditional_expression
        {% with_pos!(p, $1, |at| box CCond($1, None, $4, at)) }


-- parse C assignment expression (C99 6.5.16)
--
-- * NOTE: LHS of assignment is more restricted than in gcc.
--         `x ? y : z = 3' parses in gcc as `(x ? y : z) = 3',
--         but `x ? y : z' is not an unary expression.
assignment_expression :: { Box<CExpr> }
assignment_expression
  : conditional_expression
        { $1 }

  | unary_expression assignment_operator assignment_expression
        {% with_pos!(p, $1, |at| box CAssign($2.into_inner(), $1, $3, at)) }


assignment_operator :: { Located<CAssignOp> }
assignment_operator
  : '='                 { Located::new(CAssignOp, $1) }
  | "*="                { Located::new(CMulAssOp, $1) }
  | "/="                { Located::new(CDivAssOp, $1) }
  | "%="                { Located::new(CRmdAssOp, $1) }
  | "+="                { Located::new(CAddAssOp, $1) }
  | "-="                { Located::new(CSubAssOp, $1) }
  | "<<="               { Located::new(CShlAssOp, $1) }
  | ">>="               { Located::new(CShrAssOp, $1) }
  | "&="                { Located::new(CAndAssOp, $1) }
  | "^="                { Located::new(CXorAssOp, $1) }
  | "|="                { Located::new(COrAssOp,  $1) }


-- parse C expression (C99 6.5.17)
--
expression :: { Box<CExpr> }
expression
  : assignment_expression
        { $1 }

  | assignment_expression ',' comma_expression
        {% with_pos!(p, $3, |at| box CComma(prepend(*$1, $3), at)) }

comma_expression :: { Vec<CExpr> }
comma_expression
  : assignment_expression                       { vec![*$1] }
  | comma_expression ',' assignment_expression  { appended($1, *$3) }


-- The following was used for clarity
expression_opt :: { Option<Box<CExpr>> }
expression_opt
  : {- empty -}         { None }
  | expression          { Some($1) }


-- The following was used for clarity
assignment_expression_opt :: { Option<Box<CExpr>> }
assignment_expression_opt
  : {- empty -}                         { None }
  | assignment_expression               { Some($1) }


-- parse C constant expression (C99 6.6)
--
constant_expression :: { Box<CExpr> }
constant_expression
  : conditional_expression              { $1 }


-- parse C constants
--
constant :: { Box<CConst> }
constant
  : cint        {%
                    with_pos!(p, $1, move |at| {
                        if let CTokILit(_, i) = {$1} {
                            box CIntConst(i, at)
                        } else {
                            panic!("irrefutable pattern")
                        }
                    })
                }
  | cchar       {%
                    with_pos!(p, $1, move |at| {
                        if let CTokCLit(_, c) = {$1} {
                            box CCharConst(c, at)
                        } else {
                            panic!("irrefutable pattern")
                        }
                    })
                }
  | cfloat      {%
                    with_pos!(p, $1, move |at| {
                        if let CTokFLit(_, f) = {$1} {
                            box CFloatConst(f, at)
                        } else {
                            panic!("irrefutable pattern")
                        }
                    })
                }


string_literal :: { Box<CStrLit> }
string_literal
  : cstr
        {%
            with_pos!(p, $1, move |at| {
                if let CTokSLit(_, s) = {$1} {
                    box CStringLiteral(s, at)
                } else {
                    panic!("irrefutable pattern")
                }
            })
        }

  | cstr string_literal_list
        {%
            with_pos!(p, $1, move |at| {
                if let CTokSLit(_, s) = $1 {
                    box CStringLiteral(concatCStrings(prepend(s, $2)), at)
                } else {
                    panic!("irrefutable pattern")
                }
            })
        }


string_literal_list :: { Vec<CString> }
string_literal_list
  : cstr                        {
                                    if let CTokSLit(_, s) = $1 {
                                        vec![s]
                                    } else {
                                        panic!("irrefutable pattern")
                                    }
                                }
  | string_literal_list cstr    {
                                    if let CTokSLit(_, s) = $2 {
                                        appended($1, s)
                                    } else {
                                        panic!("irrefutable pattern")
                                    }
                                }

clang_version_literal :: { ClangCVersion }
  : clangcversion       { $1 }

identifier :: { Ident }
identifier
  : ident               { $1 }
  | tyident             { $1 }


-- parse GNU C attribute annotation
attrs_opt ::    { Vec<CAttr> }
attrs_opt
  : {- empty -}                                 { vec![] }
  | attrs                                       { $1 }

-- GNU C attribute annotation
attrs :: { Vec<CAttr> }
attrs
  : attr            { $1 }
  | attrs attr      { addVecs($1, $2) }

attr :: { Vec<CAttr> }
attr
  : "__attribute__" '(' '(' attribute_list ')' ')'      { $4 }

attribute_list :: { Vec<CAttr> }
  : attribute                      { $1.map_or(vec![], |a| vec![*a]) }
  | attribute_list ',' attribute   { match $3 {
                                         None => $1,
                                         Some(a) => appended($1, *a),
                                     }
                                   }


attribute :: { Option<Box<CAttr>> }
attribute
  : {- empty -}                         { None }
  | ident
        {% with_pos!(p, $1, |at| Some(box CAttribute($1, vec![], at))) }
  | const
        {% with_pos!(p, $1, |at| Some(box CAttribute(Ident::internal("const".into()), vec![], at))) }
  | ident '(' attribute_params ')'
        {% with_pos!(p, $1, |at| Some(box CAttribute($1, $3, at))) }
  | ident '(' ')'
        {% with_pos!(p, $1, |at| Some(box CAttribute($1, vec![], at))) }

-- OS X 10.9 (Mavericks) makes use of more liberal attribute syntax
-- that includes assignment-like expressions referencing version
-- numbers.

attribute_params :: { Vec<CExpr> }
attribute_params
  : constant_expression                                        { vec![*$1] }
  | unary_expression assignment_operator clang_version_literal { vec![] }
  | unary_expression assignment_operator unary_expression      { vec![] }
  | attribute_params ',' constant_expression                   { appended($1, *$3) }
  | attribute_params ',' unary_expression assignment_operator unary_expression      { $1 }
  | attribute_params ',' unary_expression assignment_operator clang_version_literal { $1 }

{

#[inline]
fn revVec<T>(mut a: Vec<T>) -> Vec<T> {
    a.reverse();
    a
}

#[inline]
fn prepend<T>(t: T, mut a: Vec<T>) -> Vec<T> {
    a.insert(0, t);
    a
}

#[inline]
fn addVecs<T>(mut a: Vec<T>, mut b: Vec<T>) -> Vec<T> {
    a.append(&mut b);
    a
}

#[inline]
fn appended<T>(mut a: Vec<T>, b: T) -> Vec<T> {
    a.push(b);
    a
}

#[inline]
fn map<T, U, F: Fn(T) -> U>(f: F, a: Vec<T>) -> Vec<U> {
    a.into_iter().map(f).collect()
}

#[inline]
fn liftTypeQuals(quals: Vec<CTypeQual>) -> Vec<CDeclSpec> {
    map(CTypeQual, quals)
}

#[inline]
fn liftCAttrs(attrs: Vec<CAttribute<NodeInfo>>) -> Vec<CDeclSpec> {
    map(|attr| CTypeQual(CAttrQual(box attr)), attrs)
}

fn appendObjAttrs(newAttrs: Vec<CAttribute<NodeInfo>>,
                  box CDeclarator(ident, indirections, asmname, cAttrs, at): Box<CDeclarator<NodeInfo>>)
                  -> Box<CDeclarator<NodeInfo>> {
    box CDeclarator(ident, indirections, asmname, addVecs(cAttrs, newAttrs), at)
}

fn addTrailingAttrs(mut declspecs: Vec<CDeclSpec>, mut new_attrs: Vec<CAttr>) -> Vec<CDeclSpec> {
    let is_new = match declspecs.last_mut() {
        Some(&mut CTypeSpec(CSUType(box CStructureUnion(_, _, _, ref mut def_attrs, _), _))) => {
            def_attrs.append(&mut new_attrs);
            false
        }
        Some(&mut CTypeSpec(CEnumType(box CEnumeration(_, _, ref mut def_attrs, _), _))) => {
            def_attrs.append(&mut new_attrs);
            false
        }
        _ => true
    };
    if is_new {
        declspecs.append(&mut liftCAttrs(new_attrs));
    }
    declspecs
}

fn happyError<T>(p: &mut Parser) -> Res<T> {
    parseError(p)
}

pub fn translUnitP(p: &mut Parser) -> Res<CTranslationUnit<NodeInfo>> {
    translation_unit(p).map(|b| *b)
}

pub fn extDeclP(p: &mut Parser) -> Res<CExtDecl> {
    external_declaration(p).map(|b| *b)
}

pub fn statementP(p: &mut Parser) -> Res<CStat> {
    statement(p).map(|b| *b)
}

pub fn expressionP(p: &mut Parser) -> Res<CExpr> {
    expression(p).map(|b| *b)
}

}
