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

#[macro_use] use corollary_support::*;
use std::boxed::FnBox;
use std::rc::Rc;

use parser_c_macro::refute;

use data::input_stream::*;
use data::position::*;
use parser::parser_monad::*;
use syntax::ast::*;
use syntax::constants::*;
use parser::tokens::*;
use data::r_list::RList::*;
use data::r_list::Reversed;
use data::node::*;
use data::r_list::snoc;
use data::ident::*;
use data::name::*;
use syntax::ops::*;
use parser::lexer::{lexC, parseError};
use parser::builtin::builtinTypeNames;
use data::name::namesStartingFrom;

// fn(A, B) -> fn(C) -> {eval fn(A, B, C)}
macro_rules! partial_1 {
    ($inner: expr) => ( box $inner );
    ($inner: expr, $($arg: expr),+ ) => ( box move |_0| { $inner($($arg),+ , _0) } )
}
macro_rules! partial_5 {
    ($inner: expr, $($arg: expr),+ ) => (
        box move |_0, _1, _2, _3, _4| { $inner($($arg),+ , _0, _1, _2, _3, _4) }
    )
}
macro_rules! curry_1_5 {
    ($inner: expr) => (
        box move |_0, _1, _2, _3, _4, _5| { ($inner(_0))(_1, _2, _3, _4, _5) };
    )
}
macro_rules! curry_5_1 {
    ($inner: expr) => (
        box move |_0, _1, _2, _3, _4, _5| { ($inner(_0, _1, _2, _3, _4))(_5) };
    )
}
macro_rules! apply_5_1_clone {
    ($inner: expr) => (
        box move |_0, _1, _2, _3, _4| {
            let a_ = $inner.clone();
            box move |_5| { a_(_0, _1, _2.clone(), _3.clone(), _4.clone(), _5) }
        }
    )
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

%monad { P } { >>= } { return }
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
"__attribute__" { CTokGnuC(GnuCAttrTok, _) }    -- special GNU C tokens
"__extension__" { CTokGnuC(GnuCExtTok,  _) }    -- special GNU C tokens
"__real__"      { CTokGnuC(GnuCComplexReal, _) }
"__imag__"      { CTokGnuC(GnuCComplexImag, _) }
-- special GNU C builtin 'functions' that actually take types as parameters:
"__builtin_va_arg"              { CTokGnuC(GnuCVaArg, _) }
"__builtin_offsetof"            { CTokGnuC(GnuCOffsetof, _) }
"__builtin_types_compatible_p"  { CTokGnuC(GnuCTyCompat, _) }
clangcversion   { CTokClangC(_, ClangCTok($$)) } -- Clang version literal

%%


-- parse a complete C translation unit
-- we have to take special care of empty translation units
translation_unit :: { CTranslUnit }
translation_unit
  : ext_decl_list {%
                      let decls = reverse($1);
                      if decls.len() == 0 {
                          thenP(getNewName(), box |n: Name| {
                              thenP(getCurrentPosition(), box move |p: Position| {
                                  let nodeinfo = NodeInfo::new(p.clone(), (p, 0), n);
                                  returnP(CTranslationUnit(decls, nodeinfo))
                              })
                          })
                      } else {
                          let d = decls[0].clone();
                          withNodeInfo(d, box |_0| CTranslationUnit(decls, _0))
                      }
                  }


-- parse a list of external declarations, making up a C translation unit (C99 6.9)
--
-- * GNU extensions:
--     allow empty translation_unit
--     allow redundant ';'
ext_decl_list :: { Reversed<Vec<CExtDecl>> }
ext_decl_list
  : {- empty -}                         { empty() }
  | ext_decl_list ';'                   { $1 }
  | ext_decl_list external_declaration  { snoc($1, $2) }


-- parse external C declaration (C99 6.9)
--
-- * GNU extensions:
--     allow extension keyword before external declaration (TODO: discarded)
--     asm definitions
external_declaration :: { CExtDecl }
external_declaration
  : function_definition                           { CFDefExt($1) }
  | declaration                                   { CDeclExt($1) }
  | "__extension__" external_declaration          { $2 }
  | asm '(' string_literal ')' ';'                {% withNodeInfo($1.clone(), partial_1!(CAsmExt, $3)) }


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
function_definition :: { CFunDef }
function_definition
  :                            function_declarator compound_statement
        {% seqP(leaveScope(), withNodeInfo($1.clone(), partial_1!(
            CFunctionDef, vec![], $1, vec![], $2))) }

  | attrs                      function_declarator compound_statement
        {% seqP(leaveScope(), withNodeInfo($1.clone(), partial_1!(
            CFunctionDef, liftCAttrs($1), $2, vec![], $3))) }

  | declaration_specifier      function_declarator compound_statement
        {% seqP(leaveScope(), withNodeInfo($1.clone(), partial_1!(
            CFunctionDef, $1, $2, vec![], $3))) }

  | type_specifier             function_declarator compound_statement
        {% seqP(leaveScope(), withNodeInfo($1.clone(), partial_1!(
            CFunctionDef, $1, $2, vec![], $3))) }

  | declaration_qualifier_list function_declarator compound_statement
        {% seqP(leaveScope(), withNodeInfo($1.clone(), partial_1!(
            CFunctionDef, reverse($1), $2, vec![], $3))) }

  | type_qualifier_list        function_declarator compound_statement
        {% seqP(leaveScope(), withNodeInfo($1.clone(), partial_1!(
            CFunctionDef, liftTypeQuals($1), $2, vec![], $3))) }

  | type_qualifier_list  attrs function_declarator compound_statement
        {% seqP(leaveScope(), withNodeInfo($1.clone(), partial_1!(
            CFunctionDef, __op_addadd(liftTypeQuals($1), liftCAttrs($2)), $3, vec![], $4))) }

  -- old function declarators

  |                            function_declarator_old declaration_list compound_statement
        {% withNodeInfo($1.clone(), partial_1!(CFunctionDef, vec![], $1, reverse($2), $3)) }

  |                      attrs function_declarator_old declaration_list compound_statement
        {% withNodeInfo($2.clone(), partial_1!(CFunctionDef, liftCAttrs($1), $2, reverse($3), $4)) }

  | declaration_specifier      function_declarator_old declaration_list compound_statement
        {% withNodeInfo($1.clone(), partial_1!(CFunctionDef, $1, $2, reverse($3), $4)) }

  | type_specifier             function_declarator_old declaration_list compound_statement
        {% withNodeInfo($1.clone(), partial_1!(CFunctionDef, $1, $2, reverse($3), $4)) }

  | declaration_qualifier_list function_declarator_old declaration_list compound_statement
        {% withNodeInfo($1.clone(), partial_1!(CFunctionDef, reverse($1), $2, reverse($3), $4)) }

  | type_qualifier_list   function_declarator_old declaration_list compound_statement
        {% withNodeInfo($1.clone(), partial_1!(CFunctionDef, liftTypeQuals($1), $2, reverse($3), $4)) }

  | type_qualifier_list attrs  function_declarator_old declaration_list compound_statement
        {% withNodeInfo($1.clone(), partial_1!(
            CFunctionDef, __op_addadd(liftTypeQuals($1), liftCAttrs($2)), $3, reverse($4), $5)) }

-- Read declarator and put function
function_declarator :: { CDeclr }
function_declarator
  : identifier_declarator
        {%
            let declr = reverseDeclr($1);
            seqP(enterScope(), seqP(doFuncParamDeclIdent(declr.clone()), returnP(declr)))
        }


-- parse C statement (C99 6.8)
--
-- * GNU extension: ' __asm__ (...); ' statements
--
statement :: { CStat }
statement
  : labeled_statement           { $1 }
  | compound_statement          { $1 }
  | expression_statement        { $1 }
  | selection_statement         { $1 }
  | iteration_statement         { $1 }
  | jump_statement              { $1 }
  | asm_statement               {% withNodeInfo($1.clone(), partial_1!(CAsm, $1)) }


-- parse C labeled statement (C99 6.8.1)
--
-- * GNU extension: case ranges
--
labeled_statement :: { CStat }
labeled_statement
  : identifier ':' attrs_opt statement          {% withNodeInfo($1.clone(), partial_1!(CLabel, $1, box $4, $3)) }
  | case constant_expression ':' statement      {% withNodeInfo($1, partial_1!(CCase, $2, box $4)) }
  | default ':' statement                       {% withNodeInfo($1, partial_1!(CDefault, box $3)) }
  | case constant_expression "..." constant_expression ':' statement
        {% withNodeInfo($1, partial_1!(CCases, $2, $4, box $6)) }


-- parse C compound statement (C99 6.8.2)
--
-- * GNU extension: '__label__ ident;' declarations
--
compound_statement :: { CStat }
compound_statement
  : '{' enter_scope block_item_list leave_scope '}'
        {% withNodeInfo($1, partial_1!(CCompound, vec![], reverse($3))) }

  | '{' enter_scope label_declarations block_item_list leave_scope '}'
        {% withNodeInfo($1, partial_1!(CCompound, reverse($3), reverse($4))) }


-- No syntax for these, just side effecting semantic actions.
--
enter_scope :: { () }
enter_scope : {% enterScope() }
leave_scope :: { () }
leave_scope : {% leaveScope() }


block_item_list :: { Reversed<Vec<CBlockItem>> }
block_item_list
  : {- empty -}                 { empty() }
  | block_item_list block_item  { snoc($1, $2) }

block_item :: { CBlockItem }
block_item
  : statement                   { CBlockStmt($1) }
  | nested_declaration          { $1 }

nested_declaration :: { CBlockItem }
nested_declaration
  : declaration                         { CBlockDecl($1) }
  | nested_function_definition          { CNestedFunDef($1) }
  | "__extension__" nested_declaration  { $2 }

nested_function_definition :: { CFunDef }
nested_function_definition
  : declaration_specifier      function_declarator compound_statement
        {% seqP(leaveScope(), withNodeInfo($1.clone(), partial_1!(
            CFunctionDef, $1, $2, vec![], $3))) }

  | type_specifier             function_declarator compound_statement
        {% seqP(leaveScope(), withNodeInfo($1.clone(), partial_1!(
            CFunctionDef, $1, $2, vec![], $3))) }

  | declaration_qualifier_list function_declarator compound_statement
        {% seqP(leaveScope(), withNodeInfo($1.clone(), partial_1!(
            CFunctionDef, reverse($1), $2, vec![], $3))) }

  | type_qualifier_list   function_declarator compound_statement
        {% seqP(leaveScope(), withNodeInfo($1.clone(), partial_1!(
            CFunctionDef, liftTypeQuals($1), $2, vec![], $3))) }

  | type_qualifier_list   attrs function_declarator compound_statement
        {% seqP(leaveScope(), withNodeInfo($1.clone(), partial_1!(
            CFunctionDef, __op_addadd(liftTypeQuals($1), liftCAttrs($2)), $3, vec![], $4))) }


label_declarations :: { Reversed<Vec<Ident>> }
label_declarations
  : "__label__" identifier_list ';'                     { $2  }
  | label_declarations "__label__" identifier_list ';'  { rappendr($1, $3) }


-- parse C expression statement (C99 6.8.3)
--
expression_statement :: { CStat }
expression_statement
  : ';'                         {% withNodeInfo($1, partial_1!(CExpr, None)) }
  | expression ';'              {% withNodeInfo($1.clone(), partial_1!(CExpr, Some($1))) }


-- parse C selection statement (C99 6.8.4)
--
selection_statement :: { CStat }
selection_statement
  : if '(' expression ')' statement
        {% withNodeInfo($1, partial_1!(CIf, $3, box $5, None)) }

  | if '(' expression ')' statement else statement
        {% withNodeInfo($1, partial_1!(CIf, $3, box $5, Some(box $7))) }

  | switch '(' expression ')' statement
        {% withNodeInfo($1, partial_1!(CSwitch, $3, box $5)) }


-- parse C iteration statement (C99 6.8.5)
--
iteration_statement :: { CStat }
iteration_statement
  : while '(' expression ')' statement
        {% withNodeInfo($1, partial_1!(CWhile, $3, box $5, false)) }

  | do statement while '(' expression ')' ';'
        {% withNodeInfo($1, partial_1!(CWhile, $5, box $2, true)) }

  | for '(' expression_opt ';' expression_opt ';' expression_opt ')' statement
        {% withNodeInfo($1, partial_1!(CFor, Left($3), $5, $7, box $9)) }

  | for '(' enter_scope declaration expression_opt ';' expression_opt ')' statement leave_scope
        {% withNodeInfo($1, partial_1!(CFor, Right($4), $5, $7, box $9)) }


-- parse C jump statement (C99 6.8.6)
--
-- * GNU extension: computed gotos
--
jump_statement :: { CStat }
jump_statement
  : goto identifier ';'                 {% withNodeInfo($1, partial_1!(CGoto, $2)) }
  | goto '*' expression ';'             {% withNodeInfo($1, partial_1!(CGotoPtr, $3)) }
  | continue ';'                        {% withNodeInfo($1, partial_1!(CCont)) }
  | break ';'                           {% withNodeInfo($1, partial_1!(CBreak)) }
  | return expression_opt ';'           {% withNodeInfo($1, partial_1!(CReturn, $2)) }


-- parse GNU C __asm__ statement (compatible with C99: J.5.10)
--
-- asm_stmt    :- asm volatile? ( "asm..." : output-operands : input-operands : asm-clobbers )
-- asm_operand :- [operand-name] "constraint" ( expr )
-- asm_clobber :- "r1", "r2", ...
--
asm_statement :: { CAsmStmt }
asm_statement
  : asm maybe_type_qualifier '(' string_literal ')' ';'
        {% withNodeInfo($1, partial_1!(CAssemblyStatement, $2, $4, vec![], vec![], vec![])) }

  | asm maybe_type_qualifier '(' string_literal ':' asm_operands ')' ';'
        {% withNodeInfo($1, partial_1!(CAssemblyStatement, $2, $4, $6, vec![], vec![])) }

  | asm maybe_type_qualifier '(' string_literal ':' asm_operands ':' asm_operands ')' ';'
        {% withNodeInfo($1, partial_1!(CAssemblyStatement, $2, $4, $6, $8, vec![])) }

  | asm maybe_type_qualifier '(' string_literal ':' asm_operands ':' asm_operands ':' asm_clobbers ')' ';'
        {% withNodeInfo($1, partial_1!(CAssemblyStatement, $2, $4, $6, $8, reverse($10))) }


maybe_type_qualifier :: { Option<CTypeQual> }
maybe_type_qualifier
  : {- empty -}           { None }
  | type_qualifier        { Some($1) }

asm_operands :: { Vec<CAsmOperand> }
asm_operands
  : {- empty -}                         { vec![] }
  | nonnull_asm_operands                { reverse($1) }

nonnull_asm_operands :: { Reversed<Vec<CAsmOperand>> }
nonnull_asm_operands
  : asm_operand                           { singleton($1) }
  | nonnull_asm_operands ',' asm_operand  { snoc($1, $3) }

asm_operand :: { CAsmOperand }
asm_operand
  : string_literal '(' expression ')'
        {% withNodeInfo($1.clone(), partial_1!(CAssemblyOperand, None, $1, $3)) }
  | '[' ident ']' string_literal '(' expression ')'
        {% withNodeInfo($1, partial_1!(CAssemblyOperand, Some($2), $4, $6)) }
  | '[' tyident ']' string_literal '(' expression ')'
        {% withNodeInfo($1, partial_1!(CAssemblyOperand, Some($2), $4, $6)) }


asm_clobbers :: { Reversed<Vec<CStrLit>> }
asm_clobbers
  : string_literal                      { singleton($1) }
  | asm_clobbers ',' string_literal     { snoc($1, $3) }

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
declaration :: { CDecl }
declaration
  : sue_declaration_specifier ';'
        {% withNodeInfo($1.clone(), partial_1!(CDecl, reverse($1), vec![])) }

  | sue_type_specifier ';'
        {% withNodeInfo($1.clone(), partial_1!(CDecl, reverse($1), vec![])) }

  | declaring_list ';'
        {%
            if let CDecl(declspecs, dies, at) = $1 {
                withLength(at, partial_1!(CDecl, declspecs, List::reverse(dies)))
            } else {
                panic!("irrefutable pattern")
            }
        }

  | default_declaring_list ';'
        {%
            if let CDecl(declspecs, dies, at) = $1 {
                withLength(at, partial_1!(CDecl, declspecs, List::reverse(dies)))
            } else {
                panic!("irrefutable pattern")
            }
        }
  | "_Static_assert" '(' constant_expression ',' string_literal ')' ';'
        {% withNodeInfo($1, partial_1!(CStaticAssert, $3, $5)) }

declaration_list :: { Reversed<Vec<CDecl>> }
declaration_list
  : {- empty -}                       { empty() }
  | declaration_list declaration      { snoc($1, $2) }


-- * SUMMARY: default_declaring_list :- qualifier* identifier_declarator asm_attrs initializer?
--                                                 { ',' identifier_declarator asm_attrs initializer? }
--
-- * GNU extensions
--   __attribute__ annotations imm. before an declarator (see Attribute Syntax, paragraph 11)
--   asm + __attribute__ annotations (end of declarations, see Attribute Syntax, paragraph 12)
--   The assembler annotation is used to specifiy an assembler name for the declarator.
--
default_declaring_list :: { CDecl }
default_declaring_list
  : declaration_qualifier_list identifier_declarator asm_attrs_opt {-{}-} initializer_opt
        {%
            let declspecs = reverse($1.clone());
            thenP(withAsmNameAttrs($3, $2), box move |declr: CDeclrR| {
                seqP(
                    // TODO: borrow these instead
                    doDeclIdent(&declspecs, declr.clone()),
                    withNodeInfo($1, partial_1!(CDecl, declspecs,
                                                vec![(Some(reverseDeclr(declr)), $4, None)])))
            })
        }

  | type_qualifier_list identifier_declarator asm_attrs_opt {-{}-} initializer_opt
        {%
            let declspecs = liftTypeQuals($1.clone());
            thenP(withAsmNameAttrs($3, $2), box move |declr: CDeclrR| {
                seqP(
                    doDeclIdent(&declspecs, declr.clone()),
                    withNodeInfo($1, partial_1!(CDecl, declspecs,
                                                vec![(Some(reverseDeclr(declr)), $4, None)])))
            })
        }

  | type_qualifier_list attrs identifier_declarator asm_attrs_opt {-{}-} initializer_opt -- FIX 1600
        {%
            let declspecs = liftTypeQuals($1.clone());
            thenP(withAsmNameAttrs($4, $3), box move |declr: CDeclrR| {
                seqP(
                    doDeclIdent(&declspecs, declr.clone()),
                    withNodeInfo($1, partial_1!(CDecl, __op_addadd(declspecs, liftCAttrs($2)),
                                                vec![(Some(reverseDeclr(declr)), $5, None)])))
            })
        }

  -- GNU extension: __attribute__ as the only qualifier
  | attrs identifier_declarator asm_attrs_opt {-{}-} initializer_opt
        {%
            let declspecs = liftCAttrs($1.clone());
            thenP(withAsmNameAttrs($3, $2), box move |declr: CDeclrR| {
                seqP(
                    doDeclIdent(&declspecs, declr.clone()),
                    withNodeInfo($1, partial_1!(CDecl, declspecs,
                                                vec![(Some(reverseDeclr(declr)), $4, None)])))
            })
        }

  | default_declaring_list ',' attrs_opt identifier_declarator asm_attrs_opt {-{}-} initializer_opt
        {%
            if let CDecl(declspecs, dies, at) = $1 {
                let (f, s) = $5;
                thenP(withAsmNameAttrs((f, __op_addadd(s, $3)), $4), box move |declr: CDeclrR| {
                    seqP(
                        doDeclIdent(&declspecs, declr.clone()),
                        withLength(at, partial_1!(CDecl, declspecs,
                                                  __op_concat((Some(reverseDeclr(declr)), $6, None), dies))))
                })
            } else {
                panic!("irrefutable pattern")
            }
        }

-- assembler, followed by attribute annotation
asm_attrs_opt :: { (Option<CStrLit>, Vec<CAttr>) }
asm_attrs_opt
  : asm_opt attrs_opt    { ($1, $2) }

--
-- SUMMARY: declaring_list :- specifier* declarator asm_attrs initializer?
--                                 { ',' declarator asm_attrs initializer? }
--
-- GNU extensions:
--      __attribute__ annotations imm. before an declarator (see Attribute Syntax, paragraph 11)
--      asm + __attribute__ annotations (end of declarations, see Attribute Syntax, paragraph 12)
--
declaring_list :: { CDecl }
declaring_list
  : declaration_specifier declarator asm_attrs_opt initializer_opt
        {%
            thenP(withAsmNameAttrs($3, $2), box move |declr: CDeclrR| {
                seqP(
                    doDeclIdent(&$1, declr.clone()),
                    withNodeInfo($1.clone(), partial_1!(CDecl, $1, vec![(Some(reverseDeclr(declr)), $4, None)])))
            })
        }

  | type_specifier declarator asm_attrs_opt initializer_opt
        {%
            thenP(withAsmNameAttrs($3, $2), box move |declr: CDeclrR| {
                seqP(
                    doDeclIdent(&$1, declr.clone()),
                    withNodeInfo($1.clone(), partial_1!(CDecl, $1, vec![(Some(reverseDeclr(declr)), $4, None)])))
            })
        }

  | declaring_list ',' attrs_opt declarator asm_attrs_opt initializer_opt
        {%
            if let CDecl(declspecs, dies, at) = $1 {
                let (f, s) = $5;
                thenP(withAsmNameAttrs((f, __op_addadd(s, $3)), $4), box move |declr: CDeclrR| {
                    seqP(
                        doDeclIdent(&declspecs, declr.clone()),
                        returnP(CDecl(declspecs, __op_concat((Some(reverseDeclr(declr)), $6, None),
                                                              dies), at)))
                })
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
  : basic_declaration_specifier         { reverse($1) }  -- Arithmetic or void
  | sue_declaration_specifier           { reverse($1) }  -- Struct/Union/Enum
  | typedef_declaration_specifier       { reverse($1) }  -- Typedef


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
declaration_qualifier_list :: { Reversed<Vec<CDeclSpec>> }
declaration_qualifier_list
  : declaration_qualifier_without_types
        { singleton($1) }

  | attrs declaration_qualifier_without_types
        { snoc(reverseList(liftCAttrs($1)), $2) }

  | type_qualifier_list declaration_qualifier_without_types
        { snoc(rmap(CTypeQual, $1), $2) }

  | type_qualifier_list attrs declaration_qualifier_without_types
        { snoc(rappend(rmap(CTypeQual, $1), liftCAttrs($2)), $3) }

  | declaration_qualifier_list declaration_qualifier
        { snoc($1, $2) }

  | declaration_qualifier_list attr
        { addTrailingAttrs($1, $2) }

--
-- declaration_qualifier :- storage_class | type_qualifier | function_specifier | alignment_specifier
--
declaration_qualifier :: { CDeclSpec }
declaration_qualifier
  : storage_class                    { CStorageSpec($1) }
  | type_qualifier                   { CTypeQual($1) }
  | function_specifier               { CFunSpec($1) }
  | alignment_specifier              { CAlignSpec($1) }


declaration_qualifier_without_types :: { CDeclSpec }
  : storage_class                    { CStorageSpec($1) }
  | function_specifier               { CFunSpec($1) }
  | alignment_specifier              { CAlignSpec($1) }


-- parse C storage class specifier (C99 6.7.1)
--
-- * GNU extensions: '__thread' thread local storage
--
storage_class :: { CStorageSpec }
storage_class
  : typedef                     {% withNodeInfo($1, partial_1!(CTypedef)) }
  | extern                      {% withNodeInfo($1, partial_1!(CExtern)) }
  | static                      {% withNodeInfo($1, partial_1!(CStatic)) }
  | auto                        {% withNodeInfo($1, partial_1!(CAuto)) }
  | register                    {% withNodeInfo($1, partial_1!(CRegister)) }
  | "__thread"                  {% withNodeInfo($1, partial_1!(CThread)) }

-- parse C function specifier (C11 6.7.4)
function_specifier :: { CFunSpec }
function_specifier
  : inline              {% withNodeInfo($1, partial_1!(CInlineQual)) }
  | "_Noreturn"         {% withNodeInfo($1, partial_1!(CNoreturnQual)) }

-- parse C alignment specifier (C11 6.7.5)
alignment_specifier :: { CAlignSpec }
alignment_specifier
  : alignas '(' type_name ')'           {% withNodeInfo($1, partial_1!(CAlignAsType, $3)) }
  | alignas '(' constant_expression ')' {% withNodeInfo($1, partial_1!(CAlignAsExpr, $3)) }

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
  : basic_type_specifier                { reverse($1) }  -- Arithmetic or void
  | sue_type_specifier                  { reverse($1) }  -- Struct/Union/Enum
  | typedef_type_specifier              { reverse($1) }  -- Typedef
--  | "_Atomic" '(' type_name ')'                         -- _Atomic(type)
--        {% withNodeInfo $1 $ \at -> [CTypeSpec (CAtomicType $3 at)] }

basic_type_name :: { CTypeSpec }
basic_type_name
  : void                        {% withNodeInfo($1, partial_1!(CVoidType)) }
  | char                        {% withNodeInfo($1, partial_1!(CCharType)) }
  | short                       {% withNodeInfo($1, partial_1!(CShortType)) }
  | int                         {% withNodeInfo($1, partial_1!(CIntType)) }
  | long                        {% withNodeInfo($1, partial_1!(CLongType)) }
  | float                       {% withNodeInfo($1, partial_1!(CFloatType)) }
  | double                      {% withNodeInfo($1, partial_1!(CDoubleType)) }
  | signed                      {% withNodeInfo($1, partial_1!(CSignedType)) }
  | unsigned                    {% withNodeInfo($1, partial_1!(CUnsigType)) }
  | "_Bool"                     {% withNodeInfo($1, partial_1!(CBoolType)) }
  | "_Complex"                  {% withNodeInfo($1, partial_1!(CComplexType)) }
  | "__int128"                  {% withNodeInfo($1, partial_1!(CInt128Type)) }


-- A mixture of type qualifiers, storage class and basic type names in any
-- order, but containing at least one basic type name and at least one storage
-- class specifier.
--
-- basic_declaration_specifier :- <permute> type_qualifier* storage_class+ basic_type_name+
--
--   GNU extensions
--     arbitrary interleaved __attribute__ annotations
--
basic_declaration_specifier :: { Reversed<Vec<CDeclSpec>> }
basic_declaration_specifier
  : declaration_qualifier_list basic_type_name
        { snoc($1, CTypeSpec($2)) }

  | basic_type_specifier storage_class
        { snoc($1, CStorageSpec($2)) }

  | basic_declaration_specifier declaration_qualifier
        { snoc($1, $2) }

  | basic_declaration_specifier basic_type_name
        { snoc($1, CTypeSpec($2)) }

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
basic_type_specifier :: { Reversed<Vec<CDeclSpec>> }
basic_type_specifier
  -- Arithmetic or void
  : basic_type_name
        { singleton(CTypeSpec($1)) }

  | attrs basic_type_name
        { snoc(reverseList(liftCAttrs($1)), CTypeSpec($2)) }

  | type_qualifier_list basic_type_name
        { snoc(rmap(CTypeQual, $1), CTypeSpec($2)) }

  | type_qualifier_list attrs basic_type_name
        { snoc(rappend(rmap(CTypeQual, $1), liftCAttrs($2)), CTypeSpec($3)) }

  | basic_type_specifier type_qualifier
        { snoc($1, CTypeQual($2)) }

  | basic_type_specifier basic_type_name
        { snoc($1, CTypeSpec($2)) }

  | basic_type_specifier attr
        { addTrailingAttrs($1, $2) }


-- A named or anonymous struct, union or enum type along with at least one
-- storage class and any mix of type qualifiers.
--
-- * Summary:
--   sue_declaration_specifier :- <permute> type_qualifier* storage_class+ elaborated_type_name
--
sue_declaration_specifier :: { Reversed<Vec<CDeclSpec>> }
sue_declaration_specifier
  : declaration_qualifier_list elaborated_type_name
        { snoc($1, CTypeSpec($2)) }

  | sue_type_specifier storage_class
        { snoc($1, CStorageSpec($2)) }

  | sue_declaration_specifier declaration_qualifier
        { snoc($1, $2) }

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
sue_type_specifier :: { Reversed<Vec<CDeclSpec>> }
sue_type_specifier
  -- struct/union/enum
  : elaborated_type_name
        { singleton(CTypeSpec($1)) }

  | attrs elaborated_type_name
        { snoc(reverseList(liftCAttrs($1)), CTypeSpec($2)) }

  | type_qualifier_list elaborated_type_name
        { snoc(rmap(CTypeQual, $1), CTypeSpec($2)) }

  | type_qualifier_list attrs elaborated_type_name
        { snoc(rappend(rmap(CTypeQual, $1), liftCAttrs($2)), CTypeSpec($3)) }

  | sue_type_specifier type_qualifier
        { snoc($1, CTypeQual($2)) }

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
typedef_declaration_specifier :: { Reversed<Vec<CDeclSpec>> }
typedef_declaration_specifier
  : typedef_type_specifier storage_class
        { snoc($1, CStorageSpec($2)) }

  | declaration_qualifier_list tyident
        {% withNodeInfo($2.clone(), box |at| snoc($1, CTypeSpec(CTypeDef($2, at)))) }

  | declaration_qualifier_list typeof '(' expression ')'
        {% withNodeInfo($2, box |at| snoc($1, CTypeSpec(CTypeOfExpr($4, at)))) }

  | declaration_qualifier_list typeof '(' type_name ')'
        {% withNodeInfo($2, box |at| snoc($1, CTypeSpec(CTypeOfType($4, at)))) }

  | typedef_declaration_specifier declaration_qualifier
        { snoc($1, $2) }

  | typedef_declaration_specifier attr
        { addTrailingAttrs($1, $2) }


-- typedef'ed type identifier with optional leading and trailing type qualifiers
--
-- * Summary:
--   type_qualifier* ( tyident | typeof '('...')' ) type_qualifier*
--
typedef_type_specifier :: { Reversed<Vec<CDeclSpec>> }
typedef_type_specifier
  : tyident
        {% withNodeInfo($1.clone(), box |at| singleton(CTypeSpec(CTypeDef($1, at)))) }

  | typeof '(' expression ')'
        {% withNodeInfo($1, box |at| singleton(CTypeSpec(CTypeOfExpr($3, at)))) }

  | typeof '(' type_name ')'
        {% withNodeInfo($1, box |at| singleton(CTypeSpec(CTypeOfType($3, at)))) }

  | type_qualifier_list tyident
        {% withNodeInfo($2.clone(), box |at| snoc(rmap(CTypeQual, $1), CTypeSpec(CTypeDef($2, at)))) }

  | type_qualifier_list typeof '(' expression ')'
        {% withNodeInfo($2, box |at| snoc(rmap(CTypeQual, $1), CTypeSpec(CTypeOfExpr($4, at)))) }

  | type_qualifier_list typeof '(' type_name ')'
        {% withNodeInfo($2, box |at| snoc(rmap(CTypeQual, $1), CTypeSpec(CTypeOfType($4, at)))) }

  -- repeat with attrs (this could be easier if type qualifier list wouldn't allow leading attributes)
  | attrs tyident
        {% withNodeInfo($2.clone(), box |at| snoc(reverseList(liftCAttrs($1)), CTypeSpec(CTypeDef($2, at)))) }

  | attrs typeof '(' expression ')'
        {% withNodeInfo($1.clone(), box |at| snoc(reverseList(liftCAttrs($1)), CTypeSpec(CTypeOfExpr($4, at)))) }

  | attrs typeof '(' type_name ')'
        {% withNodeInfo($2, box |at| snoc(reverseList(liftCAttrs($1)), CTypeSpec(CTypeOfType($4, at)))) }

  | type_qualifier_list attrs tyident
        {% withNodeInfo($3.clone(), box |at| snoc(rappend(rmap(CTypeQual, $1), liftCAttrs($2)),
                                                  CTypeSpec(CTypeDef($3, at)))) }

  | type_qualifier_list attrs typeof '(' expression ')'
        {% withNodeInfo($3, box |at| snoc(rappend(rmap(CTypeQual, $1), liftCAttrs($2)),
                                          CTypeSpec(CTypeOfExpr($5, at)))) }

  | type_qualifier_list attrs typeof '(' type_name ')'
        {% withNodeInfo($3, box |at| snoc(rappend(rmap(CTypeQual, $1), liftCAttrs($2)),
                                          CTypeSpec(CTypeOfType($5, at)))) }

  | typedef_type_specifier type_qualifier
        { snoc($1, CTypeQual($2)) }

  | typedef_type_specifier attr
        { addTrailingAttrs($1, $2) }


-- A named or anonymous struct, union or enum type.
--
-- * Summary:
--   (struct|union|enum) (identifier? '{' ... '}' | identifier)
--
elaborated_type_name :: { CTypeSpec }
elaborated_type_name
  : struct_or_union_specifier   {% withNodeInfo($1.clone(), partial_1!(CSUType, $1)) }
  | enum_specifier              {% withNodeInfo($1.clone(), partial_1!(CEnumType, $1)) }


-- parse C structure or union declaration (C99 6.7.2.1)
--
-- * Summary:
--    (struct|union) (identifier? '{' ... '}' | identifier)
--
struct_or_union_specifier :: { CStructUnion }
struct_or_union_specifier
  : struct_or_union attrs_opt identifier '{' struct_declaration_list  '}'
        {% withNodeInfo($1.clone(), partial_1!(CStructureUnion, unL($1), Some($3), Some(reverse($5)), $2)) }

  | struct_or_union attrs_opt '{' struct_declaration_list  '}'
        {% withNodeInfo($1.clone(), partial_1!(CStructureUnion, unL($1), None,     Some(reverse($4)), $2)) }

  | struct_or_union attrs_opt identifier
        {% withNodeInfo($1.clone(), partial_1!(CStructureUnion, unL($1), Some($3), None,              $2)) }


struct_or_union :: { Located<CStructTag> }
struct_or_union
  : struct                      { Located(CStructTag, posOf($1)) }
  | union                       { Located(CUnionTag, posOf($1)) }


struct_declaration_list :: { Reversed<Vec<CDecl>> }
struct_declaration_list
  : {- empty -}                                         { empty() }
  | struct_declaration_list ';'                         { $1 }
  | struct_declaration_list struct_declaration          { snoc($1, $2) }


-- parse C structure declaration (C99 6.7.2.1)
--
struct_declaration :: { CDecl }
struct_declaration
  : struct_declaring_list ';'
        {
            if let CDecl(declspecs, dies, at) = $1 {
                CDecl(declspecs, List::reverse(dies), at)
            } else {
                panic!("irrefutable pattern");
            }
        }

  | struct_default_declaring_list';'
        {
            if let CDecl(declspecs, dies, at) = $1 {
                CDecl(declspecs, List::reverse(dies), at)
            } else {
                panic!("irrefutable pattern");
            }
        }

  | "__extension__" struct_declaration  { $2 }


--
--  * Note: doesn't redeclare typedef
--
--  TODO: FIXME: AST doesn't allow recording attributes of unnamed struct members
struct_default_declaring_list :: { CDecl }
struct_default_declaring_list
  : type_qualifier_list attrs_opt struct_identifier_declarator
        {%
            withNodeInfo($1.clone(), match $3 {
                (d, s) => partial_1!(CDecl, __op_addadd(liftTypeQuals($1), liftCAttrs($2)),
                                     vec![(d, None, s)])
            })
        }

  -- GNU extension: __attribute__ as only type qualifier
  | attrs struct_identifier_declarator
        {%
            withNodeInfo($1.clone(), match $2 {
                (d, s) => partial_1!(CDecl, liftCAttrs($1), vec![(d, None, s)]),
            })
        }

  -- attrs_opt apply to the declared object
  | struct_default_declaring_list ',' attrs_opt struct_identifier_declarator
        {
            if let CDecl(declspecs, dies, at) = $1 {
                match $4 {
                    (Some(d), s) => {
                        CDecl(declspecs, __op_concat((Some(appendObjAttrs($3, d)), None, s), dies), at)
                    },
                    (None, s) => {
                        CDecl(declspecs, __op_concat((None, None, s), dies), at)
                    },
                }
            } else {
                panic!("irrefutable pattern")
            }
        } -- FIXME

-- * GNU extensions:
--     allow anonymous nested structures and unions
--     FIXME: cannot record attribute of unnamed field
struct_declaring_list :: { CDecl }
struct_declaring_list
  : type_specifier struct_declarator attrs_opt
        {%
            withNodeInfo($1.clone(), match $2 {
                (Some(d), s) => {
                    partial_1!(CDecl, $1, vec![(Some(appendObjAttrs($3, d)), None, s)])
                },
                (None, s) => {
                    partial_1!(CDecl, $1, vec![(None, None, s)])
                },
            })
        } {- DO FIXME -}

  | struct_declaring_list ',' attrs_opt struct_declarator attrs_opt
        {
            if let CDecl(declspecs, dies, at) = $1 {
                match $4 {
                    (Some(d), s) => {
                        CDecl(declspecs, __op_concat((Some(
                            appendObjAttrs(__op_addadd($3, $5), d)), None, s), dies), at)
                    },
                    (None, s) => {
                        CDecl(declspecs, __op_concat((None, None, s), dies), at)
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
        {% withNodeInfo($1.clone(), partial_1!(CDecl, $1, vec![])) }


-- parse C structure declarator (C99 6.7.2.1)
--
struct_declarator :: { (Option<CDeclr>, Option<CExpr>) }
struct_declarator
  : declarator                                  { (Some(reverseDeclr($1)), None) }
  | ':' constant_expression                     { (None, Some($2)) }
  | declarator ':' constant_expression          { (Some(reverseDeclr($1)), Some($3)) }

-- FIXME: anonymous bitfield doesn't allow recording of attributes
struct_identifier_declarator :: { (Option<CDeclr>, Option<CExpr>) }
struct_identifier_declarator
  : identifier_declarator                               { (Some(reverseDeclr($1)), None) }
  | ':' constant_expression                             { (None, Some($2)) }
  | identifier_declarator ':' constant_expression       { (Some(reverseDeclr($1)), Some($3)) }
  | struct_identifier_declarator attr
        {
            match $1 {
                (None, expr) => (None, expr),
                (Some(CDeclarator(name, derived, asmname, attrs, node)), bsz) =>
                    (Some(CDeclarator(name, derived, asmname, __op_addadd(attrs, $2), node)), bsz)
            }
        }

-- parse C enumeration declaration (C99 6.7.2.2)
--
-- * Summary:
--   enum (identifier? '{' ... '}' | identifier)
--
enum_specifier :: { CEnum }
enum_specifier
  : enum attrs_opt '{' enumerator_list '}'
        {% withNodeInfo($1, partial_1!(CEnumeration, None, Some(reverse($4)), $2)) }

  | enum attrs_opt '{' enumerator_list ',' '}'
        {% withNodeInfo($1, partial_1!(CEnumeration, None, Some(reverse($4)), $2)) }

  | enum attrs_opt identifier '{' enumerator_list '}'
        {% withNodeInfo($1, partial_1!(CEnumeration, Some($3), Some(reverse($5)), $2)) }

  | enum attrs_opt identifier '{' enumerator_list ',' '}'
        {% withNodeInfo($1, partial_1!(CEnumeration, Some($3), Some(reverse($5)), $2)) }

  | enum attrs_opt identifier
        {% withNodeInfo($1, partial_1!(CEnumeration, Some($3), None, $2)) }

enumerator_list :: { Reversed<Vec<(Ident, Option<CExpr>)>> }
enumerator_list
  : enumerator                                  { singleton($1) }
  | enumerator_list ',' enumerator              { snoc($1, $3) }


enumerator :: { (Ident, Option<CExpr>) }
enumerator
  : identifier                               { ($1, None) }
  | identifier attrs                         { ($1, None) }
  | identifier attrs '=' constant_expression { ($1, Some($4)) }
  | identifier '=' constant_expression       { ($1, Some($3)) }


-- parse C type qualifier (C11 6.7.3)
--
-- concerning atomic, note:  If the _Atomic keyword is immediately followed by a left
-- parenthesis, it should be interpreted as a type specifier (with a type name), not as a type qualifier
type_qualifier :: { CTypeQual }
type_qualifier
  : const               {% withNodeInfo($1, partial_1!(CConstQual)) }
  | volatile            {% withNodeInfo($1, partial_1!(CVolatQual)) }
  | restrict            {% withNodeInfo($1, partial_1!(CRestrQual)) }
  | "_Nullable"         {% withNodeInfo($1, partial_1!(CNullableQual)) }
  | "_Nonnull"          {% withNodeInfo($1, partial_1!(CNonnullQual)) }
  | "_Atomic"           {% withNodeInfo($1, partial_1!(CAtomicQual)) }

-- a list containing at least one type_qualifier (const, volatile, restrict, inline, _Noreturn)
--    and additionally CAttrs
type_qualifier_list :: { Reversed<Vec<CTypeQual>> }
type_qualifier_list
  : attrs_opt type_qualifier                 { snoc(reverseList(__map!(CAttrQual, $1)), $2) }
  | type_qualifier_list type_qualifier       { snoc($1, $2) }
  | type_qualifier_list attrs type_qualifier { snoc(rappend($1, __map!(CAttrQual, $2)), $3) }

-- parse C declarator (C99 6.7.5)
--
declarator :: { CDeclrR }
declarator
  : identifier_declarator               { $1 }
  | typedef_declarator                  { $1 }


-- Parse GNU C's asm annotations
--
-- Those annotations allow to give an assembler name to a function or identifier.
asm_opt :: { Option<CStrLit> }
asm_opt
  : {- empty -}                         { None }
  | asm '(' string_literal ')'          { Some($3) }

--
-- typedef_declarator :-

typedef_declarator :: { CDeclrR }
typedef_declarator
  -- would be ambiguous as parameter
  : paren_typedef_declarator            { $1 }

  -- not ambiguous as param
  | parameter_typedef_declarator        { $1 }


-- parameter_typedef_declarator :- tyident declarator_postfix?
--                              | '(' attrs? clean_typedef_declarator ')' declarator_postfix?
--                              |  '*' attrs? type_qualifier_list? parameter_typedef_declarator
--
parameter_typedef_declarator :: { CDeclrR }
parameter_typedef_declarator
  : tyident
        {% withNodeInfo($1.clone(), partial_1!(mkVarDeclr, $1)) }

  | tyident postfixing_abstract_declarator
        {% withNodeInfo($1.clone(), box move |at| { $2(mkVarDeclr($1, at)) }) }

  | clean_typedef_declarator
        { $1 }


-- The  following have at least one '*'.
-- There is no (redundant) '(' between the '*' and the tyident.
--
-- clean_typedef_declarator :-  '(' attrs? clean_typedef_declarator ')' declarator_postfix?
--                            | '*' attrs? type_qualifier_list? parameter_typedef_declarator
--
clean_typedef_declarator :: { CDeclrR }
clean_typedef_declarator
  : clean_postfix_typedef_declarator
        { $1 }

  | '*' parameter_typedef_declarator
        {% withNodeInfo($1, partial_1!(ptrDeclr, $2, vec![])) }

  | '*' attrs parameter_typedef_declarator
        {% withAttribute($1, $2, partial_1!(ptrDeclr, $3, vec![])) }

  | '*' type_qualifier_list  parameter_typedef_declarator
        {% withNodeInfo($1, partial_1!(ptrDeclr, $3, reverse($2))) }

  | '*' type_qualifier_list attrs parameter_typedef_declarator
        {% withAttribute($1, $3, partial_1!(ptrDeclr, $4, reverse($2))) }

-- clean_postfix_typedef_declarator :- ( attrs? clean_typedef_declarator ) declarator_postfix?
--
clean_postfix_typedef_declarator :: { CDeclrR }
clean_postfix_typedef_declarator
  : '(' clean_typedef_declarator ')'                                        { $2 }
  | '(' clean_typedef_declarator ')' postfixing_abstract_declarator         { $4($2) }
  | '(' attrs clean_typedef_declarator ')'                                  { appendDeclrAttrs($2, $3) }
  | '(' attrs clean_typedef_declarator ')' postfixing_abstract_declarator   { appendDeclrAttrs($2, $5($3)) }


-- The following have a redundant '(' placed
-- immediately to the left of the tyident
--
paren_typedef_declarator :: { CDeclrR }
paren_typedef_declarator
  : paren_postfix_typedef_declarator
        { $1 }

  -- redundant paren
  | '*' '(' simple_paren_typedef_declarator ')'
        {% withNodeInfo($1, partial_1!(ptrDeclr, $3, vec![])) }

  | '*' type_qualifier_list '(' simple_paren_typedef_declarator ')'
        {% withNodeInfo($1, partial_1!(ptrDeclr, $4, reverse($2))) }
  | '*' type_qualifier_list attrs '(' simple_paren_typedef_declarator ')'
        {% withAttribute($1, $3, partial_1!(ptrDeclr, $5, reverse($2)))  }

  | '*' paren_typedef_declarator
        {% withNodeInfo($1, partial_1!(ptrDeclr, $2, vec![])) }

  | '*' type_qualifier_list paren_typedef_declarator
        {% withNodeInfo($1, partial_1!(ptrDeclr, $3, reverse($2))) }
  | '*' type_qualifier_list attrs paren_typedef_declarator
        {% withAttribute($1, $3, partial_1!(ptrDeclr, $4, reverse($2))) }

-- redundant paren to left of tname
paren_postfix_typedef_declarator :: { CDeclrR }
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
simple_paren_typedef_declarator :: { CDeclrR }
simple_paren_typedef_declarator
  : tyident
        {% withNodeInfo($1.clone(), partial_1!(mkVarDeclr, $1)) }

  | '(' simple_paren_typedef_declarator ')'
        { $2 }

--
-- Declarators
-- * Summary
--   declarator :- ( '*' (type_qualifier | attr)* )* ident ( array_decl | "(" parameter-list ")" )?
--      + additional parenthesis
--
identifier_declarator :: { CDeclrR }
identifier_declarator
  : unary_identifier_declarator                 { $1 }
  | paren_identifier_declarator                 { $1 }


unary_identifier_declarator :: { CDeclrR }
unary_identifier_declarator
  : postfix_identifier_declarator
        { $1 }

  | '*' identifier_declarator
        {% withNodeInfo($1, partial_1!(ptrDeclr, $2, vec![])) }

  | '*' attrs identifier_declarator
        {% withAttribute($1, $2, partial_1!(ptrDeclr, $3, vec![])) }

  | '*' type_qualifier_list identifier_declarator
        {% withNodeInfo($1, partial_1!(ptrDeclr, $3, reverse($2))) }

  | '*' type_qualifier_list attrs identifier_declarator
        {% withAttribute($1, $3, partial_1!(ptrDeclr, $4, reverse($2))) }

postfix_identifier_declarator :: { CDeclrR }
postfix_identifier_declarator
  : paren_identifier_declarator postfixing_abstract_declarator
        { $2($1) }

   | '('  unary_identifier_declarator ')'
        { $2 }

   | '(' unary_identifier_declarator ')' postfixing_abstract_declarator
        { $4($2) }

   | '(' attrs unary_identifier_declarator ')'
        { appendDeclrAttrs($2, $3) }

   | '(' attrs unary_identifier_declarator ')' postfixing_abstract_declarator
        { appendDeclrAttrs($2, $5($3)) }


-- just an identifier in any number of nested parenthesis
paren_identifier_declarator :: { CDeclrR }
paren_identifier_declarator
  : ident
        {% withNodeInfo($1.clone(), partial_1!(mkVarDeclr, $1)) }

  | '(' paren_identifier_declarator ')'
        { $2 }

  | '(' attrs paren_identifier_declarator ')'
        { appendDeclrAttrs($2, $3) }

function_declarator_old :: { CDeclr }
function_declarator_old
  : old_function_declarator
        { reverseDeclr($1) }

old_function_declarator :: { CDeclrR }
old_function_declarator
  : postfix_old_function_declarator
        { $1 }

  | '*' old_function_declarator
        {% withNodeInfo($1, partial_1!(ptrDeclr, $2, vec![])) } -- FIXME: no attr possible here ???

  | '*' type_qualifier_list old_function_declarator
        {% withNodeInfo($1, partial_1!(ptrDeclr, $3, reverse($2))) }

postfix_old_function_declarator :: { CDeclrR }
postfix_old_function_declarator
  : paren_identifier_declarator '(' identifier_list ')'
        {% withNodeInfo($1.clone(), partial_1!(funDeclr, $1, Left(reverse($3)), vec![])) }

  | '(' old_function_declarator ')'
        { $2 }

  | '(' old_function_declarator ')' postfixing_abstract_declarator
        { $4($2) }


-- parse C parameter type list (C99 6.7.5)
--
parameter_type_list :: { (Vec<CDecl>, bool) }
parameter_type_list
  : {- empty -}                         { (vec![], false) }
  | parameter_list                      { (reverse($1), false) }
  | parameter_list ',' "..."            { (reverse($1), true) }

parameter_list :: { Reversed<Vec<CDecl>> }
parameter_list
  : parameter_declaration                       { singleton($1) }
  | parameter_list ',' parameter_declaration    { snoc($1, $3) }

parameter_declaration :: { CDecl }
parameter_declaration
  : declaration_specifier
        {% withNodeInfo($1.clone(), partial_1!(CDecl, $1, vec![])) }

  | declaration_specifier abstract_declarator
        {% withNodeInfo($1.clone(), partial_1!(CDecl, $1, vec![(Some(reverseDeclr($2)), None, None)])) }

  | declaration_specifier identifier_declarator attrs_opt
        {% withNodeInfo($1.clone(), partial_1!(CDecl, $1,
                                               vec![(Some(reverseDeclr(appendDeclrAttrs($3, $2))), None, None)])) }

  | declaration_specifier parameter_typedef_declarator attrs_opt
        {% withNodeInfo($1.clone(), partial_1!(CDecl, $1,
                                               vec![(Some(reverseDeclr(appendDeclrAttrs($3, $2))), None, None)])) }

  | declaration_qualifier_list
        {% withNodeInfo($1.clone(), partial_1!(CDecl, reverse($1), vec![])) }

  | declaration_qualifier_list abstract_declarator
        {% withNodeInfo($1.clone(), partial_1!(CDecl, reverse($1), vec![(Some(reverseDeclr($2)), None, None)])) }

  | declaration_qualifier_list identifier_declarator attrs_opt
        {% withNodeInfo($1.clone(), partial_1!(CDecl, reverse($1),
                                               vec![(Some(reverseDeclr(appendDeclrAttrs($3, $2))), None, None)])) }

  | type_specifier
        {% withNodeInfo($1.clone(), partial_1!(CDecl, $1, vec![])) }

  | type_specifier abstract_declarator
        {% withNodeInfo($1.clone(), partial_1!(CDecl, $1, vec![(Some(reverseDeclr($2)), None, None)])) }

  | type_specifier identifier_declarator attrs_opt
        {% withNodeInfo($1.clone(), partial_1!(CDecl, $1,
                                               vec![(Some(reverseDeclr(appendDeclrAttrs($3, $2))), None, None)])) }

  | type_specifier parameter_typedef_declarator attrs_opt
        {% withNodeInfo($1.clone(), partial_1!(CDecl, $1,
                                               vec![(Some(reverseDeclr(appendDeclrAttrs($3, $2))), None, None)])) }

  | type_qualifier_list
        {% withNodeInfo($1.clone(), partial_1!(CDecl, liftTypeQuals($1), vec![])) }
  | type_qualifier_list attr
        {% withNodeInfo($1.clone(), partial_1!(CDecl, __op_addadd(liftTypeQuals($1), liftCAttrs($2)), vec![])) }

  | type_qualifier_list abstract_declarator
        {% withNodeInfo($1.clone(), partial_1!(CDecl, liftTypeQuals($1),
                                               vec![(Some(reverseDeclr($2)), None, None)])) }

  | type_qualifier_list identifier_declarator attrs_opt
        {% withNodeInfo($1.clone(), partial_1!(CDecl, liftTypeQuals($1),
                                               vec![(Some(reverseDeclr(appendDeclrAttrs($3, $2))), None, None)])) }


identifier_list :: { Reversed<Vec<Ident>> }
identifier_list
  : ident                               { singleton($1) }
  | identifier_list ',' ident           { snoc($1, $3) }


-- parse C type name (C99 6.7.6)
--
type_name :: { CDecl }
type_name
  :  type_specifier
        {% withNodeInfo($1.clone(), partial_1!(CDecl, $1, vec![])) }

  |  type_specifier abstract_declarator
        {% withNodeInfo($1.clone(), partial_1!(CDecl, $1, vec![(Some(reverseDeclr($2)), None, None)])) }

  |  type_qualifier_list attr
        {% withNodeInfo($1.clone(), partial_1!(CDecl, __op_addadd(liftTypeQuals($1), liftCAttrs($2)), vec![])) }

  |  type_qualifier_list abstract_declarator
        {% withNodeInfo($1.clone(), partial_1!(CDecl, liftTypeQuals($1),
                                               vec![(Some(reverseDeclr($2)), None, None)])) }

-- parse C abstract declarator (C99 6.7.6)
--
-- postfix starts with '('
-- postfixing starts with '(' or '['
-- unary start with '*'
abstract_declarator :: { CDeclrR }
abstract_declarator
  : unary_abstract_declarator       { $1 }
  | postfix_abstract_declarator     { $1 }
  | postfixing_abstract_declarator  { $1(emptyDeclr()) }

--
-- FIXME
--  | postfixing_abstract_declarator attrs_opt  { $1(emptyDeclr()) }


postfixing_abstract_declarator :: { Rc<Box<Fn(CDeclrR) -> CDeclrR>> }
postfixing_abstract_declarator
  : array_abstract_declarator
        { $1 }

  | '(' parameter_type_list ')'
        {%
            withNodeInfo($1, box move |at: NodeInfo| {
                let a: Rc<Box<Fn(CDeclrR) -> CDeclrR>> = Rc::new(box move |declr| {
                    let (params, variadic) = $2.clone();
                    funDeclr(declr, (Right((params, variadic))), vec![], at.clone())
                });
                a
            })
        }


-- * TODO: Note that we recognise but ignore the C99 static keyword (see C99 6.7.5.3)
--
-- * TODO: We do not distinguish in the AST between incomplete array types and
-- complete variable length arrays ([ '*' ] means the latter). (see C99 6.7.5.2)
--
array_abstract_declarator :: { Rc<Box<Fn(CDeclrR) -> CDeclrR>> }
array_abstract_declarator
  : postfix_array_abstract_declarator
        { $1 }

  | array_abstract_declarator postfix_array_abstract_declarator
        { Rc::new(box move |decl| { $2($1(decl)) }) }

--
-- TODO: record static
postfix_array_abstract_declarator :: { Rc<Box<Fn(CDeclrR) -> CDeclrR>> }
postfix_array_abstract_declarator
  : '[' assignment_expression_opt ']'
        {%
            withNodeInfo($1.clone(), box move |at: NodeInfo| {
                let a: Rc<Box<Fn(CDeclrR) -> CDeclrR>> = Rc::new(box move |declr| {
                    arrDeclr(declr, vec![], false, false, $2.clone(), at.clone())
                });
                a
            })
        }

  | '[' attrs assignment_expression_opt ']'
        {% withAttributePF($1, $2, box move |at, declr|
                           arrDeclr(declr, vec![], false, false, $3.clone(), at)) }

  | '[' type_qualifier_list assignment_expression_opt ']'
        {%
            withNodeInfo($1.clone(), box move |at: NodeInfo| {
                let a: Rc<Box<Fn(CDeclrR) -> CDeclrR>> = Rc::new(
                    box move |declr| { arrDeclr(declr, reverse($2.clone()),
                                                false, false, $3.clone(), at.clone()) });
                a
            })
        }

  | '[' type_qualifier_list attrs assignment_expression_opt ']'
        {% withAttributePF($1, $3, box move |at, declr|
                           arrDeclr(declr, reverse($2.clone()), false, false, $4.clone(), at)) }

  | '[' static attrs_opt assignment_expression ']'
        {% withAttributePF($1, $3, box move |at, declr|
                           arrDeclr(declr, vec![], false, true, Some($4.clone()), at)) }

  | '[' static type_qualifier_list attrs_opt assignment_expression ']'
        {% withAttributePF($1, $4, box move |at, declr|
                           arrDeclr(declr, reverse($3.clone()), false, true, Some($5.clone()), at)) }

  | '[' type_qualifier_list attrs_opt static attrs_opt assignment_expression ']'
        {% withAttributePF($1, __op_addadd($3, $5), box move |at, declr|
                           arrDeclr(declr, reverse($2.clone()), false, true, Some($6.clone()), at)) }

  | '[' '*' attrs_opt ']'
        {% withAttributePF($1, $3, box move |at, declr|
                           arrDeclr(declr, vec![], true, false, None, at)) }
  | '[' attrs '*' attrs_opt ']'
        {% withAttributePF($1, __op_addadd($2, $4), box move |at, declr|
                           arrDeclr(declr, vec![], true, false, None, at)) }

  | '[' type_qualifier_list '*' attrs_opt ']'
        {% withAttributePF($1, $4, box move |at, declr|
                           arrDeclr(declr, reverse($2.clone()), true, false, None, at)) }
  | '[' type_qualifier_list attrs '*' attrs_opt ']'
        {% withAttributePF($1, __op_addadd($3, $5), box move |at, declr|
                           arrDeclr(declr, reverse($2.clone()), true, false, None, at)) }

unary_abstract_declarator :: { CDeclrR }
unary_abstract_declarator
  : '*'
        {% withNodeInfo($1, partial_1!(ptrDeclr, emptyDeclr(), vec![])) }

  | '*' type_qualifier_list attrs_opt
        {% withAttribute($1, $3, partial_1!(ptrDeclr, emptyDeclr(), reverse($2))) }

  | '*' abstract_declarator
        {% withNodeInfo($1, partial_1!(ptrDeclr, $2, vec![])) }

  | '*' type_qualifier_list abstract_declarator
        {% withNodeInfo($1, partial_1!(ptrDeclr, $3, reverse($2))) }

  | '*' attrs
        {% withAttribute($1, $2, partial_1!(ptrDeclr, emptyDeclr(), vec![])) }
  | '*' attrs abstract_declarator
        {% withAttribute($1, $2, partial_1!(ptrDeclr, $3, vec![])) }

-- postfix_ad starts with '(', postfixing with '(' or '[', unary_abstract starts with '*'
postfix_abstract_declarator :: { CDeclrR }
postfix_abstract_declarator
  : '(' unary_abstract_declarator ')'                                   { $2 }
  | '(' postfix_abstract_declarator ')'                                 { $2 }
  | '(' postfixing_abstract_declarator ')'                              { $2(emptyDeclr()) }
  | '(' unary_abstract_declarator ')' postfixing_abstract_declarator    { $4($2) }

-- FIX 0700
  | '(' attrs unary_abstract_declarator ')'                                 { appendDeclrAttrs($2, $3) }
  | '(' attrs postfix_abstract_declarator ')'                               { appendDeclrAttrs($2, $3) }
  | '(' attrs postfixing_abstract_declarator ')'                            { appendDeclrAttrs($2, $3(emptyDeclr())) }
  | '(' attrs unary_abstract_declarator ')' postfixing_abstract_declarator  { appendDeclrAttrs($2, $5($3)) }
  | postfix_abstract_declarator attr                                        { appendDeclrAttrs($2, $1) }


-- parse C initializer (C99 6.7.8)
--
initializer :: { CInit }
initializer
  : assignment_expression               {% withNodeInfo($1.clone(), partial_1!(CInitExpr, $1)) }
  | '{' initializer_list '}'            {% withNodeInfo($1, partial_1!(CInitList, reverse($2))) }
  | '{' initializer_list ',' '}'        {% withNodeInfo($1, partial_1!(CInitList, reverse($2))) }


initializer_opt :: { Option<CInit> }
initializer_opt
  : {- empty -}                 { None }
  | '=' initializer             { Some($2) }


initializer_list :: { Reversed<CInitList> }
initializer_list
  : {- empty -}                                         { empty() }
  | initializer                                         { singleton((vec![], $1)) }
  | designation initializer                             { singleton(($1, $2)) }
  | initializer_list ',' initializer                    { snoc($1, (vec![], $3)) }
  | initializer_list ',' designation initializer        { snoc($1, ($3, $4)) }


-- designation
--
-- * GNU extensions:
--     old style member designation: 'ident :'
--     array range designation
--
designation :: { Vec<CDesignator> }
designation
  : designator_list '='         { reverse($1) }
  | identifier ':'              {% withNodeInfo($1.clone(), box |_0| vec![CMemberDesig($1, _0)]) }
  | array_designator            { vec![$1] }


designator_list :: { Reversed<Vec<CDesignator>> }
designator_list
 : designator                           { singleton($1) }
 | designator_list designator           { snoc($1, $2) }


designator :: { CDesignator }
designator
  : '[' constant_expression ']'         {% withNodeInfo($1, partial_1!(CArrDesig, $2)) }
  | '.' identifier                      {% withNodeInfo($1, partial_1!(CMemberDesig, $2)) }
  | array_designator                    { $1 }


array_designator :: { CDesignator }
array_designator
  : '[' constant_expression "..." constant_expression ']'
        {% withNodeInfo($1, partial_1!(CRangeDesig, $2, $4)) }


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
primary_expression :: { CExpr }
primary_expression
  : ident                {% withNodeInfo($1.clone(), partial_1!(CVar, $1)) }
  | constant             { CConst($1) }
  | string_literal       { CConst(liftStrLit($1)) }
  | '(' expression ')'   { $2 }
  | "_Generic" '(' assignment_expression ',' generic_assoc_list ')'
        {% withNodeInfo($1, partial_1!(CGenericSelection, box $3, reverse($5))) }
  -- GNU extensions
  | '(' compound_statement ')'
        {% withNodeInfo($1, partial_1!(CStatExpr, box $2)) }

  | "__builtin_va_arg" '(' assignment_expression ',' type_name ')'
        {% withNodeInfo($1, box move |_0| CBuiltinExpr(box CBuiltinVaArg($3, $5, _0))) }

  | "__builtin_offsetof" '(' type_name ',' offsetof_member_designator ')'
        {% withNodeInfo($1, box move |_0| CBuiltinExpr(box CBuiltinOffsetOf($3, reverse($5), _0))) }

  | "__builtin_types_compatible_p" '(' type_name ',' type_name ')'
        {% withNodeInfo($1, box move |_0| CBuiltinExpr(box CBuiltinTypesCompatible($3, $5, _0))) }

-- Generic Selection association list (C11 6.5.1.1)
--
-- TODO: introduce AST type for generic association
generic_assoc_list :: { Reversed<Vec<(Option<CDecl>, CExpr)>> }
  : generic_assoc_list ',' generic_assoc { snoc($1, $3) }
  | generic_assoc                        { singleton($1) }
generic_assoc :: { (Option<CDecl>, CExpr) }
generic_assoc
  : type_name ':' assignment_expression { (Some($1), $3) }
  | default   ':' assignment_expression { (None, $3) }

offsetof_member_designator :: { Reversed<Vec<CDesignator>> }
offsetof_member_designator
  : identifier
        {% withNodeInfo($1.clone(), box move |_0| singleton(CMemberDesig($1, _0))) }
  | offsetof_member_designator '.' identifier
        {% withNodeInfo($3.clone(), box move |_0| snoc($1, CMemberDesig($3, _0))) }
  | offsetof_member_designator '[' expression ']'
        {% withNodeInfo($3.clone(), box move |_0| snoc($1, CArrDesig($3, _0))) }


-- parse C postfix expression (C99 6.5.2)
--
postfix_expression :: { CExpr }
postfix_expression
  : primary_expression
        { $1 }

  | postfix_expression '[' expression ']'
        {% withNodeInfo($1.clone(), partial_1!(CIndex, box $1, box $3)) }

  | postfix_expression '(' ')'
        {% withNodeInfo($1.clone(), partial_1!(CCall, box $1, vec![])) }

  | postfix_expression '(' argument_expression_list ')'
        {% withNodeInfo($1.clone(), partial_1!(CCall, box $1, reverse($3))) }

  | postfix_expression '.' identifier
        {% withNodeInfo($1.clone(), partial_1!(CMember, box $1, $3, false)) }

  | postfix_expression "->" identifier
        {% withNodeInfo($1.clone(), partial_1!(CMember, box $1, $3, true)) }

  | postfix_expression "++"
        {% withNodeInfo($1.clone(), partial_1!(CUnary, CPostIncOp, box $1)) }

  | postfix_expression "--"
        {% withNodeInfo($1.clone(), partial_1!(CUnary, CPostDecOp, box $1)) }

  | '(' type_name ')' '{' initializer_list '}'
        {% withNodeInfo($1, partial_1!(CCompoundLit, box $2, reverse($5))) }

  | '(' type_name ')' '{' initializer_list ',' '}'
        {% withNodeInfo($1, partial_1!(CCompoundLit, box $2, reverse($5))) }


argument_expression_list :: { Reversed<Vec<CExpr>> }
argument_expression_list
  : assignment_expression                               { singleton($1) }
  | argument_expression_list ',' assignment_expression  { snoc($1, $3) }


-- parse C unary expression (C99 6.5.3)
--
-- * GNU extensions:
--     'alignof' expression or type
--     '__real' and '__imag' expression
--     '__extension__' to suppress warnings about extensions
--     allow taking address of a label with: && label
--
unary_expression :: { CExpr }
unary_expression
  : postfix_expression                  { $1 }
  | "++" unary_expression               {% withNodeInfo($1, partial_1!(CUnary, CPreIncOp, box $2)) }
  | "--" unary_expression               {% withNodeInfo($1, partial_1!(CUnary, CPreDecOp, box $2)) }
  | "__extension__" cast_expression     { $2 }
  | unary_operator cast_expression      {% withNodeInfo($1.clone(), partial_1!(CUnary, unL($1), box $2)) }
  | sizeof unary_expression             {% withNodeInfo($1, partial_1!(CSizeofExpr, box $2)) }
  | sizeof '(' type_name ')'            {% withNodeInfo($1, partial_1!(CSizeofType, box $3)) }
  -- GNU: alignof, complex and && extension
  | alignof unary_expression            {% withNodeInfo($1, partial_1!(CAlignofExpr, box $2)) }
  | alignof '(' type_name ')'           {% withNodeInfo($1, partial_1!(CAlignofType, box $3)) }
  | "__real__" unary_expression         {% withNodeInfo($1, partial_1!(CComplexReal, box $2)) }
  | "__imag__" unary_expression         {% withNodeInfo($1, partial_1!(CComplexImag, box $2)) }
  | "&&" identifier                     {% withNodeInfo($1, partial_1!(CLabAddrExpr, $2)) }


unary_operator :: { Located<CUnaryOp> }
unary_operator
  : '&'         { Located(CAdrOp,  posOf($1)) }
  | '*'         { Located(CIndOp,  posOf($1)) }
  | '+'         { Located(CPlusOp, posOf($1)) }
  | '-'         { Located(CMinOp,  posOf($1)) }
  | '~'         { Located(CCompOp, posOf($1)) }
  | '!'         { Located(CNegOp,  posOf($1)) }


-- parse C cast expression (C99 6.5.4)
--
cast_expression :: { CExpr }
cast_expression
  : unary_expression                    { $1 }
  | '(' type_name ')' cast_expression   {% withNodeInfo($1, partial_1!(CCast, box $2, box $4)) }


-- parse C multiplicative expression (C99 6.5.5)
--
multiplicative_expression :: { CExpr }
multiplicative_expression
  : cast_expression
        { $1 }

  | multiplicative_expression '*' cast_expression
        {% withNodeInfo($1.clone(), partial_1!(CBinary, CMulOp, box $1, box $3)) }

  | multiplicative_expression '/' cast_expression
        {% withNodeInfo($1.clone(), partial_1!(CBinary, CDivOp, box $1, box $3)) }

  | multiplicative_expression '%' cast_expression
        {% withNodeInfo($1.clone(), partial_1!(CBinary, CRmdOp, box $1, box $3)) }


-- parse C additive expression (C99 6.5.6)
--
additive_expression :: { CExpr }
additive_expression
  : multiplicative_expression
        { $1 }

  | additive_expression '+' multiplicative_expression
        {% withNodeInfo($1.clone(), partial_1!(CBinary, CAddOp, box $1, box $3)) }

  | additive_expression '-' multiplicative_expression
        {% withNodeInfo($1.clone(), partial_1!(CBinary, CSubOp, box $1, box $3)) }


-- parse C shift expression (C99 6.5.7)
--
shift_expression :: { CExpr }
shift_expression
  : additive_expression
        { $1 }

  | shift_expression "<<" additive_expression
        {% withNodeInfo($1.clone(), partial_1!(CBinary, CShlOp, box $1, box $3)) }

  | shift_expression ">>" additive_expression
        {% withNodeInfo($1.clone(), partial_1!(CBinary, CShrOp, box $1, box $3)) }


-- parse C relational expression (C99 6.5.8)
--
relational_expression :: { CExpr }
relational_expression
  : shift_expression
        { $1 }

  | relational_expression '<' shift_expression
        {% withNodeInfo($1.clone(), partial_1!(CBinary, CLeOp, box $1, box $3)) }

  | relational_expression '>' shift_expression
        {% withNodeInfo($1.clone(), partial_1!(CBinary, CGrOp, box $1, box $3)) }

  | relational_expression "<=" shift_expression
        {% withNodeInfo($1.clone(), partial_1!(CBinary, CLeqOp, box $1, box $3)) }

  | relational_expression ">=" shift_expression
        {% withNodeInfo($1.clone(), partial_1!(CBinary, CGeqOp, box $1, box $3)) }


-- parse C equality expression (C99 6.5.9)
--
equality_expression :: { CExpr }
equality_expression
  : relational_expression
        { $1 }

  | equality_expression "==" relational_expression
        {% withNodeInfo($1.clone(), partial_1!(CBinary, CEqOp, box $1, box $3)) }

  | equality_expression "!=" relational_expression
        {% withNodeInfo($1.clone(), partial_1!(CBinary, CNeqOp, box $1, box $3)) }


-- parse C bitwise and expression (C99 6.5.10)
--
and_expression :: { CExpr }
and_expression
  : equality_expression
        { $1 }

  | and_expression '&' equality_expression
        {% withNodeInfo($1.clone(), partial_1!(CBinary, CAndOp, box $1, box $3)) }


-- parse C bitwise exclusive or expression (C99 6.5.11)
--
exclusive_or_expression :: { CExpr }
exclusive_or_expression
  : and_expression
        { $1 }

  | exclusive_or_expression '^' and_expression
        {% withNodeInfo($1.clone(), partial_1!(CBinary, CXorOp, box $1, box $3)) }


-- parse C bitwise or expression (C99 6.5.12)
--
inclusive_or_expression :: { CExpr }
inclusive_or_expression
  : exclusive_or_expression
        { $1 }

  | inclusive_or_expression '|' exclusive_or_expression
        {% withNodeInfo($1.clone(), partial_1!(CBinary, COrOp, box $1, box $3)) }


-- parse C logical and expression (C99 6.5.13)
--
logical_and_expression :: { CExpr }
logical_and_expression
  : inclusive_or_expression
        { $1 }

  | logical_and_expression "&&" inclusive_or_expression
        {% withNodeInfo($1.clone(), partial_1!(CBinary, CLndOp, box $1, box $3)) }


-- parse C logical or expression (C99 6.5.14)
--
logical_or_expression :: { CExpr }
logical_or_expression
  : logical_and_expression
        { $1 }

  | logical_or_expression "||" logical_and_expression
        {% withNodeInfo($1.clone(), partial_1!(CBinary, CLorOp, box $1, box $3)) }


-- parse C conditional expression (C99 6.5.15)
--
-- * GNU extensions:
--     omitting the `then' part
conditional_expression :: { CExpr }
conditional_expression
  : logical_or_expression
        { $1 }

  | logical_or_expression '?' expression ':' conditional_expression
        {% withNodeInfo($1.clone(), partial_1!(CCond, box $1, Some(box $3), box $5)) }

  | logical_or_expression '?' ':' conditional_expression
        {% withNodeInfo($1.clone(), partial_1!(CCond, box $1, None, box $4)) }


-- parse C assignment expression (C99 6.5.16)
--
-- * NOTE: LHS of assignment is more restricted than in gcc.
--         `x ? y : z = 3' parses in gcc as `(x ? y : z) = 3',
--         but `x ? y : z' is not an unary expression.
assignment_expression :: { CExpr }
assignment_expression
  : conditional_expression
        { $1 }

  | unary_expression assignment_operator assignment_expression
        {% withNodeInfo($1.clone(), partial_1!(CAssign, unL($2), box $1, box $3)) }


assignment_operator :: { Located<CAssignOp> }
assignment_operator
  : '='                 { Located(CAssignOp, posOf($1)) }
  | "*="                { Located(CMulAssOp, posOf($1)) }
  | "/="                { Located(CDivAssOp, posOf($1)) }
  | "%="                { Located(CRmdAssOp, posOf($1)) }
  | "+="                { Located(CAddAssOp, posOf($1)) }
  | "-="                { Located(CSubAssOp, posOf($1)) }
  | "<<="               { Located(CShlAssOp, posOf($1)) }
  | ">>="               { Located(CShrAssOp, posOf($1)) }
  | "&="                { Located(CAndAssOp, posOf($1)) }
  | "^="                { Located(CXorAssOp, posOf($1)) }
  | "|="                { Located(COrAssOp,  posOf($1)) }


-- parse C expression (C99 6.5.17)
--
expression :: { CExpr }
expression
  : assignment_expression
        { $1 }

  | assignment_expression ',' comma_expression
        {%
            let es = reverse($3);
            withNodeInfo(es.clone(), partial_1!(CComma, __op_concat($1, es)))
        }

comma_expression :: { Reversed<Vec<CExpr>> }
comma_expression
  : assignment_expression                       { singleton($1) }
  | comma_expression ',' assignment_expression  { snoc($1, $3) }


-- The following was used for clarity
expression_opt :: { Option<CExpr> }
expression_opt
  : {- empty -}         { None }
  | expression          { Some($1) }


-- The following was used for clarity
assignment_expression_opt :: { Option<CExpr> }
assignment_expression_opt
  : {- empty -}                         { None }
  | assignment_expression               { Some($1) }


-- parse C constant expression (C99 6.6)
--
constant_expression :: { CExpr }
constant_expression
  : conditional_expression              { $1 }


-- parse C constants
--
constant :: { CConst }
constant
  : cint        {%
                    withNodeInfo($1.clone(), box move |_0| {
                        // TODO: I don't get why this is a Fn closure...
                        if let CTokILit(_, ref i) = $1 {
                            CIntConst(i.clone(), _0)
                        } else {
                            panic!("irrefutable pattern")
                        }
                    })
                }
  | cchar       {%
                    withNodeInfo($1.clone(), box move |_0| {
                        if let CTokCLit(_, ref c) = $1 {
                            CCharConst(c.clone(), _0)
                        } else {
                            panic!("irrefutable pattern")
                        }
                    })
                }
  | cfloat      {%
                    withNodeInfo($1.clone(), box move |_0| {
                        if let CTokFLit(_, ref f) = $1 {
                            CFloatConst(f.clone(), _0)
                        } else {
                            panic!("irrefutable pattern")
                        }
                    })
                }


string_literal :: { CStrLit }
string_literal
  : cstr
        {%
            withNodeInfo($1.clone(), box move |_0| {
                if let CTokSLit(_, ref s) = $1 {
                    CStringLiteral(s.clone(), _0)
                } else {
                    panic!("irrefutable pattern")
                }
            })
        }

  | cstr string_literal_list
        {%
            withNodeInfo($1.clone(), box move |_0| {
                if let CTokSLit(_, s) = $1 {
                    CStringLiteral(concatCStrings(__op_concat(s, reverse($2))), _0)
                } else {
                    panic!("irrefutable pattern")
                }
            })
        }


string_literal_list :: { Reversed<Vec<CString>> }
string_literal_list
  : cstr                        {
                                    if let CTokSLit(_, s) = $1 {
                                        singleton(s)
                                    } else {
                                        panic!("irrefutable pattern")
                                    }
                                }
  | string_literal_list cstr    {
                                    if let CTokSLit(_, s) = $2 {
                                        snoc($1, s)
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
  | attrs attr      { __op_addadd($1, $2) }

attr :: { Vec<CAttr> }
attr
  : "__attribute__" '(' '(' attribute_list ')' ')'      { reverse($4) }

attribute_list :: { Reversed<Vec<CAttr>> }
  : attribute                      {
                                       match $1 {
                                           None => empty(),
                                           Some(attr) => singleton(attr),
                                       }
                                   }
  | attribute_list ',' attribute   {
                                       match $3 {
                                           Some(inner) => snoc($1, inner),
                                           None => $1,
                                       }
                                   }


attribute :: { Option<CAttr> }
attribute
  : {- empty -}                         { None }
  | ident
        {% withNodeInfo($1.clone(), box move |_0| Some(CAttribute($1, vec![], _0))) }
  | const
        {% withNodeInfo($1, box move |_0| Some(CAttribute(Ident::internal("const".into()), vec![], _0))) }
  | ident '(' attribute_params ')'
        {% withNodeInfo($1.clone(), box move |_0| Some(CAttribute($1, reverse($3), _0))) }
  | ident '(' ')'
        {% withNodeInfo($1.clone(), box move |_0| Some(CAttribute($1, vec![], _0))) }

-- OS X 10.9 (Mavericks) makes use of more liberal attribute syntax
-- that includes assignment-like expressions referencing version
-- numbers.

attribute_params :: { Reversed<Vec<CExpr>> }
attribute_params
  : constant_expression                                        { singleton($1) }
  | unary_expression assignment_operator clang_version_literal { Reversed(vec![]) }
  | unary_expression assignment_operator unary_expression      { Reversed(vec![]) }
  | attribute_params ',' constant_expression                   { snoc($1, $3) }
  | attribute_params ',' unary_expression assignment_operator unary_expression      { $1 }
  | attribute_params ',' unary_expression assignment_operator clang_version_literal { $1 }

{

/// sometimes it is neccessary to reverse an unreversed list
fn reverseList<a>(l: Vec<a>) -> Reversed<Vec<a>> {
    Reversed(List::reverse(l))
}

/// We occasionally need things to have a location when they don't naturally
/// have one built in as tokens and most AST elements do.
#[derive(Clone)]
pub struct Located<T>(T, Position);

impl<T> Pos for Located<T> {
    fn posOf(self) -> Position {
        self.1
    }
}

fn unL<T>(Located(a, pos): Located<T>) -> T {
    a
}

fn withNodeInfo<T: 'static, N: Pos + 'static>(node: N, mkAttrNode: Box<FnBox(NodeInfo) -> T>) -> P<T> {
    thenP(getNewName(), box |name: Name| {
        thenP(getSavedToken(), box move |lastTok| {
            let firstPos = posOf(node);
            let attrs = NodeInfo::new(firstPos, posLenOfTok(lastTok), name);
            returnP(mkAttrNode(attrs))
        })
    })
}

fn withLength<a: Clone + 'static>(nodeinfo: NodeInfo, mkAttrNode: Box<FnBox(NodeInfo) -> a>) -> P<a> {
    thenP(getSavedToken(), box move |lastTok| {
        let firstPos = nodeinfo.clone().pos();
        let attrs = NodeInfo::new(firstPos, posLenOfTok(lastTok),
                                  nodeinfo.name().unwrap_or_else(|| panic!("nameOfNode")));
        returnP(mkAttrNode(attrs))
    })
}

#[derive(Clone)]
pub struct CDeclrR(Option<Ident>,
                   Reversed<Vec<CDerivedDeclr>>,
                   Option<CStringLiteral<NodeInfo>>,
                   Vec<CAttribute<NodeInfo>>,
                   NodeInfo);

impl CNode for CDeclrR {
    fn nodeInfo(self) -> NodeInfo {
        self.4
    }
}

impl Pos for CDeclrR {
    fn posOf(self) -> Position {
        posOf(self.4)
    }
}

fn reverseDeclr(CDeclrR(ide, reversedDDs, asmname, cattrs, at): CDeclrR) -> CDeclarator<NodeInfo> {
    CDeclarator::<NodeInfo>(ide, reverse(reversedDDs), asmname, cattrs, at)
}

fn withAttribute<node: Pos + 'static>(node: node, cattrs: Vec<CAttribute<NodeInfo>>,
                                      mkDeclrNode: Box<FnBox(NodeInfo) -> CDeclrR>) -> P<CDeclrR> {
    thenP(getNewName(), box move |name| {
        let attrs = NodeInfo::with_pos_name(node.posOf(), name);
        let newDeclr = appendDeclrAttrs(cattrs.clone(), mkDeclrNode(attrs));
        returnP(newDeclr)
    })
}

fn withAttributePF<N: Pos + 'static>(
    node: N, cattrs: Vec<CAttribute<NodeInfo>>,
    mkDeclrCtor: Box<Fn(NodeInfo, CDeclrR) -> CDeclrR>) -> P<Rc<Box<Fn(CDeclrR) -> CDeclrR>>>
{
    let mkDeclrCtor = Rc::new(mkDeclrCtor);
    thenP(getNewName(), box move |name| {
        let attrs = NodeInfo::with_pos_name(node.posOf(), name);
        let newDeclr: Rc<Box<Fn(CDeclrR) -> CDeclrR>> = Rc::new(box move |_0| {
            appendDeclrAttrs(cattrs.clone(), mkDeclrCtor(attrs.clone(), _0))
        });
        returnP(newDeclr)
    })
}

fn appendObjAttrs(newAttrs: Vec<CAttribute<NodeInfo>>,
                  CDeclarator(ident, indirections, asmname, cAttrs, at): CDeclarator<NodeInfo>)
                  -> CDeclarator<NodeInfo> {
    CDeclarator(ident, indirections, asmname, __op_addadd(cAttrs, newAttrs), at)
}

fn appendObjAttrsR(newAttrs: Vec<CAttribute<NodeInfo>>,
                   CDeclrR(ident, indirections, asmname, cAttrs, at): CDeclrR) -> CDeclrR {
    CDeclrR(ident, indirections, asmname, __op_addadd(cAttrs, newAttrs), at)
}

fn setAsmName(mAsmName: Option<CStringLiteral<NodeInfo>>,
              CDeclrR(ident, indirections, oldName, cattrs, at): CDeclrR) -> P<CDeclrR> {

    let combinedName = match (mAsmName, oldName) {
        (None, None) => Right(None),
        (None, Some(oldname)) => Right(Some(oldname)),
        (Some(newname), None) => Right(Some(newname)),
        (Some(n1), Some(n2)) => Left((n1, n2)),
    };

    let showName = |CStringLiteral(cstr, _)| show(cstr);

    match combinedName {
        Left((n1, n2)) => {
            failP(posOf(n2.clone()),
                  vec!["Duplicate assembler name: ".to_string(), showName(n1), showName(n2)])
        },
        Right(newName) => {
            returnP(CDeclrR(ident, indirections, newName, cattrs, at))
        },
    }
}

fn withAsmNameAttrs((mAsmName, newAttrs): (Option<CStringLiteral<NodeInfo>>, Vec<CAttribute<NodeInfo>>),
                    declr: CDeclrR) -> P<CDeclrR> {
    setAsmName(mAsmName, appendObjAttrsR(newAttrs, declr))
}

fn appendDeclrAttrs(newAttrs: Vec<CAttribute<NodeInfo>>, declr: CDeclrR) -> CDeclrR {
    let CDeclrR(ident, Reversed(mut inner), asmname, cattrs, at) = declr;
    if inner.len() == 0 {
        CDeclrR(ident, empty(), asmname, __op_addadd(cattrs, newAttrs), at)
    } else {
        let x = inner.remove(0);
        let xs = inner;
        let appendAttrs = |_0| match _0 {
            CPtrDeclr(typeQuals, at) =>
                CPtrDeclr(__op_addadd(typeQuals, __map!(CAttrQual, newAttrs)), at),
            CArrDeclr(typeQuals, arraySize, at) =>
                CArrDeclr(__op_addadd(typeQuals, __map!(CAttrQual, newAttrs)), arraySize, at),
            CFunDeclr(parameters, cattrs, at) =>
                CFunDeclr(parameters, __op_addadd(cattrs, newAttrs), at),
        };
        CDeclrR(ident, Reversed(__op_concat(appendAttrs(x), xs)), asmname, cattrs, at)
    }
}

fn ptrDeclr(CDeclrR(ident, derivedDeclrs, asmname, cattrs, dat): CDeclrR,
            tyquals: Vec<CTypeQual>, at: NodeInfo) -> CDeclrR {
    CDeclrR(ident, snoc(derivedDeclrs, CPtrDeclr(tyquals, at)), asmname, cattrs, dat)
}

fn funDeclr(CDeclrR(ident, derivedDeclrs, asmname, dcattrs, dat): CDeclrR,
            params: Either<Vec<Ident>, (Vec<CDecl>, bool)>, cattrs: Vec<CAttribute<NodeInfo>>,
            at: NodeInfo) -> CDeclrR {
    CDeclrR(ident, snoc(derivedDeclrs, CFunDeclr(params, cattrs, at)), asmname, dcattrs, dat)
}

fn arrDeclr(CDeclrR(ident, derivedDeclrs, asmname, cattrs, dat): CDeclrR,
            tyquals: Vec<CTypeQual>, var_sized: bool, static_size: bool,
            size_expr_opt: Option<CExpr>, at: NodeInfo) -> CDeclrR {
    let arr_sz = match size_expr_opt {
        Some(e) => CArrSize(static_size, e),
        None => CNoArrSize(var_sized)
    };

    CDeclrR(ident, snoc(derivedDeclrs, CArrDeclr(tyquals, arr_sz, at)), asmname, cattrs, dat)
}

fn liftTypeQuals(_curry_0: Reversed<Vec<CTypeQual>>) -> Vec<CDeclSpec> {
    __map!(CTypeQual, reverse(_curry_0))
}

fn liftCAttrs(_curry_0: Vec<CAttribute<NodeInfo>>) -> Vec<CDeclSpec> {
    __map!(|_0| CTypeQual(CAttrQual(_0)), _curry_0)
}

fn addTrailingAttrs(declspecs: Reversed<Vec<CDeclSpec>>,
                    new_attrs: Vec<CAttribute<NodeInfo>>) -> Reversed<Vec<CDeclSpec>> {
    match viewr(declspecs.clone()) {
        (specs_init, CTypeSpec(CSUType(CStructureUnion(tag, name, Some(def), def_attrs, su_node), node))) => {
            snoc(specs_init, CTypeSpec(CSUType(
                CStructureUnion(tag, name, Some(def), __op_addadd(def_attrs, new_attrs), su_node), node)))
        },
        (specs_init, CTypeSpec(CEnumType(CEnumeration(name, Some(def), def_attrs, e_node), node))) => {
            snoc(specs_init, CTypeSpec(CEnumType(
                CEnumeration(name, Some(def), __op_addadd(def_attrs, new_attrs), e_node), node)))
        },
        _ => {
            rappend(declspecs, liftCAttrs(new_attrs))
        },
    }
}

// convenient instance, the position of a list of things is the position of
// the first thing in the list

impl<A: Pos> Pos for Vec<A> {
    fn posOf(mut self) -> Position {
        let item = self.remove(0);
        posOf(item)
    }
}

impl<A: Pos> Pos for Reversed<A> {
    fn posOf(self) -> Position {
        posOf(self.0)
    }
}

fn emptyDeclr() -> CDeclrR {
    CDeclrR(None, empty(), None, vec![], NodeInfo::undef())
}

fn mkVarDeclr(ident: Ident, ni: NodeInfo) -> CDeclrR {
    CDeclrR(Some(ident), empty(), None, vec![], ni)
}

fn doDeclIdent(declspecs: &[CDeclSpec], CDeclrR(mIdent, _, _, _, _): CDeclrR) -> P<()> {
    let is_typedef = |declspec: &CDeclSpec| match *declspec {
        CStorageSpec(CTypedef(_)) => true,
        _ => false,
    };

    match mIdent {
        None => returnP(()),
        Some(ident) => {
            if declspecs.iter().any(is_typedef) {
                addTypedef(ident)
            } else {
                shadowTypedef(ident)
            }
        },
    }
}

fn doFuncParamDeclIdent(_0: CDeclarator<NodeInfo>) -> P<()> {
    match (_0) {
        CDeclarator(_, ref arg, _, _, _)
            if arg.len() == 2 && matches!(arg[0].clone(), CFunDeclr(params, _, _)) => {
                let params = if let CFunDeclr(params, _, _) = arg[0].clone() { params } else { unreachable!() };
            //TODO
            let arg = arg.clone();
            unreachable!();
            // params.map(|x| fst(x)).unwrap_or(vec![])
                // .flat_map(|CDecl(_, dle, _)| dle)
                // .flat_map(|(declr, _, _)| declr.map(|x| vec![x]).unwrap_or(vec![]))
                // .map(|declr| {
                //     match getCDeclrIdent(declr) {
                //         None => P()
                //         Some(ident) => shadowTypedef(ident)
                //     }
                // })
            // TODO thread P through this
        },
        _ => {
            returnP(())
        },
    }
}

fn getCDeclrIdent(CDeclarator(mIdent, _, _, _, _): CDeclarator<NodeInfo>) -> Option<Ident> {
    mIdent
}

fn happyError<a: 'static>() -> P<a> {
    parseError()
}

pub fn parseC(input: InputStream, initialPosition: Position) -> Result<CTranslationUnit<NodeInfo>, ParseError> {
    execParser(translUnitP(), input, initialPosition, builtinTypeNames(), namesStartingFrom(0))
        .map(|x| x.0)
}

pub fn translUnitP() -> P<CTranslationUnit<NodeInfo>> {
    translation_unit()
}

pub fn extDeclP() -> P<CExtDecl> {
    external_declaration()
}

pub fn statementP() -> P<CStat> {
    statement()
}

pub fn expressionP() -> P<CExpr> {
    expression()
}

}
