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

use std::boxed::FnBox;
use either::Either::*;

use data::position::Located;
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
"__attribute__" { CTokGnuC(_, GnuCAttrTok) }    -- special GNU C tokens
"__extension__" { CTokGnuC(_, GnuCExtTok) }     -- special GNU C tokens
"__real__"      { CTokGnuC(_, GnuCComplexReal) }
"__imag__"      { CTokGnuC(_, GnuCComplexImag) }
-- special GNU C builtin 'functions' that actually take types as parameters:
"__builtin_va_arg"              { CTokGnuC(_, GnuCVaArg) }
"__builtin_offsetof"            { CTokGnuC(_, GnuCOffsetof) }
"__builtin_types_compatible_p"  { CTokGnuC(_, GnuCTyCompat) }
clangcversion   { CTokClangC(_, ClangCTok($$)) } -- Clang version literal

%%


-- parse a complete C translation unit
-- we have to take special care of empty translation units
translation_unit :: { CTranslUnit }
translation_unit
  : ext_decl_list {%
                      let decls = $1;
                      if decls.len() == 0 {
                          let name = p.getNewName();
                          let pos = p.getPosClone();
                          let nodeinfo = NodeInfo::new(pos.clone(), (pos, 0), name);
                          Ok(CTranslationUnit(decls, nodeinfo))
                      } else {
                          let d = decls[0].clone();
                          p.withNodeInfo(d, |at| CTranslationUnit(decls, at))
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
  | ext_decl_list external_declaration  { appended($1, $2) }


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
  | asm '(' string_literal ')' ';'                {% p.withNodeInfo($1, |at| CAsmExt($3, at)) }


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
        {% p.leaveScope(); p.withNodeInfo($1.clone(), |at| CFunctionDef(vec![], $1, vec![], $2, at)) }

  | attrs                      function_declarator compound_statement
        {% p.leaveScope(); p.withNodeInfo($1.clone(), |at| CFunctionDef(liftCAttrs($1), $2, vec![], $3, at)) }

  | declaration_specifier      function_declarator compound_statement
        {% p.leaveScope(); p.withNodeInfo($1.clone(), |at| CFunctionDef($1, $2, vec![], $3, at)) }

  | type_specifier             function_declarator compound_statement
        {% p.leaveScope(); p.withNodeInfo($1.clone(), |at| CFunctionDef($1, $2, vec![], $3, at)) }

  | declaration_qualifier_list function_declarator compound_statement
        {% p.leaveScope(); p.withNodeInfo($1.clone(), |at| CFunctionDef($1, $2, vec![], $3, at)) }

  | type_qualifier_list        function_declarator compound_statement
        {% p.leaveScope(); p.withNodeInfo($1.clone(), |at| CFunctionDef(liftTypeQuals($1), $2, vec![], $3, at)) }

  | type_qualifier_list  attrs function_declarator compound_statement
        {% p.leaveScope(); p.withNodeInfo($1.clone(), |at| CFunctionDef(addVecs(liftTypeQuals($1), liftCAttrs($2)), $3, vec![], $4, at)) }

  -- old function declarators

  |                            function_declarator_old declaration_list compound_statement
        {% p.withNodeInfo($1.clone(), |at| CFunctionDef(vec![], $1, $2, $3, at)) }

  |                      attrs function_declarator_old declaration_list compound_statement
        {% p.withNodeInfo($2.clone(), |at| CFunctionDef(liftCAttrs($1), $2, $3, $4, at)) }

  | declaration_specifier      function_declarator_old declaration_list compound_statement
        {% p.withNodeInfo($1.clone(), |at| CFunctionDef($1, $2, $3, $4, at)) }

  | type_specifier             function_declarator_old declaration_list compound_statement
        {% p.withNodeInfo($1.clone(), |at| CFunctionDef($1, $2, $3, $4, at)) }

  | declaration_qualifier_list function_declarator_old declaration_list compound_statement
        {% p.withNodeInfo($1.clone(), |at| CFunctionDef($1, $2, $3, $4, at)) }

  | type_qualifier_list   function_declarator_old declaration_list compound_statement
        {% p.withNodeInfo($1.clone(), |at| CFunctionDef(liftTypeQuals($1), $2, $3, $4, at)) }

  | type_qualifier_list attrs  function_declarator_old declaration_list compound_statement
        {% p.withNodeInfo($1.clone(), |at| CFunctionDef(addVecs(liftTypeQuals($1), liftCAttrs($2)),
                                                        $3, $4, $5, at)) }

-- Read declarator and put function
function_declarator :: { CDeclr }
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
statement :: { CStat }
statement
  : labeled_statement           { $1 }
  | compound_statement          { $1 }
  | expression_statement        { $1 }
  | selection_statement         { $1 }
  | iteration_statement         { $1 }
  | jump_statement              { $1 }
  | asm_statement               {% p.withNodeInfo($1.clone(), |at| CAsm($1, at)) }


-- parse C labeled statement (C99 6.8.1)
--
-- * GNU extension: case ranges
--
labeled_statement :: { CStat }
labeled_statement
  : identifier ':' attrs_opt statement          {% p.withNodeInfo($1.clone(), |at| CLabel($1, box $4, $3, at)) }
  | case constant_expression ':' statement      {% p.withNodeInfo($1, |at| CCase($2, box $4, at)) }
  | default ':' statement                       {% p.withNodeInfo($1, |at| CDefault(box $3, at)) }
  | case constant_expression "..." constant_expression ':' statement
        {% p.withNodeInfo($1, |at| CCases($2, $4, box $6, at)) }


-- parse C compound statement (C99 6.8.2)
--
-- * GNU extension: '__label__ ident;' declarations
--
compound_statement :: { CStat }
compound_statement
  : '{' enter_scope block_item_list leave_scope '}'
        {% p.withNodeInfo($1, |at| CCompound(vec![], $3, at)) }

  | '{' enter_scope label_declarations block_item_list leave_scope '}'
        {% p.withNodeInfo($1, |at| CCompound($3, $4, at)) }


-- No syntax for these, just side effecting semantic actions.
--
enter_scope :: { () }
enter_scope : {% Ok(p.enterScope()) }
leave_scope :: { () }
leave_scope : {% Ok(p.leaveScope()) }


block_item_list :: { Vec<CBlockItem> }
block_item_list
  : {- empty -}                 { vec![] }
  | block_item_list block_item  { appended($1, $2) }

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
        {% p.leaveScope(); p.withNodeInfo($1.clone(), |at| CFunctionDef($1, $2, vec![], $3, at)) }

  | type_specifier             function_declarator compound_statement
        {% p.leaveScope(); p.withNodeInfo($1.clone(), |at| CFunctionDef($1, $2, vec![], $3, at)) }

  | declaration_qualifier_list function_declarator compound_statement
        {% p.leaveScope(); p.withNodeInfo($1.clone(), |at| CFunctionDef($1, $2, vec![], $3, at)) }

  | type_qualifier_list   function_declarator compound_statement
        {% p.leaveScope(); p.withNodeInfo($1.clone(), |at| CFunctionDef(liftTypeQuals($1), $2, vec![], $3, at)) }

  | type_qualifier_list   attrs function_declarator compound_statement
        {% p.leaveScope(); p.withNodeInfo($1.clone(), |at| CFunctionDef(addVecs(liftTypeQuals($1), liftCAttrs($2)), $3, vec![], $4, at)) }


label_declarations :: { Vec<Ident> }
label_declarations
  : "__label__" identifier_list ';'                     { $2 }
  | label_declarations "__label__" identifier_list ';'  { addVecs($1, $3) }


-- parse C expression statement (C99 6.8.3)
--
expression_statement :: { CStat }
expression_statement
  : ';'                         {% p.withNodeInfo($1, |at| CExpr(None, at)) }
  | expression ';'              {% p.withNodeInfo($1.clone(), |at| CExpr(Some($1), at)) }


-- parse C selection statement (C99 6.8.4)
--
selection_statement :: { CStat }
selection_statement
  : if '(' expression ')' statement
        {% p.withNodeInfo($1, |at| CIf($3, box $5, None, at)) }

  | if '(' expression ')' statement else statement
        {% p.withNodeInfo($1, |at| CIf($3, box $5, Some(box $7), at)) }

  | switch '(' expression ')' statement
        {% p.withNodeInfo($1, |at| CSwitch($3, box $5, at)) }


-- parse C iteration statement (C99 6.8.5)
--
iteration_statement :: { CStat }
iteration_statement
  : while '(' expression ')' statement
        {% p.withNodeInfo($1, |at| CWhile($3, box $5, false, at)) }

  | do statement while '(' expression ')' ';'
        {% p.withNodeInfo($1, |at| CWhile($5, box $2, true, at)) }

  | for '(' expression_opt ';' expression_opt ';' expression_opt ')' statement
        {% p.withNodeInfo($1, |at| CFor(Left($3), $5, $7, box $9, at)) }

  | for '(' enter_scope declaration expression_opt ';' expression_opt ')' statement leave_scope
        {% p.withNodeInfo($1, |at| CFor(Right($4), $5, $7, box $9, at)) }


-- parse C jump statement (C99 6.8.6)
--
-- * GNU extension: computed gotos
--
jump_statement :: { CStat }
jump_statement
  : goto identifier ';'                 {% p.withNodeInfo($1, |at| CGoto($2, at)) }
  | goto '*' expression ';'             {% p.withNodeInfo($1, |at| CGotoPtr($3, at)) }
  | continue ';'                        {% p.withNodeInfo($1, CCont) }
  | break ';'                           {% p.withNodeInfo($1, CBreak) }
  | return expression_opt ';'           {% p.withNodeInfo($1, |at| CReturn($2, at)) }


-- parse GNU C __asm__ statement (compatible with C99: J.5.10)
--
-- asm_stmt    :- asm volatile? ( "asm..." : output-operands : input-operands : asm-clobbers )
-- asm_operand :- [operand-name] "constraint" ( expr )
-- asm_clobber :- "r1", "r2", ...
--
asm_statement :: { CAsmStmt }
asm_statement
  : asm maybe_type_qualifier '(' string_literal ')' ';'
        {% p.withNodeInfo($1, |at| CAssemblyStatement($2, $4, vec![], vec![], vec![], at)) }

  | asm maybe_type_qualifier '(' string_literal ':' asm_operands ')' ';'
        {% p.withNodeInfo($1, |at| CAssemblyStatement($2, $4, $6, vec![], vec![], at)) }

  | asm maybe_type_qualifier '(' string_literal ':' asm_operands ':' asm_operands ')' ';'
        {% p.withNodeInfo($1, |at| CAssemblyStatement($2, $4, $6, $8, vec![], at)) }

  | asm maybe_type_qualifier '(' string_literal ':' asm_operands ':' asm_operands ':' asm_clobbers ')' ';'
        {% p.withNodeInfo($1, |at| CAssemblyStatement($2, $4, $6, $8, $10, at)) }


maybe_type_qualifier :: { Option<CTypeQual> }
maybe_type_qualifier
  : {- empty -}           { None }
  | type_qualifier        { Some($1) }

asm_operands :: { Vec<CAsmOperand> }
asm_operands
  : {- empty -}                         { vec![] }
  | nonnull_asm_operands                { $1 }

nonnull_asm_operands :: { Vec<CAsmOperand> }
nonnull_asm_operands
  : asm_operand                           { vec![$1] }
  | nonnull_asm_operands ',' asm_operand  { appended($1, $3) }

asm_operand :: { CAsmOperand }
asm_operand
  : string_literal '(' expression ')'
        {% p.withNodeInfo($1.clone(), |at| CAssemblyOperand(None, $1, $3, at)) }
  | '[' ident ']' string_literal '(' expression ')'
        {% p.withNodeInfo($1, |at| CAssemblyOperand(Some($2), $4, $6, at)) }
  | '[' tyident ']' string_literal '(' expression ')'
        {% p.withNodeInfo($1, |at| CAssemblyOperand(Some($2), $4, $6, at)) }


asm_clobbers :: { Vec<CStrLit> }
asm_clobbers
  : string_literal                      { vec![$1] }
  | asm_clobbers ',' string_literal     { appended($1, $3) }

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
        {% p.withNodeInfo($1.clone(), |at| CDecl($1, vec![], at)) }

  | sue_type_specifier ';'
        {% p.withNodeInfo($1.clone(), |at| CDecl($1, vec![], at)) }

  | declaring_list ';'
        {%
            if let CDecl(declspecs, dies, at) = $1 {
                p.withLength(at, |at| CDecl(declspecs, revVec(dies), at))
            } else {
                panic!("irrefutable pattern")
            }
        }

  | default_declaring_list ';'
        {%
            if let CDecl(declspecs, dies, at) = $1 {
                p.withLength(at, |at| CDecl(declspecs, revVec(dies), at))
            } else {
                panic!("irrefutable pattern")
            }
        }
  | "_Static_assert" '(' constant_expression ',' string_literal ')' ';'
        {% p.withNodeInfo($1, |at| CStaticAssert($3, $5, at)) }

declaration_list :: { Vec<CDecl> }
declaration_list
  : {- empty -}                       { vec![] }
  | declaration_list declaration      { appended($1, $2) }


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
            let declspecs = $1.clone();
            let declr = $2.withAsmNameAttrs($3)?;
            // TODO: borrow these instead
            p.doDeclIdent(&declspecs, declr.clone());
            p.withNodeInfo($1, |at| CDecl(declspecs, vec![(Some(declr.reverse()), $4, None)], at))
        }

  | type_qualifier_list identifier_declarator asm_attrs_opt {-{}-} initializer_opt
        {%
            let declspecs = liftTypeQuals($1.clone());
            let declr = $2.withAsmNameAttrs($3)?;
            p.doDeclIdent(&declspecs, declr.clone());
            p.withNodeInfo($1, |at| CDecl(declspecs, vec![(Some(declr.reverse()), $4, None)], at))
        }

  | type_qualifier_list attrs identifier_declarator asm_attrs_opt {-{}-} initializer_opt -- FIX 1600
        {%
            let declspecs = liftTypeQuals($1.clone());
            let declr = $3.withAsmNameAttrs($4)?;
            p.doDeclIdent(&declspecs, declr.clone());
            p.withNodeInfo($1, |at| CDecl(addVecs(declspecs, liftCAttrs($2)),
                                          vec![(Some(declr.reverse()), $5, None)], at))
        }

  -- GNU extension: __attribute__ as the only qualifier
  | attrs identifier_declarator asm_attrs_opt {-{}-} initializer_opt
        {%
            let declspecs = liftCAttrs($1.clone());
            let declr = $2.withAsmNameAttrs($3)?;
            p.doDeclIdent(&declspecs, declr.clone());
            p.withNodeInfo($1, |at| CDecl(declspecs, vec![(Some(declr.reverse()), $4, None)], at))
        }

  | default_declaring_list ',' attrs_opt identifier_declarator asm_attrs_opt {-{}-} initializer_opt
        {%
            if let CDecl(declspecs, dies, at) = $1 {
                let (f, s) = $5;
                let declr = $4.withAsmNameAttrs((f, addVecs(s, $3)))?;
                p.doDeclIdent(&declspecs, declr.clone());
                p.withLength(at, |at| CDecl(declspecs, prepend((Some(declr.reverse()), $6, None), dies), at))
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
            let declr = $2.withAsmNameAttrs($3)?;
            p.doDeclIdent(&$1, declr.clone());
            p.withNodeInfo($1.clone(), |at| CDecl($1, vec![(Some(declr.reverse()), $4, None)], at))
        }

  | type_specifier declarator asm_attrs_opt initializer_opt
        {%
            let declr = $2.withAsmNameAttrs($3)?;
            p.doDeclIdent(&$1, declr.clone());
            p.withNodeInfo($1.clone(), |at| CDecl($1, vec![(Some(declr.reverse()), $4, None)], at))
        }

  | declaring_list ',' attrs_opt declarator asm_attrs_opt initializer_opt
        {%
            if let CDecl(declspecs, dies, at) = $1 {
                let (f, s) = $5;
                let declr = $4.withAsmNameAttrs((f, addVecs(s, $3)))?;
                p.doDeclIdent(&declspecs, declr.clone());
                Ok(CDecl(declspecs, prepend((Some(declr.reverse()), $6, None), dies), at))
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
        { vec![$1] }

  | attrs declaration_qualifier_without_types
        { appended(liftCAttrs($1), $2) }

  | type_qualifier_list declaration_qualifier_without_types
        { appended(map(CTypeQual, $1), $2) }

  | type_qualifier_list attrs declaration_qualifier_without_types
        { appended(addVecs(map(CTypeQual, $1), liftCAttrs($2)), $3) }

  | declaration_qualifier_list declaration_qualifier
        { appended($1, $2) }

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
  : typedef                     {% p.withNodeInfo($1, CTypedef) }
  | extern                      {% p.withNodeInfo($1, CExtern) }
  | static                      {% p.withNodeInfo($1, CStatic) }
  | auto                        {% p.withNodeInfo($1, CAuto) }
  | register                    {% p.withNodeInfo($1, CRegister) }
  | "__thread"                  {% p.withNodeInfo($1, CThread) }

-- parse C function specifier (C11 6.7.4)
function_specifier :: { CFunSpec }
function_specifier
  : inline              {% p.withNodeInfo($1, CInlineQual) }
  | "_Noreturn"         {% p.withNodeInfo($1, CNoreturnQual) }

-- parse C alignment specifier (C11 6.7.5)
alignment_specifier :: { CAlignSpec }
alignment_specifier
  : alignas '(' type_name ')'           {% p.withNodeInfo($1, |at| CAlignAsType($3, at)) }
  | alignas '(' constant_expression ')' {% p.withNodeInfo($1, |at| CAlignAsExpr($3, at)) }

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

basic_type_name :: { CTypeSpec }
basic_type_name
  : void                        {% p.withNodeInfo($1, CVoidType) }
  | char                        {% p.withNodeInfo($1, CCharType) }
  | short                       {% p.withNodeInfo($1, CShortType) }
  | int                         {% p.withNodeInfo($1, CIntType) }
  | long                        {% p.withNodeInfo($1, CLongType) }
  | float                       {% p.withNodeInfo($1, CFloatType) }
  | double                      {% p.withNodeInfo($1, CDoubleType) }
  | signed                      {% p.withNodeInfo($1, CSignedType) }
  | unsigned                    {% p.withNodeInfo($1, CUnsigType) }
  | "_Bool"                     {% p.withNodeInfo($1, CBoolType) }
  | "_Complex"                  {% p.withNodeInfo($1, CComplexType) }
  | "__int128"                  {% p.withNodeInfo($1, CInt128Type) }


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
        { appended($1, CTypeSpec($2)) }

  | basic_type_specifier storage_class
        { appended($1, CStorageSpec($2)) }

  | basic_declaration_specifier declaration_qualifier
        { appended($1, $2) }

  | basic_declaration_specifier basic_type_name
        { appended($1, CTypeSpec($2)) }

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
        { vec![CTypeSpec($1)] }

  | attrs basic_type_name
        { appended(liftCAttrs($1), CTypeSpec($2)) }

  | type_qualifier_list basic_type_name
        { appended(map(CTypeQual, $1), CTypeSpec($2)) }

  | type_qualifier_list attrs basic_type_name
        { appended(addVecs(map(CTypeQual, $1), liftCAttrs($2)), CTypeSpec($3)) }

  | basic_type_specifier type_qualifier
        { appended($1, CTypeQual($2)) }

  | basic_type_specifier basic_type_name
        { appended($1, CTypeSpec($2)) }

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
        { appended($1, CTypeSpec($2)) }

  | sue_type_specifier storage_class
        { appended($1, CStorageSpec($2)) }

  | sue_declaration_specifier declaration_qualifier
        { appended($1, $2) }

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
        { vec![CTypeSpec($1)] }

  | attrs elaborated_type_name
        { appended(liftCAttrs($1), CTypeSpec($2)) }

  | type_qualifier_list elaborated_type_name
        { appended(map(CTypeQual, $1), CTypeSpec($2)) }

  | type_qualifier_list attrs elaborated_type_name
        { appended(addVecs(map(CTypeQual, $1), liftCAttrs($2)), CTypeSpec($3)) }

  | sue_type_specifier type_qualifier
        { appended($1, CTypeQual($2)) }

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
        { appended($1, CStorageSpec($2)) }

  | declaration_qualifier_list tyident
        {% p.withNodeInfo($2.clone(), |at| appended($1, CTypeSpec(CTypeDef($2, at)))) }

  | declaration_qualifier_list typeof '(' expression ')'
        {% p.withNodeInfo($2, |at| appended($1, CTypeSpec(CTypeOfExpr($4, at)))) }

  | declaration_qualifier_list typeof '(' type_name ')'
        {% p.withNodeInfo($2, |at| appended($1, CTypeSpec(CTypeOfType($4, at)))) }

  | typedef_declaration_specifier declaration_qualifier
        { appended($1, $2) }

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
        {% p.withNodeInfo($1.clone(), |at| vec![CTypeSpec(CTypeDef($1, at))]) }

  | typeof '(' expression ')'
        {% p.withNodeInfo($1, |at| vec![CTypeSpec(CTypeOfExpr($3, at))]) }

  | typeof '(' type_name ')'
        {% p.withNodeInfo($1, |at| vec![CTypeSpec(CTypeOfType($3, at))]) }

  | type_qualifier_list tyident
        {% p.withNodeInfo($2.clone(), |at| appended(map(CTypeQual, $1), CTypeSpec(CTypeDef($2, at)))) }

  | type_qualifier_list typeof '(' expression ')'
        {% p.withNodeInfo($2, |at| appended(map(CTypeQual, $1), CTypeSpec(CTypeOfExpr($4, at)))) }

  | type_qualifier_list typeof '(' type_name ')'
        {% p.withNodeInfo($2, |at| appended(map(CTypeQual, $1), CTypeSpec(CTypeOfType($4, at)))) }

  -- repeat with attrs (this could be easier if type qualifier list wouldn't allow leading attributes)
  | attrs tyident
        {% p.withNodeInfo($2.clone(), |at| appended(liftCAttrs($1), CTypeSpec(CTypeDef($2, at)))) }

  | attrs typeof '(' expression ')'
        {% p.withNodeInfo($1.clone(), |at| appended(liftCAttrs($1), CTypeSpec(CTypeOfExpr($4, at)))) }

  | attrs typeof '(' type_name ')'
        {% p.withNodeInfo($2, |at| appended(liftCAttrs($1), CTypeSpec(CTypeOfType($4, at)))) }

  | type_qualifier_list attrs tyident
        {% p.withNodeInfo($3.clone(), |at| appended(addVecs(map(CTypeQual, $1), liftCAttrs($2)),
                                                    CTypeSpec(CTypeDef($3, at)))) }

  | type_qualifier_list attrs typeof '(' expression ')'
        {% p.withNodeInfo($3, |at| appended(addVecs(map(CTypeQual, $1), liftCAttrs($2)),
                                            CTypeSpec(CTypeOfExpr($5, at)))) }

  | type_qualifier_list attrs typeof '(' type_name ')'
        {% p.withNodeInfo($3, |at| appended(addVecs(map(CTypeQual, $1), liftCAttrs($2)),
                                            CTypeSpec(CTypeOfType($5, at)))) }

  | typedef_type_specifier type_qualifier
        { appended($1, CTypeQual($2)) }

  | typedef_type_specifier attr
        { addTrailingAttrs($1, $2) }


-- A named or anonymous struct, union or enum type.
--
-- * Summary:
--   (struct|union|enum) (identifier? '{' ... '}' | identifier)
--
elaborated_type_name :: { CTypeSpec }
elaborated_type_name
  : struct_or_union_specifier   {% p.withNodeInfo($1.clone(), |at| CSUType($1, at)) }
  | enum_specifier              {% p.withNodeInfo($1.clone(), |at| CEnumType($1, at)) }


-- parse C structure or union declaration (C99 6.7.2.1)
--
-- * Summary:
--    (struct|union) (identifier? '{' ... '}' | identifier)
--
struct_or_union_specifier :: { CStructUnion }
struct_or_union_specifier
  : struct_or_union attrs_opt identifier '{' struct_declaration_list  '}'
        {% p.withNodeInfo($1.clone(), |at| CStructureUnion($1.into_inner(), Some($3), Some($5), $2, at)) }

  | struct_or_union attrs_opt '{' struct_declaration_list  '}'
        {% p.withNodeInfo($1.clone(), |at| CStructureUnion($1.into_inner(), None,     Some($4), $2, at)) }

  | struct_or_union attrs_opt identifier
        {% p.withNodeInfo($1.clone(), |at| CStructureUnion($1.into_inner(), Some($3), None,     $2, at)) }


struct_or_union :: { Located<CStructTag> }
struct_or_union
  : struct                      { Located::new(CStructTag, $1) }
  | union                       { Located::new(CUnionTag, $1) }


struct_declaration_list :: { Vec<CDecl> }
struct_declaration_list
  : {- empty -}                                         { vec![] }
  | struct_declaration_list ';'                         { $1 }
  | struct_declaration_list struct_declaration          { appended($1, $2) }


-- parse C structure declaration (C99 6.7.2.1)
--
struct_declaration :: { CDecl }
struct_declaration
  : struct_declaring_list ';'
        {
            if let CDecl(declspecs, dies, at) = $1 {
                CDecl(declspecs, revVec(dies), at)
            } else {
                panic!("irrefutable pattern");
            }
        }

  | struct_default_declaring_list';'
        {
            if let CDecl(declspecs, dies, at) = $1 {
                CDecl(declspecs, revVec(dies), at)
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
            p.withNodeInfo($1.clone(), match $3 {
                (d, s) => |at| CDecl(addVecs(liftTypeQuals($1), liftCAttrs($2)), vec![(d, None, s)], at)
            })
        }

  -- GNU extension: __attribute__ as only type qualifier
  | attrs struct_identifier_declarator
        {%
            p.withNodeInfo($1.clone(), match $2 {
                (d, s) => |at| CDecl(liftCAttrs($1), vec![(d, None, s)], at),
            })
        }

  -- attrs_opt apply to the declared object
  | struct_default_declaring_list ',' attrs_opt struct_identifier_declarator
        {
            if let CDecl(declspecs, dies, at) = $1 {
                match $4 {
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
struct_declaring_list :: { CDecl }
struct_declaring_list
  : type_specifier struct_declarator attrs_opt
        {%
            p.withNodeInfo($1.clone(), move |at| match $2 {
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
            if let CDecl(declspecs, dies, at) = $1 {
                match $4 {
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
        {% p.withNodeInfo($1.clone(), |at| CDecl($1, vec![], at)) }


-- parse C structure declarator (C99 6.7.2.1)
--
struct_declarator :: { (Option<CDeclr>, Option<CExpr>) }
struct_declarator
  : declarator                                  { (Some($1.reverse()), None) }
  | ':' constant_expression                     { (None, Some($2)) }
  | declarator ':' constant_expression          { (Some($1.reverse()), Some($3)) }

-- FIXME: anonymous bitfield doesn't allow recording of attributes
struct_identifier_declarator :: { (Option<CDeclr>, Option<CExpr>) }
struct_identifier_declarator
  : identifier_declarator                               { (Some($1.reverse()), None) }
  | ':' constant_expression                             { (None, Some($2)) }
  | identifier_declarator ':' constant_expression       { (Some($1.reverse()), Some($3)) }
  | struct_identifier_declarator attr
        {
            match $1 {
                (None, expr) => (None, expr),
                (Some(CDeclarator(name, derived, asmname, attrs, node)), bsz) =>
                    (Some(CDeclarator(name, derived, asmname, addVecs(attrs, $2), node)), bsz)
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
        {% p.withNodeInfo($1, |at| CEnumeration(None, Some($4), $2, at)) }

  | enum attrs_opt '{' enumerator_list ',' '}'
        {% p.withNodeInfo($1, |at| CEnumeration(None, Some($4), $2, at)) }

  | enum attrs_opt identifier '{' enumerator_list '}'
        {% p.withNodeInfo($1, |at| CEnumeration(Some($3), Some($5), $2, at)) }

  | enum attrs_opt identifier '{' enumerator_list ',' '}'
        {% p.withNodeInfo($1, |at| CEnumeration(Some($3), Some($5), $2, at)) }

  | enum attrs_opt identifier
        {% p.withNodeInfo($1, |at| CEnumeration(Some($3), None, $2, at)) }

enumerator_list :: { Vec<(Ident, Option<CExpr>)> }
enumerator_list
  : enumerator                                  { vec![$1] }
  | enumerator_list ',' enumerator              { appended($1, $3) }


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
  : const               {% p.withNodeInfo($1, CConstQual) }
  | volatile            {% p.withNodeInfo($1, CVolatQual) }
  | restrict            {% p.withNodeInfo($1, CRestrQual) }
  | "_Nullable"         {% p.withNodeInfo($1, CNullableQual) }
  | "_Nonnull"          {% p.withNodeInfo($1, CNonnullQual) }
  | "_Atomic"           {% p.withNodeInfo($1, CAtomicQual) }

-- a list containing at least one type_qualifier (const, volatile, restrict, inline, _Noreturn)
--    and additionally CAttrs
type_qualifier_list :: { Vec<CTypeQual> }
type_qualifier_list
  : attrs_opt type_qualifier                 { appended(map(CAttrQual, $1), $2) }
  | type_qualifier_list type_qualifier       { appended($1, $2) }
  | type_qualifier_list attrs type_qualifier { appended(addVecs($1, map(CAttrQual, $2)), $3) }

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
        {% p.withNodeInfo($1.clone(), |at| CDeclrR::from_var($1, at)) }

  | tyident postfixing_abstract_declarator
        {% p.withNodeInfo($1.clone(), |at| { $2(CDeclrR::from_var($1, at)) }) }

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
        {% p.withNodeInfo($1, |at| $2.ptrDeclr(vec![], at)) }

  | '*' attrs parameter_typedef_declarator
        {% p.withAttribute($1, $2, |at| $3.ptrDeclr(vec![], at)) }

  | '*' type_qualifier_list  parameter_typedef_declarator
        {% p.withNodeInfo($1, |at| $3.ptrDeclr($2, at)) }

  | '*' type_qualifier_list attrs parameter_typedef_declarator
        {% p.withAttribute($1, $3, |at| $4.ptrDeclr($2, at)) }

-- clean_postfix_typedef_declarator :- ( attrs? clean_typedef_declarator ) declarator_postfix?
--
clean_postfix_typedef_declarator :: { CDeclrR }
clean_postfix_typedef_declarator
  : '(' clean_typedef_declarator ')'                                        { $2 }
  | '(' clean_typedef_declarator ')' postfixing_abstract_declarator         { $4($2) }
  | '(' attrs clean_typedef_declarator ')'                                  { $3.appendAttrs($2) }
  | '(' attrs clean_typedef_declarator ')' postfixing_abstract_declarator   { $5($3).appendAttrs($2) }


-- The following have a redundant '(' placed
-- immediately to the left of the tyident
--
paren_typedef_declarator :: { CDeclrR }
paren_typedef_declarator
  : paren_postfix_typedef_declarator
        { $1 }

  -- redundant paren
  | '*' '(' simple_paren_typedef_declarator ')'
        {% p.withNodeInfo($1, |at| $3.ptrDeclr(vec![], at)) }

  | '*' type_qualifier_list '(' simple_paren_typedef_declarator ')'
        {% p.withNodeInfo($1, |at| $4.ptrDeclr($2, at)) }
  | '*' type_qualifier_list attrs '(' simple_paren_typedef_declarator ')'
        {% p.withAttribute($1, $3, |at| $5.ptrDeclr($2, at))  }

  | '*' paren_typedef_declarator
        {% p.withNodeInfo($1, |at| $2.ptrDeclr(vec![], at)) }

  | '*' type_qualifier_list paren_typedef_declarator
        {% p.withNodeInfo($1, |at| $3.ptrDeclr($2, at)) }
  | '*' type_qualifier_list attrs paren_typedef_declarator
        {% p.withAttribute($1, $3, |at| $4.ptrDeclr($2, at)) }

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
        {% p.withNodeInfo($1.clone(), |at| CDeclrR::from_var($1, at)) }

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
        {% p.withNodeInfo($1, |at| $2.ptrDeclr(vec![], at)) }

  | '*' attrs identifier_declarator
        {% p.withAttribute($1, $2, |at| $3.ptrDeclr(vec![], at)) }

  | '*' type_qualifier_list identifier_declarator
        {% p.withNodeInfo($1, |at| $3.ptrDeclr($2, at)) }

  | '*' type_qualifier_list attrs identifier_declarator
        {% p.withAttribute($1, $3, |at| $4.ptrDeclr($2, at)) }

postfix_identifier_declarator :: { CDeclrR }
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
paren_identifier_declarator :: { CDeclrR }
paren_identifier_declarator
  : ident
        {% p.withNodeInfo($1.clone(), |at| CDeclrR::from_var($1, at)) }

  | '(' paren_identifier_declarator ')'
        { $2 }

  | '(' attrs paren_identifier_declarator ')'
        { $3.appendAttrs($2) }

function_declarator_old :: { CDeclr }
function_declarator_old
  : old_function_declarator
        { $1.reverse() }

old_function_declarator :: { CDeclrR }
old_function_declarator
  : postfix_old_function_declarator
        { $1 }

  | '*' old_function_declarator
        {% p.withNodeInfo($1, |at| $2.ptrDeclr(vec![], at)) } -- FIXME: no attr possible here ???

  | '*' type_qualifier_list old_function_declarator
        {% p.withNodeInfo($1, |at| $3.ptrDeclr($2, at)) }

postfix_old_function_declarator :: { CDeclrR }
postfix_old_function_declarator
  : paren_identifier_declarator '(' identifier_list ')'
        {% p.withNodeInfo($1.clone(), |_0| $1.funDeclr(Left($3), vec![], _0)) }

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
  : parameter_declaration                       { vec![$1] }
  | parameter_list ',' parameter_declaration    { appended($1, $3) }

parameter_declaration :: { CDecl }
parameter_declaration
  : declaration_specifier
        {% p.withNodeInfo($1.clone(), |at| CDecl($1, vec![], at)) }

  | declaration_specifier abstract_declarator
        {% p.withNodeInfo($1.clone(), |at| CDecl($1, vec![(Some($2.reverse()), None, None)], at)) }

  | declaration_specifier identifier_declarator attrs_opt
        {% p.withNodeInfo($1.clone(), |at| CDecl($1, vec![(Some($2.appendAttrs($3).reverse()), None, None)], at)) }

  | declaration_specifier parameter_typedef_declarator attrs_opt
        {% p.withNodeInfo($1.clone(), |at| CDecl($1, vec![(Some($2.appendAttrs($3).reverse()), None, None)], at)) }

  | declaration_qualifier_list
        {% p.withNodeInfo($1.clone(), |at| CDecl($1, vec![], at)) }

  | declaration_qualifier_list abstract_declarator
        {% p.withNodeInfo($1.clone(), |at| CDecl($1, vec![(Some($2.reverse()), None, None)], at)) }

  | declaration_qualifier_list identifier_declarator attrs_opt
        {% p.withNodeInfo($1.clone(), |at| CDecl($1,
                                                 vec![(Some($2.appendAttrs($3).reverse()), None, None)], at)) }

  | type_specifier
        {% p.withNodeInfo($1.clone(), |at| CDecl($1, vec![], at)) }

  | type_specifier abstract_declarator
        {% p.withNodeInfo($1.clone(), |at| CDecl($1, vec![(Some($2.reverse()), None, None)], at)) }

  | type_specifier identifier_declarator attrs_opt
        {% p.withNodeInfo($1.clone(), |at| CDecl($1,
                                                 vec![(Some($2.appendAttrs($3).reverse()), None, None)], at)) }

  | type_specifier parameter_typedef_declarator attrs_opt
        {% p.withNodeInfo($1.clone(), |at| CDecl($1,
                                                 vec![(Some($2.appendAttrs($3).reverse()), None, None)], at)) }

  | type_qualifier_list
        {% p.withNodeInfo($1.clone(), |at| CDecl(liftTypeQuals($1), vec![], at)) }
  | type_qualifier_list attr
        {% p.withNodeInfo($1.clone(), |at| CDecl(addVecs(liftTypeQuals($1), liftCAttrs($2)), vec![], at)) }

  | type_qualifier_list abstract_declarator
        {% p.withNodeInfo($1.clone(), |at| CDecl(liftTypeQuals($1), vec![(Some($2.reverse()), None, None)], at)) }

  | type_qualifier_list identifier_declarator attrs_opt
        {% p.withNodeInfo($1.clone(), |at| CDecl(liftTypeQuals($1),
                                                 vec![(Some($2.appendAttrs($3).reverse()), None, None)], at)) }


identifier_list :: { Vec<Ident> }
identifier_list
  : ident                               { vec![$1] }
  | identifier_list ',' ident           { appended($1, $3) }


-- parse C type name (C99 6.7.6)
--
type_name :: { CDecl }
type_name
  :  type_specifier
        {% p.withNodeInfo($1.clone(), |at| CDecl($1, vec![], at)) }

  |  type_specifier abstract_declarator
        {% p.withNodeInfo($1.clone(), |at| CDecl($1, vec![(Some($2.reverse()), None, None)], at)) }

  |  type_qualifier_list attr
        {% p.withNodeInfo($1.clone(), |at| CDecl(addVecs(liftTypeQuals($1), liftCAttrs($2)), vec![], at)) }

  |  type_qualifier_list abstract_declarator
        {% p.withNodeInfo($1.clone(), |at| CDecl(liftTypeQuals($1), vec![(Some($2.reverse()), None, None)], at)) }

-- parse C abstract declarator (C99 6.7.6)
--
-- postfix starts with '('
-- postfixing starts with '(' or '['
-- unary start with '*'
abstract_declarator :: { CDeclrR }
abstract_declarator
  : unary_abstract_declarator       { $1 }
  | postfix_abstract_declarator     { $1 }
  | postfixing_abstract_declarator  { $1(CDeclrR::empty()) }

--
-- FIXME
--  | postfixing_abstract_declarator attrs_opt  { $1(CDeclrR::empty()) }


postfixing_abstract_declarator :: { Box<FnBox(CDeclrR) -> CDeclrR> }
postfixing_abstract_declarator
  : array_abstract_declarator
        { $1 }

  | '(' parameter_type_list ')'
        {%
            p.withNodeInfo($1, |at| {
                let a: Box<FnBox(CDeclrR) -> CDeclrR> = box move |declr: CDeclrR| {
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
array_abstract_declarator :: { Box<FnBox(CDeclrR) -> CDeclrR> }
array_abstract_declarator
  : postfix_array_abstract_declarator
        { $1 }

  | array_abstract_declarator postfix_array_abstract_declarator
        { box |decl| { $2($1(decl)) } }

--
-- TODO: record static
postfix_array_abstract_declarator :: { Box<FnBox(CDeclrR) -> CDeclrR> }
postfix_array_abstract_declarator
  : '[' assignment_expression_opt ']'
        {%
            p.withNodeInfo($1.clone(), |at| {
                let a: Box<FnBox(CDeclrR) -> CDeclrR> = box |declr: CDeclrR| {
                    declr.arrDeclr(vec![], false, false, $2, at)
                };
                a
            })
        }

  | '[' attrs assignment_expression_opt ']'
        {% p.withAttributePF($1, $2, |at, declr| declr.arrDeclr(vec![], false, false, $3, at)) }

  | '[' type_qualifier_list assignment_expression_opt ']'
        {%
            p.withNodeInfo($1.clone(), |at| {
                let a: Box<FnBox(CDeclrR) -> CDeclrR> =
                    box |declr: CDeclrR| declr.arrDeclr($2, false, false, $3, at);
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

unary_abstract_declarator :: { CDeclrR }
unary_abstract_declarator
  : '*'
        {% p.withNodeInfo($1, |at| CDeclrR::empty().ptrDeclr(vec![], at)) }

  | '*' type_qualifier_list attrs_opt
        {% p.withAttribute($1, $3, |at| CDeclrR::empty().ptrDeclr($2, at)) }

  | '*' abstract_declarator
        {% p.withNodeInfo($1, |at| $2.ptrDeclr(vec![], at)) }

  | '*' type_qualifier_list abstract_declarator
        {% p.withNodeInfo($1, |at| $3.ptrDeclr($2, at)) }

  | '*' attrs
        {% p.withAttribute($1, $2, |at| CDeclrR::empty().ptrDeclr(vec![], at)) }
  | '*' attrs abstract_declarator
        {% p.withAttribute($1, $2, |at| $3.ptrDeclr(vec![], at)) }

-- postfix_ad starts with '(', postfixing with '(' or '[', unary_abstract starts with '*'
postfix_abstract_declarator :: { CDeclrR }
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
initializer :: { CInit }
initializer
  : assignment_expression               {% p.withNodeInfo($1.clone(), |at| CInitExpr($1, at)) }
  | '{' initializer_list '}'            {% p.withNodeInfo($1, |at| CInitList($2, at)) }
  | '{' initializer_list ',' '}'        {% p.withNodeInfo($1, |at| CInitList($2, at)) }


initializer_opt :: { Option<CInit> }
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
  | identifier ':'              {% p.withNodeInfo($1.clone(), |_0| vec![CMemberDesig($1, _0)]) }
  | array_designator            { vec![$1] }


designator_list :: { Vec<CDesignator> }
designator_list
 : designator                           { vec![$1] }
 | designator_list designator           { appended($1, $2) }


designator :: { CDesignator }
designator
  : '[' constant_expression ']'         {% p.withNodeInfo($1, |at| CArrDesig($2, at)) }
  | '.' identifier                      {% p.withNodeInfo($1, |at| CMemberDesig($2, at)) }
  | array_designator                    { $1 }


array_designator :: { CDesignator }
array_designator
  : '[' constant_expression "..." constant_expression ']'
        {% p.withNodeInfo($1, |at| CRangeDesig($2, $4, at)) }


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
  : ident                {% p.withNodeInfo($1.clone(), |at| CVar($1, at)) }
  | constant             { CConst($1) }
  | string_literal       { CConst(liftStrLit($1)) }
  | '(' expression ')'   { $2 }
  | "_Generic" '(' assignment_expression ',' generic_assoc_list ')'
        {% p.withNodeInfo($1, |at| CGenericSelection(box $3, $5, at)) }
  -- GNU extensions
  | '(' compound_statement ')'
        {% p.withNodeInfo($1, |at| CStatExpr(box $2, at)) }

  | "__builtin_va_arg" '(' assignment_expression ',' type_name ')'
        {% p.withNodeInfo($1, |_0| CBuiltinExpr(box CBuiltinVaArg($3, $5, _0))) }

  | "__builtin_offsetof" '(' type_name ',' offsetof_member_designator ')'
        {% p.withNodeInfo($1, |_0| CBuiltinExpr(box CBuiltinOffsetOf($3, $5, _0))) }

  | "__builtin_types_compatible_p" '(' type_name ',' type_name ')'
        {% p.withNodeInfo($1, |_0| CBuiltinExpr(box CBuiltinTypesCompatible($3, $5, _0))) }

-- Generic Selection association list (C11 6.5.1.1)
--
-- TODO: introduce AST type for generic association
generic_assoc_list :: { Vec<(Option<CDecl>, CExpr)> }
  : generic_assoc_list ',' generic_assoc { appended($1, $3) }
  | generic_assoc                        { vec![$1] }
generic_assoc :: { (Option<CDecl>, CExpr) }
generic_assoc
  : type_name ':' assignment_expression { (Some($1), $3) }
  | default   ':' assignment_expression { (None, $3) }

offsetof_member_designator :: { Vec<CDesignator> }
offsetof_member_designator
  : identifier
        {% p.withNodeInfo($1.clone(), |_0| vec![CMemberDesig($1, _0)]) }
  | offsetof_member_designator '.' identifier
        {% p.withNodeInfo($3.clone(), |_0| appended($1, CMemberDesig($3, _0))) }
  | offsetof_member_designator '[' expression ']'
        {% p.withNodeInfo($3.clone(), |_0| appended($1, CArrDesig($3, _0))) }


-- parse C postfix expression (C99 6.5.2)
--
postfix_expression :: { CExpr }
postfix_expression
  : primary_expression
        { $1 }

  | postfix_expression '[' expression ']'
        {% p.withNodeInfo($1.clone(), |at| CIndex(box $1, box $3, at)) }

  | postfix_expression '(' ')'
        {% p.withNodeInfo($1.clone(), |at| CCall(box $1, vec![], at)) }

  | postfix_expression '(' argument_expression_list ')'
        {% p.withNodeInfo($1.clone(), |at| CCall(box $1, $3, at)) }

  | postfix_expression '.' identifier
        {% p.withNodeInfo($1.clone(), |at| CMember(box $1, $3, false, at)) }

  | postfix_expression "->" identifier
        {% p.withNodeInfo($1.clone(), |at| CMember(box $1, $3, true, at)) }

  | postfix_expression "++"
        {% p.withNodeInfo($1.clone(), |at| CUnary(CPostIncOp, box $1, at)) }

  | postfix_expression "--"
        {% p.withNodeInfo($1.clone(), |at| CUnary(CPostDecOp, box $1, at)) }

  | '(' type_name ')' '{' initializer_list '}'
        {% p.withNodeInfo($1, |at| CCompoundLit(box $2, $5, at)) }

  | '(' type_name ')' '{' initializer_list ',' '}'
        {% p.withNodeInfo($1, |at| CCompoundLit(box $2, $5, at)) }


argument_expression_list :: { Vec<CExpr> }
argument_expression_list
  : assignment_expression                               { vec![$1] }
  | argument_expression_list ',' assignment_expression  { appended($1, $3) }


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
  | "++" unary_expression               {% p.withNodeInfo($1, |at| CUnary(CPreIncOp, box $2, at)) }
  | "--" unary_expression               {% p.withNodeInfo($1, |at| CUnary(CPreDecOp, box $2, at)) }
  | "__extension__" cast_expression     { $2 }
  | unary_operator cast_expression      {% p.withNodeInfo($1.clone(), |at| CUnary($1.into_inner(), box $2, at)) }
  | sizeof unary_expression             {% p.withNodeInfo($1, |at| CSizeofExpr(box $2, at)) }
  | sizeof '(' type_name ')'            {% p.withNodeInfo($1, |at| CSizeofType(box $3, at)) }
  -- GNU: alignof, complex and && extension
  | alignof unary_expression            {% p.withNodeInfo($1, |at| CAlignofExpr(box $2, at)) }
  | alignof '(' type_name ')'           {% p.withNodeInfo($1, |at| CAlignofType(box $3, at)) }
  | "__real__" unary_expression         {% p.withNodeInfo($1, |at| CComplexReal(box $2, at)) }
  | "__imag__" unary_expression         {% p.withNodeInfo($1, |at| CComplexImag(box $2, at)) }
  | "&&" identifier                     {% p.withNodeInfo($1, |at| CLabAddrExpr($2, at)) }


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
cast_expression :: { CExpr }
cast_expression
  : unary_expression                    { $1 }
  | '(' type_name ')' cast_expression   {% p.withNodeInfo($1, |at| CCast(box $2, box $4, at)) }


-- parse C multiplicative expression (C99 6.5.5)
--
multiplicative_expression :: { CExpr }
multiplicative_expression
  : cast_expression
        { $1 }

  | multiplicative_expression '*' cast_expression
        {% p.withNodeInfo($1.clone(), |at| CBinary(CMulOp, box $1, box $3, at)) }

  | multiplicative_expression '/' cast_expression
        {% p.withNodeInfo($1.clone(), |at| CBinary(CDivOp, box $1, box $3, at)) }

  | multiplicative_expression '%' cast_expression
        {% p.withNodeInfo($1.clone(), |at| CBinary(CRmdOp, box $1, box $3, at)) }


-- parse C additive expression (C99 6.5.6)
--
additive_expression :: { CExpr }
additive_expression
  : multiplicative_expression
        { $1 }

  | additive_expression '+' multiplicative_expression
        {% p.withNodeInfo($1.clone(), |at| CBinary(CAddOp, box $1, box $3, at)) }

  | additive_expression '-' multiplicative_expression
        {% p.withNodeInfo($1.clone(), |at| CBinary(CSubOp, box $1, box $3, at)) }


-- parse C shift expression (C99 6.5.7)
--
shift_expression :: { CExpr }
shift_expression
  : additive_expression
        { $1 }

  | shift_expression "<<" additive_expression
        {% p.withNodeInfo($1.clone(), |at| CBinary(CShlOp, box $1, box $3, at)) }

  | shift_expression ">>" additive_expression
        {% p.withNodeInfo($1.clone(), |at| CBinary(CShrOp, box $1, box $3, at)) }


-- parse C relational expression (C99 6.5.8)
--
relational_expression :: { CExpr }
relational_expression
  : shift_expression
        { $1 }

  | relational_expression '<' shift_expression
        {% p.withNodeInfo($1.clone(), |at| CBinary(CLeOp, box $1, box $3, at)) }

  | relational_expression '>' shift_expression
        {% p.withNodeInfo($1.clone(), |at| CBinary(CGrOp, box $1, box $3, at)) }

  | relational_expression "<=" shift_expression
        {% p.withNodeInfo($1.clone(), |at| CBinary(CLeqOp, box $1, box $3, at)) }

  | relational_expression ">=" shift_expression
        {% p.withNodeInfo($1.clone(), |at| CBinary(CGeqOp, box $1, box $3, at)) }


-- parse C equality expression (C99 6.5.9)
--
equality_expression :: { CExpr }
equality_expression
  : relational_expression
        { $1 }

  | equality_expression "==" relational_expression
        {% p.withNodeInfo($1.clone(), |at| CBinary(CEqOp, box $1, box $3, at)) }

  | equality_expression "!=" relational_expression
        {% p.withNodeInfo($1.clone(), |at| CBinary(CNeqOp, box $1, box $3, at)) }


-- parse C bitwise and expression (C99 6.5.10)
--
and_expression :: { CExpr }
and_expression
  : equality_expression
        { $1 }

  | and_expression '&' equality_expression
        {% p.withNodeInfo($1.clone(), |at| CBinary(CAndOp, box $1, box $3, at)) }


-- parse C bitwise exclusive or expression (C99 6.5.11)
--
exclusive_or_expression :: { CExpr }
exclusive_or_expression
  : and_expression
        { $1 }

  | exclusive_or_expression '^' and_expression
        {% p.withNodeInfo($1.clone(), |at| CBinary(CXorOp, box $1, box $3, at)) }


-- parse C bitwise or expression (C99 6.5.12)
--
inclusive_or_expression :: { CExpr }
inclusive_or_expression
  : exclusive_or_expression
        { $1 }

  | inclusive_or_expression '|' exclusive_or_expression
        {% p.withNodeInfo($1.clone(), |at| CBinary(COrOp, box $1, box $3, at)) }


-- parse C logical and expression (C99 6.5.13)
--
logical_and_expression :: { CExpr }
logical_and_expression
  : inclusive_or_expression
        { $1 }

  | logical_and_expression "&&" inclusive_or_expression
        {% p.withNodeInfo($1.clone(), |at| CBinary(CLndOp, box $1, box $3, at)) }


-- parse C logical or expression (C99 6.5.14)
--
logical_or_expression :: { CExpr }
logical_or_expression
  : logical_and_expression
        { $1 }

  | logical_or_expression "||" logical_and_expression
        {% p.withNodeInfo($1.clone(), |at| CBinary(CLorOp, box $1, box $3, at)) }


-- parse C conditional expression (C99 6.5.15)
--
-- * GNU extensions:
--     omitting the `then' part
conditional_expression :: { CExpr }
conditional_expression
  : logical_or_expression
        { $1 }

  | logical_or_expression '?' expression ':' conditional_expression
        {% p.withNodeInfo($1.clone(), |at| CCond(box $1, Some(box $3), box $5, at)) }

  | logical_or_expression '?' ':' conditional_expression
        {% p.withNodeInfo($1.clone(), |at| CCond(box $1, None, box $4, at)) }


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
        {% p.withNodeInfo($1.clone(), |at| CAssign($2.into_inner(), box $1, box $3, at)) }


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
expression :: { CExpr }
expression
  : assignment_expression
        { $1 }

  | assignment_expression ',' comma_expression
        {% p.withNodeInfo($3.clone(), |at| CComma(prepend($1, $3), at)) }

comma_expression :: { Vec<CExpr> }
comma_expression
  : assignment_expression                       { vec![$1] }
  | comma_expression ',' assignment_expression  { appended($1, $3) }


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
                    p.withNodeInfo($1.clone(), move |at| {
                        if let CTokILit(_, i) = $1 {
                            CIntConst(i, at)
                        } else {
                            panic!("irrefutable pattern")
                        }
                    })
                }
  | cchar       {%
                    p.withNodeInfo($1.clone(), move |at| {
                        if let CTokCLit(_, c) = $1 {
                            CCharConst(c, at)
                        } else {
                            panic!("irrefutable pattern")
                        }
                    })
                }
  | cfloat      {%
                    p.withNodeInfo($1.clone(), move |at| {
                        if let CTokFLit(_, f) = $1 {
                            CFloatConst(f, at)
                        } else {
                            panic!("irrefutable pattern")
                        }
                    })
                }


string_literal :: { CStrLit }
string_literal
  : cstr
        {%
            p.withNodeInfo($1.clone(), move |at| {
                if let CTokSLit(_, s) = $1 {
                    CStringLiteral(s, at)
                } else {
                    panic!("irrefutable pattern")
                }
            })
        }

  | cstr string_literal_list
        {%
            p.withNodeInfo($1.clone(), move |at| {
                if let CTokSLit(_, s) = $1 {
                    CStringLiteral(concatCStrings(prepend(s, $2)), at)
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
  : attribute                      {
                                       match $1 {
                                           None => vec![],
                                           Some(attr) => vec![attr],
                                       }
                                   }
  | attribute_list ',' attribute   {
                                       match $3 {
                                           Some(inner) => appended($1, inner),
                                           None => $1,
                                       }
                                   }


attribute :: { Option<CAttr> }
attribute
  : {- empty -}                         { None }
  | ident
        {% p.withNodeInfo($1.clone(), |at| Some(CAttribute($1, vec![], at))) }
  | const
        {% p.withNodeInfo($1, |at| Some(CAttribute(Ident::internal("const".into()), vec![], at))) }
  | ident '(' attribute_params ')'
        {% p.withNodeInfo($1.clone(), |at| Some(CAttribute($1, $3, at))) }
  | ident '(' ')'
        {% p.withNodeInfo($1.clone(), |at| Some(CAttribute($1, vec![], at))) }

-- OS X 10.9 (Mavericks) makes use of more liberal attribute syntax
-- that includes assignment-like expressions referencing version
-- numbers.

attribute_params :: { Vec<CExpr> }
attribute_params
  : constant_expression                                        { vec![$1] }
  | unary_expression assignment_operator clang_version_literal { vec![] }
  | unary_expression assignment_operator unary_expression      { vec![] }
  | attribute_params ',' constant_expression                   { appended($1, $3) }
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
    map(|attr| CTypeQual(CAttrQual(attr)), attrs)
}

fn appendObjAttrs(newAttrs: Vec<CAttribute<NodeInfo>>,
                  CDeclarator(ident, indirections, asmname, cAttrs, at): CDeclarator<NodeInfo>)
                  -> CDeclarator<NodeInfo> {
    CDeclarator(ident, indirections, asmname, addVecs(cAttrs, newAttrs), at)
}

fn addTrailingAttrs(mut declspecs: Vec<CDeclSpec>, mut new_attrs: Vec<CAttr>) -> Vec<CDeclSpec> {
    let is_new = match declspecs.last_mut() {
        Some(&mut CTypeSpec(CSUType(CStructureUnion(_, _, _, ref mut def_attrs, _), _))) => {
            def_attrs.append(&mut new_attrs);
            false
        }
        Some(&mut CTypeSpec(CEnumType(CEnumeration(_, _, ref mut def_attrs, _), _))) => {
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
    translation_unit(p)
}

pub fn extDeclP(p: &mut Parser) -> Res<CExtDecl> {
    external_declaration(p)
}

pub fn statementP(p: &mut Parser) -> Res<CStat> {
    statement(p)
}

pub fn expressionP(p: &mut Parser) -> Res<CExpr> {
    expression(p)
}

}
