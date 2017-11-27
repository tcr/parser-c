-----------------------------------------------------------------------------
-- Module      :  Lexer.x
-- Copyright   : (c) [1999..2004] Manuel M T Chakravarty
--               (c) 2005 Duncan Coutts
--               (c) 2008 Benedikt Huber
-- License     :  BSD-style
-- Maintainer  :  benedikt.huber@gmail.com
-- Portability :  portable
--
--  Lexer for C files, after being processed by the C preprocessor
--
--  We assume that the input already went through cpp.  Thus, we do not handle
--  comments and preprocessor directives here.  It supports the
--  C99 `restrict' extension: <http://www.lysator.liu.se/c/restrict.html> as
--  well as inline functions.
--
--  Comments:
--
--  * Universal character names and multi-character character constants,
--    as well as trigraphs are unsupported. They are lexed, but yield an error.
--
--  * We add `typedef-name' (K&R 8.9) as a token, as proposed in K&R A13.
--    However, as these tokens cannot be recognized lexically, but require a
--    context analysis, they are never produced by the lexer, but instead have
--    to be introduced in a later phase (by converting the corresponding
--    identifiers).
--
--  * We also recognize GNU C `__attribute__', `__extension__', `__complex__',
--    `__const',  `__const__', `__imag', `__imag__', `__inline', `__inline__',
--    `__real', `__real__, `__restrict', and `__restrict__'.
--
--  * Any line starting with `#pragma' is ignored.
--
--  With K&R we refer to ``The C Programming Language'', second edition, Brain
--  W. Kernighan and Dennis M. Ritchie, Prentice Hall, 1988.
--
--  With C99 we refer to ``ISO/IEC 9899:TC3'',
--  available online at http://www.open-std.org/JTC1/SC22/WG14/www/docs/n1256.pdf.
--
--- TODO ----------------------------------------------------------------------
--
--  * There are more GNU C specific keywords.  Add them and change `Parser.y'
--    correspondingly (in particular, most tokens within __attribute ((...))
--    expressions are actually keywords, but we handle them as identifiers at
--    the moment).
--
--  * Add support for bytestrings

{

/*

RUST VERSION of this lexer.  The token definitions should stay the same, the
actions have to be translated.

*/

use std::rc::Rc;

use data::input_stream::InputStream;
use data::ident::Ident;
use data::position::{Position, PosLength, Pos};
use parser::{Parser, ParseError};
use parser::tokens::*;
use syntax::constants::*;

type Token = CToken;
type Res<T> = Result<T, ParseError>;

}

$space = [ \ \t ]                           -- horizontal white space
$eol   = \n                                 -- end of line

$letter   = [a-zA-Z]
$identletter = [a-zA-Z_\$]                  -- GNU extension: allow $ in variable names
$octdigit = 0-7
$digit    = 0-9
$digitNZ  = 1-9
$hexdigit = [0-9a-fA-F]

$inchar   = . # [ \\ \' \n \r ]       -- valid character in char constant
$instr    = . # [ \\ \" \n \r ]       -- valid character in a string literal
$infname  = . # [ \\ \" ]             -- valid character in a filename

@sp  = $space*

-- character escape sequence (follows K&R A2.5.2)
--
-- * also used for strings
-- * C99: 6.4.4.4
@charesc  = \\([ntvbrfaeE\\\?\'\"]|$octdigit{1,3}|x$hexdigit+)
@ucn      = \\u$hexdigit{4}|\\U$hexdigit{8}

-- components of integer constants
--
-- * C99: 6.4.4.1
@int = $digitNZ$digit*

-- integer suffixes
@llsuffix  = ll|LL
@gnusuffix = [ij]?
@intsuffix = [uU][lL]?|[uU]@llsuffix|[lL][uU]?|@llsuffix[uU]?
@intgnusuffix = @intsuffix@gnusuffix?|@gnusuffix@intsuffix?

-- components of float constants (follows K&R A2.5.3)
--
-- * C99: 6.4.4.2
@digits    = $digit+
@intpart   = @digits
@fractpart = @digits

@mantpart  = @intpart?\.@fractpart|@intpart\.
@exppart   = [eE][\+\-]?@digits

@hexprefix = 0x
@hexdigits = $hexdigit+
@hexmant   = @hexdigits?\.@hexdigits|@hexdigits\.
@binexp    = [pP][\+\-]?@digits

-- Suffixes `qQwW` are GNU floating type extensions: <https://gcc.gnu.org/onlinedocs/gcc/Floating-Types.html>
@floatsuffix    = [fFlLqQwW]
@floatgnusuffix = @floatsuffix@gnusuffix?|@gnusuffix@floatsuffix?

-- clang version literals with a major.minor.rev
@clangversion = @intpart\.@intpart\.@intpart

tokens :-

-- whitespace (follows K&R A2.1)
--
-- * horizontal and vertical tabs, newlines, and form feeds are filter out by
--   `Lexers.ctrlLexer'
--
-- * comments are not handled, as we assume the input already went through cpp
--
$white+         ;

-- #line directive (C11 6.10.4, GCC Line Control)
--
-- * standard form: int => change line number
-- * standard form: int string => change source file and line number
-- * preprocessor (gcc/clang): int string int => change source file and line number,
--       push or pop item from stack
--
-- * see https://gcc.gnu.org/onlinedocs/cpp/Preprocessor-Output.html
--
\#$space*@int$space*(\"($infname|@charesc)*\"$space*)?(@int$space*)*\r?$eol
  {
     let new_pos = adjust_line_directive(p.tok_str(), pos)?;
     p.set_pos(new_pos);
     lex_inner(p, false)
  }

-- #pragma directive (K&R A12.8)
--
-- * we simply ignore any #pragma (but take care to update the position
--   information)
--
\#$space*pragma.*$eol   ;

-- #ident directive, eg used by rcs/cvs
--
-- * we simply ignore any #ident (but take care to update the position
--   information)
--
\#$space*ident.*$eol    ;

-- keywords and builtin types
--
"_Alignas"                     { tok(8, CTokAlignas, pos) }
"_Alignof"                     { tok(8, CTokAlignof, pos) }
"_Atomic"                      { tok(7, CTokAtomic, pos) }
"_Bool"                        { tok(5, CTokBool, pos) }
"_Complex"                     { tok(8, CTokComplex, pos) }
"_Nonnull"                     { tok(8, CTokNonnull, pos) }
"_Generic"                     { tok(8, CTokGeneric, pos) }
"_Noreturn"                    { tok(9, CTokNoreturn, pos) }
"_Nullable"                    { tok(9, CTokNullable, pos) }
"_Static_assert"               { tok(14, CTokStaticAssert, pos) }
"_Thread_local"                { tok(13, CTokThread, pos) }
"__alignof"                    { tok(9, CTokAlignof, pos) }
"alignof"                      { tok(7, CTokAlignof, pos) }
"__alignof__"                  { tok(11, CTokAlignof, pos) }
"__asm"                        { tok(5, CTokAsm, pos) }
"asm"                          { tok(3, CTokAsm, pos) }
"__asm__"                      { tok(7, CTokAsm, pos) }
"__attribute"                  { tok(11, |posl| CTokGnuC(posl, GnuCTok::Attr), pos) }
"__attribute__"                { tok(13, |posl| CTokGnuC(posl, GnuCTok::Attr), pos) }
"auto"                         { tok(4, CTokAuto, pos) }
"break"                        { tok(5, CTokBreak, pos) }
"__builtin_offsetof"           { tok(18, |posl| CTokGnuC(posl, GnuCTok::Offsetof), pos) }
"__builtin_types_compatible_p" { tok(28, |posl| CTokGnuC(posl, GnuCTok::TyCompat), pos) }
"__builtin_va_arg"             { tok(16, |posl| CTokGnuC(posl, GnuCTok::VaArg), pos) }
"__builtin_convertvector"      { tok(23, |posl| CTokClangC(posl, ClangCTok::ConvertVector), pos) }
"case"                         { tok(4, CTokCase, pos) }
"char"                         { tok(4, CTokChar, pos) }
"__complex__"                  { tok(11, CTokComplex, pos) }
"__const"                      { tok(7, CTokConst, pos) }
"const"                        { tok(5, CTokConst, pos) }
"__const__"                    { tok(9, CTokConst, pos) }
"continue"                     { tok(8, CTokContinue, pos) }
"default"                      { tok(7, CTokDefault, pos) }
"do"                           { tok(2, CTokDo, pos) }
"double"                       { tok(6, CTokDouble, pos) }
"else"                         { tok(4, CTokElse, pos) }
"enum"                         { tok(4, CTokEnum, pos) }
"__extension__"                { tok(13, |posl| CTokGnuC(posl, GnuCTok::Ext), pos) }
"extern"                       { tok(6, CTokExtern, pos) }
"float"                        { tok(5, CTokFloat, pos) }
"__float128"                   { tok(10, CTokFloat128, pos) }
"_Float128"                    { tok(9, CTokFloat128, pos) }
"for"                          { tok(3, CTokFor, pos) }
"goto"                         { tok(4, CTokGoto, pos) }
"if"                           { tok(2, CTokIf, pos) }
"__imag"                       { tok(6, |posl| CTokGnuC(posl, GnuCTok::ComplexImag), pos) }
"__imag__"                     { tok(8, |posl| CTokGnuC(posl, GnuCTok::ComplexImag), pos) }
"__inline"                     { tok(8, CTokInline, pos) }
"inline"                       { tok(6, CTokInline, pos) }
"__inline__"                   { tok(10, CTokInline, pos) }
"int"                          { tok(3, CTokInt, pos) }
"__int128"                     { tok(8, CTokInt128, pos) }
"__label__"                    { tok(9, CTokLabel, pos) }
"long"                         { tok(4, CTokLong, pos) }
"__nonnull"                    { tok(9, CTokNonnull, pos) }
"__nullable"                   { tok(10, CTokNullable, pos) }
"__real"                       { tok(6, |posl| CTokGnuC(posl, GnuCTok::ComplexReal), pos) }
"__real__"                     { tok(8, |posl| CTokGnuC(posl, GnuCTok::ComplexReal), pos) }
"register"                     { tok(8, CTokRegister, pos) }
"__restrict"                   { tok(10, CTokRestrict, pos) }
"restrict"                     { tok(8, CTokRestrict, pos) }
"__restrict__"                 { tok(12, CTokRestrict, pos) }
"return"                       { tok(6, CTokReturn, pos) }
"short"                        { tok(5, CTokShort, pos) }
"__signed"                     { tok(8, CTokSigned, pos) }
"signed"                       { tok(6, CTokSigned, pos) }
"__signed__"                   { tok(10, CTokSigned, pos) }
"sizeof"                       { tok(6, CTokSizeof, pos) }
"static"                       { tok(6, CTokStatic, pos) }
"struct"                       { tok(6, CTokStruct, pos) }
"switch"                       { tok(6, CTokSwitch, pos) }
"__thread"                     { tok(8, CTokThread, pos) }
"typedef"                      { tok(7, CTokTypedef, pos) }
"__typeof"                     { tok(8, CTokTypeof, pos) }
"typeof"                       { tok(6, CTokTypeof, pos) }
"__typeof__"                   { tok(10, CTokTypeof, pos) }
"union"                        { tok(5, CTokUnion, pos) }
"unsigned"                     { tok(8, CTokUnsigned, pos) }
"void"                         { tok(4, CTokVoid, pos) }
"__volatile"                   { tok(10, CTokVolatile, pos) }
"volatile"                     { tok(8, CTokVolatile, pos) }
"__volatile__"                 { tok(12, CTokVolatile, pos) }
"while"                        { tok(5, CTokWhile, pos) }

-- other identifiers (follows K&R A2.3 and A2.4)
--
$identletter($identletter|$digit)*   { id_token(p, pos, len) }

-- constants (follows K&R A2.5)
--
-- * K&R,C99 explicitly mention `enumeration-constants'; however, as they are
--   lexically identifiers, we do not have an extra case for them
--

-- integer constants (follows K&R A2.5.1, C99 6.4.4.1)
-- NOTE: 0 is lexed as octal integer constant, and readCOctal takes care of this
0$octdigit*@intgnusuffix?       { token_plus(p, CTokILit, CInteger::parse_octal, pos, len) }
$digitNZ$digit*@intgnusuffix?   { token_plus(p, CTokILit, |lit| CInteger::parse(CIntRepr::Dec, lit), pos, len) }
0[xX]$hexdigit+@intgnusuffix?   { token_plus(p, CTokILit, |lit| CInteger::parse(CIntRepr::Hex, &lit[2..]), pos, len) }

(0$octdigit*|$digitNZ$digit*|0[xX]$hexdigit+)[uUlL]+ { token_fail("Invalid integer constant suffix", pos) }

-- character constants (follows K&R A2.5.2, C99 6.4.4.4)
--
-- * Universal Character Names are unsupported and cause an error.
\'($inchar|@charesc)\'      { token_plus(p, CTokCLit,
                                         |lit| CChar::unescape(&lit[1..]).map(|t| CChar::new(t.0)), pos, len) }
L\'($inchar|@charesc)\'     { token_plus(p, CTokCLit,
                                         |lit| CChar::unescape(&lit[2..]).map(|t| CChar::new_wide(t.0)), pos, len) }
\'($inchar|@charesc){2,}\'  { token_plus(p, CTokCLit,
                                         |lit| CChar::unescape_multi(&lit[1..lit.len()-1]).map(|t| CChar::new_multi(t, false)),
                                         pos, len) }
L\'($inchar|@charesc){2,}\' { token_plus(p, CTokCLit,
                                         |lit| CChar::unescape_multi(&lit[2..lit.len()-1]).map(|t| CChar::new_multi(t, true)),
                                         pos, len) }

-- Clang version literals
@clangversion               { token(p, |pos, vers| CTokClangC(pos, ClangCTok::CVersion(vers)),
                                    ClangCVersion::parse, pos, len) }

-- float constants (follows K&R A2.5.3. C99 6.4.4.2)
--
-- * NOTE: Hexadecimal floating constants without binary exponents are forbidden.
--         They generate a lexer error, because they are hard to recognize in the parser.
(@mantpart@exppart?|@intpart@exppart)@floatgnusuffix?  { token(p, CTokFLit, CFloat::parse, pos, len) }
@hexprefix(@hexmant|@hexdigits)@binexp@floatgnusuffix? { token(p, CTokFLit, CFloat::parse, pos, len) }
@hexprefix@hexmant                                     { token_fail("Hexadecimal floating constant requires an exponent",
                                                                    pos) }

-- string literal (follows K&R A2.6)
-- C99: 6.4.5.
\"($instr|@charesc)*\"      { token_plus(p, CTokSLit,
                                         |lit| CString::unescape(&lit[1..lit.len()-1]).map(CString::new),
                                         pos, len) }
L\"($instr|@charesc)*\"     { token_plus(p, CTokSLit,
                                         |lit| CString::unescape(&lit[2..lit.len()-1]).map(CString::new_wide),
                                         pos, len) }

L?\'@ucn\'                        { token_fail("Universal character names are unsupported", pos) }
L?\'\\[^0-7'\"\?\\abfnrtvuUx]\'   { token_fail("Invalid escape sequence", pos) }
L?\"($inchar|@charesc)*@ucn($inchar|@charesc|@ucn)*\"
  {
    token_fail("Universal character names in string literals are unsupported", pos)
  }

-- operators and separators
--
"("   { tok(1, CTokLParen, pos) }
")"   { tok(1, CTokRParen, pos)  }
"["   { tok(1, CTokLBracket, pos) }
"]"   { tok(1, CTokRBracket, pos) }
"->"  { tok(2, CTokArrow, pos) }
"."   { tok(1, CTokDot, pos) }
"!"   { tok(1, CTokExclam, pos) }
"~"   { tok(1, CTokTilde, pos) }
"++"  { tok(2, CTokInc, pos) }
"--"  { tok(2, CTokDec, pos) }
"+"   { tok(1, CTokPlus, pos) }
"-"   { tok(1, CTokMinus, pos) }
"*"   { tok(1, CTokStar, pos) }
"/"   { tok(1, CTokSlash, pos) }
"%"   { tok(1, CTokPercent, pos) }
"&"   { tok(1, CTokAmper, pos) }
"<<"  { tok(2, CTokShiftL, pos) }
">>"  { tok(2, CTokShiftR, pos) }
"<"   { tok(1, CTokLess, pos) }
"<="  { tok(2, CTokLessEq, pos) }
">"   { tok(1, CTokHigh, pos) }
">="  { tok(2, CTokHighEq, pos) }
"=="  { tok(2, CTokEqual, pos) }
"!="  { tok(2, CTokUnequal, pos) }
"^"   { tok(1, CTokHat, pos) }
"|"   { tok(1, CTokBar, pos) }
"&&"  { tok(2, CTokAnd, pos) }
"||"  { tok(2, CTokOr, pos) }
"?"   { tok(1, CTokQuest, pos) }
":"   { tok(1, CTokColon, pos) }
"="   { tok(1, CTokAssign, pos) }
"+="  { tok(2, CTokPlusAss, pos) }
"-="  { tok(2, CTokMinusAss, pos) }
"*="  { tok(2, CTokStarAss, pos) }
"/="  { tok(2, CTokSlashAss, pos) }
"%="  { tok(2, CTokPercAss, pos) }
"&="  { tok(2, CTokAmpAss, pos) }
"^="  { tok(2, CTokHatAss, pos) }
"|="  { tok(2, CTokBarAss, pos) }
"<<=" { tok(3, CTokSLAss, pos) }
">>=" { tok(3, CTokSRAss, pos) }
","   { tok(1, CTokComma, pos) }
";"   { tok(1, CTokSemic, pos) }
"{"   { tok(1, CTokLBrace, pos) }
"}"   { tok(1, CTokRBrace, pos) }
"..." { tok(3, CTokEllipsis, pos) }

{

fn id_token(p: &mut Parser, pos: Position, len: usize) -> Res<CToken> {
    let name = p.new_name();
    let pos = Rc::new(pos);
    let idstr = p.tok_str().to_string();
    let ident = Ident::new(pos.clone(), idstr, name);
    if p.is_type_ident(&ident) {
        Ok(CTokTyIdent((pos, len), ident))
    } else {
        Ok(CTokIdent((pos, len), ident))
    }
}

fn adjust_line_directive(pragma: &str, pos: Position) -> Res<Position> {
    if pos.offset().is_none() {
        // internal position -> do not touch
        return Ok(pos);
    }

    let prag_bytes = pragma.as_bytes();

    // calculate new offset (changes by length of #line pragma)
    let offs_q = pos.offset().unwrap() + pragma.len();

    // find the new row (first number on the line)
    let row_start = prag_bytes.iter().position(|&b| b.is_ascii_digit()).unwrap();
    let row_end = row_start +
        prag_bytes[row_start..].iter().position(|&b| !b.is_ascii_digit()).unwrap_or(prag_bytes.len() - row_start);
    let new_row = pragma[row_start..row_end].parse().map_err(
        |_| ParseError::lexical(pos.clone(), "Invalid line in #line pragma".into()))?;

    let mut flag_start = row_end;
    // find the filename, if any (everything between first and last quote
    let first_quote = prag_bytes.iter().position(|&ch| ch == b'"');
    let last_quote = prag_bytes.iter().rposition(|&ch| ch == b'"');
    let current_fname = pos.file().unwrap();
    let new_fname = if first_quote != last_quote { // found a filename
        let first_quote = first_quote.unwrap();
        let last_quote = last_quote.unwrap();
        flag_start = last_quote + 1;
        // TODO fname should be unescaped if it contains backslashes
        let fname = &pragma[first_quote+1..last_quote];
        if &*current_fname == fname { current_fname } else { Rc::new(fname.to_string()) }
    } else { // no or just one quote
        current_fname
    };

    // find the flags
    let mut lowest_flag = 3;
    for flag in pragma[flag_start..].split_whitespace() {
        if flag == "1" { lowest_flag = 1; }
        else if flag == "2" { lowest_flag = lowest_flag.min(2); }
    }

    // process parent from flags
    let new_parent: Option<Rc<Position>> = if lowest_flag == 1 {
        // entering new file -> push parent
        Some(Rc::new(pos))
    } else if lowest_flag == 2 {
        // returning to this file -> pop parent
        pos.parent().and_then(|p| p.parent())
    } else {
        // same file
        pos.parent()
    };

    Ok(Position::new(offs_q, new_fname, new_row, 1, new_parent))
}

#[inline]
fn tok<M>(len: usize, mk_tok: M, pos: Position) -> Res<CToken>
    where M: Fn(PosLength) -> CToken
{
    Ok(mk_tok((Rc::new(pos), len)))
}

/// error token
#[inline]
fn token_fail(errmsg: &str, pos: Position) -> Res<CToken> {
    Err(ParseError::lexical(pos, errmsg.to_string()))
}

/// token that uses the string
#[inline]
fn token<T, R, M>(p: &Parser, mk_tok: M, from_str: R, pos: Position, len: usize) -> Res<CToken>
    where R: Fn(&str) -> T, M: Fn(PosLength, T) -> CToken
{
    Ok(mk_tok((Rc::new(pos), len), from_str(p.tok_str())))
}

/// token that may fail
#[inline]
fn token_plus<T, R, M>(p: &Parser, mk_tok: M, from_str: R, pos: Position, len: usize) -> Res<CToken>
    where R: Fn(&str) -> Result<T, String>, M: Fn(PosLength, T) -> CToken
{
    match from_str(p.tok_str()) {
        Err(err) => Err(ParseError::lexical(pos, err)),
        Ok(ok)   => Ok(mk_tok((Rc::new(pos), len), ok)),
    }
}

// -----------------------------------------------------------------------------
// The input type

type AlexInput = (Position, InputStream);

fn alex_get_byte(input: &mut AlexInput) -> Option<u8> {
    input.1.peek_byte()
}

fn alex_move(input: &mut AlexInput, len: usize) {
    input.1.move_token(len, &mut input.0);
}

fn lexical_error<T>(p: &mut Parser) -> Res<T> {
    let input = p.input();
    let c = input.1.last_char().unwrap_or('?');
    Err(ParseError::lexical(input.0.clone(),
                            format!("The character {:?} does not fit here", c)))
}

pub fn parse_error<T>(p: &mut Parser) -> Res<T> {
    let last_tok = p.last_token();
    let errmsg = format!("The symbol '{}' does not fit here", last_tok);
    Err(ParseError::syntax(last_tok.pos(), errmsg))
}


// there is a problem with ignored tokens here (that aren't skipped)
// consider
// 1 > int x;
// 2 > LINE "ex.c" 4
// 4 > int y;
// when we get to LINE, we have [int (1,1),x (1,4)] in the token cache.
// Now we run
// > action  (pos 2,0) 14 "LINE \"ex.c\" 3\n"
// which in turn adjusts the position and then calls lex again
// we get `int (pos 4,0)', and have [x (1,4), int (4,1) ] in the token cache (fine)
// but then, we again call setLastToken when returning and get [int (4,1),int (4,1)] in the token cache (bad)
// to resolve this, recursive calls invoke lex_inner(p, false).
pub fn lex(p: &mut Parser) -> Res<CToken> {
    lex_inner(p, true)
}

fn lex_inner(p: &mut Parser, modify_cache: bool) -> Res<CToken> {
    match alex_scan(p.input()) {
        AlexReturn::EOF => {
            p.handle_eof_token();
            Ok(CTokEof)
        },
        AlexReturn::Error => {
            lexical_error(p)
        },
        AlexReturn::Skip(len_bytes) => {
            alex_move(p.input(), len_bytes);
            lex_inner(p, modify_cache)
        },
        AlexReturn::Token(len_bytes, len_chars, action) => {
            let pos = p.pos_clone();
            alex_move(p.input(), len_bytes);
            p.set_last_tok_len(len_bytes);
            let next_tok = action(p, pos, len_chars)?;
            if modify_cache {
                p.set_last_token(&next_tok);
            }
            Ok(next_tok)
        },
    }
}

}
