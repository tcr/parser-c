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

@floatsuffix    = [fFlL]
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

-- #line directive (K&R A12.6)
--
-- * allows further ints after the file name a la GCC; as the GCC CPP docu
--   doesn't say how many ints there can be, we allow an unbound number
--
\#$space*@int$space*(\"($infname|@charesc)*\"$space*)?(@int$space*)*\r?$eol
  {
     let new_pos = adjustLineDirective(p.getTokString(), pos)?;
     p.setPos(new_pos);
     lexToken_q(p, false)
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
$identletter($identletter|$digit)*   { idtok(p, pos, len) }

-- constants (follows K&R A2.5)
--
-- * K&R,C99 explicitly mention `enumeration-constants'; however, as they are
--   lexically identifiers, we do not have an extra case for them
--

-- integer constants (follows K&R A2.5.1, C99 6.4.4.1)
-- NOTE: 0 is lexed as octal integer constant, and readCOctal takes care of this
0$octdigit*@intgnusuffix?       { token_plus(p, CTokILit, readCOctal, pos, len) }
$digitNZ$digit*@intgnusuffix?   { token_plus(p, CTokILit, |lit| readCInteger(DecRepr, lit), pos, len) }
0[xX]$hexdigit+@intgnusuffix?   { token_plus(p, CTokILit, |lit| readCInteger(HexRepr, &lit[2..]), pos, len) }

(0$octdigit*|$digitNZ$digit*|0[xX]$hexdigit+)[uUlL]+ { token_fail("Invalid integer constant suffix", pos) }

-- character constants (follows K&R A2.5.2, C99 6.4.4.4)
--
-- * Universal Character Names are unsupported and cause an error.
\'($inchar|@charesc)\'      { token_plus(p, CTokCLit,
                                         |lit| unescapeChar(&lit[1..]).map(|t| cChar(t.0)), pos, len) }
L\'($inchar|@charesc)\'     { token_plus(p, CTokCLit,
                                         |lit| unescapeChar(&lit[2..]).map(|t| cChar_w(t.0)), pos, len) }
\'($inchar|@charesc){2,}\'  { token_plus(p, CTokCLit,
                                         |lit| unescapeMultiChars(&lit[1..lit.len()-1]).map(|t| cChars(false, t)),
                                         pos, len) }
L\'($inchar|@charesc){2,}\' { token_plus(p, CTokCLit,
                                         |lit| unescapeMultiChars(&lit[2..lit.len()-1]).map(|t| cChars(true, t)),
                                         pos, len) }

-- Clang version literals
@clangversion               { token(p, |pos, vers| CTokClangC(pos, ClangCTok::CVersion(vers)),
                                    readClangCVersion, pos, len) }

-- float constants (follows K&R A2.5.3. C99 6.4.4.2)
--
-- * NOTE: Hexadecimal floating constants without binary exponents are forbidden.
--         They generate a lexer error, because they are hard to recognize in the parser.
(@mantpart@exppart?|@intpart@exppart)@floatgnusuffix?  { token(p, CTokFLit, readCFloat, pos, len) }
@hexprefix(@hexmant|@hexdigits)@binexp@floatgnusuffix? { token(p, CTokFLit, readCFloat, pos, len) }
@hexprefix@hexmant                                     { token_fail("Hexadecimal floating constant requires an exponent",
                                                                    pos) }

-- string literal (follows K&R A2.6)
-- C99: 6.4.5.
\"($instr|@charesc)*\"      { token_plus(p, CTokSLit,
                                         |lit| unescapeString(&lit[1..lit.len()-1]).map(cString),
                                         pos, len) }
L\"($instr|@charesc)*\"     { token_plus(p, CTokSLit,
                                         |lit| unescapeString(&lit[2..lit.len()-1]).map(cString_w),
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

fn idtok(p: &mut Parser, pos: Position, len: usize) -> Res<CToken> {
    let name = p.getNewName();
    let pos = Rc::new(pos);
    let idstr = p.getTokString().to_string();
    let ident = Ident::new(pos.clone(), idstr, name);
    if p.isTypeIdent(&ident) {
        Ok(CTokTyIdent((pos, len), ident))
    } else {
        Ok(CTokIdent((pos, len), ident))
    }
}

fn adjustLineDirective(pragma: &str, pos: Position) -> Res<Position> {
    // calculate new offset
    let offs_q = pos.offset() + pragma.len();
    // find the row
    let row: String = pragma[1..].trim().chars().take_while(|&ch| ch.is_digit(10)).collect();
    let row = row.parse().map_err(
        |_| ParseError::lexical(pos.clone(), "Invalid line in #line pragma".into()))?;
    // find the filename, if present
    let current_fname = pos.file();
    let new_fname = if let Some(fname_start) = pragma.as_bytes().iter().position(|&ch| ch == b'"') {
        let fname_end = pragma[fname_start+1..].as_bytes().iter().position(|&ch| ch == b'"').unwrap();
        let fname = &pragma[fname_start+1..fname_start+fname_end+1];
        if &*current_fname == fname { current_fname } else { Rc::new(fname.to_string()) }
    } else {
        current_fname
    };

    Ok(Position::new(offs_q, new_fname, row, 1))
}

#[inline]
fn tok<M>(len: usize, mkTok: M, pos: Position) -> Res<CToken>
    where M: Fn(PosLength) -> CToken
{
    Ok(mkTok((Rc::new(pos), len)))
}

/// error token
#[inline]
fn token_fail(errmsg: &str, pos: Position) -> Res<CToken> {
    Err(ParseError::lexical(pos, errmsg.to_string()))
}

/// token that uses the string
#[inline]
fn token<T, R, M>(p: &Parser, mkTok: M, fromStr: R, pos: Position, len: usize) -> Res<CToken>
    where R: Fn(&str) -> T, M: Fn(PosLength, T) -> CToken
{
    Ok(mkTok((Rc::new(pos), len), fromStr(p.getTokString())))
}

/// token that may fail
#[inline]
fn token_plus<T, R, M>(p: &Parser, mkTok: M, fromStr: R, pos: Position, len: usize) -> Res<CToken>
    where R: Fn(&str) -> Result<T, String>, M: Fn(PosLength, T) -> CToken
{
    match fromStr(p.getTokString()) {
        Err(err) => Err(ParseError::lexical(pos, err)),
        Ok(ok)   => Ok(mkTok((Rc::new(pos), len), ok)),
    }
}

// -----------------------------------------------------------------------------
// The input type

type AlexInput = (Position, InputStream);

fn alexGetByte(input: &mut AlexInput) -> Option<u8> {
    input.1.peek_byte()
}

fn alexMove(input: &mut AlexInput, len: usize) {
    input.1.move_token(len, &mut input.0);
}

fn lexicalError<T>(p: &mut Parser) -> Res<T> {
    let input = p.getInput();
    let c = input.1.last_char().unwrap_or('?');
    Err(ParseError::lexical(input.0.clone(),
                            format!("The character {:?} does not fit here", c)))
}

pub fn parseError<T>(p: &mut Parser) -> Res<T> {
    let lastTok = p.getLastToken();
    let errmsg = format!("The symbol '{}' does not fit here", lastTok);
    Err(ParseError::syntax(lastTok.pos(), errmsg))
}


// there is a problem with ignored tokens here (that aren't skipped)
// consider
// 1 > int x;
// 2 > LINE "ex.c" 4
// 4 > int y;
// when we get to LINE, we have [int (1,1),x (1,4)] in the token cache.
// Now we run
// > action  (pos 2,0) 14 "LINE \"ex.c\" 3\n"
// which in turn adjusts the position and then calls lexToken again
// we get `int (pos 4,0)', and have [x (1,4), int (4,1) ] in the token cache (fine)
// but then, we again call setLastToken when returning and get [int (4,1),int (4,1)] in the token cache (bad)
// to resolve this, recursive calls invoke lexToken' False.
pub fn lexC(p: &mut Parser) -> Res<CToken> {
    lexToken_q(p, true)
}

fn lexToken_q(p: &mut Parser, modifyCache: bool) -> Res<CToken> {
    match alexScan(p.getInput()) {
        AlexReturn::EOF => {
            p.handleEofToken();
            Ok(CTokEof)
        },
        AlexReturn::Error => {
            lexicalError(p)
        },
        AlexReturn::Skip(len_bytes) => {
            alexMove(p.getInput(), len_bytes);
            lexToken_q(p, modifyCache)
        },
        AlexReturn::Token(len_bytes, len_chars, action) => {
            let pos = p.getPosClone();
            alexMove(p.getInput(), len_bytes);
            p.setLastTokLen(len_bytes);
            let nextTok = action(p, pos, len_chars)?;
            if modifyCache {
                p.setLastToken(&nextTok);
            }
            Ok(nextTok)
        },
    }
}

}
