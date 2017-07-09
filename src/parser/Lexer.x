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

use data::input_stream::*;
use data::ident::*;
use data::position::*;
use syntax::constants::*;
use parser::parser_monad::*;
use parser::tokens::*;
use std::str::FromStr;

// fn(A, B) -> fn(C) -> {eval fn(A, B, C)}
#[allow(unused_macros)]
macro_rules! partial_1 {
    ($inner: expr) => ( box $inner );
    ($inner: expr, $($arg: expr),+ ) => ( box |_0| { $inner($($arg),+ , _0) } )
}

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
    rshift_monad(setPos(adjustLineDirective(len, takeChars_str(len, inp), pos)), lexToken_q(false))
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

-- identifiers and keywords (follows K&R A2.3 and A2.4)
--
$identletter($identletter|$digit)*   { idkwtok(takeChars_str(len, inp), pos) }

-- constants (follows K&R A2.5)
--
-- * K&R,C99 explicitly mention `enumeration-constants'; however, as they are
--   lexically identifiers, we do not have an extra case for them
--

-- integer constants (follows K&R A2.5.1, C99 6.4.4.1)
-- NOTE: 0 is lexed as octal integer constant, and readCOctal takes care of this
0$octdigit*@intgnusuffix?       { token_plus(box CTokILit, box readCOctal, pos, len, inp) }
$digitNZ$digit*@intgnusuffix?   { token_plus(box CTokILit, partial_1!(readCInteger, DecRepr), pos, len, inp) }
0[xX]$hexdigit+@intgnusuffix?   { token_plus(box CTokILit, box move |_0| readCInteger(HexRepr, drop_str(2, _0)),
                                             pos, len, inp) }

(0$octdigit*|$digitNZ$digit*|0[xX]$hexdigit+)[uUlL]+ { token_fail("Invalid integer constant suffix", pos, len, inp) }

-- character constants (follows K&R A2.5.2, C99 6.4.4.4)
--
-- * Universal Character Names are unsupported and cause an error.
\'($inchar|@charesc)\'  { token(box CTokCLit,
                                box move |_0| cChar(fst(unescapeChar(tail_str(_0)))),
                                pos, len, inp) }
L\'($inchar|@charesc)\' { token(box CTokCLit,
                                box move |_0| cChar_w(fst(unescapeChar(tail_str(tail_str(_0))))),
                                pos, len, inp) }
\'($inchar|@charesc){2,}\' { token(box CTokCLit,
                                   box move |_0| flip(cChars, false, unescapeMultiChars(tail_str(_0))),
                                   pos, len, inp) }
L\'($inchar|@charesc){2,}\' { token(box CTokCLit,
                                    box move |_0| flip(cChars, true, unescapeMultiChars(tail_str(tail_str(_0)))),
                                    pos, len, inp) }

-- Clang version literals
@clangversion           { token(box move |pos, _0| { CTokClangC(pos, ClangCTok(_0)) },
                                box readClangCVersion, pos, len, inp) }

-- float constants (follows K&R A2.5.3. C99 6.4.4.2)
--
-- * NOTE: Hexadecimal floating constants without binary exponents are forbidden.
--         They generate a lexer error, because they are hard to recognize in the parser.
(@mantpart@exppart?|@intpart@exppart)@floatgnusuffix?  { token(box CTokFLit, box readCFloat, pos, len, inp) }
@hexprefix(@hexmant|@hexdigits)@binexp@floatgnusuffix? { token(box CTokFLit, box readCFloat, pos, len, inp) }
@hexprefix@hexmant                                     { token_fail("Hexadecimal floating constant requires an exponent",
                                                                    pos, len, inp) }

-- string literal (follows K&R A2.6)
-- C99: 6.4.5.
\"($instr|@charesc)*\"      { token(box CTokSLit,
                                    box move |_0| cString(unescapeString(init_str(tail_str(_0)))),
                                    pos, len, inp) }
L\"($instr|@charesc)*\"     { token(box CTokSLit,
                                    box move |_0| cString_w(unescapeString(init_str(tail_str(tail_str(_0))))),
                                    pos, len, inp) }

L?\'@ucn\'                        { token_fail("Universal character names are unsupported", pos, len, inp) }
L?\'\\[^0-7'\"\?\\abfnrtvuUx]\'   { token_fail("Invalid escape sequence", pos, len, inp) }
L?\"($inchar|@charesc)*@ucn($inchar|@charesc|@ucn)*\"
  {
    token_fail("Universal character names in string literals are unsupported", pos, len, inp)
  }

-- operators and separators
--
"("   { token_(1, box CTokLParen, pos, len, inp) }
")"   { token_(1, box CTokRParen, pos, len, inp)  }
"["   { token_(1, box CTokLBracket, pos, len, inp) }
"]"   { token_(1, box CTokRBracket, pos, len, inp) }
"->"  { token_(2, box CTokArrow, pos, len, inp) }
"."   { token_(1, box CTokDot, pos, len, inp) }
"!"   { token_(1, box CTokExclam, pos, len, inp) }
"~"   { token_(1, box CTokTilde, pos, len, inp) }
"++"  { token_(2, box CTokInc, pos, len, inp) }
"--"  { token_(2, box CTokDec, pos, len, inp) }
"+"   { token_(1, box CTokPlus, pos, len, inp) }
"-"   { token_(1, box CTokMinus, pos, len, inp) }
"*"   { token_(1, box CTokStar, pos, len, inp) }
"/"   { token_(1, box CTokSlash, pos, len, inp) }
"%"   { token_(1, box CTokPercent, pos, len, inp) }
"&"   { token_(1, box CTokAmper, pos, len, inp) }
"<<"  { token_(2, box CTokShiftL, pos, len, inp) }
">>"  { token_(2, box CTokShiftR, pos, len, inp) }
"<"   { token_(1, box CTokLess, pos, len, inp) }
"<="  { token_(2, box CTokLessEq, pos, len, inp) }
">"   { token_(1, box CTokHigh, pos, len, inp) }
">="  { token_(2, box CTokHighEq, pos, len, inp) }
"=="  { token_(2, box CTokEqual, pos, len, inp) }
"!="  { token_(2, box CTokUnequal, pos, len, inp) }
"^"   { token_(1, box CTokHat, pos, len, inp) }
"|"   { token_(1, box CTokBar, pos, len, inp) }
"&&"  { token_(2, box CTokAnd, pos, len, inp) }
"||"  { token_(2, box CTokOr, pos, len, inp) }
"?"   { token_(1, box CTokQuest, pos, len, inp) }
":"   { token_(1, box CTokColon, pos, len, inp) }
"="   { token_(1, box CTokAssign, pos, len, inp) }
"+="  { token_(2, box CTokPlusAss, pos, len, inp) }
"-="  { token_(2, box CTokMinusAss, pos, len, inp) }
"*="  { token_(2, box CTokStarAss, pos, len, inp) }
"/="  { token_(2, box CTokSlashAss, pos, len, inp) }
"%="  { token_(2, box CTokPercAss, pos, len, inp) }
"&="  { token_(2, box CTokAmpAss, pos, len, inp) }
"^="  { token_(2, box CTokHatAss, pos, len, inp) }
"|="  { token_(2, box CTokBarAss, pos, len, inp) }
"<<=" { token_(3, box CTokSLAss, pos, len, inp) }
">>=" { token_(3, box CTokSRAss, pos, len, inp) }
","   { token_(1, box CTokComma, pos, len, inp) }
\;    { token_(1, box CTokSemic, pos, len, inp) }
"{"   { token_(1, box CTokLBrace, pos, len, inp) }
"}"   { token_(1, box CTokRBrace, pos, len, inp) }
"..." { token_(3, box CTokEllipsis, pos, len, inp) }


{

/// Fix the 'octal' lexing of '0'
pub fn readCOctal(s: String) -> Result<CInteger, String> {
    if s.chars().nth(0) == Some('0') {
        if s.len() > 1 && isDigit(s.chars().nth(1).unwrap()) {
            readCInteger(OctalRepr, s[1..].to_string())
        } else {
            readCInteger(DecRepr, s)
        }
    } else {
        panic!("ReadOctal: string does not start with `0'")
    }
}

// We use the odd looking list of string patterns here rather than normal
// string literals since GHC converts the latter into a sequence of string
// comparisons (ie a linear search) but it translates the former using its
// effecient pattern matching which gives us the expected radix-style search.
// This change makes a significant performance difference [chak]
//
// To make this a little more maintainable, we autogenerate it from this list,
// using the script GenerateKeywords.hs (in /scripts)
/*
alignas _Alignas, alignof _Alignof __alignof alignof __alignof__, asm @__, atomic _Atomic, auto
break, bool _Bool,
case, char, const @__, continue, complex _Complex __complex__
default, do, double,
else, enum, extern,
float, for, goto,
if, inline @__, int, __int128, long, noreturn _Noreturn,
float, for,
generic _Generic, goto,
if, inline @__, int, int128 __int128, long,
noreturn _Noreturn,  _Nullable __nullable, _Nonnull __nonnull,
register, restrict @__, return
short, signed @__, sizeof, static, staticAssert _Static_assert, struct, switch,
typedef, typeof @__, thread __thread _Thread_local
union, unsigned, void, volatile @__,
while,
label __label__
(CTokGnuC GnuCAttrTok) __attribute __attribute__
(CTokGnuC GnuCExtTok) __extension__
(CTokGnuC GnuCComplexReal) __real __real__
(CTokGnuC GnuCComplexImag) __imag __imag__
(CTokGnuC GnuCVaArg) __builtin_va_arg
(CTokGnuC GnuCOffsetof) __builtin_offsetof
(CTokGnuC GnuCTyCompat) __builtin_types_compatible_p
*/
// Tokens: _Alignas _Alignof __alignof alignof __alignof__ __asm asm __asm__ _Atomic auto break _Bool case char __const const __const__ continue _Complex __complex__ default do double else enum extern float for _Generic goto if __inline inline __inline__ int __int128 long _Noreturn  _Nullable __nullable _Nonnull __nonnull register __restrict restrict __restrict__ return short __signed signed __signed__ sizeof static _Static_assert struct switch typedef __typeof typeof __typeof__ __thread _Thread_local union unsigned void __volatile volatile __volatile__ while __label__ __attribute __attribute__ __extension__ __real __real__ __imag __imag__ __builtin_va_arg __builtin_offsetof __builtin_types_compatible_p

pub fn idkwtok(id: String, pos: Position) -> P<CToken> {
    match id.as_ref() {
        "_Alignas" => tok(8, box CTokAlignas, pos),
        "_Alignof" => tok(8, box CTokAlignof, pos),
        "_Atomic" => tok(7, box CTokAtomic, pos),
        "_Bool" => tok(5, box CTokBool, pos),
        "_Complex" => tok(8, box CTokComplex, pos),
        "_Nonnull" => tok(8, box CTokNonnull, pos),
        "_Generic" => tok(8, box CTokGeneric, pos),
        "_Noreturn" => tok(9, box CTokNoreturn, pos),
        "_Nullable" => tok(9, box CTokNullable, pos),
        "_Static_assert" => tok(14, box CTokStaticAssert, pos),
        "_Thread_local" => tok(13, box CTokThread, pos),
        "__alignof" => tok(9, box CTokAlignof, pos),
        "alignof" => tok(7, box CTokAlignof, pos),
        "__alignof__" => tok(11, box CTokAlignof, pos),
        "__asm" => tok(5, box CTokAsm, pos),
        "asm" => tok(3, box CTokAsm, pos),
        "__asm__" => tok(7, box CTokAsm, pos),
        "__attribute" => tok(11, partial_1!(CTokGnuC, GnuCAttrTok), pos),
        "__attribute__" => tok(13, partial_1!(CTokGnuC, GnuCAttrTok), pos),
        "auto" => tok(4, box CTokAuto, pos),
        "break" => tok(5, box CTokBreak, pos),
        "__builtin_offsetof" => tok(18, partial_1!(CTokGnuC, GnuCOffsetof), pos),
        "__builtin_types_compatible_p" => tok(28, partial_1!(CTokGnuC, GnuCTyCompat), pos),
        "__builtin_va_arg" => tok(16, partial_1!(CTokGnuC, GnuCVaArg), pos),
        "case" => tok(4, box CTokCase, pos),
        "char" => tok(4, box CTokChar, pos),
        "__complex__" => tok(11, box CTokComplex, pos),
        "__const" => tok(7, box CTokConst, pos),
        "const" => tok(5, box CTokConst, pos),
        "__const__" => tok(9, box CTokConst, pos),
        "continue" => tok(8, box CTokContinue, pos),
        "default" => tok(7, box CTokDefault, pos),
        "do" => tok(2, box CTokDo, pos),
        "double" => tok(6, box CTokDouble, pos),
        "else" => tok(4, box CTokElse, pos),
        "enum" => tok(4, box CTokEnum, pos),
        "__extension__" => tok(13, partial_1!(CTokGnuC, GnuCExtTok), pos),
        "extern" => tok(6, box CTokExtern, pos),
        "float" => tok(5, box CTokFloat, pos),
        "for" => tok(3, box CTokFor, pos),
        "goto" => tok(4, box CTokGoto, pos),
        "if" => tok(2, box CTokIf, pos),
        "__imag" => tok(6, partial_1!(CTokGnuC, GnuCComplexImag), pos),
        "__imag__" => tok(8, partial_1!(CTokGnuC, GnuCComplexImag), pos),
        "__inline" => tok(8, box CTokInline, pos),
        "inline" => tok(6, box CTokInline, pos),
        "__inline__" => tok(10, box CTokInline, pos),
        "int" => tok(3, box CTokInt, pos),
        "__int128" => tok(8, box CTokInt128, pos),
        "__label__" => tok(9, box CTokLabel, pos),
        "long" => tok(4, box CTokLong, pos),
        "__nonnull" => tok(9, box CTokNonnull, pos),
        "__nullable" => tok(10, box CTokNullable, pos),
        "__real" => tok(6, partial_1!(CTokGnuC, GnuCComplexReal), pos),
        "__real__" => tok(8, partial_1!(CTokGnuC, GnuCComplexReal), pos),
        "register" => tok(8, box CTokRegister, pos),
        "__restrict" => tok(10, box CTokRestrict, pos),
        "restrict" => tok(8, box CTokRestrict, pos),
        "__restrict__" => tok(12, box CTokRestrict, pos),
        "return" => tok(6, box CTokReturn, pos),
        "short" => tok(5, box CTokShort, pos),
        "__signed" => tok(8, box CTokSigned, pos),
        "signed" => tok(6, box CTokSigned, pos),
        "__signed__" => tok(10, box CTokSigned, pos),
        "sizeof" => tok(6, box CTokSizeof, pos),
        "static" => tok(6, box CTokStatic, pos),
        "struct" => tok(6, box CTokStruct, pos),
        "switch" => tok(6, box CTokSwitch, pos),
        "__thread" => tok(8, box CTokThread, pos),
        "typedef" => tok(7, box CTokTypedef, pos),
        "__typeof" => tok(8, box CTokTypeof, pos),
        "typeof" => tok(6, box CTokTypeof, pos),
        "__typeof__" => tok(10, box CTokTypeof, pos),
        "union" => tok(5, box CTokUnion, pos),
        "unsigned" => tok(8, box CTokUnsigned, pos),
        "void" => tok(4, box CTokVoid, pos),
        "__volatile" => tok(10, box CTokVolatile, pos),
        "volatile" => tok(8, box CTokVolatile, pos),
        "__volatile__" => tok(12, box CTokVolatile, pos),
        "while" => tok(5, box CTokWhile, pos),
        cs => {
            // TODO
            let cs = cs.to_owned();
            let pos = pos.clone();

            thenP(
                /* let name = */ getNewName(),
                box move |name| {
                    let pos = pos.clone();
                    let len = cs.len() as isize;
                    let ident = mkIdent(pos.clone(), cs.clone(), name);

                    thenP(isTypeIdent(ident.clone()), box move |tyident| {
                        if tyident {
                            __return(CTokTyIdent((pos.clone(), len), ident.clone()))
                        } else {
                            __return(CTokIdent((pos.clone(), len), ident.clone()))
                        }
                    })
                })
        },
    }
}

pub fn ignoreAttribute() -> P<()> {
    pub fn skipTokens(n: isize) -> P<()> {
        thenP(lexToken_q(false), box move |ntok| {
            match ntok {
                CTokRParen(_) if n == 1 => { __return(()) }
                CTokRParen(_) => { skipTokens(n - 1) }
                CTokLParen(_) => { skipTokens(n + 1) },
                _             => { skipTokens(n) },
            }
        })
    }
    skipTokens(0)
}

pub fn tok(len: isize, tc: Box<Fn(PosLength) -> CToken>, pos: Position) -> P<CToken> {
    __return(tc((pos, len)))
}

pub fn adjustLineDirective(pragmaLen: isize, __str: String, pos: Position) -> Position {
    fn dropWhite(input: String) -> String {
        dropWhile((|c| { (c == ' ') || (c == '\t') }), input)
    }

    // TODO cleanup
    let offs_q = pos.offset() + pragmaLen;

    let str_q = dropWhite(drop_str(1, __str));

    let (rowStr, str_q_q) = span(isDigit, str_q);

    // from read(rowStr)
    let row_q = isize::from_str(&rowStr).unwrap();

    let str_q_q_q = dropWhite(str_q_q);

    let fnameStr = takeWhile_str(|x| { x != '\"' }, drop_str(1, str_q_q_q.clone()));

    let fname = pos.file();

    let fname_q = if str_q_q_q.len() == 0 || head_str(str_q_q_q) != '"' {
        fname
    } else if fnameStr == fname {
        // try and get more sharing of file name strings
        fname
    } else {
        fnameStr
    };

    Position::new(offs_q, fname_q, row_q, 1)
}

/// special utility for the lexer
pub fn unescapeMultiChars(cs: String) -> String {
    if cs.len() > 2 {
        // TODO cleanup
        let cs_0 = cs.chars().nth(0).unwrap();
        let value = unescapeChar(cs.clone());
        format!("{}{}", cs_0, unescapeMultiChars(cs.chars().skip(1).collect::<String>()))
    } else if cs.len() == 1 && cs.chars().nth(0).unwrap() == '\'' {
        "".to_string()
    } else {
        panic!("Unexpected end of multi-char constant")
    }
}

/// token that ignores the string
pub fn token_(len: isize, mkTok: Box<Fn(PosLength) -> CToken>, pos: Position,
              _: isize, _: InputStream) -> P<CToken> {
    __return(mkTok((pos, len)))
}

/// error token
pub fn token_fail(errmsg: &str, pos: Position, _: isize, _: InputStream) -> P<CToken> {
    failP(pos, vec!["Lexical Error !".to_string(), errmsg.to_string()])
}

/// token that uses the string
pub fn token<a>(mkTok: Box<Fn(PosLength, a) -> CToken>,
                fromStr: Box<Fn(String) -> a>, pos: Position, len: isize, __str: InputStream) -> P<CToken> {
    __return(mkTok((pos, len), fromStr(takeChars_str(len, __str))))
}

/// token that may fail
pub fn token_plus<a>(mkTok: Box<Fn(PosLength, a) -> CToken>,
                     fromStr: Box<Fn(String) -> Result<a, String>>,
                     pos: Position, len: isize, __str: InputStream) -> P<CToken> {
    match fromStr(takeChars_str(len, __str)) {
        Err(err) => {
            failP(pos, vec!["Lexical error ! ".to_string(), err])
        },
        Ok(ok) => {
            __return(mkTok((pos, len), ok))
        },
    }
}

// -----------------------------------------------------------------------------
// The input type

pub type AlexInput = (Position, InputStream);

pub fn alexInputPrevChar(_: AlexInput) -> char {
    panic!("alexInputPrevChar not used")
}

pub fn alexGetByte((p, is): AlexInput) -> Option<(Word8, AlexInput)> {
    if inputStreamEmpty(is.clone()) {
        None
    } else {
        let (b, s) = takeByte(is);
        // this is safe for latin-1, but ugly
        let p_q = alexMove(p, chr(fromIntegral(b as isize)));
        Some((b, (p_q, s)))
    }
}

pub fn alexMove(pos: Position, ch: char) -> Position {
    match ch {
        ' '  => pos.inc(1),
        '\n' => pos.retPos(),
        '\r' => pos.incOffset(1),
        _    => pos.inc(1),
    }
}

pub fn lexicalError<a: 'static>() -> P<a> {
    thenP(getPos(), box move |pos| {
        thenP(getInput(), box move |input| {
            let (c, _) = takeChar(input);
            let pos = pos.clone();
            failP(pos, vec![
                "Lexical error !".to_string(),
                format!("The character {} does not fit here.", c),
            ])
        })
    })
}

pub fn parseError<a: 'static>() -> P<a> {
    thenP(getLastToken(), box move |lastTok| {
        failP(posOf(lastTok.clone()), vec![
            "Syntax error !".to_string(),
            format!("The symbol `{}' does not fit here.", lastTok)
        ])
    })
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
pub fn lexToken() -> P<CToken> {
    lexToken_q(true)
}

pub fn lexToken_q(modifyCache: bool) -> P<CToken> {
    thenP(getPos(), box move |pos| {

        thenP(getInput(), box move |inp| {
            let pos = pos.clone();

            match alexScan((pos.clone(), inp.clone()), 0) {
                AlexEOF => {
                    thenP(handleEofToken(), box move |_| __return(CTokEof))
                },
                AlexError(_inp) => {
                    lexicalError()
                },
                AlexSkip((pos_q, inp_q), _len) => {
                    let _0 = setPos(pos_q);
                    let _1 = setInput(inp_q);
                    let _2 = lexToken_q(modifyCache);
                    thenP(_0, box move |_| { let _2 = _2.clone(); thenP(_1.clone(), box move |_| _2.clone()) })
                },
                AlexToken((pos_q, inp_q), len, action) => {
                    let _0 = setPos(pos_q);
                    let _1 = setInput(inp_q);
                    let _2 = thenP(action(pos, len, inp), box move |nextTok| {
                        if modifyCache {
                            thenP(setLastToken(nextTok.clone()), box move |_| __return(nextTok.clone()))
                        } else {
                            __return(nextTok)
                        }
                    });
                    thenP(_0, box move |_| { let _2 = _2.clone(); thenP(_1.clone(), box move |_| _2.clone()) })
                },
            }
        })
    })
}

pub fn lexC<a: 'static>(cont: Box<Fn(CToken) -> P<a>>) -> P<a> {
    thenP(lexToken(), box move |nextTok| {
        cont(nextTok)
    })
}

}
