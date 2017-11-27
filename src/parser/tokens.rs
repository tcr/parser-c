// Original file: "Tokens.hs"
// File auto-generated using Corollary.

use data::position::{Position, PosLength, Pos};
use data::ident::Ident;
use syntax::constants::*;

#[derive(Clone, Debug, Pos)]
pub enum CToken {
    CTokLParen(PosLength),
    CTokRParen(PosLength),
    CTokLBracket(PosLength),
    CTokRBracket(PosLength),
    CTokArrow(PosLength),
    CTokDot(PosLength),
    CTokExclam(PosLength),
    CTokTilde(PosLength),
    CTokInc(PosLength),
    CTokDec(PosLength),
    CTokPlus(PosLength),
    CTokMinus(PosLength),
    CTokStar(PosLength),
    CTokSlash(PosLength),
    CTokPercent(PosLength),
    CTokAmper(PosLength),
    CTokShiftL(PosLength),
    CTokShiftR(PosLength),
    CTokLess(PosLength),
    CTokLessEq(PosLength),
    CTokHigh(PosLength),
    CTokHighEq(PosLength),
    CTokEqual(PosLength),
    CTokUnequal(PosLength),
    CTokHat(PosLength),
    CTokBar(PosLength),
    CTokAnd(PosLength),
    CTokOr(PosLength),
    CTokQuest(PosLength),
    CTokColon(PosLength),
    CTokAssign(PosLength),
    CTokPlusAss(PosLength),
    CTokMinusAss(PosLength),
    CTokStarAss(PosLength),
    CTokSlashAss(PosLength),
    CTokPercAss(PosLength),
    CTokAmpAss(PosLength),
    CTokHatAss(PosLength),
    CTokBarAss(PosLength),
    CTokSLAss(PosLength),
    CTokSRAss(PosLength),
    CTokComma(PosLength),
    CTokSemic(PosLength),
    CTokLBrace(PosLength),
    CTokRBrace(PosLength),
    CTokEllipsis(PosLength),
    CTokAlignof(PosLength),
    CTokAlignas(PosLength),
    CTokAsm(PosLength),
    CTokAtomic(PosLength),
    CTokAuto(PosLength),
    CTokBreak(PosLength),
    CTokBool(PosLength),
    CTokCase(PosLength),
    CTokChar(PosLength),
    CTokConst(PosLength),
    CTokContinue(PosLength),
    CTokComplex(PosLength),
    CTokDefault(PosLength),
    CTokDo(PosLength),
    CTokDouble(PosLength),
    CTokElse(PosLength),
    CTokEnum(PosLength),
    CTokExtern(PosLength),
    CTokFloat(PosLength),
    CTokFloat128(PosLength),
    CTokFor(PosLength),
    CTokGeneric(PosLength),
    CTokGoto(PosLength),
    CTokIf(PosLength),
    CTokInline(PosLength),
    CTokInt(PosLength),
    CTokInt128(PosLength),
    CTokLong(PosLength),
    CTokLabel(PosLength),
    CTokNoreturn(PosLength),
    CTokNullable(PosLength),
    CTokNonnull(PosLength),
    CTokRegister(PosLength),
    CTokRestrict(PosLength),
    CTokReturn(PosLength),
    CTokShort(PosLength),
    CTokSigned(PosLength),
    CTokSizeof(PosLength),
    CTokStatic(PosLength),
    CTokStaticAssert(PosLength),
    CTokStruct(PosLength),
    CTokSwitch(PosLength),
    CTokTypedef(PosLength),
    CTokTypeof(PosLength),
    CTokThread(PosLength),
    CTokUnion(PosLength),
    CTokUnsigned(PosLength),
    CTokVoid(PosLength),
    CTokVolatile(PosLength),
    CTokWhile(PosLength),
    CTokCLit(PosLength, CChar),
    CTokILit(PosLength, CInteger),
    CTokFLit(PosLength, CFloat),
    CTokSLit(PosLength, CString),
    CTokIdent(PosLength, Ident),
    CTokTyIdent(PosLength, Ident),
    CTokGnuC(PosLength, GnuCTok),
    CTokClangC(PosLength, ClangCTok),
    CTokEof,
}
pub use self::CToken::*;

impl CToken {
    pub fn pos_len(&self) -> PosLength {
        match *self {
            CTokLParen((ref pos, len)) |
            CTokRParen((ref pos, len)) |
            CTokLBracket((ref pos, len)) |
            CTokRBracket((ref pos, len)) |
            CTokArrow((ref pos, len)) |
            CTokDot((ref pos, len)) |
            CTokExclam((ref pos, len)) |
            CTokTilde((ref pos, len)) |
            CTokInc((ref pos, len)) |
            CTokDec((ref pos, len)) |
            CTokPlus((ref pos, len)) |
            CTokMinus((ref pos, len)) |
            CTokStar((ref pos, len)) |
            CTokSlash((ref pos, len)) |
            CTokPercent((ref pos, len)) |
            CTokAmper((ref pos, len)) |
            CTokShiftL((ref pos, len)) |
            CTokShiftR((ref pos, len)) |
            CTokLess((ref pos, len)) |
            CTokLessEq((ref pos, len)) |
            CTokHigh((ref pos, len)) |
            CTokHighEq((ref pos, len)) |
            CTokEqual((ref pos, len)) |
            CTokUnequal((ref pos, len)) |
            CTokHat((ref pos, len)) |
            CTokBar((ref pos, len)) |
            CTokAnd((ref pos, len)) |
            CTokOr((ref pos, len)) |
            CTokQuest((ref pos, len)) |
            CTokColon((ref pos, len)) |
            CTokAssign((ref pos, len)) |
            CTokPlusAss((ref pos, len)) |
            CTokMinusAss((ref pos, len)) |
            CTokStarAss((ref pos, len)) |
            CTokSlashAss((ref pos, len)) |
            CTokPercAss((ref pos, len)) |
            CTokAmpAss((ref pos, len)) |
            CTokHatAss((ref pos, len)) |
            CTokBarAss((ref pos, len)) |
            CTokSLAss((ref pos, len)) |
            CTokSRAss((ref pos, len)) |
            CTokComma((ref pos, len)) |
            CTokSemic((ref pos, len)) |
            CTokLBrace((ref pos, len)) |
            CTokRBrace((ref pos, len)) |
            CTokEllipsis((ref pos, len)) |
            CTokAlignof((ref pos, len)) |
            CTokAlignas((ref pos, len)) |
            CTokAsm((ref pos, len)) |
            CTokAtomic((ref pos, len)) |
            CTokAuto((ref pos, len)) |
            CTokBreak((ref pos, len)) |
            CTokBool((ref pos, len)) |
            CTokCase((ref pos, len)) |
            CTokChar((ref pos, len)) |
            CTokConst((ref pos, len)) |
            CTokContinue((ref pos, len)) |
            CTokComplex((ref pos, len)) |
            CTokDefault((ref pos, len)) |
            CTokDo((ref pos, len)) |
            CTokDouble((ref pos, len)) |
            CTokElse((ref pos, len)) |
            CTokEnum((ref pos, len)) |
            CTokExtern((ref pos, len)) |
            CTokFloat((ref pos, len)) |
            CTokFloat128((ref pos, len)) |
            CTokFor((ref pos, len)) |
            CTokGeneric((ref pos, len)) |
            CTokGoto((ref pos, len)) |
            CTokInt((ref pos, len)) |
            CTokInt128((ref pos, len)) |
            CTokInline((ref pos, len)) |
            CTokIf((ref pos, len)) |
            CTokLong((ref pos, len)) |
            CTokLabel((ref pos, len)) |
            CTokNoreturn((ref pos, len)) |
            CTokNullable((ref pos, len)) |
            CTokNonnull((ref pos, len)) |
            CTokRegister((ref pos, len)) |
            CTokRestrict((ref pos, len)) |
            CTokReturn((ref pos, len)) |
            CTokShort((ref pos, len)) |
            CTokSigned((ref pos, len)) |
            CTokSizeof((ref pos, len)) |
            CTokStatic((ref pos, len)) |
            CTokStaticAssert((ref pos, len)) |
            CTokStruct((ref pos, len)) |
            CTokSwitch((ref pos, len)) |
            CTokTypedef((ref pos, len)) |
            CTokTypeof((ref pos, len)) |
            CTokThread((ref pos, len)) |
            CTokUnion((ref pos, len)) |
            CTokUnsigned((ref pos, len)) |
            CTokVoid((ref pos, len)) |
            CTokVolatile((ref pos, len)) |
            CTokWhile((ref pos, len)) |
            CTokCLit((ref pos, len), _) |
            CTokILit((ref pos, len), _) |
            CTokFLit((ref pos, len), _) |
            CTokSLit((ref pos, len), _) |
            CTokIdent((ref pos, len), _) |
            CTokTyIdent((ref pos, len), _) |
            CTokGnuC((ref pos, len), _) |
            CTokClangC((ref pos, len), _) => (pos.clone(), len),
            CTokEof => panic!("CToken::pos_len: Eof"),
        }
    }
}

use std::fmt::{self, Display, Formatter};
impl Display for CToken {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match *self {
            CTokLParen(_)         => write!(f, "("),
            CTokRParen(_)         => write!(f, ")"),
            CTokLBracket(_)       => write!(f, "["),
            CTokRBracket(_)       => write!(f, "]"),
            CTokArrow(_)          => write!(f, "->"),
            CTokDot(_)            => write!(f, "."),
            CTokExclam(_)         => write!(f, "!"),
            CTokTilde(_)          => write!(f, "~"),
            CTokInc(_)            => write!(f, "++"),
            CTokDec(_)            => write!(f, "--"),
            CTokPlus(_)           => write!(f, "+"),
            CTokMinus(_)          => write!(f, "-"),
            CTokStar(_)           => write!(f, "*"),
            CTokSlash(_)          => write!(f, "/"),
            CTokPercent(_)        => write!(f, "%"),
            CTokAmper(_)          => write!(f, "&"),
            CTokShiftL(_)         => write!(f, "<<"),
            CTokShiftR(_)         => write!(f, ">>"),
            CTokLess(_)           => write!(f, "<"),
            CTokLessEq(_)         => write!(f, "<="),
            CTokHigh(_)           => write!(f, ">"),
            CTokHighEq(_)         => write!(f, ">="),
            CTokEqual(_)          => write!(f, "=="),
            CTokUnequal(_)        => write!(f, "!="),
            CTokHat(_)            => write!(f, "^"),
            CTokBar(_)            => write!(f, "|"),
            CTokAnd(_)            => write!(f, "&&"),
            CTokOr(_)             => write!(f, "||"),
            CTokQuest(_)          => write!(f, "?"),
            CTokColon(_)          => write!(f, ":"),
            CTokAssign(_)         => write!(f, "="),
            CTokPlusAss(_)        => write!(f, "+="),
            CTokMinusAss(_)       => write!(f, "-="),
            CTokStarAss(_)        => write!(f, "*="),
            CTokSlashAss(_)       => write!(f, "/="),
            CTokPercAss(_)        => write!(f, "%="),
            CTokAmpAss(_)         => write!(f, "&="),
            CTokHatAss(_)         => write!(f, "^="),
            CTokBarAss(_)         => write!(f, "|="),
            CTokSLAss(_)          => write!(f, "<<="),
            CTokSRAss(_)          => write!(f, ">>="),
            CTokComma(_)          => write!(f, ","),
            CTokSemic(_)          => write!(f, ";"),
            CTokLBrace(_)         => write!(f, "{{"),
            CTokRBrace(_)         => write!(f, "}}"),
            CTokEllipsis(_)       => write!(f, "..."),
            CTokAlignof(_)        => write!(f, "alignof"),
            CTokAlignas(_)        => write!(f, "_Alignas"),
            CTokAsm(_)            => write!(f, "asm"),
            CTokAtomic(_)         => write!(f, "_Atomic"),
            CTokAuto(_)           => write!(f, "auto"),
            CTokBreak(_)          => write!(f, "break"),
            CTokBool(_)           => write!(f, "_Bool"),
            CTokCase(_)           => write!(f, "case"),
            CTokChar(_)           => write!(f, "char"),
            CTokConst(_)          => write!(f, "const"),
            CTokContinue(_)       => write!(f, "continue"),
            CTokComplex(_)        => write!(f, "_Complex"),
            CTokDefault(_)        => write!(f, "default"),
            CTokDo(_)             => write!(f, "do"),
            CTokDouble(_)         => write!(f, "double"),
            CTokElse(_)           => write!(f, "else"),
            CTokEnum(_)           => write!(f, "enum"),
            CTokExtern(_)         => write!(f, "extern"),
            CTokFloat(_)          => write!(f, "float"),
            CTokFloat128(_)       => write!(f, "__float128"),
            CTokFor(_)            => write!(f, "for"),
            CTokGeneric(_)        => write!(f, "_Generic"),
            CTokGoto(_)           => write!(f, "goto"),
            CTokInt(_)            => write!(f, "int"),
            CTokInt128(_)         => write!(f, "__int128"),
            CTokInline(_)         => write!(f, "inline"),
            CTokIf(_)             => write!(f, "if"),
            CTokLong(_)           => write!(f, "long"),
            CTokLabel(_)          => write!(f, "__label__"),
            CTokNoreturn(_)       => write!(f, "_Noreturn"),
            CTokNullable(_)       => write!(f, "_Nullable"),
            CTokNonnull(_)        => write!(f, "_Nonnull"),
            CTokRegister(_)       => write!(f, "register"),
            CTokRestrict(_)       => write!(f, "restrict"),
            CTokReturn(_)         => write!(f, "return"),
            CTokShort(_)          => write!(f, "short"),
            CTokSigned(_)         => write!(f, "signed"),
            CTokSizeof(_)         => write!(f, "sizeof"),
            CTokStatic(_)         => write!(f, "static"),
            CTokStaticAssert(_)   => write!(f, "_Static_assert"),
            CTokStruct(_)         => write!(f, "struct"),
            CTokSwitch(_)         => write!(f, "switch"),
            CTokTypedef(_)        => write!(f, "typedef"),
            CTokTypeof(_)         => write!(f, "typeof"),
            CTokThread(_)         => write!(f, "_Thread_local"),
            CTokUnion(_)          => write!(f, "union"),
            CTokUnsigned(_)       => write!(f, "unsigned"),
            CTokVoid(_)           => write!(f, "void"),
            CTokVolatile(_)       => write!(f, "volatile"),
            CTokWhile(_)          => write!(f, "while"),
            CTokCLit(_, ref v)    => write!(f, "{}", v),
            CTokILit(_, ref v)    => write!(f, "{}", v),
            CTokFLit(_, ref v)    => write!(f, "{}", v),
            CTokSLit(_, ref v)    => write!(f, "{}", v),
            CTokIdent(_, ref v)   => write!(f, "{}", v.as_str()),
            CTokTyIdent(_, ref v) => write!(f, "{}", v.as_str()),
            CTokGnuC(_, GnuCTok::Attr) => write!(f, "__attribute__"),
            CTokGnuC(_, GnuCTok::Ext)  => write!(f, "__extension__"),
            CTokGnuC(_, GnuCTok::VaArg)   => write!(f, "__builtin_va_arg"),
            CTokGnuC(_, GnuCTok::Offsetof) => write!(f, "__builtin_offsetof"),
            CTokGnuC(_, GnuCTok::TyCompat) => write!(f, "__builtin_types_compatible_p"),
            CTokGnuC(_, GnuCTok::ComplexReal) => write!(f, "__real__"),
            CTokGnuC(_, GnuCTok::ComplexImag) => write!(f, "__image__"),
            CTokClangC(_, ClangCTok::CVersion(ClangCVersion(ref v))) => write!(f, "{}", v),
            CTokClangC(_, ClangCTok::ConvertVector) => write!(f, "__builtin_convertvector"),
            CTokEof               => write!(f, "<EOF>"),
        }
    }
}

#[derive(Clone, Debug)]
pub enum GnuCTok {
    Attr,
    Ext,
    VaArg,
    Offsetof,
    TyCompat,
    ComplexReal,
    ComplexImag,
}

#[derive(Clone, Debug)]
pub enum ClangCTok {
    CVersion(ClangCVersion),
    ConvertVector,
}
