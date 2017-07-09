#![allow(unreachable_patterns)]
#[macro_use] use corollary_support::*;
#[macro_use] use matches;
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
macro_rules! clones {
    ($($value: ident),*) => {
        $( let $value = $value.clone(); )*
    }
}

// Relevant C99 sections:
//
// 6.5 Expressions .1 - .17 and 6.6 (almost literally)
//  Supported GNU extensions:
//     - Allow a compound statement as an expression
//     - Various __builtin_* forms that take type parameters
//     - `alignof' expression or type
//     - `__extension__' to suppress warnings about extensions
//     - Allow taking address of a label with: && label
//     - Omitting the `then' part of conditional expressions
//     - complex numbers
//
// 6.7 C Declarations .1 -.8
//  Supported GNU extensions:
//     - '__thread' thread local storage (6.7.1)
//
// 6.8 Statements .1 - .8
//  Supported GNU extensions:
//    - case ranges (C99 6.8.1)
//    - '__label__ ident;' declarations (C99 6.8.2)
//    - computed gotos (C99 6.8.6)
//
// 6.9 Translation unit
//  Supported GNU extensions:
//     - allow empty translation_unit
//     - allow redundant ';'
//     - allow extension keyword before external declaration
//     - asm definitions
//
//  Since some of the grammar productions are quite difficult to read,
//  (especially those involved with the decleration syntax) we document them
//  with an extended syntax that allows a more consise representation:
//
//  Ordinary rules
//
//   foo      named terminal or non-terminal
//
//   'c'      terminal, literal character token
//
//   A B      concatenation
//
//   A | B    alternation
//
//   (A)      grouping
//
//  Extended rules
//
//   A?       optional, short hand for (A|) or [A]{ 0==A || 1==A }
//
//   ...      stands for some part of the grammar omitted for clarity
//
//   {A}      represents sequences, 0 or more.
//
//   <permute> modifier which states that any permutation of the immediate subterms is valid
//
//
//- TODO ----------------------------------------------------------------------
//
//  !* We ignore C11 _Atomic type annotations
//  !* We ignore the C99 static keyword (see C99 6.7.5.3)
//  !* We do not distinguish in the AST between incomplete array types and
//      complete variable length arrays ([ '*' ] means the latter). (see C99 6.7.5.2)
//  !* The AST doesn't allow recording __attribute__ of unnamed struct field
//     (see , struct_default_declaring_list, struct_identifier_declarator)
//  !* see `We're being far to liberal here' (... struct definition within structs)
//  * Documentation isn't complete and consistent yet.

// Parser produced by modified Happy Version 1.19.6

#[derive(Clone)]
enum HappyAbsSyn {
    HappyTerminal(CToken),
    HappyErrorToken(isize),
    HappyAbsSyn7(CTranslUnit),
    HappyAbsSyn8(Reversed<Vec<CExtDecl>>),
    HappyAbsSyn9(CExtDecl),
    HappyAbsSyn10(CFunDef),
    HappyAbsSyn11(CDeclr),
    HappyAbsSyn12(CStat),
    HappyAbsSyn15(()),
    HappyAbsSyn17(Reversed<Vec<CBlockItem>>),
    HappyAbsSyn18(CBlockItem),
    HappyAbsSyn21(Reversed<Vec<Ident>>),
    HappyAbsSyn26(CAsmStmt),
    HappyAbsSyn27(Option<CTypeQual>),
    HappyAbsSyn28(Vec<CAsmOperand>),
    HappyAbsSyn29(Reversed<Vec<CAsmOperand>>),
    HappyAbsSyn30(CAsmOperand),
    HappyAbsSyn31(Reversed<Vec<CStrLit>>),
    HappyAbsSyn32(CDecl),
    HappyAbsSyn33(Reversed<Vec<CDecl>>),
    HappyAbsSyn35((Option<CStrLit>, Vec<CAttr>)),
    HappyAbsSyn37(Vec<CDeclSpec>),
    HappyAbsSyn38(Reversed<Vec<CDeclSpec>>),
    HappyAbsSyn39(CDeclSpec),
    HappyAbsSyn41(CStorageSpec),
    HappyAbsSyn42(CFunSpec),
    HappyAbsSyn43(CAlignSpec),
    HappyAbsSyn45(CTypeSpec),
    HappyAbsSyn53(CStructUnion),
    HappyAbsSyn54(Located<CStructTag>),
    HappyAbsSyn59((Option<CDeclr>, Option<CExpr>)),
    HappyAbsSyn61(CEnum),
    HappyAbsSyn62(Reversed<Vec<(Ident, Option<CExpr>)>>),
    HappyAbsSyn63((Ident, Option<CExpr>)),
    HappyAbsSyn64(CTypeQual),
    HappyAbsSyn65(Reversed<Vec<CTypeQual>>),
    HappyAbsSyn66(CDeclrR),
    HappyAbsSyn67(Option<CStrLit>),
    HappyAbsSyn82((Vec<CDecl>, bool)),
    HappyAbsSyn88(Rc<Box<Fn(CDeclrR) -> CDeclrR>>),
    HappyAbsSyn93(CInit),
    HappyAbsSyn94(Option<CInit>),
    HappyAbsSyn95(Reversed<CInitList>),
    HappyAbsSyn96(Vec<CDesignator>),
    HappyAbsSyn97(Reversed<Vec<CDesignator>>),
    HappyAbsSyn98(CDesignator),
    HappyAbsSyn100(CExpr),
    HappyAbsSyn101(Reversed<Vec<(Option<CDecl>, CExpr)>>),
    HappyAbsSyn102((Option<CDecl>, CExpr)),
    HappyAbsSyn105(Reversed<Vec<CExpr>>),
    HappyAbsSyn107(Located<CUnaryOp>),
    HappyAbsSyn121(Located<CAssignOp>),
    HappyAbsSyn124(Option<CExpr>),
    HappyAbsSyn127(CConst),
    HappyAbsSyn128(CStrLit),
    HappyAbsSyn129(Reversed<Vec<CString>>),
    HappyAbsSyn130(ClangCVersion),
    HappyAbsSyn131(Ident),
    HappyAbsSyn132(Vec<CAttr>),
    HappyAbsSyn135(Reversed<Vec<CAttr>>),
    HappyAbsSyn136(Option<CAttr>),
}
use self::HappyAbsSyn::*;


type ActionReturn = Box<FnBox(isize, (CToken), HappyState<(CToken), Box<FnBox(HappyStk<HappyAbsSyn>) -> P<HappyAbsSyn>>>,
                           Vec<HappyState<(CToken), Box<FnBox(HappyStk<HappyAbsSyn>) -> P<HappyAbsSyn>>>>,
                           HappyStk<HappyAbsSyn>) -> P<HappyAbsSyn>>;
type Action<A, B> = Box<Fn(isize, isize, (CToken), HappyState<(CToken), Box<FnBox(B) -> P<A>>>,
                           Vec<HappyState<(CToken), Box<FnBox(B) -> P<A>>>>, B) -> P<A>>;

fn action_0(i: isize) -> ActionReturn {
    match i {
        7 => partial_5!(happyGoto, curry_1_5!(action_144)),
        8 => partial_5!(happyGoto, curry_1_5!(action_5)),
        _ => happyReduce_5()
    }
}

fn action_1(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_111)),
        150 => partial_5!(happyShift, curry_1_5!(action_112)),
        185 => partial_5!(happyShift, curry_1_5!(action_113)),
        187 => partial_5!(happyShift, curry_1_5!(action_114)),
        188 => partial_5!(happyShift, curry_1_5!(action_115)),
        190 => partial_5!(happyShift, curry_1_5!(action_116)),
        192 => partial_5!(happyShift, curry_1_5!(action_117)),
        195 => partial_5!(happyShift, curry_1_5!(action_118)),
        198 => partial_5!(happyShift, curry_1_5!(action_119)),
        200 => partial_5!(happyShift, curry_1_5!(action_120)),
        201 => partial_5!(happyShift, curry_1_5!(action_121)),
        202 => partial_5!(happyShift, curry_1_5!(action_122)),
        207 => partial_5!(happyShift, curry_1_5!(action_123)),
        208 => partial_5!(happyShift, curry_1_5!(action_124)),
        209 => partial_5!(happyShift, curry_1_5!(action_125)),
        210 => partial_5!(happyShift, curry_1_5!(action_126)),
        212 => partial_5!(happyShift, curry_1_5!(action_127)),
        215 => partial_5!(happyShift, curry_1_5!(action_128)),
        218 => partial_5!(happyShift, curry_1_5!(action_129)),
        219 => partial_5!(happyShift, curry_1_5!(action_130)),
        221 => partial_5!(happyShift, curry_1_5!(action_131)),
        222 => partial_5!(happyShift, curry_1_5!(action_132)),
        223 => partial_5!(happyShift, curry_1_5!(action_133)),
        225 => partial_5!(happyShift, curry_1_5!(action_134)),
        226 => partial_5!(happyShift, curry_1_5!(action_135)),
        227 => partial_5!(happyShift, curry_1_5!(action_136)),
        228 => partial_5!(happyShift, curry_1_5!(action_137)),
        229 => partial_5!(happyShift, curry_1_5!(action_138)),
        230 => partial_5!(happyShift, curry_1_5!(action_139)),
        237 => partial_5!(happyShift, curry_1_5!(action_140)),
        238 => partial_5!(happyShift, curry_1_5!(action_141)),
        239 => partial_5!(happyShift, curry_1_5!(action_142)),
        240 => partial_5!(happyShift, curry_1_5!(action_143)),
        9 => partial_5!(happyGoto, curry_1_5!(action_76)),
        10 => partial_5!(happyGoto, curry_1_5!(action_77)),
        11 => partial_5!(happyGoto, curry_1_5!(action_78)),
        32 => partial_5!(happyGoto, curry_1_5!(action_79)),
        34 => partial_5!(happyGoto, curry_1_5!(action_80)),
        36 => partial_5!(happyGoto, curry_1_5!(action_81)),
        37 => partial_5!(happyGoto, curry_1_5!(action_82)),
        38 => partial_5!(happyGoto, curry_1_5!(action_83)),
        40 => partial_5!(happyGoto, curry_1_5!(action_84)),
        41 => partial_5!(happyGoto, curry_1_5!(action_85)),
        42 => partial_5!(happyGoto, curry_1_5!(action_86)),
        43 => partial_5!(happyGoto, curry_1_5!(action_87)),
        44 => partial_5!(happyGoto, curry_1_5!(action_88)),
        45 => partial_5!(happyGoto, curry_1_5!(action_89)),
        46 => partial_5!(happyGoto, curry_1_5!(action_90)),
        47 => partial_5!(happyGoto, curry_1_5!(action_91)),
        48 => partial_5!(happyGoto, curry_1_5!(action_92)),
        49 => partial_5!(happyGoto, curry_1_5!(action_93)),
        50 => partial_5!(happyGoto, curry_1_5!(action_94)),
        51 => partial_5!(happyGoto, curry_1_5!(action_95)),
        52 => partial_5!(happyGoto, curry_1_5!(action_96)),
        53 => partial_5!(happyGoto, curry_1_5!(action_97)),
        54 => partial_5!(happyGoto, curry_1_5!(action_98)),
        61 => partial_5!(happyGoto, curry_1_5!(action_99)),
        65 => partial_5!(happyGoto, curry_1_5!(action_100)),
        75 => partial_5!(happyGoto, curry_1_5!(action_101)),
        76 => partial_5!(happyGoto, curry_1_5!(action_102)),
        77 => partial_5!(happyGoto, curry_1_5!(action_103)),
        78 => partial_5!(happyGoto, curry_1_5!(action_104)),
        79 => partial_5!(happyGoto, curry_1_5!(action_105)),
        80 => partial_5!(happyGoto, curry_1_5!(action_106)),
        81 => partial_5!(happyGoto, curry_1_5!(action_107)),
        132 => partial_5!(happyGoto, curry_1_5!(action_108)),
        133 => partial_5!(happyGoto, curry_1_5!(action_109)),
        134 => partial_5!(happyGoto, curry_1_5!(action_110)),
        _ => happyReduce_471()
    }
}

fn action_2(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_26)),
        144 => partial_5!(happyShift, curry_1_5!(action_27)),
        145 => partial_5!(happyShift, curry_1_5!(action_28)),
        146 => partial_5!(happyShift, curry_1_5!(action_29)),
        147 => partial_5!(happyShift, curry_1_5!(action_30)),
        148 => partial_5!(happyShift, curry_1_5!(action_31)),
        149 => partial_5!(happyShift, curry_1_5!(action_32)),
        150 => partial_5!(happyShift, curry_1_5!(action_33)),
        153 => partial_5!(happyShift, curry_1_5!(action_34)),
        164 => partial_5!(happyShift, curry_1_5!(action_35)),
        180 => partial_5!(happyShift, curry_1_5!(action_60)),
        181 => partial_5!(happyShift, curry_1_5!(action_61)),
        184 => partial_5!(happyShift, curry_1_5!(action_36)),
        187 => partial_5!(happyShift, curry_1_5!(action_62)),
        189 => partial_5!(happyShift, curry_1_5!(action_63)),
        191 => partial_5!(happyShift, curry_1_5!(action_64)),
        194 => partial_5!(happyShift, curry_1_5!(action_65)),
        196 => partial_5!(happyShift, curry_1_5!(action_66)),
        197 => partial_5!(happyShift, curry_1_5!(action_67)),
        203 => partial_5!(happyShift, curry_1_5!(action_68)),
        204 => partial_5!(happyShift, curry_1_5!(action_37)),
        205 => partial_5!(happyShift, curry_1_5!(action_69)),
        206 => partial_5!(happyShift, curry_1_5!(action_70)),
        217 => partial_5!(happyShift, curry_1_5!(action_71)),
        220 => partial_5!(happyShift, curry_1_5!(action_38)),
        224 => partial_5!(happyShift, curry_1_5!(action_72)),
        232 => partial_5!(happyShift, curry_1_5!(action_73)),
        233 => partial_5!(happyShift, curry_1_5!(action_39)),
        234 => partial_5!(happyShift, curry_1_5!(action_40)),
        235 => partial_5!(happyShift, curry_1_5!(action_41)),
        236 => partial_5!(happyShift, curry_1_5!(action_42)),
        237 => partial_5!(happyShift, curry_1_5!(action_74)),
        238 => partial_5!(happyShift, curry_1_5!(action_75)),
        240 => partial_5!(happyShift, curry_1_5!(action_44)),
        241 => partial_5!(happyShift, curry_1_5!(action_45)),
        242 => partial_5!(happyShift, curry_1_5!(action_46)),
        243 => partial_5!(happyShift, curry_1_5!(action_47)),
        244 => partial_5!(happyShift, curry_1_5!(action_48)),
        245 => partial_5!(happyShift, curry_1_5!(action_49)),
        12 => partial_5!(happyGoto, curry_1_5!(action_50)),
        13 => partial_5!(happyGoto, curry_1_5!(action_51)),
        14 => partial_5!(happyGoto, curry_1_5!(action_52)),
        22 => partial_5!(happyGoto, curry_1_5!(action_53)),
        23 => partial_5!(happyGoto, curry_1_5!(action_54)),
        24 => partial_5!(happyGoto, curry_1_5!(action_55)),
        25 => partial_5!(happyGoto, curry_1_5!(action_56)),
        26 => partial_5!(happyGoto, curry_1_5!(action_57)),
        100 => partial_5!(happyGoto, curry_1_5!(action_6)),
        104 => partial_5!(happyGoto, curry_1_5!(action_7)),
        106 => partial_5!(happyGoto, curry_1_5!(action_8)),
        107 => partial_5!(happyGoto, curry_1_5!(action_9)),
        108 => partial_5!(happyGoto, curry_1_5!(action_10)),
        109 => partial_5!(happyGoto, curry_1_5!(action_11)),
        110 => partial_5!(happyGoto, curry_1_5!(action_12)),
        111 => partial_5!(happyGoto, curry_1_5!(action_13)),
        112 => partial_5!(happyGoto, curry_1_5!(action_14)),
        113 => partial_5!(happyGoto, curry_1_5!(action_15)),
        114 => partial_5!(happyGoto, curry_1_5!(action_16)),
        115 => partial_5!(happyGoto, curry_1_5!(action_17)),
        116 => partial_5!(happyGoto, curry_1_5!(action_18)),
        117 => partial_5!(happyGoto, curry_1_5!(action_19)),
        118 => partial_5!(happyGoto, curry_1_5!(action_20)),
        119 => partial_5!(happyGoto, curry_1_5!(action_21)),
        120 => partial_5!(happyGoto, curry_1_5!(action_22)),
        122 => partial_5!(happyGoto, curry_1_5!(action_58)),
        127 => partial_5!(happyGoto, curry_1_5!(action_24)),
        128 => partial_5!(happyGoto, curry_1_5!(action_25)),
        131 => partial_5!(happyGoto, curry_1_5!(action_59)),
        _ => box happyFail
    }
}

fn action_3(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_26)),
        144 => partial_5!(happyShift, curry_1_5!(action_27)),
        145 => partial_5!(happyShift, curry_1_5!(action_28)),
        146 => partial_5!(happyShift, curry_1_5!(action_29)),
        147 => partial_5!(happyShift, curry_1_5!(action_30)),
        148 => partial_5!(happyShift, curry_1_5!(action_31)),
        149 => partial_5!(happyShift, curry_1_5!(action_32)),
        150 => partial_5!(happyShift, curry_1_5!(action_33)),
        153 => partial_5!(happyShift, curry_1_5!(action_34)),
        164 => partial_5!(happyShift, curry_1_5!(action_35)),
        184 => partial_5!(happyShift, curry_1_5!(action_36)),
        204 => partial_5!(happyShift, curry_1_5!(action_37)),
        220 => partial_5!(happyShift, curry_1_5!(action_38)),
        233 => partial_5!(happyShift, curry_1_5!(action_39)),
        234 => partial_5!(happyShift, curry_1_5!(action_40)),
        235 => partial_5!(happyShift, curry_1_5!(action_41)),
        236 => partial_5!(happyShift, curry_1_5!(action_42)),
        237 => partial_5!(happyShift, curry_1_5!(action_43)),
        240 => partial_5!(happyShift, curry_1_5!(action_44)),
        241 => partial_5!(happyShift, curry_1_5!(action_45)),
        242 => partial_5!(happyShift, curry_1_5!(action_46)),
        243 => partial_5!(happyShift, curry_1_5!(action_47)),
        244 => partial_5!(happyShift, curry_1_5!(action_48)),
        245 => partial_5!(happyShift, curry_1_5!(action_49)),
        100 => partial_5!(happyGoto, curry_1_5!(action_6)),
        104 => partial_5!(happyGoto, curry_1_5!(action_7)),
        106 => partial_5!(happyGoto, curry_1_5!(action_8)),
        107 => partial_5!(happyGoto, curry_1_5!(action_9)),
        108 => partial_5!(happyGoto, curry_1_5!(action_10)),
        109 => partial_5!(happyGoto, curry_1_5!(action_11)),
        110 => partial_5!(happyGoto, curry_1_5!(action_12)),
        111 => partial_5!(happyGoto, curry_1_5!(action_13)),
        112 => partial_5!(happyGoto, curry_1_5!(action_14)),
        113 => partial_5!(happyGoto, curry_1_5!(action_15)),
        114 => partial_5!(happyGoto, curry_1_5!(action_16)),
        115 => partial_5!(happyGoto, curry_1_5!(action_17)),
        116 => partial_5!(happyGoto, curry_1_5!(action_18)),
        117 => partial_5!(happyGoto, curry_1_5!(action_19)),
        118 => partial_5!(happyGoto, curry_1_5!(action_20)),
        119 => partial_5!(happyGoto, curry_1_5!(action_21)),
        120 => partial_5!(happyGoto, curry_1_5!(action_22)),
        122 => partial_5!(happyGoto, curry_1_5!(action_23)),
        127 => partial_5!(happyGoto, curry_1_5!(action_24)),
        128 => partial_5!(happyGoto, curry_1_5!(action_25)),
        _ => box happyFail
    }
}

fn action_4(i: isize) -> ActionReturn {
    match i {
        8 => partial_5!(happyGoto, curry_1_5!(action_5)),
        _ => box happyFail
    }
}

fn action_5(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_111)),
        150 => partial_5!(happyShift, curry_1_5!(action_112)),
        180 => partial_5!(happyShift, curry_1_5!(action_334)),
        185 => partial_5!(happyShift, curry_1_5!(action_113)),
        187 => partial_5!(happyShift, curry_1_5!(action_114)),
        188 => partial_5!(happyShift, curry_1_5!(action_115)),
        190 => partial_5!(happyShift, curry_1_5!(action_116)),
        192 => partial_5!(happyShift, curry_1_5!(action_117)),
        195 => partial_5!(happyShift, curry_1_5!(action_118)),
        198 => partial_5!(happyShift, curry_1_5!(action_119)),
        200 => partial_5!(happyShift, curry_1_5!(action_120)),
        201 => partial_5!(happyShift, curry_1_5!(action_121)),
        202 => partial_5!(happyShift, curry_1_5!(action_122)),
        207 => partial_5!(happyShift, curry_1_5!(action_123)),
        208 => partial_5!(happyShift, curry_1_5!(action_124)),
        209 => partial_5!(happyShift, curry_1_5!(action_125)),
        210 => partial_5!(happyShift, curry_1_5!(action_126)),
        212 => partial_5!(happyShift, curry_1_5!(action_127)),
        215 => partial_5!(happyShift, curry_1_5!(action_128)),
        218 => partial_5!(happyShift, curry_1_5!(action_129)),
        219 => partial_5!(happyShift, curry_1_5!(action_130)),
        221 => partial_5!(happyShift, curry_1_5!(action_131)),
        222 => partial_5!(happyShift, curry_1_5!(action_132)),
        223 => partial_5!(happyShift, curry_1_5!(action_133)),
        225 => partial_5!(happyShift, curry_1_5!(action_134)),
        226 => partial_5!(happyShift, curry_1_5!(action_135)),
        227 => partial_5!(happyShift, curry_1_5!(action_136)),
        228 => partial_5!(happyShift, curry_1_5!(action_137)),
        229 => partial_5!(happyShift, curry_1_5!(action_138)),
        230 => partial_5!(happyShift, curry_1_5!(action_139)),
        237 => partial_5!(happyShift, curry_1_5!(action_140)),
        238 => partial_5!(happyShift, curry_1_5!(action_141)),
        239 => partial_5!(happyShift, curry_1_5!(action_142)),
        240 => partial_5!(happyShift, curry_1_5!(action_143)),
        247 => happyReduce_4(),
        9 => partial_5!(happyGoto, curry_1_5!(action_333)),
        10 => partial_5!(happyGoto, curry_1_5!(action_77)),
        11 => partial_5!(happyGoto, curry_1_5!(action_78)),
        32 => partial_5!(happyGoto, curry_1_5!(action_79)),
        34 => partial_5!(happyGoto, curry_1_5!(action_80)),
        36 => partial_5!(happyGoto, curry_1_5!(action_81)),
        37 => partial_5!(happyGoto, curry_1_5!(action_82)),
        38 => partial_5!(happyGoto, curry_1_5!(action_83)),
        40 => partial_5!(happyGoto, curry_1_5!(action_84)),
        41 => partial_5!(happyGoto, curry_1_5!(action_85)),
        42 => partial_5!(happyGoto, curry_1_5!(action_86)),
        43 => partial_5!(happyGoto, curry_1_5!(action_87)),
        44 => partial_5!(happyGoto, curry_1_5!(action_88)),
        45 => partial_5!(happyGoto, curry_1_5!(action_89)),
        46 => partial_5!(happyGoto, curry_1_5!(action_90)),
        47 => partial_5!(happyGoto, curry_1_5!(action_91)),
        48 => partial_5!(happyGoto, curry_1_5!(action_92)),
        49 => partial_5!(happyGoto, curry_1_5!(action_93)),
        50 => partial_5!(happyGoto, curry_1_5!(action_94)),
        51 => partial_5!(happyGoto, curry_1_5!(action_95)),
        52 => partial_5!(happyGoto, curry_1_5!(action_96)),
        53 => partial_5!(happyGoto, curry_1_5!(action_97)),
        54 => partial_5!(happyGoto, curry_1_5!(action_98)),
        61 => partial_5!(happyGoto, curry_1_5!(action_99)),
        65 => partial_5!(happyGoto, curry_1_5!(action_100)),
        75 => partial_5!(happyGoto, curry_1_5!(action_101)),
        76 => partial_5!(happyGoto, curry_1_5!(action_102)),
        77 => partial_5!(happyGoto, curry_1_5!(action_103)),
        78 => partial_5!(happyGoto, curry_1_5!(action_104)),
        79 => partial_5!(happyGoto, curry_1_5!(action_105)),
        80 => partial_5!(happyGoto, curry_1_5!(action_106)),
        81 => partial_5!(happyGoto, curry_1_5!(action_107)),
        132 => partial_5!(happyGoto, curry_1_5!(action_108)),
        133 => partial_5!(happyGoto, curry_1_5!(action_109)),
        134 => partial_5!(happyGoto, curry_1_5!(action_110)),
        _ => happyReduce_471()
    }
}

fn action_6(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_376()
    }
}

fn action_7(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_327)),
        140 => partial_5!(happyShift, curry_1_5!(action_328)),
        142 => partial_5!(happyShift, curry_1_5!(action_329)),
        143 => partial_5!(happyShift, curry_1_5!(action_330)),
        146 => partial_5!(happyShift, curry_1_5!(action_331)),
        147 => partial_5!(happyShift, curry_1_5!(action_332)),
        _ => happyReduce_388()
    }
}

fn action_8(i: isize) -> ActionReturn {
    match i {
        168 => partial_5!(happyShift, curry_1_5!(action_316)),
        169 => partial_5!(happyShift, curry_1_5!(action_317)),
        170 => partial_5!(happyShift, curry_1_5!(action_318)),
        171 => partial_5!(happyShift, curry_1_5!(action_319)),
        172 => partial_5!(happyShift, curry_1_5!(action_320)),
        173 => partial_5!(happyShift, curry_1_5!(action_321)),
        174 => partial_5!(happyShift, curry_1_5!(action_322)),
        175 => partial_5!(happyShift, curry_1_5!(action_323)),
        176 => partial_5!(happyShift, curry_1_5!(action_324)),
        177 => partial_5!(happyShift, curry_1_5!(action_325)),
        178 => partial_5!(happyShift, curry_1_5!(action_326)),
        121 => partial_5!(happyGoto, curry_1_5!(action_315)),
        _ => happyReduce_406()
    }
}

fn action_9(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_26)),
        144 => partial_5!(happyShift, curry_1_5!(action_27)),
        145 => partial_5!(happyShift, curry_1_5!(action_28)),
        146 => partial_5!(happyShift, curry_1_5!(action_29)),
        147 => partial_5!(happyShift, curry_1_5!(action_30)),
        148 => partial_5!(happyShift, curry_1_5!(action_31)),
        149 => partial_5!(happyShift, curry_1_5!(action_32)),
        150 => partial_5!(happyShift, curry_1_5!(action_33)),
        153 => partial_5!(happyShift, curry_1_5!(action_34)),
        164 => partial_5!(happyShift, curry_1_5!(action_35)),
        184 => partial_5!(happyShift, curry_1_5!(action_36)),
        204 => partial_5!(happyShift, curry_1_5!(action_37)),
        220 => partial_5!(happyShift, curry_1_5!(action_38)),
        233 => partial_5!(happyShift, curry_1_5!(action_39)),
        234 => partial_5!(happyShift, curry_1_5!(action_40)),
        235 => partial_5!(happyShift, curry_1_5!(action_41)),
        236 => partial_5!(happyShift, curry_1_5!(action_42)),
        237 => partial_5!(happyShift, curry_1_5!(action_43)),
        240 => partial_5!(happyShift, curry_1_5!(action_44)),
        241 => partial_5!(happyShift, curry_1_5!(action_45)),
        242 => partial_5!(happyShift, curry_1_5!(action_46)),
        243 => partial_5!(happyShift, curry_1_5!(action_47)),
        244 => partial_5!(happyShift, curry_1_5!(action_48)),
        245 => partial_5!(happyShift, curry_1_5!(action_49)),
        100 => partial_5!(happyGoto, curry_1_5!(action_6)),
        104 => partial_5!(happyGoto, curry_1_5!(action_7)),
        106 => partial_5!(happyGoto, curry_1_5!(action_259)),
        107 => partial_5!(happyGoto, curry_1_5!(action_9)),
        108 => partial_5!(happyGoto, curry_1_5!(action_314)),
        127 => partial_5!(happyGoto, curry_1_5!(action_24)),
        128 => partial_5!(happyGoto, curry_1_5!(action_25)),
        _ => box happyFail
    }
}

fn action_10(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_408()
    }
}

fn action_11(i: isize) -> ActionReturn {
    match i {
        150 => partial_5!(happyShift, curry_1_5!(action_311)),
        151 => partial_5!(happyShift, curry_1_5!(action_312)),
        152 => partial_5!(happyShift, curry_1_5!(action_313)),
        _ => happyReduce_412()
    }
}

fn action_12(i: isize) -> ActionReturn {
    match i {
        148 => partial_5!(happyShift, curry_1_5!(action_309)),
        149 => partial_5!(happyShift, curry_1_5!(action_310)),
        _ => happyReduce_415()
    }
}

fn action_13(i: isize) -> ActionReturn {
    match i {
        154 => partial_5!(happyShift, curry_1_5!(action_307)),
        155 => partial_5!(happyShift, curry_1_5!(action_308)),
        _ => happyReduce_418()
    }
}

fn action_14(i: isize) -> ActionReturn {
    match i {
        156 => partial_5!(happyShift, curry_1_5!(action_303)),
        157 => partial_5!(happyShift, curry_1_5!(action_304)),
        158 => partial_5!(happyShift, curry_1_5!(action_305)),
        159 => partial_5!(happyShift, curry_1_5!(action_306)),
        _ => happyReduce_423()
    }
}

fn action_15(i: isize) -> ActionReturn {
    match i {
        160 => partial_5!(happyShift, curry_1_5!(action_301)),
        161 => partial_5!(happyShift, curry_1_5!(action_302)),
        _ => happyReduce_426()
    }
}

fn action_16(i: isize) -> ActionReturn {
    match i {
        153 => partial_5!(happyShift, curry_1_5!(action_300)),
        _ => happyReduce_428()
    }
}

fn action_17(i: isize) -> ActionReturn {
    match i {
        162 => partial_5!(happyShift, curry_1_5!(action_299)),
        _ => happyReduce_430()
    }
}

fn action_18(i: isize) -> ActionReturn {
    match i {
        163 => partial_5!(happyShift, curry_1_5!(action_298)),
        _ => happyReduce_432()
    }
}

fn action_19(i: isize) -> ActionReturn {
    match i {
        164 => partial_5!(happyShift, curry_1_5!(action_297)),
        _ => happyReduce_434()
    }
}

fn action_20(i: isize) -> ActionReturn {
    match i {
        165 => partial_5!(happyShift, curry_1_5!(action_295)),
        166 => partial_5!(happyShift, curry_1_5!(action_296)),
        _ => happyReduce_436()
    }
}

fn action_21(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_439()
    }
}

fn action_22(i: isize) -> ActionReturn {
    match i {
        179 => partial_5!(happyShift, curry_1_5!(action_294)),
        _ => happyReduce_452()
    }
}

fn action_23(i: isize) -> ActionReturn {
    match i {
        247 => box happyAccept,
        _ => box happyFail
    }
}

fn action_24(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_361()
    }
}

fn action_25(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_362()
    }
}

fn action_26(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_26)),
        144 => partial_5!(happyShift, curry_1_5!(action_27)),
        145 => partial_5!(happyShift, curry_1_5!(action_28)),
        146 => partial_5!(happyShift, curry_1_5!(action_29)),
        147 => partial_5!(happyShift, curry_1_5!(action_30)),
        148 => partial_5!(happyShift, curry_1_5!(action_31)),
        149 => partial_5!(happyShift, curry_1_5!(action_32)),
        150 => partial_5!(happyShift, curry_1_5!(action_33)),
        153 => partial_5!(happyShift, curry_1_5!(action_34)),
        164 => partial_5!(happyShift, curry_1_5!(action_35)),
        181 => partial_5!(happyShift, curry_1_5!(action_61)),
        184 => partial_5!(happyShift, curry_1_5!(action_36)),
        190 => partial_5!(happyShift, curry_1_5!(action_116)),
        192 => partial_5!(happyShift, curry_1_5!(action_117)),
        195 => partial_5!(happyShift, curry_1_5!(action_118)),
        198 => partial_5!(happyShift, curry_1_5!(action_119)),
        200 => partial_5!(happyShift, curry_1_5!(action_120)),
        202 => partial_5!(happyShift, curry_1_5!(action_122)),
        204 => partial_5!(happyShift, curry_1_5!(action_37)),
        208 => partial_5!(happyShift, curry_1_5!(action_124)),
        209 => partial_5!(happyShift, curry_1_5!(action_125)),
        210 => partial_5!(happyShift, curry_1_5!(action_126)),
        218 => partial_5!(happyShift, curry_1_5!(action_129)),
        219 => partial_5!(happyShift, curry_1_5!(action_130)),
        220 => partial_5!(happyShift, curry_1_5!(action_38)),
        223 => partial_5!(happyShift, curry_1_5!(action_133)),
        226 => partial_5!(happyShift, curry_1_5!(action_135)),
        228 => partial_5!(happyShift, curry_1_5!(action_137)),
        229 => partial_5!(happyShift, curry_1_5!(action_138)),
        230 => partial_5!(happyShift, curry_1_5!(action_139)),
        233 => partial_5!(happyShift, curry_1_5!(action_39)),
        234 => partial_5!(happyShift, curry_1_5!(action_40)),
        235 => partial_5!(happyShift, curry_1_5!(action_41)),
        236 => partial_5!(happyShift, curry_1_5!(action_42)),
        237 => partial_5!(happyShift, curry_1_5!(action_43)),
        238 => partial_5!(happyShift, curry_1_5!(action_141)),
        239 => partial_5!(happyShift, curry_1_5!(action_142)),
        240 => partial_5!(happyShift, curry_1_5!(action_44)),
        241 => partial_5!(happyShift, curry_1_5!(action_45)),
        242 => partial_5!(happyShift, curry_1_5!(action_46)),
        243 => partial_5!(happyShift, curry_1_5!(action_47)),
        244 => partial_5!(happyShift, curry_1_5!(action_48)),
        245 => partial_5!(happyShift, curry_1_5!(action_49)),
        14 => partial_5!(happyGoto, curry_1_5!(action_285)),
        44 => partial_5!(happyGoto, curry_1_5!(action_286)),
        45 => partial_5!(happyGoto, curry_1_5!(action_89)),
        47 => partial_5!(happyGoto, curry_1_5!(action_287)),
        49 => partial_5!(happyGoto, curry_1_5!(action_288)),
        51 => partial_5!(happyGoto, curry_1_5!(action_289)),
        52 => partial_5!(happyGoto, curry_1_5!(action_96)),
        53 => partial_5!(happyGoto, curry_1_5!(action_97)),
        54 => partial_5!(happyGoto, curry_1_5!(action_98)),
        61 => partial_5!(happyGoto, curry_1_5!(action_99)),
        65 => partial_5!(happyGoto, curry_1_5!(action_290)),
        86 => partial_5!(happyGoto, curry_1_5!(action_291)),
        100 => partial_5!(happyGoto, curry_1_5!(action_6)),
        104 => partial_5!(happyGoto, curry_1_5!(action_7)),
        106 => partial_5!(happyGoto, curry_1_5!(action_8)),
        107 => partial_5!(happyGoto, curry_1_5!(action_9)),
        108 => partial_5!(happyGoto, curry_1_5!(action_10)),
        109 => partial_5!(happyGoto, curry_1_5!(action_11)),
        110 => partial_5!(happyGoto, curry_1_5!(action_12)),
        111 => partial_5!(happyGoto, curry_1_5!(action_13)),
        112 => partial_5!(happyGoto, curry_1_5!(action_14)),
        113 => partial_5!(happyGoto, curry_1_5!(action_15)),
        114 => partial_5!(happyGoto, curry_1_5!(action_16)),
        115 => partial_5!(happyGoto, curry_1_5!(action_17)),
        116 => partial_5!(happyGoto, curry_1_5!(action_18)),
        117 => partial_5!(happyGoto, curry_1_5!(action_19)),
        118 => partial_5!(happyGoto, curry_1_5!(action_20)),
        119 => partial_5!(happyGoto, curry_1_5!(action_21)),
        120 => partial_5!(happyGoto, curry_1_5!(action_22)),
        122 => partial_5!(happyGoto, curry_1_5!(action_292)),
        127 => partial_5!(happyGoto, curry_1_5!(action_24)),
        128 => partial_5!(happyGoto, curry_1_5!(action_25)),
        132 => partial_5!(happyGoto, curry_1_5!(action_108)),
        133 => partial_5!(happyGoto, curry_1_5!(action_293)),
        134 => partial_5!(happyGoto, curry_1_5!(action_110)),
        _ => happyReduce_471()
    }
}

fn action_27(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_405()
    }
}

fn action_28(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_404()
    }
}

fn action_29(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_272)),
        144 => partial_5!(happyShift, curry_1_5!(action_27)),
        145 => partial_5!(happyShift, curry_1_5!(action_28)),
        146 => partial_5!(happyShift, curry_1_5!(action_29)),
        147 => partial_5!(happyShift, curry_1_5!(action_30)),
        148 => partial_5!(happyShift, curry_1_5!(action_31)),
        149 => partial_5!(happyShift, curry_1_5!(action_32)),
        150 => partial_5!(happyShift, curry_1_5!(action_33)),
        153 => partial_5!(happyShift, curry_1_5!(action_34)),
        164 => partial_5!(happyShift, curry_1_5!(action_35)),
        184 => partial_5!(happyShift, curry_1_5!(action_36)),
        204 => partial_5!(happyShift, curry_1_5!(action_37)),
        220 => partial_5!(happyShift, curry_1_5!(action_38)),
        233 => partial_5!(happyShift, curry_1_5!(action_39)),
        234 => partial_5!(happyShift, curry_1_5!(action_40)),
        235 => partial_5!(happyShift, curry_1_5!(action_41)),
        236 => partial_5!(happyShift, curry_1_5!(action_42)),
        237 => partial_5!(happyShift, curry_1_5!(action_43)),
        240 => partial_5!(happyShift, curry_1_5!(action_44)),
        241 => partial_5!(happyShift, curry_1_5!(action_45)),
        242 => partial_5!(happyShift, curry_1_5!(action_46)),
        243 => partial_5!(happyShift, curry_1_5!(action_47)),
        244 => partial_5!(happyShift, curry_1_5!(action_48)),
        245 => partial_5!(happyShift, curry_1_5!(action_49)),
        100 => partial_5!(happyGoto, curry_1_5!(action_6)),
        104 => partial_5!(happyGoto, curry_1_5!(action_7)),
        106 => partial_5!(happyGoto, curry_1_5!(action_284)),
        107 => partial_5!(happyGoto, curry_1_5!(action_9)),
        127 => partial_5!(happyGoto, curry_1_5!(action_24)),
        128 => partial_5!(happyGoto, curry_1_5!(action_25)),
        _ => box happyFail
    }
}

fn action_30(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_272)),
        144 => partial_5!(happyShift, curry_1_5!(action_27)),
        145 => partial_5!(happyShift, curry_1_5!(action_28)),
        146 => partial_5!(happyShift, curry_1_5!(action_29)),
        147 => partial_5!(happyShift, curry_1_5!(action_30)),
        148 => partial_5!(happyShift, curry_1_5!(action_31)),
        149 => partial_5!(happyShift, curry_1_5!(action_32)),
        150 => partial_5!(happyShift, curry_1_5!(action_33)),
        153 => partial_5!(happyShift, curry_1_5!(action_34)),
        164 => partial_5!(happyShift, curry_1_5!(action_35)),
        184 => partial_5!(happyShift, curry_1_5!(action_36)),
        204 => partial_5!(happyShift, curry_1_5!(action_37)),
        220 => partial_5!(happyShift, curry_1_5!(action_38)),
        233 => partial_5!(happyShift, curry_1_5!(action_39)),
        234 => partial_5!(happyShift, curry_1_5!(action_40)),
        235 => partial_5!(happyShift, curry_1_5!(action_41)),
        236 => partial_5!(happyShift, curry_1_5!(action_42)),
        237 => partial_5!(happyShift, curry_1_5!(action_43)),
        240 => partial_5!(happyShift, curry_1_5!(action_44)),
        241 => partial_5!(happyShift, curry_1_5!(action_45)),
        242 => partial_5!(happyShift, curry_1_5!(action_46)),
        243 => partial_5!(happyShift, curry_1_5!(action_47)),
        244 => partial_5!(happyShift, curry_1_5!(action_48)),
        245 => partial_5!(happyShift, curry_1_5!(action_49)),
        100 => partial_5!(happyGoto, curry_1_5!(action_6)),
        104 => partial_5!(happyGoto, curry_1_5!(action_7)),
        106 => partial_5!(happyGoto, curry_1_5!(action_283)),
        107 => partial_5!(happyGoto, curry_1_5!(action_9)),
        127 => partial_5!(happyGoto, curry_1_5!(action_24)),
        128 => partial_5!(happyGoto, curry_1_5!(action_25)),
        _ => box happyFail
    }
}

fn action_31(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_402()
    }
}

fn action_32(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_403()
    }
}

fn action_33(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_401()
    }
}

fn action_34(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_400()
    }
}

fn action_35(i: isize) -> ActionReturn {
    match i {
        237 => partial_5!(happyShift, curry_1_5!(action_254)),
        238 => partial_5!(happyShift, curry_1_5!(action_75)),
        131 => partial_5!(happyGoto, curry_1_5!(action_282)),
        _ => box happyFail
    }
}

fn action_36(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_281)),
        144 => partial_5!(happyShift, curry_1_5!(action_27)),
        145 => partial_5!(happyShift, curry_1_5!(action_28)),
        146 => partial_5!(happyShift, curry_1_5!(action_29)),
        147 => partial_5!(happyShift, curry_1_5!(action_30)),
        148 => partial_5!(happyShift, curry_1_5!(action_31)),
        149 => partial_5!(happyShift, curry_1_5!(action_32)),
        150 => partial_5!(happyShift, curry_1_5!(action_33)),
        153 => partial_5!(happyShift, curry_1_5!(action_34)),
        164 => partial_5!(happyShift, curry_1_5!(action_35)),
        184 => partial_5!(happyShift, curry_1_5!(action_36)),
        204 => partial_5!(happyShift, curry_1_5!(action_37)),
        220 => partial_5!(happyShift, curry_1_5!(action_38)),
        233 => partial_5!(happyShift, curry_1_5!(action_39)),
        234 => partial_5!(happyShift, curry_1_5!(action_40)),
        235 => partial_5!(happyShift, curry_1_5!(action_41)),
        236 => partial_5!(happyShift, curry_1_5!(action_42)),
        237 => partial_5!(happyShift, curry_1_5!(action_43)),
        240 => partial_5!(happyShift, curry_1_5!(action_44)),
        241 => partial_5!(happyShift, curry_1_5!(action_45)),
        242 => partial_5!(happyShift, curry_1_5!(action_46)),
        243 => partial_5!(happyShift, curry_1_5!(action_47)),
        244 => partial_5!(happyShift, curry_1_5!(action_48)),
        245 => partial_5!(happyShift, curry_1_5!(action_49)),
        100 => partial_5!(happyGoto, curry_1_5!(action_6)),
        104 => partial_5!(happyGoto, curry_1_5!(action_7)),
        106 => partial_5!(happyGoto, curry_1_5!(action_280)),
        107 => partial_5!(happyGoto, curry_1_5!(action_9)),
        127 => partial_5!(happyGoto, curry_1_5!(action_24)),
        128 => partial_5!(happyGoto, curry_1_5!(action_25)),
        _ => box happyFail
    }
}

fn action_37(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_279)),
        _ => box happyFail
    }
}

fn action_38(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_278)),
        144 => partial_5!(happyShift, curry_1_5!(action_27)),
        145 => partial_5!(happyShift, curry_1_5!(action_28)),
        146 => partial_5!(happyShift, curry_1_5!(action_29)),
        147 => partial_5!(happyShift, curry_1_5!(action_30)),
        148 => partial_5!(happyShift, curry_1_5!(action_31)),
        149 => partial_5!(happyShift, curry_1_5!(action_32)),
        150 => partial_5!(happyShift, curry_1_5!(action_33)),
        153 => partial_5!(happyShift, curry_1_5!(action_34)),
        164 => partial_5!(happyShift, curry_1_5!(action_35)),
        184 => partial_5!(happyShift, curry_1_5!(action_36)),
        204 => partial_5!(happyShift, curry_1_5!(action_37)),
        220 => partial_5!(happyShift, curry_1_5!(action_38)),
        233 => partial_5!(happyShift, curry_1_5!(action_39)),
        234 => partial_5!(happyShift, curry_1_5!(action_40)),
        235 => partial_5!(happyShift, curry_1_5!(action_41)),
        236 => partial_5!(happyShift, curry_1_5!(action_42)),
        237 => partial_5!(happyShift, curry_1_5!(action_43)),
        240 => partial_5!(happyShift, curry_1_5!(action_44)),
        241 => partial_5!(happyShift, curry_1_5!(action_45)),
        242 => partial_5!(happyShift, curry_1_5!(action_46)),
        243 => partial_5!(happyShift, curry_1_5!(action_47)),
        244 => partial_5!(happyShift, curry_1_5!(action_48)),
        245 => partial_5!(happyShift, curry_1_5!(action_49)),
        100 => partial_5!(happyGoto, curry_1_5!(action_6)),
        104 => partial_5!(happyGoto, curry_1_5!(action_7)),
        106 => partial_5!(happyGoto, curry_1_5!(action_277)),
        107 => partial_5!(happyGoto, curry_1_5!(action_9)),
        127 => partial_5!(happyGoto, curry_1_5!(action_24)),
        128 => partial_5!(happyGoto, curry_1_5!(action_25)),
        _ => box happyFail
    }
}

fn action_39(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_462()
    }
}

fn action_40(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_461()
    }
}

fn action_41(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_463()
    }
}

fn action_42(i: isize) -> ActionReturn {
    match i {
        236 => partial_5!(happyShift, curry_1_5!(action_276)),
        129 => partial_5!(happyGoto, curry_1_5!(action_275)),
        _ => happyReduce_464()
    }
}

fn action_43(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_360()
    }
}

fn action_44(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_26)),
        144 => partial_5!(happyShift, curry_1_5!(action_27)),
        145 => partial_5!(happyShift, curry_1_5!(action_28)),
        146 => partial_5!(happyShift, curry_1_5!(action_29)),
        147 => partial_5!(happyShift, curry_1_5!(action_30)),
        148 => partial_5!(happyShift, curry_1_5!(action_31)),
        149 => partial_5!(happyShift, curry_1_5!(action_32)),
        150 => partial_5!(happyShift, curry_1_5!(action_33)),
        153 => partial_5!(happyShift, curry_1_5!(action_34)),
        164 => partial_5!(happyShift, curry_1_5!(action_35)),
        184 => partial_5!(happyShift, curry_1_5!(action_36)),
        204 => partial_5!(happyShift, curry_1_5!(action_37)),
        220 => partial_5!(happyShift, curry_1_5!(action_38)),
        233 => partial_5!(happyShift, curry_1_5!(action_39)),
        234 => partial_5!(happyShift, curry_1_5!(action_40)),
        235 => partial_5!(happyShift, curry_1_5!(action_41)),
        236 => partial_5!(happyShift, curry_1_5!(action_42)),
        237 => partial_5!(happyShift, curry_1_5!(action_43)),
        240 => partial_5!(happyShift, curry_1_5!(action_44)),
        241 => partial_5!(happyShift, curry_1_5!(action_45)),
        242 => partial_5!(happyShift, curry_1_5!(action_46)),
        243 => partial_5!(happyShift, curry_1_5!(action_47)),
        244 => partial_5!(happyShift, curry_1_5!(action_48)),
        245 => partial_5!(happyShift, curry_1_5!(action_49)),
        100 => partial_5!(happyGoto, curry_1_5!(action_6)),
        104 => partial_5!(happyGoto, curry_1_5!(action_7)),
        106 => partial_5!(happyGoto, curry_1_5!(action_259)),
        107 => partial_5!(happyGoto, curry_1_5!(action_9)),
        108 => partial_5!(happyGoto, curry_1_5!(action_274)),
        127 => partial_5!(happyGoto, curry_1_5!(action_24)),
        128 => partial_5!(happyGoto, curry_1_5!(action_25)),
        _ => box happyFail
    }
}

fn action_45(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_272)),
        144 => partial_5!(happyShift, curry_1_5!(action_27)),
        145 => partial_5!(happyShift, curry_1_5!(action_28)),
        146 => partial_5!(happyShift, curry_1_5!(action_29)),
        147 => partial_5!(happyShift, curry_1_5!(action_30)),
        148 => partial_5!(happyShift, curry_1_5!(action_31)),
        149 => partial_5!(happyShift, curry_1_5!(action_32)),
        150 => partial_5!(happyShift, curry_1_5!(action_33)),
        153 => partial_5!(happyShift, curry_1_5!(action_34)),
        164 => partial_5!(happyShift, curry_1_5!(action_35)),
        184 => partial_5!(happyShift, curry_1_5!(action_36)),
        204 => partial_5!(happyShift, curry_1_5!(action_37)),
        220 => partial_5!(happyShift, curry_1_5!(action_38)),
        233 => partial_5!(happyShift, curry_1_5!(action_39)),
        234 => partial_5!(happyShift, curry_1_5!(action_40)),
        235 => partial_5!(happyShift, curry_1_5!(action_41)),
        236 => partial_5!(happyShift, curry_1_5!(action_42)),
        237 => partial_5!(happyShift, curry_1_5!(action_43)),
        240 => partial_5!(happyShift, curry_1_5!(action_44)),
        241 => partial_5!(happyShift, curry_1_5!(action_45)),
        242 => partial_5!(happyShift, curry_1_5!(action_46)),
        243 => partial_5!(happyShift, curry_1_5!(action_47)),
        244 => partial_5!(happyShift, curry_1_5!(action_48)),
        245 => partial_5!(happyShift, curry_1_5!(action_49)),
        100 => partial_5!(happyGoto, curry_1_5!(action_6)),
        104 => partial_5!(happyGoto, curry_1_5!(action_7)),
        106 => partial_5!(happyGoto, curry_1_5!(action_273)),
        107 => partial_5!(happyGoto, curry_1_5!(action_9)),
        127 => partial_5!(happyGoto, curry_1_5!(action_24)),
        128 => partial_5!(happyGoto, curry_1_5!(action_25)),
        _ => box happyFail
    }
}

fn action_46(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_272)),
        144 => partial_5!(happyShift, curry_1_5!(action_27)),
        145 => partial_5!(happyShift, curry_1_5!(action_28)),
        146 => partial_5!(happyShift, curry_1_5!(action_29)),
        147 => partial_5!(happyShift, curry_1_5!(action_30)),
        148 => partial_5!(happyShift, curry_1_5!(action_31)),
        149 => partial_5!(happyShift, curry_1_5!(action_32)),
        150 => partial_5!(happyShift, curry_1_5!(action_33)),
        153 => partial_5!(happyShift, curry_1_5!(action_34)),
        164 => partial_5!(happyShift, curry_1_5!(action_35)),
        184 => partial_5!(happyShift, curry_1_5!(action_36)),
        204 => partial_5!(happyShift, curry_1_5!(action_37)),
        220 => partial_5!(happyShift, curry_1_5!(action_38)),
        233 => partial_5!(happyShift, curry_1_5!(action_39)),
        234 => partial_5!(happyShift, curry_1_5!(action_40)),
        235 => partial_5!(happyShift, curry_1_5!(action_41)),
        236 => partial_5!(happyShift, curry_1_5!(action_42)),
        237 => partial_5!(happyShift, curry_1_5!(action_43)),
        240 => partial_5!(happyShift, curry_1_5!(action_44)),
        241 => partial_5!(happyShift, curry_1_5!(action_45)),
        242 => partial_5!(happyShift, curry_1_5!(action_46)),
        243 => partial_5!(happyShift, curry_1_5!(action_47)),
        244 => partial_5!(happyShift, curry_1_5!(action_48)),
        245 => partial_5!(happyShift, curry_1_5!(action_49)),
        100 => partial_5!(happyGoto, curry_1_5!(action_6)),
        104 => partial_5!(happyGoto, curry_1_5!(action_7)),
        106 => partial_5!(happyGoto, curry_1_5!(action_271)),
        107 => partial_5!(happyGoto, curry_1_5!(action_9)),
        127 => partial_5!(happyGoto, curry_1_5!(action_24)),
        128 => partial_5!(happyGoto, curry_1_5!(action_25)),
        _ => box happyFail
    }
}

fn action_47(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_270)),
        _ => box happyFail
    }
}

fn action_48(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_269)),
        _ => box happyFail
    }
}

fn action_49(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_268)),
        _ => box happyFail
    }
}

fn action_50(i: isize) -> ActionReturn {
    match i {
        247 => box happyAccept,
        _ => box happyFail
    }
}

fn action_51(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_27()
    }
}

fn action_52(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_28()
    }
}

fn action_53(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_29()
    }
}

fn action_54(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_30()
    }
}

fn action_55(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_31()
    }
}

fn action_56(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_32()
    }
}

fn action_57(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_33()
    }
}

fn action_58(i: isize) -> ActionReturn {
    match i {
        180 => partial_5!(happyShift, curry_1_5!(action_267)),
        _ => box happyFail
    }
}

fn action_59(i: isize) -> ActionReturn {
    match i {
        167 => partial_5!(happyShift, curry_1_5!(action_266)),
        _ => box happyFail
    }
}

fn action_60(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_56()
    }
}

fn action_61(i: isize) -> ActionReturn {
    match i {
        15 => partial_5!(happyGoto, curry_1_5!(action_265)),
        _ => happyReduce_40()
    }
}

fn action_62(i: isize) -> ActionReturn {
    match i {
        186 => partial_5!(happyShift, curry_1_5!(action_171)),
        193 => partial_5!(happyShift, curry_1_5!(action_172)),
        213 => partial_5!(happyShift, curry_1_5!(action_173)),
        214 => partial_5!(happyShift, curry_1_5!(action_174)),
        216 => partial_5!(happyShift, curry_1_5!(action_175)),
        231 => partial_5!(happyShift, curry_1_5!(action_176)),
        27 => partial_5!(happyGoto, curry_1_5!(action_263)),
        64 => partial_5!(happyGoto, curry_1_5!(action_264)),
        _ => happyReduce_74()
    }
}

fn action_63(i: isize) -> ActionReturn {
    match i {
        180 => partial_5!(happyShift, curry_1_5!(action_262)),
        _ => box happyFail
    }
}

fn action_64(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_26)),
        144 => partial_5!(happyShift, curry_1_5!(action_27)),
        145 => partial_5!(happyShift, curry_1_5!(action_28)),
        146 => partial_5!(happyShift, curry_1_5!(action_29)),
        147 => partial_5!(happyShift, curry_1_5!(action_30)),
        148 => partial_5!(happyShift, curry_1_5!(action_31)),
        149 => partial_5!(happyShift, curry_1_5!(action_32)),
        150 => partial_5!(happyShift, curry_1_5!(action_33)),
        153 => partial_5!(happyShift, curry_1_5!(action_34)),
        164 => partial_5!(happyShift, curry_1_5!(action_35)),
        184 => partial_5!(happyShift, curry_1_5!(action_36)),
        204 => partial_5!(happyShift, curry_1_5!(action_37)),
        220 => partial_5!(happyShift, curry_1_5!(action_38)),
        233 => partial_5!(happyShift, curry_1_5!(action_39)),
        234 => partial_5!(happyShift, curry_1_5!(action_40)),
        235 => partial_5!(happyShift, curry_1_5!(action_41)),
        236 => partial_5!(happyShift, curry_1_5!(action_42)),
        237 => partial_5!(happyShift, curry_1_5!(action_43)),
        240 => partial_5!(happyShift, curry_1_5!(action_44)),
        241 => partial_5!(happyShift, curry_1_5!(action_45)),
        242 => partial_5!(happyShift, curry_1_5!(action_46)),
        243 => partial_5!(happyShift, curry_1_5!(action_47)),
        244 => partial_5!(happyShift, curry_1_5!(action_48)),
        245 => partial_5!(happyShift, curry_1_5!(action_49)),
        100 => partial_5!(happyGoto, curry_1_5!(action_6)),
        104 => partial_5!(happyGoto, curry_1_5!(action_7)),
        106 => partial_5!(happyGoto, curry_1_5!(action_259)),
        107 => partial_5!(happyGoto, curry_1_5!(action_9)),
        108 => partial_5!(happyGoto, curry_1_5!(action_10)),
        109 => partial_5!(happyGoto, curry_1_5!(action_11)),
        110 => partial_5!(happyGoto, curry_1_5!(action_12)),
        111 => partial_5!(happyGoto, curry_1_5!(action_13)),
        112 => partial_5!(happyGoto, curry_1_5!(action_14)),
        113 => partial_5!(happyGoto, curry_1_5!(action_15)),
        114 => partial_5!(happyGoto, curry_1_5!(action_16)),
        115 => partial_5!(happyGoto, curry_1_5!(action_17)),
        116 => partial_5!(happyGoto, curry_1_5!(action_18)),
        117 => partial_5!(happyGoto, curry_1_5!(action_19)),
        118 => partial_5!(happyGoto, curry_1_5!(action_20)),
        119 => partial_5!(happyGoto, curry_1_5!(action_260)),
        126 => partial_5!(happyGoto, curry_1_5!(action_261)),
        127 => partial_5!(happyGoto, curry_1_5!(action_24)),
        128 => partial_5!(happyGoto, curry_1_5!(action_25)),
        _ => box happyFail
    }
}

fn action_65(i: isize) -> ActionReturn {
    match i {
        180 => partial_5!(happyShift, curry_1_5!(action_258)),
        _ => box happyFail
    }
}

fn action_66(i: isize) -> ActionReturn {
    match i {
        167 => partial_5!(happyShift, curry_1_5!(action_257)),
        _ => box happyFail
    }
}

fn action_67(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_26)),
        144 => partial_5!(happyShift, curry_1_5!(action_27)),
        145 => partial_5!(happyShift, curry_1_5!(action_28)),
        146 => partial_5!(happyShift, curry_1_5!(action_29)),
        147 => partial_5!(happyShift, curry_1_5!(action_30)),
        148 => partial_5!(happyShift, curry_1_5!(action_31)),
        149 => partial_5!(happyShift, curry_1_5!(action_32)),
        150 => partial_5!(happyShift, curry_1_5!(action_33)),
        153 => partial_5!(happyShift, curry_1_5!(action_34)),
        164 => partial_5!(happyShift, curry_1_5!(action_35)),
        180 => partial_5!(happyShift, curry_1_5!(action_60)),
        181 => partial_5!(happyShift, curry_1_5!(action_61)),
        184 => partial_5!(happyShift, curry_1_5!(action_36)),
        187 => partial_5!(happyShift, curry_1_5!(action_62)),
        189 => partial_5!(happyShift, curry_1_5!(action_63)),
        191 => partial_5!(happyShift, curry_1_5!(action_64)),
        194 => partial_5!(happyShift, curry_1_5!(action_65)),
        196 => partial_5!(happyShift, curry_1_5!(action_66)),
        197 => partial_5!(happyShift, curry_1_5!(action_67)),
        203 => partial_5!(happyShift, curry_1_5!(action_68)),
        204 => partial_5!(happyShift, curry_1_5!(action_37)),
        205 => partial_5!(happyShift, curry_1_5!(action_69)),
        206 => partial_5!(happyShift, curry_1_5!(action_70)),
        217 => partial_5!(happyShift, curry_1_5!(action_71)),
        220 => partial_5!(happyShift, curry_1_5!(action_38)),
        224 => partial_5!(happyShift, curry_1_5!(action_72)),
        232 => partial_5!(happyShift, curry_1_5!(action_73)),
        233 => partial_5!(happyShift, curry_1_5!(action_39)),
        234 => partial_5!(happyShift, curry_1_5!(action_40)),
        235 => partial_5!(happyShift, curry_1_5!(action_41)),
        236 => partial_5!(happyShift, curry_1_5!(action_42)),
        237 => partial_5!(happyShift, curry_1_5!(action_74)),
        238 => partial_5!(happyShift, curry_1_5!(action_75)),
        240 => partial_5!(happyShift, curry_1_5!(action_44)),
        241 => partial_5!(happyShift, curry_1_5!(action_45)),
        242 => partial_5!(happyShift, curry_1_5!(action_46)),
        243 => partial_5!(happyShift, curry_1_5!(action_47)),
        244 => partial_5!(happyShift, curry_1_5!(action_48)),
        245 => partial_5!(happyShift, curry_1_5!(action_49)),
        12 => partial_5!(happyGoto, curry_1_5!(action_256)),
        13 => partial_5!(happyGoto, curry_1_5!(action_51)),
        14 => partial_5!(happyGoto, curry_1_5!(action_52)),
        22 => partial_5!(happyGoto, curry_1_5!(action_53)),
        23 => partial_5!(happyGoto, curry_1_5!(action_54)),
        24 => partial_5!(happyGoto, curry_1_5!(action_55)),
        25 => partial_5!(happyGoto, curry_1_5!(action_56)),
        26 => partial_5!(happyGoto, curry_1_5!(action_57)),
        100 => partial_5!(happyGoto, curry_1_5!(action_6)),
        104 => partial_5!(happyGoto, curry_1_5!(action_7)),
        106 => partial_5!(happyGoto, curry_1_5!(action_8)),
        107 => partial_5!(happyGoto, curry_1_5!(action_9)),
        108 => partial_5!(happyGoto, curry_1_5!(action_10)),
        109 => partial_5!(happyGoto, curry_1_5!(action_11)),
        110 => partial_5!(happyGoto, curry_1_5!(action_12)),
        111 => partial_5!(happyGoto, curry_1_5!(action_13)),
        112 => partial_5!(happyGoto, curry_1_5!(action_14)),
        113 => partial_5!(happyGoto, curry_1_5!(action_15)),
        114 => partial_5!(happyGoto, curry_1_5!(action_16)),
        115 => partial_5!(happyGoto, curry_1_5!(action_17)),
        116 => partial_5!(happyGoto, curry_1_5!(action_18)),
        117 => partial_5!(happyGoto, curry_1_5!(action_19)),
        118 => partial_5!(happyGoto, curry_1_5!(action_20)),
        119 => partial_5!(happyGoto, curry_1_5!(action_21)),
        120 => partial_5!(happyGoto, curry_1_5!(action_22)),
        122 => partial_5!(happyGoto, curry_1_5!(action_58)),
        127 => partial_5!(happyGoto, curry_1_5!(action_24)),
        128 => partial_5!(happyGoto, curry_1_5!(action_25)),
        131 => partial_5!(happyGoto, curry_1_5!(action_59)),
        _ => box happyFail
    }
}

fn action_68(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_255)),
        _ => box happyFail
    }
}

fn action_69(i: isize) -> ActionReturn {
    match i {
        150 => partial_5!(happyShift, curry_1_5!(action_253)),
        237 => partial_5!(happyShift, curry_1_5!(action_254)),
        238 => partial_5!(happyShift, curry_1_5!(action_75)),
        131 => partial_5!(happyGoto, curry_1_5!(action_252)),
        _ => box happyFail
    }
}

fn action_70(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_251)),
        _ => box happyFail
    }
}

fn action_71(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_26)),
        144 => partial_5!(happyShift, curry_1_5!(action_27)),
        145 => partial_5!(happyShift, curry_1_5!(action_28)),
        146 => partial_5!(happyShift, curry_1_5!(action_29)),
        147 => partial_5!(happyShift, curry_1_5!(action_30)),
        148 => partial_5!(happyShift, curry_1_5!(action_31)),
        149 => partial_5!(happyShift, curry_1_5!(action_32)),
        150 => partial_5!(happyShift, curry_1_5!(action_33)),
        153 => partial_5!(happyShift, curry_1_5!(action_34)),
        164 => partial_5!(happyShift, curry_1_5!(action_35)),
        184 => partial_5!(happyShift, curry_1_5!(action_36)),
        204 => partial_5!(happyShift, curry_1_5!(action_37)),
        220 => partial_5!(happyShift, curry_1_5!(action_38)),
        233 => partial_5!(happyShift, curry_1_5!(action_39)),
        234 => partial_5!(happyShift, curry_1_5!(action_40)),
        235 => partial_5!(happyShift, curry_1_5!(action_41)),
        236 => partial_5!(happyShift, curry_1_5!(action_42)),
        237 => partial_5!(happyShift, curry_1_5!(action_43)),
        240 => partial_5!(happyShift, curry_1_5!(action_44)),
        241 => partial_5!(happyShift, curry_1_5!(action_45)),
        242 => partial_5!(happyShift, curry_1_5!(action_46)),
        243 => partial_5!(happyShift, curry_1_5!(action_47)),
        244 => partial_5!(happyShift, curry_1_5!(action_48)),
        245 => partial_5!(happyShift, curry_1_5!(action_49)),
        100 => partial_5!(happyGoto, curry_1_5!(action_6)),
        104 => partial_5!(happyGoto, curry_1_5!(action_7)),
        106 => partial_5!(happyGoto, curry_1_5!(action_8)),
        107 => partial_5!(happyGoto, curry_1_5!(action_9)),
        108 => partial_5!(happyGoto, curry_1_5!(action_10)),
        109 => partial_5!(happyGoto, curry_1_5!(action_11)),
        110 => partial_5!(happyGoto, curry_1_5!(action_12)),
        111 => partial_5!(happyGoto, curry_1_5!(action_13)),
        112 => partial_5!(happyGoto, curry_1_5!(action_14)),
        113 => partial_5!(happyGoto, curry_1_5!(action_15)),
        114 => partial_5!(happyGoto, curry_1_5!(action_16)),
        115 => partial_5!(happyGoto, curry_1_5!(action_17)),
        116 => partial_5!(happyGoto, curry_1_5!(action_18)),
        117 => partial_5!(happyGoto, curry_1_5!(action_19)),
        118 => partial_5!(happyGoto, curry_1_5!(action_20)),
        119 => partial_5!(happyGoto, curry_1_5!(action_21)),
        120 => partial_5!(happyGoto, curry_1_5!(action_22)),
        122 => partial_5!(happyGoto, curry_1_5!(action_249)),
        124 => partial_5!(happyGoto, curry_1_5!(action_250)),
        127 => partial_5!(happyGoto, curry_1_5!(action_24)),
        128 => partial_5!(happyGoto, curry_1_5!(action_25)),
        _ => happyReduce_456()
    }
}

fn action_72(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_248)),
        _ => box happyFail
    }
}

fn action_73(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_247)),
        _ => box happyFail
    }
}

fn action_74(i: isize) -> ActionReturn {
    match i {
        167 => happyReduce_469(),
        _ => happyReduce_360()
    }
}

fn action_75(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_470()
    }
}

fn action_76(i: isize) -> ActionReturn {
    match i {
        247 => box happyAccept,
        _ => box happyFail
    }
}

fn action_77(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_8()
    }
}

fn action_78(i: isize) -> ActionReturn {
    match i {
        181 => partial_5!(happyShift, curry_1_5!(action_61)),
        14 => partial_5!(happyGoto, curry_1_5!(action_246)),
        _ => box happyFail
    }
}

fn action_79(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_9()
    }
}

fn action_80(i: isize) -> ActionReturn {
    match i {
        179 => partial_5!(happyShift, curry_1_5!(action_244)),
        180 => partial_5!(happyShift, curry_1_5!(action_245)),
        _ => box happyFail
    }
}

fn action_81(i: isize) -> ActionReturn {
    match i {
        179 => partial_5!(happyShift, curry_1_5!(action_242)),
        180 => partial_5!(happyShift, curry_1_5!(action_243)),
        _ => box happyFail
    }
}

fn action_82(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_227)),
        150 => partial_5!(happyShift, curry_1_5!(action_228)),
        237 => partial_5!(happyShift, curry_1_5!(action_140)),
        238 => partial_5!(happyShift, curry_1_5!(action_229)),
        11 => partial_5!(happyGoto, curry_1_5!(action_239)),
        66 => partial_5!(happyGoto, curry_1_5!(action_240)),
        68 => partial_5!(happyGoto, curry_1_5!(action_219)),
        69 => partial_5!(happyGoto, curry_1_5!(action_220)),
        70 => partial_5!(happyGoto, curry_1_5!(action_221)),
        71 => partial_5!(happyGoto, curry_1_5!(action_222)),
        72 => partial_5!(happyGoto, curry_1_5!(action_223)),
        73 => partial_5!(happyGoto, curry_1_5!(action_224)),
        75 => partial_5!(happyGoto, curry_1_5!(action_225)),
        76 => partial_5!(happyGoto, curry_1_5!(action_102)),
        77 => partial_5!(happyGoto, curry_1_5!(action_103)),
        78 => partial_5!(happyGoto, curry_1_5!(action_104)),
        79 => partial_5!(happyGoto, curry_1_5!(action_241)),
        80 => partial_5!(happyGoto, curry_1_5!(action_106)),
        81 => partial_5!(happyGoto, curry_1_5!(action_107)),
        _ => box happyFail
    }
}

fn action_83(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_111)),
        150 => partial_5!(happyShift, curry_1_5!(action_112)),
        185 => partial_5!(happyShift, curry_1_5!(action_113)),
        186 => partial_5!(happyShift, curry_1_5!(action_171)),
        188 => partial_5!(happyShift, curry_1_5!(action_115)),
        190 => partial_5!(happyShift, curry_1_5!(action_116)),
        192 => partial_5!(happyShift, curry_1_5!(action_117)),
        193 => partial_5!(happyShift, curry_1_5!(action_172)),
        195 => partial_5!(happyShift, curry_1_5!(action_118)),
        198 => partial_5!(happyShift, curry_1_5!(action_119)),
        200 => partial_5!(happyShift, curry_1_5!(action_120)),
        201 => partial_5!(happyShift, curry_1_5!(action_121)),
        202 => partial_5!(happyShift, curry_1_5!(action_122)),
        207 => partial_5!(happyShift, curry_1_5!(action_123)),
        208 => partial_5!(happyShift, curry_1_5!(action_124)),
        209 => partial_5!(happyShift, curry_1_5!(action_125)),
        210 => partial_5!(happyShift, curry_1_5!(action_126)),
        212 => partial_5!(happyShift, curry_1_5!(action_127)),
        213 => partial_5!(happyShift, curry_1_5!(action_173)),
        214 => partial_5!(happyShift, curry_1_5!(action_174)),
        215 => partial_5!(happyShift, curry_1_5!(action_128)),
        216 => partial_5!(happyShift, curry_1_5!(action_175)),
        218 => partial_5!(happyShift, curry_1_5!(action_129)),
        219 => partial_5!(happyShift, curry_1_5!(action_130)),
        221 => partial_5!(happyShift, curry_1_5!(action_131)),
        223 => partial_5!(happyShift, curry_1_5!(action_133)),
        225 => partial_5!(happyShift, curry_1_5!(action_134)),
        226 => partial_5!(happyShift, curry_1_5!(action_237)),
        227 => partial_5!(happyShift, curry_1_5!(action_136)),
        228 => partial_5!(happyShift, curry_1_5!(action_137)),
        229 => partial_5!(happyShift, curry_1_5!(action_138)),
        230 => partial_5!(happyShift, curry_1_5!(action_139)),
        231 => partial_5!(happyShift, curry_1_5!(action_176)),
        237 => partial_5!(happyShift, curry_1_5!(action_140)),
        238 => partial_5!(happyShift, curry_1_5!(action_238)),
        239 => partial_5!(happyShift, curry_1_5!(action_142)),
        11 => partial_5!(happyGoto, curry_1_5!(action_230)),
        39 => partial_5!(happyGoto, curry_1_5!(action_231)),
        41 => partial_5!(happyGoto, curry_1_5!(action_198)),
        42 => partial_5!(happyGoto, curry_1_5!(action_199)),
        43 => partial_5!(happyGoto, curry_1_5!(action_200)),
        45 => partial_5!(happyGoto, curry_1_5!(action_232)),
        52 => partial_5!(happyGoto, curry_1_5!(action_233)),
        53 => partial_5!(happyGoto, curry_1_5!(action_97)),
        54 => partial_5!(happyGoto, curry_1_5!(action_98)),
        61 => partial_5!(happyGoto, curry_1_5!(action_99)),
        64 => partial_5!(happyGoto, curry_1_5!(action_201)),
        75 => partial_5!(happyGoto, curry_1_5!(action_234)),
        76 => partial_5!(happyGoto, curry_1_5!(action_102)),
        77 => partial_5!(happyGoto, curry_1_5!(action_103)),
        78 => partial_5!(happyGoto, curry_1_5!(action_104)),
        79 => partial_5!(happyGoto, curry_1_5!(action_235)),
        80 => partial_5!(happyGoto, curry_1_5!(action_106)),
        81 => partial_5!(happyGoto, curry_1_5!(action_107)),
        134 => partial_5!(happyGoto, curry_1_5!(action_236)),
        _ => box happyFail
    }
}

fn action_84(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_104()
    }
}

fn action_85(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_114()
    }
}

fn action_86(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_115()
    }
}

fn action_87(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_116()
    }
}

fn action_88(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_227)),
        150 => partial_5!(happyShift, curry_1_5!(action_228)),
        237 => partial_5!(happyShift, curry_1_5!(action_140)),
        238 => partial_5!(happyShift, curry_1_5!(action_229)),
        11 => partial_5!(happyGoto, curry_1_5!(action_217)),
        66 => partial_5!(happyGoto, curry_1_5!(action_218)),
        68 => partial_5!(happyGoto, curry_1_5!(action_219)),
        69 => partial_5!(happyGoto, curry_1_5!(action_220)),
        70 => partial_5!(happyGoto, curry_1_5!(action_221)),
        71 => partial_5!(happyGoto, curry_1_5!(action_222)),
        72 => partial_5!(happyGoto, curry_1_5!(action_223)),
        73 => partial_5!(happyGoto, curry_1_5!(action_224)),
        75 => partial_5!(happyGoto, curry_1_5!(action_225)),
        76 => partial_5!(happyGoto, curry_1_5!(action_102)),
        77 => partial_5!(happyGoto, curry_1_5!(action_103)),
        78 => partial_5!(happyGoto, curry_1_5!(action_104)),
        79 => partial_5!(happyGoto, curry_1_5!(action_226)),
        80 => partial_5!(happyGoto, curry_1_5!(action_106)),
        81 => partial_5!(happyGoto, curry_1_5!(action_107)),
        _ => box happyFail
    }
}

fn action_89(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_147()
    }
}

fn action_90(i: isize) -> ActionReturn {
    match i {
        185 => partial_5!(happyShift, curry_1_5!(action_113)),
        186 => partial_5!(happyShift, curry_1_5!(action_171)),
        188 => partial_5!(happyShift, curry_1_5!(action_115)),
        190 => partial_5!(happyShift, curry_1_5!(action_116)),
        192 => partial_5!(happyShift, curry_1_5!(action_117)),
        193 => partial_5!(happyShift, curry_1_5!(action_172)),
        195 => partial_5!(happyShift, curry_1_5!(action_118)),
        198 => partial_5!(happyShift, curry_1_5!(action_119)),
        201 => partial_5!(happyShift, curry_1_5!(action_121)),
        202 => partial_5!(happyShift, curry_1_5!(action_122)),
        207 => partial_5!(happyShift, curry_1_5!(action_123)),
        208 => partial_5!(happyShift, curry_1_5!(action_124)),
        209 => partial_5!(happyShift, curry_1_5!(action_125)),
        210 => partial_5!(happyShift, curry_1_5!(action_126)),
        212 => partial_5!(happyShift, curry_1_5!(action_127)),
        213 => partial_5!(happyShift, curry_1_5!(action_173)),
        214 => partial_5!(happyShift, curry_1_5!(action_174)),
        215 => partial_5!(happyShift, curry_1_5!(action_128)),
        216 => partial_5!(happyShift, curry_1_5!(action_175)),
        218 => partial_5!(happyShift, curry_1_5!(action_129)),
        219 => partial_5!(happyShift, curry_1_5!(action_130)),
        221 => partial_5!(happyShift, curry_1_5!(action_131)),
        225 => partial_5!(happyShift, curry_1_5!(action_134)),
        227 => partial_5!(happyShift, curry_1_5!(action_136)),
        229 => partial_5!(happyShift, curry_1_5!(action_138)),
        230 => partial_5!(happyShift, curry_1_5!(action_139)),
        231 => partial_5!(happyShift, curry_1_5!(action_176)),
        239 => partial_5!(happyShift, curry_1_5!(action_142)),
        39 => partial_5!(happyGoto, curry_1_5!(action_214)),
        41 => partial_5!(happyGoto, curry_1_5!(action_198)),
        42 => partial_5!(happyGoto, curry_1_5!(action_199)),
        43 => partial_5!(happyGoto, curry_1_5!(action_200)),
        45 => partial_5!(happyGoto, curry_1_5!(action_215)),
        64 => partial_5!(happyGoto, curry_1_5!(action_201)),
        134 => partial_5!(happyGoto, curry_1_5!(action_216)),
        _ => happyReduce_101()
    }
}

fn action_91(i: isize) -> ActionReturn {
    match i {
        186 => partial_5!(happyShift, curry_1_5!(action_171)),
        188 => partial_5!(happyShift, curry_1_5!(action_115)),
        190 => partial_5!(happyShift, curry_1_5!(action_116)),
        192 => partial_5!(happyShift, curry_1_5!(action_117)),
        193 => partial_5!(happyShift, curry_1_5!(action_172)),
        195 => partial_5!(happyShift, curry_1_5!(action_118)),
        198 => partial_5!(happyShift, curry_1_5!(action_119)),
        201 => partial_5!(happyShift, curry_1_5!(action_121)),
        202 => partial_5!(happyShift, curry_1_5!(action_122)),
        208 => partial_5!(happyShift, curry_1_5!(action_124)),
        209 => partial_5!(happyShift, curry_1_5!(action_125)),
        210 => partial_5!(happyShift, curry_1_5!(action_126)),
        213 => partial_5!(happyShift, curry_1_5!(action_173)),
        214 => partial_5!(happyShift, curry_1_5!(action_174)),
        215 => partial_5!(happyShift, curry_1_5!(action_128)),
        216 => partial_5!(happyShift, curry_1_5!(action_175)),
        218 => partial_5!(happyShift, curry_1_5!(action_129)),
        219 => partial_5!(happyShift, curry_1_5!(action_130)),
        221 => partial_5!(happyShift, curry_1_5!(action_131)),
        225 => partial_5!(happyShift, curry_1_5!(action_134)),
        227 => partial_5!(happyShift, curry_1_5!(action_136)),
        229 => partial_5!(happyShift, curry_1_5!(action_138)),
        230 => partial_5!(happyShift, curry_1_5!(action_139)),
        231 => partial_5!(happyShift, curry_1_5!(action_176)),
        239 => partial_5!(happyShift, curry_1_5!(action_142)),
        41 => partial_5!(happyGoto, curry_1_5!(action_210)),
        45 => partial_5!(happyGoto, curry_1_5!(action_211)),
        64 => partial_5!(happyGoto, curry_1_5!(action_212)),
        134 => partial_5!(happyGoto, curry_1_5!(action_213)),
        _ => happyReduce_127()
    }
}

fn action_92(i: isize) -> ActionReturn {
    match i {
        180 => partial_5!(happyShift, curry_1_5!(action_209)),
        185 => partial_5!(happyShift, curry_1_5!(action_113)),
        186 => partial_5!(happyShift, curry_1_5!(action_171)),
        188 => partial_5!(happyShift, curry_1_5!(action_115)),
        193 => partial_5!(happyShift, curry_1_5!(action_172)),
        201 => partial_5!(happyShift, curry_1_5!(action_121)),
        207 => partial_5!(happyShift, curry_1_5!(action_123)),
        212 => partial_5!(happyShift, curry_1_5!(action_127)),
        213 => partial_5!(happyShift, curry_1_5!(action_173)),
        214 => partial_5!(happyShift, curry_1_5!(action_174)),
        215 => partial_5!(happyShift, curry_1_5!(action_128)),
        216 => partial_5!(happyShift, curry_1_5!(action_175)),
        221 => partial_5!(happyShift, curry_1_5!(action_131)),
        225 => partial_5!(happyShift, curry_1_5!(action_134)),
        227 => partial_5!(happyShift, curry_1_5!(action_136)),
        231 => partial_5!(happyShift, curry_1_5!(action_176)),
        239 => partial_5!(happyShift, curry_1_5!(action_142)),
        39 => partial_5!(happyGoto, curry_1_5!(action_207)),
        41 => partial_5!(happyGoto, curry_1_5!(action_198)),
        42 => partial_5!(happyGoto, curry_1_5!(action_199)),
        43 => partial_5!(happyGoto, curry_1_5!(action_200)),
        64 => partial_5!(happyGoto, curry_1_5!(action_201)),
        134 => partial_5!(happyGoto, curry_1_5!(action_208)),
        _ => happyReduce_102()
    }
}

fn action_93(i: isize) -> ActionReturn {
    match i {
        180 => partial_5!(happyShift, curry_1_5!(action_206)),
        186 => partial_5!(happyShift, curry_1_5!(action_171)),
        188 => partial_5!(happyShift, curry_1_5!(action_115)),
        193 => partial_5!(happyShift, curry_1_5!(action_172)),
        201 => partial_5!(happyShift, curry_1_5!(action_121)),
        213 => partial_5!(happyShift, curry_1_5!(action_173)),
        214 => partial_5!(happyShift, curry_1_5!(action_174)),
        215 => partial_5!(happyShift, curry_1_5!(action_128)),
        216 => partial_5!(happyShift, curry_1_5!(action_175)),
        221 => partial_5!(happyShift, curry_1_5!(action_131)),
        225 => partial_5!(happyShift, curry_1_5!(action_134)),
        227 => partial_5!(happyShift, curry_1_5!(action_136)),
        231 => partial_5!(happyShift, curry_1_5!(action_176)),
        239 => partial_5!(happyShift, curry_1_5!(action_142)),
        41 => partial_5!(happyGoto, curry_1_5!(action_203)),
        64 => partial_5!(happyGoto, curry_1_5!(action_204)),
        134 => partial_5!(happyGoto, curry_1_5!(action_205)),
        _ => happyReduce_128()
    }
}

fn action_94(i: isize) -> ActionReturn {
    match i {
        185 => partial_5!(happyShift, curry_1_5!(action_113)),
        186 => partial_5!(happyShift, curry_1_5!(action_171)),
        188 => partial_5!(happyShift, curry_1_5!(action_115)),
        193 => partial_5!(happyShift, curry_1_5!(action_172)),
        201 => partial_5!(happyShift, curry_1_5!(action_121)),
        207 => partial_5!(happyShift, curry_1_5!(action_123)),
        212 => partial_5!(happyShift, curry_1_5!(action_127)),
        213 => partial_5!(happyShift, curry_1_5!(action_173)),
        214 => partial_5!(happyShift, curry_1_5!(action_174)),
        215 => partial_5!(happyShift, curry_1_5!(action_128)),
        216 => partial_5!(happyShift, curry_1_5!(action_175)),
        221 => partial_5!(happyShift, curry_1_5!(action_131)),
        225 => partial_5!(happyShift, curry_1_5!(action_134)),
        227 => partial_5!(happyShift, curry_1_5!(action_136)),
        231 => partial_5!(happyShift, curry_1_5!(action_176)),
        239 => partial_5!(happyShift, curry_1_5!(action_142)),
        39 => partial_5!(happyGoto, curry_1_5!(action_197)),
        41 => partial_5!(happyGoto, curry_1_5!(action_198)),
        42 => partial_5!(happyGoto, curry_1_5!(action_199)),
        43 => partial_5!(happyGoto, curry_1_5!(action_200)),
        64 => partial_5!(happyGoto, curry_1_5!(action_201)),
        134 => partial_5!(happyGoto, curry_1_5!(action_202)),
        _ => happyReduce_103()
    }
}

fn action_95(i: isize) -> ActionReturn {
    match i {
        186 => partial_5!(happyShift, curry_1_5!(action_171)),
        188 => partial_5!(happyShift, curry_1_5!(action_115)),
        193 => partial_5!(happyShift, curry_1_5!(action_172)),
        201 => partial_5!(happyShift, curry_1_5!(action_121)),
        213 => partial_5!(happyShift, curry_1_5!(action_173)),
        214 => partial_5!(happyShift, curry_1_5!(action_174)),
        215 => partial_5!(happyShift, curry_1_5!(action_128)),
        216 => partial_5!(happyShift, curry_1_5!(action_175)),
        221 => partial_5!(happyShift, curry_1_5!(action_131)),
        225 => partial_5!(happyShift, curry_1_5!(action_134)),
        227 => partial_5!(happyShift, curry_1_5!(action_136)),
        231 => partial_5!(happyShift, curry_1_5!(action_176)),
        239 => partial_5!(happyShift, curry_1_5!(action_142)),
        41 => partial_5!(happyGoto, curry_1_5!(action_194)),
        64 => partial_5!(happyGoto, curry_1_5!(action_195)),
        134 => partial_5!(happyGoto, curry_1_5!(action_196)),
        _ => happyReduce_129()
    }
}

fn action_96(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_158()
    }
}

fn action_97(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_184()
    }
}

fn action_98(i: isize) -> ActionReturn {
    match i {
        239 => partial_5!(happyShift, curry_1_5!(action_142)),
        132 => partial_5!(happyGoto, curry_1_5!(action_193)),
        133 => partial_5!(happyGoto, curry_1_5!(action_150)),
        134 => partial_5!(happyGoto, curry_1_5!(action_110)),
        _ => happyReduce_471()
    }
}

fn action_99(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_185()
    }
}

fn action_100(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_111)),
        150 => partial_5!(happyShift, curry_1_5!(action_112)),
        185 => partial_5!(happyShift, curry_1_5!(action_113)),
        186 => partial_5!(happyShift, curry_1_5!(action_171)),
        188 => partial_5!(happyShift, curry_1_5!(action_115)),
        190 => partial_5!(happyShift, curry_1_5!(action_116)),
        192 => partial_5!(happyShift, curry_1_5!(action_117)),
        193 => partial_5!(happyShift, curry_1_5!(action_172)),
        195 => partial_5!(happyShift, curry_1_5!(action_118)),
        198 => partial_5!(happyShift, curry_1_5!(action_119)),
        200 => partial_5!(happyShift, curry_1_5!(action_120)),
        201 => partial_5!(happyShift, curry_1_5!(action_121)),
        202 => partial_5!(happyShift, curry_1_5!(action_122)),
        207 => partial_5!(happyShift, curry_1_5!(action_123)),
        208 => partial_5!(happyShift, curry_1_5!(action_124)),
        209 => partial_5!(happyShift, curry_1_5!(action_125)),
        210 => partial_5!(happyShift, curry_1_5!(action_126)),
        212 => partial_5!(happyShift, curry_1_5!(action_127)),
        213 => partial_5!(happyShift, curry_1_5!(action_173)),
        214 => partial_5!(happyShift, curry_1_5!(action_174)),
        215 => partial_5!(happyShift, curry_1_5!(action_128)),
        216 => partial_5!(happyShift, curry_1_5!(action_175)),
        218 => partial_5!(happyShift, curry_1_5!(action_129)),
        219 => partial_5!(happyShift, curry_1_5!(action_130)),
        221 => partial_5!(happyShift, curry_1_5!(action_131)),
        223 => partial_5!(happyShift, curry_1_5!(action_133)),
        225 => partial_5!(happyShift, curry_1_5!(action_134)),
        226 => partial_5!(happyShift, curry_1_5!(action_191)),
        227 => partial_5!(happyShift, curry_1_5!(action_136)),
        228 => partial_5!(happyShift, curry_1_5!(action_137)),
        229 => partial_5!(happyShift, curry_1_5!(action_138)),
        230 => partial_5!(happyShift, curry_1_5!(action_139)),
        231 => partial_5!(happyShift, curry_1_5!(action_176)),
        237 => partial_5!(happyShift, curry_1_5!(action_140)),
        238 => partial_5!(happyShift, curry_1_5!(action_192)),
        239 => partial_5!(happyShift, curry_1_5!(action_142)),
        11 => partial_5!(happyGoto, curry_1_5!(action_183)),
        40 => partial_5!(happyGoto, curry_1_5!(action_184)),
        41 => partial_5!(happyGoto, curry_1_5!(action_85)),
        42 => partial_5!(happyGoto, curry_1_5!(action_86)),
        43 => partial_5!(happyGoto, curry_1_5!(action_87)),
        45 => partial_5!(happyGoto, curry_1_5!(action_185)),
        52 => partial_5!(happyGoto, curry_1_5!(action_186)),
        53 => partial_5!(happyGoto, curry_1_5!(action_97)),
        54 => partial_5!(happyGoto, curry_1_5!(action_98)),
        61 => partial_5!(happyGoto, curry_1_5!(action_99)),
        64 => partial_5!(happyGoto, curry_1_5!(action_187)),
        75 => partial_5!(happyGoto, curry_1_5!(action_188)),
        76 => partial_5!(happyGoto, curry_1_5!(action_102)),
        77 => partial_5!(happyGoto, curry_1_5!(action_103)),
        78 => partial_5!(happyGoto, curry_1_5!(action_104)),
        79 => partial_5!(happyGoto, curry_1_5!(action_189)),
        80 => partial_5!(happyGoto, curry_1_5!(action_106)),
        81 => partial_5!(happyGoto, curry_1_5!(action_107)),
        133 => partial_5!(happyGoto, curry_1_5!(action_190)),
        134 => partial_5!(happyGoto, curry_1_5!(action_110)),
        _ => box happyFail
    }
}

fn action_101(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_26()
    }
}

fn action_102(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_260()
    }
}

fn action_103(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_262()
    }
}

fn action_104(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_181)),
        140 => partial_5!(happyShift, curry_1_5!(action_182)),
        88 => partial_5!(happyGoto, curry_1_5!(action_178)),
        89 => partial_5!(happyGoto, curry_1_5!(action_179)),
        90 => partial_5!(happyGoto, curry_1_5!(action_180)),
        _ => happyReduce_261()
    }
}

fn action_105(i: isize) -> ActionReturn {
    match i {
        33 => partial_5!(happyGoto, curry_1_5!(action_177)),
        _ => happyReduce_90()
    }
}

fn action_106(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_275()
    }
}

fn action_107(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_276()
    }
}

fn action_108(i: isize) -> ActionReturn {
    match i {
        186 => partial_5!(happyShift, curry_1_5!(action_171)),
        193 => partial_5!(happyShift, curry_1_5!(action_172)),
        213 => partial_5!(happyShift, curry_1_5!(action_173)),
        214 => partial_5!(happyShift, curry_1_5!(action_174)),
        216 => partial_5!(happyShift, curry_1_5!(action_175)),
        231 => partial_5!(happyShift, curry_1_5!(action_176)),
        64 => partial_5!(happyGoto, curry_1_5!(action_170)),
        _ => box happyFail
    }
}

fn action_109(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_111)),
        150 => partial_5!(happyShift, curry_1_5!(action_112)),
        185 => partial_5!(happyShift, curry_1_5!(action_113)),
        188 => partial_5!(happyShift, curry_1_5!(action_115)),
        190 => partial_5!(happyShift, curry_1_5!(action_116)),
        192 => partial_5!(happyShift, curry_1_5!(action_117)),
        195 => partial_5!(happyShift, curry_1_5!(action_118)),
        198 => partial_5!(happyShift, curry_1_5!(action_119)),
        200 => partial_5!(happyShift, curry_1_5!(action_120)),
        201 => partial_5!(happyShift, curry_1_5!(action_121)),
        202 => partial_5!(happyShift, curry_1_5!(action_122)),
        207 => partial_5!(happyShift, curry_1_5!(action_123)),
        208 => partial_5!(happyShift, curry_1_5!(action_124)),
        209 => partial_5!(happyShift, curry_1_5!(action_125)),
        210 => partial_5!(happyShift, curry_1_5!(action_126)),
        212 => partial_5!(happyShift, curry_1_5!(action_127)),
        215 => partial_5!(happyShift, curry_1_5!(action_128)),
        218 => partial_5!(happyShift, curry_1_5!(action_129)),
        219 => partial_5!(happyShift, curry_1_5!(action_130)),
        221 => partial_5!(happyShift, curry_1_5!(action_131)),
        223 => partial_5!(happyShift, curry_1_5!(action_133)),
        225 => partial_5!(happyShift, curry_1_5!(action_134)),
        226 => partial_5!(happyShift, curry_1_5!(action_168)),
        227 => partial_5!(happyShift, curry_1_5!(action_136)),
        228 => partial_5!(happyShift, curry_1_5!(action_137)),
        229 => partial_5!(happyShift, curry_1_5!(action_138)),
        230 => partial_5!(happyShift, curry_1_5!(action_139)),
        237 => partial_5!(happyShift, curry_1_5!(action_140)),
        238 => partial_5!(happyShift, curry_1_5!(action_169)),
        239 => partial_5!(happyShift, curry_1_5!(action_142)),
        11 => partial_5!(happyGoto, curry_1_5!(action_161)),
        40 => partial_5!(happyGoto, curry_1_5!(action_162)),
        41 => partial_5!(happyGoto, curry_1_5!(action_85)),
        42 => partial_5!(happyGoto, curry_1_5!(action_86)),
        43 => partial_5!(happyGoto, curry_1_5!(action_87)),
        45 => partial_5!(happyGoto, curry_1_5!(action_163)),
        52 => partial_5!(happyGoto, curry_1_5!(action_164)),
        53 => partial_5!(happyGoto, curry_1_5!(action_97)),
        54 => partial_5!(happyGoto, curry_1_5!(action_98)),
        61 => partial_5!(happyGoto, curry_1_5!(action_99)),
        75 => partial_5!(happyGoto, curry_1_5!(action_165)),
        76 => partial_5!(happyGoto, curry_1_5!(action_102)),
        77 => partial_5!(happyGoto, curry_1_5!(action_103)),
        78 => partial_5!(happyGoto, curry_1_5!(action_104)),
        79 => partial_5!(happyGoto, curry_1_5!(action_166)),
        80 => partial_5!(happyGoto, curry_1_5!(action_106)),
        81 => partial_5!(happyGoto, curry_1_5!(action_107)),
        134 => partial_5!(happyGoto, curry_1_5!(action_167)),
        _ => happyReduce_472()
    }
}

fn action_110(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_473()
    }
}

fn action_111(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_111)),
        150 => partial_5!(happyShift, curry_1_5!(action_112)),
        237 => partial_5!(happyShift, curry_1_5!(action_140)),
        239 => partial_5!(happyShift, curry_1_5!(action_142)),
        76 => partial_5!(happyGoto, curry_1_5!(action_157)),
        77 => partial_5!(happyGoto, curry_1_5!(action_103)),
        78 => partial_5!(happyGoto, curry_1_5!(action_158)),
        80 => partial_5!(happyGoto, curry_1_5!(action_159)),
        81 => partial_5!(happyGoto, curry_1_5!(action_107)),
        133 => partial_5!(happyGoto, curry_1_5!(action_160)),
        134 => partial_5!(happyGoto, curry_1_5!(action_110)),
        _ => box happyFail
    }
}

fn action_112(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_111)),
        150 => partial_5!(happyShift, curry_1_5!(action_112)),
        237 => partial_5!(happyShift, curry_1_5!(action_140)),
        239 => partial_5!(happyShift, curry_1_5!(action_142)),
        65 => partial_5!(happyGoto, curry_1_5!(action_153)),
        75 => partial_5!(happyGoto, curry_1_5!(action_154)),
        76 => partial_5!(happyGoto, curry_1_5!(action_102)),
        77 => partial_5!(happyGoto, curry_1_5!(action_103)),
        78 => partial_5!(happyGoto, curry_1_5!(action_104)),
        80 => partial_5!(happyGoto, curry_1_5!(action_155)),
        81 => partial_5!(happyGoto, curry_1_5!(action_107)),
        132 => partial_5!(happyGoto, curry_1_5!(action_108)),
        133 => partial_5!(happyGoto, curry_1_5!(action_156)),
        134 => partial_5!(happyGoto, curry_1_5!(action_110)),
        _ => happyReduce_471()
    }
}

fn action_113(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_152)),
        _ => box happyFail
    }
}

fn action_114(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_151)),
        _ => box happyFail
    }
}

fn action_115(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_120()
    }
}

fn action_116(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_139()
    }
}

fn action_117(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_131()
    }
}

fn action_118(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_140()
    }
}

fn action_119(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_136()
    }
}

fn action_120(i: isize) -> ActionReturn {
    match i {
        239 => partial_5!(happyShift, curry_1_5!(action_142)),
        132 => partial_5!(happyGoto, curry_1_5!(action_149)),
        133 => partial_5!(happyGoto, curry_1_5!(action_150)),
        134 => partial_5!(happyGoto, curry_1_5!(action_110)),
        _ => happyReduce_471()
    }
}

fn action_121(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_118()
    }
}

fn action_122(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_135()
    }
}

fn action_123(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_123()
    }
}

fn action_124(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_133()
    }
}

fn action_125(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_141()
    }
}

fn action_126(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_134()
    }
}

fn action_127(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_124()
    }
}

fn action_128(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_121()
    }
}

fn action_129(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_132()
    }
}

fn action_130(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_137()
    }
}

fn action_131(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_119()
    }
}

fn action_132(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_148)),
        _ => box happyFail
    }
}

fn action_133(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_189()
    }
}

fn action_134(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_117()
    }
}

fn action_135(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_147)),
        _ => box happyFail
    }
}

fn action_136(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_122()
    }
}

fn action_137(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_190()
    }
}

fn action_138(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_138()
    }
}

fn action_139(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_130()
    }
}

fn action_140(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_272()
    }
}

fn action_141(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_170()
    }
}

fn action_142(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_146)),
        _ => box happyFail
    }
}

fn action_143(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_111)),
        150 => partial_5!(happyShift, curry_1_5!(action_112)),
        185 => partial_5!(happyShift, curry_1_5!(action_113)),
        187 => partial_5!(happyShift, curry_1_5!(action_114)),
        188 => partial_5!(happyShift, curry_1_5!(action_115)),
        190 => partial_5!(happyShift, curry_1_5!(action_116)),
        192 => partial_5!(happyShift, curry_1_5!(action_117)),
        195 => partial_5!(happyShift, curry_1_5!(action_118)),
        198 => partial_5!(happyShift, curry_1_5!(action_119)),
        200 => partial_5!(happyShift, curry_1_5!(action_120)),
        201 => partial_5!(happyShift, curry_1_5!(action_121)),
        202 => partial_5!(happyShift, curry_1_5!(action_122)),
        207 => partial_5!(happyShift, curry_1_5!(action_123)),
        208 => partial_5!(happyShift, curry_1_5!(action_124)),
        209 => partial_5!(happyShift, curry_1_5!(action_125)),
        210 => partial_5!(happyShift, curry_1_5!(action_126)),
        212 => partial_5!(happyShift, curry_1_5!(action_127)),
        215 => partial_5!(happyShift, curry_1_5!(action_128)),
        218 => partial_5!(happyShift, curry_1_5!(action_129)),
        219 => partial_5!(happyShift, curry_1_5!(action_130)),
        221 => partial_5!(happyShift, curry_1_5!(action_131)),
        222 => partial_5!(happyShift, curry_1_5!(action_132)),
        223 => partial_5!(happyShift, curry_1_5!(action_133)),
        225 => partial_5!(happyShift, curry_1_5!(action_134)),
        226 => partial_5!(happyShift, curry_1_5!(action_135)),
        227 => partial_5!(happyShift, curry_1_5!(action_136)),
        228 => partial_5!(happyShift, curry_1_5!(action_137)),
        229 => partial_5!(happyShift, curry_1_5!(action_138)),
        230 => partial_5!(happyShift, curry_1_5!(action_139)),
        237 => partial_5!(happyShift, curry_1_5!(action_140)),
        238 => partial_5!(happyShift, curry_1_5!(action_141)),
        239 => partial_5!(happyShift, curry_1_5!(action_142)),
        240 => partial_5!(happyShift, curry_1_5!(action_143)),
        9 => partial_5!(happyGoto, curry_1_5!(action_145)),
        10 => partial_5!(happyGoto, curry_1_5!(action_77)),
        11 => partial_5!(happyGoto, curry_1_5!(action_78)),
        32 => partial_5!(happyGoto, curry_1_5!(action_79)),
        34 => partial_5!(happyGoto, curry_1_5!(action_80)),
        36 => partial_5!(happyGoto, curry_1_5!(action_81)),
        37 => partial_5!(happyGoto, curry_1_5!(action_82)),
        38 => partial_5!(happyGoto, curry_1_5!(action_83)),
        40 => partial_5!(happyGoto, curry_1_5!(action_84)),
        41 => partial_5!(happyGoto, curry_1_5!(action_85)),
        42 => partial_5!(happyGoto, curry_1_5!(action_86)),
        43 => partial_5!(happyGoto, curry_1_5!(action_87)),
        44 => partial_5!(happyGoto, curry_1_5!(action_88)),
        45 => partial_5!(happyGoto, curry_1_5!(action_89)),
        46 => partial_5!(happyGoto, curry_1_5!(action_90)),
        47 => partial_5!(happyGoto, curry_1_5!(action_91)),
        48 => partial_5!(happyGoto, curry_1_5!(action_92)),
        49 => partial_5!(happyGoto, curry_1_5!(action_93)),
        50 => partial_5!(happyGoto, curry_1_5!(action_94)),
        51 => partial_5!(happyGoto, curry_1_5!(action_95)),
        52 => partial_5!(happyGoto, curry_1_5!(action_96)),
        53 => partial_5!(happyGoto, curry_1_5!(action_97)),
        54 => partial_5!(happyGoto, curry_1_5!(action_98)),
        61 => partial_5!(happyGoto, curry_1_5!(action_99)),
        65 => partial_5!(happyGoto, curry_1_5!(action_100)),
        75 => partial_5!(happyGoto, curry_1_5!(action_101)),
        76 => partial_5!(happyGoto, curry_1_5!(action_102)),
        77 => partial_5!(happyGoto, curry_1_5!(action_103)),
        78 => partial_5!(happyGoto, curry_1_5!(action_104)),
        79 => partial_5!(happyGoto, curry_1_5!(action_105)),
        80 => partial_5!(happyGoto, curry_1_5!(action_106)),
        81 => partial_5!(happyGoto, curry_1_5!(action_107)),
        132 => partial_5!(happyGoto, curry_1_5!(action_108)),
        133 => partial_5!(happyGoto, curry_1_5!(action_109)),
        134 => partial_5!(happyGoto, curry_1_5!(action_110)),
        _ => happyReduce_471()
    }
}

fn action_144(i: isize) -> ActionReturn {
    match i {
        247 => box happyAccept,
        _ => box happyFail
    }
}

fn action_145(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_10()
    }
}

fn action_146(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_493)),
        _ => box happyFail
    }
}

fn action_147(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_26)),
        144 => partial_5!(happyShift, curry_1_5!(action_27)),
        145 => partial_5!(happyShift, curry_1_5!(action_28)),
        146 => partial_5!(happyShift, curry_1_5!(action_29)),
        147 => partial_5!(happyShift, curry_1_5!(action_30)),
        148 => partial_5!(happyShift, curry_1_5!(action_31)),
        149 => partial_5!(happyShift, curry_1_5!(action_32)),
        150 => partial_5!(happyShift, curry_1_5!(action_33)),
        153 => partial_5!(happyShift, curry_1_5!(action_34)),
        164 => partial_5!(happyShift, curry_1_5!(action_35)),
        184 => partial_5!(happyShift, curry_1_5!(action_36)),
        190 => partial_5!(happyShift, curry_1_5!(action_116)),
        192 => partial_5!(happyShift, curry_1_5!(action_117)),
        195 => partial_5!(happyShift, curry_1_5!(action_118)),
        198 => partial_5!(happyShift, curry_1_5!(action_119)),
        200 => partial_5!(happyShift, curry_1_5!(action_120)),
        202 => partial_5!(happyShift, curry_1_5!(action_122)),
        204 => partial_5!(happyShift, curry_1_5!(action_37)),
        208 => partial_5!(happyShift, curry_1_5!(action_124)),
        209 => partial_5!(happyShift, curry_1_5!(action_125)),
        210 => partial_5!(happyShift, curry_1_5!(action_126)),
        218 => partial_5!(happyShift, curry_1_5!(action_129)),
        219 => partial_5!(happyShift, curry_1_5!(action_130)),
        220 => partial_5!(happyShift, curry_1_5!(action_38)),
        223 => partial_5!(happyShift, curry_1_5!(action_133)),
        226 => partial_5!(happyShift, curry_1_5!(action_135)),
        228 => partial_5!(happyShift, curry_1_5!(action_137)),
        229 => partial_5!(happyShift, curry_1_5!(action_138)),
        230 => partial_5!(happyShift, curry_1_5!(action_139)),
        233 => partial_5!(happyShift, curry_1_5!(action_39)),
        234 => partial_5!(happyShift, curry_1_5!(action_40)),
        235 => partial_5!(happyShift, curry_1_5!(action_41)),
        236 => partial_5!(happyShift, curry_1_5!(action_42)),
        237 => partial_5!(happyShift, curry_1_5!(action_43)),
        238 => partial_5!(happyShift, curry_1_5!(action_141)),
        239 => partial_5!(happyShift, curry_1_5!(action_142)),
        240 => partial_5!(happyShift, curry_1_5!(action_44)),
        241 => partial_5!(happyShift, curry_1_5!(action_45)),
        242 => partial_5!(happyShift, curry_1_5!(action_46)),
        243 => partial_5!(happyShift, curry_1_5!(action_47)),
        244 => partial_5!(happyShift, curry_1_5!(action_48)),
        245 => partial_5!(happyShift, curry_1_5!(action_49)),
        44 => partial_5!(happyGoto, curry_1_5!(action_286)),
        45 => partial_5!(happyGoto, curry_1_5!(action_89)),
        47 => partial_5!(happyGoto, curry_1_5!(action_287)),
        49 => partial_5!(happyGoto, curry_1_5!(action_288)),
        51 => partial_5!(happyGoto, curry_1_5!(action_289)),
        52 => partial_5!(happyGoto, curry_1_5!(action_96)),
        53 => partial_5!(happyGoto, curry_1_5!(action_97)),
        54 => partial_5!(happyGoto, curry_1_5!(action_98)),
        61 => partial_5!(happyGoto, curry_1_5!(action_99)),
        65 => partial_5!(happyGoto, curry_1_5!(action_290)),
        86 => partial_5!(happyGoto, curry_1_5!(action_491)),
        100 => partial_5!(happyGoto, curry_1_5!(action_6)),
        104 => partial_5!(happyGoto, curry_1_5!(action_7)),
        106 => partial_5!(happyGoto, curry_1_5!(action_8)),
        107 => partial_5!(happyGoto, curry_1_5!(action_9)),
        108 => partial_5!(happyGoto, curry_1_5!(action_10)),
        109 => partial_5!(happyGoto, curry_1_5!(action_11)),
        110 => partial_5!(happyGoto, curry_1_5!(action_12)),
        111 => partial_5!(happyGoto, curry_1_5!(action_13)),
        112 => partial_5!(happyGoto, curry_1_5!(action_14)),
        113 => partial_5!(happyGoto, curry_1_5!(action_15)),
        114 => partial_5!(happyGoto, curry_1_5!(action_16)),
        115 => partial_5!(happyGoto, curry_1_5!(action_17)),
        116 => partial_5!(happyGoto, curry_1_5!(action_18)),
        117 => partial_5!(happyGoto, curry_1_5!(action_19)),
        118 => partial_5!(happyGoto, curry_1_5!(action_20)),
        119 => partial_5!(happyGoto, curry_1_5!(action_21)),
        120 => partial_5!(happyGoto, curry_1_5!(action_22)),
        122 => partial_5!(happyGoto, curry_1_5!(action_492)),
        127 => partial_5!(happyGoto, curry_1_5!(action_24)),
        128 => partial_5!(happyGoto, curry_1_5!(action_25)),
        132 => partial_5!(happyGoto, curry_1_5!(action_108)),
        133 => partial_5!(happyGoto, curry_1_5!(action_293)),
        134 => partial_5!(happyGoto, curry_1_5!(action_110)),
        _ => happyReduce_471()
    }
}

fn action_148(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_26)),
        144 => partial_5!(happyShift, curry_1_5!(action_27)),
        145 => partial_5!(happyShift, curry_1_5!(action_28)),
        146 => partial_5!(happyShift, curry_1_5!(action_29)),
        147 => partial_5!(happyShift, curry_1_5!(action_30)),
        148 => partial_5!(happyShift, curry_1_5!(action_31)),
        149 => partial_5!(happyShift, curry_1_5!(action_32)),
        150 => partial_5!(happyShift, curry_1_5!(action_33)),
        153 => partial_5!(happyShift, curry_1_5!(action_34)),
        164 => partial_5!(happyShift, curry_1_5!(action_35)),
        184 => partial_5!(happyShift, curry_1_5!(action_36)),
        204 => partial_5!(happyShift, curry_1_5!(action_37)),
        220 => partial_5!(happyShift, curry_1_5!(action_38)),
        233 => partial_5!(happyShift, curry_1_5!(action_39)),
        234 => partial_5!(happyShift, curry_1_5!(action_40)),
        235 => partial_5!(happyShift, curry_1_5!(action_41)),
        236 => partial_5!(happyShift, curry_1_5!(action_42)),
        237 => partial_5!(happyShift, curry_1_5!(action_43)),
        240 => partial_5!(happyShift, curry_1_5!(action_44)),
        241 => partial_5!(happyShift, curry_1_5!(action_45)),
        242 => partial_5!(happyShift, curry_1_5!(action_46)),
        243 => partial_5!(happyShift, curry_1_5!(action_47)),
        244 => partial_5!(happyShift, curry_1_5!(action_48)),
        245 => partial_5!(happyShift, curry_1_5!(action_49)),
        100 => partial_5!(happyGoto, curry_1_5!(action_6)),
        104 => partial_5!(happyGoto, curry_1_5!(action_7)),
        106 => partial_5!(happyGoto, curry_1_5!(action_259)),
        107 => partial_5!(happyGoto, curry_1_5!(action_9)),
        108 => partial_5!(happyGoto, curry_1_5!(action_10)),
        109 => partial_5!(happyGoto, curry_1_5!(action_11)),
        110 => partial_5!(happyGoto, curry_1_5!(action_12)),
        111 => partial_5!(happyGoto, curry_1_5!(action_13)),
        112 => partial_5!(happyGoto, curry_1_5!(action_14)),
        113 => partial_5!(happyGoto, curry_1_5!(action_15)),
        114 => partial_5!(happyGoto, curry_1_5!(action_16)),
        115 => partial_5!(happyGoto, curry_1_5!(action_17)),
        116 => partial_5!(happyGoto, curry_1_5!(action_18)),
        117 => partial_5!(happyGoto, curry_1_5!(action_19)),
        118 => partial_5!(happyGoto, curry_1_5!(action_20)),
        119 => partial_5!(happyGoto, curry_1_5!(action_260)),
        126 => partial_5!(happyGoto, curry_1_5!(action_490)),
        127 => partial_5!(happyGoto, curry_1_5!(action_24)),
        128 => partial_5!(happyGoto, curry_1_5!(action_25)),
        _ => box happyFail
    }
}

fn action_149(i: isize) -> ActionReturn {
    match i {
        181 => partial_5!(happyShift, curry_1_5!(action_489)),
        237 => partial_5!(happyShift, curry_1_5!(action_254)),
        238 => partial_5!(happyShift, curry_1_5!(action_75)),
        131 => partial_5!(happyGoto, curry_1_5!(action_488)),
        _ => box happyFail
    }
}

fn action_150(i: isize) -> ActionReturn {
    match i {
        239 => partial_5!(happyShift, curry_1_5!(action_142)),
        134 => partial_5!(happyGoto, curry_1_5!(action_167)),
        _ => happyReduce_472()
    }
}

fn action_151(i: isize) -> ActionReturn {
    match i {
        236 => partial_5!(happyShift, curry_1_5!(action_42)),
        128 => partial_5!(happyGoto, curry_1_5!(action_487)),
        _ => box happyFail
    }
}

fn action_152(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_26)),
        144 => partial_5!(happyShift, curry_1_5!(action_27)),
        145 => partial_5!(happyShift, curry_1_5!(action_28)),
        146 => partial_5!(happyShift, curry_1_5!(action_29)),
        147 => partial_5!(happyShift, curry_1_5!(action_30)),
        148 => partial_5!(happyShift, curry_1_5!(action_31)),
        149 => partial_5!(happyShift, curry_1_5!(action_32)),
        150 => partial_5!(happyShift, curry_1_5!(action_33)),
        153 => partial_5!(happyShift, curry_1_5!(action_34)),
        164 => partial_5!(happyShift, curry_1_5!(action_35)),
        184 => partial_5!(happyShift, curry_1_5!(action_36)),
        190 => partial_5!(happyShift, curry_1_5!(action_116)),
        192 => partial_5!(happyShift, curry_1_5!(action_117)),
        195 => partial_5!(happyShift, curry_1_5!(action_118)),
        198 => partial_5!(happyShift, curry_1_5!(action_119)),
        200 => partial_5!(happyShift, curry_1_5!(action_120)),
        202 => partial_5!(happyShift, curry_1_5!(action_122)),
        204 => partial_5!(happyShift, curry_1_5!(action_37)),
        208 => partial_5!(happyShift, curry_1_5!(action_124)),
        209 => partial_5!(happyShift, curry_1_5!(action_125)),
        210 => partial_5!(happyShift, curry_1_5!(action_126)),
        218 => partial_5!(happyShift, curry_1_5!(action_129)),
        219 => partial_5!(happyShift, curry_1_5!(action_130)),
        220 => partial_5!(happyShift, curry_1_5!(action_38)),
        223 => partial_5!(happyShift, curry_1_5!(action_133)),
        226 => partial_5!(happyShift, curry_1_5!(action_135)),
        228 => partial_5!(happyShift, curry_1_5!(action_137)),
        229 => partial_5!(happyShift, curry_1_5!(action_138)),
        230 => partial_5!(happyShift, curry_1_5!(action_139)),
        233 => partial_5!(happyShift, curry_1_5!(action_39)),
        234 => partial_5!(happyShift, curry_1_5!(action_40)),
        235 => partial_5!(happyShift, curry_1_5!(action_41)),
        236 => partial_5!(happyShift, curry_1_5!(action_42)),
        237 => partial_5!(happyShift, curry_1_5!(action_43)),
        238 => partial_5!(happyShift, curry_1_5!(action_141)),
        239 => partial_5!(happyShift, curry_1_5!(action_142)),
        240 => partial_5!(happyShift, curry_1_5!(action_44)),
        241 => partial_5!(happyShift, curry_1_5!(action_45)),
        242 => partial_5!(happyShift, curry_1_5!(action_46)),
        243 => partial_5!(happyShift, curry_1_5!(action_47)),
        244 => partial_5!(happyShift, curry_1_5!(action_48)),
        245 => partial_5!(happyShift, curry_1_5!(action_49)),
        44 => partial_5!(happyGoto, curry_1_5!(action_286)),
        45 => partial_5!(happyGoto, curry_1_5!(action_89)),
        47 => partial_5!(happyGoto, curry_1_5!(action_287)),
        49 => partial_5!(happyGoto, curry_1_5!(action_288)),
        51 => partial_5!(happyGoto, curry_1_5!(action_289)),
        52 => partial_5!(happyGoto, curry_1_5!(action_96)),
        53 => partial_5!(happyGoto, curry_1_5!(action_97)),
        54 => partial_5!(happyGoto, curry_1_5!(action_98)),
        61 => partial_5!(happyGoto, curry_1_5!(action_99)),
        65 => partial_5!(happyGoto, curry_1_5!(action_290)),
        86 => partial_5!(happyGoto, curry_1_5!(action_485)),
        100 => partial_5!(happyGoto, curry_1_5!(action_6)),
        104 => partial_5!(happyGoto, curry_1_5!(action_7)),
        106 => partial_5!(happyGoto, curry_1_5!(action_259)),
        107 => partial_5!(happyGoto, curry_1_5!(action_9)),
        108 => partial_5!(happyGoto, curry_1_5!(action_10)),
        109 => partial_5!(happyGoto, curry_1_5!(action_11)),
        110 => partial_5!(happyGoto, curry_1_5!(action_12)),
        111 => partial_5!(happyGoto, curry_1_5!(action_13)),
        112 => partial_5!(happyGoto, curry_1_5!(action_14)),
        113 => partial_5!(happyGoto, curry_1_5!(action_15)),
        114 => partial_5!(happyGoto, curry_1_5!(action_16)),
        115 => partial_5!(happyGoto, curry_1_5!(action_17)),
        116 => partial_5!(happyGoto, curry_1_5!(action_18)),
        117 => partial_5!(happyGoto, curry_1_5!(action_19)),
        118 => partial_5!(happyGoto, curry_1_5!(action_20)),
        119 => partial_5!(happyGoto, curry_1_5!(action_260)),
        126 => partial_5!(happyGoto, curry_1_5!(action_486)),
        127 => partial_5!(happyGoto, curry_1_5!(action_24)),
        128 => partial_5!(happyGoto, curry_1_5!(action_25)),
        132 => partial_5!(happyGoto, curry_1_5!(action_108)),
        133 => partial_5!(happyGoto, curry_1_5!(action_293)),
        134 => partial_5!(happyGoto, curry_1_5!(action_110)),
        _ => happyReduce_471()
    }
}

fn action_153(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_111)),
        150 => partial_5!(happyShift, curry_1_5!(action_112)),
        186 => partial_5!(happyShift, curry_1_5!(action_171)),
        193 => partial_5!(happyShift, curry_1_5!(action_172)),
        213 => partial_5!(happyShift, curry_1_5!(action_173)),
        214 => partial_5!(happyShift, curry_1_5!(action_174)),
        216 => partial_5!(happyShift, curry_1_5!(action_175)),
        231 => partial_5!(happyShift, curry_1_5!(action_176)),
        237 => partial_5!(happyShift, curry_1_5!(action_140)),
        239 => partial_5!(happyShift, curry_1_5!(action_142)),
        64 => partial_5!(happyGoto, curry_1_5!(action_187)),
        75 => partial_5!(happyGoto, curry_1_5!(action_482)),
        76 => partial_5!(happyGoto, curry_1_5!(action_102)),
        77 => partial_5!(happyGoto, curry_1_5!(action_103)),
        78 => partial_5!(happyGoto, curry_1_5!(action_104)),
        80 => partial_5!(happyGoto, curry_1_5!(action_483)),
        81 => partial_5!(happyGoto, curry_1_5!(action_107)),
        133 => partial_5!(happyGoto, curry_1_5!(action_484)),
        134 => partial_5!(happyGoto, curry_1_5!(action_110)),
        _ => box happyFail
    }
}

fn action_154(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_263()
    }
}

fn action_155(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_277()
    }
}

fn action_156(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_475)),
        150 => partial_5!(happyShift, curry_1_5!(action_476)),
        237 => partial_5!(happyShift, curry_1_5!(action_140)),
        239 => partial_5!(happyShift, curry_1_5!(action_142)),
        75 => partial_5!(happyGoto, curry_1_5!(action_480)),
        76 => partial_5!(happyGoto, curry_1_5!(action_102)),
        77 => partial_5!(happyGoto, curry_1_5!(action_103)),
        78 => partial_5!(happyGoto, curry_1_5!(action_481)),
        134 => partial_5!(happyGoto, curry_1_5!(action_167)),
        _ => happyReduce_472()
    }
}

fn action_157(i: isize) -> ActionReturn {
    match i {
        139 => partial_5!(happyShift, curry_1_5!(action_479)),
        _ => box happyFail
    }
}

fn action_158(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_181)),
        139 => partial_5!(happyShift, curry_1_5!(action_478)),
        140 => partial_5!(happyShift, curry_1_5!(action_182)),
        88 => partial_5!(happyGoto, curry_1_5!(action_178)),
        89 => partial_5!(happyGoto, curry_1_5!(action_179)),
        90 => partial_5!(happyGoto, curry_1_5!(action_180)),
        _ => box happyFail
    }
}

fn action_159(i: isize) -> ActionReturn {
    match i {
        139 => partial_5!(happyShift, curry_1_5!(action_477)),
        _ => box happyFail
    }
}

fn action_160(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_475)),
        150 => partial_5!(happyShift, curry_1_5!(action_476)),
        237 => partial_5!(happyShift, curry_1_5!(action_140)),
        239 => partial_5!(happyShift, curry_1_5!(action_142)),
        76 => partial_5!(happyGoto, curry_1_5!(action_473)),
        77 => partial_5!(happyGoto, curry_1_5!(action_103)),
        78 => partial_5!(happyGoto, curry_1_5!(action_474)),
        134 => partial_5!(happyGoto, curry_1_5!(action_167)),
        _ => box happyFail
    }
}

fn action_161(i: isize) -> ActionReturn {
    match i {
        181 => partial_5!(happyShift, curry_1_5!(action_61)),
        14 => partial_5!(happyGoto, curry_1_5!(action_472)),
        _ => box happyFail
    }
}

fn action_162(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_105()
    }
}

fn action_163(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_148()
    }
}

fn action_164(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_159()
    }
}

fn action_165(i: isize) -> ActionReturn {
    match i {
        181 => happyReduce_26(),
        187 => partial_5!(happyShift, curry_1_5!(action_406)),
        35 => partial_5!(happyGoto, curry_1_5!(action_471)),
        67 => partial_5!(happyGoto, curry_1_5!(action_405)),
        _ => happyReduce_232()
    }
}

fn action_166(i: isize) -> ActionReturn {
    match i {
        33 => partial_5!(happyGoto, curry_1_5!(action_470)),
        _ => happyReduce_90()
    }
}

fn action_167(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_474()
    }
}

fn action_168(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_469)),
        _ => box happyFail
    }
}

fn action_169(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_176()
    }
}

fn action_170(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_227()
    }
}

fn action_171(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_226()
    }
}

fn action_172(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_221()
    }
}

fn action_173(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_224()
    }
}

fn action_174(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_225()
    }
}

fn action_175(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_223()
    }
}

fn action_176(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_222()
    }
}

fn action_177(i: isize) -> ActionReturn {
    match i {
        181 => partial_5!(happyShift, curry_1_5!(action_61)),
        185 => partial_5!(happyShift, curry_1_5!(action_113)),
        188 => partial_5!(happyShift, curry_1_5!(action_115)),
        190 => partial_5!(happyShift, curry_1_5!(action_116)),
        192 => partial_5!(happyShift, curry_1_5!(action_117)),
        195 => partial_5!(happyShift, curry_1_5!(action_118)),
        198 => partial_5!(happyShift, curry_1_5!(action_119)),
        200 => partial_5!(happyShift, curry_1_5!(action_120)),
        201 => partial_5!(happyShift, curry_1_5!(action_121)),
        202 => partial_5!(happyShift, curry_1_5!(action_122)),
        207 => partial_5!(happyShift, curry_1_5!(action_123)),
        208 => partial_5!(happyShift, curry_1_5!(action_124)),
        209 => partial_5!(happyShift, curry_1_5!(action_125)),
        210 => partial_5!(happyShift, curry_1_5!(action_126)),
        212 => partial_5!(happyShift, curry_1_5!(action_127)),
        215 => partial_5!(happyShift, curry_1_5!(action_128)),
        218 => partial_5!(happyShift, curry_1_5!(action_129)),
        219 => partial_5!(happyShift, curry_1_5!(action_130)),
        221 => partial_5!(happyShift, curry_1_5!(action_131)),
        222 => partial_5!(happyShift, curry_1_5!(action_132)),
        223 => partial_5!(happyShift, curry_1_5!(action_133)),
        225 => partial_5!(happyShift, curry_1_5!(action_134)),
        226 => partial_5!(happyShift, curry_1_5!(action_135)),
        227 => partial_5!(happyShift, curry_1_5!(action_136)),
        228 => partial_5!(happyShift, curry_1_5!(action_137)),
        229 => partial_5!(happyShift, curry_1_5!(action_138)),
        230 => partial_5!(happyShift, curry_1_5!(action_139)),
        238 => partial_5!(happyShift, curry_1_5!(action_141)),
        239 => partial_5!(happyShift, curry_1_5!(action_142)),
        14 => partial_5!(happyGoto, curry_1_5!(action_462)),
        32 => partial_5!(happyGoto, curry_1_5!(action_463)),
        34 => partial_5!(happyGoto, curry_1_5!(action_80)),
        36 => partial_5!(happyGoto, curry_1_5!(action_81)),
        37 => partial_5!(happyGoto, curry_1_5!(action_464)),
        38 => partial_5!(happyGoto, curry_1_5!(action_465)),
        40 => partial_5!(happyGoto, curry_1_5!(action_84)),
        41 => partial_5!(happyGoto, curry_1_5!(action_85)),
        42 => partial_5!(happyGoto, curry_1_5!(action_86)),
        43 => partial_5!(happyGoto, curry_1_5!(action_87)),
        44 => partial_5!(happyGoto, curry_1_5!(action_466)),
        45 => partial_5!(happyGoto, curry_1_5!(action_89)),
        46 => partial_5!(happyGoto, curry_1_5!(action_90)),
        47 => partial_5!(happyGoto, curry_1_5!(action_91)),
        48 => partial_5!(happyGoto, curry_1_5!(action_92)),
        49 => partial_5!(happyGoto, curry_1_5!(action_93)),
        50 => partial_5!(happyGoto, curry_1_5!(action_94)),
        51 => partial_5!(happyGoto, curry_1_5!(action_95)),
        52 => partial_5!(happyGoto, curry_1_5!(action_96)),
        53 => partial_5!(happyGoto, curry_1_5!(action_97)),
        54 => partial_5!(happyGoto, curry_1_5!(action_98)),
        61 => partial_5!(happyGoto, curry_1_5!(action_99)),
        65 => partial_5!(happyGoto, curry_1_5!(action_467)),
        132 => partial_5!(happyGoto, curry_1_5!(action_108)),
        133 => partial_5!(happyGoto, curry_1_5!(action_468)),
        134 => partial_5!(happyGoto, curry_1_5!(action_110)),
        _ => happyReduce_471()
    }
}

fn action_178(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_267()
    }
}

fn action_179(i: isize) -> ActionReturn {
    match i {
        140 => partial_5!(happyShift, curry_1_5!(action_182)),
        90 => partial_5!(happyGoto, curry_1_5!(action_461)),
        _ => happyReduce_311()
    }
}

fn action_180(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_313()
    }
}

fn action_181(i: isize) -> ActionReturn {
    match i {
        185 => partial_5!(happyShift, curry_1_5!(action_113)),
        186 => happyReduce_471(),
        188 => partial_5!(happyShift, curry_1_5!(action_115)),
        190 => partial_5!(happyShift, curry_1_5!(action_116)),
        192 => partial_5!(happyShift, curry_1_5!(action_117)),
        193 => happyReduce_471(),
        195 => partial_5!(happyShift, curry_1_5!(action_118)),
        198 => partial_5!(happyShift, curry_1_5!(action_119)),
        200 => partial_5!(happyShift, curry_1_5!(action_120)),
        201 => partial_5!(happyShift, curry_1_5!(action_121)),
        202 => partial_5!(happyShift, curry_1_5!(action_122)),
        207 => partial_5!(happyShift, curry_1_5!(action_123)),
        208 => partial_5!(happyShift, curry_1_5!(action_124)),
        209 => partial_5!(happyShift, curry_1_5!(action_125)),
        210 => partial_5!(happyShift, curry_1_5!(action_126)),
        212 => partial_5!(happyShift, curry_1_5!(action_127)),
        213 => happyReduce_471(),
        214 => happyReduce_471(),
        215 => partial_5!(happyShift, curry_1_5!(action_128)),
        216 => happyReduce_471(),
        218 => partial_5!(happyShift, curry_1_5!(action_129)),
        219 => partial_5!(happyShift, curry_1_5!(action_130)),
        221 => partial_5!(happyShift, curry_1_5!(action_131)),
        223 => partial_5!(happyShift, curry_1_5!(action_133)),
        225 => partial_5!(happyShift, curry_1_5!(action_134)),
        226 => partial_5!(happyShift, curry_1_5!(action_135)),
        227 => partial_5!(happyShift, curry_1_5!(action_136)),
        228 => partial_5!(happyShift, curry_1_5!(action_137)),
        229 => partial_5!(happyShift, curry_1_5!(action_138)),
        230 => partial_5!(happyShift, curry_1_5!(action_139)),
        231 => happyReduce_471(),
        237 => partial_5!(happyShift, curry_1_5!(action_460)),
        238 => partial_5!(happyShift, curry_1_5!(action_141)),
        239 => partial_5!(happyShift, curry_1_5!(action_142)),
        37 => partial_5!(happyGoto, curry_1_5!(action_449)),
        38 => partial_5!(happyGoto, curry_1_5!(action_450)),
        40 => partial_5!(happyGoto, curry_1_5!(action_84)),
        41 => partial_5!(happyGoto, curry_1_5!(action_85)),
        42 => partial_5!(happyGoto, curry_1_5!(action_86)),
        43 => partial_5!(happyGoto, curry_1_5!(action_87)),
        44 => partial_5!(happyGoto, curry_1_5!(action_451)),
        45 => partial_5!(happyGoto, curry_1_5!(action_89)),
        46 => partial_5!(happyGoto, curry_1_5!(action_90)),
        47 => partial_5!(happyGoto, curry_1_5!(action_91)),
        48 => partial_5!(happyGoto, curry_1_5!(action_452)),
        49 => partial_5!(happyGoto, curry_1_5!(action_453)),
        50 => partial_5!(happyGoto, curry_1_5!(action_94)),
        51 => partial_5!(happyGoto, curry_1_5!(action_95)),
        52 => partial_5!(happyGoto, curry_1_5!(action_96)),
        53 => partial_5!(happyGoto, curry_1_5!(action_97)),
        54 => partial_5!(happyGoto, curry_1_5!(action_98)),
        61 => partial_5!(happyGoto, curry_1_5!(action_99)),
        65 => partial_5!(happyGoto, curry_1_5!(action_454)),
        82 => partial_5!(happyGoto, curry_1_5!(action_455)),
        83 => partial_5!(happyGoto, curry_1_5!(action_456)),
        84 => partial_5!(happyGoto, curry_1_5!(action_457)),
        85 => partial_5!(happyGoto, curry_1_5!(action_458)),
        132 => partial_5!(happyGoto, curry_1_5!(action_108)),
        133 => partial_5!(happyGoto, curry_1_5!(action_459)),
        134 => partial_5!(happyGoto, curry_1_5!(action_110)),
        _ => happyReduce_282()
    }
}

fn action_182(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_26)),
        144 => partial_5!(happyShift, curry_1_5!(action_27)),
        145 => partial_5!(happyShift, curry_1_5!(action_28)),
        146 => partial_5!(happyShift, curry_1_5!(action_29)),
        147 => partial_5!(happyShift, curry_1_5!(action_30)),
        148 => partial_5!(happyShift, curry_1_5!(action_31)),
        149 => partial_5!(happyShift, curry_1_5!(action_32)),
        150 => partial_5!(happyShift, curry_1_5!(action_447)),
        153 => partial_5!(happyShift, curry_1_5!(action_34)),
        164 => partial_5!(happyShift, curry_1_5!(action_35)),
        184 => partial_5!(happyShift, curry_1_5!(action_36)),
        186 => happyReduce_471(),
        193 => happyReduce_471(),
        204 => partial_5!(happyShift, curry_1_5!(action_37)),
        213 => happyReduce_471(),
        214 => happyReduce_471(),
        216 => happyReduce_471(),
        220 => partial_5!(happyShift, curry_1_5!(action_38)),
        221 => partial_5!(happyShift, curry_1_5!(action_448)),
        231 => happyReduce_471(),
        233 => partial_5!(happyShift, curry_1_5!(action_39)),
        234 => partial_5!(happyShift, curry_1_5!(action_40)),
        235 => partial_5!(happyShift, curry_1_5!(action_41)),
        236 => partial_5!(happyShift, curry_1_5!(action_42)),
        237 => partial_5!(happyShift, curry_1_5!(action_43)),
        239 => partial_5!(happyShift, curry_1_5!(action_142)),
        240 => partial_5!(happyShift, curry_1_5!(action_44)),
        241 => partial_5!(happyShift, curry_1_5!(action_45)),
        242 => partial_5!(happyShift, curry_1_5!(action_46)),
        243 => partial_5!(happyShift, curry_1_5!(action_47)),
        244 => partial_5!(happyShift, curry_1_5!(action_48)),
        245 => partial_5!(happyShift, curry_1_5!(action_49)),
        65 => partial_5!(happyGoto, curry_1_5!(action_443)),
        100 => partial_5!(happyGoto, curry_1_5!(action_6)),
        104 => partial_5!(happyGoto, curry_1_5!(action_7)),
        106 => partial_5!(happyGoto, curry_1_5!(action_8)),
        107 => partial_5!(happyGoto, curry_1_5!(action_9)),
        108 => partial_5!(happyGoto, curry_1_5!(action_10)),
        109 => partial_5!(happyGoto, curry_1_5!(action_11)),
        110 => partial_5!(happyGoto, curry_1_5!(action_12)),
        111 => partial_5!(happyGoto, curry_1_5!(action_13)),
        112 => partial_5!(happyGoto, curry_1_5!(action_14)),
        113 => partial_5!(happyGoto, curry_1_5!(action_15)),
        114 => partial_5!(happyGoto, curry_1_5!(action_16)),
        115 => partial_5!(happyGoto, curry_1_5!(action_17)),
        116 => partial_5!(happyGoto, curry_1_5!(action_18)),
        117 => partial_5!(happyGoto, curry_1_5!(action_19)),
        118 => partial_5!(happyGoto, curry_1_5!(action_20)),
        119 => partial_5!(happyGoto, curry_1_5!(action_21)),
        120 => partial_5!(happyGoto, curry_1_5!(action_444)),
        125 => partial_5!(happyGoto, curry_1_5!(action_445)),
        127 => partial_5!(happyGoto, curry_1_5!(action_24)),
        128 => partial_5!(happyGoto, curry_1_5!(action_25)),
        132 => partial_5!(happyGoto, curry_1_5!(action_108)),
        133 => partial_5!(happyGoto, curry_1_5!(action_446)),
        134 => partial_5!(happyGoto, curry_1_5!(action_110)),
        _ => happyReduce_458()
    }
}

fn action_183(i: isize) -> ActionReturn {
    match i {
        181 => partial_5!(happyShift, curry_1_5!(action_61)),
        14 => partial_5!(happyGoto, curry_1_5!(action_442)),
        _ => box happyFail
    }
}

fn action_184(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_106()
    }
}

fn action_185(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_149()
    }
}

fn action_186(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_160()
    }
}

fn action_187(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_228()
    }
}

fn action_188(i: isize) -> ActionReturn {
    match i {
        181 => happyReduce_26(),
        187 => partial_5!(happyShift, curry_1_5!(action_406)),
        35 => partial_5!(happyGoto, curry_1_5!(action_441)),
        67 => partial_5!(happyGoto, curry_1_5!(action_405)),
        _ => happyReduce_232()
    }
}

fn action_189(i: isize) -> ActionReturn {
    match i {
        33 => partial_5!(happyGoto, curry_1_5!(action_440)),
        _ => happyReduce_90()
    }
}

fn action_190(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_111)),
        150 => partial_5!(happyShift, curry_1_5!(action_112)),
        185 => partial_5!(happyShift, curry_1_5!(action_113)),
        186 => partial_5!(happyShift, curry_1_5!(action_171)),
        188 => partial_5!(happyShift, curry_1_5!(action_115)),
        190 => partial_5!(happyShift, curry_1_5!(action_116)),
        192 => partial_5!(happyShift, curry_1_5!(action_117)),
        193 => partial_5!(happyShift, curry_1_5!(action_172)),
        195 => partial_5!(happyShift, curry_1_5!(action_118)),
        198 => partial_5!(happyShift, curry_1_5!(action_119)),
        200 => partial_5!(happyShift, curry_1_5!(action_120)),
        201 => partial_5!(happyShift, curry_1_5!(action_121)),
        202 => partial_5!(happyShift, curry_1_5!(action_122)),
        207 => partial_5!(happyShift, curry_1_5!(action_123)),
        208 => partial_5!(happyShift, curry_1_5!(action_124)),
        209 => partial_5!(happyShift, curry_1_5!(action_125)),
        210 => partial_5!(happyShift, curry_1_5!(action_126)),
        212 => partial_5!(happyShift, curry_1_5!(action_127)),
        213 => partial_5!(happyShift, curry_1_5!(action_173)),
        214 => partial_5!(happyShift, curry_1_5!(action_174)),
        215 => partial_5!(happyShift, curry_1_5!(action_128)),
        216 => partial_5!(happyShift, curry_1_5!(action_175)),
        218 => partial_5!(happyShift, curry_1_5!(action_129)),
        219 => partial_5!(happyShift, curry_1_5!(action_130)),
        221 => partial_5!(happyShift, curry_1_5!(action_131)),
        223 => partial_5!(happyShift, curry_1_5!(action_133)),
        225 => partial_5!(happyShift, curry_1_5!(action_134)),
        226 => partial_5!(happyShift, curry_1_5!(action_438)),
        227 => partial_5!(happyShift, curry_1_5!(action_136)),
        228 => partial_5!(happyShift, curry_1_5!(action_137)),
        229 => partial_5!(happyShift, curry_1_5!(action_138)),
        230 => partial_5!(happyShift, curry_1_5!(action_139)),
        231 => partial_5!(happyShift, curry_1_5!(action_176)),
        237 => partial_5!(happyShift, curry_1_5!(action_140)),
        238 => partial_5!(happyShift, curry_1_5!(action_439)),
        239 => partial_5!(happyShift, curry_1_5!(action_142)),
        11 => partial_5!(happyGoto, curry_1_5!(action_431)),
        40 => partial_5!(happyGoto, curry_1_5!(action_432)),
        41 => partial_5!(happyGoto, curry_1_5!(action_85)),
        42 => partial_5!(happyGoto, curry_1_5!(action_86)),
        43 => partial_5!(happyGoto, curry_1_5!(action_87)),
        45 => partial_5!(happyGoto, curry_1_5!(action_433)),
        52 => partial_5!(happyGoto, curry_1_5!(action_434)),
        53 => partial_5!(happyGoto, curry_1_5!(action_97)),
        54 => partial_5!(happyGoto, curry_1_5!(action_98)),
        61 => partial_5!(happyGoto, curry_1_5!(action_99)),
        64 => partial_5!(happyGoto, curry_1_5!(action_435)),
        75 => partial_5!(happyGoto, curry_1_5!(action_436)),
        76 => partial_5!(happyGoto, curry_1_5!(action_102)),
        77 => partial_5!(happyGoto, curry_1_5!(action_103)),
        78 => partial_5!(happyGoto, curry_1_5!(action_104)),
        79 => partial_5!(happyGoto, curry_1_5!(action_437)),
        80 => partial_5!(happyGoto, curry_1_5!(action_106)),
        81 => partial_5!(happyGoto, curry_1_5!(action_107)),
        134 => partial_5!(happyGoto, curry_1_5!(action_167)),
        _ => box happyFail
    }
}

fn action_191(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_430)),
        _ => box happyFail
    }
}

fn action_192(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_173()
    }
}

fn action_193(i: isize) -> ActionReturn {
    match i {
        181 => partial_5!(happyShift, curry_1_5!(action_429)),
        237 => partial_5!(happyShift, curry_1_5!(action_254)),
        238 => partial_5!(happyShift, curry_1_5!(action_75)),
        131 => partial_5!(happyGoto, curry_1_5!(action_428)),
        _ => box happyFail
    }
}

fn action_194(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_164()
    }
}

fn action_195(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_182()
    }
}

fn action_196(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_183()
    }
}

fn action_197(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_168()
    }
}

fn action_198(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_110()
    }
}

fn action_199(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_112()
    }
}

fn action_200(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_113()
    }
}

fn action_201(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_111()
    }
}

fn action_202(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_169()
    }
}

fn action_203(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_155()
    }
}

fn action_204(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_162()
    }
}

fn action_205(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_163()
    }
}

fn action_206(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_86()
    }
}

fn action_207(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_156()
    }
}

fn action_208(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_157()
    }
}

fn action_209(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_85()
    }
}

fn action_210(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_143()
    }
}

fn action_211(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_152()
    }
}

fn action_212(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_151()
    }
}

fn action_213(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_153()
    }
}

fn action_214(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_144()
    }
}

fn action_215(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_145()
    }
}

fn action_216(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_146()
    }
}

fn action_217(i: isize) -> ActionReturn {
    match i {
        181 => partial_5!(happyShift, curry_1_5!(action_61)),
        14 => partial_5!(happyGoto, curry_1_5!(action_427)),
        _ => box happyFail
    }
}

fn action_218(i: isize) -> ActionReturn {
    match i {
        187 => partial_5!(happyShift, curry_1_5!(action_406)),
        35 => partial_5!(happyGoto, curry_1_5!(action_426)),
        67 => partial_5!(happyGoto, curry_1_5!(action_405)),
        _ => happyReduce_232()
    }
}

fn action_219(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_231()
    }
}

fn action_220(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_235()
    }
}

fn action_221(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_238()
    }
}

fn action_222(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_239()
    }
}

fn action_223(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_234()
    }
}

fn action_224(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_248()
    }
}

fn action_225(i: isize) -> ActionReturn {
    match i {
        181 => happyReduce_26(),
        _ => happyReduce_230()
    }
}

fn action_226(i: isize) -> ActionReturn {
    match i {
        33 => partial_5!(happyGoto, curry_1_5!(action_425)),
        _ => happyReduce_90()
    }
}

fn action_227(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_423)),
        150 => partial_5!(happyShift, curry_1_5!(action_228)),
        237 => partial_5!(happyShift, curry_1_5!(action_140)),
        238 => partial_5!(happyShift, curry_1_5!(action_424)),
        239 => partial_5!(happyShift, curry_1_5!(action_142)),
        70 => partial_5!(happyGoto, curry_1_5!(action_419)),
        71 => partial_5!(happyGoto, curry_1_5!(action_222)),
        72 => partial_5!(happyGoto, curry_1_5!(action_420)),
        73 => partial_5!(happyGoto, curry_1_5!(action_224)),
        74 => partial_5!(happyGoto, curry_1_5!(action_421)),
        76 => partial_5!(happyGoto, curry_1_5!(action_157)),
        77 => partial_5!(happyGoto, curry_1_5!(action_103)),
        78 => partial_5!(happyGoto, curry_1_5!(action_158)),
        80 => partial_5!(happyGoto, curry_1_5!(action_159)),
        81 => partial_5!(happyGoto, curry_1_5!(action_107)),
        133 => partial_5!(happyGoto, curry_1_5!(action_422)),
        134 => partial_5!(happyGoto, curry_1_5!(action_110)),
        _ => box happyFail
    }
}

fn action_228(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_418)),
        150 => partial_5!(happyShift, curry_1_5!(action_228)),
        237 => partial_5!(happyShift, curry_1_5!(action_140)),
        238 => partial_5!(happyShift, curry_1_5!(action_229)),
        239 => partial_5!(happyShift, curry_1_5!(action_142)),
        65 => partial_5!(happyGoto, curry_1_5!(action_414)),
        69 => partial_5!(happyGoto, curry_1_5!(action_415)),
        70 => partial_5!(happyGoto, curry_1_5!(action_221)),
        71 => partial_5!(happyGoto, curry_1_5!(action_222)),
        72 => partial_5!(happyGoto, curry_1_5!(action_416)),
        73 => partial_5!(happyGoto, curry_1_5!(action_224)),
        75 => partial_5!(happyGoto, curry_1_5!(action_154)),
        76 => partial_5!(happyGoto, curry_1_5!(action_102)),
        77 => partial_5!(happyGoto, curry_1_5!(action_103)),
        78 => partial_5!(happyGoto, curry_1_5!(action_104)),
        80 => partial_5!(happyGoto, curry_1_5!(action_155)),
        81 => partial_5!(happyGoto, curry_1_5!(action_107)),
        132 => partial_5!(happyGoto, curry_1_5!(action_108)),
        133 => partial_5!(happyGoto, curry_1_5!(action_417)),
        134 => partial_5!(happyGoto, curry_1_5!(action_110)),
        _ => happyReduce_471()
    }
}

fn action_229(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_413)),
        140 => partial_5!(happyShift, curry_1_5!(action_182)),
        88 => partial_5!(happyGoto, curry_1_5!(action_412)),
        89 => partial_5!(happyGoto, curry_1_5!(action_179)),
        90 => partial_5!(happyGoto, curry_1_5!(action_180)),
        _ => happyReduce_236()
    }
}

fn action_230(i: isize) -> ActionReturn {
    match i {
        181 => partial_5!(happyShift, curry_1_5!(action_61)),
        14 => partial_5!(happyGoto, curry_1_5!(action_411)),
        _ => box happyFail
    }
}

fn action_231(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_108()
    }
}

fn action_232(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_142()
    }
}

fn action_233(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_154()
    }
}

fn action_234(i: isize) -> ActionReturn {
    match i {
        181 => happyReduce_26(),
        187 => partial_5!(happyShift, curry_1_5!(action_406)),
        35 => partial_5!(happyGoto, curry_1_5!(action_410)),
        67 => partial_5!(happyGoto, curry_1_5!(action_405)),
        _ => happyReduce_232()
    }
}

fn action_235(i: isize) -> ActionReturn {
    match i {
        33 => partial_5!(happyGoto, curry_1_5!(action_409)),
        _ => happyReduce_90()
    }
}

fn action_236(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_109()
    }
}

fn action_237(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_408)),
        _ => box happyFail
    }
}

fn action_238(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_165()
    }
}

fn action_239(i: isize) -> ActionReturn {
    match i {
        181 => partial_5!(happyShift, curry_1_5!(action_61)),
        14 => partial_5!(happyGoto, curry_1_5!(action_407)),
        _ => box happyFail
    }
}

fn action_240(i: isize) -> ActionReturn {
    match i {
        187 => partial_5!(happyShift, curry_1_5!(action_406)),
        35 => partial_5!(happyGoto, curry_1_5!(action_404)),
        67 => partial_5!(happyGoto, curry_1_5!(action_405)),
        _ => happyReduce_232()
    }
}

fn action_241(i: isize) -> ActionReturn {
    match i {
        33 => partial_5!(happyGoto, curry_1_5!(action_403)),
        _ => happyReduce_90()
    }
}

fn action_242(i: isize) -> ActionReturn {
    match i {
        239 => partial_5!(happyShift, curry_1_5!(action_142)),
        132 => partial_5!(happyGoto, curry_1_5!(action_402)),
        133 => partial_5!(happyGoto, curry_1_5!(action_150)),
        134 => partial_5!(happyGoto, curry_1_5!(action_110)),
        _ => happyReduce_471()
    }
}

fn action_243(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_87()
    }
}

fn action_244(i: isize) -> ActionReturn {
    match i {
        239 => partial_5!(happyShift, curry_1_5!(action_142)),
        132 => partial_5!(happyGoto, curry_1_5!(action_401)),
        133 => partial_5!(happyGoto, curry_1_5!(action_150)),
        134 => partial_5!(happyGoto, curry_1_5!(action_110)),
        _ => happyReduce_471()
    }
}

fn action_245(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_88()
    }
}

fn action_246(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_12()
    }
}

fn action_247(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_26)),
        144 => partial_5!(happyShift, curry_1_5!(action_27)),
        145 => partial_5!(happyShift, curry_1_5!(action_28)),
        146 => partial_5!(happyShift, curry_1_5!(action_29)),
        147 => partial_5!(happyShift, curry_1_5!(action_30)),
        148 => partial_5!(happyShift, curry_1_5!(action_31)),
        149 => partial_5!(happyShift, curry_1_5!(action_32)),
        150 => partial_5!(happyShift, curry_1_5!(action_33)),
        153 => partial_5!(happyShift, curry_1_5!(action_34)),
        164 => partial_5!(happyShift, curry_1_5!(action_35)),
        184 => partial_5!(happyShift, curry_1_5!(action_36)),
        204 => partial_5!(happyShift, curry_1_5!(action_37)),
        220 => partial_5!(happyShift, curry_1_5!(action_38)),
        233 => partial_5!(happyShift, curry_1_5!(action_39)),
        234 => partial_5!(happyShift, curry_1_5!(action_40)),
        235 => partial_5!(happyShift, curry_1_5!(action_41)),
        236 => partial_5!(happyShift, curry_1_5!(action_42)),
        237 => partial_5!(happyShift, curry_1_5!(action_43)),
        240 => partial_5!(happyShift, curry_1_5!(action_44)),
        241 => partial_5!(happyShift, curry_1_5!(action_45)),
        242 => partial_5!(happyShift, curry_1_5!(action_46)),
        243 => partial_5!(happyShift, curry_1_5!(action_47)),
        244 => partial_5!(happyShift, curry_1_5!(action_48)),
        245 => partial_5!(happyShift, curry_1_5!(action_49)),
        100 => partial_5!(happyGoto, curry_1_5!(action_6)),
        104 => partial_5!(happyGoto, curry_1_5!(action_7)),
        106 => partial_5!(happyGoto, curry_1_5!(action_8)),
        107 => partial_5!(happyGoto, curry_1_5!(action_9)),
        108 => partial_5!(happyGoto, curry_1_5!(action_10)),
        109 => partial_5!(happyGoto, curry_1_5!(action_11)),
        110 => partial_5!(happyGoto, curry_1_5!(action_12)),
        111 => partial_5!(happyGoto, curry_1_5!(action_13)),
        112 => partial_5!(happyGoto, curry_1_5!(action_14)),
        113 => partial_5!(happyGoto, curry_1_5!(action_15)),
        114 => partial_5!(happyGoto, curry_1_5!(action_16)),
        115 => partial_5!(happyGoto, curry_1_5!(action_17)),
        116 => partial_5!(happyGoto, curry_1_5!(action_18)),
        117 => partial_5!(happyGoto, curry_1_5!(action_19)),
        118 => partial_5!(happyGoto, curry_1_5!(action_20)),
        119 => partial_5!(happyGoto, curry_1_5!(action_21)),
        120 => partial_5!(happyGoto, curry_1_5!(action_22)),
        122 => partial_5!(happyGoto, curry_1_5!(action_400)),
        127 => partial_5!(happyGoto, curry_1_5!(action_24)),
        128 => partial_5!(happyGoto, curry_1_5!(action_25)),
        _ => box happyFail
    }
}

fn action_248(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_26)),
        144 => partial_5!(happyShift, curry_1_5!(action_27)),
        145 => partial_5!(happyShift, curry_1_5!(action_28)),
        146 => partial_5!(happyShift, curry_1_5!(action_29)),
        147 => partial_5!(happyShift, curry_1_5!(action_30)),
        148 => partial_5!(happyShift, curry_1_5!(action_31)),
        149 => partial_5!(happyShift, curry_1_5!(action_32)),
        150 => partial_5!(happyShift, curry_1_5!(action_33)),
        153 => partial_5!(happyShift, curry_1_5!(action_34)),
        164 => partial_5!(happyShift, curry_1_5!(action_35)),
        184 => partial_5!(happyShift, curry_1_5!(action_36)),
        204 => partial_5!(happyShift, curry_1_5!(action_37)),
        220 => partial_5!(happyShift, curry_1_5!(action_38)),
        233 => partial_5!(happyShift, curry_1_5!(action_39)),
        234 => partial_5!(happyShift, curry_1_5!(action_40)),
        235 => partial_5!(happyShift, curry_1_5!(action_41)),
        236 => partial_5!(happyShift, curry_1_5!(action_42)),
        237 => partial_5!(happyShift, curry_1_5!(action_43)),
        240 => partial_5!(happyShift, curry_1_5!(action_44)),
        241 => partial_5!(happyShift, curry_1_5!(action_45)),
        242 => partial_5!(happyShift, curry_1_5!(action_46)),
        243 => partial_5!(happyShift, curry_1_5!(action_47)),
        244 => partial_5!(happyShift, curry_1_5!(action_48)),
        245 => partial_5!(happyShift, curry_1_5!(action_49)),
        100 => partial_5!(happyGoto, curry_1_5!(action_6)),
        104 => partial_5!(happyGoto, curry_1_5!(action_7)),
        106 => partial_5!(happyGoto, curry_1_5!(action_8)),
        107 => partial_5!(happyGoto, curry_1_5!(action_9)),
        108 => partial_5!(happyGoto, curry_1_5!(action_10)),
        109 => partial_5!(happyGoto, curry_1_5!(action_11)),
        110 => partial_5!(happyGoto, curry_1_5!(action_12)),
        111 => partial_5!(happyGoto, curry_1_5!(action_13)),
        112 => partial_5!(happyGoto, curry_1_5!(action_14)),
        113 => partial_5!(happyGoto, curry_1_5!(action_15)),
        114 => partial_5!(happyGoto, curry_1_5!(action_16)),
        115 => partial_5!(happyGoto, curry_1_5!(action_17)),
        116 => partial_5!(happyGoto, curry_1_5!(action_18)),
        117 => partial_5!(happyGoto, curry_1_5!(action_19)),
        118 => partial_5!(happyGoto, curry_1_5!(action_20)),
        119 => partial_5!(happyGoto, curry_1_5!(action_21)),
        120 => partial_5!(happyGoto, curry_1_5!(action_22)),
        122 => partial_5!(happyGoto, curry_1_5!(action_399)),
        127 => partial_5!(happyGoto, curry_1_5!(action_24)),
        128 => partial_5!(happyGoto, curry_1_5!(action_25)),
        _ => box happyFail
    }
}

fn action_249(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_457()
    }
}

fn action_250(i: isize) -> ActionReturn {
    match i {
        180 => partial_5!(happyShift, curry_1_5!(action_398)),
        _ => box happyFail
    }
}

fn action_251(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_26)),
        144 => partial_5!(happyShift, curry_1_5!(action_27)),
        145 => partial_5!(happyShift, curry_1_5!(action_28)),
        146 => partial_5!(happyShift, curry_1_5!(action_29)),
        147 => partial_5!(happyShift, curry_1_5!(action_30)),
        148 => partial_5!(happyShift, curry_1_5!(action_31)),
        149 => partial_5!(happyShift, curry_1_5!(action_32)),
        150 => partial_5!(happyShift, curry_1_5!(action_33)),
        153 => partial_5!(happyShift, curry_1_5!(action_34)),
        164 => partial_5!(happyShift, curry_1_5!(action_35)),
        184 => partial_5!(happyShift, curry_1_5!(action_36)),
        204 => partial_5!(happyShift, curry_1_5!(action_37)),
        220 => partial_5!(happyShift, curry_1_5!(action_38)),
        233 => partial_5!(happyShift, curry_1_5!(action_39)),
        234 => partial_5!(happyShift, curry_1_5!(action_40)),
        235 => partial_5!(happyShift, curry_1_5!(action_41)),
        236 => partial_5!(happyShift, curry_1_5!(action_42)),
        237 => partial_5!(happyShift, curry_1_5!(action_43)),
        240 => partial_5!(happyShift, curry_1_5!(action_44)),
        241 => partial_5!(happyShift, curry_1_5!(action_45)),
        242 => partial_5!(happyShift, curry_1_5!(action_46)),
        243 => partial_5!(happyShift, curry_1_5!(action_47)),
        244 => partial_5!(happyShift, curry_1_5!(action_48)),
        245 => partial_5!(happyShift, curry_1_5!(action_49)),
        100 => partial_5!(happyGoto, curry_1_5!(action_6)),
        104 => partial_5!(happyGoto, curry_1_5!(action_7)),
        106 => partial_5!(happyGoto, curry_1_5!(action_8)),
        107 => partial_5!(happyGoto, curry_1_5!(action_9)),
        108 => partial_5!(happyGoto, curry_1_5!(action_10)),
        109 => partial_5!(happyGoto, curry_1_5!(action_11)),
        110 => partial_5!(happyGoto, curry_1_5!(action_12)),
        111 => partial_5!(happyGoto, curry_1_5!(action_13)),
        112 => partial_5!(happyGoto, curry_1_5!(action_14)),
        113 => partial_5!(happyGoto, curry_1_5!(action_15)),
        114 => partial_5!(happyGoto, curry_1_5!(action_16)),
        115 => partial_5!(happyGoto, curry_1_5!(action_17)),
        116 => partial_5!(happyGoto, curry_1_5!(action_18)),
        117 => partial_5!(happyGoto, curry_1_5!(action_19)),
        118 => partial_5!(happyGoto, curry_1_5!(action_20)),
        119 => partial_5!(happyGoto, curry_1_5!(action_21)),
        120 => partial_5!(happyGoto, curry_1_5!(action_22)),
        122 => partial_5!(happyGoto, curry_1_5!(action_397)),
        127 => partial_5!(happyGoto, curry_1_5!(action_24)),
        128 => partial_5!(happyGoto, curry_1_5!(action_25)),
        _ => box happyFail
    }
}

fn action_252(i: isize) -> ActionReturn {
    match i {
        180 => partial_5!(happyShift, curry_1_5!(action_396)),
        _ => box happyFail
    }
}

fn action_253(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_26)),
        144 => partial_5!(happyShift, curry_1_5!(action_27)),
        145 => partial_5!(happyShift, curry_1_5!(action_28)),
        146 => partial_5!(happyShift, curry_1_5!(action_29)),
        147 => partial_5!(happyShift, curry_1_5!(action_30)),
        148 => partial_5!(happyShift, curry_1_5!(action_31)),
        149 => partial_5!(happyShift, curry_1_5!(action_32)),
        150 => partial_5!(happyShift, curry_1_5!(action_33)),
        153 => partial_5!(happyShift, curry_1_5!(action_34)),
        164 => partial_5!(happyShift, curry_1_5!(action_35)),
        184 => partial_5!(happyShift, curry_1_5!(action_36)),
        204 => partial_5!(happyShift, curry_1_5!(action_37)),
        220 => partial_5!(happyShift, curry_1_5!(action_38)),
        233 => partial_5!(happyShift, curry_1_5!(action_39)),
        234 => partial_5!(happyShift, curry_1_5!(action_40)),
        235 => partial_5!(happyShift, curry_1_5!(action_41)),
        236 => partial_5!(happyShift, curry_1_5!(action_42)),
        237 => partial_5!(happyShift, curry_1_5!(action_43)),
        240 => partial_5!(happyShift, curry_1_5!(action_44)),
        241 => partial_5!(happyShift, curry_1_5!(action_45)),
        242 => partial_5!(happyShift, curry_1_5!(action_46)),
        243 => partial_5!(happyShift, curry_1_5!(action_47)),
        244 => partial_5!(happyShift, curry_1_5!(action_48)),
        245 => partial_5!(happyShift, curry_1_5!(action_49)),
        100 => partial_5!(happyGoto, curry_1_5!(action_6)),
        104 => partial_5!(happyGoto, curry_1_5!(action_7)),
        106 => partial_5!(happyGoto, curry_1_5!(action_8)),
        107 => partial_5!(happyGoto, curry_1_5!(action_9)),
        108 => partial_5!(happyGoto, curry_1_5!(action_10)),
        109 => partial_5!(happyGoto, curry_1_5!(action_11)),
        110 => partial_5!(happyGoto, curry_1_5!(action_12)),
        111 => partial_5!(happyGoto, curry_1_5!(action_13)),
        112 => partial_5!(happyGoto, curry_1_5!(action_14)),
        113 => partial_5!(happyGoto, curry_1_5!(action_15)),
        114 => partial_5!(happyGoto, curry_1_5!(action_16)),
        115 => partial_5!(happyGoto, curry_1_5!(action_17)),
        116 => partial_5!(happyGoto, curry_1_5!(action_18)),
        117 => partial_5!(happyGoto, curry_1_5!(action_19)),
        118 => partial_5!(happyGoto, curry_1_5!(action_20)),
        119 => partial_5!(happyGoto, curry_1_5!(action_21)),
        120 => partial_5!(happyGoto, curry_1_5!(action_22)),
        122 => partial_5!(happyGoto, curry_1_5!(action_395)),
        127 => partial_5!(happyGoto, curry_1_5!(action_24)),
        128 => partial_5!(happyGoto, curry_1_5!(action_25)),
        _ => box happyFail
    }
}

fn action_254(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_469()
    }
}

fn action_255(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_26)),
        144 => partial_5!(happyShift, curry_1_5!(action_27)),
        145 => partial_5!(happyShift, curry_1_5!(action_28)),
        146 => partial_5!(happyShift, curry_1_5!(action_29)),
        147 => partial_5!(happyShift, curry_1_5!(action_30)),
        148 => partial_5!(happyShift, curry_1_5!(action_31)),
        149 => partial_5!(happyShift, curry_1_5!(action_32)),
        150 => partial_5!(happyShift, curry_1_5!(action_33)),
        153 => partial_5!(happyShift, curry_1_5!(action_34)),
        164 => partial_5!(happyShift, curry_1_5!(action_35)),
        184 => partial_5!(happyShift, curry_1_5!(action_36)),
        185 => happyReduce_40(),
        186 => happyReduce_40(),
        188 => happyReduce_40(),
        190 => happyReduce_40(),
        192 => happyReduce_40(),
        193 => happyReduce_40(),
        195 => happyReduce_40(),
        198 => happyReduce_40(),
        200 => happyReduce_40(),
        201 => happyReduce_40(),
        202 => happyReduce_40(),
        204 => partial_5!(happyShift, curry_1_5!(action_37)),
        207 => happyReduce_40(),
        208 => happyReduce_40(),
        209 => happyReduce_40(),
        210 => happyReduce_40(),
        212 => happyReduce_40(),
        213 => happyReduce_40(),
        214 => happyReduce_40(),
        215 => happyReduce_40(),
        216 => happyReduce_40(),
        218 => happyReduce_40(),
        219 => happyReduce_40(),
        220 => partial_5!(happyShift, curry_1_5!(action_38)),
        221 => happyReduce_40(),
        222 => happyReduce_40(),
        223 => happyReduce_40(),
        225 => happyReduce_40(),
        226 => happyReduce_40(),
        227 => happyReduce_40(),
        228 => happyReduce_40(),
        229 => happyReduce_40(),
        230 => happyReduce_40(),
        231 => happyReduce_40(),
        233 => partial_5!(happyShift, curry_1_5!(action_39)),
        234 => partial_5!(happyShift, curry_1_5!(action_40)),
        235 => partial_5!(happyShift, curry_1_5!(action_41)),
        236 => partial_5!(happyShift, curry_1_5!(action_42)),
        237 => partial_5!(happyShift, curry_1_5!(action_43)),
        238 => happyReduce_40(),
        239 => happyReduce_40(),
        240 => partial_5!(happyShift, curry_1_5!(action_44)),
        241 => partial_5!(happyShift, curry_1_5!(action_45)),
        242 => partial_5!(happyShift, curry_1_5!(action_46)),
        243 => partial_5!(happyShift, curry_1_5!(action_47)),
        244 => partial_5!(happyShift, curry_1_5!(action_48)),
        245 => partial_5!(happyShift, curry_1_5!(action_49)),
        15 => partial_5!(happyGoto, curry_1_5!(action_393)),
        100 => partial_5!(happyGoto, curry_1_5!(action_6)),
        104 => partial_5!(happyGoto, curry_1_5!(action_7)),
        106 => partial_5!(happyGoto, curry_1_5!(action_8)),
        107 => partial_5!(happyGoto, curry_1_5!(action_9)),
        108 => partial_5!(happyGoto, curry_1_5!(action_10)),
        109 => partial_5!(happyGoto, curry_1_5!(action_11)),
        110 => partial_5!(happyGoto, curry_1_5!(action_12)),
        111 => partial_5!(happyGoto, curry_1_5!(action_13)),
        112 => partial_5!(happyGoto, curry_1_5!(action_14)),
        113 => partial_5!(happyGoto, curry_1_5!(action_15)),
        114 => partial_5!(happyGoto, curry_1_5!(action_16)),
        115 => partial_5!(happyGoto, curry_1_5!(action_17)),
        116 => partial_5!(happyGoto, curry_1_5!(action_18)),
        117 => partial_5!(happyGoto, curry_1_5!(action_19)),
        118 => partial_5!(happyGoto, curry_1_5!(action_20)),
        119 => partial_5!(happyGoto, curry_1_5!(action_21)),
        120 => partial_5!(happyGoto, curry_1_5!(action_22)),
        122 => partial_5!(happyGoto, curry_1_5!(action_249)),
        124 => partial_5!(happyGoto, curry_1_5!(action_394)),
        127 => partial_5!(happyGoto, curry_1_5!(action_24)),
        128 => partial_5!(happyGoto, curry_1_5!(action_25)),
        _ => happyReduce_456()
    }
}

fn action_256(i: isize) -> ActionReturn {
    match i {
        232 => partial_5!(happyShift, curry_1_5!(action_392)),
        _ => box happyFail
    }
}

fn action_257(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_26)),
        144 => partial_5!(happyShift, curry_1_5!(action_27)),
        145 => partial_5!(happyShift, curry_1_5!(action_28)),
        146 => partial_5!(happyShift, curry_1_5!(action_29)),
        147 => partial_5!(happyShift, curry_1_5!(action_30)),
        148 => partial_5!(happyShift, curry_1_5!(action_31)),
        149 => partial_5!(happyShift, curry_1_5!(action_32)),
        150 => partial_5!(happyShift, curry_1_5!(action_33)),
        153 => partial_5!(happyShift, curry_1_5!(action_34)),
        164 => partial_5!(happyShift, curry_1_5!(action_35)),
        180 => partial_5!(happyShift, curry_1_5!(action_60)),
        181 => partial_5!(happyShift, curry_1_5!(action_61)),
        184 => partial_5!(happyShift, curry_1_5!(action_36)),
        187 => partial_5!(happyShift, curry_1_5!(action_62)),
        189 => partial_5!(happyShift, curry_1_5!(action_63)),
        191 => partial_5!(happyShift, curry_1_5!(action_64)),
        194 => partial_5!(happyShift, curry_1_5!(action_65)),
        196 => partial_5!(happyShift, curry_1_5!(action_66)),
        197 => partial_5!(happyShift, curry_1_5!(action_67)),
        203 => partial_5!(happyShift, curry_1_5!(action_68)),
        204 => partial_5!(happyShift, curry_1_5!(action_37)),
        205 => partial_5!(happyShift, curry_1_5!(action_69)),
        206 => partial_5!(happyShift, curry_1_5!(action_70)),
        217 => partial_5!(happyShift, curry_1_5!(action_71)),
        220 => partial_5!(happyShift, curry_1_5!(action_38)),
        224 => partial_5!(happyShift, curry_1_5!(action_72)),
        232 => partial_5!(happyShift, curry_1_5!(action_73)),
        233 => partial_5!(happyShift, curry_1_5!(action_39)),
        234 => partial_5!(happyShift, curry_1_5!(action_40)),
        235 => partial_5!(happyShift, curry_1_5!(action_41)),
        236 => partial_5!(happyShift, curry_1_5!(action_42)),
        237 => partial_5!(happyShift, curry_1_5!(action_74)),
        238 => partial_5!(happyShift, curry_1_5!(action_75)),
        240 => partial_5!(happyShift, curry_1_5!(action_44)),
        241 => partial_5!(happyShift, curry_1_5!(action_45)),
        242 => partial_5!(happyShift, curry_1_5!(action_46)),
        243 => partial_5!(happyShift, curry_1_5!(action_47)),
        244 => partial_5!(happyShift, curry_1_5!(action_48)),
        245 => partial_5!(happyShift, curry_1_5!(action_49)),
        12 => partial_5!(happyGoto, curry_1_5!(action_391)),
        13 => partial_5!(happyGoto, curry_1_5!(action_51)),
        14 => partial_5!(happyGoto, curry_1_5!(action_52)),
        22 => partial_5!(happyGoto, curry_1_5!(action_53)),
        23 => partial_5!(happyGoto, curry_1_5!(action_54)),
        24 => partial_5!(happyGoto, curry_1_5!(action_55)),
        25 => partial_5!(happyGoto, curry_1_5!(action_56)),
        26 => partial_5!(happyGoto, curry_1_5!(action_57)),
        100 => partial_5!(happyGoto, curry_1_5!(action_6)),
        104 => partial_5!(happyGoto, curry_1_5!(action_7)),
        106 => partial_5!(happyGoto, curry_1_5!(action_8)),
        107 => partial_5!(happyGoto, curry_1_5!(action_9)),
        108 => partial_5!(happyGoto, curry_1_5!(action_10)),
        109 => partial_5!(happyGoto, curry_1_5!(action_11)),
        110 => partial_5!(happyGoto, curry_1_5!(action_12)),
        111 => partial_5!(happyGoto, curry_1_5!(action_13)),
        112 => partial_5!(happyGoto, curry_1_5!(action_14)),
        113 => partial_5!(happyGoto, curry_1_5!(action_15)),
        114 => partial_5!(happyGoto, curry_1_5!(action_16)),
        115 => partial_5!(happyGoto, curry_1_5!(action_17)),
        116 => partial_5!(happyGoto, curry_1_5!(action_18)),
        117 => partial_5!(happyGoto, curry_1_5!(action_19)),
        118 => partial_5!(happyGoto, curry_1_5!(action_20)),
        119 => partial_5!(happyGoto, curry_1_5!(action_21)),
        120 => partial_5!(happyGoto, curry_1_5!(action_22)),
        122 => partial_5!(happyGoto, curry_1_5!(action_58)),
        127 => partial_5!(happyGoto, curry_1_5!(action_24)),
        128 => partial_5!(happyGoto, curry_1_5!(action_25)),
        131 => partial_5!(happyGoto, curry_1_5!(action_59)),
        _ => box happyFail
    }
}

fn action_258(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_67()
    }
}

fn action_259(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_406()
    }
}

fn action_260(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_460()
    }
}

fn action_261(i: isize) -> ActionReturn {
    match i {
        167 => partial_5!(happyShift, curry_1_5!(action_389)),
        183 => partial_5!(happyShift, curry_1_5!(action_390)),
        _ => box happyFail
    }
}

fn action_262(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_68()
    }
}

fn action_263(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_388)),
        _ => box happyFail
    }
}

fn action_264(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_75()
    }
}

fn action_265(i: isize) -> ActionReturn {
    match i {
        211 => partial_5!(happyShift, curry_1_5!(action_387)),
        17 => partial_5!(happyGoto, curry_1_5!(action_385)),
        21 => partial_5!(happyGoto, curry_1_5!(action_386)),
        _ => happyReduce_42()
    }
}

fn action_266(i: isize) -> ActionReturn {
    match i {
        239 => partial_5!(happyShift, curry_1_5!(action_142)),
        132 => partial_5!(happyGoto, curry_1_5!(action_384)),
        133 => partial_5!(happyGoto, curry_1_5!(action_150)),
        134 => partial_5!(happyGoto, curry_1_5!(action_110)),
        _ => happyReduce_471()
    }
}

fn action_267(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_57()
    }
}

fn action_268(i: isize) -> ActionReturn {
    match i {
        190 => partial_5!(happyShift, curry_1_5!(action_116)),
        192 => partial_5!(happyShift, curry_1_5!(action_117)),
        195 => partial_5!(happyShift, curry_1_5!(action_118)),
        198 => partial_5!(happyShift, curry_1_5!(action_119)),
        200 => partial_5!(happyShift, curry_1_5!(action_120)),
        202 => partial_5!(happyShift, curry_1_5!(action_122)),
        208 => partial_5!(happyShift, curry_1_5!(action_124)),
        209 => partial_5!(happyShift, curry_1_5!(action_125)),
        210 => partial_5!(happyShift, curry_1_5!(action_126)),
        218 => partial_5!(happyShift, curry_1_5!(action_129)),
        219 => partial_5!(happyShift, curry_1_5!(action_130)),
        223 => partial_5!(happyShift, curry_1_5!(action_133)),
        226 => partial_5!(happyShift, curry_1_5!(action_135)),
        228 => partial_5!(happyShift, curry_1_5!(action_137)),
        229 => partial_5!(happyShift, curry_1_5!(action_138)),
        230 => partial_5!(happyShift, curry_1_5!(action_139)),
        238 => partial_5!(happyShift, curry_1_5!(action_141)),
        239 => partial_5!(happyShift, curry_1_5!(action_142)),
        44 => partial_5!(happyGoto, curry_1_5!(action_286)),
        45 => partial_5!(happyGoto, curry_1_5!(action_89)),
        47 => partial_5!(happyGoto, curry_1_5!(action_287)),
        49 => partial_5!(happyGoto, curry_1_5!(action_288)),
        51 => partial_5!(happyGoto, curry_1_5!(action_289)),
        52 => partial_5!(happyGoto, curry_1_5!(action_96)),
        53 => partial_5!(happyGoto, curry_1_5!(action_97)),
        54 => partial_5!(happyGoto, curry_1_5!(action_98)),
        61 => partial_5!(happyGoto, curry_1_5!(action_99)),
        65 => partial_5!(happyGoto, curry_1_5!(action_290)),
        86 => partial_5!(happyGoto, curry_1_5!(action_383)),
        132 => partial_5!(happyGoto, curry_1_5!(action_108)),
        133 => partial_5!(happyGoto, curry_1_5!(action_293)),
        134 => partial_5!(happyGoto, curry_1_5!(action_110)),
        _ => happyReduce_471()
    }
}

fn action_269(i: isize) -> ActionReturn {
    match i {
        190 => partial_5!(happyShift, curry_1_5!(action_116)),
        192 => partial_5!(happyShift, curry_1_5!(action_117)),
        195 => partial_5!(happyShift, curry_1_5!(action_118)),
        198 => partial_5!(happyShift, curry_1_5!(action_119)),
        200 => partial_5!(happyShift, curry_1_5!(action_120)),
        202 => partial_5!(happyShift, curry_1_5!(action_122)),
        208 => partial_5!(happyShift, curry_1_5!(action_124)),
        209 => partial_5!(happyShift, curry_1_5!(action_125)),
        210 => partial_5!(happyShift, curry_1_5!(action_126)),
        218 => partial_5!(happyShift, curry_1_5!(action_129)),
        219 => partial_5!(happyShift, curry_1_5!(action_130)),
        223 => partial_5!(happyShift, curry_1_5!(action_133)),
        226 => partial_5!(happyShift, curry_1_5!(action_135)),
        228 => partial_5!(happyShift, curry_1_5!(action_137)),
        229 => partial_5!(happyShift, curry_1_5!(action_138)),
        230 => partial_5!(happyShift, curry_1_5!(action_139)),
        238 => partial_5!(happyShift, curry_1_5!(action_141)),
        239 => partial_5!(happyShift, curry_1_5!(action_142)),
        44 => partial_5!(happyGoto, curry_1_5!(action_286)),
        45 => partial_5!(happyGoto, curry_1_5!(action_89)),
        47 => partial_5!(happyGoto, curry_1_5!(action_287)),
        49 => partial_5!(happyGoto, curry_1_5!(action_288)),
        51 => partial_5!(happyGoto, curry_1_5!(action_289)),
        52 => partial_5!(happyGoto, curry_1_5!(action_96)),
        53 => partial_5!(happyGoto, curry_1_5!(action_97)),
        54 => partial_5!(happyGoto, curry_1_5!(action_98)),
        61 => partial_5!(happyGoto, curry_1_5!(action_99)),
        65 => partial_5!(happyGoto, curry_1_5!(action_290)),
        86 => partial_5!(happyGoto, curry_1_5!(action_382)),
        132 => partial_5!(happyGoto, curry_1_5!(action_108)),
        133 => partial_5!(happyGoto, curry_1_5!(action_293)),
        134 => partial_5!(happyGoto, curry_1_5!(action_110)),
        _ => happyReduce_471()
    }
}

fn action_270(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_26)),
        144 => partial_5!(happyShift, curry_1_5!(action_27)),
        145 => partial_5!(happyShift, curry_1_5!(action_28)),
        146 => partial_5!(happyShift, curry_1_5!(action_29)),
        147 => partial_5!(happyShift, curry_1_5!(action_30)),
        148 => partial_5!(happyShift, curry_1_5!(action_31)),
        149 => partial_5!(happyShift, curry_1_5!(action_32)),
        150 => partial_5!(happyShift, curry_1_5!(action_33)),
        153 => partial_5!(happyShift, curry_1_5!(action_34)),
        164 => partial_5!(happyShift, curry_1_5!(action_35)),
        184 => partial_5!(happyShift, curry_1_5!(action_36)),
        204 => partial_5!(happyShift, curry_1_5!(action_37)),
        220 => partial_5!(happyShift, curry_1_5!(action_38)),
        233 => partial_5!(happyShift, curry_1_5!(action_39)),
        234 => partial_5!(happyShift, curry_1_5!(action_40)),
        235 => partial_5!(happyShift, curry_1_5!(action_41)),
        236 => partial_5!(happyShift, curry_1_5!(action_42)),
        237 => partial_5!(happyShift, curry_1_5!(action_43)),
        240 => partial_5!(happyShift, curry_1_5!(action_44)),
        241 => partial_5!(happyShift, curry_1_5!(action_45)),
        242 => partial_5!(happyShift, curry_1_5!(action_46)),
        243 => partial_5!(happyShift, curry_1_5!(action_47)),
        244 => partial_5!(happyShift, curry_1_5!(action_48)),
        245 => partial_5!(happyShift, curry_1_5!(action_49)),
        100 => partial_5!(happyGoto, curry_1_5!(action_6)),
        104 => partial_5!(happyGoto, curry_1_5!(action_7)),
        106 => partial_5!(happyGoto, curry_1_5!(action_8)),
        107 => partial_5!(happyGoto, curry_1_5!(action_9)),
        108 => partial_5!(happyGoto, curry_1_5!(action_10)),
        109 => partial_5!(happyGoto, curry_1_5!(action_11)),
        110 => partial_5!(happyGoto, curry_1_5!(action_12)),
        111 => partial_5!(happyGoto, curry_1_5!(action_13)),
        112 => partial_5!(happyGoto, curry_1_5!(action_14)),
        113 => partial_5!(happyGoto, curry_1_5!(action_15)),
        114 => partial_5!(happyGoto, curry_1_5!(action_16)),
        115 => partial_5!(happyGoto, curry_1_5!(action_17)),
        116 => partial_5!(happyGoto, curry_1_5!(action_18)),
        117 => partial_5!(happyGoto, curry_1_5!(action_19)),
        118 => partial_5!(happyGoto, curry_1_5!(action_20)),
        119 => partial_5!(happyGoto, curry_1_5!(action_21)),
        120 => partial_5!(happyGoto, curry_1_5!(action_381)),
        127 => partial_5!(happyGoto, curry_1_5!(action_24)),
        128 => partial_5!(happyGoto, curry_1_5!(action_25)),
        _ => box happyFail
    }
}

fn action_271(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_398()
    }
}

fn action_272(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_26)),
        144 => partial_5!(happyShift, curry_1_5!(action_27)),
        145 => partial_5!(happyShift, curry_1_5!(action_28)),
        146 => partial_5!(happyShift, curry_1_5!(action_29)),
        147 => partial_5!(happyShift, curry_1_5!(action_30)),
        148 => partial_5!(happyShift, curry_1_5!(action_31)),
        149 => partial_5!(happyShift, curry_1_5!(action_32)),
        150 => partial_5!(happyShift, curry_1_5!(action_33)),
        153 => partial_5!(happyShift, curry_1_5!(action_34)),
        164 => partial_5!(happyShift, curry_1_5!(action_35)),
        181 => partial_5!(happyShift, curry_1_5!(action_61)),
        184 => partial_5!(happyShift, curry_1_5!(action_36)),
        190 => partial_5!(happyShift, curry_1_5!(action_116)),
        192 => partial_5!(happyShift, curry_1_5!(action_117)),
        195 => partial_5!(happyShift, curry_1_5!(action_118)),
        198 => partial_5!(happyShift, curry_1_5!(action_119)),
        200 => partial_5!(happyShift, curry_1_5!(action_120)),
        202 => partial_5!(happyShift, curry_1_5!(action_122)),
        204 => partial_5!(happyShift, curry_1_5!(action_37)),
        208 => partial_5!(happyShift, curry_1_5!(action_124)),
        209 => partial_5!(happyShift, curry_1_5!(action_125)),
        210 => partial_5!(happyShift, curry_1_5!(action_126)),
        218 => partial_5!(happyShift, curry_1_5!(action_129)),
        219 => partial_5!(happyShift, curry_1_5!(action_130)),
        220 => partial_5!(happyShift, curry_1_5!(action_38)),
        223 => partial_5!(happyShift, curry_1_5!(action_133)),
        226 => partial_5!(happyShift, curry_1_5!(action_135)),
        228 => partial_5!(happyShift, curry_1_5!(action_137)),
        229 => partial_5!(happyShift, curry_1_5!(action_138)),
        230 => partial_5!(happyShift, curry_1_5!(action_139)),
        233 => partial_5!(happyShift, curry_1_5!(action_39)),
        234 => partial_5!(happyShift, curry_1_5!(action_40)),
        235 => partial_5!(happyShift, curry_1_5!(action_41)),
        236 => partial_5!(happyShift, curry_1_5!(action_42)),
        237 => partial_5!(happyShift, curry_1_5!(action_43)),
        238 => partial_5!(happyShift, curry_1_5!(action_141)),
        239 => partial_5!(happyShift, curry_1_5!(action_142)),
        240 => partial_5!(happyShift, curry_1_5!(action_44)),
        241 => partial_5!(happyShift, curry_1_5!(action_45)),
        242 => partial_5!(happyShift, curry_1_5!(action_46)),
        243 => partial_5!(happyShift, curry_1_5!(action_47)),
        244 => partial_5!(happyShift, curry_1_5!(action_48)),
        245 => partial_5!(happyShift, curry_1_5!(action_49)),
        14 => partial_5!(happyGoto, curry_1_5!(action_285)),
        44 => partial_5!(happyGoto, curry_1_5!(action_286)),
        45 => partial_5!(happyGoto, curry_1_5!(action_89)),
        47 => partial_5!(happyGoto, curry_1_5!(action_287)),
        49 => partial_5!(happyGoto, curry_1_5!(action_288)),
        51 => partial_5!(happyGoto, curry_1_5!(action_289)),
        52 => partial_5!(happyGoto, curry_1_5!(action_96)),
        53 => partial_5!(happyGoto, curry_1_5!(action_97)),
        54 => partial_5!(happyGoto, curry_1_5!(action_98)),
        61 => partial_5!(happyGoto, curry_1_5!(action_99)),
        65 => partial_5!(happyGoto, curry_1_5!(action_290)),
        86 => partial_5!(happyGoto, curry_1_5!(action_380)),
        100 => partial_5!(happyGoto, curry_1_5!(action_6)),
        104 => partial_5!(happyGoto, curry_1_5!(action_7)),
        106 => partial_5!(happyGoto, curry_1_5!(action_8)),
        107 => partial_5!(happyGoto, curry_1_5!(action_9)),
        108 => partial_5!(happyGoto, curry_1_5!(action_10)),
        109 => partial_5!(happyGoto, curry_1_5!(action_11)),
        110 => partial_5!(happyGoto, curry_1_5!(action_12)),
        111 => partial_5!(happyGoto, curry_1_5!(action_13)),
        112 => partial_5!(happyGoto, curry_1_5!(action_14)),
        113 => partial_5!(happyGoto, curry_1_5!(action_15)),
        114 => partial_5!(happyGoto, curry_1_5!(action_16)),
        115 => partial_5!(happyGoto, curry_1_5!(action_17)),
        116 => partial_5!(happyGoto, curry_1_5!(action_18)),
        117 => partial_5!(happyGoto, curry_1_5!(action_19)),
        118 => partial_5!(happyGoto, curry_1_5!(action_20)),
        119 => partial_5!(happyGoto, curry_1_5!(action_21)),
        120 => partial_5!(happyGoto, curry_1_5!(action_22)),
        122 => partial_5!(happyGoto, curry_1_5!(action_292)),
        127 => partial_5!(happyGoto, curry_1_5!(action_24)),
        128 => partial_5!(happyGoto, curry_1_5!(action_25)),
        132 => partial_5!(happyGoto, curry_1_5!(action_108)),
        133 => partial_5!(happyGoto, curry_1_5!(action_293)),
        134 => partial_5!(happyGoto, curry_1_5!(action_110)),
        _ => happyReduce_471()
    }
}

fn action_273(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_397()
    }
}

fn action_274(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_391()
    }
}

fn action_275(i: isize) -> ActionReturn {
    match i {
        236 => partial_5!(happyShift, curry_1_5!(action_379)),
        _ => happyReduce_465()
    }
}

fn action_276(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_466()
    }
}

fn action_277(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_393()
    }
}

fn action_278(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_26)),
        144 => partial_5!(happyShift, curry_1_5!(action_27)),
        145 => partial_5!(happyShift, curry_1_5!(action_28)),
        146 => partial_5!(happyShift, curry_1_5!(action_29)),
        147 => partial_5!(happyShift, curry_1_5!(action_30)),
        148 => partial_5!(happyShift, curry_1_5!(action_31)),
        149 => partial_5!(happyShift, curry_1_5!(action_32)),
        150 => partial_5!(happyShift, curry_1_5!(action_33)),
        153 => partial_5!(happyShift, curry_1_5!(action_34)),
        164 => partial_5!(happyShift, curry_1_5!(action_35)),
        181 => partial_5!(happyShift, curry_1_5!(action_61)),
        184 => partial_5!(happyShift, curry_1_5!(action_36)),
        190 => partial_5!(happyShift, curry_1_5!(action_116)),
        192 => partial_5!(happyShift, curry_1_5!(action_117)),
        195 => partial_5!(happyShift, curry_1_5!(action_118)),
        198 => partial_5!(happyShift, curry_1_5!(action_119)),
        200 => partial_5!(happyShift, curry_1_5!(action_120)),
        202 => partial_5!(happyShift, curry_1_5!(action_122)),
        204 => partial_5!(happyShift, curry_1_5!(action_37)),
        208 => partial_5!(happyShift, curry_1_5!(action_124)),
        209 => partial_5!(happyShift, curry_1_5!(action_125)),
        210 => partial_5!(happyShift, curry_1_5!(action_126)),
        218 => partial_5!(happyShift, curry_1_5!(action_129)),
        219 => partial_5!(happyShift, curry_1_5!(action_130)),
        220 => partial_5!(happyShift, curry_1_5!(action_38)),
        223 => partial_5!(happyShift, curry_1_5!(action_133)),
        226 => partial_5!(happyShift, curry_1_5!(action_135)),
        228 => partial_5!(happyShift, curry_1_5!(action_137)),
        229 => partial_5!(happyShift, curry_1_5!(action_138)),
        230 => partial_5!(happyShift, curry_1_5!(action_139)),
        233 => partial_5!(happyShift, curry_1_5!(action_39)),
        234 => partial_5!(happyShift, curry_1_5!(action_40)),
        235 => partial_5!(happyShift, curry_1_5!(action_41)),
        236 => partial_5!(happyShift, curry_1_5!(action_42)),
        237 => partial_5!(happyShift, curry_1_5!(action_43)),
        238 => partial_5!(happyShift, curry_1_5!(action_141)),
        239 => partial_5!(happyShift, curry_1_5!(action_142)),
        240 => partial_5!(happyShift, curry_1_5!(action_44)),
        241 => partial_5!(happyShift, curry_1_5!(action_45)),
        242 => partial_5!(happyShift, curry_1_5!(action_46)),
        243 => partial_5!(happyShift, curry_1_5!(action_47)),
        244 => partial_5!(happyShift, curry_1_5!(action_48)),
        245 => partial_5!(happyShift, curry_1_5!(action_49)),
        14 => partial_5!(happyGoto, curry_1_5!(action_285)),
        44 => partial_5!(happyGoto, curry_1_5!(action_286)),
        45 => partial_5!(happyGoto, curry_1_5!(action_89)),
        47 => partial_5!(happyGoto, curry_1_5!(action_287)),
        49 => partial_5!(happyGoto, curry_1_5!(action_288)),
        51 => partial_5!(happyGoto, curry_1_5!(action_289)),
        52 => partial_5!(happyGoto, curry_1_5!(action_96)),
        53 => partial_5!(happyGoto, curry_1_5!(action_97)),
        54 => partial_5!(happyGoto, curry_1_5!(action_98)),
        61 => partial_5!(happyGoto, curry_1_5!(action_99)),
        65 => partial_5!(happyGoto, curry_1_5!(action_290)),
        86 => partial_5!(happyGoto, curry_1_5!(action_378)),
        100 => partial_5!(happyGoto, curry_1_5!(action_6)),
        104 => partial_5!(happyGoto, curry_1_5!(action_7)),
        106 => partial_5!(happyGoto, curry_1_5!(action_8)),
        107 => partial_5!(happyGoto, curry_1_5!(action_9)),
        108 => partial_5!(happyGoto, curry_1_5!(action_10)),
        109 => partial_5!(happyGoto, curry_1_5!(action_11)),
        110 => partial_5!(happyGoto, curry_1_5!(action_12)),
        111 => partial_5!(happyGoto, curry_1_5!(action_13)),
        112 => partial_5!(happyGoto, curry_1_5!(action_14)),
        113 => partial_5!(happyGoto, curry_1_5!(action_15)),
        114 => partial_5!(happyGoto, curry_1_5!(action_16)),
        115 => partial_5!(happyGoto, curry_1_5!(action_17)),
        116 => partial_5!(happyGoto, curry_1_5!(action_18)),
        117 => partial_5!(happyGoto, curry_1_5!(action_19)),
        118 => partial_5!(happyGoto, curry_1_5!(action_20)),
        119 => partial_5!(happyGoto, curry_1_5!(action_21)),
        120 => partial_5!(happyGoto, curry_1_5!(action_22)),
        122 => partial_5!(happyGoto, curry_1_5!(action_292)),
        127 => partial_5!(happyGoto, curry_1_5!(action_24)),
        128 => partial_5!(happyGoto, curry_1_5!(action_25)),
        132 => partial_5!(happyGoto, curry_1_5!(action_108)),
        133 => partial_5!(happyGoto, curry_1_5!(action_293)),
        134 => partial_5!(happyGoto, curry_1_5!(action_110)),
        _ => happyReduce_471()
    }
}

fn action_279(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_26)),
        144 => partial_5!(happyShift, curry_1_5!(action_27)),
        145 => partial_5!(happyShift, curry_1_5!(action_28)),
        146 => partial_5!(happyShift, curry_1_5!(action_29)),
        147 => partial_5!(happyShift, curry_1_5!(action_30)),
        148 => partial_5!(happyShift, curry_1_5!(action_31)),
        149 => partial_5!(happyShift, curry_1_5!(action_32)),
        150 => partial_5!(happyShift, curry_1_5!(action_33)),
        153 => partial_5!(happyShift, curry_1_5!(action_34)),
        164 => partial_5!(happyShift, curry_1_5!(action_35)),
        184 => partial_5!(happyShift, curry_1_5!(action_36)),
        204 => partial_5!(happyShift, curry_1_5!(action_37)),
        220 => partial_5!(happyShift, curry_1_5!(action_38)),
        233 => partial_5!(happyShift, curry_1_5!(action_39)),
        234 => partial_5!(happyShift, curry_1_5!(action_40)),
        235 => partial_5!(happyShift, curry_1_5!(action_41)),
        236 => partial_5!(happyShift, curry_1_5!(action_42)),
        237 => partial_5!(happyShift, curry_1_5!(action_43)),
        240 => partial_5!(happyShift, curry_1_5!(action_44)),
        241 => partial_5!(happyShift, curry_1_5!(action_45)),
        242 => partial_5!(happyShift, curry_1_5!(action_46)),
        243 => partial_5!(happyShift, curry_1_5!(action_47)),
        244 => partial_5!(happyShift, curry_1_5!(action_48)),
        245 => partial_5!(happyShift, curry_1_5!(action_49)),
        100 => partial_5!(happyGoto, curry_1_5!(action_6)),
        104 => partial_5!(happyGoto, curry_1_5!(action_7)),
        106 => partial_5!(happyGoto, curry_1_5!(action_8)),
        107 => partial_5!(happyGoto, curry_1_5!(action_9)),
        108 => partial_5!(happyGoto, curry_1_5!(action_10)),
        109 => partial_5!(happyGoto, curry_1_5!(action_11)),
        110 => partial_5!(happyGoto, curry_1_5!(action_12)),
        111 => partial_5!(happyGoto, curry_1_5!(action_13)),
        112 => partial_5!(happyGoto, curry_1_5!(action_14)),
        113 => partial_5!(happyGoto, curry_1_5!(action_15)),
        114 => partial_5!(happyGoto, curry_1_5!(action_16)),
        115 => partial_5!(happyGoto, curry_1_5!(action_17)),
        116 => partial_5!(happyGoto, curry_1_5!(action_18)),
        117 => partial_5!(happyGoto, curry_1_5!(action_19)),
        118 => partial_5!(happyGoto, curry_1_5!(action_20)),
        119 => partial_5!(happyGoto, curry_1_5!(action_21)),
        120 => partial_5!(happyGoto, curry_1_5!(action_377)),
        127 => partial_5!(happyGoto, curry_1_5!(action_24)),
        128 => partial_5!(happyGoto, curry_1_5!(action_25)),
        _ => box happyFail
    }
}

fn action_280(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_395()
    }
}

fn action_281(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_26)),
        144 => partial_5!(happyShift, curry_1_5!(action_27)),
        145 => partial_5!(happyShift, curry_1_5!(action_28)),
        146 => partial_5!(happyShift, curry_1_5!(action_29)),
        147 => partial_5!(happyShift, curry_1_5!(action_30)),
        148 => partial_5!(happyShift, curry_1_5!(action_31)),
        149 => partial_5!(happyShift, curry_1_5!(action_32)),
        150 => partial_5!(happyShift, curry_1_5!(action_33)),
        153 => partial_5!(happyShift, curry_1_5!(action_34)),
        164 => partial_5!(happyShift, curry_1_5!(action_35)),
        181 => partial_5!(happyShift, curry_1_5!(action_61)),
        184 => partial_5!(happyShift, curry_1_5!(action_36)),
        190 => partial_5!(happyShift, curry_1_5!(action_116)),
        192 => partial_5!(happyShift, curry_1_5!(action_117)),
        195 => partial_5!(happyShift, curry_1_5!(action_118)),
        198 => partial_5!(happyShift, curry_1_5!(action_119)),
        200 => partial_5!(happyShift, curry_1_5!(action_120)),
        202 => partial_5!(happyShift, curry_1_5!(action_122)),
        204 => partial_5!(happyShift, curry_1_5!(action_37)),
        208 => partial_5!(happyShift, curry_1_5!(action_124)),
        209 => partial_5!(happyShift, curry_1_5!(action_125)),
        210 => partial_5!(happyShift, curry_1_5!(action_126)),
        218 => partial_5!(happyShift, curry_1_5!(action_129)),
        219 => partial_5!(happyShift, curry_1_5!(action_130)),
        220 => partial_5!(happyShift, curry_1_5!(action_38)),
        223 => partial_5!(happyShift, curry_1_5!(action_133)),
        226 => partial_5!(happyShift, curry_1_5!(action_135)),
        228 => partial_5!(happyShift, curry_1_5!(action_137)),
        229 => partial_5!(happyShift, curry_1_5!(action_138)),
        230 => partial_5!(happyShift, curry_1_5!(action_139)),
        233 => partial_5!(happyShift, curry_1_5!(action_39)),
        234 => partial_5!(happyShift, curry_1_5!(action_40)),
        235 => partial_5!(happyShift, curry_1_5!(action_41)),
        236 => partial_5!(happyShift, curry_1_5!(action_42)),
        237 => partial_5!(happyShift, curry_1_5!(action_43)),
        238 => partial_5!(happyShift, curry_1_5!(action_141)),
        239 => partial_5!(happyShift, curry_1_5!(action_142)),
        240 => partial_5!(happyShift, curry_1_5!(action_44)),
        241 => partial_5!(happyShift, curry_1_5!(action_45)),
        242 => partial_5!(happyShift, curry_1_5!(action_46)),
        243 => partial_5!(happyShift, curry_1_5!(action_47)),
        244 => partial_5!(happyShift, curry_1_5!(action_48)),
        245 => partial_5!(happyShift, curry_1_5!(action_49)),
        14 => partial_5!(happyGoto, curry_1_5!(action_285)),
        44 => partial_5!(happyGoto, curry_1_5!(action_286)),
        45 => partial_5!(happyGoto, curry_1_5!(action_89)),
        47 => partial_5!(happyGoto, curry_1_5!(action_287)),
        49 => partial_5!(happyGoto, curry_1_5!(action_288)),
        51 => partial_5!(happyGoto, curry_1_5!(action_289)),
        52 => partial_5!(happyGoto, curry_1_5!(action_96)),
        53 => partial_5!(happyGoto, curry_1_5!(action_97)),
        54 => partial_5!(happyGoto, curry_1_5!(action_98)),
        61 => partial_5!(happyGoto, curry_1_5!(action_99)),
        65 => partial_5!(happyGoto, curry_1_5!(action_290)),
        86 => partial_5!(happyGoto, curry_1_5!(action_376)),
        100 => partial_5!(happyGoto, curry_1_5!(action_6)),
        104 => partial_5!(happyGoto, curry_1_5!(action_7)),
        106 => partial_5!(happyGoto, curry_1_5!(action_8)),
        107 => partial_5!(happyGoto, curry_1_5!(action_9)),
        108 => partial_5!(happyGoto, curry_1_5!(action_10)),
        109 => partial_5!(happyGoto, curry_1_5!(action_11)),
        110 => partial_5!(happyGoto, curry_1_5!(action_12)),
        111 => partial_5!(happyGoto, curry_1_5!(action_13)),
        112 => partial_5!(happyGoto, curry_1_5!(action_14)),
        113 => partial_5!(happyGoto, curry_1_5!(action_15)),
        114 => partial_5!(happyGoto, curry_1_5!(action_16)),
        115 => partial_5!(happyGoto, curry_1_5!(action_17)),
        116 => partial_5!(happyGoto, curry_1_5!(action_18)),
        117 => partial_5!(happyGoto, curry_1_5!(action_19)),
        118 => partial_5!(happyGoto, curry_1_5!(action_20)),
        119 => partial_5!(happyGoto, curry_1_5!(action_21)),
        120 => partial_5!(happyGoto, curry_1_5!(action_22)),
        122 => partial_5!(happyGoto, curry_1_5!(action_292)),
        127 => partial_5!(happyGoto, curry_1_5!(action_24)),
        128 => partial_5!(happyGoto, curry_1_5!(action_25)),
        132 => partial_5!(happyGoto, curry_1_5!(action_108)),
        133 => partial_5!(happyGoto, curry_1_5!(action_293)),
        134 => partial_5!(happyGoto, curry_1_5!(action_110)),
        _ => happyReduce_471()
    }
}

fn action_282(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_399()
    }
}

fn action_283(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_390()
    }
}

fn action_284(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_389()
    }
}

fn action_285(i: isize) -> ActionReturn {
    match i {
        139 => partial_5!(happyShift, curry_1_5!(action_375)),
        _ => box happyFail
    }
}

fn action_286(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_372)),
        140 => partial_5!(happyShift, curry_1_5!(action_182)),
        150 => partial_5!(happyShift, curry_1_5!(action_373)),
        87 => partial_5!(happyGoto, curry_1_5!(action_374)),
        88 => partial_5!(happyGoto, curry_1_5!(action_367)),
        89 => partial_5!(happyGoto, curry_1_5!(action_179)),
        90 => partial_5!(happyGoto, curry_1_5!(action_180)),
        91 => partial_5!(happyGoto, curry_1_5!(action_368)),
        92 => partial_5!(happyGoto, curry_1_5!(action_369)),
        _ => happyReduce_304()
    }
}

fn action_287(i: isize) -> ActionReturn {
    match i {
        186 => partial_5!(happyShift, curry_1_5!(action_171)),
        190 => partial_5!(happyShift, curry_1_5!(action_116)),
        192 => partial_5!(happyShift, curry_1_5!(action_117)),
        193 => partial_5!(happyShift, curry_1_5!(action_172)),
        195 => partial_5!(happyShift, curry_1_5!(action_118)),
        198 => partial_5!(happyShift, curry_1_5!(action_119)),
        202 => partial_5!(happyShift, curry_1_5!(action_122)),
        208 => partial_5!(happyShift, curry_1_5!(action_124)),
        209 => partial_5!(happyShift, curry_1_5!(action_125)),
        210 => partial_5!(happyShift, curry_1_5!(action_126)),
        213 => partial_5!(happyShift, curry_1_5!(action_173)),
        214 => partial_5!(happyShift, curry_1_5!(action_174)),
        216 => partial_5!(happyShift, curry_1_5!(action_175)),
        218 => partial_5!(happyShift, curry_1_5!(action_129)),
        219 => partial_5!(happyShift, curry_1_5!(action_130)),
        229 => partial_5!(happyShift, curry_1_5!(action_138)),
        230 => partial_5!(happyShift, curry_1_5!(action_139)),
        231 => partial_5!(happyShift, curry_1_5!(action_176)),
        239 => partial_5!(happyShift, curry_1_5!(action_142)),
        45 => partial_5!(happyGoto, curry_1_5!(action_211)),
        64 => partial_5!(happyGoto, curry_1_5!(action_212)),
        134 => partial_5!(happyGoto, curry_1_5!(action_213)),
        _ => happyReduce_127()
    }
}

fn action_288(i: isize) -> ActionReturn {
    match i {
        186 => partial_5!(happyShift, curry_1_5!(action_171)),
        193 => partial_5!(happyShift, curry_1_5!(action_172)),
        213 => partial_5!(happyShift, curry_1_5!(action_173)),
        214 => partial_5!(happyShift, curry_1_5!(action_174)),
        216 => partial_5!(happyShift, curry_1_5!(action_175)),
        231 => partial_5!(happyShift, curry_1_5!(action_176)),
        239 => partial_5!(happyShift, curry_1_5!(action_142)),
        64 => partial_5!(happyGoto, curry_1_5!(action_204)),
        134 => partial_5!(happyGoto, curry_1_5!(action_205)),
        _ => happyReduce_128()
    }
}

fn action_289(i: isize) -> ActionReturn {
    match i {
        186 => partial_5!(happyShift, curry_1_5!(action_171)),
        193 => partial_5!(happyShift, curry_1_5!(action_172)),
        213 => partial_5!(happyShift, curry_1_5!(action_173)),
        214 => partial_5!(happyShift, curry_1_5!(action_174)),
        216 => partial_5!(happyShift, curry_1_5!(action_175)),
        231 => partial_5!(happyShift, curry_1_5!(action_176)),
        239 => partial_5!(happyShift, curry_1_5!(action_142)),
        64 => partial_5!(happyGoto, curry_1_5!(action_195)),
        134 => partial_5!(happyGoto, curry_1_5!(action_196)),
        _ => happyReduce_129()
    }
}

fn action_290(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_372)),
        140 => partial_5!(happyShift, curry_1_5!(action_182)),
        150 => partial_5!(happyShift, curry_1_5!(action_373)),
        186 => partial_5!(happyShift, curry_1_5!(action_171)),
        190 => partial_5!(happyShift, curry_1_5!(action_116)),
        192 => partial_5!(happyShift, curry_1_5!(action_117)),
        193 => partial_5!(happyShift, curry_1_5!(action_172)),
        195 => partial_5!(happyShift, curry_1_5!(action_118)),
        198 => partial_5!(happyShift, curry_1_5!(action_119)),
        200 => partial_5!(happyShift, curry_1_5!(action_120)),
        202 => partial_5!(happyShift, curry_1_5!(action_122)),
        208 => partial_5!(happyShift, curry_1_5!(action_124)),
        209 => partial_5!(happyShift, curry_1_5!(action_125)),
        210 => partial_5!(happyShift, curry_1_5!(action_126)),
        213 => partial_5!(happyShift, curry_1_5!(action_173)),
        214 => partial_5!(happyShift, curry_1_5!(action_174)),
        216 => partial_5!(happyShift, curry_1_5!(action_175)),
        218 => partial_5!(happyShift, curry_1_5!(action_129)),
        219 => partial_5!(happyShift, curry_1_5!(action_130)),
        223 => partial_5!(happyShift, curry_1_5!(action_133)),
        226 => partial_5!(happyShift, curry_1_5!(action_191)),
        228 => partial_5!(happyShift, curry_1_5!(action_137)),
        229 => partial_5!(happyShift, curry_1_5!(action_138)),
        230 => partial_5!(happyShift, curry_1_5!(action_139)),
        231 => partial_5!(happyShift, curry_1_5!(action_176)),
        238 => partial_5!(happyShift, curry_1_5!(action_192)),
        239 => partial_5!(happyShift, curry_1_5!(action_142)),
        45 => partial_5!(happyGoto, curry_1_5!(action_185)),
        52 => partial_5!(happyGoto, curry_1_5!(action_186)),
        53 => partial_5!(happyGoto, curry_1_5!(action_97)),
        54 => partial_5!(happyGoto, curry_1_5!(action_98)),
        61 => partial_5!(happyGoto, curry_1_5!(action_99)),
        64 => partial_5!(happyGoto, curry_1_5!(action_187)),
        87 => partial_5!(happyGoto, curry_1_5!(action_366)),
        88 => partial_5!(happyGoto, curry_1_5!(action_367)),
        89 => partial_5!(happyGoto, curry_1_5!(action_179)),
        90 => partial_5!(happyGoto, curry_1_5!(action_180)),
        91 => partial_5!(happyGoto, curry_1_5!(action_368)),
        92 => partial_5!(happyGoto, curry_1_5!(action_369)),
        133 => partial_5!(happyGoto, curry_1_5!(action_370)),
        134 => partial_5!(happyGoto, curry_1_5!(action_371)),
        _ => box happyFail
    }
}

fn action_291(i: isize) -> ActionReturn {
    match i {
        139 => partial_5!(happyShift, curry_1_5!(action_365)),
        _ => box happyFail
    }
}

fn action_292(i: isize) -> ActionReturn {
    match i {
        139 => partial_5!(happyShift, curry_1_5!(action_364)),
        _ => box happyFail
    }
}

fn action_293(i: isize) -> ActionReturn {
    match i {
        190 => partial_5!(happyShift, curry_1_5!(action_116)),
        192 => partial_5!(happyShift, curry_1_5!(action_117)),
        195 => partial_5!(happyShift, curry_1_5!(action_118)),
        198 => partial_5!(happyShift, curry_1_5!(action_119)),
        200 => partial_5!(happyShift, curry_1_5!(action_120)),
        202 => partial_5!(happyShift, curry_1_5!(action_122)),
        208 => partial_5!(happyShift, curry_1_5!(action_124)),
        209 => partial_5!(happyShift, curry_1_5!(action_125)),
        210 => partial_5!(happyShift, curry_1_5!(action_126)),
        218 => partial_5!(happyShift, curry_1_5!(action_129)),
        219 => partial_5!(happyShift, curry_1_5!(action_130)),
        223 => partial_5!(happyShift, curry_1_5!(action_133)),
        226 => partial_5!(happyShift, curry_1_5!(action_168)),
        228 => partial_5!(happyShift, curry_1_5!(action_137)),
        229 => partial_5!(happyShift, curry_1_5!(action_138)),
        230 => partial_5!(happyShift, curry_1_5!(action_139)),
        238 => partial_5!(happyShift, curry_1_5!(action_169)),
        239 => partial_5!(happyShift, curry_1_5!(action_142)),
        45 => partial_5!(happyGoto, curry_1_5!(action_163)),
        52 => partial_5!(happyGoto, curry_1_5!(action_164)),
        53 => partial_5!(happyGoto, curry_1_5!(action_97)),
        54 => partial_5!(happyGoto, curry_1_5!(action_98)),
        61 => partial_5!(happyGoto, curry_1_5!(action_99)),
        134 => partial_5!(happyGoto, curry_1_5!(action_167)),
        _ => happyReduce_472()
    }
}

fn action_294(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_26)),
        144 => partial_5!(happyShift, curry_1_5!(action_27)),
        145 => partial_5!(happyShift, curry_1_5!(action_28)),
        146 => partial_5!(happyShift, curry_1_5!(action_29)),
        147 => partial_5!(happyShift, curry_1_5!(action_30)),
        148 => partial_5!(happyShift, curry_1_5!(action_31)),
        149 => partial_5!(happyShift, curry_1_5!(action_32)),
        150 => partial_5!(happyShift, curry_1_5!(action_33)),
        153 => partial_5!(happyShift, curry_1_5!(action_34)),
        164 => partial_5!(happyShift, curry_1_5!(action_35)),
        184 => partial_5!(happyShift, curry_1_5!(action_36)),
        204 => partial_5!(happyShift, curry_1_5!(action_37)),
        220 => partial_5!(happyShift, curry_1_5!(action_38)),
        233 => partial_5!(happyShift, curry_1_5!(action_39)),
        234 => partial_5!(happyShift, curry_1_5!(action_40)),
        235 => partial_5!(happyShift, curry_1_5!(action_41)),
        236 => partial_5!(happyShift, curry_1_5!(action_42)),
        237 => partial_5!(happyShift, curry_1_5!(action_43)),
        240 => partial_5!(happyShift, curry_1_5!(action_44)),
        241 => partial_5!(happyShift, curry_1_5!(action_45)),
        242 => partial_5!(happyShift, curry_1_5!(action_46)),
        243 => partial_5!(happyShift, curry_1_5!(action_47)),
        244 => partial_5!(happyShift, curry_1_5!(action_48)),
        245 => partial_5!(happyShift, curry_1_5!(action_49)),
        100 => partial_5!(happyGoto, curry_1_5!(action_6)),
        104 => partial_5!(happyGoto, curry_1_5!(action_7)),
        106 => partial_5!(happyGoto, curry_1_5!(action_8)),
        107 => partial_5!(happyGoto, curry_1_5!(action_9)),
        108 => partial_5!(happyGoto, curry_1_5!(action_10)),
        109 => partial_5!(happyGoto, curry_1_5!(action_11)),
        110 => partial_5!(happyGoto, curry_1_5!(action_12)),
        111 => partial_5!(happyGoto, curry_1_5!(action_13)),
        112 => partial_5!(happyGoto, curry_1_5!(action_14)),
        113 => partial_5!(happyGoto, curry_1_5!(action_15)),
        114 => partial_5!(happyGoto, curry_1_5!(action_16)),
        115 => partial_5!(happyGoto, curry_1_5!(action_17)),
        116 => partial_5!(happyGoto, curry_1_5!(action_18)),
        117 => partial_5!(happyGoto, curry_1_5!(action_19)),
        118 => partial_5!(happyGoto, curry_1_5!(action_20)),
        119 => partial_5!(happyGoto, curry_1_5!(action_21)),
        120 => partial_5!(happyGoto, curry_1_5!(action_362)),
        123 => partial_5!(happyGoto, curry_1_5!(action_363)),
        127 => partial_5!(happyGoto, curry_1_5!(action_24)),
        128 => partial_5!(happyGoto, curry_1_5!(action_25)),
        _ => box happyFail
    }
}

fn action_295(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_26)),
        144 => partial_5!(happyShift, curry_1_5!(action_27)),
        145 => partial_5!(happyShift, curry_1_5!(action_28)),
        146 => partial_5!(happyShift, curry_1_5!(action_29)),
        147 => partial_5!(happyShift, curry_1_5!(action_30)),
        148 => partial_5!(happyShift, curry_1_5!(action_31)),
        149 => partial_5!(happyShift, curry_1_5!(action_32)),
        150 => partial_5!(happyShift, curry_1_5!(action_33)),
        153 => partial_5!(happyShift, curry_1_5!(action_34)),
        164 => partial_5!(happyShift, curry_1_5!(action_35)),
        184 => partial_5!(happyShift, curry_1_5!(action_36)),
        204 => partial_5!(happyShift, curry_1_5!(action_37)),
        220 => partial_5!(happyShift, curry_1_5!(action_38)),
        233 => partial_5!(happyShift, curry_1_5!(action_39)),
        234 => partial_5!(happyShift, curry_1_5!(action_40)),
        235 => partial_5!(happyShift, curry_1_5!(action_41)),
        236 => partial_5!(happyShift, curry_1_5!(action_42)),
        237 => partial_5!(happyShift, curry_1_5!(action_43)),
        240 => partial_5!(happyShift, curry_1_5!(action_44)),
        241 => partial_5!(happyShift, curry_1_5!(action_45)),
        242 => partial_5!(happyShift, curry_1_5!(action_46)),
        243 => partial_5!(happyShift, curry_1_5!(action_47)),
        244 => partial_5!(happyShift, curry_1_5!(action_48)),
        245 => partial_5!(happyShift, curry_1_5!(action_49)),
        100 => partial_5!(happyGoto, curry_1_5!(action_6)),
        104 => partial_5!(happyGoto, curry_1_5!(action_7)),
        106 => partial_5!(happyGoto, curry_1_5!(action_259)),
        107 => partial_5!(happyGoto, curry_1_5!(action_9)),
        108 => partial_5!(happyGoto, curry_1_5!(action_10)),
        109 => partial_5!(happyGoto, curry_1_5!(action_11)),
        110 => partial_5!(happyGoto, curry_1_5!(action_12)),
        111 => partial_5!(happyGoto, curry_1_5!(action_13)),
        112 => partial_5!(happyGoto, curry_1_5!(action_14)),
        113 => partial_5!(happyGoto, curry_1_5!(action_15)),
        114 => partial_5!(happyGoto, curry_1_5!(action_16)),
        115 => partial_5!(happyGoto, curry_1_5!(action_17)),
        116 => partial_5!(happyGoto, curry_1_5!(action_18)),
        117 => partial_5!(happyGoto, curry_1_5!(action_361)),
        127 => partial_5!(happyGoto, curry_1_5!(action_24)),
        128 => partial_5!(happyGoto, curry_1_5!(action_25)),
        _ => box happyFail
    }
}

fn action_296(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_26)),
        144 => partial_5!(happyShift, curry_1_5!(action_27)),
        145 => partial_5!(happyShift, curry_1_5!(action_28)),
        146 => partial_5!(happyShift, curry_1_5!(action_29)),
        147 => partial_5!(happyShift, curry_1_5!(action_30)),
        148 => partial_5!(happyShift, curry_1_5!(action_31)),
        149 => partial_5!(happyShift, curry_1_5!(action_32)),
        150 => partial_5!(happyShift, curry_1_5!(action_33)),
        153 => partial_5!(happyShift, curry_1_5!(action_34)),
        164 => partial_5!(happyShift, curry_1_5!(action_35)),
        167 => partial_5!(happyShift, curry_1_5!(action_360)),
        184 => partial_5!(happyShift, curry_1_5!(action_36)),
        204 => partial_5!(happyShift, curry_1_5!(action_37)),
        220 => partial_5!(happyShift, curry_1_5!(action_38)),
        233 => partial_5!(happyShift, curry_1_5!(action_39)),
        234 => partial_5!(happyShift, curry_1_5!(action_40)),
        235 => partial_5!(happyShift, curry_1_5!(action_41)),
        236 => partial_5!(happyShift, curry_1_5!(action_42)),
        237 => partial_5!(happyShift, curry_1_5!(action_43)),
        240 => partial_5!(happyShift, curry_1_5!(action_44)),
        241 => partial_5!(happyShift, curry_1_5!(action_45)),
        242 => partial_5!(happyShift, curry_1_5!(action_46)),
        243 => partial_5!(happyShift, curry_1_5!(action_47)),
        244 => partial_5!(happyShift, curry_1_5!(action_48)),
        245 => partial_5!(happyShift, curry_1_5!(action_49)),
        100 => partial_5!(happyGoto, curry_1_5!(action_6)),
        104 => partial_5!(happyGoto, curry_1_5!(action_7)),
        106 => partial_5!(happyGoto, curry_1_5!(action_8)),
        107 => partial_5!(happyGoto, curry_1_5!(action_9)),
        108 => partial_5!(happyGoto, curry_1_5!(action_10)),
        109 => partial_5!(happyGoto, curry_1_5!(action_11)),
        110 => partial_5!(happyGoto, curry_1_5!(action_12)),
        111 => partial_5!(happyGoto, curry_1_5!(action_13)),
        112 => partial_5!(happyGoto, curry_1_5!(action_14)),
        113 => partial_5!(happyGoto, curry_1_5!(action_15)),
        114 => partial_5!(happyGoto, curry_1_5!(action_16)),
        115 => partial_5!(happyGoto, curry_1_5!(action_17)),
        116 => partial_5!(happyGoto, curry_1_5!(action_18)),
        117 => partial_5!(happyGoto, curry_1_5!(action_19)),
        118 => partial_5!(happyGoto, curry_1_5!(action_20)),
        119 => partial_5!(happyGoto, curry_1_5!(action_21)),
        120 => partial_5!(happyGoto, curry_1_5!(action_22)),
        122 => partial_5!(happyGoto, curry_1_5!(action_359)),
        127 => partial_5!(happyGoto, curry_1_5!(action_24)),
        128 => partial_5!(happyGoto, curry_1_5!(action_25)),
        _ => box happyFail
    }
}

fn action_297(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_26)),
        144 => partial_5!(happyShift, curry_1_5!(action_27)),
        145 => partial_5!(happyShift, curry_1_5!(action_28)),
        146 => partial_5!(happyShift, curry_1_5!(action_29)),
        147 => partial_5!(happyShift, curry_1_5!(action_30)),
        148 => partial_5!(happyShift, curry_1_5!(action_31)),
        149 => partial_5!(happyShift, curry_1_5!(action_32)),
        150 => partial_5!(happyShift, curry_1_5!(action_33)),
        153 => partial_5!(happyShift, curry_1_5!(action_34)),
        164 => partial_5!(happyShift, curry_1_5!(action_35)),
        184 => partial_5!(happyShift, curry_1_5!(action_36)),
        204 => partial_5!(happyShift, curry_1_5!(action_37)),
        220 => partial_5!(happyShift, curry_1_5!(action_38)),
        233 => partial_5!(happyShift, curry_1_5!(action_39)),
        234 => partial_5!(happyShift, curry_1_5!(action_40)),
        235 => partial_5!(happyShift, curry_1_5!(action_41)),
        236 => partial_5!(happyShift, curry_1_5!(action_42)),
        237 => partial_5!(happyShift, curry_1_5!(action_43)),
        240 => partial_5!(happyShift, curry_1_5!(action_44)),
        241 => partial_5!(happyShift, curry_1_5!(action_45)),
        242 => partial_5!(happyShift, curry_1_5!(action_46)),
        243 => partial_5!(happyShift, curry_1_5!(action_47)),
        244 => partial_5!(happyShift, curry_1_5!(action_48)),
        245 => partial_5!(happyShift, curry_1_5!(action_49)),
        100 => partial_5!(happyGoto, curry_1_5!(action_6)),
        104 => partial_5!(happyGoto, curry_1_5!(action_7)),
        106 => partial_5!(happyGoto, curry_1_5!(action_259)),
        107 => partial_5!(happyGoto, curry_1_5!(action_9)),
        108 => partial_5!(happyGoto, curry_1_5!(action_10)),
        109 => partial_5!(happyGoto, curry_1_5!(action_11)),
        110 => partial_5!(happyGoto, curry_1_5!(action_12)),
        111 => partial_5!(happyGoto, curry_1_5!(action_13)),
        112 => partial_5!(happyGoto, curry_1_5!(action_14)),
        113 => partial_5!(happyGoto, curry_1_5!(action_15)),
        114 => partial_5!(happyGoto, curry_1_5!(action_16)),
        115 => partial_5!(happyGoto, curry_1_5!(action_17)),
        116 => partial_5!(happyGoto, curry_1_5!(action_358)),
        127 => partial_5!(happyGoto, curry_1_5!(action_24)),
        128 => partial_5!(happyGoto, curry_1_5!(action_25)),
        _ => box happyFail
    }
}

fn action_298(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_26)),
        144 => partial_5!(happyShift, curry_1_5!(action_27)),
        145 => partial_5!(happyShift, curry_1_5!(action_28)),
        146 => partial_5!(happyShift, curry_1_5!(action_29)),
        147 => partial_5!(happyShift, curry_1_5!(action_30)),
        148 => partial_5!(happyShift, curry_1_5!(action_31)),
        149 => partial_5!(happyShift, curry_1_5!(action_32)),
        150 => partial_5!(happyShift, curry_1_5!(action_33)),
        153 => partial_5!(happyShift, curry_1_5!(action_34)),
        164 => partial_5!(happyShift, curry_1_5!(action_35)),
        184 => partial_5!(happyShift, curry_1_5!(action_36)),
        204 => partial_5!(happyShift, curry_1_5!(action_37)),
        220 => partial_5!(happyShift, curry_1_5!(action_38)),
        233 => partial_5!(happyShift, curry_1_5!(action_39)),
        234 => partial_5!(happyShift, curry_1_5!(action_40)),
        235 => partial_5!(happyShift, curry_1_5!(action_41)),
        236 => partial_5!(happyShift, curry_1_5!(action_42)),
        237 => partial_5!(happyShift, curry_1_5!(action_43)),
        240 => partial_5!(happyShift, curry_1_5!(action_44)),
        241 => partial_5!(happyShift, curry_1_5!(action_45)),
        242 => partial_5!(happyShift, curry_1_5!(action_46)),
        243 => partial_5!(happyShift, curry_1_5!(action_47)),
        244 => partial_5!(happyShift, curry_1_5!(action_48)),
        245 => partial_5!(happyShift, curry_1_5!(action_49)),
        100 => partial_5!(happyGoto, curry_1_5!(action_6)),
        104 => partial_5!(happyGoto, curry_1_5!(action_7)),
        106 => partial_5!(happyGoto, curry_1_5!(action_259)),
        107 => partial_5!(happyGoto, curry_1_5!(action_9)),
        108 => partial_5!(happyGoto, curry_1_5!(action_10)),
        109 => partial_5!(happyGoto, curry_1_5!(action_11)),
        110 => partial_5!(happyGoto, curry_1_5!(action_12)),
        111 => partial_5!(happyGoto, curry_1_5!(action_13)),
        112 => partial_5!(happyGoto, curry_1_5!(action_14)),
        113 => partial_5!(happyGoto, curry_1_5!(action_15)),
        114 => partial_5!(happyGoto, curry_1_5!(action_16)),
        115 => partial_5!(happyGoto, curry_1_5!(action_357)),
        127 => partial_5!(happyGoto, curry_1_5!(action_24)),
        128 => partial_5!(happyGoto, curry_1_5!(action_25)),
        _ => box happyFail
    }
}

fn action_299(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_26)),
        144 => partial_5!(happyShift, curry_1_5!(action_27)),
        145 => partial_5!(happyShift, curry_1_5!(action_28)),
        146 => partial_5!(happyShift, curry_1_5!(action_29)),
        147 => partial_5!(happyShift, curry_1_5!(action_30)),
        148 => partial_5!(happyShift, curry_1_5!(action_31)),
        149 => partial_5!(happyShift, curry_1_5!(action_32)),
        150 => partial_5!(happyShift, curry_1_5!(action_33)),
        153 => partial_5!(happyShift, curry_1_5!(action_34)),
        164 => partial_5!(happyShift, curry_1_5!(action_35)),
        184 => partial_5!(happyShift, curry_1_5!(action_36)),
        204 => partial_5!(happyShift, curry_1_5!(action_37)),
        220 => partial_5!(happyShift, curry_1_5!(action_38)),
        233 => partial_5!(happyShift, curry_1_5!(action_39)),
        234 => partial_5!(happyShift, curry_1_5!(action_40)),
        235 => partial_5!(happyShift, curry_1_5!(action_41)),
        236 => partial_5!(happyShift, curry_1_5!(action_42)),
        237 => partial_5!(happyShift, curry_1_5!(action_43)),
        240 => partial_5!(happyShift, curry_1_5!(action_44)),
        241 => partial_5!(happyShift, curry_1_5!(action_45)),
        242 => partial_5!(happyShift, curry_1_5!(action_46)),
        243 => partial_5!(happyShift, curry_1_5!(action_47)),
        244 => partial_5!(happyShift, curry_1_5!(action_48)),
        245 => partial_5!(happyShift, curry_1_5!(action_49)),
        100 => partial_5!(happyGoto, curry_1_5!(action_6)),
        104 => partial_5!(happyGoto, curry_1_5!(action_7)),
        106 => partial_5!(happyGoto, curry_1_5!(action_259)),
        107 => partial_5!(happyGoto, curry_1_5!(action_9)),
        108 => partial_5!(happyGoto, curry_1_5!(action_10)),
        109 => partial_5!(happyGoto, curry_1_5!(action_11)),
        110 => partial_5!(happyGoto, curry_1_5!(action_12)),
        111 => partial_5!(happyGoto, curry_1_5!(action_13)),
        112 => partial_5!(happyGoto, curry_1_5!(action_14)),
        113 => partial_5!(happyGoto, curry_1_5!(action_15)),
        114 => partial_5!(happyGoto, curry_1_5!(action_356)),
        127 => partial_5!(happyGoto, curry_1_5!(action_24)),
        128 => partial_5!(happyGoto, curry_1_5!(action_25)),
        _ => box happyFail
    }
}

fn action_300(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_26)),
        144 => partial_5!(happyShift, curry_1_5!(action_27)),
        145 => partial_5!(happyShift, curry_1_5!(action_28)),
        146 => partial_5!(happyShift, curry_1_5!(action_29)),
        147 => partial_5!(happyShift, curry_1_5!(action_30)),
        148 => partial_5!(happyShift, curry_1_5!(action_31)),
        149 => partial_5!(happyShift, curry_1_5!(action_32)),
        150 => partial_5!(happyShift, curry_1_5!(action_33)),
        153 => partial_5!(happyShift, curry_1_5!(action_34)),
        164 => partial_5!(happyShift, curry_1_5!(action_35)),
        184 => partial_5!(happyShift, curry_1_5!(action_36)),
        204 => partial_5!(happyShift, curry_1_5!(action_37)),
        220 => partial_5!(happyShift, curry_1_5!(action_38)),
        233 => partial_5!(happyShift, curry_1_5!(action_39)),
        234 => partial_5!(happyShift, curry_1_5!(action_40)),
        235 => partial_5!(happyShift, curry_1_5!(action_41)),
        236 => partial_5!(happyShift, curry_1_5!(action_42)),
        237 => partial_5!(happyShift, curry_1_5!(action_43)),
        240 => partial_5!(happyShift, curry_1_5!(action_44)),
        241 => partial_5!(happyShift, curry_1_5!(action_45)),
        242 => partial_5!(happyShift, curry_1_5!(action_46)),
        243 => partial_5!(happyShift, curry_1_5!(action_47)),
        244 => partial_5!(happyShift, curry_1_5!(action_48)),
        245 => partial_5!(happyShift, curry_1_5!(action_49)),
        100 => partial_5!(happyGoto, curry_1_5!(action_6)),
        104 => partial_5!(happyGoto, curry_1_5!(action_7)),
        106 => partial_5!(happyGoto, curry_1_5!(action_259)),
        107 => partial_5!(happyGoto, curry_1_5!(action_9)),
        108 => partial_5!(happyGoto, curry_1_5!(action_10)),
        109 => partial_5!(happyGoto, curry_1_5!(action_11)),
        110 => partial_5!(happyGoto, curry_1_5!(action_12)),
        111 => partial_5!(happyGoto, curry_1_5!(action_13)),
        112 => partial_5!(happyGoto, curry_1_5!(action_14)),
        113 => partial_5!(happyGoto, curry_1_5!(action_355)),
        127 => partial_5!(happyGoto, curry_1_5!(action_24)),
        128 => partial_5!(happyGoto, curry_1_5!(action_25)),
        _ => box happyFail
    }
}

fn action_301(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_26)),
        144 => partial_5!(happyShift, curry_1_5!(action_27)),
        145 => partial_5!(happyShift, curry_1_5!(action_28)),
        146 => partial_5!(happyShift, curry_1_5!(action_29)),
        147 => partial_5!(happyShift, curry_1_5!(action_30)),
        148 => partial_5!(happyShift, curry_1_5!(action_31)),
        149 => partial_5!(happyShift, curry_1_5!(action_32)),
        150 => partial_5!(happyShift, curry_1_5!(action_33)),
        153 => partial_5!(happyShift, curry_1_5!(action_34)),
        164 => partial_5!(happyShift, curry_1_5!(action_35)),
        184 => partial_5!(happyShift, curry_1_5!(action_36)),
        204 => partial_5!(happyShift, curry_1_5!(action_37)),
        220 => partial_5!(happyShift, curry_1_5!(action_38)),
        233 => partial_5!(happyShift, curry_1_5!(action_39)),
        234 => partial_5!(happyShift, curry_1_5!(action_40)),
        235 => partial_5!(happyShift, curry_1_5!(action_41)),
        236 => partial_5!(happyShift, curry_1_5!(action_42)),
        237 => partial_5!(happyShift, curry_1_5!(action_43)),
        240 => partial_5!(happyShift, curry_1_5!(action_44)),
        241 => partial_5!(happyShift, curry_1_5!(action_45)),
        242 => partial_5!(happyShift, curry_1_5!(action_46)),
        243 => partial_5!(happyShift, curry_1_5!(action_47)),
        244 => partial_5!(happyShift, curry_1_5!(action_48)),
        245 => partial_5!(happyShift, curry_1_5!(action_49)),
        100 => partial_5!(happyGoto, curry_1_5!(action_6)),
        104 => partial_5!(happyGoto, curry_1_5!(action_7)),
        106 => partial_5!(happyGoto, curry_1_5!(action_259)),
        107 => partial_5!(happyGoto, curry_1_5!(action_9)),
        108 => partial_5!(happyGoto, curry_1_5!(action_10)),
        109 => partial_5!(happyGoto, curry_1_5!(action_11)),
        110 => partial_5!(happyGoto, curry_1_5!(action_12)),
        111 => partial_5!(happyGoto, curry_1_5!(action_13)),
        112 => partial_5!(happyGoto, curry_1_5!(action_354)),
        127 => partial_5!(happyGoto, curry_1_5!(action_24)),
        128 => partial_5!(happyGoto, curry_1_5!(action_25)),
        _ => box happyFail
    }
}

fn action_302(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_26)),
        144 => partial_5!(happyShift, curry_1_5!(action_27)),
        145 => partial_5!(happyShift, curry_1_5!(action_28)),
        146 => partial_5!(happyShift, curry_1_5!(action_29)),
        147 => partial_5!(happyShift, curry_1_5!(action_30)),
        148 => partial_5!(happyShift, curry_1_5!(action_31)),
        149 => partial_5!(happyShift, curry_1_5!(action_32)),
        150 => partial_5!(happyShift, curry_1_5!(action_33)),
        153 => partial_5!(happyShift, curry_1_5!(action_34)),
        164 => partial_5!(happyShift, curry_1_5!(action_35)),
        184 => partial_5!(happyShift, curry_1_5!(action_36)),
        204 => partial_5!(happyShift, curry_1_5!(action_37)),
        220 => partial_5!(happyShift, curry_1_5!(action_38)),
        233 => partial_5!(happyShift, curry_1_5!(action_39)),
        234 => partial_5!(happyShift, curry_1_5!(action_40)),
        235 => partial_5!(happyShift, curry_1_5!(action_41)),
        236 => partial_5!(happyShift, curry_1_5!(action_42)),
        237 => partial_5!(happyShift, curry_1_5!(action_43)),
        240 => partial_5!(happyShift, curry_1_5!(action_44)),
        241 => partial_5!(happyShift, curry_1_5!(action_45)),
        242 => partial_5!(happyShift, curry_1_5!(action_46)),
        243 => partial_5!(happyShift, curry_1_5!(action_47)),
        244 => partial_5!(happyShift, curry_1_5!(action_48)),
        245 => partial_5!(happyShift, curry_1_5!(action_49)),
        100 => partial_5!(happyGoto, curry_1_5!(action_6)),
        104 => partial_5!(happyGoto, curry_1_5!(action_7)),
        106 => partial_5!(happyGoto, curry_1_5!(action_259)),
        107 => partial_5!(happyGoto, curry_1_5!(action_9)),
        108 => partial_5!(happyGoto, curry_1_5!(action_10)),
        109 => partial_5!(happyGoto, curry_1_5!(action_11)),
        110 => partial_5!(happyGoto, curry_1_5!(action_12)),
        111 => partial_5!(happyGoto, curry_1_5!(action_13)),
        112 => partial_5!(happyGoto, curry_1_5!(action_353)),
        127 => partial_5!(happyGoto, curry_1_5!(action_24)),
        128 => partial_5!(happyGoto, curry_1_5!(action_25)),
        _ => box happyFail
    }
}

fn action_303(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_26)),
        144 => partial_5!(happyShift, curry_1_5!(action_27)),
        145 => partial_5!(happyShift, curry_1_5!(action_28)),
        146 => partial_5!(happyShift, curry_1_5!(action_29)),
        147 => partial_5!(happyShift, curry_1_5!(action_30)),
        148 => partial_5!(happyShift, curry_1_5!(action_31)),
        149 => partial_5!(happyShift, curry_1_5!(action_32)),
        150 => partial_5!(happyShift, curry_1_5!(action_33)),
        153 => partial_5!(happyShift, curry_1_5!(action_34)),
        164 => partial_5!(happyShift, curry_1_5!(action_35)),
        184 => partial_5!(happyShift, curry_1_5!(action_36)),
        204 => partial_5!(happyShift, curry_1_5!(action_37)),
        220 => partial_5!(happyShift, curry_1_5!(action_38)),
        233 => partial_5!(happyShift, curry_1_5!(action_39)),
        234 => partial_5!(happyShift, curry_1_5!(action_40)),
        235 => partial_5!(happyShift, curry_1_5!(action_41)),
        236 => partial_5!(happyShift, curry_1_5!(action_42)),
        237 => partial_5!(happyShift, curry_1_5!(action_43)),
        240 => partial_5!(happyShift, curry_1_5!(action_44)),
        241 => partial_5!(happyShift, curry_1_5!(action_45)),
        242 => partial_5!(happyShift, curry_1_5!(action_46)),
        243 => partial_5!(happyShift, curry_1_5!(action_47)),
        244 => partial_5!(happyShift, curry_1_5!(action_48)),
        245 => partial_5!(happyShift, curry_1_5!(action_49)),
        100 => partial_5!(happyGoto, curry_1_5!(action_6)),
        104 => partial_5!(happyGoto, curry_1_5!(action_7)),
        106 => partial_5!(happyGoto, curry_1_5!(action_259)),
        107 => partial_5!(happyGoto, curry_1_5!(action_9)),
        108 => partial_5!(happyGoto, curry_1_5!(action_10)),
        109 => partial_5!(happyGoto, curry_1_5!(action_11)),
        110 => partial_5!(happyGoto, curry_1_5!(action_12)),
        111 => partial_5!(happyGoto, curry_1_5!(action_352)),
        127 => partial_5!(happyGoto, curry_1_5!(action_24)),
        128 => partial_5!(happyGoto, curry_1_5!(action_25)),
        _ => box happyFail
    }
}

fn action_304(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_26)),
        144 => partial_5!(happyShift, curry_1_5!(action_27)),
        145 => partial_5!(happyShift, curry_1_5!(action_28)),
        146 => partial_5!(happyShift, curry_1_5!(action_29)),
        147 => partial_5!(happyShift, curry_1_5!(action_30)),
        148 => partial_5!(happyShift, curry_1_5!(action_31)),
        149 => partial_5!(happyShift, curry_1_5!(action_32)),
        150 => partial_5!(happyShift, curry_1_5!(action_33)),
        153 => partial_5!(happyShift, curry_1_5!(action_34)),
        164 => partial_5!(happyShift, curry_1_5!(action_35)),
        184 => partial_5!(happyShift, curry_1_5!(action_36)),
        204 => partial_5!(happyShift, curry_1_5!(action_37)),
        220 => partial_5!(happyShift, curry_1_5!(action_38)),
        233 => partial_5!(happyShift, curry_1_5!(action_39)),
        234 => partial_5!(happyShift, curry_1_5!(action_40)),
        235 => partial_5!(happyShift, curry_1_5!(action_41)),
        236 => partial_5!(happyShift, curry_1_5!(action_42)),
        237 => partial_5!(happyShift, curry_1_5!(action_43)),
        240 => partial_5!(happyShift, curry_1_5!(action_44)),
        241 => partial_5!(happyShift, curry_1_5!(action_45)),
        242 => partial_5!(happyShift, curry_1_5!(action_46)),
        243 => partial_5!(happyShift, curry_1_5!(action_47)),
        244 => partial_5!(happyShift, curry_1_5!(action_48)),
        245 => partial_5!(happyShift, curry_1_5!(action_49)),
        100 => partial_5!(happyGoto, curry_1_5!(action_6)),
        104 => partial_5!(happyGoto, curry_1_5!(action_7)),
        106 => partial_5!(happyGoto, curry_1_5!(action_259)),
        107 => partial_5!(happyGoto, curry_1_5!(action_9)),
        108 => partial_5!(happyGoto, curry_1_5!(action_10)),
        109 => partial_5!(happyGoto, curry_1_5!(action_11)),
        110 => partial_5!(happyGoto, curry_1_5!(action_12)),
        111 => partial_5!(happyGoto, curry_1_5!(action_351)),
        127 => partial_5!(happyGoto, curry_1_5!(action_24)),
        128 => partial_5!(happyGoto, curry_1_5!(action_25)),
        _ => box happyFail
    }
}

fn action_305(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_26)),
        144 => partial_5!(happyShift, curry_1_5!(action_27)),
        145 => partial_5!(happyShift, curry_1_5!(action_28)),
        146 => partial_5!(happyShift, curry_1_5!(action_29)),
        147 => partial_5!(happyShift, curry_1_5!(action_30)),
        148 => partial_5!(happyShift, curry_1_5!(action_31)),
        149 => partial_5!(happyShift, curry_1_5!(action_32)),
        150 => partial_5!(happyShift, curry_1_5!(action_33)),
        153 => partial_5!(happyShift, curry_1_5!(action_34)),
        164 => partial_5!(happyShift, curry_1_5!(action_35)),
        184 => partial_5!(happyShift, curry_1_5!(action_36)),
        204 => partial_5!(happyShift, curry_1_5!(action_37)),
        220 => partial_5!(happyShift, curry_1_5!(action_38)),
        233 => partial_5!(happyShift, curry_1_5!(action_39)),
        234 => partial_5!(happyShift, curry_1_5!(action_40)),
        235 => partial_5!(happyShift, curry_1_5!(action_41)),
        236 => partial_5!(happyShift, curry_1_5!(action_42)),
        237 => partial_5!(happyShift, curry_1_5!(action_43)),
        240 => partial_5!(happyShift, curry_1_5!(action_44)),
        241 => partial_5!(happyShift, curry_1_5!(action_45)),
        242 => partial_5!(happyShift, curry_1_5!(action_46)),
        243 => partial_5!(happyShift, curry_1_5!(action_47)),
        244 => partial_5!(happyShift, curry_1_5!(action_48)),
        245 => partial_5!(happyShift, curry_1_5!(action_49)),
        100 => partial_5!(happyGoto, curry_1_5!(action_6)),
        104 => partial_5!(happyGoto, curry_1_5!(action_7)),
        106 => partial_5!(happyGoto, curry_1_5!(action_259)),
        107 => partial_5!(happyGoto, curry_1_5!(action_9)),
        108 => partial_5!(happyGoto, curry_1_5!(action_10)),
        109 => partial_5!(happyGoto, curry_1_5!(action_11)),
        110 => partial_5!(happyGoto, curry_1_5!(action_12)),
        111 => partial_5!(happyGoto, curry_1_5!(action_350)),
        127 => partial_5!(happyGoto, curry_1_5!(action_24)),
        128 => partial_5!(happyGoto, curry_1_5!(action_25)),
        _ => box happyFail
    }
}

fn action_306(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_26)),
        144 => partial_5!(happyShift, curry_1_5!(action_27)),
        145 => partial_5!(happyShift, curry_1_5!(action_28)),
        146 => partial_5!(happyShift, curry_1_5!(action_29)),
        147 => partial_5!(happyShift, curry_1_5!(action_30)),
        148 => partial_5!(happyShift, curry_1_5!(action_31)),
        149 => partial_5!(happyShift, curry_1_5!(action_32)),
        150 => partial_5!(happyShift, curry_1_5!(action_33)),
        153 => partial_5!(happyShift, curry_1_5!(action_34)),
        164 => partial_5!(happyShift, curry_1_5!(action_35)),
        184 => partial_5!(happyShift, curry_1_5!(action_36)),
        204 => partial_5!(happyShift, curry_1_5!(action_37)),
        220 => partial_5!(happyShift, curry_1_5!(action_38)),
        233 => partial_5!(happyShift, curry_1_5!(action_39)),
        234 => partial_5!(happyShift, curry_1_5!(action_40)),
        235 => partial_5!(happyShift, curry_1_5!(action_41)),
        236 => partial_5!(happyShift, curry_1_5!(action_42)),
        237 => partial_5!(happyShift, curry_1_5!(action_43)),
        240 => partial_5!(happyShift, curry_1_5!(action_44)),
        241 => partial_5!(happyShift, curry_1_5!(action_45)),
        242 => partial_5!(happyShift, curry_1_5!(action_46)),
        243 => partial_5!(happyShift, curry_1_5!(action_47)),
        244 => partial_5!(happyShift, curry_1_5!(action_48)),
        245 => partial_5!(happyShift, curry_1_5!(action_49)),
        100 => partial_5!(happyGoto, curry_1_5!(action_6)),
        104 => partial_5!(happyGoto, curry_1_5!(action_7)),
        106 => partial_5!(happyGoto, curry_1_5!(action_259)),
        107 => partial_5!(happyGoto, curry_1_5!(action_9)),
        108 => partial_5!(happyGoto, curry_1_5!(action_10)),
        109 => partial_5!(happyGoto, curry_1_5!(action_11)),
        110 => partial_5!(happyGoto, curry_1_5!(action_12)),
        111 => partial_5!(happyGoto, curry_1_5!(action_349)),
        127 => partial_5!(happyGoto, curry_1_5!(action_24)),
        128 => partial_5!(happyGoto, curry_1_5!(action_25)),
        _ => box happyFail
    }
}

fn action_307(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_26)),
        144 => partial_5!(happyShift, curry_1_5!(action_27)),
        145 => partial_5!(happyShift, curry_1_5!(action_28)),
        146 => partial_5!(happyShift, curry_1_5!(action_29)),
        147 => partial_5!(happyShift, curry_1_5!(action_30)),
        148 => partial_5!(happyShift, curry_1_5!(action_31)),
        149 => partial_5!(happyShift, curry_1_5!(action_32)),
        150 => partial_5!(happyShift, curry_1_5!(action_33)),
        153 => partial_5!(happyShift, curry_1_5!(action_34)),
        164 => partial_5!(happyShift, curry_1_5!(action_35)),
        184 => partial_5!(happyShift, curry_1_5!(action_36)),
        204 => partial_5!(happyShift, curry_1_5!(action_37)),
        220 => partial_5!(happyShift, curry_1_5!(action_38)),
        233 => partial_5!(happyShift, curry_1_5!(action_39)),
        234 => partial_5!(happyShift, curry_1_5!(action_40)),
        235 => partial_5!(happyShift, curry_1_5!(action_41)),
        236 => partial_5!(happyShift, curry_1_5!(action_42)),
        237 => partial_5!(happyShift, curry_1_5!(action_43)),
        240 => partial_5!(happyShift, curry_1_5!(action_44)),
        241 => partial_5!(happyShift, curry_1_5!(action_45)),
        242 => partial_5!(happyShift, curry_1_5!(action_46)),
        243 => partial_5!(happyShift, curry_1_5!(action_47)),
        244 => partial_5!(happyShift, curry_1_5!(action_48)),
        245 => partial_5!(happyShift, curry_1_5!(action_49)),
        100 => partial_5!(happyGoto, curry_1_5!(action_6)),
        104 => partial_5!(happyGoto, curry_1_5!(action_7)),
        106 => partial_5!(happyGoto, curry_1_5!(action_259)),
        107 => partial_5!(happyGoto, curry_1_5!(action_9)),
        108 => partial_5!(happyGoto, curry_1_5!(action_10)),
        109 => partial_5!(happyGoto, curry_1_5!(action_11)),
        110 => partial_5!(happyGoto, curry_1_5!(action_348)),
        127 => partial_5!(happyGoto, curry_1_5!(action_24)),
        128 => partial_5!(happyGoto, curry_1_5!(action_25)),
        _ => box happyFail
    }
}

fn action_308(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_26)),
        144 => partial_5!(happyShift, curry_1_5!(action_27)),
        145 => partial_5!(happyShift, curry_1_5!(action_28)),
        146 => partial_5!(happyShift, curry_1_5!(action_29)),
        147 => partial_5!(happyShift, curry_1_5!(action_30)),
        148 => partial_5!(happyShift, curry_1_5!(action_31)),
        149 => partial_5!(happyShift, curry_1_5!(action_32)),
        150 => partial_5!(happyShift, curry_1_5!(action_33)),
        153 => partial_5!(happyShift, curry_1_5!(action_34)),
        164 => partial_5!(happyShift, curry_1_5!(action_35)),
        184 => partial_5!(happyShift, curry_1_5!(action_36)),
        204 => partial_5!(happyShift, curry_1_5!(action_37)),
        220 => partial_5!(happyShift, curry_1_5!(action_38)),
        233 => partial_5!(happyShift, curry_1_5!(action_39)),
        234 => partial_5!(happyShift, curry_1_5!(action_40)),
        235 => partial_5!(happyShift, curry_1_5!(action_41)),
        236 => partial_5!(happyShift, curry_1_5!(action_42)),
        237 => partial_5!(happyShift, curry_1_5!(action_43)),
        240 => partial_5!(happyShift, curry_1_5!(action_44)),
        241 => partial_5!(happyShift, curry_1_5!(action_45)),
        242 => partial_5!(happyShift, curry_1_5!(action_46)),
        243 => partial_5!(happyShift, curry_1_5!(action_47)),
        244 => partial_5!(happyShift, curry_1_5!(action_48)),
        245 => partial_5!(happyShift, curry_1_5!(action_49)),
        100 => partial_5!(happyGoto, curry_1_5!(action_6)),
        104 => partial_5!(happyGoto, curry_1_5!(action_7)),
        106 => partial_5!(happyGoto, curry_1_5!(action_259)),
        107 => partial_5!(happyGoto, curry_1_5!(action_9)),
        108 => partial_5!(happyGoto, curry_1_5!(action_10)),
        109 => partial_5!(happyGoto, curry_1_5!(action_11)),
        110 => partial_5!(happyGoto, curry_1_5!(action_347)),
        127 => partial_5!(happyGoto, curry_1_5!(action_24)),
        128 => partial_5!(happyGoto, curry_1_5!(action_25)),
        _ => box happyFail
    }
}

fn action_309(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_26)),
        144 => partial_5!(happyShift, curry_1_5!(action_27)),
        145 => partial_5!(happyShift, curry_1_5!(action_28)),
        146 => partial_5!(happyShift, curry_1_5!(action_29)),
        147 => partial_5!(happyShift, curry_1_5!(action_30)),
        148 => partial_5!(happyShift, curry_1_5!(action_31)),
        149 => partial_5!(happyShift, curry_1_5!(action_32)),
        150 => partial_5!(happyShift, curry_1_5!(action_33)),
        153 => partial_5!(happyShift, curry_1_5!(action_34)),
        164 => partial_5!(happyShift, curry_1_5!(action_35)),
        184 => partial_5!(happyShift, curry_1_5!(action_36)),
        204 => partial_5!(happyShift, curry_1_5!(action_37)),
        220 => partial_5!(happyShift, curry_1_5!(action_38)),
        233 => partial_5!(happyShift, curry_1_5!(action_39)),
        234 => partial_5!(happyShift, curry_1_5!(action_40)),
        235 => partial_5!(happyShift, curry_1_5!(action_41)),
        236 => partial_5!(happyShift, curry_1_5!(action_42)),
        237 => partial_5!(happyShift, curry_1_5!(action_43)),
        240 => partial_5!(happyShift, curry_1_5!(action_44)),
        241 => partial_5!(happyShift, curry_1_5!(action_45)),
        242 => partial_5!(happyShift, curry_1_5!(action_46)),
        243 => partial_5!(happyShift, curry_1_5!(action_47)),
        244 => partial_5!(happyShift, curry_1_5!(action_48)),
        245 => partial_5!(happyShift, curry_1_5!(action_49)),
        100 => partial_5!(happyGoto, curry_1_5!(action_6)),
        104 => partial_5!(happyGoto, curry_1_5!(action_7)),
        106 => partial_5!(happyGoto, curry_1_5!(action_259)),
        107 => partial_5!(happyGoto, curry_1_5!(action_9)),
        108 => partial_5!(happyGoto, curry_1_5!(action_10)),
        109 => partial_5!(happyGoto, curry_1_5!(action_346)),
        127 => partial_5!(happyGoto, curry_1_5!(action_24)),
        128 => partial_5!(happyGoto, curry_1_5!(action_25)),
        _ => box happyFail
    }
}

fn action_310(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_26)),
        144 => partial_5!(happyShift, curry_1_5!(action_27)),
        145 => partial_5!(happyShift, curry_1_5!(action_28)),
        146 => partial_5!(happyShift, curry_1_5!(action_29)),
        147 => partial_5!(happyShift, curry_1_5!(action_30)),
        148 => partial_5!(happyShift, curry_1_5!(action_31)),
        149 => partial_5!(happyShift, curry_1_5!(action_32)),
        150 => partial_5!(happyShift, curry_1_5!(action_33)),
        153 => partial_5!(happyShift, curry_1_5!(action_34)),
        164 => partial_5!(happyShift, curry_1_5!(action_35)),
        184 => partial_5!(happyShift, curry_1_5!(action_36)),
        204 => partial_5!(happyShift, curry_1_5!(action_37)),
        220 => partial_5!(happyShift, curry_1_5!(action_38)),
        233 => partial_5!(happyShift, curry_1_5!(action_39)),
        234 => partial_5!(happyShift, curry_1_5!(action_40)),
        235 => partial_5!(happyShift, curry_1_5!(action_41)),
        236 => partial_5!(happyShift, curry_1_5!(action_42)),
        237 => partial_5!(happyShift, curry_1_5!(action_43)),
        240 => partial_5!(happyShift, curry_1_5!(action_44)),
        241 => partial_5!(happyShift, curry_1_5!(action_45)),
        242 => partial_5!(happyShift, curry_1_5!(action_46)),
        243 => partial_5!(happyShift, curry_1_5!(action_47)),
        244 => partial_5!(happyShift, curry_1_5!(action_48)),
        245 => partial_5!(happyShift, curry_1_5!(action_49)),
        100 => partial_5!(happyGoto, curry_1_5!(action_6)),
        104 => partial_5!(happyGoto, curry_1_5!(action_7)),
        106 => partial_5!(happyGoto, curry_1_5!(action_259)),
        107 => partial_5!(happyGoto, curry_1_5!(action_9)),
        108 => partial_5!(happyGoto, curry_1_5!(action_10)),
        109 => partial_5!(happyGoto, curry_1_5!(action_345)),
        127 => partial_5!(happyGoto, curry_1_5!(action_24)),
        128 => partial_5!(happyGoto, curry_1_5!(action_25)),
        _ => box happyFail
    }
}

fn action_311(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_26)),
        144 => partial_5!(happyShift, curry_1_5!(action_27)),
        145 => partial_5!(happyShift, curry_1_5!(action_28)),
        146 => partial_5!(happyShift, curry_1_5!(action_29)),
        147 => partial_5!(happyShift, curry_1_5!(action_30)),
        148 => partial_5!(happyShift, curry_1_5!(action_31)),
        149 => partial_5!(happyShift, curry_1_5!(action_32)),
        150 => partial_5!(happyShift, curry_1_5!(action_33)),
        153 => partial_5!(happyShift, curry_1_5!(action_34)),
        164 => partial_5!(happyShift, curry_1_5!(action_35)),
        184 => partial_5!(happyShift, curry_1_5!(action_36)),
        204 => partial_5!(happyShift, curry_1_5!(action_37)),
        220 => partial_5!(happyShift, curry_1_5!(action_38)),
        233 => partial_5!(happyShift, curry_1_5!(action_39)),
        234 => partial_5!(happyShift, curry_1_5!(action_40)),
        235 => partial_5!(happyShift, curry_1_5!(action_41)),
        236 => partial_5!(happyShift, curry_1_5!(action_42)),
        237 => partial_5!(happyShift, curry_1_5!(action_43)),
        240 => partial_5!(happyShift, curry_1_5!(action_44)),
        241 => partial_5!(happyShift, curry_1_5!(action_45)),
        242 => partial_5!(happyShift, curry_1_5!(action_46)),
        243 => partial_5!(happyShift, curry_1_5!(action_47)),
        244 => partial_5!(happyShift, curry_1_5!(action_48)),
        245 => partial_5!(happyShift, curry_1_5!(action_49)),
        100 => partial_5!(happyGoto, curry_1_5!(action_6)),
        104 => partial_5!(happyGoto, curry_1_5!(action_7)),
        106 => partial_5!(happyGoto, curry_1_5!(action_259)),
        107 => partial_5!(happyGoto, curry_1_5!(action_9)),
        108 => partial_5!(happyGoto, curry_1_5!(action_344)),
        127 => partial_5!(happyGoto, curry_1_5!(action_24)),
        128 => partial_5!(happyGoto, curry_1_5!(action_25)),
        _ => box happyFail
    }
}

fn action_312(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_26)),
        144 => partial_5!(happyShift, curry_1_5!(action_27)),
        145 => partial_5!(happyShift, curry_1_5!(action_28)),
        146 => partial_5!(happyShift, curry_1_5!(action_29)),
        147 => partial_5!(happyShift, curry_1_5!(action_30)),
        148 => partial_5!(happyShift, curry_1_5!(action_31)),
        149 => partial_5!(happyShift, curry_1_5!(action_32)),
        150 => partial_5!(happyShift, curry_1_5!(action_33)),
        153 => partial_5!(happyShift, curry_1_5!(action_34)),
        164 => partial_5!(happyShift, curry_1_5!(action_35)),
        184 => partial_5!(happyShift, curry_1_5!(action_36)),
        204 => partial_5!(happyShift, curry_1_5!(action_37)),
        220 => partial_5!(happyShift, curry_1_5!(action_38)),
        233 => partial_5!(happyShift, curry_1_5!(action_39)),
        234 => partial_5!(happyShift, curry_1_5!(action_40)),
        235 => partial_5!(happyShift, curry_1_5!(action_41)),
        236 => partial_5!(happyShift, curry_1_5!(action_42)),
        237 => partial_5!(happyShift, curry_1_5!(action_43)),
        240 => partial_5!(happyShift, curry_1_5!(action_44)),
        241 => partial_5!(happyShift, curry_1_5!(action_45)),
        242 => partial_5!(happyShift, curry_1_5!(action_46)),
        243 => partial_5!(happyShift, curry_1_5!(action_47)),
        244 => partial_5!(happyShift, curry_1_5!(action_48)),
        245 => partial_5!(happyShift, curry_1_5!(action_49)),
        100 => partial_5!(happyGoto, curry_1_5!(action_6)),
        104 => partial_5!(happyGoto, curry_1_5!(action_7)),
        106 => partial_5!(happyGoto, curry_1_5!(action_259)),
        107 => partial_5!(happyGoto, curry_1_5!(action_9)),
        108 => partial_5!(happyGoto, curry_1_5!(action_343)),
        127 => partial_5!(happyGoto, curry_1_5!(action_24)),
        128 => partial_5!(happyGoto, curry_1_5!(action_25)),
        _ => box happyFail
    }
}

fn action_313(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_26)),
        144 => partial_5!(happyShift, curry_1_5!(action_27)),
        145 => partial_5!(happyShift, curry_1_5!(action_28)),
        146 => partial_5!(happyShift, curry_1_5!(action_29)),
        147 => partial_5!(happyShift, curry_1_5!(action_30)),
        148 => partial_5!(happyShift, curry_1_5!(action_31)),
        149 => partial_5!(happyShift, curry_1_5!(action_32)),
        150 => partial_5!(happyShift, curry_1_5!(action_33)),
        153 => partial_5!(happyShift, curry_1_5!(action_34)),
        164 => partial_5!(happyShift, curry_1_5!(action_35)),
        184 => partial_5!(happyShift, curry_1_5!(action_36)),
        204 => partial_5!(happyShift, curry_1_5!(action_37)),
        220 => partial_5!(happyShift, curry_1_5!(action_38)),
        233 => partial_5!(happyShift, curry_1_5!(action_39)),
        234 => partial_5!(happyShift, curry_1_5!(action_40)),
        235 => partial_5!(happyShift, curry_1_5!(action_41)),
        236 => partial_5!(happyShift, curry_1_5!(action_42)),
        237 => partial_5!(happyShift, curry_1_5!(action_43)),
        240 => partial_5!(happyShift, curry_1_5!(action_44)),
        241 => partial_5!(happyShift, curry_1_5!(action_45)),
        242 => partial_5!(happyShift, curry_1_5!(action_46)),
        243 => partial_5!(happyShift, curry_1_5!(action_47)),
        244 => partial_5!(happyShift, curry_1_5!(action_48)),
        245 => partial_5!(happyShift, curry_1_5!(action_49)),
        100 => partial_5!(happyGoto, curry_1_5!(action_6)),
        104 => partial_5!(happyGoto, curry_1_5!(action_7)),
        106 => partial_5!(happyGoto, curry_1_5!(action_259)),
        107 => partial_5!(happyGoto, curry_1_5!(action_9)),
        108 => partial_5!(happyGoto, curry_1_5!(action_342)),
        127 => partial_5!(happyGoto, curry_1_5!(action_24)),
        128 => partial_5!(happyGoto, curry_1_5!(action_25)),
        _ => box happyFail
    }
}

fn action_314(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_392()
    }
}

fn action_315(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_26)),
        144 => partial_5!(happyShift, curry_1_5!(action_27)),
        145 => partial_5!(happyShift, curry_1_5!(action_28)),
        146 => partial_5!(happyShift, curry_1_5!(action_29)),
        147 => partial_5!(happyShift, curry_1_5!(action_30)),
        148 => partial_5!(happyShift, curry_1_5!(action_31)),
        149 => partial_5!(happyShift, curry_1_5!(action_32)),
        150 => partial_5!(happyShift, curry_1_5!(action_33)),
        153 => partial_5!(happyShift, curry_1_5!(action_34)),
        164 => partial_5!(happyShift, curry_1_5!(action_35)),
        184 => partial_5!(happyShift, curry_1_5!(action_36)),
        204 => partial_5!(happyShift, curry_1_5!(action_37)),
        220 => partial_5!(happyShift, curry_1_5!(action_38)),
        233 => partial_5!(happyShift, curry_1_5!(action_39)),
        234 => partial_5!(happyShift, curry_1_5!(action_40)),
        235 => partial_5!(happyShift, curry_1_5!(action_41)),
        236 => partial_5!(happyShift, curry_1_5!(action_42)),
        237 => partial_5!(happyShift, curry_1_5!(action_43)),
        240 => partial_5!(happyShift, curry_1_5!(action_44)),
        241 => partial_5!(happyShift, curry_1_5!(action_45)),
        242 => partial_5!(happyShift, curry_1_5!(action_46)),
        243 => partial_5!(happyShift, curry_1_5!(action_47)),
        244 => partial_5!(happyShift, curry_1_5!(action_48)),
        245 => partial_5!(happyShift, curry_1_5!(action_49)),
        100 => partial_5!(happyGoto, curry_1_5!(action_6)),
        104 => partial_5!(happyGoto, curry_1_5!(action_7)),
        106 => partial_5!(happyGoto, curry_1_5!(action_8)),
        107 => partial_5!(happyGoto, curry_1_5!(action_9)),
        108 => partial_5!(happyGoto, curry_1_5!(action_10)),
        109 => partial_5!(happyGoto, curry_1_5!(action_11)),
        110 => partial_5!(happyGoto, curry_1_5!(action_12)),
        111 => partial_5!(happyGoto, curry_1_5!(action_13)),
        112 => partial_5!(happyGoto, curry_1_5!(action_14)),
        113 => partial_5!(happyGoto, curry_1_5!(action_15)),
        114 => partial_5!(happyGoto, curry_1_5!(action_16)),
        115 => partial_5!(happyGoto, curry_1_5!(action_17)),
        116 => partial_5!(happyGoto, curry_1_5!(action_18)),
        117 => partial_5!(happyGoto, curry_1_5!(action_19)),
        118 => partial_5!(happyGoto, curry_1_5!(action_20)),
        119 => partial_5!(happyGoto, curry_1_5!(action_21)),
        120 => partial_5!(happyGoto, curry_1_5!(action_341)),
        127 => partial_5!(happyGoto, curry_1_5!(action_24)),
        128 => partial_5!(happyGoto, curry_1_5!(action_25)),
        _ => box happyFail
    }
}

fn action_316(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_441()
    }
}

fn action_317(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_445()
    }
}

fn action_318(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_446()
    }
}

fn action_319(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_442()
    }
}

fn action_320(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_443()
    }
}

fn action_321(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_444()
    }
}

fn action_322(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_449()
    }
}

fn action_323(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_450()
    }
}

fn action_324(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_451()
    }
}

fn action_325(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_447()
    }
}

fn action_326(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_448()
    }
}

fn action_327(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_26)),
        139 => partial_5!(happyShift, curry_1_5!(action_340)),
        144 => partial_5!(happyShift, curry_1_5!(action_27)),
        145 => partial_5!(happyShift, curry_1_5!(action_28)),
        146 => partial_5!(happyShift, curry_1_5!(action_29)),
        147 => partial_5!(happyShift, curry_1_5!(action_30)),
        148 => partial_5!(happyShift, curry_1_5!(action_31)),
        149 => partial_5!(happyShift, curry_1_5!(action_32)),
        150 => partial_5!(happyShift, curry_1_5!(action_33)),
        153 => partial_5!(happyShift, curry_1_5!(action_34)),
        164 => partial_5!(happyShift, curry_1_5!(action_35)),
        184 => partial_5!(happyShift, curry_1_5!(action_36)),
        204 => partial_5!(happyShift, curry_1_5!(action_37)),
        220 => partial_5!(happyShift, curry_1_5!(action_38)),
        233 => partial_5!(happyShift, curry_1_5!(action_39)),
        234 => partial_5!(happyShift, curry_1_5!(action_40)),
        235 => partial_5!(happyShift, curry_1_5!(action_41)),
        236 => partial_5!(happyShift, curry_1_5!(action_42)),
        237 => partial_5!(happyShift, curry_1_5!(action_43)),
        240 => partial_5!(happyShift, curry_1_5!(action_44)),
        241 => partial_5!(happyShift, curry_1_5!(action_45)),
        242 => partial_5!(happyShift, curry_1_5!(action_46)),
        243 => partial_5!(happyShift, curry_1_5!(action_47)),
        244 => partial_5!(happyShift, curry_1_5!(action_48)),
        245 => partial_5!(happyShift, curry_1_5!(action_49)),
        100 => partial_5!(happyGoto, curry_1_5!(action_6)),
        104 => partial_5!(happyGoto, curry_1_5!(action_7)),
        105 => partial_5!(happyGoto, curry_1_5!(action_338)),
        106 => partial_5!(happyGoto, curry_1_5!(action_8)),
        107 => partial_5!(happyGoto, curry_1_5!(action_9)),
        108 => partial_5!(happyGoto, curry_1_5!(action_10)),
        109 => partial_5!(happyGoto, curry_1_5!(action_11)),
        110 => partial_5!(happyGoto, curry_1_5!(action_12)),
        111 => partial_5!(happyGoto, curry_1_5!(action_13)),
        112 => partial_5!(happyGoto, curry_1_5!(action_14)),
        113 => partial_5!(happyGoto, curry_1_5!(action_15)),
        114 => partial_5!(happyGoto, curry_1_5!(action_16)),
        115 => partial_5!(happyGoto, curry_1_5!(action_17)),
        116 => partial_5!(happyGoto, curry_1_5!(action_18)),
        117 => partial_5!(happyGoto, curry_1_5!(action_19)),
        118 => partial_5!(happyGoto, curry_1_5!(action_20)),
        119 => partial_5!(happyGoto, curry_1_5!(action_21)),
        120 => partial_5!(happyGoto, curry_1_5!(action_339)),
        127 => partial_5!(happyGoto, curry_1_5!(action_24)),
        128 => partial_5!(happyGoto, curry_1_5!(action_25)),
        _ => box happyFail
    }
}

fn action_328(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_26)),
        144 => partial_5!(happyShift, curry_1_5!(action_27)),
        145 => partial_5!(happyShift, curry_1_5!(action_28)),
        146 => partial_5!(happyShift, curry_1_5!(action_29)),
        147 => partial_5!(happyShift, curry_1_5!(action_30)),
        148 => partial_5!(happyShift, curry_1_5!(action_31)),
        149 => partial_5!(happyShift, curry_1_5!(action_32)),
        150 => partial_5!(happyShift, curry_1_5!(action_33)),
        153 => partial_5!(happyShift, curry_1_5!(action_34)),
        164 => partial_5!(happyShift, curry_1_5!(action_35)),
        184 => partial_5!(happyShift, curry_1_5!(action_36)),
        204 => partial_5!(happyShift, curry_1_5!(action_37)),
        220 => partial_5!(happyShift, curry_1_5!(action_38)),
        233 => partial_5!(happyShift, curry_1_5!(action_39)),
        234 => partial_5!(happyShift, curry_1_5!(action_40)),
        235 => partial_5!(happyShift, curry_1_5!(action_41)),
        236 => partial_5!(happyShift, curry_1_5!(action_42)),
        237 => partial_5!(happyShift, curry_1_5!(action_43)),
        240 => partial_5!(happyShift, curry_1_5!(action_44)),
        241 => partial_5!(happyShift, curry_1_5!(action_45)),
        242 => partial_5!(happyShift, curry_1_5!(action_46)),
        243 => partial_5!(happyShift, curry_1_5!(action_47)),
        244 => partial_5!(happyShift, curry_1_5!(action_48)),
        245 => partial_5!(happyShift, curry_1_5!(action_49)),
        100 => partial_5!(happyGoto, curry_1_5!(action_6)),
        104 => partial_5!(happyGoto, curry_1_5!(action_7)),
        106 => partial_5!(happyGoto, curry_1_5!(action_8)),
        107 => partial_5!(happyGoto, curry_1_5!(action_9)),
        108 => partial_5!(happyGoto, curry_1_5!(action_10)),
        109 => partial_5!(happyGoto, curry_1_5!(action_11)),
        110 => partial_5!(happyGoto, curry_1_5!(action_12)),
        111 => partial_5!(happyGoto, curry_1_5!(action_13)),
        112 => partial_5!(happyGoto, curry_1_5!(action_14)),
        113 => partial_5!(happyGoto, curry_1_5!(action_15)),
        114 => partial_5!(happyGoto, curry_1_5!(action_16)),
        115 => partial_5!(happyGoto, curry_1_5!(action_17)),
        116 => partial_5!(happyGoto, curry_1_5!(action_18)),
        117 => partial_5!(happyGoto, curry_1_5!(action_19)),
        118 => partial_5!(happyGoto, curry_1_5!(action_20)),
        119 => partial_5!(happyGoto, curry_1_5!(action_21)),
        120 => partial_5!(happyGoto, curry_1_5!(action_22)),
        122 => partial_5!(happyGoto, curry_1_5!(action_337)),
        127 => partial_5!(happyGoto, curry_1_5!(action_24)),
        128 => partial_5!(happyGoto, curry_1_5!(action_25)),
        _ => box happyFail
    }
}

fn action_329(i: isize) -> ActionReturn {
    match i {
        237 => partial_5!(happyShift, curry_1_5!(action_254)),
        238 => partial_5!(happyShift, curry_1_5!(action_75)),
        131 => partial_5!(happyGoto, curry_1_5!(action_336)),
        _ => box happyFail
    }
}

fn action_330(i: isize) -> ActionReturn {
    match i {
        237 => partial_5!(happyShift, curry_1_5!(action_254)),
        238 => partial_5!(happyShift, curry_1_5!(action_75)),
        131 => partial_5!(happyGoto, curry_1_5!(action_335)),
        _ => box happyFail
    }
}

fn action_331(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_382()
    }
}

fn action_332(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_383()
    }
}

fn action_333(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_7()
    }
}

fn action_334(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_6()
    }
}

fn action_335(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_380()
    }
}

fn action_336(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_381()
    }
}

fn action_337(i: isize) -> ActionReturn {
    match i {
        141 => partial_5!(happyShift, curry_1_5!(action_640)),
        _ => box happyFail
    }
}

fn action_338(i: isize) -> ActionReturn {
    match i {
        139 => partial_5!(happyShift, curry_1_5!(action_638)),
        179 => partial_5!(happyShift, curry_1_5!(action_639)),
        _ => box happyFail
    }
}

fn action_339(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_386()
    }
}

fn action_340(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_378()
    }
}

fn action_341(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_440()
    }
}

fn action_342(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_411()
    }
}

fn action_343(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_410()
    }
}

fn action_344(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_409()
    }
}

fn action_345(i: isize) -> ActionReturn {
    match i {
        150 => partial_5!(happyShift, curry_1_5!(action_311)),
        151 => partial_5!(happyShift, curry_1_5!(action_312)),
        152 => partial_5!(happyShift, curry_1_5!(action_313)),
        _ => happyReduce_414()
    }
}

fn action_346(i: isize) -> ActionReturn {
    match i {
        150 => partial_5!(happyShift, curry_1_5!(action_311)),
        151 => partial_5!(happyShift, curry_1_5!(action_312)),
        152 => partial_5!(happyShift, curry_1_5!(action_313)),
        _ => happyReduce_413()
    }
}

fn action_347(i: isize) -> ActionReturn {
    match i {
        148 => partial_5!(happyShift, curry_1_5!(action_309)),
        149 => partial_5!(happyShift, curry_1_5!(action_310)),
        _ => happyReduce_417()
    }
}

fn action_348(i: isize) -> ActionReturn {
    match i {
        148 => partial_5!(happyShift, curry_1_5!(action_309)),
        149 => partial_5!(happyShift, curry_1_5!(action_310)),
        _ => happyReduce_416()
    }
}

fn action_349(i: isize) -> ActionReturn {
    match i {
        154 => partial_5!(happyShift, curry_1_5!(action_307)),
        155 => partial_5!(happyShift, curry_1_5!(action_308)),
        _ => happyReduce_422()
    }
}

fn action_350(i: isize) -> ActionReturn {
    match i {
        154 => partial_5!(happyShift, curry_1_5!(action_307)),
        155 => partial_5!(happyShift, curry_1_5!(action_308)),
        _ => happyReduce_420()
    }
}

fn action_351(i: isize) -> ActionReturn {
    match i {
        154 => partial_5!(happyShift, curry_1_5!(action_307)),
        155 => partial_5!(happyShift, curry_1_5!(action_308)),
        _ => happyReduce_421()
    }
}

fn action_352(i: isize) -> ActionReturn {
    match i {
        154 => partial_5!(happyShift, curry_1_5!(action_307)),
        155 => partial_5!(happyShift, curry_1_5!(action_308)),
        _ => happyReduce_419()
    }
}

fn action_353(i: isize) -> ActionReturn {
    match i {
        156 => partial_5!(happyShift, curry_1_5!(action_303)),
        157 => partial_5!(happyShift, curry_1_5!(action_304)),
        158 => partial_5!(happyShift, curry_1_5!(action_305)),
        159 => partial_5!(happyShift, curry_1_5!(action_306)),
        _ => happyReduce_425()
    }
}

fn action_354(i: isize) -> ActionReturn {
    match i {
        156 => partial_5!(happyShift, curry_1_5!(action_303)),
        157 => partial_5!(happyShift, curry_1_5!(action_304)),
        158 => partial_5!(happyShift, curry_1_5!(action_305)),
        159 => partial_5!(happyShift, curry_1_5!(action_306)),
        _ => happyReduce_424()
    }
}

fn action_355(i: isize) -> ActionReturn {
    match i {
        160 => partial_5!(happyShift, curry_1_5!(action_301)),
        161 => partial_5!(happyShift, curry_1_5!(action_302)),
        _ => happyReduce_427()
    }
}

fn action_356(i: isize) -> ActionReturn {
    match i {
        153 => partial_5!(happyShift, curry_1_5!(action_300)),
        _ => happyReduce_429()
    }
}

fn action_357(i: isize) -> ActionReturn {
    match i {
        162 => partial_5!(happyShift, curry_1_5!(action_299)),
        _ => happyReduce_431()
    }
}

fn action_358(i: isize) -> ActionReturn {
    match i {
        163 => partial_5!(happyShift, curry_1_5!(action_298)),
        _ => happyReduce_433()
    }
}

fn action_359(i: isize) -> ActionReturn {
    match i {
        167 => partial_5!(happyShift, curry_1_5!(action_637)),
        _ => box happyFail
    }
}

fn action_360(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_26)),
        144 => partial_5!(happyShift, curry_1_5!(action_27)),
        145 => partial_5!(happyShift, curry_1_5!(action_28)),
        146 => partial_5!(happyShift, curry_1_5!(action_29)),
        147 => partial_5!(happyShift, curry_1_5!(action_30)),
        148 => partial_5!(happyShift, curry_1_5!(action_31)),
        149 => partial_5!(happyShift, curry_1_5!(action_32)),
        150 => partial_5!(happyShift, curry_1_5!(action_33)),
        153 => partial_5!(happyShift, curry_1_5!(action_34)),
        164 => partial_5!(happyShift, curry_1_5!(action_35)),
        184 => partial_5!(happyShift, curry_1_5!(action_36)),
        204 => partial_5!(happyShift, curry_1_5!(action_37)),
        220 => partial_5!(happyShift, curry_1_5!(action_38)),
        233 => partial_5!(happyShift, curry_1_5!(action_39)),
        234 => partial_5!(happyShift, curry_1_5!(action_40)),
        235 => partial_5!(happyShift, curry_1_5!(action_41)),
        236 => partial_5!(happyShift, curry_1_5!(action_42)),
        237 => partial_5!(happyShift, curry_1_5!(action_43)),
        240 => partial_5!(happyShift, curry_1_5!(action_44)),
        241 => partial_5!(happyShift, curry_1_5!(action_45)),
        242 => partial_5!(happyShift, curry_1_5!(action_46)),
        243 => partial_5!(happyShift, curry_1_5!(action_47)),
        244 => partial_5!(happyShift, curry_1_5!(action_48)),
        245 => partial_5!(happyShift, curry_1_5!(action_49)),
        100 => partial_5!(happyGoto, curry_1_5!(action_6)),
        104 => partial_5!(happyGoto, curry_1_5!(action_7)),
        106 => partial_5!(happyGoto, curry_1_5!(action_259)),
        107 => partial_5!(happyGoto, curry_1_5!(action_9)),
        108 => partial_5!(happyGoto, curry_1_5!(action_10)),
        109 => partial_5!(happyGoto, curry_1_5!(action_11)),
        110 => partial_5!(happyGoto, curry_1_5!(action_12)),
        111 => partial_5!(happyGoto, curry_1_5!(action_13)),
        112 => partial_5!(happyGoto, curry_1_5!(action_14)),
        113 => partial_5!(happyGoto, curry_1_5!(action_15)),
        114 => partial_5!(happyGoto, curry_1_5!(action_16)),
        115 => partial_5!(happyGoto, curry_1_5!(action_17)),
        116 => partial_5!(happyGoto, curry_1_5!(action_18)),
        117 => partial_5!(happyGoto, curry_1_5!(action_19)),
        118 => partial_5!(happyGoto, curry_1_5!(action_20)),
        119 => partial_5!(happyGoto, curry_1_5!(action_636)),
        127 => partial_5!(happyGoto, curry_1_5!(action_24)),
        128 => partial_5!(happyGoto, curry_1_5!(action_25)),
        _ => box happyFail
    }
}

fn action_361(i: isize) -> ActionReturn {
    match i {
        164 => partial_5!(happyShift, curry_1_5!(action_297)),
        _ => happyReduce_435()
    }
}

fn action_362(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_454()
    }
}

fn action_363(i: isize) -> ActionReturn {
    match i {
        179 => partial_5!(happyShift, curry_1_5!(action_635)),
        _ => happyReduce_453()
    }
}

fn action_364(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_363()
    }
}

fn action_365(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_26)),
        144 => partial_5!(happyShift, curry_1_5!(action_27)),
        145 => partial_5!(happyShift, curry_1_5!(action_28)),
        146 => partial_5!(happyShift, curry_1_5!(action_29)),
        147 => partial_5!(happyShift, curry_1_5!(action_30)),
        148 => partial_5!(happyShift, curry_1_5!(action_31)),
        149 => partial_5!(happyShift, curry_1_5!(action_32)),
        150 => partial_5!(happyShift, curry_1_5!(action_33)),
        153 => partial_5!(happyShift, curry_1_5!(action_34)),
        164 => partial_5!(happyShift, curry_1_5!(action_35)),
        181 => partial_5!(happyShift, curry_1_5!(action_634)),
        184 => partial_5!(happyShift, curry_1_5!(action_36)),
        204 => partial_5!(happyShift, curry_1_5!(action_37)),
        220 => partial_5!(happyShift, curry_1_5!(action_38)),
        233 => partial_5!(happyShift, curry_1_5!(action_39)),
        234 => partial_5!(happyShift, curry_1_5!(action_40)),
        235 => partial_5!(happyShift, curry_1_5!(action_41)),
        236 => partial_5!(happyShift, curry_1_5!(action_42)),
        237 => partial_5!(happyShift, curry_1_5!(action_43)),
        240 => partial_5!(happyShift, curry_1_5!(action_44)),
        241 => partial_5!(happyShift, curry_1_5!(action_45)),
        242 => partial_5!(happyShift, curry_1_5!(action_46)),
        243 => partial_5!(happyShift, curry_1_5!(action_47)),
        244 => partial_5!(happyShift, curry_1_5!(action_48)),
        245 => partial_5!(happyShift, curry_1_5!(action_49)),
        100 => partial_5!(happyGoto, curry_1_5!(action_6)),
        104 => partial_5!(happyGoto, curry_1_5!(action_7)),
        106 => partial_5!(happyGoto, curry_1_5!(action_259)),
        107 => partial_5!(happyGoto, curry_1_5!(action_9)),
        108 => partial_5!(happyGoto, curry_1_5!(action_633)),
        127 => partial_5!(happyGoto, curry_1_5!(action_24)),
        128 => partial_5!(happyGoto, curry_1_5!(action_25)),
        _ => box happyFail
    }
}

fn action_366(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_307()
    }
}

fn action_367(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_310()
    }
}

fn action_368(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_308()
    }
}

fn action_369(i: isize) -> ActionReturn {
    match i {
        239 => partial_5!(happyShift, curry_1_5!(action_142)),
        134 => partial_5!(happyGoto, curry_1_5!(action_632)),
        _ => happyReduce_309()
    }
}

fn action_370(i: isize) -> ActionReturn {
    match i {
        186 => partial_5!(happyShift, curry_1_5!(action_171)),
        190 => partial_5!(happyShift, curry_1_5!(action_116)),
        192 => partial_5!(happyShift, curry_1_5!(action_117)),
        193 => partial_5!(happyShift, curry_1_5!(action_172)),
        195 => partial_5!(happyShift, curry_1_5!(action_118)),
        198 => partial_5!(happyShift, curry_1_5!(action_119)),
        200 => partial_5!(happyShift, curry_1_5!(action_120)),
        202 => partial_5!(happyShift, curry_1_5!(action_122)),
        208 => partial_5!(happyShift, curry_1_5!(action_124)),
        209 => partial_5!(happyShift, curry_1_5!(action_125)),
        210 => partial_5!(happyShift, curry_1_5!(action_126)),
        213 => partial_5!(happyShift, curry_1_5!(action_173)),
        214 => partial_5!(happyShift, curry_1_5!(action_174)),
        216 => partial_5!(happyShift, curry_1_5!(action_175)),
        218 => partial_5!(happyShift, curry_1_5!(action_129)),
        219 => partial_5!(happyShift, curry_1_5!(action_130)),
        223 => partial_5!(happyShift, curry_1_5!(action_133)),
        226 => partial_5!(happyShift, curry_1_5!(action_438)),
        228 => partial_5!(happyShift, curry_1_5!(action_137)),
        229 => partial_5!(happyShift, curry_1_5!(action_138)),
        230 => partial_5!(happyShift, curry_1_5!(action_139)),
        231 => partial_5!(happyShift, curry_1_5!(action_176)),
        238 => partial_5!(happyShift, curry_1_5!(action_439)),
        239 => partial_5!(happyShift, curry_1_5!(action_142)),
        45 => partial_5!(happyGoto, curry_1_5!(action_433)),
        52 => partial_5!(happyGoto, curry_1_5!(action_434)),
        53 => partial_5!(happyGoto, curry_1_5!(action_97)),
        54 => partial_5!(happyGoto, curry_1_5!(action_98)),
        61 => partial_5!(happyGoto, curry_1_5!(action_99)),
        64 => partial_5!(happyGoto, curry_1_5!(action_435)),
        134 => partial_5!(happyGoto, curry_1_5!(action_167)),
        _ => box happyFail
    }
}

fn action_371(i: isize) -> ActionReturn {
    match i {
        186 => happyReduce_473(),
        190 => happyReduce_473(),
        192 => happyReduce_473(),
        193 => happyReduce_473(),
        195 => happyReduce_473(),
        198 => happyReduce_473(),
        200 => happyReduce_473(),
        202 => happyReduce_473(),
        208 => happyReduce_473(),
        209 => happyReduce_473(),
        210 => happyReduce_473(),
        213 => happyReduce_473(),
        214 => happyReduce_473(),
        216 => happyReduce_473(),
        218 => happyReduce_473(),
        219 => happyReduce_473(),
        223 => happyReduce_473(),
        226 => happyReduce_473(),
        228 => happyReduce_473(),
        229 => happyReduce_473(),
        230 => happyReduce_473(),
        231 => happyReduce_473(),
        238 => happyReduce_473(),
        239 => happyReduce_473(),
        _ => happyReduce_306()
    }
}

fn action_372(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_372)),
        140 => partial_5!(happyShift, curry_1_5!(action_182)),
        150 => partial_5!(happyShift, curry_1_5!(action_373)),
        185 => partial_5!(happyShift, curry_1_5!(action_113)),
        186 => happyReduce_471(),
        188 => partial_5!(happyShift, curry_1_5!(action_115)),
        190 => partial_5!(happyShift, curry_1_5!(action_116)),
        192 => partial_5!(happyShift, curry_1_5!(action_117)),
        193 => happyReduce_471(),
        195 => partial_5!(happyShift, curry_1_5!(action_118)),
        198 => partial_5!(happyShift, curry_1_5!(action_119)),
        200 => partial_5!(happyShift, curry_1_5!(action_120)),
        201 => partial_5!(happyShift, curry_1_5!(action_121)),
        202 => partial_5!(happyShift, curry_1_5!(action_122)),
        207 => partial_5!(happyShift, curry_1_5!(action_123)),
        208 => partial_5!(happyShift, curry_1_5!(action_124)),
        209 => partial_5!(happyShift, curry_1_5!(action_125)),
        210 => partial_5!(happyShift, curry_1_5!(action_126)),
        212 => partial_5!(happyShift, curry_1_5!(action_127)),
        213 => happyReduce_471(),
        214 => happyReduce_471(),
        215 => partial_5!(happyShift, curry_1_5!(action_128)),
        216 => happyReduce_471(),
        218 => partial_5!(happyShift, curry_1_5!(action_129)),
        219 => partial_5!(happyShift, curry_1_5!(action_130)),
        221 => partial_5!(happyShift, curry_1_5!(action_131)),
        223 => partial_5!(happyShift, curry_1_5!(action_133)),
        225 => partial_5!(happyShift, curry_1_5!(action_134)),
        226 => partial_5!(happyShift, curry_1_5!(action_135)),
        227 => partial_5!(happyShift, curry_1_5!(action_136)),
        228 => partial_5!(happyShift, curry_1_5!(action_137)),
        229 => partial_5!(happyShift, curry_1_5!(action_138)),
        230 => partial_5!(happyShift, curry_1_5!(action_139)),
        231 => happyReduce_471(),
        238 => partial_5!(happyShift, curry_1_5!(action_141)),
        239 => partial_5!(happyShift, curry_1_5!(action_142)),
        37 => partial_5!(happyGoto, curry_1_5!(action_449)),
        38 => partial_5!(happyGoto, curry_1_5!(action_450)),
        40 => partial_5!(happyGoto, curry_1_5!(action_84)),
        41 => partial_5!(happyGoto, curry_1_5!(action_85)),
        42 => partial_5!(happyGoto, curry_1_5!(action_86)),
        43 => partial_5!(happyGoto, curry_1_5!(action_87)),
        44 => partial_5!(happyGoto, curry_1_5!(action_451)),
        45 => partial_5!(happyGoto, curry_1_5!(action_89)),
        46 => partial_5!(happyGoto, curry_1_5!(action_90)),
        47 => partial_5!(happyGoto, curry_1_5!(action_91)),
        48 => partial_5!(happyGoto, curry_1_5!(action_452)),
        49 => partial_5!(happyGoto, curry_1_5!(action_453)),
        50 => partial_5!(happyGoto, curry_1_5!(action_94)),
        51 => partial_5!(happyGoto, curry_1_5!(action_95)),
        52 => partial_5!(happyGoto, curry_1_5!(action_96)),
        53 => partial_5!(happyGoto, curry_1_5!(action_97)),
        54 => partial_5!(happyGoto, curry_1_5!(action_98)),
        61 => partial_5!(happyGoto, curry_1_5!(action_99)),
        65 => partial_5!(happyGoto, curry_1_5!(action_454)),
        82 => partial_5!(happyGoto, curry_1_5!(action_455)),
        83 => partial_5!(happyGoto, curry_1_5!(action_456)),
        84 => partial_5!(happyGoto, curry_1_5!(action_457)),
        88 => partial_5!(happyGoto, curry_1_5!(action_628)),
        89 => partial_5!(happyGoto, curry_1_5!(action_179)),
        90 => partial_5!(happyGoto, curry_1_5!(action_180)),
        91 => partial_5!(happyGoto, curry_1_5!(action_629)),
        92 => partial_5!(happyGoto, curry_1_5!(action_630)),
        132 => partial_5!(happyGoto, curry_1_5!(action_108)),
        133 => partial_5!(happyGoto, curry_1_5!(action_631)),
        134 => partial_5!(happyGoto, curry_1_5!(action_110)),
        _ => happyReduce_282()
    }
}

fn action_373(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_372)),
        140 => partial_5!(happyShift, curry_1_5!(action_182)),
        150 => partial_5!(happyShift, curry_1_5!(action_373)),
        186 => happyReduce_471(),
        193 => happyReduce_471(),
        213 => happyReduce_471(),
        214 => happyReduce_471(),
        216 => happyReduce_471(),
        231 => happyReduce_471(),
        239 => partial_5!(happyShift, curry_1_5!(action_142)),
        65 => partial_5!(happyGoto, curry_1_5!(action_625)),
        87 => partial_5!(happyGoto, curry_1_5!(action_626)),
        88 => partial_5!(happyGoto, curry_1_5!(action_367)),
        89 => partial_5!(happyGoto, curry_1_5!(action_179)),
        90 => partial_5!(happyGoto, curry_1_5!(action_180)),
        91 => partial_5!(happyGoto, curry_1_5!(action_368)),
        92 => partial_5!(happyGoto, curry_1_5!(action_369)),
        132 => partial_5!(happyGoto, curry_1_5!(action_108)),
        133 => partial_5!(happyGoto, curry_1_5!(action_627)),
        134 => partial_5!(happyGoto, curry_1_5!(action_110)),
        _ => happyReduce_326()
    }
}

fn action_374(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_305()
    }
}

fn action_375(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_365()
    }
}

fn action_376(i: isize) -> ActionReturn {
    match i {
        139 => partial_5!(happyShift, curry_1_5!(action_624)),
        _ => box happyFail
    }
}

fn action_377(i: isize) -> ActionReturn {
    match i {
        179 => partial_5!(happyShift, curry_1_5!(action_623)),
        _ => box happyFail
    }
}

fn action_378(i: isize) -> ActionReturn {
    match i {
        139 => partial_5!(happyShift, curry_1_5!(action_622)),
        _ => box happyFail
    }
}

fn action_379(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_467()
    }
}

fn action_380(i: isize) -> ActionReturn {
    match i {
        139 => partial_5!(happyShift, curry_1_5!(action_621)),
        _ => box happyFail
    }
}

fn action_381(i: isize) -> ActionReturn {
    match i {
        179 => partial_5!(happyShift, curry_1_5!(action_620)),
        _ => box happyFail
    }
}

fn action_382(i: isize) -> ActionReturn {
    match i {
        179 => partial_5!(happyShift, curry_1_5!(action_619)),
        _ => box happyFail
    }
}

fn action_383(i: isize) -> ActionReturn {
    match i {
        179 => partial_5!(happyShift, curry_1_5!(action_618)),
        _ => box happyFail
    }
}

fn action_384(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_26)),
        144 => partial_5!(happyShift, curry_1_5!(action_27)),
        145 => partial_5!(happyShift, curry_1_5!(action_28)),
        146 => partial_5!(happyShift, curry_1_5!(action_29)),
        147 => partial_5!(happyShift, curry_1_5!(action_30)),
        148 => partial_5!(happyShift, curry_1_5!(action_31)),
        149 => partial_5!(happyShift, curry_1_5!(action_32)),
        150 => partial_5!(happyShift, curry_1_5!(action_33)),
        153 => partial_5!(happyShift, curry_1_5!(action_34)),
        164 => partial_5!(happyShift, curry_1_5!(action_35)),
        180 => partial_5!(happyShift, curry_1_5!(action_60)),
        181 => partial_5!(happyShift, curry_1_5!(action_61)),
        184 => partial_5!(happyShift, curry_1_5!(action_36)),
        187 => partial_5!(happyShift, curry_1_5!(action_62)),
        189 => partial_5!(happyShift, curry_1_5!(action_63)),
        191 => partial_5!(happyShift, curry_1_5!(action_64)),
        194 => partial_5!(happyShift, curry_1_5!(action_65)),
        196 => partial_5!(happyShift, curry_1_5!(action_66)),
        197 => partial_5!(happyShift, curry_1_5!(action_67)),
        203 => partial_5!(happyShift, curry_1_5!(action_68)),
        204 => partial_5!(happyShift, curry_1_5!(action_37)),
        205 => partial_5!(happyShift, curry_1_5!(action_69)),
        206 => partial_5!(happyShift, curry_1_5!(action_70)),
        217 => partial_5!(happyShift, curry_1_5!(action_71)),
        220 => partial_5!(happyShift, curry_1_5!(action_38)),
        224 => partial_5!(happyShift, curry_1_5!(action_72)),
        232 => partial_5!(happyShift, curry_1_5!(action_73)),
        233 => partial_5!(happyShift, curry_1_5!(action_39)),
        234 => partial_5!(happyShift, curry_1_5!(action_40)),
        235 => partial_5!(happyShift, curry_1_5!(action_41)),
        236 => partial_5!(happyShift, curry_1_5!(action_42)),
        237 => partial_5!(happyShift, curry_1_5!(action_74)),
        238 => partial_5!(happyShift, curry_1_5!(action_75)),
        240 => partial_5!(happyShift, curry_1_5!(action_44)),
        241 => partial_5!(happyShift, curry_1_5!(action_45)),
        242 => partial_5!(happyShift, curry_1_5!(action_46)),
        243 => partial_5!(happyShift, curry_1_5!(action_47)),
        244 => partial_5!(happyShift, curry_1_5!(action_48)),
        245 => partial_5!(happyShift, curry_1_5!(action_49)),
        12 => partial_5!(happyGoto, curry_1_5!(action_617)),
        13 => partial_5!(happyGoto, curry_1_5!(action_51)),
        14 => partial_5!(happyGoto, curry_1_5!(action_52)),
        22 => partial_5!(happyGoto, curry_1_5!(action_53)),
        23 => partial_5!(happyGoto, curry_1_5!(action_54)),
        24 => partial_5!(happyGoto, curry_1_5!(action_55)),
        25 => partial_5!(happyGoto, curry_1_5!(action_56)),
        26 => partial_5!(happyGoto, curry_1_5!(action_57)),
        100 => partial_5!(happyGoto, curry_1_5!(action_6)),
        104 => partial_5!(happyGoto, curry_1_5!(action_7)),
        106 => partial_5!(happyGoto, curry_1_5!(action_8)),
        107 => partial_5!(happyGoto, curry_1_5!(action_9)),
        108 => partial_5!(happyGoto, curry_1_5!(action_10)),
        109 => partial_5!(happyGoto, curry_1_5!(action_11)),
        110 => partial_5!(happyGoto, curry_1_5!(action_12)),
        111 => partial_5!(happyGoto, curry_1_5!(action_13)),
        112 => partial_5!(happyGoto, curry_1_5!(action_14)),
        113 => partial_5!(happyGoto, curry_1_5!(action_15)),
        114 => partial_5!(happyGoto, curry_1_5!(action_16)),
        115 => partial_5!(happyGoto, curry_1_5!(action_17)),
        116 => partial_5!(happyGoto, curry_1_5!(action_18)),
        117 => partial_5!(happyGoto, curry_1_5!(action_19)),
        118 => partial_5!(happyGoto, curry_1_5!(action_20)),
        119 => partial_5!(happyGoto, curry_1_5!(action_21)),
        120 => partial_5!(happyGoto, curry_1_5!(action_22)),
        122 => partial_5!(happyGoto, curry_1_5!(action_58)),
        127 => partial_5!(happyGoto, curry_1_5!(action_24)),
        128 => partial_5!(happyGoto, curry_1_5!(action_25)),
        131 => partial_5!(happyGoto, curry_1_5!(action_59)),
        _ => box happyFail
    }
}

fn action_385(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_26)),
        144 => partial_5!(happyShift, curry_1_5!(action_27)),
        145 => partial_5!(happyShift, curry_1_5!(action_28)),
        146 => partial_5!(happyShift, curry_1_5!(action_29)),
        147 => partial_5!(happyShift, curry_1_5!(action_30)),
        148 => partial_5!(happyShift, curry_1_5!(action_31)),
        149 => partial_5!(happyShift, curry_1_5!(action_32)),
        150 => partial_5!(happyShift, curry_1_5!(action_33)),
        153 => partial_5!(happyShift, curry_1_5!(action_34)),
        164 => partial_5!(happyShift, curry_1_5!(action_35)),
        180 => partial_5!(happyShift, curry_1_5!(action_60)),
        181 => partial_5!(happyShift, curry_1_5!(action_61)),
        184 => partial_5!(happyShift, curry_1_5!(action_36)),
        185 => partial_5!(happyShift, curry_1_5!(action_113)),
        186 => happyReduce_471(),
        187 => partial_5!(happyShift, curry_1_5!(action_62)),
        188 => partial_5!(happyShift, curry_1_5!(action_115)),
        189 => partial_5!(happyShift, curry_1_5!(action_63)),
        190 => partial_5!(happyShift, curry_1_5!(action_116)),
        191 => partial_5!(happyShift, curry_1_5!(action_64)),
        192 => partial_5!(happyShift, curry_1_5!(action_117)),
        193 => happyReduce_471(),
        194 => partial_5!(happyShift, curry_1_5!(action_65)),
        195 => partial_5!(happyShift, curry_1_5!(action_118)),
        196 => partial_5!(happyShift, curry_1_5!(action_66)),
        197 => partial_5!(happyShift, curry_1_5!(action_67)),
        198 => partial_5!(happyShift, curry_1_5!(action_119)),
        200 => partial_5!(happyShift, curry_1_5!(action_120)),
        201 => partial_5!(happyShift, curry_1_5!(action_121)),
        202 => partial_5!(happyShift, curry_1_5!(action_122)),
        203 => partial_5!(happyShift, curry_1_5!(action_68)),
        204 => partial_5!(happyShift, curry_1_5!(action_37)),
        205 => partial_5!(happyShift, curry_1_5!(action_69)),
        206 => partial_5!(happyShift, curry_1_5!(action_70)),
        207 => partial_5!(happyShift, curry_1_5!(action_123)),
        208 => partial_5!(happyShift, curry_1_5!(action_124)),
        209 => partial_5!(happyShift, curry_1_5!(action_125)),
        210 => partial_5!(happyShift, curry_1_5!(action_126)),
        212 => partial_5!(happyShift, curry_1_5!(action_127)),
        213 => happyReduce_471(),
        214 => happyReduce_471(),
        215 => partial_5!(happyShift, curry_1_5!(action_128)),
        216 => happyReduce_471(),
        217 => partial_5!(happyShift, curry_1_5!(action_71)),
        218 => partial_5!(happyShift, curry_1_5!(action_129)),
        219 => partial_5!(happyShift, curry_1_5!(action_130)),
        220 => partial_5!(happyShift, curry_1_5!(action_38)),
        221 => partial_5!(happyShift, curry_1_5!(action_131)),
        222 => partial_5!(happyShift, curry_1_5!(action_132)),
        223 => partial_5!(happyShift, curry_1_5!(action_133)),
        224 => partial_5!(happyShift, curry_1_5!(action_72)),
        225 => partial_5!(happyShift, curry_1_5!(action_134)),
        226 => partial_5!(happyShift, curry_1_5!(action_135)),
        227 => partial_5!(happyShift, curry_1_5!(action_136)),
        228 => partial_5!(happyShift, curry_1_5!(action_137)),
        229 => partial_5!(happyShift, curry_1_5!(action_138)),
        230 => partial_5!(happyShift, curry_1_5!(action_139)),
        231 => happyReduce_471(),
        232 => partial_5!(happyShift, curry_1_5!(action_73)),
        233 => partial_5!(happyShift, curry_1_5!(action_39)),
        234 => partial_5!(happyShift, curry_1_5!(action_40)),
        235 => partial_5!(happyShift, curry_1_5!(action_41)),
        236 => partial_5!(happyShift, curry_1_5!(action_42)),
        237 => partial_5!(happyShift, curry_1_5!(action_74)),
        238 => partial_5!(happyShift, curry_1_5!(action_615)),
        239 => partial_5!(happyShift, curry_1_5!(action_142)),
        240 => partial_5!(happyShift, curry_1_5!(action_616)),
        241 => partial_5!(happyShift, curry_1_5!(action_45)),
        242 => partial_5!(happyShift, curry_1_5!(action_46)),
        243 => partial_5!(happyShift, curry_1_5!(action_47)),
        244 => partial_5!(happyShift, curry_1_5!(action_48)),
        245 => partial_5!(happyShift, curry_1_5!(action_49)),
        12 => partial_5!(happyGoto, curry_1_5!(action_605)),
        13 => partial_5!(happyGoto, curry_1_5!(action_51)),
        14 => partial_5!(happyGoto, curry_1_5!(action_52)),
        16 => partial_5!(happyGoto, curry_1_5!(action_606)),
        18 => partial_5!(happyGoto, curry_1_5!(action_607)),
        19 => partial_5!(happyGoto, curry_1_5!(action_608)),
        20 => partial_5!(happyGoto, curry_1_5!(action_609)),
        22 => partial_5!(happyGoto, curry_1_5!(action_53)),
        23 => partial_5!(happyGoto, curry_1_5!(action_54)),
        24 => partial_5!(happyGoto, curry_1_5!(action_55)),
        25 => partial_5!(happyGoto, curry_1_5!(action_56)),
        26 => partial_5!(happyGoto, curry_1_5!(action_57)),
        32 => partial_5!(happyGoto, curry_1_5!(action_610)),
        34 => partial_5!(happyGoto, curry_1_5!(action_80)),
        36 => partial_5!(happyGoto, curry_1_5!(action_81)),
        37 => partial_5!(happyGoto, curry_1_5!(action_611)),
        38 => partial_5!(happyGoto, curry_1_5!(action_612)),
        40 => partial_5!(happyGoto, curry_1_5!(action_84)),
        41 => partial_5!(happyGoto, curry_1_5!(action_85)),
        42 => partial_5!(happyGoto, curry_1_5!(action_86)),
        43 => partial_5!(happyGoto, curry_1_5!(action_87)),
        44 => partial_5!(happyGoto, curry_1_5!(action_613)),
        45 => partial_5!(happyGoto, curry_1_5!(action_89)),
        46 => partial_5!(happyGoto, curry_1_5!(action_90)),
        47 => partial_5!(happyGoto, curry_1_5!(action_91)),
        48 => partial_5!(happyGoto, curry_1_5!(action_92)),
        49 => partial_5!(happyGoto, curry_1_5!(action_93)),
        50 => partial_5!(happyGoto, curry_1_5!(action_94)),
        51 => partial_5!(happyGoto, curry_1_5!(action_95)),
        52 => partial_5!(happyGoto, curry_1_5!(action_96)),
        53 => partial_5!(happyGoto, curry_1_5!(action_97)),
        54 => partial_5!(happyGoto, curry_1_5!(action_98)),
        61 => partial_5!(happyGoto, curry_1_5!(action_99)),
        65 => partial_5!(happyGoto, curry_1_5!(action_614)),
        100 => partial_5!(happyGoto, curry_1_5!(action_6)),
        104 => partial_5!(happyGoto, curry_1_5!(action_7)),
        106 => partial_5!(happyGoto, curry_1_5!(action_8)),
        107 => partial_5!(happyGoto, curry_1_5!(action_9)),
        108 => partial_5!(happyGoto, curry_1_5!(action_10)),
        109 => partial_5!(happyGoto, curry_1_5!(action_11)),
        110 => partial_5!(happyGoto, curry_1_5!(action_12)),
        111 => partial_5!(happyGoto, curry_1_5!(action_13)),
        112 => partial_5!(happyGoto, curry_1_5!(action_14)),
        113 => partial_5!(happyGoto, curry_1_5!(action_15)),
        114 => partial_5!(happyGoto, curry_1_5!(action_16)),
        115 => partial_5!(happyGoto, curry_1_5!(action_17)),
        116 => partial_5!(happyGoto, curry_1_5!(action_18)),
        117 => partial_5!(happyGoto, curry_1_5!(action_19)),
        118 => partial_5!(happyGoto, curry_1_5!(action_20)),
        119 => partial_5!(happyGoto, curry_1_5!(action_21)),
        120 => partial_5!(happyGoto, curry_1_5!(action_22)),
        122 => partial_5!(happyGoto, curry_1_5!(action_58)),
        127 => partial_5!(happyGoto, curry_1_5!(action_24)),
        128 => partial_5!(happyGoto, curry_1_5!(action_25)),
        131 => partial_5!(happyGoto, curry_1_5!(action_59)),
        132 => partial_5!(happyGoto, curry_1_5!(action_108)),
        133 => partial_5!(happyGoto, curry_1_5!(action_468)),
        134 => partial_5!(happyGoto, curry_1_5!(action_110)),
        _ => happyReduce_41()
    }
}

fn action_386(i: isize) -> ActionReturn {
    match i {
        211 => partial_5!(happyShift, curry_1_5!(action_604)),
        17 => partial_5!(happyGoto, curry_1_5!(action_603)),
        _ => happyReduce_42()
    }
}

fn action_387(i: isize) -> ActionReturn {
    match i {
        237 => partial_5!(happyShift, curry_1_5!(action_460)),
        85 => partial_5!(happyGoto, curry_1_5!(action_602)),
        _ => box happyFail
    }
}

fn action_388(i: isize) -> ActionReturn {
    match i {
        236 => partial_5!(happyShift, curry_1_5!(action_42)),
        128 => partial_5!(happyGoto, curry_1_5!(action_601)),
        _ => box happyFail
    }
}

fn action_389(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_26)),
        144 => partial_5!(happyShift, curry_1_5!(action_27)),
        145 => partial_5!(happyShift, curry_1_5!(action_28)),
        146 => partial_5!(happyShift, curry_1_5!(action_29)),
        147 => partial_5!(happyShift, curry_1_5!(action_30)),
        148 => partial_5!(happyShift, curry_1_5!(action_31)),
        149 => partial_5!(happyShift, curry_1_5!(action_32)),
        150 => partial_5!(happyShift, curry_1_5!(action_33)),
        153 => partial_5!(happyShift, curry_1_5!(action_34)),
        164 => partial_5!(happyShift, curry_1_5!(action_35)),
        180 => partial_5!(happyShift, curry_1_5!(action_60)),
        181 => partial_5!(happyShift, curry_1_5!(action_61)),
        184 => partial_5!(happyShift, curry_1_5!(action_36)),
        187 => partial_5!(happyShift, curry_1_5!(action_62)),
        189 => partial_5!(happyShift, curry_1_5!(action_63)),
        191 => partial_5!(happyShift, curry_1_5!(action_64)),
        194 => partial_5!(happyShift, curry_1_5!(action_65)),
        196 => partial_5!(happyShift, curry_1_5!(action_66)),
        197 => partial_5!(happyShift, curry_1_5!(action_67)),
        203 => partial_5!(happyShift, curry_1_5!(action_68)),
        204 => partial_5!(happyShift, curry_1_5!(action_37)),
        205 => partial_5!(happyShift, curry_1_5!(action_69)),
        206 => partial_5!(happyShift, curry_1_5!(action_70)),
        217 => partial_5!(happyShift, curry_1_5!(action_71)),
        220 => partial_5!(happyShift, curry_1_5!(action_38)),
        224 => partial_5!(happyShift, curry_1_5!(action_72)),
        232 => partial_5!(happyShift, curry_1_5!(action_73)),
        233 => partial_5!(happyShift, curry_1_5!(action_39)),
        234 => partial_5!(happyShift, curry_1_5!(action_40)),
        235 => partial_5!(happyShift, curry_1_5!(action_41)),
        236 => partial_5!(happyShift, curry_1_5!(action_42)),
        237 => partial_5!(happyShift, curry_1_5!(action_74)),
        238 => partial_5!(happyShift, curry_1_5!(action_75)),
        240 => partial_5!(happyShift, curry_1_5!(action_44)),
        241 => partial_5!(happyShift, curry_1_5!(action_45)),
        242 => partial_5!(happyShift, curry_1_5!(action_46)),
        243 => partial_5!(happyShift, curry_1_5!(action_47)),
        244 => partial_5!(happyShift, curry_1_5!(action_48)),
        245 => partial_5!(happyShift, curry_1_5!(action_49)),
        12 => partial_5!(happyGoto, curry_1_5!(action_600)),
        13 => partial_5!(happyGoto, curry_1_5!(action_51)),
        14 => partial_5!(happyGoto, curry_1_5!(action_52)),
        22 => partial_5!(happyGoto, curry_1_5!(action_53)),
        23 => partial_5!(happyGoto, curry_1_5!(action_54)),
        24 => partial_5!(happyGoto, curry_1_5!(action_55)),
        25 => partial_5!(happyGoto, curry_1_5!(action_56)),
        26 => partial_5!(happyGoto, curry_1_5!(action_57)),
        100 => partial_5!(happyGoto, curry_1_5!(action_6)),
        104 => partial_5!(happyGoto, curry_1_5!(action_7)),
        106 => partial_5!(happyGoto, curry_1_5!(action_8)),
        107 => partial_5!(happyGoto, curry_1_5!(action_9)),
        108 => partial_5!(happyGoto, curry_1_5!(action_10)),
        109 => partial_5!(happyGoto, curry_1_5!(action_11)),
        110 => partial_5!(happyGoto, curry_1_5!(action_12)),
        111 => partial_5!(happyGoto, curry_1_5!(action_13)),
        112 => partial_5!(happyGoto, curry_1_5!(action_14)),
        113 => partial_5!(happyGoto, curry_1_5!(action_15)),
        114 => partial_5!(happyGoto, curry_1_5!(action_16)),
        115 => partial_5!(happyGoto, curry_1_5!(action_17)),
        116 => partial_5!(happyGoto, curry_1_5!(action_18)),
        117 => partial_5!(happyGoto, curry_1_5!(action_19)),
        118 => partial_5!(happyGoto, curry_1_5!(action_20)),
        119 => partial_5!(happyGoto, curry_1_5!(action_21)),
        120 => partial_5!(happyGoto, curry_1_5!(action_22)),
        122 => partial_5!(happyGoto, curry_1_5!(action_58)),
        127 => partial_5!(happyGoto, curry_1_5!(action_24)),
        128 => partial_5!(happyGoto, curry_1_5!(action_25)),
        131 => partial_5!(happyGoto, curry_1_5!(action_59)),
        _ => box happyFail
    }
}

fn action_390(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_26)),
        144 => partial_5!(happyShift, curry_1_5!(action_27)),
        145 => partial_5!(happyShift, curry_1_5!(action_28)),
        146 => partial_5!(happyShift, curry_1_5!(action_29)),
        147 => partial_5!(happyShift, curry_1_5!(action_30)),
        148 => partial_5!(happyShift, curry_1_5!(action_31)),
        149 => partial_5!(happyShift, curry_1_5!(action_32)),
        150 => partial_5!(happyShift, curry_1_5!(action_33)),
        153 => partial_5!(happyShift, curry_1_5!(action_34)),
        164 => partial_5!(happyShift, curry_1_5!(action_35)),
        184 => partial_5!(happyShift, curry_1_5!(action_36)),
        204 => partial_5!(happyShift, curry_1_5!(action_37)),
        220 => partial_5!(happyShift, curry_1_5!(action_38)),
        233 => partial_5!(happyShift, curry_1_5!(action_39)),
        234 => partial_5!(happyShift, curry_1_5!(action_40)),
        235 => partial_5!(happyShift, curry_1_5!(action_41)),
        236 => partial_5!(happyShift, curry_1_5!(action_42)),
        237 => partial_5!(happyShift, curry_1_5!(action_43)),
        240 => partial_5!(happyShift, curry_1_5!(action_44)),
        241 => partial_5!(happyShift, curry_1_5!(action_45)),
        242 => partial_5!(happyShift, curry_1_5!(action_46)),
        243 => partial_5!(happyShift, curry_1_5!(action_47)),
        244 => partial_5!(happyShift, curry_1_5!(action_48)),
        245 => partial_5!(happyShift, curry_1_5!(action_49)),
        100 => partial_5!(happyGoto, curry_1_5!(action_6)),
        104 => partial_5!(happyGoto, curry_1_5!(action_7)),
        106 => partial_5!(happyGoto, curry_1_5!(action_259)),
        107 => partial_5!(happyGoto, curry_1_5!(action_9)),
        108 => partial_5!(happyGoto, curry_1_5!(action_10)),
        109 => partial_5!(happyGoto, curry_1_5!(action_11)),
        110 => partial_5!(happyGoto, curry_1_5!(action_12)),
        111 => partial_5!(happyGoto, curry_1_5!(action_13)),
        112 => partial_5!(happyGoto, curry_1_5!(action_14)),
        113 => partial_5!(happyGoto, curry_1_5!(action_15)),
        114 => partial_5!(happyGoto, curry_1_5!(action_16)),
        115 => partial_5!(happyGoto, curry_1_5!(action_17)),
        116 => partial_5!(happyGoto, curry_1_5!(action_18)),
        117 => partial_5!(happyGoto, curry_1_5!(action_19)),
        118 => partial_5!(happyGoto, curry_1_5!(action_20)),
        119 => partial_5!(happyGoto, curry_1_5!(action_260)),
        126 => partial_5!(happyGoto, curry_1_5!(action_599)),
        127 => partial_5!(happyGoto, curry_1_5!(action_24)),
        128 => partial_5!(happyGoto, curry_1_5!(action_25)),
        _ => box happyFail
    }
}

fn action_391(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_36()
    }
}

fn action_392(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_598)),
        _ => box happyFail
    }
}

fn action_393(i: isize) -> ActionReturn {
    match i {
        185 => partial_5!(happyShift, curry_1_5!(action_113)),
        188 => partial_5!(happyShift, curry_1_5!(action_115)),
        190 => partial_5!(happyShift, curry_1_5!(action_116)),
        192 => partial_5!(happyShift, curry_1_5!(action_117)),
        195 => partial_5!(happyShift, curry_1_5!(action_118)),
        198 => partial_5!(happyShift, curry_1_5!(action_119)),
        200 => partial_5!(happyShift, curry_1_5!(action_120)),
        201 => partial_5!(happyShift, curry_1_5!(action_121)),
        202 => partial_5!(happyShift, curry_1_5!(action_122)),
        207 => partial_5!(happyShift, curry_1_5!(action_123)),
        208 => partial_5!(happyShift, curry_1_5!(action_124)),
        209 => partial_5!(happyShift, curry_1_5!(action_125)),
        210 => partial_5!(happyShift, curry_1_5!(action_126)),
        212 => partial_5!(happyShift, curry_1_5!(action_127)),
        215 => partial_5!(happyShift, curry_1_5!(action_128)),
        218 => partial_5!(happyShift, curry_1_5!(action_129)),
        219 => partial_5!(happyShift, curry_1_5!(action_130)),
        221 => partial_5!(happyShift, curry_1_5!(action_131)),
        222 => partial_5!(happyShift, curry_1_5!(action_132)),
        223 => partial_5!(happyShift, curry_1_5!(action_133)),
        225 => partial_5!(happyShift, curry_1_5!(action_134)),
        226 => partial_5!(happyShift, curry_1_5!(action_135)),
        227 => partial_5!(happyShift, curry_1_5!(action_136)),
        228 => partial_5!(happyShift, curry_1_5!(action_137)),
        229 => partial_5!(happyShift, curry_1_5!(action_138)),
        230 => partial_5!(happyShift, curry_1_5!(action_139)),
        238 => partial_5!(happyShift, curry_1_5!(action_141)),
        239 => partial_5!(happyShift, curry_1_5!(action_142)),
        32 => partial_5!(happyGoto, curry_1_5!(action_597)),
        34 => partial_5!(happyGoto, curry_1_5!(action_80)),
        36 => partial_5!(happyGoto, curry_1_5!(action_81)),
        37 => partial_5!(happyGoto, curry_1_5!(action_464)),
        38 => partial_5!(happyGoto, curry_1_5!(action_465)),
        40 => partial_5!(happyGoto, curry_1_5!(action_84)),
        41 => partial_5!(happyGoto, curry_1_5!(action_85)),
        42 => partial_5!(happyGoto, curry_1_5!(action_86)),
        43 => partial_5!(happyGoto, curry_1_5!(action_87)),
        44 => partial_5!(happyGoto, curry_1_5!(action_466)),
        45 => partial_5!(happyGoto, curry_1_5!(action_89)),
        46 => partial_5!(happyGoto, curry_1_5!(action_90)),
        47 => partial_5!(happyGoto, curry_1_5!(action_91)),
        48 => partial_5!(happyGoto, curry_1_5!(action_92)),
        49 => partial_5!(happyGoto, curry_1_5!(action_93)),
        50 => partial_5!(happyGoto, curry_1_5!(action_94)),
        51 => partial_5!(happyGoto, curry_1_5!(action_95)),
        52 => partial_5!(happyGoto, curry_1_5!(action_96)),
        53 => partial_5!(happyGoto, curry_1_5!(action_97)),
        54 => partial_5!(happyGoto, curry_1_5!(action_98)),
        61 => partial_5!(happyGoto, curry_1_5!(action_99)),
        65 => partial_5!(happyGoto, curry_1_5!(action_467)),
        132 => partial_5!(happyGoto, curry_1_5!(action_108)),
        133 => partial_5!(happyGoto, curry_1_5!(action_468)),
        134 => partial_5!(happyGoto, curry_1_5!(action_110)),
        _ => happyReduce_471()
    }
}

fn action_394(i: isize) -> ActionReturn {
    match i {
        180 => partial_5!(happyShift, curry_1_5!(action_596)),
        _ => box happyFail
    }
}

fn action_395(i: isize) -> ActionReturn {
    match i {
        180 => partial_5!(happyShift, curry_1_5!(action_595)),
        _ => box happyFail
    }
}

fn action_396(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_65()
    }
}

fn action_397(i: isize) -> ActionReturn {
    match i {
        139 => partial_5!(happyShift, curry_1_5!(action_594)),
        _ => box happyFail
    }
}

fn action_398(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_69()
    }
}

fn action_399(i: isize) -> ActionReturn {
    match i {
        139 => partial_5!(happyShift, curry_1_5!(action_593)),
        _ => box happyFail
    }
}

fn action_400(i: isize) -> ActionReturn {
    match i {
        139 => partial_5!(happyShift, curry_1_5!(action_592)),
        _ => box happyFail
    }
}

fn action_401(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_475)),
        150 => partial_5!(happyShift, curry_1_5!(action_476)),
        237 => partial_5!(happyShift, curry_1_5!(action_140)),
        75 => partial_5!(happyGoto, curry_1_5!(action_591)),
        76 => partial_5!(happyGoto, curry_1_5!(action_102)),
        77 => partial_5!(happyGoto, curry_1_5!(action_103)),
        78 => partial_5!(happyGoto, curry_1_5!(action_481)),
        _ => box happyFail
    }
}

fn action_402(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_524)),
        150 => partial_5!(happyShift, curry_1_5!(action_525)),
        237 => partial_5!(happyShift, curry_1_5!(action_140)),
        238 => partial_5!(happyShift, curry_1_5!(action_229)),
        66 => partial_5!(happyGoto, curry_1_5!(action_590)),
        68 => partial_5!(happyGoto, curry_1_5!(action_219)),
        69 => partial_5!(happyGoto, curry_1_5!(action_220)),
        70 => partial_5!(happyGoto, curry_1_5!(action_221)),
        71 => partial_5!(happyGoto, curry_1_5!(action_222)),
        72 => partial_5!(happyGoto, curry_1_5!(action_223)),
        73 => partial_5!(happyGoto, curry_1_5!(action_224)),
        75 => partial_5!(happyGoto, curry_1_5!(action_523)),
        76 => partial_5!(happyGoto, curry_1_5!(action_102)),
        77 => partial_5!(happyGoto, curry_1_5!(action_103)),
        78 => partial_5!(happyGoto, curry_1_5!(action_481)),
        _ => box happyFail
    }
}

fn action_403(i: isize) -> ActionReturn {
    match i {
        181 => partial_5!(happyShift, curry_1_5!(action_61)),
        185 => partial_5!(happyShift, curry_1_5!(action_113)),
        188 => partial_5!(happyShift, curry_1_5!(action_115)),
        190 => partial_5!(happyShift, curry_1_5!(action_116)),
        192 => partial_5!(happyShift, curry_1_5!(action_117)),
        195 => partial_5!(happyShift, curry_1_5!(action_118)),
        198 => partial_5!(happyShift, curry_1_5!(action_119)),
        200 => partial_5!(happyShift, curry_1_5!(action_120)),
        201 => partial_5!(happyShift, curry_1_5!(action_121)),
        202 => partial_5!(happyShift, curry_1_5!(action_122)),
        207 => partial_5!(happyShift, curry_1_5!(action_123)),
        208 => partial_5!(happyShift, curry_1_5!(action_124)),
        209 => partial_5!(happyShift, curry_1_5!(action_125)),
        210 => partial_5!(happyShift, curry_1_5!(action_126)),
        212 => partial_5!(happyShift, curry_1_5!(action_127)),
        215 => partial_5!(happyShift, curry_1_5!(action_128)),
        218 => partial_5!(happyShift, curry_1_5!(action_129)),
        219 => partial_5!(happyShift, curry_1_5!(action_130)),
        221 => partial_5!(happyShift, curry_1_5!(action_131)),
        222 => partial_5!(happyShift, curry_1_5!(action_132)),
        223 => partial_5!(happyShift, curry_1_5!(action_133)),
        225 => partial_5!(happyShift, curry_1_5!(action_134)),
        226 => partial_5!(happyShift, curry_1_5!(action_135)),
        227 => partial_5!(happyShift, curry_1_5!(action_136)),
        228 => partial_5!(happyShift, curry_1_5!(action_137)),
        229 => partial_5!(happyShift, curry_1_5!(action_138)),
        230 => partial_5!(happyShift, curry_1_5!(action_139)),
        238 => partial_5!(happyShift, curry_1_5!(action_141)),
        239 => partial_5!(happyShift, curry_1_5!(action_142)),
        14 => partial_5!(happyGoto, curry_1_5!(action_589)),
        32 => partial_5!(happyGoto, curry_1_5!(action_463)),
        34 => partial_5!(happyGoto, curry_1_5!(action_80)),
        36 => partial_5!(happyGoto, curry_1_5!(action_81)),
        37 => partial_5!(happyGoto, curry_1_5!(action_464)),
        38 => partial_5!(happyGoto, curry_1_5!(action_465)),
        40 => partial_5!(happyGoto, curry_1_5!(action_84)),
        41 => partial_5!(happyGoto, curry_1_5!(action_85)),
        42 => partial_5!(happyGoto, curry_1_5!(action_86)),
        43 => partial_5!(happyGoto, curry_1_5!(action_87)),
        44 => partial_5!(happyGoto, curry_1_5!(action_466)),
        45 => partial_5!(happyGoto, curry_1_5!(action_89)),
        46 => partial_5!(happyGoto, curry_1_5!(action_90)),
        47 => partial_5!(happyGoto, curry_1_5!(action_91)),
        48 => partial_5!(happyGoto, curry_1_5!(action_92)),
        49 => partial_5!(happyGoto, curry_1_5!(action_93)),
        50 => partial_5!(happyGoto, curry_1_5!(action_94)),
        51 => partial_5!(happyGoto, curry_1_5!(action_95)),
        52 => partial_5!(happyGoto, curry_1_5!(action_96)),
        53 => partial_5!(happyGoto, curry_1_5!(action_97)),
        54 => partial_5!(happyGoto, curry_1_5!(action_98)),
        61 => partial_5!(happyGoto, curry_1_5!(action_99)),
        65 => partial_5!(happyGoto, curry_1_5!(action_467)),
        132 => partial_5!(happyGoto, curry_1_5!(action_108)),
        133 => partial_5!(happyGoto, curry_1_5!(action_468)),
        134 => partial_5!(happyGoto, curry_1_5!(action_110)),
        _ => happyReduce_471()
    }
}

fn action_404(i: isize) -> ActionReturn {
    match i {
        168 => partial_5!(happyShift, curry_1_5!(action_516)),
        94 => partial_5!(happyGoto, curry_1_5!(action_588)),
        _ => happyReduce_344()
    }
}

fn action_405(i: isize) -> ActionReturn {
    match i {
        239 => partial_5!(happyShift, curry_1_5!(action_142)),
        132 => partial_5!(happyGoto, curry_1_5!(action_587)),
        133 => partial_5!(happyGoto, curry_1_5!(action_150)),
        134 => partial_5!(happyGoto, curry_1_5!(action_110)),
        _ => happyReduce_471()
    }
}

fn action_406(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_586)),
        _ => box happyFail
    }
}

fn action_407(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_14()
    }
}

fn action_408(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_26)),
        144 => partial_5!(happyShift, curry_1_5!(action_27)),
        145 => partial_5!(happyShift, curry_1_5!(action_28)),
        146 => partial_5!(happyShift, curry_1_5!(action_29)),
        147 => partial_5!(happyShift, curry_1_5!(action_30)),
        148 => partial_5!(happyShift, curry_1_5!(action_31)),
        149 => partial_5!(happyShift, curry_1_5!(action_32)),
        150 => partial_5!(happyShift, curry_1_5!(action_33)),
        153 => partial_5!(happyShift, curry_1_5!(action_34)),
        164 => partial_5!(happyShift, curry_1_5!(action_35)),
        184 => partial_5!(happyShift, curry_1_5!(action_36)),
        190 => partial_5!(happyShift, curry_1_5!(action_116)),
        192 => partial_5!(happyShift, curry_1_5!(action_117)),
        195 => partial_5!(happyShift, curry_1_5!(action_118)),
        198 => partial_5!(happyShift, curry_1_5!(action_119)),
        200 => partial_5!(happyShift, curry_1_5!(action_120)),
        202 => partial_5!(happyShift, curry_1_5!(action_122)),
        204 => partial_5!(happyShift, curry_1_5!(action_37)),
        208 => partial_5!(happyShift, curry_1_5!(action_124)),
        209 => partial_5!(happyShift, curry_1_5!(action_125)),
        210 => partial_5!(happyShift, curry_1_5!(action_126)),
        218 => partial_5!(happyShift, curry_1_5!(action_129)),
        219 => partial_5!(happyShift, curry_1_5!(action_130)),
        220 => partial_5!(happyShift, curry_1_5!(action_38)),
        223 => partial_5!(happyShift, curry_1_5!(action_133)),
        226 => partial_5!(happyShift, curry_1_5!(action_135)),
        228 => partial_5!(happyShift, curry_1_5!(action_137)),
        229 => partial_5!(happyShift, curry_1_5!(action_138)),
        230 => partial_5!(happyShift, curry_1_5!(action_139)),
        233 => partial_5!(happyShift, curry_1_5!(action_39)),
        234 => partial_5!(happyShift, curry_1_5!(action_40)),
        235 => partial_5!(happyShift, curry_1_5!(action_41)),
        236 => partial_5!(happyShift, curry_1_5!(action_42)),
        237 => partial_5!(happyShift, curry_1_5!(action_43)),
        238 => partial_5!(happyShift, curry_1_5!(action_141)),
        239 => partial_5!(happyShift, curry_1_5!(action_142)),
        240 => partial_5!(happyShift, curry_1_5!(action_44)),
        241 => partial_5!(happyShift, curry_1_5!(action_45)),
        242 => partial_5!(happyShift, curry_1_5!(action_46)),
        243 => partial_5!(happyShift, curry_1_5!(action_47)),
        244 => partial_5!(happyShift, curry_1_5!(action_48)),
        245 => partial_5!(happyShift, curry_1_5!(action_49)),
        44 => partial_5!(happyGoto, curry_1_5!(action_286)),
        45 => partial_5!(happyGoto, curry_1_5!(action_89)),
        47 => partial_5!(happyGoto, curry_1_5!(action_287)),
        49 => partial_5!(happyGoto, curry_1_5!(action_288)),
        51 => partial_5!(happyGoto, curry_1_5!(action_289)),
        52 => partial_5!(happyGoto, curry_1_5!(action_96)),
        53 => partial_5!(happyGoto, curry_1_5!(action_97)),
        54 => partial_5!(happyGoto, curry_1_5!(action_98)),
        61 => partial_5!(happyGoto, curry_1_5!(action_99)),
        65 => partial_5!(happyGoto, curry_1_5!(action_290)),
        86 => partial_5!(happyGoto, curry_1_5!(action_584)),
        100 => partial_5!(happyGoto, curry_1_5!(action_6)),
        104 => partial_5!(happyGoto, curry_1_5!(action_7)),
        106 => partial_5!(happyGoto, curry_1_5!(action_8)),
        107 => partial_5!(happyGoto, curry_1_5!(action_9)),
        108 => partial_5!(happyGoto, curry_1_5!(action_10)),
        109 => partial_5!(happyGoto, curry_1_5!(action_11)),
        110 => partial_5!(happyGoto, curry_1_5!(action_12)),
        111 => partial_5!(happyGoto, curry_1_5!(action_13)),
        112 => partial_5!(happyGoto, curry_1_5!(action_14)),
        113 => partial_5!(happyGoto, curry_1_5!(action_15)),
        114 => partial_5!(happyGoto, curry_1_5!(action_16)),
        115 => partial_5!(happyGoto, curry_1_5!(action_17)),
        116 => partial_5!(happyGoto, curry_1_5!(action_18)),
        117 => partial_5!(happyGoto, curry_1_5!(action_19)),
        118 => partial_5!(happyGoto, curry_1_5!(action_20)),
        119 => partial_5!(happyGoto, curry_1_5!(action_21)),
        120 => partial_5!(happyGoto, curry_1_5!(action_22)),
        122 => partial_5!(happyGoto, curry_1_5!(action_585)),
        127 => partial_5!(happyGoto, curry_1_5!(action_24)),
        128 => partial_5!(happyGoto, curry_1_5!(action_25)),
        132 => partial_5!(happyGoto, curry_1_5!(action_108)),
        133 => partial_5!(happyGoto, curry_1_5!(action_293)),
        134 => partial_5!(happyGoto, curry_1_5!(action_110)),
        _ => happyReduce_471()
    }
}

fn action_409(i: isize) -> ActionReturn {
    match i {
        181 => partial_5!(happyShift, curry_1_5!(action_61)),
        185 => partial_5!(happyShift, curry_1_5!(action_113)),
        188 => partial_5!(happyShift, curry_1_5!(action_115)),
        190 => partial_5!(happyShift, curry_1_5!(action_116)),
        192 => partial_5!(happyShift, curry_1_5!(action_117)),
        195 => partial_5!(happyShift, curry_1_5!(action_118)),
        198 => partial_5!(happyShift, curry_1_5!(action_119)),
        200 => partial_5!(happyShift, curry_1_5!(action_120)),
        201 => partial_5!(happyShift, curry_1_5!(action_121)),
        202 => partial_5!(happyShift, curry_1_5!(action_122)),
        207 => partial_5!(happyShift, curry_1_5!(action_123)),
        208 => partial_5!(happyShift, curry_1_5!(action_124)),
        209 => partial_5!(happyShift, curry_1_5!(action_125)),
        210 => partial_5!(happyShift, curry_1_5!(action_126)),
        212 => partial_5!(happyShift, curry_1_5!(action_127)),
        215 => partial_5!(happyShift, curry_1_5!(action_128)),
        218 => partial_5!(happyShift, curry_1_5!(action_129)),
        219 => partial_5!(happyShift, curry_1_5!(action_130)),
        221 => partial_5!(happyShift, curry_1_5!(action_131)),
        222 => partial_5!(happyShift, curry_1_5!(action_132)),
        223 => partial_5!(happyShift, curry_1_5!(action_133)),
        225 => partial_5!(happyShift, curry_1_5!(action_134)),
        226 => partial_5!(happyShift, curry_1_5!(action_135)),
        227 => partial_5!(happyShift, curry_1_5!(action_136)),
        228 => partial_5!(happyShift, curry_1_5!(action_137)),
        229 => partial_5!(happyShift, curry_1_5!(action_138)),
        230 => partial_5!(happyShift, curry_1_5!(action_139)),
        238 => partial_5!(happyShift, curry_1_5!(action_141)),
        239 => partial_5!(happyShift, curry_1_5!(action_142)),
        14 => partial_5!(happyGoto, curry_1_5!(action_583)),
        32 => partial_5!(happyGoto, curry_1_5!(action_463)),
        34 => partial_5!(happyGoto, curry_1_5!(action_80)),
        36 => partial_5!(happyGoto, curry_1_5!(action_81)),
        37 => partial_5!(happyGoto, curry_1_5!(action_464)),
        38 => partial_5!(happyGoto, curry_1_5!(action_465)),
        40 => partial_5!(happyGoto, curry_1_5!(action_84)),
        41 => partial_5!(happyGoto, curry_1_5!(action_85)),
        42 => partial_5!(happyGoto, curry_1_5!(action_86)),
        43 => partial_5!(happyGoto, curry_1_5!(action_87)),
        44 => partial_5!(happyGoto, curry_1_5!(action_466)),
        45 => partial_5!(happyGoto, curry_1_5!(action_89)),
        46 => partial_5!(happyGoto, curry_1_5!(action_90)),
        47 => partial_5!(happyGoto, curry_1_5!(action_91)),
        48 => partial_5!(happyGoto, curry_1_5!(action_92)),
        49 => partial_5!(happyGoto, curry_1_5!(action_93)),
        50 => partial_5!(happyGoto, curry_1_5!(action_94)),
        51 => partial_5!(happyGoto, curry_1_5!(action_95)),
        52 => partial_5!(happyGoto, curry_1_5!(action_96)),
        53 => partial_5!(happyGoto, curry_1_5!(action_97)),
        54 => partial_5!(happyGoto, curry_1_5!(action_98)),
        61 => partial_5!(happyGoto, curry_1_5!(action_99)),
        65 => partial_5!(happyGoto, curry_1_5!(action_467)),
        132 => partial_5!(happyGoto, curry_1_5!(action_108)),
        133 => partial_5!(happyGoto, curry_1_5!(action_468)),
        134 => partial_5!(happyGoto, curry_1_5!(action_110)),
        _ => happyReduce_471()
    }
}

fn action_410(i: isize) -> ActionReturn {
    match i {
        168 => partial_5!(happyShift, curry_1_5!(action_516)),
        94 => partial_5!(happyGoto, curry_1_5!(action_582)),
        _ => happyReduce_344()
    }
}

fn action_411(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_16()
    }
}

fn action_412(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_237()
    }
}

fn action_413(i: isize) -> ActionReturn {
    match i {
        185 => partial_5!(happyShift, curry_1_5!(action_113)),
        186 => happyReduce_471(),
        188 => partial_5!(happyShift, curry_1_5!(action_115)),
        190 => partial_5!(happyShift, curry_1_5!(action_116)),
        192 => partial_5!(happyShift, curry_1_5!(action_117)),
        193 => happyReduce_471(),
        195 => partial_5!(happyShift, curry_1_5!(action_118)),
        198 => partial_5!(happyShift, curry_1_5!(action_119)),
        200 => partial_5!(happyShift, curry_1_5!(action_120)),
        201 => partial_5!(happyShift, curry_1_5!(action_121)),
        202 => partial_5!(happyShift, curry_1_5!(action_122)),
        207 => partial_5!(happyShift, curry_1_5!(action_123)),
        208 => partial_5!(happyShift, curry_1_5!(action_124)),
        209 => partial_5!(happyShift, curry_1_5!(action_125)),
        210 => partial_5!(happyShift, curry_1_5!(action_126)),
        212 => partial_5!(happyShift, curry_1_5!(action_127)),
        213 => happyReduce_471(),
        214 => happyReduce_471(),
        215 => partial_5!(happyShift, curry_1_5!(action_128)),
        216 => happyReduce_471(),
        218 => partial_5!(happyShift, curry_1_5!(action_129)),
        219 => partial_5!(happyShift, curry_1_5!(action_130)),
        221 => partial_5!(happyShift, curry_1_5!(action_131)),
        223 => partial_5!(happyShift, curry_1_5!(action_133)),
        225 => partial_5!(happyShift, curry_1_5!(action_134)),
        226 => partial_5!(happyShift, curry_1_5!(action_135)),
        227 => partial_5!(happyShift, curry_1_5!(action_136)),
        228 => partial_5!(happyShift, curry_1_5!(action_137)),
        229 => partial_5!(happyShift, curry_1_5!(action_138)),
        230 => partial_5!(happyShift, curry_1_5!(action_139)),
        231 => happyReduce_471(),
        238 => partial_5!(happyShift, curry_1_5!(action_141)),
        239 => partial_5!(happyShift, curry_1_5!(action_142)),
        37 => partial_5!(happyGoto, curry_1_5!(action_449)),
        38 => partial_5!(happyGoto, curry_1_5!(action_450)),
        40 => partial_5!(happyGoto, curry_1_5!(action_84)),
        41 => partial_5!(happyGoto, curry_1_5!(action_85)),
        42 => partial_5!(happyGoto, curry_1_5!(action_86)),
        43 => partial_5!(happyGoto, curry_1_5!(action_87)),
        44 => partial_5!(happyGoto, curry_1_5!(action_451)),
        45 => partial_5!(happyGoto, curry_1_5!(action_89)),
        46 => partial_5!(happyGoto, curry_1_5!(action_90)),
        47 => partial_5!(happyGoto, curry_1_5!(action_91)),
        48 => partial_5!(happyGoto, curry_1_5!(action_452)),
        49 => partial_5!(happyGoto, curry_1_5!(action_453)),
        50 => partial_5!(happyGoto, curry_1_5!(action_94)),
        51 => partial_5!(happyGoto, curry_1_5!(action_95)),
        52 => partial_5!(happyGoto, curry_1_5!(action_96)),
        53 => partial_5!(happyGoto, curry_1_5!(action_97)),
        54 => partial_5!(happyGoto, curry_1_5!(action_98)),
        61 => partial_5!(happyGoto, curry_1_5!(action_99)),
        65 => partial_5!(happyGoto, curry_1_5!(action_454)),
        82 => partial_5!(happyGoto, curry_1_5!(action_455)),
        83 => partial_5!(happyGoto, curry_1_5!(action_456)),
        84 => partial_5!(happyGoto, curry_1_5!(action_457)),
        132 => partial_5!(happyGoto, curry_1_5!(action_108)),
        133 => partial_5!(happyGoto, curry_1_5!(action_459)),
        134 => partial_5!(happyGoto, curry_1_5!(action_110)),
        _ => happyReduce_282()
    }
}

fn action_414(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_581)),
        150 => partial_5!(happyShift, curry_1_5!(action_228)),
        186 => partial_5!(happyShift, curry_1_5!(action_171)),
        193 => partial_5!(happyShift, curry_1_5!(action_172)),
        213 => partial_5!(happyShift, curry_1_5!(action_173)),
        214 => partial_5!(happyShift, curry_1_5!(action_174)),
        216 => partial_5!(happyShift, curry_1_5!(action_175)),
        231 => partial_5!(happyShift, curry_1_5!(action_176)),
        237 => partial_5!(happyShift, curry_1_5!(action_140)),
        238 => partial_5!(happyShift, curry_1_5!(action_229)),
        239 => partial_5!(happyShift, curry_1_5!(action_142)),
        64 => partial_5!(happyGoto, curry_1_5!(action_187)),
        69 => partial_5!(happyGoto, curry_1_5!(action_578)),
        70 => partial_5!(happyGoto, curry_1_5!(action_221)),
        71 => partial_5!(happyGoto, curry_1_5!(action_222)),
        72 => partial_5!(happyGoto, curry_1_5!(action_579)),
        73 => partial_5!(happyGoto, curry_1_5!(action_224)),
        75 => partial_5!(happyGoto, curry_1_5!(action_482)),
        76 => partial_5!(happyGoto, curry_1_5!(action_102)),
        77 => partial_5!(happyGoto, curry_1_5!(action_103)),
        78 => partial_5!(happyGoto, curry_1_5!(action_104)),
        80 => partial_5!(happyGoto, curry_1_5!(action_483)),
        81 => partial_5!(happyGoto, curry_1_5!(action_107)),
        133 => partial_5!(happyGoto, curry_1_5!(action_580)),
        134 => partial_5!(happyGoto, curry_1_5!(action_110)),
        _ => box happyFail
    }
}

fn action_415(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_240()
    }
}

fn action_416(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_252()
    }
}

fn action_417(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_571)),
        150 => partial_5!(happyShift, curry_1_5!(action_572)),
        237 => partial_5!(happyShift, curry_1_5!(action_140)),
        238 => partial_5!(happyShift, curry_1_5!(action_229)),
        239 => partial_5!(happyShift, curry_1_5!(action_142)),
        69 => partial_5!(happyGoto, curry_1_5!(action_577)),
        70 => partial_5!(happyGoto, curry_1_5!(action_221)),
        71 => partial_5!(happyGoto, curry_1_5!(action_222)),
        75 => partial_5!(happyGoto, curry_1_5!(action_480)),
        76 => partial_5!(happyGoto, curry_1_5!(action_102)),
        77 => partial_5!(happyGoto, curry_1_5!(action_103)),
        78 => partial_5!(happyGoto, curry_1_5!(action_481)),
        134 => partial_5!(happyGoto, curry_1_5!(action_167)),
        _ => happyReduce_472()
    }
}

fn action_418(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_423)),
        150 => partial_5!(happyShift, curry_1_5!(action_228)),
        237 => partial_5!(happyShift, curry_1_5!(action_140)),
        238 => partial_5!(happyShift, curry_1_5!(action_424)),
        239 => partial_5!(happyShift, curry_1_5!(action_142)),
        70 => partial_5!(happyGoto, curry_1_5!(action_419)),
        71 => partial_5!(happyGoto, curry_1_5!(action_222)),
        72 => partial_5!(happyGoto, curry_1_5!(action_420)),
        73 => partial_5!(happyGoto, curry_1_5!(action_224)),
        74 => partial_5!(happyGoto, curry_1_5!(action_576)),
        76 => partial_5!(happyGoto, curry_1_5!(action_157)),
        77 => partial_5!(happyGoto, curry_1_5!(action_103)),
        78 => partial_5!(happyGoto, curry_1_5!(action_158)),
        80 => partial_5!(happyGoto, curry_1_5!(action_159)),
        81 => partial_5!(happyGoto, curry_1_5!(action_107)),
        133 => partial_5!(happyGoto, curry_1_5!(action_422)),
        134 => partial_5!(happyGoto, curry_1_5!(action_110)),
        _ => box happyFail
    }
}

fn action_419(i: isize) -> ActionReturn {
    match i {
        139 => partial_5!(happyShift, curry_1_5!(action_575)),
        _ => box happyFail
    }
}

fn action_420(i: isize) -> ActionReturn {
    match i {
        139 => partial_5!(happyShift, curry_1_5!(action_574)),
        _ => box happyFail
    }
}

fn action_421(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_413)),
        140 => partial_5!(happyShift, curry_1_5!(action_182)),
        88 => partial_5!(happyGoto, curry_1_5!(action_573)),
        89 => partial_5!(happyGoto, curry_1_5!(action_179)),
        90 => partial_5!(happyGoto, curry_1_5!(action_180)),
        _ => box happyFail
    }
}

fn action_422(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_571)),
        150 => partial_5!(happyShift, curry_1_5!(action_572)),
        237 => partial_5!(happyShift, curry_1_5!(action_140)),
        239 => partial_5!(happyShift, curry_1_5!(action_142)),
        70 => partial_5!(happyGoto, curry_1_5!(action_570)),
        71 => partial_5!(happyGoto, curry_1_5!(action_222)),
        76 => partial_5!(happyGoto, curry_1_5!(action_473)),
        77 => partial_5!(happyGoto, curry_1_5!(action_103)),
        78 => partial_5!(happyGoto, curry_1_5!(action_474)),
        134 => partial_5!(happyGoto, curry_1_5!(action_167)),
        _ => box happyFail
    }
}

fn action_423(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_423)),
        150 => partial_5!(happyShift, curry_1_5!(action_228)),
        237 => partial_5!(happyShift, curry_1_5!(action_140)),
        238 => partial_5!(happyShift, curry_1_5!(action_424)),
        239 => partial_5!(happyShift, curry_1_5!(action_142)),
        70 => partial_5!(happyGoto, curry_1_5!(action_419)),
        71 => partial_5!(happyGoto, curry_1_5!(action_222)),
        72 => partial_5!(happyGoto, curry_1_5!(action_420)),
        73 => partial_5!(happyGoto, curry_1_5!(action_224)),
        74 => partial_5!(happyGoto, curry_1_5!(action_569)),
        76 => partial_5!(happyGoto, curry_1_5!(action_157)),
        77 => partial_5!(happyGoto, curry_1_5!(action_103)),
        78 => partial_5!(happyGoto, curry_1_5!(action_158)),
        80 => partial_5!(happyGoto, curry_1_5!(action_159)),
        81 => partial_5!(happyGoto, curry_1_5!(action_107)),
        133 => partial_5!(happyGoto, curry_1_5!(action_422)),
        134 => partial_5!(happyGoto, curry_1_5!(action_110)),
        _ => box happyFail
    }
}

fn action_424(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_258()
    }
}

fn action_425(i: isize) -> ActionReturn {
    match i {
        181 => partial_5!(happyShift, curry_1_5!(action_61)),
        185 => partial_5!(happyShift, curry_1_5!(action_113)),
        188 => partial_5!(happyShift, curry_1_5!(action_115)),
        190 => partial_5!(happyShift, curry_1_5!(action_116)),
        192 => partial_5!(happyShift, curry_1_5!(action_117)),
        195 => partial_5!(happyShift, curry_1_5!(action_118)),
        198 => partial_5!(happyShift, curry_1_5!(action_119)),
        200 => partial_5!(happyShift, curry_1_5!(action_120)),
        201 => partial_5!(happyShift, curry_1_5!(action_121)),
        202 => partial_5!(happyShift, curry_1_5!(action_122)),
        207 => partial_5!(happyShift, curry_1_5!(action_123)),
        208 => partial_5!(happyShift, curry_1_5!(action_124)),
        209 => partial_5!(happyShift, curry_1_5!(action_125)),
        210 => partial_5!(happyShift, curry_1_5!(action_126)),
        212 => partial_5!(happyShift, curry_1_5!(action_127)),
        215 => partial_5!(happyShift, curry_1_5!(action_128)),
        218 => partial_5!(happyShift, curry_1_5!(action_129)),
        219 => partial_5!(happyShift, curry_1_5!(action_130)),
        221 => partial_5!(happyShift, curry_1_5!(action_131)),
        222 => partial_5!(happyShift, curry_1_5!(action_132)),
        223 => partial_5!(happyShift, curry_1_5!(action_133)),
        225 => partial_5!(happyShift, curry_1_5!(action_134)),
        226 => partial_5!(happyShift, curry_1_5!(action_135)),
        227 => partial_5!(happyShift, curry_1_5!(action_136)),
        228 => partial_5!(happyShift, curry_1_5!(action_137)),
        229 => partial_5!(happyShift, curry_1_5!(action_138)),
        230 => partial_5!(happyShift, curry_1_5!(action_139)),
        238 => partial_5!(happyShift, curry_1_5!(action_141)),
        239 => partial_5!(happyShift, curry_1_5!(action_142)),
        14 => partial_5!(happyGoto, curry_1_5!(action_568)),
        32 => partial_5!(happyGoto, curry_1_5!(action_463)),
        34 => partial_5!(happyGoto, curry_1_5!(action_80)),
        36 => partial_5!(happyGoto, curry_1_5!(action_81)),
        37 => partial_5!(happyGoto, curry_1_5!(action_464)),
        38 => partial_5!(happyGoto, curry_1_5!(action_465)),
        40 => partial_5!(happyGoto, curry_1_5!(action_84)),
        41 => partial_5!(happyGoto, curry_1_5!(action_85)),
        42 => partial_5!(happyGoto, curry_1_5!(action_86)),
        43 => partial_5!(happyGoto, curry_1_5!(action_87)),
        44 => partial_5!(happyGoto, curry_1_5!(action_466)),
        45 => partial_5!(happyGoto, curry_1_5!(action_89)),
        46 => partial_5!(happyGoto, curry_1_5!(action_90)),
        47 => partial_5!(happyGoto, curry_1_5!(action_91)),
        48 => partial_5!(happyGoto, curry_1_5!(action_92)),
        49 => partial_5!(happyGoto, curry_1_5!(action_93)),
        50 => partial_5!(happyGoto, curry_1_5!(action_94)),
        51 => partial_5!(happyGoto, curry_1_5!(action_95)),
        52 => partial_5!(happyGoto, curry_1_5!(action_96)),
        53 => partial_5!(happyGoto, curry_1_5!(action_97)),
        54 => partial_5!(happyGoto, curry_1_5!(action_98)),
        61 => partial_5!(happyGoto, curry_1_5!(action_99)),
        65 => partial_5!(happyGoto, curry_1_5!(action_467)),
        132 => partial_5!(happyGoto, curry_1_5!(action_108)),
        133 => partial_5!(happyGoto, curry_1_5!(action_468)),
        134 => partial_5!(happyGoto, curry_1_5!(action_110)),
        _ => happyReduce_471()
    }
}

fn action_426(i: isize) -> ActionReturn {
    match i {
        168 => partial_5!(happyShift, curry_1_5!(action_516)),
        94 => partial_5!(happyGoto, curry_1_5!(action_567)),
        _ => happyReduce_344()
    }
}

fn action_427(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_15()
    }
}

fn action_428(i: isize) -> ActionReturn {
    match i {
        181 => partial_5!(happyShift, curry_1_5!(action_566)),
        _ => happyReduce_188()
    }
}

fn action_429(i: isize) -> ActionReturn {
    match i {
        55 => partial_5!(happyGoto, curry_1_5!(action_565)),
        _ => happyReduce_191()
    }
}

fn action_430(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_26)),
        144 => partial_5!(happyShift, curry_1_5!(action_27)),
        145 => partial_5!(happyShift, curry_1_5!(action_28)),
        146 => partial_5!(happyShift, curry_1_5!(action_29)),
        147 => partial_5!(happyShift, curry_1_5!(action_30)),
        148 => partial_5!(happyShift, curry_1_5!(action_31)),
        149 => partial_5!(happyShift, curry_1_5!(action_32)),
        150 => partial_5!(happyShift, curry_1_5!(action_33)),
        153 => partial_5!(happyShift, curry_1_5!(action_34)),
        164 => partial_5!(happyShift, curry_1_5!(action_35)),
        184 => partial_5!(happyShift, curry_1_5!(action_36)),
        190 => partial_5!(happyShift, curry_1_5!(action_116)),
        192 => partial_5!(happyShift, curry_1_5!(action_117)),
        195 => partial_5!(happyShift, curry_1_5!(action_118)),
        198 => partial_5!(happyShift, curry_1_5!(action_119)),
        200 => partial_5!(happyShift, curry_1_5!(action_120)),
        202 => partial_5!(happyShift, curry_1_5!(action_122)),
        204 => partial_5!(happyShift, curry_1_5!(action_37)),
        208 => partial_5!(happyShift, curry_1_5!(action_124)),
        209 => partial_5!(happyShift, curry_1_5!(action_125)),
        210 => partial_5!(happyShift, curry_1_5!(action_126)),
        218 => partial_5!(happyShift, curry_1_5!(action_129)),
        219 => partial_5!(happyShift, curry_1_5!(action_130)),
        220 => partial_5!(happyShift, curry_1_5!(action_38)),
        223 => partial_5!(happyShift, curry_1_5!(action_133)),
        226 => partial_5!(happyShift, curry_1_5!(action_135)),
        228 => partial_5!(happyShift, curry_1_5!(action_137)),
        229 => partial_5!(happyShift, curry_1_5!(action_138)),
        230 => partial_5!(happyShift, curry_1_5!(action_139)),
        233 => partial_5!(happyShift, curry_1_5!(action_39)),
        234 => partial_5!(happyShift, curry_1_5!(action_40)),
        235 => partial_5!(happyShift, curry_1_5!(action_41)),
        236 => partial_5!(happyShift, curry_1_5!(action_42)),
        237 => partial_5!(happyShift, curry_1_5!(action_43)),
        238 => partial_5!(happyShift, curry_1_5!(action_141)),
        239 => partial_5!(happyShift, curry_1_5!(action_142)),
        240 => partial_5!(happyShift, curry_1_5!(action_44)),
        241 => partial_5!(happyShift, curry_1_5!(action_45)),
        242 => partial_5!(happyShift, curry_1_5!(action_46)),
        243 => partial_5!(happyShift, curry_1_5!(action_47)),
        244 => partial_5!(happyShift, curry_1_5!(action_48)),
        245 => partial_5!(happyShift, curry_1_5!(action_49)),
        44 => partial_5!(happyGoto, curry_1_5!(action_286)),
        45 => partial_5!(happyGoto, curry_1_5!(action_89)),
        47 => partial_5!(happyGoto, curry_1_5!(action_287)),
        49 => partial_5!(happyGoto, curry_1_5!(action_288)),
        51 => partial_5!(happyGoto, curry_1_5!(action_289)),
        52 => partial_5!(happyGoto, curry_1_5!(action_96)),
        53 => partial_5!(happyGoto, curry_1_5!(action_97)),
        54 => partial_5!(happyGoto, curry_1_5!(action_98)),
        61 => partial_5!(happyGoto, curry_1_5!(action_99)),
        65 => partial_5!(happyGoto, curry_1_5!(action_290)),
        86 => partial_5!(happyGoto, curry_1_5!(action_563)),
        100 => partial_5!(happyGoto, curry_1_5!(action_6)),
        104 => partial_5!(happyGoto, curry_1_5!(action_7)),
        106 => partial_5!(happyGoto, curry_1_5!(action_8)),
        107 => partial_5!(happyGoto, curry_1_5!(action_9)),
        108 => partial_5!(happyGoto, curry_1_5!(action_10)),
        109 => partial_5!(happyGoto, curry_1_5!(action_11)),
        110 => partial_5!(happyGoto, curry_1_5!(action_12)),
        111 => partial_5!(happyGoto, curry_1_5!(action_13)),
        112 => partial_5!(happyGoto, curry_1_5!(action_14)),
        113 => partial_5!(happyGoto, curry_1_5!(action_15)),
        114 => partial_5!(happyGoto, curry_1_5!(action_16)),
        115 => partial_5!(happyGoto, curry_1_5!(action_17)),
        116 => partial_5!(happyGoto, curry_1_5!(action_18)),
        117 => partial_5!(happyGoto, curry_1_5!(action_19)),
        118 => partial_5!(happyGoto, curry_1_5!(action_20)),
        119 => partial_5!(happyGoto, curry_1_5!(action_21)),
        120 => partial_5!(happyGoto, curry_1_5!(action_22)),
        122 => partial_5!(happyGoto, curry_1_5!(action_564)),
        127 => partial_5!(happyGoto, curry_1_5!(action_24)),
        128 => partial_5!(happyGoto, curry_1_5!(action_25)),
        132 => partial_5!(happyGoto, curry_1_5!(action_108)),
        133 => partial_5!(happyGoto, curry_1_5!(action_293)),
        134 => partial_5!(happyGoto, curry_1_5!(action_110)),
        _ => happyReduce_471()
    }
}

fn action_431(i: isize) -> ActionReturn {
    match i {
        181 => partial_5!(happyShift, curry_1_5!(action_61)),
        14 => partial_5!(happyGoto, curry_1_5!(action_562)),
        _ => box happyFail
    }
}

fn action_432(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_107()
    }
}

fn action_433(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_150()
    }
}

fn action_434(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_161()
    }
}

fn action_435(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_229()
    }
}

fn action_436(i: isize) -> ActionReturn {
    match i {
        181 => happyReduce_26(),
        187 => partial_5!(happyShift, curry_1_5!(action_406)),
        35 => partial_5!(happyGoto, curry_1_5!(action_561)),
        67 => partial_5!(happyGoto, curry_1_5!(action_405)),
        _ => happyReduce_232()
    }
}

fn action_437(i: isize) -> ActionReturn {
    match i {
        33 => partial_5!(happyGoto, curry_1_5!(action_560)),
        _ => happyReduce_90()
    }
}

fn action_438(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_559)),
        _ => box happyFail
    }
}

fn action_439(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_179()
    }
}

fn action_440(i: isize) -> ActionReturn {
    match i {
        181 => partial_5!(happyShift, curry_1_5!(action_61)),
        185 => partial_5!(happyShift, curry_1_5!(action_113)),
        188 => partial_5!(happyShift, curry_1_5!(action_115)),
        190 => partial_5!(happyShift, curry_1_5!(action_116)),
        192 => partial_5!(happyShift, curry_1_5!(action_117)),
        195 => partial_5!(happyShift, curry_1_5!(action_118)),
        198 => partial_5!(happyShift, curry_1_5!(action_119)),
        200 => partial_5!(happyShift, curry_1_5!(action_120)),
        201 => partial_5!(happyShift, curry_1_5!(action_121)),
        202 => partial_5!(happyShift, curry_1_5!(action_122)),
        207 => partial_5!(happyShift, curry_1_5!(action_123)),
        208 => partial_5!(happyShift, curry_1_5!(action_124)),
        209 => partial_5!(happyShift, curry_1_5!(action_125)),
        210 => partial_5!(happyShift, curry_1_5!(action_126)),
        212 => partial_5!(happyShift, curry_1_5!(action_127)),
        215 => partial_5!(happyShift, curry_1_5!(action_128)),
        218 => partial_5!(happyShift, curry_1_5!(action_129)),
        219 => partial_5!(happyShift, curry_1_5!(action_130)),
        221 => partial_5!(happyShift, curry_1_5!(action_131)),
        222 => partial_5!(happyShift, curry_1_5!(action_132)),
        223 => partial_5!(happyShift, curry_1_5!(action_133)),
        225 => partial_5!(happyShift, curry_1_5!(action_134)),
        226 => partial_5!(happyShift, curry_1_5!(action_135)),
        227 => partial_5!(happyShift, curry_1_5!(action_136)),
        228 => partial_5!(happyShift, curry_1_5!(action_137)),
        229 => partial_5!(happyShift, curry_1_5!(action_138)),
        230 => partial_5!(happyShift, curry_1_5!(action_139)),
        238 => partial_5!(happyShift, curry_1_5!(action_141)),
        239 => partial_5!(happyShift, curry_1_5!(action_142)),
        14 => partial_5!(happyGoto, curry_1_5!(action_558)),
        32 => partial_5!(happyGoto, curry_1_5!(action_463)),
        34 => partial_5!(happyGoto, curry_1_5!(action_80)),
        36 => partial_5!(happyGoto, curry_1_5!(action_81)),
        37 => partial_5!(happyGoto, curry_1_5!(action_464)),
        38 => partial_5!(happyGoto, curry_1_5!(action_465)),
        40 => partial_5!(happyGoto, curry_1_5!(action_84)),
        41 => partial_5!(happyGoto, curry_1_5!(action_85)),
        42 => partial_5!(happyGoto, curry_1_5!(action_86)),
        43 => partial_5!(happyGoto, curry_1_5!(action_87)),
        44 => partial_5!(happyGoto, curry_1_5!(action_466)),
        45 => partial_5!(happyGoto, curry_1_5!(action_89)),
        46 => partial_5!(happyGoto, curry_1_5!(action_90)),
        47 => partial_5!(happyGoto, curry_1_5!(action_91)),
        48 => partial_5!(happyGoto, curry_1_5!(action_92)),
        49 => partial_5!(happyGoto, curry_1_5!(action_93)),
        50 => partial_5!(happyGoto, curry_1_5!(action_94)),
        51 => partial_5!(happyGoto, curry_1_5!(action_95)),
        52 => partial_5!(happyGoto, curry_1_5!(action_96)),
        53 => partial_5!(happyGoto, curry_1_5!(action_97)),
        54 => partial_5!(happyGoto, curry_1_5!(action_98)),
        61 => partial_5!(happyGoto, curry_1_5!(action_99)),
        65 => partial_5!(happyGoto, curry_1_5!(action_467)),
        132 => partial_5!(happyGoto, curry_1_5!(action_108)),
        133 => partial_5!(happyGoto, curry_1_5!(action_468)),
        134 => partial_5!(happyGoto, curry_1_5!(action_110)),
        _ => happyReduce_471()
    }
}

fn action_441(i: isize) -> ActionReturn {
    match i {
        168 => partial_5!(happyShift, curry_1_5!(action_516)),
        94 => partial_5!(happyGoto, curry_1_5!(action_557)),
        _ => happyReduce_344()
    }
}

fn action_442(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_17()
    }
}

fn action_443(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_26)),
        144 => partial_5!(happyShift, curry_1_5!(action_27)),
        145 => partial_5!(happyShift, curry_1_5!(action_28)),
        146 => partial_5!(happyShift, curry_1_5!(action_29)),
        147 => partial_5!(happyShift, curry_1_5!(action_30)),
        148 => partial_5!(happyShift, curry_1_5!(action_31)),
        149 => partial_5!(happyShift, curry_1_5!(action_32)),
        150 => partial_5!(happyShift, curry_1_5!(action_556)),
        153 => partial_5!(happyShift, curry_1_5!(action_34)),
        164 => partial_5!(happyShift, curry_1_5!(action_35)),
        184 => partial_5!(happyShift, curry_1_5!(action_36)),
        186 => partial_5!(happyShift, curry_1_5!(action_171)),
        193 => partial_5!(happyShift, curry_1_5!(action_172)),
        204 => partial_5!(happyShift, curry_1_5!(action_37)),
        213 => partial_5!(happyShift, curry_1_5!(action_173)),
        214 => partial_5!(happyShift, curry_1_5!(action_174)),
        216 => partial_5!(happyShift, curry_1_5!(action_175)),
        220 => partial_5!(happyShift, curry_1_5!(action_38)),
        221 => happyReduce_471(),
        231 => partial_5!(happyShift, curry_1_5!(action_176)),
        233 => partial_5!(happyShift, curry_1_5!(action_39)),
        234 => partial_5!(happyShift, curry_1_5!(action_40)),
        235 => partial_5!(happyShift, curry_1_5!(action_41)),
        236 => partial_5!(happyShift, curry_1_5!(action_42)),
        237 => partial_5!(happyShift, curry_1_5!(action_43)),
        239 => partial_5!(happyShift, curry_1_5!(action_142)),
        240 => partial_5!(happyShift, curry_1_5!(action_44)),
        241 => partial_5!(happyShift, curry_1_5!(action_45)),
        242 => partial_5!(happyShift, curry_1_5!(action_46)),
        243 => partial_5!(happyShift, curry_1_5!(action_47)),
        244 => partial_5!(happyShift, curry_1_5!(action_48)),
        245 => partial_5!(happyShift, curry_1_5!(action_49)),
        64 => partial_5!(happyGoto, curry_1_5!(action_187)),
        100 => partial_5!(happyGoto, curry_1_5!(action_6)),
        104 => partial_5!(happyGoto, curry_1_5!(action_7)),
        106 => partial_5!(happyGoto, curry_1_5!(action_8)),
        107 => partial_5!(happyGoto, curry_1_5!(action_9)),
        108 => partial_5!(happyGoto, curry_1_5!(action_10)),
        109 => partial_5!(happyGoto, curry_1_5!(action_11)),
        110 => partial_5!(happyGoto, curry_1_5!(action_12)),
        111 => partial_5!(happyGoto, curry_1_5!(action_13)),
        112 => partial_5!(happyGoto, curry_1_5!(action_14)),
        113 => partial_5!(happyGoto, curry_1_5!(action_15)),
        114 => partial_5!(happyGoto, curry_1_5!(action_16)),
        115 => partial_5!(happyGoto, curry_1_5!(action_17)),
        116 => partial_5!(happyGoto, curry_1_5!(action_18)),
        117 => partial_5!(happyGoto, curry_1_5!(action_19)),
        118 => partial_5!(happyGoto, curry_1_5!(action_20)),
        119 => partial_5!(happyGoto, curry_1_5!(action_21)),
        120 => partial_5!(happyGoto, curry_1_5!(action_444)),
        125 => partial_5!(happyGoto, curry_1_5!(action_553)),
        127 => partial_5!(happyGoto, curry_1_5!(action_24)),
        128 => partial_5!(happyGoto, curry_1_5!(action_25)),
        132 => partial_5!(happyGoto, curry_1_5!(action_554)),
        133 => partial_5!(happyGoto, curry_1_5!(action_555)),
        134 => partial_5!(happyGoto, curry_1_5!(action_110)),
        _ => happyReduce_458()
    }
}

fn action_444(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_459()
    }
}

fn action_445(i: isize) -> ActionReturn {
    match i {
        141 => partial_5!(happyShift, curry_1_5!(action_552)),
        _ => box happyFail
    }
}

fn action_446(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_26)),
        144 => partial_5!(happyShift, curry_1_5!(action_27)),
        145 => partial_5!(happyShift, curry_1_5!(action_28)),
        146 => partial_5!(happyShift, curry_1_5!(action_29)),
        147 => partial_5!(happyShift, curry_1_5!(action_30)),
        148 => partial_5!(happyShift, curry_1_5!(action_31)),
        149 => partial_5!(happyShift, curry_1_5!(action_32)),
        150 => partial_5!(happyShift, curry_1_5!(action_551)),
        153 => partial_5!(happyShift, curry_1_5!(action_34)),
        164 => partial_5!(happyShift, curry_1_5!(action_35)),
        184 => partial_5!(happyShift, curry_1_5!(action_36)),
        186 => happyReduce_472(),
        193 => happyReduce_472(),
        204 => partial_5!(happyShift, curry_1_5!(action_37)),
        213 => happyReduce_472(),
        214 => happyReduce_472(),
        216 => happyReduce_472(),
        220 => partial_5!(happyShift, curry_1_5!(action_38)),
        231 => happyReduce_472(),
        233 => partial_5!(happyShift, curry_1_5!(action_39)),
        234 => partial_5!(happyShift, curry_1_5!(action_40)),
        235 => partial_5!(happyShift, curry_1_5!(action_41)),
        236 => partial_5!(happyShift, curry_1_5!(action_42)),
        237 => partial_5!(happyShift, curry_1_5!(action_43)),
        239 => partial_5!(happyShift, curry_1_5!(action_142)),
        240 => partial_5!(happyShift, curry_1_5!(action_44)),
        241 => partial_5!(happyShift, curry_1_5!(action_45)),
        242 => partial_5!(happyShift, curry_1_5!(action_46)),
        243 => partial_5!(happyShift, curry_1_5!(action_47)),
        244 => partial_5!(happyShift, curry_1_5!(action_48)),
        245 => partial_5!(happyShift, curry_1_5!(action_49)),
        100 => partial_5!(happyGoto, curry_1_5!(action_6)),
        104 => partial_5!(happyGoto, curry_1_5!(action_7)),
        106 => partial_5!(happyGoto, curry_1_5!(action_8)),
        107 => partial_5!(happyGoto, curry_1_5!(action_9)),
        108 => partial_5!(happyGoto, curry_1_5!(action_10)),
        109 => partial_5!(happyGoto, curry_1_5!(action_11)),
        110 => partial_5!(happyGoto, curry_1_5!(action_12)),
        111 => partial_5!(happyGoto, curry_1_5!(action_13)),
        112 => partial_5!(happyGoto, curry_1_5!(action_14)),
        113 => partial_5!(happyGoto, curry_1_5!(action_15)),
        114 => partial_5!(happyGoto, curry_1_5!(action_16)),
        115 => partial_5!(happyGoto, curry_1_5!(action_17)),
        116 => partial_5!(happyGoto, curry_1_5!(action_18)),
        117 => partial_5!(happyGoto, curry_1_5!(action_19)),
        118 => partial_5!(happyGoto, curry_1_5!(action_20)),
        119 => partial_5!(happyGoto, curry_1_5!(action_21)),
        120 => partial_5!(happyGoto, curry_1_5!(action_444)),
        125 => partial_5!(happyGoto, curry_1_5!(action_550)),
        127 => partial_5!(happyGoto, curry_1_5!(action_24)),
        128 => partial_5!(happyGoto, curry_1_5!(action_25)),
        134 => partial_5!(happyGoto, curry_1_5!(action_167)),
        _ => happyReduce_458()
    }
}

fn action_447(i: isize) -> ActionReturn {
    match i {
        141 => happyReduce_471(),
        239 => partial_5!(happyShift, curry_1_5!(action_142)),
        132 => partial_5!(happyGoto, curry_1_5!(action_549)),
        133 => partial_5!(happyGoto, curry_1_5!(action_150)),
        134 => partial_5!(happyGoto, curry_1_5!(action_110)),
        _ => happyReduce_401()
    }
}

fn action_448(i: isize) -> ActionReturn {
    match i {
        239 => partial_5!(happyShift, curry_1_5!(action_142)),
        65 => partial_5!(happyGoto, curry_1_5!(action_547)),
        132 => partial_5!(happyGoto, curry_1_5!(action_548)),
        133 => partial_5!(happyGoto, curry_1_5!(action_150)),
        134 => partial_5!(happyGoto, curry_1_5!(action_110)),
        _ => happyReduce_471()
    }
}

fn action_449(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_540)),
        140 => partial_5!(happyShift, curry_1_5!(action_182)),
        150 => partial_5!(happyShift, curry_1_5!(action_541)),
        237 => partial_5!(happyShift, curry_1_5!(action_140)),
        238 => partial_5!(happyShift, curry_1_5!(action_229)),
        69 => partial_5!(happyGoto, curry_1_5!(action_544)),
        70 => partial_5!(happyGoto, curry_1_5!(action_221)),
        71 => partial_5!(happyGoto, curry_1_5!(action_222)),
        75 => partial_5!(happyGoto, curry_1_5!(action_545)),
        76 => partial_5!(happyGoto, curry_1_5!(action_102)),
        77 => partial_5!(happyGoto, curry_1_5!(action_103)),
        78 => partial_5!(happyGoto, curry_1_5!(action_481)),
        87 => partial_5!(happyGoto, curry_1_5!(action_546)),
        88 => partial_5!(happyGoto, curry_1_5!(action_367)),
        89 => partial_5!(happyGoto, curry_1_5!(action_179)),
        90 => partial_5!(happyGoto, curry_1_5!(action_180)),
        91 => partial_5!(happyGoto, curry_1_5!(action_368)),
        92 => partial_5!(happyGoto, curry_1_5!(action_369)),
        _ => happyReduce_287()
    }
}

fn action_450(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_535)),
        140 => partial_5!(happyShift, curry_1_5!(action_182)),
        150 => partial_5!(happyShift, curry_1_5!(action_536)),
        185 => partial_5!(happyShift, curry_1_5!(action_113)),
        186 => partial_5!(happyShift, curry_1_5!(action_171)),
        188 => partial_5!(happyShift, curry_1_5!(action_115)),
        190 => partial_5!(happyShift, curry_1_5!(action_116)),
        192 => partial_5!(happyShift, curry_1_5!(action_117)),
        193 => partial_5!(happyShift, curry_1_5!(action_172)),
        195 => partial_5!(happyShift, curry_1_5!(action_118)),
        198 => partial_5!(happyShift, curry_1_5!(action_119)),
        200 => partial_5!(happyShift, curry_1_5!(action_120)),
        201 => partial_5!(happyShift, curry_1_5!(action_121)),
        202 => partial_5!(happyShift, curry_1_5!(action_122)),
        207 => partial_5!(happyShift, curry_1_5!(action_123)),
        208 => partial_5!(happyShift, curry_1_5!(action_124)),
        209 => partial_5!(happyShift, curry_1_5!(action_125)),
        210 => partial_5!(happyShift, curry_1_5!(action_126)),
        212 => partial_5!(happyShift, curry_1_5!(action_127)),
        213 => partial_5!(happyShift, curry_1_5!(action_173)),
        214 => partial_5!(happyShift, curry_1_5!(action_174)),
        215 => partial_5!(happyShift, curry_1_5!(action_128)),
        216 => partial_5!(happyShift, curry_1_5!(action_175)),
        218 => partial_5!(happyShift, curry_1_5!(action_129)),
        219 => partial_5!(happyShift, curry_1_5!(action_130)),
        221 => partial_5!(happyShift, curry_1_5!(action_131)),
        223 => partial_5!(happyShift, curry_1_5!(action_133)),
        225 => partial_5!(happyShift, curry_1_5!(action_134)),
        226 => partial_5!(happyShift, curry_1_5!(action_237)),
        227 => partial_5!(happyShift, curry_1_5!(action_136)),
        228 => partial_5!(happyShift, curry_1_5!(action_137)),
        229 => partial_5!(happyShift, curry_1_5!(action_138)),
        230 => partial_5!(happyShift, curry_1_5!(action_139)),
        231 => partial_5!(happyShift, curry_1_5!(action_176)),
        237 => partial_5!(happyShift, curry_1_5!(action_140)),
        238 => partial_5!(happyShift, curry_1_5!(action_238)),
        239 => partial_5!(happyShift, curry_1_5!(action_142)),
        39 => partial_5!(happyGoto, curry_1_5!(action_231)),
        41 => partial_5!(happyGoto, curry_1_5!(action_198)),
        42 => partial_5!(happyGoto, curry_1_5!(action_199)),
        43 => partial_5!(happyGoto, curry_1_5!(action_200)),
        45 => partial_5!(happyGoto, curry_1_5!(action_232)),
        52 => partial_5!(happyGoto, curry_1_5!(action_233)),
        53 => partial_5!(happyGoto, curry_1_5!(action_97)),
        54 => partial_5!(happyGoto, curry_1_5!(action_98)),
        61 => partial_5!(happyGoto, curry_1_5!(action_99)),
        64 => partial_5!(happyGoto, curry_1_5!(action_201)),
        75 => partial_5!(happyGoto, curry_1_5!(action_542)),
        76 => partial_5!(happyGoto, curry_1_5!(action_102)),
        77 => partial_5!(happyGoto, curry_1_5!(action_103)),
        78 => partial_5!(happyGoto, curry_1_5!(action_481)),
        87 => partial_5!(happyGoto, curry_1_5!(action_543)),
        88 => partial_5!(happyGoto, curry_1_5!(action_367)),
        89 => partial_5!(happyGoto, curry_1_5!(action_179)),
        90 => partial_5!(happyGoto, curry_1_5!(action_180)),
        91 => partial_5!(happyGoto, curry_1_5!(action_368)),
        92 => partial_5!(happyGoto, curry_1_5!(action_369)),
        134 => partial_5!(happyGoto, curry_1_5!(action_236)),
        _ => happyReduce_291()
    }
}

fn action_451(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_540)),
        140 => partial_5!(happyShift, curry_1_5!(action_182)),
        150 => partial_5!(happyShift, curry_1_5!(action_541)),
        237 => partial_5!(happyShift, curry_1_5!(action_140)),
        238 => partial_5!(happyShift, curry_1_5!(action_229)),
        69 => partial_5!(happyGoto, curry_1_5!(action_537)),
        70 => partial_5!(happyGoto, curry_1_5!(action_221)),
        71 => partial_5!(happyGoto, curry_1_5!(action_222)),
        75 => partial_5!(happyGoto, curry_1_5!(action_538)),
        76 => partial_5!(happyGoto, curry_1_5!(action_102)),
        77 => partial_5!(happyGoto, curry_1_5!(action_103)),
        78 => partial_5!(happyGoto, curry_1_5!(action_481)),
        87 => partial_5!(happyGoto, curry_1_5!(action_539)),
        88 => partial_5!(happyGoto, curry_1_5!(action_367)),
        89 => partial_5!(happyGoto, curry_1_5!(action_179)),
        90 => partial_5!(happyGoto, curry_1_5!(action_180)),
        91 => partial_5!(happyGoto, curry_1_5!(action_368)),
        92 => partial_5!(happyGoto, curry_1_5!(action_369)),
        _ => happyReduce_294()
    }
}

fn action_452(i: isize) -> ActionReturn {
    match i {
        185 => partial_5!(happyShift, curry_1_5!(action_113)),
        186 => partial_5!(happyShift, curry_1_5!(action_171)),
        188 => partial_5!(happyShift, curry_1_5!(action_115)),
        193 => partial_5!(happyShift, curry_1_5!(action_172)),
        201 => partial_5!(happyShift, curry_1_5!(action_121)),
        207 => partial_5!(happyShift, curry_1_5!(action_123)),
        212 => partial_5!(happyShift, curry_1_5!(action_127)),
        213 => partial_5!(happyShift, curry_1_5!(action_173)),
        214 => partial_5!(happyShift, curry_1_5!(action_174)),
        215 => partial_5!(happyShift, curry_1_5!(action_128)),
        216 => partial_5!(happyShift, curry_1_5!(action_175)),
        221 => partial_5!(happyShift, curry_1_5!(action_131)),
        225 => partial_5!(happyShift, curry_1_5!(action_134)),
        227 => partial_5!(happyShift, curry_1_5!(action_136)),
        231 => partial_5!(happyShift, curry_1_5!(action_176)),
        239 => partial_5!(happyShift, curry_1_5!(action_142)),
        39 => partial_5!(happyGoto, curry_1_5!(action_207)),
        41 => partial_5!(happyGoto, curry_1_5!(action_198)),
        42 => partial_5!(happyGoto, curry_1_5!(action_199)),
        43 => partial_5!(happyGoto, curry_1_5!(action_200)),
        64 => partial_5!(happyGoto, curry_1_5!(action_201)),
        134 => partial_5!(happyGoto, curry_1_5!(action_208)),
        _ => happyReduce_102()
    }
}

fn action_453(i: isize) -> ActionReturn {
    match i {
        186 => partial_5!(happyShift, curry_1_5!(action_171)),
        188 => partial_5!(happyShift, curry_1_5!(action_115)),
        193 => partial_5!(happyShift, curry_1_5!(action_172)),
        201 => partial_5!(happyShift, curry_1_5!(action_121)),
        213 => partial_5!(happyShift, curry_1_5!(action_173)),
        214 => partial_5!(happyShift, curry_1_5!(action_174)),
        215 => partial_5!(happyShift, curry_1_5!(action_128)),
        216 => partial_5!(happyShift, curry_1_5!(action_175)),
        221 => partial_5!(happyShift, curry_1_5!(action_131)),
        225 => partial_5!(happyShift, curry_1_5!(action_134)),
        227 => partial_5!(happyShift, curry_1_5!(action_136)),
        231 => partial_5!(happyShift, curry_1_5!(action_176)),
        239 => partial_5!(happyShift, curry_1_5!(action_142)),
        41 => partial_5!(happyGoto, curry_1_5!(action_203)),
        64 => partial_5!(happyGoto, curry_1_5!(action_204)),
        134 => partial_5!(happyGoto, curry_1_5!(action_205)),
        _ => happyReduce_128()
    }
}

fn action_454(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_535)),
        140 => partial_5!(happyShift, curry_1_5!(action_182)),
        150 => partial_5!(happyShift, curry_1_5!(action_536)),
        185 => partial_5!(happyShift, curry_1_5!(action_113)),
        186 => partial_5!(happyShift, curry_1_5!(action_171)),
        188 => partial_5!(happyShift, curry_1_5!(action_115)),
        190 => partial_5!(happyShift, curry_1_5!(action_116)),
        192 => partial_5!(happyShift, curry_1_5!(action_117)),
        193 => partial_5!(happyShift, curry_1_5!(action_172)),
        195 => partial_5!(happyShift, curry_1_5!(action_118)),
        198 => partial_5!(happyShift, curry_1_5!(action_119)),
        200 => partial_5!(happyShift, curry_1_5!(action_120)),
        201 => partial_5!(happyShift, curry_1_5!(action_121)),
        202 => partial_5!(happyShift, curry_1_5!(action_122)),
        207 => partial_5!(happyShift, curry_1_5!(action_123)),
        208 => partial_5!(happyShift, curry_1_5!(action_124)),
        209 => partial_5!(happyShift, curry_1_5!(action_125)),
        210 => partial_5!(happyShift, curry_1_5!(action_126)),
        212 => partial_5!(happyShift, curry_1_5!(action_127)),
        213 => partial_5!(happyShift, curry_1_5!(action_173)),
        214 => partial_5!(happyShift, curry_1_5!(action_174)),
        215 => partial_5!(happyShift, curry_1_5!(action_128)),
        216 => partial_5!(happyShift, curry_1_5!(action_175)),
        218 => partial_5!(happyShift, curry_1_5!(action_129)),
        219 => partial_5!(happyShift, curry_1_5!(action_130)),
        221 => partial_5!(happyShift, curry_1_5!(action_131)),
        223 => partial_5!(happyShift, curry_1_5!(action_133)),
        225 => partial_5!(happyShift, curry_1_5!(action_134)),
        226 => partial_5!(happyShift, curry_1_5!(action_191)),
        227 => partial_5!(happyShift, curry_1_5!(action_136)),
        228 => partial_5!(happyShift, curry_1_5!(action_137)),
        229 => partial_5!(happyShift, curry_1_5!(action_138)),
        230 => partial_5!(happyShift, curry_1_5!(action_139)),
        231 => partial_5!(happyShift, curry_1_5!(action_176)),
        237 => partial_5!(happyShift, curry_1_5!(action_140)),
        238 => partial_5!(happyShift, curry_1_5!(action_192)),
        239 => partial_5!(happyShift, curry_1_5!(action_142)),
        40 => partial_5!(happyGoto, curry_1_5!(action_184)),
        41 => partial_5!(happyGoto, curry_1_5!(action_85)),
        42 => partial_5!(happyGoto, curry_1_5!(action_86)),
        43 => partial_5!(happyGoto, curry_1_5!(action_87)),
        45 => partial_5!(happyGoto, curry_1_5!(action_185)),
        52 => partial_5!(happyGoto, curry_1_5!(action_186)),
        53 => partial_5!(happyGoto, curry_1_5!(action_97)),
        54 => partial_5!(happyGoto, curry_1_5!(action_98)),
        61 => partial_5!(happyGoto, curry_1_5!(action_99)),
        64 => partial_5!(happyGoto, curry_1_5!(action_187)),
        75 => partial_5!(happyGoto, curry_1_5!(action_531)),
        76 => partial_5!(happyGoto, curry_1_5!(action_102)),
        77 => partial_5!(happyGoto, curry_1_5!(action_103)),
        78 => partial_5!(happyGoto, curry_1_5!(action_481)),
        87 => partial_5!(happyGoto, curry_1_5!(action_532)),
        88 => partial_5!(happyGoto, curry_1_5!(action_367)),
        89 => partial_5!(happyGoto, curry_1_5!(action_179)),
        90 => partial_5!(happyGoto, curry_1_5!(action_180)),
        91 => partial_5!(happyGoto, curry_1_5!(action_368)),
        92 => partial_5!(happyGoto, curry_1_5!(action_369)),
        133 => partial_5!(happyGoto, curry_1_5!(action_533)),
        134 => partial_5!(happyGoto, curry_1_5!(action_534)),
        _ => happyReduce_298()
    }
}

fn action_455(i: isize) -> ActionReturn {
    match i {
        139 => partial_5!(happyShift, curry_1_5!(action_530)),
        _ => box happyFail
    }
}

fn action_456(i: isize) -> ActionReturn {
    match i {
        179 => partial_5!(happyShift, curry_1_5!(action_529)),
        _ => happyReduce_283()
    }
}

fn action_457(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_285()
    }
}

fn action_458(i: isize) -> ActionReturn {
    match i {
        139 => partial_5!(happyShift, curry_1_5!(action_527)),
        179 => partial_5!(happyShift, curry_1_5!(action_528)),
        _ => box happyFail
    }
}

fn action_459(i: isize) -> ActionReturn {
    match i {
        185 => partial_5!(happyShift, curry_1_5!(action_113)),
        188 => partial_5!(happyShift, curry_1_5!(action_115)),
        190 => partial_5!(happyShift, curry_1_5!(action_116)),
        192 => partial_5!(happyShift, curry_1_5!(action_117)),
        195 => partial_5!(happyShift, curry_1_5!(action_118)),
        198 => partial_5!(happyShift, curry_1_5!(action_119)),
        200 => partial_5!(happyShift, curry_1_5!(action_120)),
        201 => partial_5!(happyShift, curry_1_5!(action_121)),
        202 => partial_5!(happyShift, curry_1_5!(action_122)),
        207 => partial_5!(happyShift, curry_1_5!(action_123)),
        208 => partial_5!(happyShift, curry_1_5!(action_124)),
        209 => partial_5!(happyShift, curry_1_5!(action_125)),
        210 => partial_5!(happyShift, curry_1_5!(action_126)),
        212 => partial_5!(happyShift, curry_1_5!(action_127)),
        215 => partial_5!(happyShift, curry_1_5!(action_128)),
        218 => partial_5!(happyShift, curry_1_5!(action_129)),
        219 => partial_5!(happyShift, curry_1_5!(action_130)),
        221 => partial_5!(happyShift, curry_1_5!(action_131)),
        223 => partial_5!(happyShift, curry_1_5!(action_133)),
        225 => partial_5!(happyShift, curry_1_5!(action_134)),
        226 => partial_5!(happyShift, curry_1_5!(action_168)),
        227 => partial_5!(happyShift, curry_1_5!(action_136)),
        228 => partial_5!(happyShift, curry_1_5!(action_137)),
        229 => partial_5!(happyShift, curry_1_5!(action_138)),
        230 => partial_5!(happyShift, curry_1_5!(action_139)),
        238 => partial_5!(happyShift, curry_1_5!(action_169)),
        239 => partial_5!(happyShift, curry_1_5!(action_142)),
        40 => partial_5!(happyGoto, curry_1_5!(action_162)),
        41 => partial_5!(happyGoto, curry_1_5!(action_85)),
        42 => partial_5!(happyGoto, curry_1_5!(action_86)),
        43 => partial_5!(happyGoto, curry_1_5!(action_87)),
        45 => partial_5!(happyGoto, curry_1_5!(action_163)),
        52 => partial_5!(happyGoto, curry_1_5!(action_164)),
        53 => partial_5!(happyGoto, curry_1_5!(action_97)),
        54 => partial_5!(happyGoto, curry_1_5!(action_98)),
        61 => partial_5!(happyGoto, curry_1_5!(action_99)),
        134 => partial_5!(happyGoto, curry_1_5!(action_167)),
        _ => happyReduce_472()
    }
}

fn action_460(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_302()
    }
}

fn action_461(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_314()
    }
}

fn action_462(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_19()
    }
}

fn action_463(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_91()
    }
}

fn action_464(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_524)),
        150 => partial_5!(happyShift, curry_1_5!(action_525)),
        237 => partial_5!(happyShift, curry_1_5!(action_140)),
        238 => partial_5!(happyShift, curry_1_5!(action_229)),
        66 => partial_5!(happyGoto, curry_1_5!(action_240)),
        68 => partial_5!(happyGoto, curry_1_5!(action_219)),
        69 => partial_5!(happyGoto, curry_1_5!(action_220)),
        70 => partial_5!(happyGoto, curry_1_5!(action_221)),
        71 => partial_5!(happyGoto, curry_1_5!(action_222)),
        72 => partial_5!(happyGoto, curry_1_5!(action_223)),
        73 => partial_5!(happyGoto, curry_1_5!(action_224)),
        75 => partial_5!(happyGoto, curry_1_5!(action_523)),
        76 => partial_5!(happyGoto, curry_1_5!(action_102)),
        77 => partial_5!(happyGoto, curry_1_5!(action_103)),
        78 => partial_5!(happyGoto, curry_1_5!(action_481)),
        _ => box happyFail
    }
}

fn action_465(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_475)),
        150 => partial_5!(happyShift, curry_1_5!(action_476)),
        185 => partial_5!(happyShift, curry_1_5!(action_113)),
        186 => partial_5!(happyShift, curry_1_5!(action_171)),
        188 => partial_5!(happyShift, curry_1_5!(action_115)),
        190 => partial_5!(happyShift, curry_1_5!(action_116)),
        192 => partial_5!(happyShift, curry_1_5!(action_117)),
        193 => partial_5!(happyShift, curry_1_5!(action_172)),
        195 => partial_5!(happyShift, curry_1_5!(action_118)),
        198 => partial_5!(happyShift, curry_1_5!(action_119)),
        200 => partial_5!(happyShift, curry_1_5!(action_120)),
        201 => partial_5!(happyShift, curry_1_5!(action_121)),
        202 => partial_5!(happyShift, curry_1_5!(action_122)),
        207 => partial_5!(happyShift, curry_1_5!(action_123)),
        208 => partial_5!(happyShift, curry_1_5!(action_124)),
        209 => partial_5!(happyShift, curry_1_5!(action_125)),
        210 => partial_5!(happyShift, curry_1_5!(action_126)),
        212 => partial_5!(happyShift, curry_1_5!(action_127)),
        213 => partial_5!(happyShift, curry_1_5!(action_173)),
        214 => partial_5!(happyShift, curry_1_5!(action_174)),
        215 => partial_5!(happyShift, curry_1_5!(action_128)),
        216 => partial_5!(happyShift, curry_1_5!(action_175)),
        218 => partial_5!(happyShift, curry_1_5!(action_129)),
        219 => partial_5!(happyShift, curry_1_5!(action_130)),
        221 => partial_5!(happyShift, curry_1_5!(action_131)),
        223 => partial_5!(happyShift, curry_1_5!(action_133)),
        225 => partial_5!(happyShift, curry_1_5!(action_134)),
        226 => partial_5!(happyShift, curry_1_5!(action_237)),
        227 => partial_5!(happyShift, curry_1_5!(action_136)),
        228 => partial_5!(happyShift, curry_1_5!(action_137)),
        229 => partial_5!(happyShift, curry_1_5!(action_138)),
        230 => partial_5!(happyShift, curry_1_5!(action_139)),
        231 => partial_5!(happyShift, curry_1_5!(action_176)),
        237 => partial_5!(happyShift, curry_1_5!(action_140)),
        238 => partial_5!(happyShift, curry_1_5!(action_238)),
        239 => partial_5!(happyShift, curry_1_5!(action_142)),
        39 => partial_5!(happyGoto, curry_1_5!(action_231)),
        41 => partial_5!(happyGoto, curry_1_5!(action_198)),
        42 => partial_5!(happyGoto, curry_1_5!(action_199)),
        43 => partial_5!(happyGoto, curry_1_5!(action_200)),
        45 => partial_5!(happyGoto, curry_1_5!(action_232)),
        52 => partial_5!(happyGoto, curry_1_5!(action_233)),
        53 => partial_5!(happyGoto, curry_1_5!(action_97)),
        54 => partial_5!(happyGoto, curry_1_5!(action_98)),
        61 => partial_5!(happyGoto, curry_1_5!(action_99)),
        64 => partial_5!(happyGoto, curry_1_5!(action_201)),
        75 => partial_5!(happyGoto, curry_1_5!(action_526)),
        76 => partial_5!(happyGoto, curry_1_5!(action_102)),
        77 => partial_5!(happyGoto, curry_1_5!(action_103)),
        78 => partial_5!(happyGoto, curry_1_5!(action_481)),
        134 => partial_5!(happyGoto, curry_1_5!(action_236)),
        _ => box happyFail
    }
}

fn action_466(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_524)),
        150 => partial_5!(happyShift, curry_1_5!(action_525)),
        237 => partial_5!(happyShift, curry_1_5!(action_140)),
        238 => partial_5!(happyShift, curry_1_5!(action_229)),
        66 => partial_5!(happyGoto, curry_1_5!(action_218)),
        68 => partial_5!(happyGoto, curry_1_5!(action_219)),
        69 => partial_5!(happyGoto, curry_1_5!(action_220)),
        70 => partial_5!(happyGoto, curry_1_5!(action_221)),
        71 => partial_5!(happyGoto, curry_1_5!(action_222)),
        72 => partial_5!(happyGoto, curry_1_5!(action_223)),
        73 => partial_5!(happyGoto, curry_1_5!(action_224)),
        75 => partial_5!(happyGoto, curry_1_5!(action_523)),
        76 => partial_5!(happyGoto, curry_1_5!(action_102)),
        77 => partial_5!(happyGoto, curry_1_5!(action_103)),
        78 => partial_5!(happyGoto, curry_1_5!(action_481)),
        _ => box happyFail
    }
}

fn action_467(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_475)),
        150 => partial_5!(happyShift, curry_1_5!(action_476)),
        185 => partial_5!(happyShift, curry_1_5!(action_113)),
        186 => partial_5!(happyShift, curry_1_5!(action_171)),
        188 => partial_5!(happyShift, curry_1_5!(action_115)),
        190 => partial_5!(happyShift, curry_1_5!(action_116)),
        192 => partial_5!(happyShift, curry_1_5!(action_117)),
        193 => partial_5!(happyShift, curry_1_5!(action_172)),
        195 => partial_5!(happyShift, curry_1_5!(action_118)),
        198 => partial_5!(happyShift, curry_1_5!(action_119)),
        200 => partial_5!(happyShift, curry_1_5!(action_120)),
        201 => partial_5!(happyShift, curry_1_5!(action_121)),
        202 => partial_5!(happyShift, curry_1_5!(action_122)),
        207 => partial_5!(happyShift, curry_1_5!(action_123)),
        208 => partial_5!(happyShift, curry_1_5!(action_124)),
        209 => partial_5!(happyShift, curry_1_5!(action_125)),
        210 => partial_5!(happyShift, curry_1_5!(action_126)),
        212 => partial_5!(happyShift, curry_1_5!(action_127)),
        213 => partial_5!(happyShift, curry_1_5!(action_173)),
        214 => partial_5!(happyShift, curry_1_5!(action_174)),
        215 => partial_5!(happyShift, curry_1_5!(action_128)),
        216 => partial_5!(happyShift, curry_1_5!(action_175)),
        218 => partial_5!(happyShift, curry_1_5!(action_129)),
        219 => partial_5!(happyShift, curry_1_5!(action_130)),
        221 => partial_5!(happyShift, curry_1_5!(action_131)),
        223 => partial_5!(happyShift, curry_1_5!(action_133)),
        225 => partial_5!(happyShift, curry_1_5!(action_134)),
        226 => partial_5!(happyShift, curry_1_5!(action_191)),
        227 => partial_5!(happyShift, curry_1_5!(action_136)),
        228 => partial_5!(happyShift, curry_1_5!(action_137)),
        229 => partial_5!(happyShift, curry_1_5!(action_138)),
        230 => partial_5!(happyShift, curry_1_5!(action_139)),
        231 => partial_5!(happyShift, curry_1_5!(action_176)),
        237 => partial_5!(happyShift, curry_1_5!(action_140)),
        238 => partial_5!(happyShift, curry_1_5!(action_192)),
        239 => partial_5!(happyShift, curry_1_5!(action_142)),
        40 => partial_5!(happyGoto, curry_1_5!(action_184)),
        41 => partial_5!(happyGoto, curry_1_5!(action_85)),
        42 => partial_5!(happyGoto, curry_1_5!(action_86)),
        43 => partial_5!(happyGoto, curry_1_5!(action_87)),
        45 => partial_5!(happyGoto, curry_1_5!(action_185)),
        52 => partial_5!(happyGoto, curry_1_5!(action_186)),
        53 => partial_5!(happyGoto, curry_1_5!(action_97)),
        54 => partial_5!(happyGoto, curry_1_5!(action_98)),
        61 => partial_5!(happyGoto, curry_1_5!(action_99)),
        64 => partial_5!(happyGoto, curry_1_5!(action_187)),
        75 => partial_5!(happyGoto, curry_1_5!(action_521)),
        76 => partial_5!(happyGoto, curry_1_5!(action_102)),
        77 => partial_5!(happyGoto, curry_1_5!(action_103)),
        78 => partial_5!(happyGoto, curry_1_5!(action_481)),
        133 => partial_5!(happyGoto, curry_1_5!(action_522)),
        134 => partial_5!(happyGoto, curry_1_5!(action_110)),
        _ => box happyFail
    }
}

fn action_468(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_475)),
        150 => partial_5!(happyShift, curry_1_5!(action_476)),
        185 => partial_5!(happyShift, curry_1_5!(action_113)),
        188 => partial_5!(happyShift, curry_1_5!(action_115)),
        190 => partial_5!(happyShift, curry_1_5!(action_116)),
        192 => partial_5!(happyShift, curry_1_5!(action_117)),
        195 => partial_5!(happyShift, curry_1_5!(action_118)),
        198 => partial_5!(happyShift, curry_1_5!(action_119)),
        200 => partial_5!(happyShift, curry_1_5!(action_120)),
        201 => partial_5!(happyShift, curry_1_5!(action_121)),
        202 => partial_5!(happyShift, curry_1_5!(action_122)),
        207 => partial_5!(happyShift, curry_1_5!(action_123)),
        208 => partial_5!(happyShift, curry_1_5!(action_124)),
        209 => partial_5!(happyShift, curry_1_5!(action_125)),
        210 => partial_5!(happyShift, curry_1_5!(action_126)),
        212 => partial_5!(happyShift, curry_1_5!(action_127)),
        215 => partial_5!(happyShift, curry_1_5!(action_128)),
        218 => partial_5!(happyShift, curry_1_5!(action_129)),
        219 => partial_5!(happyShift, curry_1_5!(action_130)),
        221 => partial_5!(happyShift, curry_1_5!(action_131)),
        223 => partial_5!(happyShift, curry_1_5!(action_133)),
        225 => partial_5!(happyShift, curry_1_5!(action_134)),
        226 => partial_5!(happyShift, curry_1_5!(action_168)),
        227 => partial_5!(happyShift, curry_1_5!(action_136)),
        228 => partial_5!(happyShift, curry_1_5!(action_137)),
        229 => partial_5!(happyShift, curry_1_5!(action_138)),
        230 => partial_5!(happyShift, curry_1_5!(action_139)),
        237 => partial_5!(happyShift, curry_1_5!(action_140)),
        238 => partial_5!(happyShift, curry_1_5!(action_169)),
        239 => partial_5!(happyShift, curry_1_5!(action_142)),
        40 => partial_5!(happyGoto, curry_1_5!(action_162)),
        41 => partial_5!(happyGoto, curry_1_5!(action_85)),
        42 => partial_5!(happyGoto, curry_1_5!(action_86)),
        43 => partial_5!(happyGoto, curry_1_5!(action_87)),
        45 => partial_5!(happyGoto, curry_1_5!(action_163)),
        52 => partial_5!(happyGoto, curry_1_5!(action_164)),
        53 => partial_5!(happyGoto, curry_1_5!(action_97)),
        54 => partial_5!(happyGoto, curry_1_5!(action_98)),
        61 => partial_5!(happyGoto, curry_1_5!(action_99)),
        75 => partial_5!(happyGoto, curry_1_5!(action_520)),
        76 => partial_5!(happyGoto, curry_1_5!(action_102)),
        77 => partial_5!(happyGoto, curry_1_5!(action_103)),
        78 => partial_5!(happyGoto, curry_1_5!(action_481)),
        134 => partial_5!(happyGoto, curry_1_5!(action_167)),
        _ => happyReduce_472()
    }
}

fn action_469(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_26)),
        144 => partial_5!(happyShift, curry_1_5!(action_27)),
        145 => partial_5!(happyShift, curry_1_5!(action_28)),
        146 => partial_5!(happyShift, curry_1_5!(action_29)),
        147 => partial_5!(happyShift, curry_1_5!(action_30)),
        148 => partial_5!(happyShift, curry_1_5!(action_31)),
        149 => partial_5!(happyShift, curry_1_5!(action_32)),
        150 => partial_5!(happyShift, curry_1_5!(action_33)),
        153 => partial_5!(happyShift, curry_1_5!(action_34)),
        164 => partial_5!(happyShift, curry_1_5!(action_35)),
        184 => partial_5!(happyShift, curry_1_5!(action_36)),
        190 => partial_5!(happyShift, curry_1_5!(action_116)),
        192 => partial_5!(happyShift, curry_1_5!(action_117)),
        195 => partial_5!(happyShift, curry_1_5!(action_118)),
        198 => partial_5!(happyShift, curry_1_5!(action_119)),
        200 => partial_5!(happyShift, curry_1_5!(action_120)),
        202 => partial_5!(happyShift, curry_1_5!(action_122)),
        204 => partial_5!(happyShift, curry_1_5!(action_37)),
        208 => partial_5!(happyShift, curry_1_5!(action_124)),
        209 => partial_5!(happyShift, curry_1_5!(action_125)),
        210 => partial_5!(happyShift, curry_1_5!(action_126)),
        218 => partial_5!(happyShift, curry_1_5!(action_129)),
        219 => partial_5!(happyShift, curry_1_5!(action_130)),
        220 => partial_5!(happyShift, curry_1_5!(action_38)),
        223 => partial_5!(happyShift, curry_1_5!(action_133)),
        226 => partial_5!(happyShift, curry_1_5!(action_135)),
        228 => partial_5!(happyShift, curry_1_5!(action_137)),
        229 => partial_5!(happyShift, curry_1_5!(action_138)),
        230 => partial_5!(happyShift, curry_1_5!(action_139)),
        233 => partial_5!(happyShift, curry_1_5!(action_39)),
        234 => partial_5!(happyShift, curry_1_5!(action_40)),
        235 => partial_5!(happyShift, curry_1_5!(action_41)),
        236 => partial_5!(happyShift, curry_1_5!(action_42)),
        237 => partial_5!(happyShift, curry_1_5!(action_43)),
        238 => partial_5!(happyShift, curry_1_5!(action_141)),
        239 => partial_5!(happyShift, curry_1_5!(action_142)),
        240 => partial_5!(happyShift, curry_1_5!(action_44)),
        241 => partial_5!(happyShift, curry_1_5!(action_45)),
        242 => partial_5!(happyShift, curry_1_5!(action_46)),
        243 => partial_5!(happyShift, curry_1_5!(action_47)),
        244 => partial_5!(happyShift, curry_1_5!(action_48)),
        245 => partial_5!(happyShift, curry_1_5!(action_49)),
        44 => partial_5!(happyGoto, curry_1_5!(action_286)),
        45 => partial_5!(happyGoto, curry_1_5!(action_89)),
        47 => partial_5!(happyGoto, curry_1_5!(action_287)),
        49 => partial_5!(happyGoto, curry_1_5!(action_288)),
        51 => partial_5!(happyGoto, curry_1_5!(action_289)),
        52 => partial_5!(happyGoto, curry_1_5!(action_96)),
        53 => partial_5!(happyGoto, curry_1_5!(action_97)),
        54 => partial_5!(happyGoto, curry_1_5!(action_98)),
        61 => partial_5!(happyGoto, curry_1_5!(action_99)),
        65 => partial_5!(happyGoto, curry_1_5!(action_290)),
        86 => partial_5!(happyGoto, curry_1_5!(action_518)),
        100 => partial_5!(happyGoto, curry_1_5!(action_6)),
        104 => partial_5!(happyGoto, curry_1_5!(action_7)),
        106 => partial_5!(happyGoto, curry_1_5!(action_8)),
        107 => partial_5!(happyGoto, curry_1_5!(action_9)),
        108 => partial_5!(happyGoto, curry_1_5!(action_10)),
        109 => partial_5!(happyGoto, curry_1_5!(action_11)),
        110 => partial_5!(happyGoto, curry_1_5!(action_12)),
        111 => partial_5!(happyGoto, curry_1_5!(action_13)),
        112 => partial_5!(happyGoto, curry_1_5!(action_14)),
        113 => partial_5!(happyGoto, curry_1_5!(action_15)),
        114 => partial_5!(happyGoto, curry_1_5!(action_16)),
        115 => partial_5!(happyGoto, curry_1_5!(action_17)),
        116 => partial_5!(happyGoto, curry_1_5!(action_18)),
        117 => partial_5!(happyGoto, curry_1_5!(action_19)),
        118 => partial_5!(happyGoto, curry_1_5!(action_20)),
        119 => partial_5!(happyGoto, curry_1_5!(action_21)),
        120 => partial_5!(happyGoto, curry_1_5!(action_22)),
        122 => partial_5!(happyGoto, curry_1_5!(action_519)),
        127 => partial_5!(happyGoto, curry_1_5!(action_24)),
        128 => partial_5!(happyGoto, curry_1_5!(action_25)),
        132 => partial_5!(happyGoto, curry_1_5!(action_108)),
        133 => partial_5!(happyGoto, curry_1_5!(action_293)),
        134 => partial_5!(happyGoto, curry_1_5!(action_110)),
        _ => happyReduce_471()
    }
}

fn action_470(i: isize) -> ActionReturn {
    match i {
        181 => partial_5!(happyShift, curry_1_5!(action_61)),
        185 => partial_5!(happyShift, curry_1_5!(action_113)),
        188 => partial_5!(happyShift, curry_1_5!(action_115)),
        190 => partial_5!(happyShift, curry_1_5!(action_116)),
        192 => partial_5!(happyShift, curry_1_5!(action_117)),
        195 => partial_5!(happyShift, curry_1_5!(action_118)),
        198 => partial_5!(happyShift, curry_1_5!(action_119)),
        200 => partial_5!(happyShift, curry_1_5!(action_120)),
        201 => partial_5!(happyShift, curry_1_5!(action_121)),
        202 => partial_5!(happyShift, curry_1_5!(action_122)),
        207 => partial_5!(happyShift, curry_1_5!(action_123)),
        208 => partial_5!(happyShift, curry_1_5!(action_124)),
        209 => partial_5!(happyShift, curry_1_5!(action_125)),
        210 => partial_5!(happyShift, curry_1_5!(action_126)),
        212 => partial_5!(happyShift, curry_1_5!(action_127)),
        215 => partial_5!(happyShift, curry_1_5!(action_128)),
        218 => partial_5!(happyShift, curry_1_5!(action_129)),
        219 => partial_5!(happyShift, curry_1_5!(action_130)),
        221 => partial_5!(happyShift, curry_1_5!(action_131)),
        222 => partial_5!(happyShift, curry_1_5!(action_132)),
        223 => partial_5!(happyShift, curry_1_5!(action_133)),
        225 => partial_5!(happyShift, curry_1_5!(action_134)),
        226 => partial_5!(happyShift, curry_1_5!(action_135)),
        227 => partial_5!(happyShift, curry_1_5!(action_136)),
        228 => partial_5!(happyShift, curry_1_5!(action_137)),
        229 => partial_5!(happyShift, curry_1_5!(action_138)),
        230 => partial_5!(happyShift, curry_1_5!(action_139)),
        238 => partial_5!(happyShift, curry_1_5!(action_141)),
        239 => partial_5!(happyShift, curry_1_5!(action_142)),
        14 => partial_5!(happyGoto, curry_1_5!(action_517)),
        32 => partial_5!(happyGoto, curry_1_5!(action_463)),
        34 => partial_5!(happyGoto, curry_1_5!(action_80)),
        36 => partial_5!(happyGoto, curry_1_5!(action_81)),
        37 => partial_5!(happyGoto, curry_1_5!(action_464)),
        38 => partial_5!(happyGoto, curry_1_5!(action_465)),
        40 => partial_5!(happyGoto, curry_1_5!(action_84)),
        41 => partial_5!(happyGoto, curry_1_5!(action_85)),
        42 => partial_5!(happyGoto, curry_1_5!(action_86)),
        43 => partial_5!(happyGoto, curry_1_5!(action_87)),
        44 => partial_5!(happyGoto, curry_1_5!(action_466)),
        45 => partial_5!(happyGoto, curry_1_5!(action_89)),
        46 => partial_5!(happyGoto, curry_1_5!(action_90)),
        47 => partial_5!(happyGoto, curry_1_5!(action_91)),
        48 => partial_5!(happyGoto, curry_1_5!(action_92)),
        49 => partial_5!(happyGoto, curry_1_5!(action_93)),
        50 => partial_5!(happyGoto, curry_1_5!(action_94)),
        51 => partial_5!(happyGoto, curry_1_5!(action_95)),
        52 => partial_5!(happyGoto, curry_1_5!(action_96)),
        53 => partial_5!(happyGoto, curry_1_5!(action_97)),
        54 => partial_5!(happyGoto, curry_1_5!(action_98)),
        61 => partial_5!(happyGoto, curry_1_5!(action_99)),
        65 => partial_5!(happyGoto, curry_1_5!(action_467)),
        132 => partial_5!(happyGoto, curry_1_5!(action_108)),
        133 => partial_5!(happyGoto, curry_1_5!(action_468)),
        134 => partial_5!(happyGoto, curry_1_5!(action_110)),
        _ => happyReduce_471()
    }
}

fn action_471(i: isize) -> ActionReturn {
    match i {
        168 => partial_5!(happyShift, curry_1_5!(action_516)),
        94 => partial_5!(happyGoto, curry_1_5!(action_515)),
        _ => happyReduce_344()
    }
}

fn action_472(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_13()
    }
}

fn action_473(i: isize) -> ActionReturn {
    match i {
        139 => partial_5!(happyShift, curry_1_5!(action_514)),
        _ => box happyFail
    }
}

fn action_474(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_413)),
        139 => partial_5!(happyShift, curry_1_5!(action_513)),
        140 => partial_5!(happyShift, curry_1_5!(action_182)),
        88 => partial_5!(happyGoto, curry_1_5!(action_178)),
        89 => partial_5!(happyGoto, curry_1_5!(action_179)),
        90 => partial_5!(happyGoto, curry_1_5!(action_180)),
        _ => box happyFail
    }
}

fn action_475(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_475)),
        150 => partial_5!(happyShift, curry_1_5!(action_476)),
        237 => partial_5!(happyShift, curry_1_5!(action_140)),
        239 => partial_5!(happyShift, curry_1_5!(action_142)),
        76 => partial_5!(happyGoto, curry_1_5!(action_157)),
        77 => partial_5!(happyGoto, curry_1_5!(action_103)),
        78 => partial_5!(happyGoto, curry_1_5!(action_512)),
        133 => partial_5!(happyGoto, curry_1_5!(action_160)),
        134 => partial_5!(happyGoto, curry_1_5!(action_110)),
        _ => box happyFail
    }
}

fn action_476(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_475)),
        150 => partial_5!(happyShift, curry_1_5!(action_476)),
        237 => partial_5!(happyShift, curry_1_5!(action_140)),
        239 => partial_5!(happyShift, curry_1_5!(action_142)),
        65 => partial_5!(happyGoto, curry_1_5!(action_511)),
        75 => partial_5!(happyGoto, curry_1_5!(action_154)),
        76 => partial_5!(happyGoto, curry_1_5!(action_102)),
        77 => partial_5!(happyGoto, curry_1_5!(action_103)),
        78 => partial_5!(happyGoto, curry_1_5!(action_481)),
        132 => partial_5!(happyGoto, curry_1_5!(action_108)),
        133 => partial_5!(happyGoto, curry_1_5!(action_156)),
        134 => partial_5!(happyGoto, curry_1_5!(action_110)),
        _ => happyReduce_471()
    }
}

fn action_477(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_413)),
        140 => partial_5!(happyShift, curry_1_5!(action_182)),
        88 => partial_5!(happyGoto, curry_1_5!(action_510)),
        89 => partial_5!(happyGoto, curry_1_5!(action_179)),
        90 => partial_5!(happyGoto, curry_1_5!(action_180)),
        _ => happyReduce_280()
    }
}

fn action_478(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_273()
    }
}

fn action_479(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_413)),
        140 => partial_5!(happyShift, curry_1_5!(action_182)),
        88 => partial_5!(happyGoto, curry_1_5!(action_509)),
        89 => partial_5!(happyGoto, curry_1_5!(action_179)),
        90 => partial_5!(happyGoto, curry_1_5!(action_180)),
        _ => happyReduce_268()
    }
}

fn action_480(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_264()
    }
}

fn action_481(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_413)),
        140 => partial_5!(happyShift, curry_1_5!(action_182)),
        88 => partial_5!(happyGoto, curry_1_5!(action_178)),
        89 => partial_5!(happyGoto, curry_1_5!(action_179)),
        90 => partial_5!(happyGoto, curry_1_5!(action_180)),
        _ => happyReduce_261()
    }
}

fn action_482(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_265()
    }
}

fn action_483(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_278()
    }
}

fn action_484(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_475)),
        150 => partial_5!(happyShift, curry_1_5!(action_476)),
        186 => partial_5!(happyShift, curry_1_5!(action_171)),
        193 => partial_5!(happyShift, curry_1_5!(action_172)),
        213 => partial_5!(happyShift, curry_1_5!(action_173)),
        214 => partial_5!(happyShift, curry_1_5!(action_174)),
        216 => partial_5!(happyShift, curry_1_5!(action_175)),
        231 => partial_5!(happyShift, curry_1_5!(action_176)),
        237 => partial_5!(happyShift, curry_1_5!(action_140)),
        239 => partial_5!(happyShift, curry_1_5!(action_142)),
        64 => partial_5!(happyGoto, curry_1_5!(action_435)),
        75 => partial_5!(happyGoto, curry_1_5!(action_508)),
        76 => partial_5!(happyGoto, curry_1_5!(action_102)),
        77 => partial_5!(happyGoto, curry_1_5!(action_103)),
        78 => partial_5!(happyGoto, curry_1_5!(action_481)),
        134 => partial_5!(happyGoto, curry_1_5!(action_167)),
        _ => box happyFail
    }
}

fn action_485(i: isize) -> ActionReturn {
    match i {
        139 => partial_5!(happyShift, curry_1_5!(action_507)),
        _ => box happyFail
    }
}

fn action_486(i: isize) -> ActionReturn {
    match i {
        139 => partial_5!(happyShift, curry_1_5!(action_506)),
        _ => box happyFail
    }
}

fn action_487(i: isize) -> ActionReturn {
    match i {
        139 => partial_5!(happyShift, curry_1_5!(action_505)),
        _ => box happyFail
    }
}

fn action_488(i: isize) -> ActionReturn {
    match i {
        181 => partial_5!(happyShift, curry_1_5!(action_504)),
        _ => happyReduce_214()
    }
}

fn action_489(i: isize) -> ActionReturn {
    match i {
        237 => partial_5!(happyShift, curry_1_5!(action_254)),
        238 => partial_5!(happyShift, curry_1_5!(action_75)),
        62 => partial_5!(happyGoto, curry_1_5!(action_501)),
        63 => partial_5!(happyGoto, curry_1_5!(action_502)),
        131 => partial_5!(happyGoto, curry_1_5!(action_503)),
        _ => box happyFail
    }
}

fn action_490(i: isize) -> ActionReturn {
    match i {
        179 => partial_5!(happyShift, curry_1_5!(action_500)),
        _ => box happyFail
    }
}

fn action_491(i: isize) -> ActionReturn {
    match i {
        139 => partial_5!(happyShift, curry_1_5!(action_499)),
        _ => box happyFail
    }
}

fn action_492(i: isize) -> ActionReturn {
    match i {
        139 => partial_5!(happyShift, curry_1_5!(action_498)),
        _ => box happyFail
    }
}

fn action_493(i: isize) -> ActionReturn {
    match i {
        193 => partial_5!(happyShift, curry_1_5!(action_496)),
        237 => partial_5!(happyShift, curry_1_5!(action_497)),
        135 => partial_5!(happyGoto, curry_1_5!(action_494)),
        136 => partial_5!(happyGoto, curry_1_5!(action_495)),
        _ => happyReduce_478()
    }
}

fn action_494(i: isize) -> ActionReturn {
    match i {
        139 => partial_5!(happyShift, curry_1_5!(action_765)),
        179 => partial_5!(happyShift, curry_1_5!(action_766)),
        _ => box happyFail
    }
}

fn action_495(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_476()
    }
}

fn action_496(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_480()
    }
}

fn action_497(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_764)),
        _ => happyReduce_479()
    }
}

fn action_498(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_171()
    }
}

fn action_499(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_172()
    }
}

fn action_500(i: isize) -> ActionReturn {
    match i {
        236 => partial_5!(happyShift, curry_1_5!(action_42)),
        128 => partial_5!(happyGoto, curry_1_5!(action_763)),
        _ => box happyFail
    }
}

fn action_501(i: isize) -> ActionReturn {
    match i {
        179 => partial_5!(happyShift, curry_1_5!(action_761)),
        182 => partial_5!(happyShift, curry_1_5!(action_762)),
        _ => box happyFail
    }
}

fn action_502(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_215()
    }
}

fn action_503(i: isize) -> ActionReturn {
    match i {
        168 => partial_5!(happyShift, curry_1_5!(action_760)),
        239 => partial_5!(happyShift, curry_1_5!(action_142)),
        133 => partial_5!(happyGoto, curry_1_5!(action_759)),
        134 => partial_5!(happyGoto, curry_1_5!(action_110)),
        _ => happyReduce_217()
    }
}

fn action_504(i: isize) -> ActionReturn {
    match i {
        237 => partial_5!(happyShift, curry_1_5!(action_254)),
        238 => partial_5!(happyShift, curry_1_5!(action_75)),
        62 => partial_5!(happyGoto, curry_1_5!(action_758)),
        63 => partial_5!(happyGoto, curry_1_5!(action_502)),
        131 => partial_5!(happyGoto, curry_1_5!(action_503)),
        _ => box happyFail
    }
}

fn action_505(i: isize) -> ActionReturn {
    match i {
        180 => partial_5!(happyShift, curry_1_5!(action_757)),
        _ => box happyFail
    }
}

fn action_506(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_126()
    }
}

fn action_507(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_125()
    }
}

fn action_508(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_266()
    }
}

fn action_509(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_269()
    }
}

fn action_510(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_281()
    }
}

fn action_511(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_475)),
        150 => partial_5!(happyShift, curry_1_5!(action_476)),
        186 => partial_5!(happyShift, curry_1_5!(action_171)),
        193 => partial_5!(happyShift, curry_1_5!(action_172)),
        213 => partial_5!(happyShift, curry_1_5!(action_173)),
        214 => partial_5!(happyShift, curry_1_5!(action_174)),
        216 => partial_5!(happyShift, curry_1_5!(action_175)),
        231 => partial_5!(happyShift, curry_1_5!(action_176)),
        237 => partial_5!(happyShift, curry_1_5!(action_140)),
        239 => partial_5!(happyShift, curry_1_5!(action_142)),
        64 => partial_5!(happyGoto, curry_1_5!(action_187)),
        75 => partial_5!(happyGoto, curry_1_5!(action_482)),
        76 => partial_5!(happyGoto, curry_1_5!(action_102)),
        77 => partial_5!(happyGoto, curry_1_5!(action_103)),
        78 => partial_5!(happyGoto, curry_1_5!(action_481)),
        133 => partial_5!(happyGoto, curry_1_5!(action_484)),
        134 => partial_5!(happyGoto, curry_1_5!(action_110)),
        _ => box happyFail
    }
}

fn action_512(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_413)),
        139 => partial_5!(happyShift, curry_1_5!(action_478)),
        140 => partial_5!(happyShift, curry_1_5!(action_182)),
        88 => partial_5!(happyGoto, curry_1_5!(action_178)),
        89 => partial_5!(happyGoto, curry_1_5!(action_179)),
        90 => partial_5!(happyGoto, curry_1_5!(action_180)),
        _ => box happyFail
    }
}

fn action_513(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_274()
    }
}

fn action_514(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_413)),
        140 => partial_5!(happyShift, curry_1_5!(action_182)),
        88 => partial_5!(happyGoto, curry_1_5!(action_756)),
        89 => partial_5!(happyGoto, curry_1_5!(action_179)),
        90 => partial_5!(happyGoto, curry_1_5!(action_180)),
        _ => happyReduce_270()
    }
}

fn action_515(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_95()
    }
}

fn action_516(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_26)),
        144 => partial_5!(happyShift, curry_1_5!(action_27)),
        145 => partial_5!(happyShift, curry_1_5!(action_28)),
        146 => partial_5!(happyShift, curry_1_5!(action_29)),
        147 => partial_5!(happyShift, curry_1_5!(action_30)),
        148 => partial_5!(happyShift, curry_1_5!(action_31)),
        149 => partial_5!(happyShift, curry_1_5!(action_32)),
        150 => partial_5!(happyShift, curry_1_5!(action_33)),
        153 => partial_5!(happyShift, curry_1_5!(action_34)),
        164 => partial_5!(happyShift, curry_1_5!(action_35)),
        181 => partial_5!(happyShift, curry_1_5!(action_654)),
        184 => partial_5!(happyShift, curry_1_5!(action_36)),
        204 => partial_5!(happyShift, curry_1_5!(action_37)),
        220 => partial_5!(happyShift, curry_1_5!(action_38)),
        233 => partial_5!(happyShift, curry_1_5!(action_39)),
        234 => partial_5!(happyShift, curry_1_5!(action_40)),
        235 => partial_5!(happyShift, curry_1_5!(action_41)),
        236 => partial_5!(happyShift, curry_1_5!(action_42)),
        237 => partial_5!(happyShift, curry_1_5!(action_43)),
        240 => partial_5!(happyShift, curry_1_5!(action_44)),
        241 => partial_5!(happyShift, curry_1_5!(action_45)),
        242 => partial_5!(happyShift, curry_1_5!(action_46)),
        243 => partial_5!(happyShift, curry_1_5!(action_47)),
        244 => partial_5!(happyShift, curry_1_5!(action_48)),
        245 => partial_5!(happyShift, curry_1_5!(action_49)),
        93 => partial_5!(happyGoto, curry_1_5!(action_755)),
        100 => partial_5!(happyGoto, curry_1_5!(action_6)),
        104 => partial_5!(happyGoto, curry_1_5!(action_7)),
        106 => partial_5!(happyGoto, curry_1_5!(action_8)),
        107 => partial_5!(happyGoto, curry_1_5!(action_9)),
        108 => partial_5!(happyGoto, curry_1_5!(action_10)),
        109 => partial_5!(happyGoto, curry_1_5!(action_11)),
        110 => partial_5!(happyGoto, curry_1_5!(action_12)),
        111 => partial_5!(happyGoto, curry_1_5!(action_13)),
        112 => partial_5!(happyGoto, curry_1_5!(action_14)),
        113 => partial_5!(happyGoto, curry_1_5!(action_15)),
        114 => partial_5!(happyGoto, curry_1_5!(action_16)),
        115 => partial_5!(happyGoto, curry_1_5!(action_17)),
        116 => partial_5!(happyGoto, curry_1_5!(action_18)),
        117 => partial_5!(happyGoto, curry_1_5!(action_19)),
        118 => partial_5!(happyGoto, curry_1_5!(action_20)),
        119 => partial_5!(happyGoto, curry_1_5!(action_21)),
        120 => partial_5!(happyGoto, curry_1_5!(action_650)),
        127 => partial_5!(happyGoto, curry_1_5!(action_24)),
        128 => partial_5!(happyGoto, curry_1_5!(action_25)),
        _ => box happyFail
    }
}

fn action_517(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_20()
    }
}

fn action_518(i: isize) -> ActionReturn {
    match i {
        139 => partial_5!(happyShift, curry_1_5!(action_754)),
        _ => box happyFail
    }
}

fn action_519(i: isize) -> ActionReturn {
    match i {
        139 => partial_5!(happyShift, curry_1_5!(action_753)),
        _ => box happyFail
    }
}

fn action_520(i: isize) -> ActionReturn {
    match i {
        187 => partial_5!(happyShift, curry_1_5!(action_406)),
        35 => partial_5!(happyGoto, curry_1_5!(action_471)),
        67 => partial_5!(happyGoto, curry_1_5!(action_405)),
        _ => happyReduce_232()
    }
}

fn action_521(i: isize) -> ActionReturn {
    match i {
        187 => partial_5!(happyShift, curry_1_5!(action_406)),
        35 => partial_5!(happyGoto, curry_1_5!(action_441)),
        67 => partial_5!(happyGoto, curry_1_5!(action_405)),
        _ => happyReduce_232()
    }
}

fn action_522(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_475)),
        150 => partial_5!(happyShift, curry_1_5!(action_476)),
        185 => partial_5!(happyShift, curry_1_5!(action_113)),
        186 => partial_5!(happyShift, curry_1_5!(action_171)),
        188 => partial_5!(happyShift, curry_1_5!(action_115)),
        190 => partial_5!(happyShift, curry_1_5!(action_116)),
        192 => partial_5!(happyShift, curry_1_5!(action_117)),
        193 => partial_5!(happyShift, curry_1_5!(action_172)),
        195 => partial_5!(happyShift, curry_1_5!(action_118)),
        198 => partial_5!(happyShift, curry_1_5!(action_119)),
        200 => partial_5!(happyShift, curry_1_5!(action_120)),
        201 => partial_5!(happyShift, curry_1_5!(action_121)),
        202 => partial_5!(happyShift, curry_1_5!(action_122)),
        207 => partial_5!(happyShift, curry_1_5!(action_123)),
        208 => partial_5!(happyShift, curry_1_5!(action_124)),
        209 => partial_5!(happyShift, curry_1_5!(action_125)),
        210 => partial_5!(happyShift, curry_1_5!(action_126)),
        212 => partial_5!(happyShift, curry_1_5!(action_127)),
        213 => partial_5!(happyShift, curry_1_5!(action_173)),
        214 => partial_5!(happyShift, curry_1_5!(action_174)),
        215 => partial_5!(happyShift, curry_1_5!(action_128)),
        216 => partial_5!(happyShift, curry_1_5!(action_175)),
        218 => partial_5!(happyShift, curry_1_5!(action_129)),
        219 => partial_5!(happyShift, curry_1_5!(action_130)),
        221 => partial_5!(happyShift, curry_1_5!(action_131)),
        223 => partial_5!(happyShift, curry_1_5!(action_133)),
        225 => partial_5!(happyShift, curry_1_5!(action_134)),
        226 => partial_5!(happyShift, curry_1_5!(action_438)),
        227 => partial_5!(happyShift, curry_1_5!(action_136)),
        228 => partial_5!(happyShift, curry_1_5!(action_137)),
        229 => partial_5!(happyShift, curry_1_5!(action_138)),
        230 => partial_5!(happyShift, curry_1_5!(action_139)),
        231 => partial_5!(happyShift, curry_1_5!(action_176)),
        237 => partial_5!(happyShift, curry_1_5!(action_140)),
        238 => partial_5!(happyShift, curry_1_5!(action_439)),
        239 => partial_5!(happyShift, curry_1_5!(action_142)),
        40 => partial_5!(happyGoto, curry_1_5!(action_432)),
        41 => partial_5!(happyGoto, curry_1_5!(action_85)),
        42 => partial_5!(happyGoto, curry_1_5!(action_86)),
        43 => partial_5!(happyGoto, curry_1_5!(action_87)),
        45 => partial_5!(happyGoto, curry_1_5!(action_433)),
        52 => partial_5!(happyGoto, curry_1_5!(action_434)),
        53 => partial_5!(happyGoto, curry_1_5!(action_97)),
        54 => partial_5!(happyGoto, curry_1_5!(action_98)),
        61 => partial_5!(happyGoto, curry_1_5!(action_99)),
        64 => partial_5!(happyGoto, curry_1_5!(action_435)),
        75 => partial_5!(happyGoto, curry_1_5!(action_752)),
        76 => partial_5!(happyGoto, curry_1_5!(action_102)),
        77 => partial_5!(happyGoto, curry_1_5!(action_103)),
        78 => partial_5!(happyGoto, curry_1_5!(action_481)),
        134 => partial_5!(happyGoto, curry_1_5!(action_167)),
        _ => box happyFail
    }
}

fn action_523(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_230()
    }
}

fn action_524(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_751)),
        150 => partial_5!(happyShift, curry_1_5!(action_525)),
        237 => partial_5!(happyShift, curry_1_5!(action_140)),
        238 => partial_5!(happyShift, curry_1_5!(action_424)),
        239 => partial_5!(happyShift, curry_1_5!(action_142)),
        70 => partial_5!(happyGoto, curry_1_5!(action_419)),
        71 => partial_5!(happyGoto, curry_1_5!(action_222)),
        72 => partial_5!(happyGoto, curry_1_5!(action_420)),
        73 => partial_5!(happyGoto, curry_1_5!(action_224)),
        74 => partial_5!(happyGoto, curry_1_5!(action_421)),
        76 => partial_5!(happyGoto, curry_1_5!(action_157)),
        77 => partial_5!(happyGoto, curry_1_5!(action_103)),
        78 => partial_5!(happyGoto, curry_1_5!(action_512)),
        133 => partial_5!(happyGoto, curry_1_5!(action_422)),
        134 => partial_5!(happyGoto, curry_1_5!(action_110)),
        _ => box happyFail
    }
}

fn action_525(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_750)),
        150 => partial_5!(happyShift, curry_1_5!(action_525)),
        237 => partial_5!(happyShift, curry_1_5!(action_140)),
        238 => partial_5!(happyShift, curry_1_5!(action_229)),
        239 => partial_5!(happyShift, curry_1_5!(action_142)),
        65 => partial_5!(happyGoto, curry_1_5!(action_749)),
        69 => partial_5!(happyGoto, curry_1_5!(action_415)),
        70 => partial_5!(happyGoto, curry_1_5!(action_221)),
        71 => partial_5!(happyGoto, curry_1_5!(action_222)),
        72 => partial_5!(happyGoto, curry_1_5!(action_416)),
        73 => partial_5!(happyGoto, curry_1_5!(action_224)),
        75 => partial_5!(happyGoto, curry_1_5!(action_154)),
        76 => partial_5!(happyGoto, curry_1_5!(action_102)),
        77 => partial_5!(happyGoto, curry_1_5!(action_103)),
        78 => partial_5!(happyGoto, curry_1_5!(action_481)),
        132 => partial_5!(happyGoto, curry_1_5!(action_108)),
        133 => partial_5!(happyGoto, curry_1_5!(action_417)),
        134 => partial_5!(happyGoto, curry_1_5!(action_110)),
        _ => happyReduce_471()
    }
}

fn action_526(i: isize) -> ActionReturn {
    match i {
        187 => partial_5!(happyShift, curry_1_5!(action_406)),
        35 => partial_5!(happyGoto, curry_1_5!(action_410)),
        67 => partial_5!(happyGoto, curry_1_5!(action_405)),
        _ => happyReduce_232()
    }
}

fn action_527(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_279()
    }
}

fn action_528(i: isize) -> ActionReturn {
    match i {
        237 => partial_5!(happyShift, curry_1_5!(action_748)),
        _ => box happyFail
    }
}

fn action_529(i: isize) -> ActionReturn {
    match i {
        183 => partial_5!(happyShift, curry_1_5!(action_747)),
        185 => partial_5!(happyShift, curry_1_5!(action_113)),
        188 => partial_5!(happyShift, curry_1_5!(action_115)),
        190 => partial_5!(happyShift, curry_1_5!(action_116)),
        192 => partial_5!(happyShift, curry_1_5!(action_117)),
        195 => partial_5!(happyShift, curry_1_5!(action_118)),
        198 => partial_5!(happyShift, curry_1_5!(action_119)),
        200 => partial_5!(happyShift, curry_1_5!(action_120)),
        201 => partial_5!(happyShift, curry_1_5!(action_121)),
        202 => partial_5!(happyShift, curry_1_5!(action_122)),
        207 => partial_5!(happyShift, curry_1_5!(action_123)),
        208 => partial_5!(happyShift, curry_1_5!(action_124)),
        209 => partial_5!(happyShift, curry_1_5!(action_125)),
        210 => partial_5!(happyShift, curry_1_5!(action_126)),
        212 => partial_5!(happyShift, curry_1_5!(action_127)),
        215 => partial_5!(happyShift, curry_1_5!(action_128)),
        218 => partial_5!(happyShift, curry_1_5!(action_129)),
        219 => partial_5!(happyShift, curry_1_5!(action_130)),
        221 => partial_5!(happyShift, curry_1_5!(action_131)),
        223 => partial_5!(happyShift, curry_1_5!(action_133)),
        225 => partial_5!(happyShift, curry_1_5!(action_134)),
        226 => partial_5!(happyShift, curry_1_5!(action_135)),
        227 => partial_5!(happyShift, curry_1_5!(action_136)),
        228 => partial_5!(happyShift, curry_1_5!(action_137)),
        229 => partial_5!(happyShift, curry_1_5!(action_138)),
        230 => partial_5!(happyShift, curry_1_5!(action_139)),
        238 => partial_5!(happyShift, curry_1_5!(action_141)),
        239 => partial_5!(happyShift, curry_1_5!(action_142)),
        37 => partial_5!(happyGoto, curry_1_5!(action_449)),
        38 => partial_5!(happyGoto, curry_1_5!(action_450)),
        40 => partial_5!(happyGoto, curry_1_5!(action_84)),
        41 => partial_5!(happyGoto, curry_1_5!(action_85)),
        42 => partial_5!(happyGoto, curry_1_5!(action_86)),
        43 => partial_5!(happyGoto, curry_1_5!(action_87)),
        44 => partial_5!(happyGoto, curry_1_5!(action_451)),
        45 => partial_5!(happyGoto, curry_1_5!(action_89)),
        46 => partial_5!(happyGoto, curry_1_5!(action_90)),
        47 => partial_5!(happyGoto, curry_1_5!(action_91)),
        48 => partial_5!(happyGoto, curry_1_5!(action_452)),
        49 => partial_5!(happyGoto, curry_1_5!(action_453)),
        50 => partial_5!(happyGoto, curry_1_5!(action_94)),
        51 => partial_5!(happyGoto, curry_1_5!(action_95)),
        52 => partial_5!(happyGoto, curry_1_5!(action_96)),
        53 => partial_5!(happyGoto, curry_1_5!(action_97)),
        54 => partial_5!(happyGoto, curry_1_5!(action_98)),
        61 => partial_5!(happyGoto, curry_1_5!(action_99)),
        65 => partial_5!(happyGoto, curry_1_5!(action_454)),
        84 => partial_5!(happyGoto, curry_1_5!(action_746)),
        132 => partial_5!(happyGoto, curry_1_5!(action_108)),
        133 => partial_5!(happyGoto, curry_1_5!(action_459)),
        134 => partial_5!(happyGoto, curry_1_5!(action_110)),
        _ => happyReduce_471()
    }
}

fn action_530(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_312()
    }
}

fn action_531(i: isize) -> ActionReturn {
    match i {
        239 => partial_5!(happyShift, curry_1_5!(action_142)),
        132 => partial_5!(happyGoto, curry_1_5!(action_745)),
        133 => partial_5!(happyGoto, curry_1_5!(action_150)),
        134 => partial_5!(happyGoto, curry_1_5!(action_110)),
        _ => happyReduce_471()
    }
}

fn action_532(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_300()
    }
}

fn action_533(i: isize) -> ActionReturn {
    match i {
        185 => partial_5!(happyShift, curry_1_5!(action_113)),
        186 => partial_5!(happyShift, curry_1_5!(action_171)),
        188 => partial_5!(happyShift, curry_1_5!(action_115)),
        190 => partial_5!(happyShift, curry_1_5!(action_116)),
        192 => partial_5!(happyShift, curry_1_5!(action_117)),
        193 => partial_5!(happyShift, curry_1_5!(action_172)),
        195 => partial_5!(happyShift, curry_1_5!(action_118)),
        198 => partial_5!(happyShift, curry_1_5!(action_119)),
        200 => partial_5!(happyShift, curry_1_5!(action_120)),
        201 => partial_5!(happyShift, curry_1_5!(action_121)),
        202 => partial_5!(happyShift, curry_1_5!(action_122)),
        207 => partial_5!(happyShift, curry_1_5!(action_123)),
        208 => partial_5!(happyShift, curry_1_5!(action_124)),
        209 => partial_5!(happyShift, curry_1_5!(action_125)),
        210 => partial_5!(happyShift, curry_1_5!(action_126)),
        212 => partial_5!(happyShift, curry_1_5!(action_127)),
        213 => partial_5!(happyShift, curry_1_5!(action_173)),
        214 => partial_5!(happyShift, curry_1_5!(action_174)),
        215 => partial_5!(happyShift, curry_1_5!(action_128)),
        216 => partial_5!(happyShift, curry_1_5!(action_175)),
        218 => partial_5!(happyShift, curry_1_5!(action_129)),
        219 => partial_5!(happyShift, curry_1_5!(action_130)),
        221 => partial_5!(happyShift, curry_1_5!(action_131)),
        223 => partial_5!(happyShift, curry_1_5!(action_133)),
        225 => partial_5!(happyShift, curry_1_5!(action_134)),
        226 => partial_5!(happyShift, curry_1_5!(action_438)),
        227 => partial_5!(happyShift, curry_1_5!(action_136)),
        228 => partial_5!(happyShift, curry_1_5!(action_137)),
        229 => partial_5!(happyShift, curry_1_5!(action_138)),
        230 => partial_5!(happyShift, curry_1_5!(action_139)),
        231 => partial_5!(happyShift, curry_1_5!(action_176)),
        238 => partial_5!(happyShift, curry_1_5!(action_439)),
        239 => partial_5!(happyShift, curry_1_5!(action_142)),
        40 => partial_5!(happyGoto, curry_1_5!(action_432)),
        41 => partial_5!(happyGoto, curry_1_5!(action_85)),
        42 => partial_5!(happyGoto, curry_1_5!(action_86)),
        43 => partial_5!(happyGoto, curry_1_5!(action_87)),
        45 => partial_5!(happyGoto, curry_1_5!(action_433)),
        52 => partial_5!(happyGoto, curry_1_5!(action_434)),
        53 => partial_5!(happyGoto, curry_1_5!(action_97)),
        54 => partial_5!(happyGoto, curry_1_5!(action_98)),
        61 => partial_5!(happyGoto, curry_1_5!(action_99)),
        64 => partial_5!(happyGoto, curry_1_5!(action_435)),
        134 => partial_5!(happyGoto, curry_1_5!(action_167)),
        _ => box happyFail
    }
}

fn action_534(i: isize) -> ActionReturn {
    match i {
        185 => happyReduce_473(),
        186 => happyReduce_473(),
        188 => happyReduce_473(),
        190 => happyReduce_473(),
        192 => happyReduce_473(),
        193 => happyReduce_473(),
        195 => happyReduce_473(),
        198 => happyReduce_473(),
        200 => happyReduce_473(),
        201 => happyReduce_473(),
        202 => happyReduce_473(),
        207 => happyReduce_473(),
        208 => happyReduce_473(),
        209 => happyReduce_473(),
        210 => happyReduce_473(),
        212 => happyReduce_473(),
        213 => happyReduce_473(),
        214 => happyReduce_473(),
        215 => happyReduce_473(),
        216 => happyReduce_473(),
        218 => happyReduce_473(),
        219 => happyReduce_473(),
        221 => happyReduce_473(),
        223 => happyReduce_473(),
        225 => happyReduce_473(),
        226 => happyReduce_473(),
        227 => happyReduce_473(),
        228 => happyReduce_473(),
        229 => happyReduce_473(),
        230 => happyReduce_473(),
        231 => happyReduce_473(),
        238 => happyReduce_473(),
        239 => happyReduce_473(),
        _ => happyReduce_299()
    }
}

fn action_535(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_535)),
        140 => partial_5!(happyShift, curry_1_5!(action_182)),
        150 => partial_5!(happyShift, curry_1_5!(action_536)),
        185 => partial_5!(happyShift, curry_1_5!(action_113)),
        186 => happyReduce_471(),
        188 => partial_5!(happyShift, curry_1_5!(action_115)),
        190 => partial_5!(happyShift, curry_1_5!(action_116)),
        192 => partial_5!(happyShift, curry_1_5!(action_117)),
        193 => happyReduce_471(),
        195 => partial_5!(happyShift, curry_1_5!(action_118)),
        198 => partial_5!(happyShift, curry_1_5!(action_119)),
        200 => partial_5!(happyShift, curry_1_5!(action_120)),
        201 => partial_5!(happyShift, curry_1_5!(action_121)),
        202 => partial_5!(happyShift, curry_1_5!(action_122)),
        207 => partial_5!(happyShift, curry_1_5!(action_123)),
        208 => partial_5!(happyShift, curry_1_5!(action_124)),
        209 => partial_5!(happyShift, curry_1_5!(action_125)),
        210 => partial_5!(happyShift, curry_1_5!(action_126)),
        212 => partial_5!(happyShift, curry_1_5!(action_127)),
        213 => happyReduce_471(),
        214 => happyReduce_471(),
        215 => partial_5!(happyShift, curry_1_5!(action_128)),
        216 => happyReduce_471(),
        218 => partial_5!(happyShift, curry_1_5!(action_129)),
        219 => partial_5!(happyShift, curry_1_5!(action_130)),
        221 => partial_5!(happyShift, curry_1_5!(action_131)),
        223 => partial_5!(happyShift, curry_1_5!(action_133)),
        225 => partial_5!(happyShift, curry_1_5!(action_134)),
        226 => partial_5!(happyShift, curry_1_5!(action_135)),
        227 => partial_5!(happyShift, curry_1_5!(action_136)),
        228 => partial_5!(happyShift, curry_1_5!(action_137)),
        229 => partial_5!(happyShift, curry_1_5!(action_138)),
        230 => partial_5!(happyShift, curry_1_5!(action_139)),
        231 => happyReduce_471(),
        237 => partial_5!(happyShift, curry_1_5!(action_140)),
        238 => partial_5!(happyShift, curry_1_5!(action_141)),
        239 => partial_5!(happyShift, curry_1_5!(action_142)),
        37 => partial_5!(happyGoto, curry_1_5!(action_449)),
        38 => partial_5!(happyGoto, curry_1_5!(action_450)),
        40 => partial_5!(happyGoto, curry_1_5!(action_84)),
        41 => partial_5!(happyGoto, curry_1_5!(action_85)),
        42 => partial_5!(happyGoto, curry_1_5!(action_86)),
        43 => partial_5!(happyGoto, curry_1_5!(action_87)),
        44 => partial_5!(happyGoto, curry_1_5!(action_451)),
        45 => partial_5!(happyGoto, curry_1_5!(action_89)),
        46 => partial_5!(happyGoto, curry_1_5!(action_90)),
        47 => partial_5!(happyGoto, curry_1_5!(action_91)),
        48 => partial_5!(happyGoto, curry_1_5!(action_452)),
        49 => partial_5!(happyGoto, curry_1_5!(action_453)),
        50 => partial_5!(happyGoto, curry_1_5!(action_94)),
        51 => partial_5!(happyGoto, curry_1_5!(action_95)),
        52 => partial_5!(happyGoto, curry_1_5!(action_96)),
        53 => partial_5!(happyGoto, curry_1_5!(action_97)),
        54 => partial_5!(happyGoto, curry_1_5!(action_98)),
        61 => partial_5!(happyGoto, curry_1_5!(action_99)),
        65 => partial_5!(happyGoto, curry_1_5!(action_454)),
        76 => partial_5!(happyGoto, curry_1_5!(action_157)),
        77 => partial_5!(happyGoto, curry_1_5!(action_103)),
        78 => partial_5!(happyGoto, curry_1_5!(action_512)),
        82 => partial_5!(happyGoto, curry_1_5!(action_455)),
        83 => partial_5!(happyGoto, curry_1_5!(action_456)),
        84 => partial_5!(happyGoto, curry_1_5!(action_457)),
        88 => partial_5!(happyGoto, curry_1_5!(action_628)),
        89 => partial_5!(happyGoto, curry_1_5!(action_179)),
        90 => partial_5!(happyGoto, curry_1_5!(action_180)),
        91 => partial_5!(happyGoto, curry_1_5!(action_629)),
        92 => partial_5!(happyGoto, curry_1_5!(action_630)),
        132 => partial_5!(happyGoto, curry_1_5!(action_108)),
        133 => partial_5!(happyGoto, curry_1_5!(action_744)),
        134 => partial_5!(happyGoto, curry_1_5!(action_110)),
        _ => happyReduce_282()
    }
}

fn action_536(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_535)),
        140 => partial_5!(happyShift, curry_1_5!(action_182)),
        150 => partial_5!(happyShift, curry_1_5!(action_536)),
        186 => happyReduce_471(),
        193 => happyReduce_471(),
        213 => happyReduce_471(),
        214 => happyReduce_471(),
        216 => happyReduce_471(),
        231 => happyReduce_471(),
        237 => partial_5!(happyShift, curry_1_5!(action_140)),
        239 => partial_5!(happyShift, curry_1_5!(action_142)),
        65 => partial_5!(happyGoto, curry_1_5!(action_742)),
        75 => partial_5!(happyGoto, curry_1_5!(action_154)),
        76 => partial_5!(happyGoto, curry_1_5!(action_102)),
        77 => partial_5!(happyGoto, curry_1_5!(action_103)),
        78 => partial_5!(happyGoto, curry_1_5!(action_481)),
        87 => partial_5!(happyGoto, curry_1_5!(action_626)),
        88 => partial_5!(happyGoto, curry_1_5!(action_367)),
        89 => partial_5!(happyGoto, curry_1_5!(action_179)),
        90 => partial_5!(happyGoto, curry_1_5!(action_180)),
        91 => partial_5!(happyGoto, curry_1_5!(action_368)),
        92 => partial_5!(happyGoto, curry_1_5!(action_369)),
        132 => partial_5!(happyGoto, curry_1_5!(action_108)),
        133 => partial_5!(happyGoto, curry_1_5!(action_743)),
        134 => partial_5!(happyGoto, curry_1_5!(action_110)),
        _ => happyReduce_326()
    }
}

fn action_537(i: isize) -> ActionReturn {
    match i {
        239 => partial_5!(happyShift, curry_1_5!(action_142)),
        132 => partial_5!(happyGoto, curry_1_5!(action_741)),
        133 => partial_5!(happyGoto, curry_1_5!(action_150)),
        134 => partial_5!(happyGoto, curry_1_5!(action_110)),
        _ => happyReduce_471()
    }
}

fn action_538(i: isize) -> ActionReturn {
    match i {
        239 => partial_5!(happyShift, curry_1_5!(action_142)),
        132 => partial_5!(happyGoto, curry_1_5!(action_740)),
        133 => partial_5!(happyGoto, curry_1_5!(action_150)),
        134 => partial_5!(happyGoto, curry_1_5!(action_110)),
        _ => happyReduce_471()
    }
}

fn action_539(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_295()
    }
}

fn action_540(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_540)),
        140 => partial_5!(happyShift, curry_1_5!(action_182)),
        150 => partial_5!(happyShift, curry_1_5!(action_541)),
        185 => partial_5!(happyShift, curry_1_5!(action_113)),
        186 => happyReduce_471(),
        188 => partial_5!(happyShift, curry_1_5!(action_115)),
        190 => partial_5!(happyShift, curry_1_5!(action_116)),
        192 => partial_5!(happyShift, curry_1_5!(action_117)),
        193 => happyReduce_471(),
        195 => partial_5!(happyShift, curry_1_5!(action_118)),
        198 => partial_5!(happyShift, curry_1_5!(action_119)),
        200 => partial_5!(happyShift, curry_1_5!(action_120)),
        201 => partial_5!(happyShift, curry_1_5!(action_121)),
        202 => partial_5!(happyShift, curry_1_5!(action_122)),
        207 => partial_5!(happyShift, curry_1_5!(action_123)),
        208 => partial_5!(happyShift, curry_1_5!(action_124)),
        209 => partial_5!(happyShift, curry_1_5!(action_125)),
        210 => partial_5!(happyShift, curry_1_5!(action_126)),
        212 => partial_5!(happyShift, curry_1_5!(action_127)),
        213 => happyReduce_471(),
        214 => happyReduce_471(),
        215 => partial_5!(happyShift, curry_1_5!(action_128)),
        216 => happyReduce_471(),
        218 => partial_5!(happyShift, curry_1_5!(action_129)),
        219 => partial_5!(happyShift, curry_1_5!(action_130)),
        221 => partial_5!(happyShift, curry_1_5!(action_131)),
        223 => partial_5!(happyShift, curry_1_5!(action_133)),
        225 => partial_5!(happyShift, curry_1_5!(action_134)),
        226 => partial_5!(happyShift, curry_1_5!(action_135)),
        227 => partial_5!(happyShift, curry_1_5!(action_136)),
        228 => partial_5!(happyShift, curry_1_5!(action_137)),
        229 => partial_5!(happyShift, curry_1_5!(action_138)),
        230 => partial_5!(happyShift, curry_1_5!(action_139)),
        231 => happyReduce_471(),
        237 => partial_5!(happyShift, curry_1_5!(action_140)),
        238 => partial_5!(happyShift, curry_1_5!(action_141)),
        239 => partial_5!(happyShift, curry_1_5!(action_142)),
        37 => partial_5!(happyGoto, curry_1_5!(action_449)),
        38 => partial_5!(happyGoto, curry_1_5!(action_450)),
        40 => partial_5!(happyGoto, curry_1_5!(action_84)),
        41 => partial_5!(happyGoto, curry_1_5!(action_85)),
        42 => partial_5!(happyGoto, curry_1_5!(action_86)),
        43 => partial_5!(happyGoto, curry_1_5!(action_87)),
        44 => partial_5!(happyGoto, curry_1_5!(action_451)),
        45 => partial_5!(happyGoto, curry_1_5!(action_89)),
        46 => partial_5!(happyGoto, curry_1_5!(action_90)),
        47 => partial_5!(happyGoto, curry_1_5!(action_91)),
        48 => partial_5!(happyGoto, curry_1_5!(action_452)),
        49 => partial_5!(happyGoto, curry_1_5!(action_453)),
        50 => partial_5!(happyGoto, curry_1_5!(action_94)),
        51 => partial_5!(happyGoto, curry_1_5!(action_95)),
        52 => partial_5!(happyGoto, curry_1_5!(action_96)),
        53 => partial_5!(happyGoto, curry_1_5!(action_97)),
        54 => partial_5!(happyGoto, curry_1_5!(action_98)),
        61 => partial_5!(happyGoto, curry_1_5!(action_99)),
        65 => partial_5!(happyGoto, curry_1_5!(action_454)),
        70 => partial_5!(happyGoto, curry_1_5!(action_419)),
        71 => partial_5!(happyGoto, curry_1_5!(action_222)),
        76 => partial_5!(happyGoto, curry_1_5!(action_157)),
        77 => partial_5!(happyGoto, curry_1_5!(action_103)),
        78 => partial_5!(happyGoto, curry_1_5!(action_512)),
        82 => partial_5!(happyGoto, curry_1_5!(action_455)),
        83 => partial_5!(happyGoto, curry_1_5!(action_456)),
        84 => partial_5!(happyGoto, curry_1_5!(action_457)),
        88 => partial_5!(happyGoto, curry_1_5!(action_628)),
        89 => partial_5!(happyGoto, curry_1_5!(action_179)),
        90 => partial_5!(happyGoto, curry_1_5!(action_180)),
        91 => partial_5!(happyGoto, curry_1_5!(action_629)),
        92 => partial_5!(happyGoto, curry_1_5!(action_630)),
        132 => partial_5!(happyGoto, curry_1_5!(action_108)),
        133 => partial_5!(happyGoto, curry_1_5!(action_739)),
        134 => partial_5!(happyGoto, curry_1_5!(action_110)),
        _ => happyReduce_282()
    }
}

fn action_541(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_540)),
        140 => partial_5!(happyShift, curry_1_5!(action_182)),
        150 => partial_5!(happyShift, curry_1_5!(action_541)),
        186 => happyReduce_471(),
        193 => happyReduce_471(),
        213 => happyReduce_471(),
        214 => happyReduce_471(),
        216 => happyReduce_471(),
        231 => happyReduce_471(),
        237 => partial_5!(happyShift, curry_1_5!(action_140)),
        238 => partial_5!(happyShift, curry_1_5!(action_229)),
        239 => partial_5!(happyShift, curry_1_5!(action_142)),
        65 => partial_5!(happyGoto, curry_1_5!(action_737)),
        69 => partial_5!(happyGoto, curry_1_5!(action_415)),
        70 => partial_5!(happyGoto, curry_1_5!(action_221)),
        71 => partial_5!(happyGoto, curry_1_5!(action_222)),
        75 => partial_5!(happyGoto, curry_1_5!(action_154)),
        76 => partial_5!(happyGoto, curry_1_5!(action_102)),
        77 => partial_5!(happyGoto, curry_1_5!(action_103)),
        78 => partial_5!(happyGoto, curry_1_5!(action_481)),
        87 => partial_5!(happyGoto, curry_1_5!(action_626)),
        88 => partial_5!(happyGoto, curry_1_5!(action_367)),
        89 => partial_5!(happyGoto, curry_1_5!(action_179)),
        90 => partial_5!(happyGoto, curry_1_5!(action_180)),
        91 => partial_5!(happyGoto, curry_1_5!(action_368)),
        92 => partial_5!(happyGoto, curry_1_5!(action_369)),
        132 => partial_5!(happyGoto, curry_1_5!(action_108)),
        133 => partial_5!(happyGoto, curry_1_5!(action_738)),
        134 => partial_5!(happyGoto, curry_1_5!(action_110)),
        _ => happyReduce_326()
    }
}

fn action_542(i: isize) -> ActionReturn {
    match i {
        239 => partial_5!(happyShift, curry_1_5!(action_142)),
        132 => partial_5!(happyGoto, curry_1_5!(action_736)),
        133 => partial_5!(happyGoto, curry_1_5!(action_150)),
        134 => partial_5!(happyGoto, curry_1_5!(action_110)),
        _ => happyReduce_471()
    }
}

fn action_543(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_292()
    }
}

fn action_544(i: isize) -> ActionReturn {
    match i {
        239 => partial_5!(happyShift, curry_1_5!(action_142)),
        132 => partial_5!(happyGoto, curry_1_5!(action_735)),
        133 => partial_5!(happyGoto, curry_1_5!(action_150)),
        134 => partial_5!(happyGoto, curry_1_5!(action_110)),
        _ => happyReduce_471()
    }
}

fn action_545(i: isize) -> ActionReturn {
    match i {
        239 => partial_5!(happyShift, curry_1_5!(action_142)),
        132 => partial_5!(happyGoto, curry_1_5!(action_734)),
        133 => partial_5!(happyGoto, curry_1_5!(action_150)),
        134 => partial_5!(happyGoto, curry_1_5!(action_110)),
        _ => happyReduce_471()
    }
}

fn action_546(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_288()
    }
}

fn action_547(i: isize) -> ActionReturn {
    match i {
        186 => partial_5!(happyShift, curry_1_5!(action_171)),
        193 => partial_5!(happyShift, curry_1_5!(action_172)),
        213 => partial_5!(happyShift, curry_1_5!(action_173)),
        214 => partial_5!(happyShift, curry_1_5!(action_174)),
        216 => partial_5!(happyShift, curry_1_5!(action_175)),
        231 => partial_5!(happyShift, curry_1_5!(action_176)),
        239 => partial_5!(happyShift, curry_1_5!(action_142)),
        64 => partial_5!(happyGoto, curry_1_5!(action_187)),
        132 => partial_5!(happyGoto, curry_1_5!(action_733)),
        133 => partial_5!(happyGoto, curry_1_5!(action_664)),
        134 => partial_5!(happyGoto, curry_1_5!(action_110)),
        _ => happyReduce_471()
    }
}

fn action_548(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_26)),
        144 => partial_5!(happyShift, curry_1_5!(action_27)),
        145 => partial_5!(happyShift, curry_1_5!(action_28)),
        146 => partial_5!(happyShift, curry_1_5!(action_29)),
        147 => partial_5!(happyShift, curry_1_5!(action_30)),
        148 => partial_5!(happyShift, curry_1_5!(action_31)),
        149 => partial_5!(happyShift, curry_1_5!(action_32)),
        150 => partial_5!(happyShift, curry_1_5!(action_33)),
        153 => partial_5!(happyShift, curry_1_5!(action_34)),
        164 => partial_5!(happyShift, curry_1_5!(action_35)),
        184 => partial_5!(happyShift, curry_1_5!(action_36)),
        186 => partial_5!(happyShift, curry_1_5!(action_171)),
        193 => partial_5!(happyShift, curry_1_5!(action_172)),
        204 => partial_5!(happyShift, curry_1_5!(action_37)),
        213 => partial_5!(happyShift, curry_1_5!(action_173)),
        214 => partial_5!(happyShift, curry_1_5!(action_174)),
        216 => partial_5!(happyShift, curry_1_5!(action_175)),
        220 => partial_5!(happyShift, curry_1_5!(action_38)),
        231 => partial_5!(happyShift, curry_1_5!(action_176)),
        233 => partial_5!(happyShift, curry_1_5!(action_39)),
        234 => partial_5!(happyShift, curry_1_5!(action_40)),
        235 => partial_5!(happyShift, curry_1_5!(action_41)),
        236 => partial_5!(happyShift, curry_1_5!(action_42)),
        237 => partial_5!(happyShift, curry_1_5!(action_43)),
        240 => partial_5!(happyShift, curry_1_5!(action_44)),
        241 => partial_5!(happyShift, curry_1_5!(action_45)),
        242 => partial_5!(happyShift, curry_1_5!(action_46)),
        243 => partial_5!(happyShift, curry_1_5!(action_47)),
        244 => partial_5!(happyShift, curry_1_5!(action_48)),
        245 => partial_5!(happyShift, curry_1_5!(action_49)),
        64 => partial_5!(happyGoto, curry_1_5!(action_170)),
        100 => partial_5!(happyGoto, curry_1_5!(action_6)),
        104 => partial_5!(happyGoto, curry_1_5!(action_7)),
        106 => partial_5!(happyGoto, curry_1_5!(action_8)),
        107 => partial_5!(happyGoto, curry_1_5!(action_9)),
        108 => partial_5!(happyGoto, curry_1_5!(action_10)),
        109 => partial_5!(happyGoto, curry_1_5!(action_11)),
        110 => partial_5!(happyGoto, curry_1_5!(action_12)),
        111 => partial_5!(happyGoto, curry_1_5!(action_13)),
        112 => partial_5!(happyGoto, curry_1_5!(action_14)),
        113 => partial_5!(happyGoto, curry_1_5!(action_15)),
        114 => partial_5!(happyGoto, curry_1_5!(action_16)),
        115 => partial_5!(happyGoto, curry_1_5!(action_17)),
        116 => partial_5!(happyGoto, curry_1_5!(action_18)),
        117 => partial_5!(happyGoto, curry_1_5!(action_19)),
        118 => partial_5!(happyGoto, curry_1_5!(action_20)),
        119 => partial_5!(happyGoto, curry_1_5!(action_21)),
        120 => partial_5!(happyGoto, curry_1_5!(action_732)),
        127 => partial_5!(happyGoto, curry_1_5!(action_24)),
        128 => partial_5!(happyGoto, curry_1_5!(action_25)),
        _ => box happyFail
    }
}

fn action_549(i: isize) -> ActionReturn {
    match i {
        141 => partial_5!(happyShift, curry_1_5!(action_731)),
        _ => box happyFail
    }
}

fn action_550(i: isize) -> ActionReturn {
    match i {
        141 => partial_5!(happyShift, curry_1_5!(action_730)),
        _ => box happyFail
    }
}

fn action_551(i: isize) -> ActionReturn {
    match i {
        141 => happyReduce_471(),
        239 => partial_5!(happyShift, curry_1_5!(action_142)),
        132 => partial_5!(happyGoto, curry_1_5!(action_729)),
        133 => partial_5!(happyGoto, curry_1_5!(action_150)),
        134 => partial_5!(happyGoto, curry_1_5!(action_110)),
        _ => happyReduce_401()
    }
}

fn action_552(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_315()
    }
}

fn action_553(i: isize) -> ActionReturn {
    match i {
        141 => partial_5!(happyShift, curry_1_5!(action_728)),
        _ => box happyFail
    }
}

fn action_554(i: isize) -> ActionReturn {
    match i {
        221 => partial_5!(happyShift, curry_1_5!(action_727)),
        _ => box happyFail
    }
}

fn action_555(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_26)),
        144 => partial_5!(happyShift, curry_1_5!(action_27)),
        145 => partial_5!(happyShift, curry_1_5!(action_28)),
        146 => partial_5!(happyShift, curry_1_5!(action_29)),
        147 => partial_5!(happyShift, curry_1_5!(action_30)),
        148 => partial_5!(happyShift, curry_1_5!(action_31)),
        149 => partial_5!(happyShift, curry_1_5!(action_32)),
        150 => partial_5!(happyShift, curry_1_5!(action_726)),
        153 => partial_5!(happyShift, curry_1_5!(action_34)),
        164 => partial_5!(happyShift, curry_1_5!(action_35)),
        184 => partial_5!(happyShift, curry_1_5!(action_36)),
        186 => partial_5!(happyShift, curry_1_5!(action_171)),
        193 => partial_5!(happyShift, curry_1_5!(action_172)),
        204 => partial_5!(happyShift, curry_1_5!(action_37)),
        213 => partial_5!(happyShift, curry_1_5!(action_173)),
        214 => partial_5!(happyShift, curry_1_5!(action_174)),
        216 => partial_5!(happyShift, curry_1_5!(action_175)),
        220 => partial_5!(happyShift, curry_1_5!(action_38)),
        221 => happyReduce_472(),
        231 => partial_5!(happyShift, curry_1_5!(action_176)),
        233 => partial_5!(happyShift, curry_1_5!(action_39)),
        234 => partial_5!(happyShift, curry_1_5!(action_40)),
        235 => partial_5!(happyShift, curry_1_5!(action_41)),
        236 => partial_5!(happyShift, curry_1_5!(action_42)),
        237 => partial_5!(happyShift, curry_1_5!(action_43)),
        239 => partial_5!(happyShift, curry_1_5!(action_142)),
        240 => partial_5!(happyShift, curry_1_5!(action_44)),
        241 => partial_5!(happyShift, curry_1_5!(action_45)),
        242 => partial_5!(happyShift, curry_1_5!(action_46)),
        243 => partial_5!(happyShift, curry_1_5!(action_47)),
        244 => partial_5!(happyShift, curry_1_5!(action_48)),
        245 => partial_5!(happyShift, curry_1_5!(action_49)),
        64 => partial_5!(happyGoto, curry_1_5!(action_435)),
        100 => partial_5!(happyGoto, curry_1_5!(action_6)),
        104 => partial_5!(happyGoto, curry_1_5!(action_7)),
        106 => partial_5!(happyGoto, curry_1_5!(action_8)),
        107 => partial_5!(happyGoto, curry_1_5!(action_9)),
        108 => partial_5!(happyGoto, curry_1_5!(action_10)),
        109 => partial_5!(happyGoto, curry_1_5!(action_11)),
        110 => partial_5!(happyGoto, curry_1_5!(action_12)),
        111 => partial_5!(happyGoto, curry_1_5!(action_13)),
        112 => partial_5!(happyGoto, curry_1_5!(action_14)),
        113 => partial_5!(happyGoto, curry_1_5!(action_15)),
        114 => partial_5!(happyGoto, curry_1_5!(action_16)),
        115 => partial_5!(happyGoto, curry_1_5!(action_17)),
        116 => partial_5!(happyGoto, curry_1_5!(action_18)),
        117 => partial_5!(happyGoto, curry_1_5!(action_19)),
        118 => partial_5!(happyGoto, curry_1_5!(action_20)),
        119 => partial_5!(happyGoto, curry_1_5!(action_21)),
        120 => partial_5!(happyGoto, curry_1_5!(action_444)),
        125 => partial_5!(happyGoto, curry_1_5!(action_725)),
        127 => partial_5!(happyGoto, curry_1_5!(action_24)),
        128 => partial_5!(happyGoto, curry_1_5!(action_25)),
        134 => partial_5!(happyGoto, curry_1_5!(action_167)),
        _ => happyReduce_458()
    }
}

fn action_556(i: isize) -> ActionReturn {
    match i {
        141 => happyReduce_471(),
        239 => partial_5!(happyShift, curry_1_5!(action_142)),
        132 => partial_5!(happyGoto, curry_1_5!(action_724)),
        133 => partial_5!(happyGoto, curry_1_5!(action_150)),
        134 => partial_5!(happyGoto, curry_1_5!(action_110)),
        _ => happyReduce_401()
    }
}

fn action_557(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_93()
    }
}

fn action_558(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_24()
    }
}

fn action_559(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_26)),
        144 => partial_5!(happyShift, curry_1_5!(action_27)),
        145 => partial_5!(happyShift, curry_1_5!(action_28)),
        146 => partial_5!(happyShift, curry_1_5!(action_29)),
        147 => partial_5!(happyShift, curry_1_5!(action_30)),
        148 => partial_5!(happyShift, curry_1_5!(action_31)),
        149 => partial_5!(happyShift, curry_1_5!(action_32)),
        150 => partial_5!(happyShift, curry_1_5!(action_33)),
        153 => partial_5!(happyShift, curry_1_5!(action_34)),
        164 => partial_5!(happyShift, curry_1_5!(action_35)),
        184 => partial_5!(happyShift, curry_1_5!(action_36)),
        190 => partial_5!(happyShift, curry_1_5!(action_116)),
        192 => partial_5!(happyShift, curry_1_5!(action_117)),
        195 => partial_5!(happyShift, curry_1_5!(action_118)),
        198 => partial_5!(happyShift, curry_1_5!(action_119)),
        200 => partial_5!(happyShift, curry_1_5!(action_120)),
        202 => partial_5!(happyShift, curry_1_5!(action_122)),
        204 => partial_5!(happyShift, curry_1_5!(action_37)),
        208 => partial_5!(happyShift, curry_1_5!(action_124)),
        209 => partial_5!(happyShift, curry_1_5!(action_125)),
        210 => partial_5!(happyShift, curry_1_5!(action_126)),
        218 => partial_5!(happyShift, curry_1_5!(action_129)),
        219 => partial_5!(happyShift, curry_1_5!(action_130)),
        220 => partial_5!(happyShift, curry_1_5!(action_38)),
        223 => partial_5!(happyShift, curry_1_5!(action_133)),
        226 => partial_5!(happyShift, curry_1_5!(action_135)),
        228 => partial_5!(happyShift, curry_1_5!(action_137)),
        229 => partial_5!(happyShift, curry_1_5!(action_138)),
        230 => partial_5!(happyShift, curry_1_5!(action_139)),
        233 => partial_5!(happyShift, curry_1_5!(action_39)),
        234 => partial_5!(happyShift, curry_1_5!(action_40)),
        235 => partial_5!(happyShift, curry_1_5!(action_41)),
        236 => partial_5!(happyShift, curry_1_5!(action_42)),
        237 => partial_5!(happyShift, curry_1_5!(action_43)),
        238 => partial_5!(happyShift, curry_1_5!(action_141)),
        239 => partial_5!(happyShift, curry_1_5!(action_142)),
        240 => partial_5!(happyShift, curry_1_5!(action_44)),
        241 => partial_5!(happyShift, curry_1_5!(action_45)),
        242 => partial_5!(happyShift, curry_1_5!(action_46)),
        243 => partial_5!(happyShift, curry_1_5!(action_47)),
        244 => partial_5!(happyShift, curry_1_5!(action_48)),
        245 => partial_5!(happyShift, curry_1_5!(action_49)),
        44 => partial_5!(happyGoto, curry_1_5!(action_286)),
        45 => partial_5!(happyGoto, curry_1_5!(action_89)),
        47 => partial_5!(happyGoto, curry_1_5!(action_287)),
        49 => partial_5!(happyGoto, curry_1_5!(action_288)),
        51 => partial_5!(happyGoto, curry_1_5!(action_289)),
        52 => partial_5!(happyGoto, curry_1_5!(action_96)),
        53 => partial_5!(happyGoto, curry_1_5!(action_97)),
        54 => partial_5!(happyGoto, curry_1_5!(action_98)),
        61 => partial_5!(happyGoto, curry_1_5!(action_99)),
        65 => partial_5!(happyGoto, curry_1_5!(action_290)),
        86 => partial_5!(happyGoto, curry_1_5!(action_722)),
        100 => partial_5!(happyGoto, curry_1_5!(action_6)),
        104 => partial_5!(happyGoto, curry_1_5!(action_7)),
        106 => partial_5!(happyGoto, curry_1_5!(action_8)),
        107 => partial_5!(happyGoto, curry_1_5!(action_9)),
        108 => partial_5!(happyGoto, curry_1_5!(action_10)),
        109 => partial_5!(happyGoto, curry_1_5!(action_11)),
        110 => partial_5!(happyGoto, curry_1_5!(action_12)),
        111 => partial_5!(happyGoto, curry_1_5!(action_13)),
        112 => partial_5!(happyGoto, curry_1_5!(action_14)),
        113 => partial_5!(happyGoto, curry_1_5!(action_15)),
        114 => partial_5!(happyGoto, curry_1_5!(action_16)),
        115 => partial_5!(happyGoto, curry_1_5!(action_17)),
        116 => partial_5!(happyGoto, curry_1_5!(action_18)),
        117 => partial_5!(happyGoto, curry_1_5!(action_19)),
        118 => partial_5!(happyGoto, curry_1_5!(action_20)),
        119 => partial_5!(happyGoto, curry_1_5!(action_21)),
        120 => partial_5!(happyGoto, curry_1_5!(action_22)),
        122 => partial_5!(happyGoto, curry_1_5!(action_723)),
        127 => partial_5!(happyGoto, curry_1_5!(action_24)),
        128 => partial_5!(happyGoto, curry_1_5!(action_25)),
        132 => partial_5!(happyGoto, curry_1_5!(action_108)),
        133 => partial_5!(happyGoto, curry_1_5!(action_293)),
        134 => partial_5!(happyGoto, curry_1_5!(action_110)),
        _ => happyReduce_471()
    }
}

fn action_560(i: isize) -> ActionReturn {
    match i {
        181 => partial_5!(happyShift, curry_1_5!(action_61)),
        185 => partial_5!(happyShift, curry_1_5!(action_113)),
        188 => partial_5!(happyShift, curry_1_5!(action_115)),
        190 => partial_5!(happyShift, curry_1_5!(action_116)),
        192 => partial_5!(happyShift, curry_1_5!(action_117)),
        195 => partial_5!(happyShift, curry_1_5!(action_118)),
        198 => partial_5!(happyShift, curry_1_5!(action_119)),
        200 => partial_5!(happyShift, curry_1_5!(action_120)),
        201 => partial_5!(happyShift, curry_1_5!(action_121)),
        202 => partial_5!(happyShift, curry_1_5!(action_122)),
        207 => partial_5!(happyShift, curry_1_5!(action_123)),
        208 => partial_5!(happyShift, curry_1_5!(action_124)),
        209 => partial_5!(happyShift, curry_1_5!(action_125)),
        210 => partial_5!(happyShift, curry_1_5!(action_126)),
        212 => partial_5!(happyShift, curry_1_5!(action_127)),
        215 => partial_5!(happyShift, curry_1_5!(action_128)),
        218 => partial_5!(happyShift, curry_1_5!(action_129)),
        219 => partial_5!(happyShift, curry_1_5!(action_130)),
        221 => partial_5!(happyShift, curry_1_5!(action_131)),
        222 => partial_5!(happyShift, curry_1_5!(action_132)),
        223 => partial_5!(happyShift, curry_1_5!(action_133)),
        225 => partial_5!(happyShift, curry_1_5!(action_134)),
        226 => partial_5!(happyShift, curry_1_5!(action_135)),
        227 => partial_5!(happyShift, curry_1_5!(action_136)),
        228 => partial_5!(happyShift, curry_1_5!(action_137)),
        229 => partial_5!(happyShift, curry_1_5!(action_138)),
        230 => partial_5!(happyShift, curry_1_5!(action_139)),
        238 => partial_5!(happyShift, curry_1_5!(action_141)),
        239 => partial_5!(happyShift, curry_1_5!(action_142)),
        14 => partial_5!(happyGoto, curry_1_5!(action_721)),
        32 => partial_5!(happyGoto, curry_1_5!(action_463)),
        34 => partial_5!(happyGoto, curry_1_5!(action_80)),
        36 => partial_5!(happyGoto, curry_1_5!(action_81)),
        37 => partial_5!(happyGoto, curry_1_5!(action_464)),
        38 => partial_5!(happyGoto, curry_1_5!(action_465)),
        40 => partial_5!(happyGoto, curry_1_5!(action_84)),
        41 => partial_5!(happyGoto, curry_1_5!(action_85)),
        42 => partial_5!(happyGoto, curry_1_5!(action_86)),
        43 => partial_5!(happyGoto, curry_1_5!(action_87)),
        44 => partial_5!(happyGoto, curry_1_5!(action_466)),
        45 => partial_5!(happyGoto, curry_1_5!(action_89)),
        46 => partial_5!(happyGoto, curry_1_5!(action_90)),
        47 => partial_5!(happyGoto, curry_1_5!(action_91)),
        48 => partial_5!(happyGoto, curry_1_5!(action_92)),
        49 => partial_5!(happyGoto, curry_1_5!(action_93)),
        50 => partial_5!(happyGoto, curry_1_5!(action_94)),
        51 => partial_5!(happyGoto, curry_1_5!(action_95)),
        52 => partial_5!(happyGoto, curry_1_5!(action_96)),
        53 => partial_5!(happyGoto, curry_1_5!(action_97)),
        54 => partial_5!(happyGoto, curry_1_5!(action_98)),
        61 => partial_5!(happyGoto, curry_1_5!(action_99)),
        65 => partial_5!(happyGoto, curry_1_5!(action_467)),
        132 => partial_5!(happyGoto, curry_1_5!(action_108)),
        133 => partial_5!(happyGoto, curry_1_5!(action_468)),
        134 => partial_5!(happyGoto, curry_1_5!(action_110)),
        _ => happyReduce_471()
    }
}

fn action_561(i: isize) -> ActionReturn {
    match i {
        168 => partial_5!(happyShift, curry_1_5!(action_516)),
        94 => partial_5!(happyGoto, curry_1_5!(action_720)),
        _ => happyReduce_344()
    }
}

fn action_562(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_18()
    }
}

fn action_563(i: isize) -> ActionReturn {
    match i {
        139 => partial_5!(happyShift, curry_1_5!(action_719)),
        _ => box happyFail
    }
}

fn action_564(i: isize) -> ActionReturn {
    match i {
        139 => partial_5!(happyShift, curry_1_5!(action_718)),
        _ => box happyFail
    }
}

fn action_565(i: isize) -> ActionReturn {
    match i {
        180 => partial_5!(happyShift, curry_1_5!(action_715)),
        182 => partial_5!(happyShift, curry_1_5!(action_716)),
        190 => partial_5!(happyShift, curry_1_5!(action_116)),
        192 => partial_5!(happyShift, curry_1_5!(action_117)),
        195 => partial_5!(happyShift, curry_1_5!(action_118)),
        198 => partial_5!(happyShift, curry_1_5!(action_119)),
        200 => partial_5!(happyShift, curry_1_5!(action_120)),
        202 => partial_5!(happyShift, curry_1_5!(action_122)),
        208 => partial_5!(happyShift, curry_1_5!(action_124)),
        209 => partial_5!(happyShift, curry_1_5!(action_125)),
        210 => partial_5!(happyShift, curry_1_5!(action_126)),
        218 => partial_5!(happyShift, curry_1_5!(action_129)),
        219 => partial_5!(happyShift, curry_1_5!(action_130)),
        223 => partial_5!(happyShift, curry_1_5!(action_133)),
        226 => partial_5!(happyShift, curry_1_5!(action_135)),
        228 => partial_5!(happyShift, curry_1_5!(action_137)),
        229 => partial_5!(happyShift, curry_1_5!(action_138)),
        230 => partial_5!(happyShift, curry_1_5!(action_139)),
        238 => partial_5!(happyShift, curry_1_5!(action_141)),
        239 => partial_5!(happyShift, curry_1_5!(action_142)),
        240 => partial_5!(happyShift, curry_1_5!(action_717)),
        44 => partial_5!(happyGoto, curry_1_5!(action_709)),
        45 => partial_5!(happyGoto, curry_1_5!(action_89)),
        47 => partial_5!(happyGoto, curry_1_5!(action_287)),
        49 => partial_5!(happyGoto, curry_1_5!(action_288)),
        51 => partial_5!(happyGoto, curry_1_5!(action_289)),
        52 => partial_5!(happyGoto, curry_1_5!(action_96)),
        53 => partial_5!(happyGoto, curry_1_5!(action_97)),
        54 => partial_5!(happyGoto, curry_1_5!(action_98)),
        56 => partial_5!(happyGoto, curry_1_5!(action_710)),
        57 => partial_5!(happyGoto, curry_1_5!(action_711)),
        58 => partial_5!(happyGoto, curry_1_5!(action_712)),
        61 => partial_5!(happyGoto, curry_1_5!(action_99)),
        65 => partial_5!(happyGoto, curry_1_5!(action_713)),
        132 => partial_5!(happyGoto, curry_1_5!(action_108)),
        133 => partial_5!(happyGoto, curry_1_5!(action_714)),
        134 => partial_5!(happyGoto, curry_1_5!(action_110)),
        _ => happyReduce_471()
    }
}

fn action_566(i: isize) -> ActionReturn {
    match i {
        55 => partial_5!(happyGoto, curry_1_5!(action_708)),
        _ => happyReduce_191()
    }
}

fn action_567(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_99()
    }
}

fn action_568(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_22()
    }
}

fn action_569(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_413)),
        139 => partial_5!(happyShift, curry_1_5!(action_707)),
        140 => partial_5!(happyShift, curry_1_5!(action_182)),
        88 => partial_5!(happyGoto, curry_1_5!(action_573)),
        89 => partial_5!(happyGoto, curry_1_5!(action_179)),
        90 => partial_5!(happyGoto, curry_1_5!(action_180)),
        _ => box happyFail
    }
}

fn action_570(i: isize) -> ActionReturn {
    match i {
        139 => partial_5!(happyShift, curry_1_5!(action_706)),
        _ => box happyFail
    }
}

fn action_571(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_571)),
        150 => partial_5!(happyShift, curry_1_5!(action_572)),
        237 => partial_5!(happyShift, curry_1_5!(action_140)),
        239 => partial_5!(happyShift, curry_1_5!(action_142)),
        70 => partial_5!(happyGoto, curry_1_5!(action_419)),
        71 => partial_5!(happyGoto, curry_1_5!(action_222)),
        76 => partial_5!(happyGoto, curry_1_5!(action_157)),
        77 => partial_5!(happyGoto, curry_1_5!(action_103)),
        78 => partial_5!(happyGoto, curry_1_5!(action_512)),
        133 => partial_5!(happyGoto, curry_1_5!(action_422)),
        134 => partial_5!(happyGoto, curry_1_5!(action_110)),
        _ => box happyFail
    }
}

fn action_572(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_571)),
        150 => partial_5!(happyShift, curry_1_5!(action_572)),
        237 => partial_5!(happyShift, curry_1_5!(action_140)),
        238 => partial_5!(happyShift, curry_1_5!(action_229)),
        239 => partial_5!(happyShift, curry_1_5!(action_142)),
        65 => partial_5!(happyGoto, curry_1_5!(action_705)),
        69 => partial_5!(happyGoto, curry_1_5!(action_415)),
        70 => partial_5!(happyGoto, curry_1_5!(action_221)),
        71 => partial_5!(happyGoto, curry_1_5!(action_222)),
        75 => partial_5!(happyGoto, curry_1_5!(action_154)),
        76 => partial_5!(happyGoto, curry_1_5!(action_102)),
        77 => partial_5!(happyGoto, curry_1_5!(action_103)),
        78 => partial_5!(happyGoto, curry_1_5!(action_481)),
        132 => partial_5!(happyGoto, curry_1_5!(action_108)),
        133 => partial_5!(happyGoto, curry_1_5!(action_417)),
        134 => partial_5!(happyGoto, curry_1_5!(action_110)),
        _ => happyReduce_471()
    }
}

fn action_573(i: isize) -> ActionReturn {
    match i {
        139 => partial_5!(happyShift, curry_1_5!(action_704)),
        _ => box happyFail
    }
}

fn action_574(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_413)),
        140 => partial_5!(happyShift, curry_1_5!(action_182)),
        88 => partial_5!(happyGoto, curry_1_5!(action_703)),
        89 => partial_5!(happyGoto, curry_1_5!(action_179)),
        90 => partial_5!(happyGoto, curry_1_5!(action_180)),
        _ => happyReduce_255()
    }
}

fn action_575(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_413)),
        140 => partial_5!(happyShift, curry_1_5!(action_182)),
        88 => partial_5!(happyGoto, curry_1_5!(action_702)),
        89 => partial_5!(happyGoto, curry_1_5!(action_179)),
        90 => partial_5!(happyGoto, curry_1_5!(action_180)),
        _ => happyReduce_244()
    }
}

fn action_576(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_413)),
        139 => partial_5!(happyShift, curry_1_5!(action_701)),
        140 => partial_5!(happyShift, curry_1_5!(action_182)),
        88 => partial_5!(happyGoto, curry_1_5!(action_573)),
        89 => partial_5!(happyGoto, curry_1_5!(action_179)),
        90 => partial_5!(happyGoto, curry_1_5!(action_180)),
        _ => box happyFail
    }
}

fn action_577(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_241()
    }
}

fn action_578(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_242()
    }
}

fn action_579(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_253()
    }
}

fn action_580(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_700)),
        150 => partial_5!(happyShift, curry_1_5!(action_525)),
        186 => partial_5!(happyShift, curry_1_5!(action_171)),
        193 => partial_5!(happyShift, curry_1_5!(action_172)),
        213 => partial_5!(happyShift, curry_1_5!(action_173)),
        214 => partial_5!(happyShift, curry_1_5!(action_174)),
        216 => partial_5!(happyShift, curry_1_5!(action_175)),
        231 => partial_5!(happyShift, curry_1_5!(action_176)),
        237 => partial_5!(happyShift, curry_1_5!(action_140)),
        238 => partial_5!(happyShift, curry_1_5!(action_229)),
        239 => partial_5!(happyShift, curry_1_5!(action_142)),
        64 => partial_5!(happyGoto, curry_1_5!(action_435)),
        69 => partial_5!(happyGoto, curry_1_5!(action_698)),
        70 => partial_5!(happyGoto, curry_1_5!(action_221)),
        71 => partial_5!(happyGoto, curry_1_5!(action_222)),
        72 => partial_5!(happyGoto, curry_1_5!(action_699)),
        73 => partial_5!(happyGoto, curry_1_5!(action_224)),
        75 => partial_5!(happyGoto, curry_1_5!(action_508)),
        76 => partial_5!(happyGoto, curry_1_5!(action_102)),
        77 => partial_5!(happyGoto, curry_1_5!(action_103)),
        78 => partial_5!(happyGoto, curry_1_5!(action_481)),
        134 => partial_5!(happyGoto, curry_1_5!(action_167)),
        _ => box happyFail
    }
}

fn action_581(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_423)),
        150 => partial_5!(happyShift, curry_1_5!(action_228)),
        237 => partial_5!(happyShift, curry_1_5!(action_140)),
        238 => partial_5!(happyShift, curry_1_5!(action_424)),
        239 => partial_5!(happyShift, curry_1_5!(action_142)),
        70 => partial_5!(happyGoto, curry_1_5!(action_419)),
        71 => partial_5!(happyGoto, curry_1_5!(action_222)),
        72 => partial_5!(happyGoto, curry_1_5!(action_420)),
        73 => partial_5!(happyGoto, curry_1_5!(action_224)),
        74 => partial_5!(happyGoto, curry_1_5!(action_697)),
        76 => partial_5!(happyGoto, curry_1_5!(action_157)),
        77 => partial_5!(happyGoto, curry_1_5!(action_103)),
        78 => partial_5!(happyGoto, curry_1_5!(action_158)),
        80 => partial_5!(happyGoto, curry_1_5!(action_159)),
        81 => partial_5!(happyGoto, curry_1_5!(action_107)),
        133 => partial_5!(happyGoto, curry_1_5!(action_422)),
        134 => partial_5!(happyGoto, curry_1_5!(action_110)),
        _ => box happyFail
    }
}

fn action_582(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_92()
    }
}

fn action_583(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_23()
    }
}

fn action_584(i: isize) -> ActionReturn {
    match i {
        139 => partial_5!(happyShift, curry_1_5!(action_696)),
        _ => box happyFail
    }
}

fn action_585(i: isize) -> ActionReturn {
    match i {
        139 => partial_5!(happyShift, curry_1_5!(action_695)),
        _ => box happyFail
    }
}

fn action_586(i: isize) -> ActionReturn {
    match i {
        236 => partial_5!(happyShift, curry_1_5!(action_42)),
        128 => partial_5!(happyGoto, curry_1_5!(action_694)),
        _ => box happyFail
    }
}

fn action_587(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_97()
    }
}

fn action_588(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_98()
    }
}

fn action_589(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_21()
    }
}

fn action_590(i: isize) -> ActionReturn {
    match i {
        187 => partial_5!(happyShift, curry_1_5!(action_406)),
        35 => partial_5!(happyGoto, curry_1_5!(action_693)),
        67 => partial_5!(happyGoto, curry_1_5!(action_405)),
        _ => happyReduce_232()
    }
}

fn action_591(i: isize) -> ActionReturn {
    match i {
        187 => partial_5!(happyShift, curry_1_5!(action_406)),
        35 => partial_5!(happyGoto, curry_1_5!(action_692)),
        67 => partial_5!(happyGoto, curry_1_5!(action_405)),
        _ => happyReduce_232()
    }
}

fn action_592(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_26)),
        144 => partial_5!(happyShift, curry_1_5!(action_27)),
        145 => partial_5!(happyShift, curry_1_5!(action_28)),
        146 => partial_5!(happyShift, curry_1_5!(action_29)),
        147 => partial_5!(happyShift, curry_1_5!(action_30)),
        148 => partial_5!(happyShift, curry_1_5!(action_31)),
        149 => partial_5!(happyShift, curry_1_5!(action_32)),
        150 => partial_5!(happyShift, curry_1_5!(action_33)),
        153 => partial_5!(happyShift, curry_1_5!(action_34)),
        164 => partial_5!(happyShift, curry_1_5!(action_35)),
        180 => partial_5!(happyShift, curry_1_5!(action_60)),
        181 => partial_5!(happyShift, curry_1_5!(action_61)),
        184 => partial_5!(happyShift, curry_1_5!(action_36)),
        187 => partial_5!(happyShift, curry_1_5!(action_62)),
        189 => partial_5!(happyShift, curry_1_5!(action_63)),
        191 => partial_5!(happyShift, curry_1_5!(action_64)),
        194 => partial_5!(happyShift, curry_1_5!(action_65)),
        196 => partial_5!(happyShift, curry_1_5!(action_66)),
        197 => partial_5!(happyShift, curry_1_5!(action_67)),
        203 => partial_5!(happyShift, curry_1_5!(action_68)),
        204 => partial_5!(happyShift, curry_1_5!(action_37)),
        205 => partial_5!(happyShift, curry_1_5!(action_69)),
        206 => partial_5!(happyShift, curry_1_5!(action_70)),
        217 => partial_5!(happyShift, curry_1_5!(action_71)),
        220 => partial_5!(happyShift, curry_1_5!(action_38)),
        224 => partial_5!(happyShift, curry_1_5!(action_72)),
        232 => partial_5!(happyShift, curry_1_5!(action_73)),
        233 => partial_5!(happyShift, curry_1_5!(action_39)),
        234 => partial_5!(happyShift, curry_1_5!(action_40)),
        235 => partial_5!(happyShift, curry_1_5!(action_41)),
        236 => partial_5!(happyShift, curry_1_5!(action_42)),
        237 => partial_5!(happyShift, curry_1_5!(action_74)),
        238 => partial_5!(happyShift, curry_1_5!(action_75)),
        240 => partial_5!(happyShift, curry_1_5!(action_44)),
        241 => partial_5!(happyShift, curry_1_5!(action_45)),
        242 => partial_5!(happyShift, curry_1_5!(action_46)),
        243 => partial_5!(happyShift, curry_1_5!(action_47)),
        244 => partial_5!(happyShift, curry_1_5!(action_48)),
        245 => partial_5!(happyShift, curry_1_5!(action_49)),
        12 => partial_5!(happyGoto, curry_1_5!(action_691)),
        13 => partial_5!(happyGoto, curry_1_5!(action_51)),
        14 => partial_5!(happyGoto, curry_1_5!(action_52)),
        22 => partial_5!(happyGoto, curry_1_5!(action_53)),
        23 => partial_5!(happyGoto, curry_1_5!(action_54)),
        24 => partial_5!(happyGoto, curry_1_5!(action_55)),
        25 => partial_5!(happyGoto, curry_1_5!(action_56)),
        26 => partial_5!(happyGoto, curry_1_5!(action_57)),
        100 => partial_5!(happyGoto, curry_1_5!(action_6)),
        104 => partial_5!(happyGoto, curry_1_5!(action_7)),
        106 => partial_5!(happyGoto, curry_1_5!(action_8)),
        107 => partial_5!(happyGoto, curry_1_5!(action_9)),
        108 => partial_5!(happyGoto, curry_1_5!(action_10)),
        109 => partial_5!(happyGoto, curry_1_5!(action_11)),
        110 => partial_5!(happyGoto, curry_1_5!(action_12)),
        111 => partial_5!(happyGoto, curry_1_5!(action_13)),
        112 => partial_5!(happyGoto, curry_1_5!(action_14)),
        113 => partial_5!(happyGoto, curry_1_5!(action_15)),
        114 => partial_5!(happyGoto, curry_1_5!(action_16)),
        115 => partial_5!(happyGoto, curry_1_5!(action_17)),
        116 => partial_5!(happyGoto, curry_1_5!(action_18)),
        117 => partial_5!(happyGoto, curry_1_5!(action_19)),
        118 => partial_5!(happyGoto, curry_1_5!(action_20)),
        119 => partial_5!(happyGoto, curry_1_5!(action_21)),
        120 => partial_5!(happyGoto, curry_1_5!(action_22)),
        122 => partial_5!(happyGoto, curry_1_5!(action_58)),
        127 => partial_5!(happyGoto, curry_1_5!(action_24)),
        128 => partial_5!(happyGoto, curry_1_5!(action_25)),
        131 => partial_5!(happyGoto, curry_1_5!(action_59)),
        _ => box happyFail
    }
}

fn action_593(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_26)),
        144 => partial_5!(happyShift, curry_1_5!(action_27)),
        145 => partial_5!(happyShift, curry_1_5!(action_28)),
        146 => partial_5!(happyShift, curry_1_5!(action_29)),
        147 => partial_5!(happyShift, curry_1_5!(action_30)),
        148 => partial_5!(happyShift, curry_1_5!(action_31)),
        149 => partial_5!(happyShift, curry_1_5!(action_32)),
        150 => partial_5!(happyShift, curry_1_5!(action_33)),
        153 => partial_5!(happyShift, curry_1_5!(action_34)),
        164 => partial_5!(happyShift, curry_1_5!(action_35)),
        180 => partial_5!(happyShift, curry_1_5!(action_60)),
        181 => partial_5!(happyShift, curry_1_5!(action_61)),
        184 => partial_5!(happyShift, curry_1_5!(action_36)),
        187 => partial_5!(happyShift, curry_1_5!(action_62)),
        189 => partial_5!(happyShift, curry_1_5!(action_63)),
        191 => partial_5!(happyShift, curry_1_5!(action_64)),
        194 => partial_5!(happyShift, curry_1_5!(action_65)),
        196 => partial_5!(happyShift, curry_1_5!(action_66)),
        197 => partial_5!(happyShift, curry_1_5!(action_67)),
        203 => partial_5!(happyShift, curry_1_5!(action_68)),
        204 => partial_5!(happyShift, curry_1_5!(action_37)),
        205 => partial_5!(happyShift, curry_1_5!(action_69)),
        206 => partial_5!(happyShift, curry_1_5!(action_70)),
        217 => partial_5!(happyShift, curry_1_5!(action_71)),
        220 => partial_5!(happyShift, curry_1_5!(action_38)),
        224 => partial_5!(happyShift, curry_1_5!(action_72)),
        232 => partial_5!(happyShift, curry_1_5!(action_73)),
        233 => partial_5!(happyShift, curry_1_5!(action_39)),
        234 => partial_5!(happyShift, curry_1_5!(action_40)),
        235 => partial_5!(happyShift, curry_1_5!(action_41)),
        236 => partial_5!(happyShift, curry_1_5!(action_42)),
        237 => partial_5!(happyShift, curry_1_5!(action_74)),
        238 => partial_5!(happyShift, curry_1_5!(action_75)),
        240 => partial_5!(happyShift, curry_1_5!(action_44)),
        241 => partial_5!(happyShift, curry_1_5!(action_45)),
        242 => partial_5!(happyShift, curry_1_5!(action_46)),
        243 => partial_5!(happyShift, curry_1_5!(action_47)),
        244 => partial_5!(happyShift, curry_1_5!(action_48)),
        245 => partial_5!(happyShift, curry_1_5!(action_49)),
        12 => partial_5!(happyGoto, curry_1_5!(action_690)),
        13 => partial_5!(happyGoto, curry_1_5!(action_51)),
        14 => partial_5!(happyGoto, curry_1_5!(action_52)),
        22 => partial_5!(happyGoto, curry_1_5!(action_53)),
        23 => partial_5!(happyGoto, curry_1_5!(action_54)),
        24 => partial_5!(happyGoto, curry_1_5!(action_55)),
        25 => partial_5!(happyGoto, curry_1_5!(action_56)),
        26 => partial_5!(happyGoto, curry_1_5!(action_57)),
        100 => partial_5!(happyGoto, curry_1_5!(action_6)),
        104 => partial_5!(happyGoto, curry_1_5!(action_7)),
        106 => partial_5!(happyGoto, curry_1_5!(action_8)),
        107 => partial_5!(happyGoto, curry_1_5!(action_9)),
        108 => partial_5!(happyGoto, curry_1_5!(action_10)),
        109 => partial_5!(happyGoto, curry_1_5!(action_11)),
        110 => partial_5!(happyGoto, curry_1_5!(action_12)),
        111 => partial_5!(happyGoto, curry_1_5!(action_13)),
        112 => partial_5!(happyGoto, curry_1_5!(action_14)),
        113 => partial_5!(happyGoto, curry_1_5!(action_15)),
        114 => partial_5!(happyGoto, curry_1_5!(action_16)),
        115 => partial_5!(happyGoto, curry_1_5!(action_17)),
        116 => partial_5!(happyGoto, curry_1_5!(action_18)),
        117 => partial_5!(happyGoto, curry_1_5!(action_19)),
        118 => partial_5!(happyGoto, curry_1_5!(action_20)),
        119 => partial_5!(happyGoto, curry_1_5!(action_21)),
        120 => partial_5!(happyGoto, curry_1_5!(action_22)),
        122 => partial_5!(happyGoto, curry_1_5!(action_58)),
        127 => partial_5!(happyGoto, curry_1_5!(action_24)),
        128 => partial_5!(happyGoto, curry_1_5!(action_25)),
        131 => partial_5!(happyGoto, curry_1_5!(action_59)),
        _ => box happyFail
    }
}

fn action_594(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_26)),
        144 => partial_5!(happyShift, curry_1_5!(action_27)),
        145 => partial_5!(happyShift, curry_1_5!(action_28)),
        146 => partial_5!(happyShift, curry_1_5!(action_29)),
        147 => partial_5!(happyShift, curry_1_5!(action_30)),
        148 => partial_5!(happyShift, curry_1_5!(action_31)),
        149 => partial_5!(happyShift, curry_1_5!(action_32)),
        150 => partial_5!(happyShift, curry_1_5!(action_33)),
        153 => partial_5!(happyShift, curry_1_5!(action_34)),
        164 => partial_5!(happyShift, curry_1_5!(action_35)),
        180 => partial_5!(happyShift, curry_1_5!(action_60)),
        181 => partial_5!(happyShift, curry_1_5!(action_61)),
        184 => partial_5!(happyShift, curry_1_5!(action_36)),
        187 => partial_5!(happyShift, curry_1_5!(action_62)),
        189 => partial_5!(happyShift, curry_1_5!(action_63)),
        191 => partial_5!(happyShift, curry_1_5!(action_64)),
        194 => partial_5!(happyShift, curry_1_5!(action_65)),
        196 => partial_5!(happyShift, curry_1_5!(action_66)),
        197 => partial_5!(happyShift, curry_1_5!(action_67)),
        203 => partial_5!(happyShift, curry_1_5!(action_68)),
        204 => partial_5!(happyShift, curry_1_5!(action_37)),
        205 => partial_5!(happyShift, curry_1_5!(action_69)),
        206 => partial_5!(happyShift, curry_1_5!(action_70)),
        217 => partial_5!(happyShift, curry_1_5!(action_71)),
        220 => partial_5!(happyShift, curry_1_5!(action_38)),
        224 => partial_5!(happyShift, curry_1_5!(action_72)),
        232 => partial_5!(happyShift, curry_1_5!(action_73)),
        233 => partial_5!(happyShift, curry_1_5!(action_39)),
        234 => partial_5!(happyShift, curry_1_5!(action_40)),
        235 => partial_5!(happyShift, curry_1_5!(action_41)),
        236 => partial_5!(happyShift, curry_1_5!(action_42)),
        237 => partial_5!(happyShift, curry_1_5!(action_74)),
        238 => partial_5!(happyShift, curry_1_5!(action_75)),
        240 => partial_5!(happyShift, curry_1_5!(action_44)),
        241 => partial_5!(happyShift, curry_1_5!(action_45)),
        242 => partial_5!(happyShift, curry_1_5!(action_46)),
        243 => partial_5!(happyShift, curry_1_5!(action_47)),
        244 => partial_5!(happyShift, curry_1_5!(action_48)),
        245 => partial_5!(happyShift, curry_1_5!(action_49)),
        12 => partial_5!(happyGoto, curry_1_5!(action_689)),
        13 => partial_5!(happyGoto, curry_1_5!(action_51)),
        14 => partial_5!(happyGoto, curry_1_5!(action_52)),
        22 => partial_5!(happyGoto, curry_1_5!(action_53)),
        23 => partial_5!(happyGoto, curry_1_5!(action_54)),
        24 => partial_5!(happyGoto, curry_1_5!(action_55)),
        25 => partial_5!(happyGoto, curry_1_5!(action_56)),
        26 => partial_5!(happyGoto, curry_1_5!(action_57)),
        100 => partial_5!(happyGoto, curry_1_5!(action_6)),
        104 => partial_5!(happyGoto, curry_1_5!(action_7)),
        106 => partial_5!(happyGoto, curry_1_5!(action_8)),
        107 => partial_5!(happyGoto, curry_1_5!(action_9)),
        108 => partial_5!(happyGoto, curry_1_5!(action_10)),
        109 => partial_5!(happyGoto, curry_1_5!(action_11)),
        110 => partial_5!(happyGoto, curry_1_5!(action_12)),
        111 => partial_5!(happyGoto, curry_1_5!(action_13)),
        112 => partial_5!(happyGoto, curry_1_5!(action_14)),
        113 => partial_5!(happyGoto, curry_1_5!(action_15)),
        114 => partial_5!(happyGoto, curry_1_5!(action_16)),
        115 => partial_5!(happyGoto, curry_1_5!(action_17)),
        116 => partial_5!(happyGoto, curry_1_5!(action_18)),
        117 => partial_5!(happyGoto, curry_1_5!(action_19)),
        118 => partial_5!(happyGoto, curry_1_5!(action_20)),
        119 => partial_5!(happyGoto, curry_1_5!(action_21)),
        120 => partial_5!(happyGoto, curry_1_5!(action_22)),
        122 => partial_5!(happyGoto, curry_1_5!(action_58)),
        127 => partial_5!(happyGoto, curry_1_5!(action_24)),
        128 => partial_5!(happyGoto, curry_1_5!(action_25)),
        131 => partial_5!(happyGoto, curry_1_5!(action_59)),
        _ => box happyFail
    }
}

fn action_595(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_66()
    }
}

fn action_596(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_26)),
        144 => partial_5!(happyShift, curry_1_5!(action_27)),
        145 => partial_5!(happyShift, curry_1_5!(action_28)),
        146 => partial_5!(happyShift, curry_1_5!(action_29)),
        147 => partial_5!(happyShift, curry_1_5!(action_30)),
        148 => partial_5!(happyShift, curry_1_5!(action_31)),
        149 => partial_5!(happyShift, curry_1_5!(action_32)),
        150 => partial_5!(happyShift, curry_1_5!(action_33)),
        153 => partial_5!(happyShift, curry_1_5!(action_34)),
        164 => partial_5!(happyShift, curry_1_5!(action_35)),
        184 => partial_5!(happyShift, curry_1_5!(action_36)),
        204 => partial_5!(happyShift, curry_1_5!(action_37)),
        220 => partial_5!(happyShift, curry_1_5!(action_38)),
        233 => partial_5!(happyShift, curry_1_5!(action_39)),
        234 => partial_5!(happyShift, curry_1_5!(action_40)),
        235 => partial_5!(happyShift, curry_1_5!(action_41)),
        236 => partial_5!(happyShift, curry_1_5!(action_42)),
        237 => partial_5!(happyShift, curry_1_5!(action_43)),
        240 => partial_5!(happyShift, curry_1_5!(action_44)),
        241 => partial_5!(happyShift, curry_1_5!(action_45)),
        242 => partial_5!(happyShift, curry_1_5!(action_46)),
        243 => partial_5!(happyShift, curry_1_5!(action_47)),
        244 => partial_5!(happyShift, curry_1_5!(action_48)),
        245 => partial_5!(happyShift, curry_1_5!(action_49)),
        100 => partial_5!(happyGoto, curry_1_5!(action_6)),
        104 => partial_5!(happyGoto, curry_1_5!(action_7)),
        106 => partial_5!(happyGoto, curry_1_5!(action_8)),
        107 => partial_5!(happyGoto, curry_1_5!(action_9)),
        108 => partial_5!(happyGoto, curry_1_5!(action_10)),
        109 => partial_5!(happyGoto, curry_1_5!(action_11)),
        110 => partial_5!(happyGoto, curry_1_5!(action_12)),
        111 => partial_5!(happyGoto, curry_1_5!(action_13)),
        112 => partial_5!(happyGoto, curry_1_5!(action_14)),
        113 => partial_5!(happyGoto, curry_1_5!(action_15)),
        114 => partial_5!(happyGoto, curry_1_5!(action_16)),
        115 => partial_5!(happyGoto, curry_1_5!(action_17)),
        116 => partial_5!(happyGoto, curry_1_5!(action_18)),
        117 => partial_5!(happyGoto, curry_1_5!(action_19)),
        118 => partial_5!(happyGoto, curry_1_5!(action_20)),
        119 => partial_5!(happyGoto, curry_1_5!(action_21)),
        120 => partial_5!(happyGoto, curry_1_5!(action_22)),
        122 => partial_5!(happyGoto, curry_1_5!(action_249)),
        124 => partial_5!(happyGoto, curry_1_5!(action_688)),
        127 => partial_5!(happyGoto, curry_1_5!(action_24)),
        128 => partial_5!(happyGoto, curry_1_5!(action_25)),
        _ => happyReduce_456()
    }
}

fn action_597(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_26)),
        144 => partial_5!(happyShift, curry_1_5!(action_27)),
        145 => partial_5!(happyShift, curry_1_5!(action_28)),
        146 => partial_5!(happyShift, curry_1_5!(action_29)),
        147 => partial_5!(happyShift, curry_1_5!(action_30)),
        148 => partial_5!(happyShift, curry_1_5!(action_31)),
        149 => partial_5!(happyShift, curry_1_5!(action_32)),
        150 => partial_5!(happyShift, curry_1_5!(action_33)),
        153 => partial_5!(happyShift, curry_1_5!(action_34)),
        164 => partial_5!(happyShift, curry_1_5!(action_35)),
        184 => partial_5!(happyShift, curry_1_5!(action_36)),
        204 => partial_5!(happyShift, curry_1_5!(action_37)),
        220 => partial_5!(happyShift, curry_1_5!(action_38)),
        233 => partial_5!(happyShift, curry_1_5!(action_39)),
        234 => partial_5!(happyShift, curry_1_5!(action_40)),
        235 => partial_5!(happyShift, curry_1_5!(action_41)),
        236 => partial_5!(happyShift, curry_1_5!(action_42)),
        237 => partial_5!(happyShift, curry_1_5!(action_43)),
        240 => partial_5!(happyShift, curry_1_5!(action_44)),
        241 => partial_5!(happyShift, curry_1_5!(action_45)),
        242 => partial_5!(happyShift, curry_1_5!(action_46)),
        243 => partial_5!(happyShift, curry_1_5!(action_47)),
        244 => partial_5!(happyShift, curry_1_5!(action_48)),
        245 => partial_5!(happyShift, curry_1_5!(action_49)),
        100 => partial_5!(happyGoto, curry_1_5!(action_6)),
        104 => partial_5!(happyGoto, curry_1_5!(action_7)),
        106 => partial_5!(happyGoto, curry_1_5!(action_8)),
        107 => partial_5!(happyGoto, curry_1_5!(action_9)),
        108 => partial_5!(happyGoto, curry_1_5!(action_10)),
        109 => partial_5!(happyGoto, curry_1_5!(action_11)),
        110 => partial_5!(happyGoto, curry_1_5!(action_12)),
        111 => partial_5!(happyGoto, curry_1_5!(action_13)),
        112 => partial_5!(happyGoto, curry_1_5!(action_14)),
        113 => partial_5!(happyGoto, curry_1_5!(action_15)),
        114 => partial_5!(happyGoto, curry_1_5!(action_16)),
        115 => partial_5!(happyGoto, curry_1_5!(action_17)),
        116 => partial_5!(happyGoto, curry_1_5!(action_18)),
        117 => partial_5!(happyGoto, curry_1_5!(action_19)),
        118 => partial_5!(happyGoto, curry_1_5!(action_20)),
        119 => partial_5!(happyGoto, curry_1_5!(action_21)),
        120 => partial_5!(happyGoto, curry_1_5!(action_22)),
        122 => partial_5!(happyGoto, curry_1_5!(action_249)),
        124 => partial_5!(happyGoto, curry_1_5!(action_687)),
        127 => partial_5!(happyGoto, curry_1_5!(action_24)),
        128 => partial_5!(happyGoto, curry_1_5!(action_25)),
        _ => happyReduce_456()
    }
}

fn action_598(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_26)),
        144 => partial_5!(happyShift, curry_1_5!(action_27)),
        145 => partial_5!(happyShift, curry_1_5!(action_28)),
        146 => partial_5!(happyShift, curry_1_5!(action_29)),
        147 => partial_5!(happyShift, curry_1_5!(action_30)),
        148 => partial_5!(happyShift, curry_1_5!(action_31)),
        149 => partial_5!(happyShift, curry_1_5!(action_32)),
        150 => partial_5!(happyShift, curry_1_5!(action_33)),
        153 => partial_5!(happyShift, curry_1_5!(action_34)),
        164 => partial_5!(happyShift, curry_1_5!(action_35)),
        184 => partial_5!(happyShift, curry_1_5!(action_36)),
        204 => partial_5!(happyShift, curry_1_5!(action_37)),
        220 => partial_5!(happyShift, curry_1_5!(action_38)),
        233 => partial_5!(happyShift, curry_1_5!(action_39)),
        234 => partial_5!(happyShift, curry_1_5!(action_40)),
        235 => partial_5!(happyShift, curry_1_5!(action_41)),
        236 => partial_5!(happyShift, curry_1_5!(action_42)),
        237 => partial_5!(happyShift, curry_1_5!(action_43)),
        240 => partial_5!(happyShift, curry_1_5!(action_44)),
        241 => partial_5!(happyShift, curry_1_5!(action_45)),
        242 => partial_5!(happyShift, curry_1_5!(action_46)),
        243 => partial_5!(happyShift, curry_1_5!(action_47)),
        244 => partial_5!(happyShift, curry_1_5!(action_48)),
        245 => partial_5!(happyShift, curry_1_5!(action_49)),
        100 => partial_5!(happyGoto, curry_1_5!(action_6)),
        104 => partial_5!(happyGoto, curry_1_5!(action_7)),
        106 => partial_5!(happyGoto, curry_1_5!(action_8)),
        107 => partial_5!(happyGoto, curry_1_5!(action_9)),
        108 => partial_5!(happyGoto, curry_1_5!(action_10)),
        109 => partial_5!(happyGoto, curry_1_5!(action_11)),
        110 => partial_5!(happyGoto, curry_1_5!(action_12)),
        111 => partial_5!(happyGoto, curry_1_5!(action_13)),
        112 => partial_5!(happyGoto, curry_1_5!(action_14)),
        113 => partial_5!(happyGoto, curry_1_5!(action_15)),
        114 => partial_5!(happyGoto, curry_1_5!(action_16)),
        115 => partial_5!(happyGoto, curry_1_5!(action_17)),
        116 => partial_5!(happyGoto, curry_1_5!(action_18)),
        117 => partial_5!(happyGoto, curry_1_5!(action_19)),
        118 => partial_5!(happyGoto, curry_1_5!(action_20)),
        119 => partial_5!(happyGoto, curry_1_5!(action_21)),
        120 => partial_5!(happyGoto, curry_1_5!(action_22)),
        122 => partial_5!(happyGoto, curry_1_5!(action_686)),
        127 => partial_5!(happyGoto, curry_1_5!(action_24)),
        128 => partial_5!(happyGoto, curry_1_5!(action_25)),
        _ => box happyFail
    }
}

fn action_599(i: isize) -> ActionReturn {
    match i {
        167 => partial_5!(happyShift, curry_1_5!(action_685)),
        _ => box happyFail
    }
}

fn action_600(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_35()
    }
}

fn action_601(i: isize) -> ActionReturn {
    match i {
        139 => partial_5!(happyShift, curry_1_5!(action_683)),
        167 => partial_5!(happyShift, curry_1_5!(action_684)),
        _ => box happyFail
    }
}

fn action_602(i: isize) -> ActionReturn {
    match i {
        179 => partial_5!(happyShift, curry_1_5!(action_528)),
        180 => partial_5!(happyShift, curry_1_5!(action_682)),
        _ => box happyFail
    }
}

fn action_603(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_26)),
        144 => partial_5!(happyShift, curry_1_5!(action_27)),
        145 => partial_5!(happyShift, curry_1_5!(action_28)),
        146 => partial_5!(happyShift, curry_1_5!(action_29)),
        147 => partial_5!(happyShift, curry_1_5!(action_30)),
        148 => partial_5!(happyShift, curry_1_5!(action_31)),
        149 => partial_5!(happyShift, curry_1_5!(action_32)),
        150 => partial_5!(happyShift, curry_1_5!(action_33)),
        153 => partial_5!(happyShift, curry_1_5!(action_34)),
        164 => partial_5!(happyShift, curry_1_5!(action_35)),
        180 => partial_5!(happyShift, curry_1_5!(action_60)),
        181 => partial_5!(happyShift, curry_1_5!(action_61)),
        184 => partial_5!(happyShift, curry_1_5!(action_36)),
        185 => partial_5!(happyShift, curry_1_5!(action_113)),
        186 => happyReduce_471(),
        187 => partial_5!(happyShift, curry_1_5!(action_62)),
        188 => partial_5!(happyShift, curry_1_5!(action_115)),
        189 => partial_5!(happyShift, curry_1_5!(action_63)),
        190 => partial_5!(happyShift, curry_1_5!(action_116)),
        191 => partial_5!(happyShift, curry_1_5!(action_64)),
        192 => partial_5!(happyShift, curry_1_5!(action_117)),
        193 => happyReduce_471(),
        194 => partial_5!(happyShift, curry_1_5!(action_65)),
        195 => partial_5!(happyShift, curry_1_5!(action_118)),
        196 => partial_5!(happyShift, curry_1_5!(action_66)),
        197 => partial_5!(happyShift, curry_1_5!(action_67)),
        198 => partial_5!(happyShift, curry_1_5!(action_119)),
        200 => partial_5!(happyShift, curry_1_5!(action_120)),
        201 => partial_5!(happyShift, curry_1_5!(action_121)),
        202 => partial_5!(happyShift, curry_1_5!(action_122)),
        203 => partial_5!(happyShift, curry_1_5!(action_68)),
        204 => partial_5!(happyShift, curry_1_5!(action_37)),
        205 => partial_5!(happyShift, curry_1_5!(action_69)),
        206 => partial_5!(happyShift, curry_1_5!(action_70)),
        207 => partial_5!(happyShift, curry_1_5!(action_123)),
        208 => partial_5!(happyShift, curry_1_5!(action_124)),
        209 => partial_5!(happyShift, curry_1_5!(action_125)),
        210 => partial_5!(happyShift, curry_1_5!(action_126)),
        212 => partial_5!(happyShift, curry_1_5!(action_127)),
        213 => happyReduce_471(),
        214 => happyReduce_471(),
        215 => partial_5!(happyShift, curry_1_5!(action_128)),
        216 => happyReduce_471(),
        217 => partial_5!(happyShift, curry_1_5!(action_71)),
        218 => partial_5!(happyShift, curry_1_5!(action_129)),
        219 => partial_5!(happyShift, curry_1_5!(action_130)),
        220 => partial_5!(happyShift, curry_1_5!(action_38)),
        221 => partial_5!(happyShift, curry_1_5!(action_131)),
        222 => partial_5!(happyShift, curry_1_5!(action_132)),
        223 => partial_5!(happyShift, curry_1_5!(action_133)),
        224 => partial_5!(happyShift, curry_1_5!(action_72)),
        225 => partial_5!(happyShift, curry_1_5!(action_134)),
        226 => partial_5!(happyShift, curry_1_5!(action_135)),
        227 => partial_5!(happyShift, curry_1_5!(action_136)),
        228 => partial_5!(happyShift, curry_1_5!(action_137)),
        229 => partial_5!(happyShift, curry_1_5!(action_138)),
        230 => partial_5!(happyShift, curry_1_5!(action_139)),
        231 => happyReduce_471(),
        232 => partial_5!(happyShift, curry_1_5!(action_73)),
        233 => partial_5!(happyShift, curry_1_5!(action_39)),
        234 => partial_5!(happyShift, curry_1_5!(action_40)),
        235 => partial_5!(happyShift, curry_1_5!(action_41)),
        236 => partial_5!(happyShift, curry_1_5!(action_42)),
        237 => partial_5!(happyShift, curry_1_5!(action_74)),
        238 => partial_5!(happyShift, curry_1_5!(action_615)),
        239 => partial_5!(happyShift, curry_1_5!(action_142)),
        240 => partial_5!(happyShift, curry_1_5!(action_616)),
        241 => partial_5!(happyShift, curry_1_5!(action_45)),
        242 => partial_5!(happyShift, curry_1_5!(action_46)),
        243 => partial_5!(happyShift, curry_1_5!(action_47)),
        244 => partial_5!(happyShift, curry_1_5!(action_48)),
        245 => partial_5!(happyShift, curry_1_5!(action_49)),
        12 => partial_5!(happyGoto, curry_1_5!(action_605)),
        13 => partial_5!(happyGoto, curry_1_5!(action_51)),
        14 => partial_5!(happyGoto, curry_1_5!(action_52)),
        16 => partial_5!(happyGoto, curry_1_5!(action_681)),
        18 => partial_5!(happyGoto, curry_1_5!(action_607)),
        19 => partial_5!(happyGoto, curry_1_5!(action_608)),
        20 => partial_5!(happyGoto, curry_1_5!(action_609)),
        22 => partial_5!(happyGoto, curry_1_5!(action_53)),
        23 => partial_5!(happyGoto, curry_1_5!(action_54)),
        24 => partial_5!(happyGoto, curry_1_5!(action_55)),
        25 => partial_5!(happyGoto, curry_1_5!(action_56)),
        26 => partial_5!(happyGoto, curry_1_5!(action_57)),
        32 => partial_5!(happyGoto, curry_1_5!(action_610)),
        34 => partial_5!(happyGoto, curry_1_5!(action_80)),
        36 => partial_5!(happyGoto, curry_1_5!(action_81)),
        37 => partial_5!(happyGoto, curry_1_5!(action_611)),
        38 => partial_5!(happyGoto, curry_1_5!(action_612)),
        40 => partial_5!(happyGoto, curry_1_5!(action_84)),
        41 => partial_5!(happyGoto, curry_1_5!(action_85)),
        42 => partial_5!(happyGoto, curry_1_5!(action_86)),
        43 => partial_5!(happyGoto, curry_1_5!(action_87)),
        44 => partial_5!(happyGoto, curry_1_5!(action_613)),
        45 => partial_5!(happyGoto, curry_1_5!(action_89)),
        46 => partial_5!(happyGoto, curry_1_5!(action_90)),
        47 => partial_5!(happyGoto, curry_1_5!(action_91)),
        48 => partial_5!(happyGoto, curry_1_5!(action_92)),
        49 => partial_5!(happyGoto, curry_1_5!(action_93)),
        50 => partial_5!(happyGoto, curry_1_5!(action_94)),
        51 => partial_5!(happyGoto, curry_1_5!(action_95)),
        52 => partial_5!(happyGoto, curry_1_5!(action_96)),
        53 => partial_5!(happyGoto, curry_1_5!(action_97)),
        54 => partial_5!(happyGoto, curry_1_5!(action_98)),
        61 => partial_5!(happyGoto, curry_1_5!(action_99)),
        65 => partial_5!(happyGoto, curry_1_5!(action_614)),
        100 => partial_5!(happyGoto, curry_1_5!(action_6)),
        104 => partial_5!(happyGoto, curry_1_5!(action_7)),
        106 => partial_5!(happyGoto, curry_1_5!(action_8)),
        107 => partial_5!(happyGoto, curry_1_5!(action_9)),
        108 => partial_5!(happyGoto, curry_1_5!(action_10)),
        109 => partial_5!(happyGoto, curry_1_5!(action_11)),
        110 => partial_5!(happyGoto, curry_1_5!(action_12)),
        111 => partial_5!(happyGoto, curry_1_5!(action_13)),
        112 => partial_5!(happyGoto, curry_1_5!(action_14)),
        113 => partial_5!(happyGoto, curry_1_5!(action_15)),
        114 => partial_5!(happyGoto, curry_1_5!(action_16)),
        115 => partial_5!(happyGoto, curry_1_5!(action_17)),
        116 => partial_5!(happyGoto, curry_1_5!(action_18)),
        117 => partial_5!(happyGoto, curry_1_5!(action_19)),
        118 => partial_5!(happyGoto, curry_1_5!(action_20)),
        119 => partial_5!(happyGoto, curry_1_5!(action_21)),
        120 => partial_5!(happyGoto, curry_1_5!(action_22)),
        122 => partial_5!(happyGoto, curry_1_5!(action_58)),
        127 => partial_5!(happyGoto, curry_1_5!(action_24)),
        128 => partial_5!(happyGoto, curry_1_5!(action_25)),
        131 => partial_5!(happyGoto, curry_1_5!(action_59)),
        132 => partial_5!(happyGoto, curry_1_5!(action_108)),
        133 => partial_5!(happyGoto, curry_1_5!(action_468)),
        134 => partial_5!(happyGoto, curry_1_5!(action_110)),
        _ => happyReduce_41()
    }
}

fn action_604(i: isize) -> ActionReturn {
    match i {
        237 => partial_5!(happyShift, curry_1_5!(action_460)),
        85 => partial_5!(happyGoto, curry_1_5!(action_680)),
        _ => box happyFail
    }
}

fn action_605(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_44()
    }
}

fn action_606(i: isize) -> ActionReturn {
    match i {
        182 => partial_5!(happyShift, curry_1_5!(action_679)),
        _ => box happyFail
    }
}

fn action_607(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_43()
    }
}

fn action_608(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_45()
    }
}

fn action_609(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_47()
    }
}

fn action_610(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_46()
    }
}

fn action_611(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_524)),
        150 => partial_5!(happyShift, curry_1_5!(action_525)),
        237 => partial_5!(happyShift, curry_1_5!(action_140)),
        238 => partial_5!(happyShift, curry_1_5!(action_229)),
        11 => partial_5!(happyGoto, curry_1_5!(action_678)),
        66 => partial_5!(happyGoto, curry_1_5!(action_240)),
        68 => partial_5!(happyGoto, curry_1_5!(action_219)),
        69 => partial_5!(happyGoto, curry_1_5!(action_220)),
        70 => partial_5!(happyGoto, curry_1_5!(action_221)),
        71 => partial_5!(happyGoto, curry_1_5!(action_222)),
        72 => partial_5!(happyGoto, curry_1_5!(action_223)),
        73 => partial_5!(happyGoto, curry_1_5!(action_224)),
        75 => partial_5!(happyGoto, curry_1_5!(action_225)),
        76 => partial_5!(happyGoto, curry_1_5!(action_102)),
        77 => partial_5!(happyGoto, curry_1_5!(action_103)),
        78 => partial_5!(happyGoto, curry_1_5!(action_481)),
        _ => box happyFail
    }
}

fn action_612(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_475)),
        150 => partial_5!(happyShift, curry_1_5!(action_476)),
        185 => partial_5!(happyShift, curry_1_5!(action_113)),
        186 => partial_5!(happyShift, curry_1_5!(action_171)),
        188 => partial_5!(happyShift, curry_1_5!(action_115)),
        190 => partial_5!(happyShift, curry_1_5!(action_116)),
        192 => partial_5!(happyShift, curry_1_5!(action_117)),
        193 => partial_5!(happyShift, curry_1_5!(action_172)),
        195 => partial_5!(happyShift, curry_1_5!(action_118)),
        198 => partial_5!(happyShift, curry_1_5!(action_119)),
        200 => partial_5!(happyShift, curry_1_5!(action_120)),
        201 => partial_5!(happyShift, curry_1_5!(action_121)),
        202 => partial_5!(happyShift, curry_1_5!(action_122)),
        207 => partial_5!(happyShift, curry_1_5!(action_123)),
        208 => partial_5!(happyShift, curry_1_5!(action_124)),
        209 => partial_5!(happyShift, curry_1_5!(action_125)),
        210 => partial_5!(happyShift, curry_1_5!(action_126)),
        212 => partial_5!(happyShift, curry_1_5!(action_127)),
        213 => partial_5!(happyShift, curry_1_5!(action_173)),
        214 => partial_5!(happyShift, curry_1_5!(action_174)),
        215 => partial_5!(happyShift, curry_1_5!(action_128)),
        216 => partial_5!(happyShift, curry_1_5!(action_175)),
        218 => partial_5!(happyShift, curry_1_5!(action_129)),
        219 => partial_5!(happyShift, curry_1_5!(action_130)),
        221 => partial_5!(happyShift, curry_1_5!(action_131)),
        223 => partial_5!(happyShift, curry_1_5!(action_133)),
        225 => partial_5!(happyShift, curry_1_5!(action_134)),
        226 => partial_5!(happyShift, curry_1_5!(action_237)),
        227 => partial_5!(happyShift, curry_1_5!(action_136)),
        228 => partial_5!(happyShift, curry_1_5!(action_137)),
        229 => partial_5!(happyShift, curry_1_5!(action_138)),
        230 => partial_5!(happyShift, curry_1_5!(action_139)),
        231 => partial_5!(happyShift, curry_1_5!(action_176)),
        237 => partial_5!(happyShift, curry_1_5!(action_140)),
        238 => partial_5!(happyShift, curry_1_5!(action_238)),
        239 => partial_5!(happyShift, curry_1_5!(action_142)),
        11 => partial_5!(happyGoto, curry_1_5!(action_677)),
        39 => partial_5!(happyGoto, curry_1_5!(action_231)),
        41 => partial_5!(happyGoto, curry_1_5!(action_198)),
        42 => partial_5!(happyGoto, curry_1_5!(action_199)),
        43 => partial_5!(happyGoto, curry_1_5!(action_200)),
        45 => partial_5!(happyGoto, curry_1_5!(action_232)),
        52 => partial_5!(happyGoto, curry_1_5!(action_233)),
        53 => partial_5!(happyGoto, curry_1_5!(action_97)),
        54 => partial_5!(happyGoto, curry_1_5!(action_98)),
        61 => partial_5!(happyGoto, curry_1_5!(action_99)),
        64 => partial_5!(happyGoto, curry_1_5!(action_201)),
        75 => partial_5!(happyGoto, curry_1_5!(action_234)),
        76 => partial_5!(happyGoto, curry_1_5!(action_102)),
        77 => partial_5!(happyGoto, curry_1_5!(action_103)),
        78 => partial_5!(happyGoto, curry_1_5!(action_481)),
        134 => partial_5!(happyGoto, curry_1_5!(action_236)),
        _ => box happyFail
    }
}

fn action_613(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_524)),
        150 => partial_5!(happyShift, curry_1_5!(action_525)),
        237 => partial_5!(happyShift, curry_1_5!(action_140)),
        238 => partial_5!(happyShift, curry_1_5!(action_229)),
        11 => partial_5!(happyGoto, curry_1_5!(action_676)),
        66 => partial_5!(happyGoto, curry_1_5!(action_218)),
        68 => partial_5!(happyGoto, curry_1_5!(action_219)),
        69 => partial_5!(happyGoto, curry_1_5!(action_220)),
        70 => partial_5!(happyGoto, curry_1_5!(action_221)),
        71 => partial_5!(happyGoto, curry_1_5!(action_222)),
        72 => partial_5!(happyGoto, curry_1_5!(action_223)),
        73 => partial_5!(happyGoto, curry_1_5!(action_224)),
        75 => partial_5!(happyGoto, curry_1_5!(action_225)),
        76 => partial_5!(happyGoto, curry_1_5!(action_102)),
        77 => partial_5!(happyGoto, curry_1_5!(action_103)),
        78 => partial_5!(happyGoto, curry_1_5!(action_481)),
        _ => box happyFail
    }
}

fn action_614(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_475)),
        150 => partial_5!(happyShift, curry_1_5!(action_476)),
        185 => partial_5!(happyShift, curry_1_5!(action_113)),
        186 => partial_5!(happyShift, curry_1_5!(action_171)),
        188 => partial_5!(happyShift, curry_1_5!(action_115)),
        190 => partial_5!(happyShift, curry_1_5!(action_116)),
        192 => partial_5!(happyShift, curry_1_5!(action_117)),
        193 => partial_5!(happyShift, curry_1_5!(action_172)),
        195 => partial_5!(happyShift, curry_1_5!(action_118)),
        198 => partial_5!(happyShift, curry_1_5!(action_119)),
        200 => partial_5!(happyShift, curry_1_5!(action_120)),
        201 => partial_5!(happyShift, curry_1_5!(action_121)),
        202 => partial_5!(happyShift, curry_1_5!(action_122)),
        207 => partial_5!(happyShift, curry_1_5!(action_123)),
        208 => partial_5!(happyShift, curry_1_5!(action_124)),
        209 => partial_5!(happyShift, curry_1_5!(action_125)),
        210 => partial_5!(happyShift, curry_1_5!(action_126)),
        212 => partial_5!(happyShift, curry_1_5!(action_127)),
        213 => partial_5!(happyShift, curry_1_5!(action_173)),
        214 => partial_5!(happyShift, curry_1_5!(action_174)),
        215 => partial_5!(happyShift, curry_1_5!(action_128)),
        216 => partial_5!(happyShift, curry_1_5!(action_175)),
        218 => partial_5!(happyShift, curry_1_5!(action_129)),
        219 => partial_5!(happyShift, curry_1_5!(action_130)),
        221 => partial_5!(happyShift, curry_1_5!(action_131)),
        223 => partial_5!(happyShift, curry_1_5!(action_133)),
        225 => partial_5!(happyShift, curry_1_5!(action_134)),
        226 => partial_5!(happyShift, curry_1_5!(action_191)),
        227 => partial_5!(happyShift, curry_1_5!(action_136)),
        228 => partial_5!(happyShift, curry_1_5!(action_137)),
        229 => partial_5!(happyShift, curry_1_5!(action_138)),
        230 => partial_5!(happyShift, curry_1_5!(action_139)),
        231 => partial_5!(happyShift, curry_1_5!(action_176)),
        237 => partial_5!(happyShift, curry_1_5!(action_140)),
        238 => partial_5!(happyShift, curry_1_5!(action_192)),
        239 => partial_5!(happyShift, curry_1_5!(action_142)),
        11 => partial_5!(happyGoto, curry_1_5!(action_674)),
        40 => partial_5!(happyGoto, curry_1_5!(action_184)),
        41 => partial_5!(happyGoto, curry_1_5!(action_85)),
        42 => partial_5!(happyGoto, curry_1_5!(action_86)),
        43 => partial_5!(happyGoto, curry_1_5!(action_87)),
        45 => partial_5!(happyGoto, curry_1_5!(action_185)),
        52 => partial_5!(happyGoto, curry_1_5!(action_186)),
        53 => partial_5!(happyGoto, curry_1_5!(action_97)),
        54 => partial_5!(happyGoto, curry_1_5!(action_98)),
        61 => partial_5!(happyGoto, curry_1_5!(action_99)),
        64 => partial_5!(happyGoto, curry_1_5!(action_187)),
        75 => partial_5!(happyGoto, curry_1_5!(action_188)),
        76 => partial_5!(happyGoto, curry_1_5!(action_102)),
        77 => partial_5!(happyGoto, curry_1_5!(action_103)),
        78 => partial_5!(happyGoto, curry_1_5!(action_481)),
        133 => partial_5!(happyGoto, curry_1_5!(action_675)),
        134 => partial_5!(happyGoto, curry_1_5!(action_110)),
        _ => box happyFail
    }
}

fn action_615(i: isize) -> ActionReturn {
    match i {
        167 => happyReduce_470(),
        _ => happyReduce_170()
    }
}

fn action_616(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_26)),
        144 => partial_5!(happyShift, curry_1_5!(action_27)),
        145 => partial_5!(happyShift, curry_1_5!(action_28)),
        146 => partial_5!(happyShift, curry_1_5!(action_29)),
        147 => partial_5!(happyShift, curry_1_5!(action_30)),
        148 => partial_5!(happyShift, curry_1_5!(action_31)),
        149 => partial_5!(happyShift, curry_1_5!(action_32)),
        150 => partial_5!(happyShift, curry_1_5!(action_33)),
        153 => partial_5!(happyShift, curry_1_5!(action_34)),
        164 => partial_5!(happyShift, curry_1_5!(action_35)),
        184 => partial_5!(happyShift, curry_1_5!(action_36)),
        185 => partial_5!(happyShift, curry_1_5!(action_113)),
        188 => partial_5!(happyShift, curry_1_5!(action_115)),
        190 => partial_5!(happyShift, curry_1_5!(action_116)),
        192 => partial_5!(happyShift, curry_1_5!(action_117)),
        195 => partial_5!(happyShift, curry_1_5!(action_118)),
        198 => partial_5!(happyShift, curry_1_5!(action_119)),
        200 => partial_5!(happyShift, curry_1_5!(action_120)),
        201 => partial_5!(happyShift, curry_1_5!(action_121)),
        202 => partial_5!(happyShift, curry_1_5!(action_122)),
        204 => partial_5!(happyShift, curry_1_5!(action_37)),
        207 => partial_5!(happyShift, curry_1_5!(action_123)),
        208 => partial_5!(happyShift, curry_1_5!(action_124)),
        209 => partial_5!(happyShift, curry_1_5!(action_125)),
        210 => partial_5!(happyShift, curry_1_5!(action_126)),
        212 => partial_5!(happyShift, curry_1_5!(action_127)),
        215 => partial_5!(happyShift, curry_1_5!(action_128)),
        218 => partial_5!(happyShift, curry_1_5!(action_129)),
        219 => partial_5!(happyShift, curry_1_5!(action_130)),
        220 => partial_5!(happyShift, curry_1_5!(action_38)),
        221 => partial_5!(happyShift, curry_1_5!(action_131)),
        222 => partial_5!(happyShift, curry_1_5!(action_132)),
        223 => partial_5!(happyShift, curry_1_5!(action_133)),
        225 => partial_5!(happyShift, curry_1_5!(action_134)),
        226 => partial_5!(happyShift, curry_1_5!(action_135)),
        227 => partial_5!(happyShift, curry_1_5!(action_136)),
        228 => partial_5!(happyShift, curry_1_5!(action_137)),
        229 => partial_5!(happyShift, curry_1_5!(action_138)),
        230 => partial_5!(happyShift, curry_1_5!(action_139)),
        233 => partial_5!(happyShift, curry_1_5!(action_39)),
        234 => partial_5!(happyShift, curry_1_5!(action_40)),
        235 => partial_5!(happyShift, curry_1_5!(action_41)),
        236 => partial_5!(happyShift, curry_1_5!(action_42)),
        237 => partial_5!(happyShift, curry_1_5!(action_43)),
        238 => partial_5!(happyShift, curry_1_5!(action_141)),
        239 => partial_5!(happyShift, curry_1_5!(action_142)),
        240 => partial_5!(happyShift, curry_1_5!(action_616)),
        241 => partial_5!(happyShift, curry_1_5!(action_45)),
        242 => partial_5!(happyShift, curry_1_5!(action_46)),
        243 => partial_5!(happyShift, curry_1_5!(action_47)),
        244 => partial_5!(happyShift, curry_1_5!(action_48)),
        245 => partial_5!(happyShift, curry_1_5!(action_49)),
        19 => partial_5!(happyGoto, curry_1_5!(action_673)),
        20 => partial_5!(happyGoto, curry_1_5!(action_609)),
        32 => partial_5!(happyGoto, curry_1_5!(action_610)),
        34 => partial_5!(happyGoto, curry_1_5!(action_80)),
        36 => partial_5!(happyGoto, curry_1_5!(action_81)),
        37 => partial_5!(happyGoto, curry_1_5!(action_611)),
        38 => partial_5!(happyGoto, curry_1_5!(action_612)),
        40 => partial_5!(happyGoto, curry_1_5!(action_84)),
        41 => partial_5!(happyGoto, curry_1_5!(action_85)),
        42 => partial_5!(happyGoto, curry_1_5!(action_86)),
        43 => partial_5!(happyGoto, curry_1_5!(action_87)),
        44 => partial_5!(happyGoto, curry_1_5!(action_613)),
        45 => partial_5!(happyGoto, curry_1_5!(action_89)),
        46 => partial_5!(happyGoto, curry_1_5!(action_90)),
        47 => partial_5!(happyGoto, curry_1_5!(action_91)),
        48 => partial_5!(happyGoto, curry_1_5!(action_92)),
        49 => partial_5!(happyGoto, curry_1_5!(action_93)),
        50 => partial_5!(happyGoto, curry_1_5!(action_94)),
        51 => partial_5!(happyGoto, curry_1_5!(action_95)),
        52 => partial_5!(happyGoto, curry_1_5!(action_96)),
        53 => partial_5!(happyGoto, curry_1_5!(action_97)),
        54 => partial_5!(happyGoto, curry_1_5!(action_98)),
        61 => partial_5!(happyGoto, curry_1_5!(action_99)),
        65 => partial_5!(happyGoto, curry_1_5!(action_614)),
        100 => partial_5!(happyGoto, curry_1_5!(action_6)),
        104 => partial_5!(happyGoto, curry_1_5!(action_7)),
        106 => partial_5!(happyGoto, curry_1_5!(action_259)),
        107 => partial_5!(happyGoto, curry_1_5!(action_9)),
        108 => partial_5!(happyGoto, curry_1_5!(action_274)),
        127 => partial_5!(happyGoto, curry_1_5!(action_24)),
        128 => partial_5!(happyGoto, curry_1_5!(action_25)),
        132 => partial_5!(happyGoto, curry_1_5!(action_108)),
        133 => partial_5!(happyGoto, curry_1_5!(action_468)),
        134 => partial_5!(happyGoto, curry_1_5!(action_110)),
        _ => happyReduce_471()
    }
}

fn action_617(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_34()
    }
}

fn action_618(i: isize) -> ActionReturn {
    match i {
        190 => partial_5!(happyShift, curry_1_5!(action_116)),
        192 => partial_5!(happyShift, curry_1_5!(action_117)),
        195 => partial_5!(happyShift, curry_1_5!(action_118)),
        198 => partial_5!(happyShift, curry_1_5!(action_119)),
        200 => partial_5!(happyShift, curry_1_5!(action_120)),
        202 => partial_5!(happyShift, curry_1_5!(action_122)),
        208 => partial_5!(happyShift, curry_1_5!(action_124)),
        209 => partial_5!(happyShift, curry_1_5!(action_125)),
        210 => partial_5!(happyShift, curry_1_5!(action_126)),
        218 => partial_5!(happyShift, curry_1_5!(action_129)),
        219 => partial_5!(happyShift, curry_1_5!(action_130)),
        223 => partial_5!(happyShift, curry_1_5!(action_133)),
        226 => partial_5!(happyShift, curry_1_5!(action_135)),
        228 => partial_5!(happyShift, curry_1_5!(action_137)),
        229 => partial_5!(happyShift, curry_1_5!(action_138)),
        230 => partial_5!(happyShift, curry_1_5!(action_139)),
        238 => partial_5!(happyShift, curry_1_5!(action_141)),
        239 => partial_5!(happyShift, curry_1_5!(action_142)),
        44 => partial_5!(happyGoto, curry_1_5!(action_286)),
        45 => partial_5!(happyGoto, curry_1_5!(action_89)),
        47 => partial_5!(happyGoto, curry_1_5!(action_287)),
        49 => partial_5!(happyGoto, curry_1_5!(action_288)),
        51 => partial_5!(happyGoto, curry_1_5!(action_289)),
        52 => partial_5!(happyGoto, curry_1_5!(action_96)),
        53 => partial_5!(happyGoto, curry_1_5!(action_97)),
        54 => partial_5!(happyGoto, curry_1_5!(action_98)),
        61 => partial_5!(happyGoto, curry_1_5!(action_99)),
        65 => partial_5!(happyGoto, curry_1_5!(action_290)),
        86 => partial_5!(happyGoto, curry_1_5!(action_672)),
        132 => partial_5!(happyGoto, curry_1_5!(action_108)),
        133 => partial_5!(happyGoto, curry_1_5!(action_293)),
        134 => partial_5!(happyGoto, curry_1_5!(action_110)),
        _ => happyReduce_471()
    }
}

fn action_619(i: isize) -> ActionReturn {
    match i {
        237 => partial_5!(happyShift, curry_1_5!(action_254)),
        238 => partial_5!(happyShift, curry_1_5!(action_75)),
        103 => partial_5!(happyGoto, curry_1_5!(action_670)),
        131 => partial_5!(happyGoto, curry_1_5!(action_671)),
        _ => box happyFail
    }
}

fn action_620(i: isize) -> ActionReturn {
    match i {
        190 => partial_5!(happyShift, curry_1_5!(action_116)),
        192 => partial_5!(happyShift, curry_1_5!(action_117)),
        195 => partial_5!(happyShift, curry_1_5!(action_118)),
        198 => partial_5!(happyShift, curry_1_5!(action_119)),
        200 => partial_5!(happyShift, curry_1_5!(action_120)),
        202 => partial_5!(happyShift, curry_1_5!(action_122)),
        208 => partial_5!(happyShift, curry_1_5!(action_124)),
        209 => partial_5!(happyShift, curry_1_5!(action_125)),
        210 => partial_5!(happyShift, curry_1_5!(action_126)),
        218 => partial_5!(happyShift, curry_1_5!(action_129)),
        219 => partial_5!(happyShift, curry_1_5!(action_130)),
        223 => partial_5!(happyShift, curry_1_5!(action_133)),
        226 => partial_5!(happyShift, curry_1_5!(action_135)),
        228 => partial_5!(happyShift, curry_1_5!(action_137)),
        229 => partial_5!(happyShift, curry_1_5!(action_138)),
        230 => partial_5!(happyShift, curry_1_5!(action_139)),
        238 => partial_5!(happyShift, curry_1_5!(action_141)),
        239 => partial_5!(happyShift, curry_1_5!(action_142)),
        44 => partial_5!(happyGoto, curry_1_5!(action_286)),
        45 => partial_5!(happyGoto, curry_1_5!(action_89)),
        47 => partial_5!(happyGoto, curry_1_5!(action_287)),
        49 => partial_5!(happyGoto, curry_1_5!(action_288)),
        51 => partial_5!(happyGoto, curry_1_5!(action_289)),
        52 => partial_5!(happyGoto, curry_1_5!(action_96)),
        53 => partial_5!(happyGoto, curry_1_5!(action_97)),
        54 => partial_5!(happyGoto, curry_1_5!(action_98)),
        61 => partial_5!(happyGoto, curry_1_5!(action_99)),
        65 => partial_5!(happyGoto, curry_1_5!(action_290)),
        86 => partial_5!(happyGoto, curry_1_5!(action_669)),
        132 => partial_5!(happyGoto, curry_1_5!(action_108)),
        133 => partial_5!(happyGoto, curry_1_5!(action_293)),
        134 => partial_5!(happyGoto, curry_1_5!(action_110)),
        _ => happyReduce_471()
    }
}

fn action_621(i: isize) -> ActionReturn {
    match i {
        181 => partial_5!(happyShift, curry_1_5!(action_634)),
        _ => box happyFail
    }
}

fn action_622(i: isize) -> ActionReturn {
    match i {
        181 => partial_5!(happyShift, curry_1_5!(action_634)),
        _ => happyReduce_394()
    }
}

fn action_623(i: isize) -> ActionReturn {
    match i {
        190 => partial_5!(happyShift, curry_1_5!(action_116)),
        192 => partial_5!(happyShift, curry_1_5!(action_117)),
        195 => partial_5!(happyShift, curry_1_5!(action_118)),
        196 => partial_5!(happyShift, curry_1_5!(action_668)),
        198 => partial_5!(happyShift, curry_1_5!(action_119)),
        200 => partial_5!(happyShift, curry_1_5!(action_120)),
        202 => partial_5!(happyShift, curry_1_5!(action_122)),
        208 => partial_5!(happyShift, curry_1_5!(action_124)),
        209 => partial_5!(happyShift, curry_1_5!(action_125)),
        210 => partial_5!(happyShift, curry_1_5!(action_126)),
        218 => partial_5!(happyShift, curry_1_5!(action_129)),
        219 => partial_5!(happyShift, curry_1_5!(action_130)),
        223 => partial_5!(happyShift, curry_1_5!(action_133)),
        226 => partial_5!(happyShift, curry_1_5!(action_135)),
        228 => partial_5!(happyShift, curry_1_5!(action_137)),
        229 => partial_5!(happyShift, curry_1_5!(action_138)),
        230 => partial_5!(happyShift, curry_1_5!(action_139)),
        238 => partial_5!(happyShift, curry_1_5!(action_141)),
        239 => partial_5!(happyShift, curry_1_5!(action_142)),
        44 => partial_5!(happyGoto, curry_1_5!(action_286)),
        45 => partial_5!(happyGoto, curry_1_5!(action_89)),
        47 => partial_5!(happyGoto, curry_1_5!(action_287)),
        49 => partial_5!(happyGoto, curry_1_5!(action_288)),
        51 => partial_5!(happyGoto, curry_1_5!(action_289)),
        52 => partial_5!(happyGoto, curry_1_5!(action_96)),
        53 => partial_5!(happyGoto, curry_1_5!(action_97)),
        54 => partial_5!(happyGoto, curry_1_5!(action_98)),
        61 => partial_5!(happyGoto, curry_1_5!(action_99)),
        65 => partial_5!(happyGoto, curry_1_5!(action_290)),
        86 => partial_5!(happyGoto, curry_1_5!(action_665)),
        101 => partial_5!(happyGoto, curry_1_5!(action_666)),
        102 => partial_5!(happyGoto, curry_1_5!(action_667)),
        132 => partial_5!(happyGoto, curry_1_5!(action_108)),
        133 => partial_5!(happyGoto, curry_1_5!(action_293)),
        134 => partial_5!(happyGoto, curry_1_5!(action_110)),
        _ => happyReduce_471()
    }
}

fn action_624(i: isize) -> ActionReturn {
    match i {
        181 => partial_5!(happyShift, curry_1_5!(action_634)),
        _ => happyReduce_396()
    }
}

fn action_625(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_372)),
        140 => partial_5!(happyShift, curry_1_5!(action_182)),
        150 => partial_5!(happyShift, curry_1_5!(action_373)),
        186 => partial_5!(happyShift, curry_1_5!(action_171)),
        193 => partial_5!(happyShift, curry_1_5!(action_172)),
        213 => partial_5!(happyShift, curry_1_5!(action_173)),
        214 => partial_5!(happyShift, curry_1_5!(action_174)),
        216 => partial_5!(happyShift, curry_1_5!(action_175)),
        231 => partial_5!(happyShift, curry_1_5!(action_176)),
        239 => partial_5!(happyShift, curry_1_5!(action_142)),
        64 => partial_5!(happyGoto, curry_1_5!(action_187)),
        87 => partial_5!(happyGoto, curry_1_5!(action_662)),
        88 => partial_5!(happyGoto, curry_1_5!(action_367)),
        89 => partial_5!(happyGoto, curry_1_5!(action_179)),
        90 => partial_5!(happyGoto, curry_1_5!(action_180)),
        91 => partial_5!(happyGoto, curry_1_5!(action_368)),
        92 => partial_5!(happyGoto, curry_1_5!(action_369)),
        132 => partial_5!(happyGoto, curry_1_5!(action_663)),
        133 => partial_5!(happyGoto, curry_1_5!(action_664)),
        134 => partial_5!(happyGoto, curry_1_5!(action_110)),
        _ => happyReduce_471()
    }
}

fn action_626(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_328()
    }
}

fn action_627(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_372)),
        140 => partial_5!(happyShift, curry_1_5!(action_182)),
        150 => partial_5!(happyShift, curry_1_5!(action_373)),
        186 => happyReduce_472(),
        193 => happyReduce_472(),
        213 => happyReduce_472(),
        214 => happyReduce_472(),
        216 => happyReduce_472(),
        231 => happyReduce_472(),
        239 => partial_5!(happyShift, curry_1_5!(action_142)),
        87 => partial_5!(happyGoto, curry_1_5!(action_661)),
        88 => partial_5!(happyGoto, curry_1_5!(action_367)),
        89 => partial_5!(happyGoto, curry_1_5!(action_179)),
        90 => partial_5!(happyGoto, curry_1_5!(action_180)),
        91 => partial_5!(happyGoto, curry_1_5!(action_368)),
        92 => partial_5!(happyGoto, curry_1_5!(action_369)),
        134 => partial_5!(happyGoto, curry_1_5!(action_167)),
        _ => happyReduce_330()
    }
}

fn action_628(i: isize) -> ActionReturn {
    match i {
        139 => partial_5!(happyShift, curry_1_5!(action_660)),
        _ => box happyFail
    }
}

fn action_629(i: isize) -> ActionReturn {
    match i {
        139 => partial_5!(happyShift, curry_1_5!(action_659)),
        _ => box happyFail
    }
}

fn action_630(i: isize) -> ActionReturn {
    match i {
        139 => partial_5!(happyShift, curry_1_5!(action_658)),
        239 => partial_5!(happyShift, curry_1_5!(action_142)),
        134 => partial_5!(happyGoto, curry_1_5!(action_632)),
        _ => box happyFail
    }
}

fn action_631(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_372)),
        140 => partial_5!(happyShift, curry_1_5!(action_182)),
        150 => partial_5!(happyShift, curry_1_5!(action_373)),
        185 => partial_5!(happyShift, curry_1_5!(action_113)),
        188 => partial_5!(happyShift, curry_1_5!(action_115)),
        190 => partial_5!(happyShift, curry_1_5!(action_116)),
        192 => partial_5!(happyShift, curry_1_5!(action_117)),
        195 => partial_5!(happyShift, curry_1_5!(action_118)),
        198 => partial_5!(happyShift, curry_1_5!(action_119)),
        200 => partial_5!(happyShift, curry_1_5!(action_120)),
        201 => partial_5!(happyShift, curry_1_5!(action_121)),
        202 => partial_5!(happyShift, curry_1_5!(action_122)),
        207 => partial_5!(happyShift, curry_1_5!(action_123)),
        208 => partial_5!(happyShift, curry_1_5!(action_124)),
        209 => partial_5!(happyShift, curry_1_5!(action_125)),
        210 => partial_5!(happyShift, curry_1_5!(action_126)),
        212 => partial_5!(happyShift, curry_1_5!(action_127)),
        215 => partial_5!(happyShift, curry_1_5!(action_128)),
        218 => partial_5!(happyShift, curry_1_5!(action_129)),
        219 => partial_5!(happyShift, curry_1_5!(action_130)),
        221 => partial_5!(happyShift, curry_1_5!(action_131)),
        223 => partial_5!(happyShift, curry_1_5!(action_133)),
        225 => partial_5!(happyShift, curry_1_5!(action_134)),
        226 => partial_5!(happyShift, curry_1_5!(action_168)),
        227 => partial_5!(happyShift, curry_1_5!(action_136)),
        228 => partial_5!(happyShift, curry_1_5!(action_137)),
        229 => partial_5!(happyShift, curry_1_5!(action_138)),
        230 => partial_5!(happyShift, curry_1_5!(action_139)),
        238 => partial_5!(happyShift, curry_1_5!(action_169)),
        239 => partial_5!(happyShift, curry_1_5!(action_142)),
        40 => partial_5!(happyGoto, curry_1_5!(action_162)),
        41 => partial_5!(happyGoto, curry_1_5!(action_85)),
        42 => partial_5!(happyGoto, curry_1_5!(action_86)),
        43 => partial_5!(happyGoto, curry_1_5!(action_87)),
        45 => partial_5!(happyGoto, curry_1_5!(action_163)),
        52 => partial_5!(happyGoto, curry_1_5!(action_164)),
        53 => partial_5!(happyGoto, curry_1_5!(action_97)),
        54 => partial_5!(happyGoto, curry_1_5!(action_98)),
        61 => partial_5!(happyGoto, curry_1_5!(action_99)),
        88 => partial_5!(happyGoto, curry_1_5!(action_655)),
        89 => partial_5!(happyGoto, curry_1_5!(action_179)),
        90 => partial_5!(happyGoto, curry_1_5!(action_180)),
        91 => partial_5!(happyGoto, curry_1_5!(action_656)),
        92 => partial_5!(happyGoto, curry_1_5!(action_657)),
        134 => partial_5!(happyGoto, curry_1_5!(action_167)),
        _ => happyReduce_472()
    }
}

fn action_632(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_340()
    }
}

fn action_633(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_407()
    }
}

fn action_634(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_26)),
        140 => partial_5!(happyShift, curry_1_5!(action_652)),
        143 => partial_5!(happyShift, curry_1_5!(action_653)),
        144 => partial_5!(happyShift, curry_1_5!(action_27)),
        145 => partial_5!(happyShift, curry_1_5!(action_28)),
        146 => partial_5!(happyShift, curry_1_5!(action_29)),
        147 => partial_5!(happyShift, curry_1_5!(action_30)),
        148 => partial_5!(happyShift, curry_1_5!(action_31)),
        149 => partial_5!(happyShift, curry_1_5!(action_32)),
        150 => partial_5!(happyShift, curry_1_5!(action_33)),
        153 => partial_5!(happyShift, curry_1_5!(action_34)),
        164 => partial_5!(happyShift, curry_1_5!(action_35)),
        181 => partial_5!(happyShift, curry_1_5!(action_654)),
        184 => partial_5!(happyShift, curry_1_5!(action_36)),
        204 => partial_5!(happyShift, curry_1_5!(action_37)),
        220 => partial_5!(happyShift, curry_1_5!(action_38)),
        233 => partial_5!(happyShift, curry_1_5!(action_39)),
        234 => partial_5!(happyShift, curry_1_5!(action_40)),
        235 => partial_5!(happyShift, curry_1_5!(action_41)),
        236 => partial_5!(happyShift, curry_1_5!(action_42)),
        237 => partial_5!(happyShift, curry_1_5!(action_74)),
        238 => partial_5!(happyShift, curry_1_5!(action_75)),
        240 => partial_5!(happyShift, curry_1_5!(action_44)),
        241 => partial_5!(happyShift, curry_1_5!(action_45)),
        242 => partial_5!(happyShift, curry_1_5!(action_46)),
        243 => partial_5!(happyShift, curry_1_5!(action_47)),
        244 => partial_5!(happyShift, curry_1_5!(action_48)),
        245 => partial_5!(happyShift, curry_1_5!(action_49)),
        93 => partial_5!(happyGoto, curry_1_5!(action_644)),
        95 => partial_5!(happyGoto, curry_1_5!(action_645)),
        96 => partial_5!(happyGoto, curry_1_5!(action_646)),
        97 => partial_5!(happyGoto, curry_1_5!(action_647)),
        98 => partial_5!(happyGoto, curry_1_5!(action_648)),
        99 => partial_5!(happyGoto, curry_1_5!(action_649)),
        100 => partial_5!(happyGoto, curry_1_5!(action_6)),
        104 => partial_5!(happyGoto, curry_1_5!(action_7)),
        106 => partial_5!(happyGoto, curry_1_5!(action_8)),
        107 => partial_5!(happyGoto, curry_1_5!(action_9)),
        108 => partial_5!(happyGoto, curry_1_5!(action_10)),
        109 => partial_5!(happyGoto, curry_1_5!(action_11)),
        110 => partial_5!(happyGoto, curry_1_5!(action_12)),
        111 => partial_5!(happyGoto, curry_1_5!(action_13)),
        112 => partial_5!(happyGoto, curry_1_5!(action_14)),
        113 => partial_5!(happyGoto, curry_1_5!(action_15)),
        114 => partial_5!(happyGoto, curry_1_5!(action_16)),
        115 => partial_5!(happyGoto, curry_1_5!(action_17)),
        116 => partial_5!(happyGoto, curry_1_5!(action_18)),
        117 => partial_5!(happyGoto, curry_1_5!(action_19)),
        118 => partial_5!(happyGoto, curry_1_5!(action_20)),
        119 => partial_5!(happyGoto, curry_1_5!(action_21)),
        120 => partial_5!(happyGoto, curry_1_5!(action_650)),
        127 => partial_5!(happyGoto, curry_1_5!(action_24)),
        128 => partial_5!(happyGoto, curry_1_5!(action_25)),
        131 => partial_5!(happyGoto, curry_1_5!(action_651)),
        _ => happyReduce_346()
    }
}

fn action_635(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_26)),
        144 => partial_5!(happyShift, curry_1_5!(action_27)),
        145 => partial_5!(happyShift, curry_1_5!(action_28)),
        146 => partial_5!(happyShift, curry_1_5!(action_29)),
        147 => partial_5!(happyShift, curry_1_5!(action_30)),
        148 => partial_5!(happyShift, curry_1_5!(action_31)),
        149 => partial_5!(happyShift, curry_1_5!(action_32)),
        150 => partial_5!(happyShift, curry_1_5!(action_33)),
        153 => partial_5!(happyShift, curry_1_5!(action_34)),
        164 => partial_5!(happyShift, curry_1_5!(action_35)),
        184 => partial_5!(happyShift, curry_1_5!(action_36)),
        204 => partial_5!(happyShift, curry_1_5!(action_37)),
        220 => partial_5!(happyShift, curry_1_5!(action_38)),
        233 => partial_5!(happyShift, curry_1_5!(action_39)),
        234 => partial_5!(happyShift, curry_1_5!(action_40)),
        235 => partial_5!(happyShift, curry_1_5!(action_41)),
        236 => partial_5!(happyShift, curry_1_5!(action_42)),
        237 => partial_5!(happyShift, curry_1_5!(action_43)),
        240 => partial_5!(happyShift, curry_1_5!(action_44)),
        241 => partial_5!(happyShift, curry_1_5!(action_45)),
        242 => partial_5!(happyShift, curry_1_5!(action_46)),
        243 => partial_5!(happyShift, curry_1_5!(action_47)),
        244 => partial_5!(happyShift, curry_1_5!(action_48)),
        245 => partial_5!(happyShift, curry_1_5!(action_49)),
        100 => partial_5!(happyGoto, curry_1_5!(action_6)),
        104 => partial_5!(happyGoto, curry_1_5!(action_7)),
        106 => partial_5!(happyGoto, curry_1_5!(action_8)),
        107 => partial_5!(happyGoto, curry_1_5!(action_9)),
        108 => partial_5!(happyGoto, curry_1_5!(action_10)),
        109 => partial_5!(happyGoto, curry_1_5!(action_11)),
        110 => partial_5!(happyGoto, curry_1_5!(action_12)),
        111 => partial_5!(happyGoto, curry_1_5!(action_13)),
        112 => partial_5!(happyGoto, curry_1_5!(action_14)),
        113 => partial_5!(happyGoto, curry_1_5!(action_15)),
        114 => partial_5!(happyGoto, curry_1_5!(action_16)),
        115 => partial_5!(happyGoto, curry_1_5!(action_17)),
        116 => partial_5!(happyGoto, curry_1_5!(action_18)),
        117 => partial_5!(happyGoto, curry_1_5!(action_19)),
        118 => partial_5!(happyGoto, curry_1_5!(action_20)),
        119 => partial_5!(happyGoto, curry_1_5!(action_21)),
        120 => partial_5!(happyGoto, curry_1_5!(action_643)),
        127 => partial_5!(happyGoto, curry_1_5!(action_24)),
        128 => partial_5!(happyGoto, curry_1_5!(action_25)),
        _ => box happyFail
    }
}

fn action_636(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_438()
    }
}

fn action_637(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_26)),
        144 => partial_5!(happyShift, curry_1_5!(action_27)),
        145 => partial_5!(happyShift, curry_1_5!(action_28)),
        146 => partial_5!(happyShift, curry_1_5!(action_29)),
        147 => partial_5!(happyShift, curry_1_5!(action_30)),
        148 => partial_5!(happyShift, curry_1_5!(action_31)),
        149 => partial_5!(happyShift, curry_1_5!(action_32)),
        150 => partial_5!(happyShift, curry_1_5!(action_33)),
        153 => partial_5!(happyShift, curry_1_5!(action_34)),
        164 => partial_5!(happyShift, curry_1_5!(action_35)),
        184 => partial_5!(happyShift, curry_1_5!(action_36)),
        204 => partial_5!(happyShift, curry_1_5!(action_37)),
        220 => partial_5!(happyShift, curry_1_5!(action_38)),
        233 => partial_5!(happyShift, curry_1_5!(action_39)),
        234 => partial_5!(happyShift, curry_1_5!(action_40)),
        235 => partial_5!(happyShift, curry_1_5!(action_41)),
        236 => partial_5!(happyShift, curry_1_5!(action_42)),
        237 => partial_5!(happyShift, curry_1_5!(action_43)),
        240 => partial_5!(happyShift, curry_1_5!(action_44)),
        241 => partial_5!(happyShift, curry_1_5!(action_45)),
        242 => partial_5!(happyShift, curry_1_5!(action_46)),
        243 => partial_5!(happyShift, curry_1_5!(action_47)),
        244 => partial_5!(happyShift, curry_1_5!(action_48)),
        245 => partial_5!(happyShift, curry_1_5!(action_49)),
        100 => partial_5!(happyGoto, curry_1_5!(action_6)),
        104 => partial_5!(happyGoto, curry_1_5!(action_7)),
        106 => partial_5!(happyGoto, curry_1_5!(action_259)),
        107 => partial_5!(happyGoto, curry_1_5!(action_9)),
        108 => partial_5!(happyGoto, curry_1_5!(action_10)),
        109 => partial_5!(happyGoto, curry_1_5!(action_11)),
        110 => partial_5!(happyGoto, curry_1_5!(action_12)),
        111 => partial_5!(happyGoto, curry_1_5!(action_13)),
        112 => partial_5!(happyGoto, curry_1_5!(action_14)),
        113 => partial_5!(happyGoto, curry_1_5!(action_15)),
        114 => partial_5!(happyGoto, curry_1_5!(action_16)),
        115 => partial_5!(happyGoto, curry_1_5!(action_17)),
        116 => partial_5!(happyGoto, curry_1_5!(action_18)),
        117 => partial_5!(happyGoto, curry_1_5!(action_19)),
        118 => partial_5!(happyGoto, curry_1_5!(action_20)),
        119 => partial_5!(happyGoto, curry_1_5!(action_642)),
        127 => partial_5!(happyGoto, curry_1_5!(action_24)),
        128 => partial_5!(happyGoto, curry_1_5!(action_25)),
        _ => box happyFail
    }
}

fn action_638(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_379()
    }
}

fn action_639(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_26)),
        144 => partial_5!(happyShift, curry_1_5!(action_27)),
        145 => partial_5!(happyShift, curry_1_5!(action_28)),
        146 => partial_5!(happyShift, curry_1_5!(action_29)),
        147 => partial_5!(happyShift, curry_1_5!(action_30)),
        148 => partial_5!(happyShift, curry_1_5!(action_31)),
        149 => partial_5!(happyShift, curry_1_5!(action_32)),
        150 => partial_5!(happyShift, curry_1_5!(action_33)),
        153 => partial_5!(happyShift, curry_1_5!(action_34)),
        164 => partial_5!(happyShift, curry_1_5!(action_35)),
        184 => partial_5!(happyShift, curry_1_5!(action_36)),
        204 => partial_5!(happyShift, curry_1_5!(action_37)),
        220 => partial_5!(happyShift, curry_1_5!(action_38)),
        233 => partial_5!(happyShift, curry_1_5!(action_39)),
        234 => partial_5!(happyShift, curry_1_5!(action_40)),
        235 => partial_5!(happyShift, curry_1_5!(action_41)),
        236 => partial_5!(happyShift, curry_1_5!(action_42)),
        237 => partial_5!(happyShift, curry_1_5!(action_43)),
        240 => partial_5!(happyShift, curry_1_5!(action_44)),
        241 => partial_5!(happyShift, curry_1_5!(action_45)),
        242 => partial_5!(happyShift, curry_1_5!(action_46)),
        243 => partial_5!(happyShift, curry_1_5!(action_47)),
        244 => partial_5!(happyShift, curry_1_5!(action_48)),
        245 => partial_5!(happyShift, curry_1_5!(action_49)),
        100 => partial_5!(happyGoto, curry_1_5!(action_6)),
        104 => partial_5!(happyGoto, curry_1_5!(action_7)),
        106 => partial_5!(happyGoto, curry_1_5!(action_8)),
        107 => partial_5!(happyGoto, curry_1_5!(action_9)),
        108 => partial_5!(happyGoto, curry_1_5!(action_10)),
        109 => partial_5!(happyGoto, curry_1_5!(action_11)),
        110 => partial_5!(happyGoto, curry_1_5!(action_12)),
        111 => partial_5!(happyGoto, curry_1_5!(action_13)),
        112 => partial_5!(happyGoto, curry_1_5!(action_14)),
        113 => partial_5!(happyGoto, curry_1_5!(action_15)),
        114 => partial_5!(happyGoto, curry_1_5!(action_16)),
        115 => partial_5!(happyGoto, curry_1_5!(action_17)),
        116 => partial_5!(happyGoto, curry_1_5!(action_18)),
        117 => partial_5!(happyGoto, curry_1_5!(action_19)),
        118 => partial_5!(happyGoto, curry_1_5!(action_20)),
        119 => partial_5!(happyGoto, curry_1_5!(action_21)),
        120 => partial_5!(happyGoto, curry_1_5!(action_641)),
        127 => partial_5!(happyGoto, curry_1_5!(action_24)),
        128 => partial_5!(happyGoto, curry_1_5!(action_25)),
        _ => box happyFail
    }
}

fn action_640(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_377()
    }
}

fn action_641(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_387()
    }
}

fn action_642(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_437()
    }
}

fn action_643(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_455()
    }
}

fn action_644(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_347()
    }
}

fn action_645(i: isize) -> ActionReturn {
    match i {
        179 => partial_5!(happyShift, curry_1_5!(action_852)),
        182 => partial_5!(happyShift, curry_1_5!(action_853)),
        _ => box happyFail
    }
}

fn action_646(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_26)),
        144 => partial_5!(happyShift, curry_1_5!(action_27)),
        145 => partial_5!(happyShift, curry_1_5!(action_28)),
        146 => partial_5!(happyShift, curry_1_5!(action_29)),
        147 => partial_5!(happyShift, curry_1_5!(action_30)),
        148 => partial_5!(happyShift, curry_1_5!(action_31)),
        149 => partial_5!(happyShift, curry_1_5!(action_32)),
        150 => partial_5!(happyShift, curry_1_5!(action_33)),
        153 => partial_5!(happyShift, curry_1_5!(action_34)),
        164 => partial_5!(happyShift, curry_1_5!(action_35)),
        181 => partial_5!(happyShift, curry_1_5!(action_654)),
        184 => partial_5!(happyShift, curry_1_5!(action_36)),
        204 => partial_5!(happyShift, curry_1_5!(action_37)),
        220 => partial_5!(happyShift, curry_1_5!(action_38)),
        233 => partial_5!(happyShift, curry_1_5!(action_39)),
        234 => partial_5!(happyShift, curry_1_5!(action_40)),
        235 => partial_5!(happyShift, curry_1_5!(action_41)),
        236 => partial_5!(happyShift, curry_1_5!(action_42)),
        237 => partial_5!(happyShift, curry_1_5!(action_43)),
        240 => partial_5!(happyShift, curry_1_5!(action_44)),
        241 => partial_5!(happyShift, curry_1_5!(action_45)),
        242 => partial_5!(happyShift, curry_1_5!(action_46)),
        243 => partial_5!(happyShift, curry_1_5!(action_47)),
        244 => partial_5!(happyShift, curry_1_5!(action_48)),
        245 => partial_5!(happyShift, curry_1_5!(action_49)),
        93 => partial_5!(happyGoto, curry_1_5!(action_851)),
        100 => partial_5!(happyGoto, curry_1_5!(action_6)),
        104 => partial_5!(happyGoto, curry_1_5!(action_7)),
        106 => partial_5!(happyGoto, curry_1_5!(action_8)),
        107 => partial_5!(happyGoto, curry_1_5!(action_9)),
        108 => partial_5!(happyGoto, curry_1_5!(action_10)),
        109 => partial_5!(happyGoto, curry_1_5!(action_11)),
        110 => partial_5!(happyGoto, curry_1_5!(action_12)),
        111 => partial_5!(happyGoto, curry_1_5!(action_13)),
        112 => partial_5!(happyGoto, curry_1_5!(action_14)),
        113 => partial_5!(happyGoto, curry_1_5!(action_15)),
        114 => partial_5!(happyGoto, curry_1_5!(action_16)),
        115 => partial_5!(happyGoto, curry_1_5!(action_17)),
        116 => partial_5!(happyGoto, curry_1_5!(action_18)),
        117 => partial_5!(happyGoto, curry_1_5!(action_19)),
        118 => partial_5!(happyGoto, curry_1_5!(action_20)),
        119 => partial_5!(happyGoto, curry_1_5!(action_21)),
        120 => partial_5!(happyGoto, curry_1_5!(action_650)),
        127 => partial_5!(happyGoto, curry_1_5!(action_24)),
        128 => partial_5!(happyGoto, curry_1_5!(action_25)),
        _ => box happyFail
    }
}

fn action_647(i: isize) -> ActionReturn {
    match i {
        140 => partial_5!(happyShift, curry_1_5!(action_652)),
        143 => partial_5!(happyShift, curry_1_5!(action_653)),
        168 => partial_5!(happyShift, curry_1_5!(action_850)),
        98 => partial_5!(happyGoto, curry_1_5!(action_848)),
        99 => partial_5!(happyGoto, curry_1_5!(action_849)),
        _ => box happyFail
    }
}

fn action_648(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_354()
    }
}

fn action_649(i: isize) -> ActionReturn {
    match i {
        140 => happyReduce_358(),
        143 => happyReduce_358(),
        168 => happyReduce_358(),
        _ => happyReduce_353()
    }
}

fn action_650(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_341()
    }
}

fn action_651(i: isize) -> ActionReturn {
    match i {
        167 => partial_5!(happyShift, curry_1_5!(action_847)),
        _ => box happyFail
    }
}

fn action_652(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_26)),
        144 => partial_5!(happyShift, curry_1_5!(action_27)),
        145 => partial_5!(happyShift, curry_1_5!(action_28)),
        146 => partial_5!(happyShift, curry_1_5!(action_29)),
        147 => partial_5!(happyShift, curry_1_5!(action_30)),
        148 => partial_5!(happyShift, curry_1_5!(action_31)),
        149 => partial_5!(happyShift, curry_1_5!(action_32)),
        150 => partial_5!(happyShift, curry_1_5!(action_33)),
        153 => partial_5!(happyShift, curry_1_5!(action_34)),
        164 => partial_5!(happyShift, curry_1_5!(action_35)),
        184 => partial_5!(happyShift, curry_1_5!(action_36)),
        204 => partial_5!(happyShift, curry_1_5!(action_37)),
        220 => partial_5!(happyShift, curry_1_5!(action_38)),
        233 => partial_5!(happyShift, curry_1_5!(action_39)),
        234 => partial_5!(happyShift, curry_1_5!(action_40)),
        235 => partial_5!(happyShift, curry_1_5!(action_41)),
        236 => partial_5!(happyShift, curry_1_5!(action_42)),
        237 => partial_5!(happyShift, curry_1_5!(action_43)),
        240 => partial_5!(happyShift, curry_1_5!(action_44)),
        241 => partial_5!(happyShift, curry_1_5!(action_45)),
        242 => partial_5!(happyShift, curry_1_5!(action_46)),
        243 => partial_5!(happyShift, curry_1_5!(action_47)),
        244 => partial_5!(happyShift, curry_1_5!(action_48)),
        245 => partial_5!(happyShift, curry_1_5!(action_49)),
        100 => partial_5!(happyGoto, curry_1_5!(action_6)),
        104 => partial_5!(happyGoto, curry_1_5!(action_7)),
        106 => partial_5!(happyGoto, curry_1_5!(action_259)),
        107 => partial_5!(happyGoto, curry_1_5!(action_9)),
        108 => partial_5!(happyGoto, curry_1_5!(action_10)),
        109 => partial_5!(happyGoto, curry_1_5!(action_11)),
        110 => partial_5!(happyGoto, curry_1_5!(action_12)),
        111 => partial_5!(happyGoto, curry_1_5!(action_13)),
        112 => partial_5!(happyGoto, curry_1_5!(action_14)),
        113 => partial_5!(happyGoto, curry_1_5!(action_15)),
        114 => partial_5!(happyGoto, curry_1_5!(action_16)),
        115 => partial_5!(happyGoto, curry_1_5!(action_17)),
        116 => partial_5!(happyGoto, curry_1_5!(action_18)),
        117 => partial_5!(happyGoto, curry_1_5!(action_19)),
        118 => partial_5!(happyGoto, curry_1_5!(action_20)),
        119 => partial_5!(happyGoto, curry_1_5!(action_260)),
        126 => partial_5!(happyGoto, curry_1_5!(action_846)),
        127 => partial_5!(happyGoto, curry_1_5!(action_24)),
        128 => partial_5!(happyGoto, curry_1_5!(action_25)),
        _ => box happyFail
    }
}

fn action_653(i: isize) -> ActionReturn {
    match i {
        237 => partial_5!(happyShift, curry_1_5!(action_254)),
        238 => partial_5!(happyShift, curry_1_5!(action_75)),
        131 => partial_5!(happyGoto, curry_1_5!(action_845)),
        _ => box happyFail
    }
}

fn action_654(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_26)),
        140 => partial_5!(happyShift, curry_1_5!(action_652)),
        143 => partial_5!(happyShift, curry_1_5!(action_653)),
        144 => partial_5!(happyShift, curry_1_5!(action_27)),
        145 => partial_5!(happyShift, curry_1_5!(action_28)),
        146 => partial_5!(happyShift, curry_1_5!(action_29)),
        147 => partial_5!(happyShift, curry_1_5!(action_30)),
        148 => partial_5!(happyShift, curry_1_5!(action_31)),
        149 => partial_5!(happyShift, curry_1_5!(action_32)),
        150 => partial_5!(happyShift, curry_1_5!(action_33)),
        153 => partial_5!(happyShift, curry_1_5!(action_34)),
        164 => partial_5!(happyShift, curry_1_5!(action_35)),
        181 => partial_5!(happyShift, curry_1_5!(action_654)),
        184 => partial_5!(happyShift, curry_1_5!(action_36)),
        204 => partial_5!(happyShift, curry_1_5!(action_37)),
        220 => partial_5!(happyShift, curry_1_5!(action_38)),
        233 => partial_5!(happyShift, curry_1_5!(action_39)),
        234 => partial_5!(happyShift, curry_1_5!(action_40)),
        235 => partial_5!(happyShift, curry_1_5!(action_41)),
        236 => partial_5!(happyShift, curry_1_5!(action_42)),
        237 => partial_5!(happyShift, curry_1_5!(action_74)),
        238 => partial_5!(happyShift, curry_1_5!(action_75)),
        240 => partial_5!(happyShift, curry_1_5!(action_44)),
        241 => partial_5!(happyShift, curry_1_5!(action_45)),
        242 => partial_5!(happyShift, curry_1_5!(action_46)),
        243 => partial_5!(happyShift, curry_1_5!(action_47)),
        244 => partial_5!(happyShift, curry_1_5!(action_48)),
        245 => partial_5!(happyShift, curry_1_5!(action_49)),
        93 => partial_5!(happyGoto, curry_1_5!(action_644)),
        95 => partial_5!(happyGoto, curry_1_5!(action_844)),
        96 => partial_5!(happyGoto, curry_1_5!(action_646)),
        97 => partial_5!(happyGoto, curry_1_5!(action_647)),
        98 => partial_5!(happyGoto, curry_1_5!(action_648)),
        99 => partial_5!(happyGoto, curry_1_5!(action_649)),
        100 => partial_5!(happyGoto, curry_1_5!(action_6)),
        104 => partial_5!(happyGoto, curry_1_5!(action_7)),
        106 => partial_5!(happyGoto, curry_1_5!(action_8)),
        107 => partial_5!(happyGoto, curry_1_5!(action_9)),
        108 => partial_5!(happyGoto, curry_1_5!(action_10)),
        109 => partial_5!(happyGoto, curry_1_5!(action_11)),
        110 => partial_5!(happyGoto, curry_1_5!(action_12)),
        111 => partial_5!(happyGoto, curry_1_5!(action_13)),
        112 => partial_5!(happyGoto, curry_1_5!(action_14)),
        113 => partial_5!(happyGoto, curry_1_5!(action_15)),
        114 => partial_5!(happyGoto, curry_1_5!(action_16)),
        115 => partial_5!(happyGoto, curry_1_5!(action_17)),
        116 => partial_5!(happyGoto, curry_1_5!(action_18)),
        117 => partial_5!(happyGoto, curry_1_5!(action_19)),
        118 => partial_5!(happyGoto, curry_1_5!(action_20)),
        119 => partial_5!(happyGoto, curry_1_5!(action_21)),
        120 => partial_5!(happyGoto, curry_1_5!(action_650)),
        127 => partial_5!(happyGoto, curry_1_5!(action_24)),
        128 => partial_5!(happyGoto, curry_1_5!(action_25)),
        131 => partial_5!(happyGoto, curry_1_5!(action_651)),
        _ => happyReduce_346()
    }
}

fn action_655(i: isize) -> ActionReturn {
    match i {
        139 => partial_5!(happyShift, curry_1_5!(action_843)),
        _ => box happyFail
    }
}

fn action_656(i: isize) -> ActionReturn {
    match i {
        139 => partial_5!(happyShift, curry_1_5!(action_842)),
        _ => box happyFail
    }
}

fn action_657(i: isize) -> ActionReturn {
    match i {
        139 => partial_5!(happyShift, curry_1_5!(action_841)),
        239 => partial_5!(happyShift, curry_1_5!(action_142)),
        134 => partial_5!(happyGoto, curry_1_5!(action_632)),
        _ => box happyFail
    }
}

fn action_658(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_333()
    }
}

fn action_659(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_413)),
        140 => partial_5!(happyShift, curry_1_5!(action_182)),
        88 => partial_5!(happyGoto, curry_1_5!(action_840)),
        89 => partial_5!(happyGoto, curry_1_5!(action_179)),
        90 => partial_5!(happyGoto, curry_1_5!(action_180)),
        _ => happyReduce_332()
    }
}

fn action_660(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_334()
    }
}

fn action_661(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_331()
    }
}

fn action_662(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_329()
    }
}

fn action_663(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_327()
    }
}

fn action_664(i: isize) -> ActionReturn {
    match i {
        186 => partial_5!(happyShift, curry_1_5!(action_171)),
        193 => partial_5!(happyShift, curry_1_5!(action_172)),
        213 => partial_5!(happyShift, curry_1_5!(action_173)),
        214 => partial_5!(happyShift, curry_1_5!(action_174)),
        216 => partial_5!(happyShift, curry_1_5!(action_175)),
        231 => partial_5!(happyShift, curry_1_5!(action_176)),
        239 => partial_5!(happyShift, curry_1_5!(action_142)),
        64 => partial_5!(happyGoto, curry_1_5!(action_435)),
        134 => partial_5!(happyGoto, curry_1_5!(action_167)),
        _ => happyReduce_472()
    }
}

fn action_665(i: isize) -> ActionReturn {
    match i {
        167 => partial_5!(happyShift, curry_1_5!(action_839)),
        _ => box happyFail
    }
}

fn action_666(i: isize) -> ActionReturn {
    match i {
        139 => partial_5!(happyShift, curry_1_5!(action_837)),
        179 => partial_5!(happyShift, curry_1_5!(action_838)),
        _ => box happyFail
    }
}

fn action_667(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_370()
    }
}

fn action_668(i: isize) -> ActionReturn {
    match i {
        167 => partial_5!(happyShift, curry_1_5!(action_836)),
        _ => box happyFail
    }
}

fn action_669(i: isize) -> ActionReturn {
    match i {
        139 => partial_5!(happyShift, curry_1_5!(action_835)),
        _ => box happyFail
    }
}

fn action_670(i: isize) -> ActionReturn {
    match i {
        139 => partial_5!(happyShift, curry_1_5!(action_832)),
        140 => partial_5!(happyShift, curry_1_5!(action_833)),
        143 => partial_5!(happyShift, curry_1_5!(action_834)),
        _ => box happyFail
    }
}

fn action_671(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_373()
    }
}

fn action_672(i: isize) -> ActionReturn {
    match i {
        139 => partial_5!(happyShift, curry_1_5!(action_831)),
        _ => box happyFail
    }
}

fn action_673(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_48()
    }
}

fn action_674(i: isize) -> ActionReturn {
    match i {
        181 => partial_5!(happyShift, curry_1_5!(action_61)),
        14 => partial_5!(happyGoto, curry_1_5!(action_830)),
        _ => box happyFail
    }
}

fn action_675(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_475)),
        150 => partial_5!(happyShift, curry_1_5!(action_476)),
        185 => partial_5!(happyShift, curry_1_5!(action_113)),
        186 => partial_5!(happyShift, curry_1_5!(action_171)),
        188 => partial_5!(happyShift, curry_1_5!(action_115)),
        190 => partial_5!(happyShift, curry_1_5!(action_116)),
        192 => partial_5!(happyShift, curry_1_5!(action_117)),
        193 => partial_5!(happyShift, curry_1_5!(action_172)),
        195 => partial_5!(happyShift, curry_1_5!(action_118)),
        198 => partial_5!(happyShift, curry_1_5!(action_119)),
        200 => partial_5!(happyShift, curry_1_5!(action_120)),
        201 => partial_5!(happyShift, curry_1_5!(action_121)),
        202 => partial_5!(happyShift, curry_1_5!(action_122)),
        207 => partial_5!(happyShift, curry_1_5!(action_123)),
        208 => partial_5!(happyShift, curry_1_5!(action_124)),
        209 => partial_5!(happyShift, curry_1_5!(action_125)),
        210 => partial_5!(happyShift, curry_1_5!(action_126)),
        212 => partial_5!(happyShift, curry_1_5!(action_127)),
        213 => partial_5!(happyShift, curry_1_5!(action_173)),
        214 => partial_5!(happyShift, curry_1_5!(action_174)),
        215 => partial_5!(happyShift, curry_1_5!(action_128)),
        216 => partial_5!(happyShift, curry_1_5!(action_175)),
        218 => partial_5!(happyShift, curry_1_5!(action_129)),
        219 => partial_5!(happyShift, curry_1_5!(action_130)),
        221 => partial_5!(happyShift, curry_1_5!(action_131)),
        223 => partial_5!(happyShift, curry_1_5!(action_133)),
        225 => partial_5!(happyShift, curry_1_5!(action_134)),
        226 => partial_5!(happyShift, curry_1_5!(action_438)),
        227 => partial_5!(happyShift, curry_1_5!(action_136)),
        228 => partial_5!(happyShift, curry_1_5!(action_137)),
        229 => partial_5!(happyShift, curry_1_5!(action_138)),
        230 => partial_5!(happyShift, curry_1_5!(action_139)),
        231 => partial_5!(happyShift, curry_1_5!(action_176)),
        237 => partial_5!(happyShift, curry_1_5!(action_140)),
        238 => partial_5!(happyShift, curry_1_5!(action_439)),
        239 => partial_5!(happyShift, curry_1_5!(action_142)),
        11 => partial_5!(happyGoto, curry_1_5!(action_829)),
        40 => partial_5!(happyGoto, curry_1_5!(action_432)),
        41 => partial_5!(happyGoto, curry_1_5!(action_85)),
        42 => partial_5!(happyGoto, curry_1_5!(action_86)),
        43 => partial_5!(happyGoto, curry_1_5!(action_87)),
        45 => partial_5!(happyGoto, curry_1_5!(action_433)),
        52 => partial_5!(happyGoto, curry_1_5!(action_434)),
        53 => partial_5!(happyGoto, curry_1_5!(action_97)),
        54 => partial_5!(happyGoto, curry_1_5!(action_98)),
        61 => partial_5!(happyGoto, curry_1_5!(action_99)),
        64 => partial_5!(happyGoto, curry_1_5!(action_435)),
        75 => partial_5!(happyGoto, curry_1_5!(action_436)),
        76 => partial_5!(happyGoto, curry_1_5!(action_102)),
        77 => partial_5!(happyGoto, curry_1_5!(action_103)),
        78 => partial_5!(happyGoto, curry_1_5!(action_481)),
        134 => partial_5!(happyGoto, curry_1_5!(action_167)),
        _ => box happyFail
    }
}

fn action_676(i: isize) -> ActionReturn {
    match i {
        181 => partial_5!(happyShift, curry_1_5!(action_61)),
        14 => partial_5!(happyGoto, curry_1_5!(action_828)),
        _ => box happyFail
    }
}

fn action_677(i: isize) -> ActionReturn {
    match i {
        181 => partial_5!(happyShift, curry_1_5!(action_61)),
        14 => partial_5!(happyGoto, curry_1_5!(action_827)),
        _ => box happyFail
    }
}

fn action_678(i: isize) -> ActionReturn {
    match i {
        181 => partial_5!(happyShift, curry_1_5!(action_61)),
        14 => partial_5!(happyGoto, curry_1_5!(action_826)),
        _ => box happyFail
    }
}

fn action_679(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_38()
    }
}

fn action_680(i: isize) -> ActionReturn {
    match i {
        179 => partial_5!(happyShift, curry_1_5!(action_528)),
        180 => partial_5!(happyShift, curry_1_5!(action_825)),
        _ => box happyFail
    }
}

fn action_681(i: isize) -> ActionReturn {
    match i {
        182 => partial_5!(happyShift, curry_1_5!(action_824)),
        _ => box happyFail
    }
}

fn action_682(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_54()
    }
}

fn action_683(i: isize) -> ActionReturn {
    match i {
        180 => partial_5!(happyShift, curry_1_5!(action_823)),
        _ => box happyFail
    }
}

fn action_684(i: isize) -> ActionReturn {
    match i {
        140 => partial_5!(happyShift, curry_1_5!(action_822)),
        236 => partial_5!(happyShift, curry_1_5!(action_42)),
        28 => partial_5!(happyGoto, curry_1_5!(action_818)),
        29 => partial_5!(happyGoto, curry_1_5!(action_819)),
        30 => partial_5!(happyGoto, curry_1_5!(action_820)),
        128 => partial_5!(happyGoto, curry_1_5!(action_821)),
        _ => happyReduce_76()
    }
}

fn action_685(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_26)),
        144 => partial_5!(happyShift, curry_1_5!(action_27)),
        145 => partial_5!(happyShift, curry_1_5!(action_28)),
        146 => partial_5!(happyShift, curry_1_5!(action_29)),
        147 => partial_5!(happyShift, curry_1_5!(action_30)),
        148 => partial_5!(happyShift, curry_1_5!(action_31)),
        149 => partial_5!(happyShift, curry_1_5!(action_32)),
        150 => partial_5!(happyShift, curry_1_5!(action_33)),
        153 => partial_5!(happyShift, curry_1_5!(action_34)),
        164 => partial_5!(happyShift, curry_1_5!(action_35)),
        180 => partial_5!(happyShift, curry_1_5!(action_60)),
        181 => partial_5!(happyShift, curry_1_5!(action_61)),
        184 => partial_5!(happyShift, curry_1_5!(action_36)),
        187 => partial_5!(happyShift, curry_1_5!(action_62)),
        189 => partial_5!(happyShift, curry_1_5!(action_63)),
        191 => partial_5!(happyShift, curry_1_5!(action_64)),
        194 => partial_5!(happyShift, curry_1_5!(action_65)),
        196 => partial_5!(happyShift, curry_1_5!(action_66)),
        197 => partial_5!(happyShift, curry_1_5!(action_67)),
        203 => partial_5!(happyShift, curry_1_5!(action_68)),
        204 => partial_5!(happyShift, curry_1_5!(action_37)),
        205 => partial_5!(happyShift, curry_1_5!(action_69)),
        206 => partial_5!(happyShift, curry_1_5!(action_70)),
        217 => partial_5!(happyShift, curry_1_5!(action_71)),
        220 => partial_5!(happyShift, curry_1_5!(action_38)),
        224 => partial_5!(happyShift, curry_1_5!(action_72)),
        232 => partial_5!(happyShift, curry_1_5!(action_73)),
        233 => partial_5!(happyShift, curry_1_5!(action_39)),
        234 => partial_5!(happyShift, curry_1_5!(action_40)),
        235 => partial_5!(happyShift, curry_1_5!(action_41)),
        236 => partial_5!(happyShift, curry_1_5!(action_42)),
        237 => partial_5!(happyShift, curry_1_5!(action_74)),
        238 => partial_5!(happyShift, curry_1_5!(action_75)),
        240 => partial_5!(happyShift, curry_1_5!(action_44)),
        241 => partial_5!(happyShift, curry_1_5!(action_45)),
        242 => partial_5!(happyShift, curry_1_5!(action_46)),
        243 => partial_5!(happyShift, curry_1_5!(action_47)),
        244 => partial_5!(happyShift, curry_1_5!(action_48)),
        245 => partial_5!(happyShift, curry_1_5!(action_49)),
        12 => partial_5!(happyGoto, curry_1_5!(action_817)),
        13 => partial_5!(happyGoto, curry_1_5!(action_51)),
        14 => partial_5!(happyGoto, curry_1_5!(action_52)),
        22 => partial_5!(happyGoto, curry_1_5!(action_53)),
        23 => partial_5!(happyGoto, curry_1_5!(action_54)),
        24 => partial_5!(happyGoto, curry_1_5!(action_55)),
        25 => partial_5!(happyGoto, curry_1_5!(action_56)),
        26 => partial_5!(happyGoto, curry_1_5!(action_57)),
        100 => partial_5!(happyGoto, curry_1_5!(action_6)),
        104 => partial_5!(happyGoto, curry_1_5!(action_7)),
        106 => partial_5!(happyGoto, curry_1_5!(action_8)),
        107 => partial_5!(happyGoto, curry_1_5!(action_9)),
        108 => partial_5!(happyGoto, curry_1_5!(action_10)),
        109 => partial_5!(happyGoto, curry_1_5!(action_11)),
        110 => partial_5!(happyGoto, curry_1_5!(action_12)),
        111 => partial_5!(happyGoto, curry_1_5!(action_13)),
        112 => partial_5!(happyGoto, curry_1_5!(action_14)),
        113 => partial_5!(happyGoto, curry_1_5!(action_15)),
        114 => partial_5!(happyGoto, curry_1_5!(action_16)),
        115 => partial_5!(happyGoto, curry_1_5!(action_17)),
        116 => partial_5!(happyGoto, curry_1_5!(action_18)),
        117 => partial_5!(happyGoto, curry_1_5!(action_19)),
        118 => partial_5!(happyGoto, curry_1_5!(action_20)),
        119 => partial_5!(happyGoto, curry_1_5!(action_21)),
        120 => partial_5!(happyGoto, curry_1_5!(action_22)),
        122 => partial_5!(happyGoto, curry_1_5!(action_58)),
        127 => partial_5!(happyGoto, curry_1_5!(action_24)),
        128 => partial_5!(happyGoto, curry_1_5!(action_25)),
        131 => partial_5!(happyGoto, curry_1_5!(action_59)),
        _ => box happyFail
    }
}

fn action_686(i: isize) -> ActionReturn {
    match i {
        139 => partial_5!(happyShift, curry_1_5!(action_816)),
        _ => box happyFail
    }
}

fn action_687(i: isize) -> ActionReturn {
    match i {
        180 => partial_5!(happyShift, curry_1_5!(action_815)),
        _ => box happyFail
    }
}

fn action_688(i: isize) -> ActionReturn {
    match i {
        180 => partial_5!(happyShift, curry_1_5!(action_814)),
        _ => box happyFail
    }
}

fn action_689(i: isize) -> ActionReturn {
    match i {
        199 => partial_5!(happyShift, curry_1_5!(action_813)),
        _ => happyReduce_58()
    }
}

fn action_690(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_60()
    }
}

fn action_691(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_61()
    }
}

fn action_692(i: isize) -> ActionReturn {
    match i {
        168 => partial_5!(happyShift, curry_1_5!(action_516)),
        94 => partial_5!(happyGoto, curry_1_5!(action_812)),
        _ => happyReduce_344()
    }
}

fn action_693(i: isize) -> ActionReturn {
    match i {
        168 => partial_5!(happyShift, curry_1_5!(action_516)),
        94 => partial_5!(happyGoto, curry_1_5!(action_811)),
        _ => happyReduce_344()
    }
}

fn action_694(i: isize) -> ActionReturn {
    match i {
        139 => partial_5!(happyShift, curry_1_5!(action_810)),
        _ => box happyFail
    }
}

fn action_695(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_166()
    }
}

fn action_696(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_167()
    }
}

fn action_697(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_413)),
        139 => partial_5!(happyShift, curry_1_5!(action_809)),
        140 => partial_5!(happyShift, curry_1_5!(action_182)),
        88 => partial_5!(happyGoto, curry_1_5!(action_573)),
        89 => partial_5!(happyGoto, curry_1_5!(action_179)),
        90 => partial_5!(happyGoto, curry_1_5!(action_180)),
        _ => box happyFail
    }
}

fn action_698(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_243()
    }
}

fn action_699(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_254()
    }
}

fn action_700(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_751)),
        150 => partial_5!(happyShift, curry_1_5!(action_525)),
        237 => partial_5!(happyShift, curry_1_5!(action_140)),
        238 => partial_5!(happyShift, curry_1_5!(action_424)),
        239 => partial_5!(happyShift, curry_1_5!(action_142)),
        70 => partial_5!(happyGoto, curry_1_5!(action_419)),
        71 => partial_5!(happyGoto, curry_1_5!(action_222)),
        72 => partial_5!(happyGoto, curry_1_5!(action_420)),
        73 => partial_5!(happyGoto, curry_1_5!(action_224)),
        74 => partial_5!(happyGoto, curry_1_5!(action_808)),
        76 => partial_5!(happyGoto, curry_1_5!(action_157)),
        77 => partial_5!(happyGoto, curry_1_5!(action_103)),
        78 => partial_5!(happyGoto, curry_1_5!(action_512)),
        133 => partial_5!(happyGoto, curry_1_5!(action_422)),
        134 => partial_5!(happyGoto, curry_1_5!(action_110)),
        _ => box happyFail
    }
}

fn action_701(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_249()
    }
}

fn action_702(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_245()
    }
}

fn action_703(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_257()
    }
}

fn action_704(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_256()
    }
}

fn action_705(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_571)),
        150 => partial_5!(happyShift, curry_1_5!(action_572)),
        186 => partial_5!(happyShift, curry_1_5!(action_171)),
        193 => partial_5!(happyShift, curry_1_5!(action_172)),
        213 => partial_5!(happyShift, curry_1_5!(action_173)),
        214 => partial_5!(happyShift, curry_1_5!(action_174)),
        216 => partial_5!(happyShift, curry_1_5!(action_175)),
        231 => partial_5!(happyShift, curry_1_5!(action_176)),
        237 => partial_5!(happyShift, curry_1_5!(action_140)),
        238 => partial_5!(happyShift, curry_1_5!(action_229)),
        239 => partial_5!(happyShift, curry_1_5!(action_142)),
        64 => partial_5!(happyGoto, curry_1_5!(action_187)),
        69 => partial_5!(happyGoto, curry_1_5!(action_578)),
        70 => partial_5!(happyGoto, curry_1_5!(action_221)),
        71 => partial_5!(happyGoto, curry_1_5!(action_222)),
        75 => partial_5!(happyGoto, curry_1_5!(action_482)),
        76 => partial_5!(happyGoto, curry_1_5!(action_102)),
        77 => partial_5!(happyGoto, curry_1_5!(action_103)),
        78 => partial_5!(happyGoto, curry_1_5!(action_481)),
        133 => partial_5!(happyGoto, curry_1_5!(action_807)),
        134 => partial_5!(happyGoto, curry_1_5!(action_110)),
        _ => box happyFail
    }
}

fn action_706(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_413)),
        140 => partial_5!(happyShift, curry_1_5!(action_182)),
        88 => partial_5!(happyGoto, curry_1_5!(action_806)),
        89 => partial_5!(happyGoto, curry_1_5!(action_179)),
        90 => partial_5!(happyGoto, curry_1_5!(action_180)),
        _ => happyReduce_246()
    }
}

fn action_707(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_259()
    }
}

fn action_708(i: isize) -> ActionReturn {
    match i {
        180 => partial_5!(happyShift, curry_1_5!(action_715)),
        182 => partial_5!(happyShift, curry_1_5!(action_805)),
        190 => partial_5!(happyShift, curry_1_5!(action_116)),
        192 => partial_5!(happyShift, curry_1_5!(action_117)),
        195 => partial_5!(happyShift, curry_1_5!(action_118)),
        198 => partial_5!(happyShift, curry_1_5!(action_119)),
        200 => partial_5!(happyShift, curry_1_5!(action_120)),
        202 => partial_5!(happyShift, curry_1_5!(action_122)),
        208 => partial_5!(happyShift, curry_1_5!(action_124)),
        209 => partial_5!(happyShift, curry_1_5!(action_125)),
        210 => partial_5!(happyShift, curry_1_5!(action_126)),
        218 => partial_5!(happyShift, curry_1_5!(action_129)),
        219 => partial_5!(happyShift, curry_1_5!(action_130)),
        223 => partial_5!(happyShift, curry_1_5!(action_133)),
        226 => partial_5!(happyShift, curry_1_5!(action_135)),
        228 => partial_5!(happyShift, curry_1_5!(action_137)),
        229 => partial_5!(happyShift, curry_1_5!(action_138)),
        230 => partial_5!(happyShift, curry_1_5!(action_139)),
        238 => partial_5!(happyShift, curry_1_5!(action_141)),
        239 => partial_5!(happyShift, curry_1_5!(action_142)),
        240 => partial_5!(happyShift, curry_1_5!(action_717)),
        44 => partial_5!(happyGoto, curry_1_5!(action_709)),
        45 => partial_5!(happyGoto, curry_1_5!(action_89)),
        47 => partial_5!(happyGoto, curry_1_5!(action_287)),
        49 => partial_5!(happyGoto, curry_1_5!(action_288)),
        51 => partial_5!(happyGoto, curry_1_5!(action_289)),
        52 => partial_5!(happyGoto, curry_1_5!(action_96)),
        53 => partial_5!(happyGoto, curry_1_5!(action_97)),
        54 => partial_5!(happyGoto, curry_1_5!(action_98)),
        56 => partial_5!(happyGoto, curry_1_5!(action_710)),
        57 => partial_5!(happyGoto, curry_1_5!(action_711)),
        58 => partial_5!(happyGoto, curry_1_5!(action_712)),
        61 => partial_5!(happyGoto, curry_1_5!(action_99)),
        65 => partial_5!(happyGoto, curry_1_5!(action_713)),
        132 => partial_5!(happyGoto, curry_1_5!(action_108)),
        133 => partial_5!(happyGoto, curry_1_5!(action_714)),
        134 => partial_5!(happyGoto, curry_1_5!(action_110)),
        _ => happyReduce_471()
    }
}

fn action_709(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_524)),
        150 => partial_5!(happyShift, curry_1_5!(action_525)),
        167 => partial_5!(happyShift, curry_1_5!(action_804)),
        237 => partial_5!(happyShift, curry_1_5!(action_140)),
        238 => partial_5!(happyShift, curry_1_5!(action_229)),
        59 => partial_5!(happyGoto, curry_1_5!(action_802)),
        66 => partial_5!(happyGoto, curry_1_5!(action_803)),
        68 => partial_5!(happyGoto, curry_1_5!(action_219)),
        69 => partial_5!(happyGoto, curry_1_5!(action_220)),
        70 => partial_5!(happyGoto, curry_1_5!(action_221)),
        71 => partial_5!(happyGoto, curry_1_5!(action_222)),
        72 => partial_5!(happyGoto, curry_1_5!(action_223)),
        73 => partial_5!(happyGoto, curry_1_5!(action_224)),
        75 => partial_5!(happyGoto, curry_1_5!(action_523)),
        76 => partial_5!(happyGoto, curry_1_5!(action_102)),
        77 => partial_5!(happyGoto, curry_1_5!(action_103)),
        78 => partial_5!(happyGoto, curry_1_5!(action_481)),
        _ => happyReduce_202()
    }
}

fn action_710(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_193()
    }
}

fn action_711(i: isize) -> ActionReturn {
    match i {
        179 => partial_5!(happyShift, curry_1_5!(action_800)),
        180 => partial_5!(happyShift, curry_1_5!(action_801)),
        _ => box happyFail
    }
}

fn action_712(i: isize) -> ActionReturn {
    match i {
        179 => partial_5!(happyShift, curry_1_5!(action_798)),
        180 => partial_5!(happyShift, curry_1_5!(action_799)),
        _ => box happyFail
    }
}

fn action_713(i: isize) -> ActionReturn {
    match i {
        186 => partial_5!(happyShift, curry_1_5!(action_171)),
        190 => partial_5!(happyShift, curry_1_5!(action_116)),
        192 => partial_5!(happyShift, curry_1_5!(action_117)),
        193 => partial_5!(happyShift, curry_1_5!(action_172)),
        195 => partial_5!(happyShift, curry_1_5!(action_118)),
        198 => partial_5!(happyShift, curry_1_5!(action_119)),
        200 => partial_5!(happyShift, curry_1_5!(action_120)),
        202 => partial_5!(happyShift, curry_1_5!(action_122)),
        208 => partial_5!(happyShift, curry_1_5!(action_124)),
        209 => partial_5!(happyShift, curry_1_5!(action_125)),
        210 => partial_5!(happyShift, curry_1_5!(action_126)),
        213 => partial_5!(happyShift, curry_1_5!(action_173)),
        214 => partial_5!(happyShift, curry_1_5!(action_174)),
        216 => partial_5!(happyShift, curry_1_5!(action_175)),
        218 => partial_5!(happyShift, curry_1_5!(action_129)),
        219 => partial_5!(happyShift, curry_1_5!(action_130)),
        223 => partial_5!(happyShift, curry_1_5!(action_133)),
        226 => partial_5!(happyShift, curry_1_5!(action_191)),
        228 => partial_5!(happyShift, curry_1_5!(action_137)),
        229 => partial_5!(happyShift, curry_1_5!(action_138)),
        230 => partial_5!(happyShift, curry_1_5!(action_139)),
        231 => partial_5!(happyShift, curry_1_5!(action_176)),
        238 => partial_5!(happyShift, curry_1_5!(action_192)),
        239 => partial_5!(happyShift, curry_1_5!(action_142)),
        45 => partial_5!(happyGoto, curry_1_5!(action_185)),
        52 => partial_5!(happyGoto, curry_1_5!(action_186)),
        53 => partial_5!(happyGoto, curry_1_5!(action_97)),
        54 => partial_5!(happyGoto, curry_1_5!(action_98)),
        61 => partial_5!(happyGoto, curry_1_5!(action_99)),
        64 => partial_5!(happyGoto, curry_1_5!(action_187)),
        132 => partial_5!(happyGoto, curry_1_5!(action_796)),
        133 => partial_5!(happyGoto, curry_1_5!(action_797)),
        134 => partial_5!(happyGoto, curry_1_5!(action_110)),
        _ => happyReduce_471()
    }
}

fn action_714(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_475)),
        150 => partial_5!(happyShift, curry_1_5!(action_476)),
        167 => partial_5!(happyShift, curry_1_5!(action_795)),
        190 => partial_5!(happyShift, curry_1_5!(action_116)),
        192 => partial_5!(happyShift, curry_1_5!(action_117)),
        195 => partial_5!(happyShift, curry_1_5!(action_118)),
        198 => partial_5!(happyShift, curry_1_5!(action_119)),
        200 => partial_5!(happyShift, curry_1_5!(action_120)),
        202 => partial_5!(happyShift, curry_1_5!(action_122)),
        208 => partial_5!(happyShift, curry_1_5!(action_124)),
        209 => partial_5!(happyShift, curry_1_5!(action_125)),
        210 => partial_5!(happyShift, curry_1_5!(action_126)),
        218 => partial_5!(happyShift, curry_1_5!(action_129)),
        219 => partial_5!(happyShift, curry_1_5!(action_130)),
        223 => partial_5!(happyShift, curry_1_5!(action_133)),
        226 => partial_5!(happyShift, curry_1_5!(action_168)),
        228 => partial_5!(happyShift, curry_1_5!(action_137)),
        229 => partial_5!(happyShift, curry_1_5!(action_138)),
        230 => partial_5!(happyShift, curry_1_5!(action_139)),
        237 => partial_5!(happyShift, curry_1_5!(action_140)),
        238 => partial_5!(happyShift, curry_1_5!(action_169)),
        239 => partial_5!(happyShift, curry_1_5!(action_142)),
        45 => partial_5!(happyGoto, curry_1_5!(action_163)),
        52 => partial_5!(happyGoto, curry_1_5!(action_164)),
        53 => partial_5!(happyGoto, curry_1_5!(action_97)),
        54 => partial_5!(happyGoto, curry_1_5!(action_98)),
        60 => partial_5!(happyGoto, curry_1_5!(action_793)),
        61 => partial_5!(happyGoto, curry_1_5!(action_99)),
        75 => partial_5!(happyGoto, curry_1_5!(action_794)),
        76 => partial_5!(happyGoto, curry_1_5!(action_102)),
        77 => partial_5!(happyGoto, curry_1_5!(action_103)),
        78 => partial_5!(happyGoto, curry_1_5!(action_481)),
        134 => partial_5!(happyGoto, curry_1_5!(action_167)),
        _ => happyReduce_472()
    }
}

fn action_715(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_192()
    }
}

fn action_716(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_187()
    }
}

fn action_717(i: isize) -> ActionReturn {
    match i {
        190 => partial_5!(happyShift, curry_1_5!(action_116)),
        192 => partial_5!(happyShift, curry_1_5!(action_117)),
        195 => partial_5!(happyShift, curry_1_5!(action_118)),
        198 => partial_5!(happyShift, curry_1_5!(action_119)),
        200 => partial_5!(happyShift, curry_1_5!(action_120)),
        202 => partial_5!(happyShift, curry_1_5!(action_122)),
        208 => partial_5!(happyShift, curry_1_5!(action_124)),
        209 => partial_5!(happyShift, curry_1_5!(action_125)),
        210 => partial_5!(happyShift, curry_1_5!(action_126)),
        218 => partial_5!(happyShift, curry_1_5!(action_129)),
        219 => partial_5!(happyShift, curry_1_5!(action_130)),
        223 => partial_5!(happyShift, curry_1_5!(action_133)),
        226 => partial_5!(happyShift, curry_1_5!(action_135)),
        228 => partial_5!(happyShift, curry_1_5!(action_137)),
        229 => partial_5!(happyShift, curry_1_5!(action_138)),
        230 => partial_5!(happyShift, curry_1_5!(action_139)),
        238 => partial_5!(happyShift, curry_1_5!(action_141)),
        239 => partial_5!(happyShift, curry_1_5!(action_142)),
        240 => partial_5!(happyShift, curry_1_5!(action_717)),
        44 => partial_5!(happyGoto, curry_1_5!(action_709)),
        45 => partial_5!(happyGoto, curry_1_5!(action_89)),
        47 => partial_5!(happyGoto, curry_1_5!(action_287)),
        49 => partial_5!(happyGoto, curry_1_5!(action_288)),
        51 => partial_5!(happyGoto, curry_1_5!(action_289)),
        52 => partial_5!(happyGoto, curry_1_5!(action_96)),
        53 => partial_5!(happyGoto, curry_1_5!(action_97)),
        54 => partial_5!(happyGoto, curry_1_5!(action_98)),
        56 => partial_5!(happyGoto, curry_1_5!(action_792)),
        57 => partial_5!(happyGoto, curry_1_5!(action_711)),
        58 => partial_5!(happyGoto, curry_1_5!(action_712)),
        61 => partial_5!(happyGoto, curry_1_5!(action_99)),
        65 => partial_5!(happyGoto, curry_1_5!(action_713)),
        132 => partial_5!(happyGoto, curry_1_5!(action_108)),
        133 => partial_5!(happyGoto, curry_1_5!(action_714)),
        134 => partial_5!(happyGoto, curry_1_5!(action_110)),
        _ => happyReduce_471()
    }
}

fn action_718(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_174()
    }
}

fn action_719(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_175()
    }
}

fn action_720(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_94()
    }
}

fn action_721(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_25()
    }
}

fn action_722(i: isize) -> ActionReturn {
    match i {
        139 => partial_5!(happyShift, curry_1_5!(action_791)),
        _ => box happyFail
    }
}

fn action_723(i: isize) -> ActionReturn {
    match i {
        139 => partial_5!(happyShift, curry_1_5!(action_790)),
        _ => box happyFail
    }
}

fn action_724(i: isize) -> ActionReturn {
    match i {
        141 => partial_5!(happyShift, curry_1_5!(action_789)),
        _ => box happyFail
    }
}

fn action_725(i: isize) -> ActionReturn {
    match i {
        141 => partial_5!(happyShift, curry_1_5!(action_788)),
        _ => box happyFail
    }
}

fn action_726(i: isize) -> ActionReturn {
    match i {
        141 => happyReduce_471(),
        239 => partial_5!(happyShift, curry_1_5!(action_142)),
        132 => partial_5!(happyGoto, curry_1_5!(action_787)),
        133 => partial_5!(happyGoto, curry_1_5!(action_150)),
        134 => partial_5!(happyGoto, curry_1_5!(action_110)),
        _ => happyReduce_401()
    }
}

fn action_727(i: isize) -> ActionReturn {
    match i {
        239 => partial_5!(happyShift, curry_1_5!(action_142)),
        132 => partial_5!(happyGoto, curry_1_5!(action_786)),
        133 => partial_5!(happyGoto, curry_1_5!(action_150)),
        134 => partial_5!(happyGoto, curry_1_5!(action_110)),
        _ => happyReduce_471()
    }
}

fn action_728(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_317()
    }
}

fn action_729(i: isize) -> ActionReturn {
    match i {
        141 => partial_5!(happyShift, curry_1_5!(action_785)),
        _ => box happyFail
    }
}

fn action_730(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_316()
    }
}

fn action_731(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_322()
    }
}

fn action_732(i: isize) -> ActionReturn {
    match i {
        141 => partial_5!(happyShift, curry_1_5!(action_784)),
        _ => box happyFail
    }
}

fn action_733(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_26)),
        144 => partial_5!(happyShift, curry_1_5!(action_27)),
        145 => partial_5!(happyShift, curry_1_5!(action_28)),
        146 => partial_5!(happyShift, curry_1_5!(action_29)),
        147 => partial_5!(happyShift, curry_1_5!(action_30)),
        148 => partial_5!(happyShift, curry_1_5!(action_31)),
        149 => partial_5!(happyShift, curry_1_5!(action_32)),
        150 => partial_5!(happyShift, curry_1_5!(action_33)),
        153 => partial_5!(happyShift, curry_1_5!(action_34)),
        164 => partial_5!(happyShift, curry_1_5!(action_35)),
        184 => partial_5!(happyShift, curry_1_5!(action_36)),
        204 => partial_5!(happyShift, curry_1_5!(action_37)),
        220 => partial_5!(happyShift, curry_1_5!(action_38)),
        233 => partial_5!(happyShift, curry_1_5!(action_39)),
        234 => partial_5!(happyShift, curry_1_5!(action_40)),
        235 => partial_5!(happyShift, curry_1_5!(action_41)),
        236 => partial_5!(happyShift, curry_1_5!(action_42)),
        237 => partial_5!(happyShift, curry_1_5!(action_43)),
        240 => partial_5!(happyShift, curry_1_5!(action_44)),
        241 => partial_5!(happyShift, curry_1_5!(action_45)),
        242 => partial_5!(happyShift, curry_1_5!(action_46)),
        243 => partial_5!(happyShift, curry_1_5!(action_47)),
        244 => partial_5!(happyShift, curry_1_5!(action_48)),
        245 => partial_5!(happyShift, curry_1_5!(action_49)),
        100 => partial_5!(happyGoto, curry_1_5!(action_6)),
        104 => partial_5!(happyGoto, curry_1_5!(action_7)),
        106 => partial_5!(happyGoto, curry_1_5!(action_8)),
        107 => partial_5!(happyGoto, curry_1_5!(action_9)),
        108 => partial_5!(happyGoto, curry_1_5!(action_10)),
        109 => partial_5!(happyGoto, curry_1_5!(action_11)),
        110 => partial_5!(happyGoto, curry_1_5!(action_12)),
        111 => partial_5!(happyGoto, curry_1_5!(action_13)),
        112 => partial_5!(happyGoto, curry_1_5!(action_14)),
        113 => partial_5!(happyGoto, curry_1_5!(action_15)),
        114 => partial_5!(happyGoto, curry_1_5!(action_16)),
        115 => partial_5!(happyGoto, curry_1_5!(action_17)),
        116 => partial_5!(happyGoto, curry_1_5!(action_18)),
        117 => partial_5!(happyGoto, curry_1_5!(action_19)),
        118 => partial_5!(happyGoto, curry_1_5!(action_20)),
        119 => partial_5!(happyGoto, curry_1_5!(action_21)),
        120 => partial_5!(happyGoto, curry_1_5!(action_783)),
        127 => partial_5!(happyGoto, curry_1_5!(action_24)),
        128 => partial_5!(happyGoto, curry_1_5!(action_25)),
        _ => box happyFail
    }
}

fn action_734(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_289()
    }
}

fn action_735(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_290()
    }
}

fn action_736(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_293()
    }
}

fn action_737(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_540)),
        140 => partial_5!(happyShift, curry_1_5!(action_182)),
        150 => partial_5!(happyShift, curry_1_5!(action_541)),
        186 => partial_5!(happyShift, curry_1_5!(action_171)),
        193 => partial_5!(happyShift, curry_1_5!(action_172)),
        213 => partial_5!(happyShift, curry_1_5!(action_173)),
        214 => partial_5!(happyShift, curry_1_5!(action_174)),
        216 => partial_5!(happyShift, curry_1_5!(action_175)),
        231 => partial_5!(happyShift, curry_1_5!(action_176)),
        237 => partial_5!(happyShift, curry_1_5!(action_140)),
        238 => partial_5!(happyShift, curry_1_5!(action_229)),
        239 => partial_5!(happyShift, curry_1_5!(action_142)),
        64 => partial_5!(happyGoto, curry_1_5!(action_187)),
        69 => partial_5!(happyGoto, curry_1_5!(action_578)),
        70 => partial_5!(happyGoto, curry_1_5!(action_221)),
        71 => partial_5!(happyGoto, curry_1_5!(action_222)),
        75 => partial_5!(happyGoto, curry_1_5!(action_482)),
        76 => partial_5!(happyGoto, curry_1_5!(action_102)),
        77 => partial_5!(happyGoto, curry_1_5!(action_103)),
        78 => partial_5!(happyGoto, curry_1_5!(action_481)),
        87 => partial_5!(happyGoto, curry_1_5!(action_662)),
        88 => partial_5!(happyGoto, curry_1_5!(action_367)),
        89 => partial_5!(happyGoto, curry_1_5!(action_179)),
        90 => partial_5!(happyGoto, curry_1_5!(action_180)),
        91 => partial_5!(happyGoto, curry_1_5!(action_368)),
        92 => partial_5!(happyGoto, curry_1_5!(action_369)),
        132 => partial_5!(happyGoto, curry_1_5!(action_663)),
        133 => partial_5!(happyGoto, curry_1_5!(action_782)),
        134 => partial_5!(happyGoto, curry_1_5!(action_110)),
        _ => happyReduce_471()
    }
}

fn action_738(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_540)),
        140 => partial_5!(happyShift, curry_1_5!(action_182)),
        150 => partial_5!(happyShift, curry_1_5!(action_541)),
        186 => happyReduce_472(),
        193 => happyReduce_472(),
        213 => happyReduce_472(),
        214 => happyReduce_472(),
        216 => happyReduce_472(),
        231 => happyReduce_472(),
        237 => partial_5!(happyShift, curry_1_5!(action_140)),
        238 => partial_5!(happyShift, curry_1_5!(action_229)),
        239 => partial_5!(happyShift, curry_1_5!(action_142)),
        69 => partial_5!(happyGoto, curry_1_5!(action_577)),
        70 => partial_5!(happyGoto, curry_1_5!(action_221)),
        71 => partial_5!(happyGoto, curry_1_5!(action_222)),
        75 => partial_5!(happyGoto, curry_1_5!(action_480)),
        76 => partial_5!(happyGoto, curry_1_5!(action_102)),
        77 => partial_5!(happyGoto, curry_1_5!(action_103)),
        78 => partial_5!(happyGoto, curry_1_5!(action_481)),
        87 => partial_5!(happyGoto, curry_1_5!(action_661)),
        88 => partial_5!(happyGoto, curry_1_5!(action_367)),
        89 => partial_5!(happyGoto, curry_1_5!(action_179)),
        90 => partial_5!(happyGoto, curry_1_5!(action_180)),
        91 => partial_5!(happyGoto, curry_1_5!(action_368)),
        92 => partial_5!(happyGoto, curry_1_5!(action_369)),
        134 => partial_5!(happyGoto, curry_1_5!(action_167)),
        _ => happyReduce_330()
    }
}

fn action_739(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_540)),
        140 => partial_5!(happyShift, curry_1_5!(action_182)),
        150 => partial_5!(happyShift, curry_1_5!(action_541)),
        185 => partial_5!(happyShift, curry_1_5!(action_113)),
        188 => partial_5!(happyShift, curry_1_5!(action_115)),
        190 => partial_5!(happyShift, curry_1_5!(action_116)),
        192 => partial_5!(happyShift, curry_1_5!(action_117)),
        195 => partial_5!(happyShift, curry_1_5!(action_118)),
        198 => partial_5!(happyShift, curry_1_5!(action_119)),
        200 => partial_5!(happyShift, curry_1_5!(action_120)),
        201 => partial_5!(happyShift, curry_1_5!(action_121)),
        202 => partial_5!(happyShift, curry_1_5!(action_122)),
        207 => partial_5!(happyShift, curry_1_5!(action_123)),
        208 => partial_5!(happyShift, curry_1_5!(action_124)),
        209 => partial_5!(happyShift, curry_1_5!(action_125)),
        210 => partial_5!(happyShift, curry_1_5!(action_126)),
        212 => partial_5!(happyShift, curry_1_5!(action_127)),
        215 => partial_5!(happyShift, curry_1_5!(action_128)),
        218 => partial_5!(happyShift, curry_1_5!(action_129)),
        219 => partial_5!(happyShift, curry_1_5!(action_130)),
        221 => partial_5!(happyShift, curry_1_5!(action_131)),
        223 => partial_5!(happyShift, curry_1_5!(action_133)),
        225 => partial_5!(happyShift, curry_1_5!(action_134)),
        226 => partial_5!(happyShift, curry_1_5!(action_168)),
        227 => partial_5!(happyShift, curry_1_5!(action_136)),
        228 => partial_5!(happyShift, curry_1_5!(action_137)),
        229 => partial_5!(happyShift, curry_1_5!(action_138)),
        230 => partial_5!(happyShift, curry_1_5!(action_139)),
        237 => partial_5!(happyShift, curry_1_5!(action_140)),
        238 => partial_5!(happyShift, curry_1_5!(action_169)),
        239 => partial_5!(happyShift, curry_1_5!(action_142)),
        40 => partial_5!(happyGoto, curry_1_5!(action_162)),
        41 => partial_5!(happyGoto, curry_1_5!(action_85)),
        42 => partial_5!(happyGoto, curry_1_5!(action_86)),
        43 => partial_5!(happyGoto, curry_1_5!(action_87)),
        45 => partial_5!(happyGoto, curry_1_5!(action_163)),
        52 => partial_5!(happyGoto, curry_1_5!(action_164)),
        53 => partial_5!(happyGoto, curry_1_5!(action_97)),
        54 => partial_5!(happyGoto, curry_1_5!(action_98)),
        61 => partial_5!(happyGoto, curry_1_5!(action_99)),
        70 => partial_5!(happyGoto, curry_1_5!(action_570)),
        71 => partial_5!(happyGoto, curry_1_5!(action_222)),
        76 => partial_5!(happyGoto, curry_1_5!(action_473)),
        77 => partial_5!(happyGoto, curry_1_5!(action_103)),
        78 => partial_5!(happyGoto, curry_1_5!(action_474)),
        88 => partial_5!(happyGoto, curry_1_5!(action_655)),
        89 => partial_5!(happyGoto, curry_1_5!(action_179)),
        90 => partial_5!(happyGoto, curry_1_5!(action_180)),
        91 => partial_5!(happyGoto, curry_1_5!(action_656)),
        92 => partial_5!(happyGoto, curry_1_5!(action_657)),
        134 => partial_5!(happyGoto, curry_1_5!(action_167)),
        _ => happyReduce_472()
    }
}

fn action_740(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_296()
    }
}

fn action_741(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_297()
    }
}

fn action_742(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_535)),
        140 => partial_5!(happyShift, curry_1_5!(action_182)),
        150 => partial_5!(happyShift, curry_1_5!(action_536)),
        186 => partial_5!(happyShift, curry_1_5!(action_171)),
        193 => partial_5!(happyShift, curry_1_5!(action_172)),
        213 => partial_5!(happyShift, curry_1_5!(action_173)),
        214 => partial_5!(happyShift, curry_1_5!(action_174)),
        216 => partial_5!(happyShift, curry_1_5!(action_175)),
        231 => partial_5!(happyShift, curry_1_5!(action_176)),
        237 => partial_5!(happyShift, curry_1_5!(action_140)),
        239 => partial_5!(happyShift, curry_1_5!(action_142)),
        64 => partial_5!(happyGoto, curry_1_5!(action_187)),
        75 => partial_5!(happyGoto, curry_1_5!(action_482)),
        76 => partial_5!(happyGoto, curry_1_5!(action_102)),
        77 => partial_5!(happyGoto, curry_1_5!(action_103)),
        78 => partial_5!(happyGoto, curry_1_5!(action_481)),
        87 => partial_5!(happyGoto, curry_1_5!(action_662)),
        88 => partial_5!(happyGoto, curry_1_5!(action_367)),
        89 => partial_5!(happyGoto, curry_1_5!(action_179)),
        90 => partial_5!(happyGoto, curry_1_5!(action_180)),
        91 => partial_5!(happyGoto, curry_1_5!(action_368)),
        92 => partial_5!(happyGoto, curry_1_5!(action_369)),
        132 => partial_5!(happyGoto, curry_1_5!(action_663)),
        133 => partial_5!(happyGoto, curry_1_5!(action_781)),
        134 => partial_5!(happyGoto, curry_1_5!(action_110)),
        _ => happyReduce_471()
    }
}

fn action_743(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_535)),
        140 => partial_5!(happyShift, curry_1_5!(action_182)),
        150 => partial_5!(happyShift, curry_1_5!(action_536)),
        186 => happyReduce_472(),
        193 => happyReduce_472(),
        213 => happyReduce_472(),
        214 => happyReduce_472(),
        216 => happyReduce_472(),
        231 => happyReduce_472(),
        237 => partial_5!(happyShift, curry_1_5!(action_140)),
        239 => partial_5!(happyShift, curry_1_5!(action_142)),
        75 => partial_5!(happyGoto, curry_1_5!(action_480)),
        76 => partial_5!(happyGoto, curry_1_5!(action_102)),
        77 => partial_5!(happyGoto, curry_1_5!(action_103)),
        78 => partial_5!(happyGoto, curry_1_5!(action_481)),
        87 => partial_5!(happyGoto, curry_1_5!(action_661)),
        88 => partial_5!(happyGoto, curry_1_5!(action_367)),
        89 => partial_5!(happyGoto, curry_1_5!(action_179)),
        90 => partial_5!(happyGoto, curry_1_5!(action_180)),
        91 => partial_5!(happyGoto, curry_1_5!(action_368)),
        92 => partial_5!(happyGoto, curry_1_5!(action_369)),
        134 => partial_5!(happyGoto, curry_1_5!(action_167)),
        _ => happyReduce_330()
    }
}

fn action_744(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_535)),
        140 => partial_5!(happyShift, curry_1_5!(action_182)),
        150 => partial_5!(happyShift, curry_1_5!(action_536)),
        185 => partial_5!(happyShift, curry_1_5!(action_113)),
        188 => partial_5!(happyShift, curry_1_5!(action_115)),
        190 => partial_5!(happyShift, curry_1_5!(action_116)),
        192 => partial_5!(happyShift, curry_1_5!(action_117)),
        195 => partial_5!(happyShift, curry_1_5!(action_118)),
        198 => partial_5!(happyShift, curry_1_5!(action_119)),
        200 => partial_5!(happyShift, curry_1_5!(action_120)),
        201 => partial_5!(happyShift, curry_1_5!(action_121)),
        202 => partial_5!(happyShift, curry_1_5!(action_122)),
        207 => partial_5!(happyShift, curry_1_5!(action_123)),
        208 => partial_5!(happyShift, curry_1_5!(action_124)),
        209 => partial_5!(happyShift, curry_1_5!(action_125)),
        210 => partial_5!(happyShift, curry_1_5!(action_126)),
        212 => partial_5!(happyShift, curry_1_5!(action_127)),
        215 => partial_5!(happyShift, curry_1_5!(action_128)),
        218 => partial_5!(happyShift, curry_1_5!(action_129)),
        219 => partial_5!(happyShift, curry_1_5!(action_130)),
        221 => partial_5!(happyShift, curry_1_5!(action_131)),
        223 => partial_5!(happyShift, curry_1_5!(action_133)),
        225 => partial_5!(happyShift, curry_1_5!(action_134)),
        226 => partial_5!(happyShift, curry_1_5!(action_168)),
        227 => partial_5!(happyShift, curry_1_5!(action_136)),
        228 => partial_5!(happyShift, curry_1_5!(action_137)),
        229 => partial_5!(happyShift, curry_1_5!(action_138)),
        230 => partial_5!(happyShift, curry_1_5!(action_139)),
        237 => partial_5!(happyShift, curry_1_5!(action_140)),
        238 => partial_5!(happyShift, curry_1_5!(action_169)),
        239 => partial_5!(happyShift, curry_1_5!(action_142)),
        40 => partial_5!(happyGoto, curry_1_5!(action_162)),
        41 => partial_5!(happyGoto, curry_1_5!(action_85)),
        42 => partial_5!(happyGoto, curry_1_5!(action_86)),
        43 => partial_5!(happyGoto, curry_1_5!(action_87)),
        45 => partial_5!(happyGoto, curry_1_5!(action_163)),
        52 => partial_5!(happyGoto, curry_1_5!(action_164)),
        53 => partial_5!(happyGoto, curry_1_5!(action_97)),
        54 => partial_5!(happyGoto, curry_1_5!(action_98)),
        61 => partial_5!(happyGoto, curry_1_5!(action_99)),
        76 => partial_5!(happyGoto, curry_1_5!(action_473)),
        77 => partial_5!(happyGoto, curry_1_5!(action_103)),
        78 => partial_5!(happyGoto, curry_1_5!(action_474)),
        88 => partial_5!(happyGoto, curry_1_5!(action_655)),
        89 => partial_5!(happyGoto, curry_1_5!(action_179)),
        90 => partial_5!(happyGoto, curry_1_5!(action_180)),
        91 => partial_5!(happyGoto, curry_1_5!(action_656)),
        92 => partial_5!(happyGoto, curry_1_5!(action_657)),
        134 => partial_5!(happyGoto, curry_1_5!(action_167)),
        _ => happyReduce_472()
    }
}

fn action_745(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_301()
    }
}

fn action_746(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_286()
    }
}

fn action_747(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_284()
    }
}

fn action_748(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_303()
    }
}

fn action_749(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_780)),
        150 => partial_5!(happyShift, curry_1_5!(action_525)),
        186 => partial_5!(happyShift, curry_1_5!(action_171)),
        193 => partial_5!(happyShift, curry_1_5!(action_172)),
        213 => partial_5!(happyShift, curry_1_5!(action_173)),
        214 => partial_5!(happyShift, curry_1_5!(action_174)),
        216 => partial_5!(happyShift, curry_1_5!(action_175)),
        231 => partial_5!(happyShift, curry_1_5!(action_176)),
        237 => partial_5!(happyShift, curry_1_5!(action_140)),
        238 => partial_5!(happyShift, curry_1_5!(action_229)),
        239 => partial_5!(happyShift, curry_1_5!(action_142)),
        64 => partial_5!(happyGoto, curry_1_5!(action_187)),
        69 => partial_5!(happyGoto, curry_1_5!(action_578)),
        70 => partial_5!(happyGoto, curry_1_5!(action_221)),
        71 => partial_5!(happyGoto, curry_1_5!(action_222)),
        72 => partial_5!(happyGoto, curry_1_5!(action_579)),
        73 => partial_5!(happyGoto, curry_1_5!(action_224)),
        75 => partial_5!(happyGoto, curry_1_5!(action_482)),
        76 => partial_5!(happyGoto, curry_1_5!(action_102)),
        77 => partial_5!(happyGoto, curry_1_5!(action_103)),
        78 => partial_5!(happyGoto, curry_1_5!(action_481)),
        133 => partial_5!(happyGoto, curry_1_5!(action_580)),
        134 => partial_5!(happyGoto, curry_1_5!(action_110)),
        _ => box happyFail
    }
}

fn action_750(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_751)),
        150 => partial_5!(happyShift, curry_1_5!(action_525)),
        237 => partial_5!(happyShift, curry_1_5!(action_140)),
        238 => partial_5!(happyShift, curry_1_5!(action_424)),
        239 => partial_5!(happyShift, curry_1_5!(action_142)),
        70 => partial_5!(happyGoto, curry_1_5!(action_419)),
        71 => partial_5!(happyGoto, curry_1_5!(action_222)),
        72 => partial_5!(happyGoto, curry_1_5!(action_420)),
        73 => partial_5!(happyGoto, curry_1_5!(action_224)),
        74 => partial_5!(happyGoto, curry_1_5!(action_576)),
        76 => partial_5!(happyGoto, curry_1_5!(action_157)),
        77 => partial_5!(happyGoto, curry_1_5!(action_103)),
        78 => partial_5!(happyGoto, curry_1_5!(action_512)),
        133 => partial_5!(happyGoto, curry_1_5!(action_422)),
        134 => partial_5!(happyGoto, curry_1_5!(action_110)),
        _ => box happyFail
    }
}

fn action_751(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_751)),
        150 => partial_5!(happyShift, curry_1_5!(action_525)),
        237 => partial_5!(happyShift, curry_1_5!(action_140)),
        238 => partial_5!(happyShift, curry_1_5!(action_424)),
        239 => partial_5!(happyShift, curry_1_5!(action_142)),
        70 => partial_5!(happyGoto, curry_1_5!(action_419)),
        71 => partial_5!(happyGoto, curry_1_5!(action_222)),
        72 => partial_5!(happyGoto, curry_1_5!(action_420)),
        73 => partial_5!(happyGoto, curry_1_5!(action_224)),
        74 => partial_5!(happyGoto, curry_1_5!(action_569)),
        76 => partial_5!(happyGoto, curry_1_5!(action_157)),
        77 => partial_5!(happyGoto, curry_1_5!(action_103)),
        78 => partial_5!(happyGoto, curry_1_5!(action_512)),
        133 => partial_5!(happyGoto, curry_1_5!(action_422)),
        134 => partial_5!(happyGoto, curry_1_5!(action_110)),
        _ => box happyFail
    }
}

fn action_752(i: isize) -> ActionReturn {
    match i {
        187 => partial_5!(happyShift, curry_1_5!(action_406)),
        35 => partial_5!(happyGoto, curry_1_5!(action_561)),
        67 => partial_5!(happyGoto, curry_1_5!(action_405)),
        _ => happyReduce_232()
    }
}

fn action_753(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_177()
    }
}

fn action_754(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_178()
    }
}

fn action_755(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_345()
    }
}

fn action_756(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_271()
    }
}

fn action_757(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_11()
    }
}

fn action_758(i: isize) -> ActionReturn {
    match i {
        179 => partial_5!(happyShift, curry_1_5!(action_778)),
        182 => partial_5!(happyShift, curry_1_5!(action_779)),
        _ => box happyFail
    }
}

fn action_759(i: isize) -> ActionReturn {
    match i {
        168 => partial_5!(happyShift, curry_1_5!(action_777)),
        239 => partial_5!(happyShift, curry_1_5!(action_142)),
        134 => partial_5!(happyGoto, curry_1_5!(action_167)),
        _ => happyReduce_218()
    }
}

fn action_760(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_26)),
        144 => partial_5!(happyShift, curry_1_5!(action_27)),
        145 => partial_5!(happyShift, curry_1_5!(action_28)),
        146 => partial_5!(happyShift, curry_1_5!(action_29)),
        147 => partial_5!(happyShift, curry_1_5!(action_30)),
        148 => partial_5!(happyShift, curry_1_5!(action_31)),
        149 => partial_5!(happyShift, curry_1_5!(action_32)),
        150 => partial_5!(happyShift, curry_1_5!(action_33)),
        153 => partial_5!(happyShift, curry_1_5!(action_34)),
        164 => partial_5!(happyShift, curry_1_5!(action_35)),
        184 => partial_5!(happyShift, curry_1_5!(action_36)),
        204 => partial_5!(happyShift, curry_1_5!(action_37)),
        220 => partial_5!(happyShift, curry_1_5!(action_38)),
        233 => partial_5!(happyShift, curry_1_5!(action_39)),
        234 => partial_5!(happyShift, curry_1_5!(action_40)),
        235 => partial_5!(happyShift, curry_1_5!(action_41)),
        236 => partial_5!(happyShift, curry_1_5!(action_42)),
        237 => partial_5!(happyShift, curry_1_5!(action_43)),
        240 => partial_5!(happyShift, curry_1_5!(action_44)),
        241 => partial_5!(happyShift, curry_1_5!(action_45)),
        242 => partial_5!(happyShift, curry_1_5!(action_46)),
        243 => partial_5!(happyShift, curry_1_5!(action_47)),
        244 => partial_5!(happyShift, curry_1_5!(action_48)),
        245 => partial_5!(happyShift, curry_1_5!(action_49)),
        100 => partial_5!(happyGoto, curry_1_5!(action_6)),
        104 => partial_5!(happyGoto, curry_1_5!(action_7)),
        106 => partial_5!(happyGoto, curry_1_5!(action_259)),
        107 => partial_5!(happyGoto, curry_1_5!(action_9)),
        108 => partial_5!(happyGoto, curry_1_5!(action_10)),
        109 => partial_5!(happyGoto, curry_1_5!(action_11)),
        110 => partial_5!(happyGoto, curry_1_5!(action_12)),
        111 => partial_5!(happyGoto, curry_1_5!(action_13)),
        112 => partial_5!(happyGoto, curry_1_5!(action_14)),
        113 => partial_5!(happyGoto, curry_1_5!(action_15)),
        114 => partial_5!(happyGoto, curry_1_5!(action_16)),
        115 => partial_5!(happyGoto, curry_1_5!(action_17)),
        116 => partial_5!(happyGoto, curry_1_5!(action_18)),
        117 => partial_5!(happyGoto, curry_1_5!(action_19)),
        118 => partial_5!(happyGoto, curry_1_5!(action_20)),
        119 => partial_5!(happyGoto, curry_1_5!(action_260)),
        126 => partial_5!(happyGoto, curry_1_5!(action_776)),
        127 => partial_5!(happyGoto, curry_1_5!(action_24)),
        128 => partial_5!(happyGoto, curry_1_5!(action_25)),
        _ => box happyFail
    }
}

fn action_761(i: isize) -> ActionReturn {
    match i {
        182 => partial_5!(happyShift, curry_1_5!(action_775)),
        237 => partial_5!(happyShift, curry_1_5!(action_254)),
        238 => partial_5!(happyShift, curry_1_5!(action_75)),
        63 => partial_5!(happyGoto, curry_1_5!(action_774)),
        131 => partial_5!(happyGoto, curry_1_5!(action_503)),
        _ => box happyFail
    }
}

fn action_762(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_210()
    }
}

fn action_763(i: isize) -> ActionReturn {
    match i {
        139 => partial_5!(happyShift, curry_1_5!(action_773)),
        _ => box happyFail
    }
}

fn action_764(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_26)),
        139 => partial_5!(happyShift, curry_1_5!(action_772)),
        144 => partial_5!(happyShift, curry_1_5!(action_27)),
        145 => partial_5!(happyShift, curry_1_5!(action_28)),
        146 => partial_5!(happyShift, curry_1_5!(action_29)),
        147 => partial_5!(happyShift, curry_1_5!(action_30)),
        148 => partial_5!(happyShift, curry_1_5!(action_31)),
        149 => partial_5!(happyShift, curry_1_5!(action_32)),
        150 => partial_5!(happyShift, curry_1_5!(action_33)),
        153 => partial_5!(happyShift, curry_1_5!(action_34)),
        164 => partial_5!(happyShift, curry_1_5!(action_35)),
        184 => partial_5!(happyShift, curry_1_5!(action_36)),
        204 => partial_5!(happyShift, curry_1_5!(action_37)),
        220 => partial_5!(happyShift, curry_1_5!(action_38)),
        233 => partial_5!(happyShift, curry_1_5!(action_39)),
        234 => partial_5!(happyShift, curry_1_5!(action_40)),
        235 => partial_5!(happyShift, curry_1_5!(action_41)),
        236 => partial_5!(happyShift, curry_1_5!(action_42)),
        237 => partial_5!(happyShift, curry_1_5!(action_43)),
        240 => partial_5!(happyShift, curry_1_5!(action_44)),
        241 => partial_5!(happyShift, curry_1_5!(action_45)),
        242 => partial_5!(happyShift, curry_1_5!(action_46)),
        243 => partial_5!(happyShift, curry_1_5!(action_47)),
        244 => partial_5!(happyShift, curry_1_5!(action_48)),
        245 => partial_5!(happyShift, curry_1_5!(action_49)),
        100 => partial_5!(happyGoto, curry_1_5!(action_6)),
        104 => partial_5!(happyGoto, curry_1_5!(action_7)),
        106 => partial_5!(happyGoto, curry_1_5!(action_769)),
        107 => partial_5!(happyGoto, curry_1_5!(action_9)),
        108 => partial_5!(happyGoto, curry_1_5!(action_10)),
        109 => partial_5!(happyGoto, curry_1_5!(action_11)),
        110 => partial_5!(happyGoto, curry_1_5!(action_12)),
        111 => partial_5!(happyGoto, curry_1_5!(action_13)),
        112 => partial_5!(happyGoto, curry_1_5!(action_14)),
        113 => partial_5!(happyGoto, curry_1_5!(action_15)),
        114 => partial_5!(happyGoto, curry_1_5!(action_16)),
        115 => partial_5!(happyGoto, curry_1_5!(action_17)),
        116 => partial_5!(happyGoto, curry_1_5!(action_18)),
        117 => partial_5!(happyGoto, curry_1_5!(action_19)),
        118 => partial_5!(happyGoto, curry_1_5!(action_20)),
        119 => partial_5!(happyGoto, curry_1_5!(action_260)),
        126 => partial_5!(happyGoto, curry_1_5!(action_770)),
        127 => partial_5!(happyGoto, curry_1_5!(action_24)),
        128 => partial_5!(happyGoto, curry_1_5!(action_25)),
        137 => partial_5!(happyGoto, curry_1_5!(action_771)),
        _ => box happyFail
    }
}

fn action_765(i: isize) -> ActionReturn {
    match i {
        139 => partial_5!(happyShift, curry_1_5!(action_768)),
        _ => box happyFail
    }
}

fn action_766(i: isize) -> ActionReturn {
    match i {
        193 => partial_5!(happyShift, curry_1_5!(action_496)),
        237 => partial_5!(happyShift, curry_1_5!(action_497)),
        136 => partial_5!(happyGoto, curry_1_5!(action_767)),
        _ => happyReduce_478()
    }
}

fn action_767(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_477()
    }
}

fn action_768(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_475()
    }
}

fn action_769(i: isize) -> ActionReturn {
    match i {
        168 => partial_5!(happyShift, curry_1_5!(action_316)),
        169 => partial_5!(happyShift, curry_1_5!(action_317)),
        170 => partial_5!(happyShift, curry_1_5!(action_318)),
        171 => partial_5!(happyShift, curry_1_5!(action_319)),
        172 => partial_5!(happyShift, curry_1_5!(action_320)),
        173 => partial_5!(happyShift, curry_1_5!(action_321)),
        174 => partial_5!(happyShift, curry_1_5!(action_322)),
        175 => partial_5!(happyShift, curry_1_5!(action_323)),
        176 => partial_5!(happyShift, curry_1_5!(action_324)),
        177 => partial_5!(happyShift, curry_1_5!(action_325)),
        178 => partial_5!(happyShift, curry_1_5!(action_326)),
        121 => partial_5!(happyGoto, curry_1_5!(action_896)),
        _ => happyReduce_406()
    }
}

fn action_770(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_483()
    }
}

fn action_771(i: isize) -> ActionReturn {
    match i {
        139 => partial_5!(happyShift, curry_1_5!(action_894)),
        179 => partial_5!(happyShift, curry_1_5!(action_895)),
        _ => box happyFail
    }
}

fn action_772(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_482()
    }
}

fn action_773(i: isize) -> ActionReturn {
    match i {
        180 => partial_5!(happyShift, curry_1_5!(action_893)),
        _ => box happyFail
    }
}

fn action_774(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_216()
    }
}

fn action_775(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_211()
    }
}

fn action_776(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_220()
    }
}

fn action_777(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_26)),
        144 => partial_5!(happyShift, curry_1_5!(action_27)),
        145 => partial_5!(happyShift, curry_1_5!(action_28)),
        146 => partial_5!(happyShift, curry_1_5!(action_29)),
        147 => partial_5!(happyShift, curry_1_5!(action_30)),
        148 => partial_5!(happyShift, curry_1_5!(action_31)),
        149 => partial_5!(happyShift, curry_1_5!(action_32)),
        150 => partial_5!(happyShift, curry_1_5!(action_33)),
        153 => partial_5!(happyShift, curry_1_5!(action_34)),
        164 => partial_5!(happyShift, curry_1_5!(action_35)),
        184 => partial_5!(happyShift, curry_1_5!(action_36)),
        204 => partial_5!(happyShift, curry_1_5!(action_37)),
        220 => partial_5!(happyShift, curry_1_5!(action_38)),
        233 => partial_5!(happyShift, curry_1_5!(action_39)),
        234 => partial_5!(happyShift, curry_1_5!(action_40)),
        235 => partial_5!(happyShift, curry_1_5!(action_41)),
        236 => partial_5!(happyShift, curry_1_5!(action_42)),
        237 => partial_5!(happyShift, curry_1_5!(action_43)),
        240 => partial_5!(happyShift, curry_1_5!(action_44)),
        241 => partial_5!(happyShift, curry_1_5!(action_45)),
        242 => partial_5!(happyShift, curry_1_5!(action_46)),
        243 => partial_5!(happyShift, curry_1_5!(action_47)),
        244 => partial_5!(happyShift, curry_1_5!(action_48)),
        245 => partial_5!(happyShift, curry_1_5!(action_49)),
        100 => partial_5!(happyGoto, curry_1_5!(action_6)),
        104 => partial_5!(happyGoto, curry_1_5!(action_7)),
        106 => partial_5!(happyGoto, curry_1_5!(action_259)),
        107 => partial_5!(happyGoto, curry_1_5!(action_9)),
        108 => partial_5!(happyGoto, curry_1_5!(action_10)),
        109 => partial_5!(happyGoto, curry_1_5!(action_11)),
        110 => partial_5!(happyGoto, curry_1_5!(action_12)),
        111 => partial_5!(happyGoto, curry_1_5!(action_13)),
        112 => partial_5!(happyGoto, curry_1_5!(action_14)),
        113 => partial_5!(happyGoto, curry_1_5!(action_15)),
        114 => partial_5!(happyGoto, curry_1_5!(action_16)),
        115 => partial_5!(happyGoto, curry_1_5!(action_17)),
        116 => partial_5!(happyGoto, curry_1_5!(action_18)),
        117 => partial_5!(happyGoto, curry_1_5!(action_19)),
        118 => partial_5!(happyGoto, curry_1_5!(action_20)),
        119 => partial_5!(happyGoto, curry_1_5!(action_260)),
        126 => partial_5!(happyGoto, curry_1_5!(action_892)),
        127 => partial_5!(happyGoto, curry_1_5!(action_24)),
        128 => partial_5!(happyGoto, curry_1_5!(action_25)),
        _ => box happyFail
    }
}

fn action_778(i: isize) -> ActionReturn {
    match i {
        182 => partial_5!(happyShift, curry_1_5!(action_891)),
        237 => partial_5!(happyShift, curry_1_5!(action_254)),
        238 => partial_5!(happyShift, curry_1_5!(action_75)),
        63 => partial_5!(happyGoto, curry_1_5!(action_774)),
        131 => partial_5!(happyGoto, curry_1_5!(action_503)),
        _ => box happyFail
    }
}

fn action_779(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_212()
    }
}

fn action_780(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_751)),
        150 => partial_5!(happyShift, curry_1_5!(action_525)),
        237 => partial_5!(happyShift, curry_1_5!(action_140)),
        238 => partial_5!(happyShift, curry_1_5!(action_424)),
        239 => partial_5!(happyShift, curry_1_5!(action_142)),
        70 => partial_5!(happyGoto, curry_1_5!(action_419)),
        71 => partial_5!(happyGoto, curry_1_5!(action_222)),
        72 => partial_5!(happyGoto, curry_1_5!(action_420)),
        73 => partial_5!(happyGoto, curry_1_5!(action_224)),
        74 => partial_5!(happyGoto, curry_1_5!(action_697)),
        76 => partial_5!(happyGoto, curry_1_5!(action_157)),
        77 => partial_5!(happyGoto, curry_1_5!(action_103)),
        78 => partial_5!(happyGoto, curry_1_5!(action_512)),
        133 => partial_5!(happyGoto, curry_1_5!(action_422)),
        134 => partial_5!(happyGoto, curry_1_5!(action_110)),
        _ => box happyFail
    }
}

fn action_781(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_475)),
        150 => partial_5!(happyShift, curry_1_5!(action_476)),
        186 => partial_5!(happyShift, curry_1_5!(action_171)),
        193 => partial_5!(happyShift, curry_1_5!(action_172)),
        213 => partial_5!(happyShift, curry_1_5!(action_173)),
        214 => partial_5!(happyShift, curry_1_5!(action_174)),
        216 => partial_5!(happyShift, curry_1_5!(action_175)),
        231 => partial_5!(happyShift, curry_1_5!(action_176)),
        237 => partial_5!(happyShift, curry_1_5!(action_140)),
        239 => partial_5!(happyShift, curry_1_5!(action_142)),
        64 => partial_5!(happyGoto, curry_1_5!(action_435)),
        75 => partial_5!(happyGoto, curry_1_5!(action_508)),
        76 => partial_5!(happyGoto, curry_1_5!(action_102)),
        77 => partial_5!(happyGoto, curry_1_5!(action_103)),
        78 => partial_5!(happyGoto, curry_1_5!(action_481)),
        134 => partial_5!(happyGoto, curry_1_5!(action_167)),
        _ => happyReduce_472()
    }
}

fn action_782(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_571)),
        150 => partial_5!(happyShift, curry_1_5!(action_572)),
        186 => partial_5!(happyShift, curry_1_5!(action_171)),
        193 => partial_5!(happyShift, curry_1_5!(action_172)),
        213 => partial_5!(happyShift, curry_1_5!(action_173)),
        214 => partial_5!(happyShift, curry_1_5!(action_174)),
        216 => partial_5!(happyShift, curry_1_5!(action_175)),
        231 => partial_5!(happyShift, curry_1_5!(action_176)),
        237 => partial_5!(happyShift, curry_1_5!(action_140)),
        238 => partial_5!(happyShift, curry_1_5!(action_229)),
        239 => partial_5!(happyShift, curry_1_5!(action_142)),
        64 => partial_5!(happyGoto, curry_1_5!(action_435)),
        69 => partial_5!(happyGoto, curry_1_5!(action_698)),
        70 => partial_5!(happyGoto, curry_1_5!(action_221)),
        71 => partial_5!(happyGoto, curry_1_5!(action_222)),
        75 => partial_5!(happyGoto, curry_1_5!(action_508)),
        76 => partial_5!(happyGoto, curry_1_5!(action_102)),
        77 => partial_5!(happyGoto, curry_1_5!(action_103)),
        78 => partial_5!(happyGoto, curry_1_5!(action_481)),
        134 => partial_5!(happyGoto, curry_1_5!(action_167)),
        _ => happyReduce_472()
    }
}

fn action_783(i: isize) -> ActionReturn {
    match i {
        141 => partial_5!(happyShift, curry_1_5!(action_890)),
        _ => box happyFail
    }
}

fn action_784(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_319()
    }
}

fn action_785(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_323()
    }
}

fn action_786(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_26)),
        144 => partial_5!(happyShift, curry_1_5!(action_27)),
        145 => partial_5!(happyShift, curry_1_5!(action_28)),
        146 => partial_5!(happyShift, curry_1_5!(action_29)),
        147 => partial_5!(happyShift, curry_1_5!(action_30)),
        148 => partial_5!(happyShift, curry_1_5!(action_31)),
        149 => partial_5!(happyShift, curry_1_5!(action_32)),
        150 => partial_5!(happyShift, curry_1_5!(action_33)),
        153 => partial_5!(happyShift, curry_1_5!(action_34)),
        164 => partial_5!(happyShift, curry_1_5!(action_35)),
        184 => partial_5!(happyShift, curry_1_5!(action_36)),
        204 => partial_5!(happyShift, curry_1_5!(action_37)),
        220 => partial_5!(happyShift, curry_1_5!(action_38)),
        233 => partial_5!(happyShift, curry_1_5!(action_39)),
        234 => partial_5!(happyShift, curry_1_5!(action_40)),
        235 => partial_5!(happyShift, curry_1_5!(action_41)),
        236 => partial_5!(happyShift, curry_1_5!(action_42)),
        237 => partial_5!(happyShift, curry_1_5!(action_43)),
        240 => partial_5!(happyShift, curry_1_5!(action_44)),
        241 => partial_5!(happyShift, curry_1_5!(action_45)),
        242 => partial_5!(happyShift, curry_1_5!(action_46)),
        243 => partial_5!(happyShift, curry_1_5!(action_47)),
        244 => partial_5!(happyShift, curry_1_5!(action_48)),
        245 => partial_5!(happyShift, curry_1_5!(action_49)),
        100 => partial_5!(happyGoto, curry_1_5!(action_6)),
        104 => partial_5!(happyGoto, curry_1_5!(action_7)),
        106 => partial_5!(happyGoto, curry_1_5!(action_8)),
        107 => partial_5!(happyGoto, curry_1_5!(action_9)),
        108 => partial_5!(happyGoto, curry_1_5!(action_10)),
        109 => partial_5!(happyGoto, curry_1_5!(action_11)),
        110 => partial_5!(happyGoto, curry_1_5!(action_12)),
        111 => partial_5!(happyGoto, curry_1_5!(action_13)),
        112 => partial_5!(happyGoto, curry_1_5!(action_14)),
        113 => partial_5!(happyGoto, curry_1_5!(action_15)),
        114 => partial_5!(happyGoto, curry_1_5!(action_16)),
        115 => partial_5!(happyGoto, curry_1_5!(action_17)),
        116 => partial_5!(happyGoto, curry_1_5!(action_18)),
        117 => partial_5!(happyGoto, curry_1_5!(action_19)),
        118 => partial_5!(happyGoto, curry_1_5!(action_20)),
        119 => partial_5!(happyGoto, curry_1_5!(action_21)),
        120 => partial_5!(happyGoto, curry_1_5!(action_889)),
        127 => partial_5!(happyGoto, curry_1_5!(action_24)),
        128 => partial_5!(happyGoto, curry_1_5!(action_25)),
        _ => box happyFail
    }
}

fn action_787(i: isize) -> ActionReturn {
    match i {
        141 => partial_5!(happyShift, curry_1_5!(action_888)),
        _ => box happyFail
    }
}

fn action_788(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_318()
    }
}

fn action_789(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_324()
    }
}

fn action_790(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_180()
    }
}

fn action_791(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_181()
    }
}

fn action_792(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_196()
    }
}

fn action_793(i: isize) -> ActionReturn {
    match i {
        239 => partial_5!(happyShift, curry_1_5!(action_142)),
        134 => partial_5!(happyGoto, curry_1_5!(action_887)),
        _ => happyReduce_198()
    }
}

fn action_794(i: isize) -> ActionReturn {
    match i {
        167 => partial_5!(happyShift, curry_1_5!(action_886)),
        _ => happyReduce_206()
    }
}

fn action_795(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_26)),
        144 => partial_5!(happyShift, curry_1_5!(action_27)),
        145 => partial_5!(happyShift, curry_1_5!(action_28)),
        146 => partial_5!(happyShift, curry_1_5!(action_29)),
        147 => partial_5!(happyShift, curry_1_5!(action_30)),
        148 => partial_5!(happyShift, curry_1_5!(action_31)),
        149 => partial_5!(happyShift, curry_1_5!(action_32)),
        150 => partial_5!(happyShift, curry_1_5!(action_33)),
        153 => partial_5!(happyShift, curry_1_5!(action_34)),
        164 => partial_5!(happyShift, curry_1_5!(action_35)),
        184 => partial_5!(happyShift, curry_1_5!(action_36)),
        204 => partial_5!(happyShift, curry_1_5!(action_37)),
        220 => partial_5!(happyShift, curry_1_5!(action_38)),
        233 => partial_5!(happyShift, curry_1_5!(action_39)),
        234 => partial_5!(happyShift, curry_1_5!(action_40)),
        235 => partial_5!(happyShift, curry_1_5!(action_41)),
        236 => partial_5!(happyShift, curry_1_5!(action_42)),
        237 => partial_5!(happyShift, curry_1_5!(action_43)),
        240 => partial_5!(happyShift, curry_1_5!(action_44)),
        241 => partial_5!(happyShift, curry_1_5!(action_45)),
        242 => partial_5!(happyShift, curry_1_5!(action_46)),
        243 => partial_5!(happyShift, curry_1_5!(action_47)),
        244 => partial_5!(happyShift, curry_1_5!(action_48)),
        245 => partial_5!(happyShift, curry_1_5!(action_49)),
        100 => partial_5!(happyGoto, curry_1_5!(action_6)),
        104 => partial_5!(happyGoto, curry_1_5!(action_7)),
        106 => partial_5!(happyGoto, curry_1_5!(action_259)),
        107 => partial_5!(happyGoto, curry_1_5!(action_9)),
        108 => partial_5!(happyGoto, curry_1_5!(action_10)),
        109 => partial_5!(happyGoto, curry_1_5!(action_11)),
        110 => partial_5!(happyGoto, curry_1_5!(action_12)),
        111 => partial_5!(happyGoto, curry_1_5!(action_13)),
        112 => partial_5!(happyGoto, curry_1_5!(action_14)),
        113 => partial_5!(happyGoto, curry_1_5!(action_15)),
        114 => partial_5!(happyGoto, curry_1_5!(action_16)),
        115 => partial_5!(happyGoto, curry_1_5!(action_17)),
        116 => partial_5!(happyGoto, curry_1_5!(action_18)),
        117 => partial_5!(happyGoto, curry_1_5!(action_19)),
        118 => partial_5!(happyGoto, curry_1_5!(action_20)),
        119 => partial_5!(happyGoto, curry_1_5!(action_260)),
        126 => partial_5!(happyGoto, curry_1_5!(action_885)),
        127 => partial_5!(happyGoto, curry_1_5!(action_24)),
        128 => partial_5!(happyGoto, curry_1_5!(action_25)),
        _ => box happyFail
    }
}

fn action_796(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_475)),
        150 => partial_5!(happyShift, curry_1_5!(action_476)),
        167 => partial_5!(happyShift, curry_1_5!(action_795)),
        237 => partial_5!(happyShift, curry_1_5!(action_140)),
        60 => partial_5!(happyGoto, curry_1_5!(action_884)),
        75 => partial_5!(happyGoto, curry_1_5!(action_794)),
        76 => partial_5!(happyGoto, curry_1_5!(action_102)),
        77 => partial_5!(happyGoto, curry_1_5!(action_103)),
        78 => partial_5!(happyGoto, curry_1_5!(action_481)),
        _ => box happyFail
    }
}

fn action_797(i: isize) -> ActionReturn {
    match i {
        186 => partial_5!(happyShift, curry_1_5!(action_171)),
        190 => partial_5!(happyShift, curry_1_5!(action_116)),
        192 => partial_5!(happyShift, curry_1_5!(action_117)),
        193 => partial_5!(happyShift, curry_1_5!(action_172)),
        195 => partial_5!(happyShift, curry_1_5!(action_118)),
        198 => partial_5!(happyShift, curry_1_5!(action_119)),
        200 => partial_5!(happyShift, curry_1_5!(action_120)),
        202 => partial_5!(happyShift, curry_1_5!(action_122)),
        208 => partial_5!(happyShift, curry_1_5!(action_124)),
        209 => partial_5!(happyShift, curry_1_5!(action_125)),
        210 => partial_5!(happyShift, curry_1_5!(action_126)),
        213 => partial_5!(happyShift, curry_1_5!(action_173)),
        214 => partial_5!(happyShift, curry_1_5!(action_174)),
        216 => partial_5!(happyShift, curry_1_5!(action_175)),
        218 => partial_5!(happyShift, curry_1_5!(action_129)),
        219 => partial_5!(happyShift, curry_1_5!(action_130)),
        223 => partial_5!(happyShift, curry_1_5!(action_133)),
        226 => partial_5!(happyShift, curry_1_5!(action_438)),
        228 => partial_5!(happyShift, curry_1_5!(action_137)),
        229 => partial_5!(happyShift, curry_1_5!(action_138)),
        230 => partial_5!(happyShift, curry_1_5!(action_139)),
        231 => partial_5!(happyShift, curry_1_5!(action_176)),
        238 => partial_5!(happyShift, curry_1_5!(action_439)),
        239 => partial_5!(happyShift, curry_1_5!(action_142)),
        45 => partial_5!(happyGoto, curry_1_5!(action_433)),
        52 => partial_5!(happyGoto, curry_1_5!(action_434)),
        53 => partial_5!(happyGoto, curry_1_5!(action_97)),
        54 => partial_5!(happyGoto, curry_1_5!(action_98)),
        61 => partial_5!(happyGoto, curry_1_5!(action_99)),
        64 => partial_5!(happyGoto, curry_1_5!(action_435)),
        134 => partial_5!(happyGoto, curry_1_5!(action_167)),
        _ => happyReduce_472()
    }
}

fn action_798(i: isize) -> ActionReturn {
    match i {
        239 => partial_5!(happyShift, curry_1_5!(action_142)),
        132 => partial_5!(happyGoto, curry_1_5!(action_883)),
        133 => partial_5!(happyGoto, curry_1_5!(action_150)),
        134 => partial_5!(happyGoto, curry_1_5!(action_110)),
        _ => happyReduce_471()
    }
}

fn action_799(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_194()
    }
}

fn action_800(i: isize) -> ActionReturn {
    match i {
        239 => partial_5!(happyShift, curry_1_5!(action_142)),
        132 => partial_5!(happyGoto, curry_1_5!(action_882)),
        133 => partial_5!(happyGoto, curry_1_5!(action_150)),
        134 => partial_5!(happyGoto, curry_1_5!(action_110)),
        _ => happyReduce_471()
    }
}

fn action_801(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_195()
    }
}

fn action_802(i: isize) -> ActionReturn {
    match i {
        239 => partial_5!(happyShift, curry_1_5!(action_142)),
        132 => partial_5!(happyGoto, curry_1_5!(action_881)),
        133 => partial_5!(happyGoto, curry_1_5!(action_150)),
        134 => partial_5!(happyGoto, curry_1_5!(action_110)),
        _ => happyReduce_471()
    }
}

fn action_803(i: isize) -> ActionReturn {
    match i {
        167 => partial_5!(happyShift, curry_1_5!(action_880)),
        _ => happyReduce_203()
    }
}

fn action_804(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_26)),
        144 => partial_5!(happyShift, curry_1_5!(action_27)),
        145 => partial_5!(happyShift, curry_1_5!(action_28)),
        146 => partial_5!(happyShift, curry_1_5!(action_29)),
        147 => partial_5!(happyShift, curry_1_5!(action_30)),
        148 => partial_5!(happyShift, curry_1_5!(action_31)),
        149 => partial_5!(happyShift, curry_1_5!(action_32)),
        150 => partial_5!(happyShift, curry_1_5!(action_33)),
        153 => partial_5!(happyShift, curry_1_5!(action_34)),
        164 => partial_5!(happyShift, curry_1_5!(action_35)),
        184 => partial_5!(happyShift, curry_1_5!(action_36)),
        204 => partial_5!(happyShift, curry_1_5!(action_37)),
        220 => partial_5!(happyShift, curry_1_5!(action_38)),
        233 => partial_5!(happyShift, curry_1_5!(action_39)),
        234 => partial_5!(happyShift, curry_1_5!(action_40)),
        235 => partial_5!(happyShift, curry_1_5!(action_41)),
        236 => partial_5!(happyShift, curry_1_5!(action_42)),
        237 => partial_5!(happyShift, curry_1_5!(action_43)),
        240 => partial_5!(happyShift, curry_1_5!(action_44)),
        241 => partial_5!(happyShift, curry_1_5!(action_45)),
        242 => partial_5!(happyShift, curry_1_5!(action_46)),
        243 => partial_5!(happyShift, curry_1_5!(action_47)),
        244 => partial_5!(happyShift, curry_1_5!(action_48)),
        245 => partial_5!(happyShift, curry_1_5!(action_49)),
        100 => partial_5!(happyGoto, curry_1_5!(action_6)),
        104 => partial_5!(happyGoto, curry_1_5!(action_7)),
        106 => partial_5!(happyGoto, curry_1_5!(action_259)),
        107 => partial_5!(happyGoto, curry_1_5!(action_9)),
        108 => partial_5!(happyGoto, curry_1_5!(action_10)),
        109 => partial_5!(happyGoto, curry_1_5!(action_11)),
        110 => partial_5!(happyGoto, curry_1_5!(action_12)),
        111 => partial_5!(happyGoto, curry_1_5!(action_13)),
        112 => partial_5!(happyGoto, curry_1_5!(action_14)),
        113 => partial_5!(happyGoto, curry_1_5!(action_15)),
        114 => partial_5!(happyGoto, curry_1_5!(action_16)),
        115 => partial_5!(happyGoto, curry_1_5!(action_17)),
        116 => partial_5!(happyGoto, curry_1_5!(action_18)),
        117 => partial_5!(happyGoto, curry_1_5!(action_19)),
        118 => partial_5!(happyGoto, curry_1_5!(action_20)),
        119 => partial_5!(happyGoto, curry_1_5!(action_260)),
        126 => partial_5!(happyGoto, curry_1_5!(action_879)),
        127 => partial_5!(happyGoto, curry_1_5!(action_24)),
        128 => partial_5!(happyGoto, curry_1_5!(action_25)),
        _ => box happyFail
    }
}

fn action_805(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_186()
    }
}

fn action_806(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_247()
    }
}

fn action_807(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_571)),
        150 => partial_5!(happyShift, curry_1_5!(action_572)),
        186 => partial_5!(happyShift, curry_1_5!(action_171)),
        193 => partial_5!(happyShift, curry_1_5!(action_172)),
        213 => partial_5!(happyShift, curry_1_5!(action_173)),
        214 => partial_5!(happyShift, curry_1_5!(action_174)),
        216 => partial_5!(happyShift, curry_1_5!(action_175)),
        231 => partial_5!(happyShift, curry_1_5!(action_176)),
        237 => partial_5!(happyShift, curry_1_5!(action_140)),
        238 => partial_5!(happyShift, curry_1_5!(action_229)),
        239 => partial_5!(happyShift, curry_1_5!(action_142)),
        64 => partial_5!(happyGoto, curry_1_5!(action_435)),
        69 => partial_5!(happyGoto, curry_1_5!(action_698)),
        70 => partial_5!(happyGoto, curry_1_5!(action_221)),
        71 => partial_5!(happyGoto, curry_1_5!(action_222)),
        75 => partial_5!(happyGoto, curry_1_5!(action_508)),
        76 => partial_5!(happyGoto, curry_1_5!(action_102)),
        77 => partial_5!(happyGoto, curry_1_5!(action_103)),
        78 => partial_5!(happyGoto, curry_1_5!(action_481)),
        134 => partial_5!(happyGoto, curry_1_5!(action_167)),
        _ => box happyFail
    }
}

fn action_808(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_413)),
        139 => partial_5!(happyShift, curry_1_5!(action_878)),
        140 => partial_5!(happyShift, curry_1_5!(action_182)),
        88 => partial_5!(happyGoto, curry_1_5!(action_573)),
        89 => partial_5!(happyGoto, curry_1_5!(action_179)),
        90 => partial_5!(happyGoto, curry_1_5!(action_180)),
        _ => box happyFail
    }
}

fn action_809(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_250()
    }
}

fn action_810(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_233()
    }
}

fn action_811(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_100()
    }
}

fn action_812(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_96()
    }
}

fn action_813(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_26)),
        144 => partial_5!(happyShift, curry_1_5!(action_27)),
        145 => partial_5!(happyShift, curry_1_5!(action_28)),
        146 => partial_5!(happyShift, curry_1_5!(action_29)),
        147 => partial_5!(happyShift, curry_1_5!(action_30)),
        148 => partial_5!(happyShift, curry_1_5!(action_31)),
        149 => partial_5!(happyShift, curry_1_5!(action_32)),
        150 => partial_5!(happyShift, curry_1_5!(action_33)),
        153 => partial_5!(happyShift, curry_1_5!(action_34)),
        164 => partial_5!(happyShift, curry_1_5!(action_35)),
        180 => partial_5!(happyShift, curry_1_5!(action_60)),
        181 => partial_5!(happyShift, curry_1_5!(action_61)),
        184 => partial_5!(happyShift, curry_1_5!(action_36)),
        187 => partial_5!(happyShift, curry_1_5!(action_62)),
        189 => partial_5!(happyShift, curry_1_5!(action_63)),
        191 => partial_5!(happyShift, curry_1_5!(action_64)),
        194 => partial_5!(happyShift, curry_1_5!(action_65)),
        196 => partial_5!(happyShift, curry_1_5!(action_66)),
        197 => partial_5!(happyShift, curry_1_5!(action_67)),
        203 => partial_5!(happyShift, curry_1_5!(action_68)),
        204 => partial_5!(happyShift, curry_1_5!(action_37)),
        205 => partial_5!(happyShift, curry_1_5!(action_69)),
        206 => partial_5!(happyShift, curry_1_5!(action_70)),
        217 => partial_5!(happyShift, curry_1_5!(action_71)),
        220 => partial_5!(happyShift, curry_1_5!(action_38)),
        224 => partial_5!(happyShift, curry_1_5!(action_72)),
        232 => partial_5!(happyShift, curry_1_5!(action_73)),
        233 => partial_5!(happyShift, curry_1_5!(action_39)),
        234 => partial_5!(happyShift, curry_1_5!(action_40)),
        235 => partial_5!(happyShift, curry_1_5!(action_41)),
        236 => partial_5!(happyShift, curry_1_5!(action_42)),
        237 => partial_5!(happyShift, curry_1_5!(action_74)),
        238 => partial_5!(happyShift, curry_1_5!(action_75)),
        240 => partial_5!(happyShift, curry_1_5!(action_44)),
        241 => partial_5!(happyShift, curry_1_5!(action_45)),
        242 => partial_5!(happyShift, curry_1_5!(action_46)),
        243 => partial_5!(happyShift, curry_1_5!(action_47)),
        244 => partial_5!(happyShift, curry_1_5!(action_48)),
        245 => partial_5!(happyShift, curry_1_5!(action_49)),
        12 => partial_5!(happyGoto, curry_1_5!(action_877)),
        13 => partial_5!(happyGoto, curry_1_5!(action_51)),
        14 => partial_5!(happyGoto, curry_1_5!(action_52)),
        22 => partial_5!(happyGoto, curry_1_5!(action_53)),
        23 => partial_5!(happyGoto, curry_1_5!(action_54)),
        24 => partial_5!(happyGoto, curry_1_5!(action_55)),
        25 => partial_5!(happyGoto, curry_1_5!(action_56)),
        26 => partial_5!(happyGoto, curry_1_5!(action_57)),
        100 => partial_5!(happyGoto, curry_1_5!(action_6)),
        104 => partial_5!(happyGoto, curry_1_5!(action_7)),
        106 => partial_5!(happyGoto, curry_1_5!(action_8)),
        107 => partial_5!(happyGoto, curry_1_5!(action_9)),
        108 => partial_5!(happyGoto, curry_1_5!(action_10)),
        109 => partial_5!(happyGoto, curry_1_5!(action_11)),
        110 => partial_5!(happyGoto, curry_1_5!(action_12)),
        111 => partial_5!(happyGoto, curry_1_5!(action_13)),
        112 => partial_5!(happyGoto, curry_1_5!(action_14)),
        113 => partial_5!(happyGoto, curry_1_5!(action_15)),
        114 => partial_5!(happyGoto, curry_1_5!(action_16)),
        115 => partial_5!(happyGoto, curry_1_5!(action_17)),
        116 => partial_5!(happyGoto, curry_1_5!(action_18)),
        117 => partial_5!(happyGoto, curry_1_5!(action_19)),
        118 => partial_5!(happyGoto, curry_1_5!(action_20)),
        119 => partial_5!(happyGoto, curry_1_5!(action_21)),
        120 => partial_5!(happyGoto, curry_1_5!(action_22)),
        122 => partial_5!(happyGoto, curry_1_5!(action_58)),
        127 => partial_5!(happyGoto, curry_1_5!(action_24)),
        128 => partial_5!(happyGoto, curry_1_5!(action_25)),
        131 => partial_5!(happyGoto, curry_1_5!(action_59)),
        _ => box happyFail
    }
}

fn action_814(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_26)),
        144 => partial_5!(happyShift, curry_1_5!(action_27)),
        145 => partial_5!(happyShift, curry_1_5!(action_28)),
        146 => partial_5!(happyShift, curry_1_5!(action_29)),
        147 => partial_5!(happyShift, curry_1_5!(action_30)),
        148 => partial_5!(happyShift, curry_1_5!(action_31)),
        149 => partial_5!(happyShift, curry_1_5!(action_32)),
        150 => partial_5!(happyShift, curry_1_5!(action_33)),
        153 => partial_5!(happyShift, curry_1_5!(action_34)),
        164 => partial_5!(happyShift, curry_1_5!(action_35)),
        184 => partial_5!(happyShift, curry_1_5!(action_36)),
        204 => partial_5!(happyShift, curry_1_5!(action_37)),
        220 => partial_5!(happyShift, curry_1_5!(action_38)),
        233 => partial_5!(happyShift, curry_1_5!(action_39)),
        234 => partial_5!(happyShift, curry_1_5!(action_40)),
        235 => partial_5!(happyShift, curry_1_5!(action_41)),
        236 => partial_5!(happyShift, curry_1_5!(action_42)),
        237 => partial_5!(happyShift, curry_1_5!(action_43)),
        240 => partial_5!(happyShift, curry_1_5!(action_44)),
        241 => partial_5!(happyShift, curry_1_5!(action_45)),
        242 => partial_5!(happyShift, curry_1_5!(action_46)),
        243 => partial_5!(happyShift, curry_1_5!(action_47)),
        244 => partial_5!(happyShift, curry_1_5!(action_48)),
        245 => partial_5!(happyShift, curry_1_5!(action_49)),
        100 => partial_5!(happyGoto, curry_1_5!(action_6)),
        104 => partial_5!(happyGoto, curry_1_5!(action_7)),
        106 => partial_5!(happyGoto, curry_1_5!(action_8)),
        107 => partial_5!(happyGoto, curry_1_5!(action_9)),
        108 => partial_5!(happyGoto, curry_1_5!(action_10)),
        109 => partial_5!(happyGoto, curry_1_5!(action_11)),
        110 => partial_5!(happyGoto, curry_1_5!(action_12)),
        111 => partial_5!(happyGoto, curry_1_5!(action_13)),
        112 => partial_5!(happyGoto, curry_1_5!(action_14)),
        113 => partial_5!(happyGoto, curry_1_5!(action_15)),
        114 => partial_5!(happyGoto, curry_1_5!(action_16)),
        115 => partial_5!(happyGoto, curry_1_5!(action_17)),
        116 => partial_5!(happyGoto, curry_1_5!(action_18)),
        117 => partial_5!(happyGoto, curry_1_5!(action_19)),
        118 => partial_5!(happyGoto, curry_1_5!(action_20)),
        119 => partial_5!(happyGoto, curry_1_5!(action_21)),
        120 => partial_5!(happyGoto, curry_1_5!(action_22)),
        122 => partial_5!(happyGoto, curry_1_5!(action_249)),
        124 => partial_5!(happyGoto, curry_1_5!(action_876)),
        127 => partial_5!(happyGoto, curry_1_5!(action_24)),
        128 => partial_5!(happyGoto, curry_1_5!(action_25)),
        _ => happyReduce_456()
    }
}

fn action_815(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_26)),
        144 => partial_5!(happyShift, curry_1_5!(action_27)),
        145 => partial_5!(happyShift, curry_1_5!(action_28)),
        146 => partial_5!(happyShift, curry_1_5!(action_29)),
        147 => partial_5!(happyShift, curry_1_5!(action_30)),
        148 => partial_5!(happyShift, curry_1_5!(action_31)),
        149 => partial_5!(happyShift, curry_1_5!(action_32)),
        150 => partial_5!(happyShift, curry_1_5!(action_33)),
        153 => partial_5!(happyShift, curry_1_5!(action_34)),
        164 => partial_5!(happyShift, curry_1_5!(action_35)),
        184 => partial_5!(happyShift, curry_1_5!(action_36)),
        204 => partial_5!(happyShift, curry_1_5!(action_37)),
        220 => partial_5!(happyShift, curry_1_5!(action_38)),
        233 => partial_5!(happyShift, curry_1_5!(action_39)),
        234 => partial_5!(happyShift, curry_1_5!(action_40)),
        235 => partial_5!(happyShift, curry_1_5!(action_41)),
        236 => partial_5!(happyShift, curry_1_5!(action_42)),
        237 => partial_5!(happyShift, curry_1_5!(action_43)),
        240 => partial_5!(happyShift, curry_1_5!(action_44)),
        241 => partial_5!(happyShift, curry_1_5!(action_45)),
        242 => partial_5!(happyShift, curry_1_5!(action_46)),
        243 => partial_5!(happyShift, curry_1_5!(action_47)),
        244 => partial_5!(happyShift, curry_1_5!(action_48)),
        245 => partial_5!(happyShift, curry_1_5!(action_49)),
        100 => partial_5!(happyGoto, curry_1_5!(action_6)),
        104 => partial_5!(happyGoto, curry_1_5!(action_7)),
        106 => partial_5!(happyGoto, curry_1_5!(action_8)),
        107 => partial_5!(happyGoto, curry_1_5!(action_9)),
        108 => partial_5!(happyGoto, curry_1_5!(action_10)),
        109 => partial_5!(happyGoto, curry_1_5!(action_11)),
        110 => partial_5!(happyGoto, curry_1_5!(action_12)),
        111 => partial_5!(happyGoto, curry_1_5!(action_13)),
        112 => partial_5!(happyGoto, curry_1_5!(action_14)),
        113 => partial_5!(happyGoto, curry_1_5!(action_15)),
        114 => partial_5!(happyGoto, curry_1_5!(action_16)),
        115 => partial_5!(happyGoto, curry_1_5!(action_17)),
        116 => partial_5!(happyGoto, curry_1_5!(action_18)),
        117 => partial_5!(happyGoto, curry_1_5!(action_19)),
        118 => partial_5!(happyGoto, curry_1_5!(action_20)),
        119 => partial_5!(happyGoto, curry_1_5!(action_21)),
        120 => partial_5!(happyGoto, curry_1_5!(action_22)),
        122 => partial_5!(happyGoto, curry_1_5!(action_249)),
        124 => partial_5!(happyGoto, curry_1_5!(action_875)),
        127 => partial_5!(happyGoto, curry_1_5!(action_24)),
        128 => partial_5!(happyGoto, curry_1_5!(action_25)),
        _ => happyReduce_456()
    }
}

fn action_816(i: isize) -> ActionReturn {
    match i {
        180 => partial_5!(happyShift, curry_1_5!(action_874)),
        _ => box happyFail
    }
}

fn action_817(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_37()
    }
}

fn action_818(i: isize) -> ActionReturn {
    match i {
        139 => partial_5!(happyShift, curry_1_5!(action_872)),
        167 => partial_5!(happyShift, curry_1_5!(action_873)),
        _ => box happyFail
    }
}

fn action_819(i: isize) -> ActionReturn {
    match i {
        179 => partial_5!(happyShift, curry_1_5!(action_871)),
        _ => happyReduce_77()
    }
}

fn action_820(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_78()
    }
}

fn action_821(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_870)),
        _ => box happyFail
    }
}

fn action_822(i: isize) -> ActionReturn {
    match i {
        237 => partial_5!(happyShift, curry_1_5!(action_868)),
        238 => partial_5!(happyShift, curry_1_5!(action_869)),
        _ => box happyFail
    }
}

fn action_823(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_70()
    }
}

fn action_824(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_39()
    }
}

fn action_825(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_55()
    }
}

fn action_826(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_49()
    }
}

fn action_827(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_51()
    }
}

fn action_828(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_50()
    }
}

fn action_829(i: isize) -> ActionReturn {
    match i {
        181 => partial_5!(happyShift, curry_1_5!(action_61)),
        14 => partial_5!(happyGoto, curry_1_5!(action_867)),
        _ => box happyFail
    }
}

fn action_830(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_52()
    }
}

fn action_831(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_368()
    }
}

fn action_832(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_367()
    }
}

fn action_833(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_26)),
        144 => partial_5!(happyShift, curry_1_5!(action_27)),
        145 => partial_5!(happyShift, curry_1_5!(action_28)),
        146 => partial_5!(happyShift, curry_1_5!(action_29)),
        147 => partial_5!(happyShift, curry_1_5!(action_30)),
        148 => partial_5!(happyShift, curry_1_5!(action_31)),
        149 => partial_5!(happyShift, curry_1_5!(action_32)),
        150 => partial_5!(happyShift, curry_1_5!(action_33)),
        153 => partial_5!(happyShift, curry_1_5!(action_34)),
        164 => partial_5!(happyShift, curry_1_5!(action_35)),
        184 => partial_5!(happyShift, curry_1_5!(action_36)),
        204 => partial_5!(happyShift, curry_1_5!(action_37)),
        220 => partial_5!(happyShift, curry_1_5!(action_38)),
        233 => partial_5!(happyShift, curry_1_5!(action_39)),
        234 => partial_5!(happyShift, curry_1_5!(action_40)),
        235 => partial_5!(happyShift, curry_1_5!(action_41)),
        236 => partial_5!(happyShift, curry_1_5!(action_42)),
        237 => partial_5!(happyShift, curry_1_5!(action_43)),
        240 => partial_5!(happyShift, curry_1_5!(action_44)),
        241 => partial_5!(happyShift, curry_1_5!(action_45)),
        242 => partial_5!(happyShift, curry_1_5!(action_46)),
        243 => partial_5!(happyShift, curry_1_5!(action_47)),
        244 => partial_5!(happyShift, curry_1_5!(action_48)),
        245 => partial_5!(happyShift, curry_1_5!(action_49)),
        100 => partial_5!(happyGoto, curry_1_5!(action_6)),
        104 => partial_5!(happyGoto, curry_1_5!(action_7)),
        106 => partial_5!(happyGoto, curry_1_5!(action_8)),
        107 => partial_5!(happyGoto, curry_1_5!(action_9)),
        108 => partial_5!(happyGoto, curry_1_5!(action_10)),
        109 => partial_5!(happyGoto, curry_1_5!(action_11)),
        110 => partial_5!(happyGoto, curry_1_5!(action_12)),
        111 => partial_5!(happyGoto, curry_1_5!(action_13)),
        112 => partial_5!(happyGoto, curry_1_5!(action_14)),
        113 => partial_5!(happyGoto, curry_1_5!(action_15)),
        114 => partial_5!(happyGoto, curry_1_5!(action_16)),
        115 => partial_5!(happyGoto, curry_1_5!(action_17)),
        116 => partial_5!(happyGoto, curry_1_5!(action_18)),
        117 => partial_5!(happyGoto, curry_1_5!(action_19)),
        118 => partial_5!(happyGoto, curry_1_5!(action_20)),
        119 => partial_5!(happyGoto, curry_1_5!(action_21)),
        120 => partial_5!(happyGoto, curry_1_5!(action_22)),
        122 => partial_5!(happyGoto, curry_1_5!(action_866)),
        127 => partial_5!(happyGoto, curry_1_5!(action_24)),
        128 => partial_5!(happyGoto, curry_1_5!(action_25)),
        _ => box happyFail
    }
}

fn action_834(i: isize) -> ActionReturn {
    match i {
        237 => partial_5!(happyShift, curry_1_5!(action_254)),
        238 => partial_5!(happyShift, curry_1_5!(action_75)),
        131 => partial_5!(happyGoto, curry_1_5!(action_865)),
        _ => box happyFail
    }
}

fn action_835(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_366()
    }
}

fn action_836(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_26)),
        144 => partial_5!(happyShift, curry_1_5!(action_27)),
        145 => partial_5!(happyShift, curry_1_5!(action_28)),
        146 => partial_5!(happyShift, curry_1_5!(action_29)),
        147 => partial_5!(happyShift, curry_1_5!(action_30)),
        148 => partial_5!(happyShift, curry_1_5!(action_31)),
        149 => partial_5!(happyShift, curry_1_5!(action_32)),
        150 => partial_5!(happyShift, curry_1_5!(action_33)),
        153 => partial_5!(happyShift, curry_1_5!(action_34)),
        164 => partial_5!(happyShift, curry_1_5!(action_35)),
        184 => partial_5!(happyShift, curry_1_5!(action_36)),
        204 => partial_5!(happyShift, curry_1_5!(action_37)),
        220 => partial_5!(happyShift, curry_1_5!(action_38)),
        233 => partial_5!(happyShift, curry_1_5!(action_39)),
        234 => partial_5!(happyShift, curry_1_5!(action_40)),
        235 => partial_5!(happyShift, curry_1_5!(action_41)),
        236 => partial_5!(happyShift, curry_1_5!(action_42)),
        237 => partial_5!(happyShift, curry_1_5!(action_43)),
        240 => partial_5!(happyShift, curry_1_5!(action_44)),
        241 => partial_5!(happyShift, curry_1_5!(action_45)),
        242 => partial_5!(happyShift, curry_1_5!(action_46)),
        243 => partial_5!(happyShift, curry_1_5!(action_47)),
        244 => partial_5!(happyShift, curry_1_5!(action_48)),
        245 => partial_5!(happyShift, curry_1_5!(action_49)),
        100 => partial_5!(happyGoto, curry_1_5!(action_6)),
        104 => partial_5!(happyGoto, curry_1_5!(action_7)),
        106 => partial_5!(happyGoto, curry_1_5!(action_8)),
        107 => partial_5!(happyGoto, curry_1_5!(action_9)),
        108 => partial_5!(happyGoto, curry_1_5!(action_10)),
        109 => partial_5!(happyGoto, curry_1_5!(action_11)),
        110 => partial_5!(happyGoto, curry_1_5!(action_12)),
        111 => partial_5!(happyGoto, curry_1_5!(action_13)),
        112 => partial_5!(happyGoto, curry_1_5!(action_14)),
        113 => partial_5!(happyGoto, curry_1_5!(action_15)),
        114 => partial_5!(happyGoto, curry_1_5!(action_16)),
        115 => partial_5!(happyGoto, curry_1_5!(action_17)),
        116 => partial_5!(happyGoto, curry_1_5!(action_18)),
        117 => partial_5!(happyGoto, curry_1_5!(action_19)),
        118 => partial_5!(happyGoto, curry_1_5!(action_20)),
        119 => partial_5!(happyGoto, curry_1_5!(action_21)),
        120 => partial_5!(happyGoto, curry_1_5!(action_864)),
        127 => partial_5!(happyGoto, curry_1_5!(action_24)),
        128 => partial_5!(happyGoto, curry_1_5!(action_25)),
        _ => box happyFail
    }
}

fn action_837(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_364()
    }
}

fn action_838(i: isize) -> ActionReturn {
    match i {
        190 => partial_5!(happyShift, curry_1_5!(action_116)),
        192 => partial_5!(happyShift, curry_1_5!(action_117)),
        195 => partial_5!(happyShift, curry_1_5!(action_118)),
        196 => partial_5!(happyShift, curry_1_5!(action_668)),
        198 => partial_5!(happyShift, curry_1_5!(action_119)),
        200 => partial_5!(happyShift, curry_1_5!(action_120)),
        202 => partial_5!(happyShift, curry_1_5!(action_122)),
        208 => partial_5!(happyShift, curry_1_5!(action_124)),
        209 => partial_5!(happyShift, curry_1_5!(action_125)),
        210 => partial_5!(happyShift, curry_1_5!(action_126)),
        218 => partial_5!(happyShift, curry_1_5!(action_129)),
        219 => partial_5!(happyShift, curry_1_5!(action_130)),
        223 => partial_5!(happyShift, curry_1_5!(action_133)),
        226 => partial_5!(happyShift, curry_1_5!(action_135)),
        228 => partial_5!(happyShift, curry_1_5!(action_137)),
        229 => partial_5!(happyShift, curry_1_5!(action_138)),
        230 => partial_5!(happyShift, curry_1_5!(action_139)),
        238 => partial_5!(happyShift, curry_1_5!(action_141)),
        239 => partial_5!(happyShift, curry_1_5!(action_142)),
        44 => partial_5!(happyGoto, curry_1_5!(action_286)),
        45 => partial_5!(happyGoto, curry_1_5!(action_89)),
        47 => partial_5!(happyGoto, curry_1_5!(action_287)),
        49 => partial_5!(happyGoto, curry_1_5!(action_288)),
        51 => partial_5!(happyGoto, curry_1_5!(action_289)),
        52 => partial_5!(happyGoto, curry_1_5!(action_96)),
        53 => partial_5!(happyGoto, curry_1_5!(action_97)),
        54 => partial_5!(happyGoto, curry_1_5!(action_98)),
        61 => partial_5!(happyGoto, curry_1_5!(action_99)),
        65 => partial_5!(happyGoto, curry_1_5!(action_290)),
        86 => partial_5!(happyGoto, curry_1_5!(action_665)),
        102 => partial_5!(happyGoto, curry_1_5!(action_863)),
        132 => partial_5!(happyGoto, curry_1_5!(action_108)),
        133 => partial_5!(happyGoto, curry_1_5!(action_293)),
        134 => partial_5!(happyGoto, curry_1_5!(action_110)),
        _ => happyReduce_471()
    }
}

fn action_839(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_26)),
        144 => partial_5!(happyShift, curry_1_5!(action_27)),
        145 => partial_5!(happyShift, curry_1_5!(action_28)),
        146 => partial_5!(happyShift, curry_1_5!(action_29)),
        147 => partial_5!(happyShift, curry_1_5!(action_30)),
        148 => partial_5!(happyShift, curry_1_5!(action_31)),
        149 => partial_5!(happyShift, curry_1_5!(action_32)),
        150 => partial_5!(happyShift, curry_1_5!(action_33)),
        153 => partial_5!(happyShift, curry_1_5!(action_34)),
        164 => partial_5!(happyShift, curry_1_5!(action_35)),
        184 => partial_5!(happyShift, curry_1_5!(action_36)),
        204 => partial_5!(happyShift, curry_1_5!(action_37)),
        220 => partial_5!(happyShift, curry_1_5!(action_38)),
        233 => partial_5!(happyShift, curry_1_5!(action_39)),
        234 => partial_5!(happyShift, curry_1_5!(action_40)),
        235 => partial_5!(happyShift, curry_1_5!(action_41)),
        236 => partial_5!(happyShift, curry_1_5!(action_42)),
        237 => partial_5!(happyShift, curry_1_5!(action_43)),
        240 => partial_5!(happyShift, curry_1_5!(action_44)),
        241 => partial_5!(happyShift, curry_1_5!(action_45)),
        242 => partial_5!(happyShift, curry_1_5!(action_46)),
        243 => partial_5!(happyShift, curry_1_5!(action_47)),
        244 => partial_5!(happyShift, curry_1_5!(action_48)),
        245 => partial_5!(happyShift, curry_1_5!(action_49)),
        100 => partial_5!(happyGoto, curry_1_5!(action_6)),
        104 => partial_5!(happyGoto, curry_1_5!(action_7)),
        106 => partial_5!(happyGoto, curry_1_5!(action_8)),
        107 => partial_5!(happyGoto, curry_1_5!(action_9)),
        108 => partial_5!(happyGoto, curry_1_5!(action_10)),
        109 => partial_5!(happyGoto, curry_1_5!(action_11)),
        110 => partial_5!(happyGoto, curry_1_5!(action_12)),
        111 => partial_5!(happyGoto, curry_1_5!(action_13)),
        112 => partial_5!(happyGoto, curry_1_5!(action_14)),
        113 => partial_5!(happyGoto, curry_1_5!(action_15)),
        114 => partial_5!(happyGoto, curry_1_5!(action_16)),
        115 => partial_5!(happyGoto, curry_1_5!(action_17)),
        116 => partial_5!(happyGoto, curry_1_5!(action_18)),
        117 => partial_5!(happyGoto, curry_1_5!(action_19)),
        118 => partial_5!(happyGoto, curry_1_5!(action_20)),
        119 => partial_5!(happyGoto, curry_1_5!(action_21)),
        120 => partial_5!(happyGoto, curry_1_5!(action_862)),
        127 => partial_5!(happyGoto, curry_1_5!(action_24)),
        128 => partial_5!(happyGoto, curry_1_5!(action_25)),
        _ => box happyFail
    }
}

fn action_840(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_335()
    }
}

fn action_841(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_337()
    }
}

fn action_842(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_413)),
        140 => partial_5!(happyShift, curry_1_5!(action_182)),
        88 => partial_5!(happyGoto, curry_1_5!(action_861)),
        89 => partial_5!(happyGoto, curry_1_5!(action_179)),
        90 => partial_5!(happyGoto, curry_1_5!(action_180)),
        _ => happyReduce_336()
    }
}

fn action_843(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_338()
    }
}

fn action_844(i: isize) -> ActionReturn {
    match i {
        179 => partial_5!(happyShift, curry_1_5!(action_859)),
        182 => partial_5!(happyShift, curry_1_5!(action_860)),
        _ => box happyFail
    }
}

fn action_845(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_357()
    }
}

fn action_846(i: isize) -> ActionReturn {
    match i {
        141 => partial_5!(happyShift, curry_1_5!(action_857)),
        183 => partial_5!(happyShift, curry_1_5!(action_858)),
        _ => box happyFail
    }
}

fn action_847(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_352()
    }
}

fn action_848(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_355()
    }
}

fn action_849(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_358()
    }
}

fn action_850(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_351()
    }
}

fn action_851(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_348()
    }
}

fn action_852(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_26)),
        140 => partial_5!(happyShift, curry_1_5!(action_652)),
        143 => partial_5!(happyShift, curry_1_5!(action_653)),
        144 => partial_5!(happyShift, curry_1_5!(action_27)),
        145 => partial_5!(happyShift, curry_1_5!(action_28)),
        146 => partial_5!(happyShift, curry_1_5!(action_29)),
        147 => partial_5!(happyShift, curry_1_5!(action_30)),
        148 => partial_5!(happyShift, curry_1_5!(action_31)),
        149 => partial_5!(happyShift, curry_1_5!(action_32)),
        150 => partial_5!(happyShift, curry_1_5!(action_33)),
        153 => partial_5!(happyShift, curry_1_5!(action_34)),
        164 => partial_5!(happyShift, curry_1_5!(action_35)),
        181 => partial_5!(happyShift, curry_1_5!(action_654)),
        182 => partial_5!(happyShift, curry_1_5!(action_856)),
        184 => partial_5!(happyShift, curry_1_5!(action_36)),
        204 => partial_5!(happyShift, curry_1_5!(action_37)),
        220 => partial_5!(happyShift, curry_1_5!(action_38)),
        233 => partial_5!(happyShift, curry_1_5!(action_39)),
        234 => partial_5!(happyShift, curry_1_5!(action_40)),
        235 => partial_5!(happyShift, curry_1_5!(action_41)),
        236 => partial_5!(happyShift, curry_1_5!(action_42)),
        237 => partial_5!(happyShift, curry_1_5!(action_74)),
        238 => partial_5!(happyShift, curry_1_5!(action_75)),
        240 => partial_5!(happyShift, curry_1_5!(action_44)),
        241 => partial_5!(happyShift, curry_1_5!(action_45)),
        242 => partial_5!(happyShift, curry_1_5!(action_46)),
        243 => partial_5!(happyShift, curry_1_5!(action_47)),
        244 => partial_5!(happyShift, curry_1_5!(action_48)),
        245 => partial_5!(happyShift, curry_1_5!(action_49)),
        93 => partial_5!(happyGoto, curry_1_5!(action_854)),
        96 => partial_5!(happyGoto, curry_1_5!(action_855)),
        97 => partial_5!(happyGoto, curry_1_5!(action_647)),
        98 => partial_5!(happyGoto, curry_1_5!(action_648)),
        99 => partial_5!(happyGoto, curry_1_5!(action_649)),
        100 => partial_5!(happyGoto, curry_1_5!(action_6)),
        104 => partial_5!(happyGoto, curry_1_5!(action_7)),
        106 => partial_5!(happyGoto, curry_1_5!(action_8)),
        107 => partial_5!(happyGoto, curry_1_5!(action_9)),
        108 => partial_5!(happyGoto, curry_1_5!(action_10)),
        109 => partial_5!(happyGoto, curry_1_5!(action_11)),
        110 => partial_5!(happyGoto, curry_1_5!(action_12)),
        111 => partial_5!(happyGoto, curry_1_5!(action_13)),
        112 => partial_5!(happyGoto, curry_1_5!(action_14)),
        113 => partial_5!(happyGoto, curry_1_5!(action_15)),
        114 => partial_5!(happyGoto, curry_1_5!(action_16)),
        115 => partial_5!(happyGoto, curry_1_5!(action_17)),
        116 => partial_5!(happyGoto, curry_1_5!(action_18)),
        117 => partial_5!(happyGoto, curry_1_5!(action_19)),
        118 => partial_5!(happyGoto, curry_1_5!(action_20)),
        119 => partial_5!(happyGoto, curry_1_5!(action_21)),
        120 => partial_5!(happyGoto, curry_1_5!(action_650)),
        127 => partial_5!(happyGoto, curry_1_5!(action_24)),
        128 => partial_5!(happyGoto, curry_1_5!(action_25)),
        131 => partial_5!(happyGoto, curry_1_5!(action_651)),
        _ => box happyFail
    }
}

fn action_853(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_384()
    }
}

fn action_854(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_349()
    }
}

fn action_855(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_26)),
        144 => partial_5!(happyShift, curry_1_5!(action_27)),
        145 => partial_5!(happyShift, curry_1_5!(action_28)),
        146 => partial_5!(happyShift, curry_1_5!(action_29)),
        147 => partial_5!(happyShift, curry_1_5!(action_30)),
        148 => partial_5!(happyShift, curry_1_5!(action_31)),
        149 => partial_5!(happyShift, curry_1_5!(action_32)),
        150 => partial_5!(happyShift, curry_1_5!(action_33)),
        153 => partial_5!(happyShift, curry_1_5!(action_34)),
        164 => partial_5!(happyShift, curry_1_5!(action_35)),
        181 => partial_5!(happyShift, curry_1_5!(action_654)),
        184 => partial_5!(happyShift, curry_1_5!(action_36)),
        204 => partial_5!(happyShift, curry_1_5!(action_37)),
        220 => partial_5!(happyShift, curry_1_5!(action_38)),
        233 => partial_5!(happyShift, curry_1_5!(action_39)),
        234 => partial_5!(happyShift, curry_1_5!(action_40)),
        235 => partial_5!(happyShift, curry_1_5!(action_41)),
        236 => partial_5!(happyShift, curry_1_5!(action_42)),
        237 => partial_5!(happyShift, curry_1_5!(action_43)),
        240 => partial_5!(happyShift, curry_1_5!(action_44)),
        241 => partial_5!(happyShift, curry_1_5!(action_45)),
        242 => partial_5!(happyShift, curry_1_5!(action_46)),
        243 => partial_5!(happyShift, curry_1_5!(action_47)),
        244 => partial_5!(happyShift, curry_1_5!(action_48)),
        245 => partial_5!(happyShift, curry_1_5!(action_49)),
        93 => partial_5!(happyGoto, curry_1_5!(action_918)),
        100 => partial_5!(happyGoto, curry_1_5!(action_6)),
        104 => partial_5!(happyGoto, curry_1_5!(action_7)),
        106 => partial_5!(happyGoto, curry_1_5!(action_8)),
        107 => partial_5!(happyGoto, curry_1_5!(action_9)),
        108 => partial_5!(happyGoto, curry_1_5!(action_10)),
        109 => partial_5!(happyGoto, curry_1_5!(action_11)),
        110 => partial_5!(happyGoto, curry_1_5!(action_12)),
        111 => partial_5!(happyGoto, curry_1_5!(action_13)),
        112 => partial_5!(happyGoto, curry_1_5!(action_14)),
        113 => partial_5!(happyGoto, curry_1_5!(action_15)),
        114 => partial_5!(happyGoto, curry_1_5!(action_16)),
        115 => partial_5!(happyGoto, curry_1_5!(action_17)),
        116 => partial_5!(happyGoto, curry_1_5!(action_18)),
        117 => partial_5!(happyGoto, curry_1_5!(action_19)),
        118 => partial_5!(happyGoto, curry_1_5!(action_20)),
        119 => partial_5!(happyGoto, curry_1_5!(action_21)),
        120 => partial_5!(happyGoto, curry_1_5!(action_650)),
        127 => partial_5!(happyGoto, curry_1_5!(action_24)),
        128 => partial_5!(happyGoto, curry_1_5!(action_25)),
        _ => box happyFail
    }
}

fn action_856(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_385()
    }
}

fn action_857(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_356()
    }
}

fn action_858(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_26)),
        144 => partial_5!(happyShift, curry_1_5!(action_27)),
        145 => partial_5!(happyShift, curry_1_5!(action_28)),
        146 => partial_5!(happyShift, curry_1_5!(action_29)),
        147 => partial_5!(happyShift, curry_1_5!(action_30)),
        148 => partial_5!(happyShift, curry_1_5!(action_31)),
        149 => partial_5!(happyShift, curry_1_5!(action_32)),
        150 => partial_5!(happyShift, curry_1_5!(action_33)),
        153 => partial_5!(happyShift, curry_1_5!(action_34)),
        164 => partial_5!(happyShift, curry_1_5!(action_35)),
        184 => partial_5!(happyShift, curry_1_5!(action_36)),
        204 => partial_5!(happyShift, curry_1_5!(action_37)),
        220 => partial_5!(happyShift, curry_1_5!(action_38)),
        233 => partial_5!(happyShift, curry_1_5!(action_39)),
        234 => partial_5!(happyShift, curry_1_5!(action_40)),
        235 => partial_5!(happyShift, curry_1_5!(action_41)),
        236 => partial_5!(happyShift, curry_1_5!(action_42)),
        237 => partial_5!(happyShift, curry_1_5!(action_43)),
        240 => partial_5!(happyShift, curry_1_5!(action_44)),
        241 => partial_5!(happyShift, curry_1_5!(action_45)),
        242 => partial_5!(happyShift, curry_1_5!(action_46)),
        243 => partial_5!(happyShift, curry_1_5!(action_47)),
        244 => partial_5!(happyShift, curry_1_5!(action_48)),
        245 => partial_5!(happyShift, curry_1_5!(action_49)),
        100 => partial_5!(happyGoto, curry_1_5!(action_6)),
        104 => partial_5!(happyGoto, curry_1_5!(action_7)),
        106 => partial_5!(happyGoto, curry_1_5!(action_259)),
        107 => partial_5!(happyGoto, curry_1_5!(action_9)),
        108 => partial_5!(happyGoto, curry_1_5!(action_10)),
        109 => partial_5!(happyGoto, curry_1_5!(action_11)),
        110 => partial_5!(happyGoto, curry_1_5!(action_12)),
        111 => partial_5!(happyGoto, curry_1_5!(action_13)),
        112 => partial_5!(happyGoto, curry_1_5!(action_14)),
        113 => partial_5!(happyGoto, curry_1_5!(action_15)),
        114 => partial_5!(happyGoto, curry_1_5!(action_16)),
        115 => partial_5!(happyGoto, curry_1_5!(action_17)),
        116 => partial_5!(happyGoto, curry_1_5!(action_18)),
        117 => partial_5!(happyGoto, curry_1_5!(action_19)),
        118 => partial_5!(happyGoto, curry_1_5!(action_20)),
        119 => partial_5!(happyGoto, curry_1_5!(action_260)),
        126 => partial_5!(happyGoto, curry_1_5!(action_917)),
        127 => partial_5!(happyGoto, curry_1_5!(action_24)),
        128 => partial_5!(happyGoto, curry_1_5!(action_25)),
        _ => box happyFail
    }
}

fn action_859(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_26)),
        140 => partial_5!(happyShift, curry_1_5!(action_652)),
        143 => partial_5!(happyShift, curry_1_5!(action_653)),
        144 => partial_5!(happyShift, curry_1_5!(action_27)),
        145 => partial_5!(happyShift, curry_1_5!(action_28)),
        146 => partial_5!(happyShift, curry_1_5!(action_29)),
        147 => partial_5!(happyShift, curry_1_5!(action_30)),
        148 => partial_5!(happyShift, curry_1_5!(action_31)),
        149 => partial_5!(happyShift, curry_1_5!(action_32)),
        150 => partial_5!(happyShift, curry_1_5!(action_33)),
        153 => partial_5!(happyShift, curry_1_5!(action_34)),
        164 => partial_5!(happyShift, curry_1_5!(action_35)),
        181 => partial_5!(happyShift, curry_1_5!(action_654)),
        182 => partial_5!(happyShift, curry_1_5!(action_916)),
        184 => partial_5!(happyShift, curry_1_5!(action_36)),
        204 => partial_5!(happyShift, curry_1_5!(action_37)),
        220 => partial_5!(happyShift, curry_1_5!(action_38)),
        233 => partial_5!(happyShift, curry_1_5!(action_39)),
        234 => partial_5!(happyShift, curry_1_5!(action_40)),
        235 => partial_5!(happyShift, curry_1_5!(action_41)),
        236 => partial_5!(happyShift, curry_1_5!(action_42)),
        237 => partial_5!(happyShift, curry_1_5!(action_74)),
        238 => partial_5!(happyShift, curry_1_5!(action_75)),
        240 => partial_5!(happyShift, curry_1_5!(action_44)),
        241 => partial_5!(happyShift, curry_1_5!(action_45)),
        242 => partial_5!(happyShift, curry_1_5!(action_46)),
        243 => partial_5!(happyShift, curry_1_5!(action_47)),
        244 => partial_5!(happyShift, curry_1_5!(action_48)),
        245 => partial_5!(happyShift, curry_1_5!(action_49)),
        93 => partial_5!(happyGoto, curry_1_5!(action_854)),
        96 => partial_5!(happyGoto, curry_1_5!(action_855)),
        97 => partial_5!(happyGoto, curry_1_5!(action_647)),
        98 => partial_5!(happyGoto, curry_1_5!(action_648)),
        99 => partial_5!(happyGoto, curry_1_5!(action_649)),
        100 => partial_5!(happyGoto, curry_1_5!(action_6)),
        104 => partial_5!(happyGoto, curry_1_5!(action_7)),
        106 => partial_5!(happyGoto, curry_1_5!(action_8)),
        107 => partial_5!(happyGoto, curry_1_5!(action_9)),
        108 => partial_5!(happyGoto, curry_1_5!(action_10)),
        109 => partial_5!(happyGoto, curry_1_5!(action_11)),
        110 => partial_5!(happyGoto, curry_1_5!(action_12)),
        111 => partial_5!(happyGoto, curry_1_5!(action_13)),
        112 => partial_5!(happyGoto, curry_1_5!(action_14)),
        113 => partial_5!(happyGoto, curry_1_5!(action_15)),
        114 => partial_5!(happyGoto, curry_1_5!(action_16)),
        115 => partial_5!(happyGoto, curry_1_5!(action_17)),
        116 => partial_5!(happyGoto, curry_1_5!(action_18)),
        117 => partial_5!(happyGoto, curry_1_5!(action_19)),
        118 => partial_5!(happyGoto, curry_1_5!(action_20)),
        119 => partial_5!(happyGoto, curry_1_5!(action_21)),
        120 => partial_5!(happyGoto, curry_1_5!(action_650)),
        127 => partial_5!(happyGoto, curry_1_5!(action_24)),
        128 => partial_5!(happyGoto, curry_1_5!(action_25)),
        131 => partial_5!(happyGoto, curry_1_5!(action_651)),
        _ => box happyFail
    }
}

fn action_860(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_342()
    }
}

fn action_861(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_339()
    }
}

fn action_862(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_371()
    }
}

fn action_863(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_369()
    }
}

fn action_864(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_372()
    }
}

fn action_865(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_374()
    }
}

fn action_866(i: isize) -> ActionReturn {
    match i {
        141 => partial_5!(happyShift, curry_1_5!(action_915)),
        _ => box happyFail
    }
}

fn action_867(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_53()
    }
}

fn action_868(i: isize) -> ActionReturn {
    match i {
        141 => partial_5!(happyShift, curry_1_5!(action_914)),
        _ => box happyFail
    }
}

fn action_869(i: isize) -> ActionReturn {
    match i {
        141 => partial_5!(happyShift, curry_1_5!(action_913)),
        _ => box happyFail
    }
}

fn action_870(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_26)),
        144 => partial_5!(happyShift, curry_1_5!(action_27)),
        145 => partial_5!(happyShift, curry_1_5!(action_28)),
        146 => partial_5!(happyShift, curry_1_5!(action_29)),
        147 => partial_5!(happyShift, curry_1_5!(action_30)),
        148 => partial_5!(happyShift, curry_1_5!(action_31)),
        149 => partial_5!(happyShift, curry_1_5!(action_32)),
        150 => partial_5!(happyShift, curry_1_5!(action_33)),
        153 => partial_5!(happyShift, curry_1_5!(action_34)),
        164 => partial_5!(happyShift, curry_1_5!(action_35)),
        184 => partial_5!(happyShift, curry_1_5!(action_36)),
        204 => partial_5!(happyShift, curry_1_5!(action_37)),
        220 => partial_5!(happyShift, curry_1_5!(action_38)),
        233 => partial_5!(happyShift, curry_1_5!(action_39)),
        234 => partial_5!(happyShift, curry_1_5!(action_40)),
        235 => partial_5!(happyShift, curry_1_5!(action_41)),
        236 => partial_5!(happyShift, curry_1_5!(action_42)),
        237 => partial_5!(happyShift, curry_1_5!(action_43)),
        240 => partial_5!(happyShift, curry_1_5!(action_44)),
        241 => partial_5!(happyShift, curry_1_5!(action_45)),
        242 => partial_5!(happyShift, curry_1_5!(action_46)),
        243 => partial_5!(happyShift, curry_1_5!(action_47)),
        244 => partial_5!(happyShift, curry_1_5!(action_48)),
        245 => partial_5!(happyShift, curry_1_5!(action_49)),
        100 => partial_5!(happyGoto, curry_1_5!(action_6)),
        104 => partial_5!(happyGoto, curry_1_5!(action_7)),
        106 => partial_5!(happyGoto, curry_1_5!(action_8)),
        107 => partial_5!(happyGoto, curry_1_5!(action_9)),
        108 => partial_5!(happyGoto, curry_1_5!(action_10)),
        109 => partial_5!(happyGoto, curry_1_5!(action_11)),
        110 => partial_5!(happyGoto, curry_1_5!(action_12)),
        111 => partial_5!(happyGoto, curry_1_5!(action_13)),
        112 => partial_5!(happyGoto, curry_1_5!(action_14)),
        113 => partial_5!(happyGoto, curry_1_5!(action_15)),
        114 => partial_5!(happyGoto, curry_1_5!(action_16)),
        115 => partial_5!(happyGoto, curry_1_5!(action_17)),
        116 => partial_5!(happyGoto, curry_1_5!(action_18)),
        117 => partial_5!(happyGoto, curry_1_5!(action_19)),
        118 => partial_5!(happyGoto, curry_1_5!(action_20)),
        119 => partial_5!(happyGoto, curry_1_5!(action_21)),
        120 => partial_5!(happyGoto, curry_1_5!(action_22)),
        122 => partial_5!(happyGoto, curry_1_5!(action_912)),
        127 => partial_5!(happyGoto, curry_1_5!(action_24)),
        128 => partial_5!(happyGoto, curry_1_5!(action_25)),
        _ => box happyFail
    }
}

fn action_871(i: isize) -> ActionReturn {
    match i {
        140 => partial_5!(happyShift, curry_1_5!(action_822)),
        236 => partial_5!(happyShift, curry_1_5!(action_42)),
        30 => partial_5!(happyGoto, curry_1_5!(action_911)),
        128 => partial_5!(happyGoto, curry_1_5!(action_821)),
        _ => box happyFail
    }
}

fn action_872(i: isize) -> ActionReturn {
    match i {
        180 => partial_5!(happyShift, curry_1_5!(action_910)),
        _ => box happyFail
    }
}

fn action_873(i: isize) -> ActionReturn {
    match i {
        140 => partial_5!(happyShift, curry_1_5!(action_822)),
        236 => partial_5!(happyShift, curry_1_5!(action_42)),
        28 => partial_5!(happyGoto, curry_1_5!(action_909)),
        29 => partial_5!(happyGoto, curry_1_5!(action_819)),
        30 => partial_5!(happyGoto, curry_1_5!(action_820)),
        128 => partial_5!(happyGoto, curry_1_5!(action_821)),
        _ => happyReduce_76()
    }
}

fn action_874(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_62()
    }
}

fn action_875(i: isize) -> ActionReturn {
    match i {
        139 => partial_5!(happyShift, curry_1_5!(action_908)),
        _ => box happyFail
    }
}

fn action_876(i: isize) -> ActionReturn {
    match i {
        139 => partial_5!(happyShift, curry_1_5!(action_907)),
        _ => box happyFail
    }
}

fn action_877(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_59()
    }
}

fn action_878(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_251()
    }
}

fn action_879(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_204()
    }
}

fn action_880(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_26)),
        144 => partial_5!(happyShift, curry_1_5!(action_27)),
        145 => partial_5!(happyShift, curry_1_5!(action_28)),
        146 => partial_5!(happyShift, curry_1_5!(action_29)),
        147 => partial_5!(happyShift, curry_1_5!(action_30)),
        148 => partial_5!(happyShift, curry_1_5!(action_31)),
        149 => partial_5!(happyShift, curry_1_5!(action_32)),
        150 => partial_5!(happyShift, curry_1_5!(action_33)),
        153 => partial_5!(happyShift, curry_1_5!(action_34)),
        164 => partial_5!(happyShift, curry_1_5!(action_35)),
        184 => partial_5!(happyShift, curry_1_5!(action_36)),
        204 => partial_5!(happyShift, curry_1_5!(action_37)),
        220 => partial_5!(happyShift, curry_1_5!(action_38)),
        233 => partial_5!(happyShift, curry_1_5!(action_39)),
        234 => partial_5!(happyShift, curry_1_5!(action_40)),
        235 => partial_5!(happyShift, curry_1_5!(action_41)),
        236 => partial_5!(happyShift, curry_1_5!(action_42)),
        237 => partial_5!(happyShift, curry_1_5!(action_43)),
        240 => partial_5!(happyShift, curry_1_5!(action_44)),
        241 => partial_5!(happyShift, curry_1_5!(action_45)),
        242 => partial_5!(happyShift, curry_1_5!(action_46)),
        243 => partial_5!(happyShift, curry_1_5!(action_47)),
        244 => partial_5!(happyShift, curry_1_5!(action_48)),
        245 => partial_5!(happyShift, curry_1_5!(action_49)),
        100 => partial_5!(happyGoto, curry_1_5!(action_6)),
        104 => partial_5!(happyGoto, curry_1_5!(action_7)),
        106 => partial_5!(happyGoto, curry_1_5!(action_259)),
        107 => partial_5!(happyGoto, curry_1_5!(action_9)),
        108 => partial_5!(happyGoto, curry_1_5!(action_10)),
        109 => partial_5!(happyGoto, curry_1_5!(action_11)),
        110 => partial_5!(happyGoto, curry_1_5!(action_12)),
        111 => partial_5!(happyGoto, curry_1_5!(action_13)),
        112 => partial_5!(happyGoto, curry_1_5!(action_14)),
        113 => partial_5!(happyGoto, curry_1_5!(action_15)),
        114 => partial_5!(happyGoto, curry_1_5!(action_16)),
        115 => partial_5!(happyGoto, curry_1_5!(action_17)),
        116 => partial_5!(happyGoto, curry_1_5!(action_18)),
        117 => partial_5!(happyGoto, curry_1_5!(action_19)),
        118 => partial_5!(happyGoto, curry_1_5!(action_20)),
        119 => partial_5!(happyGoto, curry_1_5!(action_260)),
        126 => partial_5!(happyGoto, curry_1_5!(action_906)),
        127 => partial_5!(happyGoto, curry_1_5!(action_24)),
        128 => partial_5!(happyGoto, curry_1_5!(action_25)),
        _ => box happyFail
    }
}

fn action_881(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_200()
    }
}

fn action_882(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_475)),
        150 => partial_5!(happyShift, curry_1_5!(action_476)),
        167 => partial_5!(happyShift, curry_1_5!(action_795)),
        237 => partial_5!(happyShift, curry_1_5!(action_140)),
        60 => partial_5!(happyGoto, curry_1_5!(action_905)),
        75 => partial_5!(happyGoto, curry_1_5!(action_794)),
        76 => partial_5!(happyGoto, curry_1_5!(action_102)),
        77 => partial_5!(happyGoto, curry_1_5!(action_103)),
        78 => partial_5!(happyGoto, curry_1_5!(action_481)),
        _ => box happyFail
    }
}

fn action_883(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_524)),
        150 => partial_5!(happyShift, curry_1_5!(action_525)),
        167 => partial_5!(happyShift, curry_1_5!(action_804)),
        237 => partial_5!(happyShift, curry_1_5!(action_140)),
        238 => partial_5!(happyShift, curry_1_5!(action_229)),
        59 => partial_5!(happyGoto, curry_1_5!(action_904)),
        66 => partial_5!(happyGoto, curry_1_5!(action_803)),
        68 => partial_5!(happyGoto, curry_1_5!(action_219)),
        69 => partial_5!(happyGoto, curry_1_5!(action_220)),
        70 => partial_5!(happyGoto, curry_1_5!(action_221)),
        71 => partial_5!(happyGoto, curry_1_5!(action_222)),
        72 => partial_5!(happyGoto, curry_1_5!(action_223)),
        73 => partial_5!(happyGoto, curry_1_5!(action_224)),
        75 => partial_5!(happyGoto, curry_1_5!(action_523)),
        76 => partial_5!(happyGoto, curry_1_5!(action_102)),
        77 => partial_5!(happyGoto, curry_1_5!(action_103)),
        78 => partial_5!(happyGoto, curry_1_5!(action_481)),
        _ => box happyFail
    }
}

fn action_884(i: isize) -> ActionReturn {
    match i {
        239 => partial_5!(happyShift, curry_1_5!(action_142)),
        134 => partial_5!(happyGoto, curry_1_5!(action_887)),
        _ => happyReduce_197()
    }
}

fn action_885(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_207()
    }
}

fn action_886(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_26)),
        144 => partial_5!(happyShift, curry_1_5!(action_27)),
        145 => partial_5!(happyShift, curry_1_5!(action_28)),
        146 => partial_5!(happyShift, curry_1_5!(action_29)),
        147 => partial_5!(happyShift, curry_1_5!(action_30)),
        148 => partial_5!(happyShift, curry_1_5!(action_31)),
        149 => partial_5!(happyShift, curry_1_5!(action_32)),
        150 => partial_5!(happyShift, curry_1_5!(action_33)),
        153 => partial_5!(happyShift, curry_1_5!(action_34)),
        164 => partial_5!(happyShift, curry_1_5!(action_35)),
        184 => partial_5!(happyShift, curry_1_5!(action_36)),
        204 => partial_5!(happyShift, curry_1_5!(action_37)),
        220 => partial_5!(happyShift, curry_1_5!(action_38)),
        233 => partial_5!(happyShift, curry_1_5!(action_39)),
        234 => partial_5!(happyShift, curry_1_5!(action_40)),
        235 => partial_5!(happyShift, curry_1_5!(action_41)),
        236 => partial_5!(happyShift, curry_1_5!(action_42)),
        237 => partial_5!(happyShift, curry_1_5!(action_43)),
        240 => partial_5!(happyShift, curry_1_5!(action_44)),
        241 => partial_5!(happyShift, curry_1_5!(action_45)),
        242 => partial_5!(happyShift, curry_1_5!(action_46)),
        243 => partial_5!(happyShift, curry_1_5!(action_47)),
        244 => partial_5!(happyShift, curry_1_5!(action_48)),
        245 => partial_5!(happyShift, curry_1_5!(action_49)),
        100 => partial_5!(happyGoto, curry_1_5!(action_6)),
        104 => partial_5!(happyGoto, curry_1_5!(action_7)),
        106 => partial_5!(happyGoto, curry_1_5!(action_259)),
        107 => partial_5!(happyGoto, curry_1_5!(action_9)),
        108 => partial_5!(happyGoto, curry_1_5!(action_10)),
        109 => partial_5!(happyGoto, curry_1_5!(action_11)),
        110 => partial_5!(happyGoto, curry_1_5!(action_12)),
        111 => partial_5!(happyGoto, curry_1_5!(action_13)),
        112 => partial_5!(happyGoto, curry_1_5!(action_14)),
        113 => partial_5!(happyGoto, curry_1_5!(action_15)),
        114 => partial_5!(happyGoto, curry_1_5!(action_16)),
        115 => partial_5!(happyGoto, curry_1_5!(action_17)),
        116 => partial_5!(happyGoto, curry_1_5!(action_18)),
        117 => partial_5!(happyGoto, curry_1_5!(action_19)),
        118 => partial_5!(happyGoto, curry_1_5!(action_20)),
        119 => partial_5!(happyGoto, curry_1_5!(action_260)),
        126 => partial_5!(happyGoto, curry_1_5!(action_903)),
        127 => partial_5!(happyGoto, curry_1_5!(action_24)),
        128 => partial_5!(happyGoto, curry_1_5!(action_25)),
        _ => box happyFail
    }
}

fn action_887(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_209()
    }
}

fn action_888(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_325()
    }
}

fn action_889(i: isize) -> ActionReturn {
    match i {
        141 => partial_5!(happyShift, curry_1_5!(action_902)),
        _ => box happyFail
    }
}

fn action_890(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_320()
    }
}

fn action_891(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_213()
    }
}

fn action_892(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_219()
    }
}

fn action_893(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_89()
    }
}

fn action_894(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_481()
    }
}

fn action_895(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_26)),
        144 => partial_5!(happyShift, curry_1_5!(action_27)),
        145 => partial_5!(happyShift, curry_1_5!(action_28)),
        146 => partial_5!(happyShift, curry_1_5!(action_29)),
        147 => partial_5!(happyShift, curry_1_5!(action_30)),
        148 => partial_5!(happyShift, curry_1_5!(action_31)),
        149 => partial_5!(happyShift, curry_1_5!(action_32)),
        150 => partial_5!(happyShift, curry_1_5!(action_33)),
        153 => partial_5!(happyShift, curry_1_5!(action_34)),
        164 => partial_5!(happyShift, curry_1_5!(action_35)),
        184 => partial_5!(happyShift, curry_1_5!(action_36)),
        204 => partial_5!(happyShift, curry_1_5!(action_37)),
        220 => partial_5!(happyShift, curry_1_5!(action_38)),
        233 => partial_5!(happyShift, curry_1_5!(action_39)),
        234 => partial_5!(happyShift, curry_1_5!(action_40)),
        235 => partial_5!(happyShift, curry_1_5!(action_41)),
        236 => partial_5!(happyShift, curry_1_5!(action_42)),
        237 => partial_5!(happyShift, curry_1_5!(action_43)),
        240 => partial_5!(happyShift, curry_1_5!(action_44)),
        241 => partial_5!(happyShift, curry_1_5!(action_45)),
        242 => partial_5!(happyShift, curry_1_5!(action_46)),
        243 => partial_5!(happyShift, curry_1_5!(action_47)),
        244 => partial_5!(happyShift, curry_1_5!(action_48)),
        245 => partial_5!(happyShift, curry_1_5!(action_49)),
        100 => partial_5!(happyGoto, curry_1_5!(action_6)),
        104 => partial_5!(happyGoto, curry_1_5!(action_7)),
        106 => partial_5!(happyGoto, curry_1_5!(action_900)),
        107 => partial_5!(happyGoto, curry_1_5!(action_9)),
        108 => partial_5!(happyGoto, curry_1_5!(action_10)),
        109 => partial_5!(happyGoto, curry_1_5!(action_11)),
        110 => partial_5!(happyGoto, curry_1_5!(action_12)),
        111 => partial_5!(happyGoto, curry_1_5!(action_13)),
        112 => partial_5!(happyGoto, curry_1_5!(action_14)),
        113 => partial_5!(happyGoto, curry_1_5!(action_15)),
        114 => partial_5!(happyGoto, curry_1_5!(action_16)),
        115 => partial_5!(happyGoto, curry_1_5!(action_17)),
        116 => partial_5!(happyGoto, curry_1_5!(action_18)),
        117 => partial_5!(happyGoto, curry_1_5!(action_19)),
        118 => partial_5!(happyGoto, curry_1_5!(action_20)),
        119 => partial_5!(happyGoto, curry_1_5!(action_260)),
        126 => partial_5!(happyGoto, curry_1_5!(action_901)),
        127 => partial_5!(happyGoto, curry_1_5!(action_24)),
        128 => partial_5!(happyGoto, curry_1_5!(action_25)),
        _ => box happyFail
    }
}

fn action_896(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_272)),
        144 => partial_5!(happyShift, curry_1_5!(action_27)),
        145 => partial_5!(happyShift, curry_1_5!(action_28)),
        146 => partial_5!(happyShift, curry_1_5!(action_29)),
        147 => partial_5!(happyShift, curry_1_5!(action_30)),
        148 => partial_5!(happyShift, curry_1_5!(action_31)),
        149 => partial_5!(happyShift, curry_1_5!(action_32)),
        150 => partial_5!(happyShift, curry_1_5!(action_33)),
        153 => partial_5!(happyShift, curry_1_5!(action_34)),
        164 => partial_5!(happyShift, curry_1_5!(action_35)),
        184 => partial_5!(happyShift, curry_1_5!(action_36)),
        204 => partial_5!(happyShift, curry_1_5!(action_37)),
        220 => partial_5!(happyShift, curry_1_5!(action_38)),
        233 => partial_5!(happyShift, curry_1_5!(action_39)),
        234 => partial_5!(happyShift, curry_1_5!(action_40)),
        235 => partial_5!(happyShift, curry_1_5!(action_41)),
        236 => partial_5!(happyShift, curry_1_5!(action_42)),
        237 => partial_5!(happyShift, curry_1_5!(action_43)),
        240 => partial_5!(happyShift, curry_1_5!(action_44)),
        241 => partial_5!(happyShift, curry_1_5!(action_45)),
        242 => partial_5!(happyShift, curry_1_5!(action_46)),
        243 => partial_5!(happyShift, curry_1_5!(action_47)),
        244 => partial_5!(happyShift, curry_1_5!(action_48)),
        245 => partial_5!(happyShift, curry_1_5!(action_49)),
        246 => partial_5!(happyShift, curry_1_5!(action_899)),
        100 => partial_5!(happyGoto, curry_1_5!(action_6)),
        104 => partial_5!(happyGoto, curry_1_5!(action_7)),
        106 => partial_5!(happyGoto, curry_1_5!(action_897)),
        107 => partial_5!(happyGoto, curry_1_5!(action_9)),
        127 => partial_5!(happyGoto, curry_1_5!(action_24)),
        128 => partial_5!(happyGoto, curry_1_5!(action_25)),
        130 => partial_5!(happyGoto, curry_1_5!(action_898)),
        _ => box happyFail
    }
}

fn action_897(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_485()
    }
}

fn action_898(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_484()
    }
}

fn action_899(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_468()
    }
}

fn action_900(i: isize) -> ActionReturn {
    match i {
        168 => partial_5!(happyShift, curry_1_5!(action_316)),
        169 => partial_5!(happyShift, curry_1_5!(action_317)),
        170 => partial_5!(happyShift, curry_1_5!(action_318)),
        171 => partial_5!(happyShift, curry_1_5!(action_319)),
        172 => partial_5!(happyShift, curry_1_5!(action_320)),
        173 => partial_5!(happyShift, curry_1_5!(action_321)),
        174 => partial_5!(happyShift, curry_1_5!(action_322)),
        175 => partial_5!(happyShift, curry_1_5!(action_323)),
        176 => partial_5!(happyShift, curry_1_5!(action_324)),
        177 => partial_5!(happyShift, curry_1_5!(action_325)),
        178 => partial_5!(happyShift, curry_1_5!(action_326)),
        121 => partial_5!(happyGoto, curry_1_5!(action_928)),
        _ => happyReduce_406()
    }
}

fn action_901(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_486()
    }
}

fn action_902(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_321()
    }
}

fn action_903(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_208()
    }
}

fn action_904(i: isize) -> ActionReturn {
    match i {
        239 => partial_5!(happyShift, curry_1_5!(action_142)),
        132 => partial_5!(happyGoto, curry_1_5!(action_927)),
        133 => partial_5!(happyGoto, curry_1_5!(action_150)),
        134 => partial_5!(happyGoto, curry_1_5!(action_110)),
        _ => happyReduce_471()
    }
}

fn action_905(i: isize) -> ActionReturn {
    match i {
        239 => partial_5!(happyShift, curry_1_5!(action_142)),
        134 => partial_5!(happyGoto, curry_1_5!(action_887)),
        _ => happyReduce_199()
    }
}

fn action_906(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_205()
    }
}

fn action_907(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_26)),
        144 => partial_5!(happyShift, curry_1_5!(action_27)),
        145 => partial_5!(happyShift, curry_1_5!(action_28)),
        146 => partial_5!(happyShift, curry_1_5!(action_29)),
        147 => partial_5!(happyShift, curry_1_5!(action_30)),
        148 => partial_5!(happyShift, curry_1_5!(action_31)),
        149 => partial_5!(happyShift, curry_1_5!(action_32)),
        150 => partial_5!(happyShift, curry_1_5!(action_33)),
        153 => partial_5!(happyShift, curry_1_5!(action_34)),
        164 => partial_5!(happyShift, curry_1_5!(action_35)),
        180 => partial_5!(happyShift, curry_1_5!(action_60)),
        181 => partial_5!(happyShift, curry_1_5!(action_61)),
        184 => partial_5!(happyShift, curry_1_5!(action_36)),
        187 => partial_5!(happyShift, curry_1_5!(action_62)),
        189 => partial_5!(happyShift, curry_1_5!(action_63)),
        191 => partial_5!(happyShift, curry_1_5!(action_64)),
        194 => partial_5!(happyShift, curry_1_5!(action_65)),
        196 => partial_5!(happyShift, curry_1_5!(action_66)),
        197 => partial_5!(happyShift, curry_1_5!(action_67)),
        203 => partial_5!(happyShift, curry_1_5!(action_68)),
        204 => partial_5!(happyShift, curry_1_5!(action_37)),
        205 => partial_5!(happyShift, curry_1_5!(action_69)),
        206 => partial_5!(happyShift, curry_1_5!(action_70)),
        217 => partial_5!(happyShift, curry_1_5!(action_71)),
        220 => partial_5!(happyShift, curry_1_5!(action_38)),
        224 => partial_5!(happyShift, curry_1_5!(action_72)),
        232 => partial_5!(happyShift, curry_1_5!(action_73)),
        233 => partial_5!(happyShift, curry_1_5!(action_39)),
        234 => partial_5!(happyShift, curry_1_5!(action_40)),
        235 => partial_5!(happyShift, curry_1_5!(action_41)),
        236 => partial_5!(happyShift, curry_1_5!(action_42)),
        237 => partial_5!(happyShift, curry_1_5!(action_74)),
        238 => partial_5!(happyShift, curry_1_5!(action_75)),
        240 => partial_5!(happyShift, curry_1_5!(action_44)),
        241 => partial_5!(happyShift, curry_1_5!(action_45)),
        242 => partial_5!(happyShift, curry_1_5!(action_46)),
        243 => partial_5!(happyShift, curry_1_5!(action_47)),
        244 => partial_5!(happyShift, curry_1_5!(action_48)),
        245 => partial_5!(happyShift, curry_1_5!(action_49)),
        12 => partial_5!(happyGoto, curry_1_5!(action_926)),
        13 => partial_5!(happyGoto, curry_1_5!(action_51)),
        14 => partial_5!(happyGoto, curry_1_5!(action_52)),
        22 => partial_5!(happyGoto, curry_1_5!(action_53)),
        23 => partial_5!(happyGoto, curry_1_5!(action_54)),
        24 => partial_5!(happyGoto, curry_1_5!(action_55)),
        25 => partial_5!(happyGoto, curry_1_5!(action_56)),
        26 => partial_5!(happyGoto, curry_1_5!(action_57)),
        100 => partial_5!(happyGoto, curry_1_5!(action_6)),
        104 => partial_5!(happyGoto, curry_1_5!(action_7)),
        106 => partial_5!(happyGoto, curry_1_5!(action_8)),
        107 => partial_5!(happyGoto, curry_1_5!(action_9)),
        108 => partial_5!(happyGoto, curry_1_5!(action_10)),
        109 => partial_5!(happyGoto, curry_1_5!(action_11)),
        110 => partial_5!(happyGoto, curry_1_5!(action_12)),
        111 => partial_5!(happyGoto, curry_1_5!(action_13)),
        112 => partial_5!(happyGoto, curry_1_5!(action_14)),
        113 => partial_5!(happyGoto, curry_1_5!(action_15)),
        114 => partial_5!(happyGoto, curry_1_5!(action_16)),
        115 => partial_5!(happyGoto, curry_1_5!(action_17)),
        116 => partial_5!(happyGoto, curry_1_5!(action_18)),
        117 => partial_5!(happyGoto, curry_1_5!(action_19)),
        118 => partial_5!(happyGoto, curry_1_5!(action_20)),
        119 => partial_5!(happyGoto, curry_1_5!(action_21)),
        120 => partial_5!(happyGoto, curry_1_5!(action_22)),
        122 => partial_5!(happyGoto, curry_1_5!(action_58)),
        127 => partial_5!(happyGoto, curry_1_5!(action_24)),
        128 => partial_5!(happyGoto, curry_1_5!(action_25)),
        131 => partial_5!(happyGoto, curry_1_5!(action_59)),
        _ => box happyFail
    }
}

fn action_908(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_26)),
        144 => partial_5!(happyShift, curry_1_5!(action_27)),
        145 => partial_5!(happyShift, curry_1_5!(action_28)),
        146 => partial_5!(happyShift, curry_1_5!(action_29)),
        147 => partial_5!(happyShift, curry_1_5!(action_30)),
        148 => partial_5!(happyShift, curry_1_5!(action_31)),
        149 => partial_5!(happyShift, curry_1_5!(action_32)),
        150 => partial_5!(happyShift, curry_1_5!(action_33)),
        153 => partial_5!(happyShift, curry_1_5!(action_34)),
        164 => partial_5!(happyShift, curry_1_5!(action_35)),
        180 => partial_5!(happyShift, curry_1_5!(action_60)),
        181 => partial_5!(happyShift, curry_1_5!(action_61)),
        184 => partial_5!(happyShift, curry_1_5!(action_36)),
        187 => partial_5!(happyShift, curry_1_5!(action_62)),
        189 => partial_5!(happyShift, curry_1_5!(action_63)),
        191 => partial_5!(happyShift, curry_1_5!(action_64)),
        194 => partial_5!(happyShift, curry_1_5!(action_65)),
        196 => partial_5!(happyShift, curry_1_5!(action_66)),
        197 => partial_5!(happyShift, curry_1_5!(action_67)),
        203 => partial_5!(happyShift, curry_1_5!(action_68)),
        204 => partial_5!(happyShift, curry_1_5!(action_37)),
        205 => partial_5!(happyShift, curry_1_5!(action_69)),
        206 => partial_5!(happyShift, curry_1_5!(action_70)),
        217 => partial_5!(happyShift, curry_1_5!(action_71)),
        220 => partial_5!(happyShift, curry_1_5!(action_38)),
        224 => partial_5!(happyShift, curry_1_5!(action_72)),
        232 => partial_5!(happyShift, curry_1_5!(action_73)),
        233 => partial_5!(happyShift, curry_1_5!(action_39)),
        234 => partial_5!(happyShift, curry_1_5!(action_40)),
        235 => partial_5!(happyShift, curry_1_5!(action_41)),
        236 => partial_5!(happyShift, curry_1_5!(action_42)),
        237 => partial_5!(happyShift, curry_1_5!(action_74)),
        238 => partial_5!(happyShift, curry_1_5!(action_75)),
        240 => partial_5!(happyShift, curry_1_5!(action_44)),
        241 => partial_5!(happyShift, curry_1_5!(action_45)),
        242 => partial_5!(happyShift, curry_1_5!(action_46)),
        243 => partial_5!(happyShift, curry_1_5!(action_47)),
        244 => partial_5!(happyShift, curry_1_5!(action_48)),
        245 => partial_5!(happyShift, curry_1_5!(action_49)),
        12 => partial_5!(happyGoto, curry_1_5!(action_925)),
        13 => partial_5!(happyGoto, curry_1_5!(action_51)),
        14 => partial_5!(happyGoto, curry_1_5!(action_52)),
        22 => partial_5!(happyGoto, curry_1_5!(action_53)),
        23 => partial_5!(happyGoto, curry_1_5!(action_54)),
        24 => partial_5!(happyGoto, curry_1_5!(action_55)),
        25 => partial_5!(happyGoto, curry_1_5!(action_56)),
        26 => partial_5!(happyGoto, curry_1_5!(action_57)),
        100 => partial_5!(happyGoto, curry_1_5!(action_6)),
        104 => partial_5!(happyGoto, curry_1_5!(action_7)),
        106 => partial_5!(happyGoto, curry_1_5!(action_8)),
        107 => partial_5!(happyGoto, curry_1_5!(action_9)),
        108 => partial_5!(happyGoto, curry_1_5!(action_10)),
        109 => partial_5!(happyGoto, curry_1_5!(action_11)),
        110 => partial_5!(happyGoto, curry_1_5!(action_12)),
        111 => partial_5!(happyGoto, curry_1_5!(action_13)),
        112 => partial_5!(happyGoto, curry_1_5!(action_14)),
        113 => partial_5!(happyGoto, curry_1_5!(action_15)),
        114 => partial_5!(happyGoto, curry_1_5!(action_16)),
        115 => partial_5!(happyGoto, curry_1_5!(action_17)),
        116 => partial_5!(happyGoto, curry_1_5!(action_18)),
        117 => partial_5!(happyGoto, curry_1_5!(action_19)),
        118 => partial_5!(happyGoto, curry_1_5!(action_20)),
        119 => partial_5!(happyGoto, curry_1_5!(action_21)),
        120 => partial_5!(happyGoto, curry_1_5!(action_22)),
        122 => partial_5!(happyGoto, curry_1_5!(action_58)),
        127 => partial_5!(happyGoto, curry_1_5!(action_24)),
        128 => partial_5!(happyGoto, curry_1_5!(action_25)),
        131 => partial_5!(happyGoto, curry_1_5!(action_59)),
        _ => box happyFail
    }
}

fn action_909(i: isize) -> ActionReturn {
    match i {
        139 => partial_5!(happyShift, curry_1_5!(action_923)),
        167 => partial_5!(happyShift, curry_1_5!(action_924)),
        _ => box happyFail
    }
}

fn action_910(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_71()
    }
}

fn action_911(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_79()
    }
}

fn action_912(i: isize) -> ActionReturn {
    match i {
        139 => partial_5!(happyShift, curry_1_5!(action_922)),
        _ => box happyFail
    }
}

fn action_913(i: isize) -> ActionReturn {
    match i {
        236 => partial_5!(happyShift, curry_1_5!(action_42)),
        128 => partial_5!(happyGoto, curry_1_5!(action_921)),
        _ => box happyFail
    }
}

fn action_914(i: isize) -> ActionReturn {
    match i {
        236 => partial_5!(happyShift, curry_1_5!(action_42)),
        128 => partial_5!(happyGoto, curry_1_5!(action_920)),
        _ => box happyFail
    }
}

fn action_915(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_375()
    }
}

fn action_916(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_343()
    }
}

fn action_917(i: isize) -> ActionReturn {
    match i {
        141 => partial_5!(happyShift, curry_1_5!(action_919)),
        _ => box happyFail
    }
}

fn action_918(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_350()
    }
}

fn action_919(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_359()
    }
}

fn action_920(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_936)),
        _ => box happyFail
    }
}

fn action_921(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_935)),
        _ => box happyFail
    }
}

fn action_922(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_80()
    }
}

fn action_923(i: isize) -> ActionReturn {
    match i {
        180 => partial_5!(happyShift, curry_1_5!(action_934)),
        _ => box happyFail
    }
}

fn action_924(i: isize) -> ActionReturn {
    match i {
        236 => partial_5!(happyShift, curry_1_5!(action_42)),
        31 => partial_5!(happyGoto, curry_1_5!(action_932)),
        128 => partial_5!(happyGoto, curry_1_5!(action_933)),
        _ => box happyFail
    }
}

fn action_925(i: isize) -> ActionReturn {
    match i {
        16 => partial_5!(happyGoto, curry_1_5!(action_931)),
        _ => happyReduce_41()
    }
}

fn action_926(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_63()
    }
}

fn action_927(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_201()
    }
}

fn action_928(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_272)),
        144 => partial_5!(happyShift, curry_1_5!(action_27)),
        145 => partial_5!(happyShift, curry_1_5!(action_28)),
        146 => partial_5!(happyShift, curry_1_5!(action_29)),
        147 => partial_5!(happyShift, curry_1_5!(action_30)),
        148 => partial_5!(happyShift, curry_1_5!(action_31)),
        149 => partial_5!(happyShift, curry_1_5!(action_32)),
        150 => partial_5!(happyShift, curry_1_5!(action_33)),
        153 => partial_5!(happyShift, curry_1_5!(action_34)),
        164 => partial_5!(happyShift, curry_1_5!(action_35)),
        184 => partial_5!(happyShift, curry_1_5!(action_36)),
        204 => partial_5!(happyShift, curry_1_5!(action_37)),
        220 => partial_5!(happyShift, curry_1_5!(action_38)),
        233 => partial_5!(happyShift, curry_1_5!(action_39)),
        234 => partial_5!(happyShift, curry_1_5!(action_40)),
        235 => partial_5!(happyShift, curry_1_5!(action_41)),
        236 => partial_5!(happyShift, curry_1_5!(action_42)),
        237 => partial_5!(happyShift, curry_1_5!(action_43)),
        240 => partial_5!(happyShift, curry_1_5!(action_44)),
        241 => partial_5!(happyShift, curry_1_5!(action_45)),
        242 => partial_5!(happyShift, curry_1_5!(action_46)),
        243 => partial_5!(happyShift, curry_1_5!(action_47)),
        244 => partial_5!(happyShift, curry_1_5!(action_48)),
        245 => partial_5!(happyShift, curry_1_5!(action_49)),
        246 => partial_5!(happyShift, curry_1_5!(action_899)),
        100 => partial_5!(happyGoto, curry_1_5!(action_6)),
        104 => partial_5!(happyGoto, curry_1_5!(action_7)),
        106 => partial_5!(happyGoto, curry_1_5!(action_929)),
        107 => partial_5!(happyGoto, curry_1_5!(action_9)),
        127 => partial_5!(happyGoto, curry_1_5!(action_24)),
        128 => partial_5!(happyGoto, curry_1_5!(action_25)),
        130 => partial_5!(happyGoto, curry_1_5!(action_930)),
        _ => box happyFail
    }
}

fn action_929(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_487()
    }
}

fn action_930(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_488()
    }
}

fn action_931(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_64()
    }
}

fn action_932(i: isize) -> ActionReturn {
    match i {
        139 => partial_5!(happyShift, curry_1_5!(action_939)),
        179 => partial_5!(happyShift, curry_1_5!(action_940)),
        _ => box happyFail
    }
}

fn action_933(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_83()
    }
}

fn action_934(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_72()
    }
}

fn action_935(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_26)),
        144 => partial_5!(happyShift, curry_1_5!(action_27)),
        145 => partial_5!(happyShift, curry_1_5!(action_28)),
        146 => partial_5!(happyShift, curry_1_5!(action_29)),
        147 => partial_5!(happyShift, curry_1_5!(action_30)),
        148 => partial_5!(happyShift, curry_1_5!(action_31)),
        149 => partial_5!(happyShift, curry_1_5!(action_32)),
        150 => partial_5!(happyShift, curry_1_5!(action_33)),
        153 => partial_5!(happyShift, curry_1_5!(action_34)),
        164 => partial_5!(happyShift, curry_1_5!(action_35)),
        184 => partial_5!(happyShift, curry_1_5!(action_36)),
        204 => partial_5!(happyShift, curry_1_5!(action_37)),
        220 => partial_5!(happyShift, curry_1_5!(action_38)),
        233 => partial_5!(happyShift, curry_1_5!(action_39)),
        234 => partial_5!(happyShift, curry_1_5!(action_40)),
        235 => partial_5!(happyShift, curry_1_5!(action_41)),
        236 => partial_5!(happyShift, curry_1_5!(action_42)),
        237 => partial_5!(happyShift, curry_1_5!(action_43)),
        240 => partial_5!(happyShift, curry_1_5!(action_44)),
        241 => partial_5!(happyShift, curry_1_5!(action_45)),
        242 => partial_5!(happyShift, curry_1_5!(action_46)),
        243 => partial_5!(happyShift, curry_1_5!(action_47)),
        244 => partial_5!(happyShift, curry_1_5!(action_48)),
        245 => partial_5!(happyShift, curry_1_5!(action_49)),
        100 => partial_5!(happyGoto, curry_1_5!(action_6)),
        104 => partial_5!(happyGoto, curry_1_5!(action_7)),
        106 => partial_5!(happyGoto, curry_1_5!(action_8)),
        107 => partial_5!(happyGoto, curry_1_5!(action_9)),
        108 => partial_5!(happyGoto, curry_1_5!(action_10)),
        109 => partial_5!(happyGoto, curry_1_5!(action_11)),
        110 => partial_5!(happyGoto, curry_1_5!(action_12)),
        111 => partial_5!(happyGoto, curry_1_5!(action_13)),
        112 => partial_5!(happyGoto, curry_1_5!(action_14)),
        113 => partial_5!(happyGoto, curry_1_5!(action_15)),
        114 => partial_5!(happyGoto, curry_1_5!(action_16)),
        115 => partial_5!(happyGoto, curry_1_5!(action_17)),
        116 => partial_5!(happyGoto, curry_1_5!(action_18)),
        117 => partial_5!(happyGoto, curry_1_5!(action_19)),
        118 => partial_5!(happyGoto, curry_1_5!(action_20)),
        119 => partial_5!(happyGoto, curry_1_5!(action_21)),
        120 => partial_5!(happyGoto, curry_1_5!(action_22)),
        122 => partial_5!(happyGoto, curry_1_5!(action_938)),
        127 => partial_5!(happyGoto, curry_1_5!(action_24)),
        128 => partial_5!(happyGoto, curry_1_5!(action_25)),
        _ => box happyFail
    }
}

fn action_936(i: isize) -> ActionReturn {
    match i {
        138 => partial_5!(happyShift, curry_1_5!(action_26)),
        144 => partial_5!(happyShift, curry_1_5!(action_27)),
        145 => partial_5!(happyShift, curry_1_5!(action_28)),
        146 => partial_5!(happyShift, curry_1_5!(action_29)),
        147 => partial_5!(happyShift, curry_1_5!(action_30)),
        148 => partial_5!(happyShift, curry_1_5!(action_31)),
        149 => partial_5!(happyShift, curry_1_5!(action_32)),
        150 => partial_5!(happyShift, curry_1_5!(action_33)),
        153 => partial_5!(happyShift, curry_1_5!(action_34)),
        164 => partial_5!(happyShift, curry_1_5!(action_35)),
        184 => partial_5!(happyShift, curry_1_5!(action_36)),
        204 => partial_5!(happyShift, curry_1_5!(action_37)),
        220 => partial_5!(happyShift, curry_1_5!(action_38)),
        233 => partial_5!(happyShift, curry_1_5!(action_39)),
        234 => partial_5!(happyShift, curry_1_5!(action_40)),
        235 => partial_5!(happyShift, curry_1_5!(action_41)),
        236 => partial_5!(happyShift, curry_1_5!(action_42)),
        237 => partial_5!(happyShift, curry_1_5!(action_43)),
        240 => partial_5!(happyShift, curry_1_5!(action_44)),
        241 => partial_5!(happyShift, curry_1_5!(action_45)),
        242 => partial_5!(happyShift, curry_1_5!(action_46)),
        243 => partial_5!(happyShift, curry_1_5!(action_47)),
        244 => partial_5!(happyShift, curry_1_5!(action_48)),
        245 => partial_5!(happyShift, curry_1_5!(action_49)),
        100 => partial_5!(happyGoto, curry_1_5!(action_6)),
        104 => partial_5!(happyGoto, curry_1_5!(action_7)),
        106 => partial_5!(happyGoto, curry_1_5!(action_8)),
        107 => partial_5!(happyGoto, curry_1_5!(action_9)),
        108 => partial_5!(happyGoto, curry_1_5!(action_10)),
        109 => partial_5!(happyGoto, curry_1_5!(action_11)),
        110 => partial_5!(happyGoto, curry_1_5!(action_12)),
        111 => partial_5!(happyGoto, curry_1_5!(action_13)),
        112 => partial_5!(happyGoto, curry_1_5!(action_14)),
        113 => partial_5!(happyGoto, curry_1_5!(action_15)),
        114 => partial_5!(happyGoto, curry_1_5!(action_16)),
        115 => partial_5!(happyGoto, curry_1_5!(action_17)),
        116 => partial_5!(happyGoto, curry_1_5!(action_18)),
        117 => partial_5!(happyGoto, curry_1_5!(action_19)),
        118 => partial_5!(happyGoto, curry_1_5!(action_20)),
        119 => partial_5!(happyGoto, curry_1_5!(action_21)),
        120 => partial_5!(happyGoto, curry_1_5!(action_22)),
        122 => partial_5!(happyGoto, curry_1_5!(action_937)),
        127 => partial_5!(happyGoto, curry_1_5!(action_24)),
        128 => partial_5!(happyGoto, curry_1_5!(action_25)),
        _ => box happyFail
    }
}

fn action_937(i: isize) -> ActionReturn {
    match i {
        139 => partial_5!(happyShift, curry_1_5!(action_944)),
        _ => box happyFail
    }
}

fn action_938(i: isize) -> ActionReturn {
    match i {
        139 => partial_5!(happyShift, curry_1_5!(action_943)),
        _ => box happyFail
    }
}

fn action_939(i: isize) -> ActionReturn {
    match i {
        180 => partial_5!(happyShift, curry_1_5!(action_942)),
        _ => box happyFail
    }
}

fn action_940(i: isize) -> ActionReturn {
    match i {
        236 => partial_5!(happyShift, curry_1_5!(action_42)),
        128 => partial_5!(happyGoto, curry_1_5!(action_941)),
        _ => box happyFail
    }
}

fn action_941(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_84()
    }
}

fn action_942(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_73()
    }
}

fn action_943(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_82()
    }
}

fn action_944(i: isize) -> ActionReturn {
    match i {
        _ => happyReduce_81()
    }
}

fn happyReduce_4() -> ActionReturn {
    partial_5!(happyMonadReduce, 1, 7, box happyReduction_4)
}

refute! { fn happyReduction_4<T>(HappyStk(HappyAbsSyn8(happy_var_1), Some(box happyRest)): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({
                      let decls = reverse(happy_var_1);
                      if decls.len() == 0 {
                          thenP(getNewName(), box |n: Name| {
                              thenP(getCurrentPosition(), box move |p: Position| {
                                  let nodeinfo = NodeInfo::new(p.clone(), (p, 0), n);
                                  __return(CTranslationUnit(decls, nodeinfo))
                              })
                          })
                      } else {
                          let d = decls[0].clone();
                          withNodeInfo(d, box |_0| CTranslationUnit(decls, _0))
                      } }, (box move |r| { happyReturn(HappyAbsSyn7(r)) }))
}
}


fn happyReduce_5() -> ActionReturn {
    partial_5!(happySpecReduce_0, 8, happyReduction_5())
}

fn happyReduction_5() -> HappyAbsSyn {
    match () {
        () => HappyAbsSyn8(empty()),
    }
}


fn happyReduce_6() -> ActionReturn {
    partial_5!(happySpecReduce_2, 8, box happyReduction_6)
}

fn happyReduction_6(happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_2, happy_x_1) {
        (_, HappyAbsSyn8(happy_var_1)) => HappyAbsSyn8(happy_var_1),
        _ => notHappyAtAll()
    }
}


fn happyReduce_7() -> ActionReturn {
    partial_5!(happySpecReduce_2, 8, box happyReduction_7)
}

fn happyReduction_7(happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_2, happy_x_1) {
        (HappyAbsSyn9(happy_var_2), HappyAbsSyn8(happy_var_1)) => HappyAbsSyn8(snoc(happy_var_1, happy_var_2)),
        _ => notHappyAtAll()
    }
}


fn happyReduce_8() -> ActionReturn {
    partial_5!(happySpecReduce_1, 9, box happyReduction_8)
}

fn happyReduction_8(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn10(happy_var_1) => HappyAbsSyn9(CFDefExt(happy_var_1)),
        _ => notHappyAtAll()
    }
}


fn happyReduce_9() -> ActionReturn {
    partial_5!(happySpecReduce_1, 9, box happyReduction_9)
}

fn happyReduction_9(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn32(happy_var_1) => HappyAbsSyn9(CDeclExt(happy_var_1)),
        _ => notHappyAtAll()
    }
}


fn happyReduce_10() -> ActionReturn {
    partial_5!(happySpecReduce_2, 9, box happyReduction_10)
}

fn happyReduction_10(happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_2, happy_x_1) {
        (HappyAbsSyn9(happy_var_2), _) => HappyAbsSyn9(happy_var_2),
        _ => notHappyAtAll()
    }
}


fn happyReduce_11() -> ActionReturn {
    partial_5!(happyMonadReduce, 5, 9, box happyReduction_11)
}

refute! { fn happyReduction_11<T>(HappyStk(_, Some(box HappyStk(_, Some(box HappyStk(HappyAbsSyn128(happy_var_3), Some(box HappyStk(_, Some(box HappyStk(HappyTerminal(happy_var_1), Some(box happyRest)))))))))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1.clone(), partial_1!(CAsmExt, happy_var_3)) }, (box move |r| { happyReturn(HappyAbsSyn9(r)) }))
}
}


fn happyReduce_12() -> ActionReturn {
    partial_5!(happyMonadReduce, 2, 10, box happyReduction_12)
}

refute! { fn happyReduction_12<T>(HappyStk(HappyAbsSyn12(happy_var_2), Some(box HappyStk(HappyAbsSyn11(happy_var_1), Some(box happyRest)))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ rshift_monad(leaveScope(), withNodeInfo(happy_var_1.clone(), partial_1!(
            CFunctionDef, vec![], happy_var_1, vec![], happy_var_2))) }, (box move |r| { happyReturn(HappyAbsSyn10(r)) }))
}
}


fn happyReduce_13() -> ActionReturn {
    partial_5!(happyMonadReduce, 3, 10, box happyReduction_13)
}

refute! { fn happyReduction_13<T>(HappyStk(HappyAbsSyn12(happy_var_3), Some(box HappyStk(HappyAbsSyn11(happy_var_2), Some(box HappyStk(HappyAbsSyn132(happy_var_1), Some(box happyRest)))))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ rshift_monad(leaveScope(), withNodeInfo(happy_var_1.clone(), partial_1!(
            CFunctionDef, liftCAttrs(happy_var_1), happy_var_2, vec![], happy_var_3))) }, (box move |r| { happyReturn(HappyAbsSyn10(r)) }))
}
}


fn happyReduce_14() -> ActionReturn {
    partial_5!(happyMonadReduce, 3, 10, box happyReduction_14)
}

refute! { fn happyReduction_14<T>(HappyStk(HappyAbsSyn12(happy_var_3), Some(box HappyStk(HappyAbsSyn11(happy_var_2), Some(box HappyStk(HappyAbsSyn37(happy_var_1), Some(box happyRest)))))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ rshift_monad(leaveScope(), withNodeInfo(happy_var_1.clone(), partial_1!(
            CFunctionDef, happy_var_1, happy_var_2, vec![], happy_var_3))) }, (box move |r| { happyReturn(HappyAbsSyn10(r)) }))
}
}


fn happyReduce_15() -> ActionReturn {
    partial_5!(happyMonadReduce, 3, 10, box happyReduction_15)
}

refute! { fn happyReduction_15<T>(HappyStk(HappyAbsSyn12(happy_var_3), Some(box HappyStk(HappyAbsSyn11(happy_var_2), Some(box HappyStk(HappyAbsSyn37(happy_var_1), Some(box happyRest)))))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ rshift_monad(leaveScope(), withNodeInfo(happy_var_1.clone(), partial_1!(
            CFunctionDef, happy_var_1, happy_var_2, vec![], happy_var_3))) }, (box move |r| { happyReturn(HappyAbsSyn10(r)) }))
}
}


fn happyReduce_16() -> ActionReturn {
    partial_5!(happyMonadReduce, 3, 10, box happyReduction_16)
}

refute! { fn happyReduction_16<T>(HappyStk(HappyAbsSyn12(happy_var_3), Some(box HappyStk(HappyAbsSyn11(happy_var_2), Some(box HappyStk(HappyAbsSyn38(happy_var_1), Some(box happyRest)))))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ rshift_monad(leaveScope(), withNodeInfo(happy_var_1.clone(), partial_1!(
            CFunctionDef, reverse(happy_var_1), happy_var_2, vec![], happy_var_3))) }, (box move |r| { happyReturn(HappyAbsSyn10(r)) }))
}
}


fn happyReduce_17() -> ActionReturn {
    partial_5!(happyMonadReduce, 3, 10, box happyReduction_17)
}

refute! { fn happyReduction_17<T>(HappyStk(HappyAbsSyn12(happy_var_3), Some(box HappyStk(HappyAbsSyn11(happy_var_2), Some(box HappyStk(HappyAbsSyn65(happy_var_1), Some(box happyRest)))))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ rshift_monad(leaveScope(), withNodeInfo(happy_var_1.clone(), partial_1!(
            CFunctionDef, liftTypeQuals(happy_var_1), happy_var_2, vec![], happy_var_3))) }, (box move |r| { happyReturn(HappyAbsSyn10(r)) }))
}
}


fn happyReduce_18() -> ActionReturn {
    partial_5!(happyMonadReduce, 4, 10, box happyReduction_18)
}

refute! { fn happyReduction_18<T>(HappyStk(HappyAbsSyn12(happy_var_4), Some(box HappyStk(HappyAbsSyn11(happy_var_3), Some(box HappyStk(HappyAbsSyn132(happy_var_2), Some(box HappyStk(HappyAbsSyn65(happy_var_1), Some(box happyRest)))))))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ rshift_monad(leaveScope(), withNodeInfo(happy_var_1.clone(), partial_1!(
            CFunctionDef, __op_addadd(liftTypeQuals(happy_var_1), liftCAttrs(happy_var_2)), happy_var_3, vec![], happy_var_4))) }, (box move |r| { happyReturn(HappyAbsSyn10(r)) }))
}
}


fn happyReduce_19() -> ActionReturn {
    partial_5!(happyMonadReduce, 3, 10, box happyReduction_19)
}

refute! { fn happyReduction_19<T>(HappyStk(HappyAbsSyn12(happy_var_3), Some(box HappyStk(HappyAbsSyn33(happy_var_2), Some(box HappyStk(HappyAbsSyn11(happy_var_1), Some(box happyRest)))))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1.clone(), partial_1!(CFunctionDef, vec![], happy_var_1, reverse(happy_var_2), happy_var_3)) }, (box move |r| { happyReturn(HappyAbsSyn10(r)) }))
}
}


fn happyReduce_20() -> ActionReturn {
    partial_5!(happyMonadReduce, 4, 10, box happyReduction_20)
}

refute! { fn happyReduction_20<T>(HappyStk(HappyAbsSyn12(happy_var_4), Some(box HappyStk(HappyAbsSyn33(happy_var_3), Some(box HappyStk(HappyAbsSyn11(happy_var_2), Some(box HappyStk(HappyAbsSyn132(happy_var_1), Some(box happyRest)))))))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_2.clone(), partial_1!(CFunctionDef, liftCAttrs(happy_var_1), happy_var_2, reverse(happy_var_3), happy_var_4)) }, (box move |r| { happyReturn(HappyAbsSyn10(r)) }))
}
}


fn happyReduce_21() -> ActionReturn {
    partial_5!(happyMonadReduce, 4, 10, box happyReduction_21)
}

refute! { fn happyReduction_21<T>(HappyStk(HappyAbsSyn12(happy_var_4), Some(box HappyStk(HappyAbsSyn33(happy_var_3), Some(box HappyStk(HappyAbsSyn11(happy_var_2), Some(box HappyStk(HappyAbsSyn37(happy_var_1), Some(box happyRest)))))))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1.clone(), partial_1!(CFunctionDef, happy_var_1, happy_var_2, reverse(happy_var_3), happy_var_4)) }, (box move |r| { happyReturn(HappyAbsSyn10(r)) }))
}
}


fn happyReduce_22() -> ActionReturn {
    partial_5!(happyMonadReduce, 4, 10, box happyReduction_22)
}

refute! { fn happyReduction_22<T>(HappyStk(HappyAbsSyn12(happy_var_4), Some(box HappyStk(HappyAbsSyn33(happy_var_3), Some(box HappyStk(HappyAbsSyn11(happy_var_2), Some(box HappyStk(HappyAbsSyn37(happy_var_1), Some(box happyRest)))))))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1.clone(), partial_1!(CFunctionDef, happy_var_1, happy_var_2, reverse(happy_var_3), happy_var_4)) }, (box move |r| { happyReturn(HappyAbsSyn10(r)) }))
}
}


fn happyReduce_23() -> ActionReturn {
    partial_5!(happyMonadReduce, 4, 10, box happyReduction_23)
}

refute! { fn happyReduction_23<T>(HappyStk(HappyAbsSyn12(happy_var_4), Some(box HappyStk(HappyAbsSyn33(happy_var_3), Some(box HappyStk(HappyAbsSyn11(happy_var_2), Some(box HappyStk(HappyAbsSyn38(happy_var_1), Some(box happyRest)))))))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1.clone(), partial_1!(CFunctionDef, reverse(happy_var_1), happy_var_2, reverse(happy_var_3), happy_var_4)) }, (box move |r| { happyReturn(HappyAbsSyn10(r)) }))
}
}


fn happyReduce_24() -> ActionReturn {
    partial_5!(happyMonadReduce, 4, 10, box happyReduction_24)
}

refute! { fn happyReduction_24<T>(HappyStk(HappyAbsSyn12(happy_var_4), Some(box HappyStk(HappyAbsSyn33(happy_var_3), Some(box HappyStk(HappyAbsSyn11(happy_var_2), Some(box HappyStk(HappyAbsSyn65(happy_var_1), Some(box happyRest)))))))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1.clone(), partial_1!(CFunctionDef, liftTypeQuals(happy_var_1), happy_var_2, reverse(happy_var_3), happy_var_4)) }, (box move |r| { happyReturn(HappyAbsSyn10(r)) }))
}
}


fn happyReduce_25() -> ActionReturn {
    partial_5!(happyMonadReduce, 5, 10, box happyReduction_25)
}

refute! { fn happyReduction_25<T>(HappyStk(HappyAbsSyn12(happy_var_5), Some(box HappyStk(HappyAbsSyn33(happy_var_4), Some(box HappyStk(HappyAbsSyn11(happy_var_3), Some(box HappyStk(HappyAbsSyn132(happy_var_2), Some(box HappyStk(HappyAbsSyn65(happy_var_1), Some(box happyRest)))))))))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1.clone(), partial_1!(
            CFunctionDef, __op_addadd(liftTypeQuals(happy_var_1), liftCAttrs(happy_var_2)), happy_var_3, reverse(happy_var_4), happy_var_5)) }, (box move |r| { happyReturn(HappyAbsSyn10(r)) }))
}
}


fn happyReduce_26() -> ActionReturn {
    partial_5!(happyMonadReduce, 1, 11, box happyReduction_26)
}

refute! { fn happyReduction_26<T>(HappyStk(HappyAbsSyn66(happy_var_1), Some(box happyRest)): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({
            let declr = reverseDeclr(happy_var_1);
            rshift_monad(enterScope(), rshift_monad(doFuncParamDeclIdent(declr.clone()), __return(declr))) }, (box move |r| { happyReturn(HappyAbsSyn11(r)) }))
}
}


fn happyReduce_27() -> ActionReturn {
    partial_5!(happySpecReduce_1, 12, box happyReduction_27)
}

fn happyReduction_27(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn12(happy_var_1) => HappyAbsSyn12(happy_var_1),
        _ => notHappyAtAll()
    }
}


fn happyReduce_28() -> ActionReturn {
    partial_5!(happySpecReduce_1, 12, box happyReduction_28)
}

fn happyReduction_28(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn12(happy_var_1) => HappyAbsSyn12(happy_var_1),
        _ => notHappyAtAll()
    }
}


fn happyReduce_29() -> ActionReturn {
    partial_5!(happySpecReduce_1, 12, box happyReduction_29)
}

fn happyReduction_29(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn12(happy_var_1) => HappyAbsSyn12(happy_var_1),
        _ => notHappyAtAll()
    }
}


fn happyReduce_30() -> ActionReturn {
    partial_5!(happySpecReduce_1, 12, box happyReduction_30)
}

fn happyReduction_30(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn12(happy_var_1) => HappyAbsSyn12(happy_var_1),
        _ => notHappyAtAll()
    }
}


fn happyReduce_31() -> ActionReturn {
    partial_5!(happySpecReduce_1, 12, box happyReduction_31)
}

fn happyReduction_31(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn12(happy_var_1) => HappyAbsSyn12(happy_var_1),
        _ => notHappyAtAll()
    }
}


fn happyReduce_32() -> ActionReturn {
    partial_5!(happySpecReduce_1, 12, box happyReduction_32)
}

fn happyReduction_32(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn12(happy_var_1) => HappyAbsSyn12(happy_var_1),
        _ => notHappyAtAll()
    }
}


fn happyReduce_33() -> ActionReturn {
    partial_5!(happyMonadReduce, 1, 12, box happyReduction_33)
}

refute! { fn happyReduction_33<T>(HappyStk(HappyAbsSyn26(happy_var_1), Some(box happyRest)): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1.clone(), partial_1!(CAsm, happy_var_1)) }, (box move |r| { happyReturn(HappyAbsSyn12(r)) }))
}
}


fn happyReduce_34() -> ActionReturn {
    partial_5!(happyMonadReduce, 4, 13, box happyReduction_34)
}

refute! { fn happyReduction_34<T>(HappyStk(HappyAbsSyn12(happy_var_4), Some(box HappyStk(HappyAbsSyn132(happy_var_3), Some(box HappyStk(_, Some(box HappyStk(HappyAbsSyn131(happy_var_1), Some(box happyRest)))))))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1.clone(), partial_1!(CLabel, happy_var_1, box happy_var_4, happy_var_3)) }, (box move |r| { happyReturn(HappyAbsSyn12(r)) }))
}
}


fn happyReduce_35() -> ActionReturn {
    partial_5!(happyMonadReduce, 4, 13, box happyReduction_35)
}

refute! { fn happyReduction_35<T>(HappyStk(HappyAbsSyn12(happy_var_4), Some(box HappyStk(_, Some(box HappyStk(HappyAbsSyn100(happy_var_2), Some(box HappyStk(HappyTerminal(happy_var_1), Some(box happyRest)))))))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1, partial_1!(CCase, happy_var_2, box happy_var_4)) }, (box move |r| { happyReturn(HappyAbsSyn12(r)) }))
}
}


fn happyReduce_36() -> ActionReturn {
    partial_5!(happyMonadReduce, 3, 13, box happyReduction_36)
}

refute! { fn happyReduction_36<T>(HappyStk(HappyAbsSyn12(happy_var_3), Some(box HappyStk(_, Some(box HappyStk(HappyTerminal(happy_var_1), Some(box happyRest)))))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1, partial_1!(CDefault, box happy_var_3)) }, (box move |r| { happyReturn(HappyAbsSyn12(r)) }))
}
}


fn happyReduce_37() -> ActionReturn {
    partial_5!(happyMonadReduce, 6, 13, box happyReduction_37)
}

refute! { fn happyReduction_37<T>(HappyStk(HappyAbsSyn12(happy_var_6), Some(box HappyStk(_, Some(box HappyStk(HappyAbsSyn100(happy_var_4), Some(box HappyStk(_, Some(box HappyStk(HappyAbsSyn100(happy_var_2), Some(box HappyStk(HappyTerminal(happy_var_1), Some(box happyRest)))))))))))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1, partial_1!(CCases, happy_var_2, happy_var_4, box happy_var_6)) }, (box move |r| { happyReturn(HappyAbsSyn12(r)) }))
}
}


fn happyReduce_38() -> ActionReturn {
    partial_5!(happyMonadReduce, 5, 14, box happyReduction_38)
}

refute! { fn happyReduction_38<T>(HappyStk(_, Some(box HappyStk(_, Some(box HappyStk(HappyAbsSyn17(happy_var_3), Some(box HappyStk(_, Some(box HappyStk(HappyTerminal(happy_var_1), Some(box happyRest)))))))))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1, partial_1!(CCompound, vec![], reverse(happy_var_3))) }, (box move |r| { happyReturn(HappyAbsSyn12(r)) }))
}
}


fn happyReduce_39() -> ActionReturn {
    partial_5!(happyMonadReduce, 6, 14, box happyReduction_39)
}

refute! { fn happyReduction_39<T>(HappyStk(_, Some(box HappyStk(_, Some(box HappyStk(HappyAbsSyn17(happy_var_4), Some(box HappyStk(HappyAbsSyn21(happy_var_3), Some(box HappyStk(_, Some(box HappyStk(HappyTerminal(happy_var_1), Some(box happyRest)))))))))))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1, partial_1!(CCompound, reverse(happy_var_3), reverse(happy_var_4))) }, (box move |r| { happyReturn(HappyAbsSyn12(r)) }))
}
}


fn happyReduce_40() -> ActionReturn {
    partial_5!(happyMonadReduce, 0, 15, box happyReduction_40)
}

refute! { fn happyReduction_40<T>(happyRest: HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ enterScope() }, (box move |r| { happyReturn(HappyAbsSyn15(r)) }))
}
}


fn happyReduce_41() -> ActionReturn {
    partial_5!(happyMonadReduce, 0, 16, box happyReduction_41)
}

refute! { fn happyReduction_41<T>(happyRest: HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ leaveScope() }, (box move |r| { happyReturn(HappyAbsSyn15(r)) }))
}
}


fn happyReduce_42() -> ActionReturn {
    partial_5!(happySpecReduce_0, 17, happyReduction_42())
}

fn happyReduction_42() -> HappyAbsSyn {
    match () {
        () => HappyAbsSyn17(empty()),
    }
}


fn happyReduce_43() -> ActionReturn {
    partial_5!(happySpecReduce_2, 17, box happyReduction_43)
}

fn happyReduction_43(happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_2, happy_x_1) {
        (HappyAbsSyn18(happy_var_2), HappyAbsSyn17(happy_var_1)) => HappyAbsSyn17(snoc(happy_var_1, happy_var_2)),
        _ => notHappyAtAll()
    }
}


fn happyReduce_44() -> ActionReturn {
    partial_5!(happySpecReduce_1, 18, box happyReduction_44)
}

fn happyReduction_44(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn12(happy_var_1) => HappyAbsSyn18(CBlockStmt(happy_var_1)),
        _ => notHappyAtAll()
    }
}


fn happyReduce_45() -> ActionReturn {
    partial_5!(happySpecReduce_1, 18, box happyReduction_45)
}

fn happyReduction_45(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn18(happy_var_1) => HappyAbsSyn18(happy_var_1),
        _ => notHappyAtAll()
    }
}


fn happyReduce_46() -> ActionReturn {
    partial_5!(happySpecReduce_1, 19, box happyReduction_46)
}

fn happyReduction_46(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn32(happy_var_1) => HappyAbsSyn18(CBlockDecl(happy_var_1)),
        _ => notHappyAtAll()
    }
}


fn happyReduce_47() -> ActionReturn {
    partial_5!(happySpecReduce_1, 19, box happyReduction_47)
}

fn happyReduction_47(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn10(happy_var_1) => HappyAbsSyn18(CNestedFunDef(happy_var_1)),
        _ => notHappyAtAll()
    }
}


fn happyReduce_48() -> ActionReturn {
    partial_5!(happySpecReduce_2, 19, box happyReduction_48)
}

fn happyReduction_48(happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_2, happy_x_1) {
        (HappyAbsSyn18(happy_var_2), _) => HappyAbsSyn18(happy_var_2),
        _ => notHappyAtAll()
    }
}


fn happyReduce_49() -> ActionReturn {
    partial_5!(happyMonadReduce, 3, 20, box happyReduction_49)
}

refute! { fn happyReduction_49<T>(HappyStk(HappyAbsSyn12(happy_var_3), Some(box HappyStk(HappyAbsSyn11(happy_var_2), Some(box HappyStk(HappyAbsSyn37(happy_var_1), Some(box happyRest)))))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ rshift_monad(leaveScope(), withNodeInfo(happy_var_1.clone(), partial_1!(
            CFunctionDef, happy_var_1, happy_var_2, vec![], happy_var_3))) }, (box move |r| { happyReturn(HappyAbsSyn10(r)) }))
}
}


fn happyReduce_50() -> ActionReturn {
    partial_5!(happyMonadReduce, 3, 20, box happyReduction_50)
}

refute! { fn happyReduction_50<T>(HappyStk(HappyAbsSyn12(happy_var_3), Some(box HappyStk(HappyAbsSyn11(happy_var_2), Some(box HappyStk(HappyAbsSyn37(happy_var_1), Some(box happyRest)))))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ rshift_monad(leaveScope(), withNodeInfo(happy_var_1.clone(), partial_1!(
            CFunctionDef, happy_var_1, happy_var_2, vec![], happy_var_3))) }, (box move |r| { happyReturn(HappyAbsSyn10(r)) }))
}
}


fn happyReduce_51() -> ActionReturn {
    partial_5!(happyMonadReduce, 3, 20, box happyReduction_51)
}

refute! { fn happyReduction_51<T>(HappyStk(HappyAbsSyn12(happy_var_3), Some(box HappyStk(HappyAbsSyn11(happy_var_2), Some(box HappyStk(HappyAbsSyn38(happy_var_1), Some(box happyRest)))))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ rshift_monad(leaveScope(), withNodeInfo(happy_var_1.clone(), partial_1!(
            CFunctionDef, reverse(happy_var_1), happy_var_2, vec![], happy_var_3))) }, (box move |r| { happyReturn(HappyAbsSyn10(r)) }))
}
}


fn happyReduce_52() -> ActionReturn {
    partial_5!(happyMonadReduce, 3, 20, box happyReduction_52)
}

refute! { fn happyReduction_52<T>(HappyStk(HappyAbsSyn12(happy_var_3), Some(box HappyStk(HappyAbsSyn11(happy_var_2), Some(box HappyStk(HappyAbsSyn65(happy_var_1), Some(box happyRest)))))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ rshift_monad(leaveScope(), withNodeInfo(happy_var_1.clone(), partial_1!(
            CFunctionDef, liftTypeQuals(happy_var_1), happy_var_2, vec![], happy_var_3))) }, (box move |r| { happyReturn(HappyAbsSyn10(r)) }))
}
}


fn happyReduce_53() -> ActionReturn {
    partial_5!(happyMonadReduce, 4, 20, box happyReduction_53)
}

refute! { fn happyReduction_53<T>(HappyStk(HappyAbsSyn12(happy_var_4), Some(box HappyStk(HappyAbsSyn11(happy_var_3), Some(box HappyStk(HappyAbsSyn132(happy_var_2), Some(box HappyStk(HappyAbsSyn65(happy_var_1), Some(box happyRest)))))))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ rshift_monad(leaveScope(), withNodeInfo(happy_var_1.clone(), partial_1!(
            CFunctionDef, __op_addadd(liftTypeQuals(happy_var_1), liftCAttrs(happy_var_2)), happy_var_3, vec![], happy_var_4))) }, (box move |r| { happyReturn(HappyAbsSyn10(r)) }))
}
}


fn happyReduce_54() -> ActionReturn {
    partial_5!(happySpecReduce_3, 21, box happyReduction_54)
}

fn happyReduction_54(happy_x_3: HappyAbsSyn, happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_3, happy_x_2, happy_x_1) {
        (_, HappyAbsSyn21(happy_var_2), _) => HappyAbsSyn21(happy_var_2),
        _ => notHappyAtAll()
    }
}


fn happyReduce_55() -> ActionReturn {
    partial_5!(happyReduce, 4, 21, box happyReduction_55)
}

refute! { fn happyReduction_55(HappyStk(_, Some(box HappyStk(HappyAbsSyn21(happy_var_3), Some(box HappyStk(_, Some(box HappyStk(HappyAbsSyn21(happy_var_1), Some(box happyRest)))))))): HappyStk<HappyAbsSyn>) -> HappyStk<HappyAbsSyn> {
    HappyStk(HappyAbsSyn21(rappendr(happy_var_1, happy_var_3)), Some(box happyRest))
}
}


fn happyReduce_56() -> ActionReturn {
    partial_5!(happyMonadReduce, 1, 22, box happyReduction_56)
}

refute! { fn happyReduction_56<T>(HappyStk(HappyTerminal(happy_var_1), Some(box happyRest)): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1, partial_1!(CExpr, None)) }, (box move |r| { happyReturn(HappyAbsSyn12(r)) }))
}
}


fn happyReduce_57() -> ActionReturn {
    partial_5!(happyMonadReduce, 2, 22, box happyReduction_57)
}

refute! { fn happyReduction_57<T>(HappyStk(_, Some(box HappyStk(HappyAbsSyn100(happy_var_1), Some(box happyRest)))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1.clone(), partial_1!(CExpr, Some(happy_var_1))) }, (box move |r| { happyReturn(HappyAbsSyn12(r)) }))
}
}


fn happyReduce_58() -> ActionReturn {
    partial_5!(happyMonadReduce, 5, 23, box happyReduction_58)
}

refute! { fn happyReduction_58<T>(HappyStk(HappyAbsSyn12(happy_var_5), Some(box HappyStk(_, Some(box HappyStk(HappyAbsSyn100(happy_var_3), Some(box HappyStk(_, Some(box HappyStk(HappyTerminal(happy_var_1), Some(box happyRest)))))))))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1, partial_1!(CIf, happy_var_3, box happy_var_5, None)) }, (box move |r| { happyReturn(HappyAbsSyn12(r)) }))
}
}


fn happyReduce_59() -> ActionReturn {
    partial_5!(happyMonadReduce, 7, 23, box happyReduction_59)
}

refute! { fn happyReduction_59<T>(HappyStk(HappyAbsSyn12(happy_var_7), Some(box HappyStk(_, Some(box HappyStk(HappyAbsSyn12(happy_var_5), Some(box HappyStk(_, Some(box HappyStk(HappyAbsSyn100(happy_var_3), Some(box HappyStk(_, Some(box HappyStk(HappyTerminal(happy_var_1), Some(box happyRest)))))))))))))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1, partial_1!(CIf, happy_var_3, box happy_var_5, Some(box happy_var_7))) }, (box move |r| { happyReturn(HappyAbsSyn12(r)) }))
}
}


fn happyReduce_60() -> ActionReturn {
    partial_5!(happyMonadReduce, 5, 23, box happyReduction_60)
}

refute! { fn happyReduction_60<T>(HappyStk(HappyAbsSyn12(happy_var_5), Some(box HappyStk(_, Some(box HappyStk(HappyAbsSyn100(happy_var_3), Some(box HappyStk(_, Some(box HappyStk(HappyTerminal(happy_var_1), Some(box happyRest)))))))))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1, partial_1!(CSwitch, happy_var_3, box happy_var_5)) }, (box move |r| { happyReturn(HappyAbsSyn12(r)) }))
}
}


fn happyReduce_61() -> ActionReturn {
    partial_5!(happyMonadReduce, 5, 24, box happyReduction_61)
}

refute! { fn happyReduction_61<T>(HappyStk(HappyAbsSyn12(happy_var_5), Some(box HappyStk(_, Some(box HappyStk(HappyAbsSyn100(happy_var_3), Some(box HappyStk(_, Some(box HappyStk(HappyTerminal(happy_var_1), Some(box happyRest)))))))))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1, partial_1!(CWhile, happy_var_3, box happy_var_5, false)) }, (box move |r| { happyReturn(HappyAbsSyn12(r)) }))
}
}


fn happyReduce_62() -> ActionReturn {
    partial_5!(happyMonadReduce, 7, 24, box happyReduction_62)
}

refute! { fn happyReduction_62<T>(HappyStk(_, Some(box HappyStk(_, Some(box HappyStk(HappyAbsSyn100(happy_var_5), Some(box HappyStk(_, Some(box HappyStk(_, Some(box HappyStk(HappyAbsSyn12(happy_var_2), Some(box HappyStk(HappyTerminal(happy_var_1), Some(box happyRest)))))))))))))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1, partial_1!(CWhile, happy_var_5, box happy_var_2, true)) }, (box move |r| { happyReturn(HappyAbsSyn12(r)) }))
}
}


fn happyReduce_63() -> ActionReturn {
    partial_5!(happyMonadReduce, 9, 24, box happyReduction_63)
}

refute! { fn happyReduction_63<T>(HappyStk(HappyAbsSyn12(happy_var_9), Some(box HappyStk(_, Some(box HappyStk(HappyAbsSyn124(happy_var_7), Some(box HappyStk(_, Some(box HappyStk(HappyAbsSyn124(happy_var_5), Some(box HappyStk(_, Some(box HappyStk(HappyAbsSyn124(happy_var_3), Some(box HappyStk(_, Some(box HappyStk(HappyTerminal(happy_var_1), Some(box happyRest)))))))))))))))))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1, partial_1!(CFor, Left(happy_var_3), happy_var_5, happy_var_7, box happy_var_9)) }, (box move |r| { happyReturn(HappyAbsSyn12(r)) }))
}
}


fn happyReduce_64() -> ActionReturn {
    partial_5!(happyMonadReduce, 10, 24, box happyReduction_64)
}

refute! { fn happyReduction_64<T>(HappyStk(_, Some(box HappyStk(HappyAbsSyn12(happy_var_9), Some(box HappyStk(_, Some(box HappyStk(HappyAbsSyn124(happy_var_7), Some(box HappyStk(_, Some(box HappyStk(HappyAbsSyn124(happy_var_5), Some(box HappyStk(HappyAbsSyn32(happy_var_4), Some(box HappyStk(_, Some(box HappyStk(_, Some(box HappyStk(HappyTerminal(happy_var_1), Some(box happyRest)))))))))))))))))))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1, partial_1!(CFor, Right(happy_var_4), happy_var_5, happy_var_7, box happy_var_9)) }, (box move |r| { happyReturn(HappyAbsSyn12(r)) }))
}
}


fn happyReduce_65() -> ActionReturn {
    partial_5!(happyMonadReduce, 3, 25, box happyReduction_65)
}

refute! { fn happyReduction_65<T>(HappyStk(_, Some(box HappyStk(HappyAbsSyn131(happy_var_2), Some(box HappyStk(HappyTerminal(happy_var_1), Some(box happyRest)))))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1, partial_1!(CGoto, happy_var_2)) }, (box move |r| { happyReturn(HappyAbsSyn12(r)) }))
}
}


fn happyReduce_66() -> ActionReturn {
    partial_5!(happyMonadReduce, 4, 25, box happyReduction_66)
}

refute! { fn happyReduction_66<T>(HappyStk(_, Some(box HappyStk(HappyAbsSyn100(happy_var_3), Some(box HappyStk(_, Some(box HappyStk(HappyTerminal(happy_var_1), Some(box happyRest)))))))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1, partial_1!(CGotoPtr, happy_var_3)) }, (box move |r| { happyReturn(HappyAbsSyn12(r)) }))
}
}


fn happyReduce_67() -> ActionReturn {
    partial_5!(happyMonadReduce, 2, 25, box happyReduction_67)
}

refute! { fn happyReduction_67<T>(HappyStk(_, Some(box HappyStk(HappyTerminal(happy_var_1), Some(box happyRest)))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1, partial_1!(CCont)) }, (box move |r| { happyReturn(HappyAbsSyn12(r)) }))
}
}


fn happyReduce_68() -> ActionReturn {
    partial_5!(happyMonadReduce, 2, 25, box happyReduction_68)
}

refute! { fn happyReduction_68<T>(HappyStk(_, Some(box HappyStk(HappyTerminal(happy_var_1), Some(box happyRest)))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1, partial_1!(CBreak)) }, (box move |r| { happyReturn(HappyAbsSyn12(r)) }))
}
}


fn happyReduce_69() -> ActionReturn {
    partial_5!(happyMonadReduce, 3, 25, box happyReduction_69)
}

refute! { fn happyReduction_69<T>(HappyStk(_, Some(box HappyStk(HappyAbsSyn124(happy_var_2), Some(box HappyStk(HappyTerminal(happy_var_1), Some(box happyRest)))))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1, partial_1!(CReturn, happy_var_2)) }, (box move |r| { happyReturn(HappyAbsSyn12(r)) }))
}
}


fn happyReduce_70() -> ActionReturn {
    partial_5!(happyMonadReduce, 6, 26, box happyReduction_70)
}

refute! { fn happyReduction_70<T>(HappyStk(_, Some(box HappyStk(_, Some(box HappyStk(HappyAbsSyn128(happy_var_4), Some(box HappyStk(_, Some(box HappyStk(HappyAbsSyn27(happy_var_2), Some(box HappyStk(HappyTerminal(happy_var_1), Some(box happyRest)))))))))))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1, partial_1!(CAssemblyStatement, happy_var_2, happy_var_4, vec![], vec![], vec![])) }, (box move |r| { happyReturn(HappyAbsSyn26(r)) }))
}
}


fn happyReduce_71() -> ActionReturn {
    partial_5!(happyMonadReduce, 8, 26, box happyReduction_71)
}

refute! { fn happyReduction_71<T>(HappyStk(_, Some(box HappyStk(_, Some(box HappyStk(HappyAbsSyn28(happy_var_6), Some(box HappyStk(_, Some(box HappyStk(HappyAbsSyn128(happy_var_4), Some(box HappyStk(_, Some(box HappyStk(HappyAbsSyn27(happy_var_2), Some(box HappyStk(HappyTerminal(happy_var_1), Some(box happyRest)))))))))))))))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1, partial_1!(CAssemblyStatement, happy_var_2, happy_var_4, happy_var_6, vec![], vec![])) }, (box move |r| { happyReturn(HappyAbsSyn26(r)) }))
}
}


fn happyReduce_72() -> ActionReturn {
    partial_5!(happyMonadReduce, 10, 26, box happyReduction_72)
}

refute! { fn happyReduction_72<T>(HappyStk(_, Some(box HappyStk(_, Some(box HappyStk(HappyAbsSyn28(happy_var_8), Some(box HappyStk(_, Some(box HappyStk(HappyAbsSyn28(happy_var_6), Some(box HappyStk(_, Some(box HappyStk(HappyAbsSyn128(happy_var_4), Some(box HappyStk(_, Some(box HappyStk(HappyAbsSyn27(happy_var_2), Some(box HappyStk(HappyTerminal(happy_var_1), Some(box happyRest)))))))))))))))))))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1, partial_1!(CAssemblyStatement, happy_var_2, happy_var_4, happy_var_6, happy_var_8, vec![])) }, (box move |r| { happyReturn(HappyAbsSyn26(r)) }))
}
}


fn happyReduce_73() -> ActionReturn {
    partial_5!(happyMonadReduce, 12, 26, box happyReduction_73)
}

refute! { fn happyReduction_73<T>(HappyStk(_, Some(box HappyStk(_, Some(box HappyStk(HappyAbsSyn31(happy_var_10), Some(box HappyStk(_, Some(box HappyStk(HappyAbsSyn28(happy_var_8), Some(box HappyStk(_, Some(box HappyStk(HappyAbsSyn28(happy_var_6), Some(box HappyStk(_, Some(box HappyStk(HappyAbsSyn128(happy_var_4), Some(box HappyStk(_, Some(box HappyStk(HappyAbsSyn27(happy_var_2), Some(box HappyStk(HappyTerminal(happy_var_1), Some(box happyRest)))))))))))))))))))))))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1, partial_1!(CAssemblyStatement, happy_var_2, happy_var_4, happy_var_6, happy_var_8, reverse(happy_var_10))) }, (box move |r| { happyReturn(HappyAbsSyn26(r)) }))
}
}


fn happyReduce_74() -> ActionReturn {
    partial_5!(happySpecReduce_0, 27, happyReduction_74())
}

fn happyReduction_74() -> HappyAbsSyn {
    match () {
        () => HappyAbsSyn27(None),
    }
}


fn happyReduce_75() -> ActionReturn {
    partial_5!(happySpecReduce_1, 27, box happyReduction_75)
}

fn happyReduction_75(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn64(happy_var_1) => HappyAbsSyn27(Some(happy_var_1)),
        _ => notHappyAtAll()
    }
}


fn happyReduce_76() -> ActionReturn {
    partial_5!(happySpecReduce_0, 28, happyReduction_76())
}

fn happyReduction_76() -> HappyAbsSyn {
    match () {
        () => HappyAbsSyn28(vec![]),
    }
}


fn happyReduce_77() -> ActionReturn {
    partial_5!(happySpecReduce_1, 28, box happyReduction_77)
}

fn happyReduction_77(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn29(happy_var_1) => HappyAbsSyn28(reverse(happy_var_1)),
        _ => notHappyAtAll()
    }
}


fn happyReduce_78() -> ActionReturn {
    partial_5!(happySpecReduce_1, 29, box happyReduction_78)
}

fn happyReduction_78(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn30(happy_var_1) => HappyAbsSyn29(singleton(happy_var_1)),
        _ => notHappyAtAll()
    }
}


fn happyReduce_79() -> ActionReturn {
    partial_5!(happySpecReduce_3, 29, box happyReduction_79)
}

fn happyReduction_79(happy_x_3: HappyAbsSyn, happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_3, happy_x_2, happy_x_1) {
        (HappyAbsSyn30(happy_var_3), _, HappyAbsSyn29(happy_var_1)) => HappyAbsSyn29(snoc(happy_var_1, happy_var_3)),
        _ => notHappyAtAll()
    }
}


fn happyReduce_80() -> ActionReturn {
    partial_5!(happyMonadReduce, 4, 30, box happyReduction_80)
}

refute! { fn happyReduction_80<T>(HappyStk(_, Some(box HappyStk(HappyAbsSyn100(happy_var_3), Some(box HappyStk(_, Some(box HappyStk(HappyAbsSyn128(happy_var_1), Some(box happyRest)))))))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1.clone(), partial_1!(CAssemblyOperand, None, happy_var_1, happy_var_3)) }, (box move |r| { happyReturn(HappyAbsSyn30(r)) }))
}
}


fn happyReduce_81() -> ActionReturn {
    partial_5!(happyMonadReduce, 7, 30, box happyReduction_81)
}

refute! { fn happyReduction_81<T>(HappyStk(_, Some(box HappyStk(HappyAbsSyn100(happy_var_6), Some(box HappyStk(_, Some(box HappyStk(HappyAbsSyn128(happy_var_4), Some(box HappyStk(_, Some(box HappyStk(HappyTerminal(CTokIdent(_, happy_var_2)), Some(box HappyStk(HappyTerminal(happy_var_1), Some(box happyRest)))))))))))))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1, partial_1!(CAssemblyOperand, Some(happy_var_2), happy_var_4, happy_var_6)) }, (box move |r| { happyReturn(HappyAbsSyn30(r)) }))
}
}


fn happyReduce_82() -> ActionReturn {
    partial_5!(happyMonadReduce, 7, 30, box happyReduction_82)
}

refute! { fn happyReduction_82<T>(HappyStk(_, Some(box HappyStk(HappyAbsSyn100(happy_var_6), Some(box HappyStk(_, Some(box HappyStk(HappyAbsSyn128(happy_var_4), Some(box HappyStk(_, Some(box HappyStk(HappyTerminal(CTokTyIdent(_, happy_var_2)), Some(box HappyStk(HappyTerminal(happy_var_1), Some(box happyRest)))))))))))))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1, partial_1!(CAssemblyOperand, Some(happy_var_2), happy_var_4, happy_var_6)) }, (box move |r| { happyReturn(HappyAbsSyn30(r)) }))
}
}


fn happyReduce_83() -> ActionReturn {
    partial_5!(happySpecReduce_1, 31, box happyReduction_83)
}

fn happyReduction_83(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn128(happy_var_1) => HappyAbsSyn31(singleton(happy_var_1)),
        _ => notHappyAtAll()
    }
}


fn happyReduce_84() -> ActionReturn {
    partial_5!(happySpecReduce_3, 31, box happyReduction_84)
}

fn happyReduction_84(happy_x_3: HappyAbsSyn, happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_3, happy_x_2, happy_x_1) {
        (HappyAbsSyn128(happy_var_3), _, HappyAbsSyn31(happy_var_1)) => HappyAbsSyn31(snoc(happy_var_1, happy_var_3)),
        _ => notHappyAtAll()
    }
}


fn happyReduce_85() -> ActionReturn {
    partial_5!(happyMonadReduce, 2, 32, box happyReduction_85)
}

refute! { fn happyReduction_85<T>(HappyStk(_, Some(box HappyStk(HappyAbsSyn38(happy_var_1), Some(box happyRest)))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1.clone(), partial_1!(CDecl, reverse(happy_var_1), vec![])) }, (box move |r| { happyReturn(HappyAbsSyn32(r)) }))
}
}


fn happyReduce_86() -> ActionReturn {
    partial_5!(happyMonadReduce, 2, 32, box happyReduction_86)
}

refute! { fn happyReduction_86<T>(HappyStk(_, Some(box HappyStk(HappyAbsSyn38(happy_var_1), Some(box happyRest)))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1.clone(), partial_1!(CDecl, reverse(happy_var_1), vec![])) }, (box move |r| { happyReturn(HappyAbsSyn32(r)) }))
}
}


fn happyReduce_87() -> ActionReturn {
    partial_5!(happyMonadReduce, 2, 32, box happyReduction_87)
}

refute! { fn happyReduction_87<T>(HappyStk(_, Some(box HappyStk(HappyAbsSyn32(happy_var_1), Some(box happyRest)))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({
            if let CDecl(declspecs, dies, at) = happy_var_1 {
                withLength(at, partial_1!(CDecl, declspecs, List::reverse(dies)))
            } else {
                panic!("irrefutable pattern")
            } }, (box move |r| { happyReturn(HappyAbsSyn32(r)) }))
}
}


fn happyReduce_88() -> ActionReturn {
    partial_5!(happyMonadReduce, 2, 32, box happyReduction_88)
}

refute! { fn happyReduction_88<T>(HappyStk(_, Some(box HappyStk(HappyAbsSyn32(happy_var_1), Some(box happyRest)))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({
            if let CDecl(declspecs, dies, at) = happy_var_1 {
                withLength(at, partial_1!(CDecl, declspecs, List::reverse(dies)))
            } else {
                panic!("irrefutable pattern")
            } }, (box move |r| { happyReturn(HappyAbsSyn32(r)) }))
}
}


fn happyReduce_89() -> ActionReturn {
    partial_5!(happyMonadReduce, 7, 32, box happyReduction_89)
}

refute! { fn happyReduction_89<T>(HappyStk(_, Some(box HappyStk(_, Some(box HappyStk(HappyAbsSyn128(happy_var_5), Some(box HappyStk(_, Some(box HappyStk(HappyAbsSyn100(happy_var_3), Some(box HappyStk(_, Some(box HappyStk(HappyTerminal(happy_var_1), Some(box happyRest)))))))))))))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1, partial_1!(CStaticAssert, happy_var_3, happy_var_5)) }, (box move |r| { happyReturn(HappyAbsSyn32(r)) }))
}
}


fn happyReduce_90() -> ActionReturn {
    partial_5!(happySpecReduce_0, 33, happyReduction_90())
}

fn happyReduction_90() -> HappyAbsSyn {
    match () {
        () => HappyAbsSyn33(empty()),
    }
}


fn happyReduce_91() -> ActionReturn {
    partial_5!(happySpecReduce_2, 33, box happyReduction_91)
}

fn happyReduction_91(happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_2, happy_x_1) {
        (HappyAbsSyn32(happy_var_2), HappyAbsSyn33(happy_var_1)) => HappyAbsSyn33(snoc(happy_var_1, happy_var_2)),
        _ => notHappyAtAll()
    }
}


fn happyReduce_92() -> ActionReturn {
    partial_5!(happyMonadReduce, 4, 34, box happyReduction_92)
}

refute! { fn happyReduction_92<T>(HappyStk(HappyAbsSyn94(happy_var_4), Some(box HappyStk(HappyAbsSyn35(happy_var_3), Some(box HappyStk(HappyAbsSyn66(happy_var_2), Some(box HappyStk(HappyAbsSyn38(happy_var_1), Some(box happyRest)))))))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({
            let declspecs = reverse(happy_var_1.clone());
            thenP(withAsmNameAttrs(happy_var_3, happy_var_2), box move |declr: CDeclrR| {
                rshift_monad(
                    // TODO: borrow these instead
                    doDeclIdent(declspecs.clone(), declr.clone()),
                    withNodeInfo(happy_var_1, partial_1!(CDecl, declspecs,
                                                vec![(Some(reverseDeclr(declr)), happy_var_4, None)])))
            }) }, (box move |r| { happyReturn(HappyAbsSyn32(r)) }))
}
}


fn happyReduce_93() -> ActionReturn {
    partial_5!(happyMonadReduce, 4, 34, box happyReduction_93)
}

refute! { fn happyReduction_93<T>(HappyStk(HappyAbsSyn94(happy_var_4), Some(box HappyStk(HappyAbsSyn35(happy_var_3), Some(box HappyStk(HappyAbsSyn66(happy_var_2), Some(box HappyStk(HappyAbsSyn65(happy_var_1), Some(box happyRest)))))))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({
            let declspecs = liftTypeQuals(happy_var_1.clone());
            thenP(withAsmNameAttrs(happy_var_3, happy_var_2), box move |declr: CDeclrR| {
                rshift_monad(
                    doDeclIdent(declspecs.clone(), declr.clone()),
                    withNodeInfo(happy_var_1, partial_1!(CDecl, declspecs,
                                                vec![(Some(reverseDeclr(declr)), happy_var_4, None)])))
            }) }, (box move |r| { happyReturn(HappyAbsSyn32(r)) }))
}
}


fn happyReduce_94() -> ActionReturn {
    partial_5!(happyMonadReduce, 5, 34, box happyReduction_94)
}

refute! { fn happyReduction_94<T>(HappyStk(HappyAbsSyn94(happy_var_5), Some(box HappyStk(HappyAbsSyn35(happy_var_4), Some(box HappyStk(HappyAbsSyn66(happy_var_3), Some(box HappyStk(HappyAbsSyn132(happy_var_2), Some(box HappyStk(HappyAbsSyn65(happy_var_1), Some(box happyRest)))))))))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({
            let declspecs = liftTypeQuals(happy_var_1.clone());
            thenP(withAsmNameAttrs(happy_var_4, happy_var_3), box move |declr: CDeclrR| {
                rshift_monad(
                    doDeclIdent(declspecs.clone(), declr.clone()),
                    withNodeInfo(happy_var_1, partial_1!(CDecl, __op_addadd(declspecs, liftCAttrs(happy_var_2)),
                                                vec![(Some(reverseDeclr(declr)), happy_var_5, None)])))
            }) }, (box move |r| { happyReturn(HappyAbsSyn32(r)) }))
}
}


fn happyReduce_95() -> ActionReturn {
    partial_5!(happyMonadReduce, 4, 34, box happyReduction_95)
}

refute! { fn happyReduction_95<T>(HappyStk(HappyAbsSyn94(happy_var_4), Some(box HappyStk(HappyAbsSyn35(happy_var_3), Some(box HappyStk(HappyAbsSyn66(happy_var_2), Some(box HappyStk(HappyAbsSyn132(happy_var_1), Some(box happyRest)))))))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({
            let declspecs = liftCAttrs(happy_var_1.clone());
            thenP(withAsmNameAttrs(happy_var_3, happy_var_2), box move |declr: CDeclrR| {
                rshift_monad(
                    doDeclIdent(declspecs.clone(), declr.clone()),
                    withNodeInfo(happy_var_1, partial_1!(CDecl, declspecs,
                                                vec![(Some(reverseDeclr(declr)), happy_var_4, None)])))
            }) }, (box move |r| { happyReturn(HappyAbsSyn32(r)) }))
}
}


fn happyReduce_96() -> ActionReturn {
    partial_5!(happyMonadReduce, 6, 34, box happyReduction_96)
}

refute! { fn happyReduction_96<T>(HappyStk(HappyAbsSyn94(happy_var_6), Some(box HappyStk(HappyAbsSyn35(happy_var_5), Some(box HappyStk(HappyAbsSyn66(happy_var_4), Some(box HappyStk(HappyAbsSyn132(happy_var_3), Some(box HappyStk(_, Some(box HappyStk(HappyAbsSyn32(happy_var_1), Some(box happyRest)))))))))))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({
            if let CDecl(declspecs, dies, at) = happy_var_1 {
                let (f, s) = happy_var_5;
                thenP(withAsmNameAttrs((f, __op_addadd(s, happy_var_3)), happy_var_4), box move |declr: CDeclrR| {
                    rshift_monad(
                        doDeclIdent(declspecs.clone(), declr.clone()),
                        withLength(at, partial_1!(CDecl, declspecs,
                                                  __op_concat((Some(reverseDeclr(declr)), happy_var_6, None), dies))))
                })
            } else {
                panic!("irrefutable pattern")
            } }, (box move |r| { happyReturn(HappyAbsSyn32(r)) }))
}
}


fn happyReduce_97() -> ActionReturn {
    partial_5!(happySpecReduce_2, 35, box happyReduction_97)
}

fn happyReduction_97(happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_2, happy_x_1) {
        (HappyAbsSyn132(happy_var_2), HappyAbsSyn67(happy_var_1)) => HappyAbsSyn35((happy_var_1, happy_var_2)),
        _ => notHappyAtAll()
    }
}


fn happyReduce_98() -> ActionReturn {
    partial_5!(happyMonadReduce, 4, 36, box happyReduction_98)
}

refute! { fn happyReduction_98<T>(HappyStk(HappyAbsSyn94(happy_var_4), Some(box HappyStk(HappyAbsSyn35(happy_var_3), Some(box HappyStk(HappyAbsSyn66(happy_var_2), Some(box HappyStk(HappyAbsSyn37(happy_var_1), Some(box happyRest)))))))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({
            thenP(withAsmNameAttrs(happy_var_3, happy_var_2), box move |declr: CDeclrR| {
                rshift_monad(
                    doDeclIdent(happy_var_1.clone(), declr.clone()),
                    withNodeInfo(happy_var_1.clone(), partial_1!(CDecl, happy_var_1, vec![(Some(reverseDeclr(declr)), happy_var_4, None)])))
            }) }, (box move |r| { happyReturn(HappyAbsSyn32(r)) }))
}
}


fn happyReduce_99() -> ActionReturn {
    partial_5!(happyMonadReduce, 4, 36, box happyReduction_99)
}

refute! { fn happyReduction_99<T>(HappyStk(HappyAbsSyn94(happy_var_4), Some(box HappyStk(HappyAbsSyn35(happy_var_3), Some(box HappyStk(HappyAbsSyn66(happy_var_2), Some(box HappyStk(HappyAbsSyn37(happy_var_1), Some(box happyRest)))))))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({
            thenP(withAsmNameAttrs(happy_var_3, happy_var_2), box move |declr: CDeclrR| {
                rshift_monad(
                    doDeclIdent(happy_var_1.clone(), declr.clone()),
                    withNodeInfo(happy_var_1.clone(), partial_1!(CDecl, happy_var_1, vec![(Some(reverseDeclr(declr)), happy_var_4, None)])))
            }) }, (box move |r| { happyReturn(HappyAbsSyn32(r)) }))
}
}


fn happyReduce_100() -> ActionReturn {
    partial_5!(happyMonadReduce, 6, 36, box happyReduction_100)
}

refute! { fn happyReduction_100<T>(HappyStk(HappyAbsSyn94(happy_var_6), Some(box HappyStk(HappyAbsSyn35(happy_var_5), Some(box HappyStk(HappyAbsSyn66(happy_var_4), Some(box HappyStk(HappyAbsSyn132(happy_var_3), Some(box HappyStk(_, Some(box HappyStk(HappyAbsSyn32(happy_var_1), Some(box happyRest)))))))))))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({
            if let CDecl(declspecs, dies, at) = happy_var_1 {
                let (f, s) = happy_var_5;
                thenP(withAsmNameAttrs((f, __op_addadd(s, happy_var_3)), happy_var_4), box move |declr: CDeclrR| {
                    rshift_monad(
                        doDeclIdent(declspecs.clone(), declr.clone()),
                        __return(CDecl(declspecs, __op_concat((Some(reverseDeclr(declr)), happy_var_6, None),
                                                              dies), at)))
                })
            } else {
                panic!("irrefutable pattern")
            } }, (box move |r| { happyReturn(HappyAbsSyn32(r)) }))
}
}


fn happyReduce_101() -> ActionReturn {
    partial_5!(happySpecReduce_1, 37, box happyReduction_101)
}

fn happyReduction_101(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn38(happy_var_1) => HappyAbsSyn37(reverse(happy_var_1)),
        _ => notHappyAtAll()
    }
}


fn happyReduce_102() -> ActionReturn {
    partial_5!(happySpecReduce_1, 37, box happyReduction_102)
}

fn happyReduction_102(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn38(happy_var_1) => HappyAbsSyn37(reverse(happy_var_1)),
        _ => notHappyAtAll()
    }
}


fn happyReduce_103() -> ActionReturn {
    partial_5!(happySpecReduce_1, 37, box happyReduction_103)
}

fn happyReduction_103(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn38(happy_var_1) => HappyAbsSyn37(reverse(happy_var_1)),
        _ => notHappyAtAll()
    }
}


fn happyReduce_104() -> ActionReturn {
    partial_5!(happySpecReduce_1, 38, box happyReduction_104)
}

fn happyReduction_104(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn39(happy_var_1) => HappyAbsSyn38(singleton(happy_var_1)),
        _ => notHappyAtAll()
    }
}


fn happyReduce_105() -> ActionReturn {
    partial_5!(happySpecReduce_2, 38, box happyReduction_105)
}

fn happyReduction_105(happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_2, happy_x_1) {
        (HappyAbsSyn39(happy_var_2), HappyAbsSyn132(happy_var_1)) => HappyAbsSyn38(snoc(reverseList(liftCAttrs(happy_var_1)), happy_var_2)),
        _ => notHappyAtAll()
    }
}


fn happyReduce_106() -> ActionReturn {
    partial_5!(happySpecReduce_2, 38, box happyReduction_106)
}

fn happyReduction_106(happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_2, happy_x_1) {
        (HappyAbsSyn39(happy_var_2), HappyAbsSyn65(happy_var_1)) => HappyAbsSyn38(snoc(rmap(CTypeQual, happy_var_1), happy_var_2)),
        _ => notHappyAtAll()
    }
}


fn happyReduce_107() -> ActionReturn {
    partial_5!(happySpecReduce_3, 38, box happyReduction_107)
}

fn happyReduction_107(happy_x_3: HappyAbsSyn, happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_3, happy_x_2, happy_x_1) {
        (HappyAbsSyn39(happy_var_3), HappyAbsSyn132(happy_var_2), HappyAbsSyn65(happy_var_1)) => HappyAbsSyn38(snoc(rappend(rmap(CTypeQual, happy_var_1), liftCAttrs(happy_var_2)), happy_var_3)),
        _ => notHappyAtAll()
    }
}


fn happyReduce_108() -> ActionReturn {
    partial_5!(happySpecReduce_2, 38, box happyReduction_108)
}

fn happyReduction_108(happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_2, happy_x_1) {
        (HappyAbsSyn39(happy_var_2), HappyAbsSyn38(happy_var_1)) => HappyAbsSyn38(snoc(happy_var_1, happy_var_2)),
        _ => notHappyAtAll()
    }
}


fn happyReduce_109() -> ActionReturn {
    partial_5!(happySpecReduce_2, 38, box happyReduction_109)
}

fn happyReduction_109(happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_2, happy_x_1) {
        (HappyAbsSyn132(happy_var_2), HappyAbsSyn38(happy_var_1)) => HappyAbsSyn38(addTrailingAttrs(happy_var_1, happy_var_2)),
        _ => notHappyAtAll()
    }
}


fn happyReduce_110() -> ActionReturn {
    partial_5!(happySpecReduce_1, 39, box happyReduction_110)
}

fn happyReduction_110(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn41(happy_var_1) => HappyAbsSyn39(CStorageSpec(happy_var_1)),
        _ => notHappyAtAll()
    }
}


fn happyReduce_111() -> ActionReturn {
    partial_5!(happySpecReduce_1, 39, box happyReduction_111)
}

fn happyReduction_111(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn64(happy_var_1) => HappyAbsSyn39(CTypeQual(happy_var_1)),
        _ => notHappyAtAll()
    }
}


fn happyReduce_112() -> ActionReturn {
    partial_5!(happySpecReduce_1, 39, box happyReduction_112)
}

fn happyReduction_112(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn42(happy_var_1) => HappyAbsSyn39(CFunSpec(happy_var_1)),
        _ => notHappyAtAll()
    }
}


fn happyReduce_113() -> ActionReturn {
    partial_5!(happySpecReduce_1, 39, box happyReduction_113)
}

fn happyReduction_113(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn43(happy_var_1) => HappyAbsSyn39(CAlignSpec(happy_var_1)),
        _ => notHappyAtAll()
    }
}


fn happyReduce_114() -> ActionReturn {
    partial_5!(happySpecReduce_1, 40, box happyReduction_114)
}

fn happyReduction_114(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn41(happy_var_1) => HappyAbsSyn39(CStorageSpec(happy_var_1)),
        _ => notHappyAtAll()
    }
}


fn happyReduce_115() -> ActionReturn {
    partial_5!(happySpecReduce_1, 40, box happyReduction_115)
}

fn happyReduction_115(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn42(happy_var_1) => HappyAbsSyn39(CFunSpec(happy_var_1)),
        _ => notHappyAtAll()
    }
}


fn happyReduce_116() -> ActionReturn {
    partial_5!(happySpecReduce_1, 40, box happyReduction_116)
}

fn happyReduction_116(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn43(happy_var_1) => HappyAbsSyn39(CAlignSpec(happy_var_1)),
        _ => notHappyAtAll()
    }
}


fn happyReduce_117() -> ActionReturn {
    partial_5!(happyMonadReduce, 1, 41, box happyReduction_117)
}

refute! { fn happyReduction_117<T>(HappyStk(HappyTerminal(happy_var_1), Some(box happyRest)): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1, partial_1!(CTypedef)) }, (box move |r| { happyReturn(HappyAbsSyn41(r)) }))
}
}


fn happyReduce_118() -> ActionReturn {
    partial_5!(happyMonadReduce, 1, 41, box happyReduction_118)
}

refute! { fn happyReduction_118<T>(HappyStk(HappyTerminal(happy_var_1), Some(box happyRest)): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1, partial_1!(CExtern)) }, (box move |r| { happyReturn(HappyAbsSyn41(r)) }))
}
}


fn happyReduce_119() -> ActionReturn {
    partial_5!(happyMonadReduce, 1, 41, box happyReduction_119)
}

refute! { fn happyReduction_119<T>(HappyStk(HappyTerminal(happy_var_1), Some(box happyRest)): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1, partial_1!(CStatic)) }, (box move |r| { happyReturn(HappyAbsSyn41(r)) }))
}
}


fn happyReduce_120() -> ActionReturn {
    partial_5!(happyMonadReduce, 1, 41, box happyReduction_120)
}

refute! { fn happyReduction_120<T>(HappyStk(HappyTerminal(happy_var_1), Some(box happyRest)): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1, partial_1!(CAuto)) }, (box move |r| { happyReturn(HappyAbsSyn41(r)) }))
}
}


fn happyReduce_121() -> ActionReturn {
    partial_5!(happyMonadReduce, 1, 41, box happyReduction_121)
}

refute! { fn happyReduction_121<T>(HappyStk(HappyTerminal(happy_var_1), Some(box happyRest)): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1, partial_1!(CRegister)) }, (box move |r| { happyReturn(HappyAbsSyn41(r)) }))
}
}


fn happyReduce_122() -> ActionReturn {
    partial_5!(happyMonadReduce, 1, 41, box happyReduction_122)
}

refute! { fn happyReduction_122<T>(HappyStk(HappyTerminal(happy_var_1), Some(box happyRest)): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1, partial_1!(CThread)) }, (box move |r| { happyReturn(HappyAbsSyn41(r)) }))
}
}


fn happyReduce_123() -> ActionReturn {
    partial_5!(happyMonadReduce, 1, 42, box happyReduction_123)
}

refute! { fn happyReduction_123<T>(HappyStk(HappyTerminal(happy_var_1), Some(box happyRest)): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1, partial_1!(CInlineQual)) }, (box move |r| { happyReturn(HappyAbsSyn42(r)) }))
}
}


fn happyReduce_124() -> ActionReturn {
    partial_5!(happyMonadReduce, 1, 42, box happyReduction_124)
}

refute! { fn happyReduction_124<T>(HappyStk(HappyTerminal(happy_var_1), Some(box happyRest)): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1, partial_1!(CNoreturnQual)) }, (box move |r| { happyReturn(HappyAbsSyn42(r)) }))
}
}


fn happyReduce_125() -> ActionReturn {
    partial_5!(happyMonadReduce, 4, 43, box happyReduction_125)
}

refute! { fn happyReduction_125<T>(HappyStk(_, Some(box HappyStk(HappyAbsSyn32(happy_var_3), Some(box HappyStk(_, Some(box HappyStk(HappyTerminal(happy_var_1), Some(box happyRest)))))))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1, partial_1!(CAlignAsType, happy_var_3)) }, (box move |r| { happyReturn(HappyAbsSyn43(r)) }))
}
}


fn happyReduce_126() -> ActionReturn {
    partial_5!(happyMonadReduce, 4, 43, box happyReduction_126)
}

refute! { fn happyReduction_126<T>(HappyStk(_, Some(box HappyStk(HappyAbsSyn100(happy_var_3), Some(box HappyStk(_, Some(box HappyStk(HappyTerminal(happy_var_1), Some(box happyRest)))))))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1, partial_1!(CAlignAsExpr, happy_var_3)) }, (box move |r| { happyReturn(HappyAbsSyn43(r)) }))
}
}


fn happyReduce_127() -> ActionReturn {
    partial_5!(happySpecReduce_1, 44, box happyReduction_127)
}

fn happyReduction_127(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn38(happy_var_1) => HappyAbsSyn37(reverse(happy_var_1)),
        _ => notHappyAtAll()
    }
}


fn happyReduce_128() -> ActionReturn {
    partial_5!(happySpecReduce_1, 44, box happyReduction_128)
}

fn happyReduction_128(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn38(happy_var_1) => HappyAbsSyn37(reverse(happy_var_1)),
        _ => notHappyAtAll()
    }
}


fn happyReduce_129() -> ActionReturn {
    partial_5!(happySpecReduce_1, 44, box happyReduction_129)
}

fn happyReduction_129(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn38(happy_var_1) => HappyAbsSyn37(reverse(happy_var_1)),
        _ => notHappyAtAll()
    }
}


fn happyReduce_130() -> ActionReturn {
    partial_5!(happyMonadReduce, 1, 45, box happyReduction_130)
}

refute! { fn happyReduction_130<T>(HappyStk(HappyTerminal(happy_var_1), Some(box happyRest)): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1, partial_1!(CVoidType)) }, (box move |r| { happyReturn(HappyAbsSyn45(r)) }))
}
}


fn happyReduce_131() -> ActionReturn {
    partial_5!(happyMonadReduce, 1, 45, box happyReduction_131)
}

refute! { fn happyReduction_131<T>(HappyStk(HappyTerminal(happy_var_1), Some(box happyRest)): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1, partial_1!(CCharType)) }, (box move |r| { happyReturn(HappyAbsSyn45(r)) }))
}
}


fn happyReduce_132() -> ActionReturn {
    partial_5!(happyMonadReduce, 1, 45, box happyReduction_132)
}

refute! { fn happyReduction_132<T>(HappyStk(HappyTerminal(happy_var_1), Some(box happyRest)): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1, partial_1!(CShortType)) }, (box move |r| { happyReturn(HappyAbsSyn45(r)) }))
}
}


fn happyReduce_133() -> ActionReturn {
    partial_5!(happyMonadReduce, 1, 45, box happyReduction_133)
}

refute! { fn happyReduction_133<T>(HappyStk(HappyTerminal(happy_var_1), Some(box happyRest)): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1, partial_1!(CIntType)) }, (box move |r| { happyReturn(HappyAbsSyn45(r)) }))
}
}


fn happyReduce_134() -> ActionReturn {
    partial_5!(happyMonadReduce, 1, 45, box happyReduction_134)
}

refute! { fn happyReduction_134<T>(HappyStk(HappyTerminal(happy_var_1), Some(box happyRest)): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1, partial_1!(CLongType)) }, (box move |r| { happyReturn(HappyAbsSyn45(r)) }))
}
}


fn happyReduce_135() -> ActionReturn {
    partial_5!(happyMonadReduce, 1, 45, box happyReduction_135)
}

refute! { fn happyReduction_135<T>(HappyStk(HappyTerminal(happy_var_1), Some(box happyRest)): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1, partial_1!(CFloatType)) }, (box move |r| { happyReturn(HappyAbsSyn45(r)) }))
}
}


fn happyReduce_136() -> ActionReturn {
    partial_5!(happyMonadReduce, 1, 45, box happyReduction_136)
}

refute! { fn happyReduction_136<T>(HappyStk(HappyTerminal(happy_var_1), Some(box happyRest)): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1, partial_1!(CDoubleType)) }, (box move |r| { happyReturn(HappyAbsSyn45(r)) }))
}
}


fn happyReduce_137() -> ActionReturn {
    partial_5!(happyMonadReduce, 1, 45, box happyReduction_137)
}

refute! { fn happyReduction_137<T>(HappyStk(HappyTerminal(happy_var_1), Some(box happyRest)): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1, partial_1!(CSignedType)) }, (box move |r| { happyReturn(HappyAbsSyn45(r)) }))
}
}


fn happyReduce_138() -> ActionReturn {
    partial_5!(happyMonadReduce, 1, 45, box happyReduction_138)
}

refute! { fn happyReduction_138<T>(HappyStk(HappyTerminal(happy_var_1), Some(box happyRest)): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1, partial_1!(CUnsigType)) }, (box move |r| { happyReturn(HappyAbsSyn45(r)) }))
}
}


fn happyReduce_139() -> ActionReturn {
    partial_5!(happyMonadReduce, 1, 45, box happyReduction_139)
}

refute! { fn happyReduction_139<T>(HappyStk(HappyTerminal(happy_var_1), Some(box happyRest)): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1, partial_1!(CBoolType)) }, (box move |r| { happyReturn(HappyAbsSyn45(r)) }))
}
}


fn happyReduce_140() -> ActionReturn {
    partial_5!(happyMonadReduce, 1, 45, box happyReduction_140)
}

refute! { fn happyReduction_140<T>(HappyStk(HappyTerminal(happy_var_1), Some(box happyRest)): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1, partial_1!(CComplexType)) }, (box move |r| { happyReturn(HappyAbsSyn45(r)) }))
}
}


fn happyReduce_141() -> ActionReturn {
    partial_5!(happyMonadReduce, 1, 45, box happyReduction_141)
}

refute! { fn happyReduction_141<T>(HappyStk(HappyTerminal(happy_var_1), Some(box happyRest)): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1, partial_1!(CInt128Type)) }, (box move |r| { happyReturn(HappyAbsSyn45(r)) }))
}
}


fn happyReduce_142() -> ActionReturn {
    partial_5!(happySpecReduce_2, 46, box happyReduction_142)
}

fn happyReduction_142(happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_2, happy_x_1) {
        (HappyAbsSyn45(happy_var_2), HappyAbsSyn38(happy_var_1)) => HappyAbsSyn38(snoc(happy_var_1, CTypeSpec(happy_var_2))),
        _ => notHappyAtAll()
    }
}


fn happyReduce_143() -> ActionReturn {
    partial_5!(happySpecReduce_2, 46, box happyReduction_143)
}

fn happyReduction_143(happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_2, happy_x_1) {
        (HappyAbsSyn41(happy_var_2), HappyAbsSyn38(happy_var_1)) => HappyAbsSyn38(snoc(happy_var_1, CStorageSpec(happy_var_2))),
        _ => notHappyAtAll()
    }
}


fn happyReduce_144() -> ActionReturn {
    partial_5!(happySpecReduce_2, 46, box happyReduction_144)
}

fn happyReduction_144(happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_2, happy_x_1) {
        (HappyAbsSyn39(happy_var_2), HappyAbsSyn38(happy_var_1)) => HappyAbsSyn38(snoc(happy_var_1, happy_var_2)),
        _ => notHappyAtAll()
    }
}


fn happyReduce_145() -> ActionReturn {
    partial_5!(happySpecReduce_2, 46, box happyReduction_145)
}

fn happyReduction_145(happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_2, happy_x_1) {
        (HappyAbsSyn45(happy_var_2), HappyAbsSyn38(happy_var_1)) => HappyAbsSyn38(snoc(happy_var_1, CTypeSpec(happy_var_2))),
        _ => notHappyAtAll()
    }
}


fn happyReduce_146() -> ActionReturn {
    partial_5!(happySpecReduce_2, 46, box happyReduction_146)
}

fn happyReduction_146(happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_2, happy_x_1) {
        (HappyAbsSyn132(happy_var_2), HappyAbsSyn38(happy_var_1)) => HappyAbsSyn38(addTrailingAttrs(happy_var_1, happy_var_2)),
        _ => notHappyAtAll()
    }
}


fn happyReduce_147() -> ActionReturn {
    partial_5!(happySpecReduce_1, 47, box happyReduction_147)
}

fn happyReduction_147(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn45(happy_var_1) => HappyAbsSyn38(singleton(CTypeSpec(happy_var_1))),
        _ => notHappyAtAll()
    }
}


fn happyReduce_148() -> ActionReturn {
    partial_5!(happySpecReduce_2, 47, box happyReduction_148)
}

fn happyReduction_148(happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_2, happy_x_1) {
        (HappyAbsSyn45(happy_var_2), HappyAbsSyn132(happy_var_1)) => HappyAbsSyn38(snoc(reverseList(liftCAttrs(happy_var_1)), CTypeSpec(happy_var_2))),
        _ => notHappyAtAll()
    }
}


fn happyReduce_149() -> ActionReturn {
    partial_5!(happySpecReduce_2, 47, box happyReduction_149)
}

fn happyReduction_149(happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_2, happy_x_1) {
        (HappyAbsSyn45(happy_var_2), HappyAbsSyn65(happy_var_1)) => HappyAbsSyn38(snoc(rmap(CTypeQual, happy_var_1), CTypeSpec(happy_var_2))),
        _ => notHappyAtAll()
    }
}


fn happyReduce_150() -> ActionReturn {
    partial_5!(happySpecReduce_3, 47, box happyReduction_150)
}

fn happyReduction_150(happy_x_3: HappyAbsSyn, happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_3, happy_x_2, happy_x_1) {
        (HappyAbsSyn45(happy_var_3), HappyAbsSyn132(happy_var_2), HappyAbsSyn65(happy_var_1)) => HappyAbsSyn38(snoc(rappend(rmap(CTypeQual, happy_var_1), liftCAttrs(happy_var_2)), CTypeSpec(happy_var_3))),
        _ => notHappyAtAll()
    }
}


fn happyReduce_151() -> ActionReturn {
    partial_5!(happySpecReduce_2, 47, box happyReduction_151)
}

fn happyReduction_151(happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_2, happy_x_1) {
        (HappyAbsSyn64(happy_var_2), HappyAbsSyn38(happy_var_1)) => HappyAbsSyn38(snoc(happy_var_1, CTypeQual(happy_var_2))),
        _ => notHappyAtAll()
    }
}


fn happyReduce_152() -> ActionReturn {
    partial_5!(happySpecReduce_2, 47, box happyReduction_152)
}

fn happyReduction_152(happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_2, happy_x_1) {
        (HappyAbsSyn45(happy_var_2), HappyAbsSyn38(happy_var_1)) => HappyAbsSyn38(snoc(happy_var_1, CTypeSpec(happy_var_2))),
        _ => notHappyAtAll()
    }
}


fn happyReduce_153() -> ActionReturn {
    partial_5!(happySpecReduce_2, 47, box happyReduction_153)
}

fn happyReduction_153(happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_2, happy_x_1) {
        (HappyAbsSyn132(happy_var_2), HappyAbsSyn38(happy_var_1)) => HappyAbsSyn38(addTrailingAttrs(happy_var_1, happy_var_2)),
        _ => notHappyAtAll()
    }
}


fn happyReduce_154() -> ActionReturn {
    partial_5!(happySpecReduce_2, 48, box happyReduction_154)
}

fn happyReduction_154(happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_2, happy_x_1) {
        (HappyAbsSyn45(happy_var_2), HappyAbsSyn38(happy_var_1)) => HappyAbsSyn38(snoc(happy_var_1, CTypeSpec(happy_var_2))),
        _ => notHappyAtAll()
    }
}


fn happyReduce_155() -> ActionReturn {
    partial_5!(happySpecReduce_2, 48, box happyReduction_155)
}

fn happyReduction_155(happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_2, happy_x_1) {
        (HappyAbsSyn41(happy_var_2), HappyAbsSyn38(happy_var_1)) => HappyAbsSyn38(snoc(happy_var_1, CStorageSpec(happy_var_2))),
        _ => notHappyAtAll()
    }
}


fn happyReduce_156() -> ActionReturn {
    partial_5!(happySpecReduce_2, 48, box happyReduction_156)
}

fn happyReduction_156(happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_2, happy_x_1) {
        (HappyAbsSyn39(happy_var_2), HappyAbsSyn38(happy_var_1)) => HappyAbsSyn38(snoc(happy_var_1, happy_var_2)),
        _ => notHappyAtAll()
    }
}


fn happyReduce_157() -> ActionReturn {
    partial_5!(happySpecReduce_2, 48, box happyReduction_157)
}

fn happyReduction_157(happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_2, happy_x_1) {
        (HappyAbsSyn132(happy_var_2), HappyAbsSyn38(happy_var_1)) => HappyAbsSyn38(addTrailingAttrs(happy_var_1, happy_var_2)),
        _ => notHappyAtAll()
    }
}


fn happyReduce_158() -> ActionReturn {
    partial_5!(happySpecReduce_1, 49, box happyReduction_158)
}

fn happyReduction_158(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn45(happy_var_1) => HappyAbsSyn38(singleton(CTypeSpec(happy_var_1))),
        _ => notHappyAtAll()
    }
}


fn happyReduce_159() -> ActionReturn {
    partial_5!(happySpecReduce_2, 49, box happyReduction_159)
}

fn happyReduction_159(happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_2, happy_x_1) {
        (HappyAbsSyn45(happy_var_2), HappyAbsSyn132(happy_var_1)) => HappyAbsSyn38(snoc(reverseList(liftCAttrs(happy_var_1)), CTypeSpec(happy_var_2))),
        _ => notHappyAtAll()
    }
}


fn happyReduce_160() -> ActionReturn {
    partial_5!(happySpecReduce_2, 49, box happyReduction_160)
}

fn happyReduction_160(happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_2, happy_x_1) {
        (HappyAbsSyn45(happy_var_2), HappyAbsSyn65(happy_var_1)) => HappyAbsSyn38(snoc(rmap(CTypeQual, happy_var_1), CTypeSpec(happy_var_2))),
        _ => notHappyAtAll()
    }
}


fn happyReduce_161() -> ActionReturn {
    partial_5!(happySpecReduce_3, 49, box happyReduction_161)
}

fn happyReduction_161(happy_x_3: HappyAbsSyn, happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_3, happy_x_2, happy_x_1) {
        (HappyAbsSyn45(happy_var_3), HappyAbsSyn132(happy_var_2), HappyAbsSyn65(happy_var_1)) => HappyAbsSyn38(snoc(rappend(rmap(CTypeQual, happy_var_1), liftCAttrs(happy_var_2)), CTypeSpec(happy_var_3))),
        _ => notHappyAtAll()
    }
}


fn happyReduce_162() -> ActionReturn {
    partial_5!(happySpecReduce_2, 49, box happyReduction_162)
}

fn happyReduction_162(happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_2, happy_x_1) {
        (HappyAbsSyn64(happy_var_2), HappyAbsSyn38(happy_var_1)) => HappyAbsSyn38(snoc(happy_var_1, CTypeQual(happy_var_2))),
        _ => notHappyAtAll()
    }
}


fn happyReduce_163() -> ActionReturn {
    partial_5!(happySpecReduce_2, 49, box happyReduction_163)
}

fn happyReduction_163(happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_2, happy_x_1) {
        (HappyAbsSyn132(happy_var_2), HappyAbsSyn38(happy_var_1)) => HappyAbsSyn38(addTrailingAttrs(happy_var_1, happy_var_2)),
        _ => notHappyAtAll()
    }
}


fn happyReduce_164() -> ActionReturn {
    partial_5!(happySpecReduce_2, 50, box happyReduction_164)
}

fn happyReduction_164(happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_2, happy_x_1) {
        (HappyAbsSyn41(happy_var_2), HappyAbsSyn38(happy_var_1)) => HappyAbsSyn38(snoc(happy_var_1, CStorageSpec(happy_var_2))),
        _ => notHappyAtAll()
    }
}


fn happyReduce_165() -> ActionReturn {
    partial_5!(happyMonadReduce, 2, 50, box happyReduction_165)
}

refute! { fn happyReduction_165<T>(HappyStk(HappyTerminal(CTokTyIdent(_, happy_var_2)), Some(box HappyStk(HappyAbsSyn38(happy_var_1), Some(box happyRest)))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_2.clone(), box |at| snoc(happy_var_1, CTypeSpec(CTypeDef(happy_var_2, at)))) }, (box move |r| { happyReturn(HappyAbsSyn38(r)) }))
}
}


fn happyReduce_166() -> ActionReturn {
    partial_5!(happyMonadReduce, 5, 50, box happyReduction_166)
}

refute! { fn happyReduction_166<T>(HappyStk(_, Some(box HappyStk(HappyAbsSyn100(happy_var_4), Some(box HappyStk(_, Some(box HappyStk(HappyTerminal(happy_var_2), Some(box HappyStk(HappyAbsSyn38(happy_var_1), Some(box happyRest)))))))))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_2, box |at| snoc(happy_var_1, CTypeSpec(CTypeOfExpr(happy_var_4, at)))) }, (box move |r| { happyReturn(HappyAbsSyn38(r)) }))
}
}


fn happyReduce_167() -> ActionReturn {
    partial_5!(happyMonadReduce, 5, 50, box happyReduction_167)
}

refute! { fn happyReduction_167<T>(HappyStk(_, Some(box HappyStk(HappyAbsSyn32(happy_var_4), Some(box HappyStk(_, Some(box HappyStk(HappyTerminal(happy_var_2), Some(box HappyStk(HappyAbsSyn38(happy_var_1), Some(box happyRest)))))))))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_2, box |at| snoc(happy_var_1, CTypeSpec(CTypeOfType(happy_var_4, at)))) }, (box move |r| { happyReturn(HappyAbsSyn38(r)) }))
}
}


fn happyReduce_168() -> ActionReturn {
    partial_5!(happySpecReduce_2, 50, box happyReduction_168)
}

fn happyReduction_168(happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_2, happy_x_1) {
        (HappyAbsSyn39(happy_var_2), HappyAbsSyn38(happy_var_1)) => HappyAbsSyn38(snoc(happy_var_1, happy_var_2)),
        _ => notHappyAtAll()
    }
}


fn happyReduce_169() -> ActionReturn {
    partial_5!(happySpecReduce_2, 50, box happyReduction_169)
}

fn happyReduction_169(happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_2, happy_x_1) {
        (HappyAbsSyn132(happy_var_2), HappyAbsSyn38(happy_var_1)) => HappyAbsSyn38(addTrailingAttrs(happy_var_1, happy_var_2)),
        _ => notHappyAtAll()
    }
}


fn happyReduce_170() -> ActionReturn {
    partial_5!(happyMonadReduce, 1, 51, box happyReduction_170)
}

refute! { fn happyReduction_170<T>(HappyStk(HappyTerminal(CTokTyIdent(_, happy_var_1)), Some(box happyRest)): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1.clone(), box |at| singleton(CTypeSpec(CTypeDef(happy_var_1, at)))) }, (box move |r| { happyReturn(HappyAbsSyn38(r)) }))
}
}


fn happyReduce_171() -> ActionReturn {
    partial_5!(happyMonadReduce, 4, 51, box happyReduction_171)
}

refute! { fn happyReduction_171<T>(HappyStk(_, Some(box HappyStk(HappyAbsSyn100(happy_var_3), Some(box HappyStk(_, Some(box HappyStk(HappyTerminal(happy_var_1), Some(box happyRest)))))))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1, box |at| singleton(CTypeSpec(CTypeOfExpr(happy_var_3, at)))) }, (box move |r| { happyReturn(HappyAbsSyn38(r)) }))
}
}


fn happyReduce_172() -> ActionReturn {
    partial_5!(happyMonadReduce, 4, 51, box happyReduction_172)
}

refute! { fn happyReduction_172<T>(HappyStk(_, Some(box HappyStk(HappyAbsSyn32(happy_var_3), Some(box HappyStk(_, Some(box HappyStk(HappyTerminal(happy_var_1), Some(box happyRest)))))))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1, box |at| singleton(CTypeSpec(CTypeOfType(happy_var_3, at)))) }, (box move |r| { happyReturn(HappyAbsSyn38(r)) }))
}
}


fn happyReduce_173() -> ActionReturn {
    partial_5!(happyMonadReduce, 2, 51, box happyReduction_173)
}

refute! { fn happyReduction_173<T>(HappyStk(HappyTerminal(CTokTyIdent(_, happy_var_2)), Some(box HappyStk(HappyAbsSyn65(happy_var_1), Some(box happyRest)))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_2.clone(), box |at| snoc(rmap(CTypeQual, happy_var_1), CTypeSpec(CTypeDef(happy_var_2, at)))) }, (box move |r| { happyReturn(HappyAbsSyn38(r)) }))
}
}


fn happyReduce_174() -> ActionReturn {
    partial_5!(happyMonadReduce, 5, 51, box happyReduction_174)
}

refute! { fn happyReduction_174<T>(HappyStk(_, Some(box HappyStk(HappyAbsSyn100(happy_var_4), Some(box HappyStk(_, Some(box HappyStk(HappyTerminal(happy_var_2), Some(box HappyStk(HappyAbsSyn65(happy_var_1), Some(box happyRest)))))))))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_2, box |at| snoc(rmap(CTypeQual, happy_var_1), CTypeSpec(CTypeOfExpr(happy_var_4, at)))) }, (box move |r| { happyReturn(HappyAbsSyn38(r)) }))
}
}


fn happyReduce_175() -> ActionReturn {
    partial_5!(happyMonadReduce, 5, 51, box happyReduction_175)
}

refute! { fn happyReduction_175<T>(HappyStk(_, Some(box HappyStk(HappyAbsSyn32(happy_var_4), Some(box HappyStk(_, Some(box HappyStk(HappyTerminal(happy_var_2), Some(box HappyStk(HappyAbsSyn65(happy_var_1), Some(box happyRest)))))))))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_2, box |at| snoc(rmap(CTypeQual, happy_var_1), CTypeSpec(CTypeOfType(happy_var_4, at)))) }, (box move |r| { happyReturn(HappyAbsSyn38(r)) }))
}
}


fn happyReduce_176() -> ActionReturn {
    partial_5!(happyMonadReduce, 2, 51, box happyReduction_176)
}

refute! { fn happyReduction_176<T>(HappyStk(HappyTerminal(CTokTyIdent(_, happy_var_2)), Some(box HappyStk(HappyAbsSyn132(happy_var_1), Some(box happyRest)))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_2.clone(), box |at| snoc(reverseList(liftCAttrs(happy_var_1)), CTypeSpec(CTypeDef(happy_var_2, at)))) }, (box move |r| { happyReturn(HappyAbsSyn38(r)) }))
}
}


fn happyReduce_177() -> ActionReturn {
    partial_5!(happyMonadReduce, 5, 51, box happyReduction_177)
}

refute! { fn happyReduction_177<T>(HappyStk(_, Some(box HappyStk(HappyAbsSyn100(happy_var_4), Some(box HappyStk(_, Some(box HappyStk(_, Some(box HappyStk(HappyAbsSyn132(happy_var_1), Some(box happyRest)))))))))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1.clone(), box |at| snoc(reverseList(liftCAttrs(happy_var_1)), CTypeSpec(CTypeOfExpr(happy_var_4, at)))) }, (box move |r| { happyReturn(HappyAbsSyn38(r)) }))
}
}


fn happyReduce_178() -> ActionReturn {
    partial_5!(happyMonadReduce, 5, 51, box happyReduction_178)
}

refute! { fn happyReduction_178<T>(HappyStk(_, Some(box HappyStk(HappyAbsSyn32(happy_var_4), Some(box HappyStk(_, Some(box HappyStk(HappyTerminal(happy_var_2), Some(box HappyStk(HappyAbsSyn132(happy_var_1), Some(box happyRest)))))))))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_2, box |at| snoc(reverseList(liftCAttrs(happy_var_1)), CTypeSpec(CTypeOfType(happy_var_4, at)))) }, (box move |r| { happyReturn(HappyAbsSyn38(r)) }))
}
}


fn happyReduce_179() -> ActionReturn {
    partial_5!(happyMonadReduce, 3, 51, box happyReduction_179)
}

refute! { fn happyReduction_179<T>(HappyStk(HappyTerminal(CTokTyIdent(_, happy_var_3)), Some(box HappyStk(HappyAbsSyn132(happy_var_2), Some(box HappyStk(HappyAbsSyn65(happy_var_1), Some(box happyRest)))))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_3.clone(), box |at| snoc(rappend(rmap(CTypeQual, happy_var_1), liftCAttrs(happy_var_2)),
                                                  CTypeSpec(CTypeDef(happy_var_3, at)))) }, (box move |r| { happyReturn(HappyAbsSyn38(r)) }))
}
}


fn happyReduce_180() -> ActionReturn {
    partial_5!(happyMonadReduce, 6, 51, box happyReduction_180)
}

refute! { fn happyReduction_180<T>(HappyStk(_, Some(box HappyStk(HappyAbsSyn100(happy_var_5), Some(box HappyStk(_, Some(box HappyStk(HappyTerminal(happy_var_3), Some(box HappyStk(HappyAbsSyn132(happy_var_2), Some(box HappyStk(HappyAbsSyn65(happy_var_1), Some(box happyRest)))))))))))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_3, box |at| snoc(rappend(rmap(CTypeQual, happy_var_1), liftCAttrs(happy_var_2)),
                                          CTypeSpec(CTypeOfExpr(happy_var_5, at)))) }, (box move |r| { happyReturn(HappyAbsSyn38(r)) }))
}
}


fn happyReduce_181() -> ActionReturn {
    partial_5!(happyMonadReduce, 6, 51, box happyReduction_181)
}

refute! { fn happyReduction_181<T>(HappyStk(_, Some(box HappyStk(HappyAbsSyn32(happy_var_5), Some(box HappyStk(_, Some(box HappyStk(HappyTerminal(happy_var_3), Some(box HappyStk(HappyAbsSyn132(happy_var_2), Some(box HappyStk(HappyAbsSyn65(happy_var_1), Some(box happyRest)))))))))))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_3, box |at| snoc(rappend(rmap(CTypeQual, happy_var_1), liftCAttrs(happy_var_2)),
                                          CTypeSpec(CTypeOfType(happy_var_5, at)))) }, (box move |r| { happyReturn(HappyAbsSyn38(r)) }))
}
}


fn happyReduce_182() -> ActionReturn {
    partial_5!(happySpecReduce_2, 51, box happyReduction_182)
}

fn happyReduction_182(happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_2, happy_x_1) {
        (HappyAbsSyn64(happy_var_2), HappyAbsSyn38(happy_var_1)) => HappyAbsSyn38(snoc(happy_var_1, CTypeQual(happy_var_2))),
        _ => notHappyAtAll()
    }
}


fn happyReduce_183() -> ActionReturn {
    partial_5!(happySpecReduce_2, 51, box happyReduction_183)
}

fn happyReduction_183(happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_2, happy_x_1) {
        (HappyAbsSyn132(happy_var_2), HappyAbsSyn38(happy_var_1)) => HappyAbsSyn38(addTrailingAttrs(happy_var_1, happy_var_2)),
        _ => notHappyAtAll()
    }
}


fn happyReduce_184() -> ActionReturn {
    partial_5!(happyMonadReduce, 1, 52, box happyReduction_184)
}

refute! { fn happyReduction_184<T>(HappyStk(HappyAbsSyn53(happy_var_1), Some(box happyRest)): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1.clone(), partial_1!(CSUType, happy_var_1)) }, (box move |r| { happyReturn(HappyAbsSyn45(r)) }))
}
}


fn happyReduce_185() -> ActionReturn {
    partial_5!(happyMonadReduce, 1, 52, box happyReduction_185)
}

refute! { fn happyReduction_185<T>(HappyStk(HappyAbsSyn61(happy_var_1), Some(box happyRest)): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1.clone(), partial_1!(CEnumType, happy_var_1)) }, (box move |r| { happyReturn(HappyAbsSyn45(r)) }))
}
}


fn happyReduce_186() -> ActionReturn {
    partial_5!(happyMonadReduce, 6, 53, box happyReduction_186)
}

refute! { fn happyReduction_186<T>(HappyStk(_, Some(box HappyStk(HappyAbsSyn33(happy_var_5), Some(box HappyStk(_, Some(box HappyStk(HappyAbsSyn131(happy_var_3), Some(box HappyStk(HappyAbsSyn132(happy_var_2), Some(box HappyStk(HappyAbsSyn54(happy_var_1), Some(box happyRest)))))))))))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1.clone(), partial_1!(CStructureUnion, unL(happy_var_1), Some(happy_var_3), Some(reverse(happy_var_5)), happy_var_2)) }, (box move |r| { happyReturn(HappyAbsSyn53(r)) }))
}
}


fn happyReduce_187() -> ActionReturn {
    partial_5!(happyMonadReduce, 5, 53, box happyReduction_187)
}

refute! { fn happyReduction_187<T>(HappyStk(_, Some(box HappyStk(HappyAbsSyn33(happy_var_4), Some(box HappyStk(_, Some(box HappyStk(HappyAbsSyn132(happy_var_2), Some(box HappyStk(HappyAbsSyn54(happy_var_1), Some(box happyRest)))))))))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1.clone(), partial_1!(CStructureUnion, unL(happy_var_1), None,     Some(reverse(happy_var_4)), happy_var_2)) }, (box move |r| { happyReturn(HappyAbsSyn53(r)) }))
}
}


fn happyReduce_188() -> ActionReturn {
    partial_5!(happyMonadReduce, 3, 53, box happyReduction_188)
}

refute! { fn happyReduction_188<T>(HappyStk(HappyAbsSyn131(happy_var_3), Some(box HappyStk(HappyAbsSyn132(happy_var_2), Some(box HappyStk(HappyAbsSyn54(happy_var_1), Some(box happyRest)))))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1.clone(), partial_1!(CStructureUnion, unL(happy_var_1), Some(happy_var_3), None,              happy_var_2)) }, (box move |r| { happyReturn(HappyAbsSyn53(r)) }))
}
}


fn happyReduce_189() -> ActionReturn {
    partial_5!(happySpecReduce_1, 54, box happyReduction_189)
}

fn happyReduction_189(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyTerminal(happy_var_1) => HappyAbsSyn54(Located(CStructTag, posOf(happy_var_1))),
        _ => notHappyAtAll()
    }
}


fn happyReduce_190() -> ActionReturn {
    partial_5!(happySpecReduce_1, 54, box happyReduction_190)
}

fn happyReduction_190(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyTerminal(happy_var_1) => HappyAbsSyn54(Located(CUnionTag, posOf(happy_var_1))),
        _ => notHappyAtAll()
    }
}


fn happyReduce_191() -> ActionReturn {
    partial_5!(happySpecReduce_0, 55, happyReduction_191())
}

fn happyReduction_191() -> HappyAbsSyn {
    match () {
        () => HappyAbsSyn33(empty()),
    }
}


fn happyReduce_192() -> ActionReturn {
    partial_5!(happySpecReduce_2, 55, box happyReduction_192)
}

fn happyReduction_192(happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_2, happy_x_1) {
        (_, HappyAbsSyn33(happy_var_1)) => HappyAbsSyn33(happy_var_1),
        _ => notHappyAtAll()
    }
}


fn happyReduce_193() -> ActionReturn {
    partial_5!(happySpecReduce_2, 55, box happyReduction_193)
}

fn happyReduction_193(happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_2, happy_x_1) {
        (HappyAbsSyn32(happy_var_2), HappyAbsSyn33(happy_var_1)) => HappyAbsSyn33(snoc(happy_var_1, happy_var_2)),
        _ => notHappyAtAll()
    }
}


fn happyReduce_194() -> ActionReturn {
    partial_5!(happySpecReduce_2, 56, box happyReduction_194)
}

fn happyReduction_194(happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_2, happy_x_1) {
        (_, HappyAbsSyn32(happy_var_1)) => HappyAbsSyn32(if let CDecl(declspecs, dies, at) = happy_var_1 {
                CDecl(declspecs, List::reverse(dies), at)
            } else {
                panic!("irrefutable pattern");
            }),
        _ => notHappyAtAll()
    }
}


fn happyReduce_195() -> ActionReturn {
    partial_5!(happySpecReduce_2, 56, box happyReduction_195)
}

fn happyReduction_195(happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_2, happy_x_1) {
        (_, HappyAbsSyn32(happy_var_1)) => HappyAbsSyn32(if let CDecl(declspecs, dies, at) = happy_var_1 {
                CDecl(declspecs, List::reverse(dies), at)
            } else {
                panic!("irrefutable pattern");
            }),
        _ => notHappyAtAll()
    }
}


fn happyReduce_196() -> ActionReturn {
    partial_5!(happySpecReduce_2, 56, box happyReduction_196)
}

fn happyReduction_196(happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_2, happy_x_1) {
        (HappyAbsSyn32(happy_var_2), _) => HappyAbsSyn32(happy_var_2),
        _ => notHappyAtAll()
    }
}


fn happyReduce_197() -> ActionReturn {
    partial_5!(happyMonadReduce, 3, 57, box happyReduction_197)
}

refute! { fn happyReduction_197<T>(HappyStk(HappyAbsSyn59(happy_var_3), Some(box HappyStk(HappyAbsSyn132(happy_var_2), Some(box HappyStk(HappyAbsSyn65(happy_var_1), Some(box happyRest)))))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({
            withNodeInfo(happy_var_1.clone(), match happy_var_3 {
                (d, s) => partial_1!(CDecl, __op_addadd(liftTypeQuals(happy_var_1), liftCAttrs(happy_var_2)),
                                     vec![(d, None, s)])
            }) }, (box move |r| { happyReturn(HappyAbsSyn32(r)) }))
}
}


fn happyReduce_198() -> ActionReturn {
    partial_5!(happyMonadReduce, 2, 57, box happyReduction_198)
}

refute! { fn happyReduction_198<T>(HappyStk(HappyAbsSyn59(happy_var_2), Some(box HappyStk(HappyAbsSyn132(happy_var_1), Some(box happyRest)))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({
            withNodeInfo(happy_var_1.clone(), match happy_var_2 {
                (d, s) => partial_1!(CDecl, liftCAttrs(happy_var_1), vec![(d, None, s)]),
            }) }, (box move |r| { happyReturn(HappyAbsSyn32(r)) }))
}
}


fn happyReduce_199() -> ActionReturn {
    partial_5!(happyReduce, 4, 57, box happyReduction_199)
}

refute! { fn happyReduction_199(HappyStk(HappyAbsSyn59(happy_var_4), Some(box HappyStk(HappyAbsSyn132(happy_var_3), Some(box HappyStk(_, Some(box HappyStk(HappyAbsSyn32(happy_var_1), Some(box happyRest)))))))): HappyStk<HappyAbsSyn>) -> HappyStk<HappyAbsSyn> {
    HappyStk(HappyAbsSyn32(if let CDecl(declspecs, dies, at) = happy_var_1 {
                match happy_var_4 {
                    (Some(d), s) => {
                        CDecl(declspecs, __op_concat((Some(appendObjAttrs(happy_var_3, d)), None, s), dies), at)
                    },
                    (None, s) => {
                        CDecl(declspecs, __op_concat((None, None, s), dies), at)
                    },
                }
            } else {
                panic!("irrefutable pattern")
            }), Some(box happyRest))
}
}


fn happyReduce_200() -> ActionReturn {
    partial_5!(happyMonadReduce, 3, 58, box happyReduction_200)
}

refute! { fn happyReduction_200<T>(HappyStk(HappyAbsSyn132(happy_var_3), Some(box HappyStk(HappyAbsSyn59(happy_var_2), Some(box HappyStk(HappyAbsSyn37(happy_var_1), Some(box happyRest)))))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({
            withNodeInfo(happy_var_1.clone(), match happy_var_2 {
                (Some(d), s) => {
                    partial_1!(CDecl, happy_var_1, vec![(Some(appendObjAttrs(happy_var_3, d)), None, s)])
                },
                (None, s) => {
                    partial_1!(CDecl, happy_var_1, vec![(None, None, s)])
                },
            }) }, (box move |r| { happyReturn(HappyAbsSyn32(r)) }))
}
}


fn happyReduce_201() -> ActionReturn {
    partial_5!(happyReduce, 5, 58, box happyReduction_201)
}

refute! { fn happyReduction_201(HappyStk(HappyAbsSyn132(happy_var_5), Some(box HappyStk(HappyAbsSyn59(happy_var_4), Some(box HappyStk(HappyAbsSyn132(happy_var_3), Some(box HappyStk(_, Some(box HappyStk(HappyAbsSyn32(happy_var_1), Some(box happyRest)))))))))): HappyStk<HappyAbsSyn>) -> HappyStk<HappyAbsSyn> {
    HappyStk(HappyAbsSyn32(if let CDecl(declspecs, dies, at) = happy_var_1 {
                match happy_var_4 {
                    (Some(d), s) => {
                        CDecl(declspecs, __op_concat((Some(
                            appendObjAttrs(__op_addadd(happy_var_3, happy_var_5), d)), None, s), dies), at)
                    },
                    (None, s) => {
                        CDecl(declspecs, __op_concat((None, None, s), dies), at)
                    },
                }
            } else {
                panic!("irrefutable pattern");
            }), Some(box happyRest))
}
}


fn happyReduce_202() -> ActionReturn {
    partial_5!(happyMonadReduce, 1, 58, box happyReduction_202)
}

refute! { fn happyReduction_202<T>(HappyStk(HappyAbsSyn37(happy_var_1), Some(box happyRest)): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1.clone(), partial_1!(CDecl, happy_var_1, vec![])) }, (box move |r| { happyReturn(HappyAbsSyn32(r)) }))
}
}


fn happyReduce_203() -> ActionReturn {
    partial_5!(happySpecReduce_1, 59, box happyReduction_203)
}

fn happyReduction_203(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn66(happy_var_1) => HappyAbsSyn59((Some(reverseDeclr(happy_var_1)), None)),
        _ => notHappyAtAll()
    }
}


fn happyReduce_204() -> ActionReturn {
    partial_5!(happySpecReduce_2, 59, box happyReduction_204)
}

fn happyReduction_204(happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_2, happy_x_1) {
        (HappyAbsSyn100(happy_var_2), _) => HappyAbsSyn59((None, Some(happy_var_2))),
        _ => notHappyAtAll()
    }
}


fn happyReduce_205() -> ActionReturn {
    partial_5!(happySpecReduce_3, 59, box happyReduction_205)
}

fn happyReduction_205(happy_x_3: HappyAbsSyn, happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_3, happy_x_2, happy_x_1) {
        (HappyAbsSyn100(happy_var_3), _, HappyAbsSyn66(happy_var_1)) => HappyAbsSyn59((Some(reverseDeclr(happy_var_1)), Some(happy_var_3))),
        _ => notHappyAtAll()
    }
}


fn happyReduce_206() -> ActionReturn {
    partial_5!(happySpecReduce_1, 60, box happyReduction_206)
}

fn happyReduction_206(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn66(happy_var_1) => HappyAbsSyn59((Some(reverseDeclr(happy_var_1)), None)),
        _ => notHappyAtAll()
    }
}


fn happyReduce_207() -> ActionReturn {
    partial_5!(happySpecReduce_2, 60, box happyReduction_207)
}

fn happyReduction_207(happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_2, happy_x_1) {
        (HappyAbsSyn100(happy_var_2), _) => HappyAbsSyn59((None, Some(happy_var_2))),
        _ => notHappyAtAll()
    }
}


fn happyReduce_208() -> ActionReturn {
    partial_5!(happySpecReduce_3, 60, box happyReduction_208)
}

fn happyReduction_208(happy_x_3: HappyAbsSyn, happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_3, happy_x_2, happy_x_1) {
        (HappyAbsSyn100(happy_var_3), _, HappyAbsSyn66(happy_var_1)) => HappyAbsSyn59((Some(reverseDeclr(happy_var_1)), Some(happy_var_3))),
        _ => notHappyAtAll()
    }
}


fn happyReduce_209() -> ActionReturn {
    partial_5!(happySpecReduce_2, 60, box happyReduction_209)
}

fn happyReduction_209(happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_2, happy_x_1) {
        (HappyAbsSyn132(happy_var_2), HappyAbsSyn59(happy_var_1)) => HappyAbsSyn59(match happy_var_1 {
                (None, expr) => (None, expr),
                (Some(CDeclarator(name, derived, asmname, attrs, node)), bsz) =>
                    (Some(CDeclarator(name, derived, asmname, __op_addadd(attrs, happy_var_2), node)), bsz)
            }),
        _ => notHappyAtAll()
    }
}


fn happyReduce_210() -> ActionReturn {
    partial_5!(happyMonadReduce, 5, 61, box happyReduction_210)
}

refute! { fn happyReduction_210<T>(HappyStk(_, Some(box HappyStk(HappyAbsSyn62(happy_var_4), Some(box HappyStk(_, Some(box HappyStk(HappyAbsSyn132(happy_var_2), Some(box HappyStk(HappyTerminal(happy_var_1), Some(box happyRest)))))))))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1, partial_1!(CEnumeration, None, Some(reverse(happy_var_4)), happy_var_2)) }, (box move |r| { happyReturn(HappyAbsSyn61(r)) }))
}
}


fn happyReduce_211() -> ActionReturn {
    partial_5!(happyMonadReduce, 6, 61, box happyReduction_211)
}

refute! { fn happyReduction_211<T>(HappyStk(_, Some(box HappyStk(_, Some(box HappyStk(HappyAbsSyn62(happy_var_4), Some(box HappyStk(_, Some(box HappyStk(HappyAbsSyn132(happy_var_2), Some(box HappyStk(HappyTerminal(happy_var_1), Some(box happyRest)))))))))))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1, partial_1!(CEnumeration, None, Some(reverse(happy_var_4)), happy_var_2)) }, (box move |r| { happyReturn(HappyAbsSyn61(r)) }))
}
}


fn happyReduce_212() -> ActionReturn {
    partial_5!(happyMonadReduce, 6, 61, box happyReduction_212)
}

refute! { fn happyReduction_212<T>(HappyStk(_, Some(box HappyStk(HappyAbsSyn62(happy_var_5), Some(box HappyStk(_, Some(box HappyStk(HappyAbsSyn131(happy_var_3), Some(box HappyStk(HappyAbsSyn132(happy_var_2), Some(box HappyStk(HappyTerminal(happy_var_1), Some(box happyRest)))))))))))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1, partial_1!(CEnumeration, Some(happy_var_3), Some(reverse(happy_var_5)), happy_var_2)) }, (box move |r| { happyReturn(HappyAbsSyn61(r)) }))
}
}


fn happyReduce_213() -> ActionReturn {
    partial_5!(happyMonadReduce, 7, 61, box happyReduction_213)
}

refute! { fn happyReduction_213<T>(HappyStk(_, Some(box HappyStk(_, Some(box HappyStk(HappyAbsSyn62(happy_var_5), Some(box HappyStk(_, Some(box HappyStk(HappyAbsSyn131(happy_var_3), Some(box HappyStk(HappyAbsSyn132(happy_var_2), Some(box HappyStk(HappyTerminal(happy_var_1), Some(box happyRest)))))))))))))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1, partial_1!(CEnumeration, Some(happy_var_3), Some(reverse(happy_var_5)), happy_var_2)) }, (box move |r| { happyReturn(HappyAbsSyn61(r)) }))
}
}


fn happyReduce_214() -> ActionReturn {
    partial_5!(happyMonadReduce, 3, 61, box happyReduction_214)
}

refute! { fn happyReduction_214<T>(HappyStk(HappyAbsSyn131(happy_var_3), Some(box HappyStk(HappyAbsSyn132(happy_var_2), Some(box HappyStk(HappyTerminal(happy_var_1), Some(box happyRest)))))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1, partial_1!(CEnumeration, Some(happy_var_3), None, happy_var_2)) }, (box move |r| { happyReturn(HappyAbsSyn61(r)) }))
}
}


fn happyReduce_215() -> ActionReturn {
    partial_5!(happySpecReduce_1, 62, box happyReduction_215)
}

fn happyReduction_215(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn63(happy_var_1) => HappyAbsSyn62(singleton(happy_var_1)),
        _ => notHappyAtAll()
    }
}


fn happyReduce_216() -> ActionReturn {
    partial_5!(happySpecReduce_3, 62, box happyReduction_216)
}

fn happyReduction_216(happy_x_3: HappyAbsSyn, happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_3, happy_x_2, happy_x_1) {
        (HappyAbsSyn63(happy_var_3), _, HappyAbsSyn62(happy_var_1)) => HappyAbsSyn62(snoc(happy_var_1, happy_var_3)),
        _ => notHappyAtAll()
    }
}


fn happyReduce_217() -> ActionReturn {
    partial_5!(happySpecReduce_1, 63, box happyReduction_217)
}

fn happyReduction_217(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn131(happy_var_1) => HappyAbsSyn63((happy_var_1, None)),
        _ => notHappyAtAll()
    }
}


fn happyReduce_218() -> ActionReturn {
    partial_5!(happySpecReduce_2, 63, box happyReduction_218)
}

fn happyReduction_218(happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_2, happy_x_1) {
        (_, HappyAbsSyn131(happy_var_1)) => HappyAbsSyn63((happy_var_1, None)),
        _ => notHappyAtAll()
    }
}


fn happyReduce_219() -> ActionReturn {
    partial_5!(happyReduce, 4, 63, box happyReduction_219)
}

refute! { fn happyReduction_219(HappyStk(HappyAbsSyn100(happy_var_4), Some(box HappyStk(_, Some(box HappyStk(_, Some(box HappyStk(HappyAbsSyn131(happy_var_1), Some(box happyRest)))))))): HappyStk<HappyAbsSyn>) -> HappyStk<HappyAbsSyn> {
    HappyStk(HappyAbsSyn63((happy_var_1, Some(happy_var_4))), Some(box happyRest))
}
}


fn happyReduce_220() -> ActionReturn {
    partial_5!(happySpecReduce_3, 63, box happyReduction_220)
}

fn happyReduction_220(happy_x_3: HappyAbsSyn, happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_3, happy_x_2, happy_x_1) {
        (HappyAbsSyn100(happy_var_3), _, HappyAbsSyn131(happy_var_1)) => HappyAbsSyn63((happy_var_1, Some(happy_var_3))),
        _ => notHappyAtAll()
    }
}


fn happyReduce_221() -> ActionReturn {
    partial_5!(happyMonadReduce, 1, 64, box happyReduction_221)
}

refute! { fn happyReduction_221<T>(HappyStk(HappyTerminal(happy_var_1), Some(box happyRest)): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1, partial_1!(CConstQual)) }, (box move |r| { happyReturn(HappyAbsSyn64(r)) }))
}
}


fn happyReduce_222() -> ActionReturn {
    partial_5!(happyMonadReduce, 1, 64, box happyReduction_222)
}

refute! { fn happyReduction_222<T>(HappyStk(HappyTerminal(happy_var_1), Some(box happyRest)): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1, partial_1!(CVolatQual)) }, (box move |r| { happyReturn(HappyAbsSyn64(r)) }))
}
}


fn happyReduce_223() -> ActionReturn {
    partial_5!(happyMonadReduce, 1, 64, box happyReduction_223)
}

refute! { fn happyReduction_223<T>(HappyStk(HappyTerminal(happy_var_1), Some(box happyRest)): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1, partial_1!(CRestrQual)) }, (box move |r| { happyReturn(HappyAbsSyn64(r)) }))
}
}


fn happyReduce_224() -> ActionReturn {
    partial_5!(happyMonadReduce, 1, 64, box happyReduction_224)
}

refute! { fn happyReduction_224<T>(HappyStk(HappyTerminal(happy_var_1), Some(box happyRest)): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1, partial_1!(CNullableQual)) }, (box move |r| { happyReturn(HappyAbsSyn64(r)) }))
}
}


fn happyReduce_225() -> ActionReturn {
    partial_5!(happyMonadReduce, 1, 64, box happyReduction_225)
}

refute! { fn happyReduction_225<T>(HappyStk(HappyTerminal(happy_var_1), Some(box happyRest)): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1, partial_1!(CNonnullQual)) }, (box move |r| { happyReturn(HappyAbsSyn64(r)) }))
}
}


fn happyReduce_226() -> ActionReturn {
    partial_5!(happyMonadReduce, 1, 64, box happyReduction_226)
}

refute! { fn happyReduction_226<T>(HappyStk(HappyTerminal(happy_var_1), Some(box happyRest)): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1, partial_1!(CAtomicQual)) }, (box move |r| { happyReturn(HappyAbsSyn64(r)) }))
}
}


fn happyReduce_227() -> ActionReturn {
    partial_5!(happySpecReduce_2, 65, box happyReduction_227)
}

fn happyReduction_227(happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_2, happy_x_1) {
        (HappyAbsSyn64(happy_var_2), HappyAbsSyn132(happy_var_1)) => HappyAbsSyn65(snoc(reverseList(__map!(CAttrQual, happy_var_1)), happy_var_2)),
        _ => notHappyAtAll()
    }
}


fn happyReduce_228() -> ActionReturn {
    partial_5!(happySpecReduce_2, 65, box happyReduction_228)
}

fn happyReduction_228(happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_2, happy_x_1) {
        (HappyAbsSyn64(happy_var_2), HappyAbsSyn65(happy_var_1)) => HappyAbsSyn65(snoc(happy_var_1, happy_var_2)),
        _ => notHappyAtAll()
    }
}


fn happyReduce_229() -> ActionReturn {
    partial_5!(happySpecReduce_3, 65, box happyReduction_229)
}

fn happyReduction_229(happy_x_3: HappyAbsSyn, happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_3, happy_x_2, happy_x_1) {
        (HappyAbsSyn64(happy_var_3), HappyAbsSyn132(happy_var_2), HappyAbsSyn65(happy_var_1)) => HappyAbsSyn65(snoc(rappend(happy_var_1, __map!(CAttrQual, happy_var_2)), happy_var_3)),
        _ => notHappyAtAll()
    }
}


fn happyReduce_230() -> ActionReturn {
    partial_5!(happySpecReduce_1, 66, box happyReduction_230)
}

fn happyReduction_230(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn66(happy_var_1) => HappyAbsSyn66(happy_var_1),
        _ => notHappyAtAll()
    }
}


fn happyReduce_231() -> ActionReturn {
    partial_5!(happySpecReduce_1, 66, box happyReduction_231)
}

fn happyReduction_231(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn66(happy_var_1) => HappyAbsSyn66(happy_var_1),
        _ => notHappyAtAll()
    }
}


fn happyReduce_232() -> ActionReturn {
    partial_5!(happySpecReduce_0, 67, happyReduction_232())
}

fn happyReduction_232() -> HappyAbsSyn {
    match () {
        () => HappyAbsSyn67(None),
    }
}


fn happyReduce_233() -> ActionReturn {
    partial_5!(happyReduce, 4, 67, box happyReduction_233)
}

refute! { fn happyReduction_233(HappyStk(_, Some(box HappyStk(HappyAbsSyn128(happy_var_3), Some(box HappyStk(_, Some(box HappyStk(_, Some(box happyRest)))))))): HappyStk<HappyAbsSyn>) -> HappyStk<HappyAbsSyn> {
    HappyStk(HappyAbsSyn67(Some(happy_var_3)), Some(box happyRest))
}
}


fn happyReduce_234() -> ActionReturn {
    partial_5!(happySpecReduce_1, 68, box happyReduction_234)
}

fn happyReduction_234(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn66(happy_var_1) => HappyAbsSyn66(happy_var_1),
        _ => notHappyAtAll()
    }
}


fn happyReduce_235() -> ActionReturn {
    partial_5!(happySpecReduce_1, 68, box happyReduction_235)
}

fn happyReduction_235(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn66(happy_var_1) => HappyAbsSyn66(happy_var_1),
        _ => notHappyAtAll()
    }
}


fn happyReduce_236() -> ActionReturn {
    partial_5!(happyMonadReduce, 1, 69, box happyReduction_236)
}

refute! { fn happyReduction_236<T>(HappyStk(HappyTerminal(CTokTyIdent(_, happy_var_1)), Some(box happyRest)): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1.clone(), partial_1!(mkVarDeclr, happy_var_1)) }, (box move |r| { happyReturn(HappyAbsSyn66(r)) }))
}
}


fn happyReduce_237() -> ActionReturn {
    partial_5!(happyMonadReduce, 2, 69, box happyReduction_237)
}

refute! { fn happyReduction_237<T>(HappyStk(HappyAbsSyn88(happy_var_2), Some(box HappyStk(HappyTerminal(CTokTyIdent(_, happy_var_1)), Some(box happyRest)))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1.clone(), box |at| { happy_var_2(mkVarDeclr(happy_var_1, at)) }) }, (box move |r| { happyReturn(HappyAbsSyn66(r)) }))
}
}


fn happyReduce_238() -> ActionReturn {
    partial_5!(happySpecReduce_1, 69, box happyReduction_238)
}

fn happyReduction_238(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn66(happy_var_1) => HappyAbsSyn66(happy_var_1),
        _ => notHappyAtAll()
    }
}


fn happyReduce_239() -> ActionReturn {
    partial_5!(happySpecReduce_1, 70, box happyReduction_239)
}

fn happyReduction_239(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn66(happy_var_1) => HappyAbsSyn66(happy_var_1),
        _ => notHappyAtAll()
    }
}


fn happyReduce_240() -> ActionReturn {
    partial_5!(happyMonadReduce, 2, 70, box happyReduction_240)
}

refute! { fn happyReduction_240<T>(HappyStk(HappyAbsSyn66(happy_var_2), Some(box HappyStk(HappyTerminal(happy_var_1), Some(box happyRest)))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1, partial_1!(ptrDeclr, happy_var_2, vec![])) }, (box move |r| { happyReturn(HappyAbsSyn66(r)) }))
}
}


fn happyReduce_241() -> ActionReturn {
    partial_5!(happyMonadReduce, 3, 70, box happyReduction_241)
}

refute! { fn happyReduction_241<T>(HappyStk(HappyAbsSyn66(happy_var_3), Some(box HappyStk(HappyAbsSyn132(happy_var_2), Some(box HappyStk(HappyTerminal(happy_var_1), Some(box happyRest)))))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withAttribute(happy_var_1, happy_var_2, partial_1!(ptrDeclr, happy_var_3, vec![])) }, (box move |r| { happyReturn(HappyAbsSyn66(r)) }))
}
}


fn happyReduce_242() -> ActionReturn {
    partial_5!(happyMonadReduce, 3, 70, box happyReduction_242)
}

refute! { fn happyReduction_242<T>(HappyStk(HappyAbsSyn66(happy_var_3), Some(box HappyStk(HappyAbsSyn65(happy_var_2), Some(box HappyStk(HappyTerminal(happy_var_1), Some(box happyRest)))))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1, partial_1!(ptrDeclr, happy_var_3, reverse(happy_var_2))) }, (box move |r| { happyReturn(HappyAbsSyn66(r)) }))
}
}


fn happyReduce_243() -> ActionReturn {
    partial_5!(happyMonadReduce, 4, 70, box happyReduction_243)
}

refute! { fn happyReduction_243<T>(HappyStk(HappyAbsSyn66(happy_var_4), Some(box HappyStk(HappyAbsSyn132(happy_var_3), Some(box HappyStk(HappyAbsSyn65(happy_var_2), Some(box HappyStk(HappyTerminal(happy_var_1), Some(box happyRest)))))))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withAttribute(happy_var_1, happy_var_3, partial_1!(ptrDeclr, happy_var_4, reverse(happy_var_2))) }, (box move |r| { happyReturn(HappyAbsSyn66(r)) }))
}
}


fn happyReduce_244() -> ActionReturn {
    partial_5!(happySpecReduce_3, 71, box happyReduction_244)
}

fn happyReduction_244(happy_x_3: HappyAbsSyn, happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_3, happy_x_2, happy_x_1) {
        (_, HappyAbsSyn66(happy_var_2), _) => HappyAbsSyn66(happy_var_2),
        _ => notHappyAtAll()
    }
}


fn happyReduce_245() -> ActionReturn {
    partial_5!(happyReduce, 4, 71, box happyReduction_245)
}

refute! { fn happyReduction_245(HappyStk(HappyAbsSyn88(happy_var_4), Some(box HappyStk(_, Some(box HappyStk(HappyAbsSyn66(happy_var_2), Some(box HappyStk(_, Some(box happyRest)))))))): HappyStk<HappyAbsSyn>) -> HappyStk<HappyAbsSyn> {
    HappyStk(HappyAbsSyn66(happy_var_4(happy_var_2)), Some(box happyRest))
}
}


fn happyReduce_246() -> ActionReturn {
    partial_5!(happyReduce, 4, 71, box happyReduction_246)
}

refute! { fn happyReduction_246(HappyStk(_, Some(box HappyStk(HappyAbsSyn66(happy_var_3), Some(box HappyStk(HappyAbsSyn132(happy_var_2), Some(box HappyStk(_, Some(box happyRest)))))))): HappyStk<HappyAbsSyn>) -> HappyStk<HappyAbsSyn> {
    HappyStk(HappyAbsSyn66(appendDeclrAttrs(happy_var_2, happy_var_3)), Some(box happyRest))
}
}


fn happyReduce_247() -> ActionReturn {
    partial_5!(happyReduce, 5, 71, box happyReduction_247)
}

refute! { fn happyReduction_247(HappyStk(HappyAbsSyn88(happy_var_5), Some(box HappyStk(_, Some(box HappyStk(HappyAbsSyn66(happy_var_3), Some(box HappyStk(HappyAbsSyn132(happy_var_2), Some(box HappyStk(_, Some(box happyRest)))))))))): HappyStk<HappyAbsSyn>) -> HappyStk<HappyAbsSyn> {
    HappyStk(HappyAbsSyn66(appendDeclrAttrs(happy_var_2, happy_var_5(happy_var_3))), Some(box happyRest))
}
}


fn happyReduce_248() -> ActionReturn {
    partial_5!(happySpecReduce_1, 72, box happyReduction_248)
}

fn happyReduction_248(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn66(happy_var_1) => HappyAbsSyn66(happy_var_1),
        _ => notHappyAtAll()
    }
}


fn happyReduce_249() -> ActionReturn {
    partial_5!(happyMonadReduce, 4, 72, box happyReduction_249)
}

refute! { fn happyReduction_249<T>(HappyStk(_, Some(box HappyStk(HappyAbsSyn66(happy_var_3), Some(box HappyStk(_, Some(box HappyStk(HappyTerminal(happy_var_1), Some(box happyRest)))))))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1, partial_1!(ptrDeclr, happy_var_3, vec![])) }, (box move |r| { happyReturn(HappyAbsSyn66(r)) }))
}
}


fn happyReduce_250() -> ActionReturn {
    partial_5!(happyMonadReduce, 5, 72, box happyReduction_250)
}

refute! { fn happyReduction_250<T>(HappyStk(_, Some(box HappyStk(HappyAbsSyn66(happy_var_4), Some(box HappyStk(_, Some(box HappyStk(HappyAbsSyn65(happy_var_2), Some(box HappyStk(HappyTerminal(happy_var_1), Some(box happyRest)))))))))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1, partial_1!(ptrDeclr, happy_var_4, reverse(happy_var_2))) }, (box move |r| { happyReturn(HappyAbsSyn66(r)) }))
}
}


fn happyReduce_251() -> ActionReturn {
    partial_5!(happyMonadReduce, 6, 72, box happyReduction_251)
}

refute! { fn happyReduction_251<T>(HappyStk(_, Some(box HappyStk(HappyAbsSyn66(happy_var_5), Some(box HappyStk(_, Some(box HappyStk(HappyAbsSyn132(happy_var_3), Some(box HappyStk(HappyAbsSyn65(happy_var_2), Some(box HappyStk(HappyTerminal(happy_var_1), Some(box happyRest)))))))))))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withAttribute(happy_var_1, happy_var_3, partial_1!(ptrDeclr, happy_var_5, reverse(happy_var_2))) }, (box move |r| { happyReturn(HappyAbsSyn66(r)) }))
}
}


fn happyReduce_252() -> ActionReturn {
    partial_5!(happyMonadReduce, 2, 72, box happyReduction_252)
}

refute! { fn happyReduction_252<T>(HappyStk(HappyAbsSyn66(happy_var_2), Some(box HappyStk(HappyTerminal(happy_var_1), Some(box happyRest)))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1, partial_1!(ptrDeclr, happy_var_2, vec![])) }, (box move |r| { happyReturn(HappyAbsSyn66(r)) }))
}
}


fn happyReduce_253() -> ActionReturn {
    partial_5!(happyMonadReduce, 3, 72, box happyReduction_253)
}

refute! { fn happyReduction_253<T>(HappyStk(HappyAbsSyn66(happy_var_3), Some(box HappyStk(HappyAbsSyn65(happy_var_2), Some(box HappyStk(HappyTerminal(happy_var_1), Some(box happyRest)))))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1, partial_1!(ptrDeclr, happy_var_3, reverse(happy_var_2))) }, (box move |r| { happyReturn(HappyAbsSyn66(r)) }))
}
}


fn happyReduce_254() -> ActionReturn {
    partial_5!(happyMonadReduce, 4, 72, box happyReduction_254)
}

refute! { fn happyReduction_254<T>(HappyStk(HappyAbsSyn66(happy_var_4), Some(box HappyStk(HappyAbsSyn132(happy_var_3), Some(box HappyStk(HappyAbsSyn65(happy_var_2), Some(box HappyStk(HappyTerminal(happy_var_1), Some(box happyRest)))))))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withAttribute(happy_var_1, happy_var_3, partial_1!(ptrDeclr, happy_var_4, reverse(happy_var_2))) }, (box move |r| { happyReturn(HappyAbsSyn66(r)) }))
}
}


fn happyReduce_255() -> ActionReturn {
    partial_5!(happySpecReduce_3, 73, box happyReduction_255)
}

fn happyReduction_255(happy_x_3: HappyAbsSyn, happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_3, happy_x_2, happy_x_1) {
        (_, HappyAbsSyn66(happy_var_2), _) => HappyAbsSyn66(happy_var_2),
        _ => notHappyAtAll()
    }
}


fn happyReduce_256() -> ActionReturn {
    partial_5!(happyReduce, 4, 73, box happyReduction_256)
}

refute! { fn happyReduction_256(HappyStk(_, Some(box HappyStk(HappyAbsSyn88(happy_var_3), Some(box HappyStk(HappyAbsSyn66(happy_var_2), Some(box HappyStk(_, Some(box happyRest)))))))): HappyStk<HappyAbsSyn>) -> HappyStk<HappyAbsSyn> {
    HappyStk(HappyAbsSyn66(happy_var_3(happy_var_2)), Some(box happyRest))
}
}


fn happyReduce_257() -> ActionReturn {
    partial_5!(happyReduce, 4, 73, box happyReduction_257)
}

refute! { fn happyReduction_257(HappyStk(HappyAbsSyn88(happy_var_4), Some(box HappyStk(_, Some(box HappyStk(HappyAbsSyn66(happy_var_2), Some(box HappyStk(_, Some(box happyRest)))))))): HappyStk<HappyAbsSyn>) -> HappyStk<HappyAbsSyn> {
    HappyStk(HappyAbsSyn66(happy_var_4(happy_var_2)), Some(box happyRest))
}
}


fn happyReduce_258() -> ActionReturn {
    partial_5!(happyMonadReduce, 1, 74, box happyReduction_258)
}

refute! { fn happyReduction_258<T>(HappyStk(HappyTerminal(CTokTyIdent(_, happy_var_1)), Some(box happyRest)): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1.clone(), partial_1!(mkVarDeclr, happy_var_1)) }, (box move |r| { happyReturn(HappyAbsSyn66(r)) }))
}
}


fn happyReduce_259() -> ActionReturn {
    partial_5!(happySpecReduce_3, 74, box happyReduction_259)
}

fn happyReduction_259(happy_x_3: HappyAbsSyn, happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_3, happy_x_2, happy_x_1) {
        (_, HappyAbsSyn66(happy_var_2), _) => HappyAbsSyn66(happy_var_2),
        _ => notHappyAtAll()
    }
}


fn happyReduce_260() -> ActionReturn {
    partial_5!(happySpecReduce_1, 75, box happyReduction_260)
}

fn happyReduction_260(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn66(happy_var_1) => HappyAbsSyn66(happy_var_1),
        _ => notHappyAtAll()
    }
}


fn happyReduce_261() -> ActionReturn {
    partial_5!(happySpecReduce_1, 75, box happyReduction_261)
}

fn happyReduction_261(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn66(happy_var_1) => HappyAbsSyn66(happy_var_1),
        _ => notHappyAtAll()
    }
}


fn happyReduce_262() -> ActionReturn {
    partial_5!(happySpecReduce_1, 76, box happyReduction_262)
}

fn happyReduction_262(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn66(happy_var_1) => HappyAbsSyn66(happy_var_1),
        _ => notHappyAtAll()
    }
}


fn happyReduce_263() -> ActionReturn {
    partial_5!(happyMonadReduce, 2, 76, box happyReduction_263)
}

refute! { fn happyReduction_263<T>(HappyStk(HappyAbsSyn66(happy_var_2), Some(box HappyStk(HappyTerminal(happy_var_1), Some(box happyRest)))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1, partial_1!(ptrDeclr, happy_var_2, vec![])) }, (box move |r| { happyReturn(HappyAbsSyn66(r)) }))
}
}


fn happyReduce_264() -> ActionReturn {
    partial_5!(happyMonadReduce, 3, 76, box happyReduction_264)
}

refute! { fn happyReduction_264<T>(HappyStk(HappyAbsSyn66(happy_var_3), Some(box HappyStk(HappyAbsSyn132(happy_var_2), Some(box HappyStk(HappyTerminal(happy_var_1), Some(box happyRest)))))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withAttribute(happy_var_1, happy_var_2, partial_1!(ptrDeclr, happy_var_3, vec![])) }, (box move |r| { happyReturn(HappyAbsSyn66(r)) }))
}
}


fn happyReduce_265() -> ActionReturn {
    partial_5!(happyMonadReduce, 3, 76, box happyReduction_265)
}

refute! { fn happyReduction_265<T>(HappyStk(HappyAbsSyn66(happy_var_3), Some(box HappyStk(HappyAbsSyn65(happy_var_2), Some(box HappyStk(HappyTerminal(happy_var_1), Some(box happyRest)))))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1, partial_1!(ptrDeclr, happy_var_3, reverse(happy_var_2))) }, (box move |r| { happyReturn(HappyAbsSyn66(r)) }))
}
}


fn happyReduce_266() -> ActionReturn {
    partial_5!(happyMonadReduce, 4, 76, box happyReduction_266)
}

refute! { fn happyReduction_266<T>(HappyStk(HappyAbsSyn66(happy_var_4), Some(box HappyStk(HappyAbsSyn132(happy_var_3), Some(box HappyStk(HappyAbsSyn65(happy_var_2), Some(box HappyStk(HappyTerminal(happy_var_1), Some(box happyRest)))))))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withAttribute(happy_var_1, happy_var_3, partial_1!(ptrDeclr, happy_var_4, reverse(happy_var_2))) }, (box move |r| { happyReturn(HappyAbsSyn66(r)) }))
}
}


fn happyReduce_267() -> ActionReturn {
    partial_5!(happySpecReduce_2, 77, box happyReduction_267)
}

fn happyReduction_267(happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_2, happy_x_1) {
        (HappyAbsSyn88(happy_var_2), HappyAbsSyn66(happy_var_1)) => HappyAbsSyn66(happy_var_2(happy_var_1)),
        _ => notHappyAtAll()
    }
}


fn happyReduce_268() -> ActionReturn {
    partial_5!(happySpecReduce_3, 77, box happyReduction_268)
}

fn happyReduction_268(happy_x_3: HappyAbsSyn, happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_3, happy_x_2, happy_x_1) {
        (_, HappyAbsSyn66(happy_var_2), _) => HappyAbsSyn66(happy_var_2),
        _ => notHappyAtAll()
    }
}


fn happyReduce_269() -> ActionReturn {
    partial_5!(happyReduce, 4, 77, box happyReduction_269)
}

refute! { fn happyReduction_269(HappyStk(HappyAbsSyn88(happy_var_4), Some(box HappyStk(_, Some(box HappyStk(HappyAbsSyn66(happy_var_2), Some(box HappyStk(_, Some(box happyRest)))))))): HappyStk<HappyAbsSyn>) -> HappyStk<HappyAbsSyn> {
    HappyStk(HappyAbsSyn66(happy_var_4(happy_var_2)), Some(box happyRest))
}
}


fn happyReduce_270() -> ActionReturn {
    partial_5!(happyReduce, 4, 77, box happyReduction_270)
}

refute! { fn happyReduction_270(HappyStk(_, Some(box HappyStk(HappyAbsSyn66(happy_var_3), Some(box HappyStk(HappyAbsSyn132(happy_var_2), Some(box HappyStk(_, Some(box happyRest)))))))): HappyStk<HappyAbsSyn>) -> HappyStk<HappyAbsSyn> {
    HappyStk(HappyAbsSyn66(appendDeclrAttrs(happy_var_2, happy_var_3)), Some(box happyRest))
}
}


fn happyReduce_271() -> ActionReturn {
    partial_5!(happyReduce, 5, 77, box happyReduction_271)
}

refute! { fn happyReduction_271(HappyStk(HappyAbsSyn88(happy_var_5), Some(box HappyStk(_, Some(box HappyStk(HappyAbsSyn66(happy_var_3), Some(box HappyStk(HappyAbsSyn132(happy_var_2), Some(box HappyStk(_, Some(box happyRest)))))))))): HappyStk<HappyAbsSyn>) -> HappyStk<HappyAbsSyn> {
    HappyStk(HappyAbsSyn66(appendDeclrAttrs(happy_var_2, happy_var_5(happy_var_3))), Some(box happyRest))
}
}


fn happyReduce_272() -> ActionReturn {
    partial_5!(happyMonadReduce, 1, 78, box happyReduction_272)
}

refute! { fn happyReduction_272<T>(HappyStk(HappyTerminal(CTokIdent(_, happy_var_1)), Some(box happyRest)): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1.clone(), partial_1!(mkVarDeclr, happy_var_1)) }, (box move |r| { happyReturn(HappyAbsSyn66(r)) }))
}
}


fn happyReduce_273() -> ActionReturn {
    partial_5!(happySpecReduce_3, 78, box happyReduction_273)
}

fn happyReduction_273(happy_x_3: HappyAbsSyn, happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_3, happy_x_2, happy_x_1) {
        (_, HappyAbsSyn66(happy_var_2), _) => HappyAbsSyn66(happy_var_2),
        _ => notHappyAtAll()
    }
}


fn happyReduce_274() -> ActionReturn {
    partial_5!(happyReduce, 4, 78, box happyReduction_274)
}

refute! { fn happyReduction_274(HappyStk(_, Some(box HappyStk(HappyAbsSyn66(happy_var_3), Some(box HappyStk(HappyAbsSyn132(happy_var_2), Some(box HappyStk(_, Some(box happyRest)))))))): HappyStk<HappyAbsSyn>) -> HappyStk<HappyAbsSyn> {
    HappyStk(HappyAbsSyn66(appendDeclrAttrs(happy_var_2, happy_var_3)), Some(box happyRest))
}
}


fn happyReduce_275() -> ActionReturn {
    partial_5!(happySpecReduce_1, 79, box happyReduction_275)
}

fn happyReduction_275(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn66(happy_var_1) => HappyAbsSyn11(reverseDeclr(happy_var_1)),
        _ => notHappyAtAll()
    }
}


fn happyReduce_276() -> ActionReturn {
    partial_5!(happySpecReduce_1, 80, box happyReduction_276)
}

fn happyReduction_276(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn66(happy_var_1) => HappyAbsSyn66(happy_var_1),
        _ => notHappyAtAll()
    }
}


fn happyReduce_277() -> ActionReturn {
    partial_5!(happyMonadReduce, 2, 80, box happyReduction_277)
}

refute! { fn happyReduction_277<T>(HappyStk(HappyAbsSyn66(happy_var_2), Some(box HappyStk(HappyTerminal(happy_var_1), Some(box happyRest)))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1, partial_1!(ptrDeclr, happy_var_2, vec![])) }, (box move |r| { happyReturn(HappyAbsSyn66(r)) }))
}
}


fn happyReduce_278() -> ActionReturn {
    partial_5!(happyMonadReduce, 3, 80, box happyReduction_278)
}

refute! { fn happyReduction_278<T>(HappyStk(HappyAbsSyn66(happy_var_3), Some(box HappyStk(HappyAbsSyn65(happy_var_2), Some(box HappyStk(HappyTerminal(happy_var_1), Some(box happyRest)))))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1, partial_1!(ptrDeclr, happy_var_3, reverse(happy_var_2))) }, (box move |r| { happyReturn(HappyAbsSyn66(r)) }))
}
}


fn happyReduce_279() -> ActionReturn {
    partial_5!(happyMonadReduce, 4, 81, box happyReduction_279)
}

refute! { fn happyReduction_279<T>(HappyStk(_, Some(box HappyStk(HappyAbsSyn21(happy_var_3), Some(box HappyStk(_, Some(box HappyStk(HappyAbsSyn66(happy_var_1), Some(box happyRest)))))))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1.clone(), partial_1!(funDeclr, happy_var_1, Left(reverse(happy_var_3)), vec![])) }, (box move |r| { happyReturn(HappyAbsSyn66(r)) }))
}
}


fn happyReduce_280() -> ActionReturn {
    partial_5!(happySpecReduce_3, 81, box happyReduction_280)
}

fn happyReduction_280(happy_x_3: HappyAbsSyn, happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_3, happy_x_2, happy_x_1) {
        (_, HappyAbsSyn66(happy_var_2), _) => HappyAbsSyn66(happy_var_2),
        _ => notHappyAtAll()
    }
}


fn happyReduce_281() -> ActionReturn {
    partial_5!(happyReduce, 4, 81, box happyReduction_281)
}

refute! { fn happyReduction_281(HappyStk(HappyAbsSyn88(happy_var_4), Some(box HappyStk(_, Some(box HappyStk(HappyAbsSyn66(happy_var_2), Some(box HappyStk(_, Some(box happyRest)))))))): HappyStk<HappyAbsSyn>) -> HappyStk<HappyAbsSyn> {
    HappyStk(HappyAbsSyn66(happy_var_4(happy_var_2)), Some(box happyRest))
}
}


fn happyReduce_282() -> ActionReturn {
    partial_5!(happySpecReduce_0, 82, happyReduction_282())
}

fn happyReduction_282() -> HappyAbsSyn {
    match () {
        () => HappyAbsSyn82((vec![], false)),
    }
}


fn happyReduce_283() -> ActionReturn {
    partial_5!(happySpecReduce_1, 82, box happyReduction_283)
}

fn happyReduction_283(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn33(happy_var_1) => HappyAbsSyn82((reverse(happy_var_1), false)),
        _ => notHappyAtAll()
    }
}


fn happyReduce_284() -> ActionReturn {
    partial_5!(happySpecReduce_3, 82, box happyReduction_284)
}

fn happyReduction_284(happy_x_3: HappyAbsSyn, happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_3, happy_x_2, happy_x_1) {
        (_, _, HappyAbsSyn33(happy_var_1)) => HappyAbsSyn82((reverse(happy_var_1), true)),
        _ => notHappyAtAll()
    }
}


fn happyReduce_285() -> ActionReturn {
    partial_5!(happySpecReduce_1, 83, box happyReduction_285)
}

fn happyReduction_285(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn32(happy_var_1) => HappyAbsSyn33(singleton(happy_var_1)),
        _ => notHappyAtAll()
    }
}


fn happyReduce_286() -> ActionReturn {
    partial_5!(happySpecReduce_3, 83, box happyReduction_286)
}

fn happyReduction_286(happy_x_3: HappyAbsSyn, happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_3, happy_x_2, happy_x_1) {
        (HappyAbsSyn32(happy_var_3), _, HappyAbsSyn33(happy_var_1)) => HappyAbsSyn33(snoc(happy_var_1, happy_var_3)),
        _ => notHappyAtAll()
    }
}


fn happyReduce_287() -> ActionReturn {
    partial_5!(happyMonadReduce, 1, 84, box happyReduction_287)
}

refute! { fn happyReduction_287<T>(HappyStk(HappyAbsSyn37(happy_var_1), Some(box happyRest)): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1.clone(), partial_1!(CDecl, happy_var_1, vec![])) }, (box move |r| { happyReturn(HappyAbsSyn32(r)) }))
}
}


fn happyReduce_288() -> ActionReturn {
    partial_5!(happyMonadReduce, 2, 84, box happyReduction_288)
}

refute! { fn happyReduction_288<T>(HappyStk(HappyAbsSyn66(happy_var_2), Some(box HappyStk(HappyAbsSyn37(happy_var_1), Some(box happyRest)))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1.clone(), partial_1!(CDecl, happy_var_1, vec![(Some(reverseDeclr(happy_var_2)), None, None)])) }, (box move |r| { happyReturn(HappyAbsSyn32(r)) }))
}
}


fn happyReduce_289() -> ActionReturn {
    partial_5!(happyMonadReduce, 3, 84, box happyReduction_289)
}

refute! { fn happyReduction_289<T>(HappyStk(HappyAbsSyn132(happy_var_3), Some(box HappyStk(HappyAbsSyn66(happy_var_2), Some(box HappyStk(HappyAbsSyn37(happy_var_1), Some(box happyRest)))))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1.clone(), partial_1!(CDecl, happy_var_1,
                                               vec![(Some(reverseDeclr(appendDeclrAttrs(happy_var_3, happy_var_2))), None, None)])) }, (box move |r| { happyReturn(HappyAbsSyn32(r)) }))
}
}


fn happyReduce_290() -> ActionReturn {
    partial_5!(happyMonadReduce, 3, 84, box happyReduction_290)
}

refute! { fn happyReduction_290<T>(HappyStk(HappyAbsSyn132(happy_var_3), Some(box HappyStk(HappyAbsSyn66(happy_var_2), Some(box HappyStk(HappyAbsSyn37(happy_var_1), Some(box happyRest)))))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1.clone(), partial_1!(CDecl, happy_var_1,
                                               vec![(Some(reverseDeclr(appendDeclrAttrs(happy_var_3, happy_var_2))), None, None)])) }, (box move |r| { happyReturn(HappyAbsSyn32(r)) }))
}
}


fn happyReduce_291() -> ActionReturn {
    partial_5!(happyMonadReduce, 1, 84, box happyReduction_291)
}

refute! { fn happyReduction_291<T>(HappyStk(HappyAbsSyn38(happy_var_1), Some(box happyRest)): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1.clone(), partial_1!(CDecl, reverse(happy_var_1), vec![])) }, (box move |r| { happyReturn(HappyAbsSyn32(r)) }))
}
}


fn happyReduce_292() -> ActionReturn {
    partial_5!(happyMonadReduce, 2, 84, box happyReduction_292)
}

refute! { fn happyReduction_292<T>(HappyStk(HappyAbsSyn66(happy_var_2), Some(box HappyStk(HappyAbsSyn38(happy_var_1), Some(box happyRest)))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1.clone(), partial_1!(CDecl, reverse(happy_var_1), vec![(Some(reverseDeclr(happy_var_2)), None, None)])) }, (box move |r| { happyReturn(HappyAbsSyn32(r)) }))
}
}


fn happyReduce_293() -> ActionReturn {
    partial_5!(happyMonadReduce, 3, 84, box happyReduction_293)
}

refute! { fn happyReduction_293<T>(HappyStk(HappyAbsSyn132(happy_var_3), Some(box HappyStk(HappyAbsSyn66(happy_var_2), Some(box HappyStk(HappyAbsSyn38(happy_var_1), Some(box happyRest)))))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1.clone(), partial_1!(CDecl, reverse(happy_var_1),
                                               vec![(Some(reverseDeclr(appendDeclrAttrs(happy_var_3, happy_var_2))), None, None)])) }, (box move |r| { happyReturn(HappyAbsSyn32(r)) }))
}
}


fn happyReduce_294() -> ActionReturn {
    partial_5!(happyMonadReduce, 1, 84, box happyReduction_294)
}

refute! { fn happyReduction_294<T>(HappyStk(HappyAbsSyn37(happy_var_1), Some(box happyRest)): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1.clone(), partial_1!(CDecl, happy_var_1, vec![])) }, (box move |r| { happyReturn(HappyAbsSyn32(r)) }))
}
}


fn happyReduce_295() -> ActionReturn {
    partial_5!(happyMonadReduce, 2, 84, box happyReduction_295)
}

refute! { fn happyReduction_295<T>(HappyStk(HappyAbsSyn66(happy_var_2), Some(box HappyStk(HappyAbsSyn37(happy_var_1), Some(box happyRest)))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1.clone(), partial_1!(CDecl, happy_var_1, vec![(Some(reverseDeclr(happy_var_2)), None, None)])) }, (box move |r| { happyReturn(HappyAbsSyn32(r)) }))
}
}


fn happyReduce_296() -> ActionReturn {
    partial_5!(happyMonadReduce, 3, 84, box happyReduction_296)
}

refute! { fn happyReduction_296<T>(HappyStk(HappyAbsSyn132(happy_var_3), Some(box HappyStk(HappyAbsSyn66(happy_var_2), Some(box HappyStk(HappyAbsSyn37(happy_var_1), Some(box happyRest)))))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1.clone(), partial_1!(CDecl, happy_var_1,
                                               vec![(Some(reverseDeclr(appendDeclrAttrs(happy_var_3, happy_var_2))), None, None)])) }, (box move |r| { happyReturn(HappyAbsSyn32(r)) }))
}
}


fn happyReduce_297() -> ActionReturn {
    partial_5!(happyMonadReduce, 3, 84, box happyReduction_297)
}

refute! { fn happyReduction_297<T>(HappyStk(HappyAbsSyn132(happy_var_3), Some(box HappyStk(HappyAbsSyn66(happy_var_2), Some(box HappyStk(HappyAbsSyn37(happy_var_1), Some(box happyRest)))))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1.clone(), partial_1!(CDecl, happy_var_1,
                                               vec![(Some(reverseDeclr(appendDeclrAttrs(happy_var_3, happy_var_2))), None, None)])) }, (box move |r| { happyReturn(HappyAbsSyn32(r)) }))
}
}


fn happyReduce_298() -> ActionReturn {
    partial_5!(happyMonadReduce, 1, 84, box happyReduction_298)
}

refute! { fn happyReduction_298<T>(HappyStk(HappyAbsSyn65(happy_var_1), Some(box happyRest)): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1.clone(), partial_1!(CDecl, liftTypeQuals(happy_var_1), vec![])) }, (box move |r| { happyReturn(HappyAbsSyn32(r)) }))
}
}


fn happyReduce_299() -> ActionReturn {
    partial_5!(happyMonadReduce, 2, 84, box happyReduction_299)
}

refute! { fn happyReduction_299<T>(HappyStk(HappyAbsSyn132(happy_var_2), Some(box HappyStk(HappyAbsSyn65(happy_var_1), Some(box happyRest)))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1.clone(), partial_1!(CDecl, __op_addadd(liftTypeQuals(happy_var_1), liftCAttrs(happy_var_2)), vec![])) }, (box move |r| { happyReturn(HappyAbsSyn32(r)) }))
}
}


fn happyReduce_300() -> ActionReturn {
    partial_5!(happyMonadReduce, 2, 84, box happyReduction_300)
}

refute! { fn happyReduction_300<T>(HappyStk(HappyAbsSyn66(happy_var_2), Some(box HappyStk(HappyAbsSyn65(happy_var_1), Some(box happyRest)))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1.clone(), partial_1!(CDecl, liftTypeQuals(happy_var_1),
                                               vec![(Some(reverseDeclr(happy_var_2)), None, None)])) }, (box move |r| { happyReturn(HappyAbsSyn32(r)) }))
}
}


fn happyReduce_301() -> ActionReturn {
    partial_5!(happyMonadReduce, 3, 84, box happyReduction_301)
}

refute! { fn happyReduction_301<T>(HappyStk(HappyAbsSyn132(happy_var_3), Some(box HappyStk(HappyAbsSyn66(happy_var_2), Some(box HappyStk(HappyAbsSyn65(happy_var_1), Some(box happyRest)))))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1.clone(), partial_1!(CDecl, liftTypeQuals(happy_var_1),
                                               vec![(Some(reverseDeclr(appendDeclrAttrs(happy_var_3, happy_var_2))), None, None)])) }, (box move |r| { happyReturn(HappyAbsSyn32(r)) }))
}
}


fn happyReduce_302() -> ActionReturn {
    partial_5!(happySpecReduce_1, 85, box happyReduction_302)
}

fn happyReduction_302(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyTerminal(CTokIdent(_, happy_var_1)) => HappyAbsSyn21(singleton(happy_var_1)),
        _ => notHappyAtAll()
    }
}


fn happyReduce_303() -> ActionReturn {
    partial_5!(happySpecReduce_3, 85, box happyReduction_303)
}

fn happyReduction_303(happy_x_3: HappyAbsSyn, happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_3, happy_x_2, happy_x_1) {
        (HappyTerminal(CTokIdent(_, happy_var_3)), _, HappyAbsSyn21(happy_var_1)) => HappyAbsSyn21(snoc(happy_var_1, happy_var_3)),
        _ => notHappyAtAll()
    }
}


fn happyReduce_304() -> ActionReturn {
    partial_5!(happyMonadReduce, 1, 86, box happyReduction_304)
}

refute! { fn happyReduction_304<T>(HappyStk(HappyAbsSyn37(happy_var_1), Some(box happyRest)): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1.clone(), partial_1!(CDecl, happy_var_1, vec![])) }, (box move |r| { happyReturn(HappyAbsSyn32(r)) }))
}
}


fn happyReduce_305() -> ActionReturn {
    partial_5!(happyMonadReduce, 2, 86, box happyReduction_305)
}

refute! { fn happyReduction_305<T>(HappyStk(HappyAbsSyn66(happy_var_2), Some(box HappyStk(HappyAbsSyn37(happy_var_1), Some(box happyRest)))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1.clone(), partial_1!(CDecl, happy_var_1, vec![(Some(reverseDeclr(happy_var_2)), None, None)])) }, (box move |r| { happyReturn(HappyAbsSyn32(r)) }))
}
}


fn happyReduce_306() -> ActionReturn {
    partial_5!(happyMonadReduce, 2, 86, box happyReduction_306)
}

refute! { fn happyReduction_306<T>(HappyStk(HappyAbsSyn132(happy_var_2), Some(box HappyStk(HappyAbsSyn65(happy_var_1), Some(box happyRest)))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1.clone(), partial_1!(CDecl, __op_addadd(liftTypeQuals(happy_var_1), liftCAttrs(happy_var_2)), vec![])) }, (box move |r| { happyReturn(HappyAbsSyn32(r)) }))
}
}


fn happyReduce_307() -> ActionReturn {
    partial_5!(happyMonadReduce, 2, 86, box happyReduction_307)
}

refute! { fn happyReduction_307<T>(HappyStk(HappyAbsSyn66(happy_var_2), Some(box HappyStk(HappyAbsSyn65(happy_var_1), Some(box happyRest)))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1.clone(), partial_1!(CDecl, liftTypeQuals(happy_var_1),
                                               vec![(Some(reverseDeclr(happy_var_2)), None, None)])) }, (box move |r| { happyReturn(HappyAbsSyn32(r)) }))
}
}


fn happyReduce_308() -> ActionReturn {
    partial_5!(happySpecReduce_1, 87, box happyReduction_308)
}

fn happyReduction_308(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn66(happy_var_1) => HappyAbsSyn66(happy_var_1),
        _ => notHappyAtAll()
    }
}


fn happyReduce_309() -> ActionReturn {
    partial_5!(happySpecReduce_1, 87, box happyReduction_309)
}

fn happyReduction_309(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn66(happy_var_1) => HappyAbsSyn66(happy_var_1),
        _ => notHappyAtAll()
    }
}


fn happyReduce_310() -> ActionReturn {
    partial_5!(happySpecReduce_1, 87, box happyReduction_310)
}

fn happyReduction_310(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn88(happy_var_1) => HappyAbsSyn66(happy_var_1(emptyDeclr())),
        _ => notHappyAtAll()
    }
}


fn happyReduce_311() -> ActionReturn {
    partial_5!(happySpecReduce_1, 88, box happyReduction_311)
}

fn happyReduction_311(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn88(happy_var_1) => HappyAbsSyn88(happy_var_1),
        _ => notHappyAtAll()
    }
}


fn happyReduce_312() -> ActionReturn {
    partial_5!(happyMonadReduce, 3, 88, box happyReduction_312)
}

refute! { fn happyReduction_312<T>(HappyStk(_, Some(box HappyStk(HappyAbsSyn82(happy_var_2), Some(box HappyStk(HappyTerminal(happy_var_1), Some(box happyRest)))))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({
            withNodeInfo(happy_var_1, box move |at: NodeInfo| {
                let a: Rc<Box<Fn(CDeclrR) -> CDeclrR>> = Rc::new(box move |declr| {
                    let (params, variadic) = happy_var_2.clone();
                    funDeclr(declr, (Right((params, variadic))), vec![], at.clone())
                });
                a
            }) }, (box move |r| { happyReturn(HappyAbsSyn88(r)) }))
}
}


fn happyReduce_313() -> ActionReturn {
    partial_5!(happySpecReduce_1, 89, box happyReduction_313)
}

fn happyReduction_313(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn88(happy_var_1) => HappyAbsSyn88(happy_var_1),
        _ => notHappyAtAll()
    }
}


fn happyReduce_314() -> ActionReturn {
    partial_5!(happySpecReduce_2, 89, box happyReduction_314)
}

fn happyReduction_314(happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_2, happy_x_1) {
        (HappyAbsSyn88(happy_var_2), HappyAbsSyn88(happy_var_1)) => HappyAbsSyn88(Rc::new(box move |decl| { happy_var_2(happy_var_1(decl)) })),
        _ => notHappyAtAll()
    }
}


fn happyReduce_315() -> ActionReturn {
    partial_5!(happyMonadReduce, 3, 90, box happyReduction_315)
}

refute! { fn happyReduction_315<T>(HappyStk(_, Some(box HappyStk(HappyAbsSyn124(happy_var_2), Some(box HappyStk(HappyTerminal(happy_var_1), Some(box happyRest)))))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({
            withNodeInfo(happy_var_1.clone(), box move |at: NodeInfo| {
                let a: Rc<Box<Fn(CDeclrR) -> CDeclrR>> = Rc::new(box move |declr| {
                    arrDeclr(declr, vec![], false, false, happy_var_2.clone(), at.clone())
                });
                a
            }) }, (box move |r| { happyReturn(HappyAbsSyn88(r)) }))
}
}


fn happyReduce_316() -> ActionReturn {
    partial_5!(happyMonadReduce, 4, 90, box happyReduction_316)
}

refute! { fn happyReduction_316<T>(HappyStk(_, Some(box HappyStk(HappyAbsSyn124(happy_var_3), Some(box HappyStk(HappyAbsSyn132(happy_var_2), Some(box HappyStk(HappyTerminal(happy_var_1), Some(box happyRest)))))))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withAttributePF(happy_var_1, happy_var_2, box move |at, declr|
                           arrDeclr(declr, vec![], false, false, happy_var_3.clone(), at)) }, (box move |r| { happyReturn(HappyAbsSyn88(r)) }))
}
}


fn happyReduce_317() -> ActionReturn {
    partial_5!(happyMonadReduce, 4, 90, box happyReduction_317)
}

refute! { fn happyReduction_317<T>(HappyStk(_, Some(box HappyStk(HappyAbsSyn124(happy_var_3), Some(box HappyStk(HappyAbsSyn65(happy_var_2), Some(box HappyStk(HappyTerminal(happy_var_1), Some(box happyRest)))))))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({
            withNodeInfo(happy_var_1.clone(), box move |at: NodeInfo| {
                let a: Rc<Box<Fn(CDeclrR) -> CDeclrR>> = Rc::new(
                    box move |declr| { arrDeclr(declr, reverse(happy_var_2.clone()),
                                                false, false, happy_var_3.clone(), at.clone()) });
                a
            }) }, (box move |r| { happyReturn(HappyAbsSyn88(r)) }))
}
}


fn happyReduce_318() -> ActionReturn {
    partial_5!(happyMonadReduce, 5, 90, box happyReduction_318)
}

refute! { fn happyReduction_318<T>(HappyStk(_, Some(box HappyStk(HappyAbsSyn124(happy_var_4), Some(box HappyStk(HappyAbsSyn132(happy_var_3), Some(box HappyStk(HappyAbsSyn65(happy_var_2), Some(box HappyStk(HappyTerminal(happy_var_1), Some(box happyRest)))))))))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withAttributePF(happy_var_1, happy_var_3, box move |at, declr|
                           arrDeclr(declr, reverse(happy_var_2.clone()), false, false, happy_var_4.clone(), at)) }, (box move |r| { happyReturn(HappyAbsSyn88(r)) }))
}
}


fn happyReduce_319() -> ActionReturn {
    partial_5!(happyMonadReduce, 5, 90, box happyReduction_319)
}

refute! { fn happyReduction_319<T>(HappyStk(_, Some(box HappyStk(HappyAbsSyn100(happy_var_4), Some(box HappyStk(HappyAbsSyn132(happy_var_3), Some(box HappyStk(_, Some(box HappyStk(HappyTerminal(happy_var_1), Some(box happyRest)))))))))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withAttributePF(happy_var_1, happy_var_3, box move |at, declr|
                           arrDeclr(declr, vec![], false, true, Some(happy_var_4.clone()), at)) }, (box move |r| { happyReturn(HappyAbsSyn88(r)) }))
}
}


fn happyReduce_320() -> ActionReturn {
    partial_5!(happyMonadReduce, 6, 90, box happyReduction_320)
}

refute! { fn happyReduction_320<T>(HappyStk(_, Some(box HappyStk(HappyAbsSyn100(happy_var_5), Some(box HappyStk(HappyAbsSyn132(happy_var_4), Some(box HappyStk(HappyAbsSyn65(happy_var_3), Some(box HappyStk(_, Some(box HappyStk(HappyTerminal(happy_var_1), Some(box happyRest)))))))))))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withAttributePF(happy_var_1, happy_var_4, box move |at, declr|
                           arrDeclr(declr, reverse(happy_var_3.clone()), false, true, Some(happy_var_5.clone()), at)) }, (box move |r| { happyReturn(HappyAbsSyn88(r)) }))
}
}


fn happyReduce_321() -> ActionReturn {
    partial_5!(happyMonadReduce, 7, 90, box happyReduction_321)
}

refute! { fn happyReduction_321<T>(HappyStk(_, Some(box HappyStk(HappyAbsSyn100(happy_var_6), Some(box HappyStk(HappyAbsSyn132(happy_var_5), Some(box HappyStk(_, Some(box HappyStk(HappyAbsSyn132(happy_var_3), Some(box HappyStk(HappyAbsSyn65(happy_var_2), Some(box HappyStk(HappyTerminal(happy_var_1), Some(box happyRest)))))))))))))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withAttributePF(happy_var_1, __op_addadd(happy_var_3, happy_var_5), box move |at, declr|
                           arrDeclr(declr, reverse(happy_var_2.clone()), false, true, Some(happy_var_6.clone()), at)) }, (box move |r| { happyReturn(HappyAbsSyn88(r)) }))
}
}


fn happyReduce_322() -> ActionReturn {
    partial_5!(happyMonadReduce, 4, 90, box happyReduction_322)
}

refute! { fn happyReduction_322<T>(HappyStk(_, Some(box HappyStk(HappyAbsSyn132(happy_var_3), Some(box HappyStk(_, Some(box HappyStk(HappyTerminal(happy_var_1), Some(box happyRest)))))))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withAttributePF(happy_var_1, happy_var_3, box move |at, declr|
                           arrDeclr(declr, vec![], true, false, None, at)) }, (box move |r| { happyReturn(HappyAbsSyn88(r)) }))
}
}


fn happyReduce_323() -> ActionReturn {
    partial_5!(happyMonadReduce, 5, 90, box happyReduction_323)
}

refute! { fn happyReduction_323<T>(HappyStk(_, Some(box HappyStk(HappyAbsSyn132(happy_var_4), Some(box HappyStk(_, Some(box HappyStk(HappyAbsSyn132(happy_var_2), Some(box HappyStk(HappyTerminal(happy_var_1), Some(box happyRest)))))))))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withAttributePF(happy_var_1, __op_addadd(happy_var_2, happy_var_4), box move |at, declr|
                           arrDeclr(declr, vec![], true, false, None, at)) }, (box move |r| { happyReturn(HappyAbsSyn88(r)) }))
}
}


fn happyReduce_324() -> ActionReturn {
    partial_5!(happyMonadReduce, 5, 90, box happyReduction_324)
}

refute! { fn happyReduction_324<T>(HappyStk(_, Some(box HappyStk(HappyAbsSyn132(happy_var_4), Some(box HappyStk(_, Some(box HappyStk(HappyAbsSyn65(happy_var_2), Some(box HappyStk(HappyTerminal(happy_var_1), Some(box happyRest)))))))))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withAttributePF(happy_var_1, happy_var_4, box move |at, declr|
                           arrDeclr(declr, reverse(happy_var_2.clone()), true, false, None, at)) }, (box move |r| { happyReturn(HappyAbsSyn88(r)) }))
}
}


fn happyReduce_325() -> ActionReturn {
    partial_5!(happyMonadReduce, 6, 90, box happyReduction_325)
}

refute! { fn happyReduction_325<T>(HappyStk(_, Some(box HappyStk(HappyAbsSyn132(happy_var_5), Some(box HappyStk(_, Some(box HappyStk(HappyAbsSyn132(happy_var_3), Some(box HappyStk(HappyAbsSyn65(happy_var_2), Some(box HappyStk(HappyTerminal(happy_var_1), Some(box happyRest)))))))))))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withAttributePF(happy_var_1, __op_addadd(happy_var_3, happy_var_5), box move |at, declr|
                           arrDeclr(declr, reverse(happy_var_2.clone()), true, false, None, at)) }, (box move |r| { happyReturn(HappyAbsSyn88(r)) }))
}
}


fn happyReduce_326() -> ActionReturn {
    partial_5!(happyMonadReduce, 1, 91, box happyReduction_326)
}

refute! { fn happyReduction_326<T>(HappyStk(HappyTerminal(happy_var_1), Some(box happyRest)): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1, partial_1!(ptrDeclr, emptyDeclr(), vec![])) }, (box move |r| { happyReturn(HappyAbsSyn66(r)) }))
}
}


fn happyReduce_327() -> ActionReturn {
    partial_5!(happyMonadReduce, 3, 91, box happyReduction_327)
}

refute! { fn happyReduction_327<T>(HappyStk(HappyAbsSyn132(happy_var_3), Some(box HappyStk(HappyAbsSyn65(happy_var_2), Some(box HappyStk(HappyTerminal(happy_var_1), Some(box happyRest)))))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withAttribute(happy_var_1, happy_var_3, partial_1!(ptrDeclr, emptyDeclr(), reverse(happy_var_2))) }, (box move |r| { happyReturn(HappyAbsSyn66(r)) }))
}
}


fn happyReduce_328() -> ActionReturn {
    partial_5!(happyMonadReduce, 2, 91, box happyReduction_328)
}

refute! { fn happyReduction_328<T>(HappyStk(HappyAbsSyn66(happy_var_2), Some(box HappyStk(HappyTerminal(happy_var_1), Some(box happyRest)))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1, partial_1!(ptrDeclr, happy_var_2, vec![])) }, (box move |r| { happyReturn(HappyAbsSyn66(r)) }))
}
}


fn happyReduce_329() -> ActionReturn {
    partial_5!(happyMonadReduce, 3, 91, box happyReduction_329)
}

refute! { fn happyReduction_329<T>(HappyStk(HappyAbsSyn66(happy_var_3), Some(box HappyStk(HappyAbsSyn65(happy_var_2), Some(box HappyStk(HappyTerminal(happy_var_1), Some(box happyRest)))))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1, partial_1!(ptrDeclr, happy_var_3, reverse(happy_var_2))) }, (box move |r| { happyReturn(HappyAbsSyn66(r)) }))
}
}


fn happyReduce_330() -> ActionReturn {
    partial_5!(happyMonadReduce, 2, 91, box happyReduction_330)
}

refute! { fn happyReduction_330<T>(HappyStk(HappyAbsSyn132(happy_var_2), Some(box HappyStk(HappyTerminal(happy_var_1), Some(box happyRest)))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withAttribute(happy_var_1, happy_var_2, partial_1!(ptrDeclr, emptyDeclr(), vec![])) }, (box move |r| { happyReturn(HappyAbsSyn66(r)) }))
}
}


fn happyReduce_331() -> ActionReturn {
    partial_5!(happyMonadReduce, 3, 91, box happyReduction_331)
}

refute! { fn happyReduction_331<T>(HappyStk(HappyAbsSyn66(happy_var_3), Some(box HappyStk(HappyAbsSyn132(happy_var_2), Some(box HappyStk(HappyTerminal(happy_var_1), Some(box happyRest)))))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withAttribute(happy_var_1, happy_var_2, partial_1!(ptrDeclr, happy_var_3, vec![])) }, (box move |r| { happyReturn(HappyAbsSyn66(r)) }))
}
}


fn happyReduce_332() -> ActionReturn {
    partial_5!(happySpecReduce_3, 92, box happyReduction_332)
}

fn happyReduction_332(happy_x_3: HappyAbsSyn, happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_3, happy_x_2, happy_x_1) {
        (_, HappyAbsSyn66(happy_var_2), _) => HappyAbsSyn66(happy_var_2),
        _ => notHappyAtAll()
    }
}


fn happyReduce_333() -> ActionReturn {
    partial_5!(happySpecReduce_3, 92, box happyReduction_333)
}

fn happyReduction_333(happy_x_3: HappyAbsSyn, happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_3, happy_x_2, happy_x_1) {
        (_, HappyAbsSyn66(happy_var_2), _) => HappyAbsSyn66(happy_var_2),
        _ => notHappyAtAll()
    }
}


fn happyReduce_334() -> ActionReturn {
    partial_5!(happySpecReduce_3, 92, box happyReduction_334)
}

fn happyReduction_334(happy_x_3: HappyAbsSyn, happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_3, happy_x_2, happy_x_1) {
        (_, HappyAbsSyn88(happy_var_2), _) => HappyAbsSyn66(happy_var_2(emptyDeclr())),
        _ => notHappyAtAll()
    }
}


fn happyReduce_335() -> ActionReturn {
    partial_5!(happyReduce, 4, 92, box happyReduction_335)
}

refute! { fn happyReduction_335(HappyStk(HappyAbsSyn88(happy_var_4), Some(box HappyStk(_, Some(box HappyStk(HappyAbsSyn66(happy_var_2), Some(box HappyStk(_, Some(box happyRest)))))))): HappyStk<HappyAbsSyn>) -> HappyStk<HappyAbsSyn> {
    HappyStk(HappyAbsSyn66(happy_var_4(happy_var_2)), Some(box happyRest))
}
}


fn happyReduce_336() -> ActionReturn {
    partial_5!(happyReduce, 4, 92, box happyReduction_336)
}

refute! { fn happyReduction_336(HappyStk(_, Some(box HappyStk(HappyAbsSyn66(happy_var_3), Some(box HappyStk(HappyAbsSyn132(happy_var_2), Some(box HappyStk(_, Some(box happyRest)))))))): HappyStk<HappyAbsSyn>) -> HappyStk<HappyAbsSyn> {
    HappyStk(HappyAbsSyn66(appendDeclrAttrs(happy_var_2, happy_var_3)), Some(box happyRest))
}
}


fn happyReduce_337() -> ActionReturn {
    partial_5!(happyReduce, 4, 92, box happyReduction_337)
}

refute! { fn happyReduction_337(HappyStk(_, Some(box HappyStk(HappyAbsSyn66(happy_var_3), Some(box HappyStk(HappyAbsSyn132(happy_var_2), Some(box HappyStk(_, Some(box happyRest)))))))): HappyStk<HappyAbsSyn>) -> HappyStk<HappyAbsSyn> {
    HappyStk(HappyAbsSyn66(appendDeclrAttrs(happy_var_2, happy_var_3)), Some(box happyRest))
}
}


fn happyReduce_338() -> ActionReturn {
    partial_5!(happyReduce, 4, 92, box happyReduction_338)
}

refute! { fn happyReduction_338(HappyStk(_, Some(box HappyStk(HappyAbsSyn88(happy_var_3), Some(box HappyStk(HappyAbsSyn132(happy_var_2), Some(box HappyStk(_, Some(box happyRest)))))))): HappyStk<HappyAbsSyn>) -> HappyStk<HappyAbsSyn> {
    HappyStk(HappyAbsSyn66(appendDeclrAttrs(happy_var_2, happy_var_3(emptyDeclr()))), Some(box happyRest))
}
}


fn happyReduce_339() -> ActionReturn {
    partial_5!(happyReduce, 5, 92, box happyReduction_339)
}

refute! { fn happyReduction_339(HappyStk(HappyAbsSyn88(happy_var_5), Some(box HappyStk(_, Some(box HappyStk(HappyAbsSyn66(happy_var_3), Some(box HappyStk(HappyAbsSyn132(happy_var_2), Some(box HappyStk(_, Some(box happyRest)))))))))): HappyStk<HappyAbsSyn>) -> HappyStk<HappyAbsSyn> {
    HappyStk(HappyAbsSyn66(appendDeclrAttrs(happy_var_2, happy_var_5(happy_var_3))), Some(box happyRest))
}
}


fn happyReduce_340() -> ActionReturn {
    partial_5!(happySpecReduce_2, 92, box happyReduction_340)
}

fn happyReduction_340(happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_2, happy_x_1) {
        (HappyAbsSyn132(happy_var_2), HappyAbsSyn66(happy_var_1)) => HappyAbsSyn66(appendDeclrAttrs(happy_var_2, happy_var_1)),
        _ => notHappyAtAll()
    }
}


fn happyReduce_341() -> ActionReturn {
    partial_5!(happyMonadReduce, 1, 93, box happyReduction_341)
}

refute! { fn happyReduction_341<T>(HappyStk(HappyAbsSyn100(happy_var_1), Some(box happyRest)): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1.clone(), partial_1!(CInitExpr, happy_var_1)) }, (box move |r| { happyReturn(HappyAbsSyn93(r)) }))
}
}


fn happyReduce_342() -> ActionReturn {
    partial_5!(happyMonadReduce, 3, 93, box happyReduction_342)
}

refute! { fn happyReduction_342<T>(HappyStk(_, Some(box HappyStk(HappyAbsSyn95(happy_var_2), Some(box HappyStk(HappyTerminal(happy_var_1), Some(box happyRest)))))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1, partial_1!(CInitList, reverse(happy_var_2))) }, (box move |r| { happyReturn(HappyAbsSyn93(r)) }))
}
}


fn happyReduce_343() -> ActionReturn {
    partial_5!(happyMonadReduce, 4, 93, box happyReduction_343)
}

refute! { fn happyReduction_343<T>(HappyStk(_, Some(box HappyStk(_, Some(box HappyStk(HappyAbsSyn95(happy_var_2), Some(box HappyStk(HappyTerminal(happy_var_1), Some(box happyRest)))))))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1, partial_1!(CInitList, reverse(happy_var_2))) }, (box move |r| { happyReturn(HappyAbsSyn93(r)) }))
}
}


fn happyReduce_344() -> ActionReturn {
    partial_5!(happySpecReduce_0, 94, happyReduction_344())
}

fn happyReduction_344() -> HappyAbsSyn {
    match () {
        () => HappyAbsSyn94(None),
    }
}


fn happyReduce_345() -> ActionReturn {
    partial_5!(happySpecReduce_2, 94, box happyReduction_345)
}

fn happyReduction_345(happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_2, happy_x_1) {
        (HappyAbsSyn93(happy_var_2), _) => HappyAbsSyn94(Some(happy_var_2)),
        _ => notHappyAtAll()
    }
}


fn happyReduce_346() -> ActionReturn {
    partial_5!(happySpecReduce_0, 95, happyReduction_346())
}

fn happyReduction_346() -> HappyAbsSyn {
    match () {
        () => HappyAbsSyn95(empty()),
    }
}


fn happyReduce_347() -> ActionReturn {
    partial_5!(happySpecReduce_1, 95, box happyReduction_347)
}

fn happyReduction_347(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn93(happy_var_1) => HappyAbsSyn95(singleton((vec![], happy_var_1))),
        _ => notHappyAtAll()
    }
}


fn happyReduce_348() -> ActionReturn {
    partial_5!(happySpecReduce_2, 95, box happyReduction_348)
}

fn happyReduction_348(happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_2, happy_x_1) {
        (HappyAbsSyn93(happy_var_2), HappyAbsSyn96(happy_var_1)) => HappyAbsSyn95(singleton((happy_var_1, happy_var_2))),
        _ => notHappyAtAll()
    }
}


fn happyReduce_349() -> ActionReturn {
    partial_5!(happySpecReduce_3, 95, box happyReduction_349)
}

fn happyReduction_349(happy_x_3: HappyAbsSyn, happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_3, happy_x_2, happy_x_1) {
        (HappyAbsSyn93(happy_var_3), _, HappyAbsSyn95(happy_var_1)) => HappyAbsSyn95(snoc(happy_var_1, (vec![], happy_var_3))),
        _ => notHappyAtAll()
    }
}


fn happyReduce_350() -> ActionReturn {
    partial_5!(happyReduce, 4, 95, box happyReduction_350)
}

refute! { fn happyReduction_350(HappyStk(HappyAbsSyn93(happy_var_4), Some(box HappyStk(HappyAbsSyn96(happy_var_3), Some(box HappyStk(_, Some(box HappyStk(HappyAbsSyn95(happy_var_1), Some(box happyRest)))))))): HappyStk<HappyAbsSyn>) -> HappyStk<HappyAbsSyn> {
    HappyStk(HappyAbsSyn95(snoc(happy_var_1, (happy_var_3, happy_var_4))), Some(box happyRest))
}
}


fn happyReduce_351() -> ActionReturn {
    partial_5!(happySpecReduce_2, 96, box happyReduction_351)
}

fn happyReduction_351(happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_2, happy_x_1) {
        (_, HappyAbsSyn97(happy_var_1)) => HappyAbsSyn96(reverse(happy_var_1)),
        _ => notHappyAtAll()
    }
}


fn happyReduce_352() -> ActionReturn {
    partial_5!(happyMonadReduce, 2, 96, box happyReduction_352)
}

refute! { fn happyReduction_352<T>(HappyStk(_, Some(box HappyStk(HappyAbsSyn131(happy_var_1), Some(box happyRest)))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1.clone(), box |_0| vec![CMemberDesig(happy_var_1, _0)]) }, (box move |r| { happyReturn(HappyAbsSyn96(r)) }))
}
}


fn happyReduce_353() -> ActionReturn {
    partial_5!(happySpecReduce_1, 96, box happyReduction_353)
}

fn happyReduction_353(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn98(happy_var_1) => HappyAbsSyn96(vec![happy_var_1]),
        _ => notHappyAtAll()
    }
}


fn happyReduce_354() -> ActionReturn {
    partial_5!(happySpecReduce_1, 97, box happyReduction_354)
}

fn happyReduction_354(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn98(happy_var_1) => HappyAbsSyn97(singleton(happy_var_1)),
        _ => notHappyAtAll()
    }
}


fn happyReduce_355() -> ActionReturn {
    partial_5!(happySpecReduce_2, 97, box happyReduction_355)
}

fn happyReduction_355(happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_2, happy_x_1) {
        (HappyAbsSyn98(happy_var_2), HappyAbsSyn97(happy_var_1)) => HappyAbsSyn97(snoc(happy_var_1, happy_var_2)),
        _ => notHappyAtAll()
    }
}


fn happyReduce_356() -> ActionReturn {
    partial_5!(happyMonadReduce, 3, 98, box happyReduction_356)
}

refute! { fn happyReduction_356<T>(HappyStk(_, Some(box HappyStk(HappyAbsSyn100(happy_var_2), Some(box HappyStk(HappyTerminal(happy_var_1), Some(box happyRest)))))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1, partial_1!(CArrDesig, happy_var_2)) }, (box move |r| { happyReturn(HappyAbsSyn98(r)) }))
}
}


fn happyReduce_357() -> ActionReturn {
    partial_5!(happyMonadReduce, 2, 98, box happyReduction_357)
}

refute! { fn happyReduction_357<T>(HappyStk(HappyAbsSyn131(happy_var_2), Some(box HappyStk(HappyTerminal(happy_var_1), Some(box happyRest)))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1, partial_1!(CMemberDesig, happy_var_2)) }, (box move |r| { happyReturn(HappyAbsSyn98(r)) }))
}
}


fn happyReduce_358() -> ActionReturn {
    partial_5!(happySpecReduce_1, 98, box happyReduction_358)
}

fn happyReduction_358(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn98(happy_var_1) => HappyAbsSyn98(happy_var_1),
        _ => notHappyAtAll()
    }
}


fn happyReduce_359() -> ActionReturn {
    partial_5!(happyMonadReduce, 5, 99, box happyReduction_359)
}

refute! { fn happyReduction_359<T>(HappyStk(_, Some(box HappyStk(HappyAbsSyn100(happy_var_4), Some(box HappyStk(_, Some(box HappyStk(HappyAbsSyn100(happy_var_2), Some(box HappyStk(HappyTerminal(happy_var_1), Some(box happyRest)))))))))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1, partial_1!(CRangeDesig, happy_var_2, happy_var_4)) }, (box move |r| { happyReturn(HappyAbsSyn98(r)) }))
}
}


fn happyReduce_360() -> ActionReturn {
    partial_5!(happyMonadReduce, 1, 100, box happyReduction_360)
}

refute! { fn happyReduction_360<T>(HappyStk(HappyTerminal(CTokIdent(_, happy_var_1)), Some(box happyRest)): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1.clone(), partial_1!(CVar, happy_var_1)) }, (box move |r| { happyReturn(HappyAbsSyn100(r)) }))
}
}


fn happyReduce_361() -> ActionReturn {
    partial_5!(happySpecReduce_1, 100, box happyReduction_361)
}

fn happyReduction_361(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn127(happy_var_1) => HappyAbsSyn100(CConst(happy_var_1)),
        _ => notHappyAtAll()
    }
}


fn happyReduce_362() -> ActionReturn {
    partial_5!(happySpecReduce_1, 100, box happyReduction_362)
}

fn happyReduction_362(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn128(happy_var_1) => HappyAbsSyn100(CConst(liftStrLit(happy_var_1))),
        _ => notHappyAtAll()
    }
}


fn happyReduce_363() -> ActionReturn {
    partial_5!(happySpecReduce_3, 100, box happyReduction_363)
}

fn happyReduction_363(happy_x_3: HappyAbsSyn, happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_3, happy_x_2, happy_x_1) {
        (_, HappyAbsSyn100(happy_var_2), _) => HappyAbsSyn100(happy_var_2),
        _ => notHappyAtAll()
    }
}


fn happyReduce_364() -> ActionReturn {
    partial_5!(happyMonadReduce, 6, 100, box happyReduction_364)
}

refute! { fn happyReduction_364<T>(HappyStk(_, Some(box HappyStk(HappyAbsSyn101(happy_var_5), Some(box HappyStk(_, Some(box HappyStk(HappyAbsSyn100(happy_var_3), Some(box HappyStk(_, Some(box HappyStk(HappyTerminal(happy_var_1), Some(box happyRest)))))))))))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1, partial_1!(CGenericSelection, box happy_var_3, reverse(happy_var_5))) }, (box move |r| { happyReturn(HappyAbsSyn100(r)) }))
}
}


fn happyReduce_365() -> ActionReturn {
    partial_5!(happyMonadReduce, 3, 100, box happyReduction_365)
}

refute! { fn happyReduction_365<T>(HappyStk(_, Some(box HappyStk(HappyAbsSyn12(happy_var_2), Some(box HappyStk(HappyTerminal(happy_var_1), Some(box happyRest)))))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1, partial_1!(CStatExpr, box happy_var_2)) }, (box move |r| { happyReturn(HappyAbsSyn100(r)) }))
}
}


fn happyReduce_366() -> ActionReturn {
    partial_5!(happyMonadReduce, 6, 100, box happyReduction_366)
}

refute! { fn happyReduction_366<T>(HappyStk(_, Some(box HappyStk(HappyAbsSyn32(happy_var_5), Some(box HappyStk(_, Some(box HappyStk(HappyAbsSyn100(happy_var_3), Some(box HappyStk(_, Some(box HappyStk(HappyTerminal(happy_var_1), Some(box happyRest)))))))))))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1, box move |_0| CBuiltinExpr(box CBuiltinVaArg(happy_var_3, happy_var_5, _0))) }, (box move |r| { happyReturn(HappyAbsSyn100(r)) }))
}
}


fn happyReduce_367() -> ActionReturn {
    partial_5!(happyMonadReduce, 6, 100, box happyReduction_367)
}

refute! { fn happyReduction_367<T>(HappyStk(_, Some(box HappyStk(HappyAbsSyn97(happy_var_5), Some(box HappyStk(_, Some(box HappyStk(HappyAbsSyn32(happy_var_3), Some(box HappyStk(_, Some(box HappyStk(HappyTerminal(happy_var_1), Some(box happyRest)))))))))))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1, box move |_0| CBuiltinExpr(box CBuiltinOffsetOf(happy_var_3, reverse(happy_var_5), _0))) }, (box move |r| { happyReturn(HappyAbsSyn100(r)) }))
}
}


fn happyReduce_368() -> ActionReturn {
    partial_5!(happyMonadReduce, 6, 100, box happyReduction_368)
}

refute! { fn happyReduction_368<T>(HappyStk(_, Some(box HappyStk(HappyAbsSyn32(happy_var_5), Some(box HappyStk(_, Some(box HappyStk(HappyAbsSyn32(happy_var_3), Some(box HappyStk(_, Some(box HappyStk(HappyTerminal(happy_var_1), Some(box happyRest)))))))))))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1, box move |_0| CBuiltinExpr(box CBuiltinTypesCompatible(happy_var_3, happy_var_5, _0))) }, (box move |r| { happyReturn(HappyAbsSyn100(r)) }))
}
}


fn happyReduce_369() -> ActionReturn {
    partial_5!(happySpecReduce_3, 101, box happyReduction_369)
}

fn happyReduction_369(happy_x_3: HappyAbsSyn, happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_3, happy_x_2, happy_x_1) {
        (HappyAbsSyn102(happy_var_3), _, HappyAbsSyn101(happy_var_1)) => HappyAbsSyn101(snoc(happy_var_1, happy_var_3)),
        _ => notHappyAtAll()
    }
}


fn happyReduce_370() -> ActionReturn {
    partial_5!(happySpecReduce_1, 101, box happyReduction_370)
}

fn happyReduction_370(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn102(happy_var_1) => HappyAbsSyn101(singleton(happy_var_1)),
        _ => notHappyAtAll()
    }
}


fn happyReduce_371() -> ActionReturn {
    partial_5!(happySpecReduce_3, 102, box happyReduction_371)
}

fn happyReduction_371(happy_x_3: HappyAbsSyn, happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_3, happy_x_2, happy_x_1) {
        (HappyAbsSyn100(happy_var_3), _, HappyAbsSyn32(happy_var_1)) => HappyAbsSyn102((Some(happy_var_1), happy_var_3)),
        _ => notHappyAtAll()
    }
}


fn happyReduce_372() -> ActionReturn {
    partial_5!(happySpecReduce_3, 102, box happyReduction_372)
}

fn happyReduction_372(happy_x_3: HappyAbsSyn, happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_3, happy_x_2, happy_x_1) {
        (HappyAbsSyn100(happy_var_3), _, _) => HappyAbsSyn102((None, happy_var_3)),
        _ => notHappyAtAll()
    }
}


fn happyReduce_373() -> ActionReturn {
    partial_5!(happyMonadReduce, 1, 103, box happyReduction_373)
}

refute! { fn happyReduction_373<T>(HappyStk(HappyAbsSyn131(happy_var_1), Some(box happyRest)): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1.clone(), box move |_0| singleton(CMemberDesig(happy_var_1, _0))) }, (box move |r| { happyReturn(HappyAbsSyn97(r)) }))
}
}


fn happyReduce_374() -> ActionReturn {
    partial_5!(happyMonadReduce, 3, 103, box happyReduction_374)
}

refute! { fn happyReduction_374<T>(HappyStk(HappyAbsSyn131(happy_var_3), Some(box HappyStk(_, Some(box HappyStk(HappyAbsSyn97(happy_var_1), Some(box happyRest)))))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_3.clone(), box move |_0| snoc(happy_var_1, CMemberDesig(happy_var_3, _0))) }, (box move |r| { happyReturn(HappyAbsSyn97(r)) }))
}
}


fn happyReduce_375() -> ActionReturn {
    partial_5!(happyMonadReduce, 4, 103, box happyReduction_375)
}

refute! { fn happyReduction_375<T>(HappyStk(_, Some(box HappyStk(HappyAbsSyn100(happy_var_3), Some(box HappyStk(_, Some(box HappyStk(HappyAbsSyn97(happy_var_1), Some(box happyRest)))))))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_3.clone(), box move |_0| snoc(happy_var_1, CArrDesig(happy_var_3, _0))) }, (box move |r| { happyReturn(HappyAbsSyn97(r)) }))
}
}


fn happyReduce_376() -> ActionReturn {
    partial_5!(happySpecReduce_1, 104, box happyReduction_376)
}

fn happyReduction_376(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn100(happy_var_1) => HappyAbsSyn100(happy_var_1),
        _ => notHappyAtAll()
    }
}


fn happyReduce_377() -> ActionReturn {
    partial_5!(happyMonadReduce, 4, 104, box happyReduction_377)
}

refute! { fn happyReduction_377<T>(HappyStk(_, Some(box HappyStk(HappyAbsSyn100(happy_var_3), Some(box HappyStk(_, Some(box HappyStk(HappyAbsSyn100(happy_var_1), Some(box happyRest)))))))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1.clone(), partial_1!(CIndex, box happy_var_1, box happy_var_3)) }, (box move |r| { happyReturn(HappyAbsSyn100(r)) }))
}
}


fn happyReduce_378() -> ActionReturn {
    partial_5!(happyMonadReduce, 3, 104, box happyReduction_378)
}

refute! { fn happyReduction_378<T>(HappyStk(_, Some(box HappyStk(_, Some(box HappyStk(HappyAbsSyn100(happy_var_1), Some(box happyRest)))))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1.clone(), partial_1!(CCall, box happy_var_1, vec![])) }, (box move |r| { happyReturn(HappyAbsSyn100(r)) }))
}
}


fn happyReduce_379() -> ActionReturn {
    partial_5!(happyMonadReduce, 4, 104, box happyReduction_379)
}

refute! { fn happyReduction_379<T>(HappyStk(_, Some(box HappyStk(HappyAbsSyn105(happy_var_3), Some(box HappyStk(_, Some(box HappyStk(HappyAbsSyn100(happy_var_1), Some(box happyRest)))))))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1.clone(), partial_1!(CCall, box happy_var_1, reverse(happy_var_3))) }, (box move |r| { happyReturn(HappyAbsSyn100(r)) }))
}
}


fn happyReduce_380() -> ActionReturn {
    partial_5!(happyMonadReduce, 3, 104, box happyReduction_380)
}

refute! { fn happyReduction_380<T>(HappyStk(HappyAbsSyn131(happy_var_3), Some(box HappyStk(_, Some(box HappyStk(HappyAbsSyn100(happy_var_1), Some(box happyRest)))))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1.clone(), partial_1!(CMember, box happy_var_1, happy_var_3, false)) }, (box move |r| { happyReturn(HappyAbsSyn100(r)) }))
}
}


fn happyReduce_381() -> ActionReturn {
    partial_5!(happyMonadReduce, 3, 104, box happyReduction_381)
}

refute! { fn happyReduction_381<T>(HappyStk(HappyAbsSyn131(happy_var_3), Some(box HappyStk(_, Some(box HappyStk(HappyAbsSyn100(happy_var_1), Some(box happyRest)))))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1.clone(), partial_1!(CMember, box happy_var_1, happy_var_3, true)) }, (box move |r| { happyReturn(HappyAbsSyn100(r)) }))
}
}


fn happyReduce_382() -> ActionReturn {
    partial_5!(happyMonadReduce, 2, 104, box happyReduction_382)
}

refute! { fn happyReduction_382<T>(HappyStk(_, Some(box HappyStk(HappyAbsSyn100(happy_var_1), Some(box happyRest)))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1.clone(), partial_1!(CUnary, CPostIncOp, box happy_var_1)) }, (box move |r| { happyReturn(HappyAbsSyn100(r)) }))
}
}


fn happyReduce_383() -> ActionReturn {
    partial_5!(happyMonadReduce, 2, 104, box happyReduction_383)
}

refute! { fn happyReduction_383<T>(HappyStk(_, Some(box HappyStk(HappyAbsSyn100(happy_var_1), Some(box happyRest)))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1.clone(), partial_1!(CUnary, CPostDecOp, box happy_var_1)) }, (box move |r| { happyReturn(HappyAbsSyn100(r)) }))
}
}


fn happyReduce_384() -> ActionReturn {
    partial_5!(happyMonadReduce, 6, 104, box happyReduction_384)
}

refute! { fn happyReduction_384<T>(HappyStk(_, Some(box HappyStk(HappyAbsSyn95(happy_var_5), Some(box HappyStk(_, Some(box HappyStk(_, Some(box HappyStk(HappyAbsSyn32(happy_var_2), Some(box HappyStk(HappyTerminal(happy_var_1), Some(box happyRest)))))))))))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1, partial_1!(CCompoundLit, box happy_var_2, reverse(happy_var_5))) }, (box move |r| { happyReturn(HappyAbsSyn100(r)) }))
}
}


fn happyReduce_385() -> ActionReturn {
    partial_5!(happyMonadReduce, 7, 104, box happyReduction_385)
}

refute! { fn happyReduction_385<T>(HappyStk(_, Some(box HappyStk(_, Some(box HappyStk(HappyAbsSyn95(happy_var_5), Some(box HappyStk(_, Some(box HappyStk(_, Some(box HappyStk(HappyAbsSyn32(happy_var_2), Some(box HappyStk(HappyTerminal(happy_var_1), Some(box happyRest)))))))))))))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1, partial_1!(CCompoundLit, box happy_var_2, reverse(happy_var_5))) }, (box move |r| { happyReturn(HappyAbsSyn100(r)) }))
}
}


fn happyReduce_386() -> ActionReturn {
    partial_5!(happySpecReduce_1, 105, box happyReduction_386)
}

fn happyReduction_386(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn100(happy_var_1) => HappyAbsSyn105(singleton(happy_var_1)),
        _ => notHappyAtAll()
    }
}


fn happyReduce_387() -> ActionReturn {
    partial_5!(happySpecReduce_3, 105, box happyReduction_387)
}

fn happyReduction_387(happy_x_3: HappyAbsSyn, happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_3, happy_x_2, happy_x_1) {
        (HappyAbsSyn100(happy_var_3), _, HappyAbsSyn105(happy_var_1)) => HappyAbsSyn105(snoc(happy_var_1, happy_var_3)),
        _ => notHappyAtAll()
    }
}


fn happyReduce_388() -> ActionReturn {
    partial_5!(happySpecReduce_1, 106, box happyReduction_388)
}

fn happyReduction_388(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn100(happy_var_1) => HappyAbsSyn100(happy_var_1),
        _ => notHappyAtAll()
    }
}


fn happyReduce_389() -> ActionReturn {
    partial_5!(happyMonadReduce, 2, 106, box happyReduction_389)
}

refute! { fn happyReduction_389<T>(HappyStk(HappyAbsSyn100(happy_var_2), Some(box HappyStk(HappyTerminal(happy_var_1), Some(box happyRest)))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1, partial_1!(CUnary, CPreIncOp, box happy_var_2)) }, (box move |r| { happyReturn(HappyAbsSyn100(r)) }))
}
}


fn happyReduce_390() -> ActionReturn {
    partial_5!(happyMonadReduce, 2, 106, box happyReduction_390)
}

refute! { fn happyReduction_390<T>(HappyStk(HappyAbsSyn100(happy_var_2), Some(box HappyStk(HappyTerminal(happy_var_1), Some(box happyRest)))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1, partial_1!(CUnary, CPreDecOp, box happy_var_2)) }, (box move |r| { happyReturn(HappyAbsSyn100(r)) }))
}
}


fn happyReduce_391() -> ActionReturn {
    partial_5!(happySpecReduce_2, 106, box happyReduction_391)
}

fn happyReduction_391(happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_2, happy_x_1) {
        (HappyAbsSyn100(happy_var_2), _) => HappyAbsSyn100(happy_var_2),
        _ => notHappyAtAll()
    }
}


fn happyReduce_392() -> ActionReturn {
    partial_5!(happyMonadReduce, 2, 106, box happyReduction_392)
}

refute! { fn happyReduction_392<T>(HappyStk(HappyAbsSyn100(happy_var_2), Some(box HappyStk(HappyAbsSyn107(happy_var_1), Some(box happyRest)))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1.clone(), partial_1!(CUnary, unL(happy_var_1), box happy_var_2)) }, (box move |r| { happyReturn(HappyAbsSyn100(r)) }))
}
}


fn happyReduce_393() -> ActionReturn {
    partial_5!(happyMonadReduce, 2, 106, box happyReduction_393)
}

refute! { fn happyReduction_393<T>(HappyStk(HappyAbsSyn100(happy_var_2), Some(box HappyStk(HappyTerminal(happy_var_1), Some(box happyRest)))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1, partial_1!(CSizeofExpr, box happy_var_2)) }, (box move |r| { happyReturn(HappyAbsSyn100(r)) }))
}
}


fn happyReduce_394() -> ActionReturn {
    partial_5!(happyMonadReduce, 4, 106, box happyReduction_394)
}

refute! { fn happyReduction_394<T>(HappyStk(_, Some(box HappyStk(HappyAbsSyn32(happy_var_3), Some(box HappyStk(_, Some(box HappyStk(HappyTerminal(happy_var_1), Some(box happyRest)))))))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1, partial_1!(CSizeofType, box happy_var_3)) }, (box move |r| { happyReturn(HappyAbsSyn100(r)) }))
}
}


fn happyReduce_395() -> ActionReturn {
    partial_5!(happyMonadReduce, 2, 106, box happyReduction_395)
}

refute! { fn happyReduction_395<T>(HappyStk(HappyAbsSyn100(happy_var_2), Some(box HappyStk(HappyTerminal(happy_var_1), Some(box happyRest)))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1, partial_1!(CAlignofExpr, box happy_var_2)) }, (box move |r| { happyReturn(HappyAbsSyn100(r)) }))
}
}


fn happyReduce_396() -> ActionReturn {
    partial_5!(happyMonadReduce, 4, 106, box happyReduction_396)
}

refute! { fn happyReduction_396<T>(HappyStk(_, Some(box HappyStk(HappyAbsSyn32(happy_var_3), Some(box HappyStk(_, Some(box HappyStk(HappyTerminal(happy_var_1), Some(box happyRest)))))))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1, partial_1!(CAlignofType, box happy_var_3)) }, (box move |r| { happyReturn(HappyAbsSyn100(r)) }))
}
}


fn happyReduce_397() -> ActionReturn {
    partial_5!(happyMonadReduce, 2, 106, box happyReduction_397)
}

refute! { fn happyReduction_397<T>(HappyStk(HappyAbsSyn100(happy_var_2), Some(box HappyStk(HappyTerminal(happy_var_1), Some(box happyRest)))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1, partial_1!(CComplexReal, box happy_var_2)) }, (box move |r| { happyReturn(HappyAbsSyn100(r)) }))
}
}


fn happyReduce_398() -> ActionReturn {
    partial_5!(happyMonadReduce, 2, 106, box happyReduction_398)
}

refute! { fn happyReduction_398<T>(HappyStk(HappyAbsSyn100(happy_var_2), Some(box HappyStk(HappyTerminal(happy_var_1), Some(box happyRest)))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1, partial_1!(CComplexImag, box happy_var_2)) }, (box move |r| { happyReturn(HappyAbsSyn100(r)) }))
}
}


fn happyReduce_399() -> ActionReturn {
    partial_5!(happyMonadReduce, 2, 106, box happyReduction_399)
}

refute! { fn happyReduction_399<T>(HappyStk(HappyAbsSyn131(happy_var_2), Some(box HappyStk(HappyTerminal(happy_var_1), Some(box happyRest)))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1, partial_1!(CLabAddrExpr, happy_var_2)) }, (box move |r| { happyReturn(HappyAbsSyn100(r)) }))
}
}


fn happyReduce_400() -> ActionReturn {
    partial_5!(happySpecReduce_1, 107, box happyReduction_400)
}

fn happyReduction_400(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyTerminal(happy_var_1) => HappyAbsSyn107(Located(CAdrOp,  posOf(happy_var_1))),
        _ => notHappyAtAll()
    }
}


fn happyReduce_401() -> ActionReturn {
    partial_5!(happySpecReduce_1, 107, box happyReduction_401)
}

fn happyReduction_401(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyTerminal(happy_var_1) => HappyAbsSyn107(Located(CIndOp,  posOf(happy_var_1))),
        _ => notHappyAtAll()
    }
}


fn happyReduce_402() -> ActionReturn {
    partial_5!(happySpecReduce_1, 107, box happyReduction_402)
}

fn happyReduction_402(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyTerminal(happy_var_1) => HappyAbsSyn107(Located(CPlusOp, posOf(happy_var_1))),
        _ => notHappyAtAll()
    }
}


fn happyReduce_403() -> ActionReturn {
    partial_5!(happySpecReduce_1, 107, box happyReduction_403)
}

fn happyReduction_403(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyTerminal(happy_var_1) => HappyAbsSyn107(Located(CMinOp,  posOf(happy_var_1))),
        _ => notHappyAtAll()
    }
}


fn happyReduce_404() -> ActionReturn {
    partial_5!(happySpecReduce_1, 107, box happyReduction_404)
}

fn happyReduction_404(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyTerminal(happy_var_1) => HappyAbsSyn107(Located(CCompOp, posOf(happy_var_1))),
        _ => notHappyAtAll()
    }
}


fn happyReduce_405() -> ActionReturn {
    partial_5!(happySpecReduce_1, 107, box happyReduction_405)
}

fn happyReduction_405(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyTerminal(happy_var_1) => HappyAbsSyn107(Located(CNegOp,  posOf(happy_var_1))),
        _ => notHappyAtAll()
    }
}


fn happyReduce_406() -> ActionReturn {
    partial_5!(happySpecReduce_1, 108, box happyReduction_406)
}

fn happyReduction_406(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn100(happy_var_1) => HappyAbsSyn100(happy_var_1),
        _ => notHappyAtAll()
    }
}


fn happyReduce_407() -> ActionReturn {
    partial_5!(happyMonadReduce, 4, 108, box happyReduction_407)
}

refute! { fn happyReduction_407<T>(HappyStk(HappyAbsSyn100(happy_var_4), Some(box HappyStk(_, Some(box HappyStk(HappyAbsSyn32(happy_var_2), Some(box HappyStk(HappyTerminal(happy_var_1), Some(box happyRest)))))))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1, partial_1!(CCast, box happy_var_2, box happy_var_4)) }, (box move |r| { happyReturn(HappyAbsSyn100(r)) }))
}
}


fn happyReduce_408() -> ActionReturn {
    partial_5!(happySpecReduce_1, 109, box happyReduction_408)
}

fn happyReduction_408(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn100(happy_var_1) => HappyAbsSyn100(happy_var_1),
        _ => notHappyAtAll()
    }
}


fn happyReduce_409() -> ActionReturn {
    partial_5!(happyMonadReduce, 3, 109, box happyReduction_409)
}

refute! { fn happyReduction_409<T>(HappyStk(HappyAbsSyn100(happy_var_3), Some(box HappyStk(_, Some(box HappyStk(HappyAbsSyn100(happy_var_1), Some(box happyRest)))))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1.clone(), partial_1!(CBinary, CMulOp, box happy_var_1, box happy_var_3)) }, (box move |r| { happyReturn(HappyAbsSyn100(r)) }))
}
}


fn happyReduce_410() -> ActionReturn {
    partial_5!(happyMonadReduce, 3, 109, box happyReduction_410)
}

refute! { fn happyReduction_410<T>(HappyStk(HappyAbsSyn100(happy_var_3), Some(box HappyStk(_, Some(box HappyStk(HappyAbsSyn100(happy_var_1), Some(box happyRest)))))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1.clone(), partial_1!(CBinary, CDivOp, box happy_var_1, box happy_var_3)) }, (box move |r| { happyReturn(HappyAbsSyn100(r)) }))
}
}


fn happyReduce_411() -> ActionReturn {
    partial_5!(happyMonadReduce, 3, 109, box happyReduction_411)
}

refute! { fn happyReduction_411<T>(HappyStk(HappyAbsSyn100(happy_var_3), Some(box HappyStk(_, Some(box HappyStk(HappyAbsSyn100(happy_var_1), Some(box happyRest)))))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1.clone(), partial_1!(CBinary, CRmdOp, box happy_var_1, box happy_var_3)) }, (box move |r| { happyReturn(HappyAbsSyn100(r)) }))
}
}


fn happyReduce_412() -> ActionReturn {
    partial_5!(happySpecReduce_1, 110, box happyReduction_412)
}

fn happyReduction_412(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn100(happy_var_1) => HappyAbsSyn100(happy_var_1),
        _ => notHappyAtAll()
    }
}


fn happyReduce_413() -> ActionReturn {
    partial_5!(happyMonadReduce, 3, 110, box happyReduction_413)
}

refute! { fn happyReduction_413<T>(HappyStk(HappyAbsSyn100(happy_var_3), Some(box HappyStk(_, Some(box HappyStk(HappyAbsSyn100(happy_var_1), Some(box happyRest)))))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1.clone(), partial_1!(CBinary, CAddOp, box happy_var_1, box happy_var_3)) }, (box move |r| { happyReturn(HappyAbsSyn100(r)) }))
}
}


fn happyReduce_414() -> ActionReturn {
    partial_5!(happyMonadReduce, 3, 110, box happyReduction_414)
}

refute! { fn happyReduction_414<T>(HappyStk(HappyAbsSyn100(happy_var_3), Some(box HappyStk(_, Some(box HappyStk(HappyAbsSyn100(happy_var_1), Some(box happyRest)))))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1.clone(), partial_1!(CBinary, CSubOp, box happy_var_1, box happy_var_3)) }, (box move |r| { happyReturn(HappyAbsSyn100(r)) }))
}
}


fn happyReduce_415() -> ActionReturn {
    partial_5!(happySpecReduce_1, 111, box happyReduction_415)
}

fn happyReduction_415(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn100(happy_var_1) => HappyAbsSyn100(happy_var_1),
        _ => notHappyAtAll()
    }
}


fn happyReduce_416() -> ActionReturn {
    partial_5!(happyMonadReduce, 3, 111, box happyReduction_416)
}

refute! { fn happyReduction_416<T>(HappyStk(HappyAbsSyn100(happy_var_3), Some(box HappyStk(_, Some(box HappyStk(HappyAbsSyn100(happy_var_1), Some(box happyRest)))))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1.clone(), partial_1!(CBinary, CShlOp, box happy_var_1, box happy_var_3)) }, (box move |r| { happyReturn(HappyAbsSyn100(r)) }))
}
}


fn happyReduce_417() -> ActionReturn {
    partial_5!(happyMonadReduce, 3, 111, box happyReduction_417)
}

refute! { fn happyReduction_417<T>(HappyStk(HappyAbsSyn100(happy_var_3), Some(box HappyStk(_, Some(box HappyStk(HappyAbsSyn100(happy_var_1), Some(box happyRest)))))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1.clone(), partial_1!(CBinary, CShrOp, box happy_var_1, box happy_var_3)) }, (box move |r| { happyReturn(HappyAbsSyn100(r)) }))
}
}


fn happyReduce_418() -> ActionReturn {
    partial_5!(happySpecReduce_1, 112, box happyReduction_418)
}

fn happyReduction_418(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn100(happy_var_1) => HappyAbsSyn100(happy_var_1),
        _ => notHappyAtAll()
    }
}


fn happyReduce_419() -> ActionReturn {
    partial_5!(happyMonadReduce, 3, 112, box happyReduction_419)
}

refute! { fn happyReduction_419<T>(HappyStk(HappyAbsSyn100(happy_var_3), Some(box HappyStk(_, Some(box HappyStk(HappyAbsSyn100(happy_var_1), Some(box happyRest)))))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1.clone(), partial_1!(CBinary, CLeOp, box happy_var_1, box happy_var_3)) }, (box move |r| { happyReturn(HappyAbsSyn100(r)) }))
}
}


fn happyReduce_420() -> ActionReturn {
    partial_5!(happyMonadReduce, 3, 112, box happyReduction_420)
}

refute! { fn happyReduction_420<T>(HappyStk(HappyAbsSyn100(happy_var_3), Some(box HappyStk(_, Some(box HappyStk(HappyAbsSyn100(happy_var_1), Some(box happyRest)))))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1.clone(), partial_1!(CBinary, CGrOp, box happy_var_1, box happy_var_3)) }, (box move |r| { happyReturn(HappyAbsSyn100(r)) }))
}
}


fn happyReduce_421() -> ActionReturn {
    partial_5!(happyMonadReduce, 3, 112, box happyReduction_421)
}

refute! { fn happyReduction_421<T>(HappyStk(HappyAbsSyn100(happy_var_3), Some(box HappyStk(_, Some(box HappyStk(HappyAbsSyn100(happy_var_1), Some(box happyRest)))))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1.clone(), partial_1!(CBinary, CLeqOp, box happy_var_1, box happy_var_3)) }, (box move |r| { happyReturn(HappyAbsSyn100(r)) }))
}
}


fn happyReduce_422() -> ActionReturn {
    partial_5!(happyMonadReduce, 3, 112, box happyReduction_422)
}

refute! { fn happyReduction_422<T>(HappyStk(HappyAbsSyn100(happy_var_3), Some(box HappyStk(_, Some(box HappyStk(HappyAbsSyn100(happy_var_1), Some(box happyRest)))))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1.clone(), partial_1!(CBinary, CGeqOp, box happy_var_1, box happy_var_3)) }, (box move |r| { happyReturn(HappyAbsSyn100(r)) }))
}
}


fn happyReduce_423() -> ActionReturn {
    partial_5!(happySpecReduce_1, 113, box happyReduction_423)
}

fn happyReduction_423(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn100(happy_var_1) => HappyAbsSyn100(happy_var_1),
        _ => notHappyAtAll()
    }
}


fn happyReduce_424() -> ActionReturn {
    partial_5!(happyMonadReduce, 3, 113, box happyReduction_424)
}

refute! { fn happyReduction_424<T>(HappyStk(HappyAbsSyn100(happy_var_3), Some(box HappyStk(_, Some(box HappyStk(HappyAbsSyn100(happy_var_1), Some(box happyRest)))))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1.clone(), partial_1!(CBinary, CEqOp, box happy_var_1, box happy_var_3)) }, (box move |r| { happyReturn(HappyAbsSyn100(r)) }))
}
}


fn happyReduce_425() -> ActionReturn {
    partial_5!(happyMonadReduce, 3, 113, box happyReduction_425)
}

refute! { fn happyReduction_425<T>(HappyStk(HappyAbsSyn100(happy_var_3), Some(box HappyStk(_, Some(box HappyStk(HappyAbsSyn100(happy_var_1), Some(box happyRest)))))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1.clone(), partial_1!(CBinary, CNeqOp, box happy_var_1, box happy_var_3)) }, (box move |r| { happyReturn(HappyAbsSyn100(r)) }))
}
}


fn happyReduce_426() -> ActionReturn {
    partial_5!(happySpecReduce_1, 114, box happyReduction_426)
}

fn happyReduction_426(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn100(happy_var_1) => HappyAbsSyn100(happy_var_1),
        _ => notHappyAtAll()
    }
}


fn happyReduce_427() -> ActionReturn {
    partial_5!(happyMonadReduce, 3, 114, box happyReduction_427)
}

refute! { fn happyReduction_427<T>(HappyStk(HappyAbsSyn100(happy_var_3), Some(box HappyStk(_, Some(box HappyStk(HappyAbsSyn100(happy_var_1), Some(box happyRest)))))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1.clone(), partial_1!(CBinary, CAndOp, box happy_var_1, box happy_var_3)) }, (box move |r| { happyReturn(HappyAbsSyn100(r)) }))
}
}


fn happyReduce_428() -> ActionReturn {
    partial_5!(happySpecReduce_1, 115, box happyReduction_428)
}

fn happyReduction_428(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn100(happy_var_1) => HappyAbsSyn100(happy_var_1),
        _ => notHappyAtAll()
    }
}


fn happyReduce_429() -> ActionReturn {
    partial_5!(happyMonadReduce, 3, 115, box happyReduction_429)
}

refute! { fn happyReduction_429<T>(HappyStk(HappyAbsSyn100(happy_var_3), Some(box HappyStk(_, Some(box HappyStk(HappyAbsSyn100(happy_var_1), Some(box happyRest)))))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1.clone(), partial_1!(CBinary, CXorOp, box happy_var_1, box happy_var_3)) }, (box move |r| { happyReturn(HappyAbsSyn100(r)) }))
}
}


fn happyReduce_430() -> ActionReturn {
    partial_5!(happySpecReduce_1, 116, box happyReduction_430)
}

fn happyReduction_430(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn100(happy_var_1) => HappyAbsSyn100(happy_var_1),
        _ => notHappyAtAll()
    }
}


fn happyReduce_431() -> ActionReturn {
    partial_5!(happyMonadReduce, 3, 116, box happyReduction_431)
}

refute! { fn happyReduction_431<T>(HappyStk(HappyAbsSyn100(happy_var_3), Some(box HappyStk(_, Some(box HappyStk(HappyAbsSyn100(happy_var_1), Some(box happyRest)))))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1.clone(), partial_1!(CBinary, COrOp, box happy_var_1, box happy_var_3)) }, (box move |r| { happyReturn(HappyAbsSyn100(r)) }))
}
}


fn happyReduce_432() -> ActionReturn {
    partial_5!(happySpecReduce_1, 117, box happyReduction_432)
}

fn happyReduction_432(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn100(happy_var_1) => HappyAbsSyn100(happy_var_1),
        _ => notHappyAtAll()
    }
}


fn happyReduce_433() -> ActionReturn {
    partial_5!(happyMonadReduce, 3, 117, box happyReduction_433)
}

refute! { fn happyReduction_433<T>(HappyStk(HappyAbsSyn100(happy_var_3), Some(box HappyStk(_, Some(box HappyStk(HappyAbsSyn100(happy_var_1), Some(box happyRest)))))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1.clone(), partial_1!(CBinary, CLndOp, box happy_var_1, box happy_var_3)) }, (box move |r| { happyReturn(HappyAbsSyn100(r)) }))
}
}


fn happyReduce_434() -> ActionReturn {
    partial_5!(happySpecReduce_1, 118, box happyReduction_434)
}

fn happyReduction_434(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn100(happy_var_1) => HappyAbsSyn100(happy_var_1),
        _ => notHappyAtAll()
    }
}


fn happyReduce_435() -> ActionReturn {
    partial_5!(happyMonadReduce, 3, 118, box happyReduction_435)
}

refute! { fn happyReduction_435<T>(HappyStk(HappyAbsSyn100(happy_var_3), Some(box HappyStk(_, Some(box HappyStk(HappyAbsSyn100(happy_var_1), Some(box happyRest)))))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1.clone(), partial_1!(CBinary, CLorOp, box happy_var_1, box happy_var_3)) }, (box move |r| { happyReturn(HappyAbsSyn100(r)) }))
}
}


fn happyReduce_436() -> ActionReturn {
    partial_5!(happySpecReduce_1, 119, box happyReduction_436)
}

fn happyReduction_436(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn100(happy_var_1) => HappyAbsSyn100(happy_var_1),
        _ => notHappyAtAll()
    }
}


fn happyReduce_437() -> ActionReturn {
    partial_5!(happyMonadReduce, 5, 119, box happyReduction_437)
}

refute! { fn happyReduction_437<T>(HappyStk(HappyAbsSyn100(happy_var_5), Some(box HappyStk(_, Some(box HappyStk(HappyAbsSyn100(happy_var_3), Some(box HappyStk(_, Some(box HappyStk(HappyAbsSyn100(happy_var_1), Some(box happyRest)))))))))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1.clone(), partial_1!(CCond, box happy_var_1, Some(box happy_var_3), box happy_var_5)) }, (box move |r| { happyReturn(HappyAbsSyn100(r)) }))
}
}


fn happyReduce_438() -> ActionReturn {
    partial_5!(happyMonadReduce, 4, 119, box happyReduction_438)
}

refute! { fn happyReduction_438<T>(HappyStk(HappyAbsSyn100(happy_var_4), Some(box HappyStk(_, Some(box HappyStk(_, Some(box HappyStk(HappyAbsSyn100(happy_var_1), Some(box happyRest)))))))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1.clone(), partial_1!(CCond, box happy_var_1, None, box happy_var_4)) }, (box move |r| { happyReturn(HappyAbsSyn100(r)) }))
}
}


fn happyReduce_439() -> ActionReturn {
    partial_5!(happySpecReduce_1, 120, box happyReduction_439)
}

fn happyReduction_439(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn100(happy_var_1) => HappyAbsSyn100(happy_var_1),
        _ => notHappyAtAll()
    }
}


fn happyReduce_440() -> ActionReturn {
    partial_5!(happyMonadReduce, 3, 120, box happyReduction_440)
}

refute! { fn happyReduction_440<T>(HappyStk(HappyAbsSyn100(happy_var_3), Some(box HappyStk(HappyAbsSyn121(happy_var_2), Some(box HappyStk(HappyAbsSyn100(happy_var_1), Some(box happyRest)))))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1.clone(), partial_1!(CAssign, unL(happy_var_2), box happy_var_1, box happy_var_3)) }, (box move |r| { happyReturn(HappyAbsSyn100(r)) }))
}
}


fn happyReduce_441() -> ActionReturn {
    partial_5!(happySpecReduce_1, 121, box happyReduction_441)
}

fn happyReduction_441(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyTerminal(happy_var_1) => HappyAbsSyn121(Located(CAssignOp, posOf(happy_var_1))),
        _ => notHappyAtAll()
    }
}


fn happyReduce_442() -> ActionReturn {
    partial_5!(happySpecReduce_1, 121, box happyReduction_442)
}

fn happyReduction_442(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyTerminal(happy_var_1) => HappyAbsSyn121(Located(CMulAssOp, posOf(happy_var_1))),
        _ => notHappyAtAll()
    }
}


fn happyReduce_443() -> ActionReturn {
    partial_5!(happySpecReduce_1, 121, box happyReduction_443)
}

fn happyReduction_443(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyTerminal(happy_var_1) => HappyAbsSyn121(Located(CDivAssOp, posOf(happy_var_1))),
        _ => notHappyAtAll()
    }
}


fn happyReduce_444() -> ActionReturn {
    partial_5!(happySpecReduce_1, 121, box happyReduction_444)
}

fn happyReduction_444(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyTerminal(happy_var_1) => HappyAbsSyn121(Located(CRmdAssOp, posOf(happy_var_1))),
        _ => notHappyAtAll()
    }
}


fn happyReduce_445() -> ActionReturn {
    partial_5!(happySpecReduce_1, 121, box happyReduction_445)
}

fn happyReduction_445(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyTerminal(happy_var_1) => HappyAbsSyn121(Located(CAddAssOp, posOf(happy_var_1))),
        _ => notHappyAtAll()
    }
}


fn happyReduce_446() -> ActionReturn {
    partial_5!(happySpecReduce_1, 121, box happyReduction_446)
}

fn happyReduction_446(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyTerminal(happy_var_1) => HappyAbsSyn121(Located(CSubAssOp, posOf(happy_var_1))),
        _ => notHappyAtAll()
    }
}


fn happyReduce_447() -> ActionReturn {
    partial_5!(happySpecReduce_1, 121, box happyReduction_447)
}

fn happyReduction_447(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyTerminal(happy_var_1) => HappyAbsSyn121(Located(CShlAssOp, posOf(happy_var_1))),
        _ => notHappyAtAll()
    }
}


fn happyReduce_448() -> ActionReturn {
    partial_5!(happySpecReduce_1, 121, box happyReduction_448)
}

fn happyReduction_448(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyTerminal(happy_var_1) => HappyAbsSyn121(Located(CShrAssOp, posOf(happy_var_1))),
        _ => notHappyAtAll()
    }
}


fn happyReduce_449() -> ActionReturn {
    partial_5!(happySpecReduce_1, 121, box happyReduction_449)
}

fn happyReduction_449(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyTerminal(happy_var_1) => HappyAbsSyn121(Located(CAndAssOp, posOf(happy_var_1))),
        _ => notHappyAtAll()
    }
}


fn happyReduce_450() -> ActionReturn {
    partial_5!(happySpecReduce_1, 121, box happyReduction_450)
}

fn happyReduction_450(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyTerminal(happy_var_1) => HappyAbsSyn121(Located(CXorAssOp, posOf(happy_var_1))),
        _ => notHappyAtAll()
    }
}


fn happyReduce_451() -> ActionReturn {
    partial_5!(happySpecReduce_1, 121, box happyReduction_451)
}

fn happyReduction_451(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyTerminal(happy_var_1) => HappyAbsSyn121(Located(COrAssOp,  posOf(happy_var_1))),
        _ => notHappyAtAll()
    }
}


fn happyReduce_452() -> ActionReturn {
    partial_5!(happySpecReduce_1, 122, box happyReduction_452)
}

fn happyReduction_452(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn100(happy_var_1) => HappyAbsSyn100(happy_var_1),
        _ => notHappyAtAll()
    }
}


fn happyReduce_453() -> ActionReturn {
    partial_5!(happyMonadReduce, 3, 122, box happyReduction_453)
}

refute! { fn happyReduction_453<T>(HappyStk(HappyAbsSyn105(happy_var_3), Some(box HappyStk(_, Some(box HappyStk(HappyAbsSyn100(happy_var_1), Some(box happyRest)))))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({
            let es = reverse(happy_var_3);
            withNodeInfo(es.clone(), partial_1!(CComma, __op_concat(happy_var_1, es))) }, (box move |r| { happyReturn(HappyAbsSyn100(r)) }))
}
}


fn happyReduce_454() -> ActionReturn {
    partial_5!(happySpecReduce_1, 123, box happyReduction_454)
}

fn happyReduction_454(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn100(happy_var_1) => HappyAbsSyn105(singleton(happy_var_1)),
        _ => notHappyAtAll()
    }
}


fn happyReduce_455() -> ActionReturn {
    partial_5!(happySpecReduce_3, 123, box happyReduction_455)
}

fn happyReduction_455(happy_x_3: HappyAbsSyn, happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_3, happy_x_2, happy_x_1) {
        (HappyAbsSyn100(happy_var_3), _, HappyAbsSyn105(happy_var_1)) => HappyAbsSyn105(snoc(happy_var_1, happy_var_3)),
        _ => notHappyAtAll()
    }
}


fn happyReduce_456() -> ActionReturn {
    partial_5!(happySpecReduce_0, 124, happyReduction_456())
}

fn happyReduction_456() -> HappyAbsSyn {
    match () {
        () => HappyAbsSyn124(None),
    }
}


fn happyReduce_457() -> ActionReturn {
    partial_5!(happySpecReduce_1, 124, box happyReduction_457)
}

fn happyReduction_457(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn100(happy_var_1) => HappyAbsSyn124(Some(happy_var_1)),
        _ => notHappyAtAll()
    }
}


fn happyReduce_458() -> ActionReturn {
    partial_5!(happySpecReduce_0, 125, happyReduction_458())
}

fn happyReduction_458() -> HappyAbsSyn {
    match () {
        () => HappyAbsSyn124(None),
    }
}


fn happyReduce_459() -> ActionReturn {
    partial_5!(happySpecReduce_1, 125, box happyReduction_459)
}

fn happyReduction_459(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn100(happy_var_1) => HappyAbsSyn124(Some(happy_var_1)),
        _ => notHappyAtAll()
    }
}


fn happyReduce_460() -> ActionReturn {
    partial_5!(happySpecReduce_1, 126, box happyReduction_460)
}

fn happyReduction_460(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn100(happy_var_1) => HappyAbsSyn100(happy_var_1),
        _ => notHappyAtAll()
    }
}


fn happyReduce_461() -> ActionReturn {
    partial_5!(happyMonadReduce, 1, 127, box happyReduction_461)
}

refute! { fn happyReduction_461<T>(HappyStk(HappyTerminal(happy_var_1), Some(box happyRest)): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({
                    withNodeInfo(happy_var_1.clone(), box move |_0| {
                        if let CTokILit(_, i) = happy_var_1 {
                            CIntConst(i, _0)
                        } else {
                            panic!("irrefutable pattern")
                        }
                    }) }, (box move |r| { happyReturn(HappyAbsSyn127(r)) }))
}
}


fn happyReduce_462() -> ActionReturn {
    partial_5!(happyMonadReduce, 1, 127, box happyReduction_462)
}

refute! { fn happyReduction_462<T>(HappyStk(HappyTerminal(happy_var_1), Some(box happyRest)): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({
                    withNodeInfo(happy_var_1.clone(), box move |_0| {
                        if let CTokCLit(_, c) = happy_var_1 {
                            CCharConst(c, _0)
                        } else {
                            panic!("irrefutable pattern")
                        }
                    }) }, (box move |r| { happyReturn(HappyAbsSyn127(r)) }))
}
}


fn happyReduce_463() -> ActionReturn {
    partial_5!(happyMonadReduce, 1, 127, box happyReduction_463)
}

refute! { fn happyReduction_463<T>(HappyStk(HappyTerminal(happy_var_1), Some(box happyRest)): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({
                    withNodeInfo(happy_var_1.clone(), box move |_0| {
                        if let CTokFLit(_, f) = happy_var_1 {
                            CFloatConst(f, _0)
                        } else {
                            panic!("irrefutable pattern")
                        }
                    }) }, (box move |r| { happyReturn(HappyAbsSyn127(r)) }))
}
}


fn happyReduce_464() -> ActionReturn {
    partial_5!(happyMonadReduce, 1, 128, box happyReduction_464)
}

refute! { fn happyReduction_464<T>(HappyStk(HappyTerminal(happy_var_1), Some(box happyRest)): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({
            withNodeInfo(happy_var_1.clone(), box move |_0| {
                if let CTokSLit(_, s) = happy_var_1 {
                    CStringLiteral(s, _0)
                } else {
                    panic!("irrefutable pattern")
                }
            }) }, (box move |r| { happyReturn(HappyAbsSyn128(r)) }))
}
}


fn happyReduce_465() -> ActionReturn {
    partial_5!(happyMonadReduce, 2, 128, box happyReduction_465)
}

refute! { fn happyReduction_465<T>(HappyStk(HappyAbsSyn129(happy_var_2), Some(box HappyStk(HappyTerminal(happy_var_1), Some(box happyRest)))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({
            withNodeInfo(happy_var_1.clone(), box move |_0| {
                if let CTokSLit(_, s) = happy_var_1 {
                    CStringLiteral(concatCStrings(__op_concat(s, reverse(happy_var_2))), _0)
                } else {
                    panic!("irrefutable pattern")
                }
            }) }, (box move |r| { happyReturn(HappyAbsSyn128(r)) }))
}
}


fn happyReduce_466() -> ActionReturn {
    partial_5!(happySpecReduce_1, 129, box happyReduction_466)
}

fn happyReduction_466(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyTerminal(happy_var_1) => HappyAbsSyn129(if let CTokSLit(_, s) = happy_var_1 {
                                        singleton(s)
                                    } else {
                                        panic!("irrefutable pattern")
                                    }),
        _ => notHappyAtAll()
    }
}


fn happyReduce_467() -> ActionReturn {
    partial_5!(happySpecReduce_2, 129, box happyReduction_467)
}

fn happyReduction_467(happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_2, happy_x_1) {
        (HappyTerminal(happy_var_2), HappyAbsSyn129(happy_var_1)) => HappyAbsSyn129(if let CTokSLit(_, s) = happy_var_2 {
                                        snoc(happy_var_1, s)
                                    } else {
                                        panic!("irrefutable pattern")
                                    }),
        _ => notHappyAtAll()
    }
}


fn happyReduce_468() -> ActionReturn {
    partial_5!(happySpecReduce_1, 130, box happyReduction_468)
}

fn happyReduction_468(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyTerminal(CTokClangC(_, ClangCTok(happy_var_1))) => HappyAbsSyn130(happy_var_1),
        _ => notHappyAtAll()
    }
}


fn happyReduce_469() -> ActionReturn {
    partial_5!(happySpecReduce_1, 131, box happyReduction_469)
}

fn happyReduction_469(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyTerminal(CTokIdent(_, happy_var_1)) => HappyAbsSyn131(happy_var_1),
        _ => notHappyAtAll()
    }
}


fn happyReduce_470() -> ActionReturn {
    partial_5!(happySpecReduce_1, 131, box happyReduction_470)
}

fn happyReduction_470(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyTerminal(CTokTyIdent(_, happy_var_1)) => HappyAbsSyn131(happy_var_1),
        _ => notHappyAtAll()
    }
}


fn happyReduce_471() -> ActionReturn {
    partial_5!(happySpecReduce_0, 132, happyReduction_471())
}

fn happyReduction_471() -> HappyAbsSyn {
    match () {
        () => HappyAbsSyn132(vec![]),
    }
}


fn happyReduce_472() -> ActionReturn {
    partial_5!(happySpecReduce_1, 132, box happyReduction_472)
}

fn happyReduction_472(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn132(happy_var_1) => HappyAbsSyn132(happy_var_1),
        _ => notHappyAtAll()
    }
}


fn happyReduce_473() -> ActionReturn {
    partial_5!(happySpecReduce_1, 133, box happyReduction_473)
}

fn happyReduction_473(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn132(happy_var_1) => HappyAbsSyn132(happy_var_1),
        _ => notHappyAtAll()
    }
}


fn happyReduce_474() -> ActionReturn {
    partial_5!(happySpecReduce_2, 133, box happyReduction_474)
}

fn happyReduction_474(happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_2, happy_x_1) {
        (HappyAbsSyn132(happy_var_2), HappyAbsSyn132(happy_var_1)) => HappyAbsSyn132(__op_addadd(happy_var_1, happy_var_2)),
        _ => notHappyAtAll()
    }
}


fn happyReduce_475() -> ActionReturn {
    partial_5!(happyReduce, 6, 134, box happyReduction_475)
}

refute! { fn happyReduction_475(HappyStk(_, Some(box HappyStk(_, Some(box HappyStk(HappyAbsSyn135(happy_var_4), Some(box HappyStk(_, Some(box HappyStk(_, Some(box HappyStk(_, Some(box happyRest)))))))))))): HappyStk<HappyAbsSyn>) -> HappyStk<HappyAbsSyn> {
    HappyStk(HappyAbsSyn132(reverse(happy_var_4)), Some(box happyRest))
}
}


fn happyReduce_476() -> ActionReturn {
    partial_5!(happySpecReduce_1, 135, box happyReduction_476)
}

fn happyReduction_476(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn136(happy_var_1) => HappyAbsSyn135(match happy_var_1 {
                                           None => empty(),
                                           Some(attr) => singleton(attr),
                                       }),
        _ => notHappyAtAll()
    }
}


fn happyReduce_477() -> ActionReturn {
    partial_5!(happySpecReduce_3, 135, box happyReduction_477)
}

fn happyReduction_477(happy_x_3: HappyAbsSyn, happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_3, happy_x_2, happy_x_1) {
        (HappyAbsSyn136(happy_var_3), _, HappyAbsSyn135(happy_var_1)) => HappyAbsSyn135(match happy_var_3 {
                                           Some(inner) => snoc(happy_var_1, inner),
                                           None => happy_var_1,
                                       }),
        _ => notHappyAtAll()
    }
}


fn happyReduce_478() -> ActionReturn {
    partial_5!(happySpecReduce_0, 136, happyReduction_478())
}

fn happyReduction_478() -> HappyAbsSyn {
    match () {
        () => HappyAbsSyn136(None),
    }
}


fn happyReduce_479() -> ActionReturn {
    partial_5!(happyMonadReduce, 1, 136, box happyReduction_479)
}

refute! { fn happyReduction_479<T>(HappyStk(HappyTerminal(CTokIdent(_, happy_var_1)), Some(box happyRest)): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1.clone(), box move |_0| Some(CAttribute(happy_var_1, vec![], _0))) }, (box move |r| { happyReturn(HappyAbsSyn136(r)) }))
}
}


fn happyReduce_480() -> ActionReturn {
    partial_5!(happyMonadReduce, 1, 136, box happyReduction_480)
}

refute! { fn happyReduction_480<T>(HappyStk(HappyTerminal(happy_var_1), Some(box happyRest)): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1, box move |_0| Some(CAttribute(internalIdent("const".to_string()), vec![], _0))) }, (box move |r| { happyReturn(HappyAbsSyn136(r)) }))
}
}


fn happyReduce_481() -> ActionReturn {
    partial_5!(happyMonadReduce, 4, 136, box happyReduction_481)
}

refute! { fn happyReduction_481<T>(HappyStk(_, Some(box HappyStk(HappyAbsSyn105(happy_var_3), Some(box HappyStk(_, Some(box HappyStk(HappyTerminal(CTokIdent(_, happy_var_1)), Some(box happyRest)))))))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1.clone(), box move |_0| Some(CAttribute(happy_var_1, reverse(happy_var_3), _0))) }, (box move |r| { happyReturn(HappyAbsSyn136(r)) }))
}
}


fn happyReduce_482() -> ActionReturn {
    partial_5!(happyMonadReduce, 3, 136, box happyReduction_482)
}

refute! { fn happyReduction_482<T>(HappyStk(_, Some(box HappyStk(_, Some(box HappyStk(HappyTerminal(CTokIdent(_, happy_var_1)), Some(box happyRest)))))): HappyStk<HappyAbsSyn>, tk: T) -> P<HappyAbsSyn> {
    happyThen({ withNodeInfo(happy_var_1.clone(), box move |_0| Some(CAttribute(happy_var_1, vec![], _0))) }, (box move |r| { happyReturn(HappyAbsSyn136(r)) }))
}
}


fn happyReduce_483() -> ActionReturn {
    partial_5!(happySpecReduce_1, 137, box happyReduction_483)
}

fn happyReduction_483(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn100(happy_var_1) => HappyAbsSyn105(singleton(happy_var_1)),
        _ => notHappyAtAll()
    }
}


fn happyReduce_484() -> ActionReturn {
    partial_5!(happySpecReduce_3, 137, box happyReduction_484)
}

fn happyReduction_484(happy_x_3: HappyAbsSyn, happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_3, happy_x_2, happy_x_1) {
        (_, _, _) => HappyAbsSyn105(Reversed(vec![])),
        _ => notHappyAtAll()
    }
}


fn happyReduce_485() -> ActionReturn {
    partial_5!(happySpecReduce_3, 137, box happyReduction_485)
}

fn happyReduction_485(happy_x_3: HappyAbsSyn, happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_3, happy_x_2, happy_x_1) {
        (_, _, _) => HappyAbsSyn105(Reversed(vec![])),
        _ => notHappyAtAll()
    }
}


fn happyReduce_486() -> ActionReturn {
    partial_5!(happySpecReduce_3, 137, box happyReduction_486)
}

fn happyReduction_486(happy_x_3: HappyAbsSyn, happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_3, happy_x_2, happy_x_1) {
        (HappyAbsSyn100(happy_var_3), _, HappyAbsSyn105(happy_var_1)) => HappyAbsSyn105(snoc(happy_var_1, happy_var_3)),
        _ => notHappyAtAll()
    }
}


fn happyReduce_487() -> ActionReturn {
    partial_5!(happyReduce, 5, 137, box happyReduction_487)
}

refute! { fn happyReduction_487(HappyStk(_, Some(box HappyStk(_, Some(box HappyStk(_, Some(box HappyStk(_, Some(box HappyStk(HappyAbsSyn105(happy_var_1), Some(box happyRest)))))))))): HappyStk<HappyAbsSyn>) -> HappyStk<HappyAbsSyn> {
    HappyStk(HappyAbsSyn105(happy_var_1), Some(box happyRest))
}
}


fn happyReduce_488() -> ActionReturn {
    partial_5!(happyReduce, 5, 137, box happyReduction_488)
}

refute! { fn happyReduction_488(HappyStk(_, Some(box HappyStk(_, Some(box HappyStk(_, Some(box HappyStk(_, Some(box HappyStk(HappyAbsSyn105(happy_var_1), Some(box happyRest)))))))))): HappyStk<HappyAbsSyn>) -> HappyStk<HappyAbsSyn> {
    HappyStk(HappyAbsSyn105(happy_var_1), Some(box happyRest))
}
}


fn happyNewToken<T: 'static, S: 'static + Clone>(action: Action<T, S>, sts: Vec<HappyState<(CToken), Box<FnBox(S) -> P<T>>>>, stk: S) -> P<T> {
    let action = Rc::new(action);
    lexC(box move |tk| {
        let tk_ = tk.clone();
        let sts_ = sts.clone();
        let stk_ = stk.clone();
        let action_ = action.clone();
        let cont = move |i| {
            let action__ = action_.clone();
            action_(i, i, tk_, HappyState(Rc::new(apply_5_1_clone!(action__))), sts_, stk_)
        };
        match tk {
            CTokEof => cont(247),
            CTokLParen(_) => cont(138),
            CTokRParen(_) => cont(139),
            CTokLBracket(_) => cont(140),
            CTokRBracket(_) => cont(141),
            CTokArrow(_) => cont(142),
            CTokDot(_) => cont(143),
            CTokExclam(_) => cont(144),
            CTokTilde(_) => cont(145),
            CTokInc(_) => cont(146),
            CTokDec(_) => cont(147),
            CTokPlus(_) => cont(148),
            CTokMinus(_) => cont(149),
            CTokStar(_) => cont(150),
            CTokSlash(_) => cont(151),
            CTokPercent(_) => cont(152),
            CTokAmper(_) => cont(153),
            CTokShiftL(_) => cont(154),
            CTokShiftR(_) => cont(155),
            CTokLess(_) => cont(156),
            CTokLessEq(_) => cont(157),
            CTokHigh(_) => cont(158),
            CTokHighEq(_) => cont(159),
            CTokEqual(_) => cont(160),
            CTokUnequal(_) => cont(161),
            CTokHat(_) => cont(162),
            CTokBar(_) => cont(163),
            CTokAnd(_) => cont(164),
            CTokOr(_) => cont(165),
            CTokQuest(_) => cont(166),
            CTokColon(_) => cont(167),
            CTokAssign(_) => cont(168),
            CTokPlusAss(_) => cont(169),
            CTokMinusAss(_) => cont(170),
            CTokStarAss(_) => cont(171),
            CTokSlashAss(_) => cont(172),
            CTokPercAss(_) => cont(173),
            CTokAmpAss(_) => cont(174),
            CTokHatAss(_) => cont(175),
            CTokBarAss(_) => cont(176),
            CTokSLAss(_) => cont(177),
            CTokSRAss(_) => cont(178),
            CTokComma(_) => cont(179),
            CTokSemic(_) => cont(180),
            CTokLBrace(_) => cont(181),
            CTokRBrace(_) => cont(182),
            CTokEllipsis(_) => cont(183),
            CTokAlignof(_) => cont(184),
            CTokAlignas(_) => cont(185),
            CTokAtomic(_) => cont(186),
            CTokAsm(_) => cont(187),
            CTokAuto(_) => cont(188),
            CTokBreak(_) => cont(189),
            CTokBool(_) => cont(190),
            CTokCase(_) => cont(191),
            CTokChar(_) => cont(192),
            CTokConst(_) => cont(193),
            CTokContinue(_) => cont(194),
            CTokComplex(_) => cont(195),
            CTokDefault(_) => cont(196),
            CTokDo(_) => cont(197),
            CTokDouble(_) => cont(198),
            CTokElse(_) => cont(199),
            CTokEnum(_) => cont(200),
            CTokExtern(_) => cont(201),
            CTokFloat(_) => cont(202),
            CTokFor(_) => cont(203),
            CTokGeneric(_) => cont(204),
            CTokGoto(_) => cont(205),
            CTokIf(_) => cont(206),
            CTokInline(_) => cont(207),
            CTokInt(_) => cont(208),
            CTokInt128(_) => cont(209),
            CTokLong(_) => cont(210),
            CTokLabel(_) => cont(211),
            CTokNoreturn(_) => cont(212),
            CTokNullable(_) => cont(213),
            CTokNonnull(_) => cont(214),
            CTokRegister(_) => cont(215),
            CTokRestrict(_) => cont(216),
            CTokReturn(_) => cont(217),
            CTokShort(_) => cont(218),
            CTokSigned(_) => cont(219),
            CTokSizeof(_) => cont(220),
            CTokStatic(_) => cont(221),
            CTokStaticAssert(_) => cont(222),
            CTokStruct(_) => cont(223),
            CTokSwitch(_) => cont(224),
            CTokTypedef(_) => cont(225),
            CTokTypeof(_) => cont(226),
            CTokThread(_) => cont(227),
            CTokUnion(_) => cont(228),
            CTokUnsigned(_) => cont(229),
            CTokVoid(_) => cont(230),
            CTokVolatile(_) => cont(231),
            CTokWhile(_) => cont(232),
            CTokCLit(_, _) => cont(233),
            CTokILit(_, _) => cont(234),
            CTokFLit(_, _) => cont(235),
            CTokSLit(_, _) => cont(236),
            CTokIdent(_, happy_dollar_dollar) => cont(237),
            CTokTyIdent(_, happy_dollar_dollar) => cont(238),
            CTokGnuC(GnuCAttrTok, _) => cont(239),
            CTokGnuC(GnuCExtTok,  _) => cont(240),
            CTokGnuC(GnuCComplexReal, _) => cont(241),
            CTokGnuC(GnuCComplexImag, _) => cont(242),
            CTokGnuC(GnuCVaArg, _) => cont(243),
            CTokGnuC(GnuCOffsetof, _) => cont(244),
            CTokGnuC(GnuCTyCompat, _) => cont(245),
            CTokClangC(_, ClangCTok(happy_dollar_dollar)) => cont(246),
        }
    })
}

fn happyError_<T: 'static>(_: isize, tk: (CToken)) -> P<T> {
    happyError_q(tk)
}


fn happyThen<A: 'static, B: 'static>(m: P<A>, f: Box<FnBox(A) -> P<B>>) -> P<B> {
    thenP(m, f)
}
fn happyReturn<A: 'static + Clone>(v: A) -> P<A> {
    __return(v)
}
fn happyThen1<A: 'static, B: 'static>(m: P<A>, f: Box<FnBox(A) -> P<B>>) -> P<B> {
    thenP(m, f)
}
fn happyReturn1<A: 'static + Clone>(v: A) -> P<A> {
    __return(v)
}
fn happyError_q<A: 'static>(tk: (CToken)) -> P<A> {
    // TODO
    happyError()
}

fn translation_unit() -> P<CTranslUnit> {
    happyThen(happyParse(curry_1_5!(action_0)), box |x| match x {
        HappyAbsSyn7(z) => happyReturn(z),
        _ => notHappyAtAll()
    })
}

fn external_declaration() -> P<CExtDecl> {
    happyThen(happyParse(curry_1_5!(action_1)), box |x| match x {
        HappyAbsSyn9(z) => happyReturn(z),
        _ => notHappyAtAll()
    })
}

fn statement() -> P<CStat> {
    happyThen(happyParse(curry_1_5!(action_2)), box |x| match x {
        HappyAbsSyn12(z) => happyReturn(z),
        _ => notHappyAtAll()
    })
}

fn expression() -> P<CExpr> {
    happyThen(happyParse(curry_1_5!(action_3)), box |x| match x {
        HappyAbsSyn100(z) => happyReturn(z),
        _ => notHappyAtAll()
    })
}

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
            __return(mkAttrNode(attrs))
        })
    })
}

fn withLength<a: Clone + 'static>(nodeinfo: NodeInfo, mkAttrNode: Box<FnBox(NodeInfo) -> a>) -> P<a> {
    thenP(getSavedToken(), box move |lastTok| {
        let firstPos = nodeinfo.clone().pos();
        let attrs = NodeInfo::new(firstPos, posLenOfTok(lastTok),
                                  nodeinfo.name().unwrap_or_else(|| panic!("nameOfNode")));
        __return(mkAttrNode(attrs))
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
        __return(newDeclr)
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
        __return(newDeclr)
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
            __return(CDeclrR(ident, indirections, newName, cattrs, at))
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

fn doDeclIdent(declspecs: Vec<CDeclSpec>, CDeclrR(mIdent, _, _, _, _): CDeclrR) -> P<()> {
    let iypedef = |_0| {
        match (_0) {
            CStorageSpec(CTypedef(_)) => true,
            _ => false,
        }
    };

    match mIdent {
        None => __return(()),
        Some(ident) => {
            if any(iypedef, declspecs) { addTypedef(ident) }
            else { shadowTypedef(ident) }
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
            __return(())
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
// Original location: "templates/GenericTemplate.hs", line 1
// Original location: "templates/GenericTemplate.hs", line 1
// Original location: "<built-in>", line 1
// Original location: "<command-line>", line 1
// Original location: "<command-line>", line 8
// Original location: "/usr/include/stdc-predef.h", line 1

// Original location: "/usr/include/stdc-predef.h", line 17











































// Original location: "<command-line>", line 8
// Original location: "/usr/lib64/ghc-8.0.2/include/ghcversion.h", line 1

















// Original location: "<command-line>", line 8
// Original location: "/tmp/ghc25861_0/ghc_2.h", line 1






























































































































































































































// Original location: "<command-line>", line 8
// Original location: "templates/GenericTemplate.hs", line 1




// Original location: "templates/GenericTemplate.hs", line 16







// Original location: "templates/GenericTemplate.hs", line 36

// Original location: "templates/GenericTemplate.hs", line 46

// Original location: "templates/GenericTemplate.hs", line 55

#[derive(Clone)]
pub struct HappyStk<a>(a, Option<Box<HappyStk<a>>>);

// -----------------------------------------------------------------------------
/// starting the parse
fn happyParse(start_state: Action<HappyAbsSyn, HappyStk<HappyAbsSyn>>) -> P<HappyAbsSyn> {
    // TODO this is lazy failure
    happyNewToken(start_state, vec![], HappyStk(HappyAbsSyn::HappyErrorToken(0), None))
}

// -----------------------------------------------------------------------------
/// Accepting the parse
///
/// If the current token is (1), it means we've just accepted a partial
/// parse (a %partial parser).  We must ignore the saved token on the top of
/// the stack in this case.
fn happyAccept(_0: isize, _1: CToken, _2: HappyState<CToken, Box<FnBox(HappyStk<HappyAbsSyn>) -> P<HappyAbsSyn>>>, _3: Vec<HappyState<CToken, Box<FnBox(HappyStk<HappyAbsSyn>) -> P<HappyAbsSyn>>>>, _4: HappyStk<HappyAbsSyn>) -> P<HappyAbsSyn> {
    match (_0, _1, _2, _3, _4) {
        (1, tk, st, sts, HappyStk(_, Some(box HappyStk(ans, _)))) => {
            happyReturn1(ans)
        },
        (j, tk, st, sts, HappyStk(ans, _)) => {
            (happyReturn1(ans))
        },
    }
}

// -----------------------------------------------------------------------------
/// HappyState data type (not arrays)
pub struct HappyState<b, c>(Rc<Box<Fn(isize, isize, b, HappyState<b, c>, Vec<HappyState<b, c>>) -> c>>);

impl<b, c> Clone for HappyState<b, c> {
    fn clone(&self) -> Self {
        HappyState(self.0.clone())
    }
}

// -----------------------------------------------------------------------------
/// Shifting a token
fn happyShift(new_state: Action<HappyAbsSyn, HappyStk<HappyAbsSyn>>, _1: isize, tk: CToken,
              st: HappyState<CToken, Box<FnBox(HappyStk<HappyAbsSyn>) -> P<HappyAbsSyn>>>,
              sts: Vec<HappyState<CToken, Box<FnBox(HappyStk<HappyAbsSyn>) -> P<HappyAbsSyn>>>>,
              stk: HappyStk<HappyAbsSyn>) -> P<HappyAbsSyn> {
    match _1 {
        1 => {
            let HappyStk(x, _) = stk.clone();
            let i = (match x {
                HappyErrorToken(i) => {
                    i
                },
                _ => unreachable!(),
            });

            let new_state = Rc::new(new_state);
            let new_state_ = new_state.clone();
            new_state(i, i, tk, (HappyState(Rc::new(apply_5_1_clone!(new_state_)))), __op_concat(st, sts), stk)
        }
        i => {
            happyNewToken(new_state, __op_concat(st, sts), (HappyStk(HappyTerminal(tk), Some(box stk))))
        },
    }
}

// happyReduce is specialised for the common cases.

fn happySpecReduce_0(nt: isize, __fn: HappyAbsSyn, _2: isize, tk: CToken,
                     st: HappyState<CToken, Box<FnBox(HappyStk<HappyAbsSyn>) -> P<HappyAbsSyn>>>,
                     sts: Vec<HappyState<CToken, Box<FnBox(HappyStk<HappyAbsSyn>) -> P<HappyAbsSyn>>>>,
                     stk: HappyStk<HappyAbsSyn>) -> P<HappyAbsSyn> {
    match _2 {
        1 => happyFail(1, tk, st, sts, stk),
        j => {
            let HappyState(action) = st.clone();
            action(nt, j, tk, st.clone(), __op_concat(st, sts))(HappyStk(__fn, Some(box stk)))
        },
    }
}

fn happySpecReduce_1(nt: isize, __fn: Box<FnBox(HappyAbsSyn) -> HappyAbsSyn>, _2: isize, tk: CToken,
                     st: HappyState<CToken, Box<FnBox(HappyStk<HappyAbsSyn>) -> P<HappyAbsSyn>>>,
                     sts: Vec<HappyState<CToken, Box<FnBox(HappyStk<HappyAbsSyn>) -> P<HappyAbsSyn>>>>,
                     stk: HappyStk<HappyAbsSyn>) -> P<HappyAbsSyn> {
    match (_2, stk) {
        (1, stk) => happyFail(1, tk, st, sts, stk),
        (j, HappyStk(v1, stk_q)) => {
            // TODO assert len > 0?
            let st = sts.clone().remove(0);
            let HappyState(action) = st.clone();
            let r = __fn(v1);

            action(nt, j, tk, st, sts)(HappyStk(r, stk_q))
        }
    }
}

fn happySpecReduce_2(nt: isize, __fn: Box<FnBox(HappyAbsSyn, HappyAbsSyn) -> HappyAbsSyn>,
                     _2: isize, tk: CToken,
                     st: HappyState<CToken, Box<FnBox(HappyStk<HappyAbsSyn>) -> P<HappyAbsSyn>>>,
                     mut sts: Vec<HappyState<CToken, Box<FnBox(HappyStk<HappyAbsSyn>) -> P<HappyAbsSyn>>>>,
                     stk: HappyStk<HappyAbsSyn>) -> P<HappyAbsSyn> {
    match (_2, stk) {
        (1, stk) => happyFail(1, tk, st, sts, stk),
        (j, HappyStk(v1, Some(box HappyStk(v2, Some(box stk_q))))) => {
            sts.remove(0);
            let st = sts.clone().remove(0);
            let HappyState(action) = st.clone();
            let r = __fn(v1, v2);
            action(nt, j, tk, st, sts)(HappyStk(r, Some(box stk_q)))
        },
        _ => {
            panic!("irrefutable pattern")
        }
    }
}

fn happySpecReduce_3(nt: isize, __fn: Box<FnBox(HappyAbsSyn, HappyAbsSyn, HappyAbsSyn) -> HappyAbsSyn>,
                     _2: isize, tk: CToken,
                     st: HappyState<CToken, Box<FnBox(HappyStk<HappyAbsSyn>) -> P<HappyAbsSyn>>>,
                     mut stses: Vec<HappyState<CToken, Box<FnBox(HappyStk<HappyAbsSyn>) -> P<HappyAbsSyn>>>>,
                     stk: HappyStk<HappyAbsSyn>) -> P<HappyAbsSyn> {
    match (_2, stk) {
        (1, stk) => happyFail(1, tk, st, stses, stk),
        (j, HappyStk(v1, Some(box HappyStk(v2, Some(box HappyStk(v3, stk_q)))))) => {
            stses.remove(0);
            stses.remove(0);
            let sts = stses.clone();
            let st = stses.clone().remove(0);
            let HappyState(action) = st.clone();

            let r = __fn(v1, v2, v3);
            action(nt, j, tk, st, sts)(HappyStk(r, stk_q))
        },
        _ => {
            panic!("irrefutable pattern")
        }
    }
}

fn happyReduce<T: 'static>(k: isize, nt: isize,
                           __fn: Box<FnBox(HappyStk<HappyAbsSyn>) -> HappyStk<HappyAbsSyn>>,
                           _3: isize, tk: CToken,
                           st: HappyState<CToken, Box<FnBox(HappyStk<HappyAbsSyn>) -> P<T>>>,
                           sts: Vec<HappyState<CToken, Box<FnBox(HappyStk<HappyAbsSyn>) -> P<T>>>>,
                           stk: HappyStk<HappyAbsSyn>) -> P<T> {
    match _3 {
        1 => happyFail(1, tk, st, sts, stk),
        j => {
            let sts1 = happyDrop(k - 1, sts);
            let st1 = sts1.clone().remove(0);
            let HappyState(action) = st1.clone();
            let r = __fn(stk);
            action(nt, j, tk, st1, sts1)(r)
        },
    }
}

fn happyMonadReduce<T: 'static>(k: isize, nt: isize,
                                __fn: Box<FnBox(HappyStk<HappyAbsSyn>, CToken) -> P<HappyAbsSyn>>,
                                _3: isize, tk: CToken,
                                st: HappyState<CToken, Box<FnBox(HappyStk<HappyAbsSyn>) -> P<T>>>,
                                sts: Vec<HappyState<CToken, Box<FnBox(HappyStk<HappyAbsSyn>) -> P<T>>>>,
                                stk: HappyStk<HappyAbsSyn>) -> P<T> {
    match _3 {
        1 => happyFail(1, tk, st, sts, stk),
        j => {
            let sts1 = happyDrop(k, __op_concat(st, sts));
            let st1 = sts1.clone().remove(0);
            let HappyState(action) = st1.clone();

            let drop_stk = happyDropStk(k, stk.clone());

            happyThen1(__fn(stk.clone(), tk.clone()), box move |r| {
                clones!(sts1, drop_stk, st1, tk);
                action(nt, j, tk, st1, sts1)(HappyStk(r, Some(box drop_stk)))
            })
        }
    }
}

fn happyMonad2Reduce<T: 'static, U>(k: isize, nt: U,
                                    __fn: Box<FnBox(HappyStk<HappyAbsSyn>, CToken) -> P<HappyAbsSyn>>,
                                    _3: isize, tk: CToken,
                                    st: HappyState<CToken, Box<FnBox(HappyStk<HappyAbsSyn>) -> P<T>>>,
                                    sts: Vec<HappyState<CToken, Box<FnBox(HappyStk<HappyAbsSyn>) -> P<T>>>>,
                                    stk: HappyStk<HappyAbsSyn>) -> P<T> {
    match _3 {
        1 => happyFail(1, tk, st, sts, stk),
        j => {
            let sts1 = happyDrop(k, __op_concat(st, sts));
            let st1 = sts1.clone().remove(0);
            let HappyState(action) = st1.clone();

            let drop_stk = happyDropStk(k, stk.clone());

            let new_state = action;

            happyThen1(__fn(stk, tk), box move |r| {
                clones!(drop_stk, sts1, new_state);
                happyNewToken(curry_5_1!(new_state), sts1, HappyStk(r, Some(box drop_stk)))
            })
        }
    }
}

fn happyDrop<T>(n: isize, mut l: Vec<T>) -> Vec<T> {
    if n == 0 { l } else {
        l.remove(0);
        happyDrop(n - 1, l)
    }
}

fn happyDropStk<T>(n: isize, stk: HappyStk<T>) -> HappyStk<T> {
    match (n, stk) {
        (0, stk) => stk,
        (n, HappyStk(x, Some(box xs))) => happyDropStk(n - 1, xs),
        _ => panic!("irrefutable pattern"),
    }
}

// -----------------------------------------------------------------------------
/// Moving to a new state after a reduction
fn happyGoto(action: Action<HappyAbsSyn, HappyStk<HappyAbsSyn>>, j: isize, tk: CToken,
             st: HappyState<CToken, Box<FnBox(HappyStk<HappyAbsSyn>) -> P<HappyAbsSyn>>>,
             _curry_4: Vec<HappyState<CToken, Box<FnBox(HappyStk<HappyAbsSyn>) -> P<HappyAbsSyn>>>>,
             _curry_5: HappyStk<HappyAbsSyn>) -> P<HappyAbsSyn> {
    let action = Rc::new(action);
    let action_ = action.clone();
    action(j, j, tk, (HappyState(Rc::new(apply_5_1_clone!(action_)))), _curry_4, _curry_5)
}

// -----------------------------------------------------------------------------
/// Error recovery ((1) is the error token)
fn happyFail<T: 'static>(i: isize, tk: CToken,
                         old_st: HappyState<CToken, Box<FnBox(HappyStk<HappyAbsSyn>) -> P<T>>>,
                         sts: Vec<HappyState<CToken, Box<FnBox(HappyStk<HappyAbsSyn>) -> P<T>>>>,
                         stk: HappyStk<HappyAbsSyn>) -> P<T> {
    match (i, old_st, stk) {
        (1, old_st, HappyStk(x, Some(_))) => {
            let i = match x {
                HappyErrorToken(i) => i,
                _ => unreachable!(),
            };
            happyError_(i, tk)
        },
        (i, HappyState(action), stk) => {
            action(1, 1, tk, HappyState(action.clone()), sts)(HappyStk(HappyErrorToken(i), Some(box stk)))
        },
    }
}

fn notHappyAtAll<a: 'static>() -> a {
    panic!("Internal Happy error")
}

fn happySeq<a, b>(a: a, b: b) -> b {
    seq(a, b)
}

fn happyDoSeq<a, b>(a: a, b: b) -> b {
    seq(a, b)
}

fn happyDontSeq<a, b>(a: a, b: b) -> b {
    b
}

// end of Happy Template.
