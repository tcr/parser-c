#![allow(unreachable_patterns)]
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
use data::name::new_name_supply;

// fn(A, B) -> fn(C) -> {eval fn(A, B, C)}
macro_rules! partial_1 {
    ($inner: expr) => ( box $inner );
    ($inner: expr, $($arg: expr),+ ) => ( box move |_0| { $inner($($arg),+ , _0) } )
}

// Parser produced by modified Happy Version 1.19.6

#[derive(Clone)]
pub enum HappyAbsSyn {
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


type Monad<T> = P<T>;
type Token = (CToken);
fn action_0(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        7 => happyGoto(p, action_144, j),
        8 => happyGoto(p, action_5, j),
        _ => happyReduce_5(p, j)
    }
}

fn action_1(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_111, j),
        150 => happyShift(p, action_112, j),
        185 => happyShift(p, action_113, j),
        187 => happyShift(p, action_114, j),
        188 => happyShift(p, action_115, j),
        190 => happyShift(p, action_116, j),
        192 => happyShift(p, action_117, j),
        195 => happyShift(p, action_118, j),
        198 => happyShift(p, action_119, j),
        200 => happyShift(p, action_120, j),
        201 => happyShift(p, action_121, j),
        202 => happyShift(p, action_122, j),
        207 => happyShift(p, action_123, j),
        208 => happyShift(p, action_124, j),
        209 => happyShift(p, action_125, j),
        210 => happyShift(p, action_126, j),
        212 => happyShift(p, action_127, j),
        215 => happyShift(p, action_128, j),
        218 => happyShift(p, action_129, j),
        219 => happyShift(p, action_130, j),
        221 => happyShift(p, action_131, j),
        222 => happyShift(p, action_132, j),
        223 => happyShift(p, action_133, j),
        225 => happyShift(p, action_134, j),
        226 => happyShift(p, action_135, j),
        227 => happyShift(p, action_136, j),
        228 => happyShift(p, action_137, j),
        229 => happyShift(p, action_138, j),
        230 => happyShift(p, action_139, j),
        237 => happyShift(p, action_140, j),
        238 => happyShift(p, action_141, j),
        239 => happyShift(p, action_142, j),
        240 => happyShift(p, action_143, j),
        9 => happyGoto(p, action_76, j),
        10 => happyGoto(p, action_77, j),
        11 => happyGoto(p, action_78, j),
        32 => happyGoto(p, action_79, j),
        34 => happyGoto(p, action_80, j),
        36 => happyGoto(p, action_81, j),
        37 => happyGoto(p, action_82, j),
        38 => happyGoto(p, action_83, j),
        40 => happyGoto(p, action_84, j),
        41 => happyGoto(p, action_85, j),
        42 => happyGoto(p, action_86, j),
        43 => happyGoto(p, action_87, j),
        44 => happyGoto(p, action_88, j),
        45 => happyGoto(p, action_89, j),
        46 => happyGoto(p, action_90, j),
        47 => happyGoto(p, action_91, j),
        48 => happyGoto(p, action_92, j),
        49 => happyGoto(p, action_93, j),
        50 => happyGoto(p, action_94, j),
        51 => happyGoto(p, action_95, j),
        52 => happyGoto(p, action_96, j),
        53 => happyGoto(p, action_97, j),
        54 => happyGoto(p, action_98, j),
        61 => happyGoto(p, action_99, j),
        65 => happyGoto(p, action_100, j),
        75 => happyGoto(p, action_101, j),
        76 => happyGoto(p, action_102, j),
        77 => happyGoto(p, action_103, j),
        78 => happyGoto(p, action_104, j),
        79 => happyGoto(p, action_105, j),
        80 => happyGoto(p, action_106, j),
        81 => happyGoto(p, action_107, j),
        132 => happyGoto(p, action_108, j),
        133 => happyGoto(p, action_109, j),
        134 => happyGoto(p, action_110, j),
        _ => happyReduce_471(p, j)
    }
}

fn action_2(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_26, j),
        144 => happyShift(p, action_27, j),
        145 => happyShift(p, action_28, j),
        146 => happyShift(p, action_29, j),
        147 => happyShift(p, action_30, j),
        148 => happyShift(p, action_31, j),
        149 => happyShift(p, action_32, j),
        150 => happyShift(p, action_33, j),
        153 => happyShift(p, action_34, j),
        164 => happyShift(p, action_35, j),
        180 => happyShift(p, action_60, j),
        181 => happyShift(p, action_61, j),
        184 => happyShift(p, action_36, j),
        187 => happyShift(p, action_62, j),
        189 => happyShift(p, action_63, j),
        191 => happyShift(p, action_64, j),
        194 => happyShift(p, action_65, j),
        196 => happyShift(p, action_66, j),
        197 => happyShift(p, action_67, j),
        203 => happyShift(p, action_68, j),
        204 => happyShift(p, action_37, j),
        205 => happyShift(p, action_69, j),
        206 => happyShift(p, action_70, j),
        217 => happyShift(p, action_71, j),
        220 => happyShift(p, action_38, j),
        224 => happyShift(p, action_72, j),
        232 => happyShift(p, action_73, j),
        233 => happyShift(p, action_39, j),
        234 => happyShift(p, action_40, j),
        235 => happyShift(p, action_41, j),
        236 => happyShift(p, action_42, j),
        237 => happyShift(p, action_74, j),
        238 => happyShift(p, action_75, j),
        240 => happyShift(p, action_44, j),
        241 => happyShift(p, action_45, j),
        242 => happyShift(p, action_46, j),
        243 => happyShift(p, action_47, j),
        244 => happyShift(p, action_48, j),
        245 => happyShift(p, action_49, j),
        12 => happyGoto(p, action_50, j),
        13 => happyGoto(p, action_51, j),
        14 => happyGoto(p, action_52, j),
        22 => happyGoto(p, action_53, j),
        23 => happyGoto(p, action_54, j),
        24 => happyGoto(p, action_55, j),
        25 => happyGoto(p, action_56, j),
        26 => happyGoto(p, action_57, j),
        100 => happyGoto(p, action_6, j),
        104 => happyGoto(p, action_7, j),
        106 => happyGoto(p, action_8, j),
        107 => happyGoto(p, action_9, j),
        108 => happyGoto(p, action_10, j),
        109 => happyGoto(p, action_11, j),
        110 => happyGoto(p, action_12, j),
        111 => happyGoto(p, action_13, j),
        112 => happyGoto(p, action_14, j),
        113 => happyGoto(p, action_15, j),
        114 => happyGoto(p, action_16, j),
        115 => happyGoto(p, action_17, j),
        116 => happyGoto(p, action_18, j),
        117 => happyGoto(p, action_19, j),
        118 => happyGoto(p, action_20, j),
        119 => happyGoto(p, action_21, j),
        120 => happyGoto(p, action_22, j),
        122 => happyGoto(p, action_58, j),
        127 => happyGoto(p, action_24, j),
        128 => happyGoto(p, action_25, j),
        131 => happyGoto(p, action_59, j),
        _ => happyFail(p, j)
    }
}

fn action_3(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_26, j),
        144 => happyShift(p, action_27, j),
        145 => happyShift(p, action_28, j),
        146 => happyShift(p, action_29, j),
        147 => happyShift(p, action_30, j),
        148 => happyShift(p, action_31, j),
        149 => happyShift(p, action_32, j),
        150 => happyShift(p, action_33, j),
        153 => happyShift(p, action_34, j),
        164 => happyShift(p, action_35, j),
        184 => happyShift(p, action_36, j),
        204 => happyShift(p, action_37, j),
        220 => happyShift(p, action_38, j),
        233 => happyShift(p, action_39, j),
        234 => happyShift(p, action_40, j),
        235 => happyShift(p, action_41, j),
        236 => happyShift(p, action_42, j),
        237 => happyShift(p, action_43, j),
        240 => happyShift(p, action_44, j),
        241 => happyShift(p, action_45, j),
        242 => happyShift(p, action_46, j),
        243 => happyShift(p, action_47, j),
        244 => happyShift(p, action_48, j),
        245 => happyShift(p, action_49, j),
        100 => happyGoto(p, action_6, j),
        104 => happyGoto(p, action_7, j),
        106 => happyGoto(p, action_8, j),
        107 => happyGoto(p, action_9, j),
        108 => happyGoto(p, action_10, j),
        109 => happyGoto(p, action_11, j),
        110 => happyGoto(p, action_12, j),
        111 => happyGoto(p, action_13, j),
        112 => happyGoto(p, action_14, j),
        113 => happyGoto(p, action_15, j),
        114 => happyGoto(p, action_16, j),
        115 => happyGoto(p, action_17, j),
        116 => happyGoto(p, action_18, j),
        117 => happyGoto(p, action_19, j),
        118 => happyGoto(p, action_20, j),
        119 => happyGoto(p, action_21, j),
        120 => happyGoto(p, action_22, j),
        122 => happyGoto(p, action_23, j),
        127 => happyGoto(p, action_24, j),
        128 => happyGoto(p, action_25, j),
        _ => happyFail(p, j)
    }
}

fn action_4(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        8 => happyGoto(p, action_5, j),
        _ => happyFail(p, j)
    }
}

fn action_5(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_111, j),
        150 => happyShift(p, action_112, j),
        180 => happyShift(p, action_334, j),
        185 => happyShift(p, action_113, j),
        187 => happyShift(p, action_114, j),
        188 => happyShift(p, action_115, j),
        190 => happyShift(p, action_116, j),
        192 => happyShift(p, action_117, j),
        195 => happyShift(p, action_118, j),
        198 => happyShift(p, action_119, j),
        200 => happyShift(p, action_120, j),
        201 => happyShift(p, action_121, j),
        202 => happyShift(p, action_122, j),
        207 => happyShift(p, action_123, j),
        208 => happyShift(p, action_124, j),
        209 => happyShift(p, action_125, j),
        210 => happyShift(p, action_126, j),
        212 => happyShift(p, action_127, j),
        215 => happyShift(p, action_128, j),
        218 => happyShift(p, action_129, j),
        219 => happyShift(p, action_130, j),
        221 => happyShift(p, action_131, j),
        222 => happyShift(p, action_132, j),
        223 => happyShift(p, action_133, j),
        225 => happyShift(p, action_134, j),
        226 => happyShift(p, action_135, j),
        227 => happyShift(p, action_136, j),
        228 => happyShift(p, action_137, j),
        229 => happyShift(p, action_138, j),
        230 => happyShift(p, action_139, j),
        237 => happyShift(p, action_140, j),
        238 => happyShift(p, action_141, j),
        239 => happyShift(p, action_142, j),
        240 => happyShift(p, action_143, j),
        247 => happyReduce_4(p, j),
        9 => happyGoto(p, action_333, j),
        10 => happyGoto(p, action_77, j),
        11 => happyGoto(p, action_78, j),
        32 => happyGoto(p, action_79, j),
        34 => happyGoto(p, action_80, j),
        36 => happyGoto(p, action_81, j),
        37 => happyGoto(p, action_82, j),
        38 => happyGoto(p, action_83, j),
        40 => happyGoto(p, action_84, j),
        41 => happyGoto(p, action_85, j),
        42 => happyGoto(p, action_86, j),
        43 => happyGoto(p, action_87, j),
        44 => happyGoto(p, action_88, j),
        45 => happyGoto(p, action_89, j),
        46 => happyGoto(p, action_90, j),
        47 => happyGoto(p, action_91, j),
        48 => happyGoto(p, action_92, j),
        49 => happyGoto(p, action_93, j),
        50 => happyGoto(p, action_94, j),
        51 => happyGoto(p, action_95, j),
        52 => happyGoto(p, action_96, j),
        53 => happyGoto(p, action_97, j),
        54 => happyGoto(p, action_98, j),
        61 => happyGoto(p, action_99, j),
        65 => happyGoto(p, action_100, j),
        75 => happyGoto(p, action_101, j),
        76 => happyGoto(p, action_102, j),
        77 => happyGoto(p, action_103, j),
        78 => happyGoto(p, action_104, j),
        79 => happyGoto(p, action_105, j),
        80 => happyGoto(p, action_106, j),
        81 => happyGoto(p, action_107, j),
        132 => happyGoto(p, action_108, j),
        133 => happyGoto(p, action_109, j),
        134 => happyGoto(p, action_110, j),
        _ => happyReduce_471(p, j)
    }
}

fn action_6(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_376(p, j)
    }
}

fn action_7(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_327, j),
        140 => happyShift(p, action_328, j),
        142 => happyShift(p, action_329, j),
        143 => happyShift(p, action_330, j),
        146 => happyShift(p, action_331, j),
        147 => happyShift(p, action_332, j),
        _ => happyReduce_388(p, j)
    }
}

fn action_8(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        168 => happyShift(p, action_316, j),
        169 => happyShift(p, action_317, j),
        170 => happyShift(p, action_318, j),
        171 => happyShift(p, action_319, j),
        172 => happyShift(p, action_320, j),
        173 => happyShift(p, action_321, j),
        174 => happyShift(p, action_322, j),
        175 => happyShift(p, action_323, j),
        176 => happyShift(p, action_324, j),
        177 => happyShift(p, action_325, j),
        178 => happyShift(p, action_326, j),
        121 => happyGoto(p, action_315, j),
        _ => happyReduce_406(p, j)
    }
}

fn action_9(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_26, j),
        144 => happyShift(p, action_27, j),
        145 => happyShift(p, action_28, j),
        146 => happyShift(p, action_29, j),
        147 => happyShift(p, action_30, j),
        148 => happyShift(p, action_31, j),
        149 => happyShift(p, action_32, j),
        150 => happyShift(p, action_33, j),
        153 => happyShift(p, action_34, j),
        164 => happyShift(p, action_35, j),
        184 => happyShift(p, action_36, j),
        204 => happyShift(p, action_37, j),
        220 => happyShift(p, action_38, j),
        233 => happyShift(p, action_39, j),
        234 => happyShift(p, action_40, j),
        235 => happyShift(p, action_41, j),
        236 => happyShift(p, action_42, j),
        237 => happyShift(p, action_43, j),
        240 => happyShift(p, action_44, j),
        241 => happyShift(p, action_45, j),
        242 => happyShift(p, action_46, j),
        243 => happyShift(p, action_47, j),
        244 => happyShift(p, action_48, j),
        245 => happyShift(p, action_49, j),
        100 => happyGoto(p, action_6, j),
        104 => happyGoto(p, action_7, j),
        106 => happyGoto(p, action_259, j),
        107 => happyGoto(p, action_9, j),
        108 => happyGoto(p, action_314, j),
        127 => happyGoto(p, action_24, j),
        128 => happyGoto(p, action_25, j),
        _ => happyFail(p, j)
    }
}

fn action_10(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_408(p, j)
    }
}

fn action_11(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        150 => happyShift(p, action_311, j),
        151 => happyShift(p, action_312, j),
        152 => happyShift(p, action_313, j),
        _ => happyReduce_412(p, j)
    }
}

fn action_12(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        148 => happyShift(p, action_309, j),
        149 => happyShift(p, action_310, j),
        _ => happyReduce_415(p, j)
    }
}

fn action_13(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        154 => happyShift(p, action_307, j),
        155 => happyShift(p, action_308, j),
        _ => happyReduce_418(p, j)
    }
}

fn action_14(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        156 => happyShift(p, action_303, j),
        157 => happyShift(p, action_304, j),
        158 => happyShift(p, action_305, j),
        159 => happyShift(p, action_306, j),
        _ => happyReduce_423(p, j)
    }
}

fn action_15(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        160 => happyShift(p, action_301, j),
        161 => happyShift(p, action_302, j),
        _ => happyReduce_426(p, j)
    }
}

fn action_16(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        153 => happyShift(p, action_300, j),
        _ => happyReduce_428(p, j)
    }
}

fn action_17(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        162 => happyShift(p, action_299, j),
        _ => happyReduce_430(p, j)
    }
}

fn action_18(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        163 => happyShift(p, action_298, j),
        _ => happyReduce_432(p, j)
    }
}

fn action_19(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        164 => happyShift(p, action_297, j),
        _ => happyReduce_434(p, j)
    }
}

fn action_20(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        165 => happyShift(p, action_295, j),
        166 => happyShift(p, action_296, j),
        _ => happyReduce_436(p, j)
    }
}

fn action_21(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_439(p, j)
    }
}

fn action_22(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        179 => happyShift(p, action_294, j),
        _ => happyReduce_452(p, j)
    }
}

fn action_23(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        247 => Ok(Cont::Accept(j)),
        _ => happyFail(p, j)
    }
}

fn action_24(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_361(p, j)
    }
}

fn action_25(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_362(p, j)
    }
}

fn action_26(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_26, j),
        144 => happyShift(p, action_27, j),
        145 => happyShift(p, action_28, j),
        146 => happyShift(p, action_29, j),
        147 => happyShift(p, action_30, j),
        148 => happyShift(p, action_31, j),
        149 => happyShift(p, action_32, j),
        150 => happyShift(p, action_33, j),
        153 => happyShift(p, action_34, j),
        164 => happyShift(p, action_35, j),
        181 => happyShift(p, action_61, j),
        184 => happyShift(p, action_36, j),
        190 => happyShift(p, action_116, j),
        192 => happyShift(p, action_117, j),
        195 => happyShift(p, action_118, j),
        198 => happyShift(p, action_119, j),
        200 => happyShift(p, action_120, j),
        202 => happyShift(p, action_122, j),
        204 => happyShift(p, action_37, j),
        208 => happyShift(p, action_124, j),
        209 => happyShift(p, action_125, j),
        210 => happyShift(p, action_126, j),
        218 => happyShift(p, action_129, j),
        219 => happyShift(p, action_130, j),
        220 => happyShift(p, action_38, j),
        223 => happyShift(p, action_133, j),
        226 => happyShift(p, action_135, j),
        228 => happyShift(p, action_137, j),
        229 => happyShift(p, action_138, j),
        230 => happyShift(p, action_139, j),
        233 => happyShift(p, action_39, j),
        234 => happyShift(p, action_40, j),
        235 => happyShift(p, action_41, j),
        236 => happyShift(p, action_42, j),
        237 => happyShift(p, action_43, j),
        238 => happyShift(p, action_141, j),
        239 => happyShift(p, action_142, j),
        240 => happyShift(p, action_44, j),
        241 => happyShift(p, action_45, j),
        242 => happyShift(p, action_46, j),
        243 => happyShift(p, action_47, j),
        244 => happyShift(p, action_48, j),
        245 => happyShift(p, action_49, j),
        14 => happyGoto(p, action_285, j),
        44 => happyGoto(p, action_286, j),
        45 => happyGoto(p, action_89, j),
        47 => happyGoto(p, action_287, j),
        49 => happyGoto(p, action_288, j),
        51 => happyGoto(p, action_289, j),
        52 => happyGoto(p, action_96, j),
        53 => happyGoto(p, action_97, j),
        54 => happyGoto(p, action_98, j),
        61 => happyGoto(p, action_99, j),
        65 => happyGoto(p, action_290, j),
        86 => happyGoto(p, action_291, j),
        100 => happyGoto(p, action_6, j),
        104 => happyGoto(p, action_7, j),
        106 => happyGoto(p, action_8, j),
        107 => happyGoto(p, action_9, j),
        108 => happyGoto(p, action_10, j),
        109 => happyGoto(p, action_11, j),
        110 => happyGoto(p, action_12, j),
        111 => happyGoto(p, action_13, j),
        112 => happyGoto(p, action_14, j),
        113 => happyGoto(p, action_15, j),
        114 => happyGoto(p, action_16, j),
        115 => happyGoto(p, action_17, j),
        116 => happyGoto(p, action_18, j),
        117 => happyGoto(p, action_19, j),
        118 => happyGoto(p, action_20, j),
        119 => happyGoto(p, action_21, j),
        120 => happyGoto(p, action_22, j),
        122 => happyGoto(p, action_292, j),
        127 => happyGoto(p, action_24, j),
        128 => happyGoto(p, action_25, j),
        132 => happyGoto(p, action_108, j),
        133 => happyGoto(p, action_293, j),
        134 => happyGoto(p, action_110, j),
        _ => happyReduce_471(p, j)
    }
}

fn action_27(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_405(p, j)
    }
}

fn action_28(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_404(p, j)
    }
}

fn action_29(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_272, j),
        144 => happyShift(p, action_27, j),
        145 => happyShift(p, action_28, j),
        146 => happyShift(p, action_29, j),
        147 => happyShift(p, action_30, j),
        148 => happyShift(p, action_31, j),
        149 => happyShift(p, action_32, j),
        150 => happyShift(p, action_33, j),
        153 => happyShift(p, action_34, j),
        164 => happyShift(p, action_35, j),
        184 => happyShift(p, action_36, j),
        204 => happyShift(p, action_37, j),
        220 => happyShift(p, action_38, j),
        233 => happyShift(p, action_39, j),
        234 => happyShift(p, action_40, j),
        235 => happyShift(p, action_41, j),
        236 => happyShift(p, action_42, j),
        237 => happyShift(p, action_43, j),
        240 => happyShift(p, action_44, j),
        241 => happyShift(p, action_45, j),
        242 => happyShift(p, action_46, j),
        243 => happyShift(p, action_47, j),
        244 => happyShift(p, action_48, j),
        245 => happyShift(p, action_49, j),
        100 => happyGoto(p, action_6, j),
        104 => happyGoto(p, action_7, j),
        106 => happyGoto(p, action_284, j),
        107 => happyGoto(p, action_9, j),
        127 => happyGoto(p, action_24, j),
        128 => happyGoto(p, action_25, j),
        _ => happyFail(p, j)
    }
}

fn action_30(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_272, j),
        144 => happyShift(p, action_27, j),
        145 => happyShift(p, action_28, j),
        146 => happyShift(p, action_29, j),
        147 => happyShift(p, action_30, j),
        148 => happyShift(p, action_31, j),
        149 => happyShift(p, action_32, j),
        150 => happyShift(p, action_33, j),
        153 => happyShift(p, action_34, j),
        164 => happyShift(p, action_35, j),
        184 => happyShift(p, action_36, j),
        204 => happyShift(p, action_37, j),
        220 => happyShift(p, action_38, j),
        233 => happyShift(p, action_39, j),
        234 => happyShift(p, action_40, j),
        235 => happyShift(p, action_41, j),
        236 => happyShift(p, action_42, j),
        237 => happyShift(p, action_43, j),
        240 => happyShift(p, action_44, j),
        241 => happyShift(p, action_45, j),
        242 => happyShift(p, action_46, j),
        243 => happyShift(p, action_47, j),
        244 => happyShift(p, action_48, j),
        245 => happyShift(p, action_49, j),
        100 => happyGoto(p, action_6, j),
        104 => happyGoto(p, action_7, j),
        106 => happyGoto(p, action_283, j),
        107 => happyGoto(p, action_9, j),
        127 => happyGoto(p, action_24, j),
        128 => happyGoto(p, action_25, j),
        _ => happyFail(p, j)
    }
}

fn action_31(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_402(p, j)
    }
}

fn action_32(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_403(p, j)
    }
}

fn action_33(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_401(p, j)
    }
}

fn action_34(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_400(p, j)
    }
}

fn action_35(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        237 => happyShift(p, action_254, j),
        238 => happyShift(p, action_75, j),
        131 => happyGoto(p, action_282, j),
        _ => happyFail(p, j)
    }
}

fn action_36(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_281, j),
        144 => happyShift(p, action_27, j),
        145 => happyShift(p, action_28, j),
        146 => happyShift(p, action_29, j),
        147 => happyShift(p, action_30, j),
        148 => happyShift(p, action_31, j),
        149 => happyShift(p, action_32, j),
        150 => happyShift(p, action_33, j),
        153 => happyShift(p, action_34, j),
        164 => happyShift(p, action_35, j),
        184 => happyShift(p, action_36, j),
        204 => happyShift(p, action_37, j),
        220 => happyShift(p, action_38, j),
        233 => happyShift(p, action_39, j),
        234 => happyShift(p, action_40, j),
        235 => happyShift(p, action_41, j),
        236 => happyShift(p, action_42, j),
        237 => happyShift(p, action_43, j),
        240 => happyShift(p, action_44, j),
        241 => happyShift(p, action_45, j),
        242 => happyShift(p, action_46, j),
        243 => happyShift(p, action_47, j),
        244 => happyShift(p, action_48, j),
        245 => happyShift(p, action_49, j),
        100 => happyGoto(p, action_6, j),
        104 => happyGoto(p, action_7, j),
        106 => happyGoto(p, action_280, j),
        107 => happyGoto(p, action_9, j),
        127 => happyGoto(p, action_24, j),
        128 => happyGoto(p, action_25, j),
        _ => happyFail(p, j)
    }
}

fn action_37(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_279, j),
        _ => happyFail(p, j)
    }
}

fn action_38(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_278, j),
        144 => happyShift(p, action_27, j),
        145 => happyShift(p, action_28, j),
        146 => happyShift(p, action_29, j),
        147 => happyShift(p, action_30, j),
        148 => happyShift(p, action_31, j),
        149 => happyShift(p, action_32, j),
        150 => happyShift(p, action_33, j),
        153 => happyShift(p, action_34, j),
        164 => happyShift(p, action_35, j),
        184 => happyShift(p, action_36, j),
        204 => happyShift(p, action_37, j),
        220 => happyShift(p, action_38, j),
        233 => happyShift(p, action_39, j),
        234 => happyShift(p, action_40, j),
        235 => happyShift(p, action_41, j),
        236 => happyShift(p, action_42, j),
        237 => happyShift(p, action_43, j),
        240 => happyShift(p, action_44, j),
        241 => happyShift(p, action_45, j),
        242 => happyShift(p, action_46, j),
        243 => happyShift(p, action_47, j),
        244 => happyShift(p, action_48, j),
        245 => happyShift(p, action_49, j),
        100 => happyGoto(p, action_6, j),
        104 => happyGoto(p, action_7, j),
        106 => happyGoto(p, action_277, j),
        107 => happyGoto(p, action_9, j),
        127 => happyGoto(p, action_24, j),
        128 => happyGoto(p, action_25, j),
        _ => happyFail(p, j)
    }
}

fn action_39(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_462(p, j)
    }
}

fn action_40(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_461(p, j)
    }
}

fn action_41(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_463(p, j)
    }
}

fn action_42(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        236 => happyShift(p, action_276, j),
        129 => happyGoto(p, action_275, j),
        _ => happyReduce_464(p, j)
    }
}

fn action_43(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_360(p, j)
    }
}

fn action_44(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_26, j),
        144 => happyShift(p, action_27, j),
        145 => happyShift(p, action_28, j),
        146 => happyShift(p, action_29, j),
        147 => happyShift(p, action_30, j),
        148 => happyShift(p, action_31, j),
        149 => happyShift(p, action_32, j),
        150 => happyShift(p, action_33, j),
        153 => happyShift(p, action_34, j),
        164 => happyShift(p, action_35, j),
        184 => happyShift(p, action_36, j),
        204 => happyShift(p, action_37, j),
        220 => happyShift(p, action_38, j),
        233 => happyShift(p, action_39, j),
        234 => happyShift(p, action_40, j),
        235 => happyShift(p, action_41, j),
        236 => happyShift(p, action_42, j),
        237 => happyShift(p, action_43, j),
        240 => happyShift(p, action_44, j),
        241 => happyShift(p, action_45, j),
        242 => happyShift(p, action_46, j),
        243 => happyShift(p, action_47, j),
        244 => happyShift(p, action_48, j),
        245 => happyShift(p, action_49, j),
        100 => happyGoto(p, action_6, j),
        104 => happyGoto(p, action_7, j),
        106 => happyGoto(p, action_259, j),
        107 => happyGoto(p, action_9, j),
        108 => happyGoto(p, action_274, j),
        127 => happyGoto(p, action_24, j),
        128 => happyGoto(p, action_25, j),
        _ => happyFail(p, j)
    }
}

fn action_45(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_272, j),
        144 => happyShift(p, action_27, j),
        145 => happyShift(p, action_28, j),
        146 => happyShift(p, action_29, j),
        147 => happyShift(p, action_30, j),
        148 => happyShift(p, action_31, j),
        149 => happyShift(p, action_32, j),
        150 => happyShift(p, action_33, j),
        153 => happyShift(p, action_34, j),
        164 => happyShift(p, action_35, j),
        184 => happyShift(p, action_36, j),
        204 => happyShift(p, action_37, j),
        220 => happyShift(p, action_38, j),
        233 => happyShift(p, action_39, j),
        234 => happyShift(p, action_40, j),
        235 => happyShift(p, action_41, j),
        236 => happyShift(p, action_42, j),
        237 => happyShift(p, action_43, j),
        240 => happyShift(p, action_44, j),
        241 => happyShift(p, action_45, j),
        242 => happyShift(p, action_46, j),
        243 => happyShift(p, action_47, j),
        244 => happyShift(p, action_48, j),
        245 => happyShift(p, action_49, j),
        100 => happyGoto(p, action_6, j),
        104 => happyGoto(p, action_7, j),
        106 => happyGoto(p, action_273, j),
        107 => happyGoto(p, action_9, j),
        127 => happyGoto(p, action_24, j),
        128 => happyGoto(p, action_25, j),
        _ => happyFail(p, j)
    }
}

fn action_46(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_272, j),
        144 => happyShift(p, action_27, j),
        145 => happyShift(p, action_28, j),
        146 => happyShift(p, action_29, j),
        147 => happyShift(p, action_30, j),
        148 => happyShift(p, action_31, j),
        149 => happyShift(p, action_32, j),
        150 => happyShift(p, action_33, j),
        153 => happyShift(p, action_34, j),
        164 => happyShift(p, action_35, j),
        184 => happyShift(p, action_36, j),
        204 => happyShift(p, action_37, j),
        220 => happyShift(p, action_38, j),
        233 => happyShift(p, action_39, j),
        234 => happyShift(p, action_40, j),
        235 => happyShift(p, action_41, j),
        236 => happyShift(p, action_42, j),
        237 => happyShift(p, action_43, j),
        240 => happyShift(p, action_44, j),
        241 => happyShift(p, action_45, j),
        242 => happyShift(p, action_46, j),
        243 => happyShift(p, action_47, j),
        244 => happyShift(p, action_48, j),
        245 => happyShift(p, action_49, j),
        100 => happyGoto(p, action_6, j),
        104 => happyGoto(p, action_7, j),
        106 => happyGoto(p, action_271, j),
        107 => happyGoto(p, action_9, j),
        127 => happyGoto(p, action_24, j),
        128 => happyGoto(p, action_25, j),
        _ => happyFail(p, j)
    }
}

fn action_47(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_270, j),
        _ => happyFail(p, j)
    }
}

fn action_48(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_269, j),
        _ => happyFail(p, j)
    }
}

fn action_49(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_268, j),
        _ => happyFail(p, j)
    }
}

fn action_50(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        247 => Ok(Cont::Accept(j)),
        _ => happyFail(p, j)
    }
}

fn action_51(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_27(p, j)
    }
}

fn action_52(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_28(p, j)
    }
}

fn action_53(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_29(p, j)
    }
}

fn action_54(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_30(p, j)
    }
}

fn action_55(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_31(p, j)
    }
}

fn action_56(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_32(p, j)
    }
}

fn action_57(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_33(p, j)
    }
}

fn action_58(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        180 => happyShift(p, action_267, j),
        _ => happyFail(p, j)
    }
}

fn action_59(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        167 => happyShift(p, action_266, j),
        _ => happyFail(p, j)
    }
}

fn action_60(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_56(p, j)
    }
}

fn action_61(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        15 => happyGoto(p, action_265, j),
        _ => happyReduce_40(p, j)
    }
}

fn action_62(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        186 => happyShift(p, action_171, j),
        193 => happyShift(p, action_172, j),
        213 => happyShift(p, action_173, j),
        214 => happyShift(p, action_174, j),
        216 => happyShift(p, action_175, j),
        231 => happyShift(p, action_176, j),
        27 => happyGoto(p, action_263, j),
        64 => happyGoto(p, action_264, j),
        _ => happyReduce_74(p, j)
    }
}

fn action_63(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        180 => happyShift(p, action_262, j),
        _ => happyFail(p, j)
    }
}

fn action_64(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_26, j),
        144 => happyShift(p, action_27, j),
        145 => happyShift(p, action_28, j),
        146 => happyShift(p, action_29, j),
        147 => happyShift(p, action_30, j),
        148 => happyShift(p, action_31, j),
        149 => happyShift(p, action_32, j),
        150 => happyShift(p, action_33, j),
        153 => happyShift(p, action_34, j),
        164 => happyShift(p, action_35, j),
        184 => happyShift(p, action_36, j),
        204 => happyShift(p, action_37, j),
        220 => happyShift(p, action_38, j),
        233 => happyShift(p, action_39, j),
        234 => happyShift(p, action_40, j),
        235 => happyShift(p, action_41, j),
        236 => happyShift(p, action_42, j),
        237 => happyShift(p, action_43, j),
        240 => happyShift(p, action_44, j),
        241 => happyShift(p, action_45, j),
        242 => happyShift(p, action_46, j),
        243 => happyShift(p, action_47, j),
        244 => happyShift(p, action_48, j),
        245 => happyShift(p, action_49, j),
        100 => happyGoto(p, action_6, j),
        104 => happyGoto(p, action_7, j),
        106 => happyGoto(p, action_259, j),
        107 => happyGoto(p, action_9, j),
        108 => happyGoto(p, action_10, j),
        109 => happyGoto(p, action_11, j),
        110 => happyGoto(p, action_12, j),
        111 => happyGoto(p, action_13, j),
        112 => happyGoto(p, action_14, j),
        113 => happyGoto(p, action_15, j),
        114 => happyGoto(p, action_16, j),
        115 => happyGoto(p, action_17, j),
        116 => happyGoto(p, action_18, j),
        117 => happyGoto(p, action_19, j),
        118 => happyGoto(p, action_20, j),
        119 => happyGoto(p, action_260, j),
        126 => happyGoto(p, action_261, j),
        127 => happyGoto(p, action_24, j),
        128 => happyGoto(p, action_25, j),
        _ => happyFail(p, j)
    }
}

fn action_65(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        180 => happyShift(p, action_258, j),
        _ => happyFail(p, j)
    }
}

fn action_66(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        167 => happyShift(p, action_257, j),
        _ => happyFail(p, j)
    }
}

fn action_67(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_26, j),
        144 => happyShift(p, action_27, j),
        145 => happyShift(p, action_28, j),
        146 => happyShift(p, action_29, j),
        147 => happyShift(p, action_30, j),
        148 => happyShift(p, action_31, j),
        149 => happyShift(p, action_32, j),
        150 => happyShift(p, action_33, j),
        153 => happyShift(p, action_34, j),
        164 => happyShift(p, action_35, j),
        180 => happyShift(p, action_60, j),
        181 => happyShift(p, action_61, j),
        184 => happyShift(p, action_36, j),
        187 => happyShift(p, action_62, j),
        189 => happyShift(p, action_63, j),
        191 => happyShift(p, action_64, j),
        194 => happyShift(p, action_65, j),
        196 => happyShift(p, action_66, j),
        197 => happyShift(p, action_67, j),
        203 => happyShift(p, action_68, j),
        204 => happyShift(p, action_37, j),
        205 => happyShift(p, action_69, j),
        206 => happyShift(p, action_70, j),
        217 => happyShift(p, action_71, j),
        220 => happyShift(p, action_38, j),
        224 => happyShift(p, action_72, j),
        232 => happyShift(p, action_73, j),
        233 => happyShift(p, action_39, j),
        234 => happyShift(p, action_40, j),
        235 => happyShift(p, action_41, j),
        236 => happyShift(p, action_42, j),
        237 => happyShift(p, action_74, j),
        238 => happyShift(p, action_75, j),
        240 => happyShift(p, action_44, j),
        241 => happyShift(p, action_45, j),
        242 => happyShift(p, action_46, j),
        243 => happyShift(p, action_47, j),
        244 => happyShift(p, action_48, j),
        245 => happyShift(p, action_49, j),
        12 => happyGoto(p, action_256, j),
        13 => happyGoto(p, action_51, j),
        14 => happyGoto(p, action_52, j),
        22 => happyGoto(p, action_53, j),
        23 => happyGoto(p, action_54, j),
        24 => happyGoto(p, action_55, j),
        25 => happyGoto(p, action_56, j),
        26 => happyGoto(p, action_57, j),
        100 => happyGoto(p, action_6, j),
        104 => happyGoto(p, action_7, j),
        106 => happyGoto(p, action_8, j),
        107 => happyGoto(p, action_9, j),
        108 => happyGoto(p, action_10, j),
        109 => happyGoto(p, action_11, j),
        110 => happyGoto(p, action_12, j),
        111 => happyGoto(p, action_13, j),
        112 => happyGoto(p, action_14, j),
        113 => happyGoto(p, action_15, j),
        114 => happyGoto(p, action_16, j),
        115 => happyGoto(p, action_17, j),
        116 => happyGoto(p, action_18, j),
        117 => happyGoto(p, action_19, j),
        118 => happyGoto(p, action_20, j),
        119 => happyGoto(p, action_21, j),
        120 => happyGoto(p, action_22, j),
        122 => happyGoto(p, action_58, j),
        127 => happyGoto(p, action_24, j),
        128 => happyGoto(p, action_25, j),
        131 => happyGoto(p, action_59, j),
        _ => happyFail(p, j)
    }
}

fn action_68(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_255, j),
        _ => happyFail(p, j)
    }
}

fn action_69(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        150 => happyShift(p, action_253, j),
        237 => happyShift(p, action_254, j),
        238 => happyShift(p, action_75, j),
        131 => happyGoto(p, action_252, j),
        _ => happyFail(p, j)
    }
}

fn action_70(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_251, j),
        _ => happyFail(p, j)
    }
}

fn action_71(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_26, j),
        144 => happyShift(p, action_27, j),
        145 => happyShift(p, action_28, j),
        146 => happyShift(p, action_29, j),
        147 => happyShift(p, action_30, j),
        148 => happyShift(p, action_31, j),
        149 => happyShift(p, action_32, j),
        150 => happyShift(p, action_33, j),
        153 => happyShift(p, action_34, j),
        164 => happyShift(p, action_35, j),
        184 => happyShift(p, action_36, j),
        204 => happyShift(p, action_37, j),
        220 => happyShift(p, action_38, j),
        233 => happyShift(p, action_39, j),
        234 => happyShift(p, action_40, j),
        235 => happyShift(p, action_41, j),
        236 => happyShift(p, action_42, j),
        237 => happyShift(p, action_43, j),
        240 => happyShift(p, action_44, j),
        241 => happyShift(p, action_45, j),
        242 => happyShift(p, action_46, j),
        243 => happyShift(p, action_47, j),
        244 => happyShift(p, action_48, j),
        245 => happyShift(p, action_49, j),
        100 => happyGoto(p, action_6, j),
        104 => happyGoto(p, action_7, j),
        106 => happyGoto(p, action_8, j),
        107 => happyGoto(p, action_9, j),
        108 => happyGoto(p, action_10, j),
        109 => happyGoto(p, action_11, j),
        110 => happyGoto(p, action_12, j),
        111 => happyGoto(p, action_13, j),
        112 => happyGoto(p, action_14, j),
        113 => happyGoto(p, action_15, j),
        114 => happyGoto(p, action_16, j),
        115 => happyGoto(p, action_17, j),
        116 => happyGoto(p, action_18, j),
        117 => happyGoto(p, action_19, j),
        118 => happyGoto(p, action_20, j),
        119 => happyGoto(p, action_21, j),
        120 => happyGoto(p, action_22, j),
        122 => happyGoto(p, action_249, j),
        124 => happyGoto(p, action_250, j),
        127 => happyGoto(p, action_24, j),
        128 => happyGoto(p, action_25, j),
        _ => happyReduce_456(p, j)
    }
}

fn action_72(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_248, j),
        _ => happyFail(p, j)
    }
}

fn action_73(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_247, j),
        _ => happyFail(p, j)
    }
}

fn action_74(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        167 => happyReduce_469(p, j),
        _ => happyReduce_360(p, j)
    }
}

fn action_75(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_470(p, j)
    }
}

fn action_76(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        247 => Ok(Cont::Accept(j)),
        _ => happyFail(p, j)
    }
}

fn action_77(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_8(p, j)
    }
}

fn action_78(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        181 => happyShift(p, action_61, j),
        14 => happyGoto(p, action_246, j),
        _ => happyFail(p, j)
    }
}

fn action_79(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_9(p, j)
    }
}

fn action_80(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        179 => happyShift(p, action_244, j),
        180 => happyShift(p, action_245, j),
        _ => happyFail(p, j)
    }
}

fn action_81(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        179 => happyShift(p, action_242, j),
        180 => happyShift(p, action_243, j),
        _ => happyFail(p, j)
    }
}

fn action_82(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_227, j),
        150 => happyShift(p, action_228, j),
        237 => happyShift(p, action_140, j),
        238 => happyShift(p, action_229, j),
        11 => happyGoto(p, action_239, j),
        66 => happyGoto(p, action_240, j),
        68 => happyGoto(p, action_219, j),
        69 => happyGoto(p, action_220, j),
        70 => happyGoto(p, action_221, j),
        71 => happyGoto(p, action_222, j),
        72 => happyGoto(p, action_223, j),
        73 => happyGoto(p, action_224, j),
        75 => happyGoto(p, action_225, j),
        76 => happyGoto(p, action_102, j),
        77 => happyGoto(p, action_103, j),
        78 => happyGoto(p, action_104, j),
        79 => happyGoto(p, action_241, j),
        80 => happyGoto(p, action_106, j),
        81 => happyGoto(p, action_107, j),
        _ => happyFail(p, j)
    }
}

fn action_83(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_111, j),
        150 => happyShift(p, action_112, j),
        185 => happyShift(p, action_113, j),
        186 => happyShift(p, action_171, j),
        188 => happyShift(p, action_115, j),
        190 => happyShift(p, action_116, j),
        192 => happyShift(p, action_117, j),
        193 => happyShift(p, action_172, j),
        195 => happyShift(p, action_118, j),
        198 => happyShift(p, action_119, j),
        200 => happyShift(p, action_120, j),
        201 => happyShift(p, action_121, j),
        202 => happyShift(p, action_122, j),
        207 => happyShift(p, action_123, j),
        208 => happyShift(p, action_124, j),
        209 => happyShift(p, action_125, j),
        210 => happyShift(p, action_126, j),
        212 => happyShift(p, action_127, j),
        213 => happyShift(p, action_173, j),
        214 => happyShift(p, action_174, j),
        215 => happyShift(p, action_128, j),
        216 => happyShift(p, action_175, j),
        218 => happyShift(p, action_129, j),
        219 => happyShift(p, action_130, j),
        221 => happyShift(p, action_131, j),
        223 => happyShift(p, action_133, j),
        225 => happyShift(p, action_134, j),
        226 => happyShift(p, action_237, j),
        227 => happyShift(p, action_136, j),
        228 => happyShift(p, action_137, j),
        229 => happyShift(p, action_138, j),
        230 => happyShift(p, action_139, j),
        231 => happyShift(p, action_176, j),
        237 => happyShift(p, action_140, j),
        238 => happyShift(p, action_238, j),
        239 => happyShift(p, action_142, j),
        11 => happyGoto(p, action_230, j),
        39 => happyGoto(p, action_231, j),
        41 => happyGoto(p, action_198, j),
        42 => happyGoto(p, action_199, j),
        43 => happyGoto(p, action_200, j),
        45 => happyGoto(p, action_232, j),
        52 => happyGoto(p, action_233, j),
        53 => happyGoto(p, action_97, j),
        54 => happyGoto(p, action_98, j),
        61 => happyGoto(p, action_99, j),
        64 => happyGoto(p, action_201, j),
        75 => happyGoto(p, action_234, j),
        76 => happyGoto(p, action_102, j),
        77 => happyGoto(p, action_103, j),
        78 => happyGoto(p, action_104, j),
        79 => happyGoto(p, action_235, j),
        80 => happyGoto(p, action_106, j),
        81 => happyGoto(p, action_107, j),
        134 => happyGoto(p, action_236, j),
        _ => happyFail(p, j)
    }
}

fn action_84(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_104(p, j)
    }
}

fn action_85(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_114(p, j)
    }
}

fn action_86(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_115(p, j)
    }
}

fn action_87(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_116(p, j)
    }
}

fn action_88(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_227, j),
        150 => happyShift(p, action_228, j),
        237 => happyShift(p, action_140, j),
        238 => happyShift(p, action_229, j),
        11 => happyGoto(p, action_217, j),
        66 => happyGoto(p, action_218, j),
        68 => happyGoto(p, action_219, j),
        69 => happyGoto(p, action_220, j),
        70 => happyGoto(p, action_221, j),
        71 => happyGoto(p, action_222, j),
        72 => happyGoto(p, action_223, j),
        73 => happyGoto(p, action_224, j),
        75 => happyGoto(p, action_225, j),
        76 => happyGoto(p, action_102, j),
        77 => happyGoto(p, action_103, j),
        78 => happyGoto(p, action_104, j),
        79 => happyGoto(p, action_226, j),
        80 => happyGoto(p, action_106, j),
        81 => happyGoto(p, action_107, j),
        _ => happyFail(p, j)
    }
}

fn action_89(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_147(p, j)
    }
}

fn action_90(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        185 => happyShift(p, action_113, j),
        186 => happyShift(p, action_171, j),
        188 => happyShift(p, action_115, j),
        190 => happyShift(p, action_116, j),
        192 => happyShift(p, action_117, j),
        193 => happyShift(p, action_172, j),
        195 => happyShift(p, action_118, j),
        198 => happyShift(p, action_119, j),
        201 => happyShift(p, action_121, j),
        202 => happyShift(p, action_122, j),
        207 => happyShift(p, action_123, j),
        208 => happyShift(p, action_124, j),
        209 => happyShift(p, action_125, j),
        210 => happyShift(p, action_126, j),
        212 => happyShift(p, action_127, j),
        213 => happyShift(p, action_173, j),
        214 => happyShift(p, action_174, j),
        215 => happyShift(p, action_128, j),
        216 => happyShift(p, action_175, j),
        218 => happyShift(p, action_129, j),
        219 => happyShift(p, action_130, j),
        221 => happyShift(p, action_131, j),
        225 => happyShift(p, action_134, j),
        227 => happyShift(p, action_136, j),
        229 => happyShift(p, action_138, j),
        230 => happyShift(p, action_139, j),
        231 => happyShift(p, action_176, j),
        239 => happyShift(p, action_142, j),
        39 => happyGoto(p, action_214, j),
        41 => happyGoto(p, action_198, j),
        42 => happyGoto(p, action_199, j),
        43 => happyGoto(p, action_200, j),
        45 => happyGoto(p, action_215, j),
        64 => happyGoto(p, action_201, j),
        134 => happyGoto(p, action_216, j),
        _ => happyReduce_101(p, j)
    }
}

fn action_91(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        186 => happyShift(p, action_171, j),
        188 => happyShift(p, action_115, j),
        190 => happyShift(p, action_116, j),
        192 => happyShift(p, action_117, j),
        193 => happyShift(p, action_172, j),
        195 => happyShift(p, action_118, j),
        198 => happyShift(p, action_119, j),
        201 => happyShift(p, action_121, j),
        202 => happyShift(p, action_122, j),
        208 => happyShift(p, action_124, j),
        209 => happyShift(p, action_125, j),
        210 => happyShift(p, action_126, j),
        213 => happyShift(p, action_173, j),
        214 => happyShift(p, action_174, j),
        215 => happyShift(p, action_128, j),
        216 => happyShift(p, action_175, j),
        218 => happyShift(p, action_129, j),
        219 => happyShift(p, action_130, j),
        221 => happyShift(p, action_131, j),
        225 => happyShift(p, action_134, j),
        227 => happyShift(p, action_136, j),
        229 => happyShift(p, action_138, j),
        230 => happyShift(p, action_139, j),
        231 => happyShift(p, action_176, j),
        239 => happyShift(p, action_142, j),
        41 => happyGoto(p, action_210, j),
        45 => happyGoto(p, action_211, j),
        64 => happyGoto(p, action_212, j),
        134 => happyGoto(p, action_213, j),
        _ => happyReduce_127(p, j)
    }
}

fn action_92(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        180 => happyShift(p, action_209, j),
        185 => happyShift(p, action_113, j),
        186 => happyShift(p, action_171, j),
        188 => happyShift(p, action_115, j),
        193 => happyShift(p, action_172, j),
        201 => happyShift(p, action_121, j),
        207 => happyShift(p, action_123, j),
        212 => happyShift(p, action_127, j),
        213 => happyShift(p, action_173, j),
        214 => happyShift(p, action_174, j),
        215 => happyShift(p, action_128, j),
        216 => happyShift(p, action_175, j),
        221 => happyShift(p, action_131, j),
        225 => happyShift(p, action_134, j),
        227 => happyShift(p, action_136, j),
        231 => happyShift(p, action_176, j),
        239 => happyShift(p, action_142, j),
        39 => happyGoto(p, action_207, j),
        41 => happyGoto(p, action_198, j),
        42 => happyGoto(p, action_199, j),
        43 => happyGoto(p, action_200, j),
        64 => happyGoto(p, action_201, j),
        134 => happyGoto(p, action_208, j),
        _ => happyReduce_102(p, j)
    }
}

fn action_93(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        180 => happyShift(p, action_206, j),
        186 => happyShift(p, action_171, j),
        188 => happyShift(p, action_115, j),
        193 => happyShift(p, action_172, j),
        201 => happyShift(p, action_121, j),
        213 => happyShift(p, action_173, j),
        214 => happyShift(p, action_174, j),
        215 => happyShift(p, action_128, j),
        216 => happyShift(p, action_175, j),
        221 => happyShift(p, action_131, j),
        225 => happyShift(p, action_134, j),
        227 => happyShift(p, action_136, j),
        231 => happyShift(p, action_176, j),
        239 => happyShift(p, action_142, j),
        41 => happyGoto(p, action_203, j),
        64 => happyGoto(p, action_204, j),
        134 => happyGoto(p, action_205, j),
        _ => happyReduce_128(p, j)
    }
}

fn action_94(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        185 => happyShift(p, action_113, j),
        186 => happyShift(p, action_171, j),
        188 => happyShift(p, action_115, j),
        193 => happyShift(p, action_172, j),
        201 => happyShift(p, action_121, j),
        207 => happyShift(p, action_123, j),
        212 => happyShift(p, action_127, j),
        213 => happyShift(p, action_173, j),
        214 => happyShift(p, action_174, j),
        215 => happyShift(p, action_128, j),
        216 => happyShift(p, action_175, j),
        221 => happyShift(p, action_131, j),
        225 => happyShift(p, action_134, j),
        227 => happyShift(p, action_136, j),
        231 => happyShift(p, action_176, j),
        239 => happyShift(p, action_142, j),
        39 => happyGoto(p, action_197, j),
        41 => happyGoto(p, action_198, j),
        42 => happyGoto(p, action_199, j),
        43 => happyGoto(p, action_200, j),
        64 => happyGoto(p, action_201, j),
        134 => happyGoto(p, action_202, j),
        _ => happyReduce_103(p, j)
    }
}

fn action_95(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        186 => happyShift(p, action_171, j),
        188 => happyShift(p, action_115, j),
        193 => happyShift(p, action_172, j),
        201 => happyShift(p, action_121, j),
        213 => happyShift(p, action_173, j),
        214 => happyShift(p, action_174, j),
        215 => happyShift(p, action_128, j),
        216 => happyShift(p, action_175, j),
        221 => happyShift(p, action_131, j),
        225 => happyShift(p, action_134, j),
        227 => happyShift(p, action_136, j),
        231 => happyShift(p, action_176, j),
        239 => happyShift(p, action_142, j),
        41 => happyGoto(p, action_194, j),
        64 => happyGoto(p, action_195, j),
        134 => happyGoto(p, action_196, j),
        _ => happyReduce_129(p, j)
    }
}

fn action_96(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_158(p, j)
    }
}

fn action_97(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_184(p, j)
    }
}

fn action_98(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        239 => happyShift(p, action_142, j),
        132 => happyGoto(p, action_193, j),
        133 => happyGoto(p, action_150, j),
        134 => happyGoto(p, action_110, j),
        _ => happyReduce_471(p, j)
    }
}

fn action_99(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_185(p, j)
    }
}

fn action_100(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_111, j),
        150 => happyShift(p, action_112, j),
        185 => happyShift(p, action_113, j),
        186 => happyShift(p, action_171, j),
        188 => happyShift(p, action_115, j),
        190 => happyShift(p, action_116, j),
        192 => happyShift(p, action_117, j),
        193 => happyShift(p, action_172, j),
        195 => happyShift(p, action_118, j),
        198 => happyShift(p, action_119, j),
        200 => happyShift(p, action_120, j),
        201 => happyShift(p, action_121, j),
        202 => happyShift(p, action_122, j),
        207 => happyShift(p, action_123, j),
        208 => happyShift(p, action_124, j),
        209 => happyShift(p, action_125, j),
        210 => happyShift(p, action_126, j),
        212 => happyShift(p, action_127, j),
        213 => happyShift(p, action_173, j),
        214 => happyShift(p, action_174, j),
        215 => happyShift(p, action_128, j),
        216 => happyShift(p, action_175, j),
        218 => happyShift(p, action_129, j),
        219 => happyShift(p, action_130, j),
        221 => happyShift(p, action_131, j),
        223 => happyShift(p, action_133, j),
        225 => happyShift(p, action_134, j),
        226 => happyShift(p, action_191, j),
        227 => happyShift(p, action_136, j),
        228 => happyShift(p, action_137, j),
        229 => happyShift(p, action_138, j),
        230 => happyShift(p, action_139, j),
        231 => happyShift(p, action_176, j),
        237 => happyShift(p, action_140, j),
        238 => happyShift(p, action_192, j),
        239 => happyShift(p, action_142, j),
        11 => happyGoto(p, action_183, j),
        40 => happyGoto(p, action_184, j),
        41 => happyGoto(p, action_85, j),
        42 => happyGoto(p, action_86, j),
        43 => happyGoto(p, action_87, j),
        45 => happyGoto(p, action_185, j),
        52 => happyGoto(p, action_186, j),
        53 => happyGoto(p, action_97, j),
        54 => happyGoto(p, action_98, j),
        61 => happyGoto(p, action_99, j),
        64 => happyGoto(p, action_187, j),
        75 => happyGoto(p, action_188, j),
        76 => happyGoto(p, action_102, j),
        77 => happyGoto(p, action_103, j),
        78 => happyGoto(p, action_104, j),
        79 => happyGoto(p, action_189, j),
        80 => happyGoto(p, action_106, j),
        81 => happyGoto(p, action_107, j),
        133 => happyGoto(p, action_190, j),
        134 => happyGoto(p, action_110, j),
        _ => happyFail(p, j)
    }
}

fn action_101(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_26(p, j)
    }
}

fn action_102(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_260(p, j)
    }
}

fn action_103(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_262(p, j)
    }
}

fn action_104(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_181, j),
        140 => happyShift(p, action_182, j),
        88 => happyGoto(p, action_178, j),
        89 => happyGoto(p, action_179, j),
        90 => happyGoto(p, action_180, j),
        _ => happyReduce_261(p, j)
    }
}

fn action_105(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        33 => happyGoto(p, action_177, j),
        _ => happyReduce_90(p, j)
    }
}

fn action_106(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_275(p, j)
    }
}

fn action_107(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_276(p, j)
    }
}

fn action_108(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        186 => happyShift(p, action_171, j),
        193 => happyShift(p, action_172, j),
        213 => happyShift(p, action_173, j),
        214 => happyShift(p, action_174, j),
        216 => happyShift(p, action_175, j),
        231 => happyShift(p, action_176, j),
        64 => happyGoto(p, action_170, j),
        _ => happyFail(p, j)
    }
}

fn action_109(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_111, j),
        150 => happyShift(p, action_112, j),
        185 => happyShift(p, action_113, j),
        188 => happyShift(p, action_115, j),
        190 => happyShift(p, action_116, j),
        192 => happyShift(p, action_117, j),
        195 => happyShift(p, action_118, j),
        198 => happyShift(p, action_119, j),
        200 => happyShift(p, action_120, j),
        201 => happyShift(p, action_121, j),
        202 => happyShift(p, action_122, j),
        207 => happyShift(p, action_123, j),
        208 => happyShift(p, action_124, j),
        209 => happyShift(p, action_125, j),
        210 => happyShift(p, action_126, j),
        212 => happyShift(p, action_127, j),
        215 => happyShift(p, action_128, j),
        218 => happyShift(p, action_129, j),
        219 => happyShift(p, action_130, j),
        221 => happyShift(p, action_131, j),
        223 => happyShift(p, action_133, j),
        225 => happyShift(p, action_134, j),
        226 => happyShift(p, action_168, j),
        227 => happyShift(p, action_136, j),
        228 => happyShift(p, action_137, j),
        229 => happyShift(p, action_138, j),
        230 => happyShift(p, action_139, j),
        237 => happyShift(p, action_140, j),
        238 => happyShift(p, action_169, j),
        239 => happyShift(p, action_142, j),
        11 => happyGoto(p, action_161, j),
        40 => happyGoto(p, action_162, j),
        41 => happyGoto(p, action_85, j),
        42 => happyGoto(p, action_86, j),
        43 => happyGoto(p, action_87, j),
        45 => happyGoto(p, action_163, j),
        52 => happyGoto(p, action_164, j),
        53 => happyGoto(p, action_97, j),
        54 => happyGoto(p, action_98, j),
        61 => happyGoto(p, action_99, j),
        75 => happyGoto(p, action_165, j),
        76 => happyGoto(p, action_102, j),
        77 => happyGoto(p, action_103, j),
        78 => happyGoto(p, action_104, j),
        79 => happyGoto(p, action_166, j),
        80 => happyGoto(p, action_106, j),
        81 => happyGoto(p, action_107, j),
        134 => happyGoto(p, action_167, j),
        _ => happyReduce_472(p, j)
    }
}

fn action_110(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_473(p, j)
    }
}

fn action_111(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_111, j),
        150 => happyShift(p, action_112, j),
        237 => happyShift(p, action_140, j),
        239 => happyShift(p, action_142, j),
        76 => happyGoto(p, action_157, j),
        77 => happyGoto(p, action_103, j),
        78 => happyGoto(p, action_158, j),
        80 => happyGoto(p, action_159, j),
        81 => happyGoto(p, action_107, j),
        133 => happyGoto(p, action_160, j),
        134 => happyGoto(p, action_110, j),
        _ => happyFail(p, j)
    }
}

fn action_112(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_111, j),
        150 => happyShift(p, action_112, j),
        237 => happyShift(p, action_140, j),
        239 => happyShift(p, action_142, j),
        65 => happyGoto(p, action_153, j),
        75 => happyGoto(p, action_154, j),
        76 => happyGoto(p, action_102, j),
        77 => happyGoto(p, action_103, j),
        78 => happyGoto(p, action_104, j),
        80 => happyGoto(p, action_155, j),
        81 => happyGoto(p, action_107, j),
        132 => happyGoto(p, action_108, j),
        133 => happyGoto(p, action_156, j),
        134 => happyGoto(p, action_110, j),
        _ => happyReduce_471(p, j)
    }
}

fn action_113(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_152, j),
        _ => happyFail(p, j)
    }
}

fn action_114(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_151, j),
        _ => happyFail(p, j)
    }
}

fn action_115(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_120(p, j)
    }
}

fn action_116(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_139(p, j)
    }
}

fn action_117(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_131(p, j)
    }
}

fn action_118(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_140(p, j)
    }
}

fn action_119(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_136(p, j)
    }
}

fn action_120(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        239 => happyShift(p, action_142, j),
        132 => happyGoto(p, action_149, j),
        133 => happyGoto(p, action_150, j),
        134 => happyGoto(p, action_110, j),
        _ => happyReduce_471(p, j)
    }
}

fn action_121(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_118(p, j)
    }
}

fn action_122(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_135(p, j)
    }
}

fn action_123(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_123(p, j)
    }
}

fn action_124(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_133(p, j)
    }
}

fn action_125(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_141(p, j)
    }
}

fn action_126(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_134(p, j)
    }
}

fn action_127(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_124(p, j)
    }
}

fn action_128(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_121(p, j)
    }
}

fn action_129(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_132(p, j)
    }
}

fn action_130(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_137(p, j)
    }
}

fn action_131(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_119(p, j)
    }
}

fn action_132(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_148, j),
        _ => happyFail(p, j)
    }
}

fn action_133(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_189(p, j)
    }
}

fn action_134(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_117(p, j)
    }
}

fn action_135(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_147, j),
        _ => happyFail(p, j)
    }
}

fn action_136(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_122(p, j)
    }
}

fn action_137(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_190(p, j)
    }
}

fn action_138(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_138(p, j)
    }
}

fn action_139(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_130(p, j)
    }
}

fn action_140(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_272(p, j)
    }
}

fn action_141(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_170(p, j)
    }
}

fn action_142(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_146, j),
        _ => happyFail(p, j)
    }
}

fn action_143(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_111, j),
        150 => happyShift(p, action_112, j),
        185 => happyShift(p, action_113, j),
        187 => happyShift(p, action_114, j),
        188 => happyShift(p, action_115, j),
        190 => happyShift(p, action_116, j),
        192 => happyShift(p, action_117, j),
        195 => happyShift(p, action_118, j),
        198 => happyShift(p, action_119, j),
        200 => happyShift(p, action_120, j),
        201 => happyShift(p, action_121, j),
        202 => happyShift(p, action_122, j),
        207 => happyShift(p, action_123, j),
        208 => happyShift(p, action_124, j),
        209 => happyShift(p, action_125, j),
        210 => happyShift(p, action_126, j),
        212 => happyShift(p, action_127, j),
        215 => happyShift(p, action_128, j),
        218 => happyShift(p, action_129, j),
        219 => happyShift(p, action_130, j),
        221 => happyShift(p, action_131, j),
        222 => happyShift(p, action_132, j),
        223 => happyShift(p, action_133, j),
        225 => happyShift(p, action_134, j),
        226 => happyShift(p, action_135, j),
        227 => happyShift(p, action_136, j),
        228 => happyShift(p, action_137, j),
        229 => happyShift(p, action_138, j),
        230 => happyShift(p, action_139, j),
        237 => happyShift(p, action_140, j),
        238 => happyShift(p, action_141, j),
        239 => happyShift(p, action_142, j),
        240 => happyShift(p, action_143, j),
        9 => happyGoto(p, action_145, j),
        10 => happyGoto(p, action_77, j),
        11 => happyGoto(p, action_78, j),
        32 => happyGoto(p, action_79, j),
        34 => happyGoto(p, action_80, j),
        36 => happyGoto(p, action_81, j),
        37 => happyGoto(p, action_82, j),
        38 => happyGoto(p, action_83, j),
        40 => happyGoto(p, action_84, j),
        41 => happyGoto(p, action_85, j),
        42 => happyGoto(p, action_86, j),
        43 => happyGoto(p, action_87, j),
        44 => happyGoto(p, action_88, j),
        45 => happyGoto(p, action_89, j),
        46 => happyGoto(p, action_90, j),
        47 => happyGoto(p, action_91, j),
        48 => happyGoto(p, action_92, j),
        49 => happyGoto(p, action_93, j),
        50 => happyGoto(p, action_94, j),
        51 => happyGoto(p, action_95, j),
        52 => happyGoto(p, action_96, j),
        53 => happyGoto(p, action_97, j),
        54 => happyGoto(p, action_98, j),
        61 => happyGoto(p, action_99, j),
        65 => happyGoto(p, action_100, j),
        75 => happyGoto(p, action_101, j),
        76 => happyGoto(p, action_102, j),
        77 => happyGoto(p, action_103, j),
        78 => happyGoto(p, action_104, j),
        79 => happyGoto(p, action_105, j),
        80 => happyGoto(p, action_106, j),
        81 => happyGoto(p, action_107, j),
        132 => happyGoto(p, action_108, j),
        133 => happyGoto(p, action_109, j),
        134 => happyGoto(p, action_110, j),
        _ => happyReduce_471(p, j)
    }
}

fn action_144(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        247 => Ok(Cont::Accept(j)),
        _ => happyFail(p, j)
    }
}

fn action_145(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_10(p, j)
    }
}

fn action_146(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_493, j),
        _ => happyFail(p, j)
    }
}

fn action_147(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_26, j),
        144 => happyShift(p, action_27, j),
        145 => happyShift(p, action_28, j),
        146 => happyShift(p, action_29, j),
        147 => happyShift(p, action_30, j),
        148 => happyShift(p, action_31, j),
        149 => happyShift(p, action_32, j),
        150 => happyShift(p, action_33, j),
        153 => happyShift(p, action_34, j),
        164 => happyShift(p, action_35, j),
        184 => happyShift(p, action_36, j),
        190 => happyShift(p, action_116, j),
        192 => happyShift(p, action_117, j),
        195 => happyShift(p, action_118, j),
        198 => happyShift(p, action_119, j),
        200 => happyShift(p, action_120, j),
        202 => happyShift(p, action_122, j),
        204 => happyShift(p, action_37, j),
        208 => happyShift(p, action_124, j),
        209 => happyShift(p, action_125, j),
        210 => happyShift(p, action_126, j),
        218 => happyShift(p, action_129, j),
        219 => happyShift(p, action_130, j),
        220 => happyShift(p, action_38, j),
        223 => happyShift(p, action_133, j),
        226 => happyShift(p, action_135, j),
        228 => happyShift(p, action_137, j),
        229 => happyShift(p, action_138, j),
        230 => happyShift(p, action_139, j),
        233 => happyShift(p, action_39, j),
        234 => happyShift(p, action_40, j),
        235 => happyShift(p, action_41, j),
        236 => happyShift(p, action_42, j),
        237 => happyShift(p, action_43, j),
        238 => happyShift(p, action_141, j),
        239 => happyShift(p, action_142, j),
        240 => happyShift(p, action_44, j),
        241 => happyShift(p, action_45, j),
        242 => happyShift(p, action_46, j),
        243 => happyShift(p, action_47, j),
        244 => happyShift(p, action_48, j),
        245 => happyShift(p, action_49, j),
        44 => happyGoto(p, action_286, j),
        45 => happyGoto(p, action_89, j),
        47 => happyGoto(p, action_287, j),
        49 => happyGoto(p, action_288, j),
        51 => happyGoto(p, action_289, j),
        52 => happyGoto(p, action_96, j),
        53 => happyGoto(p, action_97, j),
        54 => happyGoto(p, action_98, j),
        61 => happyGoto(p, action_99, j),
        65 => happyGoto(p, action_290, j),
        86 => happyGoto(p, action_491, j),
        100 => happyGoto(p, action_6, j),
        104 => happyGoto(p, action_7, j),
        106 => happyGoto(p, action_8, j),
        107 => happyGoto(p, action_9, j),
        108 => happyGoto(p, action_10, j),
        109 => happyGoto(p, action_11, j),
        110 => happyGoto(p, action_12, j),
        111 => happyGoto(p, action_13, j),
        112 => happyGoto(p, action_14, j),
        113 => happyGoto(p, action_15, j),
        114 => happyGoto(p, action_16, j),
        115 => happyGoto(p, action_17, j),
        116 => happyGoto(p, action_18, j),
        117 => happyGoto(p, action_19, j),
        118 => happyGoto(p, action_20, j),
        119 => happyGoto(p, action_21, j),
        120 => happyGoto(p, action_22, j),
        122 => happyGoto(p, action_492, j),
        127 => happyGoto(p, action_24, j),
        128 => happyGoto(p, action_25, j),
        132 => happyGoto(p, action_108, j),
        133 => happyGoto(p, action_293, j),
        134 => happyGoto(p, action_110, j),
        _ => happyReduce_471(p, j)
    }
}

fn action_148(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_26, j),
        144 => happyShift(p, action_27, j),
        145 => happyShift(p, action_28, j),
        146 => happyShift(p, action_29, j),
        147 => happyShift(p, action_30, j),
        148 => happyShift(p, action_31, j),
        149 => happyShift(p, action_32, j),
        150 => happyShift(p, action_33, j),
        153 => happyShift(p, action_34, j),
        164 => happyShift(p, action_35, j),
        184 => happyShift(p, action_36, j),
        204 => happyShift(p, action_37, j),
        220 => happyShift(p, action_38, j),
        233 => happyShift(p, action_39, j),
        234 => happyShift(p, action_40, j),
        235 => happyShift(p, action_41, j),
        236 => happyShift(p, action_42, j),
        237 => happyShift(p, action_43, j),
        240 => happyShift(p, action_44, j),
        241 => happyShift(p, action_45, j),
        242 => happyShift(p, action_46, j),
        243 => happyShift(p, action_47, j),
        244 => happyShift(p, action_48, j),
        245 => happyShift(p, action_49, j),
        100 => happyGoto(p, action_6, j),
        104 => happyGoto(p, action_7, j),
        106 => happyGoto(p, action_259, j),
        107 => happyGoto(p, action_9, j),
        108 => happyGoto(p, action_10, j),
        109 => happyGoto(p, action_11, j),
        110 => happyGoto(p, action_12, j),
        111 => happyGoto(p, action_13, j),
        112 => happyGoto(p, action_14, j),
        113 => happyGoto(p, action_15, j),
        114 => happyGoto(p, action_16, j),
        115 => happyGoto(p, action_17, j),
        116 => happyGoto(p, action_18, j),
        117 => happyGoto(p, action_19, j),
        118 => happyGoto(p, action_20, j),
        119 => happyGoto(p, action_260, j),
        126 => happyGoto(p, action_490, j),
        127 => happyGoto(p, action_24, j),
        128 => happyGoto(p, action_25, j),
        _ => happyFail(p, j)
    }
}

fn action_149(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        181 => happyShift(p, action_489, j),
        237 => happyShift(p, action_254, j),
        238 => happyShift(p, action_75, j),
        131 => happyGoto(p, action_488, j),
        _ => happyFail(p, j)
    }
}

fn action_150(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        239 => happyShift(p, action_142, j),
        134 => happyGoto(p, action_167, j),
        _ => happyReduce_472(p, j)
    }
}

fn action_151(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        236 => happyShift(p, action_42, j),
        128 => happyGoto(p, action_487, j),
        _ => happyFail(p, j)
    }
}

fn action_152(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_26, j),
        144 => happyShift(p, action_27, j),
        145 => happyShift(p, action_28, j),
        146 => happyShift(p, action_29, j),
        147 => happyShift(p, action_30, j),
        148 => happyShift(p, action_31, j),
        149 => happyShift(p, action_32, j),
        150 => happyShift(p, action_33, j),
        153 => happyShift(p, action_34, j),
        164 => happyShift(p, action_35, j),
        184 => happyShift(p, action_36, j),
        190 => happyShift(p, action_116, j),
        192 => happyShift(p, action_117, j),
        195 => happyShift(p, action_118, j),
        198 => happyShift(p, action_119, j),
        200 => happyShift(p, action_120, j),
        202 => happyShift(p, action_122, j),
        204 => happyShift(p, action_37, j),
        208 => happyShift(p, action_124, j),
        209 => happyShift(p, action_125, j),
        210 => happyShift(p, action_126, j),
        218 => happyShift(p, action_129, j),
        219 => happyShift(p, action_130, j),
        220 => happyShift(p, action_38, j),
        223 => happyShift(p, action_133, j),
        226 => happyShift(p, action_135, j),
        228 => happyShift(p, action_137, j),
        229 => happyShift(p, action_138, j),
        230 => happyShift(p, action_139, j),
        233 => happyShift(p, action_39, j),
        234 => happyShift(p, action_40, j),
        235 => happyShift(p, action_41, j),
        236 => happyShift(p, action_42, j),
        237 => happyShift(p, action_43, j),
        238 => happyShift(p, action_141, j),
        239 => happyShift(p, action_142, j),
        240 => happyShift(p, action_44, j),
        241 => happyShift(p, action_45, j),
        242 => happyShift(p, action_46, j),
        243 => happyShift(p, action_47, j),
        244 => happyShift(p, action_48, j),
        245 => happyShift(p, action_49, j),
        44 => happyGoto(p, action_286, j),
        45 => happyGoto(p, action_89, j),
        47 => happyGoto(p, action_287, j),
        49 => happyGoto(p, action_288, j),
        51 => happyGoto(p, action_289, j),
        52 => happyGoto(p, action_96, j),
        53 => happyGoto(p, action_97, j),
        54 => happyGoto(p, action_98, j),
        61 => happyGoto(p, action_99, j),
        65 => happyGoto(p, action_290, j),
        86 => happyGoto(p, action_485, j),
        100 => happyGoto(p, action_6, j),
        104 => happyGoto(p, action_7, j),
        106 => happyGoto(p, action_259, j),
        107 => happyGoto(p, action_9, j),
        108 => happyGoto(p, action_10, j),
        109 => happyGoto(p, action_11, j),
        110 => happyGoto(p, action_12, j),
        111 => happyGoto(p, action_13, j),
        112 => happyGoto(p, action_14, j),
        113 => happyGoto(p, action_15, j),
        114 => happyGoto(p, action_16, j),
        115 => happyGoto(p, action_17, j),
        116 => happyGoto(p, action_18, j),
        117 => happyGoto(p, action_19, j),
        118 => happyGoto(p, action_20, j),
        119 => happyGoto(p, action_260, j),
        126 => happyGoto(p, action_486, j),
        127 => happyGoto(p, action_24, j),
        128 => happyGoto(p, action_25, j),
        132 => happyGoto(p, action_108, j),
        133 => happyGoto(p, action_293, j),
        134 => happyGoto(p, action_110, j),
        _ => happyReduce_471(p, j)
    }
}

fn action_153(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_111, j),
        150 => happyShift(p, action_112, j),
        186 => happyShift(p, action_171, j),
        193 => happyShift(p, action_172, j),
        213 => happyShift(p, action_173, j),
        214 => happyShift(p, action_174, j),
        216 => happyShift(p, action_175, j),
        231 => happyShift(p, action_176, j),
        237 => happyShift(p, action_140, j),
        239 => happyShift(p, action_142, j),
        64 => happyGoto(p, action_187, j),
        75 => happyGoto(p, action_482, j),
        76 => happyGoto(p, action_102, j),
        77 => happyGoto(p, action_103, j),
        78 => happyGoto(p, action_104, j),
        80 => happyGoto(p, action_483, j),
        81 => happyGoto(p, action_107, j),
        133 => happyGoto(p, action_484, j),
        134 => happyGoto(p, action_110, j),
        _ => happyFail(p, j)
    }
}

fn action_154(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_263(p, j)
    }
}

fn action_155(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_277(p, j)
    }
}

fn action_156(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_475, j),
        150 => happyShift(p, action_476, j),
        237 => happyShift(p, action_140, j),
        239 => happyShift(p, action_142, j),
        75 => happyGoto(p, action_480, j),
        76 => happyGoto(p, action_102, j),
        77 => happyGoto(p, action_103, j),
        78 => happyGoto(p, action_481, j),
        134 => happyGoto(p, action_167, j),
        _ => happyReduce_472(p, j)
    }
}

fn action_157(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        139 => happyShift(p, action_479, j),
        _ => happyFail(p, j)
    }
}

fn action_158(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_181, j),
        139 => happyShift(p, action_478, j),
        140 => happyShift(p, action_182, j),
        88 => happyGoto(p, action_178, j),
        89 => happyGoto(p, action_179, j),
        90 => happyGoto(p, action_180, j),
        _ => happyFail(p, j)
    }
}

fn action_159(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        139 => happyShift(p, action_477, j),
        _ => happyFail(p, j)
    }
}

fn action_160(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_475, j),
        150 => happyShift(p, action_476, j),
        237 => happyShift(p, action_140, j),
        239 => happyShift(p, action_142, j),
        76 => happyGoto(p, action_473, j),
        77 => happyGoto(p, action_103, j),
        78 => happyGoto(p, action_474, j),
        134 => happyGoto(p, action_167, j),
        _ => happyFail(p, j)
    }
}

fn action_161(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        181 => happyShift(p, action_61, j),
        14 => happyGoto(p, action_472, j),
        _ => happyFail(p, j)
    }
}

fn action_162(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_105(p, j)
    }
}

fn action_163(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_148(p, j)
    }
}

fn action_164(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_159(p, j)
    }
}

fn action_165(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        181 => happyReduce_26(p, j),
        187 => happyShift(p, action_406, j),
        35 => happyGoto(p, action_471, j),
        67 => happyGoto(p, action_405, j),
        _ => happyReduce_232(p, j)
    }
}

fn action_166(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        33 => happyGoto(p, action_470, j),
        _ => happyReduce_90(p, j)
    }
}

fn action_167(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_474(p, j)
    }
}

fn action_168(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_469, j),
        _ => happyFail(p, j)
    }
}

fn action_169(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_176(p, j)
    }
}

fn action_170(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_227(p, j)
    }
}

fn action_171(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_226(p, j)
    }
}

fn action_172(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_221(p, j)
    }
}

fn action_173(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_224(p, j)
    }
}

fn action_174(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_225(p, j)
    }
}

fn action_175(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_223(p, j)
    }
}

fn action_176(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_222(p, j)
    }
}

fn action_177(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        181 => happyShift(p, action_61, j),
        185 => happyShift(p, action_113, j),
        188 => happyShift(p, action_115, j),
        190 => happyShift(p, action_116, j),
        192 => happyShift(p, action_117, j),
        195 => happyShift(p, action_118, j),
        198 => happyShift(p, action_119, j),
        200 => happyShift(p, action_120, j),
        201 => happyShift(p, action_121, j),
        202 => happyShift(p, action_122, j),
        207 => happyShift(p, action_123, j),
        208 => happyShift(p, action_124, j),
        209 => happyShift(p, action_125, j),
        210 => happyShift(p, action_126, j),
        212 => happyShift(p, action_127, j),
        215 => happyShift(p, action_128, j),
        218 => happyShift(p, action_129, j),
        219 => happyShift(p, action_130, j),
        221 => happyShift(p, action_131, j),
        222 => happyShift(p, action_132, j),
        223 => happyShift(p, action_133, j),
        225 => happyShift(p, action_134, j),
        226 => happyShift(p, action_135, j),
        227 => happyShift(p, action_136, j),
        228 => happyShift(p, action_137, j),
        229 => happyShift(p, action_138, j),
        230 => happyShift(p, action_139, j),
        238 => happyShift(p, action_141, j),
        239 => happyShift(p, action_142, j),
        14 => happyGoto(p, action_462, j),
        32 => happyGoto(p, action_463, j),
        34 => happyGoto(p, action_80, j),
        36 => happyGoto(p, action_81, j),
        37 => happyGoto(p, action_464, j),
        38 => happyGoto(p, action_465, j),
        40 => happyGoto(p, action_84, j),
        41 => happyGoto(p, action_85, j),
        42 => happyGoto(p, action_86, j),
        43 => happyGoto(p, action_87, j),
        44 => happyGoto(p, action_466, j),
        45 => happyGoto(p, action_89, j),
        46 => happyGoto(p, action_90, j),
        47 => happyGoto(p, action_91, j),
        48 => happyGoto(p, action_92, j),
        49 => happyGoto(p, action_93, j),
        50 => happyGoto(p, action_94, j),
        51 => happyGoto(p, action_95, j),
        52 => happyGoto(p, action_96, j),
        53 => happyGoto(p, action_97, j),
        54 => happyGoto(p, action_98, j),
        61 => happyGoto(p, action_99, j),
        65 => happyGoto(p, action_467, j),
        132 => happyGoto(p, action_108, j),
        133 => happyGoto(p, action_468, j),
        134 => happyGoto(p, action_110, j),
        _ => happyReduce_471(p, j)
    }
}

fn action_178(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_267(p, j)
    }
}

fn action_179(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        140 => happyShift(p, action_182, j),
        90 => happyGoto(p, action_461, j),
        _ => happyReduce_311(p, j)
    }
}

fn action_180(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_313(p, j)
    }
}

fn action_181(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        185 => happyShift(p, action_113, j),
        186 => happyReduce_471(p, j),
        188 => happyShift(p, action_115, j),
        190 => happyShift(p, action_116, j),
        192 => happyShift(p, action_117, j),
        193 => happyReduce_471(p, j),
        195 => happyShift(p, action_118, j),
        198 => happyShift(p, action_119, j),
        200 => happyShift(p, action_120, j),
        201 => happyShift(p, action_121, j),
        202 => happyShift(p, action_122, j),
        207 => happyShift(p, action_123, j),
        208 => happyShift(p, action_124, j),
        209 => happyShift(p, action_125, j),
        210 => happyShift(p, action_126, j),
        212 => happyShift(p, action_127, j),
        213 => happyReduce_471(p, j),
        214 => happyReduce_471(p, j),
        215 => happyShift(p, action_128, j),
        216 => happyReduce_471(p, j),
        218 => happyShift(p, action_129, j),
        219 => happyShift(p, action_130, j),
        221 => happyShift(p, action_131, j),
        223 => happyShift(p, action_133, j),
        225 => happyShift(p, action_134, j),
        226 => happyShift(p, action_135, j),
        227 => happyShift(p, action_136, j),
        228 => happyShift(p, action_137, j),
        229 => happyShift(p, action_138, j),
        230 => happyShift(p, action_139, j),
        231 => happyReduce_471(p, j),
        237 => happyShift(p, action_460, j),
        238 => happyShift(p, action_141, j),
        239 => happyShift(p, action_142, j),
        37 => happyGoto(p, action_449, j),
        38 => happyGoto(p, action_450, j),
        40 => happyGoto(p, action_84, j),
        41 => happyGoto(p, action_85, j),
        42 => happyGoto(p, action_86, j),
        43 => happyGoto(p, action_87, j),
        44 => happyGoto(p, action_451, j),
        45 => happyGoto(p, action_89, j),
        46 => happyGoto(p, action_90, j),
        47 => happyGoto(p, action_91, j),
        48 => happyGoto(p, action_452, j),
        49 => happyGoto(p, action_453, j),
        50 => happyGoto(p, action_94, j),
        51 => happyGoto(p, action_95, j),
        52 => happyGoto(p, action_96, j),
        53 => happyGoto(p, action_97, j),
        54 => happyGoto(p, action_98, j),
        61 => happyGoto(p, action_99, j),
        65 => happyGoto(p, action_454, j),
        82 => happyGoto(p, action_455, j),
        83 => happyGoto(p, action_456, j),
        84 => happyGoto(p, action_457, j),
        85 => happyGoto(p, action_458, j),
        132 => happyGoto(p, action_108, j),
        133 => happyGoto(p, action_459, j),
        134 => happyGoto(p, action_110, j),
        _ => happyReduce_282(p, j)
    }
}

fn action_182(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_26, j),
        144 => happyShift(p, action_27, j),
        145 => happyShift(p, action_28, j),
        146 => happyShift(p, action_29, j),
        147 => happyShift(p, action_30, j),
        148 => happyShift(p, action_31, j),
        149 => happyShift(p, action_32, j),
        150 => happyShift(p, action_447, j),
        153 => happyShift(p, action_34, j),
        164 => happyShift(p, action_35, j),
        184 => happyShift(p, action_36, j),
        186 => happyReduce_471(p, j),
        193 => happyReduce_471(p, j),
        204 => happyShift(p, action_37, j),
        213 => happyReduce_471(p, j),
        214 => happyReduce_471(p, j),
        216 => happyReduce_471(p, j),
        220 => happyShift(p, action_38, j),
        221 => happyShift(p, action_448, j),
        231 => happyReduce_471(p, j),
        233 => happyShift(p, action_39, j),
        234 => happyShift(p, action_40, j),
        235 => happyShift(p, action_41, j),
        236 => happyShift(p, action_42, j),
        237 => happyShift(p, action_43, j),
        239 => happyShift(p, action_142, j),
        240 => happyShift(p, action_44, j),
        241 => happyShift(p, action_45, j),
        242 => happyShift(p, action_46, j),
        243 => happyShift(p, action_47, j),
        244 => happyShift(p, action_48, j),
        245 => happyShift(p, action_49, j),
        65 => happyGoto(p, action_443, j),
        100 => happyGoto(p, action_6, j),
        104 => happyGoto(p, action_7, j),
        106 => happyGoto(p, action_8, j),
        107 => happyGoto(p, action_9, j),
        108 => happyGoto(p, action_10, j),
        109 => happyGoto(p, action_11, j),
        110 => happyGoto(p, action_12, j),
        111 => happyGoto(p, action_13, j),
        112 => happyGoto(p, action_14, j),
        113 => happyGoto(p, action_15, j),
        114 => happyGoto(p, action_16, j),
        115 => happyGoto(p, action_17, j),
        116 => happyGoto(p, action_18, j),
        117 => happyGoto(p, action_19, j),
        118 => happyGoto(p, action_20, j),
        119 => happyGoto(p, action_21, j),
        120 => happyGoto(p, action_444, j),
        125 => happyGoto(p, action_445, j),
        127 => happyGoto(p, action_24, j),
        128 => happyGoto(p, action_25, j),
        132 => happyGoto(p, action_108, j),
        133 => happyGoto(p, action_446, j),
        134 => happyGoto(p, action_110, j),
        _ => happyReduce_458(p, j)
    }
}

fn action_183(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        181 => happyShift(p, action_61, j),
        14 => happyGoto(p, action_442, j),
        _ => happyFail(p, j)
    }
}

fn action_184(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_106(p, j)
    }
}

fn action_185(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_149(p, j)
    }
}

fn action_186(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_160(p, j)
    }
}

fn action_187(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_228(p, j)
    }
}

fn action_188(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        181 => happyReduce_26(p, j),
        187 => happyShift(p, action_406, j),
        35 => happyGoto(p, action_441, j),
        67 => happyGoto(p, action_405, j),
        _ => happyReduce_232(p, j)
    }
}

fn action_189(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        33 => happyGoto(p, action_440, j),
        _ => happyReduce_90(p, j)
    }
}

fn action_190(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_111, j),
        150 => happyShift(p, action_112, j),
        185 => happyShift(p, action_113, j),
        186 => happyShift(p, action_171, j),
        188 => happyShift(p, action_115, j),
        190 => happyShift(p, action_116, j),
        192 => happyShift(p, action_117, j),
        193 => happyShift(p, action_172, j),
        195 => happyShift(p, action_118, j),
        198 => happyShift(p, action_119, j),
        200 => happyShift(p, action_120, j),
        201 => happyShift(p, action_121, j),
        202 => happyShift(p, action_122, j),
        207 => happyShift(p, action_123, j),
        208 => happyShift(p, action_124, j),
        209 => happyShift(p, action_125, j),
        210 => happyShift(p, action_126, j),
        212 => happyShift(p, action_127, j),
        213 => happyShift(p, action_173, j),
        214 => happyShift(p, action_174, j),
        215 => happyShift(p, action_128, j),
        216 => happyShift(p, action_175, j),
        218 => happyShift(p, action_129, j),
        219 => happyShift(p, action_130, j),
        221 => happyShift(p, action_131, j),
        223 => happyShift(p, action_133, j),
        225 => happyShift(p, action_134, j),
        226 => happyShift(p, action_438, j),
        227 => happyShift(p, action_136, j),
        228 => happyShift(p, action_137, j),
        229 => happyShift(p, action_138, j),
        230 => happyShift(p, action_139, j),
        231 => happyShift(p, action_176, j),
        237 => happyShift(p, action_140, j),
        238 => happyShift(p, action_439, j),
        239 => happyShift(p, action_142, j),
        11 => happyGoto(p, action_431, j),
        40 => happyGoto(p, action_432, j),
        41 => happyGoto(p, action_85, j),
        42 => happyGoto(p, action_86, j),
        43 => happyGoto(p, action_87, j),
        45 => happyGoto(p, action_433, j),
        52 => happyGoto(p, action_434, j),
        53 => happyGoto(p, action_97, j),
        54 => happyGoto(p, action_98, j),
        61 => happyGoto(p, action_99, j),
        64 => happyGoto(p, action_435, j),
        75 => happyGoto(p, action_436, j),
        76 => happyGoto(p, action_102, j),
        77 => happyGoto(p, action_103, j),
        78 => happyGoto(p, action_104, j),
        79 => happyGoto(p, action_437, j),
        80 => happyGoto(p, action_106, j),
        81 => happyGoto(p, action_107, j),
        134 => happyGoto(p, action_167, j),
        _ => happyFail(p, j)
    }
}

fn action_191(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_430, j),
        _ => happyFail(p, j)
    }
}

fn action_192(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_173(p, j)
    }
}

fn action_193(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        181 => happyShift(p, action_429, j),
        237 => happyShift(p, action_254, j),
        238 => happyShift(p, action_75, j),
        131 => happyGoto(p, action_428, j),
        _ => happyFail(p, j)
    }
}

fn action_194(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_164(p, j)
    }
}

fn action_195(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_182(p, j)
    }
}

fn action_196(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_183(p, j)
    }
}

fn action_197(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_168(p, j)
    }
}

fn action_198(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_110(p, j)
    }
}

fn action_199(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_112(p, j)
    }
}

fn action_200(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_113(p, j)
    }
}

fn action_201(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_111(p, j)
    }
}

fn action_202(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_169(p, j)
    }
}

fn action_203(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_155(p, j)
    }
}

fn action_204(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_162(p, j)
    }
}

fn action_205(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_163(p, j)
    }
}

fn action_206(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_86(p, j)
    }
}

fn action_207(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_156(p, j)
    }
}

fn action_208(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_157(p, j)
    }
}

fn action_209(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_85(p, j)
    }
}

fn action_210(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_143(p, j)
    }
}

fn action_211(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_152(p, j)
    }
}

fn action_212(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_151(p, j)
    }
}

fn action_213(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_153(p, j)
    }
}

fn action_214(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_144(p, j)
    }
}

fn action_215(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_145(p, j)
    }
}

fn action_216(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_146(p, j)
    }
}

fn action_217(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        181 => happyShift(p, action_61, j),
        14 => happyGoto(p, action_427, j),
        _ => happyFail(p, j)
    }
}

fn action_218(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        187 => happyShift(p, action_406, j),
        35 => happyGoto(p, action_426, j),
        67 => happyGoto(p, action_405, j),
        _ => happyReduce_232(p, j)
    }
}

fn action_219(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_231(p, j)
    }
}

fn action_220(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_235(p, j)
    }
}

fn action_221(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_238(p, j)
    }
}

fn action_222(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_239(p, j)
    }
}

fn action_223(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_234(p, j)
    }
}

fn action_224(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_248(p, j)
    }
}

fn action_225(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        181 => happyReduce_26(p, j),
        _ => happyReduce_230(p, j)
    }
}

fn action_226(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        33 => happyGoto(p, action_425, j),
        _ => happyReduce_90(p, j)
    }
}

fn action_227(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_423, j),
        150 => happyShift(p, action_228, j),
        237 => happyShift(p, action_140, j),
        238 => happyShift(p, action_424, j),
        239 => happyShift(p, action_142, j),
        70 => happyGoto(p, action_419, j),
        71 => happyGoto(p, action_222, j),
        72 => happyGoto(p, action_420, j),
        73 => happyGoto(p, action_224, j),
        74 => happyGoto(p, action_421, j),
        76 => happyGoto(p, action_157, j),
        77 => happyGoto(p, action_103, j),
        78 => happyGoto(p, action_158, j),
        80 => happyGoto(p, action_159, j),
        81 => happyGoto(p, action_107, j),
        133 => happyGoto(p, action_422, j),
        134 => happyGoto(p, action_110, j),
        _ => happyFail(p, j)
    }
}

fn action_228(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_418, j),
        150 => happyShift(p, action_228, j),
        237 => happyShift(p, action_140, j),
        238 => happyShift(p, action_229, j),
        239 => happyShift(p, action_142, j),
        65 => happyGoto(p, action_414, j),
        69 => happyGoto(p, action_415, j),
        70 => happyGoto(p, action_221, j),
        71 => happyGoto(p, action_222, j),
        72 => happyGoto(p, action_416, j),
        73 => happyGoto(p, action_224, j),
        75 => happyGoto(p, action_154, j),
        76 => happyGoto(p, action_102, j),
        77 => happyGoto(p, action_103, j),
        78 => happyGoto(p, action_104, j),
        80 => happyGoto(p, action_155, j),
        81 => happyGoto(p, action_107, j),
        132 => happyGoto(p, action_108, j),
        133 => happyGoto(p, action_417, j),
        134 => happyGoto(p, action_110, j),
        _ => happyReduce_471(p, j)
    }
}

fn action_229(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_413, j),
        140 => happyShift(p, action_182, j),
        88 => happyGoto(p, action_412, j),
        89 => happyGoto(p, action_179, j),
        90 => happyGoto(p, action_180, j),
        _ => happyReduce_236(p, j)
    }
}

fn action_230(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        181 => happyShift(p, action_61, j),
        14 => happyGoto(p, action_411, j),
        _ => happyFail(p, j)
    }
}

fn action_231(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_108(p, j)
    }
}

fn action_232(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_142(p, j)
    }
}

fn action_233(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_154(p, j)
    }
}

fn action_234(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        181 => happyReduce_26(p, j),
        187 => happyShift(p, action_406, j),
        35 => happyGoto(p, action_410, j),
        67 => happyGoto(p, action_405, j),
        _ => happyReduce_232(p, j)
    }
}

fn action_235(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        33 => happyGoto(p, action_409, j),
        _ => happyReduce_90(p, j)
    }
}

fn action_236(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_109(p, j)
    }
}

fn action_237(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_408, j),
        _ => happyFail(p, j)
    }
}

fn action_238(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_165(p, j)
    }
}

fn action_239(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        181 => happyShift(p, action_61, j),
        14 => happyGoto(p, action_407, j),
        _ => happyFail(p, j)
    }
}

fn action_240(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        187 => happyShift(p, action_406, j),
        35 => happyGoto(p, action_404, j),
        67 => happyGoto(p, action_405, j),
        _ => happyReduce_232(p, j)
    }
}

fn action_241(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        33 => happyGoto(p, action_403, j),
        _ => happyReduce_90(p, j)
    }
}

fn action_242(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        239 => happyShift(p, action_142, j),
        132 => happyGoto(p, action_402, j),
        133 => happyGoto(p, action_150, j),
        134 => happyGoto(p, action_110, j),
        _ => happyReduce_471(p, j)
    }
}

fn action_243(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_87(p, j)
    }
}

fn action_244(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        239 => happyShift(p, action_142, j),
        132 => happyGoto(p, action_401, j),
        133 => happyGoto(p, action_150, j),
        134 => happyGoto(p, action_110, j),
        _ => happyReduce_471(p, j)
    }
}

fn action_245(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_88(p, j)
    }
}

fn action_246(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_12(p, j)
    }
}

fn action_247(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_26, j),
        144 => happyShift(p, action_27, j),
        145 => happyShift(p, action_28, j),
        146 => happyShift(p, action_29, j),
        147 => happyShift(p, action_30, j),
        148 => happyShift(p, action_31, j),
        149 => happyShift(p, action_32, j),
        150 => happyShift(p, action_33, j),
        153 => happyShift(p, action_34, j),
        164 => happyShift(p, action_35, j),
        184 => happyShift(p, action_36, j),
        204 => happyShift(p, action_37, j),
        220 => happyShift(p, action_38, j),
        233 => happyShift(p, action_39, j),
        234 => happyShift(p, action_40, j),
        235 => happyShift(p, action_41, j),
        236 => happyShift(p, action_42, j),
        237 => happyShift(p, action_43, j),
        240 => happyShift(p, action_44, j),
        241 => happyShift(p, action_45, j),
        242 => happyShift(p, action_46, j),
        243 => happyShift(p, action_47, j),
        244 => happyShift(p, action_48, j),
        245 => happyShift(p, action_49, j),
        100 => happyGoto(p, action_6, j),
        104 => happyGoto(p, action_7, j),
        106 => happyGoto(p, action_8, j),
        107 => happyGoto(p, action_9, j),
        108 => happyGoto(p, action_10, j),
        109 => happyGoto(p, action_11, j),
        110 => happyGoto(p, action_12, j),
        111 => happyGoto(p, action_13, j),
        112 => happyGoto(p, action_14, j),
        113 => happyGoto(p, action_15, j),
        114 => happyGoto(p, action_16, j),
        115 => happyGoto(p, action_17, j),
        116 => happyGoto(p, action_18, j),
        117 => happyGoto(p, action_19, j),
        118 => happyGoto(p, action_20, j),
        119 => happyGoto(p, action_21, j),
        120 => happyGoto(p, action_22, j),
        122 => happyGoto(p, action_400, j),
        127 => happyGoto(p, action_24, j),
        128 => happyGoto(p, action_25, j),
        _ => happyFail(p, j)
    }
}

fn action_248(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_26, j),
        144 => happyShift(p, action_27, j),
        145 => happyShift(p, action_28, j),
        146 => happyShift(p, action_29, j),
        147 => happyShift(p, action_30, j),
        148 => happyShift(p, action_31, j),
        149 => happyShift(p, action_32, j),
        150 => happyShift(p, action_33, j),
        153 => happyShift(p, action_34, j),
        164 => happyShift(p, action_35, j),
        184 => happyShift(p, action_36, j),
        204 => happyShift(p, action_37, j),
        220 => happyShift(p, action_38, j),
        233 => happyShift(p, action_39, j),
        234 => happyShift(p, action_40, j),
        235 => happyShift(p, action_41, j),
        236 => happyShift(p, action_42, j),
        237 => happyShift(p, action_43, j),
        240 => happyShift(p, action_44, j),
        241 => happyShift(p, action_45, j),
        242 => happyShift(p, action_46, j),
        243 => happyShift(p, action_47, j),
        244 => happyShift(p, action_48, j),
        245 => happyShift(p, action_49, j),
        100 => happyGoto(p, action_6, j),
        104 => happyGoto(p, action_7, j),
        106 => happyGoto(p, action_8, j),
        107 => happyGoto(p, action_9, j),
        108 => happyGoto(p, action_10, j),
        109 => happyGoto(p, action_11, j),
        110 => happyGoto(p, action_12, j),
        111 => happyGoto(p, action_13, j),
        112 => happyGoto(p, action_14, j),
        113 => happyGoto(p, action_15, j),
        114 => happyGoto(p, action_16, j),
        115 => happyGoto(p, action_17, j),
        116 => happyGoto(p, action_18, j),
        117 => happyGoto(p, action_19, j),
        118 => happyGoto(p, action_20, j),
        119 => happyGoto(p, action_21, j),
        120 => happyGoto(p, action_22, j),
        122 => happyGoto(p, action_399, j),
        127 => happyGoto(p, action_24, j),
        128 => happyGoto(p, action_25, j),
        _ => happyFail(p, j)
    }
}

fn action_249(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_457(p, j)
    }
}

fn action_250(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        180 => happyShift(p, action_398, j),
        _ => happyFail(p, j)
    }
}

fn action_251(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_26, j),
        144 => happyShift(p, action_27, j),
        145 => happyShift(p, action_28, j),
        146 => happyShift(p, action_29, j),
        147 => happyShift(p, action_30, j),
        148 => happyShift(p, action_31, j),
        149 => happyShift(p, action_32, j),
        150 => happyShift(p, action_33, j),
        153 => happyShift(p, action_34, j),
        164 => happyShift(p, action_35, j),
        184 => happyShift(p, action_36, j),
        204 => happyShift(p, action_37, j),
        220 => happyShift(p, action_38, j),
        233 => happyShift(p, action_39, j),
        234 => happyShift(p, action_40, j),
        235 => happyShift(p, action_41, j),
        236 => happyShift(p, action_42, j),
        237 => happyShift(p, action_43, j),
        240 => happyShift(p, action_44, j),
        241 => happyShift(p, action_45, j),
        242 => happyShift(p, action_46, j),
        243 => happyShift(p, action_47, j),
        244 => happyShift(p, action_48, j),
        245 => happyShift(p, action_49, j),
        100 => happyGoto(p, action_6, j),
        104 => happyGoto(p, action_7, j),
        106 => happyGoto(p, action_8, j),
        107 => happyGoto(p, action_9, j),
        108 => happyGoto(p, action_10, j),
        109 => happyGoto(p, action_11, j),
        110 => happyGoto(p, action_12, j),
        111 => happyGoto(p, action_13, j),
        112 => happyGoto(p, action_14, j),
        113 => happyGoto(p, action_15, j),
        114 => happyGoto(p, action_16, j),
        115 => happyGoto(p, action_17, j),
        116 => happyGoto(p, action_18, j),
        117 => happyGoto(p, action_19, j),
        118 => happyGoto(p, action_20, j),
        119 => happyGoto(p, action_21, j),
        120 => happyGoto(p, action_22, j),
        122 => happyGoto(p, action_397, j),
        127 => happyGoto(p, action_24, j),
        128 => happyGoto(p, action_25, j),
        _ => happyFail(p, j)
    }
}

fn action_252(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        180 => happyShift(p, action_396, j),
        _ => happyFail(p, j)
    }
}

fn action_253(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_26, j),
        144 => happyShift(p, action_27, j),
        145 => happyShift(p, action_28, j),
        146 => happyShift(p, action_29, j),
        147 => happyShift(p, action_30, j),
        148 => happyShift(p, action_31, j),
        149 => happyShift(p, action_32, j),
        150 => happyShift(p, action_33, j),
        153 => happyShift(p, action_34, j),
        164 => happyShift(p, action_35, j),
        184 => happyShift(p, action_36, j),
        204 => happyShift(p, action_37, j),
        220 => happyShift(p, action_38, j),
        233 => happyShift(p, action_39, j),
        234 => happyShift(p, action_40, j),
        235 => happyShift(p, action_41, j),
        236 => happyShift(p, action_42, j),
        237 => happyShift(p, action_43, j),
        240 => happyShift(p, action_44, j),
        241 => happyShift(p, action_45, j),
        242 => happyShift(p, action_46, j),
        243 => happyShift(p, action_47, j),
        244 => happyShift(p, action_48, j),
        245 => happyShift(p, action_49, j),
        100 => happyGoto(p, action_6, j),
        104 => happyGoto(p, action_7, j),
        106 => happyGoto(p, action_8, j),
        107 => happyGoto(p, action_9, j),
        108 => happyGoto(p, action_10, j),
        109 => happyGoto(p, action_11, j),
        110 => happyGoto(p, action_12, j),
        111 => happyGoto(p, action_13, j),
        112 => happyGoto(p, action_14, j),
        113 => happyGoto(p, action_15, j),
        114 => happyGoto(p, action_16, j),
        115 => happyGoto(p, action_17, j),
        116 => happyGoto(p, action_18, j),
        117 => happyGoto(p, action_19, j),
        118 => happyGoto(p, action_20, j),
        119 => happyGoto(p, action_21, j),
        120 => happyGoto(p, action_22, j),
        122 => happyGoto(p, action_395, j),
        127 => happyGoto(p, action_24, j),
        128 => happyGoto(p, action_25, j),
        _ => happyFail(p, j)
    }
}

fn action_254(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_469(p, j)
    }
}

fn action_255(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_26, j),
        144 => happyShift(p, action_27, j),
        145 => happyShift(p, action_28, j),
        146 => happyShift(p, action_29, j),
        147 => happyShift(p, action_30, j),
        148 => happyShift(p, action_31, j),
        149 => happyShift(p, action_32, j),
        150 => happyShift(p, action_33, j),
        153 => happyShift(p, action_34, j),
        164 => happyShift(p, action_35, j),
        184 => happyShift(p, action_36, j),
        185 => happyReduce_40(p, j),
        186 => happyReduce_40(p, j),
        188 => happyReduce_40(p, j),
        190 => happyReduce_40(p, j),
        192 => happyReduce_40(p, j),
        193 => happyReduce_40(p, j),
        195 => happyReduce_40(p, j),
        198 => happyReduce_40(p, j),
        200 => happyReduce_40(p, j),
        201 => happyReduce_40(p, j),
        202 => happyReduce_40(p, j),
        204 => happyShift(p, action_37, j),
        207 => happyReduce_40(p, j),
        208 => happyReduce_40(p, j),
        209 => happyReduce_40(p, j),
        210 => happyReduce_40(p, j),
        212 => happyReduce_40(p, j),
        213 => happyReduce_40(p, j),
        214 => happyReduce_40(p, j),
        215 => happyReduce_40(p, j),
        216 => happyReduce_40(p, j),
        218 => happyReduce_40(p, j),
        219 => happyReduce_40(p, j),
        220 => happyShift(p, action_38, j),
        221 => happyReduce_40(p, j),
        222 => happyReduce_40(p, j),
        223 => happyReduce_40(p, j),
        225 => happyReduce_40(p, j),
        226 => happyReduce_40(p, j),
        227 => happyReduce_40(p, j),
        228 => happyReduce_40(p, j),
        229 => happyReduce_40(p, j),
        230 => happyReduce_40(p, j),
        231 => happyReduce_40(p, j),
        233 => happyShift(p, action_39, j),
        234 => happyShift(p, action_40, j),
        235 => happyShift(p, action_41, j),
        236 => happyShift(p, action_42, j),
        237 => happyShift(p, action_43, j),
        238 => happyReduce_40(p, j),
        239 => happyReduce_40(p, j),
        240 => happyShift(p, action_44, j),
        241 => happyShift(p, action_45, j),
        242 => happyShift(p, action_46, j),
        243 => happyShift(p, action_47, j),
        244 => happyShift(p, action_48, j),
        245 => happyShift(p, action_49, j),
        15 => happyGoto(p, action_393, j),
        100 => happyGoto(p, action_6, j),
        104 => happyGoto(p, action_7, j),
        106 => happyGoto(p, action_8, j),
        107 => happyGoto(p, action_9, j),
        108 => happyGoto(p, action_10, j),
        109 => happyGoto(p, action_11, j),
        110 => happyGoto(p, action_12, j),
        111 => happyGoto(p, action_13, j),
        112 => happyGoto(p, action_14, j),
        113 => happyGoto(p, action_15, j),
        114 => happyGoto(p, action_16, j),
        115 => happyGoto(p, action_17, j),
        116 => happyGoto(p, action_18, j),
        117 => happyGoto(p, action_19, j),
        118 => happyGoto(p, action_20, j),
        119 => happyGoto(p, action_21, j),
        120 => happyGoto(p, action_22, j),
        122 => happyGoto(p, action_249, j),
        124 => happyGoto(p, action_394, j),
        127 => happyGoto(p, action_24, j),
        128 => happyGoto(p, action_25, j),
        _ => happyReduce_456(p, j)
    }
}

fn action_256(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        232 => happyShift(p, action_392, j),
        _ => happyFail(p, j)
    }
}

fn action_257(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_26, j),
        144 => happyShift(p, action_27, j),
        145 => happyShift(p, action_28, j),
        146 => happyShift(p, action_29, j),
        147 => happyShift(p, action_30, j),
        148 => happyShift(p, action_31, j),
        149 => happyShift(p, action_32, j),
        150 => happyShift(p, action_33, j),
        153 => happyShift(p, action_34, j),
        164 => happyShift(p, action_35, j),
        180 => happyShift(p, action_60, j),
        181 => happyShift(p, action_61, j),
        184 => happyShift(p, action_36, j),
        187 => happyShift(p, action_62, j),
        189 => happyShift(p, action_63, j),
        191 => happyShift(p, action_64, j),
        194 => happyShift(p, action_65, j),
        196 => happyShift(p, action_66, j),
        197 => happyShift(p, action_67, j),
        203 => happyShift(p, action_68, j),
        204 => happyShift(p, action_37, j),
        205 => happyShift(p, action_69, j),
        206 => happyShift(p, action_70, j),
        217 => happyShift(p, action_71, j),
        220 => happyShift(p, action_38, j),
        224 => happyShift(p, action_72, j),
        232 => happyShift(p, action_73, j),
        233 => happyShift(p, action_39, j),
        234 => happyShift(p, action_40, j),
        235 => happyShift(p, action_41, j),
        236 => happyShift(p, action_42, j),
        237 => happyShift(p, action_74, j),
        238 => happyShift(p, action_75, j),
        240 => happyShift(p, action_44, j),
        241 => happyShift(p, action_45, j),
        242 => happyShift(p, action_46, j),
        243 => happyShift(p, action_47, j),
        244 => happyShift(p, action_48, j),
        245 => happyShift(p, action_49, j),
        12 => happyGoto(p, action_391, j),
        13 => happyGoto(p, action_51, j),
        14 => happyGoto(p, action_52, j),
        22 => happyGoto(p, action_53, j),
        23 => happyGoto(p, action_54, j),
        24 => happyGoto(p, action_55, j),
        25 => happyGoto(p, action_56, j),
        26 => happyGoto(p, action_57, j),
        100 => happyGoto(p, action_6, j),
        104 => happyGoto(p, action_7, j),
        106 => happyGoto(p, action_8, j),
        107 => happyGoto(p, action_9, j),
        108 => happyGoto(p, action_10, j),
        109 => happyGoto(p, action_11, j),
        110 => happyGoto(p, action_12, j),
        111 => happyGoto(p, action_13, j),
        112 => happyGoto(p, action_14, j),
        113 => happyGoto(p, action_15, j),
        114 => happyGoto(p, action_16, j),
        115 => happyGoto(p, action_17, j),
        116 => happyGoto(p, action_18, j),
        117 => happyGoto(p, action_19, j),
        118 => happyGoto(p, action_20, j),
        119 => happyGoto(p, action_21, j),
        120 => happyGoto(p, action_22, j),
        122 => happyGoto(p, action_58, j),
        127 => happyGoto(p, action_24, j),
        128 => happyGoto(p, action_25, j),
        131 => happyGoto(p, action_59, j),
        _ => happyFail(p, j)
    }
}

fn action_258(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_67(p, j)
    }
}

fn action_259(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_406(p, j)
    }
}

fn action_260(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_460(p, j)
    }
}

fn action_261(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        167 => happyShift(p, action_389, j),
        183 => happyShift(p, action_390, j),
        _ => happyFail(p, j)
    }
}

fn action_262(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_68(p, j)
    }
}

fn action_263(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_388, j),
        _ => happyFail(p, j)
    }
}

fn action_264(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_75(p, j)
    }
}

fn action_265(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        211 => happyShift(p, action_387, j),
        17 => happyGoto(p, action_385, j),
        21 => happyGoto(p, action_386, j),
        _ => happyReduce_42(p, j)
    }
}

fn action_266(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        239 => happyShift(p, action_142, j),
        132 => happyGoto(p, action_384, j),
        133 => happyGoto(p, action_150, j),
        134 => happyGoto(p, action_110, j),
        _ => happyReduce_471(p, j)
    }
}

fn action_267(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_57(p, j)
    }
}

fn action_268(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        190 => happyShift(p, action_116, j),
        192 => happyShift(p, action_117, j),
        195 => happyShift(p, action_118, j),
        198 => happyShift(p, action_119, j),
        200 => happyShift(p, action_120, j),
        202 => happyShift(p, action_122, j),
        208 => happyShift(p, action_124, j),
        209 => happyShift(p, action_125, j),
        210 => happyShift(p, action_126, j),
        218 => happyShift(p, action_129, j),
        219 => happyShift(p, action_130, j),
        223 => happyShift(p, action_133, j),
        226 => happyShift(p, action_135, j),
        228 => happyShift(p, action_137, j),
        229 => happyShift(p, action_138, j),
        230 => happyShift(p, action_139, j),
        238 => happyShift(p, action_141, j),
        239 => happyShift(p, action_142, j),
        44 => happyGoto(p, action_286, j),
        45 => happyGoto(p, action_89, j),
        47 => happyGoto(p, action_287, j),
        49 => happyGoto(p, action_288, j),
        51 => happyGoto(p, action_289, j),
        52 => happyGoto(p, action_96, j),
        53 => happyGoto(p, action_97, j),
        54 => happyGoto(p, action_98, j),
        61 => happyGoto(p, action_99, j),
        65 => happyGoto(p, action_290, j),
        86 => happyGoto(p, action_383, j),
        132 => happyGoto(p, action_108, j),
        133 => happyGoto(p, action_293, j),
        134 => happyGoto(p, action_110, j),
        _ => happyReduce_471(p, j)
    }
}

fn action_269(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        190 => happyShift(p, action_116, j),
        192 => happyShift(p, action_117, j),
        195 => happyShift(p, action_118, j),
        198 => happyShift(p, action_119, j),
        200 => happyShift(p, action_120, j),
        202 => happyShift(p, action_122, j),
        208 => happyShift(p, action_124, j),
        209 => happyShift(p, action_125, j),
        210 => happyShift(p, action_126, j),
        218 => happyShift(p, action_129, j),
        219 => happyShift(p, action_130, j),
        223 => happyShift(p, action_133, j),
        226 => happyShift(p, action_135, j),
        228 => happyShift(p, action_137, j),
        229 => happyShift(p, action_138, j),
        230 => happyShift(p, action_139, j),
        238 => happyShift(p, action_141, j),
        239 => happyShift(p, action_142, j),
        44 => happyGoto(p, action_286, j),
        45 => happyGoto(p, action_89, j),
        47 => happyGoto(p, action_287, j),
        49 => happyGoto(p, action_288, j),
        51 => happyGoto(p, action_289, j),
        52 => happyGoto(p, action_96, j),
        53 => happyGoto(p, action_97, j),
        54 => happyGoto(p, action_98, j),
        61 => happyGoto(p, action_99, j),
        65 => happyGoto(p, action_290, j),
        86 => happyGoto(p, action_382, j),
        132 => happyGoto(p, action_108, j),
        133 => happyGoto(p, action_293, j),
        134 => happyGoto(p, action_110, j),
        _ => happyReduce_471(p, j)
    }
}

fn action_270(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_26, j),
        144 => happyShift(p, action_27, j),
        145 => happyShift(p, action_28, j),
        146 => happyShift(p, action_29, j),
        147 => happyShift(p, action_30, j),
        148 => happyShift(p, action_31, j),
        149 => happyShift(p, action_32, j),
        150 => happyShift(p, action_33, j),
        153 => happyShift(p, action_34, j),
        164 => happyShift(p, action_35, j),
        184 => happyShift(p, action_36, j),
        204 => happyShift(p, action_37, j),
        220 => happyShift(p, action_38, j),
        233 => happyShift(p, action_39, j),
        234 => happyShift(p, action_40, j),
        235 => happyShift(p, action_41, j),
        236 => happyShift(p, action_42, j),
        237 => happyShift(p, action_43, j),
        240 => happyShift(p, action_44, j),
        241 => happyShift(p, action_45, j),
        242 => happyShift(p, action_46, j),
        243 => happyShift(p, action_47, j),
        244 => happyShift(p, action_48, j),
        245 => happyShift(p, action_49, j),
        100 => happyGoto(p, action_6, j),
        104 => happyGoto(p, action_7, j),
        106 => happyGoto(p, action_8, j),
        107 => happyGoto(p, action_9, j),
        108 => happyGoto(p, action_10, j),
        109 => happyGoto(p, action_11, j),
        110 => happyGoto(p, action_12, j),
        111 => happyGoto(p, action_13, j),
        112 => happyGoto(p, action_14, j),
        113 => happyGoto(p, action_15, j),
        114 => happyGoto(p, action_16, j),
        115 => happyGoto(p, action_17, j),
        116 => happyGoto(p, action_18, j),
        117 => happyGoto(p, action_19, j),
        118 => happyGoto(p, action_20, j),
        119 => happyGoto(p, action_21, j),
        120 => happyGoto(p, action_381, j),
        127 => happyGoto(p, action_24, j),
        128 => happyGoto(p, action_25, j),
        _ => happyFail(p, j)
    }
}

fn action_271(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_398(p, j)
    }
}

fn action_272(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_26, j),
        144 => happyShift(p, action_27, j),
        145 => happyShift(p, action_28, j),
        146 => happyShift(p, action_29, j),
        147 => happyShift(p, action_30, j),
        148 => happyShift(p, action_31, j),
        149 => happyShift(p, action_32, j),
        150 => happyShift(p, action_33, j),
        153 => happyShift(p, action_34, j),
        164 => happyShift(p, action_35, j),
        181 => happyShift(p, action_61, j),
        184 => happyShift(p, action_36, j),
        190 => happyShift(p, action_116, j),
        192 => happyShift(p, action_117, j),
        195 => happyShift(p, action_118, j),
        198 => happyShift(p, action_119, j),
        200 => happyShift(p, action_120, j),
        202 => happyShift(p, action_122, j),
        204 => happyShift(p, action_37, j),
        208 => happyShift(p, action_124, j),
        209 => happyShift(p, action_125, j),
        210 => happyShift(p, action_126, j),
        218 => happyShift(p, action_129, j),
        219 => happyShift(p, action_130, j),
        220 => happyShift(p, action_38, j),
        223 => happyShift(p, action_133, j),
        226 => happyShift(p, action_135, j),
        228 => happyShift(p, action_137, j),
        229 => happyShift(p, action_138, j),
        230 => happyShift(p, action_139, j),
        233 => happyShift(p, action_39, j),
        234 => happyShift(p, action_40, j),
        235 => happyShift(p, action_41, j),
        236 => happyShift(p, action_42, j),
        237 => happyShift(p, action_43, j),
        238 => happyShift(p, action_141, j),
        239 => happyShift(p, action_142, j),
        240 => happyShift(p, action_44, j),
        241 => happyShift(p, action_45, j),
        242 => happyShift(p, action_46, j),
        243 => happyShift(p, action_47, j),
        244 => happyShift(p, action_48, j),
        245 => happyShift(p, action_49, j),
        14 => happyGoto(p, action_285, j),
        44 => happyGoto(p, action_286, j),
        45 => happyGoto(p, action_89, j),
        47 => happyGoto(p, action_287, j),
        49 => happyGoto(p, action_288, j),
        51 => happyGoto(p, action_289, j),
        52 => happyGoto(p, action_96, j),
        53 => happyGoto(p, action_97, j),
        54 => happyGoto(p, action_98, j),
        61 => happyGoto(p, action_99, j),
        65 => happyGoto(p, action_290, j),
        86 => happyGoto(p, action_380, j),
        100 => happyGoto(p, action_6, j),
        104 => happyGoto(p, action_7, j),
        106 => happyGoto(p, action_8, j),
        107 => happyGoto(p, action_9, j),
        108 => happyGoto(p, action_10, j),
        109 => happyGoto(p, action_11, j),
        110 => happyGoto(p, action_12, j),
        111 => happyGoto(p, action_13, j),
        112 => happyGoto(p, action_14, j),
        113 => happyGoto(p, action_15, j),
        114 => happyGoto(p, action_16, j),
        115 => happyGoto(p, action_17, j),
        116 => happyGoto(p, action_18, j),
        117 => happyGoto(p, action_19, j),
        118 => happyGoto(p, action_20, j),
        119 => happyGoto(p, action_21, j),
        120 => happyGoto(p, action_22, j),
        122 => happyGoto(p, action_292, j),
        127 => happyGoto(p, action_24, j),
        128 => happyGoto(p, action_25, j),
        132 => happyGoto(p, action_108, j),
        133 => happyGoto(p, action_293, j),
        134 => happyGoto(p, action_110, j),
        _ => happyReduce_471(p, j)
    }
}

fn action_273(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_397(p, j)
    }
}

fn action_274(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_391(p, j)
    }
}

fn action_275(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        236 => happyShift(p, action_379, j),
        _ => happyReduce_465(p, j)
    }
}

fn action_276(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_466(p, j)
    }
}

fn action_277(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_393(p, j)
    }
}

fn action_278(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_26, j),
        144 => happyShift(p, action_27, j),
        145 => happyShift(p, action_28, j),
        146 => happyShift(p, action_29, j),
        147 => happyShift(p, action_30, j),
        148 => happyShift(p, action_31, j),
        149 => happyShift(p, action_32, j),
        150 => happyShift(p, action_33, j),
        153 => happyShift(p, action_34, j),
        164 => happyShift(p, action_35, j),
        181 => happyShift(p, action_61, j),
        184 => happyShift(p, action_36, j),
        190 => happyShift(p, action_116, j),
        192 => happyShift(p, action_117, j),
        195 => happyShift(p, action_118, j),
        198 => happyShift(p, action_119, j),
        200 => happyShift(p, action_120, j),
        202 => happyShift(p, action_122, j),
        204 => happyShift(p, action_37, j),
        208 => happyShift(p, action_124, j),
        209 => happyShift(p, action_125, j),
        210 => happyShift(p, action_126, j),
        218 => happyShift(p, action_129, j),
        219 => happyShift(p, action_130, j),
        220 => happyShift(p, action_38, j),
        223 => happyShift(p, action_133, j),
        226 => happyShift(p, action_135, j),
        228 => happyShift(p, action_137, j),
        229 => happyShift(p, action_138, j),
        230 => happyShift(p, action_139, j),
        233 => happyShift(p, action_39, j),
        234 => happyShift(p, action_40, j),
        235 => happyShift(p, action_41, j),
        236 => happyShift(p, action_42, j),
        237 => happyShift(p, action_43, j),
        238 => happyShift(p, action_141, j),
        239 => happyShift(p, action_142, j),
        240 => happyShift(p, action_44, j),
        241 => happyShift(p, action_45, j),
        242 => happyShift(p, action_46, j),
        243 => happyShift(p, action_47, j),
        244 => happyShift(p, action_48, j),
        245 => happyShift(p, action_49, j),
        14 => happyGoto(p, action_285, j),
        44 => happyGoto(p, action_286, j),
        45 => happyGoto(p, action_89, j),
        47 => happyGoto(p, action_287, j),
        49 => happyGoto(p, action_288, j),
        51 => happyGoto(p, action_289, j),
        52 => happyGoto(p, action_96, j),
        53 => happyGoto(p, action_97, j),
        54 => happyGoto(p, action_98, j),
        61 => happyGoto(p, action_99, j),
        65 => happyGoto(p, action_290, j),
        86 => happyGoto(p, action_378, j),
        100 => happyGoto(p, action_6, j),
        104 => happyGoto(p, action_7, j),
        106 => happyGoto(p, action_8, j),
        107 => happyGoto(p, action_9, j),
        108 => happyGoto(p, action_10, j),
        109 => happyGoto(p, action_11, j),
        110 => happyGoto(p, action_12, j),
        111 => happyGoto(p, action_13, j),
        112 => happyGoto(p, action_14, j),
        113 => happyGoto(p, action_15, j),
        114 => happyGoto(p, action_16, j),
        115 => happyGoto(p, action_17, j),
        116 => happyGoto(p, action_18, j),
        117 => happyGoto(p, action_19, j),
        118 => happyGoto(p, action_20, j),
        119 => happyGoto(p, action_21, j),
        120 => happyGoto(p, action_22, j),
        122 => happyGoto(p, action_292, j),
        127 => happyGoto(p, action_24, j),
        128 => happyGoto(p, action_25, j),
        132 => happyGoto(p, action_108, j),
        133 => happyGoto(p, action_293, j),
        134 => happyGoto(p, action_110, j),
        _ => happyReduce_471(p, j)
    }
}

fn action_279(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_26, j),
        144 => happyShift(p, action_27, j),
        145 => happyShift(p, action_28, j),
        146 => happyShift(p, action_29, j),
        147 => happyShift(p, action_30, j),
        148 => happyShift(p, action_31, j),
        149 => happyShift(p, action_32, j),
        150 => happyShift(p, action_33, j),
        153 => happyShift(p, action_34, j),
        164 => happyShift(p, action_35, j),
        184 => happyShift(p, action_36, j),
        204 => happyShift(p, action_37, j),
        220 => happyShift(p, action_38, j),
        233 => happyShift(p, action_39, j),
        234 => happyShift(p, action_40, j),
        235 => happyShift(p, action_41, j),
        236 => happyShift(p, action_42, j),
        237 => happyShift(p, action_43, j),
        240 => happyShift(p, action_44, j),
        241 => happyShift(p, action_45, j),
        242 => happyShift(p, action_46, j),
        243 => happyShift(p, action_47, j),
        244 => happyShift(p, action_48, j),
        245 => happyShift(p, action_49, j),
        100 => happyGoto(p, action_6, j),
        104 => happyGoto(p, action_7, j),
        106 => happyGoto(p, action_8, j),
        107 => happyGoto(p, action_9, j),
        108 => happyGoto(p, action_10, j),
        109 => happyGoto(p, action_11, j),
        110 => happyGoto(p, action_12, j),
        111 => happyGoto(p, action_13, j),
        112 => happyGoto(p, action_14, j),
        113 => happyGoto(p, action_15, j),
        114 => happyGoto(p, action_16, j),
        115 => happyGoto(p, action_17, j),
        116 => happyGoto(p, action_18, j),
        117 => happyGoto(p, action_19, j),
        118 => happyGoto(p, action_20, j),
        119 => happyGoto(p, action_21, j),
        120 => happyGoto(p, action_377, j),
        127 => happyGoto(p, action_24, j),
        128 => happyGoto(p, action_25, j),
        _ => happyFail(p, j)
    }
}

fn action_280(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_395(p, j)
    }
}

fn action_281(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_26, j),
        144 => happyShift(p, action_27, j),
        145 => happyShift(p, action_28, j),
        146 => happyShift(p, action_29, j),
        147 => happyShift(p, action_30, j),
        148 => happyShift(p, action_31, j),
        149 => happyShift(p, action_32, j),
        150 => happyShift(p, action_33, j),
        153 => happyShift(p, action_34, j),
        164 => happyShift(p, action_35, j),
        181 => happyShift(p, action_61, j),
        184 => happyShift(p, action_36, j),
        190 => happyShift(p, action_116, j),
        192 => happyShift(p, action_117, j),
        195 => happyShift(p, action_118, j),
        198 => happyShift(p, action_119, j),
        200 => happyShift(p, action_120, j),
        202 => happyShift(p, action_122, j),
        204 => happyShift(p, action_37, j),
        208 => happyShift(p, action_124, j),
        209 => happyShift(p, action_125, j),
        210 => happyShift(p, action_126, j),
        218 => happyShift(p, action_129, j),
        219 => happyShift(p, action_130, j),
        220 => happyShift(p, action_38, j),
        223 => happyShift(p, action_133, j),
        226 => happyShift(p, action_135, j),
        228 => happyShift(p, action_137, j),
        229 => happyShift(p, action_138, j),
        230 => happyShift(p, action_139, j),
        233 => happyShift(p, action_39, j),
        234 => happyShift(p, action_40, j),
        235 => happyShift(p, action_41, j),
        236 => happyShift(p, action_42, j),
        237 => happyShift(p, action_43, j),
        238 => happyShift(p, action_141, j),
        239 => happyShift(p, action_142, j),
        240 => happyShift(p, action_44, j),
        241 => happyShift(p, action_45, j),
        242 => happyShift(p, action_46, j),
        243 => happyShift(p, action_47, j),
        244 => happyShift(p, action_48, j),
        245 => happyShift(p, action_49, j),
        14 => happyGoto(p, action_285, j),
        44 => happyGoto(p, action_286, j),
        45 => happyGoto(p, action_89, j),
        47 => happyGoto(p, action_287, j),
        49 => happyGoto(p, action_288, j),
        51 => happyGoto(p, action_289, j),
        52 => happyGoto(p, action_96, j),
        53 => happyGoto(p, action_97, j),
        54 => happyGoto(p, action_98, j),
        61 => happyGoto(p, action_99, j),
        65 => happyGoto(p, action_290, j),
        86 => happyGoto(p, action_376, j),
        100 => happyGoto(p, action_6, j),
        104 => happyGoto(p, action_7, j),
        106 => happyGoto(p, action_8, j),
        107 => happyGoto(p, action_9, j),
        108 => happyGoto(p, action_10, j),
        109 => happyGoto(p, action_11, j),
        110 => happyGoto(p, action_12, j),
        111 => happyGoto(p, action_13, j),
        112 => happyGoto(p, action_14, j),
        113 => happyGoto(p, action_15, j),
        114 => happyGoto(p, action_16, j),
        115 => happyGoto(p, action_17, j),
        116 => happyGoto(p, action_18, j),
        117 => happyGoto(p, action_19, j),
        118 => happyGoto(p, action_20, j),
        119 => happyGoto(p, action_21, j),
        120 => happyGoto(p, action_22, j),
        122 => happyGoto(p, action_292, j),
        127 => happyGoto(p, action_24, j),
        128 => happyGoto(p, action_25, j),
        132 => happyGoto(p, action_108, j),
        133 => happyGoto(p, action_293, j),
        134 => happyGoto(p, action_110, j),
        _ => happyReduce_471(p, j)
    }
}

fn action_282(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_399(p, j)
    }
}

fn action_283(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_390(p, j)
    }
}

fn action_284(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_389(p, j)
    }
}

fn action_285(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        139 => happyShift(p, action_375, j),
        _ => happyFail(p, j)
    }
}

fn action_286(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_372, j),
        140 => happyShift(p, action_182, j),
        150 => happyShift(p, action_373, j),
        87 => happyGoto(p, action_374, j),
        88 => happyGoto(p, action_367, j),
        89 => happyGoto(p, action_179, j),
        90 => happyGoto(p, action_180, j),
        91 => happyGoto(p, action_368, j),
        92 => happyGoto(p, action_369, j),
        _ => happyReduce_304(p, j)
    }
}

fn action_287(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        186 => happyShift(p, action_171, j),
        190 => happyShift(p, action_116, j),
        192 => happyShift(p, action_117, j),
        193 => happyShift(p, action_172, j),
        195 => happyShift(p, action_118, j),
        198 => happyShift(p, action_119, j),
        202 => happyShift(p, action_122, j),
        208 => happyShift(p, action_124, j),
        209 => happyShift(p, action_125, j),
        210 => happyShift(p, action_126, j),
        213 => happyShift(p, action_173, j),
        214 => happyShift(p, action_174, j),
        216 => happyShift(p, action_175, j),
        218 => happyShift(p, action_129, j),
        219 => happyShift(p, action_130, j),
        229 => happyShift(p, action_138, j),
        230 => happyShift(p, action_139, j),
        231 => happyShift(p, action_176, j),
        239 => happyShift(p, action_142, j),
        45 => happyGoto(p, action_211, j),
        64 => happyGoto(p, action_212, j),
        134 => happyGoto(p, action_213, j),
        _ => happyReduce_127(p, j)
    }
}

fn action_288(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        186 => happyShift(p, action_171, j),
        193 => happyShift(p, action_172, j),
        213 => happyShift(p, action_173, j),
        214 => happyShift(p, action_174, j),
        216 => happyShift(p, action_175, j),
        231 => happyShift(p, action_176, j),
        239 => happyShift(p, action_142, j),
        64 => happyGoto(p, action_204, j),
        134 => happyGoto(p, action_205, j),
        _ => happyReduce_128(p, j)
    }
}

fn action_289(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        186 => happyShift(p, action_171, j),
        193 => happyShift(p, action_172, j),
        213 => happyShift(p, action_173, j),
        214 => happyShift(p, action_174, j),
        216 => happyShift(p, action_175, j),
        231 => happyShift(p, action_176, j),
        239 => happyShift(p, action_142, j),
        64 => happyGoto(p, action_195, j),
        134 => happyGoto(p, action_196, j),
        _ => happyReduce_129(p, j)
    }
}

fn action_290(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_372, j),
        140 => happyShift(p, action_182, j),
        150 => happyShift(p, action_373, j),
        186 => happyShift(p, action_171, j),
        190 => happyShift(p, action_116, j),
        192 => happyShift(p, action_117, j),
        193 => happyShift(p, action_172, j),
        195 => happyShift(p, action_118, j),
        198 => happyShift(p, action_119, j),
        200 => happyShift(p, action_120, j),
        202 => happyShift(p, action_122, j),
        208 => happyShift(p, action_124, j),
        209 => happyShift(p, action_125, j),
        210 => happyShift(p, action_126, j),
        213 => happyShift(p, action_173, j),
        214 => happyShift(p, action_174, j),
        216 => happyShift(p, action_175, j),
        218 => happyShift(p, action_129, j),
        219 => happyShift(p, action_130, j),
        223 => happyShift(p, action_133, j),
        226 => happyShift(p, action_191, j),
        228 => happyShift(p, action_137, j),
        229 => happyShift(p, action_138, j),
        230 => happyShift(p, action_139, j),
        231 => happyShift(p, action_176, j),
        238 => happyShift(p, action_192, j),
        239 => happyShift(p, action_142, j),
        45 => happyGoto(p, action_185, j),
        52 => happyGoto(p, action_186, j),
        53 => happyGoto(p, action_97, j),
        54 => happyGoto(p, action_98, j),
        61 => happyGoto(p, action_99, j),
        64 => happyGoto(p, action_187, j),
        87 => happyGoto(p, action_366, j),
        88 => happyGoto(p, action_367, j),
        89 => happyGoto(p, action_179, j),
        90 => happyGoto(p, action_180, j),
        91 => happyGoto(p, action_368, j),
        92 => happyGoto(p, action_369, j),
        133 => happyGoto(p, action_370, j),
        134 => happyGoto(p, action_371, j),
        _ => happyFail(p, j)
    }
}

fn action_291(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        139 => happyShift(p, action_365, j),
        _ => happyFail(p, j)
    }
}

fn action_292(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        139 => happyShift(p, action_364, j),
        _ => happyFail(p, j)
    }
}

fn action_293(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        190 => happyShift(p, action_116, j),
        192 => happyShift(p, action_117, j),
        195 => happyShift(p, action_118, j),
        198 => happyShift(p, action_119, j),
        200 => happyShift(p, action_120, j),
        202 => happyShift(p, action_122, j),
        208 => happyShift(p, action_124, j),
        209 => happyShift(p, action_125, j),
        210 => happyShift(p, action_126, j),
        218 => happyShift(p, action_129, j),
        219 => happyShift(p, action_130, j),
        223 => happyShift(p, action_133, j),
        226 => happyShift(p, action_168, j),
        228 => happyShift(p, action_137, j),
        229 => happyShift(p, action_138, j),
        230 => happyShift(p, action_139, j),
        238 => happyShift(p, action_169, j),
        239 => happyShift(p, action_142, j),
        45 => happyGoto(p, action_163, j),
        52 => happyGoto(p, action_164, j),
        53 => happyGoto(p, action_97, j),
        54 => happyGoto(p, action_98, j),
        61 => happyGoto(p, action_99, j),
        134 => happyGoto(p, action_167, j),
        _ => happyReduce_472(p, j)
    }
}

fn action_294(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_26, j),
        144 => happyShift(p, action_27, j),
        145 => happyShift(p, action_28, j),
        146 => happyShift(p, action_29, j),
        147 => happyShift(p, action_30, j),
        148 => happyShift(p, action_31, j),
        149 => happyShift(p, action_32, j),
        150 => happyShift(p, action_33, j),
        153 => happyShift(p, action_34, j),
        164 => happyShift(p, action_35, j),
        184 => happyShift(p, action_36, j),
        204 => happyShift(p, action_37, j),
        220 => happyShift(p, action_38, j),
        233 => happyShift(p, action_39, j),
        234 => happyShift(p, action_40, j),
        235 => happyShift(p, action_41, j),
        236 => happyShift(p, action_42, j),
        237 => happyShift(p, action_43, j),
        240 => happyShift(p, action_44, j),
        241 => happyShift(p, action_45, j),
        242 => happyShift(p, action_46, j),
        243 => happyShift(p, action_47, j),
        244 => happyShift(p, action_48, j),
        245 => happyShift(p, action_49, j),
        100 => happyGoto(p, action_6, j),
        104 => happyGoto(p, action_7, j),
        106 => happyGoto(p, action_8, j),
        107 => happyGoto(p, action_9, j),
        108 => happyGoto(p, action_10, j),
        109 => happyGoto(p, action_11, j),
        110 => happyGoto(p, action_12, j),
        111 => happyGoto(p, action_13, j),
        112 => happyGoto(p, action_14, j),
        113 => happyGoto(p, action_15, j),
        114 => happyGoto(p, action_16, j),
        115 => happyGoto(p, action_17, j),
        116 => happyGoto(p, action_18, j),
        117 => happyGoto(p, action_19, j),
        118 => happyGoto(p, action_20, j),
        119 => happyGoto(p, action_21, j),
        120 => happyGoto(p, action_362, j),
        123 => happyGoto(p, action_363, j),
        127 => happyGoto(p, action_24, j),
        128 => happyGoto(p, action_25, j),
        _ => happyFail(p, j)
    }
}

fn action_295(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_26, j),
        144 => happyShift(p, action_27, j),
        145 => happyShift(p, action_28, j),
        146 => happyShift(p, action_29, j),
        147 => happyShift(p, action_30, j),
        148 => happyShift(p, action_31, j),
        149 => happyShift(p, action_32, j),
        150 => happyShift(p, action_33, j),
        153 => happyShift(p, action_34, j),
        164 => happyShift(p, action_35, j),
        184 => happyShift(p, action_36, j),
        204 => happyShift(p, action_37, j),
        220 => happyShift(p, action_38, j),
        233 => happyShift(p, action_39, j),
        234 => happyShift(p, action_40, j),
        235 => happyShift(p, action_41, j),
        236 => happyShift(p, action_42, j),
        237 => happyShift(p, action_43, j),
        240 => happyShift(p, action_44, j),
        241 => happyShift(p, action_45, j),
        242 => happyShift(p, action_46, j),
        243 => happyShift(p, action_47, j),
        244 => happyShift(p, action_48, j),
        245 => happyShift(p, action_49, j),
        100 => happyGoto(p, action_6, j),
        104 => happyGoto(p, action_7, j),
        106 => happyGoto(p, action_259, j),
        107 => happyGoto(p, action_9, j),
        108 => happyGoto(p, action_10, j),
        109 => happyGoto(p, action_11, j),
        110 => happyGoto(p, action_12, j),
        111 => happyGoto(p, action_13, j),
        112 => happyGoto(p, action_14, j),
        113 => happyGoto(p, action_15, j),
        114 => happyGoto(p, action_16, j),
        115 => happyGoto(p, action_17, j),
        116 => happyGoto(p, action_18, j),
        117 => happyGoto(p, action_361, j),
        127 => happyGoto(p, action_24, j),
        128 => happyGoto(p, action_25, j),
        _ => happyFail(p, j)
    }
}

fn action_296(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_26, j),
        144 => happyShift(p, action_27, j),
        145 => happyShift(p, action_28, j),
        146 => happyShift(p, action_29, j),
        147 => happyShift(p, action_30, j),
        148 => happyShift(p, action_31, j),
        149 => happyShift(p, action_32, j),
        150 => happyShift(p, action_33, j),
        153 => happyShift(p, action_34, j),
        164 => happyShift(p, action_35, j),
        167 => happyShift(p, action_360, j),
        184 => happyShift(p, action_36, j),
        204 => happyShift(p, action_37, j),
        220 => happyShift(p, action_38, j),
        233 => happyShift(p, action_39, j),
        234 => happyShift(p, action_40, j),
        235 => happyShift(p, action_41, j),
        236 => happyShift(p, action_42, j),
        237 => happyShift(p, action_43, j),
        240 => happyShift(p, action_44, j),
        241 => happyShift(p, action_45, j),
        242 => happyShift(p, action_46, j),
        243 => happyShift(p, action_47, j),
        244 => happyShift(p, action_48, j),
        245 => happyShift(p, action_49, j),
        100 => happyGoto(p, action_6, j),
        104 => happyGoto(p, action_7, j),
        106 => happyGoto(p, action_8, j),
        107 => happyGoto(p, action_9, j),
        108 => happyGoto(p, action_10, j),
        109 => happyGoto(p, action_11, j),
        110 => happyGoto(p, action_12, j),
        111 => happyGoto(p, action_13, j),
        112 => happyGoto(p, action_14, j),
        113 => happyGoto(p, action_15, j),
        114 => happyGoto(p, action_16, j),
        115 => happyGoto(p, action_17, j),
        116 => happyGoto(p, action_18, j),
        117 => happyGoto(p, action_19, j),
        118 => happyGoto(p, action_20, j),
        119 => happyGoto(p, action_21, j),
        120 => happyGoto(p, action_22, j),
        122 => happyGoto(p, action_359, j),
        127 => happyGoto(p, action_24, j),
        128 => happyGoto(p, action_25, j),
        _ => happyFail(p, j)
    }
}

fn action_297(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_26, j),
        144 => happyShift(p, action_27, j),
        145 => happyShift(p, action_28, j),
        146 => happyShift(p, action_29, j),
        147 => happyShift(p, action_30, j),
        148 => happyShift(p, action_31, j),
        149 => happyShift(p, action_32, j),
        150 => happyShift(p, action_33, j),
        153 => happyShift(p, action_34, j),
        164 => happyShift(p, action_35, j),
        184 => happyShift(p, action_36, j),
        204 => happyShift(p, action_37, j),
        220 => happyShift(p, action_38, j),
        233 => happyShift(p, action_39, j),
        234 => happyShift(p, action_40, j),
        235 => happyShift(p, action_41, j),
        236 => happyShift(p, action_42, j),
        237 => happyShift(p, action_43, j),
        240 => happyShift(p, action_44, j),
        241 => happyShift(p, action_45, j),
        242 => happyShift(p, action_46, j),
        243 => happyShift(p, action_47, j),
        244 => happyShift(p, action_48, j),
        245 => happyShift(p, action_49, j),
        100 => happyGoto(p, action_6, j),
        104 => happyGoto(p, action_7, j),
        106 => happyGoto(p, action_259, j),
        107 => happyGoto(p, action_9, j),
        108 => happyGoto(p, action_10, j),
        109 => happyGoto(p, action_11, j),
        110 => happyGoto(p, action_12, j),
        111 => happyGoto(p, action_13, j),
        112 => happyGoto(p, action_14, j),
        113 => happyGoto(p, action_15, j),
        114 => happyGoto(p, action_16, j),
        115 => happyGoto(p, action_17, j),
        116 => happyGoto(p, action_358, j),
        127 => happyGoto(p, action_24, j),
        128 => happyGoto(p, action_25, j),
        _ => happyFail(p, j)
    }
}

fn action_298(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_26, j),
        144 => happyShift(p, action_27, j),
        145 => happyShift(p, action_28, j),
        146 => happyShift(p, action_29, j),
        147 => happyShift(p, action_30, j),
        148 => happyShift(p, action_31, j),
        149 => happyShift(p, action_32, j),
        150 => happyShift(p, action_33, j),
        153 => happyShift(p, action_34, j),
        164 => happyShift(p, action_35, j),
        184 => happyShift(p, action_36, j),
        204 => happyShift(p, action_37, j),
        220 => happyShift(p, action_38, j),
        233 => happyShift(p, action_39, j),
        234 => happyShift(p, action_40, j),
        235 => happyShift(p, action_41, j),
        236 => happyShift(p, action_42, j),
        237 => happyShift(p, action_43, j),
        240 => happyShift(p, action_44, j),
        241 => happyShift(p, action_45, j),
        242 => happyShift(p, action_46, j),
        243 => happyShift(p, action_47, j),
        244 => happyShift(p, action_48, j),
        245 => happyShift(p, action_49, j),
        100 => happyGoto(p, action_6, j),
        104 => happyGoto(p, action_7, j),
        106 => happyGoto(p, action_259, j),
        107 => happyGoto(p, action_9, j),
        108 => happyGoto(p, action_10, j),
        109 => happyGoto(p, action_11, j),
        110 => happyGoto(p, action_12, j),
        111 => happyGoto(p, action_13, j),
        112 => happyGoto(p, action_14, j),
        113 => happyGoto(p, action_15, j),
        114 => happyGoto(p, action_16, j),
        115 => happyGoto(p, action_357, j),
        127 => happyGoto(p, action_24, j),
        128 => happyGoto(p, action_25, j),
        _ => happyFail(p, j)
    }
}

fn action_299(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_26, j),
        144 => happyShift(p, action_27, j),
        145 => happyShift(p, action_28, j),
        146 => happyShift(p, action_29, j),
        147 => happyShift(p, action_30, j),
        148 => happyShift(p, action_31, j),
        149 => happyShift(p, action_32, j),
        150 => happyShift(p, action_33, j),
        153 => happyShift(p, action_34, j),
        164 => happyShift(p, action_35, j),
        184 => happyShift(p, action_36, j),
        204 => happyShift(p, action_37, j),
        220 => happyShift(p, action_38, j),
        233 => happyShift(p, action_39, j),
        234 => happyShift(p, action_40, j),
        235 => happyShift(p, action_41, j),
        236 => happyShift(p, action_42, j),
        237 => happyShift(p, action_43, j),
        240 => happyShift(p, action_44, j),
        241 => happyShift(p, action_45, j),
        242 => happyShift(p, action_46, j),
        243 => happyShift(p, action_47, j),
        244 => happyShift(p, action_48, j),
        245 => happyShift(p, action_49, j),
        100 => happyGoto(p, action_6, j),
        104 => happyGoto(p, action_7, j),
        106 => happyGoto(p, action_259, j),
        107 => happyGoto(p, action_9, j),
        108 => happyGoto(p, action_10, j),
        109 => happyGoto(p, action_11, j),
        110 => happyGoto(p, action_12, j),
        111 => happyGoto(p, action_13, j),
        112 => happyGoto(p, action_14, j),
        113 => happyGoto(p, action_15, j),
        114 => happyGoto(p, action_356, j),
        127 => happyGoto(p, action_24, j),
        128 => happyGoto(p, action_25, j),
        _ => happyFail(p, j)
    }
}

fn action_300(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_26, j),
        144 => happyShift(p, action_27, j),
        145 => happyShift(p, action_28, j),
        146 => happyShift(p, action_29, j),
        147 => happyShift(p, action_30, j),
        148 => happyShift(p, action_31, j),
        149 => happyShift(p, action_32, j),
        150 => happyShift(p, action_33, j),
        153 => happyShift(p, action_34, j),
        164 => happyShift(p, action_35, j),
        184 => happyShift(p, action_36, j),
        204 => happyShift(p, action_37, j),
        220 => happyShift(p, action_38, j),
        233 => happyShift(p, action_39, j),
        234 => happyShift(p, action_40, j),
        235 => happyShift(p, action_41, j),
        236 => happyShift(p, action_42, j),
        237 => happyShift(p, action_43, j),
        240 => happyShift(p, action_44, j),
        241 => happyShift(p, action_45, j),
        242 => happyShift(p, action_46, j),
        243 => happyShift(p, action_47, j),
        244 => happyShift(p, action_48, j),
        245 => happyShift(p, action_49, j),
        100 => happyGoto(p, action_6, j),
        104 => happyGoto(p, action_7, j),
        106 => happyGoto(p, action_259, j),
        107 => happyGoto(p, action_9, j),
        108 => happyGoto(p, action_10, j),
        109 => happyGoto(p, action_11, j),
        110 => happyGoto(p, action_12, j),
        111 => happyGoto(p, action_13, j),
        112 => happyGoto(p, action_14, j),
        113 => happyGoto(p, action_355, j),
        127 => happyGoto(p, action_24, j),
        128 => happyGoto(p, action_25, j),
        _ => happyFail(p, j)
    }
}

fn action_301(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_26, j),
        144 => happyShift(p, action_27, j),
        145 => happyShift(p, action_28, j),
        146 => happyShift(p, action_29, j),
        147 => happyShift(p, action_30, j),
        148 => happyShift(p, action_31, j),
        149 => happyShift(p, action_32, j),
        150 => happyShift(p, action_33, j),
        153 => happyShift(p, action_34, j),
        164 => happyShift(p, action_35, j),
        184 => happyShift(p, action_36, j),
        204 => happyShift(p, action_37, j),
        220 => happyShift(p, action_38, j),
        233 => happyShift(p, action_39, j),
        234 => happyShift(p, action_40, j),
        235 => happyShift(p, action_41, j),
        236 => happyShift(p, action_42, j),
        237 => happyShift(p, action_43, j),
        240 => happyShift(p, action_44, j),
        241 => happyShift(p, action_45, j),
        242 => happyShift(p, action_46, j),
        243 => happyShift(p, action_47, j),
        244 => happyShift(p, action_48, j),
        245 => happyShift(p, action_49, j),
        100 => happyGoto(p, action_6, j),
        104 => happyGoto(p, action_7, j),
        106 => happyGoto(p, action_259, j),
        107 => happyGoto(p, action_9, j),
        108 => happyGoto(p, action_10, j),
        109 => happyGoto(p, action_11, j),
        110 => happyGoto(p, action_12, j),
        111 => happyGoto(p, action_13, j),
        112 => happyGoto(p, action_354, j),
        127 => happyGoto(p, action_24, j),
        128 => happyGoto(p, action_25, j),
        _ => happyFail(p, j)
    }
}

fn action_302(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_26, j),
        144 => happyShift(p, action_27, j),
        145 => happyShift(p, action_28, j),
        146 => happyShift(p, action_29, j),
        147 => happyShift(p, action_30, j),
        148 => happyShift(p, action_31, j),
        149 => happyShift(p, action_32, j),
        150 => happyShift(p, action_33, j),
        153 => happyShift(p, action_34, j),
        164 => happyShift(p, action_35, j),
        184 => happyShift(p, action_36, j),
        204 => happyShift(p, action_37, j),
        220 => happyShift(p, action_38, j),
        233 => happyShift(p, action_39, j),
        234 => happyShift(p, action_40, j),
        235 => happyShift(p, action_41, j),
        236 => happyShift(p, action_42, j),
        237 => happyShift(p, action_43, j),
        240 => happyShift(p, action_44, j),
        241 => happyShift(p, action_45, j),
        242 => happyShift(p, action_46, j),
        243 => happyShift(p, action_47, j),
        244 => happyShift(p, action_48, j),
        245 => happyShift(p, action_49, j),
        100 => happyGoto(p, action_6, j),
        104 => happyGoto(p, action_7, j),
        106 => happyGoto(p, action_259, j),
        107 => happyGoto(p, action_9, j),
        108 => happyGoto(p, action_10, j),
        109 => happyGoto(p, action_11, j),
        110 => happyGoto(p, action_12, j),
        111 => happyGoto(p, action_13, j),
        112 => happyGoto(p, action_353, j),
        127 => happyGoto(p, action_24, j),
        128 => happyGoto(p, action_25, j),
        _ => happyFail(p, j)
    }
}

fn action_303(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_26, j),
        144 => happyShift(p, action_27, j),
        145 => happyShift(p, action_28, j),
        146 => happyShift(p, action_29, j),
        147 => happyShift(p, action_30, j),
        148 => happyShift(p, action_31, j),
        149 => happyShift(p, action_32, j),
        150 => happyShift(p, action_33, j),
        153 => happyShift(p, action_34, j),
        164 => happyShift(p, action_35, j),
        184 => happyShift(p, action_36, j),
        204 => happyShift(p, action_37, j),
        220 => happyShift(p, action_38, j),
        233 => happyShift(p, action_39, j),
        234 => happyShift(p, action_40, j),
        235 => happyShift(p, action_41, j),
        236 => happyShift(p, action_42, j),
        237 => happyShift(p, action_43, j),
        240 => happyShift(p, action_44, j),
        241 => happyShift(p, action_45, j),
        242 => happyShift(p, action_46, j),
        243 => happyShift(p, action_47, j),
        244 => happyShift(p, action_48, j),
        245 => happyShift(p, action_49, j),
        100 => happyGoto(p, action_6, j),
        104 => happyGoto(p, action_7, j),
        106 => happyGoto(p, action_259, j),
        107 => happyGoto(p, action_9, j),
        108 => happyGoto(p, action_10, j),
        109 => happyGoto(p, action_11, j),
        110 => happyGoto(p, action_12, j),
        111 => happyGoto(p, action_352, j),
        127 => happyGoto(p, action_24, j),
        128 => happyGoto(p, action_25, j),
        _ => happyFail(p, j)
    }
}

fn action_304(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_26, j),
        144 => happyShift(p, action_27, j),
        145 => happyShift(p, action_28, j),
        146 => happyShift(p, action_29, j),
        147 => happyShift(p, action_30, j),
        148 => happyShift(p, action_31, j),
        149 => happyShift(p, action_32, j),
        150 => happyShift(p, action_33, j),
        153 => happyShift(p, action_34, j),
        164 => happyShift(p, action_35, j),
        184 => happyShift(p, action_36, j),
        204 => happyShift(p, action_37, j),
        220 => happyShift(p, action_38, j),
        233 => happyShift(p, action_39, j),
        234 => happyShift(p, action_40, j),
        235 => happyShift(p, action_41, j),
        236 => happyShift(p, action_42, j),
        237 => happyShift(p, action_43, j),
        240 => happyShift(p, action_44, j),
        241 => happyShift(p, action_45, j),
        242 => happyShift(p, action_46, j),
        243 => happyShift(p, action_47, j),
        244 => happyShift(p, action_48, j),
        245 => happyShift(p, action_49, j),
        100 => happyGoto(p, action_6, j),
        104 => happyGoto(p, action_7, j),
        106 => happyGoto(p, action_259, j),
        107 => happyGoto(p, action_9, j),
        108 => happyGoto(p, action_10, j),
        109 => happyGoto(p, action_11, j),
        110 => happyGoto(p, action_12, j),
        111 => happyGoto(p, action_351, j),
        127 => happyGoto(p, action_24, j),
        128 => happyGoto(p, action_25, j),
        _ => happyFail(p, j)
    }
}

fn action_305(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_26, j),
        144 => happyShift(p, action_27, j),
        145 => happyShift(p, action_28, j),
        146 => happyShift(p, action_29, j),
        147 => happyShift(p, action_30, j),
        148 => happyShift(p, action_31, j),
        149 => happyShift(p, action_32, j),
        150 => happyShift(p, action_33, j),
        153 => happyShift(p, action_34, j),
        164 => happyShift(p, action_35, j),
        184 => happyShift(p, action_36, j),
        204 => happyShift(p, action_37, j),
        220 => happyShift(p, action_38, j),
        233 => happyShift(p, action_39, j),
        234 => happyShift(p, action_40, j),
        235 => happyShift(p, action_41, j),
        236 => happyShift(p, action_42, j),
        237 => happyShift(p, action_43, j),
        240 => happyShift(p, action_44, j),
        241 => happyShift(p, action_45, j),
        242 => happyShift(p, action_46, j),
        243 => happyShift(p, action_47, j),
        244 => happyShift(p, action_48, j),
        245 => happyShift(p, action_49, j),
        100 => happyGoto(p, action_6, j),
        104 => happyGoto(p, action_7, j),
        106 => happyGoto(p, action_259, j),
        107 => happyGoto(p, action_9, j),
        108 => happyGoto(p, action_10, j),
        109 => happyGoto(p, action_11, j),
        110 => happyGoto(p, action_12, j),
        111 => happyGoto(p, action_350, j),
        127 => happyGoto(p, action_24, j),
        128 => happyGoto(p, action_25, j),
        _ => happyFail(p, j)
    }
}

fn action_306(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_26, j),
        144 => happyShift(p, action_27, j),
        145 => happyShift(p, action_28, j),
        146 => happyShift(p, action_29, j),
        147 => happyShift(p, action_30, j),
        148 => happyShift(p, action_31, j),
        149 => happyShift(p, action_32, j),
        150 => happyShift(p, action_33, j),
        153 => happyShift(p, action_34, j),
        164 => happyShift(p, action_35, j),
        184 => happyShift(p, action_36, j),
        204 => happyShift(p, action_37, j),
        220 => happyShift(p, action_38, j),
        233 => happyShift(p, action_39, j),
        234 => happyShift(p, action_40, j),
        235 => happyShift(p, action_41, j),
        236 => happyShift(p, action_42, j),
        237 => happyShift(p, action_43, j),
        240 => happyShift(p, action_44, j),
        241 => happyShift(p, action_45, j),
        242 => happyShift(p, action_46, j),
        243 => happyShift(p, action_47, j),
        244 => happyShift(p, action_48, j),
        245 => happyShift(p, action_49, j),
        100 => happyGoto(p, action_6, j),
        104 => happyGoto(p, action_7, j),
        106 => happyGoto(p, action_259, j),
        107 => happyGoto(p, action_9, j),
        108 => happyGoto(p, action_10, j),
        109 => happyGoto(p, action_11, j),
        110 => happyGoto(p, action_12, j),
        111 => happyGoto(p, action_349, j),
        127 => happyGoto(p, action_24, j),
        128 => happyGoto(p, action_25, j),
        _ => happyFail(p, j)
    }
}

fn action_307(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_26, j),
        144 => happyShift(p, action_27, j),
        145 => happyShift(p, action_28, j),
        146 => happyShift(p, action_29, j),
        147 => happyShift(p, action_30, j),
        148 => happyShift(p, action_31, j),
        149 => happyShift(p, action_32, j),
        150 => happyShift(p, action_33, j),
        153 => happyShift(p, action_34, j),
        164 => happyShift(p, action_35, j),
        184 => happyShift(p, action_36, j),
        204 => happyShift(p, action_37, j),
        220 => happyShift(p, action_38, j),
        233 => happyShift(p, action_39, j),
        234 => happyShift(p, action_40, j),
        235 => happyShift(p, action_41, j),
        236 => happyShift(p, action_42, j),
        237 => happyShift(p, action_43, j),
        240 => happyShift(p, action_44, j),
        241 => happyShift(p, action_45, j),
        242 => happyShift(p, action_46, j),
        243 => happyShift(p, action_47, j),
        244 => happyShift(p, action_48, j),
        245 => happyShift(p, action_49, j),
        100 => happyGoto(p, action_6, j),
        104 => happyGoto(p, action_7, j),
        106 => happyGoto(p, action_259, j),
        107 => happyGoto(p, action_9, j),
        108 => happyGoto(p, action_10, j),
        109 => happyGoto(p, action_11, j),
        110 => happyGoto(p, action_348, j),
        127 => happyGoto(p, action_24, j),
        128 => happyGoto(p, action_25, j),
        _ => happyFail(p, j)
    }
}

fn action_308(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_26, j),
        144 => happyShift(p, action_27, j),
        145 => happyShift(p, action_28, j),
        146 => happyShift(p, action_29, j),
        147 => happyShift(p, action_30, j),
        148 => happyShift(p, action_31, j),
        149 => happyShift(p, action_32, j),
        150 => happyShift(p, action_33, j),
        153 => happyShift(p, action_34, j),
        164 => happyShift(p, action_35, j),
        184 => happyShift(p, action_36, j),
        204 => happyShift(p, action_37, j),
        220 => happyShift(p, action_38, j),
        233 => happyShift(p, action_39, j),
        234 => happyShift(p, action_40, j),
        235 => happyShift(p, action_41, j),
        236 => happyShift(p, action_42, j),
        237 => happyShift(p, action_43, j),
        240 => happyShift(p, action_44, j),
        241 => happyShift(p, action_45, j),
        242 => happyShift(p, action_46, j),
        243 => happyShift(p, action_47, j),
        244 => happyShift(p, action_48, j),
        245 => happyShift(p, action_49, j),
        100 => happyGoto(p, action_6, j),
        104 => happyGoto(p, action_7, j),
        106 => happyGoto(p, action_259, j),
        107 => happyGoto(p, action_9, j),
        108 => happyGoto(p, action_10, j),
        109 => happyGoto(p, action_11, j),
        110 => happyGoto(p, action_347, j),
        127 => happyGoto(p, action_24, j),
        128 => happyGoto(p, action_25, j),
        _ => happyFail(p, j)
    }
}

fn action_309(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_26, j),
        144 => happyShift(p, action_27, j),
        145 => happyShift(p, action_28, j),
        146 => happyShift(p, action_29, j),
        147 => happyShift(p, action_30, j),
        148 => happyShift(p, action_31, j),
        149 => happyShift(p, action_32, j),
        150 => happyShift(p, action_33, j),
        153 => happyShift(p, action_34, j),
        164 => happyShift(p, action_35, j),
        184 => happyShift(p, action_36, j),
        204 => happyShift(p, action_37, j),
        220 => happyShift(p, action_38, j),
        233 => happyShift(p, action_39, j),
        234 => happyShift(p, action_40, j),
        235 => happyShift(p, action_41, j),
        236 => happyShift(p, action_42, j),
        237 => happyShift(p, action_43, j),
        240 => happyShift(p, action_44, j),
        241 => happyShift(p, action_45, j),
        242 => happyShift(p, action_46, j),
        243 => happyShift(p, action_47, j),
        244 => happyShift(p, action_48, j),
        245 => happyShift(p, action_49, j),
        100 => happyGoto(p, action_6, j),
        104 => happyGoto(p, action_7, j),
        106 => happyGoto(p, action_259, j),
        107 => happyGoto(p, action_9, j),
        108 => happyGoto(p, action_10, j),
        109 => happyGoto(p, action_346, j),
        127 => happyGoto(p, action_24, j),
        128 => happyGoto(p, action_25, j),
        _ => happyFail(p, j)
    }
}

fn action_310(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_26, j),
        144 => happyShift(p, action_27, j),
        145 => happyShift(p, action_28, j),
        146 => happyShift(p, action_29, j),
        147 => happyShift(p, action_30, j),
        148 => happyShift(p, action_31, j),
        149 => happyShift(p, action_32, j),
        150 => happyShift(p, action_33, j),
        153 => happyShift(p, action_34, j),
        164 => happyShift(p, action_35, j),
        184 => happyShift(p, action_36, j),
        204 => happyShift(p, action_37, j),
        220 => happyShift(p, action_38, j),
        233 => happyShift(p, action_39, j),
        234 => happyShift(p, action_40, j),
        235 => happyShift(p, action_41, j),
        236 => happyShift(p, action_42, j),
        237 => happyShift(p, action_43, j),
        240 => happyShift(p, action_44, j),
        241 => happyShift(p, action_45, j),
        242 => happyShift(p, action_46, j),
        243 => happyShift(p, action_47, j),
        244 => happyShift(p, action_48, j),
        245 => happyShift(p, action_49, j),
        100 => happyGoto(p, action_6, j),
        104 => happyGoto(p, action_7, j),
        106 => happyGoto(p, action_259, j),
        107 => happyGoto(p, action_9, j),
        108 => happyGoto(p, action_10, j),
        109 => happyGoto(p, action_345, j),
        127 => happyGoto(p, action_24, j),
        128 => happyGoto(p, action_25, j),
        _ => happyFail(p, j)
    }
}

fn action_311(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_26, j),
        144 => happyShift(p, action_27, j),
        145 => happyShift(p, action_28, j),
        146 => happyShift(p, action_29, j),
        147 => happyShift(p, action_30, j),
        148 => happyShift(p, action_31, j),
        149 => happyShift(p, action_32, j),
        150 => happyShift(p, action_33, j),
        153 => happyShift(p, action_34, j),
        164 => happyShift(p, action_35, j),
        184 => happyShift(p, action_36, j),
        204 => happyShift(p, action_37, j),
        220 => happyShift(p, action_38, j),
        233 => happyShift(p, action_39, j),
        234 => happyShift(p, action_40, j),
        235 => happyShift(p, action_41, j),
        236 => happyShift(p, action_42, j),
        237 => happyShift(p, action_43, j),
        240 => happyShift(p, action_44, j),
        241 => happyShift(p, action_45, j),
        242 => happyShift(p, action_46, j),
        243 => happyShift(p, action_47, j),
        244 => happyShift(p, action_48, j),
        245 => happyShift(p, action_49, j),
        100 => happyGoto(p, action_6, j),
        104 => happyGoto(p, action_7, j),
        106 => happyGoto(p, action_259, j),
        107 => happyGoto(p, action_9, j),
        108 => happyGoto(p, action_344, j),
        127 => happyGoto(p, action_24, j),
        128 => happyGoto(p, action_25, j),
        _ => happyFail(p, j)
    }
}

fn action_312(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_26, j),
        144 => happyShift(p, action_27, j),
        145 => happyShift(p, action_28, j),
        146 => happyShift(p, action_29, j),
        147 => happyShift(p, action_30, j),
        148 => happyShift(p, action_31, j),
        149 => happyShift(p, action_32, j),
        150 => happyShift(p, action_33, j),
        153 => happyShift(p, action_34, j),
        164 => happyShift(p, action_35, j),
        184 => happyShift(p, action_36, j),
        204 => happyShift(p, action_37, j),
        220 => happyShift(p, action_38, j),
        233 => happyShift(p, action_39, j),
        234 => happyShift(p, action_40, j),
        235 => happyShift(p, action_41, j),
        236 => happyShift(p, action_42, j),
        237 => happyShift(p, action_43, j),
        240 => happyShift(p, action_44, j),
        241 => happyShift(p, action_45, j),
        242 => happyShift(p, action_46, j),
        243 => happyShift(p, action_47, j),
        244 => happyShift(p, action_48, j),
        245 => happyShift(p, action_49, j),
        100 => happyGoto(p, action_6, j),
        104 => happyGoto(p, action_7, j),
        106 => happyGoto(p, action_259, j),
        107 => happyGoto(p, action_9, j),
        108 => happyGoto(p, action_343, j),
        127 => happyGoto(p, action_24, j),
        128 => happyGoto(p, action_25, j),
        _ => happyFail(p, j)
    }
}

fn action_313(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_26, j),
        144 => happyShift(p, action_27, j),
        145 => happyShift(p, action_28, j),
        146 => happyShift(p, action_29, j),
        147 => happyShift(p, action_30, j),
        148 => happyShift(p, action_31, j),
        149 => happyShift(p, action_32, j),
        150 => happyShift(p, action_33, j),
        153 => happyShift(p, action_34, j),
        164 => happyShift(p, action_35, j),
        184 => happyShift(p, action_36, j),
        204 => happyShift(p, action_37, j),
        220 => happyShift(p, action_38, j),
        233 => happyShift(p, action_39, j),
        234 => happyShift(p, action_40, j),
        235 => happyShift(p, action_41, j),
        236 => happyShift(p, action_42, j),
        237 => happyShift(p, action_43, j),
        240 => happyShift(p, action_44, j),
        241 => happyShift(p, action_45, j),
        242 => happyShift(p, action_46, j),
        243 => happyShift(p, action_47, j),
        244 => happyShift(p, action_48, j),
        245 => happyShift(p, action_49, j),
        100 => happyGoto(p, action_6, j),
        104 => happyGoto(p, action_7, j),
        106 => happyGoto(p, action_259, j),
        107 => happyGoto(p, action_9, j),
        108 => happyGoto(p, action_342, j),
        127 => happyGoto(p, action_24, j),
        128 => happyGoto(p, action_25, j),
        _ => happyFail(p, j)
    }
}

fn action_314(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_392(p, j)
    }
}

fn action_315(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_26, j),
        144 => happyShift(p, action_27, j),
        145 => happyShift(p, action_28, j),
        146 => happyShift(p, action_29, j),
        147 => happyShift(p, action_30, j),
        148 => happyShift(p, action_31, j),
        149 => happyShift(p, action_32, j),
        150 => happyShift(p, action_33, j),
        153 => happyShift(p, action_34, j),
        164 => happyShift(p, action_35, j),
        184 => happyShift(p, action_36, j),
        204 => happyShift(p, action_37, j),
        220 => happyShift(p, action_38, j),
        233 => happyShift(p, action_39, j),
        234 => happyShift(p, action_40, j),
        235 => happyShift(p, action_41, j),
        236 => happyShift(p, action_42, j),
        237 => happyShift(p, action_43, j),
        240 => happyShift(p, action_44, j),
        241 => happyShift(p, action_45, j),
        242 => happyShift(p, action_46, j),
        243 => happyShift(p, action_47, j),
        244 => happyShift(p, action_48, j),
        245 => happyShift(p, action_49, j),
        100 => happyGoto(p, action_6, j),
        104 => happyGoto(p, action_7, j),
        106 => happyGoto(p, action_8, j),
        107 => happyGoto(p, action_9, j),
        108 => happyGoto(p, action_10, j),
        109 => happyGoto(p, action_11, j),
        110 => happyGoto(p, action_12, j),
        111 => happyGoto(p, action_13, j),
        112 => happyGoto(p, action_14, j),
        113 => happyGoto(p, action_15, j),
        114 => happyGoto(p, action_16, j),
        115 => happyGoto(p, action_17, j),
        116 => happyGoto(p, action_18, j),
        117 => happyGoto(p, action_19, j),
        118 => happyGoto(p, action_20, j),
        119 => happyGoto(p, action_21, j),
        120 => happyGoto(p, action_341, j),
        127 => happyGoto(p, action_24, j),
        128 => happyGoto(p, action_25, j),
        _ => happyFail(p, j)
    }
}

fn action_316(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_441(p, j)
    }
}

fn action_317(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_445(p, j)
    }
}

fn action_318(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_446(p, j)
    }
}

fn action_319(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_442(p, j)
    }
}

fn action_320(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_443(p, j)
    }
}

fn action_321(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_444(p, j)
    }
}

fn action_322(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_449(p, j)
    }
}

fn action_323(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_450(p, j)
    }
}

fn action_324(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_451(p, j)
    }
}

fn action_325(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_447(p, j)
    }
}

fn action_326(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_448(p, j)
    }
}

fn action_327(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_26, j),
        139 => happyShift(p, action_340, j),
        144 => happyShift(p, action_27, j),
        145 => happyShift(p, action_28, j),
        146 => happyShift(p, action_29, j),
        147 => happyShift(p, action_30, j),
        148 => happyShift(p, action_31, j),
        149 => happyShift(p, action_32, j),
        150 => happyShift(p, action_33, j),
        153 => happyShift(p, action_34, j),
        164 => happyShift(p, action_35, j),
        184 => happyShift(p, action_36, j),
        204 => happyShift(p, action_37, j),
        220 => happyShift(p, action_38, j),
        233 => happyShift(p, action_39, j),
        234 => happyShift(p, action_40, j),
        235 => happyShift(p, action_41, j),
        236 => happyShift(p, action_42, j),
        237 => happyShift(p, action_43, j),
        240 => happyShift(p, action_44, j),
        241 => happyShift(p, action_45, j),
        242 => happyShift(p, action_46, j),
        243 => happyShift(p, action_47, j),
        244 => happyShift(p, action_48, j),
        245 => happyShift(p, action_49, j),
        100 => happyGoto(p, action_6, j),
        104 => happyGoto(p, action_7, j),
        105 => happyGoto(p, action_338, j),
        106 => happyGoto(p, action_8, j),
        107 => happyGoto(p, action_9, j),
        108 => happyGoto(p, action_10, j),
        109 => happyGoto(p, action_11, j),
        110 => happyGoto(p, action_12, j),
        111 => happyGoto(p, action_13, j),
        112 => happyGoto(p, action_14, j),
        113 => happyGoto(p, action_15, j),
        114 => happyGoto(p, action_16, j),
        115 => happyGoto(p, action_17, j),
        116 => happyGoto(p, action_18, j),
        117 => happyGoto(p, action_19, j),
        118 => happyGoto(p, action_20, j),
        119 => happyGoto(p, action_21, j),
        120 => happyGoto(p, action_339, j),
        127 => happyGoto(p, action_24, j),
        128 => happyGoto(p, action_25, j),
        _ => happyFail(p, j)
    }
}

fn action_328(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_26, j),
        144 => happyShift(p, action_27, j),
        145 => happyShift(p, action_28, j),
        146 => happyShift(p, action_29, j),
        147 => happyShift(p, action_30, j),
        148 => happyShift(p, action_31, j),
        149 => happyShift(p, action_32, j),
        150 => happyShift(p, action_33, j),
        153 => happyShift(p, action_34, j),
        164 => happyShift(p, action_35, j),
        184 => happyShift(p, action_36, j),
        204 => happyShift(p, action_37, j),
        220 => happyShift(p, action_38, j),
        233 => happyShift(p, action_39, j),
        234 => happyShift(p, action_40, j),
        235 => happyShift(p, action_41, j),
        236 => happyShift(p, action_42, j),
        237 => happyShift(p, action_43, j),
        240 => happyShift(p, action_44, j),
        241 => happyShift(p, action_45, j),
        242 => happyShift(p, action_46, j),
        243 => happyShift(p, action_47, j),
        244 => happyShift(p, action_48, j),
        245 => happyShift(p, action_49, j),
        100 => happyGoto(p, action_6, j),
        104 => happyGoto(p, action_7, j),
        106 => happyGoto(p, action_8, j),
        107 => happyGoto(p, action_9, j),
        108 => happyGoto(p, action_10, j),
        109 => happyGoto(p, action_11, j),
        110 => happyGoto(p, action_12, j),
        111 => happyGoto(p, action_13, j),
        112 => happyGoto(p, action_14, j),
        113 => happyGoto(p, action_15, j),
        114 => happyGoto(p, action_16, j),
        115 => happyGoto(p, action_17, j),
        116 => happyGoto(p, action_18, j),
        117 => happyGoto(p, action_19, j),
        118 => happyGoto(p, action_20, j),
        119 => happyGoto(p, action_21, j),
        120 => happyGoto(p, action_22, j),
        122 => happyGoto(p, action_337, j),
        127 => happyGoto(p, action_24, j),
        128 => happyGoto(p, action_25, j),
        _ => happyFail(p, j)
    }
}

fn action_329(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        237 => happyShift(p, action_254, j),
        238 => happyShift(p, action_75, j),
        131 => happyGoto(p, action_336, j),
        _ => happyFail(p, j)
    }
}

fn action_330(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        237 => happyShift(p, action_254, j),
        238 => happyShift(p, action_75, j),
        131 => happyGoto(p, action_335, j),
        _ => happyFail(p, j)
    }
}

fn action_331(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_382(p, j)
    }
}

fn action_332(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_383(p, j)
    }
}

fn action_333(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_7(p, j)
    }
}

fn action_334(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_6(p, j)
    }
}

fn action_335(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_380(p, j)
    }
}

fn action_336(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_381(p, j)
    }
}

fn action_337(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        141 => happyShift(p, action_640, j),
        _ => happyFail(p, j)
    }
}

fn action_338(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        139 => happyShift(p, action_638, j),
        179 => happyShift(p, action_639, j),
        _ => happyFail(p, j)
    }
}

fn action_339(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_386(p, j)
    }
}

fn action_340(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_378(p, j)
    }
}

fn action_341(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_440(p, j)
    }
}

fn action_342(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_411(p, j)
    }
}

fn action_343(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_410(p, j)
    }
}

fn action_344(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_409(p, j)
    }
}

fn action_345(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        150 => happyShift(p, action_311, j),
        151 => happyShift(p, action_312, j),
        152 => happyShift(p, action_313, j),
        _ => happyReduce_414(p, j)
    }
}

fn action_346(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        150 => happyShift(p, action_311, j),
        151 => happyShift(p, action_312, j),
        152 => happyShift(p, action_313, j),
        _ => happyReduce_413(p, j)
    }
}

fn action_347(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        148 => happyShift(p, action_309, j),
        149 => happyShift(p, action_310, j),
        _ => happyReduce_417(p, j)
    }
}

fn action_348(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        148 => happyShift(p, action_309, j),
        149 => happyShift(p, action_310, j),
        _ => happyReduce_416(p, j)
    }
}

fn action_349(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        154 => happyShift(p, action_307, j),
        155 => happyShift(p, action_308, j),
        _ => happyReduce_422(p, j)
    }
}

fn action_350(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        154 => happyShift(p, action_307, j),
        155 => happyShift(p, action_308, j),
        _ => happyReduce_420(p, j)
    }
}

fn action_351(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        154 => happyShift(p, action_307, j),
        155 => happyShift(p, action_308, j),
        _ => happyReduce_421(p, j)
    }
}

fn action_352(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        154 => happyShift(p, action_307, j),
        155 => happyShift(p, action_308, j),
        _ => happyReduce_419(p, j)
    }
}

fn action_353(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        156 => happyShift(p, action_303, j),
        157 => happyShift(p, action_304, j),
        158 => happyShift(p, action_305, j),
        159 => happyShift(p, action_306, j),
        _ => happyReduce_425(p, j)
    }
}

fn action_354(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        156 => happyShift(p, action_303, j),
        157 => happyShift(p, action_304, j),
        158 => happyShift(p, action_305, j),
        159 => happyShift(p, action_306, j),
        _ => happyReduce_424(p, j)
    }
}

fn action_355(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        160 => happyShift(p, action_301, j),
        161 => happyShift(p, action_302, j),
        _ => happyReduce_427(p, j)
    }
}

fn action_356(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        153 => happyShift(p, action_300, j),
        _ => happyReduce_429(p, j)
    }
}

fn action_357(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        162 => happyShift(p, action_299, j),
        _ => happyReduce_431(p, j)
    }
}

fn action_358(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        163 => happyShift(p, action_298, j),
        _ => happyReduce_433(p, j)
    }
}

fn action_359(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        167 => happyShift(p, action_637, j),
        _ => happyFail(p, j)
    }
}

fn action_360(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_26, j),
        144 => happyShift(p, action_27, j),
        145 => happyShift(p, action_28, j),
        146 => happyShift(p, action_29, j),
        147 => happyShift(p, action_30, j),
        148 => happyShift(p, action_31, j),
        149 => happyShift(p, action_32, j),
        150 => happyShift(p, action_33, j),
        153 => happyShift(p, action_34, j),
        164 => happyShift(p, action_35, j),
        184 => happyShift(p, action_36, j),
        204 => happyShift(p, action_37, j),
        220 => happyShift(p, action_38, j),
        233 => happyShift(p, action_39, j),
        234 => happyShift(p, action_40, j),
        235 => happyShift(p, action_41, j),
        236 => happyShift(p, action_42, j),
        237 => happyShift(p, action_43, j),
        240 => happyShift(p, action_44, j),
        241 => happyShift(p, action_45, j),
        242 => happyShift(p, action_46, j),
        243 => happyShift(p, action_47, j),
        244 => happyShift(p, action_48, j),
        245 => happyShift(p, action_49, j),
        100 => happyGoto(p, action_6, j),
        104 => happyGoto(p, action_7, j),
        106 => happyGoto(p, action_259, j),
        107 => happyGoto(p, action_9, j),
        108 => happyGoto(p, action_10, j),
        109 => happyGoto(p, action_11, j),
        110 => happyGoto(p, action_12, j),
        111 => happyGoto(p, action_13, j),
        112 => happyGoto(p, action_14, j),
        113 => happyGoto(p, action_15, j),
        114 => happyGoto(p, action_16, j),
        115 => happyGoto(p, action_17, j),
        116 => happyGoto(p, action_18, j),
        117 => happyGoto(p, action_19, j),
        118 => happyGoto(p, action_20, j),
        119 => happyGoto(p, action_636, j),
        127 => happyGoto(p, action_24, j),
        128 => happyGoto(p, action_25, j),
        _ => happyFail(p, j)
    }
}

fn action_361(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        164 => happyShift(p, action_297, j),
        _ => happyReduce_435(p, j)
    }
}

fn action_362(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_454(p, j)
    }
}

fn action_363(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        179 => happyShift(p, action_635, j),
        _ => happyReduce_453(p, j)
    }
}

fn action_364(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_363(p, j)
    }
}

fn action_365(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_26, j),
        144 => happyShift(p, action_27, j),
        145 => happyShift(p, action_28, j),
        146 => happyShift(p, action_29, j),
        147 => happyShift(p, action_30, j),
        148 => happyShift(p, action_31, j),
        149 => happyShift(p, action_32, j),
        150 => happyShift(p, action_33, j),
        153 => happyShift(p, action_34, j),
        164 => happyShift(p, action_35, j),
        181 => happyShift(p, action_634, j),
        184 => happyShift(p, action_36, j),
        204 => happyShift(p, action_37, j),
        220 => happyShift(p, action_38, j),
        233 => happyShift(p, action_39, j),
        234 => happyShift(p, action_40, j),
        235 => happyShift(p, action_41, j),
        236 => happyShift(p, action_42, j),
        237 => happyShift(p, action_43, j),
        240 => happyShift(p, action_44, j),
        241 => happyShift(p, action_45, j),
        242 => happyShift(p, action_46, j),
        243 => happyShift(p, action_47, j),
        244 => happyShift(p, action_48, j),
        245 => happyShift(p, action_49, j),
        100 => happyGoto(p, action_6, j),
        104 => happyGoto(p, action_7, j),
        106 => happyGoto(p, action_259, j),
        107 => happyGoto(p, action_9, j),
        108 => happyGoto(p, action_633, j),
        127 => happyGoto(p, action_24, j),
        128 => happyGoto(p, action_25, j),
        _ => happyFail(p, j)
    }
}

fn action_366(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_307(p, j)
    }
}

fn action_367(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_310(p, j)
    }
}

fn action_368(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_308(p, j)
    }
}

fn action_369(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        239 => happyShift(p, action_142, j),
        134 => happyGoto(p, action_632, j),
        _ => happyReduce_309(p, j)
    }
}

fn action_370(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        186 => happyShift(p, action_171, j),
        190 => happyShift(p, action_116, j),
        192 => happyShift(p, action_117, j),
        193 => happyShift(p, action_172, j),
        195 => happyShift(p, action_118, j),
        198 => happyShift(p, action_119, j),
        200 => happyShift(p, action_120, j),
        202 => happyShift(p, action_122, j),
        208 => happyShift(p, action_124, j),
        209 => happyShift(p, action_125, j),
        210 => happyShift(p, action_126, j),
        213 => happyShift(p, action_173, j),
        214 => happyShift(p, action_174, j),
        216 => happyShift(p, action_175, j),
        218 => happyShift(p, action_129, j),
        219 => happyShift(p, action_130, j),
        223 => happyShift(p, action_133, j),
        226 => happyShift(p, action_438, j),
        228 => happyShift(p, action_137, j),
        229 => happyShift(p, action_138, j),
        230 => happyShift(p, action_139, j),
        231 => happyShift(p, action_176, j),
        238 => happyShift(p, action_439, j),
        239 => happyShift(p, action_142, j),
        45 => happyGoto(p, action_433, j),
        52 => happyGoto(p, action_434, j),
        53 => happyGoto(p, action_97, j),
        54 => happyGoto(p, action_98, j),
        61 => happyGoto(p, action_99, j),
        64 => happyGoto(p, action_435, j),
        134 => happyGoto(p, action_167, j),
        _ => happyFail(p, j)
    }
}

fn action_371(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        186 => happyReduce_473(p, j),
        190 => happyReduce_473(p, j),
        192 => happyReduce_473(p, j),
        193 => happyReduce_473(p, j),
        195 => happyReduce_473(p, j),
        198 => happyReduce_473(p, j),
        200 => happyReduce_473(p, j),
        202 => happyReduce_473(p, j),
        208 => happyReduce_473(p, j),
        209 => happyReduce_473(p, j),
        210 => happyReduce_473(p, j),
        213 => happyReduce_473(p, j),
        214 => happyReduce_473(p, j),
        216 => happyReduce_473(p, j),
        218 => happyReduce_473(p, j),
        219 => happyReduce_473(p, j),
        223 => happyReduce_473(p, j),
        226 => happyReduce_473(p, j),
        228 => happyReduce_473(p, j),
        229 => happyReduce_473(p, j),
        230 => happyReduce_473(p, j),
        231 => happyReduce_473(p, j),
        238 => happyReduce_473(p, j),
        239 => happyReduce_473(p, j),
        _ => happyReduce_306(p, j)
    }
}

fn action_372(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_372, j),
        140 => happyShift(p, action_182, j),
        150 => happyShift(p, action_373, j),
        185 => happyShift(p, action_113, j),
        186 => happyReduce_471(p, j),
        188 => happyShift(p, action_115, j),
        190 => happyShift(p, action_116, j),
        192 => happyShift(p, action_117, j),
        193 => happyReduce_471(p, j),
        195 => happyShift(p, action_118, j),
        198 => happyShift(p, action_119, j),
        200 => happyShift(p, action_120, j),
        201 => happyShift(p, action_121, j),
        202 => happyShift(p, action_122, j),
        207 => happyShift(p, action_123, j),
        208 => happyShift(p, action_124, j),
        209 => happyShift(p, action_125, j),
        210 => happyShift(p, action_126, j),
        212 => happyShift(p, action_127, j),
        213 => happyReduce_471(p, j),
        214 => happyReduce_471(p, j),
        215 => happyShift(p, action_128, j),
        216 => happyReduce_471(p, j),
        218 => happyShift(p, action_129, j),
        219 => happyShift(p, action_130, j),
        221 => happyShift(p, action_131, j),
        223 => happyShift(p, action_133, j),
        225 => happyShift(p, action_134, j),
        226 => happyShift(p, action_135, j),
        227 => happyShift(p, action_136, j),
        228 => happyShift(p, action_137, j),
        229 => happyShift(p, action_138, j),
        230 => happyShift(p, action_139, j),
        231 => happyReduce_471(p, j),
        238 => happyShift(p, action_141, j),
        239 => happyShift(p, action_142, j),
        37 => happyGoto(p, action_449, j),
        38 => happyGoto(p, action_450, j),
        40 => happyGoto(p, action_84, j),
        41 => happyGoto(p, action_85, j),
        42 => happyGoto(p, action_86, j),
        43 => happyGoto(p, action_87, j),
        44 => happyGoto(p, action_451, j),
        45 => happyGoto(p, action_89, j),
        46 => happyGoto(p, action_90, j),
        47 => happyGoto(p, action_91, j),
        48 => happyGoto(p, action_452, j),
        49 => happyGoto(p, action_453, j),
        50 => happyGoto(p, action_94, j),
        51 => happyGoto(p, action_95, j),
        52 => happyGoto(p, action_96, j),
        53 => happyGoto(p, action_97, j),
        54 => happyGoto(p, action_98, j),
        61 => happyGoto(p, action_99, j),
        65 => happyGoto(p, action_454, j),
        82 => happyGoto(p, action_455, j),
        83 => happyGoto(p, action_456, j),
        84 => happyGoto(p, action_457, j),
        88 => happyGoto(p, action_628, j),
        89 => happyGoto(p, action_179, j),
        90 => happyGoto(p, action_180, j),
        91 => happyGoto(p, action_629, j),
        92 => happyGoto(p, action_630, j),
        132 => happyGoto(p, action_108, j),
        133 => happyGoto(p, action_631, j),
        134 => happyGoto(p, action_110, j),
        _ => happyReduce_282(p, j)
    }
}

fn action_373(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_372, j),
        140 => happyShift(p, action_182, j),
        150 => happyShift(p, action_373, j),
        186 => happyReduce_471(p, j),
        193 => happyReduce_471(p, j),
        213 => happyReduce_471(p, j),
        214 => happyReduce_471(p, j),
        216 => happyReduce_471(p, j),
        231 => happyReduce_471(p, j),
        239 => happyShift(p, action_142, j),
        65 => happyGoto(p, action_625, j),
        87 => happyGoto(p, action_626, j),
        88 => happyGoto(p, action_367, j),
        89 => happyGoto(p, action_179, j),
        90 => happyGoto(p, action_180, j),
        91 => happyGoto(p, action_368, j),
        92 => happyGoto(p, action_369, j),
        132 => happyGoto(p, action_108, j),
        133 => happyGoto(p, action_627, j),
        134 => happyGoto(p, action_110, j),
        _ => happyReduce_326(p, j)
    }
}

fn action_374(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_305(p, j)
    }
}

fn action_375(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_365(p, j)
    }
}

fn action_376(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        139 => happyShift(p, action_624, j),
        _ => happyFail(p, j)
    }
}

fn action_377(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        179 => happyShift(p, action_623, j),
        _ => happyFail(p, j)
    }
}

fn action_378(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        139 => happyShift(p, action_622, j),
        _ => happyFail(p, j)
    }
}

fn action_379(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_467(p, j)
    }
}

fn action_380(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        139 => happyShift(p, action_621, j),
        _ => happyFail(p, j)
    }
}

fn action_381(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        179 => happyShift(p, action_620, j),
        _ => happyFail(p, j)
    }
}

fn action_382(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        179 => happyShift(p, action_619, j),
        _ => happyFail(p, j)
    }
}

fn action_383(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        179 => happyShift(p, action_618, j),
        _ => happyFail(p, j)
    }
}

fn action_384(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_26, j),
        144 => happyShift(p, action_27, j),
        145 => happyShift(p, action_28, j),
        146 => happyShift(p, action_29, j),
        147 => happyShift(p, action_30, j),
        148 => happyShift(p, action_31, j),
        149 => happyShift(p, action_32, j),
        150 => happyShift(p, action_33, j),
        153 => happyShift(p, action_34, j),
        164 => happyShift(p, action_35, j),
        180 => happyShift(p, action_60, j),
        181 => happyShift(p, action_61, j),
        184 => happyShift(p, action_36, j),
        187 => happyShift(p, action_62, j),
        189 => happyShift(p, action_63, j),
        191 => happyShift(p, action_64, j),
        194 => happyShift(p, action_65, j),
        196 => happyShift(p, action_66, j),
        197 => happyShift(p, action_67, j),
        203 => happyShift(p, action_68, j),
        204 => happyShift(p, action_37, j),
        205 => happyShift(p, action_69, j),
        206 => happyShift(p, action_70, j),
        217 => happyShift(p, action_71, j),
        220 => happyShift(p, action_38, j),
        224 => happyShift(p, action_72, j),
        232 => happyShift(p, action_73, j),
        233 => happyShift(p, action_39, j),
        234 => happyShift(p, action_40, j),
        235 => happyShift(p, action_41, j),
        236 => happyShift(p, action_42, j),
        237 => happyShift(p, action_74, j),
        238 => happyShift(p, action_75, j),
        240 => happyShift(p, action_44, j),
        241 => happyShift(p, action_45, j),
        242 => happyShift(p, action_46, j),
        243 => happyShift(p, action_47, j),
        244 => happyShift(p, action_48, j),
        245 => happyShift(p, action_49, j),
        12 => happyGoto(p, action_617, j),
        13 => happyGoto(p, action_51, j),
        14 => happyGoto(p, action_52, j),
        22 => happyGoto(p, action_53, j),
        23 => happyGoto(p, action_54, j),
        24 => happyGoto(p, action_55, j),
        25 => happyGoto(p, action_56, j),
        26 => happyGoto(p, action_57, j),
        100 => happyGoto(p, action_6, j),
        104 => happyGoto(p, action_7, j),
        106 => happyGoto(p, action_8, j),
        107 => happyGoto(p, action_9, j),
        108 => happyGoto(p, action_10, j),
        109 => happyGoto(p, action_11, j),
        110 => happyGoto(p, action_12, j),
        111 => happyGoto(p, action_13, j),
        112 => happyGoto(p, action_14, j),
        113 => happyGoto(p, action_15, j),
        114 => happyGoto(p, action_16, j),
        115 => happyGoto(p, action_17, j),
        116 => happyGoto(p, action_18, j),
        117 => happyGoto(p, action_19, j),
        118 => happyGoto(p, action_20, j),
        119 => happyGoto(p, action_21, j),
        120 => happyGoto(p, action_22, j),
        122 => happyGoto(p, action_58, j),
        127 => happyGoto(p, action_24, j),
        128 => happyGoto(p, action_25, j),
        131 => happyGoto(p, action_59, j),
        _ => happyFail(p, j)
    }
}

fn action_385(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_26, j),
        144 => happyShift(p, action_27, j),
        145 => happyShift(p, action_28, j),
        146 => happyShift(p, action_29, j),
        147 => happyShift(p, action_30, j),
        148 => happyShift(p, action_31, j),
        149 => happyShift(p, action_32, j),
        150 => happyShift(p, action_33, j),
        153 => happyShift(p, action_34, j),
        164 => happyShift(p, action_35, j),
        180 => happyShift(p, action_60, j),
        181 => happyShift(p, action_61, j),
        184 => happyShift(p, action_36, j),
        185 => happyShift(p, action_113, j),
        186 => happyReduce_471(p, j),
        187 => happyShift(p, action_62, j),
        188 => happyShift(p, action_115, j),
        189 => happyShift(p, action_63, j),
        190 => happyShift(p, action_116, j),
        191 => happyShift(p, action_64, j),
        192 => happyShift(p, action_117, j),
        193 => happyReduce_471(p, j),
        194 => happyShift(p, action_65, j),
        195 => happyShift(p, action_118, j),
        196 => happyShift(p, action_66, j),
        197 => happyShift(p, action_67, j),
        198 => happyShift(p, action_119, j),
        200 => happyShift(p, action_120, j),
        201 => happyShift(p, action_121, j),
        202 => happyShift(p, action_122, j),
        203 => happyShift(p, action_68, j),
        204 => happyShift(p, action_37, j),
        205 => happyShift(p, action_69, j),
        206 => happyShift(p, action_70, j),
        207 => happyShift(p, action_123, j),
        208 => happyShift(p, action_124, j),
        209 => happyShift(p, action_125, j),
        210 => happyShift(p, action_126, j),
        212 => happyShift(p, action_127, j),
        213 => happyReduce_471(p, j),
        214 => happyReduce_471(p, j),
        215 => happyShift(p, action_128, j),
        216 => happyReduce_471(p, j),
        217 => happyShift(p, action_71, j),
        218 => happyShift(p, action_129, j),
        219 => happyShift(p, action_130, j),
        220 => happyShift(p, action_38, j),
        221 => happyShift(p, action_131, j),
        222 => happyShift(p, action_132, j),
        223 => happyShift(p, action_133, j),
        224 => happyShift(p, action_72, j),
        225 => happyShift(p, action_134, j),
        226 => happyShift(p, action_135, j),
        227 => happyShift(p, action_136, j),
        228 => happyShift(p, action_137, j),
        229 => happyShift(p, action_138, j),
        230 => happyShift(p, action_139, j),
        231 => happyReduce_471(p, j),
        232 => happyShift(p, action_73, j),
        233 => happyShift(p, action_39, j),
        234 => happyShift(p, action_40, j),
        235 => happyShift(p, action_41, j),
        236 => happyShift(p, action_42, j),
        237 => happyShift(p, action_74, j),
        238 => happyShift(p, action_615, j),
        239 => happyShift(p, action_142, j),
        240 => happyShift(p, action_616, j),
        241 => happyShift(p, action_45, j),
        242 => happyShift(p, action_46, j),
        243 => happyShift(p, action_47, j),
        244 => happyShift(p, action_48, j),
        245 => happyShift(p, action_49, j),
        12 => happyGoto(p, action_605, j),
        13 => happyGoto(p, action_51, j),
        14 => happyGoto(p, action_52, j),
        16 => happyGoto(p, action_606, j),
        18 => happyGoto(p, action_607, j),
        19 => happyGoto(p, action_608, j),
        20 => happyGoto(p, action_609, j),
        22 => happyGoto(p, action_53, j),
        23 => happyGoto(p, action_54, j),
        24 => happyGoto(p, action_55, j),
        25 => happyGoto(p, action_56, j),
        26 => happyGoto(p, action_57, j),
        32 => happyGoto(p, action_610, j),
        34 => happyGoto(p, action_80, j),
        36 => happyGoto(p, action_81, j),
        37 => happyGoto(p, action_611, j),
        38 => happyGoto(p, action_612, j),
        40 => happyGoto(p, action_84, j),
        41 => happyGoto(p, action_85, j),
        42 => happyGoto(p, action_86, j),
        43 => happyGoto(p, action_87, j),
        44 => happyGoto(p, action_613, j),
        45 => happyGoto(p, action_89, j),
        46 => happyGoto(p, action_90, j),
        47 => happyGoto(p, action_91, j),
        48 => happyGoto(p, action_92, j),
        49 => happyGoto(p, action_93, j),
        50 => happyGoto(p, action_94, j),
        51 => happyGoto(p, action_95, j),
        52 => happyGoto(p, action_96, j),
        53 => happyGoto(p, action_97, j),
        54 => happyGoto(p, action_98, j),
        61 => happyGoto(p, action_99, j),
        65 => happyGoto(p, action_614, j),
        100 => happyGoto(p, action_6, j),
        104 => happyGoto(p, action_7, j),
        106 => happyGoto(p, action_8, j),
        107 => happyGoto(p, action_9, j),
        108 => happyGoto(p, action_10, j),
        109 => happyGoto(p, action_11, j),
        110 => happyGoto(p, action_12, j),
        111 => happyGoto(p, action_13, j),
        112 => happyGoto(p, action_14, j),
        113 => happyGoto(p, action_15, j),
        114 => happyGoto(p, action_16, j),
        115 => happyGoto(p, action_17, j),
        116 => happyGoto(p, action_18, j),
        117 => happyGoto(p, action_19, j),
        118 => happyGoto(p, action_20, j),
        119 => happyGoto(p, action_21, j),
        120 => happyGoto(p, action_22, j),
        122 => happyGoto(p, action_58, j),
        127 => happyGoto(p, action_24, j),
        128 => happyGoto(p, action_25, j),
        131 => happyGoto(p, action_59, j),
        132 => happyGoto(p, action_108, j),
        133 => happyGoto(p, action_468, j),
        134 => happyGoto(p, action_110, j),
        _ => happyReduce_41(p, j)
    }
}

fn action_386(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        211 => happyShift(p, action_604, j),
        17 => happyGoto(p, action_603, j),
        _ => happyReduce_42(p, j)
    }
}

fn action_387(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        237 => happyShift(p, action_460, j),
        85 => happyGoto(p, action_602, j),
        _ => happyFail(p, j)
    }
}

fn action_388(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        236 => happyShift(p, action_42, j),
        128 => happyGoto(p, action_601, j),
        _ => happyFail(p, j)
    }
}

fn action_389(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_26, j),
        144 => happyShift(p, action_27, j),
        145 => happyShift(p, action_28, j),
        146 => happyShift(p, action_29, j),
        147 => happyShift(p, action_30, j),
        148 => happyShift(p, action_31, j),
        149 => happyShift(p, action_32, j),
        150 => happyShift(p, action_33, j),
        153 => happyShift(p, action_34, j),
        164 => happyShift(p, action_35, j),
        180 => happyShift(p, action_60, j),
        181 => happyShift(p, action_61, j),
        184 => happyShift(p, action_36, j),
        187 => happyShift(p, action_62, j),
        189 => happyShift(p, action_63, j),
        191 => happyShift(p, action_64, j),
        194 => happyShift(p, action_65, j),
        196 => happyShift(p, action_66, j),
        197 => happyShift(p, action_67, j),
        203 => happyShift(p, action_68, j),
        204 => happyShift(p, action_37, j),
        205 => happyShift(p, action_69, j),
        206 => happyShift(p, action_70, j),
        217 => happyShift(p, action_71, j),
        220 => happyShift(p, action_38, j),
        224 => happyShift(p, action_72, j),
        232 => happyShift(p, action_73, j),
        233 => happyShift(p, action_39, j),
        234 => happyShift(p, action_40, j),
        235 => happyShift(p, action_41, j),
        236 => happyShift(p, action_42, j),
        237 => happyShift(p, action_74, j),
        238 => happyShift(p, action_75, j),
        240 => happyShift(p, action_44, j),
        241 => happyShift(p, action_45, j),
        242 => happyShift(p, action_46, j),
        243 => happyShift(p, action_47, j),
        244 => happyShift(p, action_48, j),
        245 => happyShift(p, action_49, j),
        12 => happyGoto(p, action_600, j),
        13 => happyGoto(p, action_51, j),
        14 => happyGoto(p, action_52, j),
        22 => happyGoto(p, action_53, j),
        23 => happyGoto(p, action_54, j),
        24 => happyGoto(p, action_55, j),
        25 => happyGoto(p, action_56, j),
        26 => happyGoto(p, action_57, j),
        100 => happyGoto(p, action_6, j),
        104 => happyGoto(p, action_7, j),
        106 => happyGoto(p, action_8, j),
        107 => happyGoto(p, action_9, j),
        108 => happyGoto(p, action_10, j),
        109 => happyGoto(p, action_11, j),
        110 => happyGoto(p, action_12, j),
        111 => happyGoto(p, action_13, j),
        112 => happyGoto(p, action_14, j),
        113 => happyGoto(p, action_15, j),
        114 => happyGoto(p, action_16, j),
        115 => happyGoto(p, action_17, j),
        116 => happyGoto(p, action_18, j),
        117 => happyGoto(p, action_19, j),
        118 => happyGoto(p, action_20, j),
        119 => happyGoto(p, action_21, j),
        120 => happyGoto(p, action_22, j),
        122 => happyGoto(p, action_58, j),
        127 => happyGoto(p, action_24, j),
        128 => happyGoto(p, action_25, j),
        131 => happyGoto(p, action_59, j),
        _ => happyFail(p, j)
    }
}

fn action_390(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_26, j),
        144 => happyShift(p, action_27, j),
        145 => happyShift(p, action_28, j),
        146 => happyShift(p, action_29, j),
        147 => happyShift(p, action_30, j),
        148 => happyShift(p, action_31, j),
        149 => happyShift(p, action_32, j),
        150 => happyShift(p, action_33, j),
        153 => happyShift(p, action_34, j),
        164 => happyShift(p, action_35, j),
        184 => happyShift(p, action_36, j),
        204 => happyShift(p, action_37, j),
        220 => happyShift(p, action_38, j),
        233 => happyShift(p, action_39, j),
        234 => happyShift(p, action_40, j),
        235 => happyShift(p, action_41, j),
        236 => happyShift(p, action_42, j),
        237 => happyShift(p, action_43, j),
        240 => happyShift(p, action_44, j),
        241 => happyShift(p, action_45, j),
        242 => happyShift(p, action_46, j),
        243 => happyShift(p, action_47, j),
        244 => happyShift(p, action_48, j),
        245 => happyShift(p, action_49, j),
        100 => happyGoto(p, action_6, j),
        104 => happyGoto(p, action_7, j),
        106 => happyGoto(p, action_259, j),
        107 => happyGoto(p, action_9, j),
        108 => happyGoto(p, action_10, j),
        109 => happyGoto(p, action_11, j),
        110 => happyGoto(p, action_12, j),
        111 => happyGoto(p, action_13, j),
        112 => happyGoto(p, action_14, j),
        113 => happyGoto(p, action_15, j),
        114 => happyGoto(p, action_16, j),
        115 => happyGoto(p, action_17, j),
        116 => happyGoto(p, action_18, j),
        117 => happyGoto(p, action_19, j),
        118 => happyGoto(p, action_20, j),
        119 => happyGoto(p, action_260, j),
        126 => happyGoto(p, action_599, j),
        127 => happyGoto(p, action_24, j),
        128 => happyGoto(p, action_25, j),
        _ => happyFail(p, j)
    }
}

fn action_391(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_36(p, j)
    }
}

fn action_392(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_598, j),
        _ => happyFail(p, j)
    }
}

fn action_393(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        185 => happyShift(p, action_113, j),
        188 => happyShift(p, action_115, j),
        190 => happyShift(p, action_116, j),
        192 => happyShift(p, action_117, j),
        195 => happyShift(p, action_118, j),
        198 => happyShift(p, action_119, j),
        200 => happyShift(p, action_120, j),
        201 => happyShift(p, action_121, j),
        202 => happyShift(p, action_122, j),
        207 => happyShift(p, action_123, j),
        208 => happyShift(p, action_124, j),
        209 => happyShift(p, action_125, j),
        210 => happyShift(p, action_126, j),
        212 => happyShift(p, action_127, j),
        215 => happyShift(p, action_128, j),
        218 => happyShift(p, action_129, j),
        219 => happyShift(p, action_130, j),
        221 => happyShift(p, action_131, j),
        222 => happyShift(p, action_132, j),
        223 => happyShift(p, action_133, j),
        225 => happyShift(p, action_134, j),
        226 => happyShift(p, action_135, j),
        227 => happyShift(p, action_136, j),
        228 => happyShift(p, action_137, j),
        229 => happyShift(p, action_138, j),
        230 => happyShift(p, action_139, j),
        238 => happyShift(p, action_141, j),
        239 => happyShift(p, action_142, j),
        32 => happyGoto(p, action_597, j),
        34 => happyGoto(p, action_80, j),
        36 => happyGoto(p, action_81, j),
        37 => happyGoto(p, action_464, j),
        38 => happyGoto(p, action_465, j),
        40 => happyGoto(p, action_84, j),
        41 => happyGoto(p, action_85, j),
        42 => happyGoto(p, action_86, j),
        43 => happyGoto(p, action_87, j),
        44 => happyGoto(p, action_466, j),
        45 => happyGoto(p, action_89, j),
        46 => happyGoto(p, action_90, j),
        47 => happyGoto(p, action_91, j),
        48 => happyGoto(p, action_92, j),
        49 => happyGoto(p, action_93, j),
        50 => happyGoto(p, action_94, j),
        51 => happyGoto(p, action_95, j),
        52 => happyGoto(p, action_96, j),
        53 => happyGoto(p, action_97, j),
        54 => happyGoto(p, action_98, j),
        61 => happyGoto(p, action_99, j),
        65 => happyGoto(p, action_467, j),
        132 => happyGoto(p, action_108, j),
        133 => happyGoto(p, action_468, j),
        134 => happyGoto(p, action_110, j),
        _ => happyReduce_471(p, j)
    }
}

fn action_394(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        180 => happyShift(p, action_596, j),
        _ => happyFail(p, j)
    }
}

fn action_395(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        180 => happyShift(p, action_595, j),
        _ => happyFail(p, j)
    }
}

fn action_396(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_65(p, j)
    }
}

fn action_397(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        139 => happyShift(p, action_594, j),
        _ => happyFail(p, j)
    }
}

fn action_398(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_69(p, j)
    }
}

fn action_399(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        139 => happyShift(p, action_593, j),
        _ => happyFail(p, j)
    }
}

fn action_400(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        139 => happyShift(p, action_592, j),
        _ => happyFail(p, j)
    }
}

fn action_401(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_475, j),
        150 => happyShift(p, action_476, j),
        237 => happyShift(p, action_140, j),
        75 => happyGoto(p, action_591, j),
        76 => happyGoto(p, action_102, j),
        77 => happyGoto(p, action_103, j),
        78 => happyGoto(p, action_481, j),
        _ => happyFail(p, j)
    }
}

fn action_402(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_524, j),
        150 => happyShift(p, action_525, j),
        237 => happyShift(p, action_140, j),
        238 => happyShift(p, action_229, j),
        66 => happyGoto(p, action_590, j),
        68 => happyGoto(p, action_219, j),
        69 => happyGoto(p, action_220, j),
        70 => happyGoto(p, action_221, j),
        71 => happyGoto(p, action_222, j),
        72 => happyGoto(p, action_223, j),
        73 => happyGoto(p, action_224, j),
        75 => happyGoto(p, action_523, j),
        76 => happyGoto(p, action_102, j),
        77 => happyGoto(p, action_103, j),
        78 => happyGoto(p, action_481, j),
        _ => happyFail(p, j)
    }
}

fn action_403(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        181 => happyShift(p, action_61, j),
        185 => happyShift(p, action_113, j),
        188 => happyShift(p, action_115, j),
        190 => happyShift(p, action_116, j),
        192 => happyShift(p, action_117, j),
        195 => happyShift(p, action_118, j),
        198 => happyShift(p, action_119, j),
        200 => happyShift(p, action_120, j),
        201 => happyShift(p, action_121, j),
        202 => happyShift(p, action_122, j),
        207 => happyShift(p, action_123, j),
        208 => happyShift(p, action_124, j),
        209 => happyShift(p, action_125, j),
        210 => happyShift(p, action_126, j),
        212 => happyShift(p, action_127, j),
        215 => happyShift(p, action_128, j),
        218 => happyShift(p, action_129, j),
        219 => happyShift(p, action_130, j),
        221 => happyShift(p, action_131, j),
        222 => happyShift(p, action_132, j),
        223 => happyShift(p, action_133, j),
        225 => happyShift(p, action_134, j),
        226 => happyShift(p, action_135, j),
        227 => happyShift(p, action_136, j),
        228 => happyShift(p, action_137, j),
        229 => happyShift(p, action_138, j),
        230 => happyShift(p, action_139, j),
        238 => happyShift(p, action_141, j),
        239 => happyShift(p, action_142, j),
        14 => happyGoto(p, action_589, j),
        32 => happyGoto(p, action_463, j),
        34 => happyGoto(p, action_80, j),
        36 => happyGoto(p, action_81, j),
        37 => happyGoto(p, action_464, j),
        38 => happyGoto(p, action_465, j),
        40 => happyGoto(p, action_84, j),
        41 => happyGoto(p, action_85, j),
        42 => happyGoto(p, action_86, j),
        43 => happyGoto(p, action_87, j),
        44 => happyGoto(p, action_466, j),
        45 => happyGoto(p, action_89, j),
        46 => happyGoto(p, action_90, j),
        47 => happyGoto(p, action_91, j),
        48 => happyGoto(p, action_92, j),
        49 => happyGoto(p, action_93, j),
        50 => happyGoto(p, action_94, j),
        51 => happyGoto(p, action_95, j),
        52 => happyGoto(p, action_96, j),
        53 => happyGoto(p, action_97, j),
        54 => happyGoto(p, action_98, j),
        61 => happyGoto(p, action_99, j),
        65 => happyGoto(p, action_467, j),
        132 => happyGoto(p, action_108, j),
        133 => happyGoto(p, action_468, j),
        134 => happyGoto(p, action_110, j),
        _ => happyReduce_471(p, j)
    }
}

fn action_404(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        168 => happyShift(p, action_516, j),
        94 => happyGoto(p, action_588, j),
        _ => happyReduce_344(p, j)
    }
}

fn action_405(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        239 => happyShift(p, action_142, j),
        132 => happyGoto(p, action_587, j),
        133 => happyGoto(p, action_150, j),
        134 => happyGoto(p, action_110, j),
        _ => happyReduce_471(p, j)
    }
}

fn action_406(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_586, j),
        _ => happyFail(p, j)
    }
}

fn action_407(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_14(p, j)
    }
}

fn action_408(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_26, j),
        144 => happyShift(p, action_27, j),
        145 => happyShift(p, action_28, j),
        146 => happyShift(p, action_29, j),
        147 => happyShift(p, action_30, j),
        148 => happyShift(p, action_31, j),
        149 => happyShift(p, action_32, j),
        150 => happyShift(p, action_33, j),
        153 => happyShift(p, action_34, j),
        164 => happyShift(p, action_35, j),
        184 => happyShift(p, action_36, j),
        190 => happyShift(p, action_116, j),
        192 => happyShift(p, action_117, j),
        195 => happyShift(p, action_118, j),
        198 => happyShift(p, action_119, j),
        200 => happyShift(p, action_120, j),
        202 => happyShift(p, action_122, j),
        204 => happyShift(p, action_37, j),
        208 => happyShift(p, action_124, j),
        209 => happyShift(p, action_125, j),
        210 => happyShift(p, action_126, j),
        218 => happyShift(p, action_129, j),
        219 => happyShift(p, action_130, j),
        220 => happyShift(p, action_38, j),
        223 => happyShift(p, action_133, j),
        226 => happyShift(p, action_135, j),
        228 => happyShift(p, action_137, j),
        229 => happyShift(p, action_138, j),
        230 => happyShift(p, action_139, j),
        233 => happyShift(p, action_39, j),
        234 => happyShift(p, action_40, j),
        235 => happyShift(p, action_41, j),
        236 => happyShift(p, action_42, j),
        237 => happyShift(p, action_43, j),
        238 => happyShift(p, action_141, j),
        239 => happyShift(p, action_142, j),
        240 => happyShift(p, action_44, j),
        241 => happyShift(p, action_45, j),
        242 => happyShift(p, action_46, j),
        243 => happyShift(p, action_47, j),
        244 => happyShift(p, action_48, j),
        245 => happyShift(p, action_49, j),
        44 => happyGoto(p, action_286, j),
        45 => happyGoto(p, action_89, j),
        47 => happyGoto(p, action_287, j),
        49 => happyGoto(p, action_288, j),
        51 => happyGoto(p, action_289, j),
        52 => happyGoto(p, action_96, j),
        53 => happyGoto(p, action_97, j),
        54 => happyGoto(p, action_98, j),
        61 => happyGoto(p, action_99, j),
        65 => happyGoto(p, action_290, j),
        86 => happyGoto(p, action_584, j),
        100 => happyGoto(p, action_6, j),
        104 => happyGoto(p, action_7, j),
        106 => happyGoto(p, action_8, j),
        107 => happyGoto(p, action_9, j),
        108 => happyGoto(p, action_10, j),
        109 => happyGoto(p, action_11, j),
        110 => happyGoto(p, action_12, j),
        111 => happyGoto(p, action_13, j),
        112 => happyGoto(p, action_14, j),
        113 => happyGoto(p, action_15, j),
        114 => happyGoto(p, action_16, j),
        115 => happyGoto(p, action_17, j),
        116 => happyGoto(p, action_18, j),
        117 => happyGoto(p, action_19, j),
        118 => happyGoto(p, action_20, j),
        119 => happyGoto(p, action_21, j),
        120 => happyGoto(p, action_22, j),
        122 => happyGoto(p, action_585, j),
        127 => happyGoto(p, action_24, j),
        128 => happyGoto(p, action_25, j),
        132 => happyGoto(p, action_108, j),
        133 => happyGoto(p, action_293, j),
        134 => happyGoto(p, action_110, j),
        _ => happyReduce_471(p, j)
    }
}

fn action_409(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        181 => happyShift(p, action_61, j),
        185 => happyShift(p, action_113, j),
        188 => happyShift(p, action_115, j),
        190 => happyShift(p, action_116, j),
        192 => happyShift(p, action_117, j),
        195 => happyShift(p, action_118, j),
        198 => happyShift(p, action_119, j),
        200 => happyShift(p, action_120, j),
        201 => happyShift(p, action_121, j),
        202 => happyShift(p, action_122, j),
        207 => happyShift(p, action_123, j),
        208 => happyShift(p, action_124, j),
        209 => happyShift(p, action_125, j),
        210 => happyShift(p, action_126, j),
        212 => happyShift(p, action_127, j),
        215 => happyShift(p, action_128, j),
        218 => happyShift(p, action_129, j),
        219 => happyShift(p, action_130, j),
        221 => happyShift(p, action_131, j),
        222 => happyShift(p, action_132, j),
        223 => happyShift(p, action_133, j),
        225 => happyShift(p, action_134, j),
        226 => happyShift(p, action_135, j),
        227 => happyShift(p, action_136, j),
        228 => happyShift(p, action_137, j),
        229 => happyShift(p, action_138, j),
        230 => happyShift(p, action_139, j),
        238 => happyShift(p, action_141, j),
        239 => happyShift(p, action_142, j),
        14 => happyGoto(p, action_583, j),
        32 => happyGoto(p, action_463, j),
        34 => happyGoto(p, action_80, j),
        36 => happyGoto(p, action_81, j),
        37 => happyGoto(p, action_464, j),
        38 => happyGoto(p, action_465, j),
        40 => happyGoto(p, action_84, j),
        41 => happyGoto(p, action_85, j),
        42 => happyGoto(p, action_86, j),
        43 => happyGoto(p, action_87, j),
        44 => happyGoto(p, action_466, j),
        45 => happyGoto(p, action_89, j),
        46 => happyGoto(p, action_90, j),
        47 => happyGoto(p, action_91, j),
        48 => happyGoto(p, action_92, j),
        49 => happyGoto(p, action_93, j),
        50 => happyGoto(p, action_94, j),
        51 => happyGoto(p, action_95, j),
        52 => happyGoto(p, action_96, j),
        53 => happyGoto(p, action_97, j),
        54 => happyGoto(p, action_98, j),
        61 => happyGoto(p, action_99, j),
        65 => happyGoto(p, action_467, j),
        132 => happyGoto(p, action_108, j),
        133 => happyGoto(p, action_468, j),
        134 => happyGoto(p, action_110, j),
        _ => happyReduce_471(p, j)
    }
}

fn action_410(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        168 => happyShift(p, action_516, j),
        94 => happyGoto(p, action_582, j),
        _ => happyReduce_344(p, j)
    }
}

fn action_411(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_16(p, j)
    }
}

fn action_412(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_237(p, j)
    }
}

fn action_413(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        185 => happyShift(p, action_113, j),
        186 => happyReduce_471(p, j),
        188 => happyShift(p, action_115, j),
        190 => happyShift(p, action_116, j),
        192 => happyShift(p, action_117, j),
        193 => happyReduce_471(p, j),
        195 => happyShift(p, action_118, j),
        198 => happyShift(p, action_119, j),
        200 => happyShift(p, action_120, j),
        201 => happyShift(p, action_121, j),
        202 => happyShift(p, action_122, j),
        207 => happyShift(p, action_123, j),
        208 => happyShift(p, action_124, j),
        209 => happyShift(p, action_125, j),
        210 => happyShift(p, action_126, j),
        212 => happyShift(p, action_127, j),
        213 => happyReduce_471(p, j),
        214 => happyReduce_471(p, j),
        215 => happyShift(p, action_128, j),
        216 => happyReduce_471(p, j),
        218 => happyShift(p, action_129, j),
        219 => happyShift(p, action_130, j),
        221 => happyShift(p, action_131, j),
        223 => happyShift(p, action_133, j),
        225 => happyShift(p, action_134, j),
        226 => happyShift(p, action_135, j),
        227 => happyShift(p, action_136, j),
        228 => happyShift(p, action_137, j),
        229 => happyShift(p, action_138, j),
        230 => happyShift(p, action_139, j),
        231 => happyReduce_471(p, j),
        238 => happyShift(p, action_141, j),
        239 => happyShift(p, action_142, j),
        37 => happyGoto(p, action_449, j),
        38 => happyGoto(p, action_450, j),
        40 => happyGoto(p, action_84, j),
        41 => happyGoto(p, action_85, j),
        42 => happyGoto(p, action_86, j),
        43 => happyGoto(p, action_87, j),
        44 => happyGoto(p, action_451, j),
        45 => happyGoto(p, action_89, j),
        46 => happyGoto(p, action_90, j),
        47 => happyGoto(p, action_91, j),
        48 => happyGoto(p, action_452, j),
        49 => happyGoto(p, action_453, j),
        50 => happyGoto(p, action_94, j),
        51 => happyGoto(p, action_95, j),
        52 => happyGoto(p, action_96, j),
        53 => happyGoto(p, action_97, j),
        54 => happyGoto(p, action_98, j),
        61 => happyGoto(p, action_99, j),
        65 => happyGoto(p, action_454, j),
        82 => happyGoto(p, action_455, j),
        83 => happyGoto(p, action_456, j),
        84 => happyGoto(p, action_457, j),
        132 => happyGoto(p, action_108, j),
        133 => happyGoto(p, action_459, j),
        134 => happyGoto(p, action_110, j),
        _ => happyReduce_282(p, j)
    }
}

fn action_414(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_581, j),
        150 => happyShift(p, action_228, j),
        186 => happyShift(p, action_171, j),
        193 => happyShift(p, action_172, j),
        213 => happyShift(p, action_173, j),
        214 => happyShift(p, action_174, j),
        216 => happyShift(p, action_175, j),
        231 => happyShift(p, action_176, j),
        237 => happyShift(p, action_140, j),
        238 => happyShift(p, action_229, j),
        239 => happyShift(p, action_142, j),
        64 => happyGoto(p, action_187, j),
        69 => happyGoto(p, action_578, j),
        70 => happyGoto(p, action_221, j),
        71 => happyGoto(p, action_222, j),
        72 => happyGoto(p, action_579, j),
        73 => happyGoto(p, action_224, j),
        75 => happyGoto(p, action_482, j),
        76 => happyGoto(p, action_102, j),
        77 => happyGoto(p, action_103, j),
        78 => happyGoto(p, action_104, j),
        80 => happyGoto(p, action_483, j),
        81 => happyGoto(p, action_107, j),
        133 => happyGoto(p, action_580, j),
        134 => happyGoto(p, action_110, j),
        _ => happyFail(p, j)
    }
}

fn action_415(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_240(p, j)
    }
}

fn action_416(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_252(p, j)
    }
}

fn action_417(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_571, j),
        150 => happyShift(p, action_572, j),
        237 => happyShift(p, action_140, j),
        238 => happyShift(p, action_229, j),
        239 => happyShift(p, action_142, j),
        69 => happyGoto(p, action_577, j),
        70 => happyGoto(p, action_221, j),
        71 => happyGoto(p, action_222, j),
        75 => happyGoto(p, action_480, j),
        76 => happyGoto(p, action_102, j),
        77 => happyGoto(p, action_103, j),
        78 => happyGoto(p, action_481, j),
        134 => happyGoto(p, action_167, j),
        _ => happyReduce_472(p, j)
    }
}

fn action_418(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_423, j),
        150 => happyShift(p, action_228, j),
        237 => happyShift(p, action_140, j),
        238 => happyShift(p, action_424, j),
        239 => happyShift(p, action_142, j),
        70 => happyGoto(p, action_419, j),
        71 => happyGoto(p, action_222, j),
        72 => happyGoto(p, action_420, j),
        73 => happyGoto(p, action_224, j),
        74 => happyGoto(p, action_576, j),
        76 => happyGoto(p, action_157, j),
        77 => happyGoto(p, action_103, j),
        78 => happyGoto(p, action_158, j),
        80 => happyGoto(p, action_159, j),
        81 => happyGoto(p, action_107, j),
        133 => happyGoto(p, action_422, j),
        134 => happyGoto(p, action_110, j),
        _ => happyFail(p, j)
    }
}

fn action_419(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        139 => happyShift(p, action_575, j),
        _ => happyFail(p, j)
    }
}

fn action_420(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        139 => happyShift(p, action_574, j),
        _ => happyFail(p, j)
    }
}

fn action_421(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_413, j),
        140 => happyShift(p, action_182, j),
        88 => happyGoto(p, action_573, j),
        89 => happyGoto(p, action_179, j),
        90 => happyGoto(p, action_180, j),
        _ => happyFail(p, j)
    }
}

fn action_422(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_571, j),
        150 => happyShift(p, action_572, j),
        237 => happyShift(p, action_140, j),
        239 => happyShift(p, action_142, j),
        70 => happyGoto(p, action_570, j),
        71 => happyGoto(p, action_222, j),
        76 => happyGoto(p, action_473, j),
        77 => happyGoto(p, action_103, j),
        78 => happyGoto(p, action_474, j),
        134 => happyGoto(p, action_167, j),
        _ => happyFail(p, j)
    }
}

fn action_423(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_423, j),
        150 => happyShift(p, action_228, j),
        237 => happyShift(p, action_140, j),
        238 => happyShift(p, action_424, j),
        239 => happyShift(p, action_142, j),
        70 => happyGoto(p, action_419, j),
        71 => happyGoto(p, action_222, j),
        72 => happyGoto(p, action_420, j),
        73 => happyGoto(p, action_224, j),
        74 => happyGoto(p, action_569, j),
        76 => happyGoto(p, action_157, j),
        77 => happyGoto(p, action_103, j),
        78 => happyGoto(p, action_158, j),
        80 => happyGoto(p, action_159, j),
        81 => happyGoto(p, action_107, j),
        133 => happyGoto(p, action_422, j),
        134 => happyGoto(p, action_110, j),
        _ => happyFail(p, j)
    }
}

fn action_424(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_258(p, j)
    }
}

fn action_425(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        181 => happyShift(p, action_61, j),
        185 => happyShift(p, action_113, j),
        188 => happyShift(p, action_115, j),
        190 => happyShift(p, action_116, j),
        192 => happyShift(p, action_117, j),
        195 => happyShift(p, action_118, j),
        198 => happyShift(p, action_119, j),
        200 => happyShift(p, action_120, j),
        201 => happyShift(p, action_121, j),
        202 => happyShift(p, action_122, j),
        207 => happyShift(p, action_123, j),
        208 => happyShift(p, action_124, j),
        209 => happyShift(p, action_125, j),
        210 => happyShift(p, action_126, j),
        212 => happyShift(p, action_127, j),
        215 => happyShift(p, action_128, j),
        218 => happyShift(p, action_129, j),
        219 => happyShift(p, action_130, j),
        221 => happyShift(p, action_131, j),
        222 => happyShift(p, action_132, j),
        223 => happyShift(p, action_133, j),
        225 => happyShift(p, action_134, j),
        226 => happyShift(p, action_135, j),
        227 => happyShift(p, action_136, j),
        228 => happyShift(p, action_137, j),
        229 => happyShift(p, action_138, j),
        230 => happyShift(p, action_139, j),
        238 => happyShift(p, action_141, j),
        239 => happyShift(p, action_142, j),
        14 => happyGoto(p, action_568, j),
        32 => happyGoto(p, action_463, j),
        34 => happyGoto(p, action_80, j),
        36 => happyGoto(p, action_81, j),
        37 => happyGoto(p, action_464, j),
        38 => happyGoto(p, action_465, j),
        40 => happyGoto(p, action_84, j),
        41 => happyGoto(p, action_85, j),
        42 => happyGoto(p, action_86, j),
        43 => happyGoto(p, action_87, j),
        44 => happyGoto(p, action_466, j),
        45 => happyGoto(p, action_89, j),
        46 => happyGoto(p, action_90, j),
        47 => happyGoto(p, action_91, j),
        48 => happyGoto(p, action_92, j),
        49 => happyGoto(p, action_93, j),
        50 => happyGoto(p, action_94, j),
        51 => happyGoto(p, action_95, j),
        52 => happyGoto(p, action_96, j),
        53 => happyGoto(p, action_97, j),
        54 => happyGoto(p, action_98, j),
        61 => happyGoto(p, action_99, j),
        65 => happyGoto(p, action_467, j),
        132 => happyGoto(p, action_108, j),
        133 => happyGoto(p, action_468, j),
        134 => happyGoto(p, action_110, j),
        _ => happyReduce_471(p, j)
    }
}

fn action_426(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        168 => happyShift(p, action_516, j),
        94 => happyGoto(p, action_567, j),
        _ => happyReduce_344(p, j)
    }
}

fn action_427(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_15(p, j)
    }
}

fn action_428(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        181 => happyShift(p, action_566, j),
        _ => happyReduce_188(p, j)
    }
}

fn action_429(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        55 => happyGoto(p, action_565, j),
        _ => happyReduce_191(p, j)
    }
}

fn action_430(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_26, j),
        144 => happyShift(p, action_27, j),
        145 => happyShift(p, action_28, j),
        146 => happyShift(p, action_29, j),
        147 => happyShift(p, action_30, j),
        148 => happyShift(p, action_31, j),
        149 => happyShift(p, action_32, j),
        150 => happyShift(p, action_33, j),
        153 => happyShift(p, action_34, j),
        164 => happyShift(p, action_35, j),
        184 => happyShift(p, action_36, j),
        190 => happyShift(p, action_116, j),
        192 => happyShift(p, action_117, j),
        195 => happyShift(p, action_118, j),
        198 => happyShift(p, action_119, j),
        200 => happyShift(p, action_120, j),
        202 => happyShift(p, action_122, j),
        204 => happyShift(p, action_37, j),
        208 => happyShift(p, action_124, j),
        209 => happyShift(p, action_125, j),
        210 => happyShift(p, action_126, j),
        218 => happyShift(p, action_129, j),
        219 => happyShift(p, action_130, j),
        220 => happyShift(p, action_38, j),
        223 => happyShift(p, action_133, j),
        226 => happyShift(p, action_135, j),
        228 => happyShift(p, action_137, j),
        229 => happyShift(p, action_138, j),
        230 => happyShift(p, action_139, j),
        233 => happyShift(p, action_39, j),
        234 => happyShift(p, action_40, j),
        235 => happyShift(p, action_41, j),
        236 => happyShift(p, action_42, j),
        237 => happyShift(p, action_43, j),
        238 => happyShift(p, action_141, j),
        239 => happyShift(p, action_142, j),
        240 => happyShift(p, action_44, j),
        241 => happyShift(p, action_45, j),
        242 => happyShift(p, action_46, j),
        243 => happyShift(p, action_47, j),
        244 => happyShift(p, action_48, j),
        245 => happyShift(p, action_49, j),
        44 => happyGoto(p, action_286, j),
        45 => happyGoto(p, action_89, j),
        47 => happyGoto(p, action_287, j),
        49 => happyGoto(p, action_288, j),
        51 => happyGoto(p, action_289, j),
        52 => happyGoto(p, action_96, j),
        53 => happyGoto(p, action_97, j),
        54 => happyGoto(p, action_98, j),
        61 => happyGoto(p, action_99, j),
        65 => happyGoto(p, action_290, j),
        86 => happyGoto(p, action_563, j),
        100 => happyGoto(p, action_6, j),
        104 => happyGoto(p, action_7, j),
        106 => happyGoto(p, action_8, j),
        107 => happyGoto(p, action_9, j),
        108 => happyGoto(p, action_10, j),
        109 => happyGoto(p, action_11, j),
        110 => happyGoto(p, action_12, j),
        111 => happyGoto(p, action_13, j),
        112 => happyGoto(p, action_14, j),
        113 => happyGoto(p, action_15, j),
        114 => happyGoto(p, action_16, j),
        115 => happyGoto(p, action_17, j),
        116 => happyGoto(p, action_18, j),
        117 => happyGoto(p, action_19, j),
        118 => happyGoto(p, action_20, j),
        119 => happyGoto(p, action_21, j),
        120 => happyGoto(p, action_22, j),
        122 => happyGoto(p, action_564, j),
        127 => happyGoto(p, action_24, j),
        128 => happyGoto(p, action_25, j),
        132 => happyGoto(p, action_108, j),
        133 => happyGoto(p, action_293, j),
        134 => happyGoto(p, action_110, j),
        _ => happyReduce_471(p, j)
    }
}

fn action_431(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        181 => happyShift(p, action_61, j),
        14 => happyGoto(p, action_562, j),
        _ => happyFail(p, j)
    }
}

fn action_432(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_107(p, j)
    }
}

fn action_433(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_150(p, j)
    }
}

fn action_434(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_161(p, j)
    }
}

fn action_435(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_229(p, j)
    }
}

fn action_436(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        181 => happyReduce_26(p, j),
        187 => happyShift(p, action_406, j),
        35 => happyGoto(p, action_561, j),
        67 => happyGoto(p, action_405, j),
        _ => happyReduce_232(p, j)
    }
}

fn action_437(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        33 => happyGoto(p, action_560, j),
        _ => happyReduce_90(p, j)
    }
}

fn action_438(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_559, j),
        _ => happyFail(p, j)
    }
}

fn action_439(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_179(p, j)
    }
}

fn action_440(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        181 => happyShift(p, action_61, j),
        185 => happyShift(p, action_113, j),
        188 => happyShift(p, action_115, j),
        190 => happyShift(p, action_116, j),
        192 => happyShift(p, action_117, j),
        195 => happyShift(p, action_118, j),
        198 => happyShift(p, action_119, j),
        200 => happyShift(p, action_120, j),
        201 => happyShift(p, action_121, j),
        202 => happyShift(p, action_122, j),
        207 => happyShift(p, action_123, j),
        208 => happyShift(p, action_124, j),
        209 => happyShift(p, action_125, j),
        210 => happyShift(p, action_126, j),
        212 => happyShift(p, action_127, j),
        215 => happyShift(p, action_128, j),
        218 => happyShift(p, action_129, j),
        219 => happyShift(p, action_130, j),
        221 => happyShift(p, action_131, j),
        222 => happyShift(p, action_132, j),
        223 => happyShift(p, action_133, j),
        225 => happyShift(p, action_134, j),
        226 => happyShift(p, action_135, j),
        227 => happyShift(p, action_136, j),
        228 => happyShift(p, action_137, j),
        229 => happyShift(p, action_138, j),
        230 => happyShift(p, action_139, j),
        238 => happyShift(p, action_141, j),
        239 => happyShift(p, action_142, j),
        14 => happyGoto(p, action_558, j),
        32 => happyGoto(p, action_463, j),
        34 => happyGoto(p, action_80, j),
        36 => happyGoto(p, action_81, j),
        37 => happyGoto(p, action_464, j),
        38 => happyGoto(p, action_465, j),
        40 => happyGoto(p, action_84, j),
        41 => happyGoto(p, action_85, j),
        42 => happyGoto(p, action_86, j),
        43 => happyGoto(p, action_87, j),
        44 => happyGoto(p, action_466, j),
        45 => happyGoto(p, action_89, j),
        46 => happyGoto(p, action_90, j),
        47 => happyGoto(p, action_91, j),
        48 => happyGoto(p, action_92, j),
        49 => happyGoto(p, action_93, j),
        50 => happyGoto(p, action_94, j),
        51 => happyGoto(p, action_95, j),
        52 => happyGoto(p, action_96, j),
        53 => happyGoto(p, action_97, j),
        54 => happyGoto(p, action_98, j),
        61 => happyGoto(p, action_99, j),
        65 => happyGoto(p, action_467, j),
        132 => happyGoto(p, action_108, j),
        133 => happyGoto(p, action_468, j),
        134 => happyGoto(p, action_110, j),
        _ => happyReduce_471(p, j)
    }
}

fn action_441(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        168 => happyShift(p, action_516, j),
        94 => happyGoto(p, action_557, j),
        _ => happyReduce_344(p, j)
    }
}

fn action_442(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_17(p, j)
    }
}

fn action_443(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_26, j),
        144 => happyShift(p, action_27, j),
        145 => happyShift(p, action_28, j),
        146 => happyShift(p, action_29, j),
        147 => happyShift(p, action_30, j),
        148 => happyShift(p, action_31, j),
        149 => happyShift(p, action_32, j),
        150 => happyShift(p, action_556, j),
        153 => happyShift(p, action_34, j),
        164 => happyShift(p, action_35, j),
        184 => happyShift(p, action_36, j),
        186 => happyShift(p, action_171, j),
        193 => happyShift(p, action_172, j),
        204 => happyShift(p, action_37, j),
        213 => happyShift(p, action_173, j),
        214 => happyShift(p, action_174, j),
        216 => happyShift(p, action_175, j),
        220 => happyShift(p, action_38, j),
        221 => happyReduce_471(p, j),
        231 => happyShift(p, action_176, j),
        233 => happyShift(p, action_39, j),
        234 => happyShift(p, action_40, j),
        235 => happyShift(p, action_41, j),
        236 => happyShift(p, action_42, j),
        237 => happyShift(p, action_43, j),
        239 => happyShift(p, action_142, j),
        240 => happyShift(p, action_44, j),
        241 => happyShift(p, action_45, j),
        242 => happyShift(p, action_46, j),
        243 => happyShift(p, action_47, j),
        244 => happyShift(p, action_48, j),
        245 => happyShift(p, action_49, j),
        64 => happyGoto(p, action_187, j),
        100 => happyGoto(p, action_6, j),
        104 => happyGoto(p, action_7, j),
        106 => happyGoto(p, action_8, j),
        107 => happyGoto(p, action_9, j),
        108 => happyGoto(p, action_10, j),
        109 => happyGoto(p, action_11, j),
        110 => happyGoto(p, action_12, j),
        111 => happyGoto(p, action_13, j),
        112 => happyGoto(p, action_14, j),
        113 => happyGoto(p, action_15, j),
        114 => happyGoto(p, action_16, j),
        115 => happyGoto(p, action_17, j),
        116 => happyGoto(p, action_18, j),
        117 => happyGoto(p, action_19, j),
        118 => happyGoto(p, action_20, j),
        119 => happyGoto(p, action_21, j),
        120 => happyGoto(p, action_444, j),
        125 => happyGoto(p, action_553, j),
        127 => happyGoto(p, action_24, j),
        128 => happyGoto(p, action_25, j),
        132 => happyGoto(p, action_554, j),
        133 => happyGoto(p, action_555, j),
        134 => happyGoto(p, action_110, j),
        _ => happyReduce_458(p, j)
    }
}

fn action_444(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_459(p, j)
    }
}

fn action_445(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        141 => happyShift(p, action_552, j),
        _ => happyFail(p, j)
    }
}

fn action_446(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_26, j),
        144 => happyShift(p, action_27, j),
        145 => happyShift(p, action_28, j),
        146 => happyShift(p, action_29, j),
        147 => happyShift(p, action_30, j),
        148 => happyShift(p, action_31, j),
        149 => happyShift(p, action_32, j),
        150 => happyShift(p, action_551, j),
        153 => happyShift(p, action_34, j),
        164 => happyShift(p, action_35, j),
        184 => happyShift(p, action_36, j),
        186 => happyReduce_472(p, j),
        193 => happyReduce_472(p, j),
        204 => happyShift(p, action_37, j),
        213 => happyReduce_472(p, j),
        214 => happyReduce_472(p, j),
        216 => happyReduce_472(p, j),
        220 => happyShift(p, action_38, j),
        231 => happyReduce_472(p, j),
        233 => happyShift(p, action_39, j),
        234 => happyShift(p, action_40, j),
        235 => happyShift(p, action_41, j),
        236 => happyShift(p, action_42, j),
        237 => happyShift(p, action_43, j),
        239 => happyShift(p, action_142, j),
        240 => happyShift(p, action_44, j),
        241 => happyShift(p, action_45, j),
        242 => happyShift(p, action_46, j),
        243 => happyShift(p, action_47, j),
        244 => happyShift(p, action_48, j),
        245 => happyShift(p, action_49, j),
        100 => happyGoto(p, action_6, j),
        104 => happyGoto(p, action_7, j),
        106 => happyGoto(p, action_8, j),
        107 => happyGoto(p, action_9, j),
        108 => happyGoto(p, action_10, j),
        109 => happyGoto(p, action_11, j),
        110 => happyGoto(p, action_12, j),
        111 => happyGoto(p, action_13, j),
        112 => happyGoto(p, action_14, j),
        113 => happyGoto(p, action_15, j),
        114 => happyGoto(p, action_16, j),
        115 => happyGoto(p, action_17, j),
        116 => happyGoto(p, action_18, j),
        117 => happyGoto(p, action_19, j),
        118 => happyGoto(p, action_20, j),
        119 => happyGoto(p, action_21, j),
        120 => happyGoto(p, action_444, j),
        125 => happyGoto(p, action_550, j),
        127 => happyGoto(p, action_24, j),
        128 => happyGoto(p, action_25, j),
        134 => happyGoto(p, action_167, j),
        _ => happyReduce_458(p, j)
    }
}

fn action_447(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        141 => happyReduce_471(p, j),
        239 => happyShift(p, action_142, j),
        132 => happyGoto(p, action_549, j),
        133 => happyGoto(p, action_150, j),
        134 => happyGoto(p, action_110, j),
        _ => happyReduce_401(p, j)
    }
}

fn action_448(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        239 => happyShift(p, action_142, j),
        65 => happyGoto(p, action_547, j),
        132 => happyGoto(p, action_548, j),
        133 => happyGoto(p, action_150, j),
        134 => happyGoto(p, action_110, j),
        _ => happyReduce_471(p, j)
    }
}

fn action_449(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_540, j),
        140 => happyShift(p, action_182, j),
        150 => happyShift(p, action_541, j),
        237 => happyShift(p, action_140, j),
        238 => happyShift(p, action_229, j),
        69 => happyGoto(p, action_544, j),
        70 => happyGoto(p, action_221, j),
        71 => happyGoto(p, action_222, j),
        75 => happyGoto(p, action_545, j),
        76 => happyGoto(p, action_102, j),
        77 => happyGoto(p, action_103, j),
        78 => happyGoto(p, action_481, j),
        87 => happyGoto(p, action_546, j),
        88 => happyGoto(p, action_367, j),
        89 => happyGoto(p, action_179, j),
        90 => happyGoto(p, action_180, j),
        91 => happyGoto(p, action_368, j),
        92 => happyGoto(p, action_369, j),
        _ => happyReduce_287(p, j)
    }
}

fn action_450(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_535, j),
        140 => happyShift(p, action_182, j),
        150 => happyShift(p, action_536, j),
        185 => happyShift(p, action_113, j),
        186 => happyShift(p, action_171, j),
        188 => happyShift(p, action_115, j),
        190 => happyShift(p, action_116, j),
        192 => happyShift(p, action_117, j),
        193 => happyShift(p, action_172, j),
        195 => happyShift(p, action_118, j),
        198 => happyShift(p, action_119, j),
        200 => happyShift(p, action_120, j),
        201 => happyShift(p, action_121, j),
        202 => happyShift(p, action_122, j),
        207 => happyShift(p, action_123, j),
        208 => happyShift(p, action_124, j),
        209 => happyShift(p, action_125, j),
        210 => happyShift(p, action_126, j),
        212 => happyShift(p, action_127, j),
        213 => happyShift(p, action_173, j),
        214 => happyShift(p, action_174, j),
        215 => happyShift(p, action_128, j),
        216 => happyShift(p, action_175, j),
        218 => happyShift(p, action_129, j),
        219 => happyShift(p, action_130, j),
        221 => happyShift(p, action_131, j),
        223 => happyShift(p, action_133, j),
        225 => happyShift(p, action_134, j),
        226 => happyShift(p, action_237, j),
        227 => happyShift(p, action_136, j),
        228 => happyShift(p, action_137, j),
        229 => happyShift(p, action_138, j),
        230 => happyShift(p, action_139, j),
        231 => happyShift(p, action_176, j),
        237 => happyShift(p, action_140, j),
        238 => happyShift(p, action_238, j),
        239 => happyShift(p, action_142, j),
        39 => happyGoto(p, action_231, j),
        41 => happyGoto(p, action_198, j),
        42 => happyGoto(p, action_199, j),
        43 => happyGoto(p, action_200, j),
        45 => happyGoto(p, action_232, j),
        52 => happyGoto(p, action_233, j),
        53 => happyGoto(p, action_97, j),
        54 => happyGoto(p, action_98, j),
        61 => happyGoto(p, action_99, j),
        64 => happyGoto(p, action_201, j),
        75 => happyGoto(p, action_542, j),
        76 => happyGoto(p, action_102, j),
        77 => happyGoto(p, action_103, j),
        78 => happyGoto(p, action_481, j),
        87 => happyGoto(p, action_543, j),
        88 => happyGoto(p, action_367, j),
        89 => happyGoto(p, action_179, j),
        90 => happyGoto(p, action_180, j),
        91 => happyGoto(p, action_368, j),
        92 => happyGoto(p, action_369, j),
        134 => happyGoto(p, action_236, j),
        _ => happyReduce_291(p, j)
    }
}

fn action_451(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_540, j),
        140 => happyShift(p, action_182, j),
        150 => happyShift(p, action_541, j),
        237 => happyShift(p, action_140, j),
        238 => happyShift(p, action_229, j),
        69 => happyGoto(p, action_537, j),
        70 => happyGoto(p, action_221, j),
        71 => happyGoto(p, action_222, j),
        75 => happyGoto(p, action_538, j),
        76 => happyGoto(p, action_102, j),
        77 => happyGoto(p, action_103, j),
        78 => happyGoto(p, action_481, j),
        87 => happyGoto(p, action_539, j),
        88 => happyGoto(p, action_367, j),
        89 => happyGoto(p, action_179, j),
        90 => happyGoto(p, action_180, j),
        91 => happyGoto(p, action_368, j),
        92 => happyGoto(p, action_369, j),
        _ => happyReduce_294(p, j)
    }
}

fn action_452(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        185 => happyShift(p, action_113, j),
        186 => happyShift(p, action_171, j),
        188 => happyShift(p, action_115, j),
        193 => happyShift(p, action_172, j),
        201 => happyShift(p, action_121, j),
        207 => happyShift(p, action_123, j),
        212 => happyShift(p, action_127, j),
        213 => happyShift(p, action_173, j),
        214 => happyShift(p, action_174, j),
        215 => happyShift(p, action_128, j),
        216 => happyShift(p, action_175, j),
        221 => happyShift(p, action_131, j),
        225 => happyShift(p, action_134, j),
        227 => happyShift(p, action_136, j),
        231 => happyShift(p, action_176, j),
        239 => happyShift(p, action_142, j),
        39 => happyGoto(p, action_207, j),
        41 => happyGoto(p, action_198, j),
        42 => happyGoto(p, action_199, j),
        43 => happyGoto(p, action_200, j),
        64 => happyGoto(p, action_201, j),
        134 => happyGoto(p, action_208, j),
        _ => happyReduce_102(p, j)
    }
}

fn action_453(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        186 => happyShift(p, action_171, j),
        188 => happyShift(p, action_115, j),
        193 => happyShift(p, action_172, j),
        201 => happyShift(p, action_121, j),
        213 => happyShift(p, action_173, j),
        214 => happyShift(p, action_174, j),
        215 => happyShift(p, action_128, j),
        216 => happyShift(p, action_175, j),
        221 => happyShift(p, action_131, j),
        225 => happyShift(p, action_134, j),
        227 => happyShift(p, action_136, j),
        231 => happyShift(p, action_176, j),
        239 => happyShift(p, action_142, j),
        41 => happyGoto(p, action_203, j),
        64 => happyGoto(p, action_204, j),
        134 => happyGoto(p, action_205, j),
        _ => happyReduce_128(p, j)
    }
}

fn action_454(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_535, j),
        140 => happyShift(p, action_182, j),
        150 => happyShift(p, action_536, j),
        185 => happyShift(p, action_113, j),
        186 => happyShift(p, action_171, j),
        188 => happyShift(p, action_115, j),
        190 => happyShift(p, action_116, j),
        192 => happyShift(p, action_117, j),
        193 => happyShift(p, action_172, j),
        195 => happyShift(p, action_118, j),
        198 => happyShift(p, action_119, j),
        200 => happyShift(p, action_120, j),
        201 => happyShift(p, action_121, j),
        202 => happyShift(p, action_122, j),
        207 => happyShift(p, action_123, j),
        208 => happyShift(p, action_124, j),
        209 => happyShift(p, action_125, j),
        210 => happyShift(p, action_126, j),
        212 => happyShift(p, action_127, j),
        213 => happyShift(p, action_173, j),
        214 => happyShift(p, action_174, j),
        215 => happyShift(p, action_128, j),
        216 => happyShift(p, action_175, j),
        218 => happyShift(p, action_129, j),
        219 => happyShift(p, action_130, j),
        221 => happyShift(p, action_131, j),
        223 => happyShift(p, action_133, j),
        225 => happyShift(p, action_134, j),
        226 => happyShift(p, action_191, j),
        227 => happyShift(p, action_136, j),
        228 => happyShift(p, action_137, j),
        229 => happyShift(p, action_138, j),
        230 => happyShift(p, action_139, j),
        231 => happyShift(p, action_176, j),
        237 => happyShift(p, action_140, j),
        238 => happyShift(p, action_192, j),
        239 => happyShift(p, action_142, j),
        40 => happyGoto(p, action_184, j),
        41 => happyGoto(p, action_85, j),
        42 => happyGoto(p, action_86, j),
        43 => happyGoto(p, action_87, j),
        45 => happyGoto(p, action_185, j),
        52 => happyGoto(p, action_186, j),
        53 => happyGoto(p, action_97, j),
        54 => happyGoto(p, action_98, j),
        61 => happyGoto(p, action_99, j),
        64 => happyGoto(p, action_187, j),
        75 => happyGoto(p, action_531, j),
        76 => happyGoto(p, action_102, j),
        77 => happyGoto(p, action_103, j),
        78 => happyGoto(p, action_481, j),
        87 => happyGoto(p, action_532, j),
        88 => happyGoto(p, action_367, j),
        89 => happyGoto(p, action_179, j),
        90 => happyGoto(p, action_180, j),
        91 => happyGoto(p, action_368, j),
        92 => happyGoto(p, action_369, j),
        133 => happyGoto(p, action_533, j),
        134 => happyGoto(p, action_534, j),
        _ => happyReduce_298(p, j)
    }
}

fn action_455(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        139 => happyShift(p, action_530, j),
        _ => happyFail(p, j)
    }
}

fn action_456(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        179 => happyShift(p, action_529, j),
        _ => happyReduce_283(p, j)
    }
}

fn action_457(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_285(p, j)
    }
}

fn action_458(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        139 => happyShift(p, action_527, j),
        179 => happyShift(p, action_528, j),
        _ => happyFail(p, j)
    }
}

fn action_459(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        185 => happyShift(p, action_113, j),
        188 => happyShift(p, action_115, j),
        190 => happyShift(p, action_116, j),
        192 => happyShift(p, action_117, j),
        195 => happyShift(p, action_118, j),
        198 => happyShift(p, action_119, j),
        200 => happyShift(p, action_120, j),
        201 => happyShift(p, action_121, j),
        202 => happyShift(p, action_122, j),
        207 => happyShift(p, action_123, j),
        208 => happyShift(p, action_124, j),
        209 => happyShift(p, action_125, j),
        210 => happyShift(p, action_126, j),
        212 => happyShift(p, action_127, j),
        215 => happyShift(p, action_128, j),
        218 => happyShift(p, action_129, j),
        219 => happyShift(p, action_130, j),
        221 => happyShift(p, action_131, j),
        223 => happyShift(p, action_133, j),
        225 => happyShift(p, action_134, j),
        226 => happyShift(p, action_168, j),
        227 => happyShift(p, action_136, j),
        228 => happyShift(p, action_137, j),
        229 => happyShift(p, action_138, j),
        230 => happyShift(p, action_139, j),
        238 => happyShift(p, action_169, j),
        239 => happyShift(p, action_142, j),
        40 => happyGoto(p, action_162, j),
        41 => happyGoto(p, action_85, j),
        42 => happyGoto(p, action_86, j),
        43 => happyGoto(p, action_87, j),
        45 => happyGoto(p, action_163, j),
        52 => happyGoto(p, action_164, j),
        53 => happyGoto(p, action_97, j),
        54 => happyGoto(p, action_98, j),
        61 => happyGoto(p, action_99, j),
        134 => happyGoto(p, action_167, j),
        _ => happyReduce_472(p, j)
    }
}

fn action_460(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_302(p, j)
    }
}

fn action_461(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_314(p, j)
    }
}

fn action_462(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_19(p, j)
    }
}

fn action_463(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_91(p, j)
    }
}

fn action_464(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_524, j),
        150 => happyShift(p, action_525, j),
        237 => happyShift(p, action_140, j),
        238 => happyShift(p, action_229, j),
        66 => happyGoto(p, action_240, j),
        68 => happyGoto(p, action_219, j),
        69 => happyGoto(p, action_220, j),
        70 => happyGoto(p, action_221, j),
        71 => happyGoto(p, action_222, j),
        72 => happyGoto(p, action_223, j),
        73 => happyGoto(p, action_224, j),
        75 => happyGoto(p, action_523, j),
        76 => happyGoto(p, action_102, j),
        77 => happyGoto(p, action_103, j),
        78 => happyGoto(p, action_481, j),
        _ => happyFail(p, j)
    }
}

fn action_465(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_475, j),
        150 => happyShift(p, action_476, j),
        185 => happyShift(p, action_113, j),
        186 => happyShift(p, action_171, j),
        188 => happyShift(p, action_115, j),
        190 => happyShift(p, action_116, j),
        192 => happyShift(p, action_117, j),
        193 => happyShift(p, action_172, j),
        195 => happyShift(p, action_118, j),
        198 => happyShift(p, action_119, j),
        200 => happyShift(p, action_120, j),
        201 => happyShift(p, action_121, j),
        202 => happyShift(p, action_122, j),
        207 => happyShift(p, action_123, j),
        208 => happyShift(p, action_124, j),
        209 => happyShift(p, action_125, j),
        210 => happyShift(p, action_126, j),
        212 => happyShift(p, action_127, j),
        213 => happyShift(p, action_173, j),
        214 => happyShift(p, action_174, j),
        215 => happyShift(p, action_128, j),
        216 => happyShift(p, action_175, j),
        218 => happyShift(p, action_129, j),
        219 => happyShift(p, action_130, j),
        221 => happyShift(p, action_131, j),
        223 => happyShift(p, action_133, j),
        225 => happyShift(p, action_134, j),
        226 => happyShift(p, action_237, j),
        227 => happyShift(p, action_136, j),
        228 => happyShift(p, action_137, j),
        229 => happyShift(p, action_138, j),
        230 => happyShift(p, action_139, j),
        231 => happyShift(p, action_176, j),
        237 => happyShift(p, action_140, j),
        238 => happyShift(p, action_238, j),
        239 => happyShift(p, action_142, j),
        39 => happyGoto(p, action_231, j),
        41 => happyGoto(p, action_198, j),
        42 => happyGoto(p, action_199, j),
        43 => happyGoto(p, action_200, j),
        45 => happyGoto(p, action_232, j),
        52 => happyGoto(p, action_233, j),
        53 => happyGoto(p, action_97, j),
        54 => happyGoto(p, action_98, j),
        61 => happyGoto(p, action_99, j),
        64 => happyGoto(p, action_201, j),
        75 => happyGoto(p, action_526, j),
        76 => happyGoto(p, action_102, j),
        77 => happyGoto(p, action_103, j),
        78 => happyGoto(p, action_481, j),
        134 => happyGoto(p, action_236, j),
        _ => happyFail(p, j)
    }
}

fn action_466(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_524, j),
        150 => happyShift(p, action_525, j),
        237 => happyShift(p, action_140, j),
        238 => happyShift(p, action_229, j),
        66 => happyGoto(p, action_218, j),
        68 => happyGoto(p, action_219, j),
        69 => happyGoto(p, action_220, j),
        70 => happyGoto(p, action_221, j),
        71 => happyGoto(p, action_222, j),
        72 => happyGoto(p, action_223, j),
        73 => happyGoto(p, action_224, j),
        75 => happyGoto(p, action_523, j),
        76 => happyGoto(p, action_102, j),
        77 => happyGoto(p, action_103, j),
        78 => happyGoto(p, action_481, j),
        _ => happyFail(p, j)
    }
}

fn action_467(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_475, j),
        150 => happyShift(p, action_476, j),
        185 => happyShift(p, action_113, j),
        186 => happyShift(p, action_171, j),
        188 => happyShift(p, action_115, j),
        190 => happyShift(p, action_116, j),
        192 => happyShift(p, action_117, j),
        193 => happyShift(p, action_172, j),
        195 => happyShift(p, action_118, j),
        198 => happyShift(p, action_119, j),
        200 => happyShift(p, action_120, j),
        201 => happyShift(p, action_121, j),
        202 => happyShift(p, action_122, j),
        207 => happyShift(p, action_123, j),
        208 => happyShift(p, action_124, j),
        209 => happyShift(p, action_125, j),
        210 => happyShift(p, action_126, j),
        212 => happyShift(p, action_127, j),
        213 => happyShift(p, action_173, j),
        214 => happyShift(p, action_174, j),
        215 => happyShift(p, action_128, j),
        216 => happyShift(p, action_175, j),
        218 => happyShift(p, action_129, j),
        219 => happyShift(p, action_130, j),
        221 => happyShift(p, action_131, j),
        223 => happyShift(p, action_133, j),
        225 => happyShift(p, action_134, j),
        226 => happyShift(p, action_191, j),
        227 => happyShift(p, action_136, j),
        228 => happyShift(p, action_137, j),
        229 => happyShift(p, action_138, j),
        230 => happyShift(p, action_139, j),
        231 => happyShift(p, action_176, j),
        237 => happyShift(p, action_140, j),
        238 => happyShift(p, action_192, j),
        239 => happyShift(p, action_142, j),
        40 => happyGoto(p, action_184, j),
        41 => happyGoto(p, action_85, j),
        42 => happyGoto(p, action_86, j),
        43 => happyGoto(p, action_87, j),
        45 => happyGoto(p, action_185, j),
        52 => happyGoto(p, action_186, j),
        53 => happyGoto(p, action_97, j),
        54 => happyGoto(p, action_98, j),
        61 => happyGoto(p, action_99, j),
        64 => happyGoto(p, action_187, j),
        75 => happyGoto(p, action_521, j),
        76 => happyGoto(p, action_102, j),
        77 => happyGoto(p, action_103, j),
        78 => happyGoto(p, action_481, j),
        133 => happyGoto(p, action_522, j),
        134 => happyGoto(p, action_110, j),
        _ => happyFail(p, j)
    }
}

fn action_468(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_475, j),
        150 => happyShift(p, action_476, j),
        185 => happyShift(p, action_113, j),
        188 => happyShift(p, action_115, j),
        190 => happyShift(p, action_116, j),
        192 => happyShift(p, action_117, j),
        195 => happyShift(p, action_118, j),
        198 => happyShift(p, action_119, j),
        200 => happyShift(p, action_120, j),
        201 => happyShift(p, action_121, j),
        202 => happyShift(p, action_122, j),
        207 => happyShift(p, action_123, j),
        208 => happyShift(p, action_124, j),
        209 => happyShift(p, action_125, j),
        210 => happyShift(p, action_126, j),
        212 => happyShift(p, action_127, j),
        215 => happyShift(p, action_128, j),
        218 => happyShift(p, action_129, j),
        219 => happyShift(p, action_130, j),
        221 => happyShift(p, action_131, j),
        223 => happyShift(p, action_133, j),
        225 => happyShift(p, action_134, j),
        226 => happyShift(p, action_168, j),
        227 => happyShift(p, action_136, j),
        228 => happyShift(p, action_137, j),
        229 => happyShift(p, action_138, j),
        230 => happyShift(p, action_139, j),
        237 => happyShift(p, action_140, j),
        238 => happyShift(p, action_169, j),
        239 => happyShift(p, action_142, j),
        40 => happyGoto(p, action_162, j),
        41 => happyGoto(p, action_85, j),
        42 => happyGoto(p, action_86, j),
        43 => happyGoto(p, action_87, j),
        45 => happyGoto(p, action_163, j),
        52 => happyGoto(p, action_164, j),
        53 => happyGoto(p, action_97, j),
        54 => happyGoto(p, action_98, j),
        61 => happyGoto(p, action_99, j),
        75 => happyGoto(p, action_520, j),
        76 => happyGoto(p, action_102, j),
        77 => happyGoto(p, action_103, j),
        78 => happyGoto(p, action_481, j),
        134 => happyGoto(p, action_167, j),
        _ => happyReduce_472(p, j)
    }
}

fn action_469(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_26, j),
        144 => happyShift(p, action_27, j),
        145 => happyShift(p, action_28, j),
        146 => happyShift(p, action_29, j),
        147 => happyShift(p, action_30, j),
        148 => happyShift(p, action_31, j),
        149 => happyShift(p, action_32, j),
        150 => happyShift(p, action_33, j),
        153 => happyShift(p, action_34, j),
        164 => happyShift(p, action_35, j),
        184 => happyShift(p, action_36, j),
        190 => happyShift(p, action_116, j),
        192 => happyShift(p, action_117, j),
        195 => happyShift(p, action_118, j),
        198 => happyShift(p, action_119, j),
        200 => happyShift(p, action_120, j),
        202 => happyShift(p, action_122, j),
        204 => happyShift(p, action_37, j),
        208 => happyShift(p, action_124, j),
        209 => happyShift(p, action_125, j),
        210 => happyShift(p, action_126, j),
        218 => happyShift(p, action_129, j),
        219 => happyShift(p, action_130, j),
        220 => happyShift(p, action_38, j),
        223 => happyShift(p, action_133, j),
        226 => happyShift(p, action_135, j),
        228 => happyShift(p, action_137, j),
        229 => happyShift(p, action_138, j),
        230 => happyShift(p, action_139, j),
        233 => happyShift(p, action_39, j),
        234 => happyShift(p, action_40, j),
        235 => happyShift(p, action_41, j),
        236 => happyShift(p, action_42, j),
        237 => happyShift(p, action_43, j),
        238 => happyShift(p, action_141, j),
        239 => happyShift(p, action_142, j),
        240 => happyShift(p, action_44, j),
        241 => happyShift(p, action_45, j),
        242 => happyShift(p, action_46, j),
        243 => happyShift(p, action_47, j),
        244 => happyShift(p, action_48, j),
        245 => happyShift(p, action_49, j),
        44 => happyGoto(p, action_286, j),
        45 => happyGoto(p, action_89, j),
        47 => happyGoto(p, action_287, j),
        49 => happyGoto(p, action_288, j),
        51 => happyGoto(p, action_289, j),
        52 => happyGoto(p, action_96, j),
        53 => happyGoto(p, action_97, j),
        54 => happyGoto(p, action_98, j),
        61 => happyGoto(p, action_99, j),
        65 => happyGoto(p, action_290, j),
        86 => happyGoto(p, action_518, j),
        100 => happyGoto(p, action_6, j),
        104 => happyGoto(p, action_7, j),
        106 => happyGoto(p, action_8, j),
        107 => happyGoto(p, action_9, j),
        108 => happyGoto(p, action_10, j),
        109 => happyGoto(p, action_11, j),
        110 => happyGoto(p, action_12, j),
        111 => happyGoto(p, action_13, j),
        112 => happyGoto(p, action_14, j),
        113 => happyGoto(p, action_15, j),
        114 => happyGoto(p, action_16, j),
        115 => happyGoto(p, action_17, j),
        116 => happyGoto(p, action_18, j),
        117 => happyGoto(p, action_19, j),
        118 => happyGoto(p, action_20, j),
        119 => happyGoto(p, action_21, j),
        120 => happyGoto(p, action_22, j),
        122 => happyGoto(p, action_519, j),
        127 => happyGoto(p, action_24, j),
        128 => happyGoto(p, action_25, j),
        132 => happyGoto(p, action_108, j),
        133 => happyGoto(p, action_293, j),
        134 => happyGoto(p, action_110, j),
        _ => happyReduce_471(p, j)
    }
}

fn action_470(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        181 => happyShift(p, action_61, j),
        185 => happyShift(p, action_113, j),
        188 => happyShift(p, action_115, j),
        190 => happyShift(p, action_116, j),
        192 => happyShift(p, action_117, j),
        195 => happyShift(p, action_118, j),
        198 => happyShift(p, action_119, j),
        200 => happyShift(p, action_120, j),
        201 => happyShift(p, action_121, j),
        202 => happyShift(p, action_122, j),
        207 => happyShift(p, action_123, j),
        208 => happyShift(p, action_124, j),
        209 => happyShift(p, action_125, j),
        210 => happyShift(p, action_126, j),
        212 => happyShift(p, action_127, j),
        215 => happyShift(p, action_128, j),
        218 => happyShift(p, action_129, j),
        219 => happyShift(p, action_130, j),
        221 => happyShift(p, action_131, j),
        222 => happyShift(p, action_132, j),
        223 => happyShift(p, action_133, j),
        225 => happyShift(p, action_134, j),
        226 => happyShift(p, action_135, j),
        227 => happyShift(p, action_136, j),
        228 => happyShift(p, action_137, j),
        229 => happyShift(p, action_138, j),
        230 => happyShift(p, action_139, j),
        238 => happyShift(p, action_141, j),
        239 => happyShift(p, action_142, j),
        14 => happyGoto(p, action_517, j),
        32 => happyGoto(p, action_463, j),
        34 => happyGoto(p, action_80, j),
        36 => happyGoto(p, action_81, j),
        37 => happyGoto(p, action_464, j),
        38 => happyGoto(p, action_465, j),
        40 => happyGoto(p, action_84, j),
        41 => happyGoto(p, action_85, j),
        42 => happyGoto(p, action_86, j),
        43 => happyGoto(p, action_87, j),
        44 => happyGoto(p, action_466, j),
        45 => happyGoto(p, action_89, j),
        46 => happyGoto(p, action_90, j),
        47 => happyGoto(p, action_91, j),
        48 => happyGoto(p, action_92, j),
        49 => happyGoto(p, action_93, j),
        50 => happyGoto(p, action_94, j),
        51 => happyGoto(p, action_95, j),
        52 => happyGoto(p, action_96, j),
        53 => happyGoto(p, action_97, j),
        54 => happyGoto(p, action_98, j),
        61 => happyGoto(p, action_99, j),
        65 => happyGoto(p, action_467, j),
        132 => happyGoto(p, action_108, j),
        133 => happyGoto(p, action_468, j),
        134 => happyGoto(p, action_110, j),
        _ => happyReduce_471(p, j)
    }
}

fn action_471(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        168 => happyShift(p, action_516, j),
        94 => happyGoto(p, action_515, j),
        _ => happyReduce_344(p, j)
    }
}

fn action_472(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_13(p, j)
    }
}

fn action_473(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        139 => happyShift(p, action_514, j),
        _ => happyFail(p, j)
    }
}

fn action_474(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_413, j),
        139 => happyShift(p, action_513, j),
        140 => happyShift(p, action_182, j),
        88 => happyGoto(p, action_178, j),
        89 => happyGoto(p, action_179, j),
        90 => happyGoto(p, action_180, j),
        _ => happyFail(p, j)
    }
}

fn action_475(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_475, j),
        150 => happyShift(p, action_476, j),
        237 => happyShift(p, action_140, j),
        239 => happyShift(p, action_142, j),
        76 => happyGoto(p, action_157, j),
        77 => happyGoto(p, action_103, j),
        78 => happyGoto(p, action_512, j),
        133 => happyGoto(p, action_160, j),
        134 => happyGoto(p, action_110, j),
        _ => happyFail(p, j)
    }
}

fn action_476(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_475, j),
        150 => happyShift(p, action_476, j),
        237 => happyShift(p, action_140, j),
        239 => happyShift(p, action_142, j),
        65 => happyGoto(p, action_511, j),
        75 => happyGoto(p, action_154, j),
        76 => happyGoto(p, action_102, j),
        77 => happyGoto(p, action_103, j),
        78 => happyGoto(p, action_481, j),
        132 => happyGoto(p, action_108, j),
        133 => happyGoto(p, action_156, j),
        134 => happyGoto(p, action_110, j),
        _ => happyReduce_471(p, j)
    }
}

fn action_477(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_413, j),
        140 => happyShift(p, action_182, j),
        88 => happyGoto(p, action_510, j),
        89 => happyGoto(p, action_179, j),
        90 => happyGoto(p, action_180, j),
        _ => happyReduce_280(p, j)
    }
}

fn action_478(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_273(p, j)
    }
}

fn action_479(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_413, j),
        140 => happyShift(p, action_182, j),
        88 => happyGoto(p, action_509, j),
        89 => happyGoto(p, action_179, j),
        90 => happyGoto(p, action_180, j),
        _ => happyReduce_268(p, j)
    }
}

fn action_480(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_264(p, j)
    }
}

fn action_481(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_413, j),
        140 => happyShift(p, action_182, j),
        88 => happyGoto(p, action_178, j),
        89 => happyGoto(p, action_179, j),
        90 => happyGoto(p, action_180, j),
        _ => happyReduce_261(p, j)
    }
}

fn action_482(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_265(p, j)
    }
}

fn action_483(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_278(p, j)
    }
}

fn action_484(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_475, j),
        150 => happyShift(p, action_476, j),
        186 => happyShift(p, action_171, j),
        193 => happyShift(p, action_172, j),
        213 => happyShift(p, action_173, j),
        214 => happyShift(p, action_174, j),
        216 => happyShift(p, action_175, j),
        231 => happyShift(p, action_176, j),
        237 => happyShift(p, action_140, j),
        239 => happyShift(p, action_142, j),
        64 => happyGoto(p, action_435, j),
        75 => happyGoto(p, action_508, j),
        76 => happyGoto(p, action_102, j),
        77 => happyGoto(p, action_103, j),
        78 => happyGoto(p, action_481, j),
        134 => happyGoto(p, action_167, j),
        _ => happyFail(p, j)
    }
}

fn action_485(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        139 => happyShift(p, action_507, j),
        _ => happyFail(p, j)
    }
}

fn action_486(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        139 => happyShift(p, action_506, j),
        _ => happyFail(p, j)
    }
}

fn action_487(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        139 => happyShift(p, action_505, j),
        _ => happyFail(p, j)
    }
}

fn action_488(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        181 => happyShift(p, action_504, j),
        _ => happyReduce_214(p, j)
    }
}

fn action_489(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        237 => happyShift(p, action_254, j),
        238 => happyShift(p, action_75, j),
        62 => happyGoto(p, action_501, j),
        63 => happyGoto(p, action_502, j),
        131 => happyGoto(p, action_503, j),
        _ => happyFail(p, j)
    }
}

fn action_490(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        179 => happyShift(p, action_500, j),
        _ => happyFail(p, j)
    }
}

fn action_491(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        139 => happyShift(p, action_499, j),
        _ => happyFail(p, j)
    }
}

fn action_492(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        139 => happyShift(p, action_498, j),
        _ => happyFail(p, j)
    }
}

fn action_493(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        193 => happyShift(p, action_496, j),
        237 => happyShift(p, action_497, j),
        135 => happyGoto(p, action_494, j),
        136 => happyGoto(p, action_495, j),
        _ => happyReduce_478(p, j)
    }
}

fn action_494(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        139 => happyShift(p, action_765, j),
        179 => happyShift(p, action_766, j),
        _ => happyFail(p, j)
    }
}

fn action_495(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_476(p, j)
    }
}

fn action_496(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_480(p, j)
    }
}

fn action_497(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_764, j),
        _ => happyReduce_479(p, j)
    }
}

fn action_498(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_171(p, j)
    }
}

fn action_499(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_172(p, j)
    }
}

fn action_500(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        236 => happyShift(p, action_42, j),
        128 => happyGoto(p, action_763, j),
        _ => happyFail(p, j)
    }
}

fn action_501(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        179 => happyShift(p, action_761, j),
        182 => happyShift(p, action_762, j),
        _ => happyFail(p, j)
    }
}

fn action_502(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_215(p, j)
    }
}

fn action_503(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        168 => happyShift(p, action_760, j),
        239 => happyShift(p, action_142, j),
        133 => happyGoto(p, action_759, j),
        134 => happyGoto(p, action_110, j),
        _ => happyReduce_217(p, j)
    }
}

fn action_504(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        237 => happyShift(p, action_254, j),
        238 => happyShift(p, action_75, j),
        62 => happyGoto(p, action_758, j),
        63 => happyGoto(p, action_502, j),
        131 => happyGoto(p, action_503, j),
        _ => happyFail(p, j)
    }
}

fn action_505(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        180 => happyShift(p, action_757, j),
        _ => happyFail(p, j)
    }
}

fn action_506(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_126(p, j)
    }
}

fn action_507(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_125(p, j)
    }
}

fn action_508(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_266(p, j)
    }
}

fn action_509(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_269(p, j)
    }
}

fn action_510(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_281(p, j)
    }
}

fn action_511(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_475, j),
        150 => happyShift(p, action_476, j),
        186 => happyShift(p, action_171, j),
        193 => happyShift(p, action_172, j),
        213 => happyShift(p, action_173, j),
        214 => happyShift(p, action_174, j),
        216 => happyShift(p, action_175, j),
        231 => happyShift(p, action_176, j),
        237 => happyShift(p, action_140, j),
        239 => happyShift(p, action_142, j),
        64 => happyGoto(p, action_187, j),
        75 => happyGoto(p, action_482, j),
        76 => happyGoto(p, action_102, j),
        77 => happyGoto(p, action_103, j),
        78 => happyGoto(p, action_481, j),
        133 => happyGoto(p, action_484, j),
        134 => happyGoto(p, action_110, j),
        _ => happyFail(p, j)
    }
}

fn action_512(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_413, j),
        139 => happyShift(p, action_478, j),
        140 => happyShift(p, action_182, j),
        88 => happyGoto(p, action_178, j),
        89 => happyGoto(p, action_179, j),
        90 => happyGoto(p, action_180, j),
        _ => happyFail(p, j)
    }
}

fn action_513(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_274(p, j)
    }
}

fn action_514(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_413, j),
        140 => happyShift(p, action_182, j),
        88 => happyGoto(p, action_756, j),
        89 => happyGoto(p, action_179, j),
        90 => happyGoto(p, action_180, j),
        _ => happyReduce_270(p, j)
    }
}

fn action_515(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_95(p, j)
    }
}

fn action_516(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_26, j),
        144 => happyShift(p, action_27, j),
        145 => happyShift(p, action_28, j),
        146 => happyShift(p, action_29, j),
        147 => happyShift(p, action_30, j),
        148 => happyShift(p, action_31, j),
        149 => happyShift(p, action_32, j),
        150 => happyShift(p, action_33, j),
        153 => happyShift(p, action_34, j),
        164 => happyShift(p, action_35, j),
        181 => happyShift(p, action_654, j),
        184 => happyShift(p, action_36, j),
        204 => happyShift(p, action_37, j),
        220 => happyShift(p, action_38, j),
        233 => happyShift(p, action_39, j),
        234 => happyShift(p, action_40, j),
        235 => happyShift(p, action_41, j),
        236 => happyShift(p, action_42, j),
        237 => happyShift(p, action_43, j),
        240 => happyShift(p, action_44, j),
        241 => happyShift(p, action_45, j),
        242 => happyShift(p, action_46, j),
        243 => happyShift(p, action_47, j),
        244 => happyShift(p, action_48, j),
        245 => happyShift(p, action_49, j),
        93 => happyGoto(p, action_755, j),
        100 => happyGoto(p, action_6, j),
        104 => happyGoto(p, action_7, j),
        106 => happyGoto(p, action_8, j),
        107 => happyGoto(p, action_9, j),
        108 => happyGoto(p, action_10, j),
        109 => happyGoto(p, action_11, j),
        110 => happyGoto(p, action_12, j),
        111 => happyGoto(p, action_13, j),
        112 => happyGoto(p, action_14, j),
        113 => happyGoto(p, action_15, j),
        114 => happyGoto(p, action_16, j),
        115 => happyGoto(p, action_17, j),
        116 => happyGoto(p, action_18, j),
        117 => happyGoto(p, action_19, j),
        118 => happyGoto(p, action_20, j),
        119 => happyGoto(p, action_21, j),
        120 => happyGoto(p, action_650, j),
        127 => happyGoto(p, action_24, j),
        128 => happyGoto(p, action_25, j),
        _ => happyFail(p, j)
    }
}

fn action_517(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_20(p, j)
    }
}

fn action_518(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        139 => happyShift(p, action_754, j),
        _ => happyFail(p, j)
    }
}

fn action_519(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        139 => happyShift(p, action_753, j),
        _ => happyFail(p, j)
    }
}

fn action_520(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        187 => happyShift(p, action_406, j),
        35 => happyGoto(p, action_471, j),
        67 => happyGoto(p, action_405, j),
        _ => happyReduce_232(p, j)
    }
}

fn action_521(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        187 => happyShift(p, action_406, j),
        35 => happyGoto(p, action_441, j),
        67 => happyGoto(p, action_405, j),
        _ => happyReduce_232(p, j)
    }
}

fn action_522(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_475, j),
        150 => happyShift(p, action_476, j),
        185 => happyShift(p, action_113, j),
        186 => happyShift(p, action_171, j),
        188 => happyShift(p, action_115, j),
        190 => happyShift(p, action_116, j),
        192 => happyShift(p, action_117, j),
        193 => happyShift(p, action_172, j),
        195 => happyShift(p, action_118, j),
        198 => happyShift(p, action_119, j),
        200 => happyShift(p, action_120, j),
        201 => happyShift(p, action_121, j),
        202 => happyShift(p, action_122, j),
        207 => happyShift(p, action_123, j),
        208 => happyShift(p, action_124, j),
        209 => happyShift(p, action_125, j),
        210 => happyShift(p, action_126, j),
        212 => happyShift(p, action_127, j),
        213 => happyShift(p, action_173, j),
        214 => happyShift(p, action_174, j),
        215 => happyShift(p, action_128, j),
        216 => happyShift(p, action_175, j),
        218 => happyShift(p, action_129, j),
        219 => happyShift(p, action_130, j),
        221 => happyShift(p, action_131, j),
        223 => happyShift(p, action_133, j),
        225 => happyShift(p, action_134, j),
        226 => happyShift(p, action_438, j),
        227 => happyShift(p, action_136, j),
        228 => happyShift(p, action_137, j),
        229 => happyShift(p, action_138, j),
        230 => happyShift(p, action_139, j),
        231 => happyShift(p, action_176, j),
        237 => happyShift(p, action_140, j),
        238 => happyShift(p, action_439, j),
        239 => happyShift(p, action_142, j),
        40 => happyGoto(p, action_432, j),
        41 => happyGoto(p, action_85, j),
        42 => happyGoto(p, action_86, j),
        43 => happyGoto(p, action_87, j),
        45 => happyGoto(p, action_433, j),
        52 => happyGoto(p, action_434, j),
        53 => happyGoto(p, action_97, j),
        54 => happyGoto(p, action_98, j),
        61 => happyGoto(p, action_99, j),
        64 => happyGoto(p, action_435, j),
        75 => happyGoto(p, action_752, j),
        76 => happyGoto(p, action_102, j),
        77 => happyGoto(p, action_103, j),
        78 => happyGoto(p, action_481, j),
        134 => happyGoto(p, action_167, j),
        _ => happyFail(p, j)
    }
}

fn action_523(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_230(p, j)
    }
}

fn action_524(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_751, j),
        150 => happyShift(p, action_525, j),
        237 => happyShift(p, action_140, j),
        238 => happyShift(p, action_424, j),
        239 => happyShift(p, action_142, j),
        70 => happyGoto(p, action_419, j),
        71 => happyGoto(p, action_222, j),
        72 => happyGoto(p, action_420, j),
        73 => happyGoto(p, action_224, j),
        74 => happyGoto(p, action_421, j),
        76 => happyGoto(p, action_157, j),
        77 => happyGoto(p, action_103, j),
        78 => happyGoto(p, action_512, j),
        133 => happyGoto(p, action_422, j),
        134 => happyGoto(p, action_110, j),
        _ => happyFail(p, j)
    }
}

fn action_525(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_750, j),
        150 => happyShift(p, action_525, j),
        237 => happyShift(p, action_140, j),
        238 => happyShift(p, action_229, j),
        239 => happyShift(p, action_142, j),
        65 => happyGoto(p, action_749, j),
        69 => happyGoto(p, action_415, j),
        70 => happyGoto(p, action_221, j),
        71 => happyGoto(p, action_222, j),
        72 => happyGoto(p, action_416, j),
        73 => happyGoto(p, action_224, j),
        75 => happyGoto(p, action_154, j),
        76 => happyGoto(p, action_102, j),
        77 => happyGoto(p, action_103, j),
        78 => happyGoto(p, action_481, j),
        132 => happyGoto(p, action_108, j),
        133 => happyGoto(p, action_417, j),
        134 => happyGoto(p, action_110, j),
        _ => happyReduce_471(p, j)
    }
}

fn action_526(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        187 => happyShift(p, action_406, j),
        35 => happyGoto(p, action_410, j),
        67 => happyGoto(p, action_405, j),
        _ => happyReduce_232(p, j)
    }
}

fn action_527(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_279(p, j)
    }
}

fn action_528(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        237 => happyShift(p, action_748, j),
        _ => happyFail(p, j)
    }
}

fn action_529(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        183 => happyShift(p, action_747, j),
        185 => happyShift(p, action_113, j),
        188 => happyShift(p, action_115, j),
        190 => happyShift(p, action_116, j),
        192 => happyShift(p, action_117, j),
        195 => happyShift(p, action_118, j),
        198 => happyShift(p, action_119, j),
        200 => happyShift(p, action_120, j),
        201 => happyShift(p, action_121, j),
        202 => happyShift(p, action_122, j),
        207 => happyShift(p, action_123, j),
        208 => happyShift(p, action_124, j),
        209 => happyShift(p, action_125, j),
        210 => happyShift(p, action_126, j),
        212 => happyShift(p, action_127, j),
        215 => happyShift(p, action_128, j),
        218 => happyShift(p, action_129, j),
        219 => happyShift(p, action_130, j),
        221 => happyShift(p, action_131, j),
        223 => happyShift(p, action_133, j),
        225 => happyShift(p, action_134, j),
        226 => happyShift(p, action_135, j),
        227 => happyShift(p, action_136, j),
        228 => happyShift(p, action_137, j),
        229 => happyShift(p, action_138, j),
        230 => happyShift(p, action_139, j),
        238 => happyShift(p, action_141, j),
        239 => happyShift(p, action_142, j),
        37 => happyGoto(p, action_449, j),
        38 => happyGoto(p, action_450, j),
        40 => happyGoto(p, action_84, j),
        41 => happyGoto(p, action_85, j),
        42 => happyGoto(p, action_86, j),
        43 => happyGoto(p, action_87, j),
        44 => happyGoto(p, action_451, j),
        45 => happyGoto(p, action_89, j),
        46 => happyGoto(p, action_90, j),
        47 => happyGoto(p, action_91, j),
        48 => happyGoto(p, action_452, j),
        49 => happyGoto(p, action_453, j),
        50 => happyGoto(p, action_94, j),
        51 => happyGoto(p, action_95, j),
        52 => happyGoto(p, action_96, j),
        53 => happyGoto(p, action_97, j),
        54 => happyGoto(p, action_98, j),
        61 => happyGoto(p, action_99, j),
        65 => happyGoto(p, action_454, j),
        84 => happyGoto(p, action_746, j),
        132 => happyGoto(p, action_108, j),
        133 => happyGoto(p, action_459, j),
        134 => happyGoto(p, action_110, j),
        _ => happyReduce_471(p, j)
    }
}

fn action_530(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_312(p, j)
    }
}

fn action_531(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        239 => happyShift(p, action_142, j),
        132 => happyGoto(p, action_745, j),
        133 => happyGoto(p, action_150, j),
        134 => happyGoto(p, action_110, j),
        _ => happyReduce_471(p, j)
    }
}

fn action_532(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_300(p, j)
    }
}

fn action_533(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        185 => happyShift(p, action_113, j),
        186 => happyShift(p, action_171, j),
        188 => happyShift(p, action_115, j),
        190 => happyShift(p, action_116, j),
        192 => happyShift(p, action_117, j),
        193 => happyShift(p, action_172, j),
        195 => happyShift(p, action_118, j),
        198 => happyShift(p, action_119, j),
        200 => happyShift(p, action_120, j),
        201 => happyShift(p, action_121, j),
        202 => happyShift(p, action_122, j),
        207 => happyShift(p, action_123, j),
        208 => happyShift(p, action_124, j),
        209 => happyShift(p, action_125, j),
        210 => happyShift(p, action_126, j),
        212 => happyShift(p, action_127, j),
        213 => happyShift(p, action_173, j),
        214 => happyShift(p, action_174, j),
        215 => happyShift(p, action_128, j),
        216 => happyShift(p, action_175, j),
        218 => happyShift(p, action_129, j),
        219 => happyShift(p, action_130, j),
        221 => happyShift(p, action_131, j),
        223 => happyShift(p, action_133, j),
        225 => happyShift(p, action_134, j),
        226 => happyShift(p, action_438, j),
        227 => happyShift(p, action_136, j),
        228 => happyShift(p, action_137, j),
        229 => happyShift(p, action_138, j),
        230 => happyShift(p, action_139, j),
        231 => happyShift(p, action_176, j),
        238 => happyShift(p, action_439, j),
        239 => happyShift(p, action_142, j),
        40 => happyGoto(p, action_432, j),
        41 => happyGoto(p, action_85, j),
        42 => happyGoto(p, action_86, j),
        43 => happyGoto(p, action_87, j),
        45 => happyGoto(p, action_433, j),
        52 => happyGoto(p, action_434, j),
        53 => happyGoto(p, action_97, j),
        54 => happyGoto(p, action_98, j),
        61 => happyGoto(p, action_99, j),
        64 => happyGoto(p, action_435, j),
        134 => happyGoto(p, action_167, j),
        _ => happyFail(p, j)
    }
}

fn action_534(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        185 => happyReduce_473(p, j),
        186 => happyReduce_473(p, j),
        188 => happyReduce_473(p, j),
        190 => happyReduce_473(p, j),
        192 => happyReduce_473(p, j),
        193 => happyReduce_473(p, j),
        195 => happyReduce_473(p, j),
        198 => happyReduce_473(p, j),
        200 => happyReduce_473(p, j),
        201 => happyReduce_473(p, j),
        202 => happyReduce_473(p, j),
        207 => happyReduce_473(p, j),
        208 => happyReduce_473(p, j),
        209 => happyReduce_473(p, j),
        210 => happyReduce_473(p, j),
        212 => happyReduce_473(p, j),
        213 => happyReduce_473(p, j),
        214 => happyReduce_473(p, j),
        215 => happyReduce_473(p, j),
        216 => happyReduce_473(p, j),
        218 => happyReduce_473(p, j),
        219 => happyReduce_473(p, j),
        221 => happyReduce_473(p, j),
        223 => happyReduce_473(p, j),
        225 => happyReduce_473(p, j),
        226 => happyReduce_473(p, j),
        227 => happyReduce_473(p, j),
        228 => happyReduce_473(p, j),
        229 => happyReduce_473(p, j),
        230 => happyReduce_473(p, j),
        231 => happyReduce_473(p, j),
        238 => happyReduce_473(p, j),
        239 => happyReduce_473(p, j),
        _ => happyReduce_299(p, j)
    }
}

fn action_535(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_535, j),
        140 => happyShift(p, action_182, j),
        150 => happyShift(p, action_536, j),
        185 => happyShift(p, action_113, j),
        186 => happyReduce_471(p, j),
        188 => happyShift(p, action_115, j),
        190 => happyShift(p, action_116, j),
        192 => happyShift(p, action_117, j),
        193 => happyReduce_471(p, j),
        195 => happyShift(p, action_118, j),
        198 => happyShift(p, action_119, j),
        200 => happyShift(p, action_120, j),
        201 => happyShift(p, action_121, j),
        202 => happyShift(p, action_122, j),
        207 => happyShift(p, action_123, j),
        208 => happyShift(p, action_124, j),
        209 => happyShift(p, action_125, j),
        210 => happyShift(p, action_126, j),
        212 => happyShift(p, action_127, j),
        213 => happyReduce_471(p, j),
        214 => happyReduce_471(p, j),
        215 => happyShift(p, action_128, j),
        216 => happyReduce_471(p, j),
        218 => happyShift(p, action_129, j),
        219 => happyShift(p, action_130, j),
        221 => happyShift(p, action_131, j),
        223 => happyShift(p, action_133, j),
        225 => happyShift(p, action_134, j),
        226 => happyShift(p, action_135, j),
        227 => happyShift(p, action_136, j),
        228 => happyShift(p, action_137, j),
        229 => happyShift(p, action_138, j),
        230 => happyShift(p, action_139, j),
        231 => happyReduce_471(p, j),
        237 => happyShift(p, action_140, j),
        238 => happyShift(p, action_141, j),
        239 => happyShift(p, action_142, j),
        37 => happyGoto(p, action_449, j),
        38 => happyGoto(p, action_450, j),
        40 => happyGoto(p, action_84, j),
        41 => happyGoto(p, action_85, j),
        42 => happyGoto(p, action_86, j),
        43 => happyGoto(p, action_87, j),
        44 => happyGoto(p, action_451, j),
        45 => happyGoto(p, action_89, j),
        46 => happyGoto(p, action_90, j),
        47 => happyGoto(p, action_91, j),
        48 => happyGoto(p, action_452, j),
        49 => happyGoto(p, action_453, j),
        50 => happyGoto(p, action_94, j),
        51 => happyGoto(p, action_95, j),
        52 => happyGoto(p, action_96, j),
        53 => happyGoto(p, action_97, j),
        54 => happyGoto(p, action_98, j),
        61 => happyGoto(p, action_99, j),
        65 => happyGoto(p, action_454, j),
        76 => happyGoto(p, action_157, j),
        77 => happyGoto(p, action_103, j),
        78 => happyGoto(p, action_512, j),
        82 => happyGoto(p, action_455, j),
        83 => happyGoto(p, action_456, j),
        84 => happyGoto(p, action_457, j),
        88 => happyGoto(p, action_628, j),
        89 => happyGoto(p, action_179, j),
        90 => happyGoto(p, action_180, j),
        91 => happyGoto(p, action_629, j),
        92 => happyGoto(p, action_630, j),
        132 => happyGoto(p, action_108, j),
        133 => happyGoto(p, action_744, j),
        134 => happyGoto(p, action_110, j),
        _ => happyReduce_282(p, j)
    }
}

fn action_536(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_535, j),
        140 => happyShift(p, action_182, j),
        150 => happyShift(p, action_536, j),
        186 => happyReduce_471(p, j),
        193 => happyReduce_471(p, j),
        213 => happyReduce_471(p, j),
        214 => happyReduce_471(p, j),
        216 => happyReduce_471(p, j),
        231 => happyReduce_471(p, j),
        237 => happyShift(p, action_140, j),
        239 => happyShift(p, action_142, j),
        65 => happyGoto(p, action_742, j),
        75 => happyGoto(p, action_154, j),
        76 => happyGoto(p, action_102, j),
        77 => happyGoto(p, action_103, j),
        78 => happyGoto(p, action_481, j),
        87 => happyGoto(p, action_626, j),
        88 => happyGoto(p, action_367, j),
        89 => happyGoto(p, action_179, j),
        90 => happyGoto(p, action_180, j),
        91 => happyGoto(p, action_368, j),
        92 => happyGoto(p, action_369, j),
        132 => happyGoto(p, action_108, j),
        133 => happyGoto(p, action_743, j),
        134 => happyGoto(p, action_110, j),
        _ => happyReduce_326(p, j)
    }
}

fn action_537(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        239 => happyShift(p, action_142, j),
        132 => happyGoto(p, action_741, j),
        133 => happyGoto(p, action_150, j),
        134 => happyGoto(p, action_110, j),
        _ => happyReduce_471(p, j)
    }
}

fn action_538(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        239 => happyShift(p, action_142, j),
        132 => happyGoto(p, action_740, j),
        133 => happyGoto(p, action_150, j),
        134 => happyGoto(p, action_110, j),
        _ => happyReduce_471(p, j)
    }
}

fn action_539(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_295(p, j)
    }
}

fn action_540(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_540, j),
        140 => happyShift(p, action_182, j),
        150 => happyShift(p, action_541, j),
        185 => happyShift(p, action_113, j),
        186 => happyReduce_471(p, j),
        188 => happyShift(p, action_115, j),
        190 => happyShift(p, action_116, j),
        192 => happyShift(p, action_117, j),
        193 => happyReduce_471(p, j),
        195 => happyShift(p, action_118, j),
        198 => happyShift(p, action_119, j),
        200 => happyShift(p, action_120, j),
        201 => happyShift(p, action_121, j),
        202 => happyShift(p, action_122, j),
        207 => happyShift(p, action_123, j),
        208 => happyShift(p, action_124, j),
        209 => happyShift(p, action_125, j),
        210 => happyShift(p, action_126, j),
        212 => happyShift(p, action_127, j),
        213 => happyReduce_471(p, j),
        214 => happyReduce_471(p, j),
        215 => happyShift(p, action_128, j),
        216 => happyReduce_471(p, j),
        218 => happyShift(p, action_129, j),
        219 => happyShift(p, action_130, j),
        221 => happyShift(p, action_131, j),
        223 => happyShift(p, action_133, j),
        225 => happyShift(p, action_134, j),
        226 => happyShift(p, action_135, j),
        227 => happyShift(p, action_136, j),
        228 => happyShift(p, action_137, j),
        229 => happyShift(p, action_138, j),
        230 => happyShift(p, action_139, j),
        231 => happyReduce_471(p, j),
        237 => happyShift(p, action_140, j),
        238 => happyShift(p, action_141, j),
        239 => happyShift(p, action_142, j),
        37 => happyGoto(p, action_449, j),
        38 => happyGoto(p, action_450, j),
        40 => happyGoto(p, action_84, j),
        41 => happyGoto(p, action_85, j),
        42 => happyGoto(p, action_86, j),
        43 => happyGoto(p, action_87, j),
        44 => happyGoto(p, action_451, j),
        45 => happyGoto(p, action_89, j),
        46 => happyGoto(p, action_90, j),
        47 => happyGoto(p, action_91, j),
        48 => happyGoto(p, action_452, j),
        49 => happyGoto(p, action_453, j),
        50 => happyGoto(p, action_94, j),
        51 => happyGoto(p, action_95, j),
        52 => happyGoto(p, action_96, j),
        53 => happyGoto(p, action_97, j),
        54 => happyGoto(p, action_98, j),
        61 => happyGoto(p, action_99, j),
        65 => happyGoto(p, action_454, j),
        70 => happyGoto(p, action_419, j),
        71 => happyGoto(p, action_222, j),
        76 => happyGoto(p, action_157, j),
        77 => happyGoto(p, action_103, j),
        78 => happyGoto(p, action_512, j),
        82 => happyGoto(p, action_455, j),
        83 => happyGoto(p, action_456, j),
        84 => happyGoto(p, action_457, j),
        88 => happyGoto(p, action_628, j),
        89 => happyGoto(p, action_179, j),
        90 => happyGoto(p, action_180, j),
        91 => happyGoto(p, action_629, j),
        92 => happyGoto(p, action_630, j),
        132 => happyGoto(p, action_108, j),
        133 => happyGoto(p, action_739, j),
        134 => happyGoto(p, action_110, j),
        _ => happyReduce_282(p, j)
    }
}

fn action_541(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_540, j),
        140 => happyShift(p, action_182, j),
        150 => happyShift(p, action_541, j),
        186 => happyReduce_471(p, j),
        193 => happyReduce_471(p, j),
        213 => happyReduce_471(p, j),
        214 => happyReduce_471(p, j),
        216 => happyReduce_471(p, j),
        231 => happyReduce_471(p, j),
        237 => happyShift(p, action_140, j),
        238 => happyShift(p, action_229, j),
        239 => happyShift(p, action_142, j),
        65 => happyGoto(p, action_737, j),
        69 => happyGoto(p, action_415, j),
        70 => happyGoto(p, action_221, j),
        71 => happyGoto(p, action_222, j),
        75 => happyGoto(p, action_154, j),
        76 => happyGoto(p, action_102, j),
        77 => happyGoto(p, action_103, j),
        78 => happyGoto(p, action_481, j),
        87 => happyGoto(p, action_626, j),
        88 => happyGoto(p, action_367, j),
        89 => happyGoto(p, action_179, j),
        90 => happyGoto(p, action_180, j),
        91 => happyGoto(p, action_368, j),
        92 => happyGoto(p, action_369, j),
        132 => happyGoto(p, action_108, j),
        133 => happyGoto(p, action_738, j),
        134 => happyGoto(p, action_110, j),
        _ => happyReduce_326(p, j)
    }
}

fn action_542(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        239 => happyShift(p, action_142, j),
        132 => happyGoto(p, action_736, j),
        133 => happyGoto(p, action_150, j),
        134 => happyGoto(p, action_110, j),
        _ => happyReduce_471(p, j)
    }
}

fn action_543(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_292(p, j)
    }
}

fn action_544(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        239 => happyShift(p, action_142, j),
        132 => happyGoto(p, action_735, j),
        133 => happyGoto(p, action_150, j),
        134 => happyGoto(p, action_110, j),
        _ => happyReduce_471(p, j)
    }
}

fn action_545(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        239 => happyShift(p, action_142, j),
        132 => happyGoto(p, action_734, j),
        133 => happyGoto(p, action_150, j),
        134 => happyGoto(p, action_110, j),
        _ => happyReduce_471(p, j)
    }
}

fn action_546(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_288(p, j)
    }
}

fn action_547(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        186 => happyShift(p, action_171, j),
        193 => happyShift(p, action_172, j),
        213 => happyShift(p, action_173, j),
        214 => happyShift(p, action_174, j),
        216 => happyShift(p, action_175, j),
        231 => happyShift(p, action_176, j),
        239 => happyShift(p, action_142, j),
        64 => happyGoto(p, action_187, j),
        132 => happyGoto(p, action_733, j),
        133 => happyGoto(p, action_664, j),
        134 => happyGoto(p, action_110, j),
        _ => happyReduce_471(p, j)
    }
}

fn action_548(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_26, j),
        144 => happyShift(p, action_27, j),
        145 => happyShift(p, action_28, j),
        146 => happyShift(p, action_29, j),
        147 => happyShift(p, action_30, j),
        148 => happyShift(p, action_31, j),
        149 => happyShift(p, action_32, j),
        150 => happyShift(p, action_33, j),
        153 => happyShift(p, action_34, j),
        164 => happyShift(p, action_35, j),
        184 => happyShift(p, action_36, j),
        186 => happyShift(p, action_171, j),
        193 => happyShift(p, action_172, j),
        204 => happyShift(p, action_37, j),
        213 => happyShift(p, action_173, j),
        214 => happyShift(p, action_174, j),
        216 => happyShift(p, action_175, j),
        220 => happyShift(p, action_38, j),
        231 => happyShift(p, action_176, j),
        233 => happyShift(p, action_39, j),
        234 => happyShift(p, action_40, j),
        235 => happyShift(p, action_41, j),
        236 => happyShift(p, action_42, j),
        237 => happyShift(p, action_43, j),
        240 => happyShift(p, action_44, j),
        241 => happyShift(p, action_45, j),
        242 => happyShift(p, action_46, j),
        243 => happyShift(p, action_47, j),
        244 => happyShift(p, action_48, j),
        245 => happyShift(p, action_49, j),
        64 => happyGoto(p, action_170, j),
        100 => happyGoto(p, action_6, j),
        104 => happyGoto(p, action_7, j),
        106 => happyGoto(p, action_8, j),
        107 => happyGoto(p, action_9, j),
        108 => happyGoto(p, action_10, j),
        109 => happyGoto(p, action_11, j),
        110 => happyGoto(p, action_12, j),
        111 => happyGoto(p, action_13, j),
        112 => happyGoto(p, action_14, j),
        113 => happyGoto(p, action_15, j),
        114 => happyGoto(p, action_16, j),
        115 => happyGoto(p, action_17, j),
        116 => happyGoto(p, action_18, j),
        117 => happyGoto(p, action_19, j),
        118 => happyGoto(p, action_20, j),
        119 => happyGoto(p, action_21, j),
        120 => happyGoto(p, action_732, j),
        127 => happyGoto(p, action_24, j),
        128 => happyGoto(p, action_25, j),
        _ => happyFail(p, j)
    }
}

fn action_549(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        141 => happyShift(p, action_731, j),
        _ => happyFail(p, j)
    }
}

fn action_550(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        141 => happyShift(p, action_730, j),
        _ => happyFail(p, j)
    }
}

fn action_551(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        141 => happyReduce_471(p, j),
        239 => happyShift(p, action_142, j),
        132 => happyGoto(p, action_729, j),
        133 => happyGoto(p, action_150, j),
        134 => happyGoto(p, action_110, j),
        _ => happyReduce_401(p, j)
    }
}

fn action_552(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_315(p, j)
    }
}

fn action_553(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        141 => happyShift(p, action_728, j),
        _ => happyFail(p, j)
    }
}

fn action_554(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        221 => happyShift(p, action_727, j),
        _ => happyFail(p, j)
    }
}

fn action_555(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_26, j),
        144 => happyShift(p, action_27, j),
        145 => happyShift(p, action_28, j),
        146 => happyShift(p, action_29, j),
        147 => happyShift(p, action_30, j),
        148 => happyShift(p, action_31, j),
        149 => happyShift(p, action_32, j),
        150 => happyShift(p, action_726, j),
        153 => happyShift(p, action_34, j),
        164 => happyShift(p, action_35, j),
        184 => happyShift(p, action_36, j),
        186 => happyShift(p, action_171, j),
        193 => happyShift(p, action_172, j),
        204 => happyShift(p, action_37, j),
        213 => happyShift(p, action_173, j),
        214 => happyShift(p, action_174, j),
        216 => happyShift(p, action_175, j),
        220 => happyShift(p, action_38, j),
        221 => happyReduce_472(p, j),
        231 => happyShift(p, action_176, j),
        233 => happyShift(p, action_39, j),
        234 => happyShift(p, action_40, j),
        235 => happyShift(p, action_41, j),
        236 => happyShift(p, action_42, j),
        237 => happyShift(p, action_43, j),
        239 => happyShift(p, action_142, j),
        240 => happyShift(p, action_44, j),
        241 => happyShift(p, action_45, j),
        242 => happyShift(p, action_46, j),
        243 => happyShift(p, action_47, j),
        244 => happyShift(p, action_48, j),
        245 => happyShift(p, action_49, j),
        64 => happyGoto(p, action_435, j),
        100 => happyGoto(p, action_6, j),
        104 => happyGoto(p, action_7, j),
        106 => happyGoto(p, action_8, j),
        107 => happyGoto(p, action_9, j),
        108 => happyGoto(p, action_10, j),
        109 => happyGoto(p, action_11, j),
        110 => happyGoto(p, action_12, j),
        111 => happyGoto(p, action_13, j),
        112 => happyGoto(p, action_14, j),
        113 => happyGoto(p, action_15, j),
        114 => happyGoto(p, action_16, j),
        115 => happyGoto(p, action_17, j),
        116 => happyGoto(p, action_18, j),
        117 => happyGoto(p, action_19, j),
        118 => happyGoto(p, action_20, j),
        119 => happyGoto(p, action_21, j),
        120 => happyGoto(p, action_444, j),
        125 => happyGoto(p, action_725, j),
        127 => happyGoto(p, action_24, j),
        128 => happyGoto(p, action_25, j),
        134 => happyGoto(p, action_167, j),
        _ => happyReduce_458(p, j)
    }
}

fn action_556(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        141 => happyReduce_471(p, j),
        239 => happyShift(p, action_142, j),
        132 => happyGoto(p, action_724, j),
        133 => happyGoto(p, action_150, j),
        134 => happyGoto(p, action_110, j),
        _ => happyReduce_401(p, j)
    }
}

fn action_557(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_93(p, j)
    }
}

fn action_558(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_24(p, j)
    }
}

fn action_559(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_26, j),
        144 => happyShift(p, action_27, j),
        145 => happyShift(p, action_28, j),
        146 => happyShift(p, action_29, j),
        147 => happyShift(p, action_30, j),
        148 => happyShift(p, action_31, j),
        149 => happyShift(p, action_32, j),
        150 => happyShift(p, action_33, j),
        153 => happyShift(p, action_34, j),
        164 => happyShift(p, action_35, j),
        184 => happyShift(p, action_36, j),
        190 => happyShift(p, action_116, j),
        192 => happyShift(p, action_117, j),
        195 => happyShift(p, action_118, j),
        198 => happyShift(p, action_119, j),
        200 => happyShift(p, action_120, j),
        202 => happyShift(p, action_122, j),
        204 => happyShift(p, action_37, j),
        208 => happyShift(p, action_124, j),
        209 => happyShift(p, action_125, j),
        210 => happyShift(p, action_126, j),
        218 => happyShift(p, action_129, j),
        219 => happyShift(p, action_130, j),
        220 => happyShift(p, action_38, j),
        223 => happyShift(p, action_133, j),
        226 => happyShift(p, action_135, j),
        228 => happyShift(p, action_137, j),
        229 => happyShift(p, action_138, j),
        230 => happyShift(p, action_139, j),
        233 => happyShift(p, action_39, j),
        234 => happyShift(p, action_40, j),
        235 => happyShift(p, action_41, j),
        236 => happyShift(p, action_42, j),
        237 => happyShift(p, action_43, j),
        238 => happyShift(p, action_141, j),
        239 => happyShift(p, action_142, j),
        240 => happyShift(p, action_44, j),
        241 => happyShift(p, action_45, j),
        242 => happyShift(p, action_46, j),
        243 => happyShift(p, action_47, j),
        244 => happyShift(p, action_48, j),
        245 => happyShift(p, action_49, j),
        44 => happyGoto(p, action_286, j),
        45 => happyGoto(p, action_89, j),
        47 => happyGoto(p, action_287, j),
        49 => happyGoto(p, action_288, j),
        51 => happyGoto(p, action_289, j),
        52 => happyGoto(p, action_96, j),
        53 => happyGoto(p, action_97, j),
        54 => happyGoto(p, action_98, j),
        61 => happyGoto(p, action_99, j),
        65 => happyGoto(p, action_290, j),
        86 => happyGoto(p, action_722, j),
        100 => happyGoto(p, action_6, j),
        104 => happyGoto(p, action_7, j),
        106 => happyGoto(p, action_8, j),
        107 => happyGoto(p, action_9, j),
        108 => happyGoto(p, action_10, j),
        109 => happyGoto(p, action_11, j),
        110 => happyGoto(p, action_12, j),
        111 => happyGoto(p, action_13, j),
        112 => happyGoto(p, action_14, j),
        113 => happyGoto(p, action_15, j),
        114 => happyGoto(p, action_16, j),
        115 => happyGoto(p, action_17, j),
        116 => happyGoto(p, action_18, j),
        117 => happyGoto(p, action_19, j),
        118 => happyGoto(p, action_20, j),
        119 => happyGoto(p, action_21, j),
        120 => happyGoto(p, action_22, j),
        122 => happyGoto(p, action_723, j),
        127 => happyGoto(p, action_24, j),
        128 => happyGoto(p, action_25, j),
        132 => happyGoto(p, action_108, j),
        133 => happyGoto(p, action_293, j),
        134 => happyGoto(p, action_110, j),
        _ => happyReduce_471(p, j)
    }
}

fn action_560(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        181 => happyShift(p, action_61, j),
        185 => happyShift(p, action_113, j),
        188 => happyShift(p, action_115, j),
        190 => happyShift(p, action_116, j),
        192 => happyShift(p, action_117, j),
        195 => happyShift(p, action_118, j),
        198 => happyShift(p, action_119, j),
        200 => happyShift(p, action_120, j),
        201 => happyShift(p, action_121, j),
        202 => happyShift(p, action_122, j),
        207 => happyShift(p, action_123, j),
        208 => happyShift(p, action_124, j),
        209 => happyShift(p, action_125, j),
        210 => happyShift(p, action_126, j),
        212 => happyShift(p, action_127, j),
        215 => happyShift(p, action_128, j),
        218 => happyShift(p, action_129, j),
        219 => happyShift(p, action_130, j),
        221 => happyShift(p, action_131, j),
        222 => happyShift(p, action_132, j),
        223 => happyShift(p, action_133, j),
        225 => happyShift(p, action_134, j),
        226 => happyShift(p, action_135, j),
        227 => happyShift(p, action_136, j),
        228 => happyShift(p, action_137, j),
        229 => happyShift(p, action_138, j),
        230 => happyShift(p, action_139, j),
        238 => happyShift(p, action_141, j),
        239 => happyShift(p, action_142, j),
        14 => happyGoto(p, action_721, j),
        32 => happyGoto(p, action_463, j),
        34 => happyGoto(p, action_80, j),
        36 => happyGoto(p, action_81, j),
        37 => happyGoto(p, action_464, j),
        38 => happyGoto(p, action_465, j),
        40 => happyGoto(p, action_84, j),
        41 => happyGoto(p, action_85, j),
        42 => happyGoto(p, action_86, j),
        43 => happyGoto(p, action_87, j),
        44 => happyGoto(p, action_466, j),
        45 => happyGoto(p, action_89, j),
        46 => happyGoto(p, action_90, j),
        47 => happyGoto(p, action_91, j),
        48 => happyGoto(p, action_92, j),
        49 => happyGoto(p, action_93, j),
        50 => happyGoto(p, action_94, j),
        51 => happyGoto(p, action_95, j),
        52 => happyGoto(p, action_96, j),
        53 => happyGoto(p, action_97, j),
        54 => happyGoto(p, action_98, j),
        61 => happyGoto(p, action_99, j),
        65 => happyGoto(p, action_467, j),
        132 => happyGoto(p, action_108, j),
        133 => happyGoto(p, action_468, j),
        134 => happyGoto(p, action_110, j),
        _ => happyReduce_471(p, j)
    }
}

fn action_561(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        168 => happyShift(p, action_516, j),
        94 => happyGoto(p, action_720, j),
        _ => happyReduce_344(p, j)
    }
}

fn action_562(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_18(p, j)
    }
}

fn action_563(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        139 => happyShift(p, action_719, j),
        _ => happyFail(p, j)
    }
}

fn action_564(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        139 => happyShift(p, action_718, j),
        _ => happyFail(p, j)
    }
}

fn action_565(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        180 => happyShift(p, action_715, j),
        182 => happyShift(p, action_716, j),
        190 => happyShift(p, action_116, j),
        192 => happyShift(p, action_117, j),
        195 => happyShift(p, action_118, j),
        198 => happyShift(p, action_119, j),
        200 => happyShift(p, action_120, j),
        202 => happyShift(p, action_122, j),
        208 => happyShift(p, action_124, j),
        209 => happyShift(p, action_125, j),
        210 => happyShift(p, action_126, j),
        218 => happyShift(p, action_129, j),
        219 => happyShift(p, action_130, j),
        223 => happyShift(p, action_133, j),
        226 => happyShift(p, action_135, j),
        228 => happyShift(p, action_137, j),
        229 => happyShift(p, action_138, j),
        230 => happyShift(p, action_139, j),
        238 => happyShift(p, action_141, j),
        239 => happyShift(p, action_142, j),
        240 => happyShift(p, action_717, j),
        44 => happyGoto(p, action_709, j),
        45 => happyGoto(p, action_89, j),
        47 => happyGoto(p, action_287, j),
        49 => happyGoto(p, action_288, j),
        51 => happyGoto(p, action_289, j),
        52 => happyGoto(p, action_96, j),
        53 => happyGoto(p, action_97, j),
        54 => happyGoto(p, action_98, j),
        56 => happyGoto(p, action_710, j),
        57 => happyGoto(p, action_711, j),
        58 => happyGoto(p, action_712, j),
        61 => happyGoto(p, action_99, j),
        65 => happyGoto(p, action_713, j),
        132 => happyGoto(p, action_108, j),
        133 => happyGoto(p, action_714, j),
        134 => happyGoto(p, action_110, j),
        _ => happyReduce_471(p, j)
    }
}

fn action_566(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        55 => happyGoto(p, action_708, j),
        _ => happyReduce_191(p, j)
    }
}

fn action_567(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_99(p, j)
    }
}

fn action_568(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_22(p, j)
    }
}

fn action_569(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_413, j),
        139 => happyShift(p, action_707, j),
        140 => happyShift(p, action_182, j),
        88 => happyGoto(p, action_573, j),
        89 => happyGoto(p, action_179, j),
        90 => happyGoto(p, action_180, j),
        _ => happyFail(p, j)
    }
}

fn action_570(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        139 => happyShift(p, action_706, j),
        _ => happyFail(p, j)
    }
}

fn action_571(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_571, j),
        150 => happyShift(p, action_572, j),
        237 => happyShift(p, action_140, j),
        239 => happyShift(p, action_142, j),
        70 => happyGoto(p, action_419, j),
        71 => happyGoto(p, action_222, j),
        76 => happyGoto(p, action_157, j),
        77 => happyGoto(p, action_103, j),
        78 => happyGoto(p, action_512, j),
        133 => happyGoto(p, action_422, j),
        134 => happyGoto(p, action_110, j),
        _ => happyFail(p, j)
    }
}

fn action_572(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_571, j),
        150 => happyShift(p, action_572, j),
        237 => happyShift(p, action_140, j),
        238 => happyShift(p, action_229, j),
        239 => happyShift(p, action_142, j),
        65 => happyGoto(p, action_705, j),
        69 => happyGoto(p, action_415, j),
        70 => happyGoto(p, action_221, j),
        71 => happyGoto(p, action_222, j),
        75 => happyGoto(p, action_154, j),
        76 => happyGoto(p, action_102, j),
        77 => happyGoto(p, action_103, j),
        78 => happyGoto(p, action_481, j),
        132 => happyGoto(p, action_108, j),
        133 => happyGoto(p, action_417, j),
        134 => happyGoto(p, action_110, j),
        _ => happyReduce_471(p, j)
    }
}

fn action_573(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        139 => happyShift(p, action_704, j),
        _ => happyFail(p, j)
    }
}

fn action_574(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_413, j),
        140 => happyShift(p, action_182, j),
        88 => happyGoto(p, action_703, j),
        89 => happyGoto(p, action_179, j),
        90 => happyGoto(p, action_180, j),
        _ => happyReduce_255(p, j)
    }
}

fn action_575(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_413, j),
        140 => happyShift(p, action_182, j),
        88 => happyGoto(p, action_702, j),
        89 => happyGoto(p, action_179, j),
        90 => happyGoto(p, action_180, j),
        _ => happyReduce_244(p, j)
    }
}

fn action_576(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_413, j),
        139 => happyShift(p, action_701, j),
        140 => happyShift(p, action_182, j),
        88 => happyGoto(p, action_573, j),
        89 => happyGoto(p, action_179, j),
        90 => happyGoto(p, action_180, j),
        _ => happyFail(p, j)
    }
}

fn action_577(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_241(p, j)
    }
}

fn action_578(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_242(p, j)
    }
}

fn action_579(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_253(p, j)
    }
}

fn action_580(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_700, j),
        150 => happyShift(p, action_525, j),
        186 => happyShift(p, action_171, j),
        193 => happyShift(p, action_172, j),
        213 => happyShift(p, action_173, j),
        214 => happyShift(p, action_174, j),
        216 => happyShift(p, action_175, j),
        231 => happyShift(p, action_176, j),
        237 => happyShift(p, action_140, j),
        238 => happyShift(p, action_229, j),
        239 => happyShift(p, action_142, j),
        64 => happyGoto(p, action_435, j),
        69 => happyGoto(p, action_698, j),
        70 => happyGoto(p, action_221, j),
        71 => happyGoto(p, action_222, j),
        72 => happyGoto(p, action_699, j),
        73 => happyGoto(p, action_224, j),
        75 => happyGoto(p, action_508, j),
        76 => happyGoto(p, action_102, j),
        77 => happyGoto(p, action_103, j),
        78 => happyGoto(p, action_481, j),
        134 => happyGoto(p, action_167, j),
        _ => happyFail(p, j)
    }
}

fn action_581(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_423, j),
        150 => happyShift(p, action_228, j),
        237 => happyShift(p, action_140, j),
        238 => happyShift(p, action_424, j),
        239 => happyShift(p, action_142, j),
        70 => happyGoto(p, action_419, j),
        71 => happyGoto(p, action_222, j),
        72 => happyGoto(p, action_420, j),
        73 => happyGoto(p, action_224, j),
        74 => happyGoto(p, action_697, j),
        76 => happyGoto(p, action_157, j),
        77 => happyGoto(p, action_103, j),
        78 => happyGoto(p, action_158, j),
        80 => happyGoto(p, action_159, j),
        81 => happyGoto(p, action_107, j),
        133 => happyGoto(p, action_422, j),
        134 => happyGoto(p, action_110, j),
        _ => happyFail(p, j)
    }
}

fn action_582(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_92(p, j)
    }
}

fn action_583(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_23(p, j)
    }
}

fn action_584(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        139 => happyShift(p, action_696, j),
        _ => happyFail(p, j)
    }
}

fn action_585(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        139 => happyShift(p, action_695, j),
        _ => happyFail(p, j)
    }
}

fn action_586(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        236 => happyShift(p, action_42, j),
        128 => happyGoto(p, action_694, j),
        _ => happyFail(p, j)
    }
}

fn action_587(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_97(p, j)
    }
}

fn action_588(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_98(p, j)
    }
}

fn action_589(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_21(p, j)
    }
}

fn action_590(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        187 => happyShift(p, action_406, j),
        35 => happyGoto(p, action_693, j),
        67 => happyGoto(p, action_405, j),
        _ => happyReduce_232(p, j)
    }
}

fn action_591(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        187 => happyShift(p, action_406, j),
        35 => happyGoto(p, action_692, j),
        67 => happyGoto(p, action_405, j),
        _ => happyReduce_232(p, j)
    }
}

fn action_592(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_26, j),
        144 => happyShift(p, action_27, j),
        145 => happyShift(p, action_28, j),
        146 => happyShift(p, action_29, j),
        147 => happyShift(p, action_30, j),
        148 => happyShift(p, action_31, j),
        149 => happyShift(p, action_32, j),
        150 => happyShift(p, action_33, j),
        153 => happyShift(p, action_34, j),
        164 => happyShift(p, action_35, j),
        180 => happyShift(p, action_60, j),
        181 => happyShift(p, action_61, j),
        184 => happyShift(p, action_36, j),
        187 => happyShift(p, action_62, j),
        189 => happyShift(p, action_63, j),
        191 => happyShift(p, action_64, j),
        194 => happyShift(p, action_65, j),
        196 => happyShift(p, action_66, j),
        197 => happyShift(p, action_67, j),
        203 => happyShift(p, action_68, j),
        204 => happyShift(p, action_37, j),
        205 => happyShift(p, action_69, j),
        206 => happyShift(p, action_70, j),
        217 => happyShift(p, action_71, j),
        220 => happyShift(p, action_38, j),
        224 => happyShift(p, action_72, j),
        232 => happyShift(p, action_73, j),
        233 => happyShift(p, action_39, j),
        234 => happyShift(p, action_40, j),
        235 => happyShift(p, action_41, j),
        236 => happyShift(p, action_42, j),
        237 => happyShift(p, action_74, j),
        238 => happyShift(p, action_75, j),
        240 => happyShift(p, action_44, j),
        241 => happyShift(p, action_45, j),
        242 => happyShift(p, action_46, j),
        243 => happyShift(p, action_47, j),
        244 => happyShift(p, action_48, j),
        245 => happyShift(p, action_49, j),
        12 => happyGoto(p, action_691, j),
        13 => happyGoto(p, action_51, j),
        14 => happyGoto(p, action_52, j),
        22 => happyGoto(p, action_53, j),
        23 => happyGoto(p, action_54, j),
        24 => happyGoto(p, action_55, j),
        25 => happyGoto(p, action_56, j),
        26 => happyGoto(p, action_57, j),
        100 => happyGoto(p, action_6, j),
        104 => happyGoto(p, action_7, j),
        106 => happyGoto(p, action_8, j),
        107 => happyGoto(p, action_9, j),
        108 => happyGoto(p, action_10, j),
        109 => happyGoto(p, action_11, j),
        110 => happyGoto(p, action_12, j),
        111 => happyGoto(p, action_13, j),
        112 => happyGoto(p, action_14, j),
        113 => happyGoto(p, action_15, j),
        114 => happyGoto(p, action_16, j),
        115 => happyGoto(p, action_17, j),
        116 => happyGoto(p, action_18, j),
        117 => happyGoto(p, action_19, j),
        118 => happyGoto(p, action_20, j),
        119 => happyGoto(p, action_21, j),
        120 => happyGoto(p, action_22, j),
        122 => happyGoto(p, action_58, j),
        127 => happyGoto(p, action_24, j),
        128 => happyGoto(p, action_25, j),
        131 => happyGoto(p, action_59, j),
        _ => happyFail(p, j)
    }
}

fn action_593(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_26, j),
        144 => happyShift(p, action_27, j),
        145 => happyShift(p, action_28, j),
        146 => happyShift(p, action_29, j),
        147 => happyShift(p, action_30, j),
        148 => happyShift(p, action_31, j),
        149 => happyShift(p, action_32, j),
        150 => happyShift(p, action_33, j),
        153 => happyShift(p, action_34, j),
        164 => happyShift(p, action_35, j),
        180 => happyShift(p, action_60, j),
        181 => happyShift(p, action_61, j),
        184 => happyShift(p, action_36, j),
        187 => happyShift(p, action_62, j),
        189 => happyShift(p, action_63, j),
        191 => happyShift(p, action_64, j),
        194 => happyShift(p, action_65, j),
        196 => happyShift(p, action_66, j),
        197 => happyShift(p, action_67, j),
        203 => happyShift(p, action_68, j),
        204 => happyShift(p, action_37, j),
        205 => happyShift(p, action_69, j),
        206 => happyShift(p, action_70, j),
        217 => happyShift(p, action_71, j),
        220 => happyShift(p, action_38, j),
        224 => happyShift(p, action_72, j),
        232 => happyShift(p, action_73, j),
        233 => happyShift(p, action_39, j),
        234 => happyShift(p, action_40, j),
        235 => happyShift(p, action_41, j),
        236 => happyShift(p, action_42, j),
        237 => happyShift(p, action_74, j),
        238 => happyShift(p, action_75, j),
        240 => happyShift(p, action_44, j),
        241 => happyShift(p, action_45, j),
        242 => happyShift(p, action_46, j),
        243 => happyShift(p, action_47, j),
        244 => happyShift(p, action_48, j),
        245 => happyShift(p, action_49, j),
        12 => happyGoto(p, action_690, j),
        13 => happyGoto(p, action_51, j),
        14 => happyGoto(p, action_52, j),
        22 => happyGoto(p, action_53, j),
        23 => happyGoto(p, action_54, j),
        24 => happyGoto(p, action_55, j),
        25 => happyGoto(p, action_56, j),
        26 => happyGoto(p, action_57, j),
        100 => happyGoto(p, action_6, j),
        104 => happyGoto(p, action_7, j),
        106 => happyGoto(p, action_8, j),
        107 => happyGoto(p, action_9, j),
        108 => happyGoto(p, action_10, j),
        109 => happyGoto(p, action_11, j),
        110 => happyGoto(p, action_12, j),
        111 => happyGoto(p, action_13, j),
        112 => happyGoto(p, action_14, j),
        113 => happyGoto(p, action_15, j),
        114 => happyGoto(p, action_16, j),
        115 => happyGoto(p, action_17, j),
        116 => happyGoto(p, action_18, j),
        117 => happyGoto(p, action_19, j),
        118 => happyGoto(p, action_20, j),
        119 => happyGoto(p, action_21, j),
        120 => happyGoto(p, action_22, j),
        122 => happyGoto(p, action_58, j),
        127 => happyGoto(p, action_24, j),
        128 => happyGoto(p, action_25, j),
        131 => happyGoto(p, action_59, j),
        _ => happyFail(p, j)
    }
}

fn action_594(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_26, j),
        144 => happyShift(p, action_27, j),
        145 => happyShift(p, action_28, j),
        146 => happyShift(p, action_29, j),
        147 => happyShift(p, action_30, j),
        148 => happyShift(p, action_31, j),
        149 => happyShift(p, action_32, j),
        150 => happyShift(p, action_33, j),
        153 => happyShift(p, action_34, j),
        164 => happyShift(p, action_35, j),
        180 => happyShift(p, action_60, j),
        181 => happyShift(p, action_61, j),
        184 => happyShift(p, action_36, j),
        187 => happyShift(p, action_62, j),
        189 => happyShift(p, action_63, j),
        191 => happyShift(p, action_64, j),
        194 => happyShift(p, action_65, j),
        196 => happyShift(p, action_66, j),
        197 => happyShift(p, action_67, j),
        203 => happyShift(p, action_68, j),
        204 => happyShift(p, action_37, j),
        205 => happyShift(p, action_69, j),
        206 => happyShift(p, action_70, j),
        217 => happyShift(p, action_71, j),
        220 => happyShift(p, action_38, j),
        224 => happyShift(p, action_72, j),
        232 => happyShift(p, action_73, j),
        233 => happyShift(p, action_39, j),
        234 => happyShift(p, action_40, j),
        235 => happyShift(p, action_41, j),
        236 => happyShift(p, action_42, j),
        237 => happyShift(p, action_74, j),
        238 => happyShift(p, action_75, j),
        240 => happyShift(p, action_44, j),
        241 => happyShift(p, action_45, j),
        242 => happyShift(p, action_46, j),
        243 => happyShift(p, action_47, j),
        244 => happyShift(p, action_48, j),
        245 => happyShift(p, action_49, j),
        12 => happyGoto(p, action_689, j),
        13 => happyGoto(p, action_51, j),
        14 => happyGoto(p, action_52, j),
        22 => happyGoto(p, action_53, j),
        23 => happyGoto(p, action_54, j),
        24 => happyGoto(p, action_55, j),
        25 => happyGoto(p, action_56, j),
        26 => happyGoto(p, action_57, j),
        100 => happyGoto(p, action_6, j),
        104 => happyGoto(p, action_7, j),
        106 => happyGoto(p, action_8, j),
        107 => happyGoto(p, action_9, j),
        108 => happyGoto(p, action_10, j),
        109 => happyGoto(p, action_11, j),
        110 => happyGoto(p, action_12, j),
        111 => happyGoto(p, action_13, j),
        112 => happyGoto(p, action_14, j),
        113 => happyGoto(p, action_15, j),
        114 => happyGoto(p, action_16, j),
        115 => happyGoto(p, action_17, j),
        116 => happyGoto(p, action_18, j),
        117 => happyGoto(p, action_19, j),
        118 => happyGoto(p, action_20, j),
        119 => happyGoto(p, action_21, j),
        120 => happyGoto(p, action_22, j),
        122 => happyGoto(p, action_58, j),
        127 => happyGoto(p, action_24, j),
        128 => happyGoto(p, action_25, j),
        131 => happyGoto(p, action_59, j),
        _ => happyFail(p, j)
    }
}

fn action_595(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_66(p, j)
    }
}

fn action_596(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_26, j),
        144 => happyShift(p, action_27, j),
        145 => happyShift(p, action_28, j),
        146 => happyShift(p, action_29, j),
        147 => happyShift(p, action_30, j),
        148 => happyShift(p, action_31, j),
        149 => happyShift(p, action_32, j),
        150 => happyShift(p, action_33, j),
        153 => happyShift(p, action_34, j),
        164 => happyShift(p, action_35, j),
        184 => happyShift(p, action_36, j),
        204 => happyShift(p, action_37, j),
        220 => happyShift(p, action_38, j),
        233 => happyShift(p, action_39, j),
        234 => happyShift(p, action_40, j),
        235 => happyShift(p, action_41, j),
        236 => happyShift(p, action_42, j),
        237 => happyShift(p, action_43, j),
        240 => happyShift(p, action_44, j),
        241 => happyShift(p, action_45, j),
        242 => happyShift(p, action_46, j),
        243 => happyShift(p, action_47, j),
        244 => happyShift(p, action_48, j),
        245 => happyShift(p, action_49, j),
        100 => happyGoto(p, action_6, j),
        104 => happyGoto(p, action_7, j),
        106 => happyGoto(p, action_8, j),
        107 => happyGoto(p, action_9, j),
        108 => happyGoto(p, action_10, j),
        109 => happyGoto(p, action_11, j),
        110 => happyGoto(p, action_12, j),
        111 => happyGoto(p, action_13, j),
        112 => happyGoto(p, action_14, j),
        113 => happyGoto(p, action_15, j),
        114 => happyGoto(p, action_16, j),
        115 => happyGoto(p, action_17, j),
        116 => happyGoto(p, action_18, j),
        117 => happyGoto(p, action_19, j),
        118 => happyGoto(p, action_20, j),
        119 => happyGoto(p, action_21, j),
        120 => happyGoto(p, action_22, j),
        122 => happyGoto(p, action_249, j),
        124 => happyGoto(p, action_688, j),
        127 => happyGoto(p, action_24, j),
        128 => happyGoto(p, action_25, j),
        _ => happyReduce_456(p, j)
    }
}

fn action_597(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_26, j),
        144 => happyShift(p, action_27, j),
        145 => happyShift(p, action_28, j),
        146 => happyShift(p, action_29, j),
        147 => happyShift(p, action_30, j),
        148 => happyShift(p, action_31, j),
        149 => happyShift(p, action_32, j),
        150 => happyShift(p, action_33, j),
        153 => happyShift(p, action_34, j),
        164 => happyShift(p, action_35, j),
        184 => happyShift(p, action_36, j),
        204 => happyShift(p, action_37, j),
        220 => happyShift(p, action_38, j),
        233 => happyShift(p, action_39, j),
        234 => happyShift(p, action_40, j),
        235 => happyShift(p, action_41, j),
        236 => happyShift(p, action_42, j),
        237 => happyShift(p, action_43, j),
        240 => happyShift(p, action_44, j),
        241 => happyShift(p, action_45, j),
        242 => happyShift(p, action_46, j),
        243 => happyShift(p, action_47, j),
        244 => happyShift(p, action_48, j),
        245 => happyShift(p, action_49, j),
        100 => happyGoto(p, action_6, j),
        104 => happyGoto(p, action_7, j),
        106 => happyGoto(p, action_8, j),
        107 => happyGoto(p, action_9, j),
        108 => happyGoto(p, action_10, j),
        109 => happyGoto(p, action_11, j),
        110 => happyGoto(p, action_12, j),
        111 => happyGoto(p, action_13, j),
        112 => happyGoto(p, action_14, j),
        113 => happyGoto(p, action_15, j),
        114 => happyGoto(p, action_16, j),
        115 => happyGoto(p, action_17, j),
        116 => happyGoto(p, action_18, j),
        117 => happyGoto(p, action_19, j),
        118 => happyGoto(p, action_20, j),
        119 => happyGoto(p, action_21, j),
        120 => happyGoto(p, action_22, j),
        122 => happyGoto(p, action_249, j),
        124 => happyGoto(p, action_687, j),
        127 => happyGoto(p, action_24, j),
        128 => happyGoto(p, action_25, j),
        _ => happyReduce_456(p, j)
    }
}

fn action_598(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_26, j),
        144 => happyShift(p, action_27, j),
        145 => happyShift(p, action_28, j),
        146 => happyShift(p, action_29, j),
        147 => happyShift(p, action_30, j),
        148 => happyShift(p, action_31, j),
        149 => happyShift(p, action_32, j),
        150 => happyShift(p, action_33, j),
        153 => happyShift(p, action_34, j),
        164 => happyShift(p, action_35, j),
        184 => happyShift(p, action_36, j),
        204 => happyShift(p, action_37, j),
        220 => happyShift(p, action_38, j),
        233 => happyShift(p, action_39, j),
        234 => happyShift(p, action_40, j),
        235 => happyShift(p, action_41, j),
        236 => happyShift(p, action_42, j),
        237 => happyShift(p, action_43, j),
        240 => happyShift(p, action_44, j),
        241 => happyShift(p, action_45, j),
        242 => happyShift(p, action_46, j),
        243 => happyShift(p, action_47, j),
        244 => happyShift(p, action_48, j),
        245 => happyShift(p, action_49, j),
        100 => happyGoto(p, action_6, j),
        104 => happyGoto(p, action_7, j),
        106 => happyGoto(p, action_8, j),
        107 => happyGoto(p, action_9, j),
        108 => happyGoto(p, action_10, j),
        109 => happyGoto(p, action_11, j),
        110 => happyGoto(p, action_12, j),
        111 => happyGoto(p, action_13, j),
        112 => happyGoto(p, action_14, j),
        113 => happyGoto(p, action_15, j),
        114 => happyGoto(p, action_16, j),
        115 => happyGoto(p, action_17, j),
        116 => happyGoto(p, action_18, j),
        117 => happyGoto(p, action_19, j),
        118 => happyGoto(p, action_20, j),
        119 => happyGoto(p, action_21, j),
        120 => happyGoto(p, action_22, j),
        122 => happyGoto(p, action_686, j),
        127 => happyGoto(p, action_24, j),
        128 => happyGoto(p, action_25, j),
        _ => happyFail(p, j)
    }
}

fn action_599(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        167 => happyShift(p, action_685, j),
        _ => happyFail(p, j)
    }
}

fn action_600(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_35(p, j)
    }
}

fn action_601(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        139 => happyShift(p, action_683, j),
        167 => happyShift(p, action_684, j),
        _ => happyFail(p, j)
    }
}

fn action_602(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        179 => happyShift(p, action_528, j),
        180 => happyShift(p, action_682, j),
        _ => happyFail(p, j)
    }
}

fn action_603(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_26, j),
        144 => happyShift(p, action_27, j),
        145 => happyShift(p, action_28, j),
        146 => happyShift(p, action_29, j),
        147 => happyShift(p, action_30, j),
        148 => happyShift(p, action_31, j),
        149 => happyShift(p, action_32, j),
        150 => happyShift(p, action_33, j),
        153 => happyShift(p, action_34, j),
        164 => happyShift(p, action_35, j),
        180 => happyShift(p, action_60, j),
        181 => happyShift(p, action_61, j),
        184 => happyShift(p, action_36, j),
        185 => happyShift(p, action_113, j),
        186 => happyReduce_471(p, j),
        187 => happyShift(p, action_62, j),
        188 => happyShift(p, action_115, j),
        189 => happyShift(p, action_63, j),
        190 => happyShift(p, action_116, j),
        191 => happyShift(p, action_64, j),
        192 => happyShift(p, action_117, j),
        193 => happyReduce_471(p, j),
        194 => happyShift(p, action_65, j),
        195 => happyShift(p, action_118, j),
        196 => happyShift(p, action_66, j),
        197 => happyShift(p, action_67, j),
        198 => happyShift(p, action_119, j),
        200 => happyShift(p, action_120, j),
        201 => happyShift(p, action_121, j),
        202 => happyShift(p, action_122, j),
        203 => happyShift(p, action_68, j),
        204 => happyShift(p, action_37, j),
        205 => happyShift(p, action_69, j),
        206 => happyShift(p, action_70, j),
        207 => happyShift(p, action_123, j),
        208 => happyShift(p, action_124, j),
        209 => happyShift(p, action_125, j),
        210 => happyShift(p, action_126, j),
        212 => happyShift(p, action_127, j),
        213 => happyReduce_471(p, j),
        214 => happyReduce_471(p, j),
        215 => happyShift(p, action_128, j),
        216 => happyReduce_471(p, j),
        217 => happyShift(p, action_71, j),
        218 => happyShift(p, action_129, j),
        219 => happyShift(p, action_130, j),
        220 => happyShift(p, action_38, j),
        221 => happyShift(p, action_131, j),
        222 => happyShift(p, action_132, j),
        223 => happyShift(p, action_133, j),
        224 => happyShift(p, action_72, j),
        225 => happyShift(p, action_134, j),
        226 => happyShift(p, action_135, j),
        227 => happyShift(p, action_136, j),
        228 => happyShift(p, action_137, j),
        229 => happyShift(p, action_138, j),
        230 => happyShift(p, action_139, j),
        231 => happyReduce_471(p, j),
        232 => happyShift(p, action_73, j),
        233 => happyShift(p, action_39, j),
        234 => happyShift(p, action_40, j),
        235 => happyShift(p, action_41, j),
        236 => happyShift(p, action_42, j),
        237 => happyShift(p, action_74, j),
        238 => happyShift(p, action_615, j),
        239 => happyShift(p, action_142, j),
        240 => happyShift(p, action_616, j),
        241 => happyShift(p, action_45, j),
        242 => happyShift(p, action_46, j),
        243 => happyShift(p, action_47, j),
        244 => happyShift(p, action_48, j),
        245 => happyShift(p, action_49, j),
        12 => happyGoto(p, action_605, j),
        13 => happyGoto(p, action_51, j),
        14 => happyGoto(p, action_52, j),
        16 => happyGoto(p, action_681, j),
        18 => happyGoto(p, action_607, j),
        19 => happyGoto(p, action_608, j),
        20 => happyGoto(p, action_609, j),
        22 => happyGoto(p, action_53, j),
        23 => happyGoto(p, action_54, j),
        24 => happyGoto(p, action_55, j),
        25 => happyGoto(p, action_56, j),
        26 => happyGoto(p, action_57, j),
        32 => happyGoto(p, action_610, j),
        34 => happyGoto(p, action_80, j),
        36 => happyGoto(p, action_81, j),
        37 => happyGoto(p, action_611, j),
        38 => happyGoto(p, action_612, j),
        40 => happyGoto(p, action_84, j),
        41 => happyGoto(p, action_85, j),
        42 => happyGoto(p, action_86, j),
        43 => happyGoto(p, action_87, j),
        44 => happyGoto(p, action_613, j),
        45 => happyGoto(p, action_89, j),
        46 => happyGoto(p, action_90, j),
        47 => happyGoto(p, action_91, j),
        48 => happyGoto(p, action_92, j),
        49 => happyGoto(p, action_93, j),
        50 => happyGoto(p, action_94, j),
        51 => happyGoto(p, action_95, j),
        52 => happyGoto(p, action_96, j),
        53 => happyGoto(p, action_97, j),
        54 => happyGoto(p, action_98, j),
        61 => happyGoto(p, action_99, j),
        65 => happyGoto(p, action_614, j),
        100 => happyGoto(p, action_6, j),
        104 => happyGoto(p, action_7, j),
        106 => happyGoto(p, action_8, j),
        107 => happyGoto(p, action_9, j),
        108 => happyGoto(p, action_10, j),
        109 => happyGoto(p, action_11, j),
        110 => happyGoto(p, action_12, j),
        111 => happyGoto(p, action_13, j),
        112 => happyGoto(p, action_14, j),
        113 => happyGoto(p, action_15, j),
        114 => happyGoto(p, action_16, j),
        115 => happyGoto(p, action_17, j),
        116 => happyGoto(p, action_18, j),
        117 => happyGoto(p, action_19, j),
        118 => happyGoto(p, action_20, j),
        119 => happyGoto(p, action_21, j),
        120 => happyGoto(p, action_22, j),
        122 => happyGoto(p, action_58, j),
        127 => happyGoto(p, action_24, j),
        128 => happyGoto(p, action_25, j),
        131 => happyGoto(p, action_59, j),
        132 => happyGoto(p, action_108, j),
        133 => happyGoto(p, action_468, j),
        134 => happyGoto(p, action_110, j),
        _ => happyReduce_41(p, j)
    }
}

fn action_604(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        237 => happyShift(p, action_460, j),
        85 => happyGoto(p, action_680, j),
        _ => happyFail(p, j)
    }
}

fn action_605(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_44(p, j)
    }
}

fn action_606(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        182 => happyShift(p, action_679, j),
        _ => happyFail(p, j)
    }
}

fn action_607(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_43(p, j)
    }
}

fn action_608(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_45(p, j)
    }
}

fn action_609(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_47(p, j)
    }
}

fn action_610(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_46(p, j)
    }
}

fn action_611(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_524, j),
        150 => happyShift(p, action_525, j),
        237 => happyShift(p, action_140, j),
        238 => happyShift(p, action_229, j),
        11 => happyGoto(p, action_678, j),
        66 => happyGoto(p, action_240, j),
        68 => happyGoto(p, action_219, j),
        69 => happyGoto(p, action_220, j),
        70 => happyGoto(p, action_221, j),
        71 => happyGoto(p, action_222, j),
        72 => happyGoto(p, action_223, j),
        73 => happyGoto(p, action_224, j),
        75 => happyGoto(p, action_225, j),
        76 => happyGoto(p, action_102, j),
        77 => happyGoto(p, action_103, j),
        78 => happyGoto(p, action_481, j),
        _ => happyFail(p, j)
    }
}

fn action_612(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_475, j),
        150 => happyShift(p, action_476, j),
        185 => happyShift(p, action_113, j),
        186 => happyShift(p, action_171, j),
        188 => happyShift(p, action_115, j),
        190 => happyShift(p, action_116, j),
        192 => happyShift(p, action_117, j),
        193 => happyShift(p, action_172, j),
        195 => happyShift(p, action_118, j),
        198 => happyShift(p, action_119, j),
        200 => happyShift(p, action_120, j),
        201 => happyShift(p, action_121, j),
        202 => happyShift(p, action_122, j),
        207 => happyShift(p, action_123, j),
        208 => happyShift(p, action_124, j),
        209 => happyShift(p, action_125, j),
        210 => happyShift(p, action_126, j),
        212 => happyShift(p, action_127, j),
        213 => happyShift(p, action_173, j),
        214 => happyShift(p, action_174, j),
        215 => happyShift(p, action_128, j),
        216 => happyShift(p, action_175, j),
        218 => happyShift(p, action_129, j),
        219 => happyShift(p, action_130, j),
        221 => happyShift(p, action_131, j),
        223 => happyShift(p, action_133, j),
        225 => happyShift(p, action_134, j),
        226 => happyShift(p, action_237, j),
        227 => happyShift(p, action_136, j),
        228 => happyShift(p, action_137, j),
        229 => happyShift(p, action_138, j),
        230 => happyShift(p, action_139, j),
        231 => happyShift(p, action_176, j),
        237 => happyShift(p, action_140, j),
        238 => happyShift(p, action_238, j),
        239 => happyShift(p, action_142, j),
        11 => happyGoto(p, action_677, j),
        39 => happyGoto(p, action_231, j),
        41 => happyGoto(p, action_198, j),
        42 => happyGoto(p, action_199, j),
        43 => happyGoto(p, action_200, j),
        45 => happyGoto(p, action_232, j),
        52 => happyGoto(p, action_233, j),
        53 => happyGoto(p, action_97, j),
        54 => happyGoto(p, action_98, j),
        61 => happyGoto(p, action_99, j),
        64 => happyGoto(p, action_201, j),
        75 => happyGoto(p, action_234, j),
        76 => happyGoto(p, action_102, j),
        77 => happyGoto(p, action_103, j),
        78 => happyGoto(p, action_481, j),
        134 => happyGoto(p, action_236, j),
        _ => happyFail(p, j)
    }
}

fn action_613(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_524, j),
        150 => happyShift(p, action_525, j),
        237 => happyShift(p, action_140, j),
        238 => happyShift(p, action_229, j),
        11 => happyGoto(p, action_676, j),
        66 => happyGoto(p, action_218, j),
        68 => happyGoto(p, action_219, j),
        69 => happyGoto(p, action_220, j),
        70 => happyGoto(p, action_221, j),
        71 => happyGoto(p, action_222, j),
        72 => happyGoto(p, action_223, j),
        73 => happyGoto(p, action_224, j),
        75 => happyGoto(p, action_225, j),
        76 => happyGoto(p, action_102, j),
        77 => happyGoto(p, action_103, j),
        78 => happyGoto(p, action_481, j),
        _ => happyFail(p, j)
    }
}

fn action_614(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_475, j),
        150 => happyShift(p, action_476, j),
        185 => happyShift(p, action_113, j),
        186 => happyShift(p, action_171, j),
        188 => happyShift(p, action_115, j),
        190 => happyShift(p, action_116, j),
        192 => happyShift(p, action_117, j),
        193 => happyShift(p, action_172, j),
        195 => happyShift(p, action_118, j),
        198 => happyShift(p, action_119, j),
        200 => happyShift(p, action_120, j),
        201 => happyShift(p, action_121, j),
        202 => happyShift(p, action_122, j),
        207 => happyShift(p, action_123, j),
        208 => happyShift(p, action_124, j),
        209 => happyShift(p, action_125, j),
        210 => happyShift(p, action_126, j),
        212 => happyShift(p, action_127, j),
        213 => happyShift(p, action_173, j),
        214 => happyShift(p, action_174, j),
        215 => happyShift(p, action_128, j),
        216 => happyShift(p, action_175, j),
        218 => happyShift(p, action_129, j),
        219 => happyShift(p, action_130, j),
        221 => happyShift(p, action_131, j),
        223 => happyShift(p, action_133, j),
        225 => happyShift(p, action_134, j),
        226 => happyShift(p, action_191, j),
        227 => happyShift(p, action_136, j),
        228 => happyShift(p, action_137, j),
        229 => happyShift(p, action_138, j),
        230 => happyShift(p, action_139, j),
        231 => happyShift(p, action_176, j),
        237 => happyShift(p, action_140, j),
        238 => happyShift(p, action_192, j),
        239 => happyShift(p, action_142, j),
        11 => happyGoto(p, action_674, j),
        40 => happyGoto(p, action_184, j),
        41 => happyGoto(p, action_85, j),
        42 => happyGoto(p, action_86, j),
        43 => happyGoto(p, action_87, j),
        45 => happyGoto(p, action_185, j),
        52 => happyGoto(p, action_186, j),
        53 => happyGoto(p, action_97, j),
        54 => happyGoto(p, action_98, j),
        61 => happyGoto(p, action_99, j),
        64 => happyGoto(p, action_187, j),
        75 => happyGoto(p, action_188, j),
        76 => happyGoto(p, action_102, j),
        77 => happyGoto(p, action_103, j),
        78 => happyGoto(p, action_481, j),
        133 => happyGoto(p, action_675, j),
        134 => happyGoto(p, action_110, j),
        _ => happyFail(p, j)
    }
}

fn action_615(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        167 => happyReduce_470(p, j),
        _ => happyReduce_170(p, j)
    }
}

fn action_616(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_26, j),
        144 => happyShift(p, action_27, j),
        145 => happyShift(p, action_28, j),
        146 => happyShift(p, action_29, j),
        147 => happyShift(p, action_30, j),
        148 => happyShift(p, action_31, j),
        149 => happyShift(p, action_32, j),
        150 => happyShift(p, action_33, j),
        153 => happyShift(p, action_34, j),
        164 => happyShift(p, action_35, j),
        184 => happyShift(p, action_36, j),
        185 => happyShift(p, action_113, j),
        188 => happyShift(p, action_115, j),
        190 => happyShift(p, action_116, j),
        192 => happyShift(p, action_117, j),
        195 => happyShift(p, action_118, j),
        198 => happyShift(p, action_119, j),
        200 => happyShift(p, action_120, j),
        201 => happyShift(p, action_121, j),
        202 => happyShift(p, action_122, j),
        204 => happyShift(p, action_37, j),
        207 => happyShift(p, action_123, j),
        208 => happyShift(p, action_124, j),
        209 => happyShift(p, action_125, j),
        210 => happyShift(p, action_126, j),
        212 => happyShift(p, action_127, j),
        215 => happyShift(p, action_128, j),
        218 => happyShift(p, action_129, j),
        219 => happyShift(p, action_130, j),
        220 => happyShift(p, action_38, j),
        221 => happyShift(p, action_131, j),
        222 => happyShift(p, action_132, j),
        223 => happyShift(p, action_133, j),
        225 => happyShift(p, action_134, j),
        226 => happyShift(p, action_135, j),
        227 => happyShift(p, action_136, j),
        228 => happyShift(p, action_137, j),
        229 => happyShift(p, action_138, j),
        230 => happyShift(p, action_139, j),
        233 => happyShift(p, action_39, j),
        234 => happyShift(p, action_40, j),
        235 => happyShift(p, action_41, j),
        236 => happyShift(p, action_42, j),
        237 => happyShift(p, action_43, j),
        238 => happyShift(p, action_141, j),
        239 => happyShift(p, action_142, j),
        240 => happyShift(p, action_616, j),
        241 => happyShift(p, action_45, j),
        242 => happyShift(p, action_46, j),
        243 => happyShift(p, action_47, j),
        244 => happyShift(p, action_48, j),
        245 => happyShift(p, action_49, j),
        19 => happyGoto(p, action_673, j),
        20 => happyGoto(p, action_609, j),
        32 => happyGoto(p, action_610, j),
        34 => happyGoto(p, action_80, j),
        36 => happyGoto(p, action_81, j),
        37 => happyGoto(p, action_611, j),
        38 => happyGoto(p, action_612, j),
        40 => happyGoto(p, action_84, j),
        41 => happyGoto(p, action_85, j),
        42 => happyGoto(p, action_86, j),
        43 => happyGoto(p, action_87, j),
        44 => happyGoto(p, action_613, j),
        45 => happyGoto(p, action_89, j),
        46 => happyGoto(p, action_90, j),
        47 => happyGoto(p, action_91, j),
        48 => happyGoto(p, action_92, j),
        49 => happyGoto(p, action_93, j),
        50 => happyGoto(p, action_94, j),
        51 => happyGoto(p, action_95, j),
        52 => happyGoto(p, action_96, j),
        53 => happyGoto(p, action_97, j),
        54 => happyGoto(p, action_98, j),
        61 => happyGoto(p, action_99, j),
        65 => happyGoto(p, action_614, j),
        100 => happyGoto(p, action_6, j),
        104 => happyGoto(p, action_7, j),
        106 => happyGoto(p, action_259, j),
        107 => happyGoto(p, action_9, j),
        108 => happyGoto(p, action_274, j),
        127 => happyGoto(p, action_24, j),
        128 => happyGoto(p, action_25, j),
        132 => happyGoto(p, action_108, j),
        133 => happyGoto(p, action_468, j),
        134 => happyGoto(p, action_110, j),
        _ => happyReduce_471(p, j)
    }
}

fn action_617(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_34(p, j)
    }
}

fn action_618(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        190 => happyShift(p, action_116, j),
        192 => happyShift(p, action_117, j),
        195 => happyShift(p, action_118, j),
        198 => happyShift(p, action_119, j),
        200 => happyShift(p, action_120, j),
        202 => happyShift(p, action_122, j),
        208 => happyShift(p, action_124, j),
        209 => happyShift(p, action_125, j),
        210 => happyShift(p, action_126, j),
        218 => happyShift(p, action_129, j),
        219 => happyShift(p, action_130, j),
        223 => happyShift(p, action_133, j),
        226 => happyShift(p, action_135, j),
        228 => happyShift(p, action_137, j),
        229 => happyShift(p, action_138, j),
        230 => happyShift(p, action_139, j),
        238 => happyShift(p, action_141, j),
        239 => happyShift(p, action_142, j),
        44 => happyGoto(p, action_286, j),
        45 => happyGoto(p, action_89, j),
        47 => happyGoto(p, action_287, j),
        49 => happyGoto(p, action_288, j),
        51 => happyGoto(p, action_289, j),
        52 => happyGoto(p, action_96, j),
        53 => happyGoto(p, action_97, j),
        54 => happyGoto(p, action_98, j),
        61 => happyGoto(p, action_99, j),
        65 => happyGoto(p, action_290, j),
        86 => happyGoto(p, action_672, j),
        132 => happyGoto(p, action_108, j),
        133 => happyGoto(p, action_293, j),
        134 => happyGoto(p, action_110, j),
        _ => happyReduce_471(p, j)
    }
}

fn action_619(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        237 => happyShift(p, action_254, j),
        238 => happyShift(p, action_75, j),
        103 => happyGoto(p, action_670, j),
        131 => happyGoto(p, action_671, j),
        _ => happyFail(p, j)
    }
}

fn action_620(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        190 => happyShift(p, action_116, j),
        192 => happyShift(p, action_117, j),
        195 => happyShift(p, action_118, j),
        198 => happyShift(p, action_119, j),
        200 => happyShift(p, action_120, j),
        202 => happyShift(p, action_122, j),
        208 => happyShift(p, action_124, j),
        209 => happyShift(p, action_125, j),
        210 => happyShift(p, action_126, j),
        218 => happyShift(p, action_129, j),
        219 => happyShift(p, action_130, j),
        223 => happyShift(p, action_133, j),
        226 => happyShift(p, action_135, j),
        228 => happyShift(p, action_137, j),
        229 => happyShift(p, action_138, j),
        230 => happyShift(p, action_139, j),
        238 => happyShift(p, action_141, j),
        239 => happyShift(p, action_142, j),
        44 => happyGoto(p, action_286, j),
        45 => happyGoto(p, action_89, j),
        47 => happyGoto(p, action_287, j),
        49 => happyGoto(p, action_288, j),
        51 => happyGoto(p, action_289, j),
        52 => happyGoto(p, action_96, j),
        53 => happyGoto(p, action_97, j),
        54 => happyGoto(p, action_98, j),
        61 => happyGoto(p, action_99, j),
        65 => happyGoto(p, action_290, j),
        86 => happyGoto(p, action_669, j),
        132 => happyGoto(p, action_108, j),
        133 => happyGoto(p, action_293, j),
        134 => happyGoto(p, action_110, j),
        _ => happyReduce_471(p, j)
    }
}

fn action_621(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        181 => happyShift(p, action_634, j),
        _ => happyFail(p, j)
    }
}

fn action_622(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        181 => happyShift(p, action_634, j),
        _ => happyReduce_394(p, j)
    }
}

fn action_623(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        190 => happyShift(p, action_116, j),
        192 => happyShift(p, action_117, j),
        195 => happyShift(p, action_118, j),
        196 => happyShift(p, action_668, j),
        198 => happyShift(p, action_119, j),
        200 => happyShift(p, action_120, j),
        202 => happyShift(p, action_122, j),
        208 => happyShift(p, action_124, j),
        209 => happyShift(p, action_125, j),
        210 => happyShift(p, action_126, j),
        218 => happyShift(p, action_129, j),
        219 => happyShift(p, action_130, j),
        223 => happyShift(p, action_133, j),
        226 => happyShift(p, action_135, j),
        228 => happyShift(p, action_137, j),
        229 => happyShift(p, action_138, j),
        230 => happyShift(p, action_139, j),
        238 => happyShift(p, action_141, j),
        239 => happyShift(p, action_142, j),
        44 => happyGoto(p, action_286, j),
        45 => happyGoto(p, action_89, j),
        47 => happyGoto(p, action_287, j),
        49 => happyGoto(p, action_288, j),
        51 => happyGoto(p, action_289, j),
        52 => happyGoto(p, action_96, j),
        53 => happyGoto(p, action_97, j),
        54 => happyGoto(p, action_98, j),
        61 => happyGoto(p, action_99, j),
        65 => happyGoto(p, action_290, j),
        86 => happyGoto(p, action_665, j),
        101 => happyGoto(p, action_666, j),
        102 => happyGoto(p, action_667, j),
        132 => happyGoto(p, action_108, j),
        133 => happyGoto(p, action_293, j),
        134 => happyGoto(p, action_110, j),
        _ => happyReduce_471(p, j)
    }
}

fn action_624(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        181 => happyShift(p, action_634, j),
        _ => happyReduce_396(p, j)
    }
}

fn action_625(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_372, j),
        140 => happyShift(p, action_182, j),
        150 => happyShift(p, action_373, j),
        186 => happyShift(p, action_171, j),
        193 => happyShift(p, action_172, j),
        213 => happyShift(p, action_173, j),
        214 => happyShift(p, action_174, j),
        216 => happyShift(p, action_175, j),
        231 => happyShift(p, action_176, j),
        239 => happyShift(p, action_142, j),
        64 => happyGoto(p, action_187, j),
        87 => happyGoto(p, action_662, j),
        88 => happyGoto(p, action_367, j),
        89 => happyGoto(p, action_179, j),
        90 => happyGoto(p, action_180, j),
        91 => happyGoto(p, action_368, j),
        92 => happyGoto(p, action_369, j),
        132 => happyGoto(p, action_663, j),
        133 => happyGoto(p, action_664, j),
        134 => happyGoto(p, action_110, j),
        _ => happyReduce_471(p, j)
    }
}

fn action_626(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_328(p, j)
    }
}

fn action_627(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_372, j),
        140 => happyShift(p, action_182, j),
        150 => happyShift(p, action_373, j),
        186 => happyReduce_472(p, j),
        193 => happyReduce_472(p, j),
        213 => happyReduce_472(p, j),
        214 => happyReduce_472(p, j),
        216 => happyReduce_472(p, j),
        231 => happyReduce_472(p, j),
        239 => happyShift(p, action_142, j),
        87 => happyGoto(p, action_661, j),
        88 => happyGoto(p, action_367, j),
        89 => happyGoto(p, action_179, j),
        90 => happyGoto(p, action_180, j),
        91 => happyGoto(p, action_368, j),
        92 => happyGoto(p, action_369, j),
        134 => happyGoto(p, action_167, j),
        _ => happyReduce_330(p, j)
    }
}

fn action_628(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        139 => happyShift(p, action_660, j),
        _ => happyFail(p, j)
    }
}

fn action_629(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        139 => happyShift(p, action_659, j),
        _ => happyFail(p, j)
    }
}

fn action_630(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        139 => happyShift(p, action_658, j),
        239 => happyShift(p, action_142, j),
        134 => happyGoto(p, action_632, j),
        _ => happyFail(p, j)
    }
}

fn action_631(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_372, j),
        140 => happyShift(p, action_182, j),
        150 => happyShift(p, action_373, j),
        185 => happyShift(p, action_113, j),
        188 => happyShift(p, action_115, j),
        190 => happyShift(p, action_116, j),
        192 => happyShift(p, action_117, j),
        195 => happyShift(p, action_118, j),
        198 => happyShift(p, action_119, j),
        200 => happyShift(p, action_120, j),
        201 => happyShift(p, action_121, j),
        202 => happyShift(p, action_122, j),
        207 => happyShift(p, action_123, j),
        208 => happyShift(p, action_124, j),
        209 => happyShift(p, action_125, j),
        210 => happyShift(p, action_126, j),
        212 => happyShift(p, action_127, j),
        215 => happyShift(p, action_128, j),
        218 => happyShift(p, action_129, j),
        219 => happyShift(p, action_130, j),
        221 => happyShift(p, action_131, j),
        223 => happyShift(p, action_133, j),
        225 => happyShift(p, action_134, j),
        226 => happyShift(p, action_168, j),
        227 => happyShift(p, action_136, j),
        228 => happyShift(p, action_137, j),
        229 => happyShift(p, action_138, j),
        230 => happyShift(p, action_139, j),
        238 => happyShift(p, action_169, j),
        239 => happyShift(p, action_142, j),
        40 => happyGoto(p, action_162, j),
        41 => happyGoto(p, action_85, j),
        42 => happyGoto(p, action_86, j),
        43 => happyGoto(p, action_87, j),
        45 => happyGoto(p, action_163, j),
        52 => happyGoto(p, action_164, j),
        53 => happyGoto(p, action_97, j),
        54 => happyGoto(p, action_98, j),
        61 => happyGoto(p, action_99, j),
        88 => happyGoto(p, action_655, j),
        89 => happyGoto(p, action_179, j),
        90 => happyGoto(p, action_180, j),
        91 => happyGoto(p, action_656, j),
        92 => happyGoto(p, action_657, j),
        134 => happyGoto(p, action_167, j),
        _ => happyReduce_472(p, j)
    }
}

fn action_632(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_340(p, j)
    }
}

fn action_633(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_407(p, j)
    }
}

fn action_634(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_26, j),
        140 => happyShift(p, action_652, j),
        143 => happyShift(p, action_653, j),
        144 => happyShift(p, action_27, j),
        145 => happyShift(p, action_28, j),
        146 => happyShift(p, action_29, j),
        147 => happyShift(p, action_30, j),
        148 => happyShift(p, action_31, j),
        149 => happyShift(p, action_32, j),
        150 => happyShift(p, action_33, j),
        153 => happyShift(p, action_34, j),
        164 => happyShift(p, action_35, j),
        181 => happyShift(p, action_654, j),
        184 => happyShift(p, action_36, j),
        204 => happyShift(p, action_37, j),
        220 => happyShift(p, action_38, j),
        233 => happyShift(p, action_39, j),
        234 => happyShift(p, action_40, j),
        235 => happyShift(p, action_41, j),
        236 => happyShift(p, action_42, j),
        237 => happyShift(p, action_74, j),
        238 => happyShift(p, action_75, j),
        240 => happyShift(p, action_44, j),
        241 => happyShift(p, action_45, j),
        242 => happyShift(p, action_46, j),
        243 => happyShift(p, action_47, j),
        244 => happyShift(p, action_48, j),
        245 => happyShift(p, action_49, j),
        93 => happyGoto(p, action_644, j),
        95 => happyGoto(p, action_645, j),
        96 => happyGoto(p, action_646, j),
        97 => happyGoto(p, action_647, j),
        98 => happyGoto(p, action_648, j),
        99 => happyGoto(p, action_649, j),
        100 => happyGoto(p, action_6, j),
        104 => happyGoto(p, action_7, j),
        106 => happyGoto(p, action_8, j),
        107 => happyGoto(p, action_9, j),
        108 => happyGoto(p, action_10, j),
        109 => happyGoto(p, action_11, j),
        110 => happyGoto(p, action_12, j),
        111 => happyGoto(p, action_13, j),
        112 => happyGoto(p, action_14, j),
        113 => happyGoto(p, action_15, j),
        114 => happyGoto(p, action_16, j),
        115 => happyGoto(p, action_17, j),
        116 => happyGoto(p, action_18, j),
        117 => happyGoto(p, action_19, j),
        118 => happyGoto(p, action_20, j),
        119 => happyGoto(p, action_21, j),
        120 => happyGoto(p, action_650, j),
        127 => happyGoto(p, action_24, j),
        128 => happyGoto(p, action_25, j),
        131 => happyGoto(p, action_651, j),
        _ => happyReduce_346(p, j)
    }
}

fn action_635(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_26, j),
        144 => happyShift(p, action_27, j),
        145 => happyShift(p, action_28, j),
        146 => happyShift(p, action_29, j),
        147 => happyShift(p, action_30, j),
        148 => happyShift(p, action_31, j),
        149 => happyShift(p, action_32, j),
        150 => happyShift(p, action_33, j),
        153 => happyShift(p, action_34, j),
        164 => happyShift(p, action_35, j),
        184 => happyShift(p, action_36, j),
        204 => happyShift(p, action_37, j),
        220 => happyShift(p, action_38, j),
        233 => happyShift(p, action_39, j),
        234 => happyShift(p, action_40, j),
        235 => happyShift(p, action_41, j),
        236 => happyShift(p, action_42, j),
        237 => happyShift(p, action_43, j),
        240 => happyShift(p, action_44, j),
        241 => happyShift(p, action_45, j),
        242 => happyShift(p, action_46, j),
        243 => happyShift(p, action_47, j),
        244 => happyShift(p, action_48, j),
        245 => happyShift(p, action_49, j),
        100 => happyGoto(p, action_6, j),
        104 => happyGoto(p, action_7, j),
        106 => happyGoto(p, action_8, j),
        107 => happyGoto(p, action_9, j),
        108 => happyGoto(p, action_10, j),
        109 => happyGoto(p, action_11, j),
        110 => happyGoto(p, action_12, j),
        111 => happyGoto(p, action_13, j),
        112 => happyGoto(p, action_14, j),
        113 => happyGoto(p, action_15, j),
        114 => happyGoto(p, action_16, j),
        115 => happyGoto(p, action_17, j),
        116 => happyGoto(p, action_18, j),
        117 => happyGoto(p, action_19, j),
        118 => happyGoto(p, action_20, j),
        119 => happyGoto(p, action_21, j),
        120 => happyGoto(p, action_643, j),
        127 => happyGoto(p, action_24, j),
        128 => happyGoto(p, action_25, j),
        _ => happyFail(p, j)
    }
}

fn action_636(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_438(p, j)
    }
}

fn action_637(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_26, j),
        144 => happyShift(p, action_27, j),
        145 => happyShift(p, action_28, j),
        146 => happyShift(p, action_29, j),
        147 => happyShift(p, action_30, j),
        148 => happyShift(p, action_31, j),
        149 => happyShift(p, action_32, j),
        150 => happyShift(p, action_33, j),
        153 => happyShift(p, action_34, j),
        164 => happyShift(p, action_35, j),
        184 => happyShift(p, action_36, j),
        204 => happyShift(p, action_37, j),
        220 => happyShift(p, action_38, j),
        233 => happyShift(p, action_39, j),
        234 => happyShift(p, action_40, j),
        235 => happyShift(p, action_41, j),
        236 => happyShift(p, action_42, j),
        237 => happyShift(p, action_43, j),
        240 => happyShift(p, action_44, j),
        241 => happyShift(p, action_45, j),
        242 => happyShift(p, action_46, j),
        243 => happyShift(p, action_47, j),
        244 => happyShift(p, action_48, j),
        245 => happyShift(p, action_49, j),
        100 => happyGoto(p, action_6, j),
        104 => happyGoto(p, action_7, j),
        106 => happyGoto(p, action_259, j),
        107 => happyGoto(p, action_9, j),
        108 => happyGoto(p, action_10, j),
        109 => happyGoto(p, action_11, j),
        110 => happyGoto(p, action_12, j),
        111 => happyGoto(p, action_13, j),
        112 => happyGoto(p, action_14, j),
        113 => happyGoto(p, action_15, j),
        114 => happyGoto(p, action_16, j),
        115 => happyGoto(p, action_17, j),
        116 => happyGoto(p, action_18, j),
        117 => happyGoto(p, action_19, j),
        118 => happyGoto(p, action_20, j),
        119 => happyGoto(p, action_642, j),
        127 => happyGoto(p, action_24, j),
        128 => happyGoto(p, action_25, j),
        _ => happyFail(p, j)
    }
}

fn action_638(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_379(p, j)
    }
}

fn action_639(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_26, j),
        144 => happyShift(p, action_27, j),
        145 => happyShift(p, action_28, j),
        146 => happyShift(p, action_29, j),
        147 => happyShift(p, action_30, j),
        148 => happyShift(p, action_31, j),
        149 => happyShift(p, action_32, j),
        150 => happyShift(p, action_33, j),
        153 => happyShift(p, action_34, j),
        164 => happyShift(p, action_35, j),
        184 => happyShift(p, action_36, j),
        204 => happyShift(p, action_37, j),
        220 => happyShift(p, action_38, j),
        233 => happyShift(p, action_39, j),
        234 => happyShift(p, action_40, j),
        235 => happyShift(p, action_41, j),
        236 => happyShift(p, action_42, j),
        237 => happyShift(p, action_43, j),
        240 => happyShift(p, action_44, j),
        241 => happyShift(p, action_45, j),
        242 => happyShift(p, action_46, j),
        243 => happyShift(p, action_47, j),
        244 => happyShift(p, action_48, j),
        245 => happyShift(p, action_49, j),
        100 => happyGoto(p, action_6, j),
        104 => happyGoto(p, action_7, j),
        106 => happyGoto(p, action_8, j),
        107 => happyGoto(p, action_9, j),
        108 => happyGoto(p, action_10, j),
        109 => happyGoto(p, action_11, j),
        110 => happyGoto(p, action_12, j),
        111 => happyGoto(p, action_13, j),
        112 => happyGoto(p, action_14, j),
        113 => happyGoto(p, action_15, j),
        114 => happyGoto(p, action_16, j),
        115 => happyGoto(p, action_17, j),
        116 => happyGoto(p, action_18, j),
        117 => happyGoto(p, action_19, j),
        118 => happyGoto(p, action_20, j),
        119 => happyGoto(p, action_21, j),
        120 => happyGoto(p, action_641, j),
        127 => happyGoto(p, action_24, j),
        128 => happyGoto(p, action_25, j),
        _ => happyFail(p, j)
    }
}

fn action_640(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_377(p, j)
    }
}

fn action_641(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_387(p, j)
    }
}

fn action_642(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_437(p, j)
    }
}

fn action_643(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_455(p, j)
    }
}

fn action_644(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_347(p, j)
    }
}

fn action_645(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        179 => happyShift(p, action_852, j),
        182 => happyShift(p, action_853, j),
        _ => happyFail(p, j)
    }
}

fn action_646(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_26, j),
        144 => happyShift(p, action_27, j),
        145 => happyShift(p, action_28, j),
        146 => happyShift(p, action_29, j),
        147 => happyShift(p, action_30, j),
        148 => happyShift(p, action_31, j),
        149 => happyShift(p, action_32, j),
        150 => happyShift(p, action_33, j),
        153 => happyShift(p, action_34, j),
        164 => happyShift(p, action_35, j),
        181 => happyShift(p, action_654, j),
        184 => happyShift(p, action_36, j),
        204 => happyShift(p, action_37, j),
        220 => happyShift(p, action_38, j),
        233 => happyShift(p, action_39, j),
        234 => happyShift(p, action_40, j),
        235 => happyShift(p, action_41, j),
        236 => happyShift(p, action_42, j),
        237 => happyShift(p, action_43, j),
        240 => happyShift(p, action_44, j),
        241 => happyShift(p, action_45, j),
        242 => happyShift(p, action_46, j),
        243 => happyShift(p, action_47, j),
        244 => happyShift(p, action_48, j),
        245 => happyShift(p, action_49, j),
        93 => happyGoto(p, action_851, j),
        100 => happyGoto(p, action_6, j),
        104 => happyGoto(p, action_7, j),
        106 => happyGoto(p, action_8, j),
        107 => happyGoto(p, action_9, j),
        108 => happyGoto(p, action_10, j),
        109 => happyGoto(p, action_11, j),
        110 => happyGoto(p, action_12, j),
        111 => happyGoto(p, action_13, j),
        112 => happyGoto(p, action_14, j),
        113 => happyGoto(p, action_15, j),
        114 => happyGoto(p, action_16, j),
        115 => happyGoto(p, action_17, j),
        116 => happyGoto(p, action_18, j),
        117 => happyGoto(p, action_19, j),
        118 => happyGoto(p, action_20, j),
        119 => happyGoto(p, action_21, j),
        120 => happyGoto(p, action_650, j),
        127 => happyGoto(p, action_24, j),
        128 => happyGoto(p, action_25, j),
        _ => happyFail(p, j)
    }
}

fn action_647(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        140 => happyShift(p, action_652, j),
        143 => happyShift(p, action_653, j),
        168 => happyShift(p, action_850, j),
        98 => happyGoto(p, action_848, j),
        99 => happyGoto(p, action_849, j),
        _ => happyFail(p, j)
    }
}

fn action_648(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_354(p, j)
    }
}

fn action_649(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        140 => happyReduce_358(p, j),
        143 => happyReduce_358(p, j),
        168 => happyReduce_358(p, j),
        _ => happyReduce_353(p, j)
    }
}

fn action_650(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_341(p, j)
    }
}

fn action_651(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        167 => happyShift(p, action_847, j),
        _ => happyFail(p, j)
    }
}

fn action_652(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_26, j),
        144 => happyShift(p, action_27, j),
        145 => happyShift(p, action_28, j),
        146 => happyShift(p, action_29, j),
        147 => happyShift(p, action_30, j),
        148 => happyShift(p, action_31, j),
        149 => happyShift(p, action_32, j),
        150 => happyShift(p, action_33, j),
        153 => happyShift(p, action_34, j),
        164 => happyShift(p, action_35, j),
        184 => happyShift(p, action_36, j),
        204 => happyShift(p, action_37, j),
        220 => happyShift(p, action_38, j),
        233 => happyShift(p, action_39, j),
        234 => happyShift(p, action_40, j),
        235 => happyShift(p, action_41, j),
        236 => happyShift(p, action_42, j),
        237 => happyShift(p, action_43, j),
        240 => happyShift(p, action_44, j),
        241 => happyShift(p, action_45, j),
        242 => happyShift(p, action_46, j),
        243 => happyShift(p, action_47, j),
        244 => happyShift(p, action_48, j),
        245 => happyShift(p, action_49, j),
        100 => happyGoto(p, action_6, j),
        104 => happyGoto(p, action_7, j),
        106 => happyGoto(p, action_259, j),
        107 => happyGoto(p, action_9, j),
        108 => happyGoto(p, action_10, j),
        109 => happyGoto(p, action_11, j),
        110 => happyGoto(p, action_12, j),
        111 => happyGoto(p, action_13, j),
        112 => happyGoto(p, action_14, j),
        113 => happyGoto(p, action_15, j),
        114 => happyGoto(p, action_16, j),
        115 => happyGoto(p, action_17, j),
        116 => happyGoto(p, action_18, j),
        117 => happyGoto(p, action_19, j),
        118 => happyGoto(p, action_20, j),
        119 => happyGoto(p, action_260, j),
        126 => happyGoto(p, action_846, j),
        127 => happyGoto(p, action_24, j),
        128 => happyGoto(p, action_25, j),
        _ => happyFail(p, j)
    }
}

fn action_653(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        237 => happyShift(p, action_254, j),
        238 => happyShift(p, action_75, j),
        131 => happyGoto(p, action_845, j),
        _ => happyFail(p, j)
    }
}

fn action_654(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_26, j),
        140 => happyShift(p, action_652, j),
        143 => happyShift(p, action_653, j),
        144 => happyShift(p, action_27, j),
        145 => happyShift(p, action_28, j),
        146 => happyShift(p, action_29, j),
        147 => happyShift(p, action_30, j),
        148 => happyShift(p, action_31, j),
        149 => happyShift(p, action_32, j),
        150 => happyShift(p, action_33, j),
        153 => happyShift(p, action_34, j),
        164 => happyShift(p, action_35, j),
        181 => happyShift(p, action_654, j),
        184 => happyShift(p, action_36, j),
        204 => happyShift(p, action_37, j),
        220 => happyShift(p, action_38, j),
        233 => happyShift(p, action_39, j),
        234 => happyShift(p, action_40, j),
        235 => happyShift(p, action_41, j),
        236 => happyShift(p, action_42, j),
        237 => happyShift(p, action_74, j),
        238 => happyShift(p, action_75, j),
        240 => happyShift(p, action_44, j),
        241 => happyShift(p, action_45, j),
        242 => happyShift(p, action_46, j),
        243 => happyShift(p, action_47, j),
        244 => happyShift(p, action_48, j),
        245 => happyShift(p, action_49, j),
        93 => happyGoto(p, action_644, j),
        95 => happyGoto(p, action_844, j),
        96 => happyGoto(p, action_646, j),
        97 => happyGoto(p, action_647, j),
        98 => happyGoto(p, action_648, j),
        99 => happyGoto(p, action_649, j),
        100 => happyGoto(p, action_6, j),
        104 => happyGoto(p, action_7, j),
        106 => happyGoto(p, action_8, j),
        107 => happyGoto(p, action_9, j),
        108 => happyGoto(p, action_10, j),
        109 => happyGoto(p, action_11, j),
        110 => happyGoto(p, action_12, j),
        111 => happyGoto(p, action_13, j),
        112 => happyGoto(p, action_14, j),
        113 => happyGoto(p, action_15, j),
        114 => happyGoto(p, action_16, j),
        115 => happyGoto(p, action_17, j),
        116 => happyGoto(p, action_18, j),
        117 => happyGoto(p, action_19, j),
        118 => happyGoto(p, action_20, j),
        119 => happyGoto(p, action_21, j),
        120 => happyGoto(p, action_650, j),
        127 => happyGoto(p, action_24, j),
        128 => happyGoto(p, action_25, j),
        131 => happyGoto(p, action_651, j),
        _ => happyReduce_346(p, j)
    }
}

fn action_655(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        139 => happyShift(p, action_843, j),
        _ => happyFail(p, j)
    }
}

fn action_656(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        139 => happyShift(p, action_842, j),
        _ => happyFail(p, j)
    }
}

fn action_657(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        139 => happyShift(p, action_841, j),
        239 => happyShift(p, action_142, j),
        134 => happyGoto(p, action_632, j),
        _ => happyFail(p, j)
    }
}

fn action_658(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_333(p, j)
    }
}

fn action_659(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_413, j),
        140 => happyShift(p, action_182, j),
        88 => happyGoto(p, action_840, j),
        89 => happyGoto(p, action_179, j),
        90 => happyGoto(p, action_180, j),
        _ => happyReduce_332(p, j)
    }
}

fn action_660(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_334(p, j)
    }
}

fn action_661(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_331(p, j)
    }
}

fn action_662(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_329(p, j)
    }
}

fn action_663(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_327(p, j)
    }
}

fn action_664(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        186 => happyShift(p, action_171, j),
        193 => happyShift(p, action_172, j),
        213 => happyShift(p, action_173, j),
        214 => happyShift(p, action_174, j),
        216 => happyShift(p, action_175, j),
        231 => happyShift(p, action_176, j),
        239 => happyShift(p, action_142, j),
        64 => happyGoto(p, action_435, j),
        134 => happyGoto(p, action_167, j),
        _ => happyReduce_472(p, j)
    }
}

fn action_665(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        167 => happyShift(p, action_839, j),
        _ => happyFail(p, j)
    }
}

fn action_666(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        139 => happyShift(p, action_837, j),
        179 => happyShift(p, action_838, j),
        _ => happyFail(p, j)
    }
}

fn action_667(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_370(p, j)
    }
}

fn action_668(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        167 => happyShift(p, action_836, j),
        _ => happyFail(p, j)
    }
}

fn action_669(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        139 => happyShift(p, action_835, j),
        _ => happyFail(p, j)
    }
}

fn action_670(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        139 => happyShift(p, action_832, j),
        140 => happyShift(p, action_833, j),
        143 => happyShift(p, action_834, j),
        _ => happyFail(p, j)
    }
}

fn action_671(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_373(p, j)
    }
}

fn action_672(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        139 => happyShift(p, action_831, j),
        _ => happyFail(p, j)
    }
}

fn action_673(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_48(p, j)
    }
}

fn action_674(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        181 => happyShift(p, action_61, j),
        14 => happyGoto(p, action_830, j),
        _ => happyFail(p, j)
    }
}

fn action_675(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_475, j),
        150 => happyShift(p, action_476, j),
        185 => happyShift(p, action_113, j),
        186 => happyShift(p, action_171, j),
        188 => happyShift(p, action_115, j),
        190 => happyShift(p, action_116, j),
        192 => happyShift(p, action_117, j),
        193 => happyShift(p, action_172, j),
        195 => happyShift(p, action_118, j),
        198 => happyShift(p, action_119, j),
        200 => happyShift(p, action_120, j),
        201 => happyShift(p, action_121, j),
        202 => happyShift(p, action_122, j),
        207 => happyShift(p, action_123, j),
        208 => happyShift(p, action_124, j),
        209 => happyShift(p, action_125, j),
        210 => happyShift(p, action_126, j),
        212 => happyShift(p, action_127, j),
        213 => happyShift(p, action_173, j),
        214 => happyShift(p, action_174, j),
        215 => happyShift(p, action_128, j),
        216 => happyShift(p, action_175, j),
        218 => happyShift(p, action_129, j),
        219 => happyShift(p, action_130, j),
        221 => happyShift(p, action_131, j),
        223 => happyShift(p, action_133, j),
        225 => happyShift(p, action_134, j),
        226 => happyShift(p, action_438, j),
        227 => happyShift(p, action_136, j),
        228 => happyShift(p, action_137, j),
        229 => happyShift(p, action_138, j),
        230 => happyShift(p, action_139, j),
        231 => happyShift(p, action_176, j),
        237 => happyShift(p, action_140, j),
        238 => happyShift(p, action_439, j),
        239 => happyShift(p, action_142, j),
        11 => happyGoto(p, action_829, j),
        40 => happyGoto(p, action_432, j),
        41 => happyGoto(p, action_85, j),
        42 => happyGoto(p, action_86, j),
        43 => happyGoto(p, action_87, j),
        45 => happyGoto(p, action_433, j),
        52 => happyGoto(p, action_434, j),
        53 => happyGoto(p, action_97, j),
        54 => happyGoto(p, action_98, j),
        61 => happyGoto(p, action_99, j),
        64 => happyGoto(p, action_435, j),
        75 => happyGoto(p, action_436, j),
        76 => happyGoto(p, action_102, j),
        77 => happyGoto(p, action_103, j),
        78 => happyGoto(p, action_481, j),
        134 => happyGoto(p, action_167, j),
        _ => happyFail(p, j)
    }
}

fn action_676(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        181 => happyShift(p, action_61, j),
        14 => happyGoto(p, action_828, j),
        _ => happyFail(p, j)
    }
}

fn action_677(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        181 => happyShift(p, action_61, j),
        14 => happyGoto(p, action_827, j),
        _ => happyFail(p, j)
    }
}

fn action_678(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        181 => happyShift(p, action_61, j),
        14 => happyGoto(p, action_826, j),
        _ => happyFail(p, j)
    }
}

fn action_679(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_38(p, j)
    }
}

fn action_680(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        179 => happyShift(p, action_528, j),
        180 => happyShift(p, action_825, j),
        _ => happyFail(p, j)
    }
}

fn action_681(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        182 => happyShift(p, action_824, j),
        _ => happyFail(p, j)
    }
}

fn action_682(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_54(p, j)
    }
}

fn action_683(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        180 => happyShift(p, action_823, j),
        _ => happyFail(p, j)
    }
}

fn action_684(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        140 => happyShift(p, action_822, j),
        236 => happyShift(p, action_42, j),
        28 => happyGoto(p, action_818, j),
        29 => happyGoto(p, action_819, j),
        30 => happyGoto(p, action_820, j),
        128 => happyGoto(p, action_821, j),
        _ => happyReduce_76(p, j)
    }
}

fn action_685(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_26, j),
        144 => happyShift(p, action_27, j),
        145 => happyShift(p, action_28, j),
        146 => happyShift(p, action_29, j),
        147 => happyShift(p, action_30, j),
        148 => happyShift(p, action_31, j),
        149 => happyShift(p, action_32, j),
        150 => happyShift(p, action_33, j),
        153 => happyShift(p, action_34, j),
        164 => happyShift(p, action_35, j),
        180 => happyShift(p, action_60, j),
        181 => happyShift(p, action_61, j),
        184 => happyShift(p, action_36, j),
        187 => happyShift(p, action_62, j),
        189 => happyShift(p, action_63, j),
        191 => happyShift(p, action_64, j),
        194 => happyShift(p, action_65, j),
        196 => happyShift(p, action_66, j),
        197 => happyShift(p, action_67, j),
        203 => happyShift(p, action_68, j),
        204 => happyShift(p, action_37, j),
        205 => happyShift(p, action_69, j),
        206 => happyShift(p, action_70, j),
        217 => happyShift(p, action_71, j),
        220 => happyShift(p, action_38, j),
        224 => happyShift(p, action_72, j),
        232 => happyShift(p, action_73, j),
        233 => happyShift(p, action_39, j),
        234 => happyShift(p, action_40, j),
        235 => happyShift(p, action_41, j),
        236 => happyShift(p, action_42, j),
        237 => happyShift(p, action_74, j),
        238 => happyShift(p, action_75, j),
        240 => happyShift(p, action_44, j),
        241 => happyShift(p, action_45, j),
        242 => happyShift(p, action_46, j),
        243 => happyShift(p, action_47, j),
        244 => happyShift(p, action_48, j),
        245 => happyShift(p, action_49, j),
        12 => happyGoto(p, action_817, j),
        13 => happyGoto(p, action_51, j),
        14 => happyGoto(p, action_52, j),
        22 => happyGoto(p, action_53, j),
        23 => happyGoto(p, action_54, j),
        24 => happyGoto(p, action_55, j),
        25 => happyGoto(p, action_56, j),
        26 => happyGoto(p, action_57, j),
        100 => happyGoto(p, action_6, j),
        104 => happyGoto(p, action_7, j),
        106 => happyGoto(p, action_8, j),
        107 => happyGoto(p, action_9, j),
        108 => happyGoto(p, action_10, j),
        109 => happyGoto(p, action_11, j),
        110 => happyGoto(p, action_12, j),
        111 => happyGoto(p, action_13, j),
        112 => happyGoto(p, action_14, j),
        113 => happyGoto(p, action_15, j),
        114 => happyGoto(p, action_16, j),
        115 => happyGoto(p, action_17, j),
        116 => happyGoto(p, action_18, j),
        117 => happyGoto(p, action_19, j),
        118 => happyGoto(p, action_20, j),
        119 => happyGoto(p, action_21, j),
        120 => happyGoto(p, action_22, j),
        122 => happyGoto(p, action_58, j),
        127 => happyGoto(p, action_24, j),
        128 => happyGoto(p, action_25, j),
        131 => happyGoto(p, action_59, j),
        _ => happyFail(p, j)
    }
}

fn action_686(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        139 => happyShift(p, action_816, j),
        _ => happyFail(p, j)
    }
}

fn action_687(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        180 => happyShift(p, action_815, j),
        _ => happyFail(p, j)
    }
}

fn action_688(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        180 => happyShift(p, action_814, j),
        _ => happyFail(p, j)
    }
}

fn action_689(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        199 => happyShift(p, action_813, j),
        _ => happyReduce_58(p, j)
    }
}

fn action_690(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_60(p, j)
    }
}

fn action_691(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_61(p, j)
    }
}

fn action_692(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        168 => happyShift(p, action_516, j),
        94 => happyGoto(p, action_812, j),
        _ => happyReduce_344(p, j)
    }
}

fn action_693(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        168 => happyShift(p, action_516, j),
        94 => happyGoto(p, action_811, j),
        _ => happyReduce_344(p, j)
    }
}

fn action_694(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        139 => happyShift(p, action_810, j),
        _ => happyFail(p, j)
    }
}

fn action_695(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_166(p, j)
    }
}

fn action_696(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_167(p, j)
    }
}

fn action_697(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_413, j),
        139 => happyShift(p, action_809, j),
        140 => happyShift(p, action_182, j),
        88 => happyGoto(p, action_573, j),
        89 => happyGoto(p, action_179, j),
        90 => happyGoto(p, action_180, j),
        _ => happyFail(p, j)
    }
}

fn action_698(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_243(p, j)
    }
}

fn action_699(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_254(p, j)
    }
}

fn action_700(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_751, j),
        150 => happyShift(p, action_525, j),
        237 => happyShift(p, action_140, j),
        238 => happyShift(p, action_424, j),
        239 => happyShift(p, action_142, j),
        70 => happyGoto(p, action_419, j),
        71 => happyGoto(p, action_222, j),
        72 => happyGoto(p, action_420, j),
        73 => happyGoto(p, action_224, j),
        74 => happyGoto(p, action_808, j),
        76 => happyGoto(p, action_157, j),
        77 => happyGoto(p, action_103, j),
        78 => happyGoto(p, action_512, j),
        133 => happyGoto(p, action_422, j),
        134 => happyGoto(p, action_110, j),
        _ => happyFail(p, j)
    }
}

fn action_701(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_249(p, j)
    }
}

fn action_702(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_245(p, j)
    }
}

fn action_703(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_257(p, j)
    }
}

fn action_704(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_256(p, j)
    }
}

fn action_705(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_571, j),
        150 => happyShift(p, action_572, j),
        186 => happyShift(p, action_171, j),
        193 => happyShift(p, action_172, j),
        213 => happyShift(p, action_173, j),
        214 => happyShift(p, action_174, j),
        216 => happyShift(p, action_175, j),
        231 => happyShift(p, action_176, j),
        237 => happyShift(p, action_140, j),
        238 => happyShift(p, action_229, j),
        239 => happyShift(p, action_142, j),
        64 => happyGoto(p, action_187, j),
        69 => happyGoto(p, action_578, j),
        70 => happyGoto(p, action_221, j),
        71 => happyGoto(p, action_222, j),
        75 => happyGoto(p, action_482, j),
        76 => happyGoto(p, action_102, j),
        77 => happyGoto(p, action_103, j),
        78 => happyGoto(p, action_481, j),
        133 => happyGoto(p, action_807, j),
        134 => happyGoto(p, action_110, j),
        _ => happyFail(p, j)
    }
}

fn action_706(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_413, j),
        140 => happyShift(p, action_182, j),
        88 => happyGoto(p, action_806, j),
        89 => happyGoto(p, action_179, j),
        90 => happyGoto(p, action_180, j),
        _ => happyReduce_246(p, j)
    }
}

fn action_707(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_259(p, j)
    }
}

fn action_708(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        180 => happyShift(p, action_715, j),
        182 => happyShift(p, action_805, j),
        190 => happyShift(p, action_116, j),
        192 => happyShift(p, action_117, j),
        195 => happyShift(p, action_118, j),
        198 => happyShift(p, action_119, j),
        200 => happyShift(p, action_120, j),
        202 => happyShift(p, action_122, j),
        208 => happyShift(p, action_124, j),
        209 => happyShift(p, action_125, j),
        210 => happyShift(p, action_126, j),
        218 => happyShift(p, action_129, j),
        219 => happyShift(p, action_130, j),
        223 => happyShift(p, action_133, j),
        226 => happyShift(p, action_135, j),
        228 => happyShift(p, action_137, j),
        229 => happyShift(p, action_138, j),
        230 => happyShift(p, action_139, j),
        238 => happyShift(p, action_141, j),
        239 => happyShift(p, action_142, j),
        240 => happyShift(p, action_717, j),
        44 => happyGoto(p, action_709, j),
        45 => happyGoto(p, action_89, j),
        47 => happyGoto(p, action_287, j),
        49 => happyGoto(p, action_288, j),
        51 => happyGoto(p, action_289, j),
        52 => happyGoto(p, action_96, j),
        53 => happyGoto(p, action_97, j),
        54 => happyGoto(p, action_98, j),
        56 => happyGoto(p, action_710, j),
        57 => happyGoto(p, action_711, j),
        58 => happyGoto(p, action_712, j),
        61 => happyGoto(p, action_99, j),
        65 => happyGoto(p, action_713, j),
        132 => happyGoto(p, action_108, j),
        133 => happyGoto(p, action_714, j),
        134 => happyGoto(p, action_110, j),
        _ => happyReduce_471(p, j)
    }
}

fn action_709(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_524, j),
        150 => happyShift(p, action_525, j),
        167 => happyShift(p, action_804, j),
        237 => happyShift(p, action_140, j),
        238 => happyShift(p, action_229, j),
        59 => happyGoto(p, action_802, j),
        66 => happyGoto(p, action_803, j),
        68 => happyGoto(p, action_219, j),
        69 => happyGoto(p, action_220, j),
        70 => happyGoto(p, action_221, j),
        71 => happyGoto(p, action_222, j),
        72 => happyGoto(p, action_223, j),
        73 => happyGoto(p, action_224, j),
        75 => happyGoto(p, action_523, j),
        76 => happyGoto(p, action_102, j),
        77 => happyGoto(p, action_103, j),
        78 => happyGoto(p, action_481, j),
        _ => happyReduce_202(p, j)
    }
}

fn action_710(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_193(p, j)
    }
}

fn action_711(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        179 => happyShift(p, action_800, j),
        180 => happyShift(p, action_801, j),
        _ => happyFail(p, j)
    }
}

fn action_712(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        179 => happyShift(p, action_798, j),
        180 => happyShift(p, action_799, j),
        _ => happyFail(p, j)
    }
}

fn action_713(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        186 => happyShift(p, action_171, j),
        190 => happyShift(p, action_116, j),
        192 => happyShift(p, action_117, j),
        193 => happyShift(p, action_172, j),
        195 => happyShift(p, action_118, j),
        198 => happyShift(p, action_119, j),
        200 => happyShift(p, action_120, j),
        202 => happyShift(p, action_122, j),
        208 => happyShift(p, action_124, j),
        209 => happyShift(p, action_125, j),
        210 => happyShift(p, action_126, j),
        213 => happyShift(p, action_173, j),
        214 => happyShift(p, action_174, j),
        216 => happyShift(p, action_175, j),
        218 => happyShift(p, action_129, j),
        219 => happyShift(p, action_130, j),
        223 => happyShift(p, action_133, j),
        226 => happyShift(p, action_191, j),
        228 => happyShift(p, action_137, j),
        229 => happyShift(p, action_138, j),
        230 => happyShift(p, action_139, j),
        231 => happyShift(p, action_176, j),
        238 => happyShift(p, action_192, j),
        239 => happyShift(p, action_142, j),
        45 => happyGoto(p, action_185, j),
        52 => happyGoto(p, action_186, j),
        53 => happyGoto(p, action_97, j),
        54 => happyGoto(p, action_98, j),
        61 => happyGoto(p, action_99, j),
        64 => happyGoto(p, action_187, j),
        132 => happyGoto(p, action_796, j),
        133 => happyGoto(p, action_797, j),
        134 => happyGoto(p, action_110, j),
        _ => happyReduce_471(p, j)
    }
}

fn action_714(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_475, j),
        150 => happyShift(p, action_476, j),
        167 => happyShift(p, action_795, j),
        190 => happyShift(p, action_116, j),
        192 => happyShift(p, action_117, j),
        195 => happyShift(p, action_118, j),
        198 => happyShift(p, action_119, j),
        200 => happyShift(p, action_120, j),
        202 => happyShift(p, action_122, j),
        208 => happyShift(p, action_124, j),
        209 => happyShift(p, action_125, j),
        210 => happyShift(p, action_126, j),
        218 => happyShift(p, action_129, j),
        219 => happyShift(p, action_130, j),
        223 => happyShift(p, action_133, j),
        226 => happyShift(p, action_168, j),
        228 => happyShift(p, action_137, j),
        229 => happyShift(p, action_138, j),
        230 => happyShift(p, action_139, j),
        237 => happyShift(p, action_140, j),
        238 => happyShift(p, action_169, j),
        239 => happyShift(p, action_142, j),
        45 => happyGoto(p, action_163, j),
        52 => happyGoto(p, action_164, j),
        53 => happyGoto(p, action_97, j),
        54 => happyGoto(p, action_98, j),
        60 => happyGoto(p, action_793, j),
        61 => happyGoto(p, action_99, j),
        75 => happyGoto(p, action_794, j),
        76 => happyGoto(p, action_102, j),
        77 => happyGoto(p, action_103, j),
        78 => happyGoto(p, action_481, j),
        134 => happyGoto(p, action_167, j),
        _ => happyReduce_472(p, j)
    }
}

fn action_715(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_192(p, j)
    }
}

fn action_716(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_187(p, j)
    }
}

fn action_717(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        190 => happyShift(p, action_116, j),
        192 => happyShift(p, action_117, j),
        195 => happyShift(p, action_118, j),
        198 => happyShift(p, action_119, j),
        200 => happyShift(p, action_120, j),
        202 => happyShift(p, action_122, j),
        208 => happyShift(p, action_124, j),
        209 => happyShift(p, action_125, j),
        210 => happyShift(p, action_126, j),
        218 => happyShift(p, action_129, j),
        219 => happyShift(p, action_130, j),
        223 => happyShift(p, action_133, j),
        226 => happyShift(p, action_135, j),
        228 => happyShift(p, action_137, j),
        229 => happyShift(p, action_138, j),
        230 => happyShift(p, action_139, j),
        238 => happyShift(p, action_141, j),
        239 => happyShift(p, action_142, j),
        240 => happyShift(p, action_717, j),
        44 => happyGoto(p, action_709, j),
        45 => happyGoto(p, action_89, j),
        47 => happyGoto(p, action_287, j),
        49 => happyGoto(p, action_288, j),
        51 => happyGoto(p, action_289, j),
        52 => happyGoto(p, action_96, j),
        53 => happyGoto(p, action_97, j),
        54 => happyGoto(p, action_98, j),
        56 => happyGoto(p, action_792, j),
        57 => happyGoto(p, action_711, j),
        58 => happyGoto(p, action_712, j),
        61 => happyGoto(p, action_99, j),
        65 => happyGoto(p, action_713, j),
        132 => happyGoto(p, action_108, j),
        133 => happyGoto(p, action_714, j),
        134 => happyGoto(p, action_110, j),
        _ => happyReduce_471(p, j)
    }
}

fn action_718(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_174(p, j)
    }
}

fn action_719(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_175(p, j)
    }
}

fn action_720(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_94(p, j)
    }
}

fn action_721(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_25(p, j)
    }
}

fn action_722(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        139 => happyShift(p, action_791, j),
        _ => happyFail(p, j)
    }
}

fn action_723(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        139 => happyShift(p, action_790, j),
        _ => happyFail(p, j)
    }
}

fn action_724(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        141 => happyShift(p, action_789, j),
        _ => happyFail(p, j)
    }
}

fn action_725(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        141 => happyShift(p, action_788, j),
        _ => happyFail(p, j)
    }
}

fn action_726(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        141 => happyReduce_471(p, j),
        239 => happyShift(p, action_142, j),
        132 => happyGoto(p, action_787, j),
        133 => happyGoto(p, action_150, j),
        134 => happyGoto(p, action_110, j),
        _ => happyReduce_401(p, j)
    }
}

fn action_727(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        239 => happyShift(p, action_142, j),
        132 => happyGoto(p, action_786, j),
        133 => happyGoto(p, action_150, j),
        134 => happyGoto(p, action_110, j),
        _ => happyReduce_471(p, j)
    }
}

fn action_728(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_317(p, j)
    }
}

fn action_729(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        141 => happyShift(p, action_785, j),
        _ => happyFail(p, j)
    }
}

fn action_730(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_316(p, j)
    }
}

fn action_731(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_322(p, j)
    }
}

fn action_732(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        141 => happyShift(p, action_784, j),
        _ => happyFail(p, j)
    }
}

fn action_733(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_26, j),
        144 => happyShift(p, action_27, j),
        145 => happyShift(p, action_28, j),
        146 => happyShift(p, action_29, j),
        147 => happyShift(p, action_30, j),
        148 => happyShift(p, action_31, j),
        149 => happyShift(p, action_32, j),
        150 => happyShift(p, action_33, j),
        153 => happyShift(p, action_34, j),
        164 => happyShift(p, action_35, j),
        184 => happyShift(p, action_36, j),
        204 => happyShift(p, action_37, j),
        220 => happyShift(p, action_38, j),
        233 => happyShift(p, action_39, j),
        234 => happyShift(p, action_40, j),
        235 => happyShift(p, action_41, j),
        236 => happyShift(p, action_42, j),
        237 => happyShift(p, action_43, j),
        240 => happyShift(p, action_44, j),
        241 => happyShift(p, action_45, j),
        242 => happyShift(p, action_46, j),
        243 => happyShift(p, action_47, j),
        244 => happyShift(p, action_48, j),
        245 => happyShift(p, action_49, j),
        100 => happyGoto(p, action_6, j),
        104 => happyGoto(p, action_7, j),
        106 => happyGoto(p, action_8, j),
        107 => happyGoto(p, action_9, j),
        108 => happyGoto(p, action_10, j),
        109 => happyGoto(p, action_11, j),
        110 => happyGoto(p, action_12, j),
        111 => happyGoto(p, action_13, j),
        112 => happyGoto(p, action_14, j),
        113 => happyGoto(p, action_15, j),
        114 => happyGoto(p, action_16, j),
        115 => happyGoto(p, action_17, j),
        116 => happyGoto(p, action_18, j),
        117 => happyGoto(p, action_19, j),
        118 => happyGoto(p, action_20, j),
        119 => happyGoto(p, action_21, j),
        120 => happyGoto(p, action_783, j),
        127 => happyGoto(p, action_24, j),
        128 => happyGoto(p, action_25, j),
        _ => happyFail(p, j)
    }
}

fn action_734(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_289(p, j)
    }
}

fn action_735(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_290(p, j)
    }
}

fn action_736(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_293(p, j)
    }
}

fn action_737(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_540, j),
        140 => happyShift(p, action_182, j),
        150 => happyShift(p, action_541, j),
        186 => happyShift(p, action_171, j),
        193 => happyShift(p, action_172, j),
        213 => happyShift(p, action_173, j),
        214 => happyShift(p, action_174, j),
        216 => happyShift(p, action_175, j),
        231 => happyShift(p, action_176, j),
        237 => happyShift(p, action_140, j),
        238 => happyShift(p, action_229, j),
        239 => happyShift(p, action_142, j),
        64 => happyGoto(p, action_187, j),
        69 => happyGoto(p, action_578, j),
        70 => happyGoto(p, action_221, j),
        71 => happyGoto(p, action_222, j),
        75 => happyGoto(p, action_482, j),
        76 => happyGoto(p, action_102, j),
        77 => happyGoto(p, action_103, j),
        78 => happyGoto(p, action_481, j),
        87 => happyGoto(p, action_662, j),
        88 => happyGoto(p, action_367, j),
        89 => happyGoto(p, action_179, j),
        90 => happyGoto(p, action_180, j),
        91 => happyGoto(p, action_368, j),
        92 => happyGoto(p, action_369, j),
        132 => happyGoto(p, action_663, j),
        133 => happyGoto(p, action_782, j),
        134 => happyGoto(p, action_110, j),
        _ => happyReduce_471(p, j)
    }
}

fn action_738(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_540, j),
        140 => happyShift(p, action_182, j),
        150 => happyShift(p, action_541, j),
        186 => happyReduce_472(p, j),
        193 => happyReduce_472(p, j),
        213 => happyReduce_472(p, j),
        214 => happyReduce_472(p, j),
        216 => happyReduce_472(p, j),
        231 => happyReduce_472(p, j),
        237 => happyShift(p, action_140, j),
        238 => happyShift(p, action_229, j),
        239 => happyShift(p, action_142, j),
        69 => happyGoto(p, action_577, j),
        70 => happyGoto(p, action_221, j),
        71 => happyGoto(p, action_222, j),
        75 => happyGoto(p, action_480, j),
        76 => happyGoto(p, action_102, j),
        77 => happyGoto(p, action_103, j),
        78 => happyGoto(p, action_481, j),
        87 => happyGoto(p, action_661, j),
        88 => happyGoto(p, action_367, j),
        89 => happyGoto(p, action_179, j),
        90 => happyGoto(p, action_180, j),
        91 => happyGoto(p, action_368, j),
        92 => happyGoto(p, action_369, j),
        134 => happyGoto(p, action_167, j),
        _ => happyReduce_330(p, j)
    }
}

fn action_739(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_540, j),
        140 => happyShift(p, action_182, j),
        150 => happyShift(p, action_541, j),
        185 => happyShift(p, action_113, j),
        188 => happyShift(p, action_115, j),
        190 => happyShift(p, action_116, j),
        192 => happyShift(p, action_117, j),
        195 => happyShift(p, action_118, j),
        198 => happyShift(p, action_119, j),
        200 => happyShift(p, action_120, j),
        201 => happyShift(p, action_121, j),
        202 => happyShift(p, action_122, j),
        207 => happyShift(p, action_123, j),
        208 => happyShift(p, action_124, j),
        209 => happyShift(p, action_125, j),
        210 => happyShift(p, action_126, j),
        212 => happyShift(p, action_127, j),
        215 => happyShift(p, action_128, j),
        218 => happyShift(p, action_129, j),
        219 => happyShift(p, action_130, j),
        221 => happyShift(p, action_131, j),
        223 => happyShift(p, action_133, j),
        225 => happyShift(p, action_134, j),
        226 => happyShift(p, action_168, j),
        227 => happyShift(p, action_136, j),
        228 => happyShift(p, action_137, j),
        229 => happyShift(p, action_138, j),
        230 => happyShift(p, action_139, j),
        237 => happyShift(p, action_140, j),
        238 => happyShift(p, action_169, j),
        239 => happyShift(p, action_142, j),
        40 => happyGoto(p, action_162, j),
        41 => happyGoto(p, action_85, j),
        42 => happyGoto(p, action_86, j),
        43 => happyGoto(p, action_87, j),
        45 => happyGoto(p, action_163, j),
        52 => happyGoto(p, action_164, j),
        53 => happyGoto(p, action_97, j),
        54 => happyGoto(p, action_98, j),
        61 => happyGoto(p, action_99, j),
        70 => happyGoto(p, action_570, j),
        71 => happyGoto(p, action_222, j),
        76 => happyGoto(p, action_473, j),
        77 => happyGoto(p, action_103, j),
        78 => happyGoto(p, action_474, j),
        88 => happyGoto(p, action_655, j),
        89 => happyGoto(p, action_179, j),
        90 => happyGoto(p, action_180, j),
        91 => happyGoto(p, action_656, j),
        92 => happyGoto(p, action_657, j),
        134 => happyGoto(p, action_167, j),
        _ => happyReduce_472(p, j)
    }
}

fn action_740(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_296(p, j)
    }
}

fn action_741(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_297(p, j)
    }
}

fn action_742(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_535, j),
        140 => happyShift(p, action_182, j),
        150 => happyShift(p, action_536, j),
        186 => happyShift(p, action_171, j),
        193 => happyShift(p, action_172, j),
        213 => happyShift(p, action_173, j),
        214 => happyShift(p, action_174, j),
        216 => happyShift(p, action_175, j),
        231 => happyShift(p, action_176, j),
        237 => happyShift(p, action_140, j),
        239 => happyShift(p, action_142, j),
        64 => happyGoto(p, action_187, j),
        75 => happyGoto(p, action_482, j),
        76 => happyGoto(p, action_102, j),
        77 => happyGoto(p, action_103, j),
        78 => happyGoto(p, action_481, j),
        87 => happyGoto(p, action_662, j),
        88 => happyGoto(p, action_367, j),
        89 => happyGoto(p, action_179, j),
        90 => happyGoto(p, action_180, j),
        91 => happyGoto(p, action_368, j),
        92 => happyGoto(p, action_369, j),
        132 => happyGoto(p, action_663, j),
        133 => happyGoto(p, action_781, j),
        134 => happyGoto(p, action_110, j),
        _ => happyReduce_471(p, j)
    }
}

fn action_743(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_535, j),
        140 => happyShift(p, action_182, j),
        150 => happyShift(p, action_536, j),
        186 => happyReduce_472(p, j),
        193 => happyReduce_472(p, j),
        213 => happyReduce_472(p, j),
        214 => happyReduce_472(p, j),
        216 => happyReduce_472(p, j),
        231 => happyReduce_472(p, j),
        237 => happyShift(p, action_140, j),
        239 => happyShift(p, action_142, j),
        75 => happyGoto(p, action_480, j),
        76 => happyGoto(p, action_102, j),
        77 => happyGoto(p, action_103, j),
        78 => happyGoto(p, action_481, j),
        87 => happyGoto(p, action_661, j),
        88 => happyGoto(p, action_367, j),
        89 => happyGoto(p, action_179, j),
        90 => happyGoto(p, action_180, j),
        91 => happyGoto(p, action_368, j),
        92 => happyGoto(p, action_369, j),
        134 => happyGoto(p, action_167, j),
        _ => happyReduce_330(p, j)
    }
}

fn action_744(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_535, j),
        140 => happyShift(p, action_182, j),
        150 => happyShift(p, action_536, j),
        185 => happyShift(p, action_113, j),
        188 => happyShift(p, action_115, j),
        190 => happyShift(p, action_116, j),
        192 => happyShift(p, action_117, j),
        195 => happyShift(p, action_118, j),
        198 => happyShift(p, action_119, j),
        200 => happyShift(p, action_120, j),
        201 => happyShift(p, action_121, j),
        202 => happyShift(p, action_122, j),
        207 => happyShift(p, action_123, j),
        208 => happyShift(p, action_124, j),
        209 => happyShift(p, action_125, j),
        210 => happyShift(p, action_126, j),
        212 => happyShift(p, action_127, j),
        215 => happyShift(p, action_128, j),
        218 => happyShift(p, action_129, j),
        219 => happyShift(p, action_130, j),
        221 => happyShift(p, action_131, j),
        223 => happyShift(p, action_133, j),
        225 => happyShift(p, action_134, j),
        226 => happyShift(p, action_168, j),
        227 => happyShift(p, action_136, j),
        228 => happyShift(p, action_137, j),
        229 => happyShift(p, action_138, j),
        230 => happyShift(p, action_139, j),
        237 => happyShift(p, action_140, j),
        238 => happyShift(p, action_169, j),
        239 => happyShift(p, action_142, j),
        40 => happyGoto(p, action_162, j),
        41 => happyGoto(p, action_85, j),
        42 => happyGoto(p, action_86, j),
        43 => happyGoto(p, action_87, j),
        45 => happyGoto(p, action_163, j),
        52 => happyGoto(p, action_164, j),
        53 => happyGoto(p, action_97, j),
        54 => happyGoto(p, action_98, j),
        61 => happyGoto(p, action_99, j),
        76 => happyGoto(p, action_473, j),
        77 => happyGoto(p, action_103, j),
        78 => happyGoto(p, action_474, j),
        88 => happyGoto(p, action_655, j),
        89 => happyGoto(p, action_179, j),
        90 => happyGoto(p, action_180, j),
        91 => happyGoto(p, action_656, j),
        92 => happyGoto(p, action_657, j),
        134 => happyGoto(p, action_167, j),
        _ => happyReduce_472(p, j)
    }
}

fn action_745(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_301(p, j)
    }
}

fn action_746(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_286(p, j)
    }
}

fn action_747(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_284(p, j)
    }
}

fn action_748(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_303(p, j)
    }
}

fn action_749(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_780, j),
        150 => happyShift(p, action_525, j),
        186 => happyShift(p, action_171, j),
        193 => happyShift(p, action_172, j),
        213 => happyShift(p, action_173, j),
        214 => happyShift(p, action_174, j),
        216 => happyShift(p, action_175, j),
        231 => happyShift(p, action_176, j),
        237 => happyShift(p, action_140, j),
        238 => happyShift(p, action_229, j),
        239 => happyShift(p, action_142, j),
        64 => happyGoto(p, action_187, j),
        69 => happyGoto(p, action_578, j),
        70 => happyGoto(p, action_221, j),
        71 => happyGoto(p, action_222, j),
        72 => happyGoto(p, action_579, j),
        73 => happyGoto(p, action_224, j),
        75 => happyGoto(p, action_482, j),
        76 => happyGoto(p, action_102, j),
        77 => happyGoto(p, action_103, j),
        78 => happyGoto(p, action_481, j),
        133 => happyGoto(p, action_580, j),
        134 => happyGoto(p, action_110, j),
        _ => happyFail(p, j)
    }
}

fn action_750(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_751, j),
        150 => happyShift(p, action_525, j),
        237 => happyShift(p, action_140, j),
        238 => happyShift(p, action_424, j),
        239 => happyShift(p, action_142, j),
        70 => happyGoto(p, action_419, j),
        71 => happyGoto(p, action_222, j),
        72 => happyGoto(p, action_420, j),
        73 => happyGoto(p, action_224, j),
        74 => happyGoto(p, action_576, j),
        76 => happyGoto(p, action_157, j),
        77 => happyGoto(p, action_103, j),
        78 => happyGoto(p, action_512, j),
        133 => happyGoto(p, action_422, j),
        134 => happyGoto(p, action_110, j),
        _ => happyFail(p, j)
    }
}

fn action_751(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_751, j),
        150 => happyShift(p, action_525, j),
        237 => happyShift(p, action_140, j),
        238 => happyShift(p, action_424, j),
        239 => happyShift(p, action_142, j),
        70 => happyGoto(p, action_419, j),
        71 => happyGoto(p, action_222, j),
        72 => happyGoto(p, action_420, j),
        73 => happyGoto(p, action_224, j),
        74 => happyGoto(p, action_569, j),
        76 => happyGoto(p, action_157, j),
        77 => happyGoto(p, action_103, j),
        78 => happyGoto(p, action_512, j),
        133 => happyGoto(p, action_422, j),
        134 => happyGoto(p, action_110, j),
        _ => happyFail(p, j)
    }
}

fn action_752(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        187 => happyShift(p, action_406, j),
        35 => happyGoto(p, action_561, j),
        67 => happyGoto(p, action_405, j),
        _ => happyReduce_232(p, j)
    }
}

fn action_753(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_177(p, j)
    }
}

fn action_754(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_178(p, j)
    }
}

fn action_755(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_345(p, j)
    }
}

fn action_756(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_271(p, j)
    }
}

fn action_757(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_11(p, j)
    }
}

fn action_758(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        179 => happyShift(p, action_778, j),
        182 => happyShift(p, action_779, j),
        _ => happyFail(p, j)
    }
}

fn action_759(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        168 => happyShift(p, action_777, j),
        239 => happyShift(p, action_142, j),
        134 => happyGoto(p, action_167, j),
        _ => happyReduce_218(p, j)
    }
}

fn action_760(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_26, j),
        144 => happyShift(p, action_27, j),
        145 => happyShift(p, action_28, j),
        146 => happyShift(p, action_29, j),
        147 => happyShift(p, action_30, j),
        148 => happyShift(p, action_31, j),
        149 => happyShift(p, action_32, j),
        150 => happyShift(p, action_33, j),
        153 => happyShift(p, action_34, j),
        164 => happyShift(p, action_35, j),
        184 => happyShift(p, action_36, j),
        204 => happyShift(p, action_37, j),
        220 => happyShift(p, action_38, j),
        233 => happyShift(p, action_39, j),
        234 => happyShift(p, action_40, j),
        235 => happyShift(p, action_41, j),
        236 => happyShift(p, action_42, j),
        237 => happyShift(p, action_43, j),
        240 => happyShift(p, action_44, j),
        241 => happyShift(p, action_45, j),
        242 => happyShift(p, action_46, j),
        243 => happyShift(p, action_47, j),
        244 => happyShift(p, action_48, j),
        245 => happyShift(p, action_49, j),
        100 => happyGoto(p, action_6, j),
        104 => happyGoto(p, action_7, j),
        106 => happyGoto(p, action_259, j),
        107 => happyGoto(p, action_9, j),
        108 => happyGoto(p, action_10, j),
        109 => happyGoto(p, action_11, j),
        110 => happyGoto(p, action_12, j),
        111 => happyGoto(p, action_13, j),
        112 => happyGoto(p, action_14, j),
        113 => happyGoto(p, action_15, j),
        114 => happyGoto(p, action_16, j),
        115 => happyGoto(p, action_17, j),
        116 => happyGoto(p, action_18, j),
        117 => happyGoto(p, action_19, j),
        118 => happyGoto(p, action_20, j),
        119 => happyGoto(p, action_260, j),
        126 => happyGoto(p, action_776, j),
        127 => happyGoto(p, action_24, j),
        128 => happyGoto(p, action_25, j),
        _ => happyFail(p, j)
    }
}

fn action_761(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        182 => happyShift(p, action_775, j),
        237 => happyShift(p, action_254, j),
        238 => happyShift(p, action_75, j),
        63 => happyGoto(p, action_774, j),
        131 => happyGoto(p, action_503, j),
        _ => happyFail(p, j)
    }
}

fn action_762(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_210(p, j)
    }
}

fn action_763(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        139 => happyShift(p, action_773, j),
        _ => happyFail(p, j)
    }
}

fn action_764(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_26, j),
        139 => happyShift(p, action_772, j),
        144 => happyShift(p, action_27, j),
        145 => happyShift(p, action_28, j),
        146 => happyShift(p, action_29, j),
        147 => happyShift(p, action_30, j),
        148 => happyShift(p, action_31, j),
        149 => happyShift(p, action_32, j),
        150 => happyShift(p, action_33, j),
        153 => happyShift(p, action_34, j),
        164 => happyShift(p, action_35, j),
        184 => happyShift(p, action_36, j),
        204 => happyShift(p, action_37, j),
        220 => happyShift(p, action_38, j),
        233 => happyShift(p, action_39, j),
        234 => happyShift(p, action_40, j),
        235 => happyShift(p, action_41, j),
        236 => happyShift(p, action_42, j),
        237 => happyShift(p, action_43, j),
        240 => happyShift(p, action_44, j),
        241 => happyShift(p, action_45, j),
        242 => happyShift(p, action_46, j),
        243 => happyShift(p, action_47, j),
        244 => happyShift(p, action_48, j),
        245 => happyShift(p, action_49, j),
        100 => happyGoto(p, action_6, j),
        104 => happyGoto(p, action_7, j),
        106 => happyGoto(p, action_769, j),
        107 => happyGoto(p, action_9, j),
        108 => happyGoto(p, action_10, j),
        109 => happyGoto(p, action_11, j),
        110 => happyGoto(p, action_12, j),
        111 => happyGoto(p, action_13, j),
        112 => happyGoto(p, action_14, j),
        113 => happyGoto(p, action_15, j),
        114 => happyGoto(p, action_16, j),
        115 => happyGoto(p, action_17, j),
        116 => happyGoto(p, action_18, j),
        117 => happyGoto(p, action_19, j),
        118 => happyGoto(p, action_20, j),
        119 => happyGoto(p, action_260, j),
        126 => happyGoto(p, action_770, j),
        127 => happyGoto(p, action_24, j),
        128 => happyGoto(p, action_25, j),
        137 => happyGoto(p, action_771, j),
        _ => happyFail(p, j)
    }
}

fn action_765(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        139 => happyShift(p, action_768, j),
        _ => happyFail(p, j)
    }
}

fn action_766(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        193 => happyShift(p, action_496, j),
        237 => happyShift(p, action_497, j),
        136 => happyGoto(p, action_767, j),
        _ => happyReduce_478(p, j)
    }
}

fn action_767(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_477(p, j)
    }
}

fn action_768(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_475(p, j)
    }
}

fn action_769(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        168 => happyShift(p, action_316, j),
        169 => happyShift(p, action_317, j),
        170 => happyShift(p, action_318, j),
        171 => happyShift(p, action_319, j),
        172 => happyShift(p, action_320, j),
        173 => happyShift(p, action_321, j),
        174 => happyShift(p, action_322, j),
        175 => happyShift(p, action_323, j),
        176 => happyShift(p, action_324, j),
        177 => happyShift(p, action_325, j),
        178 => happyShift(p, action_326, j),
        121 => happyGoto(p, action_896, j),
        _ => happyReduce_406(p, j)
    }
}

fn action_770(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_483(p, j)
    }
}

fn action_771(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        139 => happyShift(p, action_894, j),
        179 => happyShift(p, action_895, j),
        _ => happyFail(p, j)
    }
}

fn action_772(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_482(p, j)
    }
}

fn action_773(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        180 => happyShift(p, action_893, j),
        _ => happyFail(p, j)
    }
}

fn action_774(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_216(p, j)
    }
}

fn action_775(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_211(p, j)
    }
}

fn action_776(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_220(p, j)
    }
}

fn action_777(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_26, j),
        144 => happyShift(p, action_27, j),
        145 => happyShift(p, action_28, j),
        146 => happyShift(p, action_29, j),
        147 => happyShift(p, action_30, j),
        148 => happyShift(p, action_31, j),
        149 => happyShift(p, action_32, j),
        150 => happyShift(p, action_33, j),
        153 => happyShift(p, action_34, j),
        164 => happyShift(p, action_35, j),
        184 => happyShift(p, action_36, j),
        204 => happyShift(p, action_37, j),
        220 => happyShift(p, action_38, j),
        233 => happyShift(p, action_39, j),
        234 => happyShift(p, action_40, j),
        235 => happyShift(p, action_41, j),
        236 => happyShift(p, action_42, j),
        237 => happyShift(p, action_43, j),
        240 => happyShift(p, action_44, j),
        241 => happyShift(p, action_45, j),
        242 => happyShift(p, action_46, j),
        243 => happyShift(p, action_47, j),
        244 => happyShift(p, action_48, j),
        245 => happyShift(p, action_49, j),
        100 => happyGoto(p, action_6, j),
        104 => happyGoto(p, action_7, j),
        106 => happyGoto(p, action_259, j),
        107 => happyGoto(p, action_9, j),
        108 => happyGoto(p, action_10, j),
        109 => happyGoto(p, action_11, j),
        110 => happyGoto(p, action_12, j),
        111 => happyGoto(p, action_13, j),
        112 => happyGoto(p, action_14, j),
        113 => happyGoto(p, action_15, j),
        114 => happyGoto(p, action_16, j),
        115 => happyGoto(p, action_17, j),
        116 => happyGoto(p, action_18, j),
        117 => happyGoto(p, action_19, j),
        118 => happyGoto(p, action_20, j),
        119 => happyGoto(p, action_260, j),
        126 => happyGoto(p, action_892, j),
        127 => happyGoto(p, action_24, j),
        128 => happyGoto(p, action_25, j),
        _ => happyFail(p, j)
    }
}

fn action_778(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        182 => happyShift(p, action_891, j),
        237 => happyShift(p, action_254, j),
        238 => happyShift(p, action_75, j),
        63 => happyGoto(p, action_774, j),
        131 => happyGoto(p, action_503, j),
        _ => happyFail(p, j)
    }
}

fn action_779(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_212(p, j)
    }
}

fn action_780(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_751, j),
        150 => happyShift(p, action_525, j),
        237 => happyShift(p, action_140, j),
        238 => happyShift(p, action_424, j),
        239 => happyShift(p, action_142, j),
        70 => happyGoto(p, action_419, j),
        71 => happyGoto(p, action_222, j),
        72 => happyGoto(p, action_420, j),
        73 => happyGoto(p, action_224, j),
        74 => happyGoto(p, action_697, j),
        76 => happyGoto(p, action_157, j),
        77 => happyGoto(p, action_103, j),
        78 => happyGoto(p, action_512, j),
        133 => happyGoto(p, action_422, j),
        134 => happyGoto(p, action_110, j),
        _ => happyFail(p, j)
    }
}

fn action_781(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_475, j),
        150 => happyShift(p, action_476, j),
        186 => happyShift(p, action_171, j),
        193 => happyShift(p, action_172, j),
        213 => happyShift(p, action_173, j),
        214 => happyShift(p, action_174, j),
        216 => happyShift(p, action_175, j),
        231 => happyShift(p, action_176, j),
        237 => happyShift(p, action_140, j),
        239 => happyShift(p, action_142, j),
        64 => happyGoto(p, action_435, j),
        75 => happyGoto(p, action_508, j),
        76 => happyGoto(p, action_102, j),
        77 => happyGoto(p, action_103, j),
        78 => happyGoto(p, action_481, j),
        134 => happyGoto(p, action_167, j),
        _ => happyReduce_472(p, j)
    }
}

fn action_782(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_571, j),
        150 => happyShift(p, action_572, j),
        186 => happyShift(p, action_171, j),
        193 => happyShift(p, action_172, j),
        213 => happyShift(p, action_173, j),
        214 => happyShift(p, action_174, j),
        216 => happyShift(p, action_175, j),
        231 => happyShift(p, action_176, j),
        237 => happyShift(p, action_140, j),
        238 => happyShift(p, action_229, j),
        239 => happyShift(p, action_142, j),
        64 => happyGoto(p, action_435, j),
        69 => happyGoto(p, action_698, j),
        70 => happyGoto(p, action_221, j),
        71 => happyGoto(p, action_222, j),
        75 => happyGoto(p, action_508, j),
        76 => happyGoto(p, action_102, j),
        77 => happyGoto(p, action_103, j),
        78 => happyGoto(p, action_481, j),
        134 => happyGoto(p, action_167, j),
        _ => happyReduce_472(p, j)
    }
}

fn action_783(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        141 => happyShift(p, action_890, j),
        _ => happyFail(p, j)
    }
}

fn action_784(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_319(p, j)
    }
}

fn action_785(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_323(p, j)
    }
}

fn action_786(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_26, j),
        144 => happyShift(p, action_27, j),
        145 => happyShift(p, action_28, j),
        146 => happyShift(p, action_29, j),
        147 => happyShift(p, action_30, j),
        148 => happyShift(p, action_31, j),
        149 => happyShift(p, action_32, j),
        150 => happyShift(p, action_33, j),
        153 => happyShift(p, action_34, j),
        164 => happyShift(p, action_35, j),
        184 => happyShift(p, action_36, j),
        204 => happyShift(p, action_37, j),
        220 => happyShift(p, action_38, j),
        233 => happyShift(p, action_39, j),
        234 => happyShift(p, action_40, j),
        235 => happyShift(p, action_41, j),
        236 => happyShift(p, action_42, j),
        237 => happyShift(p, action_43, j),
        240 => happyShift(p, action_44, j),
        241 => happyShift(p, action_45, j),
        242 => happyShift(p, action_46, j),
        243 => happyShift(p, action_47, j),
        244 => happyShift(p, action_48, j),
        245 => happyShift(p, action_49, j),
        100 => happyGoto(p, action_6, j),
        104 => happyGoto(p, action_7, j),
        106 => happyGoto(p, action_8, j),
        107 => happyGoto(p, action_9, j),
        108 => happyGoto(p, action_10, j),
        109 => happyGoto(p, action_11, j),
        110 => happyGoto(p, action_12, j),
        111 => happyGoto(p, action_13, j),
        112 => happyGoto(p, action_14, j),
        113 => happyGoto(p, action_15, j),
        114 => happyGoto(p, action_16, j),
        115 => happyGoto(p, action_17, j),
        116 => happyGoto(p, action_18, j),
        117 => happyGoto(p, action_19, j),
        118 => happyGoto(p, action_20, j),
        119 => happyGoto(p, action_21, j),
        120 => happyGoto(p, action_889, j),
        127 => happyGoto(p, action_24, j),
        128 => happyGoto(p, action_25, j),
        _ => happyFail(p, j)
    }
}

fn action_787(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        141 => happyShift(p, action_888, j),
        _ => happyFail(p, j)
    }
}

fn action_788(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_318(p, j)
    }
}

fn action_789(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_324(p, j)
    }
}

fn action_790(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_180(p, j)
    }
}

fn action_791(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_181(p, j)
    }
}

fn action_792(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_196(p, j)
    }
}

fn action_793(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        239 => happyShift(p, action_142, j),
        134 => happyGoto(p, action_887, j),
        _ => happyReduce_198(p, j)
    }
}

fn action_794(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        167 => happyShift(p, action_886, j),
        _ => happyReduce_206(p, j)
    }
}

fn action_795(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_26, j),
        144 => happyShift(p, action_27, j),
        145 => happyShift(p, action_28, j),
        146 => happyShift(p, action_29, j),
        147 => happyShift(p, action_30, j),
        148 => happyShift(p, action_31, j),
        149 => happyShift(p, action_32, j),
        150 => happyShift(p, action_33, j),
        153 => happyShift(p, action_34, j),
        164 => happyShift(p, action_35, j),
        184 => happyShift(p, action_36, j),
        204 => happyShift(p, action_37, j),
        220 => happyShift(p, action_38, j),
        233 => happyShift(p, action_39, j),
        234 => happyShift(p, action_40, j),
        235 => happyShift(p, action_41, j),
        236 => happyShift(p, action_42, j),
        237 => happyShift(p, action_43, j),
        240 => happyShift(p, action_44, j),
        241 => happyShift(p, action_45, j),
        242 => happyShift(p, action_46, j),
        243 => happyShift(p, action_47, j),
        244 => happyShift(p, action_48, j),
        245 => happyShift(p, action_49, j),
        100 => happyGoto(p, action_6, j),
        104 => happyGoto(p, action_7, j),
        106 => happyGoto(p, action_259, j),
        107 => happyGoto(p, action_9, j),
        108 => happyGoto(p, action_10, j),
        109 => happyGoto(p, action_11, j),
        110 => happyGoto(p, action_12, j),
        111 => happyGoto(p, action_13, j),
        112 => happyGoto(p, action_14, j),
        113 => happyGoto(p, action_15, j),
        114 => happyGoto(p, action_16, j),
        115 => happyGoto(p, action_17, j),
        116 => happyGoto(p, action_18, j),
        117 => happyGoto(p, action_19, j),
        118 => happyGoto(p, action_20, j),
        119 => happyGoto(p, action_260, j),
        126 => happyGoto(p, action_885, j),
        127 => happyGoto(p, action_24, j),
        128 => happyGoto(p, action_25, j),
        _ => happyFail(p, j)
    }
}

fn action_796(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_475, j),
        150 => happyShift(p, action_476, j),
        167 => happyShift(p, action_795, j),
        237 => happyShift(p, action_140, j),
        60 => happyGoto(p, action_884, j),
        75 => happyGoto(p, action_794, j),
        76 => happyGoto(p, action_102, j),
        77 => happyGoto(p, action_103, j),
        78 => happyGoto(p, action_481, j),
        _ => happyFail(p, j)
    }
}

fn action_797(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        186 => happyShift(p, action_171, j),
        190 => happyShift(p, action_116, j),
        192 => happyShift(p, action_117, j),
        193 => happyShift(p, action_172, j),
        195 => happyShift(p, action_118, j),
        198 => happyShift(p, action_119, j),
        200 => happyShift(p, action_120, j),
        202 => happyShift(p, action_122, j),
        208 => happyShift(p, action_124, j),
        209 => happyShift(p, action_125, j),
        210 => happyShift(p, action_126, j),
        213 => happyShift(p, action_173, j),
        214 => happyShift(p, action_174, j),
        216 => happyShift(p, action_175, j),
        218 => happyShift(p, action_129, j),
        219 => happyShift(p, action_130, j),
        223 => happyShift(p, action_133, j),
        226 => happyShift(p, action_438, j),
        228 => happyShift(p, action_137, j),
        229 => happyShift(p, action_138, j),
        230 => happyShift(p, action_139, j),
        231 => happyShift(p, action_176, j),
        238 => happyShift(p, action_439, j),
        239 => happyShift(p, action_142, j),
        45 => happyGoto(p, action_433, j),
        52 => happyGoto(p, action_434, j),
        53 => happyGoto(p, action_97, j),
        54 => happyGoto(p, action_98, j),
        61 => happyGoto(p, action_99, j),
        64 => happyGoto(p, action_435, j),
        134 => happyGoto(p, action_167, j),
        _ => happyReduce_472(p, j)
    }
}

fn action_798(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        239 => happyShift(p, action_142, j),
        132 => happyGoto(p, action_883, j),
        133 => happyGoto(p, action_150, j),
        134 => happyGoto(p, action_110, j),
        _ => happyReduce_471(p, j)
    }
}

fn action_799(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_194(p, j)
    }
}

fn action_800(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        239 => happyShift(p, action_142, j),
        132 => happyGoto(p, action_882, j),
        133 => happyGoto(p, action_150, j),
        134 => happyGoto(p, action_110, j),
        _ => happyReduce_471(p, j)
    }
}

fn action_801(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_195(p, j)
    }
}

fn action_802(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        239 => happyShift(p, action_142, j),
        132 => happyGoto(p, action_881, j),
        133 => happyGoto(p, action_150, j),
        134 => happyGoto(p, action_110, j),
        _ => happyReduce_471(p, j)
    }
}

fn action_803(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        167 => happyShift(p, action_880, j),
        _ => happyReduce_203(p, j)
    }
}

fn action_804(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_26, j),
        144 => happyShift(p, action_27, j),
        145 => happyShift(p, action_28, j),
        146 => happyShift(p, action_29, j),
        147 => happyShift(p, action_30, j),
        148 => happyShift(p, action_31, j),
        149 => happyShift(p, action_32, j),
        150 => happyShift(p, action_33, j),
        153 => happyShift(p, action_34, j),
        164 => happyShift(p, action_35, j),
        184 => happyShift(p, action_36, j),
        204 => happyShift(p, action_37, j),
        220 => happyShift(p, action_38, j),
        233 => happyShift(p, action_39, j),
        234 => happyShift(p, action_40, j),
        235 => happyShift(p, action_41, j),
        236 => happyShift(p, action_42, j),
        237 => happyShift(p, action_43, j),
        240 => happyShift(p, action_44, j),
        241 => happyShift(p, action_45, j),
        242 => happyShift(p, action_46, j),
        243 => happyShift(p, action_47, j),
        244 => happyShift(p, action_48, j),
        245 => happyShift(p, action_49, j),
        100 => happyGoto(p, action_6, j),
        104 => happyGoto(p, action_7, j),
        106 => happyGoto(p, action_259, j),
        107 => happyGoto(p, action_9, j),
        108 => happyGoto(p, action_10, j),
        109 => happyGoto(p, action_11, j),
        110 => happyGoto(p, action_12, j),
        111 => happyGoto(p, action_13, j),
        112 => happyGoto(p, action_14, j),
        113 => happyGoto(p, action_15, j),
        114 => happyGoto(p, action_16, j),
        115 => happyGoto(p, action_17, j),
        116 => happyGoto(p, action_18, j),
        117 => happyGoto(p, action_19, j),
        118 => happyGoto(p, action_20, j),
        119 => happyGoto(p, action_260, j),
        126 => happyGoto(p, action_879, j),
        127 => happyGoto(p, action_24, j),
        128 => happyGoto(p, action_25, j),
        _ => happyFail(p, j)
    }
}

fn action_805(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_186(p, j)
    }
}

fn action_806(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_247(p, j)
    }
}

fn action_807(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_571, j),
        150 => happyShift(p, action_572, j),
        186 => happyShift(p, action_171, j),
        193 => happyShift(p, action_172, j),
        213 => happyShift(p, action_173, j),
        214 => happyShift(p, action_174, j),
        216 => happyShift(p, action_175, j),
        231 => happyShift(p, action_176, j),
        237 => happyShift(p, action_140, j),
        238 => happyShift(p, action_229, j),
        239 => happyShift(p, action_142, j),
        64 => happyGoto(p, action_435, j),
        69 => happyGoto(p, action_698, j),
        70 => happyGoto(p, action_221, j),
        71 => happyGoto(p, action_222, j),
        75 => happyGoto(p, action_508, j),
        76 => happyGoto(p, action_102, j),
        77 => happyGoto(p, action_103, j),
        78 => happyGoto(p, action_481, j),
        134 => happyGoto(p, action_167, j),
        _ => happyFail(p, j)
    }
}

fn action_808(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_413, j),
        139 => happyShift(p, action_878, j),
        140 => happyShift(p, action_182, j),
        88 => happyGoto(p, action_573, j),
        89 => happyGoto(p, action_179, j),
        90 => happyGoto(p, action_180, j),
        _ => happyFail(p, j)
    }
}

fn action_809(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_250(p, j)
    }
}

fn action_810(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_233(p, j)
    }
}

fn action_811(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_100(p, j)
    }
}

fn action_812(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_96(p, j)
    }
}

fn action_813(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_26, j),
        144 => happyShift(p, action_27, j),
        145 => happyShift(p, action_28, j),
        146 => happyShift(p, action_29, j),
        147 => happyShift(p, action_30, j),
        148 => happyShift(p, action_31, j),
        149 => happyShift(p, action_32, j),
        150 => happyShift(p, action_33, j),
        153 => happyShift(p, action_34, j),
        164 => happyShift(p, action_35, j),
        180 => happyShift(p, action_60, j),
        181 => happyShift(p, action_61, j),
        184 => happyShift(p, action_36, j),
        187 => happyShift(p, action_62, j),
        189 => happyShift(p, action_63, j),
        191 => happyShift(p, action_64, j),
        194 => happyShift(p, action_65, j),
        196 => happyShift(p, action_66, j),
        197 => happyShift(p, action_67, j),
        203 => happyShift(p, action_68, j),
        204 => happyShift(p, action_37, j),
        205 => happyShift(p, action_69, j),
        206 => happyShift(p, action_70, j),
        217 => happyShift(p, action_71, j),
        220 => happyShift(p, action_38, j),
        224 => happyShift(p, action_72, j),
        232 => happyShift(p, action_73, j),
        233 => happyShift(p, action_39, j),
        234 => happyShift(p, action_40, j),
        235 => happyShift(p, action_41, j),
        236 => happyShift(p, action_42, j),
        237 => happyShift(p, action_74, j),
        238 => happyShift(p, action_75, j),
        240 => happyShift(p, action_44, j),
        241 => happyShift(p, action_45, j),
        242 => happyShift(p, action_46, j),
        243 => happyShift(p, action_47, j),
        244 => happyShift(p, action_48, j),
        245 => happyShift(p, action_49, j),
        12 => happyGoto(p, action_877, j),
        13 => happyGoto(p, action_51, j),
        14 => happyGoto(p, action_52, j),
        22 => happyGoto(p, action_53, j),
        23 => happyGoto(p, action_54, j),
        24 => happyGoto(p, action_55, j),
        25 => happyGoto(p, action_56, j),
        26 => happyGoto(p, action_57, j),
        100 => happyGoto(p, action_6, j),
        104 => happyGoto(p, action_7, j),
        106 => happyGoto(p, action_8, j),
        107 => happyGoto(p, action_9, j),
        108 => happyGoto(p, action_10, j),
        109 => happyGoto(p, action_11, j),
        110 => happyGoto(p, action_12, j),
        111 => happyGoto(p, action_13, j),
        112 => happyGoto(p, action_14, j),
        113 => happyGoto(p, action_15, j),
        114 => happyGoto(p, action_16, j),
        115 => happyGoto(p, action_17, j),
        116 => happyGoto(p, action_18, j),
        117 => happyGoto(p, action_19, j),
        118 => happyGoto(p, action_20, j),
        119 => happyGoto(p, action_21, j),
        120 => happyGoto(p, action_22, j),
        122 => happyGoto(p, action_58, j),
        127 => happyGoto(p, action_24, j),
        128 => happyGoto(p, action_25, j),
        131 => happyGoto(p, action_59, j),
        _ => happyFail(p, j)
    }
}

fn action_814(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_26, j),
        144 => happyShift(p, action_27, j),
        145 => happyShift(p, action_28, j),
        146 => happyShift(p, action_29, j),
        147 => happyShift(p, action_30, j),
        148 => happyShift(p, action_31, j),
        149 => happyShift(p, action_32, j),
        150 => happyShift(p, action_33, j),
        153 => happyShift(p, action_34, j),
        164 => happyShift(p, action_35, j),
        184 => happyShift(p, action_36, j),
        204 => happyShift(p, action_37, j),
        220 => happyShift(p, action_38, j),
        233 => happyShift(p, action_39, j),
        234 => happyShift(p, action_40, j),
        235 => happyShift(p, action_41, j),
        236 => happyShift(p, action_42, j),
        237 => happyShift(p, action_43, j),
        240 => happyShift(p, action_44, j),
        241 => happyShift(p, action_45, j),
        242 => happyShift(p, action_46, j),
        243 => happyShift(p, action_47, j),
        244 => happyShift(p, action_48, j),
        245 => happyShift(p, action_49, j),
        100 => happyGoto(p, action_6, j),
        104 => happyGoto(p, action_7, j),
        106 => happyGoto(p, action_8, j),
        107 => happyGoto(p, action_9, j),
        108 => happyGoto(p, action_10, j),
        109 => happyGoto(p, action_11, j),
        110 => happyGoto(p, action_12, j),
        111 => happyGoto(p, action_13, j),
        112 => happyGoto(p, action_14, j),
        113 => happyGoto(p, action_15, j),
        114 => happyGoto(p, action_16, j),
        115 => happyGoto(p, action_17, j),
        116 => happyGoto(p, action_18, j),
        117 => happyGoto(p, action_19, j),
        118 => happyGoto(p, action_20, j),
        119 => happyGoto(p, action_21, j),
        120 => happyGoto(p, action_22, j),
        122 => happyGoto(p, action_249, j),
        124 => happyGoto(p, action_876, j),
        127 => happyGoto(p, action_24, j),
        128 => happyGoto(p, action_25, j),
        _ => happyReduce_456(p, j)
    }
}

fn action_815(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_26, j),
        144 => happyShift(p, action_27, j),
        145 => happyShift(p, action_28, j),
        146 => happyShift(p, action_29, j),
        147 => happyShift(p, action_30, j),
        148 => happyShift(p, action_31, j),
        149 => happyShift(p, action_32, j),
        150 => happyShift(p, action_33, j),
        153 => happyShift(p, action_34, j),
        164 => happyShift(p, action_35, j),
        184 => happyShift(p, action_36, j),
        204 => happyShift(p, action_37, j),
        220 => happyShift(p, action_38, j),
        233 => happyShift(p, action_39, j),
        234 => happyShift(p, action_40, j),
        235 => happyShift(p, action_41, j),
        236 => happyShift(p, action_42, j),
        237 => happyShift(p, action_43, j),
        240 => happyShift(p, action_44, j),
        241 => happyShift(p, action_45, j),
        242 => happyShift(p, action_46, j),
        243 => happyShift(p, action_47, j),
        244 => happyShift(p, action_48, j),
        245 => happyShift(p, action_49, j),
        100 => happyGoto(p, action_6, j),
        104 => happyGoto(p, action_7, j),
        106 => happyGoto(p, action_8, j),
        107 => happyGoto(p, action_9, j),
        108 => happyGoto(p, action_10, j),
        109 => happyGoto(p, action_11, j),
        110 => happyGoto(p, action_12, j),
        111 => happyGoto(p, action_13, j),
        112 => happyGoto(p, action_14, j),
        113 => happyGoto(p, action_15, j),
        114 => happyGoto(p, action_16, j),
        115 => happyGoto(p, action_17, j),
        116 => happyGoto(p, action_18, j),
        117 => happyGoto(p, action_19, j),
        118 => happyGoto(p, action_20, j),
        119 => happyGoto(p, action_21, j),
        120 => happyGoto(p, action_22, j),
        122 => happyGoto(p, action_249, j),
        124 => happyGoto(p, action_875, j),
        127 => happyGoto(p, action_24, j),
        128 => happyGoto(p, action_25, j),
        _ => happyReduce_456(p, j)
    }
}

fn action_816(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        180 => happyShift(p, action_874, j),
        _ => happyFail(p, j)
    }
}

fn action_817(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_37(p, j)
    }
}

fn action_818(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        139 => happyShift(p, action_872, j),
        167 => happyShift(p, action_873, j),
        _ => happyFail(p, j)
    }
}

fn action_819(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        179 => happyShift(p, action_871, j),
        _ => happyReduce_77(p, j)
    }
}

fn action_820(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_78(p, j)
    }
}

fn action_821(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_870, j),
        _ => happyFail(p, j)
    }
}

fn action_822(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        237 => happyShift(p, action_868, j),
        238 => happyShift(p, action_869, j),
        _ => happyFail(p, j)
    }
}

fn action_823(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_70(p, j)
    }
}

fn action_824(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_39(p, j)
    }
}

fn action_825(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_55(p, j)
    }
}

fn action_826(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_49(p, j)
    }
}

fn action_827(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_51(p, j)
    }
}

fn action_828(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_50(p, j)
    }
}

fn action_829(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        181 => happyShift(p, action_61, j),
        14 => happyGoto(p, action_867, j),
        _ => happyFail(p, j)
    }
}

fn action_830(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_52(p, j)
    }
}

fn action_831(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_368(p, j)
    }
}

fn action_832(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_367(p, j)
    }
}

fn action_833(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_26, j),
        144 => happyShift(p, action_27, j),
        145 => happyShift(p, action_28, j),
        146 => happyShift(p, action_29, j),
        147 => happyShift(p, action_30, j),
        148 => happyShift(p, action_31, j),
        149 => happyShift(p, action_32, j),
        150 => happyShift(p, action_33, j),
        153 => happyShift(p, action_34, j),
        164 => happyShift(p, action_35, j),
        184 => happyShift(p, action_36, j),
        204 => happyShift(p, action_37, j),
        220 => happyShift(p, action_38, j),
        233 => happyShift(p, action_39, j),
        234 => happyShift(p, action_40, j),
        235 => happyShift(p, action_41, j),
        236 => happyShift(p, action_42, j),
        237 => happyShift(p, action_43, j),
        240 => happyShift(p, action_44, j),
        241 => happyShift(p, action_45, j),
        242 => happyShift(p, action_46, j),
        243 => happyShift(p, action_47, j),
        244 => happyShift(p, action_48, j),
        245 => happyShift(p, action_49, j),
        100 => happyGoto(p, action_6, j),
        104 => happyGoto(p, action_7, j),
        106 => happyGoto(p, action_8, j),
        107 => happyGoto(p, action_9, j),
        108 => happyGoto(p, action_10, j),
        109 => happyGoto(p, action_11, j),
        110 => happyGoto(p, action_12, j),
        111 => happyGoto(p, action_13, j),
        112 => happyGoto(p, action_14, j),
        113 => happyGoto(p, action_15, j),
        114 => happyGoto(p, action_16, j),
        115 => happyGoto(p, action_17, j),
        116 => happyGoto(p, action_18, j),
        117 => happyGoto(p, action_19, j),
        118 => happyGoto(p, action_20, j),
        119 => happyGoto(p, action_21, j),
        120 => happyGoto(p, action_22, j),
        122 => happyGoto(p, action_866, j),
        127 => happyGoto(p, action_24, j),
        128 => happyGoto(p, action_25, j),
        _ => happyFail(p, j)
    }
}

fn action_834(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        237 => happyShift(p, action_254, j),
        238 => happyShift(p, action_75, j),
        131 => happyGoto(p, action_865, j),
        _ => happyFail(p, j)
    }
}

fn action_835(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_366(p, j)
    }
}

fn action_836(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_26, j),
        144 => happyShift(p, action_27, j),
        145 => happyShift(p, action_28, j),
        146 => happyShift(p, action_29, j),
        147 => happyShift(p, action_30, j),
        148 => happyShift(p, action_31, j),
        149 => happyShift(p, action_32, j),
        150 => happyShift(p, action_33, j),
        153 => happyShift(p, action_34, j),
        164 => happyShift(p, action_35, j),
        184 => happyShift(p, action_36, j),
        204 => happyShift(p, action_37, j),
        220 => happyShift(p, action_38, j),
        233 => happyShift(p, action_39, j),
        234 => happyShift(p, action_40, j),
        235 => happyShift(p, action_41, j),
        236 => happyShift(p, action_42, j),
        237 => happyShift(p, action_43, j),
        240 => happyShift(p, action_44, j),
        241 => happyShift(p, action_45, j),
        242 => happyShift(p, action_46, j),
        243 => happyShift(p, action_47, j),
        244 => happyShift(p, action_48, j),
        245 => happyShift(p, action_49, j),
        100 => happyGoto(p, action_6, j),
        104 => happyGoto(p, action_7, j),
        106 => happyGoto(p, action_8, j),
        107 => happyGoto(p, action_9, j),
        108 => happyGoto(p, action_10, j),
        109 => happyGoto(p, action_11, j),
        110 => happyGoto(p, action_12, j),
        111 => happyGoto(p, action_13, j),
        112 => happyGoto(p, action_14, j),
        113 => happyGoto(p, action_15, j),
        114 => happyGoto(p, action_16, j),
        115 => happyGoto(p, action_17, j),
        116 => happyGoto(p, action_18, j),
        117 => happyGoto(p, action_19, j),
        118 => happyGoto(p, action_20, j),
        119 => happyGoto(p, action_21, j),
        120 => happyGoto(p, action_864, j),
        127 => happyGoto(p, action_24, j),
        128 => happyGoto(p, action_25, j),
        _ => happyFail(p, j)
    }
}

fn action_837(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_364(p, j)
    }
}

fn action_838(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        190 => happyShift(p, action_116, j),
        192 => happyShift(p, action_117, j),
        195 => happyShift(p, action_118, j),
        196 => happyShift(p, action_668, j),
        198 => happyShift(p, action_119, j),
        200 => happyShift(p, action_120, j),
        202 => happyShift(p, action_122, j),
        208 => happyShift(p, action_124, j),
        209 => happyShift(p, action_125, j),
        210 => happyShift(p, action_126, j),
        218 => happyShift(p, action_129, j),
        219 => happyShift(p, action_130, j),
        223 => happyShift(p, action_133, j),
        226 => happyShift(p, action_135, j),
        228 => happyShift(p, action_137, j),
        229 => happyShift(p, action_138, j),
        230 => happyShift(p, action_139, j),
        238 => happyShift(p, action_141, j),
        239 => happyShift(p, action_142, j),
        44 => happyGoto(p, action_286, j),
        45 => happyGoto(p, action_89, j),
        47 => happyGoto(p, action_287, j),
        49 => happyGoto(p, action_288, j),
        51 => happyGoto(p, action_289, j),
        52 => happyGoto(p, action_96, j),
        53 => happyGoto(p, action_97, j),
        54 => happyGoto(p, action_98, j),
        61 => happyGoto(p, action_99, j),
        65 => happyGoto(p, action_290, j),
        86 => happyGoto(p, action_665, j),
        102 => happyGoto(p, action_863, j),
        132 => happyGoto(p, action_108, j),
        133 => happyGoto(p, action_293, j),
        134 => happyGoto(p, action_110, j),
        _ => happyReduce_471(p, j)
    }
}

fn action_839(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_26, j),
        144 => happyShift(p, action_27, j),
        145 => happyShift(p, action_28, j),
        146 => happyShift(p, action_29, j),
        147 => happyShift(p, action_30, j),
        148 => happyShift(p, action_31, j),
        149 => happyShift(p, action_32, j),
        150 => happyShift(p, action_33, j),
        153 => happyShift(p, action_34, j),
        164 => happyShift(p, action_35, j),
        184 => happyShift(p, action_36, j),
        204 => happyShift(p, action_37, j),
        220 => happyShift(p, action_38, j),
        233 => happyShift(p, action_39, j),
        234 => happyShift(p, action_40, j),
        235 => happyShift(p, action_41, j),
        236 => happyShift(p, action_42, j),
        237 => happyShift(p, action_43, j),
        240 => happyShift(p, action_44, j),
        241 => happyShift(p, action_45, j),
        242 => happyShift(p, action_46, j),
        243 => happyShift(p, action_47, j),
        244 => happyShift(p, action_48, j),
        245 => happyShift(p, action_49, j),
        100 => happyGoto(p, action_6, j),
        104 => happyGoto(p, action_7, j),
        106 => happyGoto(p, action_8, j),
        107 => happyGoto(p, action_9, j),
        108 => happyGoto(p, action_10, j),
        109 => happyGoto(p, action_11, j),
        110 => happyGoto(p, action_12, j),
        111 => happyGoto(p, action_13, j),
        112 => happyGoto(p, action_14, j),
        113 => happyGoto(p, action_15, j),
        114 => happyGoto(p, action_16, j),
        115 => happyGoto(p, action_17, j),
        116 => happyGoto(p, action_18, j),
        117 => happyGoto(p, action_19, j),
        118 => happyGoto(p, action_20, j),
        119 => happyGoto(p, action_21, j),
        120 => happyGoto(p, action_862, j),
        127 => happyGoto(p, action_24, j),
        128 => happyGoto(p, action_25, j),
        _ => happyFail(p, j)
    }
}

fn action_840(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_335(p, j)
    }
}

fn action_841(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_337(p, j)
    }
}

fn action_842(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_413, j),
        140 => happyShift(p, action_182, j),
        88 => happyGoto(p, action_861, j),
        89 => happyGoto(p, action_179, j),
        90 => happyGoto(p, action_180, j),
        _ => happyReduce_336(p, j)
    }
}

fn action_843(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_338(p, j)
    }
}

fn action_844(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        179 => happyShift(p, action_859, j),
        182 => happyShift(p, action_860, j),
        _ => happyFail(p, j)
    }
}

fn action_845(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_357(p, j)
    }
}

fn action_846(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        141 => happyShift(p, action_857, j),
        183 => happyShift(p, action_858, j),
        _ => happyFail(p, j)
    }
}

fn action_847(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_352(p, j)
    }
}

fn action_848(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_355(p, j)
    }
}

fn action_849(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_358(p, j)
    }
}

fn action_850(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_351(p, j)
    }
}

fn action_851(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_348(p, j)
    }
}

fn action_852(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_26, j),
        140 => happyShift(p, action_652, j),
        143 => happyShift(p, action_653, j),
        144 => happyShift(p, action_27, j),
        145 => happyShift(p, action_28, j),
        146 => happyShift(p, action_29, j),
        147 => happyShift(p, action_30, j),
        148 => happyShift(p, action_31, j),
        149 => happyShift(p, action_32, j),
        150 => happyShift(p, action_33, j),
        153 => happyShift(p, action_34, j),
        164 => happyShift(p, action_35, j),
        181 => happyShift(p, action_654, j),
        182 => happyShift(p, action_856, j),
        184 => happyShift(p, action_36, j),
        204 => happyShift(p, action_37, j),
        220 => happyShift(p, action_38, j),
        233 => happyShift(p, action_39, j),
        234 => happyShift(p, action_40, j),
        235 => happyShift(p, action_41, j),
        236 => happyShift(p, action_42, j),
        237 => happyShift(p, action_74, j),
        238 => happyShift(p, action_75, j),
        240 => happyShift(p, action_44, j),
        241 => happyShift(p, action_45, j),
        242 => happyShift(p, action_46, j),
        243 => happyShift(p, action_47, j),
        244 => happyShift(p, action_48, j),
        245 => happyShift(p, action_49, j),
        93 => happyGoto(p, action_854, j),
        96 => happyGoto(p, action_855, j),
        97 => happyGoto(p, action_647, j),
        98 => happyGoto(p, action_648, j),
        99 => happyGoto(p, action_649, j),
        100 => happyGoto(p, action_6, j),
        104 => happyGoto(p, action_7, j),
        106 => happyGoto(p, action_8, j),
        107 => happyGoto(p, action_9, j),
        108 => happyGoto(p, action_10, j),
        109 => happyGoto(p, action_11, j),
        110 => happyGoto(p, action_12, j),
        111 => happyGoto(p, action_13, j),
        112 => happyGoto(p, action_14, j),
        113 => happyGoto(p, action_15, j),
        114 => happyGoto(p, action_16, j),
        115 => happyGoto(p, action_17, j),
        116 => happyGoto(p, action_18, j),
        117 => happyGoto(p, action_19, j),
        118 => happyGoto(p, action_20, j),
        119 => happyGoto(p, action_21, j),
        120 => happyGoto(p, action_650, j),
        127 => happyGoto(p, action_24, j),
        128 => happyGoto(p, action_25, j),
        131 => happyGoto(p, action_651, j),
        _ => happyFail(p, j)
    }
}

fn action_853(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_384(p, j)
    }
}

fn action_854(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_349(p, j)
    }
}

fn action_855(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_26, j),
        144 => happyShift(p, action_27, j),
        145 => happyShift(p, action_28, j),
        146 => happyShift(p, action_29, j),
        147 => happyShift(p, action_30, j),
        148 => happyShift(p, action_31, j),
        149 => happyShift(p, action_32, j),
        150 => happyShift(p, action_33, j),
        153 => happyShift(p, action_34, j),
        164 => happyShift(p, action_35, j),
        181 => happyShift(p, action_654, j),
        184 => happyShift(p, action_36, j),
        204 => happyShift(p, action_37, j),
        220 => happyShift(p, action_38, j),
        233 => happyShift(p, action_39, j),
        234 => happyShift(p, action_40, j),
        235 => happyShift(p, action_41, j),
        236 => happyShift(p, action_42, j),
        237 => happyShift(p, action_43, j),
        240 => happyShift(p, action_44, j),
        241 => happyShift(p, action_45, j),
        242 => happyShift(p, action_46, j),
        243 => happyShift(p, action_47, j),
        244 => happyShift(p, action_48, j),
        245 => happyShift(p, action_49, j),
        93 => happyGoto(p, action_918, j),
        100 => happyGoto(p, action_6, j),
        104 => happyGoto(p, action_7, j),
        106 => happyGoto(p, action_8, j),
        107 => happyGoto(p, action_9, j),
        108 => happyGoto(p, action_10, j),
        109 => happyGoto(p, action_11, j),
        110 => happyGoto(p, action_12, j),
        111 => happyGoto(p, action_13, j),
        112 => happyGoto(p, action_14, j),
        113 => happyGoto(p, action_15, j),
        114 => happyGoto(p, action_16, j),
        115 => happyGoto(p, action_17, j),
        116 => happyGoto(p, action_18, j),
        117 => happyGoto(p, action_19, j),
        118 => happyGoto(p, action_20, j),
        119 => happyGoto(p, action_21, j),
        120 => happyGoto(p, action_650, j),
        127 => happyGoto(p, action_24, j),
        128 => happyGoto(p, action_25, j),
        _ => happyFail(p, j)
    }
}

fn action_856(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_385(p, j)
    }
}

fn action_857(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_356(p, j)
    }
}

fn action_858(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_26, j),
        144 => happyShift(p, action_27, j),
        145 => happyShift(p, action_28, j),
        146 => happyShift(p, action_29, j),
        147 => happyShift(p, action_30, j),
        148 => happyShift(p, action_31, j),
        149 => happyShift(p, action_32, j),
        150 => happyShift(p, action_33, j),
        153 => happyShift(p, action_34, j),
        164 => happyShift(p, action_35, j),
        184 => happyShift(p, action_36, j),
        204 => happyShift(p, action_37, j),
        220 => happyShift(p, action_38, j),
        233 => happyShift(p, action_39, j),
        234 => happyShift(p, action_40, j),
        235 => happyShift(p, action_41, j),
        236 => happyShift(p, action_42, j),
        237 => happyShift(p, action_43, j),
        240 => happyShift(p, action_44, j),
        241 => happyShift(p, action_45, j),
        242 => happyShift(p, action_46, j),
        243 => happyShift(p, action_47, j),
        244 => happyShift(p, action_48, j),
        245 => happyShift(p, action_49, j),
        100 => happyGoto(p, action_6, j),
        104 => happyGoto(p, action_7, j),
        106 => happyGoto(p, action_259, j),
        107 => happyGoto(p, action_9, j),
        108 => happyGoto(p, action_10, j),
        109 => happyGoto(p, action_11, j),
        110 => happyGoto(p, action_12, j),
        111 => happyGoto(p, action_13, j),
        112 => happyGoto(p, action_14, j),
        113 => happyGoto(p, action_15, j),
        114 => happyGoto(p, action_16, j),
        115 => happyGoto(p, action_17, j),
        116 => happyGoto(p, action_18, j),
        117 => happyGoto(p, action_19, j),
        118 => happyGoto(p, action_20, j),
        119 => happyGoto(p, action_260, j),
        126 => happyGoto(p, action_917, j),
        127 => happyGoto(p, action_24, j),
        128 => happyGoto(p, action_25, j),
        _ => happyFail(p, j)
    }
}

fn action_859(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_26, j),
        140 => happyShift(p, action_652, j),
        143 => happyShift(p, action_653, j),
        144 => happyShift(p, action_27, j),
        145 => happyShift(p, action_28, j),
        146 => happyShift(p, action_29, j),
        147 => happyShift(p, action_30, j),
        148 => happyShift(p, action_31, j),
        149 => happyShift(p, action_32, j),
        150 => happyShift(p, action_33, j),
        153 => happyShift(p, action_34, j),
        164 => happyShift(p, action_35, j),
        181 => happyShift(p, action_654, j),
        182 => happyShift(p, action_916, j),
        184 => happyShift(p, action_36, j),
        204 => happyShift(p, action_37, j),
        220 => happyShift(p, action_38, j),
        233 => happyShift(p, action_39, j),
        234 => happyShift(p, action_40, j),
        235 => happyShift(p, action_41, j),
        236 => happyShift(p, action_42, j),
        237 => happyShift(p, action_74, j),
        238 => happyShift(p, action_75, j),
        240 => happyShift(p, action_44, j),
        241 => happyShift(p, action_45, j),
        242 => happyShift(p, action_46, j),
        243 => happyShift(p, action_47, j),
        244 => happyShift(p, action_48, j),
        245 => happyShift(p, action_49, j),
        93 => happyGoto(p, action_854, j),
        96 => happyGoto(p, action_855, j),
        97 => happyGoto(p, action_647, j),
        98 => happyGoto(p, action_648, j),
        99 => happyGoto(p, action_649, j),
        100 => happyGoto(p, action_6, j),
        104 => happyGoto(p, action_7, j),
        106 => happyGoto(p, action_8, j),
        107 => happyGoto(p, action_9, j),
        108 => happyGoto(p, action_10, j),
        109 => happyGoto(p, action_11, j),
        110 => happyGoto(p, action_12, j),
        111 => happyGoto(p, action_13, j),
        112 => happyGoto(p, action_14, j),
        113 => happyGoto(p, action_15, j),
        114 => happyGoto(p, action_16, j),
        115 => happyGoto(p, action_17, j),
        116 => happyGoto(p, action_18, j),
        117 => happyGoto(p, action_19, j),
        118 => happyGoto(p, action_20, j),
        119 => happyGoto(p, action_21, j),
        120 => happyGoto(p, action_650, j),
        127 => happyGoto(p, action_24, j),
        128 => happyGoto(p, action_25, j),
        131 => happyGoto(p, action_651, j),
        _ => happyFail(p, j)
    }
}

fn action_860(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_342(p, j)
    }
}

fn action_861(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_339(p, j)
    }
}

fn action_862(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_371(p, j)
    }
}

fn action_863(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_369(p, j)
    }
}

fn action_864(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_372(p, j)
    }
}

fn action_865(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_374(p, j)
    }
}

fn action_866(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        141 => happyShift(p, action_915, j),
        _ => happyFail(p, j)
    }
}

fn action_867(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_53(p, j)
    }
}

fn action_868(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        141 => happyShift(p, action_914, j),
        _ => happyFail(p, j)
    }
}

fn action_869(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        141 => happyShift(p, action_913, j),
        _ => happyFail(p, j)
    }
}

fn action_870(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_26, j),
        144 => happyShift(p, action_27, j),
        145 => happyShift(p, action_28, j),
        146 => happyShift(p, action_29, j),
        147 => happyShift(p, action_30, j),
        148 => happyShift(p, action_31, j),
        149 => happyShift(p, action_32, j),
        150 => happyShift(p, action_33, j),
        153 => happyShift(p, action_34, j),
        164 => happyShift(p, action_35, j),
        184 => happyShift(p, action_36, j),
        204 => happyShift(p, action_37, j),
        220 => happyShift(p, action_38, j),
        233 => happyShift(p, action_39, j),
        234 => happyShift(p, action_40, j),
        235 => happyShift(p, action_41, j),
        236 => happyShift(p, action_42, j),
        237 => happyShift(p, action_43, j),
        240 => happyShift(p, action_44, j),
        241 => happyShift(p, action_45, j),
        242 => happyShift(p, action_46, j),
        243 => happyShift(p, action_47, j),
        244 => happyShift(p, action_48, j),
        245 => happyShift(p, action_49, j),
        100 => happyGoto(p, action_6, j),
        104 => happyGoto(p, action_7, j),
        106 => happyGoto(p, action_8, j),
        107 => happyGoto(p, action_9, j),
        108 => happyGoto(p, action_10, j),
        109 => happyGoto(p, action_11, j),
        110 => happyGoto(p, action_12, j),
        111 => happyGoto(p, action_13, j),
        112 => happyGoto(p, action_14, j),
        113 => happyGoto(p, action_15, j),
        114 => happyGoto(p, action_16, j),
        115 => happyGoto(p, action_17, j),
        116 => happyGoto(p, action_18, j),
        117 => happyGoto(p, action_19, j),
        118 => happyGoto(p, action_20, j),
        119 => happyGoto(p, action_21, j),
        120 => happyGoto(p, action_22, j),
        122 => happyGoto(p, action_912, j),
        127 => happyGoto(p, action_24, j),
        128 => happyGoto(p, action_25, j),
        _ => happyFail(p, j)
    }
}

fn action_871(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        140 => happyShift(p, action_822, j),
        236 => happyShift(p, action_42, j),
        30 => happyGoto(p, action_911, j),
        128 => happyGoto(p, action_821, j),
        _ => happyFail(p, j)
    }
}

fn action_872(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        180 => happyShift(p, action_910, j),
        _ => happyFail(p, j)
    }
}

fn action_873(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        140 => happyShift(p, action_822, j),
        236 => happyShift(p, action_42, j),
        28 => happyGoto(p, action_909, j),
        29 => happyGoto(p, action_819, j),
        30 => happyGoto(p, action_820, j),
        128 => happyGoto(p, action_821, j),
        _ => happyReduce_76(p, j)
    }
}

fn action_874(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_62(p, j)
    }
}

fn action_875(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        139 => happyShift(p, action_908, j),
        _ => happyFail(p, j)
    }
}

fn action_876(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        139 => happyShift(p, action_907, j),
        _ => happyFail(p, j)
    }
}

fn action_877(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_59(p, j)
    }
}

fn action_878(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_251(p, j)
    }
}

fn action_879(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_204(p, j)
    }
}

fn action_880(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_26, j),
        144 => happyShift(p, action_27, j),
        145 => happyShift(p, action_28, j),
        146 => happyShift(p, action_29, j),
        147 => happyShift(p, action_30, j),
        148 => happyShift(p, action_31, j),
        149 => happyShift(p, action_32, j),
        150 => happyShift(p, action_33, j),
        153 => happyShift(p, action_34, j),
        164 => happyShift(p, action_35, j),
        184 => happyShift(p, action_36, j),
        204 => happyShift(p, action_37, j),
        220 => happyShift(p, action_38, j),
        233 => happyShift(p, action_39, j),
        234 => happyShift(p, action_40, j),
        235 => happyShift(p, action_41, j),
        236 => happyShift(p, action_42, j),
        237 => happyShift(p, action_43, j),
        240 => happyShift(p, action_44, j),
        241 => happyShift(p, action_45, j),
        242 => happyShift(p, action_46, j),
        243 => happyShift(p, action_47, j),
        244 => happyShift(p, action_48, j),
        245 => happyShift(p, action_49, j),
        100 => happyGoto(p, action_6, j),
        104 => happyGoto(p, action_7, j),
        106 => happyGoto(p, action_259, j),
        107 => happyGoto(p, action_9, j),
        108 => happyGoto(p, action_10, j),
        109 => happyGoto(p, action_11, j),
        110 => happyGoto(p, action_12, j),
        111 => happyGoto(p, action_13, j),
        112 => happyGoto(p, action_14, j),
        113 => happyGoto(p, action_15, j),
        114 => happyGoto(p, action_16, j),
        115 => happyGoto(p, action_17, j),
        116 => happyGoto(p, action_18, j),
        117 => happyGoto(p, action_19, j),
        118 => happyGoto(p, action_20, j),
        119 => happyGoto(p, action_260, j),
        126 => happyGoto(p, action_906, j),
        127 => happyGoto(p, action_24, j),
        128 => happyGoto(p, action_25, j),
        _ => happyFail(p, j)
    }
}

fn action_881(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_200(p, j)
    }
}

fn action_882(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_475, j),
        150 => happyShift(p, action_476, j),
        167 => happyShift(p, action_795, j),
        237 => happyShift(p, action_140, j),
        60 => happyGoto(p, action_905, j),
        75 => happyGoto(p, action_794, j),
        76 => happyGoto(p, action_102, j),
        77 => happyGoto(p, action_103, j),
        78 => happyGoto(p, action_481, j),
        _ => happyFail(p, j)
    }
}

fn action_883(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_524, j),
        150 => happyShift(p, action_525, j),
        167 => happyShift(p, action_804, j),
        237 => happyShift(p, action_140, j),
        238 => happyShift(p, action_229, j),
        59 => happyGoto(p, action_904, j),
        66 => happyGoto(p, action_803, j),
        68 => happyGoto(p, action_219, j),
        69 => happyGoto(p, action_220, j),
        70 => happyGoto(p, action_221, j),
        71 => happyGoto(p, action_222, j),
        72 => happyGoto(p, action_223, j),
        73 => happyGoto(p, action_224, j),
        75 => happyGoto(p, action_523, j),
        76 => happyGoto(p, action_102, j),
        77 => happyGoto(p, action_103, j),
        78 => happyGoto(p, action_481, j),
        _ => happyFail(p, j)
    }
}

fn action_884(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        239 => happyShift(p, action_142, j),
        134 => happyGoto(p, action_887, j),
        _ => happyReduce_197(p, j)
    }
}

fn action_885(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_207(p, j)
    }
}

fn action_886(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_26, j),
        144 => happyShift(p, action_27, j),
        145 => happyShift(p, action_28, j),
        146 => happyShift(p, action_29, j),
        147 => happyShift(p, action_30, j),
        148 => happyShift(p, action_31, j),
        149 => happyShift(p, action_32, j),
        150 => happyShift(p, action_33, j),
        153 => happyShift(p, action_34, j),
        164 => happyShift(p, action_35, j),
        184 => happyShift(p, action_36, j),
        204 => happyShift(p, action_37, j),
        220 => happyShift(p, action_38, j),
        233 => happyShift(p, action_39, j),
        234 => happyShift(p, action_40, j),
        235 => happyShift(p, action_41, j),
        236 => happyShift(p, action_42, j),
        237 => happyShift(p, action_43, j),
        240 => happyShift(p, action_44, j),
        241 => happyShift(p, action_45, j),
        242 => happyShift(p, action_46, j),
        243 => happyShift(p, action_47, j),
        244 => happyShift(p, action_48, j),
        245 => happyShift(p, action_49, j),
        100 => happyGoto(p, action_6, j),
        104 => happyGoto(p, action_7, j),
        106 => happyGoto(p, action_259, j),
        107 => happyGoto(p, action_9, j),
        108 => happyGoto(p, action_10, j),
        109 => happyGoto(p, action_11, j),
        110 => happyGoto(p, action_12, j),
        111 => happyGoto(p, action_13, j),
        112 => happyGoto(p, action_14, j),
        113 => happyGoto(p, action_15, j),
        114 => happyGoto(p, action_16, j),
        115 => happyGoto(p, action_17, j),
        116 => happyGoto(p, action_18, j),
        117 => happyGoto(p, action_19, j),
        118 => happyGoto(p, action_20, j),
        119 => happyGoto(p, action_260, j),
        126 => happyGoto(p, action_903, j),
        127 => happyGoto(p, action_24, j),
        128 => happyGoto(p, action_25, j),
        _ => happyFail(p, j)
    }
}

fn action_887(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_209(p, j)
    }
}

fn action_888(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_325(p, j)
    }
}

fn action_889(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        141 => happyShift(p, action_902, j),
        _ => happyFail(p, j)
    }
}

fn action_890(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_320(p, j)
    }
}

fn action_891(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_213(p, j)
    }
}

fn action_892(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_219(p, j)
    }
}

fn action_893(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_89(p, j)
    }
}

fn action_894(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_481(p, j)
    }
}

fn action_895(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_26, j),
        144 => happyShift(p, action_27, j),
        145 => happyShift(p, action_28, j),
        146 => happyShift(p, action_29, j),
        147 => happyShift(p, action_30, j),
        148 => happyShift(p, action_31, j),
        149 => happyShift(p, action_32, j),
        150 => happyShift(p, action_33, j),
        153 => happyShift(p, action_34, j),
        164 => happyShift(p, action_35, j),
        184 => happyShift(p, action_36, j),
        204 => happyShift(p, action_37, j),
        220 => happyShift(p, action_38, j),
        233 => happyShift(p, action_39, j),
        234 => happyShift(p, action_40, j),
        235 => happyShift(p, action_41, j),
        236 => happyShift(p, action_42, j),
        237 => happyShift(p, action_43, j),
        240 => happyShift(p, action_44, j),
        241 => happyShift(p, action_45, j),
        242 => happyShift(p, action_46, j),
        243 => happyShift(p, action_47, j),
        244 => happyShift(p, action_48, j),
        245 => happyShift(p, action_49, j),
        100 => happyGoto(p, action_6, j),
        104 => happyGoto(p, action_7, j),
        106 => happyGoto(p, action_900, j),
        107 => happyGoto(p, action_9, j),
        108 => happyGoto(p, action_10, j),
        109 => happyGoto(p, action_11, j),
        110 => happyGoto(p, action_12, j),
        111 => happyGoto(p, action_13, j),
        112 => happyGoto(p, action_14, j),
        113 => happyGoto(p, action_15, j),
        114 => happyGoto(p, action_16, j),
        115 => happyGoto(p, action_17, j),
        116 => happyGoto(p, action_18, j),
        117 => happyGoto(p, action_19, j),
        118 => happyGoto(p, action_20, j),
        119 => happyGoto(p, action_260, j),
        126 => happyGoto(p, action_901, j),
        127 => happyGoto(p, action_24, j),
        128 => happyGoto(p, action_25, j),
        _ => happyFail(p, j)
    }
}

fn action_896(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_272, j),
        144 => happyShift(p, action_27, j),
        145 => happyShift(p, action_28, j),
        146 => happyShift(p, action_29, j),
        147 => happyShift(p, action_30, j),
        148 => happyShift(p, action_31, j),
        149 => happyShift(p, action_32, j),
        150 => happyShift(p, action_33, j),
        153 => happyShift(p, action_34, j),
        164 => happyShift(p, action_35, j),
        184 => happyShift(p, action_36, j),
        204 => happyShift(p, action_37, j),
        220 => happyShift(p, action_38, j),
        233 => happyShift(p, action_39, j),
        234 => happyShift(p, action_40, j),
        235 => happyShift(p, action_41, j),
        236 => happyShift(p, action_42, j),
        237 => happyShift(p, action_43, j),
        240 => happyShift(p, action_44, j),
        241 => happyShift(p, action_45, j),
        242 => happyShift(p, action_46, j),
        243 => happyShift(p, action_47, j),
        244 => happyShift(p, action_48, j),
        245 => happyShift(p, action_49, j),
        246 => happyShift(p, action_899, j),
        100 => happyGoto(p, action_6, j),
        104 => happyGoto(p, action_7, j),
        106 => happyGoto(p, action_897, j),
        107 => happyGoto(p, action_9, j),
        127 => happyGoto(p, action_24, j),
        128 => happyGoto(p, action_25, j),
        130 => happyGoto(p, action_898, j),
        _ => happyFail(p, j)
    }
}

fn action_897(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_485(p, j)
    }
}

fn action_898(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_484(p, j)
    }
}

fn action_899(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_468(p, j)
    }
}

fn action_900(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        168 => happyShift(p, action_316, j),
        169 => happyShift(p, action_317, j),
        170 => happyShift(p, action_318, j),
        171 => happyShift(p, action_319, j),
        172 => happyShift(p, action_320, j),
        173 => happyShift(p, action_321, j),
        174 => happyShift(p, action_322, j),
        175 => happyShift(p, action_323, j),
        176 => happyShift(p, action_324, j),
        177 => happyShift(p, action_325, j),
        178 => happyShift(p, action_326, j),
        121 => happyGoto(p, action_928, j),
        _ => happyReduce_406(p, j)
    }
}

fn action_901(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_486(p, j)
    }
}

fn action_902(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_321(p, j)
    }
}

fn action_903(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_208(p, j)
    }
}

fn action_904(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        239 => happyShift(p, action_142, j),
        132 => happyGoto(p, action_927, j),
        133 => happyGoto(p, action_150, j),
        134 => happyGoto(p, action_110, j),
        _ => happyReduce_471(p, j)
    }
}

fn action_905(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        239 => happyShift(p, action_142, j),
        134 => happyGoto(p, action_887, j),
        _ => happyReduce_199(p, j)
    }
}

fn action_906(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_205(p, j)
    }
}

fn action_907(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_26, j),
        144 => happyShift(p, action_27, j),
        145 => happyShift(p, action_28, j),
        146 => happyShift(p, action_29, j),
        147 => happyShift(p, action_30, j),
        148 => happyShift(p, action_31, j),
        149 => happyShift(p, action_32, j),
        150 => happyShift(p, action_33, j),
        153 => happyShift(p, action_34, j),
        164 => happyShift(p, action_35, j),
        180 => happyShift(p, action_60, j),
        181 => happyShift(p, action_61, j),
        184 => happyShift(p, action_36, j),
        187 => happyShift(p, action_62, j),
        189 => happyShift(p, action_63, j),
        191 => happyShift(p, action_64, j),
        194 => happyShift(p, action_65, j),
        196 => happyShift(p, action_66, j),
        197 => happyShift(p, action_67, j),
        203 => happyShift(p, action_68, j),
        204 => happyShift(p, action_37, j),
        205 => happyShift(p, action_69, j),
        206 => happyShift(p, action_70, j),
        217 => happyShift(p, action_71, j),
        220 => happyShift(p, action_38, j),
        224 => happyShift(p, action_72, j),
        232 => happyShift(p, action_73, j),
        233 => happyShift(p, action_39, j),
        234 => happyShift(p, action_40, j),
        235 => happyShift(p, action_41, j),
        236 => happyShift(p, action_42, j),
        237 => happyShift(p, action_74, j),
        238 => happyShift(p, action_75, j),
        240 => happyShift(p, action_44, j),
        241 => happyShift(p, action_45, j),
        242 => happyShift(p, action_46, j),
        243 => happyShift(p, action_47, j),
        244 => happyShift(p, action_48, j),
        245 => happyShift(p, action_49, j),
        12 => happyGoto(p, action_926, j),
        13 => happyGoto(p, action_51, j),
        14 => happyGoto(p, action_52, j),
        22 => happyGoto(p, action_53, j),
        23 => happyGoto(p, action_54, j),
        24 => happyGoto(p, action_55, j),
        25 => happyGoto(p, action_56, j),
        26 => happyGoto(p, action_57, j),
        100 => happyGoto(p, action_6, j),
        104 => happyGoto(p, action_7, j),
        106 => happyGoto(p, action_8, j),
        107 => happyGoto(p, action_9, j),
        108 => happyGoto(p, action_10, j),
        109 => happyGoto(p, action_11, j),
        110 => happyGoto(p, action_12, j),
        111 => happyGoto(p, action_13, j),
        112 => happyGoto(p, action_14, j),
        113 => happyGoto(p, action_15, j),
        114 => happyGoto(p, action_16, j),
        115 => happyGoto(p, action_17, j),
        116 => happyGoto(p, action_18, j),
        117 => happyGoto(p, action_19, j),
        118 => happyGoto(p, action_20, j),
        119 => happyGoto(p, action_21, j),
        120 => happyGoto(p, action_22, j),
        122 => happyGoto(p, action_58, j),
        127 => happyGoto(p, action_24, j),
        128 => happyGoto(p, action_25, j),
        131 => happyGoto(p, action_59, j),
        _ => happyFail(p, j)
    }
}

fn action_908(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_26, j),
        144 => happyShift(p, action_27, j),
        145 => happyShift(p, action_28, j),
        146 => happyShift(p, action_29, j),
        147 => happyShift(p, action_30, j),
        148 => happyShift(p, action_31, j),
        149 => happyShift(p, action_32, j),
        150 => happyShift(p, action_33, j),
        153 => happyShift(p, action_34, j),
        164 => happyShift(p, action_35, j),
        180 => happyShift(p, action_60, j),
        181 => happyShift(p, action_61, j),
        184 => happyShift(p, action_36, j),
        187 => happyShift(p, action_62, j),
        189 => happyShift(p, action_63, j),
        191 => happyShift(p, action_64, j),
        194 => happyShift(p, action_65, j),
        196 => happyShift(p, action_66, j),
        197 => happyShift(p, action_67, j),
        203 => happyShift(p, action_68, j),
        204 => happyShift(p, action_37, j),
        205 => happyShift(p, action_69, j),
        206 => happyShift(p, action_70, j),
        217 => happyShift(p, action_71, j),
        220 => happyShift(p, action_38, j),
        224 => happyShift(p, action_72, j),
        232 => happyShift(p, action_73, j),
        233 => happyShift(p, action_39, j),
        234 => happyShift(p, action_40, j),
        235 => happyShift(p, action_41, j),
        236 => happyShift(p, action_42, j),
        237 => happyShift(p, action_74, j),
        238 => happyShift(p, action_75, j),
        240 => happyShift(p, action_44, j),
        241 => happyShift(p, action_45, j),
        242 => happyShift(p, action_46, j),
        243 => happyShift(p, action_47, j),
        244 => happyShift(p, action_48, j),
        245 => happyShift(p, action_49, j),
        12 => happyGoto(p, action_925, j),
        13 => happyGoto(p, action_51, j),
        14 => happyGoto(p, action_52, j),
        22 => happyGoto(p, action_53, j),
        23 => happyGoto(p, action_54, j),
        24 => happyGoto(p, action_55, j),
        25 => happyGoto(p, action_56, j),
        26 => happyGoto(p, action_57, j),
        100 => happyGoto(p, action_6, j),
        104 => happyGoto(p, action_7, j),
        106 => happyGoto(p, action_8, j),
        107 => happyGoto(p, action_9, j),
        108 => happyGoto(p, action_10, j),
        109 => happyGoto(p, action_11, j),
        110 => happyGoto(p, action_12, j),
        111 => happyGoto(p, action_13, j),
        112 => happyGoto(p, action_14, j),
        113 => happyGoto(p, action_15, j),
        114 => happyGoto(p, action_16, j),
        115 => happyGoto(p, action_17, j),
        116 => happyGoto(p, action_18, j),
        117 => happyGoto(p, action_19, j),
        118 => happyGoto(p, action_20, j),
        119 => happyGoto(p, action_21, j),
        120 => happyGoto(p, action_22, j),
        122 => happyGoto(p, action_58, j),
        127 => happyGoto(p, action_24, j),
        128 => happyGoto(p, action_25, j),
        131 => happyGoto(p, action_59, j),
        _ => happyFail(p, j)
    }
}

fn action_909(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        139 => happyShift(p, action_923, j),
        167 => happyShift(p, action_924, j),
        _ => happyFail(p, j)
    }
}

fn action_910(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_71(p, j)
    }
}

fn action_911(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_79(p, j)
    }
}

fn action_912(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        139 => happyShift(p, action_922, j),
        _ => happyFail(p, j)
    }
}

fn action_913(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        236 => happyShift(p, action_42, j),
        128 => happyGoto(p, action_921, j),
        _ => happyFail(p, j)
    }
}

fn action_914(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        236 => happyShift(p, action_42, j),
        128 => happyGoto(p, action_920, j),
        _ => happyFail(p, j)
    }
}

fn action_915(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_375(p, j)
    }
}

fn action_916(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_343(p, j)
    }
}

fn action_917(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        141 => happyShift(p, action_919, j),
        _ => happyFail(p, j)
    }
}

fn action_918(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_350(p, j)
    }
}

fn action_919(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_359(p, j)
    }
}

fn action_920(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_936, j),
        _ => happyFail(p, j)
    }
}

fn action_921(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_935, j),
        _ => happyFail(p, j)
    }
}

fn action_922(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_80(p, j)
    }
}

fn action_923(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        180 => happyShift(p, action_934, j),
        _ => happyFail(p, j)
    }
}

fn action_924(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        236 => happyShift(p, action_42, j),
        31 => happyGoto(p, action_932, j),
        128 => happyGoto(p, action_933, j),
        _ => happyFail(p, j)
    }
}

fn action_925(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        16 => happyGoto(p, action_931, j),
        _ => happyReduce_41(p, j)
    }
}

fn action_926(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_63(p, j)
    }
}

fn action_927(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_201(p, j)
    }
}

fn action_928(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_272, j),
        144 => happyShift(p, action_27, j),
        145 => happyShift(p, action_28, j),
        146 => happyShift(p, action_29, j),
        147 => happyShift(p, action_30, j),
        148 => happyShift(p, action_31, j),
        149 => happyShift(p, action_32, j),
        150 => happyShift(p, action_33, j),
        153 => happyShift(p, action_34, j),
        164 => happyShift(p, action_35, j),
        184 => happyShift(p, action_36, j),
        204 => happyShift(p, action_37, j),
        220 => happyShift(p, action_38, j),
        233 => happyShift(p, action_39, j),
        234 => happyShift(p, action_40, j),
        235 => happyShift(p, action_41, j),
        236 => happyShift(p, action_42, j),
        237 => happyShift(p, action_43, j),
        240 => happyShift(p, action_44, j),
        241 => happyShift(p, action_45, j),
        242 => happyShift(p, action_46, j),
        243 => happyShift(p, action_47, j),
        244 => happyShift(p, action_48, j),
        245 => happyShift(p, action_49, j),
        246 => happyShift(p, action_899, j),
        100 => happyGoto(p, action_6, j),
        104 => happyGoto(p, action_7, j),
        106 => happyGoto(p, action_929, j),
        107 => happyGoto(p, action_9, j),
        127 => happyGoto(p, action_24, j),
        128 => happyGoto(p, action_25, j),
        130 => happyGoto(p, action_930, j),
        _ => happyFail(p, j)
    }
}

fn action_929(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_487(p, j)
    }
}

fn action_930(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_488(p, j)
    }
}

fn action_931(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_64(p, j)
    }
}

fn action_932(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        139 => happyShift(p, action_939, j),
        179 => happyShift(p, action_940, j),
        _ => happyFail(p, j)
    }
}

fn action_933(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_83(p, j)
    }
}

fn action_934(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_72(p, j)
    }
}

fn action_935(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_26, j),
        144 => happyShift(p, action_27, j),
        145 => happyShift(p, action_28, j),
        146 => happyShift(p, action_29, j),
        147 => happyShift(p, action_30, j),
        148 => happyShift(p, action_31, j),
        149 => happyShift(p, action_32, j),
        150 => happyShift(p, action_33, j),
        153 => happyShift(p, action_34, j),
        164 => happyShift(p, action_35, j),
        184 => happyShift(p, action_36, j),
        204 => happyShift(p, action_37, j),
        220 => happyShift(p, action_38, j),
        233 => happyShift(p, action_39, j),
        234 => happyShift(p, action_40, j),
        235 => happyShift(p, action_41, j),
        236 => happyShift(p, action_42, j),
        237 => happyShift(p, action_43, j),
        240 => happyShift(p, action_44, j),
        241 => happyShift(p, action_45, j),
        242 => happyShift(p, action_46, j),
        243 => happyShift(p, action_47, j),
        244 => happyShift(p, action_48, j),
        245 => happyShift(p, action_49, j),
        100 => happyGoto(p, action_6, j),
        104 => happyGoto(p, action_7, j),
        106 => happyGoto(p, action_8, j),
        107 => happyGoto(p, action_9, j),
        108 => happyGoto(p, action_10, j),
        109 => happyGoto(p, action_11, j),
        110 => happyGoto(p, action_12, j),
        111 => happyGoto(p, action_13, j),
        112 => happyGoto(p, action_14, j),
        113 => happyGoto(p, action_15, j),
        114 => happyGoto(p, action_16, j),
        115 => happyGoto(p, action_17, j),
        116 => happyGoto(p, action_18, j),
        117 => happyGoto(p, action_19, j),
        118 => happyGoto(p, action_20, j),
        119 => happyGoto(p, action_21, j),
        120 => happyGoto(p, action_22, j),
        122 => happyGoto(p, action_938, j),
        127 => happyGoto(p, action_24, j),
        128 => happyGoto(p, action_25, j),
        _ => happyFail(p, j)
    }
}

fn action_936(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        138 => happyShift(p, action_26, j),
        144 => happyShift(p, action_27, j),
        145 => happyShift(p, action_28, j),
        146 => happyShift(p, action_29, j),
        147 => happyShift(p, action_30, j),
        148 => happyShift(p, action_31, j),
        149 => happyShift(p, action_32, j),
        150 => happyShift(p, action_33, j),
        153 => happyShift(p, action_34, j),
        164 => happyShift(p, action_35, j),
        184 => happyShift(p, action_36, j),
        204 => happyShift(p, action_37, j),
        220 => happyShift(p, action_38, j),
        233 => happyShift(p, action_39, j),
        234 => happyShift(p, action_40, j),
        235 => happyShift(p, action_41, j),
        236 => happyShift(p, action_42, j),
        237 => happyShift(p, action_43, j),
        240 => happyShift(p, action_44, j),
        241 => happyShift(p, action_45, j),
        242 => happyShift(p, action_46, j),
        243 => happyShift(p, action_47, j),
        244 => happyShift(p, action_48, j),
        245 => happyShift(p, action_49, j),
        100 => happyGoto(p, action_6, j),
        104 => happyGoto(p, action_7, j),
        106 => happyGoto(p, action_8, j),
        107 => happyGoto(p, action_9, j),
        108 => happyGoto(p, action_10, j),
        109 => happyGoto(p, action_11, j),
        110 => happyGoto(p, action_12, j),
        111 => happyGoto(p, action_13, j),
        112 => happyGoto(p, action_14, j),
        113 => happyGoto(p, action_15, j),
        114 => happyGoto(p, action_16, j),
        115 => happyGoto(p, action_17, j),
        116 => happyGoto(p, action_18, j),
        117 => happyGoto(p, action_19, j),
        118 => happyGoto(p, action_20, j),
        119 => happyGoto(p, action_21, j),
        120 => happyGoto(p, action_22, j),
        122 => happyGoto(p, action_937, j),
        127 => happyGoto(p, action_24, j),
        128 => happyGoto(p, action_25, j),
        _ => happyFail(p, j)
    }
}

fn action_937(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        139 => happyShift(p, action_944, j),
        _ => happyFail(p, j)
    }
}

fn action_938(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        139 => happyShift(p, action_943, j),
        _ => happyFail(p, j)
    }
}

fn action_939(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        180 => happyShift(p, action_942, j),
        _ => happyFail(p, j)
    }
}

fn action_940(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        236 => happyShift(p, action_42, j),
        128 => happyGoto(p, action_941, j),
        _ => happyFail(p, j)
    }
}

fn action_941(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_84(p, j)
    }
}

fn action_942(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_73(p, j)
    }
}

fn action_943(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_82(p, j)
    }
}

fn action_944(p: &mut Parser, i: isize, j: isize) -> Monad<Cont> {
    match i {
        _ => happyReduce_81(p, j)
    }
}

fn happyReduce_4(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 1, 7, happyReduction_4, i)
}

fn happyReduction_4(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap()) {
        HappyAbsSyn8(happy_var_1) => {
                      let decls = reverse(happy_var_1);
                      if decls.len() == 0 {
                          let name = p.getNewName();
                          let pos = p.getPos();
                          let nodeinfo = NodeInfo::new(pos.clone(), (pos, 0), name);
                          Ok(CTranslationUnit(decls, nodeinfo))
                      } else {
                          let d = decls[0].clone();
                          withNodeInfo(p, d, partial_1!(CTranslationUnit, decls))
                      }
        }.map(HappyAbsSyn7),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_5(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_0(p, 8, happyReduction_5(), i)
}

fn happyReduction_5() -> HappyAbsSyn {
    HappyAbsSyn8(empty())
}


fn happyReduce_6(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_2(p, 8, happyReduction_6, i)
}

fn happyReduction_6(happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_2, happy_x_1) {
        (_, HappyAbsSyn8(happy_var_1)) => HappyAbsSyn8(happy_var_1),
        _ => notHappyAtAll()
    }
}


fn happyReduce_7(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_2(p, 8, happyReduction_7, i)
}

fn happyReduction_7(happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_2, happy_x_1) {
        (HappyAbsSyn9(happy_var_2), HappyAbsSyn8(happy_var_1)) => HappyAbsSyn8(snoc(happy_var_1, happy_var_2)),
        _ => notHappyAtAll()
    }
}


fn happyReduce_8(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_1(p, 9, happyReduction_8, i)
}

fn happyReduction_8(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn10(happy_var_1) => HappyAbsSyn9(CFDefExt(happy_var_1)),
        _ => notHappyAtAll()
    }
}


fn happyReduce_9(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_1(p, 9, happyReduction_9, i)
}

fn happyReduction_9(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn32(happy_var_1) => HappyAbsSyn9(CDeclExt(happy_var_1)),
        _ => notHappyAtAll()
    }
}


fn happyReduce_10(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_2(p, 9, happyReduction_10, i)
}

fn happyReduction_10(happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_2, happy_x_1) {
        (HappyAbsSyn9(happy_var_2), _) => HappyAbsSyn9(happy_var_2),
        _ => notHappyAtAll()
    }
}


fn happyReduce_11(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 5, 9, happyReduction_11, i)
}

fn happyReduction_11(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, _, HappyAbsSyn128(happy_var_3), _, HappyTerminal(happy_var_1)) => { withNodeInfo(p, happy_var_1.clone(), partial_1!(CAsmExt, happy_var_3))
        }.map(HappyAbsSyn9),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_12(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 2, 10, happyReduction_12, i)
}

fn happyReduction_12(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn12(happy_var_2), HappyAbsSyn11(happy_var_1)) => { p.leaveScope(); withNodeInfo(p, happy_var_1.clone(), partial_1!(
            CFunctionDef, vec![], happy_var_1, vec![], happy_var_2))
        }.map(HappyAbsSyn10),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_13(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 3, 10, happyReduction_13, i)
}

fn happyReduction_13(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn12(happy_var_3), HappyAbsSyn11(happy_var_2), HappyAbsSyn132(happy_var_1)) => { p.leaveScope(); withNodeInfo(p, happy_var_1.clone(), partial_1!(
            CFunctionDef, liftCAttrs(happy_var_1), happy_var_2, vec![], happy_var_3))
        }.map(HappyAbsSyn10),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_14(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 3, 10, happyReduction_14, i)
}

fn happyReduction_14(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn12(happy_var_3), HappyAbsSyn11(happy_var_2), HappyAbsSyn37(happy_var_1)) => { p.leaveScope(); withNodeInfo(p, happy_var_1.clone(), partial_1!(
            CFunctionDef, happy_var_1, happy_var_2, vec![], happy_var_3))
        }.map(HappyAbsSyn10),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_15(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 3, 10, happyReduction_15, i)
}

fn happyReduction_15(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn12(happy_var_3), HappyAbsSyn11(happy_var_2), HappyAbsSyn37(happy_var_1)) => { p.leaveScope(); withNodeInfo(p, happy_var_1.clone(), partial_1!(
            CFunctionDef, happy_var_1, happy_var_2, vec![], happy_var_3))
        }.map(HappyAbsSyn10),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_16(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 3, 10, happyReduction_16, i)
}

fn happyReduction_16(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn12(happy_var_3), HappyAbsSyn11(happy_var_2), HappyAbsSyn38(happy_var_1)) => { p.leaveScope(); withNodeInfo(p, happy_var_1.clone(), partial_1!(
            CFunctionDef, reverse(happy_var_1), happy_var_2, vec![], happy_var_3))
        }.map(HappyAbsSyn10),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_17(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 3, 10, happyReduction_17, i)
}

fn happyReduction_17(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn12(happy_var_3), HappyAbsSyn11(happy_var_2), HappyAbsSyn65(happy_var_1)) => { p.leaveScope(); withNodeInfo(p, happy_var_1.clone(), partial_1!(
            CFunctionDef, liftTypeQuals(happy_var_1), happy_var_2, vec![], happy_var_3))
        }.map(HappyAbsSyn10),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_18(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 4, 10, happyReduction_18, i)
}

fn happyReduction_18(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn12(happy_var_4), HappyAbsSyn11(happy_var_3), HappyAbsSyn132(happy_var_2), HappyAbsSyn65(happy_var_1)) => { p.leaveScope(); withNodeInfo(p, happy_var_1.clone(), partial_1!(
            CFunctionDef, __op_addadd(liftTypeQuals(happy_var_1), liftCAttrs(happy_var_2)), happy_var_3, vec![], happy_var_4))
        }.map(HappyAbsSyn10),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_19(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 3, 10, happyReduction_19, i)
}

fn happyReduction_19(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn12(happy_var_3), HappyAbsSyn33(happy_var_2), HappyAbsSyn11(happy_var_1)) => { withNodeInfo(p, happy_var_1.clone(), partial_1!(CFunctionDef, vec![], happy_var_1, reverse(happy_var_2), happy_var_3))
        }.map(HappyAbsSyn10),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_20(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 4, 10, happyReduction_20, i)
}

fn happyReduction_20(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn12(happy_var_4), HappyAbsSyn33(happy_var_3), HappyAbsSyn11(happy_var_2), HappyAbsSyn132(happy_var_1)) => { withNodeInfo(p, happy_var_2.clone(), partial_1!(CFunctionDef, liftCAttrs(happy_var_1), happy_var_2, reverse(happy_var_3), happy_var_4))
        }.map(HappyAbsSyn10),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_21(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 4, 10, happyReduction_21, i)
}

fn happyReduction_21(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn12(happy_var_4), HappyAbsSyn33(happy_var_3), HappyAbsSyn11(happy_var_2), HappyAbsSyn37(happy_var_1)) => { withNodeInfo(p, happy_var_1.clone(), partial_1!(CFunctionDef, happy_var_1, happy_var_2, reverse(happy_var_3), happy_var_4))
        }.map(HappyAbsSyn10),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_22(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 4, 10, happyReduction_22, i)
}

fn happyReduction_22(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn12(happy_var_4), HappyAbsSyn33(happy_var_3), HappyAbsSyn11(happy_var_2), HappyAbsSyn37(happy_var_1)) => { withNodeInfo(p, happy_var_1.clone(), partial_1!(CFunctionDef, happy_var_1, happy_var_2, reverse(happy_var_3), happy_var_4))
        }.map(HappyAbsSyn10),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_23(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 4, 10, happyReduction_23, i)
}

fn happyReduction_23(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn12(happy_var_4), HappyAbsSyn33(happy_var_3), HappyAbsSyn11(happy_var_2), HappyAbsSyn38(happy_var_1)) => { withNodeInfo(p, happy_var_1.clone(), partial_1!(CFunctionDef, reverse(happy_var_1), happy_var_2, reverse(happy_var_3), happy_var_4))
        }.map(HappyAbsSyn10),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_24(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 4, 10, happyReduction_24, i)
}

fn happyReduction_24(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn12(happy_var_4), HappyAbsSyn33(happy_var_3), HappyAbsSyn11(happy_var_2), HappyAbsSyn65(happy_var_1)) => { withNodeInfo(p, happy_var_1.clone(), partial_1!(CFunctionDef, liftTypeQuals(happy_var_1), happy_var_2, reverse(happy_var_3), happy_var_4))
        }.map(HappyAbsSyn10),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_25(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 5, 10, happyReduction_25, i)
}

fn happyReduction_25(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn12(happy_var_5), HappyAbsSyn33(happy_var_4), HappyAbsSyn11(happy_var_3), HappyAbsSyn132(happy_var_2), HappyAbsSyn65(happy_var_1)) => { withNodeInfo(p, happy_var_1.clone(), partial_1!(
            CFunctionDef, __op_addadd(liftTypeQuals(happy_var_1), liftCAttrs(happy_var_2)), happy_var_3, reverse(happy_var_4), happy_var_5))
        }.map(HappyAbsSyn10),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_26(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 1, 11, happyReduction_26, i)
}

fn happyReduction_26(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap()) {
        HappyAbsSyn66(happy_var_1) => {
            let declr = reverseDeclr(happy_var_1);
            p.enterScope();
            doFuncParamDeclIdent(declr.clone())?;
            Ok(declr)
        }.map(HappyAbsSyn11),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_27(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_1(p, 12, happyReduction_27, i)
}

fn happyReduction_27(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn12(happy_var_1) => HappyAbsSyn12(happy_var_1),
        _ => notHappyAtAll()
    }
}


fn happyReduce_28(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_1(p, 12, happyReduction_28, i)
}

fn happyReduction_28(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn12(happy_var_1) => HappyAbsSyn12(happy_var_1),
        _ => notHappyAtAll()
    }
}


fn happyReduce_29(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_1(p, 12, happyReduction_29, i)
}

fn happyReduction_29(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn12(happy_var_1) => HappyAbsSyn12(happy_var_1),
        _ => notHappyAtAll()
    }
}


fn happyReduce_30(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_1(p, 12, happyReduction_30, i)
}

fn happyReduction_30(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn12(happy_var_1) => HappyAbsSyn12(happy_var_1),
        _ => notHappyAtAll()
    }
}


fn happyReduce_31(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_1(p, 12, happyReduction_31, i)
}

fn happyReduction_31(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn12(happy_var_1) => HappyAbsSyn12(happy_var_1),
        _ => notHappyAtAll()
    }
}


fn happyReduce_32(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_1(p, 12, happyReduction_32, i)
}

fn happyReduction_32(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn12(happy_var_1) => HappyAbsSyn12(happy_var_1),
        _ => notHappyAtAll()
    }
}


fn happyReduce_33(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 1, 12, happyReduction_33, i)
}

fn happyReduction_33(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap()) {
        HappyAbsSyn26(happy_var_1) => { withNodeInfo(p, happy_var_1.clone(), partial_1!(CAsm, happy_var_1))
        }.map(HappyAbsSyn12),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_34(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 4, 13, happyReduction_34, i)
}

fn happyReduction_34(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn12(happy_var_4), HappyAbsSyn132(happy_var_3), _, HappyAbsSyn131(happy_var_1)) => { withNodeInfo(p, happy_var_1.clone(), partial_1!(CLabel, happy_var_1, box happy_var_4, happy_var_3))
        }.map(HappyAbsSyn12),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_35(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 4, 13, happyReduction_35, i)
}

fn happyReduction_35(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn12(happy_var_4), _, HappyAbsSyn100(happy_var_2), HappyTerminal(happy_var_1)) => { withNodeInfo(p, happy_var_1, partial_1!(CCase, happy_var_2, box happy_var_4))
        }.map(HappyAbsSyn12),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_36(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 3, 13, happyReduction_36, i)
}

fn happyReduction_36(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn12(happy_var_3), _, HappyTerminal(happy_var_1)) => { withNodeInfo(p, happy_var_1, partial_1!(CDefault, box happy_var_3))
        }.map(HappyAbsSyn12),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_37(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 6, 13, happyReduction_37, i)
}

fn happyReduction_37(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn12(happy_var_6), _, HappyAbsSyn100(happy_var_4), _, HappyAbsSyn100(happy_var_2), HappyTerminal(happy_var_1)) => { withNodeInfo(p, happy_var_1, partial_1!(CCases, happy_var_2, happy_var_4, box happy_var_6))
        }.map(HappyAbsSyn12),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_38(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 5, 14, happyReduction_38, i)
}

fn happyReduction_38(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, _, HappyAbsSyn17(happy_var_3), _, HappyTerminal(happy_var_1)) => { withNodeInfo(p, happy_var_1, partial_1!(CCompound, vec![], reverse(happy_var_3)))
        }.map(HappyAbsSyn12),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_39(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 6, 14, happyReduction_39, i)
}

fn happyReduction_39(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, _, HappyAbsSyn17(happy_var_4), HappyAbsSyn21(happy_var_3), _, HappyTerminal(happy_var_1)) => { withNodeInfo(p, happy_var_1, partial_1!(CCompound, reverse(happy_var_3), reverse(happy_var_4)))
        }.map(HappyAbsSyn12),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_40(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 0, 15, happyReduction_40, i)
}

fn happyReduction_40(p: &mut Parser) -> P<HappyAbsSyn> {
    match () {
        () => { Ok(p.enterScope())
        }.map(HappyAbsSyn15),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_41(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 0, 16, happyReduction_41, i)
}

fn happyReduction_41(p: &mut Parser) -> P<HappyAbsSyn> {
    match () {
        () => { Ok(p.leaveScope())
        }.map(HappyAbsSyn15),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_42(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_0(p, 17, happyReduction_42(), i)
}

fn happyReduction_42() -> HappyAbsSyn {
    HappyAbsSyn17(empty())
}


fn happyReduce_43(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_2(p, 17, happyReduction_43, i)
}

fn happyReduction_43(happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_2, happy_x_1) {
        (HappyAbsSyn18(happy_var_2), HappyAbsSyn17(happy_var_1)) => HappyAbsSyn17(snoc(happy_var_1, happy_var_2)),
        _ => notHappyAtAll()
    }
}


fn happyReduce_44(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_1(p, 18, happyReduction_44, i)
}

fn happyReduction_44(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn12(happy_var_1) => HappyAbsSyn18(CBlockStmt(happy_var_1)),
        _ => notHappyAtAll()
    }
}


fn happyReduce_45(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_1(p, 18, happyReduction_45, i)
}

fn happyReduction_45(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn18(happy_var_1) => HappyAbsSyn18(happy_var_1),
        _ => notHappyAtAll()
    }
}


fn happyReduce_46(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_1(p, 19, happyReduction_46, i)
}

fn happyReduction_46(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn32(happy_var_1) => HappyAbsSyn18(CBlockDecl(happy_var_1)),
        _ => notHappyAtAll()
    }
}


fn happyReduce_47(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_1(p, 19, happyReduction_47, i)
}

fn happyReduction_47(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn10(happy_var_1) => HappyAbsSyn18(CNestedFunDef(happy_var_1)),
        _ => notHappyAtAll()
    }
}


fn happyReduce_48(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_2(p, 19, happyReduction_48, i)
}

fn happyReduction_48(happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_2, happy_x_1) {
        (HappyAbsSyn18(happy_var_2), _) => HappyAbsSyn18(happy_var_2),
        _ => notHappyAtAll()
    }
}


fn happyReduce_49(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 3, 20, happyReduction_49, i)
}

fn happyReduction_49(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn12(happy_var_3), HappyAbsSyn11(happy_var_2), HappyAbsSyn37(happy_var_1)) => { p.leaveScope(); withNodeInfo(p, happy_var_1.clone(), partial_1!(
            CFunctionDef, happy_var_1, happy_var_2, vec![], happy_var_3))
        }.map(HappyAbsSyn10),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_50(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 3, 20, happyReduction_50, i)
}

fn happyReduction_50(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn12(happy_var_3), HappyAbsSyn11(happy_var_2), HappyAbsSyn37(happy_var_1)) => { p.leaveScope(); withNodeInfo(p, happy_var_1.clone(), partial_1!(
            CFunctionDef, happy_var_1, happy_var_2, vec![], happy_var_3))
        }.map(HappyAbsSyn10),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_51(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 3, 20, happyReduction_51, i)
}

fn happyReduction_51(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn12(happy_var_3), HappyAbsSyn11(happy_var_2), HappyAbsSyn38(happy_var_1)) => { p.leaveScope(); withNodeInfo(p, happy_var_1.clone(), partial_1!(
            CFunctionDef, reverse(happy_var_1), happy_var_2, vec![], happy_var_3))
        }.map(HappyAbsSyn10),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_52(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 3, 20, happyReduction_52, i)
}

fn happyReduction_52(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn12(happy_var_3), HappyAbsSyn11(happy_var_2), HappyAbsSyn65(happy_var_1)) => { p.leaveScope(); withNodeInfo(p, happy_var_1.clone(), partial_1!(
            CFunctionDef, liftTypeQuals(happy_var_1), happy_var_2, vec![], happy_var_3))
        }.map(HappyAbsSyn10),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_53(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 4, 20, happyReduction_53, i)
}

fn happyReduction_53(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn12(happy_var_4), HappyAbsSyn11(happy_var_3), HappyAbsSyn132(happy_var_2), HappyAbsSyn65(happy_var_1)) => { p.leaveScope(); withNodeInfo(p, happy_var_1.clone(), partial_1!(
            CFunctionDef, __op_addadd(liftTypeQuals(happy_var_1), liftCAttrs(happy_var_2)), happy_var_3, vec![], happy_var_4))
        }.map(HappyAbsSyn10),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_54(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_3(p, 21, happyReduction_54, i)
}

fn happyReduction_54(happy_x_3: HappyAbsSyn, happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_3, happy_x_2, happy_x_1) {
        (_, HappyAbsSyn21(happy_var_2), _) => HappyAbsSyn21(happy_var_2),
        _ => notHappyAtAll()
    }
}


fn happyReduce_55(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyReduce(p, 4, 21, happyReduction_55, i)
}

fn happyReduction_55(p: &mut Parser) {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, HappyAbsSyn21(happy_var_3), _, HappyAbsSyn21(happy_var_1)) => {            p.stack.push(HappyAbsSyn21(rappendr(happy_var_1, happy_var_3))); }
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_56(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 1, 22, happyReduction_56, i)
}

fn happyReduction_56(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap()) {
        HappyTerminal(happy_var_1) => { withNodeInfo(p, happy_var_1, partial_1!(CExpr, None))
        }.map(HappyAbsSyn12),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_57(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 2, 22, happyReduction_57, i)
}

fn happyReduction_57(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, HappyAbsSyn100(happy_var_1)) => { withNodeInfo(p, happy_var_1.clone(), partial_1!(CExpr, Some(happy_var_1)))
        }.map(HappyAbsSyn12),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_58(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 5, 23, happyReduction_58, i)
}

fn happyReduction_58(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn12(happy_var_5), _, HappyAbsSyn100(happy_var_3), _, HappyTerminal(happy_var_1)) => { withNodeInfo(p, happy_var_1, partial_1!(CIf, happy_var_3, box happy_var_5, None))
        }.map(HappyAbsSyn12),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_59(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 7, 23, happyReduction_59, i)
}

fn happyReduction_59(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn12(happy_var_7), _, HappyAbsSyn12(happy_var_5), _, HappyAbsSyn100(happy_var_3), _, HappyTerminal(happy_var_1)) => { withNodeInfo(p, happy_var_1, partial_1!(CIf, happy_var_3, box happy_var_5, Some(box happy_var_7)))
        }.map(HappyAbsSyn12),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_60(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 5, 23, happyReduction_60, i)
}

fn happyReduction_60(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn12(happy_var_5), _, HappyAbsSyn100(happy_var_3), _, HappyTerminal(happy_var_1)) => { withNodeInfo(p, happy_var_1, partial_1!(CSwitch, happy_var_3, box happy_var_5))
        }.map(HappyAbsSyn12),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_61(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 5, 24, happyReduction_61, i)
}

fn happyReduction_61(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn12(happy_var_5), _, HappyAbsSyn100(happy_var_3), _, HappyTerminal(happy_var_1)) => { withNodeInfo(p, happy_var_1, partial_1!(CWhile, happy_var_3, box happy_var_5, false))
        }.map(HappyAbsSyn12),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_62(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 7, 24, happyReduction_62, i)
}

fn happyReduction_62(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, _, HappyAbsSyn100(happy_var_5), _, _, HappyAbsSyn12(happy_var_2), HappyTerminal(happy_var_1)) => { withNodeInfo(p, happy_var_1, partial_1!(CWhile, happy_var_5, box happy_var_2, true))
        }.map(HappyAbsSyn12),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_63(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 9, 24, happyReduction_63, i)
}

fn happyReduction_63(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn12(happy_var_9), _, HappyAbsSyn124(happy_var_7), _, HappyAbsSyn124(happy_var_5), _, HappyAbsSyn124(happy_var_3), _, HappyTerminal(happy_var_1)) => { withNodeInfo(p, happy_var_1, partial_1!(CFor, Left(happy_var_3), happy_var_5, happy_var_7, box happy_var_9))
        }.map(HappyAbsSyn12),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_64(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 10, 24, happyReduction_64, i)
}

fn happyReduction_64(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, HappyAbsSyn12(happy_var_9), _, HappyAbsSyn124(happy_var_7), _, HappyAbsSyn124(happy_var_5), HappyAbsSyn32(happy_var_4), _, _, HappyTerminal(happy_var_1)) => { withNodeInfo(p, happy_var_1, partial_1!(CFor, Right(happy_var_4), happy_var_5, happy_var_7, box happy_var_9))
        }.map(HappyAbsSyn12),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_65(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 3, 25, happyReduction_65, i)
}

fn happyReduction_65(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, HappyAbsSyn131(happy_var_2), HappyTerminal(happy_var_1)) => { withNodeInfo(p, happy_var_1, partial_1!(CGoto, happy_var_2))
        }.map(HappyAbsSyn12),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_66(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 4, 25, happyReduction_66, i)
}

fn happyReduction_66(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, HappyAbsSyn100(happy_var_3), _, HappyTerminal(happy_var_1)) => { withNodeInfo(p, happy_var_1, partial_1!(CGotoPtr, happy_var_3))
        }.map(HappyAbsSyn12),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_67(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 2, 25, happyReduction_67, i)
}

fn happyReduction_67(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, HappyTerminal(happy_var_1)) => { withNodeInfo(p, happy_var_1, partial_1!(CCont))
        }.map(HappyAbsSyn12),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_68(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 2, 25, happyReduction_68, i)
}

fn happyReduction_68(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, HappyTerminal(happy_var_1)) => { withNodeInfo(p, happy_var_1, partial_1!(CBreak))
        }.map(HappyAbsSyn12),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_69(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 3, 25, happyReduction_69, i)
}

fn happyReduction_69(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, HappyAbsSyn124(happy_var_2), HappyTerminal(happy_var_1)) => { withNodeInfo(p, happy_var_1, partial_1!(CReturn, happy_var_2))
        }.map(HappyAbsSyn12),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_70(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 6, 26, happyReduction_70, i)
}

fn happyReduction_70(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, _, HappyAbsSyn128(happy_var_4), _, HappyAbsSyn27(happy_var_2), HappyTerminal(happy_var_1)) => { withNodeInfo(p, happy_var_1, partial_1!(CAssemblyStatement, happy_var_2, happy_var_4, vec![], vec![], vec![]))
        }.map(HappyAbsSyn26),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_71(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 8, 26, happyReduction_71, i)
}

fn happyReduction_71(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, _, HappyAbsSyn28(happy_var_6), _, HappyAbsSyn128(happy_var_4), _, HappyAbsSyn27(happy_var_2), HappyTerminal(happy_var_1)) => { withNodeInfo(p, happy_var_1, partial_1!(CAssemblyStatement, happy_var_2, happy_var_4, happy_var_6, vec![], vec![]))
        }.map(HappyAbsSyn26),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_72(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 10, 26, happyReduction_72, i)
}

fn happyReduction_72(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, _, HappyAbsSyn28(happy_var_8), _, HappyAbsSyn28(happy_var_6), _, HappyAbsSyn128(happy_var_4), _, HappyAbsSyn27(happy_var_2), HappyTerminal(happy_var_1)) => { withNodeInfo(p, happy_var_1, partial_1!(CAssemblyStatement, happy_var_2, happy_var_4, happy_var_6, happy_var_8, vec![]))
        }.map(HappyAbsSyn26),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_73(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 12, 26, happyReduction_73, i)
}

fn happyReduction_73(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, _, HappyAbsSyn31(happy_var_10), _, HappyAbsSyn28(happy_var_8), _, HappyAbsSyn28(happy_var_6), _, HappyAbsSyn128(happy_var_4), _, HappyAbsSyn27(happy_var_2), HappyTerminal(happy_var_1)) => { withNodeInfo(p, happy_var_1, partial_1!(CAssemblyStatement, happy_var_2, happy_var_4, happy_var_6, happy_var_8, reverse(happy_var_10)))
        }.map(HappyAbsSyn26),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_74(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_0(p, 27, happyReduction_74(), i)
}

fn happyReduction_74() -> HappyAbsSyn {
    HappyAbsSyn27(None)
}


fn happyReduce_75(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_1(p, 27, happyReduction_75, i)
}

fn happyReduction_75(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn64(happy_var_1) => HappyAbsSyn27(Some(happy_var_1)),
        _ => notHappyAtAll()
    }
}


fn happyReduce_76(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_0(p, 28, happyReduction_76(), i)
}

fn happyReduction_76() -> HappyAbsSyn {
    HappyAbsSyn28(vec![])
}


fn happyReduce_77(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_1(p, 28, happyReduction_77, i)
}

fn happyReduction_77(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn29(happy_var_1) => HappyAbsSyn28(reverse(happy_var_1)),
        _ => notHappyAtAll()
    }
}


fn happyReduce_78(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_1(p, 29, happyReduction_78, i)
}

fn happyReduction_78(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn30(happy_var_1) => HappyAbsSyn29(singleton(happy_var_1)),
        _ => notHappyAtAll()
    }
}


fn happyReduce_79(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_3(p, 29, happyReduction_79, i)
}

fn happyReduction_79(happy_x_3: HappyAbsSyn, happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_3, happy_x_2, happy_x_1) {
        (HappyAbsSyn30(happy_var_3), _, HappyAbsSyn29(happy_var_1)) => HappyAbsSyn29(snoc(happy_var_1, happy_var_3)),
        _ => notHappyAtAll()
    }
}


fn happyReduce_80(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 4, 30, happyReduction_80, i)
}

fn happyReduction_80(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, HappyAbsSyn100(happy_var_3), _, HappyAbsSyn128(happy_var_1)) => { withNodeInfo(p, happy_var_1.clone(), partial_1!(CAssemblyOperand, None, happy_var_1, happy_var_3))
        }.map(HappyAbsSyn30),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_81(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 7, 30, happyReduction_81, i)
}

fn happyReduction_81(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, HappyAbsSyn100(happy_var_6), _, HappyAbsSyn128(happy_var_4), _, HappyTerminal(CTokIdent(_, happy_var_2)), HappyTerminal(happy_var_1)) => { withNodeInfo(p, happy_var_1, partial_1!(CAssemblyOperand, Some(happy_var_2), happy_var_4, happy_var_6))
        }.map(HappyAbsSyn30),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_82(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 7, 30, happyReduction_82, i)
}

fn happyReduction_82(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, HappyAbsSyn100(happy_var_6), _, HappyAbsSyn128(happy_var_4), _, HappyTerminal(CTokTyIdent(_, happy_var_2)), HappyTerminal(happy_var_1)) => { withNodeInfo(p, happy_var_1, partial_1!(CAssemblyOperand, Some(happy_var_2), happy_var_4, happy_var_6))
        }.map(HappyAbsSyn30),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_83(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_1(p, 31, happyReduction_83, i)
}

fn happyReduction_83(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn128(happy_var_1) => HappyAbsSyn31(singleton(happy_var_1)),
        _ => notHappyAtAll()
    }
}


fn happyReduce_84(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_3(p, 31, happyReduction_84, i)
}

fn happyReduction_84(happy_x_3: HappyAbsSyn, happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_3, happy_x_2, happy_x_1) {
        (HappyAbsSyn128(happy_var_3), _, HappyAbsSyn31(happy_var_1)) => HappyAbsSyn31(snoc(happy_var_1, happy_var_3)),
        _ => notHappyAtAll()
    }
}


fn happyReduce_85(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 2, 32, happyReduction_85, i)
}

fn happyReduction_85(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, HappyAbsSyn38(happy_var_1)) => { withNodeInfo(p, happy_var_1.clone(), partial_1!(CDecl, reverse(happy_var_1), vec![]))
        }.map(HappyAbsSyn32),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_86(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 2, 32, happyReduction_86, i)
}

fn happyReduction_86(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, HappyAbsSyn38(happy_var_1)) => { withNodeInfo(p, happy_var_1.clone(), partial_1!(CDecl, reverse(happy_var_1), vec![]))
        }.map(HappyAbsSyn32),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_87(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 2, 32, happyReduction_87, i)
}

fn happyReduction_87(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, HappyAbsSyn32(happy_var_1)) => {
            if let CDecl(declspecs, dies, at) = happy_var_1 {
                withLength(p, at, partial_1!(CDecl, declspecs, List::reverse(dies)))
            } else {
                panic!("irrefutable pattern")
            }
        }.map(HappyAbsSyn32),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_88(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 2, 32, happyReduction_88, i)
}

fn happyReduction_88(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, HappyAbsSyn32(happy_var_1)) => {
            if let CDecl(declspecs, dies, at) = happy_var_1 {
                withLength(p, at, partial_1!(CDecl, declspecs, List::reverse(dies)))
            } else {
                panic!("irrefutable pattern")
            }
        }.map(HappyAbsSyn32),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_89(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 7, 32, happyReduction_89, i)
}

fn happyReduction_89(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, _, HappyAbsSyn128(happy_var_5), _, HappyAbsSyn100(happy_var_3), _, HappyTerminal(happy_var_1)) => { withNodeInfo(p, happy_var_1, partial_1!(CStaticAssert, happy_var_3, happy_var_5))
        }.map(HappyAbsSyn32),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_90(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_0(p, 33, happyReduction_90(), i)
}

fn happyReduction_90() -> HappyAbsSyn {
    HappyAbsSyn33(empty())
}


fn happyReduce_91(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_2(p, 33, happyReduction_91, i)
}

fn happyReduction_91(happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_2, happy_x_1) {
        (HappyAbsSyn32(happy_var_2), HappyAbsSyn33(happy_var_1)) => HappyAbsSyn33(snoc(happy_var_1, happy_var_2)),
        _ => notHappyAtAll()
    }
}


fn happyReduce_92(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 4, 34, happyReduction_92, i)
}

fn happyReduction_92(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn94(happy_var_4), HappyAbsSyn35(happy_var_3), HappyAbsSyn66(happy_var_2), HappyAbsSyn38(happy_var_1)) => {
            let declspecs = reverse(happy_var_1.clone());
            let declr = withAsmNameAttrs(happy_var_3, happy_var_2)?;
            // TODO: borrow these instead
            doDeclIdent(p, &declspecs, declr.clone());
            withNodeInfo(p, happy_var_1, partial_1!(CDecl, declspecs,
                                           vec![(Some(reverseDeclr(declr)), happy_var_4, None)]))
        }.map(HappyAbsSyn32),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_93(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 4, 34, happyReduction_93, i)
}

fn happyReduction_93(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn94(happy_var_4), HappyAbsSyn35(happy_var_3), HappyAbsSyn66(happy_var_2), HappyAbsSyn65(happy_var_1)) => {
            let declspecs = liftTypeQuals(happy_var_1.clone());
            let declr = withAsmNameAttrs(happy_var_3, happy_var_2)?;
            doDeclIdent(p, &declspecs, declr.clone());
            withNodeInfo(p, happy_var_1, partial_1!(CDecl, declspecs,
                                           vec![(Some(reverseDeclr(declr)), happy_var_4, None)]))
        }.map(HappyAbsSyn32),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_94(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 5, 34, happyReduction_94, i)
}

fn happyReduction_94(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn94(happy_var_5), HappyAbsSyn35(happy_var_4), HappyAbsSyn66(happy_var_3), HappyAbsSyn132(happy_var_2), HappyAbsSyn65(happy_var_1)) => {
            let declspecs = liftTypeQuals(happy_var_1.clone());
            let declr = withAsmNameAttrs(happy_var_4, happy_var_3)?;
            doDeclIdent(p, &declspecs, declr.clone());
            withNodeInfo(p, happy_var_1, partial_1!(CDecl, __op_addadd(declspecs, liftCAttrs(happy_var_2)),
                                           vec![(Some(reverseDeclr(declr)), happy_var_5, None)]))
        }.map(HappyAbsSyn32),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_95(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 4, 34, happyReduction_95, i)
}

fn happyReduction_95(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn94(happy_var_4), HappyAbsSyn35(happy_var_3), HappyAbsSyn66(happy_var_2), HappyAbsSyn132(happy_var_1)) => {
            let declspecs = liftCAttrs(happy_var_1.clone());
            let declr = withAsmNameAttrs(happy_var_3, happy_var_2)?;
            doDeclIdent(p, &declspecs, declr.clone());
            withNodeInfo(p, happy_var_1, partial_1!(CDecl, declspecs,
                                           vec![(Some(reverseDeclr(declr)), happy_var_4, None)]))
        }.map(HappyAbsSyn32),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_96(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 6, 34, happyReduction_96, i)
}

fn happyReduction_96(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn94(happy_var_6), HappyAbsSyn35(happy_var_5), HappyAbsSyn66(happy_var_4), HappyAbsSyn132(happy_var_3), _, HappyAbsSyn32(happy_var_1)) => {
            if let CDecl(declspecs, dies, at) = happy_var_1 {
                let (f, s) = happy_var_5;
                let declr = withAsmNameAttrs((f, __op_addadd(s, happy_var_3)), happy_var_4)?;
                doDeclIdent(p, &declspecs, declr.clone());
                withLength(p, at, partial_1!(CDecl, declspecs,
                                             __op_concat((Some(reverseDeclr(declr)), happy_var_6, None), dies)))
            } else {
                panic!("irrefutable pattern")
            }
        }.map(HappyAbsSyn32),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_97(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_2(p, 35, happyReduction_97, i)
}

fn happyReduction_97(happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_2, happy_x_1) {
        (HappyAbsSyn132(happy_var_2), HappyAbsSyn67(happy_var_1)) => HappyAbsSyn35((happy_var_1, happy_var_2)),
        _ => notHappyAtAll()
    }
}


fn happyReduce_98(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 4, 36, happyReduction_98, i)
}

fn happyReduction_98(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn94(happy_var_4), HappyAbsSyn35(happy_var_3), HappyAbsSyn66(happy_var_2), HappyAbsSyn37(happy_var_1)) => {
            let declr = withAsmNameAttrs(happy_var_3, happy_var_2)?;
            doDeclIdent(p, &happy_var_1, declr.clone());
            withNodeInfo(p, happy_var_1.clone(), partial_1!(CDecl, happy_var_1, vec![(Some(reverseDeclr(declr)), happy_var_4, None)]))
        }.map(HappyAbsSyn32),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_99(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 4, 36, happyReduction_99, i)
}

fn happyReduction_99(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn94(happy_var_4), HappyAbsSyn35(happy_var_3), HappyAbsSyn66(happy_var_2), HappyAbsSyn37(happy_var_1)) => {
            let declr = withAsmNameAttrs(happy_var_3, happy_var_2)?;
            doDeclIdent(p, &happy_var_1, declr.clone());
            withNodeInfo(p, happy_var_1.clone(), partial_1!(CDecl, happy_var_1, vec![(Some(reverseDeclr(declr)), happy_var_4, None)]))
        }.map(HappyAbsSyn32),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_100(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 6, 36, happyReduction_100, i)
}

fn happyReduction_100(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn94(happy_var_6), HappyAbsSyn35(happy_var_5), HappyAbsSyn66(happy_var_4), HappyAbsSyn132(happy_var_3), _, HappyAbsSyn32(happy_var_1)) => {
            if let CDecl(declspecs, dies, at) = happy_var_1 {
                let (f, s) = happy_var_5;
                let declr = withAsmNameAttrs((f, __op_addadd(s, happy_var_3)), happy_var_4)?;
                doDeclIdent(p, &declspecs, declr.clone());
                Ok(CDecl(declspecs, __op_concat((Some(reverseDeclr(declr)), happy_var_6, None), dies), at))
            } else {
                panic!("irrefutable pattern")
            }
        }.map(HappyAbsSyn32),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_101(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_1(p, 37, happyReduction_101, i)
}

fn happyReduction_101(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn38(happy_var_1) => HappyAbsSyn37(reverse(happy_var_1)),
        _ => notHappyAtAll()
    }
}


fn happyReduce_102(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_1(p, 37, happyReduction_102, i)
}

fn happyReduction_102(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn38(happy_var_1) => HappyAbsSyn37(reverse(happy_var_1)),
        _ => notHappyAtAll()
    }
}


fn happyReduce_103(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_1(p, 37, happyReduction_103, i)
}

fn happyReduction_103(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn38(happy_var_1) => HappyAbsSyn37(reverse(happy_var_1)),
        _ => notHappyAtAll()
    }
}


fn happyReduce_104(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_1(p, 38, happyReduction_104, i)
}

fn happyReduction_104(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn39(happy_var_1) => HappyAbsSyn38(singleton(happy_var_1)),
        _ => notHappyAtAll()
    }
}


fn happyReduce_105(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_2(p, 38, happyReduction_105, i)
}

fn happyReduction_105(happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_2, happy_x_1) {
        (HappyAbsSyn39(happy_var_2), HappyAbsSyn132(happy_var_1)) => HappyAbsSyn38(snoc(reverseList(liftCAttrs(happy_var_1)), happy_var_2)),
        _ => notHappyAtAll()
    }
}


fn happyReduce_106(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_2(p, 38, happyReduction_106, i)
}

fn happyReduction_106(happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_2, happy_x_1) {
        (HappyAbsSyn39(happy_var_2), HappyAbsSyn65(happy_var_1)) => HappyAbsSyn38(snoc(rmap(CTypeQual, happy_var_1), happy_var_2)),
        _ => notHappyAtAll()
    }
}


fn happyReduce_107(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_3(p, 38, happyReduction_107, i)
}

fn happyReduction_107(happy_x_3: HappyAbsSyn, happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_3, happy_x_2, happy_x_1) {
        (HappyAbsSyn39(happy_var_3), HappyAbsSyn132(happy_var_2), HappyAbsSyn65(happy_var_1)) => HappyAbsSyn38(snoc(rappend(rmap(CTypeQual, happy_var_1), liftCAttrs(happy_var_2)), happy_var_3)),
        _ => notHappyAtAll()
    }
}


fn happyReduce_108(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_2(p, 38, happyReduction_108, i)
}

fn happyReduction_108(happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_2, happy_x_1) {
        (HappyAbsSyn39(happy_var_2), HappyAbsSyn38(happy_var_1)) => HappyAbsSyn38(snoc(happy_var_1, happy_var_2)),
        _ => notHappyAtAll()
    }
}


fn happyReduce_109(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_2(p, 38, happyReduction_109, i)
}

fn happyReduction_109(happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_2, happy_x_1) {
        (HappyAbsSyn132(happy_var_2), HappyAbsSyn38(happy_var_1)) => HappyAbsSyn38(addTrailingAttrs(happy_var_1, happy_var_2)),
        _ => notHappyAtAll()
    }
}


fn happyReduce_110(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_1(p, 39, happyReduction_110, i)
}

fn happyReduction_110(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn41(happy_var_1) => HappyAbsSyn39(CStorageSpec(happy_var_1)),
        _ => notHappyAtAll()
    }
}


fn happyReduce_111(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_1(p, 39, happyReduction_111, i)
}

fn happyReduction_111(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn64(happy_var_1) => HappyAbsSyn39(CTypeQual(happy_var_1)),
        _ => notHappyAtAll()
    }
}


fn happyReduce_112(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_1(p, 39, happyReduction_112, i)
}

fn happyReduction_112(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn42(happy_var_1) => HappyAbsSyn39(CFunSpec(happy_var_1)),
        _ => notHappyAtAll()
    }
}


fn happyReduce_113(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_1(p, 39, happyReduction_113, i)
}

fn happyReduction_113(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn43(happy_var_1) => HappyAbsSyn39(CAlignSpec(happy_var_1)),
        _ => notHappyAtAll()
    }
}


fn happyReduce_114(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_1(p, 40, happyReduction_114, i)
}

fn happyReduction_114(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn41(happy_var_1) => HappyAbsSyn39(CStorageSpec(happy_var_1)),
        _ => notHappyAtAll()
    }
}


fn happyReduce_115(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_1(p, 40, happyReduction_115, i)
}

fn happyReduction_115(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn42(happy_var_1) => HappyAbsSyn39(CFunSpec(happy_var_1)),
        _ => notHappyAtAll()
    }
}


fn happyReduce_116(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_1(p, 40, happyReduction_116, i)
}

fn happyReduction_116(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn43(happy_var_1) => HappyAbsSyn39(CAlignSpec(happy_var_1)),
        _ => notHappyAtAll()
    }
}


fn happyReduce_117(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 1, 41, happyReduction_117, i)
}

fn happyReduction_117(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap()) {
        HappyTerminal(happy_var_1) => { withNodeInfo(p, happy_var_1, partial_1!(CTypedef))
        }.map(HappyAbsSyn41),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_118(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 1, 41, happyReduction_118, i)
}

fn happyReduction_118(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap()) {
        HappyTerminal(happy_var_1) => { withNodeInfo(p, happy_var_1, partial_1!(CExtern))
        }.map(HappyAbsSyn41),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_119(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 1, 41, happyReduction_119, i)
}

fn happyReduction_119(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap()) {
        HappyTerminal(happy_var_1) => { withNodeInfo(p, happy_var_1, partial_1!(CStatic))
        }.map(HappyAbsSyn41),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_120(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 1, 41, happyReduction_120, i)
}

fn happyReduction_120(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap()) {
        HappyTerminal(happy_var_1) => { withNodeInfo(p, happy_var_1, partial_1!(CAuto))
        }.map(HappyAbsSyn41),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_121(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 1, 41, happyReduction_121, i)
}

fn happyReduction_121(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap()) {
        HappyTerminal(happy_var_1) => { withNodeInfo(p, happy_var_1, partial_1!(CRegister))
        }.map(HappyAbsSyn41),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_122(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 1, 41, happyReduction_122, i)
}

fn happyReduction_122(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap()) {
        HappyTerminal(happy_var_1) => { withNodeInfo(p, happy_var_1, partial_1!(CThread))
        }.map(HappyAbsSyn41),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_123(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 1, 42, happyReduction_123, i)
}

fn happyReduction_123(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap()) {
        HappyTerminal(happy_var_1) => { withNodeInfo(p, happy_var_1, partial_1!(CInlineQual))
        }.map(HappyAbsSyn42),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_124(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 1, 42, happyReduction_124, i)
}

fn happyReduction_124(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap()) {
        HappyTerminal(happy_var_1) => { withNodeInfo(p, happy_var_1, partial_1!(CNoreturnQual))
        }.map(HappyAbsSyn42),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_125(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 4, 43, happyReduction_125, i)
}

fn happyReduction_125(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, HappyAbsSyn32(happy_var_3), _, HappyTerminal(happy_var_1)) => { withNodeInfo(p, happy_var_1, partial_1!(CAlignAsType, happy_var_3))
        }.map(HappyAbsSyn43),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_126(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 4, 43, happyReduction_126, i)
}

fn happyReduction_126(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, HappyAbsSyn100(happy_var_3), _, HappyTerminal(happy_var_1)) => { withNodeInfo(p, happy_var_1, partial_1!(CAlignAsExpr, happy_var_3))
        }.map(HappyAbsSyn43),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_127(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_1(p, 44, happyReduction_127, i)
}

fn happyReduction_127(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn38(happy_var_1) => HappyAbsSyn37(reverse(happy_var_1)),
        _ => notHappyAtAll()
    }
}


fn happyReduce_128(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_1(p, 44, happyReduction_128, i)
}

fn happyReduction_128(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn38(happy_var_1) => HappyAbsSyn37(reverse(happy_var_1)),
        _ => notHappyAtAll()
    }
}


fn happyReduce_129(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_1(p, 44, happyReduction_129, i)
}

fn happyReduction_129(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn38(happy_var_1) => HappyAbsSyn37(reverse(happy_var_1)),
        _ => notHappyAtAll()
    }
}


fn happyReduce_130(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 1, 45, happyReduction_130, i)
}

fn happyReduction_130(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap()) {
        HappyTerminal(happy_var_1) => { withNodeInfo(p, happy_var_1, partial_1!(CVoidType))
        }.map(HappyAbsSyn45),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_131(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 1, 45, happyReduction_131, i)
}

fn happyReduction_131(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap()) {
        HappyTerminal(happy_var_1) => { withNodeInfo(p, happy_var_1, partial_1!(CCharType))
        }.map(HappyAbsSyn45),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_132(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 1, 45, happyReduction_132, i)
}

fn happyReduction_132(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap()) {
        HappyTerminal(happy_var_1) => { withNodeInfo(p, happy_var_1, partial_1!(CShortType))
        }.map(HappyAbsSyn45),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_133(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 1, 45, happyReduction_133, i)
}

fn happyReduction_133(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap()) {
        HappyTerminal(happy_var_1) => { withNodeInfo(p, happy_var_1, partial_1!(CIntType))
        }.map(HappyAbsSyn45),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_134(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 1, 45, happyReduction_134, i)
}

fn happyReduction_134(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap()) {
        HappyTerminal(happy_var_1) => { withNodeInfo(p, happy_var_1, partial_1!(CLongType))
        }.map(HappyAbsSyn45),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_135(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 1, 45, happyReduction_135, i)
}

fn happyReduction_135(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap()) {
        HappyTerminal(happy_var_1) => { withNodeInfo(p, happy_var_1, partial_1!(CFloatType))
        }.map(HappyAbsSyn45),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_136(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 1, 45, happyReduction_136, i)
}

fn happyReduction_136(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap()) {
        HappyTerminal(happy_var_1) => { withNodeInfo(p, happy_var_1, partial_1!(CDoubleType))
        }.map(HappyAbsSyn45),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_137(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 1, 45, happyReduction_137, i)
}

fn happyReduction_137(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap()) {
        HappyTerminal(happy_var_1) => { withNodeInfo(p, happy_var_1, partial_1!(CSignedType))
        }.map(HappyAbsSyn45),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_138(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 1, 45, happyReduction_138, i)
}

fn happyReduction_138(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap()) {
        HappyTerminal(happy_var_1) => { withNodeInfo(p, happy_var_1, partial_1!(CUnsigType))
        }.map(HappyAbsSyn45),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_139(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 1, 45, happyReduction_139, i)
}

fn happyReduction_139(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap()) {
        HappyTerminal(happy_var_1) => { withNodeInfo(p, happy_var_1, partial_1!(CBoolType))
        }.map(HappyAbsSyn45),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_140(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 1, 45, happyReduction_140, i)
}

fn happyReduction_140(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap()) {
        HappyTerminal(happy_var_1) => { withNodeInfo(p, happy_var_1, partial_1!(CComplexType))
        }.map(HappyAbsSyn45),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_141(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 1, 45, happyReduction_141, i)
}

fn happyReduction_141(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap()) {
        HappyTerminal(happy_var_1) => { withNodeInfo(p, happy_var_1, partial_1!(CInt128Type))
        }.map(HappyAbsSyn45),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_142(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_2(p, 46, happyReduction_142, i)
}

fn happyReduction_142(happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_2, happy_x_1) {
        (HappyAbsSyn45(happy_var_2), HappyAbsSyn38(happy_var_1)) => HappyAbsSyn38(snoc(happy_var_1, CTypeSpec(happy_var_2))),
        _ => notHappyAtAll()
    }
}


fn happyReduce_143(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_2(p, 46, happyReduction_143, i)
}

fn happyReduction_143(happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_2, happy_x_1) {
        (HappyAbsSyn41(happy_var_2), HappyAbsSyn38(happy_var_1)) => HappyAbsSyn38(snoc(happy_var_1, CStorageSpec(happy_var_2))),
        _ => notHappyAtAll()
    }
}


fn happyReduce_144(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_2(p, 46, happyReduction_144, i)
}

fn happyReduction_144(happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_2, happy_x_1) {
        (HappyAbsSyn39(happy_var_2), HappyAbsSyn38(happy_var_1)) => HappyAbsSyn38(snoc(happy_var_1, happy_var_2)),
        _ => notHappyAtAll()
    }
}


fn happyReduce_145(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_2(p, 46, happyReduction_145, i)
}

fn happyReduction_145(happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_2, happy_x_1) {
        (HappyAbsSyn45(happy_var_2), HappyAbsSyn38(happy_var_1)) => HappyAbsSyn38(snoc(happy_var_1, CTypeSpec(happy_var_2))),
        _ => notHappyAtAll()
    }
}


fn happyReduce_146(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_2(p, 46, happyReduction_146, i)
}

fn happyReduction_146(happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_2, happy_x_1) {
        (HappyAbsSyn132(happy_var_2), HappyAbsSyn38(happy_var_1)) => HappyAbsSyn38(addTrailingAttrs(happy_var_1, happy_var_2)),
        _ => notHappyAtAll()
    }
}


fn happyReduce_147(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_1(p, 47, happyReduction_147, i)
}

fn happyReduction_147(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn45(happy_var_1) => HappyAbsSyn38(singleton(CTypeSpec(happy_var_1))),
        _ => notHappyAtAll()
    }
}


fn happyReduce_148(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_2(p, 47, happyReduction_148, i)
}

fn happyReduction_148(happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_2, happy_x_1) {
        (HappyAbsSyn45(happy_var_2), HappyAbsSyn132(happy_var_1)) => HappyAbsSyn38(snoc(reverseList(liftCAttrs(happy_var_1)), CTypeSpec(happy_var_2))),
        _ => notHappyAtAll()
    }
}


fn happyReduce_149(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_2(p, 47, happyReduction_149, i)
}

fn happyReduction_149(happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_2, happy_x_1) {
        (HappyAbsSyn45(happy_var_2), HappyAbsSyn65(happy_var_1)) => HappyAbsSyn38(snoc(rmap(CTypeQual, happy_var_1), CTypeSpec(happy_var_2))),
        _ => notHappyAtAll()
    }
}


fn happyReduce_150(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_3(p, 47, happyReduction_150, i)
}

fn happyReduction_150(happy_x_3: HappyAbsSyn, happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_3, happy_x_2, happy_x_1) {
        (HappyAbsSyn45(happy_var_3), HappyAbsSyn132(happy_var_2), HappyAbsSyn65(happy_var_1)) => HappyAbsSyn38(snoc(rappend(rmap(CTypeQual, happy_var_1), liftCAttrs(happy_var_2)), CTypeSpec(happy_var_3))),
        _ => notHappyAtAll()
    }
}


fn happyReduce_151(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_2(p, 47, happyReduction_151, i)
}

fn happyReduction_151(happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_2, happy_x_1) {
        (HappyAbsSyn64(happy_var_2), HappyAbsSyn38(happy_var_1)) => HappyAbsSyn38(snoc(happy_var_1, CTypeQual(happy_var_2))),
        _ => notHappyAtAll()
    }
}


fn happyReduce_152(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_2(p, 47, happyReduction_152, i)
}

fn happyReduction_152(happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_2, happy_x_1) {
        (HappyAbsSyn45(happy_var_2), HappyAbsSyn38(happy_var_1)) => HappyAbsSyn38(snoc(happy_var_1, CTypeSpec(happy_var_2))),
        _ => notHappyAtAll()
    }
}


fn happyReduce_153(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_2(p, 47, happyReduction_153, i)
}

fn happyReduction_153(happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_2, happy_x_1) {
        (HappyAbsSyn132(happy_var_2), HappyAbsSyn38(happy_var_1)) => HappyAbsSyn38(addTrailingAttrs(happy_var_1, happy_var_2)),
        _ => notHappyAtAll()
    }
}


fn happyReduce_154(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_2(p, 48, happyReduction_154, i)
}

fn happyReduction_154(happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_2, happy_x_1) {
        (HappyAbsSyn45(happy_var_2), HappyAbsSyn38(happy_var_1)) => HappyAbsSyn38(snoc(happy_var_1, CTypeSpec(happy_var_2))),
        _ => notHappyAtAll()
    }
}


fn happyReduce_155(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_2(p, 48, happyReduction_155, i)
}

fn happyReduction_155(happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_2, happy_x_1) {
        (HappyAbsSyn41(happy_var_2), HappyAbsSyn38(happy_var_1)) => HappyAbsSyn38(snoc(happy_var_1, CStorageSpec(happy_var_2))),
        _ => notHappyAtAll()
    }
}


fn happyReduce_156(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_2(p, 48, happyReduction_156, i)
}

fn happyReduction_156(happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_2, happy_x_1) {
        (HappyAbsSyn39(happy_var_2), HappyAbsSyn38(happy_var_1)) => HappyAbsSyn38(snoc(happy_var_1, happy_var_2)),
        _ => notHappyAtAll()
    }
}


fn happyReduce_157(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_2(p, 48, happyReduction_157, i)
}

fn happyReduction_157(happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_2, happy_x_1) {
        (HappyAbsSyn132(happy_var_2), HappyAbsSyn38(happy_var_1)) => HappyAbsSyn38(addTrailingAttrs(happy_var_1, happy_var_2)),
        _ => notHappyAtAll()
    }
}


fn happyReduce_158(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_1(p, 49, happyReduction_158, i)
}

fn happyReduction_158(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn45(happy_var_1) => HappyAbsSyn38(singleton(CTypeSpec(happy_var_1))),
        _ => notHappyAtAll()
    }
}


fn happyReduce_159(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_2(p, 49, happyReduction_159, i)
}

fn happyReduction_159(happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_2, happy_x_1) {
        (HappyAbsSyn45(happy_var_2), HappyAbsSyn132(happy_var_1)) => HappyAbsSyn38(snoc(reverseList(liftCAttrs(happy_var_1)), CTypeSpec(happy_var_2))),
        _ => notHappyAtAll()
    }
}


fn happyReduce_160(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_2(p, 49, happyReduction_160, i)
}

fn happyReduction_160(happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_2, happy_x_1) {
        (HappyAbsSyn45(happy_var_2), HappyAbsSyn65(happy_var_1)) => HappyAbsSyn38(snoc(rmap(CTypeQual, happy_var_1), CTypeSpec(happy_var_2))),
        _ => notHappyAtAll()
    }
}


fn happyReduce_161(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_3(p, 49, happyReduction_161, i)
}

fn happyReduction_161(happy_x_3: HappyAbsSyn, happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_3, happy_x_2, happy_x_1) {
        (HappyAbsSyn45(happy_var_3), HappyAbsSyn132(happy_var_2), HappyAbsSyn65(happy_var_1)) => HappyAbsSyn38(snoc(rappend(rmap(CTypeQual, happy_var_1), liftCAttrs(happy_var_2)), CTypeSpec(happy_var_3))),
        _ => notHappyAtAll()
    }
}


fn happyReduce_162(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_2(p, 49, happyReduction_162, i)
}

fn happyReduction_162(happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_2, happy_x_1) {
        (HappyAbsSyn64(happy_var_2), HappyAbsSyn38(happy_var_1)) => HappyAbsSyn38(snoc(happy_var_1, CTypeQual(happy_var_2))),
        _ => notHappyAtAll()
    }
}


fn happyReduce_163(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_2(p, 49, happyReduction_163, i)
}

fn happyReduction_163(happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_2, happy_x_1) {
        (HappyAbsSyn132(happy_var_2), HappyAbsSyn38(happy_var_1)) => HappyAbsSyn38(addTrailingAttrs(happy_var_1, happy_var_2)),
        _ => notHappyAtAll()
    }
}


fn happyReduce_164(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_2(p, 50, happyReduction_164, i)
}

fn happyReduction_164(happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_2, happy_x_1) {
        (HappyAbsSyn41(happy_var_2), HappyAbsSyn38(happy_var_1)) => HappyAbsSyn38(snoc(happy_var_1, CStorageSpec(happy_var_2))),
        _ => notHappyAtAll()
    }
}


fn happyReduce_165(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 2, 50, happyReduction_165, i)
}

fn happyReduction_165(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyTerminal(CTokTyIdent(_, happy_var_2)), HappyAbsSyn38(happy_var_1)) => { withNodeInfo(p, happy_var_2.clone(), box |at| snoc(happy_var_1, CTypeSpec(CTypeDef(happy_var_2, at))))
        }.map(HappyAbsSyn38),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_166(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 5, 50, happyReduction_166, i)
}

fn happyReduction_166(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, HappyAbsSyn100(happy_var_4), _, HappyTerminal(happy_var_2), HappyAbsSyn38(happy_var_1)) => { withNodeInfo(p, happy_var_2, box |at| snoc(happy_var_1, CTypeSpec(CTypeOfExpr(happy_var_4, at))))
        }.map(HappyAbsSyn38),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_167(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 5, 50, happyReduction_167, i)
}

fn happyReduction_167(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, HappyAbsSyn32(happy_var_4), _, HappyTerminal(happy_var_2), HappyAbsSyn38(happy_var_1)) => { withNodeInfo(p, happy_var_2, box |at| snoc(happy_var_1, CTypeSpec(CTypeOfType(happy_var_4, at))))
        }.map(HappyAbsSyn38),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_168(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_2(p, 50, happyReduction_168, i)
}

fn happyReduction_168(happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_2, happy_x_1) {
        (HappyAbsSyn39(happy_var_2), HappyAbsSyn38(happy_var_1)) => HappyAbsSyn38(snoc(happy_var_1, happy_var_2)),
        _ => notHappyAtAll()
    }
}


fn happyReduce_169(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_2(p, 50, happyReduction_169, i)
}

fn happyReduction_169(happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_2, happy_x_1) {
        (HappyAbsSyn132(happy_var_2), HappyAbsSyn38(happy_var_1)) => HappyAbsSyn38(addTrailingAttrs(happy_var_1, happy_var_2)),
        _ => notHappyAtAll()
    }
}


fn happyReduce_170(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 1, 51, happyReduction_170, i)
}

fn happyReduction_170(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap()) {
        HappyTerminal(CTokTyIdent(_, happy_var_1)) => { withNodeInfo(p, happy_var_1.clone(), box |at| singleton(CTypeSpec(CTypeDef(happy_var_1, at))))
        }.map(HappyAbsSyn38),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_171(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 4, 51, happyReduction_171, i)
}

fn happyReduction_171(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, HappyAbsSyn100(happy_var_3), _, HappyTerminal(happy_var_1)) => { withNodeInfo(p, happy_var_1, box |at| singleton(CTypeSpec(CTypeOfExpr(happy_var_3, at))))
        }.map(HappyAbsSyn38),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_172(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 4, 51, happyReduction_172, i)
}

fn happyReduction_172(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, HappyAbsSyn32(happy_var_3), _, HappyTerminal(happy_var_1)) => { withNodeInfo(p, happy_var_1, box |at| singleton(CTypeSpec(CTypeOfType(happy_var_3, at))))
        }.map(HappyAbsSyn38),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_173(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 2, 51, happyReduction_173, i)
}

fn happyReduction_173(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyTerminal(CTokTyIdent(_, happy_var_2)), HappyAbsSyn65(happy_var_1)) => { withNodeInfo(p, happy_var_2.clone(), box |at| snoc(rmap(CTypeQual, happy_var_1), CTypeSpec(CTypeDef(happy_var_2, at))))
        }.map(HappyAbsSyn38),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_174(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 5, 51, happyReduction_174, i)
}

fn happyReduction_174(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, HappyAbsSyn100(happy_var_4), _, HappyTerminal(happy_var_2), HappyAbsSyn65(happy_var_1)) => { withNodeInfo(p, happy_var_2, box |at| snoc(rmap(CTypeQual, happy_var_1), CTypeSpec(CTypeOfExpr(happy_var_4, at))))
        }.map(HappyAbsSyn38),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_175(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 5, 51, happyReduction_175, i)
}

fn happyReduction_175(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, HappyAbsSyn32(happy_var_4), _, HappyTerminal(happy_var_2), HappyAbsSyn65(happy_var_1)) => { withNodeInfo(p, happy_var_2, box |at| snoc(rmap(CTypeQual, happy_var_1), CTypeSpec(CTypeOfType(happy_var_4, at))))
        }.map(HappyAbsSyn38),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_176(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 2, 51, happyReduction_176, i)
}

fn happyReduction_176(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyTerminal(CTokTyIdent(_, happy_var_2)), HappyAbsSyn132(happy_var_1)) => { withNodeInfo(p, happy_var_2.clone(), box |at| snoc(reverseList(liftCAttrs(happy_var_1)), CTypeSpec(CTypeDef(happy_var_2, at))))
        }.map(HappyAbsSyn38),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_177(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 5, 51, happyReduction_177, i)
}

fn happyReduction_177(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, HappyAbsSyn100(happy_var_4), _, _, HappyAbsSyn132(happy_var_1)) => { withNodeInfo(p, happy_var_1.clone(), box |at| snoc(reverseList(liftCAttrs(happy_var_1)), CTypeSpec(CTypeOfExpr(happy_var_4, at))))
        }.map(HappyAbsSyn38),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_178(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 5, 51, happyReduction_178, i)
}

fn happyReduction_178(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, HappyAbsSyn32(happy_var_4), _, HappyTerminal(happy_var_2), HappyAbsSyn132(happy_var_1)) => { withNodeInfo(p, happy_var_2, box |at| snoc(reverseList(liftCAttrs(happy_var_1)), CTypeSpec(CTypeOfType(happy_var_4, at))))
        }.map(HappyAbsSyn38),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_179(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 3, 51, happyReduction_179, i)
}

fn happyReduction_179(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyTerminal(CTokTyIdent(_, happy_var_3)), HappyAbsSyn132(happy_var_2), HappyAbsSyn65(happy_var_1)) => { withNodeInfo(p, happy_var_3.clone(), box |at| snoc(rappend(rmap(CTypeQual, happy_var_1), liftCAttrs(happy_var_2)),
                                                  CTypeSpec(CTypeDef(happy_var_3, at))))
        }.map(HappyAbsSyn38),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_180(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 6, 51, happyReduction_180, i)
}

fn happyReduction_180(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, HappyAbsSyn100(happy_var_5), _, HappyTerminal(happy_var_3), HappyAbsSyn132(happy_var_2), HappyAbsSyn65(happy_var_1)) => { withNodeInfo(p, happy_var_3, box |at| snoc(rappend(rmap(CTypeQual, happy_var_1), liftCAttrs(happy_var_2)),
                                          CTypeSpec(CTypeOfExpr(happy_var_5, at))))
        }.map(HappyAbsSyn38),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_181(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 6, 51, happyReduction_181, i)
}

fn happyReduction_181(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, HappyAbsSyn32(happy_var_5), _, HappyTerminal(happy_var_3), HappyAbsSyn132(happy_var_2), HappyAbsSyn65(happy_var_1)) => { withNodeInfo(p, happy_var_3, box |at| snoc(rappend(rmap(CTypeQual, happy_var_1), liftCAttrs(happy_var_2)),
                                          CTypeSpec(CTypeOfType(happy_var_5, at))))
        }.map(HappyAbsSyn38),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_182(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_2(p, 51, happyReduction_182, i)
}

fn happyReduction_182(happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_2, happy_x_1) {
        (HappyAbsSyn64(happy_var_2), HappyAbsSyn38(happy_var_1)) => HappyAbsSyn38(snoc(happy_var_1, CTypeQual(happy_var_2))),
        _ => notHappyAtAll()
    }
}


fn happyReduce_183(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_2(p, 51, happyReduction_183, i)
}

fn happyReduction_183(happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_2, happy_x_1) {
        (HappyAbsSyn132(happy_var_2), HappyAbsSyn38(happy_var_1)) => HappyAbsSyn38(addTrailingAttrs(happy_var_1, happy_var_2)),
        _ => notHappyAtAll()
    }
}


fn happyReduce_184(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 1, 52, happyReduction_184, i)
}

fn happyReduction_184(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap()) {
        HappyAbsSyn53(happy_var_1) => { withNodeInfo(p, happy_var_1.clone(), partial_1!(CSUType, happy_var_1))
        }.map(HappyAbsSyn45),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_185(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 1, 52, happyReduction_185, i)
}

fn happyReduction_185(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap()) {
        HappyAbsSyn61(happy_var_1) => { withNodeInfo(p, happy_var_1.clone(), partial_1!(CEnumType, happy_var_1))
        }.map(HappyAbsSyn45),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_186(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 6, 53, happyReduction_186, i)
}

fn happyReduction_186(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, HappyAbsSyn33(happy_var_5), _, HappyAbsSyn131(happy_var_3), HappyAbsSyn132(happy_var_2), HappyAbsSyn54(happy_var_1)) => { withNodeInfo(p, happy_var_1.clone(), partial_1!(CStructureUnion, unL(happy_var_1), Some(happy_var_3), Some(reverse(happy_var_5)), happy_var_2))
        }.map(HappyAbsSyn53),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_187(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 5, 53, happyReduction_187, i)
}

fn happyReduction_187(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, HappyAbsSyn33(happy_var_4), _, HappyAbsSyn132(happy_var_2), HappyAbsSyn54(happy_var_1)) => { withNodeInfo(p, happy_var_1.clone(), partial_1!(CStructureUnion, unL(happy_var_1), None,     Some(reverse(happy_var_4)), happy_var_2))
        }.map(HappyAbsSyn53),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_188(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 3, 53, happyReduction_188, i)
}

fn happyReduction_188(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn131(happy_var_3), HappyAbsSyn132(happy_var_2), HappyAbsSyn54(happy_var_1)) => { withNodeInfo(p, happy_var_1.clone(), partial_1!(CStructureUnion, unL(happy_var_1), Some(happy_var_3), None,              happy_var_2))
        }.map(HappyAbsSyn53),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_189(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_1(p, 54, happyReduction_189, i)
}

fn happyReduction_189(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyTerminal(happy_var_1) => HappyAbsSyn54(Located(CStructTag, happy_var_1.into_pos())),
        _ => notHappyAtAll()
    }
}


fn happyReduce_190(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_1(p, 54, happyReduction_190, i)
}

fn happyReduction_190(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyTerminal(happy_var_1) => HappyAbsSyn54(Located(CUnionTag, happy_var_1.into_pos())),
        _ => notHappyAtAll()
    }
}


fn happyReduce_191(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_0(p, 55, happyReduction_191(), i)
}

fn happyReduction_191() -> HappyAbsSyn {
    HappyAbsSyn33(empty())
}


fn happyReduce_192(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_2(p, 55, happyReduction_192, i)
}

fn happyReduction_192(happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_2, happy_x_1) {
        (_, HappyAbsSyn33(happy_var_1)) => HappyAbsSyn33(happy_var_1),
        _ => notHappyAtAll()
    }
}


fn happyReduce_193(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_2(p, 55, happyReduction_193, i)
}

fn happyReduction_193(happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_2, happy_x_1) {
        (HappyAbsSyn32(happy_var_2), HappyAbsSyn33(happy_var_1)) => HappyAbsSyn33(snoc(happy_var_1, happy_var_2)),
        _ => notHappyAtAll()
    }
}


fn happyReduce_194(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_2(p, 56, happyReduction_194, i)
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


fn happyReduce_195(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_2(p, 56, happyReduction_195, i)
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


fn happyReduce_196(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_2(p, 56, happyReduction_196, i)
}

fn happyReduction_196(happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_2, happy_x_1) {
        (HappyAbsSyn32(happy_var_2), _) => HappyAbsSyn32(happy_var_2),
        _ => notHappyAtAll()
    }
}


fn happyReduce_197(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 3, 57, happyReduction_197, i)
}

fn happyReduction_197(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn59(happy_var_3), HappyAbsSyn132(happy_var_2), HappyAbsSyn65(happy_var_1)) => {
            withNodeInfo(p, happy_var_1.clone(), match happy_var_3 {
                (d, s) => partial_1!(CDecl, __op_addadd(liftTypeQuals(happy_var_1), liftCAttrs(happy_var_2)),
                                     vec![(d, None, s)])
            })
        }.map(HappyAbsSyn32),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_198(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 2, 57, happyReduction_198, i)
}

fn happyReduction_198(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn59(happy_var_2), HappyAbsSyn132(happy_var_1)) => {
            withNodeInfo(p, happy_var_1.clone(), match happy_var_2 {
                (d, s) => partial_1!(CDecl, liftCAttrs(happy_var_1), vec![(d, None, s)]),
            })
        }.map(HappyAbsSyn32),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_199(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyReduce(p, 4, 57, happyReduction_199, i)
}

fn happyReduction_199(p: &mut Parser) {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn59(happy_var_4), HappyAbsSyn132(happy_var_3), _, HappyAbsSyn32(happy_var_1)) => {            p.stack.push(HappyAbsSyn32(if let CDecl(declspecs, dies, at) = happy_var_1 {
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
            })); }
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_200(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 3, 58, happyReduction_200, i)
}

fn happyReduction_200(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn132(happy_var_3), HappyAbsSyn59(happy_var_2), HappyAbsSyn37(happy_var_1)) => {
            withNodeInfo(p, happy_var_1.clone(), match happy_var_2 {
                (Some(d), s) => {
                    partial_1!(CDecl, happy_var_1, vec![(Some(appendObjAttrs(happy_var_3, d)), None, s)])
                },
                (None, s) => {
                    partial_1!(CDecl, happy_var_1, vec![(None, None, s)])
                },
            })
        }.map(HappyAbsSyn32),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_201(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyReduce(p, 5, 58, happyReduction_201, i)
}

fn happyReduction_201(p: &mut Parser) {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn132(happy_var_5), HappyAbsSyn59(happy_var_4), HappyAbsSyn132(happy_var_3), _, HappyAbsSyn32(happy_var_1)) => {            p.stack.push(HappyAbsSyn32(if let CDecl(declspecs, dies, at) = happy_var_1 {
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
            })); }
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_202(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 1, 58, happyReduction_202, i)
}

fn happyReduction_202(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap()) {
        HappyAbsSyn37(happy_var_1) => { withNodeInfo(p, happy_var_1.clone(), partial_1!(CDecl, happy_var_1, vec![]))
        }.map(HappyAbsSyn32),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_203(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_1(p, 59, happyReduction_203, i)
}

fn happyReduction_203(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn66(happy_var_1) => HappyAbsSyn59((Some(reverseDeclr(happy_var_1)), None)),
        _ => notHappyAtAll()
    }
}


fn happyReduce_204(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_2(p, 59, happyReduction_204, i)
}

fn happyReduction_204(happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_2, happy_x_1) {
        (HappyAbsSyn100(happy_var_2), _) => HappyAbsSyn59((None, Some(happy_var_2))),
        _ => notHappyAtAll()
    }
}


fn happyReduce_205(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_3(p, 59, happyReduction_205, i)
}

fn happyReduction_205(happy_x_3: HappyAbsSyn, happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_3, happy_x_2, happy_x_1) {
        (HappyAbsSyn100(happy_var_3), _, HappyAbsSyn66(happy_var_1)) => HappyAbsSyn59((Some(reverseDeclr(happy_var_1)), Some(happy_var_3))),
        _ => notHappyAtAll()
    }
}


fn happyReduce_206(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_1(p, 60, happyReduction_206, i)
}

fn happyReduction_206(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn66(happy_var_1) => HappyAbsSyn59((Some(reverseDeclr(happy_var_1)), None)),
        _ => notHappyAtAll()
    }
}


fn happyReduce_207(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_2(p, 60, happyReduction_207, i)
}

fn happyReduction_207(happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_2, happy_x_1) {
        (HappyAbsSyn100(happy_var_2), _) => HappyAbsSyn59((None, Some(happy_var_2))),
        _ => notHappyAtAll()
    }
}


fn happyReduce_208(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_3(p, 60, happyReduction_208, i)
}

fn happyReduction_208(happy_x_3: HappyAbsSyn, happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_3, happy_x_2, happy_x_1) {
        (HappyAbsSyn100(happy_var_3), _, HappyAbsSyn66(happy_var_1)) => HappyAbsSyn59((Some(reverseDeclr(happy_var_1)), Some(happy_var_3))),
        _ => notHappyAtAll()
    }
}


fn happyReduce_209(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_2(p, 60, happyReduction_209, i)
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


fn happyReduce_210(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 5, 61, happyReduction_210, i)
}

fn happyReduction_210(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, HappyAbsSyn62(happy_var_4), _, HappyAbsSyn132(happy_var_2), HappyTerminal(happy_var_1)) => { withNodeInfo(p, happy_var_1, partial_1!(CEnumeration, None, Some(reverse(happy_var_4)), happy_var_2))
        }.map(HappyAbsSyn61),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_211(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 6, 61, happyReduction_211, i)
}

fn happyReduction_211(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, _, HappyAbsSyn62(happy_var_4), _, HappyAbsSyn132(happy_var_2), HappyTerminal(happy_var_1)) => { withNodeInfo(p, happy_var_1, partial_1!(CEnumeration, None, Some(reverse(happy_var_4)), happy_var_2))
        }.map(HappyAbsSyn61),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_212(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 6, 61, happyReduction_212, i)
}

fn happyReduction_212(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, HappyAbsSyn62(happy_var_5), _, HappyAbsSyn131(happy_var_3), HappyAbsSyn132(happy_var_2), HappyTerminal(happy_var_1)) => { withNodeInfo(p, happy_var_1, partial_1!(CEnumeration, Some(happy_var_3), Some(reverse(happy_var_5)), happy_var_2))
        }.map(HappyAbsSyn61),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_213(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 7, 61, happyReduction_213, i)
}

fn happyReduction_213(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, _, HappyAbsSyn62(happy_var_5), _, HappyAbsSyn131(happy_var_3), HappyAbsSyn132(happy_var_2), HappyTerminal(happy_var_1)) => { withNodeInfo(p, happy_var_1, partial_1!(CEnumeration, Some(happy_var_3), Some(reverse(happy_var_5)), happy_var_2))
        }.map(HappyAbsSyn61),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_214(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 3, 61, happyReduction_214, i)
}

fn happyReduction_214(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn131(happy_var_3), HappyAbsSyn132(happy_var_2), HappyTerminal(happy_var_1)) => { withNodeInfo(p, happy_var_1, partial_1!(CEnumeration, Some(happy_var_3), None, happy_var_2))
        }.map(HappyAbsSyn61),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_215(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_1(p, 62, happyReduction_215, i)
}

fn happyReduction_215(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn63(happy_var_1) => HappyAbsSyn62(singleton(happy_var_1)),
        _ => notHappyAtAll()
    }
}


fn happyReduce_216(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_3(p, 62, happyReduction_216, i)
}

fn happyReduction_216(happy_x_3: HappyAbsSyn, happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_3, happy_x_2, happy_x_1) {
        (HappyAbsSyn63(happy_var_3), _, HappyAbsSyn62(happy_var_1)) => HappyAbsSyn62(snoc(happy_var_1, happy_var_3)),
        _ => notHappyAtAll()
    }
}


fn happyReduce_217(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_1(p, 63, happyReduction_217, i)
}

fn happyReduction_217(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn131(happy_var_1) => HappyAbsSyn63((happy_var_1, None)),
        _ => notHappyAtAll()
    }
}


fn happyReduce_218(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_2(p, 63, happyReduction_218, i)
}

fn happyReduction_218(happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_2, happy_x_1) {
        (_, HappyAbsSyn131(happy_var_1)) => HappyAbsSyn63((happy_var_1, None)),
        _ => notHappyAtAll()
    }
}


fn happyReduce_219(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyReduce(p, 4, 63, happyReduction_219, i)
}

fn happyReduction_219(p: &mut Parser) {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn100(happy_var_4), _, _, HappyAbsSyn131(happy_var_1)) => {            p.stack.push(HappyAbsSyn63((happy_var_1, Some(happy_var_4)))); }
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_220(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_3(p, 63, happyReduction_220, i)
}

fn happyReduction_220(happy_x_3: HappyAbsSyn, happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_3, happy_x_2, happy_x_1) {
        (HappyAbsSyn100(happy_var_3), _, HappyAbsSyn131(happy_var_1)) => HappyAbsSyn63((happy_var_1, Some(happy_var_3))),
        _ => notHappyAtAll()
    }
}


fn happyReduce_221(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 1, 64, happyReduction_221, i)
}

fn happyReduction_221(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap()) {
        HappyTerminal(happy_var_1) => { withNodeInfo(p, happy_var_1, partial_1!(CConstQual))
        }.map(HappyAbsSyn64),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_222(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 1, 64, happyReduction_222, i)
}

fn happyReduction_222(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap()) {
        HappyTerminal(happy_var_1) => { withNodeInfo(p, happy_var_1, partial_1!(CVolatQual))
        }.map(HappyAbsSyn64),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_223(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 1, 64, happyReduction_223, i)
}

fn happyReduction_223(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap()) {
        HappyTerminal(happy_var_1) => { withNodeInfo(p, happy_var_1, partial_1!(CRestrQual))
        }.map(HappyAbsSyn64),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_224(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 1, 64, happyReduction_224, i)
}

fn happyReduction_224(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap()) {
        HappyTerminal(happy_var_1) => { withNodeInfo(p, happy_var_1, partial_1!(CNullableQual))
        }.map(HappyAbsSyn64),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_225(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 1, 64, happyReduction_225, i)
}

fn happyReduction_225(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap()) {
        HappyTerminal(happy_var_1) => { withNodeInfo(p, happy_var_1, partial_1!(CNonnullQual))
        }.map(HappyAbsSyn64),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_226(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 1, 64, happyReduction_226, i)
}

fn happyReduction_226(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap()) {
        HappyTerminal(happy_var_1) => { withNodeInfo(p, happy_var_1, partial_1!(CAtomicQual))
        }.map(HappyAbsSyn64),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_227(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_2(p, 65, happyReduction_227, i)
}

fn happyReduction_227(happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_2, happy_x_1) {
        (HappyAbsSyn64(happy_var_2), HappyAbsSyn132(happy_var_1)) => HappyAbsSyn65(snoc(reverseList(__map!(CAttrQual, happy_var_1)), happy_var_2)),
        _ => notHappyAtAll()
    }
}


fn happyReduce_228(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_2(p, 65, happyReduction_228, i)
}

fn happyReduction_228(happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_2, happy_x_1) {
        (HappyAbsSyn64(happy_var_2), HappyAbsSyn65(happy_var_1)) => HappyAbsSyn65(snoc(happy_var_1, happy_var_2)),
        _ => notHappyAtAll()
    }
}


fn happyReduce_229(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_3(p, 65, happyReduction_229, i)
}

fn happyReduction_229(happy_x_3: HappyAbsSyn, happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_3, happy_x_2, happy_x_1) {
        (HappyAbsSyn64(happy_var_3), HappyAbsSyn132(happy_var_2), HappyAbsSyn65(happy_var_1)) => HappyAbsSyn65(snoc(rappend(happy_var_1, __map!(CAttrQual, happy_var_2)), happy_var_3)),
        _ => notHappyAtAll()
    }
}


fn happyReduce_230(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_1(p, 66, happyReduction_230, i)
}

fn happyReduction_230(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn66(happy_var_1) => HappyAbsSyn66(happy_var_1),
        _ => notHappyAtAll()
    }
}


fn happyReduce_231(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_1(p, 66, happyReduction_231, i)
}

fn happyReduction_231(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn66(happy_var_1) => HappyAbsSyn66(happy_var_1),
        _ => notHappyAtAll()
    }
}


fn happyReduce_232(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_0(p, 67, happyReduction_232(), i)
}

fn happyReduction_232() -> HappyAbsSyn {
    HappyAbsSyn67(None)
}


fn happyReduce_233(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyReduce(p, 4, 67, happyReduction_233, i)
}

fn happyReduction_233(p: &mut Parser) {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, HappyAbsSyn128(happy_var_3), _, _) => {            p.stack.push(HappyAbsSyn67(Some(happy_var_3))); }
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_234(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_1(p, 68, happyReduction_234, i)
}

fn happyReduction_234(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn66(happy_var_1) => HappyAbsSyn66(happy_var_1),
        _ => notHappyAtAll()
    }
}


fn happyReduce_235(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_1(p, 68, happyReduction_235, i)
}

fn happyReduction_235(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn66(happy_var_1) => HappyAbsSyn66(happy_var_1),
        _ => notHappyAtAll()
    }
}


fn happyReduce_236(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 1, 69, happyReduction_236, i)
}

fn happyReduction_236(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap()) {
        HappyTerminal(CTokTyIdent(_, happy_var_1)) => { withNodeInfo(p, happy_var_1.clone(), partial_1!(mkVarDeclr, happy_var_1))
        }.map(HappyAbsSyn66),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_237(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 2, 69, happyReduction_237, i)
}

fn happyReduction_237(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn88(happy_var_2), HappyTerminal(CTokTyIdent(_, happy_var_1))) => { withNodeInfo(p, happy_var_1.clone(), box move |at| { happy_var_2(mkVarDeclr(happy_var_1, at)) })
        }.map(HappyAbsSyn66),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_238(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_1(p, 69, happyReduction_238, i)
}

fn happyReduction_238(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn66(happy_var_1) => HappyAbsSyn66(happy_var_1),
        _ => notHappyAtAll()
    }
}


fn happyReduce_239(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_1(p, 70, happyReduction_239, i)
}

fn happyReduction_239(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn66(happy_var_1) => HappyAbsSyn66(happy_var_1),
        _ => notHappyAtAll()
    }
}


fn happyReduce_240(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 2, 70, happyReduction_240, i)
}

fn happyReduction_240(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn66(happy_var_2), HappyTerminal(happy_var_1)) => { withNodeInfo(p, happy_var_1, partial_1!(ptrDeclr, happy_var_2, vec![]))
        }.map(HappyAbsSyn66),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_241(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 3, 70, happyReduction_241, i)
}

fn happyReduction_241(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn66(happy_var_3), HappyAbsSyn132(happy_var_2), HappyTerminal(happy_var_1)) => { withAttribute(p, happy_var_1, happy_var_2, partial_1!(ptrDeclr, happy_var_3, vec![]))
        }.map(HappyAbsSyn66),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_242(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 3, 70, happyReduction_242, i)
}

fn happyReduction_242(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn66(happy_var_3), HappyAbsSyn65(happy_var_2), HappyTerminal(happy_var_1)) => { withNodeInfo(p, happy_var_1, partial_1!(ptrDeclr, happy_var_3, reverse(happy_var_2)))
        }.map(HappyAbsSyn66),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_243(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 4, 70, happyReduction_243, i)
}

fn happyReduction_243(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn66(happy_var_4), HappyAbsSyn132(happy_var_3), HappyAbsSyn65(happy_var_2), HappyTerminal(happy_var_1)) => { withAttribute(p, happy_var_1, happy_var_3, partial_1!(ptrDeclr, happy_var_4, reverse(happy_var_2)))
        }.map(HappyAbsSyn66),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_244(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_3(p, 71, happyReduction_244, i)
}

fn happyReduction_244(happy_x_3: HappyAbsSyn, happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_3, happy_x_2, happy_x_1) {
        (_, HappyAbsSyn66(happy_var_2), _) => HappyAbsSyn66(happy_var_2),
        _ => notHappyAtAll()
    }
}


fn happyReduce_245(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyReduce(p, 4, 71, happyReduction_245, i)
}

fn happyReduction_245(p: &mut Parser) {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn88(happy_var_4), _, HappyAbsSyn66(happy_var_2), _) => {            p.stack.push(HappyAbsSyn66(happy_var_4(happy_var_2))); }
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_246(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyReduce(p, 4, 71, happyReduction_246, i)
}

fn happyReduction_246(p: &mut Parser) {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, HappyAbsSyn66(happy_var_3), HappyAbsSyn132(happy_var_2), _) => {            p.stack.push(HappyAbsSyn66(appendDeclrAttrs(happy_var_2, happy_var_3))); }
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_247(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyReduce(p, 5, 71, happyReduction_247, i)
}

fn happyReduction_247(p: &mut Parser) {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn88(happy_var_5), _, HappyAbsSyn66(happy_var_3), HappyAbsSyn132(happy_var_2), _) => {            p.stack.push(HappyAbsSyn66(appendDeclrAttrs(happy_var_2, happy_var_5(happy_var_3)))); }
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_248(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_1(p, 72, happyReduction_248, i)
}

fn happyReduction_248(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn66(happy_var_1) => HappyAbsSyn66(happy_var_1),
        _ => notHappyAtAll()
    }
}


fn happyReduce_249(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 4, 72, happyReduction_249, i)
}

fn happyReduction_249(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, HappyAbsSyn66(happy_var_3), _, HappyTerminal(happy_var_1)) => { withNodeInfo(p, happy_var_1, partial_1!(ptrDeclr, happy_var_3, vec![]))
        }.map(HappyAbsSyn66),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_250(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 5, 72, happyReduction_250, i)
}

fn happyReduction_250(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, HappyAbsSyn66(happy_var_4), _, HappyAbsSyn65(happy_var_2), HappyTerminal(happy_var_1)) => { withNodeInfo(p, happy_var_1, partial_1!(ptrDeclr, happy_var_4, reverse(happy_var_2)))
        }.map(HappyAbsSyn66),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_251(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 6, 72, happyReduction_251, i)
}

fn happyReduction_251(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, HappyAbsSyn66(happy_var_5), _, HappyAbsSyn132(happy_var_3), HappyAbsSyn65(happy_var_2), HappyTerminal(happy_var_1)) => { withAttribute(p, happy_var_1, happy_var_3, partial_1!(ptrDeclr, happy_var_5, reverse(happy_var_2)))
        }.map(HappyAbsSyn66),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_252(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 2, 72, happyReduction_252, i)
}

fn happyReduction_252(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn66(happy_var_2), HappyTerminal(happy_var_1)) => { withNodeInfo(p, happy_var_1, partial_1!(ptrDeclr, happy_var_2, vec![]))
        }.map(HappyAbsSyn66),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_253(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 3, 72, happyReduction_253, i)
}

fn happyReduction_253(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn66(happy_var_3), HappyAbsSyn65(happy_var_2), HappyTerminal(happy_var_1)) => { withNodeInfo(p, happy_var_1, partial_1!(ptrDeclr, happy_var_3, reverse(happy_var_2)))
        }.map(HappyAbsSyn66),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_254(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 4, 72, happyReduction_254, i)
}

fn happyReduction_254(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn66(happy_var_4), HappyAbsSyn132(happy_var_3), HappyAbsSyn65(happy_var_2), HappyTerminal(happy_var_1)) => { withAttribute(p, happy_var_1, happy_var_3, partial_1!(ptrDeclr, happy_var_4, reverse(happy_var_2)))
        }.map(HappyAbsSyn66),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_255(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_3(p, 73, happyReduction_255, i)
}

fn happyReduction_255(happy_x_3: HappyAbsSyn, happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_3, happy_x_2, happy_x_1) {
        (_, HappyAbsSyn66(happy_var_2), _) => HappyAbsSyn66(happy_var_2),
        _ => notHappyAtAll()
    }
}


fn happyReduce_256(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyReduce(p, 4, 73, happyReduction_256, i)
}

fn happyReduction_256(p: &mut Parser) {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, HappyAbsSyn88(happy_var_3), HappyAbsSyn66(happy_var_2), _) => {            p.stack.push(HappyAbsSyn66(happy_var_3(happy_var_2))); }
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_257(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyReduce(p, 4, 73, happyReduction_257, i)
}

fn happyReduction_257(p: &mut Parser) {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn88(happy_var_4), _, HappyAbsSyn66(happy_var_2), _) => {            p.stack.push(HappyAbsSyn66(happy_var_4(happy_var_2))); }
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_258(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 1, 74, happyReduction_258, i)
}

fn happyReduction_258(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap()) {
        HappyTerminal(CTokTyIdent(_, happy_var_1)) => { withNodeInfo(p, happy_var_1.clone(), partial_1!(mkVarDeclr, happy_var_1))
        }.map(HappyAbsSyn66),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_259(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_3(p, 74, happyReduction_259, i)
}

fn happyReduction_259(happy_x_3: HappyAbsSyn, happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_3, happy_x_2, happy_x_1) {
        (_, HappyAbsSyn66(happy_var_2), _) => HappyAbsSyn66(happy_var_2),
        _ => notHappyAtAll()
    }
}


fn happyReduce_260(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_1(p, 75, happyReduction_260, i)
}

fn happyReduction_260(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn66(happy_var_1) => HappyAbsSyn66(happy_var_1),
        _ => notHappyAtAll()
    }
}


fn happyReduce_261(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_1(p, 75, happyReduction_261, i)
}

fn happyReduction_261(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn66(happy_var_1) => HappyAbsSyn66(happy_var_1),
        _ => notHappyAtAll()
    }
}


fn happyReduce_262(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_1(p, 76, happyReduction_262, i)
}

fn happyReduction_262(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn66(happy_var_1) => HappyAbsSyn66(happy_var_1),
        _ => notHappyAtAll()
    }
}


fn happyReduce_263(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 2, 76, happyReduction_263, i)
}

fn happyReduction_263(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn66(happy_var_2), HappyTerminal(happy_var_1)) => { withNodeInfo(p, happy_var_1, partial_1!(ptrDeclr, happy_var_2, vec![]))
        }.map(HappyAbsSyn66),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_264(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 3, 76, happyReduction_264, i)
}

fn happyReduction_264(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn66(happy_var_3), HappyAbsSyn132(happy_var_2), HappyTerminal(happy_var_1)) => { withAttribute(p, happy_var_1, happy_var_2, partial_1!(ptrDeclr, happy_var_3, vec![]))
        }.map(HappyAbsSyn66),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_265(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 3, 76, happyReduction_265, i)
}

fn happyReduction_265(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn66(happy_var_3), HappyAbsSyn65(happy_var_2), HappyTerminal(happy_var_1)) => { withNodeInfo(p, happy_var_1, partial_1!(ptrDeclr, happy_var_3, reverse(happy_var_2)))
        }.map(HappyAbsSyn66),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_266(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 4, 76, happyReduction_266, i)
}

fn happyReduction_266(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn66(happy_var_4), HappyAbsSyn132(happy_var_3), HappyAbsSyn65(happy_var_2), HappyTerminal(happy_var_1)) => { withAttribute(p, happy_var_1, happy_var_3, partial_1!(ptrDeclr, happy_var_4, reverse(happy_var_2)))
        }.map(HappyAbsSyn66),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_267(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_2(p, 77, happyReduction_267, i)
}

fn happyReduction_267(happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_2, happy_x_1) {
        (HappyAbsSyn88(happy_var_2), HappyAbsSyn66(happy_var_1)) => HappyAbsSyn66(happy_var_2(happy_var_1)),
        _ => notHappyAtAll()
    }
}


fn happyReduce_268(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_3(p, 77, happyReduction_268, i)
}

fn happyReduction_268(happy_x_3: HappyAbsSyn, happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_3, happy_x_2, happy_x_1) {
        (_, HappyAbsSyn66(happy_var_2), _) => HappyAbsSyn66(happy_var_2),
        _ => notHappyAtAll()
    }
}


fn happyReduce_269(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyReduce(p, 4, 77, happyReduction_269, i)
}

fn happyReduction_269(p: &mut Parser) {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn88(happy_var_4), _, HappyAbsSyn66(happy_var_2), _) => {            p.stack.push(HappyAbsSyn66(happy_var_4(happy_var_2))); }
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_270(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyReduce(p, 4, 77, happyReduction_270, i)
}

fn happyReduction_270(p: &mut Parser) {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, HappyAbsSyn66(happy_var_3), HappyAbsSyn132(happy_var_2), _) => {            p.stack.push(HappyAbsSyn66(appendDeclrAttrs(happy_var_2, happy_var_3))); }
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_271(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyReduce(p, 5, 77, happyReduction_271, i)
}

fn happyReduction_271(p: &mut Parser) {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn88(happy_var_5), _, HappyAbsSyn66(happy_var_3), HappyAbsSyn132(happy_var_2), _) => {            p.stack.push(HappyAbsSyn66(appendDeclrAttrs(happy_var_2, happy_var_5(happy_var_3)))); }
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_272(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 1, 78, happyReduction_272, i)
}

fn happyReduction_272(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap()) {
        HappyTerminal(CTokIdent(_, happy_var_1)) => { withNodeInfo(p, happy_var_1.clone(), partial_1!(mkVarDeclr, happy_var_1))
        }.map(HappyAbsSyn66),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_273(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_3(p, 78, happyReduction_273, i)
}

fn happyReduction_273(happy_x_3: HappyAbsSyn, happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_3, happy_x_2, happy_x_1) {
        (_, HappyAbsSyn66(happy_var_2), _) => HappyAbsSyn66(happy_var_2),
        _ => notHappyAtAll()
    }
}


fn happyReduce_274(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyReduce(p, 4, 78, happyReduction_274, i)
}

fn happyReduction_274(p: &mut Parser) {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, HappyAbsSyn66(happy_var_3), HappyAbsSyn132(happy_var_2), _) => {            p.stack.push(HappyAbsSyn66(appendDeclrAttrs(happy_var_2, happy_var_3))); }
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_275(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_1(p, 79, happyReduction_275, i)
}

fn happyReduction_275(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn66(happy_var_1) => HappyAbsSyn11(reverseDeclr(happy_var_1)),
        _ => notHappyAtAll()
    }
}


fn happyReduce_276(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_1(p, 80, happyReduction_276, i)
}

fn happyReduction_276(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn66(happy_var_1) => HappyAbsSyn66(happy_var_1),
        _ => notHappyAtAll()
    }
}


fn happyReduce_277(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 2, 80, happyReduction_277, i)
}

fn happyReduction_277(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn66(happy_var_2), HappyTerminal(happy_var_1)) => { withNodeInfo(p, happy_var_1, partial_1!(ptrDeclr, happy_var_2, vec![]))
        }.map(HappyAbsSyn66),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_278(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 3, 80, happyReduction_278, i)
}

fn happyReduction_278(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn66(happy_var_3), HappyAbsSyn65(happy_var_2), HappyTerminal(happy_var_1)) => { withNodeInfo(p, happy_var_1, partial_1!(ptrDeclr, happy_var_3, reverse(happy_var_2)))
        }.map(HappyAbsSyn66),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_279(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 4, 81, happyReduction_279, i)
}

fn happyReduction_279(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, HappyAbsSyn21(happy_var_3), _, HappyAbsSyn66(happy_var_1)) => { withNodeInfo(p, happy_var_1.clone(), partial_1!(funDeclr, happy_var_1, Left(reverse(happy_var_3)), vec![]))
        }.map(HappyAbsSyn66),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_280(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_3(p, 81, happyReduction_280, i)
}

fn happyReduction_280(happy_x_3: HappyAbsSyn, happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_3, happy_x_2, happy_x_1) {
        (_, HappyAbsSyn66(happy_var_2), _) => HappyAbsSyn66(happy_var_2),
        _ => notHappyAtAll()
    }
}


fn happyReduce_281(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyReduce(p, 4, 81, happyReduction_281, i)
}

fn happyReduction_281(p: &mut Parser) {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn88(happy_var_4), _, HappyAbsSyn66(happy_var_2), _) => {            p.stack.push(HappyAbsSyn66(happy_var_4(happy_var_2))); }
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_282(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_0(p, 82, happyReduction_282(), i)
}

fn happyReduction_282() -> HappyAbsSyn {
    HappyAbsSyn82((vec![], false))
}


fn happyReduce_283(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_1(p, 82, happyReduction_283, i)
}

fn happyReduction_283(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn33(happy_var_1) => HappyAbsSyn82((reverse(happy_var_1), false)),
        _ => notHappyAtAll()
    }
}


fn happyReduce_284(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_3(p, 82, happyReduction_284, i)
}

fn happyReduction_284(happy_x_3: HappyAbsSyn, happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_3, happy_x_2, happy_x_1) {
        (_, _, HappyAbsSyn33(happy_var_1)) => HappyAbsSyn82((reverse(happy_var_1), true)),
        _ => notHappyAtAll()
    }
}


fn happyReduce_285(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_1(p, 83, happyReduction_285, i)
}

fn happyReduction_285(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn32(happy_var_1) => HappyAbsSyn33(singleton(happy_var_1)),
        _ => notHappyAtAll()
    }
}


fn happyReduce_286(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_3(p, 83, happyReduction_286, i)
}

fn happyReduction_286(happy_x_3: HappyAbsSyn, happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_3, happy_x_2, happy_x_1) {
        (HappyAbsSyn32(happy_var_3), _, HappyAbsSyn33(happy_var_1)) => HappyAbsSyn33(snoc(happy_var_1, happy_var_3)),
        _ => notHappyAtAll()
    }
}


fn happyReduce_287(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 1, 84, happyReduction_287, i)
}

fn happyReduction_287(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap()) {
        HappyAbsSyn37(happy_var_1) => { withNodeInfo(p, happy_var_1.clone(), partial_1!(CDecl, happy_var_1, vec![]))
        }.map(HappyAbsSyn32),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_288(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 2, 84, happyReduction_288, i)
}

fn happyReduction_288(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn66(happy_var_2), HappyAbsSyn37(happy_var_1)) => { withNodeInfo(p, happy_var_1.clone(), partial_1!(CDecl, happy_var_1, vec![(Some(reverseDeclr(happy_var_2)), None, None)]))
        }.map(HappyAbsSyn32),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_289(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 3, 84, happyReduction_289, i)
}

fn happyReduction_289(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn132(happy_var_3), HappyAbsSyn66(happy_var_2), HappyAbsSyn37(happy_var_1)) => { withNodeInfo(p, happy_var_1.clone(), partial_1!(CDecl, happy_var_1,
                                               vec![(Some(reverseDeclr(appendDeclrAttrs(happy_var_3, happy_var_2))), None, None)]))
        }.map(HappyAbsSyn32),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_290(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 3, 84, happyReduction_290, i)
}

fn happyReduction_290(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn132(happy_var_3), HappyAbsSyn66(happy_var_2), HappyAbsSyn37(happy_var_1)) => { withNodeInfo(p, happy_var_1.clone(), partial_1!(CDecl, happy_var_1,
                                               vec![(Some(reverseDeclr(appendDeclrAttrs(happy_var_3, happy_var_2))), None, None)]))
        }.map(HappyAbsSyn32),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_291(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 1, 84, happyReduction_291, i)
}

fn happyReduction_291(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap()) {
        HappyAbsSyn38(happy_var_1) => { withNodeInfo(p, happy_var_1.clone(), partial_1!(CDecl, reverse(happy_var_1), vec![]))
        }.map(HappyAbsSyn32),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_292(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 2, 84, happyReduction_292, i)
}

fn happyReduction_292(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn66(happy_var_2), HappyAbsSyn38(happy_var_1)) => { withNodeInfo(p, happy_var_1.clone(), partial_1!(CDecl, reverse(happy_var_1), vec![(Some(reverseDeclr(happy_var_2)), None, None)]))
        }.map(HappyAbsSyn32),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_293(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 3, 84, happyReduction_293, i)
}

fn happyReduction_293(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn132(happy_var_3), HappyAbsSyn66(happy_var_2), HappyAbsSyn38(happy_var_1)) => { withNodeInfo(p, happy_var_1.clone(), partial_1!(CDecl, reverse(happy_var_1),
                                               vec![(Some(reverseDeclr(appendDeclrAttrs(happy_var_3, happy_var_2))), None, None)]))
        }.map(HappyAbsSyn32),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_294(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 1, 84, happyReduction_294, i)
}

fn happyReduction_294(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap()) {
        HappyAbsSyn37(happy_var_1) => { withNodeInfo(p, happy_var_1.clone(), partial_1!(CDecl, happy_var_1, vec![]))
        }.map(HappyAbsSyn32),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_295(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 2, 84, happyReduction_295, i)
}

fn happyReduction_295(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn66(happy_var_2), HappyAbsSyn37(happy_var_1)) => { withNodeInfo(p, happy_var_1.clone(), partial_1!(CDecl, happy_var_1, vec![(Some(reverseDeclr(happy_var_2)), None, None)]))
        }.map(HappyAbsSyn32),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_296(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 3, 84, happyReduction_296, i)
}

fn happyReduction_296(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn132(happy_var_3), HappyAbsSyn66(happy_var_2), HappyAbsSyn37(happy_var_1)) => { withNodeInfo(p, happy_var_1.clone(), partial_1!(CDecl, happy_var_1,
                                               vec![(Some(reverseDeclr(appendDeclrAttrs(happy_var_3, happy_var_2))), None, None)]))
        }.map(HappyAbsSyn32),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_297(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 3, 84, happyReduction_297, i)
}

fn happyReduction_297(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn132(happy_var_3), HappyAbsSyn66(happy_var_2), HappyAbsSyn37(happy_var_1)) => { withNodeInfo(p, happy_var_1.clone(), partial_1!(CDecl, happy_var_1,
                                               vec![(Some(reverseDeclr(appendDeclrAttrs(happy_var_3, happy_var_2))), None, None)]))
        }.map(HappyAbsSyn32),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_298(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 1, 84, happyReduction_298, i)
}

fn happyReduction_298(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap()) {
        HappyAbsSyn65(happy_var_1) => { withNodeInfo(p, happy_var_1.clone(), partial_1!(CDecl, liftTypeQuals(happy_var_1), vec![]))
        }.map(HappyAbsSyn32),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_299(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 2, 84, happyReduction_299, i)
}

fn happyReduction_299(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn132(happy_var_2), HappyAbsSyn65(happy_var_1)) => { withNodeInfo(p, happy_var_1.clone(), partial_1!(CDecl, __op_addadd(liftTypeQuals(happy_var_1), liftCAttrs(happy_var_2)), vec![]))
        }.map(HappyAbsSyn32),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_300(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 2, 84, happyReduction_300, i)
}

fn happyReduction_300(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn66(happy_var_2), HappyAbsSyn65(happy_var_1)) => { withNodeInfo(p, happy_var_1.clone(), partial_1!(CDecl, liftTypeQuals(happy_var_1),
                                               vec![(Some(reverseDeclr(happy_var_2)), None, None)]))
        }.map(HappyAbsSyn32),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_301(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 3, 84, happyReduction_301, i)
}

fn happyReduction_301(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn132(happy_var_3), HappyAbsSyn66(happy_var_2), HappyAbsSyn65(happy_var_1)) => { withNodeInfo(p, happy_var_1.clone(), partial_1!(CDecl, liftTypeQuals(happy_var_1),
                                               vec![(Some(reverseDeclr(appendDeclrAttrs(happy_var_3, happy_var_2))), None, None)]))
        }.map(HappyAbsSyn32),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_302(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_1(p, 85, happyReduction_302, i)
}

fn happyReduction_302(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyTerminal(CTokIdent(_, happy_var_1)) => HappyAbsSyn21(singleton(happy_var_1)),
        _ => notHappyAtAll()
    }
}


fn happyReduce_303(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_3(p, 85, happyReduction_303, i)
}

fn happyReduction_303(happy_x_3: HappyAbsSyn, happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_3, happy_x_2, happy_x_1) {
        (HappyTerminal(CTokIdent(_, happy_var_3)), _, HappyAbsSyn21(happy_var_1)) => HappyAbsSyn21(snoc(happy_var_1, happy_var_3)),
        _ => notHappyAtAll()
    }
}


fn happyReduce_304(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 1, 86, happyReduction_304, i)
}

fn happyReduction_304(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap()) {
        HappyAbsSyn37(happy_var_1) => { withNodeInfo(p, happy_var_1.clone(), partial_1!(CDecl, happy_var_1, vec![]))
        }.map(HappyAbsSyn32),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_305(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 2, 86, happyReduction_305, i)
}

fn happyReduction_305(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn66(happy_var_2), HappyAbsSyn37(happy_var_1)) => { withNodeInfo(p, happy_var_1.clone(), partial_1!(CDecl, happy_var_1, vec![(Some(reverseDeclr(happy_var_2)), None, None)]))
        }.map(HappyAbsSyn32),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_306(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 2, 86, happyReduction_306, i)
}

fn happyReduction_306(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn132(happy_var_2), HappyAbsSyn65(happy_var_1)) => { withNodeInfo(p, happy_var_1.clone(), partial_1!(CDecl, __op_addadd(liftTypeQuals(happy_var_1), liftCAttrs(happy_var_2)), vec![]))
        }.map(HappyAbsSyn32),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_307(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 2, 86, happyReduction_307, i)
}

fn happyReduction_307(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn66(happy_var_2), HappyAbsSyn65(happy_var_1)) => { withNodeInfo(p, happy_var_1.clone(), partial_1!(CDecl, liftTypeQuals(happy_var_1),
                                               vec![(Some(reverseDeclr(happy_var_2)), None, None)]))
        }.map(HappyAbsSyn32),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_308(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_1(p, 87, happyReduction_308, i)
}

fn happyReduction_308(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn66(happy_var_1) => HappyAbsSyn66(happy_var_1),
        _ => notHappyAtAll()
    }
}


fn happyReduce_309(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_1(p, 87, happyReduction_309, i)
}

fn happyReduction_309(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn66(happy_var_1) => HappyAbsSyn66(happy_var_1),
        _ => notHappyAtAll()
    }
}


fn happyReduce_310(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_1(p, 87, happyReduction_310, i)
}

fn happyReduction_310(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn88(happy_var_1) => HappyAbsSyn66(happy_var_1(emptyDeclr())),
        _ => notHappyAtAll()
    }
}


fn happyReduce_311(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_1(p, 88, happyReduction_311, i)
}

fn happyReduction_311(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn88(happy_var_1) => HappyAbsSyn88(happy_var_1),
        _ => notHappyAtAll()
    }
}


fn happyReduce_312(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 3, 88, happyReduction_312, i)
}

fn happyReduction_312(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, HappyAbsSyn82(happy_var_2), HappyTerminal(happy_var_1)) => {
            withNodeInfo(p, happy_var_1, box move |at: NodeInfo| {
                let a: Rc<Box<Fn(CDeclrR) -> CDeclrR>> = Rc::new(box move |declr| {
                    let (params, variadic) = happy_var_2.clone();
                    funDeclr(declr, (Right((params, variadic))), vec![], at.clone())
                });
                a
            })
        }.map(HappyAbsSyn88),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_313(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_1(p, 89, happyReduction_313, i)
}

fn happyReduction_313(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn88(happy_var_1) => HappyAbsSyn88(happy_var_1),
        _ => notHappyAtAll()
    }
}


fn happyReduce_314(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_2(p, 89, happyReduction_314, i)
}

fn happyReduction_314(happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_2, happy_x_1) {
        (HappyAbsSyn88(happy_var_2), HappyAbsSyn88(happy_var_1)) => HappyAbsSyn88(Rc::new(box move |decl| { happy_var_2(happy_var_1(decl)) })),
        _ => notHappyAtAll()
    }
}


fn happyReduce_315(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 3, 90, happyReduction_315, i)
}

fn happyReduction_315(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, HappyAbsSyn124(happy_var_2), HappyTerminal(happy_var_1)) => {
            withNodeInfo(p, happy_var_1.clone(), box move |at: NodeInfo| {
                let a: Rc<Box<Fn(CDeclrR) -> CDeclrR>> = Rc::new(box move |declr| {
                    arrDeclr(declr, vec![], false, false, happy_var_2.clone(), at.clone())
                });
                a
            })
        }.map(HappyAbsSyn88),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_316(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 4, 90, happyReduction_316, i)
}

fn happyReduction_316(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, HappyAbsSyn124(happy_var_3), HappyAbsSyn132(happy_var_2), HappyTerminal(happy_var_1)) => { withAttributePF(p, happy_var_1, happy_var_2, box move |at, declr|
                           arrDeclr(declr, vec![], false, false, happy_var_3.clone(), at))
        }.map(HappyAbsSyn88),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_317(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 4, 90, happyReduction_317, i)
}

fn happyReduction_317(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, HappyAbsSyn124(happy_var_3), HappyAbsSyn65(happy_var_2), HappyTerminal(happy_var_1)) => {
            withNodeInfo(p, happy_var_1.clone(), box move |at: NodeInfo| {
                let a: Rc<Box<Fn(CDeclrR) -> CDeclrR>> = Rc::new(
                    box move |declr| { arrDeclr(declr, reverse(happy_var_2.clone()),
                                                false, false, happy_var_3.clone(), at.clone()) });
                a
            })
        }.map(HappyAbsSyn88),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_318(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 5, 90, happyReduction_318, i)
}

fn happyReduction_318(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, HappyAbsSyn124(happy_var_4), HappyAbsSyn132(happy_var_3), HappyAbsSyn65(happy_var_2), HappyTerminal(happy_var_1)) => { withAttributePF(p, happy_var_1, happy_var_3, box move |at, declr|
                           arrDeclr(declr, reverse(happy_var_2.clone()), false, false, happy_var_4.clone(), at))
        }.map(HappyAbsSyn88),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_319(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 5, 90, happyReduction_319, i)
}

fn happyReduction_319(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, HappyAbsSyn100(happy_var_4), HappyAbsSyn132(happy_var_3), _, HappyTerminal(happy_var_1)) => { withAttributePF(p, happy_var_1, happy_var_3, box move |at, declr|
                           arrDeclr(declr, vec![], false, true, Some(happy_var_4.clone()), at))
        }.map(HappyAbsSyn88),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_320(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 6, 90, happyReduction_320, i)
}

fn happyReduction_320(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, HappyAbsSyn100(happy_var_5), HappyAbsSyn132(happy_var_4), HappyAbsSyn65(happy_var_3), _, HappyTerminal(happy_var_1)) => { withAttributePF(p, happy_var_1, happy_var_4, box move |at, declr|
                           arrDeclr(declr, reverse(happy_var_3.clone()), false, true, Some(happy_var_5.clone()), at))
        }.map(HappyAbsSyn88),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_321(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 7, 90, happyReduction_321, i)
}

fn happyReduction_321(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, HappyAbsSyn100(happy_var_6), HappyAbsSyn132(happy_var_5), _, HappyAbsSyn132(happy_var_3), HappyAbsSyn65(happy_var_2), HappyTerminal(happy_var_1)) => { withAttributePF(p, happy_var_1, __op_addadd(happy_var_3, happy_var_5), box move |at, declr|
                           arrDeclr(declr, reverse(happy_var_2.clone()), false, true, Some(happy_var_6.clone()), at))
        }.map(HappyAbsSyn88),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_322(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 4, 90, happyReduction_322, i)
}

fn happyReduction_322(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, HappyAbsSyn132(happy_var_3), _, HappyTerminal(happy_var_1)) => { withAttributePF(p, happy_var_1, happy_var_3, box move |at, declr|
                           arrDeclr(declr, vec![], true, false, None, at))
        }.map(HappyAbsSyn88),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_323(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 5, 90, happyReduction_323, i)
}

fn happyReduction_323(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, HappyAbsSyn132(happy_var_4), _, HappyAbsSyn132(happy_var_2), HappyTerminal(happy_var_1)) => { withAttributePF(p, happy_var_1, __op_addadd(happy_var_2, happy_var_4), box move |at, declr|
                           arrDeclr(declr, vec![], true, false, None, at))
        }.map(HappyAbsSyn88),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_324(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 5, 90, happyReduction_324, i)
}

fn happyReduction_324(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, HappyAbsSyn132(happy_var_4), _, HappyAbsSyn65(happy_var_2), HappyTerminal(happy_var_1)) => { withAttributePF(p, happy_var_1, happy_var_4, box move |at, declr|
                           arrDeclr(declr, reverse(happy_var_2.clone()), true, false, None, at))
        }.map(HappyAbsSyn88),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_325(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 6, 90, happyReduction_325, i)
}

fn happyReduction_325(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, HappyAbsSyn132(happy_var_5), _, HappyAbsSyn132(happy_var_3), HappyAbsSyn65(happy_var_2), HappyTerminal(happy_var_1)) => { withAttributePF(p, happy_var_1, __op_addadd(happy_var_3, happy_var_5), box move |at, declr|
                           arrDeclr(declr, reverse(happy_var_2.clone()), true, false, None, at))
        }.map(HappyAbsSyn88),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_326(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 1, 91, happyReduction_326, i)
}

fn happyReduction_326(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap()) {
        HappyTerminal(happy_var_1) => { withNodeInfo(p, happy_var_1, partial_1!(ptrDeclr, emptyDeclr(), vec![]))
        }.map(HappyAbsSyn66),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_327(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 3, 91, happyReduction_327, i)
}

fn happyReduction_327(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn132(happy_var_3), HappyAbsSyn65(happy_var_2), HappyTerminal(happy_var_1)) => { withAttribute(p, happy_var_1, happy_var_3, partial_1!(ptrDeclr, emptyDeclr(), reverse(happy_var_2)))
        }.map(HappyAbsSyn66),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_328(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 2, 91, happyReduction_328, i)
}

fn happyReduction_328(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn66(happy_var_2), HappyTerminal(happy_var_1)) => { withNodeInfo(p, happy_var_1, partial_1!(ptrDeclr, happy_var_2, vec![]))
        }.map(HappyAbsSyn66),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_329(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 3, 91, happyReduction_329, i)
}

fn happyReduction_329(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn66(happy_var_3), HappyAbsSyn65(happy_var_2), HappyTerminal(happy_var_1)) => { withNodeInfo(p, happy_var_1, partial_1!(ptrDeclr, happy_var_3, reverse(happy_var_2)))
        }.map(HappyAbsSyn66),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_330(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 2, 91, happyReduction_330, i)
}

fn happyReduction_330(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn132(happy_var_2), HappyTerminal(happy_var_1)) => { withAttribute(p, happy_var_1, happy_var_2, partial_1!(ptrDeclr, emptyDeclr(), vec![]))
        }.map(HappyAbsSyn66),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_331(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 3, 91, happyReduction_331, i)
}

fn happyReduction_331(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn66(happy_var_3), HappyAbsSyn132(happy_var_2), HappyTerminal(happy_var_1)) => { withAttribute(p, happy_var_1, happy_var_2, partial_1!(ptrDeclr, happy_var_3, vec![]))
        }.map(HappyAbsSyn66),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_332(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_3(p, 92, happyReduction_332, i)
}

fn happyReduction_332(happy_x_3: HappyAbsSyn, happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_3, happy_x_2, happy_x_1) {
        (_, HappyAbsSyn66(happy_var_2), _) => HappyAbsSyn66(happy_var_2),
        _ => notHappyAtAll()
    }
}


fn happyReduce_333(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_3(p, 92, happyReduction_333, i)
}

fn happyReduction_333(happy_x_3: HappyAbsSyn, happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_3, happy_x_2, happy_x_1) {
        (_, HappyAbsSyn66(happy_var_2), _) => HappyAbsSyn66(happy_var_2),
        _ => notHappyAtAll()
    }
}


fn happyReduce_334(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_3(p, 92, happyReduction_334, i)
}

fn happyReduction_334(happy_x_3: HappyAbsSyn, happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_3, happy_x_2, happy_x_1) {
        (_, HappyAbsSyn88(happy_var_2), _) => HappyAbsSyn66(happy_var_2(emptyDeclr())),
        _ => notHappyAtAll()
    }
}


fn happyReduce_335(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyReduce(p, 4, 92, happyReduction_335, i)
}

fn happyReduction_335(p: &mut Parser) {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn88(happy_var_4), _, HappyAbsSyn66(happy_var_2), _) => {            p.stack.push(HappyAbsSyn66(happy_var_4(happy_var_2))); }
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_336(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyReduce(p, 4, 92, happyReduction_336, i)
}

fn happyReduction_336(p: &mut Parser) {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, HappyAbsSyn66(happy_var_3), HappyAbsSyn132(happy_var_2), _) => {            p.stack.push(HappyAbsSyn66(appendDeclrAttrs(happy_var_2, happy_var_3))); }
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_337(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyReduce(p, 4, 92, happyReduction_337, i)
}

fn happyReduction_337(p: &mut Parser) {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, HappyAbsSyn66(happy_var_3), HappyAbsSyn132(happy_var_2), _) => {            p.stack.push(HappyAbsSyn66(appendDeclrAttrs(happy_var_2, happy_var_3))); }
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_338(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyReduce(p, 4, 92, happyReduction_338, i)
}

fn happyReduction_338(p: &mut Parser) {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, HappyAbsSyn88(happy_var_3), HappyAbsSyn132(happy_var_2), _) => {            p.stack.push(HappyAbsSyn66(appendDeclrAttrs(happy_var_2, happy_var_3(emptyDeclr())))); }
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_339(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyReduce(p, 5, 92, happyReduction_339, i)
}

fn happyReduction_339(p: &mut Parser) {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn88(happy_var_5), _, HappyAbsSyn66(happy_var_3), HappyAbsSyn132(happy_var_2), _) => {            p.stack.push(HappyAbsSyn66(appendDeclrAttrs(happy_var_2, happy_var_5(happy_var_3)))); }
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_340(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_2(p, 92, happyReduction_340, i)
}

fn happyReduction_340(happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_2, happy_x_1) {
        (HappyAbsSyn132(happy_var_2), HappyAbsSyn66(happy_var_1)) => HappyAbsSyn66(appendDeclrAttrs(happy_var_2, happy_var_1)),
        _ => notHappyAtAll()
    }
}


fn happyReduce_341(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 1, 93, happyReduction_341, i)
}

fn happyReduction_341(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap()) {
        HappyAbsSyn100(happy_var_1) => { withNodeInfo(p, happy_var_1.clone(), partial_1!(CInitExpr, happy_var_1))
        }.map(HappyAbsSyn93),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_342(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 3, 93, happyReduction_342, i)
}

fn happyReduction_342(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, HappyAbsSyn95(happy_var_2), HappyTerminal(happy_var_1)) => { withNodeInfo(p, happy_var_1, partial_1!(CInitList, reverse(happy_var_2)))
        }.map(HappyAbsSyn93),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_343(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 4, 93, happyReduction_343, i)
}

fn happyReduction_343(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, _, HappyAbsSyn95(happy_var_2), HappyTerminal(happy_var_1)) => { withNodeInfo(p, happy_var_1, partial_1!(CInitList, reverse(happy_var_2)))
        }.map(HappyAbsSyn93),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_344(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_0(p, 94, happyReduction_344(), i)
}

fn happyReduction_344() -> HappyAbsSyn {
    HappyAbsSyn94(None)
}


fn happyReduce_345(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_2(p, 94, happyReduction_345, i)
}

fn happyReduction_345(happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_2, happy_x_1) {
        (HappyAbsSyn93(happy_var_2), _) => HappyAbsSyn94(Some(happy_var_2)),
        _ => notHappyAtAll()
    }
}


fn happyReduce_346(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_0(p, 95, happyReduction_346(), i)
}

fn happyReduction_346() -> HappyAbsSyn {
    HappyAbsSyn95(empty())
}


fn happyReduce_347(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_1(p, 95, happyReduction_347, i)
}

fn happyReduction_347(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn93(happy_var_1) => HappyAbsSyn95(singleton((vec![], happy_var_1))),
        _ => notHappyAtAll()
    }
}


fn happyReduce_348(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_2(p, 95, happyReduction_348, i)
}

fn happyReduction_348(happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_2, happy_x_1) {
        (HappyAbsSyn93(happy_var_2), HappyAbsSyn96(happy_var_1)) => HappyAbsSyn95(singleton((happy_var_1, happy_var_2))),
        _ => notHappyAtAll()
    }
}


fn happyReduce_349(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_3(p, 95, happyReduction_349, i)
}

fn happyReduction_349(happy_x_3: HappyAbsSyn, happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_3, happy_x_2, happy_x_1) {
        (HappyAbsSyn93(happy_var_3), _, HappyAbsSyn95(happy_var_1)) => HappyAbsSyn95(snoc(happy_var_1, (vec![], happy_var_3))),
        _ => notHappyAtAll()
    }
}


fn happyReduce_350(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyReduce(p, 4, 95, happyReduction_350, i)
}

fn happyReduction_350(p: &mut Parser) {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn93(happy_var_4), HappyAbsSyn96(happy_var_3), _, HappyAbsSyn95(happy_var_1)) => {            p.stack.push(HappyAbsSyn95(snoc(happy_var_1, (happy_var_3, happy_var_4)))); }
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_351(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_2(p, 96, happyReduction_351, i)
}

fn happyReduction_351(happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_2, happy_x_1) {
        (_, HappyAbsSyn97(happy_var_1)) => HappyAbsSyn96(reverse(happy_var_1)),
        _ => notHappyAtAll()
    }
}


fn happyReduce_352(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 2, 96, happyReduction_352, i)
}

fn happyReduction_352(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, HappyAbsSyn131(happy_var_1)) => { withNodeInfo(p, happy_var_1.clone(), box |_0| vec![CMemberDesig(happy_var_1, _0)])
        }.map(HappyAbsSyn96),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_353(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_1(p, 96, happyReduction_353, i)
}

fn happyReduction_353(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn98(happy_var_1) => HappyAbsSyn96(vec![happy_var_1]),
        _ => notHappyAtAll()
    }
}


fn happyReduce_354(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_1(p, 97, happyReduction_354, i)
}

fn happyReduction_354(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn98(happy_var_1) => HappyAbsSyn97(singleton(happy_var_1)),
        _ => notHappyAtAll()
    }
}


fn happyReduce_355(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_2(p, 97, happyReduction_355, i)
}

fn happyReduction_355(happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_2, happy_x_1) {
        (HappyAbsSyn98(happy_var_2), HappyAbsSyn97(happy_var_1)) => HappyAbsSyn97(snoc(happy_var_1, happy_var_2)),
        _ => notHappyAtAll()
    }
}


fn happyReduce_356(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 3, 98, happyReduction_356, i)
}

fn happyReduction_356(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, HappyAbsSyn100(happy_var_2), HappyTerminal(happy_var_1)) => { withNodeInfo(p, happy_var_1, partial_1!(CArrDesig, happy_var_2))
        }.map(HappyAbsSyn98),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_357(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 2, 98, happyReduction_357, i)
}

fn happyReduction_357(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn131(happy_var_2), HappyTerminal(happy_var_1)) => { withNodeInfo(p, happy_var_1, partial_1!(CMemberDesig, happy_var_2))
        }.map(HappyAbsSyn98),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_358(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_1(p, 98, happyReduction_358, i)
}

fn happyReduction_358(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn98(happy_var_1) => HappyAbsSyn98(happy_var_1),
        _ => notHappyAtAll()
    }
}


fn happyReduce_359(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 5, 99, happyReduction_359, i)
}

fn happyReduction_359(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, HappyAbsSyn100(happy_var_4), _, HappyAbsSyn100(happy_var_2), HappyTerminal(happy_var_1)) => { withNodeInfo(p, happy_var_1, partial_1!(CRangeDesig, happy_var_2, happy_var_4))
        }.map(HappyAbsSyn98),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_360(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 1, 100, happyReduction_360, i)
}

fn happyReduction_360(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap()) {
        HappyTerminal(CTokIdent(_, happy_var_1)) => { withNodeInfo(p, happy_var_1.clone(), partial_1!(CVar, happy_var_1))
        }.map(HappyAbsSyn100),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_361(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_1(p, 100, happyReduction_361, i)
}

fn happyReduction_361(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn127(happy_var_1) => HappyAbsSyn100(CConst(happy_var_1)),
        _ => notHappyAtAll()
    }
}


fn happyReduce_362(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_1(p, 100, happyReduction_362, i)
}

fn happyReduction_362(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn128(happy_var_1) => HappyAbsSyn100(CConst(liftStrLit(happy_var_1))),
        _ => notHappyAtAll()
    }
}


fn happyReduce_363(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_3(p, 100, happyReduction_363, i)
}

fn happyReduction_363(happy_x_3: HappyAbsSyn, happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_3, happy_x_2, happy_x_1) {
        (_, HappyAbsSyn100(happy_var_2), _) => HappyAbsSyn100(happy_var_2),
        _ => notHappyAtAll()
    }
}


fn happyReduce_364(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 6, 100, happyReduction_364, i)
}

fn happyReduction_364(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, HappyAbsSyn101(happy_var_5), _, HappyAbsSyn100(happy_var_3), _, HappyTerminal(happy_var_1)) => { withNodeInfo(p, happy_var_1, partial_1!(CGenericSelection, box happy_var_3, reverse(happy_var_5)))
        }.map(HappyAbsSyn100),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_365(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 3, 100, happyReduction_365, i)
}

fn happyReduction_365(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, HappyAbsSyn12(happy_var_2), HappyTerminal(happy_var_1)) => { withNodeInfo(p, happy_var_1, partial_1!(CStatExpr, box happy_var_2))
        }.map(HappyAbsSyn100),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_366(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 6, 100, happyReduction_366, i)
}

fn happyReduction_366(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, HappyAbsSyn32(happy_var_5), _, HappyAbsSyn100(happy_var_3), _, HappyTerminal(happy_var_1)) => { withNodeInfo(p, happy_var_1, box move |_0| CBuiltinExpr(box CBuiltinVaArg(happy_var_3, happy_var_5, _0)))
        }.map(HappyAbsSyn100),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_367(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 6, 100, happyReduction_367, i)
}

fn happyReduction_367(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, HappyAbsSyn97(happy_var_5), _, HappyAbsSyn32(happy_var_3), _, HappyTerminal(happy_var_1)) => { withNodeInfo(p, happy_var_1, box move |_0| CBuiltinExpr(box CBuiltinOffsetOf(happy_var_3, reverse(happy_var_5), _0)))
        }.map(HappyAbsSyn100),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_368(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 6, 100, happyReduction_368, i)
}

fn happyReduction_368(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, HappyAbsSyn32(happy_var_5), _, HappyAbsSyn32(happy_var_3), _, HappyTerminal(happy_var_1)) => { withNodeInfo(p, happy_var_1, box move |_0| CBuiltinExpr(box CBuiltinTypesCompatible(happy_var_3, happy_var_5, _0)))
        }.map(HappyAbsSyn100),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_369(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_3(p, 101, happyReduction_369, i)
}

fn happyReduction_369(happy_x_3: HappyAbsSyn, happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_3, happy_x_2, happy_x_1) {
        (HappyAbsSyn102(happy_var_3), _, HappyAbsSyn101(happy_var_1)) => HappyAbsSyn101(snoc(happy_var_1, happy_var_3)),
        _ => notHappyAtAll()
    }
}


fn happyReduce_370(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_1(p, 101, happyReduction_370, i)
}

fn happyReduction_370(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn102(happy_var_1) => HappyAbsSyn101(singleton(happy_var_1)),
        _ => notHappyAtAll()
    }
}


fn happyReduce_371(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_3(p, 102, happyReduction_371, i)
}

fn happyReduction_371(happy_x_3: HappyAbsSyn, happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_3, happy_x_2, happy_x_1) {
        (HappyAbsSyn100(happy_var_3), _, HappyAbsSyn32(happy_var_1)) => HappyAbsSyn102((Some(happy_var_1), happy_var_3)),
        _ => notHappyAtAll()
    }
}


fn happyReduce_372(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_3(p, 102, happyReduction_372, i)
}

fn happyReduction_372(happy_x_3: HappyAbsSyn, happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_3, happy_x_2, happy_x_1) {
        (HappyAbsSyn100(happy_var_3), _, _) => HappyAbsSyn102((None, happy_var_3)),
        _ => notHappyAtAll()
    }
}


fn happyReduce_373(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 1, 103, happyReduction_373, i)
}

fn happyReduction_373(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap()) {
        HappyAbsSyn131(happy_var_1) => { withNodeInfo(p, happy_var_1.clone(), box move |_0| singleton(CMemberDesig(happy_var_1, _0)))
        }.map(HappyAbsSyn97),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_374(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 3, 103, happyReduction_374, i)
}

fn happyReduction_374(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn131(happy_var_3), _, HappyAbsSyn97(happy_var_1)) => { withNodeInfo(p, happy_var_3.clone(), box move |_0| snoc(happy_var_1, CMemberDesig(happy_var_3, _0)))
        }.map(HappyAbsSyn97),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_375(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 4, 103, happyReduction_375, i)
}

fn happyReduction_375(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, HappyAbsSyn100(happy_var_3), _, HappyAbsSyn97(happy_var_1)) => { withNodeInfo(p, happy_var_3.clone(), box move |_0| snoc(happy_var_1, CArrDesig(happy_var_3, _0)))
        }.map(HappyAbsSyn97),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_376(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_1(p, 104, happyReduction_376, i)
}

fn happyReduction_376(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn100(happy_var_1) => HappyAbsSyn100(happy_var_1),
        _ => notHappyAtAll()
    }
}


fn happyReduce_377(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 4, 104, happyReduction_377, i)
}

fn happyReduction_377(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, HappyAbsSyn100(happy_var_3), _, HappyAbsSyn100(happy_var_1)) => { withNodeInfo(p, happy_var_1.clone(), partial_1!(CIndex, box happy_var_1, box happy_var_3))
        }.map(HappyAbsSyn100),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_378(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 3, 104, happyReduction_378, i)
}

fn happyReduction_378(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, _, HappyAbsSyn100(happy_var_1)) => { withNodeInfo(p, happy_var_1.clone(), partial_1!(CCall, box happy_var_1, vec![]))
        }.map(HappyAbsSyn100),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_379(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 4, 104, happyReduction_379, i)
}

fn happyReduction_379(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, HappyAbsSyn105(happy_var_3), _, HappyAbsSyn100(happy_var_1)) => { withNodeInfo(p, happy_var_1.clone(), partial_1!(CCall, box happy_var_1, reverse(happy_var_3)))
        }.map(HappyAbsSyn100),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_380(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 3, 104, happyReduction_380, i)
}

fn happyReduction_380(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn131(happy_var_3), _, HappyAbsSyn100(happy_var_1)) => { withNodeInfo(p, happy_var_1.clone(), partial_1!(CMember, box happy_var_1, happy_var_3, false))
        }.map(HappyAbsSyn100),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_381(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 3, 104, happyReduction_381, i)
}

fn happyReduction_381(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn131(happy_var_3), _, HappyAbsSyn100(happy_var_1)) => { withNodeInfo(p, happy_var_1.clone(), partial_1!(CMember, box happy_var_1, happy_var_3, true))
        }.map(HappyAbsSyn100),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_382(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 2, 104, happyReduction_382, i)
}

fn happyReduction_382(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, HappyAbsSyn100(happy_var_1)) => { withNodeInfo(p, happy_var_1.clone(), partial_1!(CUnary, CPostIncOp, box happy_var_1))
        }.map(HappyAbsSyn100),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_383(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 2, 104, happyReduction_383, i)
}

fn happyReduction_383(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, HappyAbsSyn100(happy_var_1)) => { withNodeInfo(p, happy_var_1.clone(), partial_1!(CUnary, CPostDecOp, box happy_var_1))
        }.map(HappyAbsSyn100),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_384(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 6, 104, happyReduction_384, i)
}

fn happyReduction_384(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, HappyAbsSyn95(happy_var_5), _, _, HappyAbsSyn32(happy_var_2), HappyTerminal(happy_var_1)) => { withNodeInfo(p, happy_var_1, partial_1!(CCompoundLit, box happy_var_2, reverse(happy_var_5)))
        }.map(HappyAbsSyn100),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_385(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 7, 104, happyReduction_385, i)
}

fn happyReduction_385(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, _, HappyAbsSyn95(happy_var_5), _, _, HappyAbsSyn32(happy_var_2), HappyTerminal(happy_var_1)) => { withNodeInfo(p, happy_var_1, partial_1!(CCompoundLit, box happy_var_2, reverse(happy_var_5)))
        }.map(HappyAbsSyn100),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_386(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_1(p, 105, happyReduction_386, i)
}

fn happyReduction_386(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn100(happy_var_1) => HappyAbsSyn105(singleton(happy_var_1)),
        _ => notHappyAtAll()
    }
}


fn happyReduce_387(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_3(p, 105, happyReduction_387, i)
}

fn happyReduction_387(happy_x_3: HappyAbsSyn, happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_3, happy_x_2, happy_x_1) {
        (HappyAbsSyn100(happy_var_3), _, HappyAbsSyn105(happy_var_1)) => HappyAbsSyn105(snoc(happy_var_1, happy_var_3)),
        _ => notHappyAtAll()
    }
}


fn happyReduce_388(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_1(p, 106, happyReduction_388, i)
}

fn happyReduction_388(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn100(happy_var_1) => HappyAbsSyn100(happy_var_1),
        _ => notHappyAtAll()
    }
}


fn happyReduce_389(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 2, 106, happyReduction_389, i)
}

fn happyReduction_389(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn100(happy_var_2), HappyTerminal(happy_var_1)) => { withNodeInfo(p, happy_var_1, partial_1!(CUnary, CPreIncOp, box happy_var_2))
        }.map(HappyAbsSyn100),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_390(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 2, 106, happyReduction_390, i)
}

fn happyReduction_390(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn100(happy_var_2), HappyTerminal(happy_var_1)) => { withNodeInfo(p, happy_var_1, partial_1!(CUnary, CPreDecOp, box happy_var_2))
        }.map(HappyAbsSyn100),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_391(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_2(p, 106, happyReduction_391, i)
}

fn happyReduction_391(happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_2, happy_x_1) {
        (HappyAbsSyn100(happy_var_2), _) => HappyAbsSyn100(happy_var_2),
        _ => notHappyAtAll()
    }
}


fn happyReduce_392(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 2, 106, happyReduction_392, i)
}

fn happyReduction_392(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn100(happy_var_2), HappyAbsSyn107(happy_var_1)) => { withNodeInfo(p, happy_var_1.clone(), partial_1!(CUnary, unL(happy_var_1), box happy_var_2))
        }.map(HappyAbsSyn100),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_393(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 2, 106, happyReduction_393, i)
}

fn happyReduction_393(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn100(happy_var_2), HappyTerminal(happy_var_1)) => { withNodeInfo(p, happy_var_1, partial_1!(CSizeofExpr, box happy_var_2))
        }.map(HappyAbsSyn100),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_394(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 4, 106, happyReduction_394, i)
}

fn happyReduction_394(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, HappyAbsSyn32(happy_var_3), _, HappyTerminal(happy_var_1)) => { withNodeInfo(p, happy_var_1, partial_1!(CSizeofType, box happy_var_3))
        }.map(HappyAbsSyn100),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_395(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 2, 106, happyReduction_395, i)
}

fn happyReduction_395(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn100(happy_var_2), HappyTerminal(happy_var_1)) => { withNodeInfo(p, happy_var_1, partial_1!(CAlignofExpr, box happy_var_2))
        }.map(HappyAbsSyn100),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_396(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 4, 106, happyReduction_396, i)
}

fn happyReduction_396(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, HappyAbsSyn32(happy_var_3), _, HappyTerminal(happy_var_1)) => { withNodeInfo(p, happy_var_1, partial_1!(CAlignofType, box happy_var_3))
        }.map(HappyAbsSyn100),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_397(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 2, 106, happyReduction_397, i)
}

fn happyReduction_397(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn100(happy_var_2), HappyTerminal(happy_var_1)) => { withNodeInfo(p, happy_var_1, partial_1!(CComplexReal, box happy_var_2))
        }.map(HappyAbsSyn100),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_398(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 2, 106, happyReduction_398, i)
}

fn happyReduction_398(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn100(happy_var_2), HappyTerminal(happy_var_1)) => { withNodeInfo(p, happy_var_1, partial_1!(CComplexImag, box happy_var_2))
        }.map(HappyAbsSyn100),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_399(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 2, 106, happyReduction_399, i)
}

fn happyReduction_399(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn131(happy_var_2), HappyTerminal(happy_var_1)) => { withNodeInfo(p, happy_var_1, partial_1!(CLabAddrExpr, happy_var_2))
        }.map(HappyAbsSyn100),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_400(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_1(p, 107, happyReduction_400, i)
}

fn happyReduction_400(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyTerminal(happy_var_1) => HappyAbsSyn107(Located(CAdrOp,  happy_var_1.into_pos())),
        _ => notHappyAtAll()
    }
}


fn happyReduce_401(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_1(p, 107, happyReduction_401, i)
}

fn happyReduction_401(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyTerminal(happy_var_1) => HappyAbsSyn107(Located(CIndOp,  happy_var_1.into_pos())),
        _ => notHappyAtAll()
    }
}


fn happyReduce_402(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_1(p, 107, happyReduction_402, i)
}

fn happyReduction_402(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyTerminal(happy_var_1) => HappyAbsSyn107(Located(CPlusOp, happy_var_1.into_pos())),
        _ => notHappyAtAll()
    }
}


fn happyReduce_403(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_1(p, 107, happyReduction_403, i)
}

fn happyReduction_403(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyTerminal(happy_var_1) => HappyAbsSyn107(Located(CMinOp,  happy_var_1.into_pos())),
        _ => notHappyAtAll()
    }
}


fn happyReduce_404(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_1(p, 107, happyReduction_404, i)
}

fn happyReduction_404(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyTerminal(happy_var_1) => HappyAbsSyn107(Located(CCompOp, happy_var_1.into_pos())),
        _ => notHappyAtAll()
    }
}


fn happyReduce_405(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_1(p, 107, happyReduction_405, i)
}

fn happyReduction_405(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyTerminal(happy_var_1) => HappyAbsSyn107(Located(CNegOp,  happy_var_1.into_pos())),
        _ => notHappyAtAll()
    }
}


fn happyReduce_406(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_1(p, 108, happyReduction_406, i)
}

fn happyReduction_406(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn100(happy_var_1) => HappyAbsSyn100(happy_var_1),
        _ => notHappyAtAll()
    }
}


fn happyReduce_407(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 4, 108, happyReduction_407, i)
}

fn happyReduction_407(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn100(happy_var_4), _, HappyAbsSyn32(happy_var_2), HappyTerminal(happy_var_1)) => { withNodeInfo(p, happy_var_1, partial_1!(CCast, box happy_var_2, box happy_var_4))
        }.map(HappyAbsSyn100),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_408(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_1(p, 109, happyReduction_408, i)
}

fn happyReduction_408(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn100(happy_var_1) => HappyAbsSyn100(happy_var_1),
        _ => notHappyAtAll()
    }
}


fn happyReduce_409(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 3, 109, happyReduction_409, i)
}

fn happyReduction_409(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn100(happy_var_3), _, HappyAbsSyn100(happy_var_1)) => { withNodeInfo(p, happy_var_1.clone(), partial_1!(CBinary, CMulOp, box happy_var_1, box happy_var_3))
        }.map(HappyAbsSyn100),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_410(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 3, 109, happyReduction_410, i)
}

fn happyReduction_410(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn100(happy_var_3), _, HappyAbsSyn100(happy_var_1)) => { withNodeInfo(p, happy_var_1.clone(), partial_1!(CBinary, CDivOp, box happy_var_1, box happy_var_3))
        }.map(HappyAbsSyn100),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_411(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 3, 109, happyReduction_411, i)
}

fn happyReduction_411(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn100(happy_var_3), _, HappyAbsSyn100(happy_var_1)) => { withNodeInfo(p, happy_var_1.clone(), partial_1!(CBinary, CRmdOp, box happy_var_1, box happy_var_3))
        }.map(HappyAbsSyn100),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_412(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_1(p, 110, happyReduction_412, i)
}

fn happyReduction_412(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn100(happy_var_1) => HappyAbsSyn100(happy_var_1),
        _ => notHappyAtAll()
    }
}


fn happyReduce_413(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 3, 110, happyReduction_413, i)
}

fn happyReduction_413(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn100(happy_var_3), _, HappyAbsSyn100(happy_var_1)) => { withNodeInfo(p, happy_var_1.clone(), partial_1!(CBinary, CAddOp, box happy_var_1, box happy_var_3))
        }.map(HappyAbsSyn100),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_414(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 3, 110, happyReduction_414, i)
}

fn happyReduction_414(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn100(happy_var_3), _, HappyAbsSyn100(happy_var_1)) => { withNodeInfo(p, happy_var_1.clone(), partial_1!(CBinary, CSubOp, box happy_var_1, box happy_var_3))
        }.map(HappyAbsSyn100),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_415(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_1(p, 111, happyReduction_415, i)
}

fn happyReduction_415(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn100(happy_var_1) => HappyAbsSyn100(happy_var_1),
        _ => notHappyAtAll()
    }
}


fn happyReduce_416(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 3, 111, happyReduction_416, i)
}

fn happyReduction_416(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn100(happy_var_3), _, HappyAbsSyn100(happy_var_1)) => { withNodeInfo(p, happy_var_1.clone(), partial_1!(CBinary, CShlOp, box happy_var_1, box happy_var_3))
        }.map(HappyAbsSyn100),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_417(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 3, 111, happyReduction_417, i)
}

fn happyReduction_417(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn100(happy_var_3), _, HappyAbsSyn100(happy_var_1)) => { withNodeInfo(p, happy_var_1.clone(), partial_1!(CBinary, CShrOp, box happy_var_1, box happy_var_3))
        }.map(HappyAbsSyn100),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_418(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_1(p, 112, happyReduction_418, i)
}

fn happyReduction_418(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn100(happy_var_1) => HappyAbsSyn100(happy_var_1),
        _ => notHappyAtAll()
    }
}


fn happyReduce_419(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 3, 112, happyReduction_419, i)
}

fn happyReduction_419(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn100(happy_var_3), _, HappyAbsSyn100(happy_var_1)) => { withNodeInfo(p, happy_var_1.clone(), partial_1!(CBinary, CLeOp, box happy_var_1, box happy_var_3))
        }.map(HappyAbsSyn100),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_420(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 3, 112, happyReduction_420, i)
}

fn happyReduction_420(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn100(happy_var_3), _, HappyAbsSyn100(happy_var_1)) => { withNodeInfo(p, happy_var_1.clone(), partial_1!(CBinary, CGrOp, box happy_var_1, box happy_var_3))
        }.map(HappyAbsSyn100),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_421(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 3, 112, happyReduction_421, i)
}

fn happyReduction_421(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn100(happy_var_3), _, HappyAbsSyn100(happy_var_1)) => { withNodeInfo(p, happy_var_1.clone(), partial_1!(CBinary, CLeqOp, box happy_var_1, box happy_var_3))
        }.map(HappyAbsSyn100),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_422(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 3, 112, happyReduction_422, i)
}

fn happyReduction_422(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn100(happy_var_3), _, HappyAbsSyn100(happy_var_1)) => { withNodeInfo(p, happy_var_1.clone(), partial_1!(CBinary, CGeqOp, box happy_var_1, box happy_var_3))
        }.map(HappyAbsSyn100),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_423(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_1(p, 113, happyReduction_423, i)
}

fn happyReduction_423(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn100(happy_var_1) => HappyAbsSyn100(happy_var_1),
        _ => notHappyAtAll()
    }
}


fn happyReduce_424(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 3, 113, happyReduction_424, i)
}

fn happyReduction_424(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn100(happy_var_3), _, HappyAbsSyn100(happy_var_1)) => { withNodeInfo(p, happy_var_1.clone(), partial_1!(CBinary, CEqOp, box happy_var_1, box happy_var_3))
        }.map(HappyAbsSyn100),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_425(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 3, 113, happyReduction_425, i)
}

fn happyReduction_425(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn100(happy_var_3), _, HappyAbsSyn100(happy_var_1)) => { withNodeInfo(p, happy_var_1.clone(), partial_1!(CBinary, CNeqOp, box happy_var_1, box happy_var_3))
        }.map(HappyAbsSyn100),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_426(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_1(p, 114, happyReduction_426, i)
}

fn happyReduction_426(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn100(happy_var_1) => HappyAbsSyn100(happy_var_1),
        _ => notHappyAtAll()
    }
}


fn happyReduce_427(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 3, 114, happyReduction_427, i)
}

fn happyReduction_427(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn100(happy_var_3), _, HappyAbsSyn100(happy_var_1)) => { withNodeInfo(p, happy_var_1.clone(), partial_1!(CBinary, CAndOp, box happy_var_1, box happy_var_3))
        }.map(HappyAbsSyn100),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_428(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_1(p, 115, happyReduction_428, i)
}

fn happyReduction_428(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn100(happy_var_1) => HappyAbsSyn100(happy_var_1),
        _ => notHappyAtAll()
    }
}


fn happyReduce_429(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 3, 115, happyReduction_429, i)
}

fn happyReduction_429(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn100(happy_var_3), _, HappyAbsSyn100(happy_var_1)) => { withNodeInfo(p, happy_var_1.clone(), partial_1!(CBinary, CXorOp, box happy_var_1, box happy_var_3))
        }.map(HappyAbsSyn100),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_430(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_1(p, 116, happyReduction_430, i)
}

fn happyReduction_430(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn100(happy_var_1) => HappyAbsSyn100(happy_var_1),
        _ => notHappyAtAll()
    }
}


fn happyReduce_431(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 3, 116, happyReduction_431, i)
}

fn happyReduction_431(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn100(happy_var_3), _, HappyAbsSyn100(happy_var_1)) => { withNodeInfo(p, happy_var_1.clone(), partial_1!(CBinary, COrOp, box happy_var_1, box happy_var_3))
        }.map(HappyAbsSyn100),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_432(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_1(p, 117, happyReduction_432, i)
}

fn happyReduction_432(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn100(happy_var_1) => HappyAbsSyn100(happy_var_1),
        _ => notHappyAtAll()
    }
}


fn happyReduce_433(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 3, 117, happyReduction_433, i)
}

fn happyReduction_433(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn100(happy_var_3), _, HappyAbsSyn100(happy_var_1)) => { withNodeInfo(p, happy_var_1.clone(), partial_1!(CBinary, CLndOp, box happy_var_1, box happy_var_3))
        }.map(HappyAbsSyn100),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_434(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_1(p, 118, happyReduction_434, i)
}

fn happyReduction_434(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn100(happy_var_1) => HappyAbsSyn100(happy_var_1),
        _ => notHappyAtAll()
    }
}


fn happyReduce_435(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 3, 118, happyReduction_435, i)
}

fn happyReduction_435(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn100(happy_var_3), _, HappyAbsSyn100(happy_var_1)) => { withNodeInfo(p, happy_var_1.clone(), partial_1!(CBinary, CLorOp, box happy_var_1, box happy_var_3))
        }.map(HappyAbsSyn100),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_436(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_1(p, 119, happyReduction_436, i)
}

fn happyReduction_436(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn100(happy_var_1) => HappyAbsSyn100(happy_var_1),
        _ => notHappyAtAll()
    }
}


fn happyReduce_437(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 5, 119, happyReduction_437, i)
}

fn happyReduction_437(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn100(happy_var_5), _, HappyAbsSyn100(happy_var_3), _, HappyAbsSyn100(happy_var_1)) => { withNodeInfo(p, happy_var_1.clone(), partial_1!(CCond, box happy_var_1, Some(box happy_var_3), box happy_var_5))
        }.map(HappyAbsSyn100),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_438(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 4, 119, happyReduction_438, i)
}

fn happyReduction_438(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn100(happy_var_4), _, _, HappyAbsSyn100(happy_var_1)) => { withNodeInfo(p, happy_var_1.clone(), partial_1!(CCond, box happy_var_1, None, box happy_var_4))
        }.map(HappyAbsSyn100),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_439(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_1(p, 120, happyReduction_439, i)
}

fn happyReduction_439(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn100(happy_var_1) => HappyAbsSyn100(happy_var_1),
        _ => notHappyAtAll()
    }
}


fn happyReduce_440(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 3, 120, happyReduction_440, i)
}

fn happyReduction_440(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn100(happy_var_3), HappyAbsSyn121(happy_var_2), HappyAbsSyn100(happy_var_1)) => { withNodeInfo(p, happy_var_1.clone(), partial_1!(CAssign, unL(happy_var_2), box happy_var_1, box happy_var_3))
        }.map(HappyAbsSyn100),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_441(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_1(p, 121, happyReduction_441, i)
}

fn happyReduction_441(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyTerminal(happy_var_1) => HappyAbsSyn121(Located(CAssignOp, happy_var_1.into_pos())),
        _ => notHappyAtAll()
    }
}


fn happyReduce_442(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_1(p, 121, happyReduction_442, i)
}

fn happyReduction_442(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyTerminal(happy_var_1) => HappyAbsSyn121(Located(CMulAssOp, happy_var_1.into_pos())),
        _ => notHappyAtAll()
    }
}


fn happyReduce_443(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_1(p, 121, happyReduction_443, i)
}

fn happyReduction_443(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyTerminal(happy_var_1) => HappyAbsSyn121(Located(CDivAssOp, happy_var_1.into_pos())),
        _ => notHappyAtAll()
    }
}


fn happyReduce_444(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_1(p, 121, happyReduction_444, i)
}

fn happyReduction_444(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyTerminal(happy_var_1) => HappyAbsSyn121(Located(CRmdAssOp, happy_var_1.into_pos())),
        _ => notHappyAtAll()
    }
}


fn happyReduce_445(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_1(p, 121, happyReduction_445, i)
}

fn happyReduction_445(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyTerminal(happy_var_1) => HappyAbsSyn121(Located(CAddAssOp, happy_var_1.into_pos())),
        _ => notHappyAtAll()
    }
}


fn happyReduce_446(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_1(p, 121, happyReduction_446, i)
}

fn happyReduction_446(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyTerminal(happy_var_1) => HappyAbsSyn121(Located(CSubAssOp, happy_var_1.into_pos())),
        _ => notHappyAtAll()
    }
}


fn happyReduce_447(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_1(p, 121, happyReduction_447, i)
}

fn happyReduction_447(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyTerminal(happy_var_1) => HappyAbsSyn121(Located(CShlAssOp, happy_var_1.into_pos())),
        _ => notHappyAtAll()
    }
}


fn happyReduce_448(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_1(p, 121, happyReduction_448, i)
}

fn happyReduction_448(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyTerminal(happy_var_1) => HappyAbsSyn121(Located(CShrAssOp, happy_var_1.into_pos())),
        _ => notHappyAtAll()
    }
}


fn happyReduce_449(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_1(p, 121, happyReduction_449, i)
}

fn happyReduction_449(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyTerminal(happy_var_1) => HappyAbsSyn121(Located(CAndAssOp, happy_var_1.into_pos())),
        _ => notHappyAtAll()
    }
}


fn happyReduce_450(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_1(p, 121, happyReduction_450, i)
}

fn happyReduction_450(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyTerminal(happy_var_1) => HappyAbsSyn121(Located(CXorAssOp, happy_var_1.into_pos())),
        _ => notHappyAtAll()
    }
}


fn happyReduce_451(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_1(p, 121, happyReduction_451, i)
}

fn happyReduction_451(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyTerminal(happy_var_1) => HappyAbsSyn121(Located(COrAssOp,  happy_var_1.into_pos())),
        _ => notHappyAtAll()
    }
}


fn happyReduce_452(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_1(p, 122, happyReduction_452, i)
}

fn happyReduction_452(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn100(happy_var_1) => HappyAbsSyn100(happy_var_1),
        _ => notHappyAtAll()
    }
}


fn happyReduce_453(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 3, 122, happyReduction_453, i)
}

fn happyReduction_453(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn105(happy_var_3), _, HappyAbsSyn100(happy_var_1)) => {
            let es = reverse(happy_var_3);
            withNodeInfo(p, es.clone(), partial_1!(CComma, __op_concat(happy_var_1, es)))
        }.map(HappyAbsSyn100),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_454(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_1(p, 123, happyReduction_454, i)
}

fn happyReduction_454(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn100(happy_var_1) => HappyAbsSyn105(singleton(happy_var_1)),
        _ => notHappyAtAll()
    }
}


fn happyReduce_455(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_3(p, 123, happyReduction_455, i)
}

fn happyReduction_455(happy_x_3: HappyAbsSyn, happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_3, happy_x_2, happy_x_1) {
        (HappyAbsSyn100(happy_var_3), _, HappyAbsSyn105(happy_var_1)) => HappyAbsSyn105(snoc(happy_var_1, happy_var_3)),
        _ => notHappyAtAll()
    }
}


fn happyReduce_456(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_0(p, 124, happyReduction_456(), i)
}

fn happyReduction_456() -> HappyAbsSyn {
    HappyAbsSyn124(None)
}


fn happyReduce_457(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_1(p, 124, happyReduction_457, i)
}

fn happyReduction_457(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn100(happy_var_1) => HappyAbsSyn124(Some(happy_var_1)),
        _ => notHappyAtAll()
    }
}


fn happyReduce_458(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_0(p, 125, happyReduction_458(), i)
}

fn happyReduction_458() -> HappyAbsSyn {
    HappyAbsSyn124(None)
}


fn happyReduce_459(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_1(p, 125, happyReduction_459, i)
}

fn happyReduction_459(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn100(happy_var_1) => HappyAbsSyn124(Some(happy_var_1)),
        _ => notHappyAtAll()
    }
}


fn happyReduce_460(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_1(p, 126, happyReduction_460, i)
}

fn happyReduction_460(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn100(happy_var_1) => HappyAbsSyn100(happy_var_1),
        _ => notHappyAtAll()
    }
}


fn happyReduce_461(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 1, 127, happyReduction_461, i)
}

fn happyReduction_461(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap()) {
        HappyTerminal(happy_var_1) => {
                    withNodeInfo(p, happy_var_1.clone(), box move |_0| {
                        // TODO: I don't get why this is a Fn closure...
                        if let CTokILit(_, ref i) = happy_var_1 {
                            CIntConst(i.clone(), _0)
                        } else {
                            panic!("irrefutable pattern")
                        }
                    })
        }.map(HappyAbsSyn127),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_462(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 1, 127, happyReduction_462, i)
}

fn happyReduction_462(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap()) {
        HappyTerminal(happy_var_1) => {
                    withNodeInfo(p, happy_var_1.clone(), box move |_0| {
                        if let CTokCLit(_, ref c) = happy_var_1 {
                            CCharConst(c.clone(), _0)
                        } else {
                            panic!("irrefutable pattern")
                        }
                    })
        }.map(HappyAbsSyn127),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_463(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 1, 127, happyReduction_463, i)
}

fn happyReduction_463(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap()) {
        HappyTerminal(happy_var_1) => {
                    withNodeInfo(p, happy_var_1.clone(), box move |_0| {
                        if let CTokFLit(_, ref f) = happy_var_1 {
                            CFloatConst(f.clone(), _0)
                        } else {
                            panic!("irrefutable pattern")
                        }
                    })
        }.map(HappyAbsSyn127),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_464(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 1, 128, happyReduction_464, i)
}

fn happyReduction_464(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap()) {
        HappyTerminal(happy_var_1) => {
            withNodeInfo(p, happy_var_1.clone(), box move |_0| {
                if let CTokSLit(_, ref s) = happy_var_1 {
                    CStringLiteral(s.clone(), _0)
                } else {
                    panic!("irrefutable pattern")
                }
            })
        }.map(HappyAbsSyn128),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_465(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 2, 128, happyReduction_465, i)
}

fn happyReduction_465(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn129(happy_var_2), HappyTerminal(happy_var_1)) => {
            withNodeInfo(p, happy_var_1.clone(), box move |_0| {
                if let CTokSLit(_, s) = happy_var_1 {
                    CStringLiteral(concatCStrings(__op_concat(s, reverse(happy_var_2))), _0)
                } else {
                    panic!("irrefutable pattern")
                }
            })
        }.map(HappyAbsSyn128),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_466(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_1(p, 129, happyReduction_466, i)
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


fn happyReduce_467(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_2(p, 129, happyReduction_467, i)
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


fn happyReduce_468(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_1(p, 130, happyReduction_468, i)
}

fn happyReduction_468(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyTerminal(CTokClangC(_, ClangCTok(happy_var_1))) => HappyAbsSyn130(happy_var_1),
        _ => notHappyAtAll()
    }
}


fn happyReduce_469(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_1(p, 131, happyReduction_469, i)
}

fn happyReduction_469(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyTerminal(CTokIdent(_, happy_var_1)) => HappyAbsSyn131(happy_var_1),
        _ => notHappyAtAll()
    }
}


fn happyReduce_470(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_1(p, 131, happyReduction_470, i)
}

fn happyReduction_470(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyTerminal(CTokTyIdent(_, happy_var_1)) => HappyAbsSyn131(happy_var_1),
        _ => notHappyAtAll()
    }
}


fn happyReduce_471(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_0(p, 132, happyReduction_471(), i)
}

fn happyReduction_471() -> HappyAbsSyn {
    HappyAbsSyn132(vec![])
}


fn happyReduce_472(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_1(p, 132, happyReduction_472, i)
}

fn happyReduction_472(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn132(happy_var_1) => HappyAbsSyn132(happy_var_1),
        _ => notHappyAtAll()
    }
}


fn happyReduce_473(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_1(p, 133, happyReduction_473, i)
}

fn happyReduction_473(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn132(happy_var_1) => HappyAbsSyn132(happy_var_1),
        _ => notHappyAtAll()
    }
}


fn happyReduce_474(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_2(p, 133, happyReduction_474, i)
}

fn happyReduction_474(happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_2, happy_x_1) {
        (HappyAbsSyn132(happy_var_2), HappyAbsSyn132(happy_var_1)) => HappyAbsSyn132(__op_addadd(happy_var_1, happy_var_2)),
        _ => notHappyAtAll()
    }
}


fn happyReduce_475(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyReduce(p, 6, 134, happyReduction_475, i)
}

fn happyReduction_475(p: &mut Parser) {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, _, HappyAbsSyn135(happy_var_4), _, _, _) => {            p.stack.push(HappyAbsSyn132(reverse(happy_var_4))); }
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_476(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_1(p, 135, happyReduction_476, i)
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


fn happyReduce_477(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_3(p, 135, happyReduction_477, i)
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


fn happyReduce_478(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_0(p, 136, happyReduction_478(), i)
}

fn happyReduction_478() -> HappyAbsSyn {
    HappyAbsSyn136(None)
}


fn happyReduce_479(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 1, 136, happyReduction_479, i)
}

fn happyReduction_479(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap()) {
        HappyTerminal(CTokIdent(_, happy_var_1)) => { withNodeInfo(p, happy_var_1.clone(), box move |_0| Some(CAttribute(happy_var_1, vec![], _0)))
        }.map(HappyAbsSyn136),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_480(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 1, 136, happyReduction_480, i)
}

fn happyReduction_480(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap()) {
        HappyTerminal(happy_var_1) => { withNodeInfo(p, happy_var_1, box move |_0| Some(CAttribute(Ident::internal("const".into()), vec![], _0)))
        }.map(HappyAbsSyn136),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_481(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 4, 136, happyReduction_481, i)
}

fn happyReduction_481(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, HappyAbsSyn105(happy_var_3), _, HappyTerminal(CTokIdent(_, happy_var_1))) => { withNodeInfo(p, happy_var_1.clone(), box move |_0| Some(CAttribute(happy_var_1, reverse(happy_var_3), _0)))
        }.map(HappyAbsSyn136),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_482(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyMonadReduce(p, 3, 136, happyReduction_482, i)
}

fn happyReduction_482(p: &mut Parser) -> P<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, _, HappyTerminal(CTokIdent(_, happy_var_1))) => { withNodeInfo(p, happy_var_1.clone(), box move |_0| Some(CAttribute(happy_var_1, vec![], _0)))
        }.map(HappyAbsSyn136),
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_483(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_1(p, 137, happyReduction_483, i)
}

fn happyReduction_483(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn100(happy_var_1) => HappyAbsSyn105(singleton(happy_var_1)),
        _ => notHappyAtAll()
    }
}


fn happyReduce_484(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_3(p, 137, happyReduction_484, i)
}

fn happyReduction_484(happy_x_3: HappyAbsSyn, happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_3, happy_x_2, happy_x_1) {
        (_, _, _) => HappyAbsSyn105(Reversed(vec![])),
        _ => notHappyAtAll()
    }
}


fn happyReduce_485(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_3(p, 137, happyReduction_485, i)
}

fn happyReduction_485(happy_x_3: HappyAbsSyn, happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_3, happy_x_2, happy_x_1) {
        (_, _, _) => HappyAbsSyn105(Reversed(vec![])),
        _ => notHappyAtAll()
    }
}


fn happyReduce_486(p: &mut Parser, i: isize) -> Monad<Cont> {
    happySpecReduce_3(p, 137, happyReduction_486, i)
}

fn happyReduction_486(happy_x_3: HappyAbsSyn, happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_3, happy_x_2, happy_x_1) {
        (HappyAbsSyn100(happy_var_3), _, HappyAbsSyn105(happy_var_1)) => HappyAbsSyn105(snoc(happy_var_1, happy_var_3)),
        _ => notHappyAtAll()
    }
}


fn happyReduce_487(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyReduce(p, 5, 137, happyReduction_487, i)
}

fn happyReduction_487(p: &mut Parser) {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, _, _, _, HappyAbsSyn105(happy_var_1)) => {            p.stack.push(HappyAbsSyn105(happy_var_1)); }
        _ => panic!("irrefutable pattern")
    }
}


fn happyReduce_488(p: &mut Parser, i: isize) -> Monad<Cont> {
    happyReduce(p, 5, 137, happyReduction_488, i)
}

fn happyReduction_488(p: &mut Parser) {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, _, _, _, HappyAbsSyn105(happy_var_1)) => {            p.stack.push(HappyAbsSyn105(happy_var_1)); }
        _ => panic!("irrefutable pattern")
    }
}


fn happyNewToken(p: &mut Parser) -> P<Cont> {
    p.token = lexC(p)?;
    let action = p.state;
    match p.token {
        CTokEof => action(p, 247, 247),
        CTokLParen(_) => action(p, 138, 138),
        CTokRParen(_) => action(p, 139, 139),
        CTokLBracket(_) => action(p, 140, 140),
        CTokRBracket(_) => action(p, 141, 141),
        CTokArrow(_) => action(p, 142, 142),
        CTokDot(_) => action(p, 143, 143),
        CTokExclam(_) => action(p, 144, 144),
        CTokTilde(_) => action(p, 145, 145),
        CTokInc(_) => action(p, 146, 146),
        CTokDec(_) => action(p, 147, 147),
        CTokPlus(_) => action(p, 148, 148),
        CTokMinus(_) => action(p, 149, 149),
        CTokStar(_) => action(p, 150, 150),
        CTokSlash(_) => action(p, 151, 151),
        CTokPercent(_) => action(p, 152, 152),
        CTokAmper(_) => action(p, 153, 153),
        CTokShiftL(_) => action(p, 154, 154),
        CTokShiftR(_) => action(p, 155, 155),
        CTokLess(_) => action(p, 156, 156),
        CTokLessEq(_) => action(p, 157, 157),
        CTokHigh(_) => action(p, 158, 158),
        CTokHighEq(_) => action(p, 159, 159),
        CTokEqual(_) => action(p, 160, 160),
        CTokUnequal(_) => action(p, 161, 161),
        CTokHat(_) => action(p, 162, 162),
        CTokBar(_) => action(p, 163, 163),
        CTokAnd(_) => action(p, 164, 164),
        CTokOr(_) => action(p, 165, 165),
        CTokQuest(_) => action(p, 166, 166),
        CTokColon(_) => action(p, 167, 167),
        CTokAssign(_) => action(p, 168, 168),
        CTokPlusAss(_) => action(p, 169, 169),
        CTokMinusAss(_) => action(p, 170, 170),
        CTokStarAss(_) => action(p, 171, 171),
        CTokSlashAss(_) => action(p, 172, 172),
        CTokPercAss(_) => action(p, 173, 173),
        CTokAmpAss(_) => action(p, 174, 174),
        CTokHatAss(_) => action(p, 175, 175),
        CTokBarAss(_) => action(p, 176, 176),
        CTokSLAss(_) => action(p, 177, 177),
        CTokSRAss(_) => action(p, 178, 178),
        CTokComma(_) => action(p, 179, 179),
        CTokSemic(_) => action(p, 180, 180),
        CTokLBrace(_) => action(p, 181, 181),
        CTokRBrace(_) => action(p, 182, 182),
        CTokEllipsis(_) => action(p, 183, 183),
        CTokAlignof(_) => action(p, 184, 184),
        CTokAlignas(_) => action(p, 185, 185),
        CTokAtomic(_) => action(p, 186, 186),
        CTokAsm(_) => action(p, 187, 187),
        CTokAuto(_) => action(p, 188, 188),
        CTokBreak(_) => action(p, 189, 189),
        CTokBool(_) => action(p, 190, 190),
        CTokCase(_) => action(p, 191, 191),
        CTokChar(_) => action(p, 192, 192),
        CTokConst(_) => action(p, 193, 193),
        CTokContinue(_) => action(p, 194, 194),
        CTokComplex(_) => action(p, 195, 195),
        CTokDefault(_) => action(p, 196, 196),
        CTokDo(_) => action(p, 197, 197),
        CTokDouble(_) => action(p, 198, 198),
        CTokElse(_) => action(p, 199, 199),
        CTokEnum(_) => action(p, 200, 200),
        CTokExtern(_) => action(p, 201, 201),
        CTokFloat(_) => action(p, 202, 202),
        CTokFor(_) => action(p, 203, 203),
        CTokGeneric(_) => action(p, 204, 204),
        CTokGoto(_) => action(p, 205, 205),
        CTokIf(_) => action(p, 206, 206),
        CTokInline(_) => action(p, 207, 207),
        CTokInt(_) => action(p, 208, 208),
        CTokInt128(_) => action(p, 209, 209),
        CTokLong(_) => action(p, 210, 210),
        CTokLabel(_) => action(p, 211, 211),
        CTokNoreturn(_) => action(p, 212, 212),
        CTokNullable(_) => action(p, 213, 213),
        CTokNonnull(_) => action(p, 214, 214),
        CTokRegister(_) => action(p, 215, 215),
        CTokRestrict(_) => action(p, 216, 216),
        CTokReturn(_) => action(p, 217, 217),
        CTokShort(_) => action(p, 218, 218),
        CTokSigned(_) => action(p, 219, 219),
        CTokSizeof(_) => action(p, 220, 220),
        CTokStatic(_) => action(p, 221, 221),
        CTokStaticAssert(_) => action(p, 222, 222),
        CTokStruct(_) => action(p, 223, 223),
        CTokSwitch(_) => action(p, 224, 224),
        CTokTypedef(_) => action(p, 225, 225),
        CTokTypeof(_) => action(p, 226, 226),
        CTokThread(_) => action(p, 227, 227),
        CTokUnion(_) => action(p, 228, 228),
        CTokUnsigned(_) => action(p, 229, 229),
        CTokVoid(_) => action(p, 230, 230),
        CTokVolatile(_) => action(p, 231, 231),
        CTokWhile(_) => action(p, 232, 232),
        CTokCLit(_, _) => action(p, 233, 233),
        CTokILit(_, _) => action(p, 234, 234),
        CTokFLit(_, _) => action(p, 235, 235),
        CTokSLit(_, _) => action(p, 236, 236),
        CTokIdent(_, _) => action(p, 237, 237),
        CTokTyIdent(_, _) => action(p, 238, 238),
        CTokGnuC(GnuCAttrTok, _) => action(p, 239, 239),
        CTokGnuC(GnuCExtTok,  _) => action(p, 240, 240),
        CTokGnuC(GnuCComplexReal, _) => action(p, 241, 241),
        CTokGnuC(GnuCComplexImag, _) => action(p, 242, 242),
        CTokGnuC(GnuCVaArg, _) => action(p, 243, 243),
        CTokGnuC(GnuCOffsetof, _) => action(p, 244, 244),
        CTokGnuC(GnuCTyCompat, _) => action(p, 245, 245),
        CTokClangC(_, ClangCTok(_)) => action(p, 246, 246),
    }
}

fn happyError_<T>(p: &mut Parser, _: isize) -> P<T> {
    happyError_q(p)
}


fn happyError_q<T>(p: &mut Parser) -> P<T> {
    // TODO
    happyError(p)
}

fn translation_unit(p: &mut Parser) -> P<CTranslUnit> {
    let x = happyParse(p, action_0)?;
    match x {
        HappyAbsSyn7(z) => Ok(z),
        _ => notHappyAtAll()
    }
}

fn external_declaration(p: &mut Parser) -> P<CExtDecl> {
    let x = happyParse(p, action_1)?;
    match x {
        HappyAbsSyn9(z) => Ok(z),
        _ => notHappyAtAll()
    }
}

fn statement(p: &mut Parser) -> P<CStat> {
    let x = happyParse(p, action_2)?;
    match x {
        HappyAbsSyn12(z) => Ok(z),
        _ => notHappyAtAll()
    }
}

fn expression(p: &mut Parser) -> P<CExpr> {
    let x = happyParse(p, action_3)?;
    match x {
        HappyAbsSyn100(z) => Ok(z),
        _ => notHappyAtAll()
    }
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
    fn pos(&self) -> &Position {
        &self.1
    }
    fn into_pos(self) -> Position {
        self.1
    }
}

fn unL<T>(Located(a, pos): Located<T>) -> T {
    a
}

fn withNodeInfo<T: 'static, N: Pos + 'static>(p: &mut Parser, node: N, mkAttrNode: Box<FnBox(NodeInfo) -> T>) -> P<T> {
    let name = p.getNewName();
    let lastTok = p.getSavedToken();
    let firstPos = node.into_pos();
    let attrs = NodeInfo::new(firstPos, movePosLenOfTok(lastTok), name);
    Ok(mkAttrNode(attrs))
}

fn withLength<a: Clone + 'static>(p: &mut Parser, nodeinfo: NodeInfo, mkAttrNode: Box<FnBox(NodeInfo) -> a>) -> P<a> {
    let lastTok = p.getSavedToken();
    let firstPos = nodeinfo.pos().clone();
    let attrs = NodeInfo::new(firstPos, movePosLenOfTok(lastTok),
                              nodeinfo.name().unwrap_or_else(|| panic!("nameOfNode")));
    Ok(mkAttrNode(attrs))
}

#[derive(Clone)]
pub struct CDeclrR(Option<Ident>,
                   Reversed<Vec<CDerivedDeclr>>,
                   Option<CStringLiteral<NodeInfo>>,
                   Vec<CAttribute<NodeInfo>>,
                   NodeInfo);

impl CNode for CDeclrR {
    fn node_info(&self) -> &NodeInfo {
        &self.4
    }
    fn into_node_info(self) -> NodeInfo {
        self.4
    }
}

fn reverseDeclr(CDeclrR(ide, reversedDDs, asmname, cattrs, at): CDeclrR) -> CDeclarator<NodeInfo> {
    CDeclarator::<NodeInfo>(ide, reverse(reversedDDs), asmname, cattrs, at)
}

fn withAttribute<node: Pos + 'static>(p: &mut Parser, node: node, cattrs: Vec<CAttribute<NodeInfo>>,
                                      mkDeclrNode: Box<FnBox(NodeInfo) -> CDeclrR>) -> P<CDeclrR> {
    let name = p.getNewName();
    let attrs = NodeInfo::with_pos_name(node.into_pos(), name);
    let newDeclr = appendDeclrAttrs(cattrs.clone(), mkDeclrNode(attrs));
    Ok(newDeclr)
}

fn withAttributePF<N: Pos + 'static>(p: &mut Parser,
    node: N, cattrs: Vec<CAttribute<NodeInfo>>,
    mkDeclrCtor: Box<Fn(NodeInfo, CDeclrR) -> CDeclrR>) -> P<Rc<Box<Fn(CDeclrR) -> CDeclrR>>>
{
    let mkDeclrCtor = Rc::new(mkDeclrCtor);
    let name = p.getNewName();
    let attrs = NodeInfo::with_pos_name(node.into_pos(), name);
    let newDeclr: Rc<Box<Fn(CDeclrR) -> CDeclrR>> = Rc::new(box move |_0| {
        appendDeclrAttrs(cattrs.clone(), mkDeclrCtor(attrs.clone(), _0))
    });
    Ok(newDeclr)
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
            Err(ParseError::new(
                n2.pos().clone(),
                vec!["Duplicate assembler name: ".to_string(), showName(n1), showName(n2)]))
        },
        Right(newName) => {
            Ok(CDeclrR(ident, indirections, newName, cattrs, at))
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
    fn pos(&self) -> &Position {
        self[0].pos()
    }
    fn into_pos(mut self) -> Position {
        self.remove(0).into_pos()
    }
}

impl<A: Pos> Pos for Reversed<A> {
    fn pos(&self) -> &Position {
        (self.0).pos()
    }
    fn into_pos(self) -> Position {
        (self.0).into_pos()
    }
}

fn emptyDeclr() -> CDeclrR {
    CDeclrR(None, empty(), None, vec![], NodeInfo::undef())
}

fn mkVarDeclr(ident: Ident, ni: NodeInfo) -> CDeclrR {
    CDeclrR(Some(ident), empty(), None, vec![], ni)
}

fn doDeclIdent(p: &mut Parser, declspecs: &[CDeclSpec], CDeclrR(mIdent, _, _, _, _): CDeclrR) {
    let is_typedef = |declspec: &CDeclSpec| match *declspec {
        CStorageSpec(CTypedef(_)) => true,
        _ => false,
    };

    match mIdent {
        None => (),
        Some(ident) => {
            if declspecs.iter().any(is_typedef) {
                p.addTypedef(ident)
            } else {
                p.shadowTypedef(ident)
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
            Ok(())
        },
    }
}

fn getCDeclrIdent(CDeclarator(mIdent, _, _, _, _): CDeclarator<NodeInfo>) -> Option<Ident> {
    mIdent
}

fn happyError<T>(p: &mut Parser) -> P<T> {
    parseError(p)
}

pub fn parseC(input: InputStream, initialPosition: Position) -> Result<CTranslationUnit<NodeInfo>, ParseError> {
    execParser(input, initialPosition, builtinTypeNames(), new_name_supply(), translUnitP)
        .map(|x| x.0)
}

pub fn translUnitP(p: &mut Parser) -> P<CTranslationUnit<NodeInfo>> {
    translation_unit(p)
}

pub fn extDeclP(p: &mut Parser) -> P<CExtDecl> {
    external_declaration(p)
}

pub fn statementP(p: &mut Parser) -> P<CStat> {
    statement(p)
}

pub fn expressionP(p: &mut Parser) -> P<CExpr> {
    expression(p)
}
// Original location: "templates/GenericTemplate.hs", line 1

// -----------------------------------------------------------------------------
// Some convenient typedefs

const ERROR_TOK: isize = 1;

pub enum Cont {
    Loop(isize, isize),
    NewToken,
    Accept(isize),
}

pub type Action = fn(&mut Parser, isize, isize) -> Monad<Cont>;
pub type Stack = Vec<HappyAbsSyn>;
pub type State = Action;
pub type States = Vec<Action>;

// -----------------------------------------------------------------------------
// Starting the parse

fn happyParse(p: &mut Parser, start_state: Action) -> Monad<HappyAbsSyn> {
    p.state = start_state;
    p.states.clear();
    p.stack.clear();
    p.stack.push(HappyAbsSyn::HappyErrorToken(0));
    let mut cont = Cont::NewToken;

    loop {
        cont = match cont {
            Cont::Loop(i, j) => (p.state)(p, i, j)?,
            Cont::NewToken => happyNewToken(p)?,
            Cont::Accept(j) => return happyAccept(p, j),
        }
    }
}

// -----------------------------------------------------------------------------
// Accepting the parse
//
// If the current token is ERROR_TOK, it means we've just accepted a partial
// parse (a %partial parser).  We must ignore the saved token on the top of
// the stack in this case.

fn happyAccept(p: &mut Parser, j: isize) -> Monad<HappyAbsSyn> {
    match j {
        ERROR_TOK if p.stack.len() > 1 => {
            p.stack.pop();
            Ok(p.stack.pop().unwrap())
        }
        _ => Ok(p.stack.pop().unwrap())
    }
}

// -----------------------------------------------------------------------------
// Shifting a token

fn happyShift(p: &mut Parser, new_state: Action, i: isize) -> Monad<Cont> {
    match i {
        ERROR_TOK => {
            let x = p.stack.pop().unwrap();
            let i = match x {
                HappyErrorToken(i) => i,
                _ => unreachable!(),
            };

            p.states.push(new_state);
            p.state = new_state;
            Ok(Cont::Loop(i, i))
        }
        i => {
            p.states.push(p.state);
            p.stack.push(HappyTerminal(p.token.clone()));
            p.state = new_state;
            Ok(Cont::NewToken)
        },
    }
}

// -----------------------------------------------------------------------------
// happyReduce is specialised for the common cases.

fn happySpecReduce_0(p: &mut Parser, nt: isize, val: HappyAbsSyn, j: isize) -> Monad<Cont> {
    match j {
        ERROR_TOK => happyFail(p, ERROR_TOK),
        j => {
            p.states.push(p.state);
            p.stack.push(val);
            Ok(Cont::Loop(nt, j))
        },
    }
}

fn happySpecReduce_1(p: &mut Parser, nt: isize,
                     reducer: fn(HappyAbsSyn) -> HappyAbsSyn, j: isize) -> Monad<Cont> {
    match j {
        ERROR_TOK => happyFail(p, ERROR_TOK),
        j => {
            let v1 = p.stack.pop().unwrap();
            p.state = *p.states.last().unwrap();
            let val = reducer(v1);
            p.stack.push(val);
            Ok(Cont::Loop(nt, j))
        }
    }
}

fn happySpecReduce_2(p: &mut Parser, nt: isize,
                     reducer: fn(HappyAbsSyn, HappyAbsSyn) -> HappyAbsSyn, j: isize) -> Monad<Cont> {
    match j {
        ERROR_TOK => happyFail(p, ERROR_TOK),
        j => {
            let v1 = p.stack.pop().unwrap();
            let v2 = p.stack.pop().unwrap();
            p.states.pop();
            p.state = *p.states.last().unwrap();
            let val = reducer(v1, v2);
            p.stack.push(val);
            Ok(Cont::Loop(nt, j))
        }
    }
}

fn happySpecReduce_3(p: &mut Parser, nt: isize,
                     reducer: fn(HappyAbsSyn, HappyAbsSyn, HappyAbsSyn) -> HappyAbsSyn,
                     j: isize) -> Monad<Cont> {
    match j {
        ERROR_TOK => happyFail(p, ERROR_TOK),
        j => {
            let v1 = p.stack.pop().unwrap();
            let v2 = p.stack.pop().unwrap();
            let v3 = p.stack.pop().unwrap();
            p.states.pop();
            p.states.pop();
            p.state = *p.states.last().unwrap();
            let val = reducer(v1, v2, v3);
            p.stack.push(val);
            Ok(Cont::Loop(nt, j))
        }
    }
}

fn happyReduce(p: &mut Parser, k: isize, nt: isize, reducer: fn(&mut Parser), j: isize) -> Monad<Cont> {
    match j {
        ERROR_TOK => happyFail(p, ERROR_TOK),
        j => {
            for _ in 0..k - 1 {
                p.states.pop();
            }
            p.state = *p.states.last().unwrap();
            reducer(p);
            Ok(Cont::Loop(nt, j))
        }
    }
}

fn happyMonadReduce(p: &mut Parser, k: isize, nt: isize,
                    reducer: fn(&mut Parser) -> Monad<HappyAbsSyn>,
                    j: isize) -> Monad<Cont> {
    match j {
        ERROR_TOK => happyFail(p, ERROR_TOK),
        j => {
            p.states.push(p.state);
            for _ in 0..k {
                p.states.pop();
            }
            p.state = *p.states.last().unwrap();
            let val = reducer(p)?;
            p.stack.push(val);
            Ok(Cont::Loop(nt, j))
        }
    }
}

// -----------------------------------------------------------------------------
// Moving to a new state after a reduction

fn happyGoto(p: &mut Parser, action: Action, j: isize) -> Monad<Cont> {
    p.state = action;
    action(p, j, j)
}

// -----------------------------------------------------------------------------
// Error recovery (ERROR_TOK is the error token)

fn happyFail(p: &mut Parser, i: isize) -> Monad<Cont> {
    match i {
        ERROR_TOK if p.stack.len() > 0 => happyError_(p, i),
        i => {
            p.stack.push(HappyErrorToken(i));
            (p.state)(p, ERROR_TOK, ERROR_TOK)
        },
    }
}

fn notHappyAtAll<a: 'static>() -> a {
    panic!("Internal Happy error")
}

// end of Happy Template.
