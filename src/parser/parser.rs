#![allow(unreachable_patterns, unused_parens, unused_mut)]
// action_4 is unused...
#![allow(dead_code)]

use std::rc::Rc;
use std::boxed::FnBox;
use either::Either::*;

use data::position::{Located, Pos};
use data::node::NodeInfo;
use data::ident::Ident;
use parser::tokens::*;
use parser::lexer::{lex, parse_error};
use parser::{ParseError, PState, CDeclrR};
use syntax::ops::*;
use syntax::ast::*;
use syntax::constants::*;

type Error = ParseError;
type State = PState;
type Token = CToken;

const EOF_TOK: Token = CToken::CTokEof;

macro_rules! with_pos {
    ($parser:expr, $infonode:expr, $closure:expr) => {{
        let pos1 = $infonode.pos();
        let (pos2, len) = $parser.saved_token().pos_len();
        Ok($closure(NodeInfo::new(pos1, pos2, len, $parser.new_name())))
    }};
}

macro_rules! unwrap_let {
    ($pat:pat = $expr:expr; $($tt:tt)+) => {
        if let $pat = $expr {
            $($tt)+
        } else {
            unreachable!()
        }
    }
}

// Parser produced by modified Happy Version 1.19.6

pub enum HappyAbsSyn {
    Terminal(CToken),
    ErrorToken(isize),
    NT7(Box<CTranslUnit>),
    NT8(Vec<CExtDecl>),
    NT9(Box<CExtDecl>),
    NT10(Box<CFunDef>),
    NT11(Box<CDeclr>),
    NT12(Box<CStat>),
    NT15(()),
    NT17(Vec<CBlockItem>),
    NT18(Box<CBlockItem>),
    NT21(Vec<Ident>),
    NT26(Box<CAsmStmt>),
    NT27(Option<Box<CTypeQual>>),
    NT28(Vec<CAsmOperand>),
    NT30(Box<CAsmOperand>),
    NT31(Vec<CStrLit>),
    NT32(Box<CDecl>),
    NT33(Vec<CDecl>),
    NT35(Box<(Option<Box<CStrLit>>, Vec<CAttr>)>),
    NT37(Vec<CDeclSpec>),
    NT39(Box<CDeclSpec>),
    NT41(Box<CStorageSpec>),
    NT42(Box<CFunSpec>),
    NT43(Box<CAlignSpec>),
    NT45(Box<CTypeSpec>),
    NT53(Box<CStructUnion>),
    NT54(Located<CStructTag>),
    NT59((Option<Box<CDeclr>>, Option<Box<CExpr>>)),
    NT61(Box<CEnum>),
    NT62(Vec<(Ident, Option<Box<CExpr>>)>),
    NT63((Ident, Option<Box<CExpr>>)),
    NT64(Box<CTypeQual>),
    NT65(Vec<CTypeQual>),
    NT66(Box<CDeclrR>),
    NT67(Option<Box<CStrLit>>),
    NT82((Vec<CDecl>, bool)),
    NT88(Box<FnBox(Box<CDeclrR>) -> Box<CDeclrR>>),
    NT93(Box<CInit>),
    NT94(Option<Box<CInit>>),
    NT95(CInitList),
    NT96(Vec<CDesignator>),
    NT98(Box<CDesignator>),
    NT100(Box<CExpr>),
    NT101(Vec<(Option<Box<CDecl>>, Box<CExpr>)>),
    NT102((Option<Box<CDecl>>, Box<CExpr>)),
    NT105(Vec<CExpr>),
    NT107(Located<CUnaryOp>),
    NT121(Located<CAssignOp>),
    NT124(Option<Box<CExpr>>),
    NT127(Box<CConst>),
    NT128(Box<CStrLit>),
    NT129(Vec<CString>),
    NT130(ClangCVersion),
    NT131(Ident),
    NT132(Vec<CAttr>),
    NT136(Option<Box<CAttr>>),
}


fn action_0(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        7 => happy_goto(p, action_146, j),
        8 => happy_goto(p, action_5, j),
        _ => happy_reduce_5(p, j)
    }
}

fn action_1(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_112, j),
        150 => happy_shift(p, action_113, j),
        185 => happy_shift(p, action_114, j),
        187 => happy_shift(p, action_115, j),
        188 => happy_shift(p, action_116, j),
        190 => happy_shift(p, action_117, j),
        192 => happy_shift(p, action_118, j),
        195 => happy_shift(p, action_119, j),
        198 => happy_shift(p, action_120, j),
        200 => happy_shift(p, action_121, j),
        201 => happy_shift(p, action_122, j),
        202 => happy_shift(p, action_123, j),
        203 => happy_shift(p, action_124, j),
        208 => happy_shift(p, action_125, j),
        209 => happy_shift(p, action_126, j),
        210 => happy_shift(p, action_127, j),
        211 => happy_shift(p, action_128, j),
        213 => happy_shift(p, action_129, j),
        216 => happy_shift(p, action_130, j),
        219 => happy_shift(p, action_131, j),
        220 => happy_shift(p, action_132, j),
        222 => happy_shift(p, action_133, j),
        223 => happy_shift(p, action_134, j),
        224 => happy_shift(p, action_135, j),
        226 => happy_shift(p, action_136, j),
        227 => happy_shift(p, action_137, j),
        228 => happy_shift(p, action_138, j),
        229 => happy_shift(p, action_139, j),
        230 => happy_shift(p, action_140, j),
        231 => happy_shift(p, action_141, j),
        238 => happy_shift(p, action_142, j),
        239 => happy_shift(p, action_143, j),
        240 => happy_shift(p, action_144, j),
        241 => happy_shift(p, action_145, j),
        9 => happy_goto(p, action_77, j),
        10 => happy_goto(p, action_78, j),
        11 => happy_goto(p, action_79, j),
        32 => happy_goto(p, action_80, j),
        34 => happy_goto(p, action_81, j),
        36 => happy_goto(p, action_82, j),
        37 => happy_goto(p, action_83, j),
        38 => happy_goto(p, action_84, j),
        40 => happy_goto(p, action_85, j),
        41 => happy_goto(p, action_86, j),
        42 => happy_goto(p, action_87, j),
        43 => happy_goto(p, action_88, j),
        44 => happy_goto(p, action_89, j),
        45 => happy_goto(p, action_90, j),
        46 => happy_goto(p, action_91, j),
        47 => happy_goto(p, action_92, j),
        48 => happy_goto(p, action_93, j),
        49 => happy_goto(p, action_94, j),
        50 => happy_goto(p, action_95, j),
        51 => happy_goto(p, action_96, j),
        52 => happy_goto(p, action_97, j),
        53 => happy_goto(p, action_98, j),
        54 => happy_goto(p, action_99, j),
        61 => happy_goto(p, action_100, j),
        65 => happy_goto(p, action_101, j),
        75 => happy_goto(p, action_102, j),
        76 => happy_goto(p, action_103, j),
        77 => happy_goto(p, action_104, j),
        78 => happy_goto(p, action_105, j),
        79 => happy_goto(p, action_106, j),
        80 => happy_goto(p, action_107, j),
        81 => happy_goto(p, action_108, j),
        132 => happy_goto(p, action_109, j),
        133 => happy_goto(p, action_110, j),
        134 => happy_goto(p, action_111, j),
        _ => happy_reduce_473(p, j)
    }
}

fn action_2(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_26, j),
        144 => happy_shift(p, action_27, j),
        145 => happy_shift(p, action_28, j),
        146 => happy_shift(p, action_29, j),
        147 => happy_shift(p, action_30, j),
        148 => happy_shift(p, action_31, j),
        149 => happy_shift(p, action_32, j),
        150 => happy_shift(p, action_33, j),
        153 => happy_shift(p, action_34, j),
        164 => happy_shift(p, action_35, j),
        180 => happy_shift(p, action_61, j),
        181 => happy_shift(p, action_62, j),
        184 => happy_shift(p, action_36, j),
        187 => happy_shift(p, action_63, j),
        189 => happy_shift(p, action_64, j),
        191 => happy_shift(p, action_65, j),
        194 => happy_shift(p, action_66, j),
        196 => happy_shift(p, action_67, j),
        197 => happy_shift(p, action_68, j),
        204 => happy_shift(p, action_69, j),
        205 => happy_shift(p, action_37, j),
        206 => happy_shift(p, action_70, j),
        207 => happy_shift(p, action_71, j),
        218 => happy_shift(p, action_72, j),
        221 => happy_shift(p, action_38, j),
        225 => happy_shift(p, action_73, j),
        233 => happy_shift(p, action_74, j),
        234 => happy_shift(p, action_39, j),
        235 => happy_shift(p, action_40, j),
        236 => happy_shift(p, action_41, j),
        237 => happy_shift(p, action_42, j),
        238 => happy_shift(p, action_75, j),
        239 => happy_shift(p, action_76, j),
        241 => happy_shift(p, action_44, j),
        242 => happy_shift(p, action_45, j),
        243 => happy_shift(p, action_46, j),
        244 => happy_shift(p, action_47, j),
        245 => happy_shift(p, action_48, j),
        246 => happy_shift(p, action_49, j),
        247 => happy_shift(p, action_50, j),
        12 => happy_goto(p, action_51, j),
        13 => happy_goto(p, action_52, j),
        14 => happy_goto(p, action_53, j),
        22 => happy_goto(p, action_54, j),
        23 => happy_goto(p, action_55, j),
        24 => happy_goto(p, action_56, j),
        25 => happy_goto(p, action_57, j),
        26 => happy_goto(p, action_58, j),
        100 => happy_goto(p, action_6, j),
        104 => happy_goto(p, action_7, j),
        106 => happy_goto(p, action_8, j),
        107 => happy_goto(p, action_9, j),
        108 => happy_goto(p, action_10, j),
        109 => happy_goto(p, action_11, j),
        110 => happy_goto(p, action_12, j),
        111 => happy_goto(p, action_13, j),
        112 => happy_goto(p, action_14, j),
        113 => happy_goto(p, action_15, j),
        114 => happy_goto(p, action_16, j),
        115 => happy_goto(p, action_17, j),
        116 => happy_goto(p, action_18, j),
        117 => happy_goto(p, action_19, j),
        118 => happy_goto(p, action_20, j),
        119 => happy_goto(p, action_21, j),
        120 => happy_goto(p, action_22, j),
        122 => happy_goto(p, action_59, j),
        127 => happy_goto(p, action_24, j),
        128 => happy_goto(p, action_25, j),
        131 => happy_goto(p, action_60, j),
        _ => happy_fail(p, j)
    }
}

fn action_3(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_26, j),
        144 => happy_shift(p, action_27, j),
        145 => happy_shift(p, action_28, j),
        146 => happy_shift(p, action_29, j),
        147 => happy_shift(p, action_30, j),
        148 => happy_shift(p, action_31, j),
        149 => happy_shift(p, action_32, j),
        150 => happy_shift(p, action_33, j),
        153 => happy_shift(p, action_34, j),
        164 => happy_shift(p, action_35, j),
        184 => happy_shift(p, action_36, j),
        205 => happy_shift(p, action_37, j),
        221 => happy_shift(p, action_38, j),
        234 => happy_shift(p, action_39, j),
        235 => happy_shift(p, action_40, j),
        236 => happy_shift(p, action_41, j),
        237 => happy_shift(p, action_42, j),
        238 => happy_shift(p, action_43, j),
        241 => happy_shift(p, action_44, j),
        242 => happy_shift(p, action_45, j),
        243 => happy_shift(p, action_46, j),
        244 => happy_shift(p, action_47, j),
        245 => happy_shift(p, action_48, j),
        246 => happy_shift(p, action_49, j),
        247 => happy_shift(p, action_50, j),
        100 => happy_goto(p, action_6, j),
        104 => happy_goto(p, action_7, j),
        106 => happy_goto(p, action_8, j),
        107 => happy_goto(p, action_9, j),
        108 => happy_goto(p, action_10, j),
        109 => happy_goto(p, action_11, j),
        110 => happy_goto(p, action_12, j),
        111 => happy_goto(p, action_13, j),
        112 => happy_goto(p, action_14, j),
        113 => happy_goto(p, action_15, j),
        114 => happy_goto(p, action_16, j),
        115 => happy_goto(p, action_17, j),
        116 => happy_goto(p, action_18, j),
        117 => happy_goto(p, action_19, j),
        118 => happy_goto(p, action_20, j),
        119 => happy_goto(p, action_21, j),
        120 => happy_goto(p, action_22, j),
        122 => happy_goto(p, action_23, j),
        127 => happy_goto(p, action_24, j),
        128 => happy_goto(p, action_25, j),
        _ => happy_fail(p, j)
    }
}

fn action_4(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        8 => happy_goto(p, action_5, j),
        _ => happy_fail(p, j)
    }
}

fn action_5(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_112, j),
        150 => happy_shift(p, action_113, j),
        180 => happy_shift(p, action_337, j),
        185 => happy_shift(p, action_114, j),
        187 => happy_shift(p, action_115, j),
        188 => happy_shift(p, action_116, j),
        190 => happy_shift(p, action_117, j),
        192 => happy_shift(p, action_118, j),
        195 => happy_shift(p, action_119, j),
        198 => happy_shift(p, action_120, j),
        200 => happy_shift(p, action_121, j),
        201 => happy_shift(p, action_122, j),
        202 => happy_shift(p, action_123, j),
        203 => happy_shift(p, action_124, j),
        208 => happy_shift(p, action_125, j),
        209 => happy_shift(p, action_126, j),
        210 => happy_shift(p, action_127, j),
        211 => happy_shift(p, action_128, j),
        213 => happy_shift(p, action_129, j),
        216 => happy_shift(p, action_130, j),
        219 => happy_shift(p, action_131, j),
        220 => happy_shift(p, action_132, j),
        222 => happy_shift(p, action_133, j),
        223 => happy_shift(p, action_134, j),
        224 => happy_shift(p, action_135, j),
        226 => happy_shift(p, action_136, j),
        227 => happy_shift(p, action_137, j),
        228 => happy_shift(p, action_138, j),
        229 => happy_shift(p, action_139, j),
        230 => happy_shift(p, action_140, j),
        231 => happy_shift(p, action_141, j),
        238 => happy_shift(p, action_142, j),
        239 => happy_shift(p, action_143, j),
        240 => happy_shift(p, action_144, j),
        241 => happy_shift(p, action_145, j),
        249 => happy_reduce_4(p, j),
        9 => happy_goto(p, action_336, j),
        10 => happy_goto(p, action_78, j),
        11 => happy_goto(p, action_79, j),
        32 => happy_goto(p, action_80, j),
        34 => happy_goto(p, action_81, j),
        36 => happy_goto(p, action_82, j),
        37 => happy_goto(p, action_83, j),
        38 => happy_goto(p, action_84, j),
        40 => happy_goto(p, action_85, j),
        41 => happy_goto(p, action_86, j),
        42 => happy_goto(p, action_87, j),
        43 => happy_goto(p, action_88, j),
        44 => happy_goto(p, action_89, j),
        45 => happy_goto(p, action_90, j),
        46 => happy_goto(p, action_91, j),
        47 => happy_goto(p, action_92, j),
        48 => happy_goto(p, action_93, j),
        49 => happy_goto(p, action_94, j),
        50 => happy_goto(p, action_95, j),
        51 => happy_goto(p, action_96, j),
        52 => happy_goto(p, action_97, j),
        53 => happy_goto(p, action_98, j),
        54 => happy_goto(p, action_99, j),
        61 => happy_goto(p, action_100, j),
        65 => happy_goto(p, action_101, j),
        75 => happy_goto(p, action_102, j),
        76 => happy_goto(p, action_103, j),
        77 => happy_goto(p, action_104, j),
        78 => happy_goto(p, action_105, j),
        79 => happy_goto(p, action_106, j),
        80 => happy_goto(p, action_107, j),
        81 => happy_goto(p, action_108, j),
        132 => happy_goto(p, action_109, j),
        133 => happy_goto(p, action_110, j),
        134 => happy_goto(p, action_111, j),
        _ => happy_reduce_473(p, j)
    }
}

fn action_6(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_378(p, j)
    }
}

fn action_7(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_330, j),
        140 => happy_shift(p, action_331, j),
        142 => happy_shift(p, action_332, j),
        143 => happy_shift(p, action_333, j),
        146 => happy_shift(p, action_334, j),
        147 => happy_shift(p, action_335, j),
        _ => happy_reduce_390(p, j)
    }
}

fn action_8(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        168 => happy_shift(p, action_319, j),
        169 => happy_shift(p, action_320, j),
        170 => happy_shift(p, action_321, j),
        171 => happy_shift(p, action_322, j),
        172 => happy_shift(p, action_323, j),
        173 => happy_shift(p, action_324, j),
        174 => happy_shift(p, action_325, j),
        175 => happy_shift(p, action_326, j),
        176 => happy_shift(p, action_327, j),
        177 => happy_shift(p, action_328, j),
        178 => happy_shift(p, action_329, j),
        121 => happy_goto(p, action_318, j),
        _ => happy_reduce_408(p, j)
    }
}

fn action_9(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_26, j),
        144 => happy_shift(p, action_27, j),
        145 => happy_shift(p, action_28, j),
        146 => happy_shift(p, action_29, j),
        147 => happy_shift(p, action_30, j),
        148 => happy_shift(p, action_31, j),
        149 => happy_shift(p, action_32, j),
        150 => happy_shift(p, action_33, j),
        153 => happy_shift(p, action_34, j),
        164 => happy_shift(p, action_35, j),
        184 => happy_shift(p, action_36, j),
        205 => happy_shift(p, action_37, j),
        221 => happy_shift(p, action_38, j),
        234 => happy_shift(p, action_39, j),
        235 => happy_shift(p, action_40, j),
        236 => happy_shift(p, action_41, j),
        237 => happy_shift(p, action_42, j),
        238 => happy_shift(p, action_43, j),
        241 => happy_shift(p, action_44, j),
        242 => happy_shift(p, action_45, j),
        243 => happy_shift(p, action_46, j),
        244 => happy_shift(p, action_47, j),
        245 => happy_shift(p, action_48, j),
        246 => happy_shift(p, action_49, j),
        247 => happy_shift(p, action_50, j),
        100 => happy_goto(p, action_6, j),
        104 => happy_goto(p, action_7, j),
        106 => happy_goto(p, action_261, j),
        107 => happy_goto(p, action_9, j),
        108 => happy_goto(p, action_317, j),
        127 => happy_goto(p, action_24, j),
        128 => happy_goto(p, action_25, j),
        _ => happy_fail(p, j)
    }
}

fn action_10(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_410(p, j)
    }
}

fn action_11(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        150 => happy_shift(p, action_314, j),
        151 => happy_shift(p, action_315, j),
        152 => happy_shift(p, action_316, j),
        _ => happy_reduce_414(p, j)
    }
}

fn action_12(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        148 => happy_shift(p, action_312, j),
        149 => happy_shift(p, action_313, j),
        _ => happy_reduce_417(p, j)
    }
}

fn action_13(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        154 => happy_shift(p, action_310, j),
        155 => happy_shift(p, action_311, j),
        _ => happy_reduce_420(p, j)
    }
}

fn action_14(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        156 => happy_shift(p, action_306, j),
        157 => happy_shift(p, action_307, j),
        158 => happy_shift(p, action_308, j),
        159 => happy_shift(p, action_309, j),
        _ => happy_reduce_425(p, j)
    }
}

fn action_15(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        160 => happy_shift(p, action_304, j),
        161 => happy_shift(p, action_305, j),
        _ => happy_reduce_428(p, j)
    }
}

fn action_16(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        153 => happy_shift(p, action_303, j),
        _ => happy_reduce_430(p, j)
    }
}

fn action_17(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        162 => happy_shift(p, action_302, j),
        _ => happy_reduce_432(p, j)
    }
}

fn action_18(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        163 => happy_shift(p, action_301, j),
        _ => happy_reduce_434(p, j)
    }
}

fn action_19(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        164 => happy_shift(p, action_300, j),
        _ => happy_reduce_436(p, j)
    }
}

fn action_20(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        165 => happy_shift(p, action_298, j),
        166 => happy_shift(p, action_299, j),
        _ => happy_reduce_438(p, j)
    }
}

fn action_21(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_441(p, j)
    }
}

fn action_22(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        179 => happy_shift(p, action_297, j),
        _ => happy_reduce_454(p, j)
    }
}

fn action_23(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        249 => Ok(Cont::Accept(j)),
        _ => happy_fail(p, j)
    }
}

fn action_24(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_362(p, j)
    }
}

fn action_25(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_363(p, j)
    }
}

fn action_26(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_26, j),
        144 => happy_shift(p, action_27, j),
        145 => happy_shift(p, action_28, j),
        146 => happy_shift(p, action_29, j),
        147 => happy_shift(p, action_30, j),
        148 => happy_shift(p, action_31, j),
        149 => happy_shift(p, action_32, j),
        150 => happy_shift(p, action_33, j),
        153 => happy_shift(p, action_34, j),
        164 => happy_shift(p, action_35, j),
        181 => happy_shift(p, action_62, j),
        184 => happy_shift(p, action_36, j),
        190 => happy_shift(p, action_117, j),
        192 => happy_shift(p, action_118, j),
        195 => happy_shift(p, action_119, j),
        198 => happy_shift(p, action_120, j),
        200 => happy_shift(p, action_121, j),
        202 => happy_shift(p, action_123, j),
        203 => happy_shift(p, action_124, j),
        205 => happy_shift(p, action_37, j),
        209 => happy_shift(p, action_126, j),
        210 => happy_shift(p, action_127, j),
        211 => happy_shift(p, action_128, j),
        219 => happy_shift(p, action_131, j),
        220 => happy_shift(p, action_132, j),
        221 => happy_shift(p, action_38, j),
        224 => happy_shift(p, action_135, j),
        227 => happy_shift(p, action_137, j),
        229 => happy_shift(p, action_139, j),
        230 => happy_shift(p, action_140, j),
        231 => happy_shift(p, action_141, j),
        234 => happy_shift(p, action_39, j),
        235 => happy_shift(p, action_40, j),
        236 => happy_shift(p, action_41, j),
        237 => happy_shift(p, action_42, j),
        238 => happy_shift(p, action_43, j),
        239 => happy_shift(p, action_143, j),
        240 => happy_shift(p, action_144, j),
        241 => happy_shift(p, action_44, j),
        242 => happy_shift(p, action_45, j),
        243 => happy_shift(p, action_46, j),
        244 => happy_shift(p, action_47, j),
        245 => happy_shift(p, action_48, j),
        246 => happy_shift(p, action_49, j),
        247 => happy_shift(p, action_50, j),
        14 => happy_goto(p, action_288, j),
        44 => happy_goto(p, action_289, j),
        45 => happy_goto(p, action_90, j),
        47 => happy_goto(p, action_290, j),
        49 => happy_goto(p, action_291, j),
        51 => happy_goto(p, action_292, j),
        52 => happy_goto(p, action_97, j),
        53 => happy_goto(p, action_98, j),
        54 => happy_goto(p, action_99, j),
        61 => happy_goto(p, action_100, j),
        65 => happy_goto(p, action_293, j),
        86 => happy_goto(p, action_294, j),
        100 => happy_goto(p, action_6, j),
        104 => happy_goto(p, action_7, j),
        106 => happy_goto(p, action_8, j),
        107 => happy_goto(p, action_9, j),
        108 => happy_goto(p, action_10, j),
        109 => happy_goto(p, action_11, j),
        110 => happy_goto(p, action_12, j),
        111 => happy_goto(p, action_13, j),
        112 => happy_goto(p, action_14, j),
        113 => happy_goto(p, action_15, j),
        114 => happy_goto(p, action_16, j),
        115 => happy_goto(p, action_17, j),
        116 => happy_goto(p, action_18, j),
        117 => happy_goto(p, action_19, j),
        118 => happy_goto(p, action_20, j),
        119 => happy_goto(p, action_21, j),
        120 => happy_goto(p, action_22, j),
        122 => happy_goto(p, action_295, j),
        127 => happy_goto(p, action_24, j),
        128 => happy_goto(p, action_25, j),
        132 => happy_goto(p, action_109, j),
        133 => happy_goto(p, action_296, j),
        134 => happy_goto(p, action_111, j),
        _ => happy_reduce_473(p, j)
    }
}

fn action_27(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_407(p, j)
    }
}

fn action_28(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_406(p, j)
    }
}

fn action_29(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_275, j),
        144 => happy_shift(p, action_27, j),
        145 => happy_shift(p, action_28, j),
        146 => happy_shift(p, action_29, j),
        147 => happy_shift(p, action_30, j),
        148 => happy_shift(p, action_31, j),
        149 => happy_shift(p, action_32, j),
        150 => happy_shift(p, action_33, j),
        153 => happy_shift(p, action_34, j),
        164 => happy_shift(p, action_35, j),
        184 => happy_shift(p, action_36, j),
        205 => happy_shift(p, action_37, j),
        221 => happy_shift(p, action_38, j),
        234 => happy_shift(p, action_39, j),
        235 => happy_shift(p, action_40, j),
        236 => happy_shift(p, action_41, j),
        237 => happy_shift(p, action_42, j),
        238 => happy_shift(p, action_43, j),
        241 => happy_shift(p, action_44, j),
        242 => happy_shift(p, action_45, j),
        243 => happy_shift(p, action_46, j),
        244 => happy_shift(p, action_47, j),
        245 => happy_shift(p, action_48, j),
        246 => happy_shift(p, action_49, j),
        247 => happy_shift(p, action_50, j),
        100 => happy_goto(p, action_6, j),
        104 => happy_goto(p, action_7, j),
        106 => happy_goto(p, action_287, j),
        107 => happy_goto(p, action_9, j),
        127 => happy_goto(p, action_24, j),
        128 => happy_goto(p, action_25, j),
        _ => happy_fail(p, j)
    }
}

fn action_30(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_275, j),
        144 => happy_shift(p, action_27, j),
        145 => happy_shift(p, action_28, j),
        146 => happy_shift(p, action_29, j),
        147 => happy_shift(p, action_30, j),
        148 => happy_shift(p, action_31, j),
        149 => happy_shift(p, action_32, j),
        150 => happy_shift(p, action_33, j),
        153 => happy_shift(p, action_34, j),
        164 => happy_shift(p, action_35, j),
        184 => happy_shift(p, action_36, j),
        205 => happy_shift(p, action_37, j),
        221 => happy_shift(p, action_38, j),
        234 => happy_shift(p, action_39, j),
        235 => happy_shift(p, action_40, j),
        236 => happy_shift(p, action_41, j),
        237 => happy_shift(p, action_42, j),
        238 => happy_shift(p, action_43, j),
        241 => happy_shift(p, action_44, j),
        242 => happy_shift(p, action_45, j),
        243 => happy_shift(p, action_46, j),
        244 => happy_shift(p, action_47, j),
        245 => happy_shift(p, action_48, j),
        246 => happy_shift(p, action_49, j),
        247 => happy_shift(p, action_50, j),
        100 => happy_goto(p, action_6, j),
        104 => happy_goto(p, action_7, j),
        106 => happy_goto(p, action_286, j),
        107 => happy_goto(p, action_9, j),
        127 => happy_goto(p, action_24, j),
        128 => happy_goto(p, action_25, j),
        _ => happy_fail(p, j)
    }
}

fn action_31(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_404(p, j)
    }
}

fn action_32(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_405(p, j)
    }
}

fn action_33(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_403(p, j)
    }
}

fn action_34(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_402(p, j)
    }
}

fn action_35(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        238 => happy_shift(p, action_256, j),
        239 => happy_shift(p, action_76, j),
        131 => happy_goto(p, action_285, j),
        _ => happy_fail(p, j)
    }
}

fn action_36(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_284, j),
        144 => happy_shift(p, action_27, j),
        145 => happy_shift(p, action_28, j),
        146 => happy_shift(p, action_29, j),
        147 => happy_shift(p, action_30, j),
        148 => happy_shift(p, action_31, j),
        149 => happy_shift(p, action_32, j),
        150 => happy_shift(p, action_33, j),
        153 => happy_shift(p, action_34, j),
        164 => happy_shift(p, action_35, j),
        184 => happy_shift(p, action_36, j),
        205 => happy_shift(p, action_37, j),
        221 => happy_shift(p, action_38, j),
        234 => happy_shift(p, action_39, j),
        235 => happy_shift(p, action_40, j),
        236 => happy_shift(p, action_41, j),
        237 => happy_shift(p, action_42, j),
        238 => happy_shift(p, action_43, j),
        241 => happy_shift(p, action_44, j),
        242 => happy_shift(p, action_45, j),
        243 => happy_shift(p, action_46, j),
        244 => happy_shift(p, action_47, j),
        245 => happy_shift(p, action_48, j),
        246 => happy_shift(p, action_49, j),
        247 => happy_shift(p, action_50, j),
        100 => happy_goto(p, action_6, j),
        104 => happy_goto(p, action_7, j),
        106 => happy_goto(p, action_283, j),
        107 => happy_goto(p, action_9, j),
        127 => happy_goto(p, action_24, j),
        128 => happy_goto(p, action_25, j),
        _ => happy_fail(p, j)
    }
}

fn action_37(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_282, j),
        _ => happy_fail(p, j)
    }
}

fn action_38(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_281, j),
        144 => happy_shift(p, action_27, j),
        145 => happy_shift(p, action_28, j),
        146 => happy_shift(p, action_29, j),
        147 => happy_shift(p, action_30, j),
        148 => happy_shift(p, action_31, j),
        149 => happy_shift(p, action_32, j),
        150 => happy_shift(p, action_33, j),
        153 => happy_shift(p, action_34, j),
        164 => happy_shift(p, action_35, j),
        184 => happy_shift(p, action_36, j),
        205 => happy_shift(p, action_37, j),
        221 => happy_shift(p, action_38, j),
        234 => happy_shift(p, action_39, j),
        235 => happy_shift(p, action_40, j),
        236 => happy_shift(p, action_41, j),
        237 => happy_shift(p, action_42, j),
        238 => happy_shift(p, action_43, j),
        241 => happy_shift(p, action_44, j),
        242 => happy_shift(p, action_45, j),
        243 => happy_shift(p, action_46, j),
        244 => happy_shift(p, action_47, j),
        245 => happy_shift(p, action_48, j),
        246 => happy_shift(p, action_49, j),
        247 => happy_shift(p, action_50, j),
        100 => happy_goto(p, action_6, j),
        104 => happy_goto(p, action_7, j),
        106 => happy_goto(p, action_280, j),
        107 => happy_goto(p, action_9, j),
        127 => happy_goto(p, action_24, j),
        128 => happy_goto(p, action_25, j),
        _ => happy_fail(p, j)
    }
}

fn action_39(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_464(p, j)
    }
}

fn action_40(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_463(p, j)
    }
}

fn action_41(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_465(p, j)
    }
}

fn action_42(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        237 => happy_shift(p, action_279, j),
        129 => happy_goto(p, action_278, j),
        _ => happy_reduce_466(p, j)
    }
}

fn action_43(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_361(p, j)
    }
}

fn action_44(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_26, j),
        144 => happy_shift(p, action_27, j),
        145 => happy_shift(p, action_28, j),
        146 => happy_shift(p, action_29, j),
        147 => happy_shift(p, action_30, j),
        148 => happy_shift(p, action_31, j),
        149 => happy_shift(p, action_32, j),
        150 => happy_shift(p, action_33, j),
        153 => happy_shift(p, action_34, j),
        164 => happy_shift(p, action_35, j),
        184 => happy_shift(p, action_36, j),
        205 => happy_shift(p, action_37, j),
        221 => happy_shift(p, action_38, j),
        234 => happy_shift(p, action_39, j),
        235 => happy_shift(p, action_40, j),
        236 => happy_shift(p, action_41, j),
        237 => happy_shift(p, action_42, j),
        238 => happy_shift(p, action_43, j),
        241 => happy_shift(p, action_44, j),
        242 => happy_shift(p, action_45, j),
        243 => happy_shift(p, action_46, j),
        244 => happy_shift(p, action_47, j),
        245 => happy_shift(p, action_48, j),
        246 => happy_shift(p, action_49, j),
        247 => happy_shift(p, action_50, j),
        100 => happy_goto(p, action_6, j),
        104 => happy_goto(p, action_7, j),
        106 => happy_goto(p, action_261, j),
        107 => happy_goto(p, action_9, j),
        108 => happy_goto(p, action_277, j),
        127 => happy_goto(p, action_24, j),
        128 => happy_goto(p, action_25, j),
        _ => happy_fail(p, j)
    }
}

fn action_45(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_275, j),
        144 => happy_shift(p, action_27, j),
        145 => happy_shift(p, action_28, j),
        146 => happy_shift(p, action_29, j),
        147 => happy_shift(p, action_30, j),
        148 => happy_shift(p, action_31, j),
        149 => happy_shift(p, action_32, j),
        150 => happy_shift(p, action_33, j),
        153 => happy_shift(p, action_34, j),
        164 => happy_shift(p, action_35, j),
        184 => happy_shift(p, action_36, j),
        205 => happy_shift(p, action_37, j),
        221 => happy_shift(p, action_38, j),
        234 => happy_shift(p, action_39, j),
        235 => happy_shift(p, action_40, j),
        236 => happy_shift(p, action_41, j),
        237 => happy_shift(p, action_42, j),
        238 => happy_shift(p, action_43, j),
        241 => happy_shift(p, action_44, j),
        242 => happy_shift(p, action_45, j),
        243 => happy_shift(p, action_46, j),
        244 => happy_shift(p, action_47, j),
        245 => happy_shift(p, action_48, j),
        246 => happy_shift(p, action_49, j),
        247 => happy_shift(p, action_50, j),
        100 => happy_goto(p, action_6, j),
        104 => happy_goto(p, action_7, j),
        106 => happy_goto(p, action_276, j),
        107 => happy_goto(p, action_9, j),
        127 => happy_goto(p, action_24, j),
        128 => happy_goto(p, action_25, j),
        _ => happy_fail(p, j)
    }
}

fn action_46(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_275, j),
        144 => happy_shift(p, action_27, j),
        145 => happy_shift(p, action_28, j),
        146 => happy_shift(p, action_29, j),
        147 => happy_shift(p, action_30, j),
        148 => happy_shift(p, action_31, j),
        149 => happy_shift(p, action_32, j),
        150 => happy_shift(p, action_33, j),
        153 => happy_shift(p, action_34, j),
        164 => happy_shift(p, action_35, j),
        184 => happy_shift(p, action_36, j),
        205 => happy_shift(p, action_37, j),
        221 => happy_shift(p, action_38, j),
        234 => happy_shift(p, action_39, j),
        235 => happy_shift(p, action_40, j),
        236 => happy_shift(p, action_41, j),
        237 => happy_shift(p, action_42, j),
        238 => happy_shift(p, action_43, j),
        241 => happy_shift(p, action_44, j),
        242 => happy_shift(p, action_45, j),
        243 => happy_shift(p, action_46, j),
        244 => happy_shift(p, action_47, j),
        245 => happy_shift(p, action_48, j),
        246 => happy_shift(p, action_49, j),
        247 => happy_shift(p, action_50, j),
        100 => happy_goto(p, action_6, j),
        104 => happy_goto(p, action_7, j),
        106 => happy_goto(p, action_274, j),
        107 => happy_goto(p, action_9, j),
        127 => happy_goto(p, action_24, j),
        128 => happy_goto(p, action_25, j),
        _ => happy_fail(p, j)
    }
}

fn action_47(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_273, j),
        _ => happy_fail(p, j)
    }
}

fn action_48(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_272, j),
        _ => happy_fail(p, j)
    }
}

fn action_49(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_271, j),
        _ => happy_fail(p, j)
    }
}

fn action_50(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_270, j),
        _ => happy_fail(p, j)
    }
}

fn action_51(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        249 => Ok(Cont::Accept(j)),
        _ => happy_fail(p, j)
    }
}

fn action_52(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_27(p, j)
    }
}

fn action_53(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_28(p, j)
    }
}

fn action_54(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_29(p, j)
    }
}

fn action_55(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_30(p, j)
    }
}

fn action_56(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_31(p, j)
    }
}

fn action_57(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_32(p, j)
    }
}

fn action_58(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_33(p, j)
    }
}

fn action_59(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        180 => happy_shift(p, action_269, j),
        _ => happy_fail(p, j)
    }
}

fn action_60(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        167 => happy_shift(p, action_268, j),
        _ => happy_fail(p, j)
    }
}

fn action_61(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_56(p, j)
    }
}

fn action_62(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        15 => happy_goto(p, action_267, j),
        _ => happy_reduce_40(p, j)
    }
}

fn action_63(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        186 => happy_shift(p, action_173, j),
        193 => happy_shift(p, action_174, j),
        214 => happy_shift(p, action_175, j),
        215 => happy_shift(p, action_176, j),
        217 => happy_shift(p, action_177, j),
        232 => happy_shift(p, action_178, j),
        27 => happy_goto(p, action_265, j),
        64 => happy_goto(p, action_266, j),
        _ => happy_reduce_74(p, j)
    }
}

fn action_64(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        180 => happy_shift(p, action_264, j),
        _ => happy_fail(p, j)
    }
}

fn action_65(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_26, j),
        144 => happy_shift(p, action_27, j),
        145 => happy_shift(p, action_28, j),
        146 => happy_shift(p, action_29, j),
        147 => happy_shift(p, action_30, j),
        148 => happy_shift(p, action_31, j),
        149 => happy_shift(p, action_32, j),
        150 => happy_shift(p, action_33, j),
        153 => happy_shift(p, action_34, j),
        164 => happy_shift(p, action_35, j),
        184 => happy_shift(p, action_36, j),
        205 => happy_shift(p, action_37, j),
        221 => happy_shift(p, action_38, j),
        234 => happy_shift(p, action_39, j),
        235 => happy_shift(p, action_40, j),
        236 => happy_shift(p, action_41, j),
        237 => happy_shift(p, action_42, j),
        238 => happy_shift(p, action_43, j),
        241 => happy_shift(p, action_44, j),
        242 => happy_shift(p, action_45, j),
        243 => happy_shift(p, action_46, j),
        244 => happy_shift(p, action_47, j),
        245 => happy_shift(p, action_48, j),
        246 => happy_shift(p, action_49, j),
        247 => happy_shift(p, action_50, j),
        100 => happy_goto(p, action_6, j),
        104 => happy_goto(p, action_7, j),
        106 => happy_goto(p, action_261, j),
        107 => happy_goto(p, action_9, j),
        108 => happy_goto(p, action_10, j),
        109 => happy_goto(p, action_11, j),
        110 => happy_goto(p, action_12, j),
        111 => happy_goto(p, action_13, j),
        112 => happy_goto(p, action_14, j),
        113 => happy_goto(p, action_15, j),
        114 => happy_goto(p, action_16, j),
        115 => happy_goto(p, action_17, j),
        116 => happy_goto(p, action_18, j),
        117 => happy_goto(p, action_19, j),
        118 => happy_goto(p, action_20, j),
        119 => happy_goto(p, action_262, j),
        126 => happy_goto(p, action_263, j),
        127 => happy_goto(p, action_24, j),
        128 => happy_goto(p, action_25, j),
        _ => happy_fail(p, j)
    }
}

fn action_66(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        180 => happy_shift(p, action_260, j),
        _ => happy_fail(p, j)
    }
}

fn action_67(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        167 => happy_shift(p, action_259, j),
        _ => happy_fail(p, j)
    }
}

fn action_68(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_26, j),
        144 => happy_shift(p, action_27, j),
        145 => happy_shift(p, action_28, j),
        146 => happy_shift(p, action_29, j),
        147 => happy_shift(p, action_30, j),
        148 => happy_shift(p, action_31, j),
        149 => happy_shift(p, action_32, j),
        150 => happy_shift(p, action_33, j),
        153 => happy_shift(p, action_34, j),
        164 => happy_shift(p, action_35, j),
        180 => happy_shift(p, action_61, j),
        181 => happy_shift(p, action_62, j),
        184 => happy_shift(p, action_36, j),
        187 => happy_shift(p, action_63, j),
        189 => happy_shift(p, action_64, j),
        191 => happy_shift(p, action_65, j),
        194 => happy_shift(p, action_66, j),
        196 => happy_shift(p, action_67, j),
        197 => happy_shift(p, action_68, j),
        204 => happy_shift(p, action_69, j),
        205 => happy_shift(p, action_37, j),
        206 => happy_shift(p, action_70, j),
        207 => happy_shift(p, action_71, j),
        218 => happy_shift(p, action_72, j),
        221 => happy_shift(p, action_38, j),
        225 => happy_shift(p, action_73, j),
        233 => happy_shift(p, action_74, j),
        234 => happy_shift(p, action_39, j),
        235 => happy_shift(p, action_40, j),
        236 => happy_shift(p, action_41, j),
        237 => happy_shift(p, action_42, j),
        238 => happy_shift(p, action_75, j),
        239 => happy_shift(p, action_76, j),
        241 => happy_shift(p, action_44, j),
        242 => happy_shift(p, action_45, j),
        243 => happy_shift(p, action_46, j),
        244 => happy_shift(p, action_47, j),
        245 => happy_shift(p, action_48, j),
        246 => happy_shift(p, action_49, j),
        247 => happy_shift(p, action_50, j),
        12 => happy_goto(p, action_258, j),
        13 => happy_goto(p, action_52, j),
        14 => happy_goto(p, action_53, j),
        22 => happy_goto(p, action_54, j),
        23 => happy_goto(p, action_55, j),
        24 => happy_goto(p, action_56, j),
        25 => happy_goto(p, action_57, j),
        26 => happy_goto(p, action_58, j),
        100 => happy_goto(p, action_6, j),
        104 => happy_goto(p, action_7, j),
        106 => happy_goto(p, action_8, j),
        107 => happy_goto(p, action_9, j),
        108 => happy_goto(p, action_10, j),
        109 => happy_goto(p, action_11, j),
        110 => happy_goto(p, action_12, j),
        111 => happy_goto(p, action_13, j),
        112 => happy_goto(p, action_14, j),
        113 => happy_goto(p, action_15, j),
        114 => happy_goto(p, action_16, j),
        115 => happy_goto(p, action_17, j),
        116 => happy_goto(p, action_18, j),
        117 => happy_goto(p, action_19, j),
        118 => happy_goto(p, action_20, j),
        119 => happy_goto(p, action_21, j),
        120 => happy_goto(p, action_22, j),
        122 => happy_goto(p, action_59, j),
        127 => happy_goto(p, action_24, j),
        128 => happy_goto(p, action_25, j),
        131 => happy_goto(p, action_60, j),
        _ => happy_fail(p, j)
    }
}

fn action_69(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_257, j),
        _ => happy_fail(p, j)
    }
}

fn action_70(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        150 => happy_shift(p, action_255, j),
        238 => happy_shift(p, action_256, j),
        239 => happy_shift(p, action_76, j),
        131 => happy_goto(p, action_254, j),
        _ => happy_fail(p, j)
    }
}

fn action_71(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_253, j),
        _ => happy_fail(p, j)
    }
}

fn action_72(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_26, j),
        144 => happy_shift(p, action_27, j),
        145 => happy_shift(p, action_28, j),
        146 => happy_shift(p, action_29, j),
        147 => happy_shift(p, action_30, j),
        148 => happy_shift(p, action_31, j),
        149 => happy_shift(p, action_32, j),
        150 => happy_shift(p, action_33, j),
        153 => happy_shift(p, action_34, j),
        164 => happy_shift(p, action_35, j),
        184 => happy_shift(p, action_36, j),
        205 => happy_shift(p, action_37, j),
        221 => happy_shift(p, action_38, j),
        234 => happy_shift(p, action_39, j),
        235 => happy_shift(p, action_40, j),
        236 => happy_shift(p, action_41, j),
        237 => happy_shift(p, action_42, j),
        238 => happy_shift(p, action_43, j),
        241 => happy_shift(p, action_44, j),
        242 => happy_shift(p, action_45, j),
        243 => happy_shift(p, action_46, j),
        244 => happy_shift(p, action_47, j),
        245 => happy_shift(p, action_48, j),
        246 => happy_shift(p, action_49, j),
        247 => happy_shift(p, action_50, j),
        100 => happy_goto(p, action_6, j),
        104 => happy_goto(p, action_7, j),
        106 => happy_goto(p, action_8, j),
        107 => happy_goto(p, action_9, j),
        108 => happy_goto(p, action_10, j),
        109 => happy_goto(p, action_11, j),
        110 => happy_goto(p, action_12, j),
        111 => happy_goto(p, action_13, j),
        112 => happy_goto(p, action_14, j),
        113 => happy_goto(p, action_15, j),
        114 => happy_goto(p, action_16, j),
        115 => happy_goto(p, action_17, j),
        116 => happy_goto(p, action_18, j),
        117 => happy_goto(p, action_19, j),
        118 => happy_goto(p, action_20, j),
        119 => happy_goto(p, action_21, j),
        120 => happy_goto(p, action_22, j),
        122 => happy_goto(p, action_251, j),
        124 => happy_goto(p, action_252, j),
        127 => happy_goto(p, action_24, j),
        128 => happy_goto(p, action_25, j),
        _ => happy_reduce_458(p, j)
    }
}

fn action_73(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_250, j),
        _ => happy_fail(p, j)
    }
}

fn action_74(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_249, j),
        _ => happy_fail(p, j)
    }
}

fn action_75(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        167 => happy_reduce_471(p, j),
        _ => happy_reduce_361(p, j)
    }
}

fn action_76(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_472(p, j)
    }
}

fn action_77(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        249 => Ok(Cont::Accept(j)),
        _ => happy_fail(p, j)
    }
}

fn action_78(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_8(p, j)
    }
}

fn action_79(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        181 => happy_shift(p, action_62, j),
        14 => happy_goto(p, action_248, j),
        _ => happy_fail(p, j)
    }
}

fn action_80(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_9(p, j)
    }
}

fn action_81(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        179 => happy_shift(p, action_246, j),
        180 => happy_shift(p, action_247, j),
        _ => happy_fail(p, j)
    }
}

fn action_82(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        179 => happy_shift(p, action_244, j),
        180 => happy_shift(p, action_245, j),
        _ => happy_fail(p, j)
    }
}

fn action_83(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_229, j),
        150 => happy_shift(p, action_230, j),
        238 => happy_shift(p, action_142, j),
        239 => happy_shift(p, action_231, j),
        11 => happy_goto(p, action_241, j),
        66 => happy_goto(p, action_242, j),
        68 => happy_goto(p, action_221, j),
        69 => happy_goto(p, action_222, j),
        70 => happy_goto(p, action_223, j),
        71 => happy_goto(p, action_224, j),
        72 => happy_goto(p, action_225, j),
        73 => happy_goto(p, action_226, j),
        75 => happy_goto(p, action_227, j),
        76 => happy_goto(p, action_103, j),
        77 => happy_goto(p, action_104, j),
        78 => happy_goto(p, action_105, j),
        79 => happy_goto(p, action_243, j),
        80 => happy_goto(p, action_107, j),
        81 => happy_goto(p, action_108, j),
        _ => happy_fail(p, j)
    }
}

fn action_84(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_112, j),
        150 => happy_shift(p, action_113, j),
        185 => happy_shift(p, action_114, j),
        186 => happy_shift(p, action_173, j),
        188 => happy_shift(p, action_116, j),
        190 => happy_shift(p, action_117, j),
        192 => happy_shift(p, action_118, j),
        193 => happy_shift(p, action_174, j),
        195 => happy_shift(p, action_119, j),
        198 => happy_shift(p, action_120, j),
        200 => happy_shift(p, action_121, j),
        201 => happy_shift(p, action_122, j),
        202 => happy_shift(p, action_123, j),
        203 => happy_shift(p, action_124, j),
        208 => happy_shift(p, action_125, j),
        209 => happy_shift(p, action_126, j),
        210 => happy_shift(p, action_127, j),
        211 => happy_shift(p, action_128, j),
        213 => happy_shift(p, action_129, j),
        214 => happy_shift(p, action_175, j),
        215 => happy_shift(p, action_176, j),
        216 => happy_shift(p, action_130, j),
        217 => happy_shift(p, action_177, j),
        219 => happy_shift(p, action_131, j),
        220 => happy_shift(p, action_132, j),
        222 => happy_shift(p, action_133, j),
        224 => happy_shift(p, action_135, j),
        226 => happy_shift(p, action_136, j),
        227 => happy_shift(p, action_239, j),
        228 => happy_shift(p, action_138, j),
        229 => happy_shift(p, action_139, j),
        230 => happy_shift(p, action_140, j),
        231 => happy_shift(p, action_141, j),
        232 => happy_shift(p, action_178, j),
        238 => happy_shift(p, action_142, j),
        239 => happy_shift(p, action_240, j),
        240 => happy_shift(p, action_144, j),
        11 => happy_goto(p, action_232, j),
        39 => happy_goto(p, action_233, j),
        41 => happy_goto(p, action_200, j),
        42 => happy_goto(p, action_201, j),
        43 => happy_goto(p, action_202, j),
        45 => happy_goto(p, action_234, j),
        52 => happy_goto(p, action_235, j),
        53 => happy_goto(p, action_98, j),
        54 => happy_goto(p, action_99, j),
        61 => happy_goto(p, action_100, j),
        64 => happy_goto(p, action_203, j),
        75 => happy_goto(p, action_236, j),
        76 => happy_goto(p, action_103, j),
        77 => happy_goto(p, action_104, j),
        78 => happy_goto(p, action_105, j),
        79 => happy_goto(p, action_237, j),
        80 => happy_goto(p, action_107, j),
        81 => happy_goto(p, action_108, j),
        134 => happy_goto(p, action_238, j),
        _ => happy_fail(p, j)
    }
}

fn action_85(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_104(p, j)
    }
}

fn action_86(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_114(p, j)
    }
}

fn action_87(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_115(p, j)
    }
}

fn action_88(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_116(p, j)
    }
}

fn action_89(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_229, j),
        150 => happy_shift(p, action_230, j),
        238 => happy_shift(p, action_142, j),
        239 => happy_shift(p, action_231, j),
        11 => happy_goto(p, action_219, j),
        66 => happy_goto(p, action_220, j),
        68 => happy_goto(p, action_221, j),
        69 => happy_goto(p, action_222, j),
        70 => happy_goto(p, action_223, j),
        71 => happy_goto(p, action_224, j),
        72 => happy_goto(p, action_225, j),
        73 => happy_goto(p, action_226, j),
        75 => happy_goto(p, action_227, j),
        76 => happy_goto(p, action_103, j),
        77 => happy_goto(p, action_104, j),
        78 => happy_goto(p, action_105, j),
        79 => happy_goto(p, action_228, j),
        80 => happy_goto(p, action_107, j),
        81 => happy_goto(p, action_108, j),
        _ => happy_fail(p, j)
    }
}

fn action_90(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_148(p, j)
    }
}

fn action_91(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        185 => happy_shift(p, action_114, j),
        186 => happy_shift(p, action_173, j),
        188 => happy_shift(p, action_116, j),
        190 => happy_shift(p, action_117, j),
        192 => happy_shift(p, action_118, j),
        193 => happy_shift(p, action_174, j),
        195 => happy_shift(p, action_119, j),
        198 => happy_shift(p, action_120, j),
        201 => happy_shift(p, action_122, j),
        202 => happy_shift(p, action_123, j),
        203 => happy_shift(p, action_124, j),
        208 => happy_shift(p, action_125, j),
        209 => happy_shift(p, action_126, j),
        210 => happy_shift(p, action_127, j),
        211 => happy_shift(p, action_128, j),
        213 => happy_shift(p, action_129, j),
        214 => happy_shift(p, action_175, j),
        215 => happy_shift(p, action_176, j),
        216 => happy_shift(p, action_130, j),
        217 => happy_shift(p, action_177, j),
        219 => happy_shift(p, action_131, j),
        220 => happy_shift(p, action_132, j),
        222 => happy_shift(p, action_133, j),
        226 => happy_shift(p, action_136, j),
        228 => happy_shift(p, action_138, j),
        230 => happy_shift(p, action_140, j),
        231 => happy_shift(p, action_141, j),
        232 => happy_shift(p, action_178, j),
        240 => happy_shift(p, action_144, j),
        39 => happy_goto(p, action_216, j),
        41 => happy_goto(p, action_200, j),
        42 => happy_goto(p, action_201, j),
        43 => happy_goto(p, action_202, j),
        45 => happy_goto(p, action_217, j),
        64 => happy_goto(p, action_203, j),
        134 => happy_goto(p, action_218, j),
        _ => happy_reduce_101(p, j)
    }
}

fn action_92(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        186 => happy_shift(p, action_173, j),
        188 => happy_shift(p, action_116, j),
        190 => happy_shift(p, action_117, j),
        192 => happy_shift(p, action_118, j),
        193 => happy_shift(p, action_174, j),
        195 => happy_shift(p, action_119, j),
        198 => happy_shift(p, action_120, j),
        201 => happy_shift(p, action_122, j),
        202 => happy_shift(p, action_123, j),
        203 => happy_shift(p, action_124, j),
        209 => happy_shift(p, action_126, j),
        210 => happy_shift(p, action_127, j),
        211 => happy_shift(p, action_128, j),
        214 => happy_shift(p, action_175, j),
        215 => happy_shift(p, action_176, j),
        216 => happy_shift(p, action_130, j),
        217 => happy_shift(p, action_177, j),
        219 => happy_shift(p, action_131, j),
        220 => happy_shift(p, action_132, j),
        222 => happy_shift(p, action_133, j),
        226 => happy_shift(p, action_136, j),
        228 => happy_shift(p, action_138, j),
        230 => happy_shift(p, action_140, j),
        231 => happy_shift(p, action_141, j),
        232 => happy_shift(p, action_178, j),
        240 => happy_shift(p, action_144, j),
        41 => happy_goto(p, action_212, j),
        45 => happy_goto(p, action_213, j),
        64 => happy_goto(p, action_214, j),
        134 => happy_goto(p, action_215, j),
        _ => happy_reduce_127(p, j)
    }
}

fn action_93(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        180 => happy_shift(p, action_211, j),
        185 => happy_shift(p, action_114, j),
        186 => happy_shift(p, action_173, j),
        188 => happy_shift(p, action_116, j),
        193 => happy_shift(p, action_174, j),
        201 => happy_shift(p, action_122, j),
        208 => happy_shift(p, action_125, j),
        213 => happy_shift(p, action_129, j),
        214 => happy_shift(p, action_175, j),
        215 => happy_shift(p, action_176, j),
        216 => happy_shift(p, action_130, j),
        217 => happy_shift(p, action_177, j),
        222 => happy_shift(p, action_133, j),
        226 => happy_shift(p, action_136, j),
        228 => happy_shift(p, action_138, j),
        232 => happy_shift(p, action_178, j),
        240 => happy_shift(p, action_144, j),
        39 => happy_goto(p, action_209, j),
        41 => happy_goto(p, action_200, j),
        42 => happy_goto(p, action_201, j),
        43 => happy_goto(p, action_202, j),
        64 => happy_goto(p, action_203, j),
        134 => happy_goto(p, action_210, j),
        _ => happy_reduce_102(p, j)
    }
}

fn action_94(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        180 => happy_shift(p, action_208, j),
        186 => happy_shift(p, action_173, j),
        188 => happy_shift(p, action_116, j),
        193 => happy_shift(p, action_174, j),
        201 => happy_shift(p, action_122, j),
        214 => happy_shift(p, action_175, j),
        215 => happy_shift(p, action_176, j),
        216 => happy_shift(p, action_130, j),
        217 => happy_shift(p, action_177, j),
        222 => happy_shift(p, action_133, j),
        226 => happy_shift(p, action_136, j),
        228 => happy_shift(p, action_138, j),
        232 => happy_shift(p, action_178, j),
        240 => happy_shift(p, action_144, j),
        41 => happy_goto(p, action_205, j),
        64 => happy_goto(p, action_206, j),
        134 => happy_goto(p, action_207, j),
        _ => happy_reduce_128(p, j)
    }
}

fn action_95(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        185 => happy_shift(p, action_114, j),
        186 => happy_shift(p, action_173, j),
        188 => happy_shift(p, action_116, j),
        193 => happy_shift(p, action_174, j),
        201 => happy_shift(p, action_122, j),
        208 => happy_shift(p, action_125, j),
        213 => happy_shift(p, action_129, j),
        214 => happy_shift(p, action_175, j),
        215 => happy_shift(p, action_176, j),
        216 => happy_shift(p, action_130, j),
        217 => happy_shift(p, action_177, j),
        222 => happy_shift(p, action_133, j),
        226 => happy_shift(p, action_136, j),
        228 => happy_shift(p, action_138, j),
        232 => happy_shift(p, action_178, j),
        240 => happy_shift(p, action_144, j),
        39 => happy_goto(p, action_199, j),
        41 => happy_goto(p, action_200, j),
        42 => happy_goto(p, action_201, j),
        43 => happy_goto(p, action_202, j),
        64 => happy_goto(p, action_203, j),
        134 => happy_goto(p, action_204, j),
        _ => happy_reduce_103(p, j)
    }
}

fn action_96(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        186 => happy_shift(p, action_173, j),
        188 => happy_shift(p, action_116, j),
        193 => happy_shift(p, action_174, j),
        201 => happy_shift(p, action_122, j),
        214 => happy_shift(p, action_175, j),
        215 => happy_shift(p, action_176, j),
        216 => happy_shift(p, action_130, j),
        217 => happy_shift(p, action_177, j),
        222 => happy_shift(p, action_133, j),
        226 => happy_shift(p, action_136, j),
        228 => happy_shift(p, action_138, j),
        232 => happy_shift(p, action_178, j),
        240 => happy_shift(p, action_144, j),
        41 => happy_goto(p, action_196, j),
        64 => happy_goto(p, action_197, j),
        134 => happy_goto(p, action_198, j),
        _ => happy_reduce_129(p, j)
    }
}

fn action_97(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_159(p, j)
    }
}

fn action_98(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_185(p, j)
    }
}

fn action_99(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        240 => happy_shift(p, action_144, j),
        132 => happy_goto(p, action_195, j),
        133 => happy_goto(p, action_152, j),
        134 => happy_goto(p, action_111, j),
        _ => happy_reduce_473(p, j)
    }
}

fn action_100(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_186(p, j)
    }
}

fn action_101(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_112, j),
        150 => happy_shift(p, action_113, j),
        185 => happy_shift(p, action_114, j),
        186 => happy_shift(p, action_173, j),
        188 => happy_shift(p, action_116, j),
        190 => happy_shift(p, action_117, j),
        192 => happy_shift(p, action_118, j),
        193 => happy_shift(p, action_174, j),
        195 => happy_shift(p, action_119, j),
        198 => happy_shift(p, action_120, j),
        200 => happy_shift(p, action_121, j),
        201 => happy_shift(p, action_122, j),
        202 => happy_shift(p, action_123, j),
        203 => happy_shift(p, action_124, j),
        208 => happy_shift(p, action_125, j),
        209 => happy_shift(p, action_126, j),
        210 => happy_shift(p, action_127, j),
        211 => happy_shift(p, action_128, j),
        213 => happy_shift(p, action_129, j),
        214 => happy_shift(p, action_175, j),
        215 => happy_shift(p, action_176, j),
        216 => happy_shift(p, action_130, j),
        217 => happy_shift(p, action_177, j),
        219 => happy_shift(p, action_131, j),
        220 => happy_shift(p, action_132, j),
        222 => happy_shift(p, action_133, j),
        224 => happy_shift(p, action_135, j),
        226 => happy_shift(p, action_136, j),
        227 => happy_shift(p, action_193, j),
        228 => happy_shift(p, action_138, j),
        229 => happy_shift(p, action_139, j),
        230 => happy_shift(p, action_140, j),
        231 => happy_shift(p, action_141, j),
        232 => happy_shift(p, action_178, j),
        238 => happy_shift(p, action_142, j),
        239 => happy_shift(p, action_194, j),
        240 => happy_shift(p, action_144, j),
        11 => happy_goto(p, action_185, j),
        40 => happy_goto(p, action_186, j),
        41 => happy_goto(p, action_86, j),
        42 => happy_goto(p, action_87, j),
        43 => happy_goto(p, action_88, j),
        45 => happy_goto(p, action_187, j),
        52 => happy_goto(p, action_188, j),
        53 => happy_goto(p, action_98, j),
        54 => happy_goto(p, action_99, j),
        61 => happy_goto(p, action_100, j),
        64 => happy_goto(p, action_189, j),
        75 => happy_goto(p, action_190, j),
        76 => happy_goto(p, action_103, j),
        77 => happy_goto(p, action_104, j),
        78 => happy_goto(p, action_105, j),
        79 => happy_goto(p, action_191, j),
        80 => happy_goto(p, action_107, j),
        81 => happy_goto(p, action_108, j),
        133 => happy_goto(p, action_192, j),
        134 => happy_goto(p, action_111, j),
        _ => happy_fail(p, j)
    }
}

fn action_102(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_26(p, j)
    }
}

fn action_103(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_261(p, j)
    }
}

fn action_104(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_263(p, j)
    }
}

fn action_105(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_183, j),
        140 => happy_shift(p, action_184, j),
        88 => happy_goto(p, action_180, j),
        89 => happy_goto(p, action_181, j),
        90 => happy_goto(p, action_182, j),
        _ => happy_reduce_262(p, j)
    }
}

fn action_106(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        33 => happy_goto(p, action_179, j),
        _ => happy_reduce_90(p, j)
    }
}

fn action_107(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_276(p, j)
    }
}

fn action_108(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_277(p, j)
    }
}

fn action_109(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        186 => happy_shift(p, action_173, j),
        193 => happy_shift(p, action_174, j),
        214 => happy_shift(p, action_175, j),
        215 => happy_shift(p, action_176, j),
        217 => happy_shift(p, action_177, j),
        232 => happy_shift(p, action_178, j),
        64 => happy_goto(p, action_172, j),
        _ => happy_fail(p, j)
    }
}

fn action_110(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_112, j),
        150 => happy_shift(p, action_113, j),
        185 => happy_shift(p, action_114, j),
        188 => happy_shift(p, action_116, j),
        190 => happy_shift(p, action_117, j),
        192 => happy_shift(p, action_118, j),
        195 => happy_shift(p, action_119, j),
        198 => happy_shift(p, action_120, j),
        200 => happy_shift(p, action_121, j),
        201 => happy_shift(p, action_122, j),
        202 => happy_shift(p, action_123, j),
        203 => happy_shift(p, action_124, j),
        208 => happy_shift(p, action_125, j),
        209 => happy_shift(p, action_126, j),
        210 => happy_shift(p, action_127, j),
        211 => happy_shift(p, action_128, j),
        213 => happy_shift(p, action_129, j),
        216 => happy_shift(p, action_130, j),
        219 => happy_shift(p, action_131, j),
        220 => happy_shift(p, action_132, j),
        222 => happy_shift(p, action_133, j),
        224 => happy_shift(p, action_135, j),
        226 => happy_shift(p, action_136, j),
        227 => happy_shift(p, action_170, j),
        228 => happy_shift(p, action_138, j),
        229 => happy_shift(p, action_139, j),
        230 => happy_shift(p, action_140, j),
        231 => happy_shift(p, action_141, j),
        238 => happy_shift(p, action_142, j),
        239 => happy_shift(p, action_171, j),
        240 => happy_shift(p, action_144, j),
        11 => happy_goto(p, action_163, j),
        40 => happy_goto(p, action_164, j),
        41 => happy_goto(p, action_86, j),
        42 => happy_goto(p, action_87, j),
        43 => happy_goto(p, action_88, j),
        45 => happy_goto(p, action_165, j),
        52 => happy_goto(p, action_166, j),
        53 => happy_goto(p, action_98, j),
        54 => happy_goto(p, action_99, j),
        61 => happy_goto(p, action_100, j),
        75 => happy_goto(p, action_167, j),
        76 => happy_goto(p, action_103, j),
        77 => happy_goto(p, action_104, j),
        78 => happy_goto(p, action_105, j),
        79 => happy_goto(p, action_168, j),
        80 => happy_goto(p, action_107, j),
        81 => happy_goto(p, action_108, j),
        134 => happy_goto(p, action_169, j),
        _ => happy_reduce_474(p, j)
    }
}

fn action_111(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_475(p, j)
    }
}

fn action_112(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_112, j),
        150 => happy_shift(p, action_113, j),
        238 => happy_shift(p, action_142, j),
        240 => happy_shift(p, action_144, j),
        76 => happy_goto(p, action_159, j),
        77 => happy_goto(p, action_104, j),
        78 => happy_goto(p, action_160, j),
        80 => happy_goto(p, action_161, j),
        81 => happy_goto(p, action_108, j),
        133 => happy_goto(p, action_162, j),
        134 => happy_goto(p, action_111, j),
        _ => happy_fail(p, j)
    }
}

fn action_113(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_112, j),
        150 => happy_shift(p, action_113, j),
        238 => happy_shift(p, action_142, j),
        240 => happy_shift(p, action_144, j),
        65 => happy_goto(p, action_155, j),
        75 => happy_goto(p, action_156, j),
        76 => happy_goto(p, action_103, j),
        77 => happy_goto(p, action_104, j),
        78 => happy_goto(p, action_105, j),
        80 => happy_goto(p, action_157, j),
        81 => happy_goto(p, action_108, j),
        132 => happy_goto(p, action_109, j),
        133 => happy_goto(p, action_158, j),
        134 => happy_goto(p, action_111, j),
        _ => happy_reduce_473(p, j)
    }
}

fn action_114(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_154, j),
        _ => happy_fail(p, j)
    }
}

fn action_115(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_153, j),
        _ => happy_fail(p, j)
    }
}

fn action_116(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_120(p, j)
    }
}

fn action_117(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_139(p, j)
    }
}

fn action_118(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_131(p, j)
    }
}

fn action_119(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_140(p, j)
    }
}

fn action_120(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_136(p, j)
    }
}

fn action_121(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        240 => happy_shift(p, action_144, j),
        132 => happy_goto(p, action_151, j),
        133 => happy_goto(p, action_152, j),
        134 => happy_goto(p, action_111, j),
        _ => happy_reduce_473(p, j)
    }
}

fn action_122(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_118(p, j)
    }
}

fn action_123(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_135(p, j)
    }
}

fn action_124(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_142(p, j)
    }
}

fn action_125(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_123(p, j)
    }
}

fn action_126(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_133(p, j)
    }
}

fn action_127(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_141(p, j)
    }
}

fn action_128(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_134(p, j)
    }
}

fn action_129(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_124(p, j)
    }
}

fn action_130(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_121(p, j)
    }
}

fn action_131(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_132(p, j)
    }
}

fn action_132(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_137(p, j)
    }
}

fn action_133(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_119(p, j)
    }
}

fn action_134(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_150, j),
        _ => happy_fail(p, j)
    }
}

fn action_135(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_190(p, j)
    }
}

fn action_136(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_117(p, j)
    }
}

fn action_137(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_149, j),
        _ => happy_fail(p, j)
    }
}

fn action_138(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_122(p, j)
    }
}

fn action_139(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_191(p, j)
    }
}

fn action_140(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_138(p, j)
    }
}

fn action_141(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_130(p, j)
    }
}

fn action_142(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_273(p, j)
    }
}

fn action_143(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_171(p, j)
    }
}

fn action_144(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_148, j),
        _ => happy_fail(p, j)
    }
}

fn action_145(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_112, j),
        150 => happy_shift(p, action_113, j),
        185 => happy_shift(p, action_114, j),
        187 => happy_shift(p, action_115, j),
        188 => happy_shift(p, action_116, j),
        190 => happy_shift(p, action_117, j),
        192 => happy_shift(p, action_118, j),
        195 => happy_shift(p, action_119, j),
        198 => happy_shift(p, action_120, j),
        200 => happy_shift(p, action_121, j),
        201 => happy_shift(p, action_122, j),
        202 => happy_shift(p, action_123, j),
        203 => happy_shift(p, action_124, j),
        208 => happy_shift(p, action_125, j),
        209 => happy_shift(p, action_126, j),
        210 => happy_shift(p, action_127, j),
        211 => happy_shift(p, action_128, j),
        213 => happy_shift(p, action_129, j),
        216 => happy_shift(p, action_130, j),
        219 => happy_shift(p, action_131, j),
        220 => happy_shift(p, action_132, j),
        222 => happy_shift(p, action_133, j),
        223 => happy_shift(p, action_134, j),
        224 => happy_shift(p, action_135, j),
        226 => happy_shift(p, action_136, j),
        227 => happy_shift(p, action_137, j),
        228 => happy_shift(p, action_138, j),
        229 => happy_shift(p, action_139, j),
        230 => happy_shift(p, action_140, j),
        231 => happy_shift(p, action_141, j),
        238 => happy_shift(p, action_142, j),
        239 => happy_shift(p, action_143, j),
        240 => happy_shift(p, action_144, j),
        241 => happy_shift(p, action_145, j),
        9 => happy_goto(p, action_147, j),
        10 => happy_goto(p, action_78, j),
        11 => happy_goto(p, action_79, j),
        32 => happy_goto(p, action_80, j),
        34 => happy_goto(p, action_81, j),
        36 => happy_goto(p, action_82, j),
        37 => happy_goto(p, action_83, j),
        38 => happy_goto(p, action_84, j),
        40 => happy_goto(p, action_85, j),
        41 => happy_goto(p, action_86, j),
        42 => happy_goto(p, action_87, j),
        43 => happy_goto(p, action_88, j),
        44 => happy_goto(p, action_89, j),
        45 => happy_goto(p, action_90, j),
        46 => happy_goto(p, action_91, j),
        47 => happy_goto(p, action_92, j),
        48 => happy_goto(p, action_93, j),
        49 => happy_goto(p, action_94, j),
        50 => happy_goto(p, action_95, j),
        51 => happy_goto(p, action_96, j),
        52 => happy_goto(p, action_97, j),
        53 => happy_goto(p, action_98, j),
        54 => happy_goto(p, action_99, j),
        61 => happy_goto(p, action_100, j),
        65 => happy_goto(p, action_101, j),
        75 => happy_goto(p, action_102, j),
        76 => happy_goto(p, action_103, j),
        77 => happy_goto(p, action_104, j),
        78 => happy_goto(p, action_105, j),
        79 => happy_goto(p, action_106, j),
        80 => happy_goto(p, action_107, j),
        81 => happy_goto(p, action_108, j),
        132 => happy_goto(p, action_109, j),
        133 => happy_goto(p, action_110, j),
        134 => happy_goto(p, action_111, j),
        _ => happy_reduce_473(p, j)
    }
}

fn action_146(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        249 => Ok(Cont::Accept(j)),
        _ => happy_fail(p, j)
    }
}

fn action_147(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_10(p, j)
    }
}

fn action_148(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_497, j),
        _ => happy_fail(p, j)
    }
}

fn action_149(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_26, j),
        144 => happy_shift(p, action_27, j),
        145 => happy_shift(p, action_28, j),
        146 => happy_shift(p, action_29, j),
        147 => happy_shift(p, action_30, j),
        148 => happy_shift(p, action_31, j),
        149 => happy_shift(p, action_32, j),
        150 => happy_shift(p, action_33, j),
        153 => happy_shift(p, action_34, j),
        164 => happy_shift(p, action_35, j),
        184 => happy_shift(p, action_36, j),
        190 => happy_shift(p, action_117, j),
        192 => happy_shift(p, action_118, j),
        195 => happy_shift(p, action_119, j),
        198 => happy_shift(p, action_120, j),
        200 => happy_shift(p, action_121, j),
        202 => happy_shift(p, action_123, j),
        203 => happy_shift(p, action_124, j),
        205 => happy_shift(p, action_37, j),
        209 => happy_shift(p, action_126, j),
        210 => happy_shift(p, action_127, j),
        211 => happy_shift(p, action_128, j),
        219 => happy_shift(p, action_131, j),
        220 => happy_shift(p, action_132, j),
        221 => happy_shift(p, action_38, j),
        224 => happy_shift(p, action_135, j),
        227 => happy_shift(p, action_137, j),
        229 => happy_shift(p, action_139, j),
        230 => happy_shift(p, action_140, j),
        231 => happy_shift(p, action_141, j),
        234 => happy_shift(p, action_39, j),
        235 => happy_shift(p, action_40, j),
        236 => happy_shift(p, action_41, j),
        237 => happy_shift(p, action_42, j),
        238 => happy_shift(p, action_43, j),
        239 => happy_shift(p, action_143, j),
        240 => happy_shift(p, action_144, j),
        241 => happy_shift(p, action_44, j),
        242 => happy_shift(p, action_45, j),
        243 => happy_shift(p, action_46, j),
        244 => happy_shift(p, action_47, j),
        245 => happy_shift(p, action_48, j),
        246 => happy_shift(p, action_49, j),
        247 => happy_shift(p, action_50, j),
        44 => happy_goto(p, action_289, j),
        45 => happy_goto(p, action_90, j),
        47 => happy_goto(p, action_290, j),
        49 => happy_goto(p, action_291, j),
        51 => happy_goto(p, action_292, j),
        52 => happy_goto(p, action_97, j),
        53 => happy_goto(p, action_98, j),
        54 => happy_goto(p, action_99, j),
        61 => happy_goto(p, action_100, j),
        65 => happy_goto(p, action_293, j),
        86 => happy_goto(p, action_495, j),
        100 => happy_goto(p, action_6, j),
        104 => happy_goto(p, action_7, j),
        106 => happy_goto(p, action_8, j),
        107 => happy_goto(p, action_9, j),
        108 => happy_goto(p, action_10, j),
        109 => happy_goto(p, action_11, j),
        110 => happy_goto(p, action_12, j),
        111 => happy_goto(p, action_13, j),
        112 => happy_goto(p, action_14, j),
        113 => happy_goto(p, action_15, j),
        114 => happy_goto(p, action_16, j),
        115 => happy_goto(p, action_17, j),
        116 => happy_goto(p, action_18, j),
        117 => happy_goto(p, action_19, j),
        118 => happy_goto(p, action_20, j),
        119 => happy_goto(p, action_21, j),
        120 => happy_goto(p, action_22, j),
        122 => happy_goto(p, action_496, j),
        127 => happy_goto(p, action_24, j),
        128 => happy_goto(p, action_25, j),
        132 => happy_goto(p, action_109, j),
        133 => happy_goto(p, action_296, j),
        134 => happy_goto(p, action_111, j),
        _ => happy_reduce_473(p, j)
    }
}

fn action_150(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_26, j),
        144 => happy_shift(p, action_27, j),
        145 => happy_shift(p, action_28, j),
        146 => happy_shift(p, action_29, j),
        147 => happy_shift(p, action_30, j),
        148 => happy_shift(p, action_31, j),
        149 => happy_shift(p, action_32, j),
        150 => happy_shift(p, action_33, j),
        153 => happy_shift(p, action_34, j),
        164 => happy_shift(p, action_35, j),
        184 => happy_shift(p, action_36, j),
        205 => happy_shift(p, action_37, j),
        221 => happy_shift(p, action_38, j),
        234 => happy_shift(p, action_39, j),
        235 => happy_shift(p, action_40, j),
        236 => happy_shift(p, action_41, j),
        237 => happy_shift(p, action_42, j),
        238 => happy_shift(p, action_43, j),
        241 => happy_shift(p, action_44, j),
        242 => happy_shift(p, action_45, j),
        243 => happy_shift(p, action_46, j),
        244 => happy_shift(p, action_47, j),
        245 => happy_shift(p, action_48, j),
        246 => happy_shift(p, action_49, j),
        247 => happy_shift(p, action_50, j),
        100 => happy_goto(p, action_6, j),
        104 => happy_goto(p, action_7, j),
        106 => happy_goto(p, action_261, j),
        107 => happy_goto(p, action_9, j),
        108 => happy_goto(p, action_10, j),
        109 => happy_goto(p, action_11, j),
        110 => happy_goto(p, action_12, j),
        111 => happy_goto(p, action_13, j),
        112 => happy_goto(p, action_14, j),
        113 => happy_goto(p, action_15, j),
        114 => happy_goto(p, action_16, j),
        115 => happy_goto(p, action_17, j),
        116 => happy_goto(p, action_18, j),
        117 => happy_goto(p, action_19, j),
        118 => happy_goto(p, action_20, j),
        119 => happy_goto(p, action_262, j),
        126 => happy_goto(p, action_494, j),
        127 => happy_goto(p, action_24, j),
        128 => happy_goto(p, action_25, j),
        _ => happy_fail(p, j)
    }
}

fn action_151(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        181 => happy_shift(p, action_493, j),
        238 => happy_shift(p, action_256, j),
        239 => happy_shift(p, action_76, j),
        131 => happy_goto(p, action_492, j),
        _ => happy_fail(p, j)
    }
}

fn action_152(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        240 => happy_shift(p, action_144, j),
        134 => happy_goto(p, action_169, j),
        _ => happy_reduce_474(p, j)
    }
}

fn action_153(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        237 => happy_shift(p, action_42, j),
        128 => happy_goto(p, action_491, j),
        _ => happy_fail(p, j)
    }
}

fn action_154(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_26, j),
        144 => happy_shift(p, action_27, j),
        145 => happy_shift(p, action_28, j),
        146 => happy_shift(p, action_29, j),
        147 => happy_shift(p, action_30, j),
        148 => happy_shift(p, action_31, j),
        149 => happy_shift(p, action_32, j),
        150 => happy_shift(p, action_33, j),
        153 => happy_shift(p, action_34, j),
        164 => happy_shift(p, action_35, j),
        184 => happy_shift(p, action_36, j),
        190 => happy_shift(p, action_117, j),
        192 => happy_shift(p, action_118, j),
        195 => happy_shift(p, action_119, j),
        198 => happy_shift(p, action_120, j),
        200 => happy_shift(p, action_121, j),
        202 => happy_shift(p, action_123, j),
        203 => happy_shift(p, action_124, j),
        205 => happy_shift(p, action_37, j),
        209 => happy_shift(p, action_126, j),
        210 => happy_shift(p, action_127, j),
        211 => happy_shift(p, action_128, j),
        219 => happy_shift(p, action_131, j),
        220 => happy_shift(p, action_132, j),
        221 => happy_shift(p, action_38, j),
        224 => happy_shift(p, action_135, j),
        227 => happy_shift(p, action_137, j),
        229 => happy_shift(p, action_139, j),
        230 => happy_shift(p, action_140, j),
        231 => happy_shift(p, action_141, j),
        234 => happy_shift(p, action_39, j),
        235 => happy_shift(p, action_40, j),
        236 => happy_shift(p, action_41, j),
        237 => happy_shift(p, action_42, j),
        238 => happy_shift(p, action_43, j),
        239 => happy_shift(p, action_143, j),
        240 => happy_shift(p, action_144, j),
        241 => happy_shift(p, action_44, j),
        242 => happy_shift(p, action_45, j),
        243 => happy_shift(p, action_46, j),
        244 => happy_shift(p, action_47, j),
        245 => happy_shift(p, action_48, j),
        246 => happy_shift(p, action_49, j),
        247 => happy_shift(p, action_50, j),
        44 => happy_goto(p, action_289, j),
        45 => happy_goto(p, action_90, j),
        47 => happy_goto(p, action_290, j),
        49 => happy_goto(p, action_291, j),
        51 => happy_goto(p, action_292, j),
        52 => happy_goto(p, action_97, j),
        53 => happy_goto(p, action_98, j),
        54 => happy_goto(p, action_99, j),
        61 => happy_goto(p, action_100, j),
        65 => happy_goto(p, action_293, j),
        86 => happy_goto(p, action_489, j),
        100 => happy_goto(p, action_6, j),
        104 => happy_goto(p, action_7, j),
        106 => happy_goto(p, action_261, j),
        107 => happy_goto(p, action_9, j),
        108 => happy_goto(p, action_10, j),
        109 => happy_goto(p, action_11, j),
        110 => happy_goto(p, action_12, j),
        111 => happy_goto(p, action_13, j),
        112 => happy_goto(p, action_14, j),
        113 => happy_goto(p, action_15, j),
        114 => happy_goto(p, action_16, j),
        115 => happy_goto(p, action_17, j),
        116 => happy_goto(p, action_18, j),
        117 => happy_goto(p, action_19, j),
        118 => happy_goto(p, action_20, j),
        119 => happy_goto(p, action_262, j),
        126 => happy_goto(p, action_490, j),
        127 => happy_goto(p, action_24, j),
        128 => happy_goto(p, action_25, j),
        132 => happy_goto(p, action_109, j),
        133 => happy_goto(p, action_296, j),
        134 => happy_goto(p, action_111, j),
        _ => happy_reduce_473(p, j)
    }
}

fn action_155(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_112, j),
        150 => happy_shift(p, action_113, j),
        186 => happy_shift(p, action_173, j),
        193 => happy_shift(p, action_174, j),
        214 => happy_shift(p, action_175, j),
        215 => happy_shift(p, action_176, j),
        217 => happy_shift(p, action_177, j),
        232 => happy_shift(p, action_178, j),
        238 => happy_shift(p, action_142, j),
        240 => happy_shift(p, action_144, j),
        64 => happy_goto(p, action_189, j),
        75 => happy_goto(p, action_486, j),
        76 => happy_goto(p, action_103, j),
        77 => happy_goto(p, action_104, j),
        78 => happy_goto(p, action_105, j),
        80 => happy_goto(p, action_487, j),
        81 => happy_goto(p, action_108, j),
        133 => happy_goto(p, action_488, j),
        134 => happy_goto(p, action_111, j),
        _ => happy_fail(p, j)
    }
}

fn action_156(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_264(p, j)
    }
}

fn action_157(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_278(p, j)
    }
}

fn action_158(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_479, j),
        150 => happy_shift(p, action_480, j),
        238 => happy_shift(p, action_142, j),
        240 => happy_shift(p, action_144, j),
        75 => happy_goto(p, action_484, j),
        76 => happy_goto(p, action_103, j),
        77 => happy_goto(p, action_104, j),
        78 => happy_goto(p, action_485, j),
        134 => happy_goto(p, action_169, j),
        _ => happy_reduce_474(p, j)
    }
}

fn action_159(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        139 => happy_shift(p, action_483, j),
        _ => happy_fail(p, j)
    }
}

fn action_160(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_183, j),
        139 => happy_shift(p, action_482, j),
        140 => happy_shift(p, action_184, j),
        88 => happy_goto(p, action_180, j),
        89 => happy_goto(p, action_181, j),
        90 => happy_goto(p, action_182, j),
        _ => happy_fail(p, j)
    }
}

fn action_161(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        139 => happy_shift(p, action_481, j),
        _ => happy_fail(p, j)
    }
}

fn action_162(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_479, j),
        150 => happy_shift(p, action_480, j),
        238 => happy_shift(p, action_142, j),
        240 => happy_shift(p, action_144, j),
        76 => happy_goto(p, action_477, j),
        77 => happy_goto(p, action_104, j),
        78 => happy_goto(p, action_478, j),
        134 => happy_goto(p, action_169, j),
        _ => happy_fail(p, j)
    }
}

fn action_163(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        181 => happy_shift(p, action_62, j),
        14 => happy_goto(p, action_476, j),
        _ => happy_fail(p, j)
    }
}

fn action_164(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_105(p, j)
    }
}

fn action_165(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_149(p, j)
    }
}

fn action_166(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_160(p, j)
    }
}

fn action_167(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        181 => happy_reduce_26(p, j),
        187 => happy_shift(p, action_410, j),
        35 => happy_goto(p, action_475, j),
        67 => happy_goto(p, action_409, j),
        _ => happy_reduce_233(p, j)
    }
}

fn action_168(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        33 => happy_goto(p, action_474, j),
        _ => happy_reduce_90(p, j)
    }
}

fn action_169(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_476(p, j)
    }
}

fn action_170(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_473, j),
        _ => happy_fail(p, j)
    }
}

fn action_171(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_177(p, j)
    }
}

fn action_172(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_228(p, j)
    }
}

fn action_173(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_227(p, j)
    }
}

fn action_174(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_222(p, j)
    }
}

fn action_175(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_225(p, j)
    }
}

fn action_176(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_226(p, j)
    }
}

fn action_177(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_224(p, j)
    }
}

fn action_178(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_223(p, j)
    }
}

fn action_179(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        181 => happy_shift(p, action_62, j),
        185 => happy_shift(p, action_114, j),
        188 => happy_shift(p, action_116, j),
        190 => happy_shift(p, action_117, j),
        192 => happy_shift(p, action_118, j),
        195 => happy_shift(p, action_119, j),
        198 => happy_shift(p, action_120, j),
        200 => happy_shift(p, action_121, j),
        201 => happy_shift(p, action_122, j),
        202 => happy_shift(p, action_123, j),
        203 => happy_shift(p, action_124, j),
        208 => happy_shift(p, action_125, j),
        209 => happy_shift(p, action_126, j),
        210 => happy_shift(p, action_127, j),
        211 => happy_shift(p, action_128, j),
        213 => happy_shift(p, action_129, j),
        216 => happy_shift(p, action_130, j),
        219 => happy_shift(p, action_131, j),
        220 => happy_shift(p, action_132, j),
        222 => happy_shift(p, action_133, j),
        223 => happy_shift(p, action_134, j),
        224 => happy_shift(p, action_135, j),
        226 => happy_shift(p, action_136, j),
        227 => happy_shift(p, action_137, j),
        228 => happy_shift(p, action_138, j),
        229 => happy_shift(p, action_139, j),
        230 => happy_shift(p, action_140, j),
        231 => happy_shift(p, action_141, j),
        239 => happy_shift(p, action_143, j),
        240 => happy_shift(p, action_144, j),
        14 => happy_goto(p, action_466, j),
        32 => happy_goto(p, action_467, j),
        34 => happy_goto(p, action_81, j),
        36 => happy_goto(p, action_82, j),
        37 => happy_goto(p, action_468, j),
        38 => happy_goto(p, action_469, j),
        40 => happy_goto(p, action_85, j),
        41 => happy_goto(p, action_86, j),
        42 => happy_goto(p, action_87, j),
        43 => happy_goto(p, action_88, j),
        44 => happy_goto(p, action_470, j),
        45 => happy_goto(p, action_90, j),
        46 => happy_goto(p, action_91, j),
        47 => happy_goto(p, action_92, j),
        48 => happy_goto(p, action_93, j),
        49 => happy_goto(p, action_94, j),
        50 => happy_goto(p, action_95, j),
        51 => happy_goto(p, action_96, j),
        52 => happy_goto(p, action_97, j),
        53 => happy_goto(p, action_98, j),
        54 => happy_goto(p, action_99, j),
        61 => happy_goto(p, action_100, j),
        65 => happy_goto(p, action_471, j),
        132 => happy_goto(p, action_109, j),
        133 => happy_goto(p, action_472, j),
        134 => happy_goto(p, action_111, j),
        _ => happy_reduce_473(p, j)
    }
}

fn action_180(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_268(p, j)
    }
}

fn action_181(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        140 => happy_shift(p, action_184, j),
        90 => happy_goto(p, action_465, j),
        _ => happy_reduce_312(p, j)
    }
}

fn action_182(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_314(p, j)
    }
}

fn action_183(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        185 => happy_shift(p, action_114, j),
        186 => happy_reduce_473(p, j),
        188 => happy_shift(p, action_116, j),
        190 => happy_shift(p, action_117, j),
        192 => happy_shift(p, action_118, j),
        193 => happy_reduce_473(p, j),
        195 => happy_shift(p, action_119, j),
        198 => happy_shift(p, action_120, j),
        200 => happy_shift(p, action_121, j),
        201 => happy_shift(p, action_122, j),
        202 => happy_shift(p, action_123, j),
        203 => happy_shift(p, action_124, j),
        208 => happy_shift(p, action_125, j),
        209 => happy_shift(p, action_126, j),
        210 => happy_shift(p, action_127, j),
        211 => happy_shift(p, action_128, j),
        213 => happy_shift(p, action_129, j),
        214 => happy_reduce_473(p, j),
        215 => happy_reduce_473(p, j),
        216 => happy_shift(p, action_130, j),
        217 => happy_reduce_473(p, j),
        219 => happy_shift(p, action_131, j),
        220 => happy_shift(p, action_132, j),
        222 => happy_shift(p, action_133, j),
        224 => happy_shift(p, action_135, j),
        226 => happy_shift(p, action_136, j),
        227 => happy_shift(p, action_137, j),
        228 => happy_shift(p, action_138, j),
        229 => happy_shift(p, action_139, j),
        230 => happy_shift(p, action_140, j),
        231 => happy_shift(p, action_141, j),
        232 => happy_reduce_473(p, j),
        238 => happy_shift(p, action_464, j),
        239 => happy_shift(p, action_143, j),
        240 => happy_shift(p, action_144, j),
        37 => happy_goto(p, action_453, j),
        38 => happy_goto(p, action_454, j),
        40 => happy_goto(p, action_85, j),
        41 => happy_goto(p, action_86, j),
        42 => happy_goto(p, action_87, j),
        43 => happy_goto(p, action_88, j),
        44 => happy_goto(p, action_455, j),
        45 => happy_goto(p, action_90, j),
        46 => happy_goto(p, action_91, j),
        47 => happy_goto(p, action_92, j),
        48 => happy_goto(p, action_456, j),
        49 => happy_goto(p, action_457, j),
        50 => happy_goto(p, action_95, j),
        51 => happy_goto(p, action_96, j),
        52 => happy_goto(p, action_97, j),
        53 => happy_goto(p, action_98, j),
        54 => happy_goto(p, action_99, j),
        61 => happy_goto(p, action_100, j),
        65 => happy_goto(p, action_458, j),
        82 => happy_goto(p, action_459, j),
        83 => happy_goto(p, action_460, j),
        84 => happy_goto(p, action_461, j),
        85 => happy_goto(p, action_462, j),
        132 => happy_goto(p, action_109, j),
        133 => happy_goto(p, action_463, j),
        134 => happy_goto(p, action_111, j),
        _ => happy_reduce_283(p, j)
    }
}

fn action_184(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_26, j),
        144 => happy_shift(p, action_27, j),
        145 => happy_shift(p, action_28, j),
        146 => happy_shift(p, action_29, j),
        147 => happy_shift(p, action_30, j),
        148 => happy_shift(p, action_31, j),
        149 => happy_shift(p, action_32, j),
        150 => happy_shift(p, action_451, j),
        153 => happy_shift(p, action_34, j),
        164 => happy_shift(p, action_35, j),
        184 => happy_shift(p, action_36, j),
        186 => happy_reduce_473(p, j),
        193 => happy_reduce_473(p, j),
        205 => happy_shift(p, action_37, j),
        214 => happy_reduce_473(p, j),
        215 => happy_reduce_473(p, j),
        217 => happy_reduce_473(p, j),
        221 => happy_shift(p, action_38, j),
        222 => happy_shift(p, action_452, j),
        232 => happy_reduce_473(p, j),
        234 => happy_shift(p, action_39, j),
        235 => happy_shift(p, action_40, j),
        236 => happy_shift(p, action_41, j),
        237 => happy_shift(p, action_42, j),
        238 => happy_shift(p, action_43, j),
        240 => happy_shift(p, action_144, j),
        241 => happy_shift(p, action_44, j),
        242 => happy_shift(p, action_45, j),
        243 => happy_shift(p, action_46, j),
        244 => happy_shift(p, action_47, j),
        245 => happy_shift(p, action_48, j),
        246 => happy_shift(p, action_49, j),
        247 => happy_shift(p, action_50, j),
        65 => happy_goto(p, action_447, j),
        100 => happy_goto(p, action_6, j),
        104 => happy_goto(p, action_7, j),
        106 => happy_goto(p, action_8, j),
        107 => happy_goto(p, action_9, j),
        108 => happy_goto(p, action_10, j),
        109 => happy_goto(p, action_11, j),
        110 => happy_goto(p, action_12, j),
        111 => happy_goto(p, action_13, j),
        112 => happy_goto(p, action_14, j),
        113 => happy_goto(p, action_15, j),
        114 => happy_goto(p, action_16, j),
        115 => happy_goto(p, action_17, j),
        116 => happy_goto(p, action_18, j),
        117 => happy_goto(p, action_19, j),
        118 => happy_goto(p, action_20, j),
        119 => happy_goto(p, action_21, j),
        120 => happy_goto(p, action_448, j),
        125 => happy_goto(p, action_449, j),
        127 => happy_goto(p, action_24, j),
        128 => happy_goto(p, action_25, j),
        132 => happy_goto(p, action_109, j),
        133 => happy_goto(p, action_450, j),
        134 => happy_goto(p, action_111, j),
        _ => happy_reduce_460(p, j)
    }
}

fn action_185(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        181 => happy_shift(p, action_62, j),
        14 => happy_goto(p, action_446, j),
        _ => happy_fail(p, j)
    }
}

fn action_186(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_106(p, j)
    }
}

fn action_187(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_150(p, j)
    }
}

fn action_188(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_161(p, j)
    }
}

fn action_189(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_229(p, j)
    }
}

fn action_190(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        181 => happy_reduce_26(p, j),
        187 => happy_shift(p, action_410, j),
        35 => happy_goto(p, action_445, j),
        67 => happy_goto(p, action_409, j),
        _ => happy_reduce_233(p, j)
    }
}

fn action_191(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        33 => happy_goto(p, action_444, j),
        _ => happy_reduce_90(p, j)
    }
}

fn action_192(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_112, j),
        150 => happy_shift(p, action_113, j),
        185 => happy_shift(p, action_114, j),
        186 => happy_shift(p, action_173, j),
        188 => happy_shift(p, action_116, j),
        190 => happy_shift(p, action_117, j),
        192 => happy_shift(p, action_118, j),
        193 => happy_shift(p, action_174, j),
        195 => happy_shift(p, action_119, j),
        198 => happy_shift(p, action_120, j),
        200 => happy_shift(p, action_121, j),
        201 => happy_shift(p, action_122, j),
        202 => happy_shift(p, action_123, j),
        203 => happy_shift(p, action_124, j),
        208 => happy_shift(p, action_125, j),
        209 => happy_shift(p, action_126, j),
        210 => happy_shift(p, action_127, j),
        211 => happy_shift(p, action_128, j),
        213 => happy_shift(p, action_129, j),
        214 => happy_shift(p, action_175, j),
        215 => happy_shift(p, action_176, j),
        216 => happy_shift(p, action_130, j),
        217 => happy_shift(p, action_177, j),
        219 => happy_shift(p, action_131, j),
        220 => happy_shift(p, action_132, j),
        222 => happy_shift(p, action_133, j),
        224 => happy_shift(p, action_135, j),
        226 => happy_shift(p, action_136, j),
        227 => happy_shift(p, action_442, j),
        228 => happy_shift(p, action_138, j),
        229 => happy_shift(p, action_139, j),
        230 => happy_shift(p, action_140, j),
        231 => happy_shift(p, action_141, j),
        232 => happy_shift(p, action_178, j),
        238 => happy_shift(p, action_142, j),
        239 => happy_shift(p, action_443, j),
        240 => happy_shift(p, action_144, j),
        11 => happy_goto(p, action_435, j),
        40 => happy_goto(p, action_436, j),
        41 => happy_goto(p, action_86, j),
        42 => happy_goto(p, action_87, j),
        43 => happy_goto(p, action_88, j),
        45 => happy_goto(p, action_437, j),
        52 => happy_goto(p, action_438, j),
        53 => happy_goto(p, action_98, j),
        54 => happy_goto(p, action_99, j),
        61 => happy_goto(p, action_100, j),
        64 => happy_goto(p, action_439, j),
        75 => happy_goto(p, action_440, j),
        76 => happy_goto(p, action_103, j),
        77 => happy_goto(p, action_104, j),
        78 => happy_goto(p, action_105, j),
        79 => happy_goto(p, action_441, j),
        80 => happy_goto(p, action_107, j),
        81 => happy_goto(p, action_108, j),
        134 => happy_goto(p, action_169, j),
        _ => happy_fail(p, j)
    }
}

fn action_193(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_434, j),
        _ => happy_fail(p, j)
    }
}

fn action_194(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_174(p, j)
    }
}

fn action_195(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        181 => happy_shift(p, action_433, j),
        238 => happy_shift(p, action_256, j),
        239 => happy_shift(p, action_76, j),
        131 => happy_goto(p, action_432, j),
        _ => happy_fail(p, j)
    }
}

fn action_196(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_165(p, j)
    }
}

fn action_197(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_183(p, j)
    }
}

fn action_198(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_184(p, j)
    }
}

fn action_199(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_169(p, j)
    }
}

fn action_200(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_110(p, j)
    }
}

fn action_201(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_112(p, j)
    }
}

fn action_202(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_113(p, j)
    }
}

fn action_203(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_111(p, j)
    }
}

fn action_204(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_170(p, j)
    }
}

fn action_205(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_156(p, j)
    }
}

fn action_206(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_163(p, j)
    }
}

fn action_207(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_164(p, j)
    }
}

fn action_208(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_86(p, j)
    }
}

fn action_209(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_157(p, j)
    }
}

fn action_210(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_158(p, j)
    }
}

fn action_211(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_85(p, j)
    }
}

fn action_212(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_144(p, j)
    }
}

fn action_213(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_153(p, j)
    }
}

fn action_214(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_152(p, j)
    }
}

fn action_215(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_154(p, j)
    }
}

fn action_216(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_145(p, j)
    }
}

fn action_217(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_146(p, j)
    }
}

fn action_218(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_147(p, j)
    }
}

fn action_219(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        181 => happy_shift(p, action_62, j),
        14 => happy_goto(p, action_431, j),
        _ => happy_fail(p, j)
    }
}

fn action_220(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        187 => happy_shift(p, action_410, j),
        35 => happy_goto(p, action_430, j),
        67 => happy_goto(p, action_409, j),
        _ => happy_reduce_233(p, j)
    }
}

fn action_221(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_232(p, j)
    }
}

fn action_222(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_236(p, j)
    }
}

fn action_223(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_239(p, j)
    }
}

fn action_224(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_240(p, j)
    }
}

fn action_225(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_235(p, j)
    }
}

fn action_226(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_249(p, j)
    }
}

fn action_227(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        181 => happy_reduce_26(p, j),
        _ => happy_reduce_231(p, j)
    }
}

fn action_228(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        33 => happy_goto(p, action_429, j),
        _ => happy_reduce_90(p, j)
    }
}

fn action_229(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_427, j),
        150 => happy_shift(p, action_230, j),
        238 => happy_shift(p, action_142, j),
        239 => happy_shift(p, action_428, j),
        240 => happy_shift(p, action_144, j),
        70 => happy_goto(p, action_423, j),
        71 => happy_goto(p, action_224, j),
        72 => happy_goto(p, action_424, j),
        73 => happy_goto(p, action_226, j),
        74 => happy_goto(p, action_425, j),
        76 => happy_goto(p, action_159, j),
        77 => happy_goto(p, action_104, j),
        78 => happy_goto(p, action_160, j),
        80 => happy_goto(p, action_161, j),
        81 => happy_goto(p, action_108, j),
        133 => happy_goto(p, action_426, j),
        134 => happy_goto(p, action_111, j),
        _ => happy_fail(p, j)
    }
}

fn action_230(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_422, j),
        150 => happy_shift(p, action_230, j),
        238 => happy_shift(p, action_142, j),
        239 => happy_shift(p, action_231, j),
        240 => happy_shift(p, action_144, j),
        65 => happy_goto(p, action_418, j),
        69 => happy_goto(p, action_419, j),
        70 => happy_goto(p, action_223, j),
        71 => happy_goto(p, action_224, j),
        72 => happy_goto(p, action_420, j),
        73 => happy_goto(p, action_226, j),
        75 => happy_goto(p, action_156, j),
        76 => happy_goto(p, action_103, j),
        77 => happy_goto(p, action_104, j),
        78 => happy_goto(p, action_105, j),
        80 => happy_goto(p, action_157, j),
        81 => happy_goto(p, action_108, j),
        132 => happy_goto(p, action_109, j),
        133 => happy_goto(p, action_421, j),
        134 => happy_goto(p, action_111, j),
        _ => happy_reduce_473(p, j)
    }
}

fn action_231(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_417, j),
        140 => happy_shift(p, action_184, j),
        88 => happy_goto(p, action_416, j),
        89 => happy_goto(p, action_181, j),
        90 => happy_goto(p, action_182, j),
        _ => happy_reduce_237(p, j)
    }
}

fn action_232(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        181 => happy_shift(p, action_62, j),
        14 => happy_goto(p, action_415, j),
        _ => happy_fail(p, j)
    }
}

fn action_233(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_108(p, j)
    }
}

fn action_234(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_143(p, j)
    }
}

fn action_235(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_155(p, j)
    }
}

fn action_236(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        181 => happy_reduce_26(p, j),
        187 => happy_shift(p, action_410, j),
        35 => happy_goto(p, action_414, j),
        67 => happy_goto(p, action_409, j),
        _ => happy_reduce_233(p, j)
    }
}

fn action_237(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        33 => happy_goto(p, action_413, j),
        _ => happy_reduce_90(p, j)
    }
}

fn action_238(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_109(p, j)
    }
}

fn action_239(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_412, j),
        _ => happy_fail(p, j)
    }
}

fn action_240(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_166(p, j)
    }
}

fn action_241(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        181 => happy_shift(p, action_62, j),
        14 => happy_goto(p, action_411, j),
        _ => happy_fail(p, j)
    }
}

fn action_242(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        187 => happy_shift(p, action_410, j),
        35 => happy_goto(p, action_408, j),
        67 => happy_goto(p, action_409, j),
        _ => happy_reduce_233(p, j)
    }
}

fn action_243(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        33 => happy_goto(p, action_407, j),
        _ => happy_reduce_90(p, j)
    }
}

fn action_244(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        240 => happy_shift(p, action_144, j),
        132 => happy_goto(p, action_406, j),
        133 => happy_goto(p, action_152, j),
        134 => happy_goto(p, action_111, j),
        _ => happy_reduce_473(p, j)
    }
}

fn action_245(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_87(p, j)
    }
}

fn action_246(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        240 => happy_shift(p, action_144, j),
        132 => happy_goto(p, action_405, j),
        133 => happy_goto(p, action_152, j),
        134 => happy_goto(p, action_111, j),
        _ => happy_reduce_473(p, j)
    }
}

fn action_247(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_88(p, j)
    }
}

fn action_248(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_12(p, j)
    }
}

fn action_249(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_26, j),
        144 => happy_shift(p, action_27, j),
        145 => happy_shift(p, action_28, j),
        146 => happy_shift(p, action_29, j),
        147 => happy_shift(p, action_30, j),
        148 => happy_shift(p, action_31, j),
        149 => happy_shift(p, action_32, j),
        150 => happy_shift(p, action_33, j),
        153 => happy_shift(p, action_34, j),
        164 => happy_shift(p, action_35, j),
        184 => happy_shift(p, action_36, j),
        205 => happy_shift(p, action_37, j),
        221 => happy_shift(p, action_38, j),
        234 => happy_shift(p, action_39, j),
        235 => happy_shift(p, action_40, j),
        236 => happy_shift(p, action_41, j),
        237 => happy_shift(p, action_42, j),
        238 => happy_shift(p, action_43, j),
        241 => happy_shift(p, action_44, j),
        242 => happy_shift(p, action_45, j),
        243 => happy_shift(p, action_46, j),
        244 => happy_shift(p, action_47, j),
        245 => happy_shift(p, action_48, j),
        246 => happy_shift(p, action_49, j),
        247 => happy_shift(p, action_50, j),
        100 => happy_goto(p, action_6, j),
        104 => happy_goto(p, action_7, j),
        106 => happy_goto(p, action_8, j),
        107 => happy_goto(p, action_9, j),
        108 => happy_goto(p, action_10, j),
        109 => happy_goto(p, action_11, j),
        110 => happy_goto(p, action_12, j),
        111 => happy_goto(p, action_13, j),
        112 => happy_goto(p, action_14, j),
        113 => happy_goto(p, action_15, j),
        114 => happy_goto(p, action_16, j),
        115 => happy_goto(p, action_17, j),
        116 => happy_goto(p, action_18, j),
        117 => happy_goto(p, action_19, j),
        118 => happy_goto(p, action_20, j),
        119 => happy_goto(p, action_21, j),
        120 => happy_goto(p, action_22, j),
        122 => happy_goto(p, action_404, j),
        127 => happy_goto(p, action_24, j),
        128 => happy_goto(p, action_25, j),
        _ => happy_fail(p, j)
    }
}

fn action_250(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_26, j),
        144 => happy_shift(p, action_27, j),
        145 => happy_shift(p, action_28, j),
        146 => happy_shift(p, action_29, j),
        147 => happy_shift(p, action_30, j),
        148 => happy_shift(p, action_31, j),
        149 => happy_shift(p, action_32, j),
        150 => happy_shift(p, action_33, j),
        153 => happy_shift(p, action_34, j),
        164 => happy_shift(p, action_35, j),
        184 => happy_shift(p, action_36, j),
        205 => happy_shift(p, action_37, j),
        221 => happy_shift(p, action_38, j),
        234 => happy_shift(p, action_39, j),
        235 => happy_shift(p, action_40, j),
        236 => happy_shift(p, action_41, j),
        237 => happy_shift(p, action_42, j),
        238 => happy_shift(p, action_43, j),
        241 => happy_shift(p, action_44, j),
        242 => happy_shift(p, action_45, j),
        243 => happy_shift(p, action_46, j),
        244 => happy_shift(p, action_47, j),
        245 => happy_shift(p, action_48, j),
        246 => happy_shift(p, action_49, j),
        247 => happy_shift(p, action_50, j),
        100 => happy_goto(p, action_6, j),
        104 => happy_goto(p, action_7, j),
        106 => happy_goto(p, action_8, j),
        107 => happy_goto(p, action_9, j),
        108 => happy_goto(p, action_10, j),
        109 => happy_goto(p, action_11, j),
        110 => happy_goto(p, action_12, j),
        111 => happy_goto(p, action_13, j),
        112 => happy_goto(p, action_14, j),
        113 => happy_goto(p, action_15, j),
        114 => happy_goto(p, action_16, j),
        115 => happy_goto(p, action_17, j),
        116 => happy_goto(p, action_18, j),
        117 => happy_goto(p, action_19, j),
        118 => happy_goto(p, action_20, j),
        119 => happy_goto(p, action_21, j),
        120 => happy_goto(p, action_22, j),
        122 => happy_goto(p, action_403, j),
        127 => happy_goto(p, action_24, j),
        128 => happy_goto(p, action_25, j),
        _ => happy_fail(p, j)
    }
}

fn action_251(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_459(p, j)
    }
}

fn action_252(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        180 => happy_shift(p, action_402, j),
        _ => happy_fail(p, j)
    }
}

fn action_253(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_26, j),
        144 => happy_shift(p, action_27, j),
        145 => happy_shift(p, action_28, j),
        146 => happy_shift(p, action_29, j),
        147 => happy_shift(p, action_30, j),
        148 => happy_shift(p, action_31, j),
        149 => happy_shift(p, action_32, j),
        150 => happy_shift(p, action_33, j),
        153 => happy_shift(p, action_34, j),
        164 => happy_shift(p, action_35, j),
        184 => happy_shift(p, action_36, j),
        205 => happy_shift(p, action_37, j),
        221 => happy_shift(p, action_38, j),
        234 => happy_shift(p, action_39, j),
        235 => happy_shift(p, action_40, j),
        236 => happy_shift(p, action_41, j),
        237 => happy_shift(p, action_42, j),
        238 => happy_shift(p, action_43, j),
        241 => happy_shift(p, action_44, j),
        242 => happy_shift(p, action_45, j),
        243 => happy_shift(p, action_46, j),
        244 => happy_shift(p, action_47, j),
        245 => happy_shift(p, action_48, j),
        246 => happy_shift(p, action_49, j),
        247 => happy_shift(p, action_50, j),
        100 => happy_goto(p, action_6, j),
        104 => happy_goto(p, action_7, j),
        106 => happy_goto(p, action_8, j),
        107 => happy_goto(p, action_9, j),
        108 => happy_goto(p, action_10, j),
        109 => happy_goto(p, action_11, j),
        110 => happy_goto(p, action_12, j),
        111 => happy_goto(p, action_13, j),
        112 => happy_goto(p, action_14, j),
        113 => happy_goto(p, action_15, j),
        114 => happy_goto(p, action_16, j),
        115 => happy_goto(p, action_17, j),
        116 => happy_goto(p, action_18, j),
        117 => happy_goto(p, action_19, j),
        118 => happy_goto(p, action_20, j),
        119 => happy_goto(p, action_21, j),
        120 => happy_goto(p, action_22, j),
        122 => happy_goto(p, action_401, j),
        127 => happy_goto(p, action_24, j),
        128 => happy_goto(p, action_25, j),
        _ => happy_fail(p, j)
    }
}

fn action_254(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        180 => happy_shift(p, action_400, j),
        _ => happy_fail(p, j)
    }
}

fn action_255(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_26, j),
        144 => happy_shift(p, action_27, j),
        145 => happy_shift(p, action_28, j),
        146 => happy_shift(p, action_29, j),
        147 => happy_shift(p, action_30, j),
        148 => happy_shift(p, action_31, j),
        149 => happy_shift(p, action_32, j),
        150 => happy_shift(p, action_33, j),
        153 => happy_shift(p, action_34, j),
        164 => happy_shift(p, action_35, j),
        184 => happy_shift(p, action_36, j),
        205 => happy_shift(p, action_37, j),
        221 => happy_shift(p, action_38, j),
        234 => happy_shift(p, action_39, j),
        235 => happy_shift(p, action_40, j),
        236 => happy_shift(p, action_41, j),
        237 => happy_shift(p, action_42, j),
        238 => happy_shift(p, action_43, j),
        241 => happy_shift(p, action_44, j),
        242 => happy_shift(p, action_45, j),
        243 => happy_shift(p, action_46, j),
        244 => happy_shift(p, action_47, j),
        245 => happy_shift(p, action_48, j),
        246 => happy_shift(p, action_49, j),
        247 => happy_shift(p, action_50, j),
        100 => happy_goto(p, action_6, j),
        104 => happy_goto(p, action_7, j),
        106 => happy_goto(p, action_8, j),
        107 => happy_goto(p, action_9, j),
        108 => happy_goto(p, action_10, j),
        109 => happy_goto(p, action_11, j),
        110 => happy_goto(p, action_12, j),
        111 => happy_goto(p, action_13, j),
        112 => happy_goto(p, action_14, j),
        113 => happy_goto(p, action_15, j),
        114 => happy_goto(p, action_16, j),
        115 => happy_goto(p, action_17, j),
        116 => happy_goto(p, action_18, j),
        117 => happy_goto(p, action_19, j),
        118 => happy_goto(p, action_20, j),
        119 => happy_goto(p, action_21, j),
        120 => happy_goto(p, action_22, j),
        122 => happy_goto(p, action_399, j),
        127 => happy_goto(p, action_24, j),
        128 => happy_goto(p, action_25, j),
        _ => happy_fail(p, j)
    }
}

fn action_256(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_471(p, j)
    }
}

fn action_257(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_26, j),
        144 => happy_shift(p, action_27, j),
        145 => happy_shift(p, action_28, j),
        146 => happy_shift(p, action_29, j),
        147 => happy_shift(p, action_30, j),
        148 => happy_shift(p, action_31, j),
        149 => happy_shift(p, action_32, j),
        150 => happy_shift(p, action_33, j),
        153 => happy_shift(p, action_34, j),
        164 => happy_shift(p, action_35, j),
        184 => happy_shift(p, action_36, j),
        185 => happy_reduce_40(p, j),
        186 => happy_reduce_40(p, j),
        188 => happy_reduce_40(p, j),
        190 => happy_reduce_40(p, j),
        192 => happy_reduce_40(p, j),
        193 => happy_reduce_40(p, j),
        195 => happy_reduce_40(p, j),
        198 => happy_reduce_40(p, j),
        200 => happy_reduce_40(p, j),
        201 => happy_reduce_40(p, j),
        202 => happy_reduce_40(p, j),
        203 => happy_reduce_40(p, j),
        205 => happy_shift(p, action_37, j),
        208 => happy_reduce_40(p, j),
        209 => happy_reduce_40(p, j),
        210 => happy_reduce_40(p, j),
        211 => happy_reduce_40(p, j),
        213 => happy_reduce_40(p, j),
        214 => happy_reduce_40(p, j),
        215 => happy_reduce_40(p, j),
        216 => happy_reduce_40(p, j),
        217 => happy_reduce_40(p, j),
        219 => happy_reduce_40(p, j),
        220 => happy_reduce_40(p, j),
        221 => happy_shift(p, action_38, j),
        222 => happy_reduce_40(p, j),
        223 => happy_reduce_40(p, j),
        224 => happy_reduce_40(p, j),
        226 => happy_reduce_40(p, j),
        227 => happy_reduce_40(p, j),
        228 => happy_reduce_40(p, j),
        229 => happy_reduce_40(p, j),
        230 => happy_reduce_40(p, j),
        231 => happy_reduce_40(p, j),
        232 => happy_reduce_40(p, j),
        234 => happy_shift(p, action_39, j),
        235 => happy_shift(p, action_40, j),
        236 => happy_shift(p, action_41, j),
        237 => happy_shift(p, action_42, j),
        238 => happy_shift(p, action_43, j),
        239 => happy_reduce_40(p, j),
        240 => happy_reduce_40(p, j),
        241 => happy_shift(p, action_44, j),
        242 => happy_shift(p, action_45, j),
        243 => happy_shift(p, action_46, j),
        244 => happy_shift(p, action_47, j),
        245 => happy_shift(p, action_48, j),
        246 => happy_shift(p, action_49, j),
        247 => happy_shift(p, action_50, j),
        15 => happy_goto(p, action_397, j),
        100 => happy_goto(p, action_6, j),
        104 => happy_goto(p, action_7, j),
        106 => happy_goto(p, action_8, j),
        107 => happy_goto(p, action_9, j),
        108 => happy_goto(p, action_10, j),
        109 => happy_goto(p, action_11, j),
        110 => happy_goto(p, action_12, j),
        111 => happy_goto(p, action_13, j),
        112 => happy_goto(p, action_14, j),
        113 => happy_goto(p, action_15, j),
        114 => happy_goto(p, action_16, j),
        115 => happy_goto(p, action_17, j),
        116 => happy_goto(p, action_18, j),
        117 => happy_goto(p, action_19, j),
        118 => happy_goto(p, action_20, j),
        119 => happy_goto(p, action_21, j),
        120 => happy_goto(p, action_22, j),
        122 => happy_goto(p, action_251, j),
        124 => happy_goto(p, action_398, j),
        127 => happy_goto(p, action_24, j),
        128 => happy_goto(p, action_25, j),
        _ => happy_reduce_458(p, j)
    }
}

fn action_258(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        233 => happy_shift(p, action_396, j),
        _ => happy_fail(p, j)
    }
}

fn action_259(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_26, j),
        144 => happy_shift(p, action_27, j),
        145 => happy_shift(p, action_28, j),
        146 => happy_shift(p, action_29, j),
        147 => happy_shift(p, action_30, j),
        148 => happy_shift(p, action_31, j),
        149 => happy_shift(p, action_32, j),
        150 => happy_shift(p, action_33, j),
        153 => happy_shift(p, action_34, j),
        164 => happy_shift(p, action_35, j),
        180 => happy_shift(p, action_61, j),
        181 => happy_shift(p, action_62, j),
        184 => happy_shift(p, action_36, j),
        187 => happy_shift(p, action_63, j),
        189 => happy_shift(p, action_64, j),
        191 => happy_shift(p, action_65, j),
        194 => happy_shift(p, action_66, j),
        196 => happy_shift(p, action_67, j),
        197 => happy_shift(p, action_68, j),
        204 => happy_shift(p, action_69, j),
        205 => happy_shift(p, action_37, j),
        206 => happy_shift(p, action_70, j),
        207 => happy_shift(p, action_71, j),
        218 => happy_shift(p, action_72, j),
        221 => happy_shift(p, action_38, j),
        225 => happy_shift(p, action_73, j),
        233 => happy_shift(p, action_74, j),
        234 => happy_shift(p, action_39, j),
        235 => happy_shift(p, action_40, j),
        236 => happy_shift(p, action_41, j),
        237 => happy_shift(p, action_42, j),
        238 => happy_shift(p, action_75, j),
        239 => happy_shift(p, action_76, j),
        241 => happy_shift(p, action_44, j),
        242 => happy_shift(p, action_45, j),
        243 => happy_shift(p, action_46, j),
        244 => happy_shift(p, action_47, j),
        245 => happy_shift(p, action_48, j),
        246 => happy_shift(p, action_49, j),
        247 => happy_shift(p, action_50, j),
        12 => happy_goto(p, action_395, j),
        13 => happy_goto(p, action_52, j),
        14 => happy_goto(p, action_53, j),
        22 => happy_goto(p, action_54, j),
        23 => happy_goto(p, action_55, j),
        24 => happy_goto(p, action_56, j),
        25 => happy_goto(p, action_57, j),
        26 => happy_goto(p, action_58, j),
        100 => happy_goto(p, action_6, j),
        104 => happy_goto(p, action_7, j),
        106 => happy_goto(p, action_8, j),
        107 => happy_goto(p, action_9, j),
        108 => happy_goto(p, action_10, j),
        109 => happy_goto(p, action_11, j),
        110 => happy_goto(p, action_12, j),
        111 => happy_goto(p, action_13, j),
        112 => happy_goto(p, action_14, j),
        113 => happy_goto(p, action_15, j),
        114 => happy_goto(p, action_16, j),
        115 => happy_goto(p, action_17, j),
        116 => happy_goto(p, action_18, j),
        117 => happy_goto(p, action_19, j),
        118 => happy_goto(p, action_20, j),
        119 => happy_goto(p, action_21, j),
        120 => happy_goto(p, action_22, j),
        122 => happy_goto(p, action_59, j),
        127 => happy_goto(p, action_24, j),
        128 => happy_goto(p, action_25, j),
        131 => happy_goto(p, action_60, j),
        _ => happy_fail(p, j)
    }
}

fn action_260(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_67(p, j)
    }
}

fn action_261(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_408(p, j)
    }
}

fn action_262(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_462(p, j)
    }
}

fn action_263(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        167 => happy_shift(p, action_393, j),
        183 => happy_shift(p, action_394, j),
        _ => happy_fail(p, j)
    }
}

fn action_264(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_68(p, j)
    }
}

fn action_265(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_392, j),
        _ => happy_fail(p, j)
    }
}

fn action_266(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_75(p, j)
    }
}

fn action_267(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        212 => happy_shift(p, action_391, j),
        17 => happy_goto(p, action_389, j),
        21 => happy_goto(p, action_390, j),
        _ => happy_reduce_42(p, j)
    }
}

fn action_268(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        240 => happy_shift(p, action_144, j),
        132 => happy_goto(p, action_388, j),
        133 => happy_goto(p, action_152, j),
        134 => happy_goto(p, action_111, j),
        _ => happy_reduce_473(p, j)
    }
}

fn action_269(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_57(p, j)
    }
}

fn action_270(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_26, j),
        144 => happy_shift(p, action_27, j),
        145 => happy_shift(p, action_28, j),
        146 => happy_shift(p, action_29, j),
        147 => happy_shift(p, action_30, j),
        148 => happy_shift(p, action_31, j),
        149 => happy_shift(p, action_32, j),
        150 => happy_shift(p, action_33, j),
        153 => happy_shift(p, action_34, j),
        164 => happy_shift(p, action_35, j),
        184 => happy_shift(p, action_36, j),
        205 => happy_shift(p, action_37, j),
        221 => happy_shift(p, action_38, j),
        234 => happy_shift(p, action_39, j),
        235 => happy_shift(p, action_40, j),
        236 => happy_shift(p, action_41, j),
        237 => happy_shift(p, action_42, j),
        238 => happy_shift(p, action_43, j),
        241 => happy_shift(p, action_44, j),
        242 => happy_shift(p, action_45, j),
        243 => happy_shift(p, action_46, j),
        244 => happy_shift(p, action_47, j),
        245 => happy_shift(p, action_48, j),
        246 => happy_shift(p, action_49, j),
        247 => happy_shift(p, action_50, j),
        100 => happy_goto(p, action_6, j),
        104 => happy_goto(p, action_7, j),
        106 => happy_goto(p, action_8, j),
        107 => happy_goto(p, action_9, j),
        108 => happy_goto(p, action_10, j),
        109 => happy_goto(p, action_11, j),
        110 => happy_goto(p, action_12, j),
        111 => happy_goto(p, action_13, j),
        112 => happy_goto(p, action_14, j),
        113 => happy_goto(p, action_15, j),
        114 => happy_goto(p, action_16, j),
        115 => happy_goto(p, action_17, j),
        116 => happy_goto(p, action_18, j),
        117 => happy_goto(p, action_19, j),
        118 => happy_goto(p, action_20, j),
        119 => happy_goto(p, action_21, j),
        120 => happy_goto(p, action_387, j),
        127 => happy_goto(p, action_24, j),
        128 => happy_goto(p, action_25, j),
        _ => happy_fail(p, j)
    }
}

fn action_271(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        190 => happy_shift(p, action_117, j),
        192 => happy_shift(p, action_118, j),
        195 => happy_shift(p, action_119, j),
        198 => happy_shift(p, action_120, j),
        200 => happy_shift(p, action_121, j),
        202 => happy_shift(p, action_123, j),
        203 => happy_shift(p, action_124, j),
        209 => happy_shift(p, action_126, j),
        210 => happy_shift(p, action_127, j),
        211 => happy_shift(p, action_128, j),
        219 => happy_shift(p, action_131, j),
        220 => happy_shift(p, action_132, j),
        224 => happy_shift(p, action_135, j),
        227 => happy_shift(p, action_137, j),
        229 => happy_shift(p, action_139, j),
        230 => happy_shift(p, action_140, j),
        231 => happy_shift(p, action_141, j),
        239 => happy_shift(p, action_143, j),
        240 => happy_shift(p, action_144, j),
        44 => happy_goto(p, action_289, j),
        45 => happy_goto(p, action_90, j),
        47 => happy_goto(p, action_290, j),
        49 => happy_goto(p, action_291, j),
        51 => happy_goto(p, action_292, j),
        52 => happy_goto(p, action_97, j),
        53 => happy_goto(p, action_98, j),
        54 => happy_goto(p, action_99, j),
        61 => happy_goto(p, action_100, j),
        65 => happy_goto(p, action_293, j),
        86 => happy_goto(p, action_386, j),
        132 => happy_goto(p, action_109, j),
        133 => happy_goto(p, action_296, j),
        134 => happy_goto(p, action_111, j),
        _ => happy_reduce_473(p, j)
    }
}

fn action_272(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        190 => happy_shift(p, action_117, j),
        192 => happy_shift(p, action_118, j),
        195 => happy_shift(p, action_119, j),
        198 => happy_shift(p, action_120, j),
        200 => happy_shift(p, action_121, j),
        202 => happy_shift(p, action_123, j),
        203 => happy_shift(p, action_124, j),
        209 => happy_shift(p, action_126, j),
        210 => happy_shift(p, action_127, j),
        211 => happy_shift(p, action_128, j),
        219 => happy_shift(p, action_131, j),
        220 => happy_shift(p, action_132, j),
        224 => happy_shift(p, action_135, j),
        227 => happy_shift(p, action_137, j),
        229 => happy_shift(p, action_139, j),
        230 => happy_shift(p, action_140, j),
        231 => happy_shift(p, action_141, j),
        239 => happy_shift(p, action_143, j),
        240 => happy_shift(p, action_144, j),
        44 => happy_goto(p, action_289, j),
        45 => happy_goto(p, action_90, j),
        47 => happy_goto(p, action_290, j),
        49 => happy_goto(p, action_291, j),
        51 => happy_goto(p, action_292, j),
        52 => happy_goto(p, action_97, j),
        53 => happy_goto(p, action_98, j),
        54 => happy_goto(p, action_99, j),
        61 => happy_goto(p, action_100, j),
        65 => happy_goto(p, action_293, j),
        86 => happy_goto(p, action_385, j),
        132 => happy_goto(p, action_109, j),
        133 => happy_goto(p, action_296, j),
        134 => happy_goto(p, action_111, j),
        _ => happy_reduce_473(p, j)
    }
}

fn action_273(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_26, j),
        144 => happy_shift(p, action_27, j),
        145 => happy_shift(p, action_28, j),
        146 => happy_shift(p, action_29, j),
        147 => happy_shift(p, action_30, j),
        148 => happy_shift(p, action_31, j),
        149 => happy_shift(p, action_32, j),
        150 => happy_shift(p, action_33, j),
        153 => happy_shift(p, action_34, j),
        164 => happy_shift(p, action_35, j),
        184 => happy_shift(p, action_36, j),
        205 => happy_shift(p, action_37, j),
        221 => happy_shift(p, action_38, j),
        234 => happy_shift(p, action_39, j),
        235 => happy_shift(p, action_40, j),
        236 => happy_shift(p, action_41, j),
        237 => happy_shift(p, action_42, j),
        238 => happy_shift(p, action_43, j),
        241 => happy_shift(p, action_44, j),
        242 => happy_shift(p, action_45, j),
        243 => happy_shift(p, action_46, j),
        244 => happy_shift(p, action_47, j),
        245 => happy_shift(p, action_48, j),
        246 => happy_shift(p, action_49, j),
        247 => happy_shift(p, action_50, j),
        100 => happy_goto(p, action_6, j),
        104 => happy_goto(p, action_7, j),
        106 => happy_goto(p, action_8, j),
        107 => happy_goto(p, action_9, j),
        108 => happy_goto(p, action_10, j),
        109 => happy_goto(p, action_11, j),
        110 => happy_goto(p, action_12, j),
        111 => happy_goto(p, action_13, j),
        112 => happy_goto(p, action_14, j),
        113 => happy_goto(p, action_15, j),
        114 => happy_goto(p, action_16, j),
        115 => happy_goto(p, action_17, j),
        116 => happy_goto(p, action_18, j),
        117 => happy_goto(p, action_19, j),
        118 => happy_goto(p, action_20, j),
        119 => happy_goto(p, action_21, j),
        120 => happy_goto(p, action_384, j),
        127 => happy_goto(p, action_24, j),
        128 => happy_goto(p, action_25, j),
        _ => happy_fail(p, j)
    }
}

fn action_274(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_400(p, j)
    }
}

fn action_275(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_26, j),
        144 => happy_shift(p, action_27, j),
        145 => happy_shift(p, action_28, j),
        146 => happy_shift(p, action_29, j),
        147 => happy_shift(p, action_30, j),
        148 => happy_shift(p, action_31, j),
        149 => happy_shift(p, action_32, j),
        150 => happy_shift(p, action_33, j),
        153 => happy_shift(p, action_34, j),
        164 => happy_shift(p, action_35, j),
        181 => happy_shift(p, action_62, j),
        184 => happy_shift(p, action_36, j),
        190 => happy_shift(p, action_117, j),
        192 => happy_shift(p, action_118, j),
        195 => happy_shift(p, action_119, j),
        198 => happy_shift(p, action_120, j),
        200 => happy_shift(p, action_121, j),
        202 => happy_shift(p, action_123, j),
        203 => happy_shift(p, action_124, j),
        205 => happy_shift(p, action_37, j),
        209 => happy_shift(p, action_126, j),
        210 => happy_shift(p, action_127, j),
        211 => happy_shift(p, action_128, j),
        219 => happy_shift(p, action_131, j),
        220 => happy_shift(p, action_132, j),
        221 => happy_shift(p, action_38, j),
        224 => happy_shift(p, action_135, j),
        227 => happy_shift(p, action_137, j),
        229 => happy_shift(p, action_139, j),
        230 => happy_shift(p, action_140, j),
        231 => happy_shift(p, action_141, j),
        234 => happy_shift(p, action_39, j),
        235 => happy_shift(p, action_40, j),
        236 => happy_shift(p, action_41, j),
        237 => happy_shift(p, action_42, j),
        238 => happy_shift(p, action_43, j),
        239 => happy_shift(p, action_143, j),
        240 => happy_shift(p, action_144, j),
        241 => happy_shift(p, action_44, j),
        242 => happy_shift(p, action_45, j),
        243 => happy_shift(p, action_46, j),
        244 => happy_shift(p, action_47, j),
        245 => happy_shift(p, action_48, j),
        246 => happy_shift(p, action_49, j),
        247 => happy_shift(p, action_50, j),
        14 => happy_goto(p, action_288, j),
        44 => happy_goto(p, action_289, j),
        45 => happy_goto(p, action_90, j),
        47 => happy_goto(p, action_290, j),
        49 => happy_goto(p, action_291, j),
        51 => happy_goto(p, action_292, j),
        52 => happy_goto(p, action_97, j),
        53 => happy_goto(p, action_98, j),
        54 => happy_goto(p, action_99, j),
        61 => happy_goto(p, action_100, j),
        65 => happy_goto(p, action_293, j),
        86 => happy_goto(p, action_383, j),
        100 => happy_goto(p, action_6, j),
        104 => happy_goto(p, action_7, j),
        106 => happy_goto(p, action_8, j),
        107 => happy_goto(p, action_9, j),
        108 => happy_goto(p, action_10, j),
        109 => happy_goto(p, action_11, j),
        110 => happy_goto(p, action_12, j),
        111 => happy_goto(p, action_13, j),
        112 => happy_goto(p, action_14, j),
        113 => happy_goto(p, action_15, j),
        114 => happy_goto(p, action_16, j),
        115 => happy_goto(p, action_17, j),
        116 => happy_goto(p, action_18, j),
        117 => happy_goto(p, action_19, j),
        118 => happy_goto(p, action_20, j),
        119 => happy_goto(p, action_21, j),
        120 => happy_goto(p, action_22, j),
        122 => happy_goto(p, action_295, j),
        127 => happy_goto(p, action_24, j),
        128 => happy_goto(p, action_25, j),
        132 => happy_goto(p, action_109, j),
        133 => happy_goto(p, action_296, j),
        134 => happy_goto(p, action_111, j),
        _ => happy_reduce_473(p, j)
    }
}

fn action_276(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_399(p, j)
    }
}

fn action_277(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_393(p, j)
    }
}

fn action_278(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        237 => happy_shift(p, action_382, j),
        _ => happy_reduce_467(p, j)
    }
}

fn action_279(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_468(p, j)
    }
}

fn action_280(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_395(p, j)
    }
}

fn action_281(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_26, j),
        144 => happy_shift(p, action_27, j),
        145 => happy_shift(p, action_28, j),
        146 => happy_shift(p, action_29, j),
        147 => happy_shift(p, action_30, j),
        148 => happy_shift(p, action_31, j),
        149 => happy_shift(p, action_32, j),
        150 => happy_shift(p, action_33, j),
        153 => happy_shift(p, action_34, j),
        164 => happy_shift(p, action_35, j),
        181 => happy_shift(p, action_62, j),
        184 => happy_shift(p, action_36, j),
        190 => happy_shift(p, action_117, j),
        192 => happy_shift(p, action_118, j),
        195 => happy_shift(p, action_119, j),
        198 => happy_shift(p, action_120, j),
        200 => happy_shift(p, action_121, j),
        202 => happy_shift(p, action_123, j),
        203 => happy_shift(p, action_124, j),
        205 => happy_shift(p, action_37, j),
        209 => happy_shift(p, action_126, j),
        210 => happy_shift(p, action_127, j),
        211 => happy_shift(p, action_128, j),
        219 => happy_shift(p, action_131, j),
        220 => happy_shift(p, action_132, j),
        221 => happy_shift(p, action_38, j),
        224 => happy_shift(p, action_135, j),
        227 => happy_shift(p, action_137, j),
        229 => happy_shift(p, action_139, j),
        230 => happy_shift(p, action_140, j),
        231 => happy_shift(p, action_141, j),
        234 => happy_shift(p, action_39, j),
        235 => happy_shift(p, action_40, j),
        236 => happy_shift(p, action_41, j),
        237 => happy_shift(p, action_42, j),
        238 => happy_shift(p, action_43, j),
        239 => happy_shift(p, action_143, j),
        240 => happy_shift(p, action_144, j),
        241 => happy_shift(p, action_44, j),
        242 => happy_shift(p, action_45, j),
        243 => happy_shift(p, action_46, j),
        244 => happy_shift(p, action_47, j),
        245 => happy_shift(p, action_48, j),
        246 => happy_shift(p, action_49, j),
        247 => happy_shift(p, action_50, j),
        14 => happy_goto(p, action_288, j),
        44 => happy_goto(p, action_289, j),
        45 => happy_goto(p, action_90, j),
        47 => happy_goto(p, action_290, j),
        49 => happy_goto(p, action_291, j),
        51 => happy_goto(p, action_292, j),
        52 => happy_goto(p, action_97, j),
        53 => happy_goto(p, action_98, j),
        54 => happy_goto(p, action_99, j),
        61 => happy_goto(p, action_100, j),
        65 => happy_goto(p, action_293, j),
        86 => happy_goto(p, action_381, j),
        100 => happy_goto(p, action_6, j),
        104 => happy_goto(p, action_7, j),
        106 => happy_goto(p, action_8, j),
        107 => happy_goto(p, action_9, j),
        108 => happy_goto(p, action_10, j),
        109 => happy_goto(p, action_11, j),
        110 => happy_goto(p, action_12, j),
        111 => happy_goto(p, action_13, j),
        112 => happy_goto(p, action_14, j),
        113 => happy_goto(p, action_15, j),
        114 => happy_goto(p, action_16, j),
        115 => happy_goto(p, action_17, j),
        116 => happy_goto(p, action_18, j),
        117 => happy_goto(p, action_19, j),
        118 => happy_goto(p, action_20, j),
        119 => happy_goto(p, action_21, j),
        120 => happy_goto(p, action_22, j),
        122 => happy_goto(p, action_295, j),
        127 => happy_goto(p, action_24, j),
        128 => happy_goto(p, action_25, j),
        132 => happy_goto(p, action_109, j),
        133 => happy_goto(p, action_296, j),
        134 => happy_goto(p, action_111, j),
        _ => happy_reduce_473(p, j)
    }
}

fn action_282(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_26, j),
        144 => happy_shift(p, action_27, j),
        145 => happy_shift(p, action_28, j),
        146 => happy_shift(p, action_29, j),
        147 => happy_shift(p, action_30, j),
        148 => happy_shift(p, action_31, j),
        149 => happy_shift(p, action_32, j),
        150 => happy_shift(p, action_33, j),
        153 => happy_shift(p, action_34, j),
        164 => happy_shift(p, action_35, j),
        184 => happy_shift(p, action_36, j),
        205 => happy_shift(p, action_37, j),
        221 => happy_shift(p, action_38, j),
        234 => happy_shift(p, action_39, j),
        235 => happy_shift(p, action_40, j),
        236 => happy_shift(p, action_41, j),
        237 => happy_shift(p, action_42, j),
        238 => happy_shift(p, action_43, j),
        241 => happy_shift(p, action_44, j),
        242 => happy_shift(p, action_45, j),
        243 => happy_shift(p, action_46, j),
        244 => happy_shift(p, action_47, j),
        245 => happy_shift(p, action_48, j),
        246 => happy_shift(p, action_49, j),
        247 => happy_shift(p, action_50, j),
        100 => happy_goto(p, action_6, j),
        104 => happy_goto(p, action_7, j),
        106 => happy_goto(p, action_8, j),
        107 => happy_goto(p, action_9, j),
        108 => happy_goto(p, action_10, j),
        109 => happy_goto(p, action_11, j),
        110 => happy_goto(p, action_12, j),
        111 => happy_goto(p, action_13, j),
        112 => happy_goto(p, action_14, j),
        113 => happy_goto(p, action_15, j),
        114 => happy_goto(p, action_16, j),
        115 => happy_goto(p, action_17, j),
        116 => happy_goto(p, action_18, j),
        117 => happy_goto(p, action_19, j),
        118 => happy_goto(p, action_20, j),
        119 => happy_goto(p, action_21, j),
        120 => happy_goto(p, action_380, j),
        127 => happy_goto(p, action_24, j),
        128 => happy_goto(p, action_25, j),
        _ => happy_fail(p, j)
    }
}

fn action_283(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_397(p, j)
    }
}

fn action_284(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_26, j),
        144 => happy_shift(p, action_27, j),
        145 => happy_shift(p, action_28, j),
        146 => happy_shift(p, action_29, j),
        147 => happy_shift(p, action_30, j),
        148 => happy_shift(p, action_31, j),
        149 => happy_shift(p, action_32, j),
        150 => happy_shift(p, action_33, j),
        153 => happy_shift(p, action_34, j),
        164 => happy_shift(p, action_35, j),
        181 => happy_shift(p, action_62, j),
        184 => happy_shift(p, action_36, j),
        190 => happy_shift(p, action_117, j),
        192 => happy_shift(p, action_118, j),
        195 => happy_shift(p, action_119, j),
        198 => happy_shift(p, action_120, j),
        200 => happy_shift(p, action_121, j),
        202 => happy_shift(p, action_123, j),
        203 => happy_shift(p, action_124, j),
        205 => happy_shift(p, action_37, j),
        209 => happy_shift(p, action_126, j),
        210 => happy_shift(p, action_127, j),
        211 => happy_shift(p, action_128, j),
        219 => happy_shift(p, action_131, j),
        220 => happy_shift(p, action_132, j),
        221 => happy_shift(p, action_38, j),
        224 => happy_shift(p, action_135, j),
        227 => happy_shift(p, action_137, j),
        229 => happy_shift(p, action_139, j),
        230 => happy_shift(p, action_140, j),
        231 => happy_shift(p, action_141, j),
        234 => happy_shift(p, action_39, j),
        235 => happy_shift(p, action_40, j),
        236 => happy_shift(p, action_41, j),
        237 => happy_shift(p, action_42, j),
        238 => happy_shift(p, action_43, j),
        239 => happy_shift(p, action_143, j),
        240 => happy_shift(p, action_144, j),
        241 => happy_shift(p, action_44, j),
        242 => happy_shift(p, action_45, j),
        243 => happy_shift(p, action_46, j),
        244 => happy_shift(p, action_47, j),
        245 => happy_shift(p, action_48, j),
        246 => happy_shift(p, action_49, j),
        247 => happy_shift(p, action_50, j),
        14 => happy_goto(p, action_288, j),
        44 => happy_goto(p, action_289, j),
        45 => happy_goto(p, action_90, j),
        47 => happy_goto(p, action_290, j),
        49 => happy_goto(p, action_291, j),
        51 => happy_goto(p, action_292, j),
        52 => happy_goto(p, action_97, j),
        53 => happy_goto(p, action_98, j),
        54 => happy_goto(p, action_99, j),
        61 => happy_goto(p, action_100, j),
        65 => happy_goto(p, action_293, j),
        86 => happy_goto(p, action_379, j),
        100 => happy_goto(p, action_6, j),
        104 => happy_goto(p, action_7, j),
        106 => happy_goto(p, action_8, j),
        107 => happy_goto(p, action_9, j),
        108 => happy_goto(p, action_10, j),
        109 => happy_goto(p, action_11, j),
        110 => happy_goto(p, action_12, j),
        111 => happy_goto(p, action_13, j),
        112 => happy_goto(p, action_14, j),
        113 => happy_goto(p, action_15, j),
        114 => happy_goto(p, action_16, j),
        115 => happy_goto(p, action_17, j),
        116 => happy_goto(p, action_18, j),
        117 => happy_goto(p, action_19, j),
        118 => happy_goto(p, action_20, j),
        119 => happy_goto(p, action_21, j),
        120 => happy_goto(p, action_22, j),
        122 => happy_goto(p, action_295, j),
        127 => happy_goto(p, action_24, j),
        128 => happy_goto(p, action_25, j),
        132 => happy_goto(p, action_109, j),
        133 => happy_goto(p, action_296, j),
        134 => happy_goto(p, action_111, j),
        _ => happy_reduce_473(p, j)
    }
}

fn action_285(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_401(p, j)
    }
}

fn action_286(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_392(p, j)
    }
}

fn action_287(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_391(p, j)
    }
}

fn action_288(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        139 => happy_shift(p, action_378, j),
        _ => happy_fail(p, j)
    }
}

fn action_289(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_375, j),
        140 => happy_shift(p, action_184, j),
        150 => happy_shift(p, action_376, j),
        87 => happy_goto(p, action_377, j),
        88 => happy_goto(p, action_370, j),
        89 => happy_goto(p, action_181, j),
        90 => happy_goto(p, action_182, j),
        91 => happy_goto(p, action_371, j),
        92 => happy_goto(p, action_372, j),
        _ => happy_reduce_305(p, j)
    }
}

fn action_290(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        186 => happy_shift(p, action_173, j),
        190 => happy_shift(p, action_117, j),
        192 => happy_shift(p, action_118, j),
        193 => happy_shift(p, action_174, j),
        195 => happy_shift(p, action_119, j),
        198 => happy_shift(p, action_120, j),
        202 => happy_shift(p, action_123, j),
        203 => happy_shift(p, action_124, j),
        209 => happy_shift(p, action_126, j),
        210 => happy_shift(p, action_127, j),
        211 => happy_shift(p, action_128, j),
        214 => happy_shift(p, action_175, j),
        215 => happy_shift(p, action_176, j),
        217 => happy_shift(p, action_177, j),
        219 => happy_shift(p, action_131, j),
        220 => happy_shift(p, action_132, j),
        230 => happy_shift(p, action_140, j),
        231 => happy_shift(p, action_141, j),
        232 => happy_shift(p, action_178, j),
        240 => happy_shift(p, action_144, j),
        45 => happy_goto(p, action_213, j),
        64 => happy_goto(p, action_214, j),
        134 => happy_goto(p, action_215, j),
        _ => happy_reduce_127(p, j)
    }
}

fn action_291(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        186 => happy_shift(p, action_173, j),
        193 => happy_shift(p, action_174, j),
        214 => happy_shift(p, action_175, j),
        215 => happy_shift(p, action_176, j),
        217 => happy_shift(p, action_177, j),
        232 => happy_shift(p, action_178, j),
        240 => happy_shift(p, action_144, j),
        64 => happy_goto(p, action_206, j),
        134 => happy_goto(p, action_207, j),
        _ => happy_reduce_128(p, j)
    }
}

fn action_292(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        186 => happy_shift(p, action_173, j),
        193 => happy_shift(p, action_174, j),
        214 => happy_shift(p, action_175, j),
        215 => happy_shift(p, action_176, j),
        217 => happy_shift(p, action_177, j),
        232 => happy_shift(p, action_178, j),
        240 => happy_shift(p, action_144, j),
        64 => happy_goto(p, action_197, j),
        134 => happy_goto(p, action_198, j),
        _ => happy_reduce_129(p, j)
    }
}

fn action_293(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_375, j),
        140 => happy_shift(p, action_184, j),
        150 => happy_shift(p, action_376, j),
        186 => happy_shift(p, action_173, j),
        190 => happy_shift(p, action_117, j),
        192 => happy_shift(p, action_118, j),
        193 => happy_shift(p, action_174, j),
        195 => happy_shift(p, action_119, j),
        198 => happy_shift(p, action_120, j),
        200 => happy_shift(p, action_121, j),
        202 => happy_shift(p, action_123, j),
        203 => happy_shift(p, action_124, j),
        209 => happy_shift(p, action_126, j),
        210 => happy_shift(p, action_127, j),
        211 => happy_shift(p, action_128, j),
        214 => happy_shift(p, action_175, j),
        215 => happy_shift(p, action_176, j),
        217 => happy_shift(p, action_177, j),
        219 => happy_shift(p, action_131, j),
        220 => happy_shift(p, action_132, j),
        224 => happy_shift(p, action_135, j),
        227 => happy_shift(p, action_193, j),
        229 => happy_shift(p, action_139, j),
        230 => happy_shift(p, action_140, j),
        231 => happy_shift(p, action_141, j),
        232 => happy_shift(p, action_178, j),
        239 => happy_shift(p, action_194, j),
        240 => happy_shift(p, action_144, j),
        45 => happy_goto(p, action_187, j),
        52 => happy_goto(p, action_188, j),
        53 => happy_goto(p, action_98, j),
        54 => happy_goto(p, action_99, j),
        61 => happy_goto(p, action_100, j),
        64 => happy_goto(p, action_189, j),
        87 => happy_goto(p, action_369, j),
        88 => happy_goto(p, action_370, j),
        89 => happy_goto(p, action_181, j),
        90 => happy_goto(p, action_182, j),
        91 => happy_goto(p, action_371, j),
        92 => happy_goto(p, action_372, j),
        133 => happy_goto(p, action_373, j),
        134 => happy_goto(p, action_374, j),
        _ => happy_fail(p, j)
    }
}

fn action_294(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        139 => happy_shift(p, action_368, j),
        _ => happy_fail(p, j)
    }
}

fn action_295(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        139 => happy_shift(p, action_367, j),
        _ => happy_fail(p, j)
    }
}

fn action_296(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        190 => happy_shift(p, action_117, j),
        192 => happy_shift(p, action_118, j),
        195 => happy_shift(p, action_119, j),
        198 => happy_shift(p, action_120, j),
        200 => happy_shift(p, action_121, j),
        202 => happy_shift(p, action_123, j),
        203 => happy_shift(p, action_124, j),
        209 => happy_shift(p, action_126, j),
        210 => happy_shift(p, action_127, j),
        211 => happy_shift(p, action_128, j),
        219 => happy_shift(p, action_131, j),
        220 => happy_shift(p, action_132, j),
        224 => happy_shift(p, action_135, j),
        227 => happy_shift(p, action_170, j),
        229 => happy_shift(p, action_139, j),
        230 => happy_shift(p, action_140, j),
        231 => happy_shift(p, action_141, j),
        239 => happy_shift(p, action_171, j),
        240 => happy_shift(p, action_144, j),
        45 => happy_goto(p, action_165, j),
        52 => happy_goto(p, action_166, j),
        53 => happy_goto(p, action_98, j),
        54 => happy_goto(p, action_99, j),
        61 => happy_goto(p, action_100, j),
        134 => happy_goto(p, action_169, j),
        _ => happy_reduce_474(p, j)
    }
}

fn action_297(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_26, j),
        144 => happy_shift(p, action_27, j),
        145 => happy_shift(p, action_28, j),
        146 => happy_shift(p, action_29, j),
        147 => happy_shift(p, action_30, j),
        148 => happy_shift(p, action_31, j),
        149 => happy_shift(p, action_32, j),
        150 => happy_shift(p, action_33, j),
        153 => happy_shift(p, action_34, j),
        164 => happy_shift(p, action_35, j),
        184 => happy_shift(p, action_36, j),
        205 => happy_shift(p, action_37, j),
        221 => happy_shift(p, action_38, j),
        234 => happy_shift(p, action_39, j),
        235 => happy_shift(p, action_40, j),
        236 => happy_shift(p, action_41, j),
        237 => happy_shift(p, action_42, j),
        238 => happy_shift(p, action_43, j),
        241 => happy_shift(p, action_44, j),
        242 => happy_shift(p, action_45, j),
        243 => happy_shift(p, action_46, j),
        244 => happy_shift(p, action_47, j),
        245 => happy_shift(p, action_48, j),
        246 => happy_shift(p, action_49, j),
        247 => happy_shift(p, action_50, j),
        100 => happy_goto(p, action_6, j),
        104 => happy_goto(p, action_7, j),
        106 => happy_goto(p, action_8, j),
        107 => happy_goto(p, action_9, j),
        108 => happy_goto(p, action_10, j),
        109 => happy_goto(p, action_11, j),
        110 => happy_goto(p, action_12, j),
        111 => happy_goto(p, action_13, j),
        112 => happy_goto(p, action_14, j),
        113 => happy_goto(p, action_15, j),
        114 => happy_goto(p, action_16, j),
        115 => happy_goto(p, action_17, j),
        116 => happy_goto(p, action_18, j),
        117 => happy_goto(p, action_19, j),
        118 => happy_goto(p, action_20, j),
        119 => happy_goto(p, action_21, j),
        120 => happy_goto(p, action_365, j),
        123 => happy_goto(p, action_366, j),
        127 => happy_goto(p, action_24, j),
        128 => happy_goto(p, action_25, j),
        _ => happy_fail(p, j)
    }
}

fn action_298(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_26, j),
        144 => happy_shift(p, action_27, j),
        145 => happy_shift(p, action_28, j),
        146 => happy_shift(p, action_29, j),
        147 => happy_shift(p, action_30, j),
        148 => happy_shift(p, action_31, j),
        149 => happy_shift(p, action_32, j),
        150 => happy_shift(p, action_33, j),
        153 => happy_shift(p, action_34, j),
        164 => happy_shift(p, action_35, j),
        184 => happy_shift(p, action_36, j),
        205 => happy_shift(p, action_37, j),
        221 => happy_shift(p, action_38, j),
        234 => happy_shift(p, action_39, j),
        235 => happy_shift(p, action_40, j),
        236 => happy_shift(p, action_41, j),
        237 => happy_shift(p, action_42, j),
        238 => happy_shift(p, action_43, j),
        241 => happy_shift(p, action_44, j),
        242 => happy_shift(p, action_45, j),
        243 => happy_shift(p, action_46, j),
        244 => happy_shift(p, action_47, j),
        245 => happy_shift(p, action_48, j),
        246 => happy_shift(p, action_49, j),
        247 => happy_shift(p, action_50, j),
        100 => happy_goto(p, action_6, j),
        104 => happy_goto(p, action_7, j),
        106 => happy_goto(p, action_261, j),
        107 => happy_goto(p, action_9, j),
        108 => happy_goto(p, action_10, j),
        109 => happy_goto(p, action_11, j),
        110 => happy_goto(p, action_12, j),
        111 => happy_goto(p, action_13, j),
        112 => happy_goto(p, action_14, j),
        113 => happy_goto(p, action_15, j),
        114 => happy_goto(p, action_16, j),
        115 => happy_goto(p, action_17, j),
        116 => happy_goto(p, action_18, j),
        117 => happy_goto(p, action_364, j),
        127 => happy_goto(p, action_24, j),
        128 => happy_goto(p, action_25, j),
        _ => happy_fail(p, j)
    }
}

fn action_299(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_26, j),
        144 => happy_shift(p, action_27, j),
        145 => happy_shift(p, action_28, j),
        146 => happy_shift(p, action_29, j),
        147 => happy_shift(p, action_30, j),
        148 => happy_shift(p, action_31, j),
        149 => happy_shift(p, action_32, j),
        150 => happy_shift(p, action_33, j),
        153 => happy_shift(p, action_34, j),
        164 => happy_shift(p, action_35, j),
        167 => happy_shift(p, action_363, j),
        184 => happy_shift(p, action_36, j),
        205 => happy_shift(p, action_37, j),
        221 => happy_shift(p, action_38, j),
        234 => happy_shift(p, action_39, j),
        235 => happy_shift(p, action_40, j),
        236 => happy_shift(p, action_41, j),
        237 => happy_shift(p, action_42, j),
        238 => happy_shift(p, action_43, j),
        241 => happy_shift(p, action_44, j),
        242 => happy_shift(p, action_45, j),
        243 => happy_shift(p, action_46, j),
        244 => happy_shift(p, action_47, j),
        245 => happy_shift(p, action_48, j),
        246 => happy_shift(p, action_49, j),
        247 => happy_shift(p, action_50, j),
        100 => happy_goto(p, action_6, j),
        104 => happy_goto(p, action_7, j),
        106 => happy_goto(p, action_8, j),
        107 => happy_goto(p, action_9, j),
        108 => happy_goto(p, action_10, j),
        109 => happy_goto(p, action_11, j),
        110 => happy_goto(p, action_12, j),
        111 => happy_goto(p, action_13, j),
        112 => happy_goto(p, action_14, j),
        113 => happy_goto(p, action_15, j),
        114 => happy_goto(p, action_16, j),
        115 => happy_goto(p, action_17, j),
        116 => happy_goto(p, action_18, j),
        117 => happy_goto(p, action_19, j),
        118 => happy_goto(p, action_20, j),
        119 => happy_goto(p, action_21, j),
        120 => happy_goto(p, action_22, j),
        122 => happy_goto(p, action_362, j),
        127 => happy_goto(p, action_24, j),
        128 => happy_goto(p, action_25, j),
        _ => happy_fail(p, j)
    }
}

fn action_300(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_26, j),
        144 => happy_shift(p, action_27, j),
        145 => happy_shift(p, action_28, j),
        146 => happy_shift(p, action_29, j),
        147 => happy_shift(p, action_30, j),
        148 => happy_shift(p, action_31, j),
        149 => happy_shift(p, action_32, j),
        150 => happy_shift(p, action_33, j),
        153 => happy_shift(p, action_34, j),
        164 => happy_shift(p, action_35, j),
        184 => happy_shift(p, action_36, j),
        205 => happy_shift(p, action_37, j),
        221 => happy_shift(p, action_38, j),
        234 => happy_shift(p, action_39, j),
        235 => happy_shift(p, action_40, j),
        236 => happy_shift(p, action_41, j),
        237 => happy_shift(p, action_42, j),
        238 => happy_shift(p, action_43, j),
        241 => happy_shift(p, action_44, j),
        242 => happy_shift(p, action_45, j),
        243 => happy_shift(p, action_46, j),
        244 => happy_shift(p, action_47, j),
        245 => happy_shift(p, action_48, j),
        246 => happy_shift(p, action_49, j),
        247 => happy_shift(p, action_50, j),
        100 => happy_goto(p, action_6, j),
        104 => happy_goto(p, action_7, j),
        106 => happy_goto(p, action_261, j),
        107 => happy_goto(p, action_9, j),
        108 => happy_goto(p, action_10, j),
        109 => happy_goto(p, action_11, j),
        110 => happy_goto(p, action_12, j),
        111 => happy_goto(p, action_13, j),
        112 => happy_goto(p, action_14, j),
        113 => happy_goto(p, action_15, j),
        114 => happy_goto(p, action_16, j),
        115 => happy_goto(p, action_17, j),
        116 => happy_goto(p, action_361, j),
        127 => happy_goto(p, action_24, j),
        128 => happy_goto(p, action_25, j),
        _ => happy_fail(p, j)
    }
}

fn action_301(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_26, j),
        144 => happy_shift(p, action_27, j),
        145 => happy_shift(p, action_28, j),
        146 => happy_shift(p, action_29, j),
        147 => happy_shift(p, action_30, j),
        148 => happy_shift(p, action_31, j),
        149 => happy_shift(p, action_32, j),
        150 => happy_shift(p, action_33, j),
        153 => happy_shift(p, action_34, j),
        164 => happy_shift(p, action_35, j),
        184 => happy_shift(p, action_36, j),
        205 => happy_shift(p, action_37, j),
        221 => happy_shift(p, action_38, j),
        234 => happy_shift(p, action_39, j),
        235 => happy_shift(p, action_40, j),
        236 => happy_shift(p, action_41, j),
        237 => happy_shift(p, action_42, j),
        238 => happy_shift(p, action_43, j),
        241 => happy_shift(p, action_44, j),
        242 => happy_shift(p, action_45, j),
        243 => happy_shift(p, action_46, j),
        244 => happy_shift(p, action_47, j),
        245 => happy_shift(p, action_48, j),
        246 => happy_shift(p, action_49, j),
        247 => happy_shift(p, action_50, j),
        100 => happy_goto(p, action_6, j),
        104 => happy_goto(p, action_7, j),
        106 => happy_goto(p, action_261, j),
        107 => happy_goto(p, action_9, j),
        108 => happy_goto(p, action_10, j),
        109 => happy_goto(p, action_11, j),
        110 => happy_goto(p, action_12, j),
        111 => happy_goto(p, action_13, j),
        112 => happy_goto(p, action_14, j),
        113 => happy_goto(p, action_15, j),
        114 => happy_goto(p, action_16, j),
        115 => happy_goto(p, action_360, j),
        127 => happy_goto(p, action_24, j),
        128 => happy_goto(p, action_25, j),
        _ => happy_fail(p, j)
    }
}

fn action_302(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_26, j),
        144 => happy_shift(p, action_27, j),
        145 => happy_shift(p, action_28, j),
        146 => happy_shift(p, action_29, j),
        147 => happy_shift(p, action_30, j),
        148 => happy_shift(p, action_31, j),
        149 => happy_shift(p, action_32, j),
        150 => happy_shift(p, action_33, j),
        153 => happy_shift(p, action_34, j),
        164 => happy_shift(p, action_35, j),
        184 => happy_shift(p, action_36, j),
        205 => happy_shift(p, action_37, j),
        221 => happy_shift(p, action_38, j),
        234 => happy_shift(p, action_39, j),
        235 => happy_shift(p, action_40, j),
        236 => happy_shift(p, action_41, j),
        237 => happy_shift(p, action_42, j),
        238 => happy_shift(p, action_43, j),
        241 => happy_shift(p, action_44, j),
        242 => happy_shift(p, action_45, j),
        243 => happy_shift(p, action_46, j),
        244 => happy_shift(p, action_47, j),
        245 => happy_shift(p, action_48, j),
        246 => happy_shift(p, action_49, j),
        247 => happy_shift(p, action_50, j),
        100 => happy_goto(p, action_6, j),
        104 => happy_goto(p, action_7, j),
        106 => happy_goto(p, action_261, j),
        107 => happy_goto(p, action_9, j),
        108 => happy_goto(p, action_10, j),
        109 => happy_goto(p, action_11, j),
        110 => happy_goto(p, action_12, j),
        111 => happy_goto(p, action_13, j),
        112 => happy_goto(p, action_14, j),
        113 => happy_goto(p, action_15, j),
        114 => happy_goto(p, action_359, j),
        127 => happy_goto(p, action_24, j),
        128 => happy_goto(p, action_25, j),
        _ => happy_fail(p, j)
    }
}

fn action_303(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_26, j),
        144 => happy_shift(p, action_27, j),
        145 => happy_shift(p, action_28, j),
        146 => happy_shift(p, action_29, j),
        147 => happy_shift(p, action_30, j),
        148 => happy_shift(p, action_31, j),
        149 => happy_shift(p, action_32, j),
        150 => happy_shift(p, action_33, j),
        153 => happy_shift(p, action_34, j),
        164 => happy_shift(p, action_35, j),
        184 => happy_shift(p, action_36, j),
        205 => happy_shift(p, action_37, j),
        221 => happy_shift(p, action_38, j),
        234 => happy_shift(p, action_39, j),
        235 => happy_shift(p, action_40, j),
        236 => happy_shift(p, action_41, j),
        237 => happy_shift(p, action_42, j),
        238 => happy_shift(p, action_43, j),
        241 => happy_shift(p, action_44, j),
        242 => happy_shift(p, action_45, j),
        243 => happy_shift(p, action_46, j),
        244 => happy_shift(p, action_47, j),
        245 => happy_shift(p, action_48, j),
        246 => happy_shift(p, action_49, j),
        247 => happy_shift(p, action_50, j),
        100 => happy_goto(p, action_6, j),
        104 => happy_goto(p, action_7, j),
        106 => happy_goto(p, action_261, j),
        107 => happy_goto(p, action_9, j),
        108 => happy_goto(p, action_10, j),
        109 => happy_goto(p, action_11, j),
        110 => happy_goto(p, action_12, j),
        111 => happy_goto(p, action_13, j),
        112 => happy_goto(p, action_14, j),
        113 => happy_goto(p, action_358, j),
        127 => happy_goto(p, action_24, j),
        128 => happy_goto(p, action_25, j),
        _ => happy_fail(p, j)
    }
}

fn action_304(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_26, j),
        144 => happy_shift(p, action_27, j),
        145 => happy_shift(p, action_28, j),
        146 => happy_shift(p, action_29, j),
        147 => happy_shift(p, action_30, j),
        148 => happy_shift(p, action_31, j),
        149 => happy_shift(p, action_32, j),
        150 => happy_shift(p, action_33, j),
        153 => happy_shift(p, action_34, j),
        164 => happy_shift(p, action_35, j),
        184 => happy_shift(p, action_36, j),
        205 => happy_shift(p, action_37, j),
        221 => happy_shift(p, action_38, j),
        234 => happy_shift(p, action_39, j),
        235 => happy_shift(p, action_40, j),
        236 => happy_shift(p, action_41, j),
        237 => happy_shift(p, action_42, j),
        238 => happy_shift(p, action_43, j),
        241 => happy_shift(p, action_44, j),
        242 => happy_shift(p, action_45, j),
        243 => happy_shift(p, action_46, j),
        244 => happy_shift(p, action_47, j),
        245 => happy_shift(p, action_48, j),
        246 => happy_shift(p, action_49, j),
        247 => happy_shift(p, action_50, j),
        100 => happy_goto(p, action_6, j),
        104 => happy_goto(p, action_7, j),
        106 => happy_goto(p, action_261, j),
        107 => happy_goto(p, action_9, j),
        108 => happy_goto(p, action_10, j),
        109 => happy_goto(p, action_11, j),
        110 => happy_goto(p, action_12, j),
        111 => happy_goto(p, action_13, j),
        112 => happy_goto(p, action_357, j),
        127 => happy_goto(p, action_24, j),
        128 => happy_goto(p, action_25, j),
        _ => happy_fail(p, j)
    }
}

fn action_305(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_26, j),
        144 => happy_shift(p, action_27, j),
        145 => happy_shift(p, action_28, j),
        146 => happy_shift(p, action_29, j),
        147 => happy_shift(p, action_30, j),
        148 => happy_shift(p, action_31, j),
        149 => happy_shift(p, action_32, j),
        150 => happy_shift(p, action_33, j),
        153 => happy_shift(p, action_34, j),
        164 => happy_shift(p, action_35, j),
        184 => happy_shift(p, action_36, j),
        205 => happy_shift(p, action_37, j),
        221 => happy_shift(p, action_38, j),
        234 => happy_shift(p, action_39, j),
        235 => happy_shift(p, action_40, j),
        236 => happy_shift(p, action_41, j),
        237 => happy_shift(p, action_42, j),
        238 => happy_shift(p, action_43, j),
        241 => happy_shift(p, action_44, j),
        242 => happy_shift(p, action_45, j),
        243 => happy_shift(p, action_46, j),
        244 => happy_shift(p, action_47, j),
        245 => happy_shift(p, action_48, j),
        246 => happy_shift(p, action_49, j),
        247 => happy_shift(p, action_50, j),
        100 => happy_goto(p, action_6, j),
        104 => happy_goto(p, action_7, j),
        106 => happy_goto(p, action_261, j),
        107 => happy_goto(p, action_9, j),
        108 => happy_goto(p, action_10, j),
        109 => happy_goto(p, action_11, j),
        110 => happy_goto(p, action_12, j),
        111 => happy_goto(p, action_13, j),
        112 => happy_goto(p, action_356, j),
        127 => happy_goto(p, action_24, j),
        128 => happy_goto(p, action_25, j),
        _ => happy_fail(p, j)
    }
}

fn action_306(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_26, j),
        144 => happy_shift(p, action_27, j),
        145 => happy_shift(p, action_28, j),
        146 => happy_shift(p, action_29, j),
        147 => happy_shift(p, action_30, j),
        148 => happy_shift(p, action_31, j),
        149 => happy_shift(p, action_32, j),
        150 => happy_shift(p, action_33, j),
        153 => happy_shift(p, action_34, j),
        164 => happy_shift(p, action_35, j),
        184 => happy_shift(p, action_36, j),
        205 => happy_shift(p, action_37, j),
        221 => happy_shift(p, action_38, j),
        234 => happy_shift(p, action_39, j),
        235 => happy_shift(p, action_40, j),
        236 => happy_shift(p, action_41, j),
        237 => happy_shift(p, action_42, j),
        238 => happy_shift(p, action_43, j),
        241 => happy_shift(p, action_44, j),
        242 => happy_shift(p, action_45, j),
        243 => happy_shift(p, action_46, j),
        244 => happy_shift(p, action_47, j),
        245 => happy_shift(p, action_48, j),
        246 => happy_shift(p, action_49, j),
        247 => happy_shift(p, action_50, j),
        100 => happy_goto(p, action_6, j),
        104 => happy_goto(p, action_7, j),
        106 => happy_goto(p, action_261, j),
        107 => happy_goto(p, action_9, j),
        108 => happy_goto(p, action_10, j),
        109 => happy_goto(p, action_11, j),
        110 => happy_goto(p, action_12, j),
        111 => happy_goto(p, action_355, j),
        127 => happy_goto(p, action_24, j),
        128 => happy_goto(p, action_25, j),
        _ => happy_fail(p, j)
    }
}

fn action_307(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_26, j),
        144 => happy_shift(p, action_27, j),
        145 => happy_shift(p, action_28, j),
        146 => happy_shift(p, action_29, j),
        147 => happy_shift(p, action_30, j),
        148 => happy_shift(p, action_31, j),
        149 => happy_shift(p, action_32, j),
        150 => happy_shift(p, action_33, j),
        153 => happy_shift(p, action_34, j),
        164 => happy_shift(p, action_35, j),
        184 => happy_shift(p, action_36, j),
        205 => happy_shift(p, action_37, j),
        221 => happy_shift(p, action_38, j),
        234 => happy_shift(p, action_39, j),
        235 => happy_shift(p, action_40, j),
        236 => happy_shift(p, action_41, j),
        237 => happy_shift(p, action_42, j),
        238 => happy_shift(p, action_43, j),
        241 => happy_shift(p, action_44, j),
        242 => happy_shift(p, action_45, j),
        243 => happy_shift(p, action_46, j),
        244 => happy_shift(p, action_47, j),
        245 => happy_shift(p, action_48, j),
        246 => happy_shift(p, action_49, j),
        247 => happy_shift(p, action_50, j),
        100 => happy_goto(p, action_6, j),
        104 => happy_goto(p, action_7, j),
        106 => happy_goto(p, action_261, j),
        107 => happy_goto(p, action_9, j),
        108 => happy_goto(p, action_10, j),
        109 => happy_goto(p, action_11, j),
        110 => happy_goto(p, action_12, j),
        111 => happy_goto(p, action_354, j),
        127 => happy_goto(p, action_24, j),
        128 => happy_goto(p, action_25, j),
        _ => happy_fail(p, j)
    }
}

fn action_308(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_26, j),
        144 => happy_shift(p, action_27, j),
        145 => happy_shift(p, action_28, j),
        146 => happy_shift(p, action_29, j),
        147 => happy_shift(p, action_30, j),
        148 => happy_shift(p, action_31, j),
        149 => happy_shift(p, action_32, j),
        150 => happy_shift(p, action_33, j),
        153 => happy_shift(p, action_34, j),
        164 => happy_shift(p, action_35, j),
        184 => happy_shift(p, action_36, j),
        205 => happy_shift(p, action_37, j),
        221 => happy_shift(p, action_38, j),
        234 => happy_shift(p, action_39, j),
        235 => happy_shift(p, action_40, j),
        236 => happy_shift(p, action_41, j),
        237 => happy_shift(p, action_42, j),
        238 => happy_shift(p, action_43, j),
        241 => happy_shift(p, action_44, j),
        242 => happy_shift(p, action_45, j),
        243 => happy_shift(p, action_46, j),
        244 => happy_shift(p, action_47, j),
        245 => happy_shift(p, action_48, j),
        246 => happy_shift(p, action_49, j),
        247 => happy_shift(p, action_50, j),
        100 => happy_goto(p, action_6, j),
        104 => happy_goto(p, action_7, j),
        106 => happy_goto(p, action_261, j),
        107 => happy_goto(p, action_9, j),
        108 => happy_goto(p, action_10, j),
        109 => happy_goto(p, action_11, j),
        110 => happy_goto(p, action_12, j),
        111 => happy_goto(p, action_353, j),
        127 => happy_goto(p, action_24, j),
        128 => happy_goto(p, action_25, j),
        _ => happy_fail(p, j)
    }
}

fn action_309(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_26, j),
        144 => happy_shift(p, action_27, j),
        145 => happy_shift(p, action_28, j),
        146 => happy_shift(p, action_29, j),
        147 => happy_shift(p, action_30, j),
        148 => happy_shift(p, action_31, j),
        149 => happy_shift(p, action_32, j),
        150 => happy_shift(p, action_33, j),
        153 => happy_shift(p, action_34, j),
        164 => happy_shift(p, action_35, j),
        184 => happy_shift(p, action_36, j),
        205 => happy_shift(p, action_37, j),
        221 => happy_shift(p, action_38, j),
        234 => happy_shift(p, action_39, j),
        235 => happy_shift(p, action_40, j),
        236 => happy_shift(p, action_41, j),
        237 => happy_shift(p, action_42, j),
        238 => happy_shift(p, action_43, j),
        241 => happy_shift(p, action_44, j),
        242 => happy_shift(p, action_45, j),
        243 => happy_shift(p, action_46, j),
        244 => happy_shift(p, action_47, j),
        245 => happy_shift(p, action_48, j),
        246 => happy_shift(p, action_49, j),
        247 => happy_shift(p, action_50, j),
        100 => happy_goto(p, action_6, j),
        104 => happy_goto(p, action_7, j),
        106 => happy_goto(p, action_261, j),
        107 => happy_goto(p, action_9, j),
        108 => happy_goto(p, action_10, j),
        109 => happy_goto(p, action_11, j),
        110 => happy_goto(p, action_12, j),
        111 => happy_goto(p, action_352, j),
        127 => happy_goto(p, action_24, j),
        128 => happy_goto(p, action_25, j),
        _ => happy_fail(p, j)
    }
}

fn action_310(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_26, j),
        144 => happy_shift(p, action_27, j),
        145 => happy_shift(p, action_28, j),
        146 => happy_shift(p, action_29, j),
        147 => happy_shift(p, action_30, j),
        148 => happy_shift(p, action_31, j),
        149 => happy_shift(p, action_32, j),
        150 => happy_shift(p, action_33, j),
        153 => happy_shift(p, action_34, j),
        164 => happy_shift(p, action_35, j),
        184 => happy_shift(p, action_36, j),
        205 => happy_shift(p, action_37, j),
        221 => happy_shift(p, action_38, j),
        234 => happy_shift(p, action_39, j),
        235 => happy_shift(p, action_40, j),
        236 => happy_shift(p, action_41, j),
        237 => happy_shift(p, action_42, j),
        238 => happy_shift(p, action_43, j),
        241 => happy_shift(p, action_44, j),
        242 => happy_shift(p, action_45, j),
        243 => happy_shift(p, action_46, j),
        244 => happy_shift(p, action_47, j),
        245 => happy_shift(p, action_48, j),
        246 => happy_shift(p, action_49, j),
        247 => happy_shift(p, action_50, j),
        100 => happy_goto(p, action_6, j),
        104 => happy_goto(p, action_7, j),
        106 => happy_goto(p, action_261, j),
        107 => happy_goto(p, action_9, j),
        108 => happy_goto(p, action_10, j),
        109 => happy_goto(p, action_11, j),
        110 => happy_goto(p, action_351, j),
        127 => happy_goto(p, action_24, j),
        128 => happy_goto(p, action_25, j),
        _ => happy_fail(p, j)
    }
}

fn action_311(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_26, j),
        144 => happy_shift(p, action_27, j),
        145 => happy_shift(p, action_28, j),
        146 => happy_shift(p, action_29, j),
        147 => happy_shift(p, action_30, j),
        148 => happy_shift(p, action_31, j),
        149 => happy_shift(p, action_32, j),
        150 => happy_shift(p, action_33, j),
        153 => happy_shift(p, action_34, j),
        164 => happy_shift(p, action_35, j),
        184 => happy_shift(p, action_36, j),
        205 => happy_shift(p, action_37, j),
        221 => happy_shift(p, action_38, j),
        234 => happy_shift(p, action_39, j),
        235 => happy_shift(p, action_40, j),
        236 => happy_shift(p, action_41, j),
        237 => happy_shift(p, action_42, j),
        238 => happy_shift(p, action_43, j),
        241 => happy_shift(p, action_44, j),
        242 => happy_shift(p, action_45, j),
        243 => happy_shift(p, action_46, j),
        244 => happy_shift(p, action_47, j),
        245 => happy_shift(p, action_48, j),
        246 => happy_shift(p, action_49, j),
        247 => happy_shift(p, action_50, j),
        100 => happy_goto(p, action_6, j),
        104 => happy_goto(p, action_7, j),
        106 => happy_goto(p, action_261, j),
        107 => happy_goto(p, action_9, j),
        108 => happy_goto(p, action_10, j),
        109 => happy_goto(p, action_11, j),
        110 => happy_goto(p, action_350, j),
        127 => happy_goto(p, action_24, j),
        128 => happy_goto(p, action_25, j),
        _ => happy_fail(p, j)
    }
}

fn action_312(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_26, j),
        144 => happy_shift(p, action_27, j),
        145 => happy_shift(p, action_28, j),
        146 => happy_shift(p, action_29, j),
        147 => happy_shift(p, action_30, j),
        148 => happy_shift(p, action_31, j),
        149 => happy_shift(p, action_32, j),
        150 => happy_shift(p, action_33, j),
        153 => happy_shift(p, action_34, j),
        164 => happy_shift(p, action_35, j),
        184 => happy_shift(p, action_36, j),
        205 => happy_shift(p, action_37, j),
        221 => happy_shift(p, action_38, j),
        234 => happy_shift(p, action_39, j),
        235 => happy_shift(p, action_40, j),
        236 => happy_shift(p, action_41, j),
        237 => happy_shift(p, action_42, j),
        238 => happy_shift(p, action_43, j),
        241 => happy_shift(p, action_44, j),
        242 => happy_shift(p, action_45, j),
        243 => happy_shift(p, action_46, j),
        244 => happy_shift(p, action_47, j),
        245 => happy_shift(p, action_48, j),
        246 => happy_shift(p, action_49, j),
        247 => happy_shift(p, action_50, j),
        100 => happy_goto(p, action_6, j),
        104 => happy_goto(p, action_7, j),
        106 => happy_goto(p, action_261, j),
        107 => happy_goto(p, action_9, j),
        108 => happy_goto(p, action_10, j),
        109 => happy_goto(p, action_349, j),
        127 => happy_goto(p, action_24, j),
        128 => happy_goto(p, action_25, j),
        _ => happy_fail(p, j)
    }
}

fn action_313(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_26, j),
        144 => happy_shift(p, action_27, j),
        145 => happy_shift(p, action_28, j),
        146 => happy_shift(p, action_29, j),
        147 => happy_shift(p, action_30, j),
        148 => happy_shift(p, action_31, j),
        149 => happy_shift(p, action_32, j),
        150 => happy_shift(p, action_33, j),
        153 => happy_shift(p, action_34, j),
        164 => happy_shift(p, action_35, j),
        184 => happy_shift(p, action_36, j),
        205 => happy_shift(p, action_37, j),
        221 => happy_shift(p, action_38, j),
        234 => happy_shift(p, action_39, j),
        235 => happy_shift(p, action_40, j),
        236 => happy_shift(p, action_41, j),
        237 => happy_shift(p, action_42, j),
        238 => happy_shift(p, action_43, j),
        241 => happy_shift(p, action_44, j),
        242 => happy_shift(p, action_45, j),
        243 => happy_shift(p, action_46, j),
        244 => happy_shift(p, action_47, j),
        245 => happy_shift(p, action_48, j),
        246 => happy_shift(p, action_49, j),
        247 => happy_shift(p, action_50, j),
        100 => happy_goto(p, action_6, j),
        104 => happy_goto(p, action_7, j),
        106 => happy_goto(p, action_261, j),
        107 => happy_goto(p, action_9, j),
        108 => happy_goto(p, action_10, j),
        109 => happy_goto(p, action_348, j),
        127 => happy_goto(p, action_24, j),
        128 => happy_goto(p, action_25, j),
        _ => happy_fail(p, j)
    }
}

fn action_314(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_26, j),
        144 => happy_shift(p, action_27, j),
        145 => happy_shift(p, action_28, j),
        146 => happy_shift(p, action_29, j),
        147 => happy_shift(p, action_30, j),
        148 => happy_shift(p, action_31, j),
        149 => happy_shift(p, action_32, j),
        150 => happy_shift(p, action_33, j),
        153 => happy_shift(p, action_34, j),
        164 => happy_shift(p, action_35, j),
        184 => happy_shift(p, action_36, j),
        205 => happy_shift(p, action_37, j),
        221 => happy_shift(p, action_38, j),
        234 => happy_shift(p, action_39, j),
        235 => happy_shift(p, action_40, j),
        236 => happy_shift(p, action_41, j),
        237 => happy_shift(p, action_42, j),
        238 => happy_shift(p, action_43, j),
        241 => happy_shift(p, action_44, j),
        242 => happy_shift(p, action_45, j),
        243 => happy_shift(p, action_46, j),
        244 => happy_shift(p, action_47, j),
        245 => happy_shift(p, action_48, j),
        246 => happy_shift(p, action_49, j),
        247 => happy_shift(p, action_50, j),
        100 => happy_goto(p, action_6, j),
        104 => happy_goto(p, action_7, j),
        106 => happy_goto(p, action_261, j),
        107 => happy_goto(p, action_9, j),
        108 => happy_goto(p, action_347, j),
        127 => happy_goto(p, action_24, j),
        128 => happy_goto(p, action_25, j),
        _ => happy_fail(p, j)
    }
}

fn action_315(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_26, j),
        144 => happy_shift(p, action_27, j),
        145 => happy_shift(p, action_28, j),
        146 => happy_shift(p, action_29, j),
        147 => happy_shift(p, action_30, j),
        148 => happy_shift(p, action_31, j),
        149 => happy_shift(p, action_32, j),
        150 => happy_shift(p, action_33, j),
        153 => happy_shift(p, action_34, j),
        164 => happy_shift(p, action_35, j),
        184 => happy_shift(p, action_36, j),
        205 => happy_shift(p, action_37, j),
        221 => happy_shift(p, action_38, j),
        234 => happy_shift(p, action_39, j),
        235 => happy_shift(p, action_40, j),
        236 => happy_shift(p, action_41, j),
        237 => happy_shift(p, action_42, j),
        238 => happy_shift(p, action_43, j),
        241 => happy_shift(p, action_44, j),
        242 => happy_shift(p, action_45, j),
        243 => happy_shift(p, action_46, j),
        244 => happy_shift(p, action_47, j),
        245 => happy_shift(p, action_48, j),
        246 => happy_shift(p, action_49, j),
        247 => happy_shift(p, action_50, j),
        100 => happy_goto(p, action_6, j),
        104 => happy_goto(p, action_7, j),
        106 => happy_goto(p, action_261, j),
        107 => happy_goto(p, action_9, j),
        108 => happy_goto(p, action_346, j),
        127 => happy_goto(p, action_24, j),
        128 => happy_goto(p, action_25, j),
        _ => happy_fail(p, j)
    }
}

fn action_316(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_26, j),
        144 => happy_shift(p, action_27, j),
        145 => happy_shift(p, action_28, j),
        146 => happy_shift(p, action_29, j),
        147 => happy_shift(p, action_30, j),
        148 => happy_shift(p, action_31, j),
        149 => happy_shift(p, action_32, j),
        150 => happy_shift(p, action_33, j),
        153 => happy_shift(p, action_34, j),
        164 => happy_shift(p, action_35, j),
        184 => happy_shift(p, action_36, j),
        205 => happy_shift(p, action_37, j),
        221 => happy_shift(p, action_38, j),
        234 => happy_shift(p, action_39, j),
        235 => happy_shift(p, action_40, j),
        236 => happy_shift(p, action_41, j),
        237 => happy_shift(p, action_42, j),
        238 => happy_shift(p, action_43, j),
        241 => happy_shift(p, action_44, j),
        242 => happy_shift(p, action_45, j),
        243 => happy_shift(p, action_46, j),
        244 => happy_shift(p, action_47, j),
        245 => happy_shift(p, action_48, j),
        246 => happy_shift(p, action_49, j),
        247 => happy_shift(p, action_50, j),
        100 => happy_goto(p, action_6, j),
        104 => happy_goto(p, action_7, j),
        106 => happy_goto(p, action_261, j),
        107 => happy_goto(p, action_9, j),
        108 => happy_goto(p, action_345, j),
        127 => happy_goto(p, action_24, j),
        128 => happy_goto(p, action_25, j),
        _ => happy_fail(p, j)
    }
}

fn action_317(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_394(p, j)
    }
}

fn action_318(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_26, j),
        144 => happy_shift(p, action_27, j),
        145 => happy_shift(p, action_28, j),
        146 => happy_shift(p, action_29, j),
        147 => happy_shift(p, action_30, j),
        148 => happy_shift(p, action_31, j),
        149 => happy_shift(p, action_32, j),
        150 => happy_shift(p, action_33, j),
        153 => happy_shift(p, action_34, j),
        164 => happy_shift(p, action_35, j),
        184 => happy_shift(p, action_36, j),
        205 => happy_shift(p, action_37, j),
        221 => happy_shift(p, action_38, j),
        234 => happy_shift(p, action_39, j),
        235 => happy_shift(p, action_40, j),
        236 => happy_shift(p, action_41, j),
        237 => happy_shift(p, action_42, j),
        238 => happy_shift(p, action_43, j),
        241 => happy_shift(p, action_44, j),
        242 => happy_shift(p, action_45, j),
        243 => happy_shift(p, action_46, j),
        244 => happy_shift(p, action_47, j),
        245 => happy_shift(p, action_48, j),
        246 => happy_shift(p, action_49, j),
        247 => happy_shift(p, action_50, j),
        100 => happy_goto(p, action_6, j),
        104 => happy_goto(p, action_7, j),
        106 => happy_goto(p, action_8, j),
        107 => happy_goto(p, action_9, j),
        108 => happy_goto(p, action_10, j),
        109 => happy_goto(p, action_11, j),
        110 => happy_goto(p, action_12, j),
        111 => happy_goto(p, action_13, j),
        112 => happy_goto(p, action_14, j),
        113 => happy_goto(p, action_15, j),
        114 => happy_goto(p, action_16, j),
        115 => happy_goto(p, action_17, j),
        116 => happy_goto(p, action_18, j),
        117 => happy_goto(p, action_19, j),
        118 => happy_goto(p, action_20, j),
        119 => happy_goto(p, action_21, j),
        120 => happy_goto(p, action_344, j),
        127 => happy_goto(p, action_24, j),
        128 => happy_goto(p, action_25, j),
        _ => happy_fail(p, j)
    }
}

fn action_319(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_443(p, j)
    }
}

fn action_320(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_447(p, j)
    }
}

fn action_321(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_448(p, j)
    }
}

fn action_322(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_444(p, j)
    }
}

fn action_323(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_445(p, j)
    }
}

fn action_324(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_446(p, j)
    }
}

fn action_325(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_451(p, j)
    }
}

fn action_326(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_452(p, j)
    }
}

fn action_327(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_453(p, j)
    }
}

fn action_328(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_449(p, j)
    }
}

fn action_329(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_450(p, j)
    }
}

fn action_330(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_26, j),
        139 => happy_shift(p, action_343, j),
        144 => happy_shift(p, action_27, j),
        145 => happy_shift(p, action_28, j),
        146 => happy_shift(p, action_29, j),
        147 => happy_shift(p, action_30, j),
        148 => happy_shift(p, action_31, j),
        149 => happy_shift(p, action_32, j),
        150 => happy_shift(p, action_33, j),
        153 => happy_shift(p, action_34, j),
        164 => happy_shift(p, action_35, j),
        184 => happy_shift(p, action_36, j),
        205 => happy_shift(p, action_37, j),
        221 => happy_shift(p, action_38, j),
        234 => happy_shift(p, action_39, j),
        235 => happy_shift(p, action_40, j),
        236 => happy_shift(p, action_41, j),
        237 => happy_shift(p, action_42, j),
        238 => happy_shift(p, action_43, j),
        241 => happy_shift(p, action_44, j),
        242 => happy_shift(p, action_45, j),
        243 => happy_shift(p, action_46, j),
        244 => happy_shift(p, action_47, j),
        245 => happy_shift(p, action_48, j),
        246 => happy_shift(p, action_49, j),
        247 => happy_shift(p, action_50, j),
        100 => happy_goto(p, action_6, j),
        104 => happy_goto(p, action_7, j),
        105 => happy_goto(p, action_341, j),
        106 => happy_goto(p, action_8, j),
        107 => happy_goto(p, action_9, j),
        108 => happy_goto(p, action_10, j),
        109 => happy_goto(p, action_11, j),
        110 => happy_goto(p, action_12, j),
        111 => happy_goto(p, action_13, j),
        112 => happy_goto(p, action_14, j),
        113 => happy_goto(p, action_15, j),
        114 => happy_goto(p, action_16, j),
        115 => happy_goto(p, action_17, j),
        116 => happy_goto(p, action_18, j),
        117 => happy_goto(p, action_19, j),
        118 => happy_goto(p, action_20, j),
        119 => happy_goto(p, action_21, j),
        120 => happy_goto(p, action_342, j),
        127 => happy_goto(p, action_24, j),
        128 => happy_goto(p, action_25, j),
        _ => happy_fail(p, j)
    }
}

fn action_331(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_26, j),
        144 => happy_shift(p, action_27, j),
        145 => happy_shift(p, action_28, j),
        146 => happy_shift(p, action_29, j),
        147 => happy_shift(p, action_30, j),
        148 => happy_shift(p, action_31, j),
        149 => happy_shift(p, action_32, j),
        150 => happy_shift(p, action_33, j),
        153 => happy_shift(p, action_34, j),
        164 => happy_shift(p, action_35, j),
        184 => happy_shift(p, action_36, j),
        205 => happy_shift(p, action_37, j),
        221 => happy_shift(p, action_38, j),
        234 => happy_shift(p, action_39, j),
        235 => happy_shift(p, action_40, j),
        236 => happy_shift(p, action_41, j),
        237 => happy_shift(p, action_42, j),
        238 => happy_shift(p, action_43, j),
        241 => happy_shift(p, action_44, j),
        242 => happy_shift(p, action_45, j),
        243 => happy_shift(p, action_46, j),
        244 => happy_shift(p, action_47, j),
        245 => happy_shift(p, action_48, j),
        246 => happy_shift(p, action_49, j),
        247 => happy_shift(p, action_50, j),
        100 => happy_goto(p, action_6, j),
        104 => happy_goto(p, action_7, j),
        106 => happy_goto(p, action_8, j),
        107 => happy_goto(p, action_9, j),
        108 => happy_goto(p, action_10, j),
        109 => happy_goto(p, action_11, j),
        110 => happy_goto(p, action_12, j),
        111 => happy_goto(p, action_13, j),
        112 => happy_goto(p, action_14, j),
        113 => happy_goto(p, action_15, j),
        114 => happy_goto(p, action_16, j),
        115 => happy_goto(p, action_17, j),
        116 => happy_goto(p, action_18, j),
        117 => happy_goto(p, action_19, j),
        118 => happy_goto(p, action_20, j),
        119 => happy_goto(p, action_21, j),
        120 => happy_goto(p, action_22, j),
        122 => happy_goto(p, action_340, j),
        127 => happy_goto(p, action_24, j),
        128 => happy_goto(p, action_25, j),
        _ => happy_fail(p, j)
    }
}

fn action_332(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        238 => happy_shift(p, action_256, j),
        239 => happy_shift(p, action_76, j),
        131 => happy_goto(p, action_339, j),
        _ => happy_fail(p, j)
    }
}

fn action_333(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        238 => happy_shift(p, action_256, j),
        239 => happy_shift(p, action_76, j),
        131 => happy_goto(p, action_338, j),
        _ => happy_fail(p, j)
    }
}

fn action_334(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_384(p, j)
    }
}

fn action_335(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_385(p, j)
    }
}

fn action_336(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_7(p, j)
    }
}

fn action_337(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_6(p, j)
    }
}

fn action_338(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_382(p, j)
    }
}

fn action_339(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_383(p, j)
    }
}

fn action_340(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        141 => happy_shift(p, action_645, j),
        _ => happy_fail(p, j)
    }
}

fn action_341(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        139 => happy_shift(p, action_643, j),
        179 => happy_shift(p, action_644, j),
        _ => happy_fail(p, j)
    }
}

fn action_342(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_388(p, j)
    }
}

fn action_343(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_380(p, j)
    }
}

fn action_344(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_442(p, j)
    }
}

fn action_345(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_413(p, j)
    }
}

fn action_346(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_412(p, j)
    }
}

fn action_347(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_411(p, j)
    }
}

fn action_348(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        150 => happy_shift(p, action_314, j),
        151 => happy_shift(p, action_315, j),
        152 => happy_shift(p, action_316, j),
        _ => happy_reduce_416(p, j)
    }
}

fn action_349(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        150 => happy_shift(p, action_314, j),
        151 => happy_shift(p, action_315, j),
        152 => happy_shift(p, action_316, j),
        _ => happy_reduce_415(p, j)
    }
}

fn action_350(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        148 => happy_shift(p, action_312, j),
        149 => happy_shift(p, action_313, j),
        _ => happy_reduce_419(p, j)
    }
}

fn action_351(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        148 => happy_shift(p, action_312, j),
        149 => happy_shift(p, action_313, j),
        _ => happy_reduce_418(p, j)
    }
}

fn action_352(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        154 => happy_shift(p, action_310, j),
        155 => happy_shift(p, action_311, j),
        _ => happy_reduce_424(p, j)
    }
}

fn action_353(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        154 => happy_shift(p, action_310, j),
        155 => happy_shift(p, action_311, j),
        _ => happy_reduce_422(p, j)
    }
}

fn action_354(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        154 => happy_shift(p, action_310, j),
        155 => happy_shift(p, action_311, j),
        _ => happy_reduce_423(p, j)
    }
}

fn action_355(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        154 => happy_shift(p, action_310, j),
        155 => happy_shift(p, action_311, j),
        _ => happy_reduce_421(p, j)
    }
}

fn action_356(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        156 => happy_shift(p, action_306, j),
        157 => happy_shift(p, action_307, j),
        158 => happy_shift(p, action_308, j),
        159 => happy_shift(p, action_309, j),
        _ => happy_reduce_427(p, j)
    }
}

fn action_357(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        156 => happy_shift(p, action_306, j),
        157 => happy_shift(p, action_307, j),
        158 => happy_shift(p, action_308, j),
        159 => happy_shift(p, action_309, j),
        _ => happy_reduce_426(p, j)
    }
}

fn action_358(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        160 => happy_shift(p, action_304, j),
        161 => happy_shift(p, action_305, j),
        _ => happy_reduce_429(p, j)
    }
}

fn action_359(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        153 => happy_shift(p, action_303, j),
        _ => happy_reduce_431(p, j)
    }
}

fn action_360(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        162 => happy_shift(p, action_302, j),
        _ => happy_reduce_433(p, j)
    }
}

fn action_361(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        163 => happy_shift(p, action_301, j),
        _ => happy_reduce_435(p, j)
    }
}

fn action_362(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        167 => happy_shift(p, action_642, j),
        _ => happy_fail(p, j)
    }
}

fn action_363(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_26, j),
        144 => happy_shift(p, action_27, j),
        145 => happy_shift(p, action_28, j),
        146 => happy_shift(p, action_29, j),
        147 => happy_shift(p, action_30, j),
        148 => happy_shift(p, action_31, j),
        149 => happy_shift(p, action_32, j),
        150 => happy_shift(p, action_33, j),
        153 => happy_shift(p, action_34, j),
        164 => happy_shift(p, action_35, j),
        184 => happy_shift(p, action_36, j),
        205 => happy_shift(p, action_37, j),
        221 => happy_shift(p, action_38, j),
        234 => happy_shift(p, action_39, j),
        235 => happy_shift(p, action_40, j),
        236 => happy_shift(p, action_41, j),
        237 => happy_shift(p, action_42, j),
        238 => happy_shift(p, action_43, j),
        241 => happy_shift(p, action_44, j),
        242 => happy_shift(p, action_45, j),
        243 => happy_shift(p, action_46, j),
        244 => happy_shift(p, action_47, j),
        245 => happy_shift(p, action_48, j),
        246 => happy_shift(p, action_49, j),
        247 => happy_shift(p, action_50, j),
        100 => happy_goto(p, action_6, j),
        104 => happy_goto(p, action_7, j),
        106 => happy_goto(p, action_261, j),
        107 => happy_goto(p, action_9, j),
        108 => happy_goto(p, action_10, j),
        109 => happy_goto(p, action_11, j),
        110 => happy_goto(p, action_12, j),
        111 => happy_goto(p, action_13, j),
        112 => happy_goto(p, action_14, j),
        113 => happy_goto(p, action_15, j),
        114 => happy_goto(p, action_16, j),
        115 => happy_goto(p, action_17, j),
        116 => happy_goto(p, action_18, j),
        117 => happy_goto(p, action_19, j),
        118 => happy_goto(p, action_20, j),
        119 => happy_goto(p, action_641, j),
        127 => happy_goto(p, action_24, j),
        128 => happy_goto(p, action_25, j),
        _ => happy_fail(p, j)
    }
}

fn action_364(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        164 => happy_shift(p, action_300, j),
        _ => happy_reduce_437(p, j)
    }
}

fn action_365(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_456(p, j)
    }
}

fn action_366(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        179 => happy_shift(p, action_640, j),
        _ => happy_reduce_455(p, j)
    }
}

fn action_367(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_364(p, j)
    }
}

fn action_368(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_26, j),
        144 => happy_shift(p, action_27, j),
        145 => happy_shift(p, action_28, j),
        146 => happy_shift(p, action_29, j),
        147 => happy_shift(p, action_30, j),
        148 => happy_shift(p, action_31, j),
        149 => happy_shift(p, action_32, j),
        150 => happy_shift(p, action_33, j),
        153 => happy_shift(p, action_34, j),
        164 => happy_shift(p, action_35, j),
        181 => happy_shift(p, action_639, j),
        184 => happy_shift(p, action_36, j),
        205 => happy_shift(p, action_37, j),
        221 => happy_shift(p, action_38, j),
        234 => happy_shift(p, action_39, j),
        235 => happy_shift(p, action_40, j),
        236 => happy_shift(p, action_41, j),
        237 => happy_shift(p, action_42, j),
        238 => happy_shift(p, action_43, j),
        241 => happy_shift(p, action_44, j),
        242 => happy_shift(p, action_45, j),
        243 => happy_shift(p, action_46, j),
        244 => happy_shift(p, action_47, j),
        245 => happy_shift(p, action_48, j),
        246 => happy_shift(p, action_49, j),
        247 => happy_shift(p, action_50, j),
        100 => happy_goto(p, action_6, j),
        104 => happy_goto(p, action_7, j),
        106 => happy_goto(p, action_261, j),
        107 => happy_goto(p, action_9, j),
        108 => happy_goto(p, action_638, j),
        127 => happy_goto(p, action_24, j),
        128 => happy_goto(p, action_25, j),
        _ => happy_fail(p, j)
    }
}

fn action_369(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_308(p, j)
    }
}

fn action_370(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_311(p, j)
    }
}

fn action_371(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_309(p, j)
    }
}

fn action_372(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        240 => happy_shift(p, action_144, j),
        134 => happy_goto(p, action_637, j),
        _ => happy_reduce_310(p, j)
    }
}

fn action_373(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        186 => happy_shift(p, action_173, j),
        190 => happy_shift(p, action_117, j),
        192 => happy_shift(p, action_118, j),
        193 => happy_shift(p, action_174, j),
        195 => happy_shift(p, action_119, j),
        198 => happy_shift(p, action_120, j),
        200 => happy_shift(p, action_121, j),
        202 => happy_shift(p, action_123, j),
        203 => happy_shift(p, action_124, j),
        209 => happy_shift(p, action_126, j),
        210 => happy_shift(p, action_127, j),
        211 => happy_shift(p, action_128, j),
        214 => happy_shift(p, action_175, j),
        215 => happy_shift(p, action_176, j),
        217 => happy_shift(p, action_177, j),
        219 => happy_shift(p, action_131, j),
        220 => happy_shift(p, action_132, j),
        224 => happy_shift(p, action_135, j),
        227 => happy_shift(p, action_442, j),
        229 => happy_shift(p, action_139, j),
        230 => happy_shift(p, action_140, j),
        231 => happy_shift(p, action_141, j),
        232 => happy_shift(p, action_178, j),
        239 => happy_shift(p, action_443, j),
        240 => happy_shift(p, action_144, j),
        45 => happy_goto(p, action_437, j),
        52 => happy_goto(p, action_438, j),
        53 => happy_goto(p, action_98, j),
        54 => happy_goto(p, action_99, j),
        61 => happy_goto(p, action_100, j),
        64 => happy_goto(p, action_439, j),
        134 => happy_goto(p, action_169, j),
        _ => happy_fail(p, j)
    }
}

fn action_374(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        186 => happy_reduce_475(p, j),
        190 => happy_reduce_475(p, j),
        192 => happy_reduce_475(p, j),
        193 => happy_reduce_475(p, j),
        195 => happy_reduce_475(p, j),
        198 => happy_reduce_475(p, j),
        200 => happy_reduce_475(p, j),
        202 => happy_reduce_475(p, j),
        203 => happy_reduce_475(p, j),
        209 => happy_reduce_475(p, j),
        210 => happy_reduce_475(p, j),
        211 => happy_reduce_475(p, j),
        214 => happy_reduce_475(p, j),
        215 => happy_reduce_475(p, j),
        217 => happy_reduce_475(p, j),
        219 => happy_reduce_475(p, j),
        220 => happy_reduce_475(p, j),
        224 => happy_reduce_475(p, j),
        227 => happy_reduce_475(p, j),
        229 => happy_reduce_475(p, j),
        230 => happy_reduce_475(p, j),
        231 => happy_reduce_475(p, j),
        232 => happy_reduce_475(p, j),
        239 => happy_reduce_475(p, j),
        240 => happy_reduce_475(p, j),
        _ => happy_reduce_307(p, j)
    }
}

fn action_375(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_375, j),
        140 => happy_shift(p, action_184, j),
        150 => happy_shift(p, action_376, j),
        185 => happy_shift(p, action_114, j),
        186 => happy_reduce_473(p, j),
        188 => happy_shift(p, action_116, j),
        190 => happy_shift(p, action_117, j),
        192 => happy_shift(p, action_118, j),
        193 => happy_reduce_473(p, j),
        195 => happy_shift(p, action_119, j),
        198 => happy_shift(p, action_120, j),
        200 => happy_shift(p, action_121, j),
        201 => happy_shift(p, action_122, j),
        202 => happy_shift(p, action_123, j),
        203 => happy_shift(p, action_124, j),
        208 => happy_shift(p, action_125, j),
        209 => happy_shift(p, action_126, j),
        210 => happy_shift(p, action_127, j),
        211 => happy_shift(p, action_128, j),
        213 => happy_shift(p, action_129, j),
        214 => happy_reduce_473(p, j),
        215 => happy_reduce_473(p, j),
        216 => happy_shift(p, action_130, j),
        217 => happy_reduce_473(p, j),
        219 => happy_shift(p, action_131, j),
        220 => happy_shift(p, action_132, j),
        222 => happy_shift(p, action_133, j),
        224 => happy_shift(p, action_135, j),
        226 => happy_shift(p, action_136, j),
        227 => happy_shift(p, action_137, j),
        228 => happy_shift(p, action_138, j),
        229 => happy_shift(p, action_139, j),
        230 => happy_shift(p, action_140, j),
        231 => happy_shift(p, action_141, j),
        232 => happy_reduce_473(p, j),
        239 => happy_shift(p, action_143, j),
        240 => happy_shift(p, action_144, j),
        37 => happy_goto(p, action_453, j),
        38 => happy_goto(p, action_454, j),
        40 => happy_goto(p, action_85, j),
        41 => happy_goto(p, action_86, j),
        42 => happy_goto(p, action_87, j),
        43 => happy_goto(p, action_88, j),
        44 => happy_goto(p, action_455, j),
        45 => happy_goto(p, action_90, j),
        46 => happy_goto(p, action_91, j),
        47 => happy_goto(p, action_92, j),
        48 => happy_goto(p, action_456, j),
        49 => happy_goto(p, action_457, j),
        50 => happy_goto(p, action_95, j),
        51 => happy_goto(p, action_96, j),
        52 => happy_goto(p, action_97, j),
        53 => happy_goto(p, action_98, j),
        54 => happy_goto(p, action_99, j),
        61 => happy_goto(p, action_100, j),
        65 => happy_goto(p, action_458, j),
        82 => happy_goto(p, action_459, j),
        83 => happy_goto(p, action_460, j),
        84 => happy_goto(p, action_461, j),
        88 => happy_goto(p, action_633, j),
        89 => happy_goto(p, action_181, j),
        90 => happy_goto(p, action_182, j),
        91 => happy_goto(p, action_634, j),
        92 => happy_goto(p, action_635, j),
        132 => happy_goto(p, action_109, j),
        133 => happy_goto(p, action_636, j),
        134 => happy_goto(p, action_111, j),
        _ => happy_reduce_283(p, j)
    }
}

fn action_376(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_375, j),
        140 => happy_shift(p, action_184, j),
        150 => happy_shift(p, action_376, j),
        186 => happy_reduce_473(p, j),
        193 => happy_reduce_473(p, j),
        214 => happy_reduce_473(p, j),
        215 => happy_reduce_473(p, j),
        217 => happy_reduce_473(p, j),
        232 => happy_reduce_473(p, j),
        240 => happy_shift(p, action_144, j),
        65 => happy_goto(p, action_630, j),
        87 => happy_goto(p, action_631, j),
        88 => happy_goto(p, action_370, j),
        89 => happy_goto(p, action_181, j),
        90 => happy_goto(p, action_182, j),
        91 => happy_goto(p, action_371, j),
        92 => happy_goto(p, action_372, j),
        132 => happy_goto(p, action_109, j),
        133 => happy_goto(p, action_632, j),
        134 => happy_goto(p, action_111, j),
        _ => happy_reduce_327(p, j)
    }
}

fn action_377(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_306(p, j)
    }
}

fn action_378(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_366(p, j)
    }
}

fn action_379(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        139 => happy_shift(p, action_629, j),
        _ => happy_fail(p, j)
    }
}

fn action_380(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        179 => happy_shift(p, action_628, j),
        _ => happy_fail(p, j)
    }
}

fn action_381(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        139 => happy_shift(p, action_627, j),
        _ => happy_fail(p, j)
    }
}

fn action_382(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_469(p, j)
    }
}

fn action_383(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        139 => happy_shift(p, action_626, j),
        _ => happy_fail(p, j)
    }
}

fn action_384(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        179 => happy_shift(p, action_625, j),
        _ => happy_fail(p, j)
    }
}

fn action_385(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        179 => happy_shift(p, action_624, j),
        _ => happy_fail(p, j)
    }
}

fn action_386(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        179 => happy_shift(p, action_623, j),
        _ => happy_fail(p, j)
    }
}

fn action_387(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        179 => happy_shift(p, action_622, j),
        _ => happy_fail(p, j)
    }
}

fn action_388(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_26, j),
        144 => happy_shift(p, action_27, j),
        145 => happy_shift(p, action_28, j),
        146 => happy_shift(p, action_29, j),
        147 => happy_shift(p, action_30, j),
        148 => happy_shift(p, action_31, j),
        149 => happy_shift(p, action_32, j),
        150 => happy_shift(p, action_33, j),
        153 => happy_shift(p, action_34, j),
        164 => happy_shift(p, action_35, j),
        180 => happy_shift(p, action_61, j),
        181 => happy_shift(p, action_62, j),
        184 => happy_shift(p, action_36, j),
        187 => happy_shift(p, action_63, j),
        189 => happy_shift(p, action_64, j),
        191 => happy_shift(p, action_65, j),
        194 => happy_shift(p, action_66, j),
        196 => happy_shift(p, action_67, j),
        197 => happy_shift(p, action_68, j),
        204 => happy_shift(p, action_69, j),
        205 => happy_shift(p, action_37, j),
        206 => happy_shift(p, action_70, j),
        207 => happy_shift(p, action_71, j),
        218 => happy_shift(p, action_72, j),
        221 => happy_shift(p, action_38, j),
        225 => happy_shift(p, action_73, j),
        233 => happy_shift(p, action_74, j),
        234 => happy_shift(p, action_39, j),
        235 => happy_shift(p, action_40, j),
        236 => happy_shift(p, action_41, j),
        237 => happy_shift(p, action_42, j),
        238 => happy_shift(p, action_75, j),
        239 => happy_shift(p, action_76, j),
        241 => happy_shift(p, action_44, j),
        242 => happy_shift(p, action_45, j),
        243 => happy_shift(p, action_46, j),
        244 => happy_shift(p, action_47, j),
        245 => happy_shift(p, action_48, j),
        246 => happy_shift(p, action_49, j),
        247 => happy_shift(p, action_50, j),
        12 => happy_goto(p, action_621, j),
        13 => happy_goto(p, action_52, j),
        14 => happy_goto(p, action_53, j),
        22 => happy_goto(p, action_54, j),
        23 => happy_goto(p, action_55, j),
        24 => happy_goto(p, action_56, j),
        25 => happy_goto(p, action_57, j),
        26 => happy_goto(p, action_58, j),
        100 => happy_goto(p, action_6, j),
        104 => happy_goto(p, action_7, j),
        106 => happy_goto(p, action_8, j),
        107 => happy_goto(p, action_9, j),
        108 => happy_goto(p, action_10, j),
        109 => happy_goto(p, action_11, j),
        110 => happy_goto(p, action_12, j),
        111 => happy_goto(p, action_13, j),
        112 => happy_goto(p, action_14, j),
        113 => happy_goto(p, action_15, j),
        114 => happy_goto(p, action_16, j),
        115 => happy_goto(p, action_17, j),
        116 => happy_goto(p, action_18, j),
        117 => happy_goto(p, action_19, j),
        118 => happy_goto(p, action_20, j),
        119 => happy_goto(p, action_21, j),
        120 => happy_goto(p, action_22, j),
        122 => happy_goto(p, action_59, j),
        127 => happy_goto(p, action_24, j),
        128 => happy_goto(p, action_25, j),
        131 => happy_goto(p, action_60, j),
        _ => happy_fail(p, j)
    }
}

fn action_389(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_26, j),
        144 => happy_shift(p, action_27, j),
        145 => happy_shift(p, action_28, j),
        146 => happy_shift(p, action_29, j),
        147 => happy_shift(p, action_30, j),
        148 => happy_shift(p, action_31, j),
        149 => happy_shift(p, action_32, j),
        150 => happy_shift(p, action_33, j),
        153 => happy_shift(p, action_34, j),
        164 => happy_shift(p, action_35, j),
        180 => happy_shift(p, action_61, j),
        181 => happy_shift(p, action_62, j),
        184 => happy_shift(p, action_36, j),
        185 => happy_shift(p, action_114, j),
        186 => happy_reduce_473(p, j),
        187 => happy_shift(p, action_63, j),
        188 => happy_shift(p, action_116, j),
        189 => happy_shift(p, action_64, j),
        190 => happy_shift(p, action_117, j),
        191 => happy_shift(p, action_65, j),
        192 => happy_shift(p, action_118, j),
        193 => happy_reduce_473(p, j),
        194 => happy_shift(p, action_66, j),
        195 => happy_shift(p, action_119, j),
        196 => happy_shift(p, action_67, j),
        197 => happy_shift(p, action_68, j),
        198 => happy_shift(p, action_120, j),
        200 => happy_shift(p, action_121, j),
        201 => happy_shift(p, action_122, j),
        202 => happy_shift(p, action_123, j),
        203 => happy_shift(p, action_124, j),
        204 => happy_shift(p, action_69, j),
        205 => happy_shift(p, action_37, j),
        206 => happy_shift(p, action_70, j),
        207 => happy_shift(p, action_71, j),
        208 => happy_shift(p, action_125, j),
        209 => happy_shift(p, action_126, j),
        210 => happy_shift(p, action_127, j),
        211 => happy_shift(p, action_128, j),
        213 => happy_shift(p, action_129, j),
        214 => happy_reduce_473(p, j),
        215 => happy_reduce_473(p, j),
        216 => happy_shift(p, action_130, j),
        217 => happy_reduce_473(p, j),
        218 => happy_shift(p, action_72, j),
        219 => happy_shift(p, action_131, j),
        220 => happy_shift(p, action_132, j),
        221 => happy_shift(p, action_38, j),
        222 => happy_shift(p, action_133, j),
        223 => happy_shift(p, action_134, j),
        224 => happy_shift(p, action_135, j),
        225 => happy_shift(p, action_73, j),
        226 => happy_shift(p, action_136, j),
        227 => happy_shift(p, action_137, j),
        228 => happy_shift(p, action_138, j),
        229 => happy_shift(p, action_139, j),
        230 => happy_shift(p, action_140, j),
        231 => happy_shift(p, action_141, j),
        232 => happy_reduce_473(p, j),
        233 => happy_shift(p, action_74, j),
        234 => happy_shift(p, action_39, j),
        235 => happy_shift(p, action_40, j),
        236 => happy_shift(p, action_41, j),
        237 => happy_shift(p, action_42, j),
        238 => happy_shift(p, action_75, j),
        239 => happy_shift(p, action_619, j),
        240 => happy_shift(p, action_144, j),
        241 => happy_shift(p, action_620, j),
        242 => happy_shift(p, action_45, j),
        243 => happy_shift(p, action_46, j),
        244 => happy_shift(p, action_47, j),
        245 => happy_shift(p, action_48, j),
        246 => happy_shift(p, action_49, j),
        247 => happy_shift(p, action_50, j),
        12 => happy_goto(p, action_609, j),
        13 => happy_goto(p, action_52, j),
        14 => happy_goto(p, action_53, j),
        16 => happy_goto(p, action_610, j),
        18 => happy_goto(p, action_611, j),
        19 => happy_goto(p, action_612, j),
        20 => happy_goto(p, action_613, j),
        22 => happy_goto(p, action_54, j),
        23 => happy_goto(p, action_55, j),
        24 => happy_goto(p, action_56, j),
        25 => happy_goto(p, action_57, j),
        26 => happy_goto(p, action_58, j),
        32 => happy_goto(p, action_614, j),
        34 => happy_goto(p, action_81, j),
        36 => happy_goto(p, action_82, j),
        37 => happy_goto(p, action_615, j),
        38 => happy_goto(p, action_616, j),
        40 => happy_goto(p, action_85, j),
        41 => happy_goto(p, action_86, j),
        42 => happy_goto(p, action_87, j),
        43 => happy_goto(p, action_88, j),
        44 => happy_goto(p, action_617, j),
        45 => happy_goto(p, action_90, j),
        46 => happy_goto(p, action_91, j),
        47 => happy_goto(p, action_92, j),
        48 => happy_goto(p, action_93, j),
        49 => happy_goto(p, action_94, j),
        50 => happy_goto(p, action_95, j),
        51 => happy_goto(p, action_96, j),
        52 => happy_goto(p, action_97, j),
        53 => happy_goto(p, action_98, j),
        54 => happy_goto(p, action_99, j),
        61 => happy_goto(p, action_100, j),
        65 => happy_goto(p, action_618, j),
        100 => happy_goto(p, action_6, j),
        104 => happy_goto(p, action_7, j),
        106 => happy_goto(p, action_8, j),
        107 => happy_goto(p, action_9, j),
        108 => happy_goto(p, action_10, j),
        109 => happy_goto(p, action_11, j),
        110 => happy_goto(p, action_12, j),
        111 => happy_goto(p, action_13, j),
        112 => happy_goto(p, action_14, j),
        113 => happy_goto(p, action_15, j),
        114 => happy_goto(p, action_16, j),
        115 => happy_goto(p, action_17, j),
        116 => happy_goto(p, action_18, j),
        117 => happy_goto(p, action_19, j),
        118 => happy_goto(p, action_20, j),
        119 => happy_goto(p, action_21, j),
        120 => happy_goto(p, action_22, j),
        122 => happy_goto(p, action_59, j),
        127 => happy_goto(p, action_24, j),
        128 => happy_goto(p, action_25, j),
        131 => happy_goto(p, action_60, j),
        132 => happy_goto(p, action_109, j),
        133 => happy_goto(p, action_472, j),
        134 => happy_goto(p, action_111, j),
        _ => happy_reduce_41(p, j)
    }
}

fn action_390(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        212 => happy_shift(p, action_608, j),
        17 => happy_goto(p, action_607, j),
        _ => happy_reduce_42(p, j)
    }
}

fn action_391(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        238 => happy_shift(p, action_464, j),
        85 => happy_goto(p, action_606, j),
        _ => happy_fail(p, j)
    }
}

fn action_392(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        237 => happy_shift(p, action_42, j),
        128 => happy_goto(p, action_605, j),
        _ => happy_fail(p, j)
    }
}

fn action_393(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_26, j),
        144 => happy_shift(p, action_27, j),
        145 => happy_shift(p, action_28, j),
        146 => happy_shift(p, action_29, j),
        147 => happy_shift(p, action_30, j),
        148 => happy_shift(p, action_31, j),
        149 => happy_shift(p, action_32, j),
        150 => happy_shift(p, action_33, j),
        153 => happy_shift(p, action_34, j),
        164 => happy_shift(p, action_35, j),
        180 => happy_shift(p, action_61, j),
        181 => happy_shift(p, action_62, j),
        184 => happy_shift(p, action_36, j),
        187 => happy_shift(p, action_63, j),
        189 => happy_shift(p, action_64, j),
        191 => happy_shift(p, action_65, j),
        194 => happy_shift(p, action_66, j),
        196 => happy_shift(p, action_67, j),
        197 => happy_shift(p, action_68, j),
        204 => happy_shift(p, action_69, j),
        205 => happy_shift(p, action_37, j),
        206 => happy_shift(p, action_70, j),
        207 => happy_shift(p, action_71, j),
        218 => happy_shift(p, action_72, j),
        221 => happy_shift(p, action_38, j),
        225 => happy_shift(p, action_73, j),
        233 => happy_shift(p, action_74, j),
        234 => happy_shift(p, action_39, j),
        235 => happy_shift(p, action_40, j),
        236 => happy_shift(p, action_41, j),
        237 => happy_shift(p, action_42, j),
        238 => happy_shift(p, action_75, j),
        239 => happy_shift(p, action_76, j),
        241 => happy_shift(p, action_44, j),
        242 => happy_shift(p, action_45, j),
        243 => happy_shift(p, action_46, j),
        244 => happy_shift(p, action_47, j),
        245 => happy_shift(p, action_48, j),
        246 => happy_shift(p, action_49, j),
        247 => happy_shift(p, action_50, j),
        12 => happy_goto(p, action_604, j),
        13 => happy_goto(p, action_52, j),
        14 => happy_goto(p, action_53, j),
        22 => happy_goto(p, action_54, j),
        23 => happy_goto(p, action_55, j),
        24 => happy_goto(p, action_56, j),
        25 => happy_goto(p, action_57, j),
        26 => happy_goto(p, action_58, j),
        100 => happy_goto(p, action_6, j),
        104 => happy_goto(p, action_7, j),
        106 => happy_goto(p, action_8, j),
        107 => happy_goto(p, action_9, j),
        108 => happy_goto(p, action_10, j),
        109 => happy_goto(p, action_11, j),
        110 => happy_goto(p, action_12, j),
        111 => happy_goto(p, action_13, j),
        112 => happy_goto(p, action_14, j),
        113 => happy_goto(p, action_15, j),
        114 => happy_goto(p, action_16, j),
        115 => happy_goto(p, action_17, j),
        116 => happy_goto(p, action_18, j),
        117 => happy_goto(p, action_19, j),
        118 => happy_goto(p, action_20, j),
        119 => happy_goto(p, action_21, j),
        120 => happy_goto(p, action_22, j),
        122 => happy_goto(p, action_59, j),
        127 => happy_goto(p, action_24, j),
        128 => happy_goto(p, action_25, j),
        131 => happy_goto(p, action_60, j),
        _ => happy_fail(p, j)
    }
}

fn action_394(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_26, j),
        144 => happy_shift(p, action_27, j),
        145 => happy_shift(p, action_28, j),
        146 => happy_shift(p, action_29, j),
        147 => happy_shift(p, action_30, j),
        148 => happy_shift(p, action_31, j),
        149 => happy_shift(p, action_32, j),
        150 => happy_shift(p, action_33, j),
        153 => happy_shift(p, action_34, j),
        164 => happy_shift(p, action_35, j),
        184 => happy_shift(p, action_36, j),
        205 => happy_shift(p, action_37, j),
        221 => happy_shift(p, action_38, j),
        234 => happy_shift(p, action_39, j),
        235 => happy_shift(p, action_40, j),
        236 => happy_shift(p, action_41, j),
        237 => happy_shift(p, action_42, j),
        238 => happy_shift(p, action_43, j),
        241 => happy_shift(p, action_44, j),
        242 => happy_shift(p, action_45, j),
        243 => happy_shift(p, action_46, j),
        244 => happy_shift(p, action_47, j),
        245 => happy_shift(p, action_48, j),
        246 => happy_shift(p, action_49, j),
        247 => happy_shift(p, action_50, j),
        100 => happy_goto(p, action_6, j),
        104 => happy_goto(p, action_7, j),
        106 => happy_goto(p, action_261, j),
        107 => happy_goto(p, action_9, j),
        108 => happy_goto(p, action_10, j),
        109 => happy_goto(p, action_11, j),
        110 => happy_goto(p, action_12, j),
        111 => happy_goto(p, action_13, j),
        112 => happy_goto(p, action_14, j),
        113 => happy_goto(p, action_15, j),
        114 => happy_goto(p, action_16, j),
        115 => happy_goto(p, action_17, j),
        116 => happy_goto(p, action_18, j),
        117 => happy_goto(p, action_19, j),
        118 => happy_goto(p, action_20, j),
        119 => happy_goto(p, action_262, j),
        126 => happy_goto(p, action_603, j),
        127 => happy_goto(p, action_24, j),
        128 => happy_goto(p, action_25, j),
        _ => happy_fail(p, j)
    }
}

fn action_395(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_36(p, j)
    }
}

fn action_396(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_602, j),
        _ => happy_fail(p, j)
    }
}

fn action_397(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        185 => happy_shift(p, action_114, j),
        188 => happy_shift(p, action_116, j),
        190 => happy_shift(p, action_117, j),
        192 => happy_shift(p, action_118, j),
        195 => happy_shift(p, action_119, j),
        198 => happy_shift(p, action_120, j),
        200 => happy_shift(p, action_121, j),
        201 => happy_shift(p, action_122, j),
        202 => happy_shift(p, action_123, j),
        203 => happy_shift(p, action_124, j),
        208 => happy_shift(p, action_125, j),
        209 => happy_shift(p, action_126, j),
        210 => happy_shift(p, action_127, j),
        211 => happy_shift(p, action_128, j),
        213 => happy_shift(p, action_129, j),
        216 => happy_shift(p, action_130, j),
        219 => happy_shift(p, action_131, j),
        220 => happy_shift(p, action_132, j),
        222 => happy_shift(p, action_133, j),
        223 => happy_shift(p, action_134, j),
        224 => happy_shift(p, action_135, j),
        226 => happy_shift(p, action_136, j),
        227 => happy_shift(p, action_137, j),
        228 => happy_shift(p, action_138, j),
        229 => happy_shift(p, action_139, j),
        230 => happy_shift(p, action_140, j),
        231 => happy_shift(p, action_141, j),
        239 => happy_shift(p, action_143, j),
        240 => happy_shift(p, action_144, j),
        32 => happy_goto(p, action_601, j),
        34 => happy_goto(p, action_81, j),
        36 => happy_goto(p, action_82, j),
        37 => happy_goto(p, action_468, j),
        38 => happy_goto(p, action_469, j),
        40 => happy_goto(p, action_85, j),
        41 => happy_goto(p, action_86, j),
        42 => happy_goto(p, action_87, j),
        43 => happy_goto(p, action_88, j),
        44 => happy_goto(p, action_470, j),
        45 => happy_goto(p, action_90, j),
        46 => happy_goto(p, action_91, j),
        47 => happy_goto(p, action_92, j),
        48 => happy_goto(p, action_93, j),
        49 => happy_goto(p, action_94, j),
        50 => happy_goto(p, action_95, j),
        51 => happy_goto(p, action_96, j),
        52 => happy_goto(p, action_97, j),
        53 => happy_goto(p, action_98, j),
        54 => happy_goto(p, action_99, j),
        61 => happy_goto(p, action_100, j),
        65 => happy_goto(p, action_471, j),
        132 => happy_goto(p, action_109, j),
        133 => happy_goto(p, action_472, j),
        134 => happy_goto(p, action_111, j),
        _ => happy_reduce_473(p, j)
    }
}

fn action_398(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        180 => happy_shift(p, action_600, j),
        _ => happy_fail(p, j)
    }
}

fn action_399(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        180 => happy_shift(p, action_599, j),
        _ => happy_fail(p, j)
    }
}

fn action_400(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_65(p, j)
    }
}

fn action_401(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        139 => happy_shift(p, action_598, j),
        _ => happy_fail(p, j)
    }
}

fn action_402(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_69(p, j)
    }
}

fn action_403(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        139 => happy_shift(p, action_597, j),
        _ => happy_fail(p, j)
    }
}

fn action_404(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        139 => happy_shift(p, action_596, j),
        _ => happy_fail(p, j)
    }
}

fn action_405(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_479, j),
        150 => happy_shift(p, action_480, j),
        238 => happy_shift(p, action_142, j),
        75 => happy_goto(p, action_595, j),
        76 => happy_goto(p, action_103, j),
        77 => happy_goto(p, action_104, j),
        78 => happy_goto(p, action_485, j),
        _ => happy_fail(p, j)
    }
}

fn action_406(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_528, j),
        150 => happy_shift(p, action_529, j),
        238 => happy_shift(p, action_142, j),
        239 => happy_shift(p, action_231, j),
        66 => happy_goto(p, action_594, j),
        68 => happy_goto(p, action_221, j),
        69 => happy_goto(p, action_222, j),
        70 => happy_goto(p, action_223, j),
        71 => happy_goto(p, action_224, j),
        72 => happy_goto(p, action_225, j),
        73 => happy_goto(p, action_226, j),
        75 => happy_goto(p, action_527, j),
        76 => happy_goto(p, action_103, j),
        77 => happy_goto(p, action_104, j),
        78 => happy_goto(p, action_485, j),
        _ => happy_fail(p, j)
    }
}

fn action_407(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        181 => happy_shift(p, action_62, j),
        185 => happy_shift(p, action_114, j),
        188 => happy_shift(p, action_116, j),
        190 => happy_shift(p, action_117, j),
        192 => happy_shift(p, action_118, j),
        195 => happy_shift(p, action_119, j),
        198 => happy_shift(p, action_120, j),
        200 => happy_shift(p, action_121, j),
        201 => happy_shift(p, action_122, j),
        202 => happy_shift(p, action_123, j),
        203 => happy_shift(p, action_124, j),
        208 => happy_shift(p, action_125, j),
        209 => happy_shift(p, action_126, j),
        210 => happy_shift(p, action_127, j),
        211 => happy_shift(p, action_128, j),
        213 => happy_shift(p, action_129, j),
        216 => happy_shift(p, action_130, j),
        219 => happy_shift(p, action_131, j),
        220 => happy_shift(p, action_132, j),
        222 => happy_shift(p, action_133, j),
        223 => happy_shift(p, action_134, j),
        224 => happy_shift(p, action_135, j),
        226 => happy_shift(p, action_136, j),
        227 => happy_shift(p, action_137, j),
        228 => happy_shift(p, action_138, j),
        229 => happy_shift(p, action_139, j),
        230 => happy_shift(p, action_140, j),
        231 => happy_shift(p, action_141, j),
        239 => happy_shift(p, action_143, j),
        240 => happy_shift(p, action_144, j),
        14 => happy_goto(p, action_593, j),
        32 => happy_goto(p, action_467, j),
        34 => happy_goto(p, action_81, j),
        36 => happy_goto(p, action_82, j),
        37 => happy_goto(p, action_468, j),
        38 => happy_goto(p, action_469, j),
        40 => happy_goto(p, action_85, j),
        41 => happy_goto(p, action_86, j),
        42 => happy_goto(p, action_87, j),
        43 => happy_goto(p, action_88, j),
        44 => happy_goto(p, action_470, j),
        45 => happy_goto(p, action_90, j),
        46 => happy_goto(p, action_91, j),
        47 => happy_goto(p, action_92, j),
        48 => happy_goto(p, action_93, j),
        49 => happy_goto(p, action_94, j),
        50 => happy_goto(p, action_95, j),
        51 => happy_goto(p, action_96, j),
        52 => happy_goto(p, action_97, j),
        53 => happy_goto(p, action_98, j),
        54 => happy_goto(p, action_99, j),
        61 => happy_goto(p, action_100, j),
        65 => happy_goto(p, action_471, j),
        132 => happy_goto(p, action_109, j),
        133 => happy_goto(p, action_472, j),
        134 => happy_goto(p, action_111, j),
        _ => happy_reduce_473(p, j)
    }
}

fn action_408(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        168 => happy_shift(p, action_520, j),
        94 => happy_goto(p, action_592, j),
        _ => happy_reduce_345(p, j)
    }
}

fn action_409(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        240 => happy_shift(p, action_144, j),
        132 => happy_goto(p, action_591, j),
        133 => happy_goto(p, action_152, j),
        134 => happy_goto(p, action_111, j),
        _ => happy_reduce_473(p, j)
    }
}

fn action_410(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_590, j),
        _ => happy_fail(p, j)
    }
}

fn action_411(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_14(p, j)
    }
}

fn action_412(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_26, j),
        144 => happy_shift(p, action_27, j),
        145 => happy_shift(p, action_28, j),
        146 => happy_shift(p, action_29, j),
        147 => happy_shift(p, action_30, j),
        148 => happy_shift(p, action_31, j),
        149 => happy_shift(p, action_32, j),
        150 => happy_shift(p, action_33, j),
        153 => happy_shift(p, action_34, j),
        164 => happy_shift(p, action_35, j),
        184 => happy_shift(p, action_36, j),
        190 => happy_shift(p, action_117, j),
        192 => happy_shift(p, action_118, j),
        195 => happy_shift(p, action_119, j),
        198 => happy_shift(p, action_120, j),
        200 => happy_shift(p, action_121, j),
        202 => happy_shift(p, action_123, j),
        203 => happy_shift(p, action_124, j),
        205 => happy_shift(p, action_37, j),
        209 => happy_shift(p, action_126, j),
        210 => happy_shift(p, action_127, j),
        211 => happy_shift(p, action_128, j),
        219 => happy_shift(p, action_131, j),
        220 => happy_shift(p, action_132, j),
        221 => happy_shift(p, action_38, j),
        224 => happy_shift(p, action_135, j),
        227 => happy_shift(p, action_137, j),
        229 => happy_shift(p, action_139, j),
        230 => happy_shift(p, action_140, j),
        231 => happy_shift(p, action_141, j),
        234 => happy_shift(p, action_39, j),
        235 => happy_shift(p, action_40, j),
        236 => happy_shift(p, action_41, j),
        237 => happy_shift(p, action_42, j),
        238 => happy_shift(p, action_43, j),
        239 => happy_shift(p, action_143, j),
        240 => happy_shift(p, action_144, j),
        241 => happy_shift(p, action_44, j),
        242 => happy_shift(p, action_45, j),
        243 => happy_shift(p, action_46, j),
        244 => happy_shift(p, action_47, j),
        245 => happy_shift(p, action_48, j),
        246 => happy_shift(p, action_49, j),
        247 => happy_shift(p, action_50, j),
        44 => happy_goto(p, action_289, j),
        45 => happy_goto(p, action_90, j),
        47 => happy_goto(p, action_290, j),
        49 => happy_goto(p, action_291, j),
        51 => happy_goto(p, action_292, j),
        52 => happy_goto(p, action_97, j),
        53 => happy_goto(p, action_98, j),
        54 => happy_goto(p, action_99, j),
        61 => happy_goto(p, action_100, j),
        65 => happy_goto(p, action_293, j),
        86 => happy_goto(p, action_588, j),
        100 => happy_goto(p, action_6, j),
        104 => happy_goto(p, action_7, j),
        106 => happy_goto(p, action_8, j),
        107 => happy_goto(p, action_9, j),
        108 => happy_goto(p, action_10, j),
        109 => happy_goto(p, action_11, j),
        110 => happy_goto(p, action_12, j),
        111 => happy_goto(p, action_13, j),
        112 => happy_goto(p, action_14, j),
        113 => happy_goto(p, action_15, j),
        114 => happy_goto(p, action_16, j),
        115 => happy_goto(p, action_17, j),
        116 => happy_goto(p, action_18, j),
        117 => happy_goto(p, action_19, j),
        118 => happy_goto(p, action_20, j),
        119 => happy_goto(p, action_21, j),
        120 => happy_goto(p, action_22, j),
        122 => happy_goto(p, action_589, j),
        127 => happy_goto(p, action_24, j),
        128 => happy_goto(p, action_25, j),
        132 => happy_goto(p, action_109, j),
        133 => happy_goto(p, action_296, j),
        134 => happy_goto(p, action_111, j),
        _ => happy_reduce_473(p, j)
    }
}

fn action_413(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        181 => happy_shift(p, action_62, j),
        185 => happy_shift(p, action_114, j),
        188 => happy_shift(p, action_116, j),
        190 => happy_shift(p, action_117, j),
        192 => happy_shift(p, action_118, j),
        195 => happy_shift(p, action_119, j),
        198 => happy_shift(p, action_120, j),
        200 => happy_shift(p, action_121, j),
        201 => happy_shift(p, action_122, j),
        202 => happy_shift(p, action_123, j),
        203 => happy_shift(p, action_124, j),
        208 => happy_shift(p, action_125, j),
        209 => happy_shift(p, action_126, j),
        210 => happy_shift(p, action_127, j),
        211 => happy_shift(p, action_128, j),
        213 => happy_shift(p, action_129, j),
        216 => happy_shift(p, action_130, j),
        219 => happy_shift(p, action_131, j),
        220 => happy_shift(p, action_132, j),
        222 => happy_shift(p, action_133, j),
        223 => happy_shift(p, action_134, j),
        224 => happy_shift(p, action_135, j),
        226 => happy_shift(p, action_136, j),
        227 => happy_shift(p, action_137, j),
        228 => happy_shift(p, action_138, j),
        229 => happy_shift(p, action_139, j),
        230 => happy_shift(p, action_140, j),
        231 => happy_shift(p, action_141, j),
        239 => happy_shift(p, action_143, j),
        240 => happy_shift(p, action_144, j),
        14 => happy_goto(p, action_587, j),
        32 => happy_goto(p, action_467, j),
        34 => happy_goto(p, action_81, j),
        36 => happy_goto(p, action_82, j),
        37 => happy_goto(p, action_468, j),
        38 => happy_goto(p, action_469, j),
        40 => happy_goto(p, action_85, j),
        41 => happy_goto(p, action_86, j),
        42 => happy_goto(p, action_87, j),
        43 => happy_goto(p, action_88, j),
        44 => happy_goto(p, action_470, j),
        45 => happy_goto(p, action_90, j),
        46 => happy_goto(p, action_91, j),
        47 => happy_goto(p, action_92, j),
        48 => happy_goto(p, action_93, j),
        49 => happy_goto(p, action_94, j),
        50 => happy_goto(p, action_95, j),
        51 => happy_goto(p, action_96, j),
        52 => happy_goto(p, action_97, j),
        53 => happy_goto(p, action_98, j),
        54 => happy_goto(p, action_99, j),
        61 => happy_goto(p, action_100, j),
        65 => happy_goto(p, action_471, j),
        132 => happy_goto(p, action_109, j),
        133 => happy_goto(p, action_472, j),
        134 => happy_goto(p, action_111, j),
        _ => happy_reduce_473(p, j)
    }
}

fn action_414(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        168 => happy_shift(p, action_520, j),
        94 => happy_goto(p, action_586, j),
        _ => happy_reduce_345(p, j)
    }
}

fn action_415(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_16(p, j)
    }
}

fn action_416(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_238(p, j)
    }
}

fn action_417(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        185 => happy_shift(p, action_114, j),
        186 => happy_reduce_473(p, j),
        188 => happy_shift(p, action_116, j),
        190 => happy_shift(p, action_117, j),
        192 => happy_shift(p, action_118, j),
        193 => happy_reduce_473(p, j),
        195 => happy_shift(p, action_119, j),
        198 => happy_shift(p, action_120, j),
        200 => happy_shift(p, action_121, j),
        201 => happy_shift(p, action_122, j),
        202 => happy_shift(p, action_123, j),
        203 => happy_shift(p, action_124, j),
        208 => happy_shift(p, action_125, j),
        209 => happy_shift(p, action_126, j),
        210 => happy_shift(p, action_127, j),
        211 => happy_shift(p, action_128, j),
        213 => happy_shift(p, action_129, j),
        214 => happy_reduce_473(p, j),
        215 => happy_reduce_473(p, j),
        216 => happy_shift(p, action_130, j),
        217 => happy_reduce_473(p, j),
        219 => happy_shift(p, action_131, j),
        220 => happy_shift(p, action_132, j),
        222 => happy_shift(p, action_133, j),
        224 => happy_shift(p, action_135, j),
        226 => happy_shift(p, action_136, j),
        227 => happy_shift(p, action_137, j),
        228 => happy_shift(p, action_138, j),
        229 => happy_shift(p, action_139, j),
        230 => happy_shift(p, action_140, j),
        231 => happy_shift(p, action_141, j),
        232 => happy_reduce_473(p, j),
        239 => happy_shift(p, action_143, j),
        240 => happy_shift(p, action_144, j),
        37 => happy_goto(p, action_453, j),
        38 => happy_goto(p, action_454, j),
        40 => happy_goto(p, action_85, j),
        41 => happy_goto(p, action_86, j),
        42 => happy_goto(p, action_87, j),
        43 => happy_goto(p, action_88, j),
        44 => happy_goto(p, action_455, j),
        45 => happy_goto(p, action_90, j),
        46 => happy_goto(p, action_91, j),
        47 => happy_goto(p, action_92, j),
        48 => happy_goto(p, action_456, j),
        49 => happy_goto(p, action_457, j),
        50 => happy_goto(p, action_95, j),
        51 => happy_goto(p, action_96, j),
        52 => happy_goto(p, action_97, j),
        53 => happy_goto(p, action_98, j),
        54 => happy_goto(p, action_99, j),
        61 => happy_goto(p, action_100, j),
        65 => happy_goto(p, action_458, j),
        82 => happy_goto(p, action_459, j),
        83 => happy_goto(p, action_460, j),
        84 => happy_goto(p, action_461, j),
        132 => happy_goto(p, action_109, j),
        133 => happy_goto(p, action_463, j),
        134 => happy_goto(p, action_111, j),
        _ => happy_reduce_283(p, j)
    }
}

fn action_418(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_585, j),
        150 => happy_shift(p, action_230, j),
        186 => happy_shift(p, action_173, j),
        193 => happy_shift(p, action_174, j),
        214 => happy_shift(p, action_175, j),
        215 => happy_shift(p, action_176, j),
        217 => happy_shift(p, action_177, j),
        232 => happy_shift(p, action_178, j),
        238 => happy_shift(p, action_142, j),
        239 => happy_shift(p, action_231, j),
        240 => happy_shift(p, action_144, j),
        64 => happy_goto(p, action_189, j),
        69 => happy_goto(p, action_582, j),
        70 => happy_goto(p, action_223, j),
        71 => happy_goto(p, action_224, j),
        72 => happy_goto(p, action_583, j),
        73 => happy_goto(p, action_226, j),
        75 => happy_goto(p, action_486, j),
        76 => happy_goto(p, action_103, j),
        77 => happy_goto(p, action_104, j),
        78 => happy_goto(p, action_105, j),
        80 => happy_goto(p, action_487, j),
        81 => happy_goto(p, action_108, j),
        133 => happy_goto(p, action_584, j),
        134 => happy_goto(p, action_111, j),
        _ => happy_fail(p, j)
    }
}

fn action_419(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_241(p, j)
    }
}

fn action_420(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_253(p, j)
    }
}

fn action_421(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_575, j),
        150 => happy_shift(p, action_576, j),
        238 => happy_shift(p, action_142, j),
        239 => happy_shift(p, action_231, j),
        240 => happy_shift(p, action_144, j),
        69 => happy_goto(p, action_581, j),
        70 => happy_goto(p, action_223, j),
        71 => happy_goto(p, action_224, j),
        75 => happy_goto(p, action_484, j),
        76 => happy_goto(p, action_103, j),
        77 => happy_goto(p, action_104, j),
        78 => happy_goto(p, action_485, j),
        134 => happy_goto(p, action_169, j),
        _ => happy_reduce_474(p, j)
    }
}

fn action_422(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_427, j),
        150 => happy_shift(p, action_230, j),
        238 => happy_shift(p, action_142, j),
        239 => happy_shift(p, action_428, j),
        240 => happy_shift(p, action_144, j),
        70 => happy_goto(p, action_423, j),
        71 => happy_goto(p, action_224, j),
        72 => happy_goto(p, action_424, j),
        73 => happy_goto(p, action_226, j),
        74 => happy_goto(p, action_580, j),
        76 => happy_goto(p, action_159, j),
        77 => happy_goto(p, action_104, j),
        78 => happy_goto(p, action_160, j),
        80 => happy_goto(p, action_161, j),
        81 => happy_goto(p, action_108, j),
        133 => happy_goto(p, action_426, j),
        134 => happy_goto(p, action_111, j),
        _ => happy_fail(p, j)
    }
}

fn action_423(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        139 => happy_shift(p, action_579, j),
        _ => happy_fail(p, j)
    }
}

fn action_424(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        139 => happy_shift(p, action_578, j),
        _ => happy_fail(p, j)
    }
}

fn action_425(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_417, j),
        140 => happy_shift(p, action_184, j),
        88 => happy_goto(p, action_577, j),
        89 => happy_goto(p, action_181, j),
        90 => happy_goto(p, action_182, j),
        _ => happy_fail(p, j)
    }
}

fn action_426(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_575, j),
        150 => happy_shift(p, action_576, j),
        238 => happy_shift(p, action_142, j),
        240 => happy_shift(p, action_144, j),
        70 => happy_goto(p, action_574, j),
        71 => happy_goto(p, action_224, j),
        76 => happy_goto(p, action_477, j),
        77 => happy_goto(p, action_104, j),
        78 => happy_goto(p, action_478, j),
        134 => happy_goto(p, action_169, j),
        _ => happy_fail(p, j)
    }
}

fn action_427(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_427, j),
        150 => happy_shift(p, action_230, j),
        238 => happy_shift(p, action_142, j),
        239 => happy_shift(p, action_428, j),
        240 => happy_shift(p, action_144, j),
        70 => happy_goto(p, action_423, j),
        71 => happy_goto(p, action_224, j),
        72 => happy_goto(p, action_424, j),
        73 => happy_goto(p, action_226, j),
        74 => happy_goto(p, action_573, j),
        76 => happy_goto(p, action_159, j),
        77 => happy_goto(p, action_104, j),
        78 => happy_goto(p, action_160, j),
        80 => happy_goto(p, action_161, j),
        81 => happy_goto(p, action_108, j),
        133 => happy_goto(p, action_426, j),
        134 => happy_goto(p, action_111, j),
        _ => happy_fail(p, j)
    }
}

fn action_428(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_259(p, j)
    }
}

fn action_429(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        181 => happy_shift(p, action_62, j),
        185 => happy_shift(p, action_114, j),
        188 => happy_shift(p, action_116, j),
        190 => happy_shift(p, action_117, j),
        192 => happy_shift(p, action_118, j),
        195 => happy_shift(p, action_119, j),
        198 => happy_shift(p, action_120, j),
        200 => happy_shift(p, action_121, j),
        201 => happy_shift(p, action_122, j),
        202 => happy_shift(p, action_123, j),
        203 => happy_shift(p, action_124, j),
        208 => happy_shift(p, action_125, j),
        209 => happy_shift(p, action_126, j),
        210 => happy_shift(p, action_127, j),
        211 => happy_shift(p, action_128, j),
        213 => happy_shift(p, action_129, j),
        216 => happy_shift(p, action_130, j),
        219 => happy_shift(p, action_131, j),
        220 => happy_shift(p, action_132, j),
        222 => happy_shift(p, action_133, j),
        223 => happy_shift(p, action_134, j),
        224 => happy_shift(p, action_135, j),
        226 => happy_shift(p, action_136, j),
        227 => happy_shift(p, action_137, j),
        228 => happy_shift(p, action_138, j),
        229 => happy_shift(p, action_139, j),
        230 => happy_shift(p, action_140, j),
        231 => happy_shift(p, action_141, j),
        239 => happy_shift(p, action_143, j),
        240 => happy_shift(p, action_144, j),
        14 => happy_goto(p, action_572, j),
        32 => happy_goto(p, action_467, j),
        34 => happy_goto(p, action_81, j),
        36 => happy_goto(p, action_82, j),
        37 => happy_goto(p, action_468, j),
        38 => happy_goto(p, action_469, j),
        40 => happy_goto(p, action_85, j),
        41 => happy_goto(p, action_86, j),
        42 => happy_goto(p, action_87, j),
        43 => happy_goto(p, action_88, j),
        44 => happy_goto(p, action_470, j),
        45 => happy_goto(p, action_90, j),
        46 => happy_goto(p, action_91, j),
        47 => happy_goto(p, action_92, j),
        48 => happy_goto(p, action_93, j),
        49 => happy_goto(p, action_94, j),
        50 => happy_goto(p, action_95, j),
        51 => happy_goto(p, action_96, j),
        52 => happy_goto(p, action_97, j),
        53 => happy_goto(p, action_98, j),
        54 => happy_goto(p, action_99, j),
        61 => happy_goto(p, action_100, j),
        65 => happy_goto(p, action_471, j),
        132 => happy_goto(p, action_109, j),
        133 => happy_goto(p, action_472, j),
        134 => happy_goto(p, action_111, j),
        _ => happy_reduce_473(p, j)
    }
}

fn action_430(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        168 => happy_shift(p, action_520, j),
        94 => happy_goto(p, action_571, j),
        _ => happy_reduce_345(p, j)
    }
}

fn action_431(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_15(p, j)
    }
}

fn action_432(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        181 => happy_shift(p, action_570, j),
        _ => happy_reduce_189(p, j)
    }
}

fn action_433(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        55 => happy_goto(p, action_569, j),
        _ => happy_reduce_192(p, j)
    }
}

fn action_434(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_26, j),
        144 => happy_shift(p, action_27, j),
        145 => happy_shift(p, action_28, j),
        146 => happy_shift(p, action_29, j),
        147 => happy_shift(p, action_30, j),
        148 => happy_shift(p, action_31, j),
        149 => happy_shift(p, action_32, j),
        150 => happy_shift(p, action_33, j),
        153 => happy_shift(p, action_34, j),
        164 => happy_shift(p, action_35, j),
        184 => happy_shift(p, action_36, j),
        190 => happy_shift(p, action_117, j),
        192 => happy_shift(p, action_118, j),
        195 => happy_shift(p, action_119, j),
        198 => happy_shift(p, action_120, j),
        200 => happy_shift(p, action_121, j),
        202 => happy_shift(p, action_123, j),
        203 => happy_shift(p, action_124, j),
        205 => happy_shift(p, action_37, j),
        209 => happy_shift(p, action_126, j),
        210 => happy_shift(p, action_127, j),
        211 => happy_shift(p, action_128, j),
        219 => happy_shift(p, action_131, j),
        220 => happy_shift(p, action_132, j),
        221 => happy_shift(p, action_38, j),
        224 => happy_shift(p, action_135, j),
        227 => happy_shift(p, action_137, j),
        229 => happy_shift(p, action_139, j),
        230 => happy_shift(p, action_140, j),
        231 => happy_shift(p, action_141, j),
        234 => happy_shift(p, action_39, j),
        235 => happy_shift(p, action_40, j),
        236 => happy_shift(p, action_41, j),
        237 => happy_shift(p, action_42, j),
        238 => happy_shift(p, action_43, j),
        239 => happy_shift(p, action_143, j),
        240 => happy_shift(p, action_144, j),
        241 => happy_shift(p, action_44, j),
        242 => happy_shift(p, action_45, j),
        243 => happy_shift(p, action_46, j),
        244 => happy_shift(p, action_47, j),
        245 => happy_shift(p, action_48, j),
        246 => happy_shift(p, action_49, j),
        247 => happy_shift(p, action_50, j),
        44 => happy_goto(p, action_289, j),
        45 => happy_goto(p, action_90, j),
        47 => happy_goto(p, action_290, j),
        49 => happy_goto(p, action_291, j),
        51 => happy_goto(p, action_292, j),
        52 => happy_goto(p, action_97, j),
        53 => happy_goto(p, action_98, j),
        54 => happy_goto(p, action_99, j),
        61 => happy_goto(p, action_100, j),
        65 => happy_goto(p, action_293, j),
        86 => happy_goto(p, action_567, j),
        100 => happy_goto(p, action_6, j),
        104 => happy_goto(p, action_7, j),
        106 => happy_goto(p, action_8, j),
        107 => happy_goto(p, action_9, j),
        108 => happy_goto(p, action_10, j),
        109 => happy_goto(p, action_11, j),
        110 => happy_goto(p, action_12, j),
        111 => happy_goto(p, action_13, j),
        112 => happy_goto(p, action_14, j),
        113 => happy_goto(p, action_15, j),
        114 => happy_goto(p, action_16, j),
        115 => happy_goto(p, action_17, j),
        116 => happy_goto(p, action_18, j),
        117 => happy_goto(p, action_19, j),
        118 => happy_goto(p, action_20, j),
        119 => happy_goto(p, action_21, j),
        120 => happy_goto(p, action_22, j),
        122 => happy_goto(p, action_568, j),
        127 => happy_goto(p, action_24, j),
        128 => happy_goto(p, action_25, j),
        132 => happy_goto(p, action_109, j),
        133 => happy_goto(p, action_296, j),
        134 => happy_goto(p, action_111, j),
        _ => happy_reduce_473(p, j)
    }
}

fn action_435(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        181 => happy_shift(p, action_62, j),
        14 => happy_goto(p, action_566, j),
        _ => happy_fail(p, j)
    }
}

fn action_436(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_107(p, j)
    }
}

fn action_437(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_151(p, j)
    }
}

fn action_438(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_162(p, j)
    }
}

fn action_439(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_230(p, j)
    }
}

fn action_440(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        181 => happy_reduce_26(p, j),
        187 => happy_shift(p, action_410, j),
        35 => happy_goto(p, action_565, j),
        67 => happy_goto(p, action_409, j),
        _ => happy_reduce_233(p, j)
    }
}

fn action_441(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        33 => happy_goto(p, action_564, j),
        _ => happy_reduce_90(p, j)
    }
}

fn action_442(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_563, j),
        _ => happy_fail(p, j)
    }
}

fn action_443(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_180(p, j)
    }
}

fn action_444(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        181 => happy_shift(p, action_62, j),
        185 => happy_shift(p, action_114, j),
        188 => happy_shift(p, action_116, j),
        190 => happy_shift(p, action_117, j),
        192 => happy_shift(p, action_118, j),
        195 => happy_shift(p, action_119, j),
        198 => happy_shift(p, action_120, j),
        200 => happy_shift(p, action_121, j),
        201 => happy_shift(p, action_122, j),
        202 => happy_shift(p, action_123, j),
        203 => happy_shift(p, action_124, j),
        208 => happy_shift(p, action_125, j),
        209 => happy_shift(p, action_126, j),
        210 => happy_shift(p, action_127, j),
        211 => happy_shift(p, action_128, j),
        213 => happy_shift(p, action_129, j),
        216 => happy_shift(p, action_130, j),
        219 => happy_shift(p, action_131, j),
        220 => happy_shift(p, action_132, j),
        222 => happy_shift(p, action_133, j),
        223 => happy_shift(p, action_134, j),
        224 => happy_shift(p, action_135, j),
        226 => happy_shift(p, action_136, j),
        227 => happy_shift(p, action_137, j),
        228 => happy_shift(p, action_138, j),
        229 => happy_shift(p, action_139, j),
        230 => happy_shift(p, action_140, j),
        231 => happy_shift(p, action_141, j),
        239 => happy_shift(p, action_143, j),
        240 => happy_shift(p, action_144, j),
        14 => happy_goto(p, action_562, j),
        32 => happy_goto(p, action_467, j),
        34 => happy_goto(p, action_81, j),
        36 => happy_goto(p, action_82, j),
        37 => happy_goto(p, action_468, j),
        38 => happy_goto(p, action_469, j),
        40 => happy_goto(p, action_85, j),
        41 => happy_goto(p, action_86, j),
        42 => happy_goto(p, action_87, j),
        43 => happy_goto(p, action_88, j),
        44 => happy_goto(p, action_470, j),
        45 => happy_goto(p, action_90, j),
        46 => happy_goto(p, action_91, j),
        47 => happy_goto(p, action_92, j),
        48 => happy_goto(p, action_93, j),
        49 => happy_goto(p, action_94, j),
        50 => happy_goto(p, action_95, j),
        51 => happy_goto(p, action_96, j),
        52 => happy_goto(p, action_97, j),
        53 => happy_goto(p, action_98, j),
        54 => happy_goto(p, action_99, j),
        61 => happy_goto(p, action_100, j),
        65 => happy_goto(p, action_471, j),
        132 => happy_goto(p, action_109, j),
        133 => happy_goto(p, action_472, j),
        134 => happy_goto(p, action_111, j),
        _ => happy_reduce_473(p, j)
    }
}

fn action_445(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        168 => happy_shift(p, action_520, j),
        94 => happy_goto(p, action_561, j),
        _ => happy_reduce_345(p, j)
    }
}

fn action_446(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_17(p, j)
    }
}

fn action_447(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_26, j),
        144 => happy_shift(p, action_27, j),
        145 => happy_shift(p, action_28, j),
        146 => happy_shift(p, action_29, j),
        147 => happy_shift(p, action_30, j),
        148 => happy_shift(p, action_31, j),
        149 => happy_shift(p, action_32, j),
        150 => happy_shift(p, action_560, j),
        153 => happy_shift(p, action_34, j),
        164 => happy_shift(p, action_35, j),
        184 => happy_shift(p, action_36, j),
        186 => happy_shift(p, action_173, j),
        193 => happy_shift(p, action_174, j),
        205 => happy_shift(p, action_37, j),
        214 => happy_shift(p, action_175, j),
        215 => happy_shift(p, action_176, j),
        217 => happy_shift(p, action_177, j),
        221 => happy_shift(p, action_38, j),
        222 => happy_reduce_473(p, j),
        232 => happy_shift(p, action_178, j),
        234 => happy_shift(p, action_39, j),
        235 => happy_shift(p, action_40, j),
        236 => happy_shift(p, action_41, j),
        237 => happy_shift(p, action_42, j),
        238 => happy_shift(p, action_43, j),
        240 => happy_shift(p, action_144, j),
        241 => happy_shift(p, action_44, j),
        242 => happy_shift(p, action_45, j),
        243 => happy_shift(p, action_46, j),
        244 => happy_shift(p, action_47, j),
        245 => happy_shift(p, action_48, j),
        246 => happy_shift(p, action_49, j),
        247 => happy_shift(p, action_50, j),
        64 => happy_goto(p, action_189, j),
        100 => happy_goto(p, action_6, j),
        104 => happy_goto(p, action_7, j),
        106 => happy_goto(p, action_8, j),
        107 => happy_goto(p, action_9, j),
        108 => happy_goto(p, action_10, j),
        109 => happy_goto(p, action_11, j),
        110 => happy_goto(p, action_12, j),
        111 => happy_goto(p, action_13, j),
        112 => happy_goto(p, action_14, j),
        113 => happy_goto(p, action_15, j),
        114 => happy_goto(p, action_16, j),
        115 => happy_goto(p, action_17, j),
        116 => happy_goto(p, action_18, j),
        117 => happy_goto(p, action_19, j),
        118 => happy_goto(p, action_20, j),
        119 => happy_goto(p, action_21, j),
        120 => happy_goto(p, action_448, j),
        125 => happy_goto(p, action_557, j),
        127 => happy_goto(p, action_24, j),
        128 => happy_goto(p, action_25, j),
        132 => happy_goto(p, action_558, j),
        133 => happy_goto(p, action_559, j),
        134 => happy_goto(p, action_111, j),
        _ => happy_reduce_460(p, j)
    }
}

fn action_448(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_461(p, j)
    }
}

fn action_449(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        141 => happy_shift(p, action_556, j),
        _ => happy_fail(p, j)
    }
}

fn action_450(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_26, j),
        144 => happy_shift(p, action_27, j),
        145 => happy_shift(p, action_28, j),
        146 => happy_shift(p, action_29, j),
        147 => happy_shift(p, action_30, j),
        148 => happy_shift(p, action_31, j),
        149 => happy_shift(p, action_32, j),
        150 => happy_shift(p, action_555, j),
        153 => happy_shift(p, action_34, j),
        164 => happy_shift(p, action_35, j),
        184 => happy_shift(p, action_36, j),
        186 => happy_reduce_474(p, j),
        193 => happy_reduce_474(p, j),
        205 => happy_shift(p, action_37, j),
        214 => happy_reduce_474(p, j),
        215 => happy_reduce_474(p, j),
        217 => happy_reduce_474(p, j),
        221 => happy_shift(p, action_38, j),
        232 => happy_reduce_474(p, j),
        234 => happy_shift(p, action_39, j),
        235 => happy_shift(p, action_40, j),
        236 => happy_shift(p, action_41, j),
        237 => happy_shift(p, action_42, j),
        238 => happy_shift(p, action_43, j),
        240 => happy_shift(p, action_144, j),
        241 => happy_shift(p, action_44, j),
        242 => happy_shift(p, action_45, j),
        243 => happy_shift(p, action_46, j),
        244 => happy_shift(p, action_47, j),
        245 => happy_shift(p, action_48, j),
        246 => happy_shift(p, action_49, j),
        247 => happy_shift(p, action_50, j),
        100 => happy_goto(p, action_6, j),
        104 => happy_goto(p, action_7, j),
        106 => happy_goto(p, action_8, j),
        107 => happy_goto(p, action_9, j),
        108 => happy_goto(p, action_10, j),
        109 => happy_goto(p, action_11, j),
        110 => happy_goto(p, action_12, j),
        111 => happy_goto(p, action_13, j),
        112 => happy_goto(p, action_14, j),
        113 => happy_goto(p, action_15, j),
        114 => happy_goto(p, action_16, j),
        115 => happy_goto(p, action_17, j),
        116 => happy_goto(p, action_18, j),
        117 => happy_goto(p, action_19, j),
        118 => happy_goto(p, action_20, j),
        119 => happy_goto(p, action_21, j),
        120 => happy_goto(p, action_448, j),
        125 => happy_goto(p, action_554, j),
        127 => happy_goto(p, action_24, j),
        128 => happy_goto(p, action_25, j),
        134 => happy_goto(p, action_169, j),
        _ => happy_reduce_460(p, j)
    }
}

fn action_451(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        141 => happy_reduce_473(p, j),
        240 => happy_shift(p, action_144, j),
        132 => happy_goto(p, action_553, j),
        133 => happy_goto(p, action_152, j),
        134 => happy_goto(p, action_111, j),
        _ => happy_reduce_403(p, j)
    }
}

fn action_452(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        240 => happy_shift(p, action_144, j),
        65 => happy_goto(p, action_551, j),
        132 => happy_goto(p, action_552, j),
        133 => happy_goto(p, action_152, j),
        134 => happy_goto(p, action_111, j),
        _ => happy_reduce_473(p, j)
    }
}

fn action_453(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_544, j),
        140 => happy_shift(p, action_184, j),
        150 => happy_shift(p, action_545, j),
        238 => happy_shift(p, action_142, j),
        239 => happy_shift(p, action_231, j),
        69 => happy_goto(p, action_548, j),
        70 => happy_goto(p, action_223, j),
        71 => happy_goto(p, action_224, j),
        75 => happy_goto(p, action_549, j),
        76 => happy_goto(p, action_103, j),
        77 => happy_goto(p, action_104, j),
        78 => happy_goto(p, action_485, j),
        87 => happy_goto(p, action_550, j),
        88 => happy_goto(p, action_370, j),
        89 => happy_goto(p, action_181, j),
        90 => happy_goto(p, action_182, j),
        91 => happy_goto(p, action_371, j),
        92 => happy_goto(p, action_372, j),
        _ => happy_reduce_288(p, j)
    }
}

fn action_454(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_539, j),
        140 => happy_shift(p, action_184, j),
        150 => happy_shift(p, action_540, j),
        185 => happy_shift(p, action_114, j),
        186 => happy_shift(p, action_173, j),
        188 => happy_shift(p, action_116, j),
        190 => happy_shift(p, action_117, j),
        192 => happy_shift(p, action_118, j),
        193 => happy_shift(p, action_174, j),
        195 => happy_shift(p, action_119, j),
        198 => happy_shift(p, action_120, j),
        200 => happy_shift(p, action_121, j),
        201 => happy_shift(p, action_122, j),
        202 => happy_shift(p, action_123, j),
        203 => happy_shift(p, action_124, j),
        208 => happy_shift(p, action_125, j),
        209 => happy_shift(p, action_126, j),
        210 => happy_shift(p, action_127, j),
        211 => happy_shift(p, action_128, j),
        213 => happy_shift(p, action_129, j),
        214 => happy_shift(p, action_175, j),
        215 => happy_shift(p, action_176, j),
        216 => happy_shift(p, action_130, j),
        217 => happy_shift(p, action_177, j),
        219 => happy_shift(p, action_131, j),
        220 => happy_shift(p, action_132, j),
        222 => happy_shift(p, action_133, j),
        224 => happy_shift(p, action_135, j),
        226 => happy_shift(p, action_136, j),
        227 => happy_shift(p, action_239, j),
        228 => happy_shift(p, action_138, j),
        229 => happy_shift(p, action_139, j),
        230 => happy_shift(p, action_140, j),
        231 => happy_shift(p, action_141, j),
        232 => happy_shift(p, action_178, j),
        238 => happy_shift(p, action_142, j),
        239 => happy_shift(p, action_240, j),
        240 => happy_shift(p, action_144, j),
        39 => happy_goto(p, action_233, j),
        41 => happy_goto(p, action_200, j),
        42 => happy_goto(p, action_201, j),
        43 => happy_goto(p, action_202, j),
        45 => happy_goto(p, action_234, j),
        52 => happy_goto(p, action_235, j),
        53 => happy_goto(p, action_98, j),
        54 => happy_goto(p, action_99, j),
        61 => happy_goto(p, action_100, j),
        64 => happy_goto(p, action_203, j),
        75 => happy_goto(p, action_546, j),
        76 => happy_goto(p, action_103, j),
        77 => happy_goto(p, action_104, j),
        78 => happy_goto(p, action_485, j),
        87 => happy_goto(p, action_547, j),
        88 => happy_goto(p, action_370, j),
        89 => happy_goto(p, action_181, j),
        90 => happy_goto(p, action_182, j),
        91 => happy_goto(p, action_371, j),
        92 => happy_goto(p, action_372, j),
        134 => happy_goto(p, action_238, j),
        _ => happy_reduce_292(p, j)
    }
}

fn action_455(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_544, j),
        140 => happy_shift(p, action_184, j),
        150 => happy_shift(p, action_545, j),
        238 => happy_shift(p, action_142, j),
        239 => happy_shift(p, action_231, j),
        69 => happy_goto(p, action_541, j),
        70 => happy_goto(p, action_223, j),
        71 => happy_goto(p, action_224, j),
        75 => happy_goto(p, action_542, j),
        76 => happy_goto(p, action_103, j),
        77 => happy_goto(p, action_104, j),
        78 => happy_goto(p, action_485, j),
        87 => happy_goto(p, action_543, j),
        88 => happy_goto(p, action_370, j),
        89 => happy_goto(p, action_181, j),
        90 => happy_goto(p, action_182, j),
        91 => happy_goto(p, action_371, j),
        92 => happy_goto(p, action_372, j),
        _ => happy_reduce_295(p, j)
    }
}

fn action_456(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        185 => happy_shift(p, action_114, j),
        186 => happy_shift(p, action_173, j),
        188 => happy_shift(p, action_116, j),
        193 => happy_shift(p, action_174, j),
        201 => happy_shift(p, action_122, j),
        208 => happy_shift(p, action_125, j),
        213 => happy_shift(p, action_129, j),
        214 => happy_shift(p, action_175, j),
        215 => happy_shift(p, action_176, j),
        216 => happy_shift(p, action_130, j),
        217 => happy_shift(p, action_177, j),
        222 => happy_shift(p, action_133, j),
        226 => happy_shift(p, action_136, j),
        228 => happy_shift(p, action_138, j),
        232 => happy_shift(p, action_178, j),
        240 => happy_shift(p, action_144, j),
        39 => happy_goto(p, action_209, j),
        41 => happy_goto(p, action_200, j),
        42 => happy_goto(p, action_201, j),
        43 => happy_goto(p, action_202, j),
        64 => happy_goto(p, action_203, j),
        134 => happy_goto(p, action_210, j),
        _ => happy_reduce_102(p, j)
    }
}

fn action_457(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        186 => happy_shift(p, action_173, j),
        188 => happy_shift(p, action_116, j),
        193 => happy_shift(p, action_174, j),
        201 => happy_shift(p, action_122, j),
        214 => happy_shift(p, action_175, j),
        215 => happy_shift(p, action_176, j),
        216 => happy_shift(p, action_130, j),
        217 => happy_shift(p, action_177, j),
        222 => happy_shift(p, action_133, j),
        226 => happy_shift(p, action_136, j),
        228 => happy_shift(p, action_138, j),
        232 => happy_shift(p, action_178, j),
        240 => happy_shift(p, action_144, j),
        41 => happy_goto(p, action_205, j),
        64 => happy_goto(p, action_206, j),
        134 => happy_goto(p, action_207, j),
        _ => happy_reduce_128(p, j)
    }
}

fn action_458(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_539, j),
        140 => happy_shift(p, action_184, j),
        150 => happy_shift(p, action_540, j),
        185 => happy_shift(p, action_114, j),
        186 => happy_shift(p, action_173, j),
        188 => happy_shift(p, action_116, j),
        190 => happy_shift(p, action_117, j),
        192 => happy_shift(p, action_118, j),
        193 => happy_shift(p, action_174, j),
        195 => happy_shift(p, action_119, j),
        198 => happy_shift(p, action_120, j),
        200 => happy_shift(p, action_121, j),
        201 => happy_shift(p, action_122, j),
        202 => happy_shift(p, action_123, j),
        203 => happy_shift(p, action_124, j),
        208 => happy_shift(p, action_125, j),
        209 => happy_shift(p, action_126, j),
        210 => happy_shift(p, action_127, j),
        211 => happy_shift(p, action_128, j),
        213 => happy_shift(p, action_129, j),
        214 => happy_shift(p, action_175, j),
        215 => happy_shift(p, action_176, j),
        216 => happy_shift(p, action_130, j),
        217 => happy_shift(p, action_177, j),
        219 => happy_shift(p, action_131, j),
        220 => happy_shift(p, action_132, j),
        222 => happy_shift(p, action_133, j),
        224 => happy_shift(p, action_135, j),
        226 => happy_shift(p, action_136, j),
        227 => happy_shift(p, action_193, j),
        228 => happy_shift(p, action_138, j),
        229 => happy_shift(p, action_139, j),
        230 => happy_shift(p, action_140, j),
        231 => happy_shift(p, action_141, j),
        232 => happy_shift(p, action_178, j),
        238 => happy_shift(p, action_142, j),
        239 => happy_shift(p, action_194, j),
        240 => happy_shift(p, action_144, j),
        40 => happy_goto(p, action_186, j),
        41 => happy_goto(p, action_86, j),
        42 => happy_goto(p, action_87, j),
        43 => happy_goto(p, action_88, j),
        45 => happy_goto(p, action_187, j),
        52 => happy_goto(p, action_188, j),
        53 => happy_goto(p, action_98, j),
        54 => happy_goto(p, action_99, j),
        61 => happy_goto(p, action_100, j),
        64 => happy_goto(p, action_189, j),
        75 => happy_goto(p, action_535, j),
        76 => happy_goto(p, action_103, j),
        77 => happy_goto(p, action_104, j),
        78 => happy_goto(p, action_485, j),
        87 => happy_goto(p, action_536, j),
        88 => happy_goto(p, action_370, j),
        89 => happy_goto(p, action_181, j),
        90 => happy_goto(p, action_182, j),
        91 => happy_goto(p, action_371, j),
        92 => happy_goto(p, action_372, j),
        133 => happy_goto(p, action_537, j),
        134 => happy_goto(p, action_538, j),
        _ => happy_reduce_299(p, j)
    }
}

fn action_459(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        139 => happy_shift(p, action_534, j),
        _ => happy_fail(p, j)
    }
}

fn action_460(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        179 => happy_shift(p, action_533, j),
        _ => happy_reduce_284(p, j)
    }
}

fn action_461(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_286(p, j)
    }
}

fn action_462(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        139 => happy_shift(p, action_531, j),
        179 => happy_shift(p, action_532, j),
        _ => happy_fail(p, j)
    }
}

fn action_463(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        185 => happy_shift(p, action_114, j),
        188 => happy_shift(p, action_116, j),
        190 => happy_shift(p, action_117, j),
        192 => happy_shift(p, action_118, j),
        195 => happy_shift(p, action_119, j),
        198 => happy_shift(p, action_120, j),
        200 => happy_shift(p, action_121, j),
        201 => happy_shift(p, action_122, j),
        202 => happy_shift(p, action_123, j),
        203 => happy_shift(p, action_124, j),
        208 => happy_shift(p, action_125, j),
        209 => happy_shift(p, action_126, j),
        210 => happy_shift(p, action_127, j),
        211 => happy_shift(p, action_128, j),
        213 => happy_shift(p, action_129, j),
        216 => happy_shift(p, action_130, j),
        219 => happy_shift(p, action_131, j),
        220 => happy_shift(p, action_132, j),
        222 => happy_shift(p, action_133, j),
        224 => happy_shift(p, action_135, j),
        226 => happy_shift(p, action_136, j),
        227 => happy_shift(p, action_170, j),
        228 => happy_shift(p, action_138, j),
        229 => happy_shift(p, action_139, j),
        230 => happy_shift(p, action_140, j),
        231 => happy_shift(p, action_141, j),
        239 => happy_shift(p, action_171, j),
        240 => happy_shift(p, action_144, j),
        40 => happy_goto(p, action_164, j),
        41 => happy_goto(p, action_86, j),
        42 => happy_goto(p, action_87, j),
        43 => happy_goto(p, action_88, j),
        45 => happy_goto(p, action_165, j),
        52 => happy_goto(p, action_166, j),
        53 => happy_goto(p, action_98, j),
        54 => happy_goto(p, action_99, j),
        61 => happy_goto(p, action_100, j),
        134 => happy_goto(p, action_169, j),
        _ => happy_reduce_474(p, j)
    }
}

fn action_464(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_303(p, j)
    }
}

fn action_465(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_315(p, j)
    }
}

fn action_466(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_19(p, j)
    }
}

fn action_467(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_91(p, j)
    }
}

fn action_468(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_528, j),
        150 => happy_shift(p, action_529, j),
        238 => happy_shift(p, action_142, j),
        239 => happy_shift(p, action_231, j),
        66 => happy_goto(p, action_242, j),
        68 => happy_goto(p, action_221, j),
        69 => happy_goto(p, action_222, j),
        70 => happy_goto(p, action_223, j),
        71 => happy_goto(p, action_224, j),
        72 => happy_goto(p, action_225, j),
        73 => happy_goto(p, action_226, j),
        75 => happy_goto(p, action_527, j),
        76 => happy_goto(p, action_103, j),
        77 => happy_goto(p, action_104, j),
        78 => happy_goto(p, action_485, j),
        _ => happy_fail(p, j)
    }
}

fn action_469(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_479, j),
        150 => happy_shift(p, action_480, j),
        185 => happy_shift(p, action_114, j),
        186 => happy_shift(p, action_173, j),
        188 => happy_shift(p, action_116, j),
        190 => happy_shift(p, action_117, j),
        192 => happy_shift(p, action_118, j),
        193 => happy_shift(p, action_174, j),
        195 => happy_shift(p, action_119, j),
        198 => happy_shift(p, action_120, j),
        200 => happy_shift(p, action_121, j),
        201 => happy_shift(p, action_122, j),
        202 => happy_shift(p, action_123, j),
        203 => happy_shift(p, action_124, j),
        208 => happy_shift(p, action_125, j),
        209 => happy_shift(p, action_126, j),
        210 => happy_shift(p, action_127, j),
        211 => happy_shift(p, action_128, j),
        213 => happy_shift(p, action_129, j),
        214 => happy_shift(p, action_175, j),
        215 => happy_shift(p, action_176, j),
        216 => happy_shift(p, action_130, j),
        217 => happy_shift(p, action_177, j),
        219 => happy_shift(p, action_131, j),
        220 => happy_shift(p, action_132, j),
        222 => happy_shift(p, action_133, j),
        224 => happy_shift(p, action_135, j),
        226 => happy_shift(p, action_136, j),
        227 => happy_shift(p, action_239, j),
        228 => happy_shift(p, action_138, j),
        229 => happy_shift(p, action_139, j),
        230 => happy_shift(p, action_140, j),
        231 => happy_shift(p, action_141, j),
        232 => happy_shift(p, action_178, j),
        238 => happy_shift(p, action_142, j),
        239 => happy_shift(p, action_240, j),
        240 => happy_shift(p, action_144, j),
        39 => happy_goto(p, action_233, j),
        41 => happy_goto(p, action_200, j),
        42 => happy_goto(p, action_201, j),
        43 => happy_goto(p, action_202, j),
        45 => happy_goto(p, action_234, j),
        52 => happy_goto(p, action_235, j),
        53 => happy_goto(p, action_98, j),
        54 => happy_goto(p, action_99, j),
        61 => happy_goto(p, action_100, j),
        64 => happy_goto(p, action_203, j),
        75 => happy_goto(p, action_530, j),
        76 => happy_goto(p, action_103, j),
        77 => happy_goto(p, action_104, j),
        78 => happy_goto(p, action_485, j),
        134 => happy_goto(p, action_238, j),
        _ => happy_fail(p, j)
    }
}

fn action_470(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_528, j),
        150 => happy_shift(p, action_529, j),
        238 => happy_shift(p, action_142, j),
        239 => happy_shift(p, action_231, j),
        66 => happy_goto(p, action_220, j),
        68 => happy_goto(p, action_221, j),
        69 => happy_goto(p, action_222, j),
        70 => happy_goto(p, action_223, j),
        71 => happy_goto(p, action_224, j),
        72 => happy_goto(p, action_225, j),
        73 => happy_goto(p, action_226, j),
        75 => happy_goto(p, action_527, j),
        76 => happy_goto(p, action_103, j),
        77 => happy_goto(p, action_104, j),
        78 => happy_goto(p, action_485, j),
        _ => happy_fail(p, j)
    }
}

fn action_471(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_479, j),
        150 => happy_shift(p, action_480, j),
        185 => happy_shift(p, action_114, j),
        186 => happy_shift(p, action_173, j),
        188 => happy_shift(p, action_116, j),
        190 => happy_shift(p, action_117, j),
        192 => happy_shift(p, action_118, j),
        193 => happy_shift(p, action_174, j),
        195 => happy_shift(p, action_119, j),
        198 => happy_shift(p, action_120, j),
        200 => happy_shift(p, action_121, j),
        201 => happy_shift(p, action_122, j),
        202 => happy_shift(p, action_123, j),
        203 => happy_shift(p, action_124, j),
        208 => happy_shift(p, action_125, j),
        209 => happy_shift(p, action_126, j),
        210 => happy_shift(p, action_127, j),
        211 => happy_shift(p, action_128, j),
        213 => happy_shift(p, action_129, j),
        214 => happy_shift(p, action_175, j),
        215 => happy_shift(p, action_176, j),
        216 => happy_shift(p, action_130, j),
        217 => happy_shift(p, action_177, j),
        219 => happy_shift(p, action_131, j),
        220 => happy_shift(p, action_132, j),
        222 => happy_shift(p, action_133, j),
        224 => happy_shift(p, action_135, j),
        226 => happy_shift(p, action_136, j),
        227 => happy_shift(p, action_193, j),
        228 => happy_shift(p, action_138, j),
        229 => happy_shift(p, action_139, j),
        230 => happy_shift(p, action_140, j),
        231 => happy_shift(p, action_141, j),
        232 => happy_shift(p, action_178, j),
        238 => happy_shift(p, action_142, j),
        239 => happy_shift(p, action_194, j),
        240 => happy_shift(p, action_144, j),
        40 => happy_goto(p, action_186, j),
        41 => happy_goto(p, action_86, j),
        42 => happy_goto(p, action_87, j),
        43 => happy_goto(p, action_88, j),
        45 => happy_goto(p, action_187, j),
        52 => happy_goto(p, action_188, j),
        53 => happy_goto(p, action_98, j),
        54 => happy_goto(p, action_99, j),
        61 => happy_goto(p, action_100, j),
        64 => happy_goto(p, action_189, j),
        75 => happy_goto(p, action_525, j),
        76 => happy_goto(p, action_103, j),
        77 => happy_goto(p, action_104, j),
        78 => happy_goto(p, action_485, j),
        133 => happy_goto(p, action_526, j),
        134 => happy_goto(p, action_111, j),
        _ => happy_fail(p, j)
    }
}

fn action_472(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_479, j),
        150 => happy_shift(p, action_480, j),
        185 => happy_shift(p, action_114, j),
        188 => happy_shift(p, action_116, j),
        190 => happy_shift(p, action_117, j),
        192 => happy_shift(p, action_118, j),
        195 => happy_shift(p, action_119, j),
        198 => happy_shift(p, action_120, j),
        200 => happy_shift(p, action_121, j),
        201 => happy_shift(p, action_122, j),
        202 => happy_shift(p, action_123, j),
        203 => happy_shift(p, action_124, j),
        208 => happy_shift(p, action_125, j),
        209 => happy_shift(p, action_126, j),
        210 => happy_shift(p, action_127, j),
        211 => happy_shift(p, action_128, j),
        213 => happy_shift(p, action_129, j),
        216 => happy_shift(p, action_130, j),
        219 => happy_shift(p, action_131, j),
        220 => happy_shift(p, action_132, j),
        222 => happy_shift(p, action_133, j),
        224 => happy_shift(p, action_135, j),
        226 => happy_shift(p, action_136, j),
        227 => happy_shift(p, action_170, j),
        228 => happy_shift(p, action_138, j),
        229 => happy_shift(p, action_139, j),
        230 => happy_shift(p, action_140, j),
        231 => happy_shift(p, action_141, j),
        238 => happy_shift(p, action_142, j),
        239 => happy_shift(p, action_171, j),
        240 => happy_shift(p, action_144, j),
        40 => happy_goto(p, action_164, j),
        41 => happy_goto(p, action_86, j),
        42 => happy_goto(p, action_87, j),
        43 => happy_goto(p, action_88, j),
        45 => happy_goto(p, action_165, j),
        52 => happy_goto(p, action_166, j),
        53 => happy_goto(p, action_98, j),
        54 => happy_goto(p, action_99, j),
        61 => happy_goto(p, action_100, j),
        75 => happy_goto(p, action_524, j),
        76 => happy_goto(p, action_103, j),
        77 => happy_goto(p, action_104, j),
        78 => happy_goto(p, action_485, j),
        134 => happy_goto(p, action_169, j),
        _ => happy_reduce_474(p, j)
    }
}

fn action_473(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_26, j),
        144 => happy_shift(p, action_27, j),
        145 => happy_shift(p, action_28, j),
        146 => happy_shift(p, action_29, j),
        147 => happy_shift(p, action_30, j),
        148 => happy_shift(p, action_31, j),
        149 => happy_shift(p, action_32, j),
        150 => happy_shift(p, action_33, j),
        153 => happy_shift(p, action_34, j),
        164 => happy_shift(p, action_35, j),
        184 => happy_shift(p, action_36, j),
        190 => happy_shift(p, action_117, j),
        192 => happy_shift(p, action_118, j),
        195 => happy_shift(p, action_119, j),
        198 => happy_shift(p, action_120, j),
        200 => happy_shift(p, action_121, j),
        202 => happy_shift(p, action_123, j),
        203 => happy_shift(p, action_124, j),
        205 => happy_shift(p, action_37, j),
        209 => happy_shift(p, action_126, j),
        210 => happy_shift(p, action_127, j),
        211 => happy_shift(p, action_128, j),
        219 => happy_shift(p, action_131, j),
        220 => happy_shift(p, action_132, j),
        221 => happy_shift(p, action_38, j),
        224 => happy_shift(p, action_135, j),
        227 => happy_shift(p, action_137, j),
        229 => happy_shift(p, action_139, j),
        230 => happy_shift(p, action_140, j),
        231 => happy_shift(p, action_141, j),
        234 => happy_shift(p, action_39, j),
        235 => happy_shift(p, action_40, j),
        236 => happy_shift(p, action_41, j),
        237 => happy_shift(p, action_42, j),
        238 => happy_shift(p, action_43, j),
        239 => happy_shift(p, action_143, j),
        240 => happy_shift(p, action_144, j),
        241 => happy_shift(p, action_44, j),
        242 => happy_shift(p, action_45, j),
        243 => happy_shift(p, action_46, j),
        244 => happy_shift(p, action_47, j),
        245 => happy_shift(p, action_48, j),
        246 => happy_shift(p, action_49, j),
        247 => happy_shift(p, action_50, j),
        44 => happy_goto(p, action_289, j),
        45 => happy_goto(p, action_90, j),
        47 => happy_goto(p, action_290, j),
        49 => happy_goto(p, action_291, j),
        51 => happy_goto(p, action_292, j),
        52 => happy_goto(p, action_97, j),
        53 => happy_goto(p, action_98, j),
        54 => happy_goto(p, action_99, j),
        61 => happy_goto(p, action_100, j),
        65 => happy_goto(p, action_293, j),
        86 => happy_goto(p, action_522, j),
        100 => happy_goto(p, action_6, j),
        104 => happy_goto(p, action_7, j),
        106 => happy_goto(p, action_8, j),
        107 => happy_goto(p, action_9, j),
        108 => happy_goto(p, action_10, j),
        109 => happy_goto(p, action_11, j),
        110 => happy_goto(p, action_12, j),
        111 => happy_goto(p, action_13, j),
        112 => happy_goto(p, action_14, j),
        113 => happy_goto(p, action_15, j),
        114 => happy_goto(p, action_16, j),
        115 => happy_goto(p, action_17, j),
        116 => happy_goto(p, action_18, j),
        117 => happy_goto(p, action_19, j),
        118 => happy_goto(p, action_20, j),
        119 => happy_goto(p, action_21, j),
        120 => happy_goto(p, action_22, j),
        122 => happy_goto(p, action_523, j),
        127 => happy_goto(p, action_24, j),
        128 => happy_goto(p, action_25, j),
        132 => happy_goto(p, action_109, j),
        133 => happy_goto(p, action_296, j),
        134 => happy_goto(p, action_111, j),
        _ => happy_reduce_473(p, j)
    }
}

fn action_474(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        181 => happy_shift(p, action_62, j),
        185 => happy_shift(p, action_114, j),
        188 => happy_shift(p, action_116, j),
        190 => happy_shift(p, action_117, j),
        192 => happy_shift(p, action_118, j),
        195 => happy_shift(p, action_119, j),
        198 => happy_shift(p, action_120, j),
        200 => happy_shift(p, action_121, j),
        201 => happy_shift(p, action_122, j),
        202 => happy_shift(p, action_123, j),
        203 => happy_shift(p, action_124, j),
        208 => happy_shift(p, action_125, j),
        209 => happy_shift(p, action_126, j),
        210 => happy_shift(p, action_127, j),
        211 => happy_shift(p, action_128, j),
        213 => happy_shift(p, action_129, j),
        216 => happy_shift(p, action_130, j),
        219 => happy_shift(p, action_131, j),
        220 => happy_shift(p, action_132, j),
        222 => happy_shift(p, action_133, j),
        223 => happy_shift(p, action_134, j),
        224 => happy_shift(p, action_135, j),
        226 => happy_shift(p, action_136, j),
        227 => happy_shift(p, action_137, j),
        228 => happy_shift(p, action_138, j),
        229 => happy_shift(p, action_139, j),
        230 => happy_shift(p, action_140, j),
        231 => happy_shift(p, action_141, j),
        239 => happy_shift(p, action_143, j),
        240 => happy_shift(p, action_144, j),
        14 => happy_goto(p, action_521, j),
        32 => happy_goto(p, action_467, j),
        34 => happy_goto(p, action_81, j),
        36 => happy_goto(p, action_82, j),
        37 => happy_goto(p, action_468, j),
        38 => happy_goto(p, action_469, j),
        40 => happy_goto(p, action_85, j),
        41 => happy_goto(p, action_86, j),
        42 => happy_goto(p, action_87, j),
        43 => happy_goto(p, action_88, j),
        44 => happy_goto(p, action_470, j),
        45 => happy_goto(p, action_90, j),
        46 => happy_goto(p, action_91, j),
        47 => happy_goto(p, action_92, j),
        48 => happy_goto(p, action_93, j),
        49 => happy_goto(p, action_94, j),
        50 => happy_goto(p, action_95, j),
        51 => happy_goto(p, action_96, j),
        52 => happy_goto(p, action_97, j),
        53 => happy_goto(p, action_98, j),
        54 => happy_goto(p, action_99, j),
        61 => happy_goto(p, action_100, j),
        65 => happy_goto(p, action_471, j),
        132 => happy_goto(p, action_109, j),
        133 => happy_goto(p, action_472, j),
        134 => happy_goto(p, action_111, j),
        _ => happy_reduce_473(p, j)
    }
}

fn action_475(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        168 => happy_shift(p, action_520, j),
        94 => happy_goto(p, action_519, j),
        _ => happy_reduce_345(p, j)
    }
}

fn action_476(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_13(p, j)
    }
}

fn action_477(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        139 => happy_shift(p, action_518, j),
        _ => happy_fail(p, j)
    }
}

fn action_478(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_417, j),
        139 => happy_shift(p, action_517, j),
        140 => happy_shift(p, action_184, j),
        88 => happy_goto(p, action_180, j),
        89 => happy_goto(p, action_181, j),
        90 => happy_goto(p, action_182, j),
        _ => happy_fail(p, j)
    }
}

fn action_479(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_479, j),
        150 => happy_shift(p, action_480, j),
        238 => happy_shift(p, action_142, j),
        240 => happy_shift(p, action_144, j),
        76 => happy_goto(p, action_159, j),
        77 => happy_goto(p, action_104, j),
        78 => happy_goto(p, action_516, j),
        133 => happy_goto(p, action_162, j),
        134 => happy_goto(p, action_111, j),
        _ => happy_fail(p, j)
    }
}

fn action_480(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_479, j),
        150 => happy_shift(p, action_480, j),
        238 => happy_shift(p, action_142, j),
        240 => happy_shift(p, action_144, j),
        65 => happy_goto(p, action_515, j),
        75 => happy_goto(p, action_156, j),
        76 => happy_goto(p, action_103, j),
        77 => happy_goto(p, action_104, j),
        78 => happy_goto(p, action_485, j),
        132 => happy_goto(p, action_109, j),
        133 => happy_goto(p, action_158, j),
        134 => happy_goto(p, action_111, j),
        _ => happy_reduce_473(p, j)
    }
}

fn action_481(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_417, j),
        140 => happy_shift(p, action_184, j),
        88 => happy_goto(p, action_514, j),
        89 => happy_goto(p, action_181, j),
        90 => happy_goto(p, action_182, j),
        _ => happy_reduce_281(p, j)
    }
}

fn action_482(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_274(p, j)
    }
}

fn action_483(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_417, j),
        140 => happy_shift(p, action_184, j),
        88 => happy_goto(p, action_513, j),
        89 => happy_goto(p, action_181, j),
        90 => happy_goto(p, action_182, j),
        _ => happy_reduce_269(p, j)
    }
}

fn action_484(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_265(p, j)
    }
}

fn action_485(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_417, j),
        140 => happy_shift(p, action_184, j),
        88 => happy_goto(p, action_180, j),
        89 => happy_goto(p, action_181, j),
        90 => happy_goto(p, action_182, j),
        _ => happy_reduce_262(p, j)
    }
}

fn action_486(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_266(p, j)
    }
}

fn action_487(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_279(p, j)
    }
}

fn action_488(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_479, j),
        150 => happy_shift(p, action_480, j),
        186 => happy_shift(p, action_173, j),
        193 => happy_shift(p, action_174, j),
        214 => happy_shift(p, action_175, j),
        215 => happy_shift(p, action_176, j),
        217 => happy_shift(p, action_177, j),
        232 => happy_shift(p, action_178, j),
        238 => happy_shift(p, action_142, j),
        240 => happy_shift(p, action_144, j),
        64 => happy_goto(p, action_439, j),
        75 => happy_goto(p, action_512, j),
        76 => happy_goto(p, action_103, j),
        77 => happy_goto(p, action_104, j),
        78 => happy_goto(p, action_485, j),
        134 => happy_goto(p, action_169, j),
        _ => happy_fail(p, j)
    }
}

fn action_489(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        139 => happy_shift(p, action_511, j),
        _ => happy_fail(p, j)
    }
}

fn action_490(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        139 => happy_shift(p, action_510, j),
        _ => happy_fail(p, j)
    }
}

fn action_491(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        139 => happy_shift(p, action_509, j),
        _ => happy_fail(p, j)
    }
}

fn action_492(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        181 => happy_shift(p, action_508, j),
        _ => happy_reduce_215(p, j)
    }
}

fn action_493(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        238 => happy_shift(p, action_256, j),
        239 => happy_shift(p, action_76, j),
        62 => happy_goto(p, action_505, j),
        63 => happy_goto(p, action_506, j),
        131 => happy_goto(p, action_507, j),
        _ => happy_fail(p, j)
    }
}

fn action_494(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        179 => happy_shift(p, action_504, j),
        _ => happy_fail(p, j)
    }
}

fn action_495(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        139 => happy_shift(p, action_503, j),
        _ => happy_fail(p, j)
    }
}

fn action_496(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        139 => happy_shift(p, action_502, j),
        _ => happy_fail(p, j)
    }
}

fn action_497(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        193 => happy_shift(p, action_500, j),
        238 => happy_shift(p, action_501, j),
        135 => happy_goto(p, action_498, j),
        136 => happy_goto(p, action_499, j),
        _ => happy_reduce_480(p, j)
    }
}

fn action_498(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        139 => happy_shift(p, action_771, j),
        179 => happy_shift(p, action_772, j),
        _ => happy_fail(p, j)
    }
}

fn action_499(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_478(p, j)
    }
}

fn action_500(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_482(p, j)
    }
}

fn action_501(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_770, j),
        _ => happy_reduce_481(p, j)
    }
}

fn action_502(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_172(p, j)
    }
}

fn action_503(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_173(p, j)
    }
}

fn action_504(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        237 => happy_shift(p, action_42, j),
        128 => happy_goto(p, action_769, j),
        _ => happy_fail(p, j)
    }
}

fn action_505(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        179 => happy_shift(p, action_767, j),
        182 => happy_shift(p, action_768, j),
        _ => happy_fail(p, j)
    }
}

fn action_506(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_216(p, j)
    }
}

fn action_507(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        168 => happy_shift(p, action_766, j),
        240 => happy_shift(p, action_144, j),
        133 => happy_goto(p, action_765, j),
        134 => happy_goto(p, action_111, j),
        _ => happy_reduce_218(p, j)
    }
}

fn action_508(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        238 => happy_shift(p, action_256, j),
        239 => happy_shift(p, action_76, j),
        62 => happy_goto(p, action_764, j),
        63 => happy_goto(p, action_506, j),
        131 => happy_goto(p, action_507, j),
        _ => happy_fail(p, j)
    }
}

fn action_509(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        180 => happy_shift(p, action_763, j),
        _ => happy_fail(p, j)
    }
}

fn action_510(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_126(p, j)
    }
}

fn action_511(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_125(p, j)
    }
}

fn action_512(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_267(p, j)
    }
}

fn action_513(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_270(p, j)
    }
}

fn action_514(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_282(p, j)
    }
}

fn action_515(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_479, j),
        150 => happy_shift(p, action_480, j),
        186 => happy_shift(p, action_173, j),
        193 => happy_shift(p, action_174, j),
        214 => happy_shift(p, action_175, j),
        215 => happy_shift(p, action_176, j),
        217 => happy_shift(p, action_177, j),
        232 => happy_shift(p, action_178, j),
        238 => happy_shift(p, action_142, j),
        240 => happy_shift(p, action_144, j),
        64 => happy_goto(p, action_189, j),
        75 => happy_goto(p, action_486, j),
        76 => happy_goto(p, action_103, j),
        77 => happy_goto(p, action_104, j),
        78 => happy_goto(p, action_485, j),
        133 => happy_goto(p, action_488, j),
        134 => happy_goto(p, action_111, j),
        _ => happy_fail(p, j)
    }
}

fn action_516(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_417, j),
        139 => happy_shift(p, action_482, j),
        140 => happy_shift(p, action_184, j),
        88 => happy_goto(p, action_180, j),
        89 => happy_goto(p, action_181, j),
        90 => happy_goto(p, action_182, j),
        _ => happy_fail(p, j)
    }
}

fn action_517(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_275(p, j)
    }
}

fn action_518(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_417, j),
        140 => happy_shift(p, action_184, j),
        88 => happy_goto(p, action_762, j),
        89 => happy_goto(p, action_181, j),
        90 => happy_goto(p, action_182, j),
        _ => happy_reduce_271(p, j)
    }
}

fn action_519(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_95(p, j)
    }
}

fn action_520(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_26, j),
        144 => happy_shift(p, action_27, j),
        145 => happy_shift(p, action_28, j),
        146 => happy_shift(p, action_29, j),
        147 => happy_shift(p, action_30, j),
        148 => happy_shift(p, action_31, j),
        149 => happy_shift(p, action_32, j),
        150 => happy_shift(p, action_33, j),
        153 => happy_shift(p, action_34, j),
        164 => happy_shift(p, action_35, j),
        181 => happy_shift(p, action_659, j),
        184 => happy_shift(p, action_36, j),
        205 => happy_shift(p, action_37, j),
        221 => happy_shift(p, action_38, j),
        234 => happy_shift(p, action_39, j),
        235 => happy_shift(p, action_40, j),
        236 => happy_shift(p, action_41, j),
        237 => happy_shift(p, action_42, j),
        238 => happy_shift(p, action_43, j),
        241 => happy_shift(p, action_44, j),
        242 => happy_shift(p, action_45, j),
        243 => happy_shift(p, action_46, j),
        244 => happy_shift(p, action_47, j),
        245 => happy_shift(p, action_48, j),
        246 => happy_shift(p, action_49, j),
        247 => happy_shift(p, action_50, j),
        93 => happy_goto(p, action_761, j),
        100 => happy_goto(p, action_6, j),
        104 => happy_goto(p, action_7, j),
        106 => happy_goto(p, action_8, j),
        107 => happy_goto(p, action_9, j),
        108 => happy_goto(p, action_10, j),
        109 => happy_goto(p, action_11, j),
        110 => happy_goto(p, action_12, j),
        111 => happy_goto(p, action_13, j),
        112 => happy_goto(p, action_14, j),
        113 => happy_goto(p, action_15, j),
        114 => happy_goto(p, action_16, j),
        115 => happy_goto(p, action_17, j),
        116 => happy_goto(p, action_18, j),
        117 => happy_goto(p, action_19, j),
        118 => happy_goto(p, action_20, j),
        119 => happy_goto(p, action_21, j),
        120 => happy_goto(p, action_655, j),
        127 => happy_goto(p, action_24, j),
        128 => happy_goto(p, action_25, j),
        _ => happy_fail(p, j)
    }
}

fn action_521(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_20(p, j)
    }
}

fn action_522(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        139 => happy_shift(p, action_760, j),
        _ => happy_fail(p, j)
    }
}

fn action_523(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        139 => happy_shift(p, action_759, j),
        _ => happy_fail(p, j)
    }
}

fn action_524(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        187 => happy_shift(p, action_410, j),
        35 => happy_goto(p, action_475, j),
        67 => happy_goto(p, action_409, j),
        _ => happy_reduce_233(p, j)
    }
}

fn action_525(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        187 => happy_shift(p, action_410, j),
        35 => happy_goto(p, action_445, j),
        67 => happy_goto(p, action_409, j),
        _ => happy_reduce_233(p, j)
    }
}

fn action_526(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_479, j),
        150 => happy_shift(p, action_480, j),
        185 => happy_shift(p, action_114, j),
        186 => happy_shift(p, action_173, j),
        188 => happy_shift(p, action_116, j),
        190 => happy_shift(p, action_117, j),
        192 => happy_shift(p, action_118, j),
        193 => happy_shift(p, action_174, j),
        195 => happy_shift(p, action_119, j),
        198 => happy_shift(p, action_120, j),
        200 => happy_shift(p, action_121, j),
        201 => happy_shift(p, action_122, j),
        202 => happy_shift(p, action_123, j),
        203 => happy_shift(p, action_124, j),
        208 => happy_shift(p, action_125, j),
        209 => happy_shift(p, action_126, j),
        210 => happy_shift(p, action_127, j),
        211 => happy_shift(p, action_128, j),
        213 => happy_shift(p, action_129, j),
        214 => happy_shift(p, action_175, j),
        215 => happy_shift(p, action_176, j),
        216 => happy_shift(p, action_130, j),
        217 => happy_shift(p, action_177, j),
        219 => happy_shift(p, action_131, j),
        220 => happy_shift(p, action_132, j),
        222 => happy_shift(p, action_133, j),
        224 => happy_shift(p, action_135, j),
        226 => happy_shift(p, action_136, j),
        227 => happy_shift(p, action_442, j),
        228 => happy_shift(p, action_138, j),
        229 => happy_shift(p, action_139, j),
        230 => happy_shift(p, action_140, j),
        231 => happy_shift(p, action_141, j),
        232 => happy_shift(p, action_178, j),
        238 => happy_shift(p, action_142, j),
        239 => happy_shift(p, action_443, j),
        240 => happy_shift(p, action_144, j),
        40 => happy_goto(p, action_436, j),
        41 => happy_goto(p, action_86, j),
        42 => happy_goto(p, action_87, j),
        43 => happy_goto(p, action_88, j),
        45 => happy_goto(p, action_437, j),
        52 => happy_goto(p, action_438, j),
        53 => happy_goto(p, action_98, j),
        54 => happy_goto(p, action_99, j),
        61 => happy_goto(p, action_100, j),
        64 => happy_goto(p, action_439, j),
        75 => happy_goto(p, action_758, j),
        76 => happy_goto(p, action_103, j),
        77 => happy_goto(p, action_104, j),
        78 => happy_goto(p, action_485, j),
        134 => happy_goto(p, action_169, j),
        _ => happy_fail(p, j)
    }
}

fn action_527(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_231(p, j)
    }
}

fn action_528(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_757, j),
        150 => happy_shift(p, action_529, j),
        238 => happy_shift(p, action_142, j),
        239 => happy_shift(p, action_428, j),
        240 => happy_shift(p, action_144, j),
        70 => happy_goto(p, action_423, j),
        71 => happy_goto(p, action_224, j),
        72 => happy_goto(p, action_424, j),
        73 => happy_goto(p, action_226, j),
        74 => happy_goto(p, action_425, j),
        76 => happy_goto(p, action_159, j),
        77 => happy_goto(p, action_104, j),
        78 => happy_goto(p, action_516, j),
        133 => happy_goto(p, action_426, j),
        134 => happy_goto(p, action_111, j),
        _ => happy_fail(p, j)
    }
}

fn action_529(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_756, j),
        150 => happy_shift(p, action_529, j),
        238 => happy_shift(p, action_142, j),
        239 => happy_shift(p, action_231, j),
        240 => happy_shift(p, action_144, j),
        65 => happy_goto(p, action_755, j),
        69 => happy_goto(p, action_419, j),
        70 => happy_goto(p, action_223, j),
        71 => happy_goto(p, action_224, j),
        72 => happy_goto(p, action_420, j),
        73 => happy_goto(p, action_226, j),
        75 => happy_goto(p, action_156, j),
        76 => happy_goto(p, action_103, j),
        77 => happy_goto(p, action_104, j),
        78 => happy_goto(p, action_485, j),
        132 => happy_goto(p, action_109, j),
        133 => happy_goto(p, action_421, j),
        134 => happy_goto(p, action_111, j),
        _ => happy_reduce_473(p, j)
    }
}

fn action_530(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        187 => happy_shift(p, action_410, j),
        35 => happy_goto(p, action_414, j),
        67 => happy_goto(p, action_409, j),
        _ => happy_reduce_233(p, j)
    }
}

fn action_531(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_280(p, j)
    }
}

fn action_532(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        238 => happy_shift(p, action_754, j),
        _ => happy_fail(p, j)
    }
}

fn action_533(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        183 => happy_shift(p, action_753, j),
        185 => happy_shift(p, action_114, j),
        188 => happy_shift(p, action_116, j),
        190 => happy_shift(p, action_117, j),
        192 => happy_shift(p, action_118, j),
        195 => happy_shift(p, action_119, j),
        198 => happy_shift(p, action_120, j),
        200 => happy_shift(p, action_121, j),
        201 => happy_shift(p, action_122, j),
        202 => happy_shift(p, action_123, j),
        203 => happy_shift(p, action_124, j),
        208 => happy_shift(p, action_125, j),
        209 => happy_shift(p, action_126, j),
        210 => happy_shift(p, action_127, j),
        211 => happy_shift(p, action_128, j),
        213 => happy_shift(p, action_129, j),
        216 => happy_shift(p, action_130, j),
        219 => happy_shift(p, action_131, j),
        220 => happy_shift(p, action_132, j),
        222 => happy_shift(p, action_133, j),
        224 => happy_shift(p, action_135, j),
        226 => happy_shift(p, action_136, j),
        227 => happy_shift(p, action_137, j),
        228 => happy_shift(p, action_138, j),
        229 => happy_shift(p, action_139, j),
        230 => happy_shift(p, action_140, j),
        231 => happy_shift(p, action_141, j),
        239 => happy_shift(p, action_143, j),
        240 => happy_shift(p, action_144, j),
        37 => happy_goto(p, action_453, j),
        38 => happy_goto(p, action_454, j),
        40 => happy_goto(p, action_85, j),
        41 => happy_goto(p, action_86, j),
        42 => happy_goto(p, action_87, j),
        43 => happy_goto(p, action_88, j),
        44 => happy_goto(p, action_455, j),
        45 => happy_goto(p, action_90, j),
        46 => happy_goto(p, action_91, j),
        47 => happy_goto(p, action_92, j),
        48 => happy_goto(p, action_456, j),
        49 => happy_goto(p, action_457, j),
        50 => happy_goto(p, action_95, j),
        51 => happy_goto(p, action_96, j),
        52 => happy_goto(p, action_97, j),
        53 => happy_goto(p, action_98, j),
        54 => happy_goto(p, action_99, j),
        61 => happy_goto(p, action_100, j),
        65 => happy_goto(p, action_458, j),
        84 => happy_goto(p, action_752, j),
        132 => happy_goto(p, action_109, j),
        133 => happy_goto(p, action_463, j),
        134 => happy_goto(p, action_111, j),
        _ => happy_reduce_473(p, j)
    }
}

fn action_534(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_313(p, j)
    }
}

fn action_535(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        240 => happy_shift(p, action_144, j),
        132 => happy_goto(p, action_751, j),
        133 => happy_goto(p, action_152, j),
        134 => happy_goto(p, action_111, j),
        _ => happy_reduce_473(p, j)
    }
}

fn action_536(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_301(p, j)
    }
}

fn action_537(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        185 => happy_shift(p, action_114, j),
        186 => happy_shift(p, action_173, j),
        188 => happy_shift(p, action_116, j),
        190 => happy_shift(p, action_117, j),
        192 => happy_shift(p, action_118, j),
        193 => happy_shift(p, action_174, j),
        195 => happy_shift(p, action_119, j),
        198 => happy_shift(p, action_120, j),
        200 => happy_shift(p, action_121, j),
        201 => happy_shift(p, action_122, j),
        202 => happy_shift(p, action_123, j),
        203 => happy_shift(p, action_124, j),
        208 => happy_shift(p, action_125, j),
        209 => happy_shift(p, action_126, j),
        210 => happy_shift(p, action_127, j),
        211 => happy_shift(p, action_128, j),
        213 => happy_shift(p, action_129, j),
        214 => happy_shift(p, action_175, j),
        215 => happy_shift(p, action_176, j),
        216 => happy_shift(p, action_130, j),
        217 => happy_shift(p, action_177, j),
        219 => happy_shift(p, action_131, j),
        220 => happy_shift(p, action_132, j),
        222 => happy_shift(p, action_133, j),
        224 => happy_shift(p, action_135, j),
        226 => happy_shift(p, action_136, j),
        227 => happy_shift(p, action_442, j),
        228 => happy_shift(p, action_138, j),
        229 => happy_shift(p, action_139, j),
        230 => happy_shift(p, action_140, j),
        231 => happy_shift(p, action_141, j),
        232 => happy_shift(p, action_178, j),
        239 => happy_shift(p, action_443, j),
        240 => happy_shift(p, action_144, j),
        40 => happy_goto(p, action_436, j),
        41 => happy_goto(p, action_86, j),
        42 => happy_goto(p, action_87, j),
        43 => happy_goto(p, action_88, j),
        45 => happy_goto(p, action_437, j),
        52 => happy_goto(p, action_438, j),
        53 => happy_goto(p, action_98, j),
        54 => happy_goto(p, action_99, j),
        61 => happy_goto(p, action_100, j),
        64 => happy_goto(p, action_439, j),
        134 => happy_goto(p, action_169, j),
        _ => happy_fail(p, j)
    }
}

fn action_538(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        185 => happy_reduce_475(p, j),
        186 => happy_reduce_475(p, j),
        188 => happy_reduce_475(p, j),
        190 => happy_reduce_475(p, j),
        192 => happy_reduce_475(p, j),
        193 => happy_reduce_475(p, j),
        195 => happy_reduce_475(p, j),
        198 => happy_reduce_475(p, j),
        200 => happy_reduce_475(p, j),
        201 => happy_reduce_475(p, j),
        202 => happy_reduce_475(p, j),
        203 => happy_reduce_475(p, j),
        208 => happy_reduce_475(p, j),
        209 => happy_reduce_475(p, j),
        210 => happy_reduce_475(p, j),
        211 => happy_reduce_475(p, j),
        213 => happy_reduce_475(p, j),
        214 => happy_reduce_475(p, j),
        215 => happy_reduce_475(p, j),
        216 => happy_reduce_475(p, j),
        217 => happy_reduce_475(p, j),
        219 => happy_reduce_475(p, j),
        220 => happy_reduce_475(p, j),
        222 => happy_reduce_475(p, j),
        224 => happy_reduce_475(p, j),
        226 => happy_reduce_475(p, j),
        227 => happy_reduce_475(p, j),
        228 => happy_reduce_475(p, j),
        229 => happy_reduce_475(p, j),
        230 => happy_reduce_475(p, j),
        231 => happy_reduce_475(p, j),
        232 => happy_reduce_475(p, j),
        239 => happy_reduce_475(p, j),
        240 => happy_reduce_475(p, j),
        _ => happy_reduce_300(p, j)
    }
}

fn action_539(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_539, j),
        140 => happy_shift(p, action_184, j),
        150 => happy_shift(p, action_540, j),
        185 => happy_shift(p, action_114, j),
        186 => happy_reduce_473(p, j),
        188 => happy_shift(p, action_116, j),
        190 => happy_shift(p, action_117, j),
        192 => happy_shift(p, action_118, j),
        193 => happy_reduce_473(p, j),
        195 => happy_shift(p, action_119, j),
        198 => happy_shift(p, action_120, j),
        200 => happy_shift(p, action_121, j),
        201 => happy_shift(p, action_122, j),
        202 => happy_shift(p, action_123, j),
        203 => happy_shift(p, action_124, j),
        208 => happy_shift(p, action_125, j),
        209 => happy_shift(p, action_126, j),
        210 => happy_shift(p, action_127, j),
        211 => happy_shift(p, action_128, j),
        213 => happy_shift(p, action_129, j),
        214 => happy_reduce_473(p, j),
        215 => happy_reduce_473(p, j),
        216 => happy_shift(p, action_130, j),
        217 => happy_reduce_473(p, j),
        219 => happy_shift(p, action_131, j),
        220 => happy_shift(p, action_132, j),
        222 => happy_shift(p, action_133, j),
        224 => happy_shift(p, action_135, j),
        226 => happy_shift(p, action_136, j),
        227 => happy_shift(p, action_137, j),
        228 => happy_shift(p, action_138, j),
        229 => happy_shift(p, action_139, j),
        230 => happy_shift(p, action_140, j),
        231 => happy_shift(p, action_141, j),
        232 => happy_reduce_473(p, j),
        238 => happy_shift(p, action_142, j),
        239 => happy_shift(p, action_143, j),
        240 => happy_shift(p, action_144, j),
        37 => happy_goto(p, action_453, j),
        38 => happy_goto(p, action_454, j),
        40 => happy_goto(p, action_85, j),
        41 => happy_goto(p, action_86, j),
        42 => happy_goto(p, action_87, j),
        43 => happy_goto(p, action_88, j),
        44 => happy_goto(p, action_455, j),
        45 => happy_goto(p, action_90, j),
        46 => happy_goto(p, action_91, j),
        47 => happy_goto(p, action_92, j),
        48 => happy_goto(p, action_456, j),
        49 => happy_goto(p, action_457, j),
        50 => happy_goto(p, action_95, j),
        51 => happy_goto(p, action_96, j),
        52 => happy_goto(p, action_97, j),
        53 => happy_goto(p, action_98, j),
        54 => happy_goto(p, action_99, j),
        61 => happy_goto(p, action_100, j),
        65 => happy_goto(p, action_458, j),
        76 => happy_goto(p, action_159, j),
        77 => happy_goto(p, action_104, j),
        78 => happy_goto(p, action_516, j),
        82 => happy_goto(p, action_459, j),
        83 => happy_goto(p, action_460, j),
        84 => happy_goto(p, action_461, j),
        88 => happy_goto(p, action_633, j),
        89 => happy_goto(p, action_181, j),
        90 => happy_goto(p, action_182, j),
        91 => happy_goto(p, action_634, j),
        92 => happy_goto(p, action_635, j),
        132 => happy_goto(p, action_109, j),
        133 => happy_goto(p, action_750, j),
        134 => happy_goto(p, action_111, j),
        _ => happy_reduce_283(p, j)
    }
}

fn action_540(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_539, j),
        140 => happy_shift(p, action_184, j),
        150 => happy_shift(p, action_540, j),
        186 => happy_reduce_473(p, j),
        193 => happy_reduce_473(p, j),
        214 => happy_reduce_473(p, j),
        215 => happy_reduce_473(p, j),
        217 => happy_reduce_473(p, j),
        232 => happy_reduce_473(p, j),
        238 => happy_shift(p, action_142, j),
        240 => happy_shift(p, action_144, j),
        65 => happy_goto(p, action_748, j),
        75 => happy_goto(p, action_156, j),
        76 => happy_goto(p, action_103, j),
        77 => happy_goto(p, action_104, j),
        78 => happy_goto(p, action_485, j),
        87 => happy_goto(p, action_631, j),
        88 => happy_goto(p, action_370, j),
        89 => happy_goto(p, action_181, j),
        90 => happy_goto(p, action_182, j),
        91 => happy_goto(p, action_371, j),
        92 => happy_goto(p, action_372, j),
        132 => happy_goto(p, action_109, j),
        133 => happy_goto(p, action_749, j),
        134 => happy_goto(p, action_111, j),
        _ => happy_reduce_327(p, j)
    }
}

fn action_541(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        240 => happy_shift(p, action_144, j),
        132 => happy_goto(p, action_747, j),
        133 => happy_goto(p, action_152, j),
        134 => happy_goto(p, action_111, j),
        _ => happy_reduce_473(p, j)
    }
}

fn action_542(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        240 => happy_shift(p, action_144, j),
        132 => happy_goto(p, action_746, j),
        133 => happy_goto(p, action_152, j),
        134 => happy_goto(p, action_111, j),
        _ => happy_reduce_473(p, j)
    }
}

fn action_543(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_296(p, j)
    }
}

fn action_544(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_544, j),
        140 => happy_shift(p, action_184, j),
        150 => happy_shift(p, action_545, j),
        185 => happy_shift(p, action_114, j),
        186 => happy_reduce_473(p, j),
        188 => happy_shift(p, action_116, j),
        190 => happy_shift(p, action_117, j),
        192 => happy_shift(p, action_118, j),
        193 => happy_reduce_473(p, j),
        195 => happy_shift(p, action_119, j),
        198 => happy_shift(p, action_120, j),
        200 => happy_shift(p, action_121, j),
        201 => happy_shift(p, action_122, j),
        202 => happy_shift(p, action_123, j),
        203 => happy_shift(p, action_124, j),
        208 => happy_shift(p, action_125, j),
        209 => happy_shift(p, action_126, j),
        210 => happy_shift(p, action_127, j),
        211 => happy_shift(p, action_128, j),
        213 => happy_shift(p, action_129, j),
        214 => happy_reduce_473(p, j),
        215 => happy_reduce_473(p, j),
        216 => happy_shift(p, action_130, j),
        217 => happy_reduce_473(p, j),
        219 => happy_shift(p, action_131, j),
        220 => happy_shift(p, action_132, j),
        222 => happy_shift(p, action_133, j),
        224 => happy_shift(p, action_135, j),
        226 => happy_shift(p, action_136, j),
        227 => happy_shift(p, action_137, j),
        228 => happy_shift(p, action_138, j),
        229 => happy_shift(p, action_139, j),
        230 => happy_shift(p, action_140, j),
        231 => happy_shift(p, action_141, j),
        232 => happy_reduce_473(p, j),
        238 => happy_shift(p, action_142, j),
        239 => happy_shift(p, action_143, j),
        240 => happy_shift(p, action_144, j),
        37 => happy_goto(p, action_453, j),
        38 => happy_goto(p, action_454, j),
        40 => happy_goto(p, action_85, j),
        41 => happy_goto(p, action_86, j),
        42 => happy_goto(p, action_87, j),
        43 => happy_goto(p, action_88, j),
        44 => happy_goto(p, action_455, j),
        45 => happy_goto(p, action_90, j),
        46 => happy_goto(p, action_91, j),
        47 => happy_goto(p, action_92, j),
        48 => happy_goto(p, action_456, j),
        49 => happy_goto(p, action_457, j),
        50 => happy_goto(p, action_95, j),
        51 => happy_goto(p, action_96, j),
        52 => happy_goto(p, action_97, j),
        53 => happy_goto(p, action_98, j),
        54 => happy_goto(p, action_99, j),
        61 => happy_goto(p, action_100, j),
        65 => happy_goto(p, action_458, j),
        70 => happy_goto(p, action_423, j),
        71 => happy_goto(p, action_224, j),
        76 => happy_goto(p, action_159, j),
        77 => happy_goto(p, action_104, j),
        78 => happy_goto(p, action_516, j),
        82 => happy_goto(p, action_459, j),
        83 => happy_goto(p, action_460, j),
        84 => happy_goto(p, action_461, j),
        88 => happy_goto(p, action_633, j),
        89 => happy_goto(p, action_181, j),
        90 => happy_goto(p, action_182, j),
        91 => happy_goto(p, action_634, j),
        92 => happy_goto(p, action_635, j),
        132 => happy_goto(p, action_109, j),
        133 => happy_goto(p, action_745, j),
        134 => happy_goto(p, action_111, j),
        _ => happy_reduce_283(p, j)
    }
}

fn action_545(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_544, j),
        140 => happy_shift(p, action_184, j),
        150 => happy_shift(p, action_545, j),
        186 => happy_reduce_473(p, j),
        193 => happy_reduce_473(p, j),
        214 => happy_reduce_473(p, j),
        215 => happy_reduce_473(p, j),
        217 => happy_reduce_473(p, j),
        232 => happy_reduce_473(p, j),
        238 => happy_shift(p, action_142, j),
        239 => happy_shift(p, action_231, j),
        240 => happy_shift(p, action_144, j),
        65 => happy_goto(p, action_743, j),
        69 => happy_goto(p, action_419, j),
        70 => happy_goto(p, action_223, j),
        71 => happy_goto(p, action_224, j),
        75 => happy_goto(p, action_156, j),
        76 => happy_goto(p, action_103, j),
        77 => happy_goto(p, action_104, j),
        78 => happy_goto(p, action_485, j),
        87 => happy_goto(p, action_631, j),
        88 => happy_goto(p, action_370, j),
        89 => happy_goto(p, action_181, j),
        90 => happy_goto(p, action_182, j),
        91 => happy_goto(p, action_371, j),
        92 => happy_goto(p, action_372, j),
        132 => happy_goto(p, action_109, j),
        133 => happy_goto(p, action_744, j),
        134 => happy_goto(p, action_111, j),
        _ => happy_reduce_327(p, j)
    }
}

fn action_546(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        240 => happy_shift(p, action_144, j),
        132 => happy_goto(p, action_742, j),
        133 => happy_goto(p, action_152, j),
        134 => happy_goto(p, action_111, j),
        _ => happy_reduce_473(p, j)
    }
}

fn action_547(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_293(p, j)
    }
}

fn action_548(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        240 => happy_shift(p, action_144, j),
        132 => happy_goto(p, action_741, j),
        133 => happy_goto(p, action_152, j),
        134 => happy_goto(p, action_111, j),
        _ => happy_reduce_473(p, j)
    }
}

fn action_549(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        240 => happy_shift(p, action_144, j),
        132 => happy_goto(p, action_740, j),
        133 => happy_goto(p, action_152, j),
        134 => happy_goto(p, action_111, j),
        _ => happy_reduce_473(p, j)
    }
}

fn action_550(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_289(p, j)
    }
}

fn action_551(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        186 => happy_shift(p, action_173, j),
        193 => happy_shift(p, action_174, j),
        214 => happy_shift(p, action_175, j),
        215 => happy_shift(p, action_176, j),
        217 => happy_shift(p, action_177, j),
        232 => happy_shift(p, action_178, j),
        240 => happy_shift(p, action_144, j),
        64 => happy_goto(p, action_189, j),
        132 => happy_goto(p, action_739, j),
        133 => happy_goto(p, action_669, j),
        134 => happy_goto(p, action_111, j),
        _ => happy_reduce_473(p, j)
    }
}

fn action_552(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_26, j),
        144 => happy_shift(p, action_27, j),
        145 => happy_shift(p, action_28, j),
        146 => happy_shift(p, action_29, j),
        147 => happy_shift(p, action_30, j),
        148 => happy_shift(p, action_31, j),
        149 => happy_shift(p, action_32, j),
        150 => happy_shift(p, action_33, j),
        153 => happy_shift(p, action_34, j),
        164 => happy_shift(p, action_35, j),
        184 => happy_shift(p, action_36, j),
        186 => happy_shift(p, action_173, j),
        193 => happy_shift(p, action_174, j),
        205 => happy_shift(p, action_37, j),
        214 => happy_shift(p, action_175, j),
        215 => happy_shift(p, action_176, j),
        217 => happy_shift(p, action_177, j),
        221 => happy_shift(p, action_38, j),
        232 => happy_shift(p, action_178, j),
        234 => happy_shift(p, action_39, j),
        235 => happy_shift(p, action_40, j),
        236 => happy_shift(p, action_41, j),
        237 => happy_shift(p, action_42, j),
        238 => happy_shift(p, action_43, j),
        241 => happy_shift(p, action_44, j),
        242 => happy_shift(p, action_45, j),
        243 => happy_shift(p, action_46, j),
        244 => happy_shift(p, action_47, j),
        245 => happy_shift(p, action_48, j),
        246 => happy_shift(p, action_49, j),
        247 => happy_shift(p, action_50, j),
        64 => happy_goto(p, action_172, j),
        100 => happy_goto(p, action_6, j),
        104 => happy_goto(p, action_7, j),
        106 => happy_goto(p, action_8, j),
        107 => happy_goto(p, action_9, j),
        108 => happy_goto(p, action_10, j),
        109 => happy_goto(p, action_11, j),
        110 => happy_goto(p, action_12, j),
        111 => happy_goto(p, action_13, j),
        112 => happy_goto(p, action_14, j),
        113 => happy_goto(p, action_15, j),
        114 => happy_goto(p, action_16, j),
        115 => happy_goto(p, action_17, j),
        116 => happy_goto(p, action_18, j),
        117 => happy_goto(p, action_19, j),
        118 => happy_goto(p, action_20, j),
        119 => happy_goto(p, action_21, j),
        120 => happy_goto(p, action_738, j),
        127 => happy_goto(p, action_24, j),
        128 => happy_goto(p, action_25, j),
        _ => happy_fail(p, j)
    }
}

fn action_553(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        141 => happy_shift(p, action_737, j),
        _ => happy_fail(p, j)
    }
}

fn action_554(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        141 => happy_shift(p, action_736, j),
        _ => happy_fail(p, j)
    }
}

fn action_555(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        141 => happy_reduce_473(p, j),
        240 => happy_shift(p, action_144, j),
        132 => happy_goto(p, action_735, j),
        133 => happy_goto(p, action_152, j),
        134 => happy_goto(p, action_111, j),
        _ => happy_reduce_403(p, j)
    }
}

fn action_556(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_316(p, j)
    }
}

fn action_557(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        141 => happy_shift(p, action_734, j),
        _ => happy_fail(p, j)
    }
}

fn action_558(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        222 => happy_shift(p, action_733, j),
        _ => happy_fail(p, j)
    }
}

fn action_559(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_26, j),
        144 => happy_shift(p, action_27, j),
        145 => happy_shift(p, action_28, j),
        146 => happy_shift(p, action_29, j),
        147 => happy_shift(p, action_30, j),
        148 => happy_shift(p, action_31, j),
        149 => happy_shift(p, action_32, j),
        150 => happy_shift(p, action_732, j),
        153 => happy_shift(p, action_34, j),
        164 => happy_shift(p, action_35, j),
        184 => happy_shift(p, action_36, j),
        186 => happy_shift(p, action_173, j),
        193 => happy_shift(p, action_174, j),
        205 => happy_shift(p, action_37, j),
        214 => happy_shift(p, action_175, j),
        215 => happy_shift(p, action_176, j),
        217 => happy_shift(p, action_177, j),
        221 => happy_shift(p, action_38, j),
        222 => happy_reduce_474(p, j),
        232 => happy_shift(p, action_178, j),
        234 => happy_shift(p, action_39, j),
        235 => happy_shift(p, action_40, j),
        236 => happy_shift(p, action_41, j),
        237 => happy_shift(p, action_42, j),
        238 => happy_shift(p, action_43, j),
        240 => happy_shift(p, action_144, j),
        241 => happy_shift(p, action_44, j),
        242 => happy_shift(p, action_45, j),
        243 => happy_shift(p, action_46, j),
        244 => happy_shift(p, action_47, j),
        245 => happy_shift(p, action_48, j),
        246 => happy_shift(p, action_49, j),
        247 => happy_shift(p, action_50, j),
        64 => happy_goto(p, action_439, j),
        100 => happy_goto(p, action_6, j),
        104 => happy_goto(p, action_7, j),
        106 => happy_goto(p, action_8, j),
        107 => happy_goto(p, action_9, j),
        108 => happy_goto(p, action_10, j),
        109 => happy_goto(p, action_11, j),
        110 => happy_goto(p, action_12, j),
        111 => happy_goto(p, action_13, j),
        112 => happy_goto(p, action_14, j),
        113 => happy_goto(p, action_15, j),
        114 => happy_goto(p, action_16, j),
        115 => happy_goto(p, action_17, j),
        116 => happy_goto(p, action_18, j),
        117 => happy_goto(p, action_19, j),
        118 => happy_goto(p, action_20, j),
        119 => happy_goto(p, action_21, j),
        120 => happy_goto(p, action_448, j),
        125 => happy_goto(p, action_731, j),
        127 => happy_goto(p, action_24, j),
        128 => happy_goto(p, action_25, j),
        134 => happy_goto(p, action_169, j),
        _ => happy_reduce_460(p, j)
    }
}

fn action_560(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        141 => happy_reduce_473(p, j),
        240 => happy_shift(p, action_144, j),
        132 => happy_goto(p, action_730, j),
        133 => happy_goto(p, action_152, j),
        134 => happy_goto(p, action_111, j),
        _ => happy_reduce_403(p, j)
    }
}

fn action_561(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_93(p, j)
    }
}

fn action_562(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_24(p, j)
    }
}

fn action_563(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_26, j),
        144 => happy_shift(p, action_27, j),
        145 => happy_shift(p, action_28, j),
        146 => happy_shift(p, action_29, j),
        147 => happy_shift(p, action_30, j),
        148 => happy_shift(p, action_31, j),
        149 => happy_shift(p, action_32, j),
        150 => happy_shift(p, action_33, j),
        153 => happy_shift(p, action_34, j),
        164 => happy_shift(p, action_35, j),
        184 => happy_shift(p, action_36, j),
        190 => happy_shift(p, action_117, j),
        192 => happy_shift(p, action_118, j),
        195 => happy_shift(p, action_119, j),
        198 => happy_shift(p, action_120, j),
        200 => happy_shift(p, action_121, j),
        202 => happy_shift(p, action_123, j),
        203 => happy_shift(p, action_124, j),
        205 => happy_shift(p, action_37, j),
        209 => happy_shift(p, action_126, j),
        210 => happy_shift(p, action_127, j),
        211 => happy_shift(p, action_128, j),
        219 => happy_shift(p, action_131, j),
        220 => happy_shift(p, action_132, j),
        221 => happy_shift(p, action_38, j),
        224 => happy_shift(p, action_135, j),
        227 => happy_shift(p, action_137, j),
        229 => happy_shift(p, action_139, j),
        230 => happy_shift(p, action_140, j),
        231 => happy_shift(p, action_141, j),
        234 => happy_shift(p, action_39, j),
        235 => happy_shift(p, action_40, j),
        236 => happy_shift(p, action_41, j),
        237 => happy_shift(p, action_42, j),
        238 => happy_shift(p, action_43, j),
        239 => happy_shift(p, action_143, j),
        240 => happy_shift(p, action_144, j),
        241 => happy_shift(p, action_44, j),
        242 => happy_shift(p, action_45, j),
        243 => happy_shift(p, action_46, j),
        244 => happy_shift(p, action_47, j),
        245 => happy_shift(p, action_48, j),
        246 => happy_shift(p, action_49, j),
        247 => happy_shift(p, action_50, j),
        44 => happy_goto(p, action_289, j),
        45 => happy_goto(p, action_90, j),
        47 => happy_goto(p, action_290, j),
        49 => happy_goto(p, action_291, j),
        51 => happy_goto(p, action_292, j),
        52 => happy_goto(p, action_97, j),
        53 => happy_goto(p, action_98, j),
        54 => happy_goto(p, action_99, j),
        61 => happy_goto(p, action_100, j),
        65 => happy_goto(p, action_293, j),
        86 => happy_goto(p, action_728, j),
        100 => happy_goto(p, action_6, j),
        104 => happy_goto(p, action_7, j),
        106 => happy_goto(p, action_8, j),
        107 => happy_goto(p, action_9, j),
        108 => happy_goto(p, action_10, j),
        109 => happy_goto(p, action_11, j),
        110 => happy_goto(p, action_12, j),
        111 => happy_goto(p, action_13, j),
        112 => happy_goto(p, action_14, j),
        113 => happy_goto(p, action_15, j),
        114 => happy_goto(p, action_16, j),
        115 => happy_goto(p, action_17, j),
        116 => happy_goto(p, action_18, j),
        117 => happy_goto(p, action_19, j),
        118 => happy_goto(p, action_20, j),
        119 => happy_goto(p, action_21, j),
        120 => happy_goto(p, action_22, j),
        122 => happy_goto(p, action_729, j),
        127 => happy_goto(p, action_24, j),
        128 => happy_goto(p, action_25, j),
        132 => happy_goto(p, action_109, j),
        133 => happy_goto(p, action_296, j),
        134 => happy_goto(p, action_111, j),
        _ => happy_reduce_473(p, j)
    }
}

fn action_564(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        181 => happy_shift(p, action_62, j),
        185 => happy_shift(p, action_114, j),
        188 => happy_shift(p, action_116, j),
        190 => happy_shift(p, action_117, j),
        192 => happy_shift(p, action_118, j),
        195 => happy_shift(p, action_119, j),
        198 => happy_shift(p, action_120, j),
        200 => happy_shift(p, action_121, j),
        201 => happy_shift(p, action_122, j),
        202 => happy_shift(p, action_123, j),
        203 => happy_shift(p, action_124, j),
        208 => happy_shift(p, action_125, j),
        209 => happy_shift(p, action_126, j),
        210 => happy_shift(p, action_127, j),
        211 => happy_shift(p, action_128, j),
        213 => happy_shift(p, action_129, j),
        216 => happy_shift(p, action_130, j),
        219 => happy_shift(p, action_131, j),
        220 => happy_shift(p, action_132, j),
        222 => happy_shift(p, action_133, j),
        223 => happy_shift(p, action_134, j),
        224 => happy_shift(p, action_135, j),
        226 => happy_shift(p, action_136, j),
        227 => happy_shift(p, action_137, j),
        228 => happy_shift(p, action_138, j),
        229 => happy_shift(p, action_139, j),
        230 => happy_shift(p, action_140, j),
        231 => happy_shift(p, action_141, j),
        239 => happy_shift(p, action_143, j),
        240 => happy_shift(p, action_144, j),
        14 => happy_goto(p, action_727, j),
        32 => happy_goto(p, action_467, j),
        34 => happy_goto(p, action_81, j),
        36 => happy_goto(p, action_82, j),
        37 => happy_goto(p, action_468, j),
        38 => happy_goto(p, action_469, j),
        40 => happy_goto(p, action_85, j),
        41 => happy_goto(p, action_86, j),
        42 => happy_goto(p, action_87, j),
        43 => happy_goto(p, action_88, j),
        44 => happy_goto(p, action_470, j),
        45 => happy_goto(p, action_90, j),
        46 => happy_goto(p, action_91, j),
        47 => happy_goto(p, action_92, j),
        48 => happy_goto(p, action_93, j),
        49 => happy_goto(p, action_94, j),
        50 => happy_goto(p, action_95, j),
        51 => happy_goto(p, action_96, j),
        52 => happy_goto(p, action_97, j),
        53 => happy_goto(p, action_98, j),
        54 => happy_goto(p, action_99, j),
        61 => happy_goto(p, action_100, j),
        65 => happy_goto(p, action_471, j),
        132 => happy_goto(p, action_109, j),
        133 => happy_goto(p, action_472, j),
        134 => happy_goto(p, action_111, j),
        _ => happy_reduce_473(p, j)
    }
}

fn action_565(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        168 => happy_shift(p, action_520, j),
        94 => happy_goto(p, action_726, j),
        _ => happy_reduce_345(p, j)
    }
}

fn action_566(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_18(p, j)
    }
}

fn action_567(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        139 => happy_shift(p, action_725, j),
        _ => happy_fail(p, j)
    }
}

fn action_568(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        139 => happy_shift(p, action_724, j),
        _ => happy_fail(p, j)
    }
}

fn action_569(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        180 => happy_shift(p, action_721, j),
        182 => happy_shift(p, action_722, j),
        190 => happy_shift(p, action_117, j),
        192 => happy_shift(p, action_118, j),
        195 => happy_shift(p, action_119, j),
        198 => happy_shift(p, action_120, j),
        200 => happy_shift(p, action_121, j),
        202 => happy_shift(p, action_123, j),
        203 => happy_shift(p, action_124, j),
        209 => happy_shift(p, action_126, j),
        210 => happy_shift(p, action_127, j),
        211 => happy_shift(p, action_128, j),
        219 => happy_shift(p, action_131, j),
        220 => happy_shift(p, action_132, j),
        224 => happy_shift(p, action_135, j),
        227 => happy_shift(p, action_137, j),
        229 => happy_shift(p, action_139, j),
        230 => happy_shift(p, action_140, j),
        231 => happy_shift(p, action_141, j),
        239 => happy_shift(p, action_143, j),
        240 => happy_shift(p, action_144, j),
        241 => happy_shift(p, action_723, j),
        44 => happy_goto(p, action_715, j),
        45 => happy_goto(p, action_90, j),
        47 => happy_goto(p, action_290, j),
        49 => happy_goto(p, action_291, j),
        51 => happy_goto(p, action_292, j),
        52 => happy_goto(p, action_97, j),
        53 => happy_goto(p, action_98, j),
        54 => happy_goto(p, action_99, j),
        56 => happy_goto(p, action_716, j),
        57 => happy_goto(p, action_717, j),
        58 => happy_goto(p, action_718, j),
        61 => happy_goto(p, action_100, j),
        65 => happy_goto(p, action_719, j),
        132 => happy_goto(p, action_109, j),
        133 => happy_goto(p, action_720, j),
        134 => happy_goto(p, action_111, j),
        _ => happy_reduce_473(p, j)
    }
}

fn action_570(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        55 => happy_goto(p, action_714, j),
        _ => happy_reduce_192(p, j)
    }
}

fn action_571(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_99(p, j)
    }
}

fn action_572(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_22(p, j)
    }
}

fn action_573(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_417, j),
        139 => happy_shift(p, action_713, j),
        140 => happy_shift(p, action_184, j),
        88 => happy_goto(p, action_577, j),
        89 => happy_goto(p, action_181, j),
        90 => happy_goto(p, action_182, j),
        _ => happy_fail(p, j)
    }
}

fn action_574(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        139 => happy_shift(p, action_712, j),
        _ => happy_fail(p, j)
    }
}

fn action_575(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_575, j),
        150 => happy_shift(p, action_576, j),
        238 => happy_shift(p, action_142, j),
        240 => happy_shift(p, action_144, j),
        70 => happy_goto(p, action_423, j),
        71 => happy_goto(p, action_224, j),
        76 => happy_goto(p, action_159, j),
        77 => happy_goto(p, action_104, j),
        78 => happy_goto(p, action_516, j),
        133 => happy_goto(p, action_426, j),
        134 => happy_goto(p, action_111, j),
        _ => happy_fail(p, j)
    }
}

fn action_576(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_575, j),
        150 => happy_shift(p, action_576, j),
        238 => happy_shift(p, action_142, j),
        239 => happy_shift(p, action_231, j),
        240 => happy_shift(p, action_144, j),
        65 => happy_goto(p, action_711, j),
        69 => happy_goto(p, action_419, j),
        70 => happy_goto(p, action_223, j),
        71 => happy_goto(p, action_224, j),
        75 => happy_goto(p, action_156, j),
        76 => happy_goto(p, action_103, j),
        77 => happy_goto(p, action_104, j),
        78 => happy_goto(p, action_485, j),
        132 => happy_goto(p, action_109, j),
        133 => happy_goto(p, action_421, j),
        134 => happy_goto(p, action_111, j),
        _ => happy_reduce_473(p, j)
    }
}

fn action_577(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        139 => happy_shift(p, action_710, j),
        _ => happy_fail(p, j)
    }
}

fn action_578(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_417, j),
        140 => happy_shift(p, action_184, j),
        88 => happy_goto(p, action_709, j),
        89 => happy_goto(p, action_181, j),
        90 => happy_goto(p, action_182, j),
        _ => happy_reduce_256(p, j)
    }
}

fn action_579(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_417, j),
        140 => happy_shift(p, action_184, j),
        88 => happy_goto(p, action_708, j),
        89 => happy_goto(p, action_181, j),
        90 => happy_goto(p, action_182, j),
        _ => happy_reduce_245(p, j)
    }
}

fn action_580(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_417, j),
        139 => happy_shift(p, action_707, j),
        140 => happy_shift(p, action_184, j),
        88 => happy_goto(p, action_577, j),
        89 => happy_goto(p, action_181, j),
        90 => happy_goto(p, action_182, j),
        _ => happy_fail(p, j)
    }
}

fn action_581(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_242(p, j)
    }
}

fn action_582(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_243(p, j)
    }
}

fn action_583(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_254(p, j)
    }
}

fn action_584(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_706, j),
        150 => happy_shift(p, action_529, j),
        186 => happy_shift(p, action_173, j),
        193 => happy_shift(p, action_174, j),
        214 => happy_shift(p, action_175, j),
        215 => happy_shift(p, action_176, j),
        217 => happy_shift(p, action_177, j),
        232 => happy_shift(p, action_178, j),
        238 => happy_shift(p, action_142, j),
        239 => happy_shift(p, action_231, j),
        240 => happy_shift(p, action_144, j),
        64 => happy_goto(p, action_439, j),
        69 => happy_goto(p, action_704, j),
        70 => happy_goto(p, action_223, j),
        71 => happy_goto(p, action_224, j),
        72 => happy_goto(p, action_705, j),
        73 => happy_goto(p, action_226, j),
        75 => happy_goto(p, action_512, j),
        76 => happy_goto(p, action_103, j),
        77 => happy_goto(p, action_104, j),
        78 => happy_goto(p, action_485, j),
        134 => happy_goto(p, action_169, j),
        _ => happy_fail(p, j)
    }
}

fn action_585(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_427, j),
        150 => happy_shift(p, action_230, j),
        238 => happy_shift(p, action_142, j),
        239 => happy_shift(p, action_428, j),
        240 => happy_shift(p, action_144, j),
        70 => happy_goto(p, action_423, j),
        71 => happy_goto(p, action_224, j),
        72 => happy_goto(p, action_424, j),
        73 => happy_goto(p, action_226, j),
        74 => happy_goto(p, action_703, j),
        76 => happy_goto(p, action_159, j),
        77 => happy_goto(p, action_104, j),
        78 => happy_goto(p, action_160, j),
        80 => happy_goto(p, action_161, j),
        81 => happy_goto(p, action_108, j),
        133 => happy_goto(p, action_426, j),
        134 => happy_goto(p, action_111, j),
        _ => happy_fail(p, j)
    }
}

fn action_586(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_92(p, j)
    }
}

fn action_587(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_23(p, j)
    }
}

fn action_588(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        139 => happy_shift(p, action_702, j),
        _ => happy_fail(p, j)
    }
}

fn action_589(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        139 => happy_shift(p, action_701, j),
        _ => happy_fail(p, j)
    }
}

fn action_590(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        237 => happy_shift(p, action_42, j),
        128 => happy_goto(p, action_700, j),
        _ => happy_fail(p, j)
    }
}

fn action_591(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_97(p, j)
    }
}

fn action_592(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_98(p, j)
    }
}

fn action_593(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_21(p, j)
    }
}

fn action_594(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        187 => happy_shift(p, action_410, j),
        35 => happy_goto(p, action_699, j),
        67 => happy_goto(p, action_409, j),
        _ => happy_reduce_233(p, j)
    }
}

fn action_595(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        187 => happy_shift(p, action_410, j),
        35 => happy_goto(p, action_698, j),
        67 => happy_goto(p, action_409, j),
        _ => happy_reduce_233(p, j)
    }
}

fn action_596(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_26, j),
        144 => happy_shift(p, action_27, j),
        145 => happy_shift(p, action_28, j),
        146 => happy_shift(p, action_29, j),
        147 => happy_shift(p, action_30, j),
        148 => happy_shift(p, action_31, j),
        149 => happy_shift(p, action_32, j),
        150 => happy_shift(p, action_33, j),
        153 => happy_shift(p, action_34, j),
        164 => happy_shift(p, action_35, j),
        180 => happy_shift(p, action_61, j),
        181 => happy_shift(p, action_62, j),
        184 => happy_shift(p, action_36, j),
        187 => happy_shift(p, action_63, j),
        189 => happy_shift(p, action_64, j),
        191 => happy_shift(p, action_65, j),
        194 => happy_shift(p, action_66, j),
        196 => happy_shift(p, action_67, j),
        197 => happy_shift(p, action_68, j),
        204 => happy_shift(p, action_69, j),
        205 => happy_shift(p, action_37, j),
        206 => happy_shift(p, action_70, j),
        207 => happy_shift(p, action_71, j),
        218 => happy_shift(p, action_72, j),
        221 => happy_shift(p, action_38, j),
        225 => happy_shift(p, action_73, j),
        233 => happy_shift(p, action_74, j),
        234 => happy_shift(p, action_39, j),
        235 => happy_shift(p, action_40, j),
        236 => happy_shift(p, action_41, j),
        237 => happy_shift(p, action_42, j),
        238 => happy_shift(p, action_75, j),
        239 => happy_shift(p, action_76, j),
        241 => happy_shift(p, action_44, j),
        242 => happy_shift(p, action_45, j),
        243 => happy_shift(p, action_46, j),
        244 => happy_shift(p, action_47, j),
        245 => happy_shift(p, action_48, j),
        246 => happy_shift(p, action_49, j),
        247 => happy_shift(p, action_50, j),
        12 => happy_goto(p, action_697, j),
        13 => happy_goto(p, action_52, j),
        14 => happy_goto(p, action_53, j),
        22 => happy_goto(p, action_54, j),
        23 => happy_goto(p, action_55, j),
        24 => happy_goto(p, action_56, j),
        25 => happy_goto(p, action_57, j),
        26 => happy_goto(p, action_58, j),
        100 => happy_goto(p, action_6, j),
        104 => happy_goto(p, action_7, j),
        106 => happy_goto(p, action_8, j),
        107 => happy_goto(p, action_9, j),
        108 => happy_goto(p, action_10, j),
        109 => happy_goto(p, action_11, j),
        110 => happy_goto(p, action_12, j),
        111 => happy_goto(p, action_13, j),
        112 => happy_goto(p, action_14, j),
        113 => happy_goto(p, action_15, j),
        114 => happy_goto(p, action_16, j),
        115 => happy_goto(p, action_17, j),
        116 => happy_goto(p, action_18, j),
        117 => happy_goto(p, action_19, j),
        118 => happy_goto(p, action_20, j),
        119 => happy_goto(p, action_21, j),
        120 => happy_goto(p, action_22, j),
        122 => happy_goto(p, action_59, j),
        127 => happy_goto(p, action_24, j),
        128 => happy_goto(p, action_25, j),
        131 => happy_goto(p, action_60, j),
        _ => happy_fail(p, j)
    }
}

fn action_597(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_26, j),
        144 => happy_shift(p, action_27, j),
        145 => happy_shift(p, action_28, j),
        146 => happy_shift(p, action_29, j),
        147 => happy_shift(p, action_30, j),
        148 => happy_shift(p, action_31, j),
        149 => happy_shift(p, action_32, j),
        150 => happy_shift(p, action_33, j),
        153 => happy_shift(p, action_34, j),
        164 => happy_shift(p, action_35, j),
        180 => happy_shift(p, action_61, j),
        181 => happy_shift(p, action_62, j),
        184 => happy_shift(p, action_36, j),
        187 => happy_shift(p, action_63, j),
        189 => happy_shift(p, action_64, j),
        191 => happy_shift(p, action_65, j),
        194 => happy_shift(p, action_66, j),
        196 => happy_shift(p, action_67, j),
        197 => happy_shift(p, action_68, j),
        204 => happy_shift(p, action_69, j),
        205 => happy_shift(p, action_37, j),
        206 => happy_shift(p, action_70, j),
        207 => happy_shift(p, action_71, j),
        218 => happy_shift(p, action_72, j),
        221 => happy_shift(p, action_38, j),
        225 => happy_shift(p, action_73, j),
        233 => happy_shift(p, action_74, j),
        234 => happy_shift(p, action_39, j),
        235 => happy_shift(p, action_40, j),
        236 => happy_shift(p, action_41, j),
        237 => happy_shift(p, action_42, j),
        238 => happy_shift(p, action_75, j),
        239 => happy_shift(p, action_76, j),
        241 => happy_shift(p, action_44, j),
        242 => happy_shift(p, action_45, j),
        243 => happy_shift(p, action_46, j),
        244 => happy_shift(p, action_47, j),
        245 => happy_shift(p, action_48, j),
        246 => happy_shift(p, action_49, j),
        247 => happy_shift(p, action_50, j),
        12 => happy_goto(p, action_696, j),
        13 => happy_goto(p, action_52, j),
        14 => happy_goto(p, action_53, j),
        22 => happy_goto(p, action_54, j),
        23 => happy_goto(p, action_55, j),
        24 => happy_goto(p, action_56, j),
        25 => happy_goto(p, action_57, j),
        26 => happy_goto(p, action_58, j),
        100 => happy_goto(p, action_6, j),
        104 => happy_goto(p, action_7, j),
        106 => happy_goto(p, action_8, j),
        107 => happy_goto(p, action_9, j),
        108 => happy_goto(p, action_10, j),
        109 => happy_goto(p, action_11, j),
        110 => happy_goto(p, action_12, j),
        111 => happy_goto(p, action_13, j),
        112 => happy_goto(p, action_14, j),
        113 => happy_goto(p, action_15, j),
        114 => happy_goto(p, action_16, j),
        115 => happy_goto(p, action_17, j),
        116 => happy_goto(p, action_18, j),
        117 => happy_goto(p, action_19, j),
        118 => happy_goto(p, action_20, j),
        119 => happy_goto(p, action_21, j),
        120 => happy_goto(p, action_22, j),
        122 => happy_goto(p, action_59, j),
        127 => happy_goto(p, action_24, j),
        128 => happy_goto(p, action_25, j),
        131 => happy_goto(p, action_60, j),
        _ => happy_fail(p, j)
    }
}

fn action_598(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_26, j),
        144 => happy_shift(p, action_27, j),
        145 => happy_shift(p, action_28, j),
        146 => happy_shift(p, action_29, j),
        147 => happy_shift(p, action_30, j),
        148 => happy_shift(p, action_31, j),
        149 => happy_shift(p, action_32, j),
        150 => happy_shift(p, action_33, j),
        153 => happy_shift(p, action_34, j),
        164 => happy_shift(p, action_35, j),
        180 => happy_shift(p, action_61, j),
        181 => happy_shift(p, action_62, j),
        184 => happy_shift(p, action_36, j),
        187 => happy_shift(p, action_63, j),
        189 => happy_shift(p, action_64, j),
        191 => happy_shift(p, action_65, j),
        194 => happy_shift(p, action_66, j),
        196 => happy_shift(p, action_67, j),
        197 => happy_shift(p, action_68, j),
        204 => happy_shift(p, action_69, j),
        205 => happy_shift(p, action_37, j),
        206 => happy_shift(p, action_70, j),
        207 => happy_shift(p, action_71, j),
        218 => happy_shift(p, action_72, j),
        221 => happy_shift(p, action_38, j),
        225 => happy_shift(p, action_73, j),
        233 => happy_shift(p, action_74, j),
        234 => happy_shift(p, action_39, j),
        235 => happy_shift(p, action_40, j),
        236 => happy_shift(p, action_41, j),
        237 => happy_shift(p, action_42, j),
        238 => happy_shift(p, action_75, j),
        239 => happy_shift(p, action_76, j),
        241 => happy_shift(p, action_44, j),
        242 => happy_shift(p, action_45, j),
        243 => happy_shift(p, action_46, j),
        244 => happy_shift(p, action_47, j),
        245 => happy_shift(p, action_48, j),
        246 => happy_shift(p, action_49, j),
        247 => happy_shift(p, action_50, j),
        12 => happy_goto(p, action_695, j),
        13 => happy_goto(p, action_52, j),
        14 => happy_goto(p, action_53, j),
        22 => happy_goto(p, action_54, j),
        23 => happy_goto(p, action_55, j),
        24 => happy_goto(p, action_56, j),
        25 => happy_goto(p, action_57, j),
        26 => happy_goto(p, action_58, j),
        100 => happy_goto(p, action_6, j),
        104 => happy_goto(p, action_7, j),
        106 => happy_goto(p, action_8, j),
        107 => happy_goto(p, action_9, j),
        108 => happy_goto(p, action_10, j),
        109 => happy_goto(p, action_11, j),
        110 => happy_goto(p, action_12, j),
        111 => happy_goto(p, action_13, j),
        112 => happy_goto(p, action_14, j),
        113 => happy_goto(p, action_15, j),
        114 => happy_goto(p, action_16, j),
        115 => happy_goto(p, action_17, j),
        116 => happy_goto(p, action_18, j),
        117 => happy_goto(p, action_19, j),
        118 => happy_goto(p, action_20, j),
        119 => happy_goto(p, action_21, j),
        120 => happy_goto(p, action_22, j),
        122 => happy_goto(p, action_59, j),
        127 => happy_goto(p, action_24, j),
        128 => happy_goto(p, action_25, j),
        131 => happy_goto(p, action_60, j),
        _ => happy_fail(p, j)
    }
}

fn action_599(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_66(p, j)
    }
}

fn action_600(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_26, j),
        144 => happy_shift(p, action_27, j),
        145 => happy_shift(p, action_28, j),
        146 => happy_shift(p, action_29, j),
        147 => happy_shift(p, action_30, j),
        148 => happy_shift(p, action_31, j),
        149 => happy_shift(p, action_32, j),
        150 => happy_shift(p, action_33, j),
        153 => happy_shift(p, action_34, j),
        164 => happy_shift(p, action_35, j),
        184 => happy_shift(p, action_36, j),
        205 => happy_shift(p, action_37, j),
        221 => happy_shift(p, action_38, j),
        234 => happy_shift(p, action_39, j),
        235 => happy_shift(p, action_40, j),
        236 => happy_shift(p, action_41, j),
        237 => happy_shift(p, action_42, j),
        238 => happy_shift(p, action_43, j),
        241 => happy_shift(p, action_44, j),
        242 => happy_shift(p, action_45, j),
        243 => happy_shift(p, action_46, j),
        244 => happy_shift(p, action_47, j),
        245 => happy_shift(p, action_48, j),
        246 => happy_shift(p, action_49, j),
        247 => happy_shift(p, action_50, j),
        100 => happy_goto(p, action_6, j),
        104 => happy_goto(p, action_7, j),
        106 => happy_goto(p, action_8, j),
        107 => happy_goto(p, action_9, j),
        108 => happy_goto(p, action_10, j),
        109 => happy_goto(p, action_11, j),
        110 => happy_goto(p, action_12, j),
        111 => happy_goto(p, action_13, j),
        112 => happy_goto(p, action_14, j),
        113 => happy_goto(p, action_15, j),
        114 => happy_goto(p, action_16, j),
        115 => happy_goto(p, action_17, j),
        116 => happy_goto(p, action_18, j),
        117 => happy_goto(p, action_19, j),
        118 => happy_goto(p, action_20, j),
        119 => happy_goto(p, action_21, j),
        120 => happy_goto(p, action_22, j),
        122 => happy_goto(p, action_251, j),
        124 => happy_goto(p, action_694, j),
        127 => happy_goto(p, action_24, j),
        128 => happy_goto(p, action_25, j),
        _ => happy_reduce_458(p, j)
    }
}

fn action_601(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_26, j),
        144 => happy_shift(p, action_27, j),
        145 => happy_shift(p, action_28, j),
        146 => happy_shift(p, action_29, j),
        147 => happy_shift(p, action_30, j),
        148 => happy_shift(p, action_31, j),
        149 => happy_shift(p, action_32, j),
        150 => happy_shift(p, action_33, j),
        153 => happy_shift(p, action_34, j),
        164 => happy_shift(p, action_35, j),
        184 => happy_shift(p, action_36, j),
        205 => happy_shift(p, action_37, j),
        221 => happy_shift(p, action_38, j),
        234 => happy_shift(p, action_39, j),
        235 => happy_shift(p, action_40, j),
        236 => happy_shift(p, action_41, j),
        237 => happy_shift(p, action_42, j),
        238 => happy_shift(p, action_43, j),
        241 => happy_shift(p, action_44, j),
        242 => happy_shift(p, action_45, j),
        243 => happy_shift(p, action_46, j),
        244 => happy_shift(p, action_47, j),
        245 => happy_shift(p, action_48, j),
        246 => happy_shift(p, action_49, j),
        247 => happy_shift(p, action_50, j),
        100 => happy_goto(p, action_6, j),
        104 => happy_goto(p, action_7, j),
        106 => happy_goto(p, action_8, j),
        107 => happy_goto(p, action_9, j),
        108 => happy_goto(p, action_10, j),
        109 => happy_goto(p, action_11, j),
        110 => happy_goto(p, action_12, j),
        111 => happy_goto(p, action_13, j),
        112 => happy_goto(p, action_14, j),
        113 => happy_goto(p, action_15, j),
        114 => happy_goto(p, action_16, j),
        115 => happy_goto(p, action_17, j),
        116 => happy_goto(p, action_18, j),
        117 => happy_goto(p, action_19, j),
        118 => happy_goto(p, action_20, j),
        119 => happy_goto(p, action_21, j),
        120 => happy_goto(p, action_22, j),
        122 => happy_goto(p, action_251, j),
        124 => happy_goto(p, action_693, j),
        127 => happy_goto(p, action_24, j),
        128 => happy_goto(p, action_25, j),
        _ => happy_reduce_458(p, j)
    }
}

fn action_602(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_26, j),
        144 => happy_shift(p, action_27, j),
        145 => happy_shift(p, action_28, j),
        146 => happy_shift(p, action_29, j),
        147 => happy_shift(p, action_30, j),
        148 => happy_shift(p, action_31, j),
        149 => happy_shift(p, action_32, j),
        150 => happy_shift(p, action_33, j),
        153 => happy_shift(p, action_34, j),
        164 => happy_shift(p, action_35, j),
        184 => happy_shift(p, action_36, j),
        205 => happy_shift(p, action_37, j),
        221 => happy_shift(p, action_38, j),
        234 => happy_shift(p, action_39, j),
        235 => happy_shift(p, action_40, j),
        236 => happy_shift(p, action_41, j),
        237 => happy_shift(p, action_42, j),
        238 => happy_shift(p, action_43, j),
        241 => happy_shift(p, action_44, j),
        242 => happy_shift(p, action_45, j),
        243 => happy_shift(p, action_46, j),
        244 => happy_shift(p, action_47, j),
        245 => happy_shift(p, action_48, j),
        246 => happy_shift(p, action_49, j),
        247 => happy_shift(p, action_50, j),
        100 => happy_goto(p, action_6, j),
        104 => happy_goto(p, action_7, j),
        106 => happy_goto(p, action_8, j),
        107 => happy_goto(p, action_9, j),
        108 => happy_goto(p, action_10, j),
        109 => happy_goto(p, action_11, j),
        110 => happy_goto(p, action_12, j),
        111 => happy_goto(p, action_13, j),
        112 => happy_goto(p, action_14, j),
        113 => happy_goto(p, action_15, j),
        114 => happy_goto(p, action_16, j),
        115 => happy_goto(p, action_17, j),
        116 => happy_goto(p, action_18, j),
        117 => happy_goto(p, action_19, j),
        118 => happy_goto(p, action_20, j),
        119 => happy_goto(p, action_21, j),
        120 => happy_goto(p, action_22, j),
        122 => happy_goto(p, action_692, j),
        127 => happy_goto(p, action_24, j),
        128 => happy_goto(p, action_25, j),
        _ => happy_fail(p, j)
    }
}

fn action_603(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        167 => happy_shift(p, action_691, j),
        _ => happy_fail(p, j)
    }
}

fn action_604(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_35(p, j)
    }
}

fn action_605(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        139 => happy_shift(p, action_689, j),
        167 => happy_shift(p, action_690, j),
        _ => happy_fail(p, j)
    }
}

fn action_606(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        179 => happy_shift(p, action_532, j),
        180 => happy_shift(p, action_688, j),
        _ => happy_fail(p, j)
    }
}

fn action_607(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_26, j),
        144 => happy_shift(p, action_27, j),
        145 => happy_shift(p, action_28, j),
        146 => happy_shift(p, action_29, j),
        147 => happy_shift(p, action_30, j),
        148 => happy_shift(p, action_31, j),
        149 => happy_shift(p, action_32, j),
        150 => happy_shift(p, action_33, j),
        153 => happy_shift(p, action_34, j),
        164 => happy_shift(p, action_35, j),
        180 => happy_shift(p, action_61, j),
        181 => happy_shift(p, action_62, j),
        184 => happy_shift(p, action_36, j),
        185 => happy_shift(p, action_114, j),
        186 => happy_reduce_473(p, j),
        187 => happy_shift(p, action_63, j),
        188 => happy_shift(p, action_116, j),
        189 => happy_shift(p, action_64, j),
        190 => happy_shift(p, action_117, j),
        191 => happy_shift(p, action_65, j),
        192 => happy_shift(p, action_118, j),
        193 => happy_reduce_473(p, j),
        194 => happy_shift(p, action_66, j),
        195 => happy_shift(p, action_119, j),
        196 => happy_shift(p, action_67, j),
        197 => happy_shift(p, action_68, j),
        198 => happy_shift(p, action_120, j),
        200 => happy_shift(p, action_121, j),
        201 => happy_shift(p, action_122, j),
        202 => happy_shift(p, action_123, j),
        203 => happy_shift(p, action_124, j),
        204 => happy_shift(p, action_69, j),
        205 => happy_shift(p, action_37, j),
        206 => happy_shift(p, action_70, j),
        207 => happy_shift(p, action_71, j),
        208 => happy_shift(p, action_125, j),
        209 => happy_shift(p, action_126, j),
        210 => happy_shift(p, action_127, j),
        211 => happy_shift(p, action_128, j),
        213 => happy_shift(p, action_129, j),
        214 => happy_reduce_473(p, j),
        215 => happy_reduce_473(p, j),
        216 => happy_shift(p, action_130, j),
        217 => happy_reduce_473(p, j),
        218 => happy_shift(p, action_72, j),
        219 => happy_shift(p, action_131, j),
        220 => happy_shift(p, action_132, j),
        221 => happy_shift(p, action_38, j),
        222 => happy_shift(p, action_133, j),
        223 => happy_shift(p, action_134, j),
        224 => happy_shift(p, action_135, j),
        225 => happy_shift(p, action_73, j),
        226 => happy_shift(p, action_136, j),
        227 => happy_shift(p, action_137, j),
        228 => happy_shift(p, action_138, j),
        229 => happy_shift(p, action_139, j),
        230 => happy_shift(p, action_140, j),
        231 => happy_shift(p, action_141, j),
        232 => happy_reduce_473(p, j),
        233 => happy_shift(p, action_74, j),
        234 => happy_shift(p, action_39, j),
        235 => happy_shift(p, action_40, j),
        236 => happy_shift(p, action_41, j),
        237 => happy_shift(p, action_42, j),
        238 => happy_shift(p, action_75, j),
        239 => happy_shift(p, action_619, j),
        240 => happy_shift(p, action_144, j),
        241 => happy_shift(p, action_620, j),
        242 => happy_shift(p, action_45, j),
        243 => happy_shift(p, action_46, j),
        244 => happy_shift(p, action_47, j),
        245 => happy_shift(p, action_48, j),
        246 => happy_shift(p, action_49, j),
        247 => happy_shift(p, action_50, j),
        12 => happy_goto(p, action_609, j),
        13 => happy_goto(p, action_52, j),
        14 => happy_goto(p, action_53, j),
        16 => happy_goto(p, action_687, j),
        18 => happy_goto(p, action_611, j),
        19 => happy_goto(p, action_612, j),
        20 => happy_goto(p, action_613, j),
        22 => happy_goto(p, action_54, j),
        23 => happy_goto(p, action_55, j),
        24 => happy_goto(p, action_56, j),
        25 => happy_goto(p, action_57, j),
        26 => happy_goto(p, action_58, j),
        32 => happy_goto(p, action_614, j),
        34 => happy_goto(p, action_81, j),
        36 => happy_goto(p, action_82, j),
        37 => happy_goto(p, action_615, j),
        38 => happy_goto(p, action_616, j),
        40 => happy_goto(p, action_85, j),
        41 => happy_goto(p, action_86, j),
        42 => happy_goto(p, action_87, j),
        43 => happy_goto(p, action_88, j),
        44 => happy_goto(p, action_617, j),
        45 => happy_goto(p, action_90, j),
        46 => happy_goto(p, action_91, j),
        47 => happy_goto(p, action_92, j),
        48 => happy_goto(p, action_93, j),
        49 => happy_goto(p, action_94, j),
        50 => happy_goto(p, action_95, j),
        51 => happy_goto(p, action_96, j),
        52 => happy_goto(p, action_97, j),
        53 => happy_goto(p, action_98, j),
        54 => happy_goto(p, action_99, j),
        61 => happy_goto(p, action_100, j),
        65 => happy_goto(p, action_618, j),
        100 => happy_goto(p, action_6, j),
        104 => happy_goto(p, action_7, j),
        106 => happy_goto(p, action_8, j),
        107 => happy_goto(p, action_9, j),
        108 => happy_goto(p, action_10, j),
        109 => happy_goto(p, action_11, j),
        110 => happy_goto(p, action_12, j),
        111 => happy_goto(p, action_13, j),
        112 => happy_goto(p, action_14, j),
        113 => happy_goto(p, action_15, j),
        114 => happy_goto(p, action_16, j),
        115 => happy_goto(p, action_17, j),
        116 => happy_goto(p, action_18, j),
        117 => happy_goto(p, action_19, j),
        118 => happy_goto(p, action_20, j),
        119 => happy_goto(p, action_21, j),
        120 => happy_goto(p, action_22, j),
        122 => happy_goto(p, action_59, j),
        127 => happy_goto(p, action_24, j),
        128 => happy_goto(p, action_25, j),
        131 => happy_goto(p, action_60, j),
        132 => happy_goto(p, action_109, j),
        133 => happy_goto(p, action_472, j),
        134 => happy_goto(p, action_111, j),
        _ => happy_reduce_41(p, j)
    }
}

fn action_608(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        238 => happy_shift(p, action_464, j),
        85 => happy_goto(p, action_686, j),
        _ => happy_fail(p, j)
    }
}

fn action_609(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_44(p, j)
    }
}

fn action_610(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        182 => happy_shift(p, action_685, j),
        _ => happy_fail(p, j)
    }
}

fn action_611(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_43(p, j)
    }
}

fn action_612(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_45(p, j)
    }
}

fn action_613(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_47(p, j)
    }
}

fn action_614(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_46(p, j)
    }
}

fn action_615(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_528, j),
        150 => happy_shift(p, action_529, j),
        238 => happy_shift(p, action_142, j),
        239 => happy_shift(p, action_231, j),
        11 => happy_goto(p, action_684, j),
        66 => happy_goto(p, action_242, j),
        68 => happy_goto(p, action_221, j),
        69 => happy_goto(p, action_222, j),
        70 => happy_goto(p, action_223, j),
        71 => happy_goto(p, action_224, j),
        72 => happy_goto(p, action_225, j),
        73 => happy_goto(p, action_226, j),
        75 => happy_goto(p, action_227, j),
        76 => happy_goto(p, action_103, j),
        77 => happy_goto(p, action_104, j),
        78 => happy_goto(p, action_485, j),
        _ => happy_fail(p, j)
    }
}

fn action_616(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_479, j),
        150 => happy_shift(p, action_480, j),
        185 => happy_shift(p, action_114, j),
        186 => happy_shift(p, action_173, j),
        188 => happy_shift(p, action_116, j),
        190 => happy_shift(p, action_117, j),
        192 => happy_shift(p, action_118, j),
        193 => happy_shift(p, action_174, j),
        195 => happy_shift(p, action_119, j),
        198 => happy_shift(p, action_120, j),
        200 => happy_shift(p, action_121, j),
        201 => happy_shift(p, action_122, j),
        202 => happy_shift(p, action_123, j),
        203 => happy_shift(p, action_124, j),
        208 => happy_shift(p, action_125, j),
        209 => happy_shift(p, action_126, j),
        210 => happy_shift(p, action_127, j),
        211 => happy_shift(p, action_128, j),
        213 => happy_shift(p, action_129, j),
        214 => happy_shift(p, action_175, j),
        215 => happy_shift(p, action_176, j),
        216 => happy_shift(p, action_130, j),
        217 => happy_shift(p, action_177, j),
        219 => happy_shift(p, action_131, j),
        220 => happy_shift(p, action_132, j),
        222 => happy_shift(p, action_133, j),
        224 => happy_shift(p, action_135, j),
        226 => happy_shift(p, action_136, j),
        227 => happy_shift(p, action_239, j),
        228 => happy_shift(p, action_138, j),
        229 => happy_shift(p, action_139, j),
        230 => happy_shift(p, action_140, j),
        231 => happy_shift(p, action_141, j),
        232 => happy_shift(p, action_178, j),
        238 => happy_shift(p, action_142, j),
        239 => happy_shift(p, action_240, j),
        240 => happy_shift(p, action_144, j),
        11 => happy_goto(p, action_683, j),
        39 => happy_goto(p, action_233, j),
        41 => happy_goto(p, action_200, j),
        42 => happy_goto(p, action_201, j),
        43 => happy_goto(p, action_202, j),
        45 => happy_goto(p, action_234, j),
        52 => happy_goto(p, action_235, j),
        53 => happy_goto(p, action_98, j),
        54 => happy_goto(p, action_99, j),
        61 => happy_goto(p, action_100, j),
        64 => happy_goto(p, action_203, j),
        75 => happy_goto(p, action_236, j),
        76 => happy_goto(p, action_103, j),
        77 => happy_goto(p, action_104, j),
        78 => happy_goto(p, action_485, j),
        134 => happy_goto(p, action_238, j),
        _ => happy_fail(p, j)
    }
}

fn action_617(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_528, j),
        150 => happy_shift(p, action_529, j),
        238 => happy_shift(p, action_142, j),
        239 => happy_shift(p, action_231, j),
        11 => happy_goto(p, action_682, j),
        66 => happy_goto(p, action_220, j),
        68 => happy_goto(p, action_221, j),
        69 => happy_goto(p, action_222, j),
        70 => happy_goto(p, action_223, j),
        71 => happy_goto(p, action_224, j),
        72 => happy_goto(p, action_225, j),
        73 => happy_goto(p, action_226, j),
        75 => happy_goto(p, action_227, j),
        76 => happy_goto(p, action_103, j),
        77 => happy_goto(p, action_104, j),
        78 => happy_goto(p, action_485, j),
        _ => happy_fail(p, j)
    }
}

fn action_618(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_479, j),
        150 => happy_shift(p, action_480, j),
        185 => happy_shift(p, action_114, j),
        186 => happy_shift(p, action_173, j),
        188 => happy_shift(p, action_116, j),
        190 => happy_shift(p, action_117, j),
        192 => happy_shift(p, action_118, j),
        193 => happy_shift(p, action_174, j),
        195 => happy_shift(p, action_119, j),
        198 => happy_shift(p, action_120, j),
        200 => happy_shift(p, action_121, j),
        201 => happy_shift(p, action_122, j),
        202 => happy_shift(p, action_123, j),
        203 => happy_shift(p, action_124, j),
        208 => happy_shift(p, action_125, j),
        209 => happy_shift(p, action_126, j),
        210 => happy_shift(p, action_127, j),
        211 => happy_shift(p, action_128, j),
        213 => happy_shift(p, action_129, j),
        214 => happy_shift(p, action_175, j),
        215 => happy_shift(p, action_176, j),
        216 => happy_shift(p, action_130, j),
        217 => happy_shift(p, action_177, j),
        219 => happy_shift(p, action_131, j),
        220 => happy_shift(p, action_132, j),
        222 => happy_shift(p, action_133, j),
        224 => happy_shift(p, action_135, j),
        226 => happy_shift(p, action_136, j),
        227 => happy_shift(p, action_193, j),
        228 => happy_shift(p, action_138, j),
        229 => happy_shift(p, action_139, j),
        230 => happy_shift(p, action_140, j),
        231 => happy_shift(p, action_141, j),
        232 => happy_shift(p, action_178, j),
        238 => happy_shift(p, action_142, j),
        239 => happy_shift(p, action_194, j),
        240 => happy_shift(p, action_144, j),
        11 => happy_goto(p, action_680, j),
        40 => happy_goto(p, action_186, j),
        41 => happy_goto(p, action_86, j),
        42 => happy_goto(p, action_87, j),
        43 => happy_goto(p, action_88, j),
        45 => happy_goto(p, action_187, j),
        52 => happy_goto(p, action_188, j),
        53 => happy_goto(p, action_98, j),
        54 => happy_goto(p, action_99, j),
        61 => happy_goto(p, action_100, j),
        64 => happy_goto(p, action_189, j),
        75 => happy_goto(p, action_190, j),
        76 => happy_goto(p, action_103, j),
        77 => happy_goto(p, action_104, j),
        78 => happy_goto(p, action_485, j),
        133 => happy_goto(p, action_681, j),
        134 => happy_goto(p, action_111, j),
        _ => happy_fail(p, j)
    }
}

fn action_619(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        167 => happy_reduce_472(p, j),
        _ => happy_reduce_171(p, j)
    }
}

fn action_620(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_26, j),
        144 => happy_shift(p, action_27, j),
        145 => happy_shift(p, action_28, j),
        146 => happy_shift(p, action_29, j),
        147 => happy_shift(p, action_30, j),
        148 => happy_shift(p, action_31, j),
        149 => happy_shift(p, action_32, j),
        150 => happy_shift(p, action_33, j),
        153 => happy_shift(p, action_34, j),
        164 => happy_shift(p, action_35, j),
        184 => happy_shift(p, action_36, j),
        185 => happy_shift(p, action_114, j),
        188 => happy_shift(p, action_116, j),
        190 => happy_shift(p, action_117, j),
        192 => happy_shift(p, action_118, j),
        195 => happy_shift(p, action_119, j),
        198 => happy_shift(p, action_120, j),
        200 => happy_shift(p, action_121, j),
        201 => happy_shift(p, action_122, j),
        202 => happy_shift(p, action_123, j),
        203 => happy_shift(p, action_124, j),
        205 => happy_shift(p, action_37, j),
        208 => happy_shift(p, action_125, j),
        209 => happy_shift(p, action_126, j),
        210 => happy_shift(p, action_127, j),
        211 => happy_shift(p, action_128, j),
        213 => happy_shift(p, action_129, j),
        216 => happy_shift(p, action_130, j),
        219 => happy_shift(p, action_131, j),
        220 => happy_shift(p, action_132, j),
        221 => happy_shift(p, action_38, j),
        222 => happy_shift(p, action_133, j),
        223 => happy_shift(p, action_134, j),
        224 => happy_shift(p, action_135, j),
        226 => happy_shift(p, action_136, j),
        227 => happy_shift(p, action_137, j),
        228 => happy_shift(p, action_138, j),
        229 => happy_shift(p, action_139, j),
        230 => happy_shift(p, action_140, j),
        231 => happy_shift(p, action_141, j),
        234 => happy_shift(p, action_39, j),
        235 => happy_shift(p, action_40, j),
        236 => happy_shift(p, action_41, j),
        237 => happy_shift(p, action_42, j),
        238 => happy_shift(p, action_43, j),
        239 => happy_shift(p, action_143, j),
        240 => happy_shift(p, action_144, j),
        241 => happy_shift(p, action_620, j),
        242 => happy_shift(p, action_45, j),
        243 => happy_shift(p, action_46, j),
        244 => happy_shift(p, action_47, j),
        245 => happy_shift(p, action_48, j),
        246 => happy_shift(p, action_49, j),
        247 => happy_shift(p, action_50, j),
        19 => happy_goto(p, action_679, j),
        20 => happy_goto(p, action_613, j),
        32 => happy_goto(p, action_614, j),
        34 => happy_goto(p, action_81, j),
        36 => happy_goto(p, action_82, j),
        37 => happy_goto(p, action_615, j),
        38 => happy_goto(p, action_616, j),
        40 => happy_goto(p, action_85, j),
        41 => happy_goto(p, action_86, j),
        42 => happy_goto(p, action_87, j),
        43 => happy_goto(p, action_88, j),
        44 => happy_goto(p, action_617, j),
        45 => happy_goto(p, action_90, j),
        46 => happy_goto(p, action_91, j),
        47 => happy_goto(p, action_92, j),
        48 => happy_goto(p, action_93, j),
        49 => happy_goto(p, action_94, j),
        50 => happy_goto(p, action_95, j),
        51 => happy_goto(p, action_96, j),
        52 => happy_goto(p, action_97, j),
        53 => happy_goto(p, action_98, j),
        54 => happy_goto(p, action_99, j),
        61 => happy_goto(p, action_100, j),
        65 => happy_goto(p, action_618, j),
        100 => happy_goto(p, action_6, j),
        104 => happy_goto(p, action_7, j),
        106 => happy_goto(p, action_261, j),
        107 => happy_goto(p, action_9, j),
        108 => happy_goto(p, action_277, j),
        127 => happy_goto(p, action_24, j),
        128 => happy_goto(p, action_25, j),
        132 => happy_goto(p, action_109, j),
        133 => happy_goto(p, action_472, j),
        134 => happy_goto(p, action_111, j),
        _ => happy_reduce_473(p, j)
    }
}

fn action_621(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_34(p, j)
    }
}

fn action_622(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        190 => happy_shift(p, action_117, j),
        192 => happy_shift(p, action_118, j),
        195 => happy_shift(p, action_119, j),
        198 => happy_shift(p, action_120, j),
        200 => happy_shift(p, action_121, j),
        202 => happy_shift(p, action_123, j),
        203 => happy_shift(p, action_124, j),
        209 => happy_shift(p, action_126, j),
        210 => happy_shift(p, action_127, j),
        211 => happy_shift(p, action_128, j),
        219 => happy_shift(p, action_131, j),
        220 => happy_shift(p, action_132, j),
        224 => happy_shift(p, action_135, j),
        227 => happy_shift(p, action_137, j),
        229 => happy_shift(p, action_139, j),
        230 => happy_shift(p, action_140, j),
        231 => happy_shift(p, action_141, j),
        239 => happy_shift(p, action_143, j),
        240 => happy_shift(p, action_144, j),
        44 => happy_goto(p, action_289, j),
        45 => happy_goto(p, action_90, j),
        47 => happy_goto(p, action_290, j),
        49 => happy_goto(p, action_291, j),
        51 => happy_goto(p, action_292, j),
        52 => happy_goto(p, action_97, j),
        53 => happy_goto(p, action_98, j),
        54 => happy_goto(p, action_99, j),
        61 => happy_goto(p, action_100, j),
        65 => happy_goto(p, action_293, j),
        86 => happy_goto(p, action_678, j),
        132 => happy_goto(p, action_109, j),
        133 => happy_goto(p, action_296, j),
        134 => happy_goto(p, action_111, j),
        _ => happy_reduce_473(p, j)
    }
}

fn action_623(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        190 => happy_shift(p, action_117, j),
        192 => happy_shift(p, action_118, j),
        195 => happy_shift(p, action_119, j),
        198 => happy_shift(p, action_120, j),
        200 => happy_shift(p, action_121, j),
        202 => happy_shift(p, action_123, j),
        203 => happy_shift(p, action_124, j),
        209 => happy_shift(p, action_126, j),
        210 => happy_shift(p, action_127, j),
        211 => happy_shift(p, action_128, j),
        219 => happy_shift(p, action_131, j),
        220 => happy_shift(p, action_132, j),
        224 => happy_shift(p, action_135, j),
        227 => happy_shift(p, action_137, j),
        229 => happy_shift(p, action_139, j),
        230 => happy_shift(p, action_140, j),
        231 => happy_shift(p, action_141, j),
        239 => happy_shift(p, action_143, j),
        240 => happy_shift(p, action_144, j),
        44 => happy_goto(p, action_289, j),
        45 => happy_goto(p, action_90, j),
        47 => happy_goto(p, action_290, j),
        49 => happy_goto(p, action_291, j),
        51 => happy_goto(p, action_292, j),
        52 => happy_goto(p, action_97, j),
        53 => happy_goto(p, action_98, j),
        54 => happy_goto(p, action_99, j),
        61 => happy_goto(p, action_100, j),
        65 => happy_goto(p, action_293, j),
        86 => happy_goto(p, action_677, j),
        132 => happy_goto(p, action_109, j),
        133 => happy_goto(p, action_296, j),
        134 => happy_goto(p, action_111, j),
        _ => happy_reduce_473(p, j)
    }
}

fn action_624(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        238 => happy_shift(p, action_256, j),
        239 => happy_shift(p, action_76, j),
        103 => happy_goto(p, action_675, j),
        131 => happy_goto(p, action_676, j),
        _ => happy_fail(p, j)
    }
}

fn action_625(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        190 => happy_shift(p, action_117, j),
        192 => happy_shift(p, action_118, j),
        195 => happy_shift(p, action_119, j),
        198 => happy_shift(p, action_120, j),
        200 => happy_shift(p, action_121, j),
        202 => happy_shift(p, action_123, j),
        203 => happy_shift(p, action_124, j),
        209 => happy_shift(p, action_126, j),
        210 => happy_shift(p, action_127, j),
        211 => happy_shift(p, action_128, j),
        219 => happy_shift(p, action_131, j),
        220 => happy_shift(p, action_132, j),
        224 => happy_shift(p, action_135, j),
        227 => happy_shift(p, action_137, j),
        229 => happy_shift(p, action_139, j),
        230 => happy_shift(p, action_140, j),
        231 => happy_shift(p, action_141, j),
        239 => happy_shift(p, action_143, j),
        240 => happy_shift(p, action_144, j),
        44 => happy_goto(p, action_289, j),
        45 => happy_goto(p, action_90, j),
        47 => happy_goto(p, action_290, j),
        49 => happy_goto(p, action_291, j),
        51 => happy_goto(p, action_292, j),
        52 => happy_goto(p, action_97, j),
        53 => happy_goto(p, action_98, j),
        54 => happy_goto(p, action_99, j),
        61 => happy_goto(p, action_100, j),
        65 => happy_goto(p, action_293, j),
        86 => happy_goto(p, action_674, j),
        132 => happy_goto(p, action_109, j),
        133 => happy_goto(p, action_296, j),
        134 => happy_goto(p, action_111, j),
        _ => happy_reduce_473(p, j)
    }
}

fn action_626(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        181 => happy_shift(p, action_639, j),
        _ => happy_fail(p, j)
    }
}

fn action_627(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        181 => happy_shift(p, action_639, j),
        _ => happy_reduce_396(p, j)
    }
}

fn action_628(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        190 => happy_shift(p, action_117, j),
        192 => happy_shift(p, action_118, j),
        195 => happy_shift(p, action_119, j),
        196 => happy_shift(p, action_673, j),
        198 => happy_shift(p, action_120, j),
        200 => happy_shift(p, action_121, j),
        202 => happy_shift(p, action_123, j),
        203 => happy_shift(p, action_124, j),
        209 => happy_shift(p, action_126, j),
        210 => happy_shift(p, action_127, j),
        211 => happy_shift(p, action_128, j),
        219 => happy_shift(p, action_131, j),
        220 => happy_shift(p, action_132, j),
        224 => happy_shift(p, action_135, j),
        227 => happy_shift(p, action_137, j),
        229 => happy_shift(p, action_139, j),
        230 => happy_shift(p, action_140, j),
        231 => happy_shift(p, action_141, j),
        239 => happy_shift(p, action_143, j),
        240 => happy_shift(p, action_144, j),
        44 => happy_goto(p, action_289, j),
        45 => happy_goto(p, action_90, j),
        47 => happy_goto(p, action_290, j),
        49 => happy_goto(p, action_291, j),
        51 => happy_goto(p, action_292, j),
        52 => happy_goto(p, action_97, j),
        53 => happy_goto(p, action_98, j),
        54 => happy_goto(p, action_99, j),
        61 => happy_goto(p, action_100, j),
        65 => happy_goto(p, action_293, j),
        86 => happy_goto(p, action_670, j),
        101 => happy_goto(p, action_671, j),
        102 => happy_goto(p, action_672, j),
        132 => happy_goto(p, action_109, j),
        133 => happy_goto(p, action_296, j),
        134 => happy_goto(p, action_111, j),
        _ => happy_reduce_473(p, j)
    }
}

fn action_629(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        181 => happy_shift(p, action_639, j),
        _ => happy_reduce_398(p, j)
    }
}

fn action_630(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_375, j),
        140 => happy_shift(p, action_184, j),
        150 => happy_shift(p, action_376, j),
        186 => happy_shift(p, action_173, j),
        193 => happy_shift(p, action_174, j),
        214 => happy_shift(p, action_175, j),
        215 => happy_shift(p, action_176, j),
        217 => happy_shift(p, action_177, j),
        232 => happy_shift(p, action_178, j),
        240 => happy_shift(p, action_144, j),
        64 => happy_goto(p, action_189, j),
        87 => happy_goto(p, action_667, j),
        88 => happy_goto(p, action_370, j),
        89 => happy_goto(p, action_181, j),
        90 => happy_goto(p, action_182, j),
        91 => happy_goto(p, action_371, j),
        92 => happy_goto(p, action_372, j),
        132 => happy_goto(p, action_668, j),
        133 => happy_goto(p, action_669, j),
        134 => happy_goto(p, action_111, j),
        _ => happy_reduce_473(p, j)
    }
}

fn action_631(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_329(p, j)
    }
}

fn action_632(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_375, j),
        140 => happy_shift(p, action_184, j),
        150 => happy_shift(p, action_376, j),
        186 => happy_reduce_474(p, j),
        193 => happy_reduce_474(p, j),
        214 => happy_reduce_474(p, j),
        215 => happy_reduce_474(p, j),
        217 => happy_reduce_474(p, j),
        232 => happy_reduce_474(p, j),
        240 => happy_shift(p, action_144, j),
        87 => happy_goto(p, action_666, j),
        88 => happy_goto(p, action_370, j),
        89 => happy_goto(p, action_181, j),
        90 => happy_goto(p, action_182, j),
        91 => happy_goto(p, action_371, j),
        92 => happy_goto(p, action_372, j),
        134 => happy_goto(p, action_169, j),
        _ => happy_reduce_331(p, j)
    }
}

fn action_633(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        139 => happy_shift(p, action_665, j),
        _ => happy_fail(p, j)
    }
}

fn action_634(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        139 => happy_shift(p, action_664, j),
        _ => happy_fail(p, j)
    }
}

fn action_635(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        139 => happy_shift(p, action_663, j),
        240 => happy_shift(p, action_144, j),
        134 => happy_goto(p, action_637, j),
        _ => happy_fail(p, j)
    }
}

fn action_636(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_375, j),
        140 => happy_shift(p, action_184, j),
        150 => happy_shift(p, action_376, j),
        185 => happy_shift(p, action_114, j),
        188 => happy_shift(p, action_116, j),
        190 => happy_shift(p, action_117, j),
        192 => happy_shift(p, action_118, j),
        195 => happy_shift(p, action_119, j),
        198 => happy_shift(p, action_120, j),
        200 => happy_shift(p, action_121, j),
        201 => happy_shift(p, action_122, j),
        202 => happy_shift(p, action_123, j),
        203 => happy_shift(p, action_124, j),
        208 => happy_shift(p, action_125, j),
        209 => happy_shift(p, action_126, j),
        210 => happy_shift(p, action_127, j),
        211 => happy_shift(p, action_128, j),
        213 => happy_shift(p, action_129, j),
        216 => happy_shift(p, action_130, j),
        219 => happy_shift(p, action_131, j),
        220 => happy_shift(p, action_132, j),
        222 => happy_shift(p, action_133, j),
        224 => happy_shift(p, action_135, j),
        226 => happy_shift(p, action_136, j),
        227 => happy_shift(p, action_170, j),
        228 => happy_shift(p, action_138, j),
        229 => happy_shift(p, action_139, j),
        230 => happy_shift(p, action_140, j),
        231 => happy_shift(p, action_141, j),
        239 => happy_shift(p, action_171, j),
        240 => happy_shift(p, action_144, j),
        40 => happy_goto(p, action_164, j),
        41 => happy_goto(p, action_86, j),
        42 => happy_goto(p, action_87, j),
        43 => happy_goto(p, action_88, j),
        45 => happy_goto(p, action_165, j),
        52 => happy_goto(p, action_166, j),
        53 => happy_goto(p, action_98, j),
        54 => happy_goto(p, action_99, j),
        61 => happy_goto(p, action_100, j),
        88 => happy_goto(p, action_660, j),
        89 => happy_goto(p, action_181, j),
        90 => happy_goto(p, action_182, j),
        91 => happy_goto(p, action_661, j),
        92 => happy_goto(p, action_662, j),
        134 => happy_goto(p, action_169, j),
        _ => happy_reduce_474(p, j)
    }
}

fn action_637(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_341(p, j)
    }
}

fn action_638(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_409(p, j)
    }
}

fn action_639(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_26, j),
        140 => happy_shift(p, action_657, j),
        143 => happy_shift(p, action_658, j),
        144 => happy_shift(p, action_27, j),
        145 => happy_shift(p, action_28, j),
        146 => happy_shift(p, action_29, j),
        147 => happy_shift(p, action_30, j),
        148 => happy_shift(p, action_31, j),
        149 => happy_shift(p, action_32, j),
        150 => happy_shift(p, action_33, j),
        153 => happy_shift(p, action_34, j),
        164 => happy_shift(p, action_35, j),
        181 => happy_shift(p, action_659, j),
        184 => happy_shift(p, action_36, j),
        205 => happy_shift(p, action_37, j),
        221 => happy_shift(p, action_38, j),
        234 => happy_shift(p, action_39, j),
        235 => happy_shift(p, action_40, j),
        236 => happy_shift(p, action_41, j),
        237 => happy_shift(p, action_42, j),
        238 => happy_shift(p, action_75, j),
        239 => happy_shift(p, action_76, j),
        241 => happy_shift(p, action_44, j),
        242 => happy_shift(p, action_45, j),
        243 => happy_shift(p, action_46, j),
        244 => happy_shift(p, action_47, j),
        245 => happy_shift(p, action_48, j),
        246 => happy_shift(p, action_49, j),
        247 => happy_shift(p, action_50, j),
        93 => happy_goto(p, action_649, j),
        95 => happy_goto(p, action_650, j),
        96 => happy_goto(p, action_651, j),
        97 => happy_goto(p, action_652, j),
        98 => happy_goto(p, action_653, j),
        99 => happy_goto(p, action_654, j),
        100 => happy_goto(p, action_6, j),
        104 => happy_goto(p, action_7, j),
        106 => happy_goto(p, action_8, j),
        107 => happy_goto(p, action_9, j),
        108 => happy_goto(p, action_10, j),
        109 => happy_goto(p, action_11, j),
        110 => happy_goto(p, action_12, j),
        111 => happy_goto(p, action_13, j),
        112 => happy_goto(p, action_14, j),
        113 => happy_goto(p, action_15, j),
        114 => happy_goto(p, action_16, j),
        115 => happy_goto(p, action_17, j),
        116 => happy_goto(p, action_18, j),
        117 => happy_goto(p, action_19, j),
        118 => happy_goto(p, action_20, j),
        119 => happy_goto(p, action_21, j),
        120 => happy_goto(p, action_655, j),
        127 => happy_goto(p, action_24, j),
        128 => happy_goto(p, action_25, j),
        131 => happy_goto(p, action_656, j),
        _ => happy_reduce_347(p, j)
    }
}

fn action_640(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_26, j),
        144 => happy_shift(p, action_27, j),
        145 => happy_shift(p, action_28, j),
        146 => happy_shift(p, action_29, j),
        147 => happy_shift(p, action_30, j),
        148 => happy_shift(p, action_31, j),
        149 => happy_shift(p, action_32, j),
        150 => happy_shift(p, action_33, j),
        153 => happy_shift(p, action_34, j),
        164 => happy_shift(p, action_35, j),
        184 => happy_shift(p, action_36, j),
        205 => happy_shift(p, action_37, j),
        221 => happy_shift(p, action_38, j),
        234 => happy_shift(p, action_39, j),
        235 => happy_shift(p, action_40, j),
        236 => happy_shift(p, action_41, j),
        237 => happy_shift(p, action_42, j),
        238 => happy_shift(p, action_43, j),
        241 => happy_shift(p, action_44, j),
        242 => happy_shift(p, action_45, j),
        243 => happy_shift(p, action_46, j),
        244 => happy_shift(p, action_47, j),
        245 => happy_shift(p, action_48, j),
        246 => happy_shift(p, action_49, j),
        247 => happy_shift(p, action_50, j),
        100 => happy_goto(p, action_6, j),
        104 => happy_goto(p, action_7, j),
        106 => happy_goto(p, action_8, j),
        107 => happy_goto(p, action_9, j),
        108 => happy_goto(p, action_10, j),
        109 => happy_goto(p, action_11, j),
        110 => happy_goto(p, action_12, j),
        111 => happy_goto(p, action_13, j),
        112 => happy_goto(p, action_14, j),
        113 => happy_goto(p, action_15, j),
        114 => happy_goto(p, action_16, j),
        115 => happy_goto(p, action_17, j),
        116 => happy_goto(p, action_18, j),
        117 => happy_goto(p, action_19, j),
        118 => happy_goto(p, action_20, j),
        119 => happy_goto(p, action_21, j),
        120 => happy_goto(p, action_648, j),
        127 => happy_goto(p, action_24, j),
        128 => happy_goto(p, action_25, j),
        _ => happy_fail(p, j)
    }
}

fn action_641(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_440(p, j)
    }
}

fn action_642(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_26, j),
        144 => happy_shift(p, action_27, j),
        145 => happy_shift(p, action_28, j),
        146 => happy_shift(p, action_29, j),
        147 => happy_shift(p, action_30, j),
        148 => happy_shift(p, action_31, j),
        149 => happy_shift(p, action_32, j),
        150 => happy_shift(p, action_33, j),
        153 => happy_shift(p, action_34, j),
        164 => happy_shift(p, action_35, j),
        184 => happy_shift(p, action_36, j),
        205 => happy_shift(p, action_37, j),
        221 => happy_shift(p, action_38, j),
        234 => happy_shift(p, action_39, j),
        235 => happy_shift(p, action_40, j),
        236 => happy_shift(p, action_41, j),
        237 => happy_shift(p, action_42, j),
        238 => happy_shift(p, action_43, j),
        241 => happy_shift(p, action_44, j),
        242 => happy_shift(p, action_45, j),
        243 => happy_shift(p, action_46, j),
        244 => happy_shift(p, action_47, j),
        245 => happy_shift(p, action_48, j),
        246 => happy_shift(p, action_49, j),
        247 => happy_shift(p, action_50, j),
        100 => happy_goto(p, action_6, j),
        104 => happy_goto(p, action_7, j),
        106 => happy_goto(p, action_261, j),
        107 => happy_goto(p, action_9, j),
        108 => happy_goto(p, action_10, j),
        109 => happy_goto(p, action_11, j),
        110 => happy_goto(p, action_12, j),
        111 => happy_goto(p, action_13, j),
        112 => happy_goto(p, action_14, j),
        113 => happy_goto(p, action_15, j),
        114 => happy_goto(p, action_16, j),
        115 => happy_goto(p, action_17, j),
        116 => happy_goto(p, action_18, j),
        117 => happy_goto(p, action_19, j),
        118 => happy_goto(p, action_20, j),
        119 => happy_goto(p, action_647, j),
        127 => happy_goto(p, action_24, j),
        128 => happy_goto(p, action_25, j),
        _ => happy_fail(p, j)
    }
}

fn action_643(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_381(p, j)
    }
}

fn action_644(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_26, j),
        144 => happy_shift(p, action_27, j),
        145 => happy_shift(p, action_28, j),
        146 => happy_shift(p, action_29, j),
        147 => happy_shift(p, action_30, j),
        148 => happy_shift(p, action_31, j),
        149 => happy_shift(p, action_32, j),
        150 => happy_shift(p, action_33, j),
        153 => happy_shift(p, action_34, j),
        164 => happy_shift(p, action_35, j),
        184 => happy_shift(p, action_36, j),
        205 => happy_shift(p, action_37, j),
        221 => happy_shift(p, action_38, j),
        234 => happy_shift(p, action_39, j),
        235 => happy_shift(p, action_40, j),
        236 => happy_shift(p, action_41, j),
        237 => happy_shift(p, action_42, j),
        238 => happy_shift(p, action_43, j),
        241 => happy_shift(p, action_44, j),
        242 => happy_shift(p, action_45, j),
        243 => happy_shift(p, action_46, j),
        244 => happy_shift(p, action_47, j),
        245 => happy_shift(p, action_48, j),
        246 => happy_shift(p, action_49, j),
        247 => happy_shift(p, action_50, j),
        100 => happy_goto(p, action_6, j),
        104 => happy_goto(p, action_7, j),
        106 => happy_goto(p, action_8, j),
        107 => happy_goto(p, action_9, j),
        108 => happy_goto(p, action_10, j),
        109 => happy_goto(p, action_11, j),
        110 => happy_goto(p, action_12, j),
        111 => happy_goto(p, action_13, j),
        112 => happy_goto(p, action_14, j),
        113 => happy_goto(p, action_15, j),
        114 => happy_goto(p, action_16, j),
        115 => happy_goto(p, action_17, j),
        116 => happy_goto(p, action_18, j),
        117 => happy_goto(p, action_19, j),
        118 => happy_goto(p, action_20, j),
        119 => happy_goto(p, action_21, j),
        120 => happy_goto(p, action_646, j),
        127 => happy_goto(p, action_24, j),
        128 => happy_goto(p, action_25, j),
        _ => happy_fail(p, j)
    }
}

fn action_645(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_379(p, j)
    }
}

fn action_646(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_389(p, j)
    }
}

fn action_647(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_439(p, j)
    }
}

fn action_648(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_457(p, j)
    }
}

fn action_649(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_348(p, j)
    }
}

fn action_650(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        179 => happy_shift(p, action_859, j),
        182 => happy_shift(p, action_860, j),
        _ => happy_fail(p, j)
    }
}

fn action_651(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_26, j),
        144 => happy_shift(p, action_27, j),
        145 => happy_shift(p, action_28, j),
        146 => happy_shift(p, action_29, j),
        147 => happy_shift(p, action_30, j),
        148 => happy_shift(p, action_31, j),
        149 => happy_shift(p, action_32, j),
        150 => happy_shift(p, action_33, j),
        153 => happy_shift(p, action_34, j),
        164 => happy_shift(p, action_35, j),
        181 => happy_shift(p, action_659, j),
        184 => happy_shift(p, action_36, j),
        205 => happy_shift(p, action_37, j),
        221 => happy_shift(p, action_38, j),
        234 => happy_shift(p, action_39, j),
        235 => happy_shift(p, action_40, j),
        236 => happy_shift(p, action_41, j),
        237 => happy_shift(p, action_42, j),
        238 => happy_shift(p, action_43, j),
        241 => happy_shift(p, action_44, j),
        242 => happy_shift(p, action_45, j),
        243 => happy_shift(p, action_46, j),
        244 => happy_shift(p, action_47, j),
        245 => happy_shift(p, action_48, j),
        246 => happy_shift(p, action_49, j),
        247 => happy_shift(p, action_50, j),
        93 => happy_goto(p, action_858, j),
        100 => happy_goto(p, action_6, j),
        104 => happy_goto(p, action_7, j),
        106 => happy_goto(p, action_8, j),
        107 => happy_goto(p, action_9, j),
        108 => happy_goto(p, action_10, j),
        109 => happy_goto(p, action_11, j),
        110 => happy_goto(p, action_12, j),
        111 => happy_goto(p, action_13, j),
        112 => happy_goto(p, action_14, j),
        113 => happy_goto(p, action_15, j),
        114 => happy_goto(p, action_16, j),
        115 => happy_goto(p, action_17, j),
        116 => happy_goto(p, action_18, j),
        117 => happy_goto(p, action_19, j),
        118 => happy_goto(p, action_20, j),
        119 => happy_goto(p, action_21, j),
        120 => happy_goto(p, action_655, j),
        127 => happy_goto(p, action_24, j),
        128 => happy_goto(p, action_25, j),
        _ => happy_fail(p, j)
    }
}

fn action_652(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        140 => happy_shift(p, action_657, j),
        143 => happy_shift(p, action_658, j),
        168 => happy_shift(p, action_857, j),
        98 => happy_goto(p, action_855, j),
        99 => happy_goto(p, action_856, j),
        _ => happy_fail(p, j)
    }
}

fn action_653(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_355(p, j)
    }
}

fn action_654(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        140 => happy_reduce_359(p, j),
        143 => happy_reduce_359(p, j),
        168 => happy_reduce_359(p, j),
        _ => happy_reduce_354(p, j)
    }
}

fn action_655(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_342(p, j)
    }
}

fn action_656(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        167 => happy_shift(p, action_854, j),
        _ => happy_fail(p, j)
    }
}

fn action_657(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_26, j),
        144 => happy_shift(p, action_27, j),
        145 => happy_shift(p, action_28, j),
        146 => happy_shift(p, action_29, j),
        147 => happy_shift(p, action_30, j),
        148 => happy_shift(p, action_31, j),
        149 => happy_shift(p, action_32, j),
        150 => happy_shift(p, action_33, j),
        153 => happy_shift(p, action_34, j),
        164 => happy_shift(p, action_35, j),
        184 => happy_shift(p, action_36, j),
        205 => happy_shift(p, action_37, j),
        221 => happy_shift(p, action_38, j),
        234 => happy_shift(p, action_39, j),
        235 => happy_shift(p, action_40, j),
        236 => happy_shift(p, action_41, j),
        237 => happy_shift(p, action_42, j),
        238 => happy_shift(p, action_43, j),
        241 => happy_shift(p, action_44, j),
        242 => happy_shift(p, action_45, j),
        243 => happy_shift(p, action_46, j),
        244 => happy_shift(p, action_47, j),
        245 => happy_shift(p, action_48, j),
        246 => happy_shift(p, action_49, j),
        247 => happy_shift(p, action_50, j),
        100 => happy_goto(p, action_6, j),
        104 => happy_goto(p, action_7, j),
        106 => happy_goto(p, action_261, j),
        107 => happy_goto(p, action_9, j),
        108 => happy_goto(p, action_10, j),
        109 => happy_goto(p, action_11, j),
        110 => happy_goto(p, action_12, j),
        111 => happy_goto(p, action_13, j),
        112 => happy_goto(p, action_14, j),
        113 => happy_goto(p, action_15, j),
        114 => happy_goto(p, action_16, j),
        115 => happy_goto(p, action_17, j),
        116 => happy_goto(p, action_18, j),
        117 => happy_goto(p, action_19, j),
        118 => happy_goto(p, action_20, j),
        119 => happy_goto(p, action_262, j),
        126 => happy_goto(p, action_853, j),
        127 => happy_goto(p, action_24, j),
        128 => happy_goto(p, action_25, j),
        _ => happy_fail(p, j)
    }
}

fn action_658(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        238 => happy_shift(p, action_256, j),
        239 => happy_shift(p, action_76, j),
        131 => happy_goto(p, action_852, j),
        _ => happy_fail(p, j)
    }
}

fn action_659(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_26, j),
        140 => happy_shift(p, action_657, j),
        143 => happy_shift(p, action_658, j),
        144 => happy_shift(p, action_27, j),
        145 => happy_shift(p, action_28, j),
        146 => happy_shift(p, action_29, j),
        147 => happy_shift(p, action_30, j),
        148 => happy_shift(p, action_31, j),
        149 => happy_shift(p, action_32, j),
        150 => happy_shift(p, action_33, j),
        153 => happy_shift(p, action_34, j),
        164 => happy_shift(p, action_35, j),
        181 => happy_shift(p, action_659, j),
        184 => happy_shift(p, action_36, j),
        205 => happy_shift(p, action_37, j),
        221 => happy_shift(p, action_38, j),
        234 => happy_shift(p, action_39, j),
        235 => happy_shift(p, action_40, j),
        236 => happy_shift(p, action_41, j),
        237 => happy_shift(p, action_42, j),
        238 => happy_shift(p, action_75, j),
        239 => happy_shift(p, action_76, j),
        241 => happy_shift(p, action_44, j),
        242 => happy_shift(p, action_45, j),
        243 => happy_shift(p, action_46, j),
        244 => happy_shift(p, action_47, j),
        245 => happy_shift(p, action_48, j),
        246 => happy_shift(p, action_49, j),
        247 => happy_shift(p, action_50, j),
        93 => happy_goto(p, action_649, j),
        95 => happy_goto(p, action_851, j),
        96 => happy_goto(p, action_651, j),
        97 => happy_goto(p, action_652, j),
        98 => happy_goto(p, action_653, j),
        99 => happy_goto(p, action_654, j),
        100 => happy_goto(p, action_6, j),
        104 => happy_goto(p, action_7, j),
        106 => happy_goto(p, action_8, j),
        107 => happy_goto(p, action_9, j),
        108 => happy_goto(p, action_10, j),
        109 => happy_goto(p, action_11, j),
        110 => happy_goto(p, action_12, j),
        111 => happy_goto(p, action_13, j),
        112 => happy_goto(p, action_14, j),
        113 => happy_goto(p, action_15, j),
        114 => happy_goto(p, action_16, j),
        115 => happy_goto(p, action_17, j),
        116 => happy_goto(p, action_18, j),
        117 => happy_goto(p, action_19, j),
        118 => happy_goto(p, action_20, j),
        119 => happy_goto(p, action_21, j),
        120 => happy_goto(p, action_655, j),
        127 => happy_goto(p, action_24, j),
        128 => happy_goto(p, action_25, j),
        131 => happy_goto(p, action_656, j),
        _ => happy_reduce_347(p, j)
    }
}

fn action_660(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        139 => happy_shift(p, action_850, j),
        _ => happy_fail(p, j)
    }
}

fn action_661(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        139 => happy_shift(p, action_849, j),
        _ => happy_fail(p, j)
    }
}

fn action_662(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        139 => happy_shift(p, action_848, j),
        240 => happy_shift(p, action_144, j),
        134 => happy_goto(p, action_637, j),
        _ => happy_fail(p, j)
    }
}

fn action_663(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_334(p, j)
    }
}

fn action_664(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_417, j),
        140 => happy_shift(p, action_184, j),
        88 => happy_goto(p, action_847, j),
        89 => happy_goto(p, action_181, j),
        90 => happy_goto(p, action_182, j),
        _ => happy_reduce_333(p, j)
    }
}

fn action_665(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_335(p, j)
    }
}

fn action_666(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_332(p, j)
    }
}

fn action_667(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_330(p, j)
    }
}

fn action_668(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_328(p, j)
    }
}

fn action_669(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        186 => happy_shift(p, action_173, j),
        193 => happy_shift(p, action_174, j),
        214 => happy_shift(p, action_175, j),
        215 => happy_shift(p, action_176, j),
        217 => happy_shift(p, action_177, j),
        232 => happy_shift(p, action_178, j),
        240 => happy_shift(p, action_144, j),
        64 => happy_goto(p, action_439, j),
        134 => happy_goto(p, action_169, j),
        _ => happy_reduce_474(p, j)
    }
}

fn action_670(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        167 => happy_shift(p, action_846, j),
        _ => happy_fail(p, j)
    }
}

fn action_671(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        139 => happy_shift(p, action_844, j),
        179 => happy_shift(p, action_845, j),
        _ => happy_fail(p, j)
    }
}

fn action_672(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_372(p, j)
    }
}

fn action_673(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        167 => happy_shift(p, action_843, j),
        _ => happy_fail(p, j)
    }
}

fn action_674(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        139 => happy_shift(p, action_842, j),
        _ => happy_fail(p, j)
    }
}

fn action_675(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        139 => happy_shift(p, action_839, j),
        140 => happy_shift(p, action_840, j),
        143 => happy_shift(p, action_841, j),
        _ => happy_fail(p, j)
    }
}

fn action_676(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_375(p, j)
    }
}

fn action_677(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        139 => happy_shift(p, action_838, j),
        _ => happy_fail(p, j)
    }
}

fn action_678(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        139 => happy_shift(p, action_837, j),
        _ => happy_fail(p, j)
    }
}

fn action_679(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_48(p, j)
    }
}

fn action_680(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        181 => happy_shift(p, action_62, j),
        14 => happy_goto(p, action_836, j),
        _ => happy_fail(p, j)
    }
}

fn action_681(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_479, j),
        150 => happy_shift(p, action_480, j),
        185 => happy_shift(p, action_114, j),
        186 => happy_shift(p, action_173, j),
        188 => happy_shift(p, action_116, j),
        190 => happy_shift(p, action_117, j),
        192 => happy_shift(p, action_118, j),
        193 => happy_shift(p, action_174, j),
        195 => happy_shift(p, action_119, j),
        198 => happy_shift(p, action_120, j),
        200 => happy_shift(p, action_121, j),
        201 => happy_shift(p, action_122, j),
        202 => happy_shift(p, action_123, j),
        203 => happy_shift(p, action_124, j),
        208 => happy_shift(p, action_125, j),
        209 => happy_shift(p, action_126, j),
        210 => happy_shift(p, action_127, j),
        211 => happy_shift(p, action_128, j),
        213 => happy_shift(p, action_129, j),
        214 => happy_shift(p, action_175, j),
        215 => happy_shift(p, action_176, j),
        216 => happy_shift(p, action_130, j),
        217 => happy_shift(p, action_177, j),
        219 => happy_shift(p, action_131, j),
        220 => happy_shift(p, action_132, j),
        222 => happy_shift(p, action_133, j),
        224 => happy_shift(p, action_135, j),
        226 => happy_shift(p, action_136, j),
        227 => happy_shift(p, action_442, j),
        228 => happy_shift(p, action_138, j),
        229 => happy_shift(p, action_139, j),
        230 => happy_shift(p, action_140, j),
        231 => happy_shift(p, action_141, j),
        232 => happy_shift(p, action_178, j),
        238 => happy_shift(p, action_142, j),
        239 => happy_shift(p, action_443, j),
        240 => happy_shift(p, action_144, j),
        11 => happy_goto(p, action_835, j),
        40 => happy_goto(p, action_436, j),
        41 => happy_goto(p, action_86, j),
        42 => happy_goto(p, action_87, j),
        43 => happy_goto(p, action_88, j),
        45 => happy_goto(p, action_437, j),
        52 => happy_goto(p, action_438, j),
        53 => happy_goto(p, action_98, j),
        54 => happy_goto(p, action_99, j),
        61 => happy_goto(p, action_100, j),
        64 => happy_goto(p, action_439, j),
        75 => happy_goto(p, action_440, j),
        76 => happy_goto(p, action_103, j),
        77 => happy_goto(p, action_104, j),
        78 => happy_goto(p, action_485, j),
        134 => happy_goto(p, action_169, j),
        _ => happy_fail(p, j)
    }
}

fn action_682(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        181 => happy_shift(p, action_62, j),
        14 => happy_goto(p, action_834, j),
        _ => happy_fail(p, j)
    }
}

fn action_683(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        181 => happy_shift(p, action_62, j),
        14 => happy_goto(p, action_833, j),
        _ => happy_fail(p, j)
    }
}

fn action_684(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        181 => happy_shift(p, action_62, j),
        14 => happy_goto(p, action_832, j),
        _ => happy_fail(p, j)
    }
}

fn action_685(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_38(p, j)
    }
}

fn action_686(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        179 => happy_shift(p, action_532, j),
        180 => happy_shift(p, action_831, j),
        _ => happy_fail(p, j)
    }
}

fn action_687(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        182 => happy_shift(p, action_830, j),
        _ => happy_fail(p, j)
    }
}

fn action_688(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_54(p, j)
    }
}

fn action_689(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        180 => happy_shift(p, action_829, j),
        _ => happy_fail(p, j)
    }
}

fn action_690(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        140 => happy_shift(p, action_828, j),
        237 => happy_shift(p, action_42, j),
        28 => happy_goto(p, action_824, j),
        29 => happy_goto(p, action_825, j),
        30 => happy_goto(p, action_826, j),
        128 => happy_goto(p, action_827, j),
        _ => happy_reduce_76(p, j)
    }
}

fn action_691(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_26, j),
        144 => happy_shift(p, action_27, j),
        145 => happy_shift(p, action_28, j),
        146 => happy_shift(p, action_29, j),
        147 => happy_shift(p, action_30, j),
        148 => happy_shift(p, action_31, j),
        149 => happy_shift(p, action_32, j),
        150 => happy_shift(p, action_33, j),
        153 => happy_shift(p, action_34, j),
        164 => happy_shift(p, action_35, j),
        180 => happy_shift(p, action_61, j),
        181 => happy_shift(p, action_62, j),
        184 => happy_shift(p, action_36, j),
        187 => happy_shift(p, action_63, j),
        189 => happy_shift(p, action_64, j),
        191 => happy_shift(p, action_65, j),
        194 => happy_shift(p, action_66, j),
        196 => happy_shift(p, action_67, j),
        197 => happy_shift(p, action_68, j),
        204 => happy_shift(p, action_69, j),
        205 => happy_shift(p, action_37, j),
        206 => happy_shift(p, action_70, j),
        207 => happy_shift(p, action_71, j),
        218 => happy_shift(p, action_72, j),
        221 => happy_shift(p, action_38, j),
        225 => happy_shift(p, action_73, j),
        233 => happy_shift(p, action_74, j),
        234 => happy_shift(p, action_39, j),
        235 => happy_shift(p, action_40, j),
        236 => happy_shift(p, action_41, j),
        237 => happy_shift(p, action_42, j),
        238 => happy_shift(p, action_75, j),
        239 => happy_shift(p, action_76, j),
        241 => happy_shift(p, action_44, j),
        242 => happy_shift(p, action_45, j),
        243 => happy_shift(p, action_46, j),
        244 => happy_shift(p, action_47, j),
        245 => happy_shift(p, action_48, j),
        246 => happy_shift(p, action_49, j),
        247 => happy_shift(p, action_50, j),
        12 => happy_goto(p, action_823, j),
        13 => happy_goto(p, action_52, j),
        14 => happy_goto(p, action_53, j),
        22 => happy_goto(p, action_54, j),
        23 => happy_goto(p, action_55, j),
        24 => happy_goto(p, action_56, j),
        25 => happy_goto(p, action_57, j),
        26 => happy_goto(p, action_58, j),
        100 => happy_goto(p, action_6, j),
        104 => happy_goto(p, action_7, j),
        106 => happy_goto(p, action_8, j),
        107 => happy_goto(p, action_9, j),
        108 => happy_goto(p, action_10, j),
        109 => happy_goto(p, action_11, j),
        110 => happy_goto(p, action_12, j),
        111 => happy_goto(p, action_13, j),
        112 => happy_goto(p, action_14, j),
        113 => happy_goto(p, action_15, j),
        114 => happy_goto(p, action_16, j),
        115 => happy_goto(p, action_17, j),
        116 => happy_goto(p, action_18, j),
        117 => happy_goto(p, action_19, j),
        118 => happy_goto(p, action_20, j),
        119 => happy_goto(p, action_21, j),
        120 => happy_goto(p, action_22, j),
        122 => happy_goto(p, action_59, j),
        127 => happy_goto(p, action_24, j),
        128 => happy_goto(p, action_25, j),
        131 => happy_goto(p, action_60, j),
        _ => happy_fail(p, j)
    }
}

fn action_692(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        139 => happy_shift(p, action_822, j),
        _ => happy_fail(p, j)
    }
}

fn action_693(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        180 => happy_shift(p, action_821, j),
        _ => happy_fail(p, j)
    }
}

fn action_694(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        180 => happy_shift(p, action_820, j),
        _ => happy_fail(p, j)
    }
}

fn action_695(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        199 => happy_shift(p, action_819, j),
        _ => happy_reduce_58(p, j)
    }
}

fn action_696(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_60(p, j)
    }
}

fn action_697(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_61(p, j)
    }
}

fn action_698(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        168 => happy_shift(p, action_520, j),
        94 => happy_goto(p, action_818, j),
        _ => happy_reduce_345(p, j)
    }
}

fn action_699(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        168 => happy_shift(p, action_520, j),
        94 => happy_goto(p, action_817, j),
        _ => happy_reduce_345(p, j)
    }
}

fn action_700(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        139 => happy_shift(p, action_816, j),
        _ => happy_fail(p, j)
    }
}

fn action_701(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_167(p, j)
    }
}

fn action_702(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_168(p, j)
    }
}

fn action_703(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_417, j),
        139 => happy_shift(p, action_815, j),
        140 => happy_shift(p, action_184, j),
        88 => happy_goto(p, action_577, j),
        89 => happy_goto(p, action_181, j),
        90 => happy_goto(p, action_182, j),
        _ => happy_fail(p, j)
    }
}

fn action_704(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_244(p, j)
    }
}

fn action_705(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_255(p, j)
    }
}

fn action_706(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_757, j),
        150 => happy_shift(p, action_529, j),
        238 => happy_shift(p, action_142, j),
        239 => happy_shift(p, action_428, j),
        240 => happy_shift(p, action_144, j),
        70 => happy_goto(p, action_423, j),
        71 => happy_goto(p, action_224, j),
        72 => happy_goto(p, action_424, j),
        73 => happy_goto(p, action_226, j),
        74 => happy_goto(p, action_814, j),
        76 => happy_goto(p, action_159, j),
        77 => happy_goto(p, action_104, j),
        78 => happy_goto(p, action_516, j),
        133 => happy_goto(p, action_426, j),
        134 => happy_goto(p, action_111, j),
        _ => happy_fail(p, j)
    }
}

fn action_707(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_250(p, j)
    }
}

fn action_708(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_246(p, j)
    }
}

fn action_709(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_258(p, j)
    }
}

fn action_710(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_257(p, j)
    }
}

fn action_711(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_575, j),
        150 => happy_shift(p, action_576, j),
        186 => happy_shift(p, action_173, j),
        193 => happy_shift(p, action_174, j),
        214 => happy_shift(p, action_175, j),
        215 => happy_shift(p, action_176, j),
        217 => happy_shift(p, action_177, j),
        232 => happy_shift(p, action_178, j),
        238 => happy_shift(p, action_142, j),
        239 => happy_shift(p, action_231, j),
        240 => happy_shift(p, action_144, j),
        64 => happy_goto(p, action_189, j),
        69 => happy_goto(p, action_582, j),
        70 => happy_goto(p, action_223, j),
        71 => happy_goto(p, action_224, j),
        75 => happy_goto(p, action_486, j),
        76 => happy_goto(p, action_103, j),
        77 => happy_goto(p, action_104, j),
        78 => happy_goto(p, action_485, j),
        133 => happy_goto(p, action_813, j),
        134 => happy_goto(p, action_111, j),
        _ => happy_fail(p, j)
    }
}

fn action_712(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_417, j),
        140 => happy_shift(p, action_184, j),
        88 => happy_goto(p, action_812, j),
        89 => happy_goto(p, action_181, j),
        90 => happy_goto(p, action_182, j),
        _ => happy_reduce_247(p, j)
    }
}

fn action_713(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_260(p, j)
    }
}

fn action_714(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        180 => happy_shift(p, action_721, j),
        182 => happy_shift(p, action_811, j),
        190 => happy_shift(p, action_117, j),
        192 => happy_shift(p, action_118, j),
        195 => happy_shift(p, action_119, j),
        198 => happy_shift(p, action_120, j),
        200 => happy_shift(p, action_121, j),
        202 => happy_shift(p, action_123, j),
        203 => happy_shift(p, action_124, j),
        209 => happy_shift(p, action_126, j),
        210 => happy_shift(p, action_127, j),
        211 => happy_shift(p, action_128, j),
        219 => happy_shift(p, action_131, j),
        220 => happy_shift(p, action_132, j),
        224 => happy_shift(p, action_135, j),
        227 => happy_shift(p, action_137, j),
        229 => happy_shift(p, action_139, j),
        230 => happy_shift(p, action_140, j),
        231 => happy_shift(p, action_141, j),
        239 => happy_shift(p, action_143, j),
        240 => happy_shift(p, action_144, j),
        241 => happy_shift(p, action_723, j),
        44 => happy_goto(p, action_715, j),
        45 => happy_goto(p, action_90, j),
        47 => happy_goto(p, action_290, j),
        49 => happy_goto(p, action_291, j),
        51 => happy_goto(p, action_292, j),
        52 => happy_goto(p, action_97, j),
        53 => happy_goto(p, action_98, j),
        54 => happy_goto(p, action_99, j),
        56 => happy_goto(p, action_716, j),
        57 => happy_goto(p, action_717, j),
        58 => happy_goto(p, action_718, j),
        61 => happy_goto(p, action_100, j),
        65 => happy_goto(p, action_719, j),
        132 => happy_goto(p, action_109, j),
        133 => happy_goto(p, action_720, j),
        134 => happy_goto(p, action_111, j),
        _ => happy_reduce_473(p, j)
    }
}

fn action_715(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_528, j),
        150 => happy_shift(p, action_529, j),
        167 => happy_shift(p, action_810, j),
        238 => happy_shift(p, action_142, j),
        239 => happy_shift(p, action_231, j),
        59 => happy_goto(p, action_808, j),
        66 => happy_goto(p, action_809, j),
        68 => happy_goto(p, action_221, j),
        69 => happy_goto(p, action_222, j),
        70 => happy_goto(p, action_223, j),
        71 => happy_goto(p, action_224, j),
        72 => happy_goto(p, action_225, j),
        73 => happy_goto(p, action_226, j),
        75 => happy_goto(p, action_527, j),
        76 => happy_goto(p, action_103, j),
        77 => happy_goto(p, action_104, j),
        78 => happy_goto(p, action_485, j),
        _ => happy_reduce_203(p, j)
    }
}

fn action_716(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_194(p, j)
    }
}

fn action_717(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        179 => happy_shift(p, action_806, j),
        180 => happy_shift(p, action_807, j),
        _ => happy_fail(p, j)
    }
}

fn action_718(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        179 => happy_shift(p, action_804, j),
        180 => happy_shift(p, action_805, j),
        _ => happy_fail(p, j)
    }
}

fn action_719(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        186 => happy_shift(p, action_173, j),
        190 => happy_shift(p, action_117, j),
        192 => happy_shift(p, action_118, j),
        193 => happy_shift(p, action_174, j),
        195 => happy_shift(p, action_119, j),
        198 => happy_shift(p, action_120, j),
        200 => happy_shift(p, action_121, j),
        202 => happy_shift(p, action_123, j),
        203 => happy_shift(p, action_124, j),
        209 => happy_shift(p, action_126, j),
        210 => happy_shift(p, action_127, j),
        211 => happy_shift(p, action_128, j),
        214 => happy_shift(p, action_175, j),
        215 => happy_shift(p, action_176, j),
        217 => happy_shift(p, action_177, j),
        219 => happy_shift(p, action_131, j),
        220 => happy_shift(p, action_132, j),
        224 => happy_shift(p, action_135, j),
        227 => happy_shift(p, action_193, j),
        229 => happy_shift(p, action_139, j),
        230 => happy_shift(p, action_140, j),
        231 => happy_shift(p, action_141, j),
        232 => happy_shift(p, action_178, j),
        239 => happy_shift(p, action_194, j),
        240 => happy_shift(p, action_144, j),
        45 => happy_goto(p, action_187, j),
        52 => happy_goto(p, action_188, j),
        53 => happy_goto(p, action_98, j),
        54 => happy_goto(p, action_99, j),
        61 => happy_goto(p, action_100, j),
        64 => happy_goto(p, action_189, j),
        132 => happy_goto(p, action_802, j),
        133 => happy_goto(p, action_803, j),
        134 => happy_goto(p, action_111, j),
        _ => happy_reduce_473(p, j)
    }
}

fn action_720(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_479, j),
        150 => happy_shift(p, action_480, j),
        167 => happy_shift(p, action_801, j),
        190 => happy_shift(p, action_117, j),
        192 => happy_shift(p, action_118, j),
        195 => happy_shift(p, action_119, j),
        198 => happy_shift(p, action_120, j),
        200 => happy_shift(p, action_121, j),
        202 => happy_shift(p, action_123, j),
        203 => happy_shift(p, action_124, j),
        209 => happy_shift(p, action_126, j),
        210 => happy_shift(p, action_127, j),
        211 => happy_shift(p, action_128, j),
        219 => happy_shift(p, action_131, j),
        220 => happy_shift(p, action_132, j),
        224 => happy_shift(p, action_135, j),
        227 => happy_shift(p, action_170, j),
        229 => happy_shift(p, action_139, j),
        230 => happy_shift(p, action_140, j),
        231 => happy_shift(p, action_141, j),
        238 => happy_shift(p, action_142, j),
        239 => happy_shift(p, action_171, j),
        240 => happy_shift(p, action_144, j),
        45 => happy_goto(p, action_165, j),
        52 => happy_goto(p, action_166, j),
        53 => happy_goto(p, action_98, j),
        54 => happy_goto(p, action_99, j),
        60 => happy_goto(p, action_799, j),
        61 => happy_goto(p, action_100, j),
        75 => happy_goto(p, action_800, j),
        76 => happy_goto(p, action_103, j),
        77 => happy_goto(p, action_104, j),
        78 => happy_goto(p, action_485, j),
        134 => happy_goto(p, action_169, j),
        _ => happy_reduce_474(p, j)
    }
}

fn action_721(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_193(p, j)
    }
}

fn action_722(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_188(p, j)
    }
}

fn action_723(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        190 => happy_shift(p, action_117, j),
        192 => happy_shift(p, action_118, j),
        195 => happy_shift(p, action_119, j),
        198 => happy_shift(p, action_120, j),
        200 => happy_shift(p, action_121, j),
        202 => happy_shift(p, action_123, j),
        203 => happy_shift(p, action_124, j),
        209 => happy_shift(p, action_126, j),
        210 => happy_shift(p, action_127, j),
        211 => happy_shift(p, action_128, j),
        219 => happy_shift(p, action_131, j),
        220 => happy_shift(p, action_132, j),
        224 => happy_shift(p, action_135, j),
        227 => happy_shift(p, action_137, j),
        229 => happy_shift(p, action_139, j),
        230 => happy_shift(p, action_140, j),
        231 => happy_shift(p, action_141, j),
        239 => happy_shift(p, action_143, j),
        240 => happy_shift(p, action_144, j),
        241 => happy_shift(p, action_723, j),
        44 => happy_goto(p, action_715, j),
        45 => happy_goto(p, action_90, j),
        47 => happy_goto(p, action_290, j),
        49 => happy_goto(p, action_291, j),
        51 => happy_goto(p, action_292, j),
        52 => happy_goto(p, action_97, j),
        53 => happy_goto(p, action_98, j),
        54 => happy_goto(p, action_99, j),
        56 => happy_goto(p, action_798, j),
        57 => happy_goto(p, action_717, j),
        58 => happy_goto(p, action_718, j),
        61 => happy_goto(p, action_100, j),
        65 => happy_goto(p, action_719, j),
        132 => happy_goto(p, action_109, j),
        133 => happy_goto(p, action_720, j),
        134 => happy_goto(p, action_111, j),
        _ => happy_reduce_473(p, j)
    }
}

fn action_724(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_175(p, j)
    }
}

fn action_725(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_176(p, j)
    }
}

fn action_726(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_94(p, j)
    }
}

fn action_727(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_25(p, j)
    }
}

fn action_728(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        139 => happy_shift(p, action_797, j),
        _ => happy_fail(p, j)
    }
}

fn action_729(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        139 => happy_shift(p, action_796, j),
        _ => happy_fail(p, j)
    }
}

fn action_730(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        141 => happy_shift(p, action_795, j),
        _ => happy_fail(p, j)
    }
}

fn action_731(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        141 => happy_shift(p, action_794, j),
        _ => happy_fail(p, j)
    }
}

fn action_732(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        141 => happy_reduce_473(p, j),
        240 => happy_shift(p, action_144, j),
        132 => happy_goto(p, action_793, j),
        133 => happy_goto(p, action_152, j),
        134 => happy_goto(p, action_111, j),
        _ => happy_reduce_403(p, j)
    }
}

fn action_733(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        240 => happy_shift(p, action_144, j),
        132 => happy_goto(p, action_792, j),
        133 => happy_goto(p, action_152, j),
        134 => happy_goto(p, action_111, j),
        _ => happy_reduce_473(p, j)
    }
}

fn action_734(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_318(p, j)
    }
}

fn action_735(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        141 => happy_shift(p, action_791, j),
        _ => happy_fail(p, j)
    }
}

fn action_736(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_317(p, j)
    }
}

fn action_737(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_323(p, j)
    }
}

fn action_738(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        141 => happy_shift(p, action_790, j),
        _ => happy_fail(p, j)
    }
}

fn action_739(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_26, j),
        144 => happy_shift(p, action_27, j),
        145 => happy_shift(p, action_28, j),
        146 => happy_shift(p, action_29, j),
        147 => happy_shift(p, action_30, j),
        148 => happy_shift(p, action_31, j),
        149 => happy_shift(p, action_32, j),
        150 => happy_shift(p, action_33, j),
        153 => happy_shift(p, action_34, j),
        164 => happy_shift(p, action_35, j),
        184 => happy_shift(p, action_36, j),
        205 => happy_shift(p, action_37, j),
        221 => happy_shift(p, action_38, j),
        234 => happy_shift(p, action_39, j),
        235 => happy_shift(p, action_40, j),
        236 => happy_shift(p, action_41, j),
        237 => happy_shift(p, action_42, j),
        238 => happy_shift(p, action_43, j),
        241 => happy_shift(p, action_44, j),
        242 => happy_shift(p, action_45, j),
        243 => happy_shift(p, action_46, j),
        244 => happy_shift(p, action_47, j),
        245 => happy_shift(p, action_48, j),
        246 => happy_shift(p, action_49, j),
        247 => happy_shift(p, action_50, j),
        100 => happy_goto(p, action_6, j),
        104 => happy_goto(p, action_7, j),
        106 => happy_goto(p, action_8, j),
        107 => happy_goto(p, action_9, j),
        108 => happy_goto(p, action_10, j),
        109 => happy_goto(p, action_11, j),
        110 => happy_goto(p, action_12, j),
        111 => happy_goto(p, action_13, j),
        112 => happy_goto(p, action_14, j),
        113 => happy_goto(p, action_15, j),
        114 => happy_goto(p, action_16, j),
        115 => happy_goto(p, action_17, j),
        116 => happy_goto(p, action_18, j),
        117 => happy_goto(p, action_19, j),
        118 => happy_goto(p, action_20, j),
        119 => happy_goto(p, action_21, j),
        120 => happy_goto(p, action_789, j),
        127 => happy_goto(p, action_24, j),
        128 => happy_goto(p, action_25, j),
        _ => happy_fail(p, j)
    }
}

fn action_740(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_290(p, j)
    }
}

fn action_741(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_291(p, j)
    }
}

fn action_742(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_294(p, j)
    }
}

fn action_743(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_544, j),
        140 => happy_shift(p, action_184, j),
        150 => happy_shift(p, action_545, j),
        186 => happy_shift(p, action_173, j),
        193 => happy_shift(p, action_174, j),
        214 => happy_shift(p, action_175, j),
        215 => happy_shift(p, action_176, j),
        217 => happy_shift(p, action_177, j),
        232 => happy_shift(p, action_178, j),
        238 => happy_shift(p, action_142, j),
        239 => happy_shift(p, action_231, j),
        240 => happy_shift(p, action_144, j),
        64 => happy_goto(p, action_189, j),
        69 => happy_goto(p, action_582, j),
        70 => happy_goto(p, action_223, j),
        71 => happy_goto(p, action_224, j),
        75 => happy_goto(p, action_486, j),
        76 => happy_goto(p, action_103, j),
        77 => happy_goto(p, action_104, j),
        78 => happy_goto(p, action_485, j),
        87 => happy_goto(p, action_667, j),
        88 => happy_goto(p, action_370, j),
        89 => happy_goto(p, action_181, j),
        90 => happy_goto(p, action_182, j),
        91 => happy_goto(p, action_371, j),
        92 => happy_goto(p, action_372, j),
        132 => happy_goto(p, action_668, j),
        133 => happy_goto(p, action_788, j),
        134 => happy_goto(p, action_111, j),
        _ => happy_reduce_473(p, j)
    }
}

fn action_744(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_544, j),
        140 => happy_shift(p, action_184, j),
        150 => happy_shift(p, action_545, j),
        186 => happy_reduce_474(p, j),
        193 => happy_reduce_474(p, j),
        214 => happy_reduce_474(p, j),
        215 => happy_reduce_474(p, j),
        217 => happy_reduce_474(p, j),
        232 => happy_reduce_474(p, j),
        238 => happy_shift(p, action_142, j),
        239 => happy_shift(p, action_231, j),
        240 => happy_shift(p, action_144, j),
        69 => happy_goto(p, action_581, j),
        70 => happy_goto(p, action_223, j),
        71 => happy_goto(p, action_224, j),
        75 => happy_goto(p, action_484, j),
        76 => happy_goto(p, action_103, j),
        77 => happy_goto(p, action_104, j),
        78 => happy_goto(p, action_485, j),
        87 => happy_goto(p, action_666, j),
        88 => happy_goto(p, action_370, j),
        89 => happy_goto(p, action_181, j),
        90 => happy_goto(p, action_182, j),
        91 => happy_goto(p, action_371, j),
        92 => happy_goto(p, action_372, j),
        134 => happy_goto(p, action_169, j),
        _ => happy_reduce_331(p, j)
    }
}

fn action_745(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_544, j),
        140 => happy_shift(p, action_184, j),
        150 => happy_shift(p, action_545, j),
        185 => happy_shift(p, action_114, j),
        188 => happy_shift(p, action_116, j),
        190 => happy_shift(p, action_117, j),
        192 => happy_shift(p, action_118, j),
        195 => happy_shift(p, action_119, j),
        198 => happy_shift(p, action_120, j),
        200 => happy_shift(p, action_121, j),
        201 => happy_shift(p, action_122, j),
        202 => happy_shift(p, action_123, j),
        203 => happy_shift(p, action_124, j),
        208 => happy_shift(p, action_125, j),
        209 => happy_shift(p, action_126, j),
        210 => happy_shift(p, action_127, j),
        211 => happy_shift(p, action_128, j),
        213 => happy_shift(p, action_129, j),
        216 => happy_shift(p, action_130, j),
        219 => happy_shift(p, action_131, j),
        220 => happy_shift(p, action_132, j),
        222 => happy_shift(p, action_133, j),
        224 => happy_shift(p, action_135, j),
        226 => happy_shift(p, action_136, j),
        227 => happy_shift(p, action_170, j),
        228 => happy_shift(p, action_138, j),
        229 => happy_shift(p, action_139, j),
        230 => happy_shift(p, action_140, j),
        231 => happy_shift(p, action_141, j),
        238 => happy_shift(p, action_142, j),
        239 => happy_shift(p, action_171, j),
        240 => happy_shift(p, action_144, j),
        40 => happy_goto(p, action_164, j),
        41 => happy_goto(p, action_86, j),
        42 => happy_goto(p, action_87, j),
        43 => happy_goto(p, action_88, j),
        45 => happy_goto(p, action_165, j),
        52 => happy_goto(p, action_166, j),
        53 => happy_goto(p, action_98, j),
        54 => happy_goto(p, action_99, j),
        61 => happy_goto(p, action_100, j),
        70 => happy_goto(p, action_574, j),
        71 => happy_goto(p, action_224, j),
        76 => happy_goto(p, action_477, j),
        77 => happy_goto(p, action_104, j),
        78 => happy_goto(p, action_478, j),
        88 => happy_goto(p, action_660, j),
        89 => happy_goto(p, action_181, j),
        90 => happy_goto(p, action_182, j),
        91 => happy_goto(p, action_661, j),
        92 => happy_goto(p, action_662, j),
        134 => happy_goto(p, action_169, j),
        _ => happy_reduce_474(p, j)
    }
}

fn action_746(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_297(p, j)
    }
}

fn action_747(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_298(p, j)
    }
}

fn action_748(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_539, j),
        140 => happy_shift(p, action_184, j),
        150 => happy_shift(p, action_540, j),
        186 => happy_shift(p, action_173, j),
        193 => happy_shift(p, action_174, j),
        214 => happy_shift(p, action_175, j),
        215 => happy_shift(p, action_176, j),
        217 => happy_shift(p, action_177, j),
        232 => happy_shift(p, action_178, j),
        238 => happy_shift(p, action_142, j),
        240 => happy_shift(p, action_144, j),
        64 => happy_goto(p, action_189, j),
        75 => happy_goto(p, action_486, j),
        76 => happy_goto(p, action_103, j),
        77 => happy_goto(p, action_104, j),
        78 => happy_goto(p, action_485, j),
        87 => happy_goto(p, action_667, j),
        88 => happy_goto(p, action_370, j),
        89 => happy_goto(p, action_181, j),
        90 => happy_goto(p, action_182, j),
        91 => happy_goto(p, action_371, j),
        92 => happy_goto(p, action_372, j),
        132 => happy_goto(p, action_668, j),
        133 => happy_goto(p, action_787, j),
        134 => happy_goto(p, action_111, j),
        _ => happy_reduce_473(p, j)
    }
}

fn action_749(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_539, j),
        140 => happy_shift(p, action_184, j),
        150 => happy_shift(p, action_540, j),
        186 => happy_reduce_474(p, j),
        193 => happy_reduce_474(p, j),
        214 => happy_reduce_474(p, j),
        215 => happy_reduce_474(p, j),
        217 => happy_reduce_474(p, j),
        232 => happy_reduce_474(p, j),
        238 => happy_shift(p, action_142, j),
        240 => happy_shift(p, action_144, j),
        75 => happy_goto(p, action_484, j),
        76 => happy_goto(p, action_103, j),
        77 => happy_goto(p, action_104, j),
        78 => happy_goto(p, action_485, j),
        87 => happy_goto(p, action_666, j),
        88 => happy_goto(p, action_370, j),
        89 => happy_goto(p, action_181, j),
        90 => happy_goto(p, action_182, j),
        91 => happy_goto(p, action_371, j),
        92 => happy_goto(p, action_372, j),
        134 => happy_goto(p, action_169, j),
        _ => happy_reduce_331(p, j)
    }
}

fn action_750(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_539, j),
        140 => happy_shift(p, action_184, j),
        150 => happy_shift(p, action_540, j),
        185 => happy_shift(p, action_114, j),
        188 => happy_shift(p, action_116, j),
        190 => happy_shift(p, action_117, j),
        192 => happy_shift(p, action_118, j),
        195 => happy_shift(p, action_119, j),
        198 => happy_shift(p, action_120, j),
        200 => happy_shift(p, action_121, j),
        201 => happy_shift(p, action_122, j),
        202 => happy_shift(p, action_123, j),
        203 => happy_shift(p, action_124, j),
        208 => happy_shift(p, action_125, j),
        209 => happy_shift(p, action_126, j),
        210 => happy_shift(p, action_127, j),
        211 => happy_shift(p, action_128, j),
        213 => happy_shift(p, action_129, j),
        216 => happy_shift(p, action_130, j),
        219 => happy_shift(p, action_131, j),
        220 => happy_shift(p, action_132, j),
        222 => happy_shift(p, action_133, j),
        224 => happy_shift(p, action_135, j),
        226 => happy_shift(p, action_136, j),
        227 => happy_shift(p, action_170, j),
        228 => happy_shift(p, action_138, j),
        229 => happy_shift(p, action_139, j),
        230 => happy_shift(p, action_140, j),
        231 => happy_shift(p, action_141, j),
        238 => happy_shift(p, action_142, j),
        239 => happy_shift(p, action_171, j),
        240 => happy_shift(p, action_144, j),
        40 => happy_goto(p, action_164, j),
        41 => happy_goto(p, action_86, j),
        42 => happy_goto(p, action_87, j),
        43 => happy_goto(p, action_88, j),
        45 => happy_goto(p, action_165, j),
        52 => happy_goto(p, action_166, j),
        53 => happy_goto(p, action_98, j),
        54 => happy_goto(p, action_99, j),
        61 => happy_goto(p, action_100, j),
        76 => happy_goto(p, action_477, j),
        77 => happy_goto(p, action_104, j),
        78 => happy_goto(p, action_478, j),
        88 => happy_goto(p, action_660, j),
        89 => happy_goto(p, action_181, j),
        90 => happy_goto(p, action_182, j),
        91 => happy_goto(p, action_661, j),
        92 => happy_goto(p, action_662, j),
        134 => happy_goto(p, action_169, j),
        _ => happy_reduce_474(p, j)
    }
}

fn action_751(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_302(p, j)
    }
}

fn action_752(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_287(p, j)
    }
}

fn action_753(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_285(p, j)
    }
}

fn action_754(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_304(p, j)
    }
}

fn action_755(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_786, j),
        150 => happy_shift(p, action_529, j),
        186 => happy_shift(p, action_173, j),
        193 => happy_shift(p, action_174, j),
        214 => happy_shift(p, action_175, j),
        215 => happy_shift(p, action_176, j),
        217 => happy_shift(p, action_177, j),
        232 => happy_shift(p, action_178, j),
        238 => happy_shift(p, action_142, j),
        239 => happy_shift(p, action_231, j),
        240 => happy_shift(p, action_144, j),
        64 => happy_goto(p, action_189, j),
        69 => happy_goto(p, action_582, j),
        70 => happy_goto(p, action_223, j),
        71 => happy_goto(p, action_224, j),
        72 => happy_goto(p, action_583, j),
        73 => happy_goto(p, action_226, j),
        75 => happy_goto(p, action_486, j),
        76 => happy_goto(p, action_103, j),
        77 => happy_goto(p, action_104, j),
        78 => happy_goto(p, action_485, j),
        133 => happy_goto(p, action_584, j),
        134 => happy_goto(p, action_111, j),
        _ => happy_fail(p, j)
    }
}

fn action_756(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_757, j),
        150 => happy_shift(p, action_529, j),
        238 => happy_shift(p, action_142, j),
        239 => happy_shift(p, action_428, j),
        240 => happy_shift(p, action_144, j),
        70 => happy_goto(p, action_423, j),
        71 => happy_goto(p, action_224, j),
        72 => happy_goto(p, action_424, j),
        73 => happy_goto(p, action_226, j),
        74 => happy_goto(p, action_580, j),
        76 => happy_goto(p, action_159, j),
        77 => happy_goto(p, action_104, j),
        78 => happy_goto(p, action_516, j),
        133 => happy_goto(p, action_426, j),
        134 => happy_goto(p, action_111, j),
        _ => happy_fail(p, j)
    }
}

fn action_757(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_757, j),
        150 => happy_shift(p, action_529, j),
        238 => happy_shift(p, action_142, j),
        239 => happy_shift(p, action_428, j),
        240 => happy_shift(p, action_144, j),
        70 => happy_goto(p, action_423, j),
        71 => happy_goto(p, action_224, j),
        72 => happy_goto(p, action_424, j),
        73 => happy_goto(p, action_226, j),
        74 => happy_goto(p, action_573, j),
        76 => happy_goto(p, action_159, j),
        77 => happy_goto(p, action_104, j),
        78 => happy_goto(p, action_516, j),
        133 => happy_goto(p, action_426, j),
        134 => happy_goto(p, action_111, j),
        _ => happy_fail(p, j)
    }
}

fn action_758(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        187 => happy_shift(p, action_410, j),
        35 => happy_goto(p, action_565, j),
        67 => happy_goto(p, action_409, j),
        _ => happy_reduce_233(p, j)
    }
}

fn action_759(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_178(p, j)
    }
}

fn action_760(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_179(p, j)
    }
}

fn action_761(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_346(p, j)
    }
}

fn action_762(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_272(p, j)
    }
}

fn action_763(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_11(p, j)
    }
}

fn action_764(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        179 => happy_shift(p, action_784, j),
        182 => happy_shift(p, action_785, j),
        _ => happy_fail(p, j)
    }
}

fn action_765(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        168 => happy_shift(p, action_783, j),
        240 => happy_shift(p, action_144, j),
        134 => happy_goto(p, action_169, j),
        _ => happy_reduce_219(p, j)
    }
}

fn action_766(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_26, j),
        144 => happy_shift(p, action_27, j),
        145 => happy_shift(p, action_28, j),
        146 => happy_shift(p, action_29, j),
        147 => happy_shift(p, action_30, j),
        148 => happy_shift(p, action_31, j),
        149 => happy_shift(p, action_32, j),
        150 => happy_shift(p, action_33, j),
        153 => happy_shift(p, action_34, j),
        164 => happy_shift(p, action_35, j),
        184 => happy_shift(p, action_36, j),
        205 => happy_shift(p, action_37, j),
        221 => happy_shift(p, action_38, j),
        234 => happy_shift(p, action_39, j),
        235 => happy_shift(p, action_40, j),
        236 => happy_shift(p, action_41, j),
        237 => happy_shift(p, action_42, j),
        238 => happy_shift(p, action_43, j),
        241 => happy_shift(p, action_44, j),
        242 => happy_shift(p, action_45, j),
        243 => happy_shift(p, action_46, j),
        244 => happy_shift(p, action_47, j),
        245 => happy_shift(p, action_48, j),
        246 => happy_shift(p, action_49, j),
        247 => happy_shift(p, action_50, j),
        100 => happy_goto(p, action_6, j),
        104 => happy_goto(p, action_7, j),
        106 => happy_goto(p, action_261, j),
        107 => happy_goto(p, action_9, j),
        108 => happy_goto(p, action_10, j),
        109 => happy_goto(p, action_11, j),
        110 => happy_goto(p, action_12, j),
        111 => happy_goto(p, action_13, j),
        112 => happy_goto(p, action_14, j),
        113 => happy_goto(p, action_15, j),
        114 => happy_goto(p, action_16, j),
        115 => happy_goto(p, action_17, j),
        116 => happy_goto(p, action_18, j),
        117 => happy_goto(p, action_19, j),
        118 => happy_goto(p, action_20, j),
        119 => happy_goto(p, action_262, j),
        126 => happy_goto(p, action_782, j),
        127 => happy_goto(p, action_24, j),
        128 => happy_goto(p, action_25, j),
        _ => happy_fail(p, j)
    }
}

fn action_767(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        182 => happy_shift(p, action_781, j),
        238 => happy_shift(p, action_256, j),
        239 => happy_shift(p, action_76, j),
        63 => happy_goto(p, action_780, j),
        131 => happy_goto(p, action_507, j),
        _ => happy_fail(p, j)
    }
}

fn action_768(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_211(p, j)
    }
}

fn action_769(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        139 => happy_shift(p, action_779, j),
        _ => happy_fail(p, j)
    }
}

fn action_770(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_26, j),
        139 => happy_shift(p, action_778, j),
        144 => happy_shift(p, action_27, j),
        145 => happy_shift(p, action_28, j),
        146 => happy_shift(p, action_29, j),
        147 => happy_shift(p, action_30, j),
        148 => happy_shift(p, action_31, j),
        149 => happy_shift(p, action_32, j),
        150 => happy_shift(p, action_33, j),
        153 => happy_shift(p, action_34, j),
        164 => happy_shift(p, action_35, j),
        184 => happy_shift(p, action_36, j),
        205 => happy_shift(p, action_37, j),
        221 => happy_shift(p, action_38, j),
        234 => happy_shift(p, action_39, j),
        235 => happy_shift(p, action_40, j),
        236 => happy_shift(p, action_41, j),
        237 => happy_shift(p, action_42, j),
        238 => happy_shift(p, action_43, j),
        241 => happy_shift(p, action_44, j),
        242 => happy_shift(p, action_45, j),
        243 => happy_shift(p, action_46, j),
        244 => happy_shift(p, action_47, j),
        245 => happy_shift(p, action_48, j),
        246 => happy_shift(p, action_49, j),
        247 => happy_shift(p, action_50, j),
        100 => happy_goto(p, action_6, j),
        104 => happy_goto(p, action_7, j),
        106 => happy_goto(p, action_775, j),
        107 => happy_goto(p, action_9, j),
        108 => happy_goto(p, action_10, j),
        109 => happy_goto(p, action_11, j),
        110 => happy_goto(p, action_12, j),
        111 => happy_goto(p, action_13, j),
        112 => happy_goto(p, action_14, j),
        113 => happy_goto(p, action_15, j),
        114 => happy_goto(p, action_16, j),
        115 => happy_goto(p, action_17, j),
        116 => happy_goto(p, action_18, j),
        117 => happy_goto(p, action_19, j),
        118 => happy_goto(p, action_20, j),
        119 => happy_goto(p, action_262, j),
        126 => happy_goto(p, action_776, j),
        127 => happy_goto(p, action_24, j),
        128 => happy_goto(p, action_25, j),
        137 => happy_goto(p, action_777, j),
        _ => happy_fail(p, j)
    }
}

fn action_771(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        139 => happy_shift(p, action_774, j),
        _ => happy_fail(p, j)
    }
}

fn action_772(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        193 => happy_shift(p, action_500, j),
        238 => happy_shift(p, action_501, j),
        136 => happy_goto(p, action_773, j),
        _ => happy_reduce_480(p, j)
    }
}

fn action_773(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_479(p, j)
    }
}

fn action_774(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_477(p, j)
    }
}

fn action_775(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        168 => happy_shift(p, action_319, j),
        169 => happy_shift(p, action_320, j),
        170 => happy_shift(p, action_321, j),
        171 => happy_shift(p, action_322, j),
        172 => happy_shift(p, action_323, j),
        173 => happy_shift(p, action_324, j),
        174 => happy_shift(p, action_325, j),
        175 => happy_shift(p, action_326, j),
        176 => happy_shift(p, action_327, j),
        177 => happy_shift(p, action_328, j),
        178 => happy_shift(p, action_329, j),
        121 => happy_goto(p, action_903, j),
        _ => happy_reduce_408(p, j)
    }
}

fn action_776(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_485(p, j)
    }
}

fn action_777(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        139 => happy_shift(p, action_901, j),
        179 => happy_shift(p, action_902, j),
        _ => happy_fail(p, j)
    }
}

fn action_778(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_484(p, j)
    }
}

fn action_779(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        180 => happy_shift(p, action_900, j),
        _ => happy_fail(p, j)
    }
}

fn action_780(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_217(p, j)
    }
}

fn action_781(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_212(p, j)
    }
}

fn action_782(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_221(p, j)
    }
}

fn action_783(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_26, j),
        144 => happy_shift(p, action_27, j),
        145 => happy_shift(p, action_28, j),
        146 => happy_shift(p, action_29, j),
        147 => happy_shift(p, action_30, j),
        148 => happy_shift(p, action_31, j),
        149 => happy_shift(p, action_32, j),
        150 => happy_shift(p, action_33, j),
        153 => happy_shift(p, action_34, j),
        164 => happy_shift(p, action_35, j),
        184 => happy_shift(p, action_36, j),
        205 => happy_shift(p, action_37, j),
        221 => happy_shift(p, action_38, j),
        234 => happy_shift(p, action_39, j),
        235 => happy_shift(p, action_40, j),
        236 => happy_shift(p, action_41, j),
        237 => happy_shift(p, action_42, j),
        238 => happy_shift(p, action_43, j),
        241 => happy_shift(p, action_44, j),
        242 => happy_shift(p, action_45, j),
        243 => happy_shift(p, action_46, j),
        244 => happy_shift(p, action_47, j),
        245 => happy_shift(p, action_48, j),
        246 => happy_shift(p, action_49, j),
        247 => happy_shift(p, action_50, j),
        100 => happy_goto(p, action_6, j),
        104 => happy_goto(p, action_7, j),
        106 => happy_goto(p, action_261, j),
        107 => happy_goto(p, action_9, j),
        108 => happy_goto(p, action_10, j),
        109 => happy_goto(p, action_11, j),
        110 => happy_goto(p, action_12, j),
        111 => happy_goto(p, action_13, j),
        112 => happy_goto(p, action_14, j),
        113 => happy_goto(p, action_15, j),
        114 => happy_goto(p, action_16, j),
        115 => happy_goto(p, action_17, j),
        116 => happy_goto(p, action_18, j),
        117 => happy_goto(p, action_19, j),
        118 => happy_goto(p, action_20, j),
        119 => happy_goto(p, action_262, j),
        126 => happy_goto(p, action_899, j),
        127 => happy_goto(p, action_24, j),
        128 => happy_goto(p, action_25, j),
        _ => happy_fail(p, j)
    }
}

fn action_784(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        182 => happy_shift(p, action_898, j),
        238 => happy_shift(p, action_256, j),
        239 => happy_shift(p, action_76, j),
        63 => happy_goto(p, action_780, j),
        131 => happy_goto(p, action_507, j),
        _ => happy_fail(p, j)
    }
}

fn action_785(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_213(p, j)
    }
}

fn action_786(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_757, j),
        150 => happy_shift(p, action_529, j),
        238 => happy_shift(p, action_142, j),
        239 => happy_shift(p, action_428, j),
        240 => happy_shift(p, action_144, j),
        70 => happy_goto(p, action_423, j),
        71 => happy_goto(p, action_224, j),
        72 => happy_goto(p, action_424, j),
        73 => happy_goto(p, action_226, j),
        74 => happy_goto(p, action_703, j),
        76 => happy_goto(p, action_159, j),
        77 => happy_goto(p, action_104, j),
        78 => happy_goto(p, action_516, j),
        133 => happy_goto(p, action_426, j),
        134 => happy_goto(p, action_111, j),
        _ => happy_fail(p, j)
    }
}

fn action_787(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_479, j),
        150 => happy_shift(p, action_480, j),
        186 => happy_shift(p, action_173, j),
        193 => happy_shift(p, action_174, j),
        214 => happy_shift(p, action_175, j),
        215 => happy_shift(p, action_176, j),
        217 => happy_shift(p, action_177, j),
        232 => happy_shift(p, action_178, j),
        238 => happy_shift(p, action_142, j),
        240 => happy_shift(p, action_144, j),
        64 => happy_goto(p, action_439, j),
        75 => happy_goto(p, action_512, j),
        76 => happy_goto(p, action_103, j),
        77 => happy_goto(p, action_104, j),
        78 => happy_goto(p, action_485, j),
        134 => happy_goto(p, action_169, j),
        _ => happy_reduce_474(p, j)
    }
}

fn action_788(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_575, j),
        150 => happy_shift(p, action_576, j),
        186 => happy_shift(p, action_173, j),
        193 => happy_shift(p, action_174, j),
        214 => happy_shift(p, action_175, j),
        215 => happy_shift(p, action_176, j),
        217 => happy_shift(p, action_177, j),
        232 => happy_shift(p, action_178, j),
        238 => happy_shift(p, action_142, j),
        239 => happy_shift(p, action_231, j),
        240 => happy_shift(p, action_144, j),
        64 => happy_goto(p, action_439, j),
        69 => happy_goto(p, action_704, j),
        70 => happy_goto(p, action_223, j),
        71 => happy_goto(p, action_224, j),
        75 => happy_goto(p, action_512, j),
        76 => happy_goto(p, action_103, j),
        77 => happy_goto(p, action_104, j),
        78 => happy_goto(p, action_485, j),
        134 => happy_goto(p, action_169, j),
        _ => happy_reduce_474(p, j)
    }
}

fn action_789(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        141 => happy_shift(p, action_897, j),
        _ => happy_fail(p, j)
    }
}

fn action_790(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_320(p, j)
    }
}

fn action_791(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_324(p, j)
    }
}

fn action_792(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_26, j),
        144 => happy_shift(p, action_27, j),
        145 => happy_shift(p, action_28, j),
        146 => happy_shift(p, action_29, j),
        147 => happy_shift(p, action_30, j),
        148 => happy_shift(p, action_31, j),
        149 => happy_shift(p, action_32, j),
        150 => happy_shift(p, action_33, j),
        153 => happy_shift(p, action_34, j),
        164 => happy_shift(p, action_35, j),
        184 => happy_shift(p, action_36, j),
        205 => happy_shift(p, action_37, j),
        221 => happy_shift(p, action_38, j),
        234 => happy_shift(p, action_39, j),
        235 => happy_shift(p, action_40, j),
        236 => happy_shift(p, action_41, j),
        237 => happy_shift(p, action_42, j),
        238 => happy_shift(p, action_43, j),
        241 => happy_shift(p, action_44, j),
        242 => happy_shift(p, action_45, j),
        243 => happy_shift(p, action_46, j),
        244 => happy_shift(p, action_47, j),
        245 => happy_shift(p, action_48, j),
        246 => happy_shift(p, action_49, j),
        247 => happy_shift(p, action_50, j),
        100 => happy_goto(p, action_6, j),
        104 => happy_goto(p, action_7, j),
        106 => happy_goto(p, action_8, j),
        107 => happy_goto(p, action_9, j),
        108 => happy_goto(p, action_10, j),
        109 => happy_goto(p, action_11, j),
        110 => happy_goto(p, action_12, j),
        111 => happy_goto(p, action_13, j),
        112 => happy_goto(p, action_14, j),
        113 => happy_goto(p, action_15, j),
        114 => happy_goto(p, action_16, j),
        115 => happy_goto(p, action_17, j),
        116 => happy_goto(p, action_18, j),
        117 => happy_goto(p, action_19, j),
        118 => happy_goto(p, action_20, j),
        119 => happy_goto(p, action_21, j),
        120 => happy_goto(p, action_896, j),
        127 => happy_goto(p, action_24, j),
        128 => happy_goto(p, action_25, j),
        _ => happy_fail(p, j)
    }
}

fn action_793(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        141 => happy_shift(p, action_895, j),
        _ => happy_fail(p, j)
    }
}

fn action_794(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_319(p, j)
    }
}

fn action_795(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_325(p, j)
    }
}

fn action_796(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_181(p, j)
    }
}

fn action_797(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_182(p, j)
    }
}

fn action_798(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_197(p, j)
    }
}

fn action_799(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        240 => happy_shift(p, action_144, j),
        134 => happy_goto(p, action_894, j),
        _ => happy_reduce_199(p, j)
    }
}

fn action_800(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        167 => happy_shift(p, action_893, j),
        _ => happy_reduce_207(p, j)
    }
}

fn action_801(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_26, j),
        144 => happy_shift(p, action_27, j),
        145 => happy_shift(p, action_28, j),
        146 => happy_shift(p, action_29, j),
        147 => happy_shift(p, action_30, j),
        148 => happy_shift(p, action_31, j),
        149 => happy_shift(p, action_32, j),
        150 => happy_shift(p, action_33, j),
        153 => happy_shift(p, action_34, j),
        164 => happy_shift(p, action_35, j),
        184 => happy_shift(p, action_36, j),
        205 => happy_shift(p, action_37, j),
        221 => happy_shift(p, action_38, j),
        234 => happy_shift(p, action_39, j),
        235 => happy_shift(p, action_40, j),
        236 => happy_shift(p, action_41, j),
        237 => happy_shift(p, action_42, j),
        238 => happy_shift(p, action_43, j),
        241 => happy_shift(p, action_44, j),
        242 => happy_shift(p, action_45, j),
        243 => happy_shift(p, action_46, j),
        244 => happy_shift(p, action_47, j),
        245 => happy_shift(p, action_48, j),
        246 => happy_shift(p, action_49, j),
        247 => happy_shift(p, action_50, j),
        100 => happy_goto(p, action_6, j),
        104 => happy_goto(p, action_7, j),
        106 => happy_goto(p, action_261, j),
        107 => happy_goto(p, action_9, j),
        108 => happy_goto(p, action_10, j),
        109 => happy_goto(p, action_11, j),
        110 => happy_goto(p, action_12, j),
        111 => happy_goto(p, action_13, j),
        112 => happy_goto(p, action_14, j),
        113 => happy_goto(p, action_15, j),
        114 => happy_goto(p, action_16, j),
        115 => happy_goto(p, action_17, j),
        116 => happy_goto(p, action_18, j),
        117 => happy_goto(p, action_19, j),
        118 => happy_goto(p, action_20, j),
        119 => happy_goto(p, action_262, j),
        126 => happy_goto(p, action_892, j),
        127 => happy_goto(p, action_24, j),
        128 => happy_goto(p, action_25, j),
        _ => happy_fail(p, j)
    }
}

fn action_802(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_479, j),
        150 => happy_shift(p, action_480, j),
        167 => happy_shift(p, action_801, j),
        238 => happy_shift(p, action_142, j),
        60 => happy_goto(p, action_891, j),
        75 => happy_goto(p, action_800, j),
        76 => happy_goto(p, action_103, j),
        77 => happy_goto(p, action_104, j),
        78 => happy_goto(p, action_485, j),
        _ => happy_fail(p, j)
    }
}

fn action_803(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        186 => happy_shift(p, action_173, j),
        190 => happy_shift(p, action_117, j),
        192 => happy_shift(p, action_118, j),
        193 => happy_shift(p, action_174, j),
        195 => happy_shift(p, action_119, j),
        198 => happy_shift(p, action_120, j),
        200 => happy_shift(p, action_121, j),
        202 => happy_shift(p, action_123, j),
        203 => happy_shift(p, action_124, j),
        209 => happy_shift(p, action_126, j),
        210 => happy_shift(p, action_127, j),
        211 => happy_shift(p, action_128, j),
        214 => happy_shift(p, action_175, j),
        215 => happy_shift(p, action_176, j),
        217 => happy_shift(p, action_177, j),
        219 => happy_shift(p, action_131, j),
        220 => happy_shift(p, action_132, j),
        224 => happy_shift(p, action_135, j),
        227 => happy_shift(p, action_442, j),
        229 => happy_shift(p, action_139, j),
        230 => happy_shift(p, action_140, j),
        231 => happy_shift(p, action_141, j),
        232 => happy_shift(p, action_178, j),
        239 => happy_shift(p, action_443, j),
        240 => happy_shift(p, action_144, j),
        45 => happy_goto(p, action_437, j),
        52 => happy_goto(p, action_438, j),
        53 => happy_goto(p, action_98, j),
        54 => happy_goto(p, action_99, j),
        61 => happy_goto(p, action_100, j),
        64 => happy_goto(p, action_439, j),
        134 => happy_goto(p, action_169, j),
        _ => happy_reduce_474(p, j)
    }
}

fn action_804(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        240 => happy_shift(p, action_144, j),
        132 => happy_goto(p, action_890, j),
        133 => happy_goto(p, action_152, j),
        134 => happy_goto(p, action_111, j),
        _ => happy_reduce_473(p, j)
    }
}

fn action_805(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_195(p, j)
    }
}

fn action_806(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        240 => happy_shift(p, action_144, j),
        132 => happy_goto(p, action_889, j),
        133 => happy_goto(p, action_152, j),
        134 => happy_goto(p, action_111, j),
        _ => happy_reduce_473(p, j)
    }
}

fn action_807(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_196(p, j)
    }
}

fn action_808(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        240 => happy_shift(p, action_144, j),
        132 => happy_goto(p, action_888, j),
        133 => happy_goto(p, action_152, j),
        134 => happy_goto(p, action_111, j),
        _ => happy_reduce_473(p, j)
    }
}

fn action_809(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        167 => happy_shift(p, action_887, j),
        _ => happy_reduce_204(p, j)
    }
}

fn action_810(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_26, j),
        144 => happy_shift(p, action_27, j),
        145 => happy_shift(p, action_28, j),
        146 => happy_shift(p, action_29, j),
        147 => happy_shift(p, action_30, j),
        148 => happy_shift(p, action_31, j),
        149 => happy_shift(p, action_32, j),
        150 => happy_shift(p, action_33, j),
        153 => happy_shift(p, action_34, j),
        164 => happy_shift(p, action_35, j),
        184 => happy_shift(p, action_36, j),
        205 => happy_shift(p, action_37, j),
        221 => happy_shift(p, action_38, j),
        234 => happy_shift(p, action_39, j),
        235 => happy_shift(p, action_40, j),
        236 => happy_shift(p, action_41, j),
        237 => happy_shift(p, action_42, j),
        238 => happy_shift(p, action_43, j),
        241 => happy_shift(p, action_44, j),
        242 => happy_shift(p, action_45, j),
        243 => happy_shift(p, action_46, j),
        244 => happy_shift(p, action_47, j),
        245 => happy_shift(p, action_48, j),
        246 => happy_shift(p, action_49, j),
        247 => happy_shift(p, action_50, j),
        100 => happy_goto(p, action_6, j),
        104 => happy_goto(p, action_7, j),
        106 => happy_goto(p, action_261, j),
        107 => happy_goto(p, action_9, j),
        108 => happy_goto(p, action_10, j),
        109 => happy_goto(p, action_11, j),
        110 => happy_goto(p, action_12, j),
        111 => happy_goto(p, action_13, j),
        112 => happy_goto(p, action_14, j),
        113 => happy_goto(p, action_15, j),
        114 => happy_goto(p, action_16, j),
        115 => happy_goto(p, action_17, j),
        116 => happy_goto(p, action_18, j),
        117 => happy_goto(p, action_19, j),
        118 => happy_goto(p, action_20, j),
        119 => happy_goto(p, action_262, j),
        126 => happy_goto(p, action_886, j),
        127 => happy_goto(p, action_24, j),
        128 => happy_goto(p, action_25, j),
        _ => happy_fail(p, j)
    }
}

fn action_811(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_187(p, j)
    }
}

fn action_812(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_248(p, j)
    }
}

fn action_813(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_575, j),
        150 => happy_shift(p, action_576, j),
        186 => happy_shift(p, action_173, j),
        193 => happy_shift(p, action_174, j),
        214 => happy_shift(p, action_175, j),
        215 => happy_shift(p, action_176, j),
        217 => happy_shift(p, action_177, j),
        232 => happy_shift(p, action_178, j),
        238 => happy_shift(p, action_142, j),
        239 => happy_shift(p, action_231, j),
        240 => happy_shift(p, action_144, j),
        64 => happy_goto(p, action_439, j),
        69 => happy_goto(p, action_704, j),
        70 => happy_goto(p, action_223, j),
        71 => happy_goto(p, action_224, j),
        75 => happy_goto(p, action_512, j),
        76 => happy_goto(p, action_103, j),
        77 => happy_goto(p, action_104, j),
        78 => happy_goto(p, action_485, j),
        134 => happy_goto(p, action_169, j),
        _ => happy_fail(p, j)
    }
}

fn action_814(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_417, j),
        139 => happy_shift(p, action_885, j),
        140 => happy_shift(p, action_184, j),
        88 => happy_goto(p, action_577, j),
        89 => happy_goto(p, action_181, j),
        90 => happy_goto(p, action_182, j),
        _ => happy_fail(p, j)
    }
}

fn action_815(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_251(p, j)
    }
}

fn action_816(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_234(p, j)
    }
}

fn action_817(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_100(p, j)
    }
}

fn action_818(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_96(p, j)
    }
}

fn action_819(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_26, j),
        144 => happy_shift(p, action_27, j),
        145 => happy_shift(p, action_28, j),
        146 => happy_shift(p, action_29, j),
        147 => happy_shift(p, action_30, j),
        148 => happy_shift(p, action_31, j),
        149 => happy_shift(p, action_32, j),
        150 => happy_shift(p, action_33, j),
        153 => happy_shift(p, action_34, j),
        164 => happy_shift(p, action_35, j),
        180 => happy_shift(p, action_61, j),
        181 => happy_shift(p, action_62, j),
        184 => happy_shift(p, action_36, j),
        187 => happy_shift(p, action_63, j),
        189 => happy_shift(p, action_64, j),
        191 => happy_shift(p, action_65, j),
        194 => happy_shift(p, action_66, j),
        196 => happy_shift(p, action_67, j),
        197 => happy_shift(p, action_68, j),
        204 => happy_shift(p, action_69, j),
        205 => happy_shift(p, action_37, j),
        206 => happy_shift(p, action_70, j),
        207 => happy_shift(p, action_71, j),
        218 => happy_shift(p, action_72, j),
        221 => happy_shift(p, action_38, j),
        225 => happy_shift(p, action_73, j),
        233 => happy_shift(p, action_74, j),
        234 => happy_shift(p, action_39, j),
        235 => happy_shift(p, action_40, j),
        236 => happy_shift(p, action_41, j),
        237 => happy_shift(p, action_42, j),
        238 => happy_shift(p, action_75, j),
        239 => happy_shift(p, action_76, j),
        241 => happy_shift(p, action_44, j),
        242 => happy_shift(p, action_45, j),
        243 => happy_shift(p, action_46, j),
        244 => happy_shift(p, action_47, j),
        245 => happy_shift(p, action_48, j),
        246 => happy_shift(p, action_49, j),
        247 => happy_shift(p, action_50, j),
        12 => happy_goto(p, action_884, j),
        13 => happy_goto(p, action_52, j),
        14 => happy_goto(p, action_53, j),
        22 => happy_goto(p, action_54, j),
        23 => happy_goto(p, action_55, j),
        24 => happy_goto(p, action_56, j),
        25 => happy_goto(p, action_57, j),
        26 => happy_goto(p, action_58, j),
        100 => happy_goto(p, action_6, j),
        104 => happy_goto(p, action_7, j),
        106 => happy_goto(p, action_8, j),
        107 => happy_goto(p, action_9, j),
        108 => happy_goto(p, action_10, j),
        109 => happy_goto(p, action_11, j),
        110 => happy_goto(p, action_12, j),
        111 => happy_goto(p, action_13, j),
        112 => happy_goto(p, action_14, j),
        113 => happy_goto(p, action_15, j),
        114 => happy_goto(p, action_16, j),
        115 => happy_goto(p, action_17, j),
        116 => happy_goto(p, action_18, j),
        117 => happy_goto(p, action_19, j),
        118 => happy_goto(p, action_20, j),
        119 => happy_goto(p, action_21, j),
        120 => happy_goto(p, action_22, j),
        122 => happy_goto(p, action_59, j),
        127 => happy_goto(p, action_24, j),
        128 => happy_goto(p, action_25, j),
        131 => happy_goto(p, action_60, j),
        _ => happy_fail(p, j)
    }
}

fn action_820(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_26, j),
        144 => happy_shift(p, action_27, j),
        145 => happy_shift(p, action_28, j),
        146 => happy_shift(p, action_29, j),
        147 => happy_shift(p, action_30, j),
        148 => happy_shift(p, action_31, j),
        149 => happy_shift(p, action_32, j),
        150 => happy_shift(p, action_33, j),
        153 => happy_shift(p, action_34, j),
        164 => happy_shift(p, action_35, j),
        184 => happy_shift(p, action_36, j),
        205 => happy_shift(p, action_37, j),
        221 => happy_shift(p, action_38, j),
        234 => happy_shift(p, action_39, j),
        235 => happy_shift(p, action_40, j),
        236 => happy_shift(p, action_41, j),
        237 => happy_shift(p, action_42, j),
        238 => happy_shift(p, action_43, j),
        241 => happy_shift(p, action_44, j),
        242 => happy_shift(p, action_45, j),
        243 => happy_shift(p, action_46, j),
        244 => happy_shift(p, action_47, j),
        245 => happy_shift(p, action_48, j),
        246 => happy_shift(p, action_49, j),
        247 => happy_shift(p, action_50, j),
        100 => happy_goto(p, action_6, j),
        104 => happy_goto(p, action_7, j),
        106 => happy_goto(p, action_8, j),
        107 => happy_goto(p, action_9, j),
        108 => happy_goto(p, action_10, j),
        109 => happy_goto(p, action_11, j),
        110 => happy_goto(p, action_12, j),
        111 => happy_goto(p, action_13, j),
        112 => happy_goto(p, action_14, j),
        113 => happy_goto(p, action_15, j),
        114 => happy_goto(p, action_16, j),
        115 => happy_goto(p, action_17, j),
        116 => happy_goto(p, action_18, j),
        117 => happy_goto(p, action_19, j),
        118 => happy_goto(p, action_20, j),
        119 => happy_goto(p, action_21, j),
        120 => happy_goto(p, action_22, j),
        122 => happy_goto(p, action_251, j),
        124 => happy_goto(p, action_883, j),
        127 => happy_goto(p, action_24, j),
        128 => happy_goto(p, action_25, j),
        _ => happy_reduce_458(p, j)
    }
}

fn action_821(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_26, j),
        144 => happy_shift(p, action_27, j),
        145 => happy_shift(p, action_28, j),
        146 => happy_shift(p, action_29, j),
        147 => happy_shift(p, action_30, j),
        148 => happy_shift(p, action_31, j),
        149 => happy_shift(p, action_32, j),
        150 => happy_shift(p, action_33, j),
        153 => happy_shift(p, action_34, j),
        164 => happy_shift(p, action_35, j),
        184 => happy_shift(p, action_36, j),
        205 => happy_shift(p, action_37, j),
        221 => happy_shift(p, action_38, j),
        234 => happy_shift(p, action_39, j),
        235 => happy_shift(p, action_40, j),
        236 => happy_shift(p, action_41, j),
        237 => happy_shift(p, action_42, j),
        238 => happy_shift(p, action_43, j),
        241 => happy_shift(p, action_44, j),
        242 => happy_shift(p, action_45, j),
        243 => happy_shift(p, action_46, j),
        244 => happy_shift(p, action_47, j),
        245 => happy_shift(p, action_48, j),
        246 => happy_shift(p, action_49, j),
        247 => happy_shift(p, action_50, j),
        100 => happy_goto(p, action_6, j),
        104 => happy_goto(p, action_7, j),
        106 => happy_goto(p, action_8, j),
        107 => happy_goto(p, action_9, j),
        108 => happy_goto(p, action_10, j),
        109 => happy_goto(p, action_11, j),
        110 => happy_goto(p, action_12, j),
        111 => happy_goto(p, action_13, j),
        112 => happy_goto(p, action_14, j),
        113 => happy_goto(p, action_15, j),
        114 => happy_goto(p, action_16, j),
        115 => happy_goto(p, action_17, j),
        116 => happy_goto(p, action_18, j),
        117 => happy_goto(p, action_19, j),
        118 => happy_goto(p, action_20, j),
        119 => happy_goto(p, action_21, j),
        120 => happy_goto(p, action_22, j),
        122 => happy_goto(p, action_251, j),
        124 => happy_goto(p, action_882, j),
        127 => happy_goto(p, action_24, j),
        128 => happy_goto(p, action_25, j),
        _ => happy_reduce_458(p, j)
    }
}

fn action_822(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        180 => happy_shift(p, action_881, j),
        _ => happy_fail(p, j)
    }
}

fn action_823(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_37(p, j)
    }
}

fn action_824(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        139 => happy_shift(p, action_879, j),
        167 => happy_shift(p, action_880, j),
        _ => happy_fail(p, j)
    }
}

fn action_825(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        179 => happy_shift(p, action_878, j),
        _ => happy_reduce_77(p, j)
    }
}

fn action_826(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_78(p, j)
    }
}

fn action_827(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_877, j),
        _ => happy_fail(p, j)
    }
}

fn action_828(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        238 => happy_shift(p, action_875, j),
        239 => happy_shift(p, action_876, j),
        _ => happy_fail(p, j)
    }
}

fn action_829(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_70(p, j)
    }
}

fn action_830(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_39(p, j)
    }
}

fn action_831(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_55(p, j)
    }
}

fn action_832(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_49(p, j)
    }
}

fn action_833(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_51(p, j)
    }
}

fn action_834(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_50(p, j)
    }
}

fn action_835(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        181 => happy_shift(p, action_62, j),
        14 => happy_goto(p, action_874, j),
        _ => happy_fail(p, j)
    }
}

fn action_836(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_52(p, j)
    }
}

fn action_837(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_370(p, j)
    }
}

fn action_838(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_369(p, j)
    }
}

fn action_839(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_368(p, j)
    }
}

fn action_840(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_26, j),
        144 => happy_shift(p, action_27, j),
        145 => happy_shift(p, action_28, j),
        146 => happy_shift(p, action_29, j),
        147 => happy_shift(p, action_30, j),
        148 => happy_shift(p, action_31, j),
        149 => happy_shift(p, action_32, j),
        150 => happy_shift(p, action_33, j),
        153 => happy_shift(p, action_34, j),
        164 => happy_shift(p, action_35, j),
        184 => happy_shift(p, action_36, j),
        205 => happy_shift(p, action_37, j),
        221 => happy_shift(p, action_38, j),
        234 => happy_shift(p, action_39, j),
        235 => happy_shift(p, action_40, j),
        236 => happy_shift(p, action_41, j),
        237 => happy_shift(p, action_42, j),
        238 => happy_shift(p, action_43, j),
        241 => happy_shift(p, action_44, j),
        242 => happy_shift(p, action_45, j),
        243 => happy_shift(p, action_46, j),
        244 => happy_shift(p, action_47, j),
        245 => happy_shift(p, action_48, j),
        246 => happy_shift(p, action_49, j),
        247 => happy_shift(p, action_50, j),
        100 => happy_goto(p, action_6, j),
        104 => happy_goto(p, action_7, j),
        106 => happy_goto(p, action_8, j),
        107 => happy_goto(p, action_9, j),
        108 => happy_goto(p, action_10, j),
        109 => happy_goto(p, action_11, j),
        110 => happy_goto(p, action_12, j),
        111 => happy_goto(p, action_13, j),
        112 => happy_goto(p, action_14, j),
        113 => happy_goto(p, action_15, j),
        114 => happy_goto(p, action_16, j),
        115 => happy_goto(p, action_17, j),
        116 => happy_goto(p, action_18, j),
        117 => happy_goto(p, action_19, j),
        118 => happy_goto(p, action_20, j),
        119 => happy_goto(p, action_21, j),
        120 => happy_goto(p, action_22, j),
        122 => happy_goto(p, action_873, j),
        127 => happy_goto(p, action_24, j),
        128 => happy_goto(p, action_25, j),
        _ => happy_fail(p, j)
    }
}

fn action_841(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        238 => happy_shift(p, action_256, j),
        239 => happy_shift(p, action_76, j),
        131 => happy_goto(p, action_872, j),
        _ => happy_fail(p, j)
    }
}

fn action_842(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_367(p, j)
    }
}

fn action_843(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_26, j),
        144 => happy_shift(p, action_27, j),
        145 => happy_shift(p, action_28, j),
        146 => happy_shift(p, action_29, j),
        147 => happy_shift(p, action_30, j),
        148 => happy_shift(p, action_31, j),
        149 => happy_shift(p, action_32, j),
        150 => happy_shift(p, action_33, j),
        153 => happy_shift(p, action_34, j),
        164 => happy_shift(p, action_35, j),
        184 => happy_shift(p, action_36, j),
        205 => happy_shift(p, action_37, j),
        221 => happy_shift(p, action_38, j),
        234 => happy_shift(p, action_39, j),
        235 => happy_shift(p, action_40, j),
        236 => happy_shift(p, action_41, j),
        237 => happy_shift(p, action_42, j),
        238 => happy_shift(p, action_43, j),
        241 => happy_shift(p, action_44, j),
        242 => happy_shift(p, action_45, j),
        243 => happy_shift(p, action_46, j),
        244 => happy_shift(p, action_47, j),
        245 => happy_shift(p, action_48, j),
        246 => happy_shift(p, action_49, j),
        247 => happy_shift(p, action_50, j),
        100 => happy_goto(p, action_6, j),
        104 => happy_goto(p, action_7, j),
        106 => happy_goto(p, action_8, j),
        107 => happy_goto(p, action_9, j),
        108 => happy_goto(p, action_10, j),
        109 => happy_goto(p, action_11, j),
        110 => happy_goto(p, action_12, j),
        111 => happy_goto(p, action_13, j),
        112 => happy_goto(p, action_14, j),
        113 => happy_goto(p, action_15, j),
        114 => happy_goto(p, action_16, j),
        115 => happy_goto(p, action_17, j),
        116 => happy_goto(p, action_18, j),
        117 => happy_goto(p, action_19, j),
        118 => happy_goto(p, action_20, j),
        119 => happy_goto(p, action_21, j),
        120 => happy_goto(p, action_871, j),
        127 => happy_goto(p, action_24, j),
        128 => happy_goto(p, action_25, j),
        _ => happy_fail(p, j)
    }
}

fn action_844(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_365(p, j)
    }
}

fn action_845(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        190 => happy_shift(p, action_117, j),
        192 => happy_shift(p, action_118, j),
        195 => happy_shift(p, action_119, j),
        196 => happy_shift(p, action_673, j),
        198 => happy_shift(p, action_120, j),
        200 => happy_shift(p, action_121, j),
        202 => happy_shift(p, action_123, j),
        203 => happy_shift(p, action_124, j),
        209 => happy_shift(p, action_126, j),
        210 => happy_shift(p, action_127, j),
        211 => happy_shift(p, action_128, j),
        219 => happy_shift(p, action_131, j),
        220 => happy_shift(p, action_132, j),
        224 => happy_shift(p, action_135, j),
        227 => happy_shift(p, action_137, j),
        229 => happy_shift(p, action_139, j),
        230 => happy_shift(p, action_140, j),
        231 => happy_shift(p, action_141, j),
        239 => happy_shift(p, action_143, j),
        240 => happy_shift(p, action_144, j),
        44 => happy_goto(p, action_289, j),
        45 => happy_goto(p, action_90, j),
        47 => happy_goto(p, action_290, j),
        49 => happy_goto(p, action_291, j),
        51 => happy_goto(p, action_292, j),
        52 => happy_goto(p, action_97, j),
        53 => happy_goto(p, action_98, j),
        54 => happy_goto(p, action_99, j),
        61 => happy_goto(p, action_100, j),
        65 => happy_goto(p, action_293, j),
        86 => happy_goto(p, action_670, j),
        102 => happy_goto(p, action_870, j),
        132 => happy_goto(p, action_109, j),
        133 => happy_goto(p, action_296, j),
        134 => happy_goto(p, action_111, j),
        _ => happy_reduce_473(p, j)
    }
}

fn action_846(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_26, j),
        144 => happy_shift(p, action_27, j),
        145 => happy_shift(p, action_28, j),
        146 => happy_shift(p, action_29, j),
        147 => happy_shift(p, action_30, j),
        148 => happy_shift(p, action_31, j),
        149 => happy_shift(p, action_32, j),
        150 => happy_shift(p, action_33, j),
        153 => happy_shift(p, action_34, j),
        164 => happy_shift(p, action_35, j),
        184 => happy_shift(p, action_36, j),
        205 => happy_shift(p, action_37, j),
        221 => happy_shift(p, action_38, j),
        234 => happy_shift(p, action_39, j),
        235 => happy_shift(p, action_40, j),
        236 => happy_shift(p, action_41, j),
        237 => happy_shift(p, action_42, j),
        238 => happy_shift(p, action_43, j),
        241 => happy_shift(p, action_44, j),
        242 => happy_shift(p, action_45, j),
        243 => happy_shift(p, action_46, j),
        244 => happy_shift(p, action_47, j),
        245 => happy_shift(p, action_48, j),
        246 => happy_shift(p, action_49, j),
        247 => happy_shift(p, action_50, j),
        100 => happy_goto(p, action_6, j),
        104 => happy_goto(p, action_7, j),
        106 => happy_goto(p, action_8, j),
        107 => happy_goto(p, action_9, j),
        108 => happy_goto(p, action_10, j),
        109 => happy_goto(p, action_11, j),
        110 => happy_goto(p, action_12, j),
        111 => happy_goto(p, action_13, j),
        112 => happy_goto(p, action_14, j),
        113 => happy_goto(p, action_15, j),
        114 => happy_goto(p, action_16, j),
        115 => happy_goto(p, action_17, j),
        116 => happy_goto(p, action_18, j),
        117 => happy_goto(p, action_19, j),
        118 => happy_goto(p, action_20, j),
        119 => happy_goto(p, action_21, j),
        120 => happy_goto(p, action_869, j),
        127 => happy_goto(p, action_24, j),
        128 => happy_goto(p, action_25, j),
        _ => happy_fail(p, j)
    }
}

fn action_847(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_336(p, j)
    }
}

fn action_848(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_338(p, j)
    }
}

fn action_849(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_417, j),
        140 => happy_shift(p, action_184, j),
        88 => happy_goto(p, action_868, j),
        89 => happy_goto(p, action_181, j),
        90 => happy_goto(p, action_182, j),
        _ => happy_reduce_337(p, j)
    }
}

fn action_850(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_339(p, j)
    }
}

fn action_851(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        179 => happy_shift(p, action_866, j),
        182 => happy_shift(p, action_867, j),
        _ => happy_fail(p, j)
    }
}

fn action_852(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_358(p, j)
    }
}

fn action_853(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        141 => happy_shift(p, action_864, j),
        183 => happy_shift(p, action_865, j),
        _ => happy_fail(p, j)
    }
}

fn action_854(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_353(p, j)
    }
}

fn action_855(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_356(p, j)
    }
}

fn action_856(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_359(p, j)
    }
}

fn action_857(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_352(p, j)
    }
}

fn action_858(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_349(p, j)
    }
}

fn action_859(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_26, j),
        140 => happy_shift(p, action_657, j),
        143 => happy_shift(p, action_658, j),
        144 => happy_shift(p, action_27, j),
        145 => happy_shift(p, action_28, j),
        146 => happy_shift(p, action_29, j),
        147 => happy_shift(p, action_30, j),
        148 => happy_shift(p, action_31, j),
        149 => happy_shift(p, action_32, j),
        150 => happy_shift(p, action_33, j),
        153 => happy_shift(p, action_34, j),
        164 => happy_shift(p, action_35, j),
        181 => happy_shift(p, action_659, j),
        182 => happy_shift(p, action_863, j),
        184 => happy_shift(p, action_36, j),
        205 => happy_shift(p, action_37, j),
        221 => happy_shift(p, action_38, j),
        234 => happy_shift(p, action_39, j),
        235 => happy_shift(p, action_40, j),
        236 => happy_shift(p, action_41, j),
        237 => happy_shift(p, action_42, j),
        238 => happy_shift(p, action_75, j),
        239 => happy_shift(p, action_76, j),
        241 => happy_shift(p, action_44, j),
        242 => happy_shift(p, action_45, j),
        243 => happy_shift(p, action_46, j),
        244 => happy_shift(p, action_47, j),
        245 => happy_shift(p, action_48, j),
        246 => happy_shift(p, action_49, j),
        247 => happy_shift(p, action_50, j),
        93 => happy_goto(p, action_861, j),
        96 => happy_goto(p, action_862, j),
        97 => happy_goto(p, action_652, j),
        98 => happy_goto(p, action_653, j),
        99 => happy_goto(p, action_654, j),
        100 => happy_goto(p, action_6, j),
        104 => happy_goto(p, action_7, j),
        106 => happy_goto(p, action_8, j),
        107 => happy_goto(p, action_9, j),
        108 => happy_goto(p, action_10, j),
        109 => happy_goto(p, action_11, j),
        110 => happy_goto(p, action_12, j),
        111 => happy_goto(p, action_13, j),
        112 => happy_goto(p, action_14, j),
        113 => happy_goto(p, action_15, j),
        114 => happy_goto(p, action_16, j),
        115 => happy_goto(p, action_17, j),
        116 => happy_goto(p, action_18, j),
        117 => happy_goto(p, action_19, j),
        118 => happy_goto(p, action_20, j),
        119 => happy_goto(p, action_21, j),
        120 => happy_goto(p, action_655, j),
        127 => happy_goto(p, action_24, j),
        128 => happy_goto(p, action_25, j),
        131 => happy_goto(p, action_656, j),
        _ => happy_fail(p, j)
    }
}

fn action_860(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_386(p, j)
    }
}

fn action_861(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_350(p, j)
    }
}

fn action_862(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_26, j),
        144 => happy_shift(p, action_27, j),
        145 => happy_shift(p, action_28, j),
        146 => happy_shift(p, action_29, j),
        147 => happy_shift(p, action_30, j),
        148 => happy_shift(p, action_31, j),
        149 => happy_shift(p, action_32, j),
        150 => happy_shift(p, action_33, j),
        153 => happy_shift(p, action_34, j),
        164 => happy_shift(p, action_35, j),
        181 => happy_shift(p, action_659, j),
        184 => happy_shift(p, action_36, j),
        205 => happy_shift(p, action_37, j),
        221 => happy_shift(p, action_38, j),
        234 => happy_shift(p, action_39, j),
        235 => happy_shift(p, action_40, j),
        236 => happy_shift(p, action_41, j),
        237 => happy_shift(p, action_42, j),
        238 => happy_shift(p, action_43, j),
        241 => happy_shift(p, action_44, j),
        242 => happy_shift(p, action_45, j),
        243 => happy_shift(p, action_46, j),
        244 => happy_shift(p, action_47, j),
        245 => happy_shift(p, action_48, j),
        246 => happy_shift(p, action_49, j),
        247 => happy_shift(p, action_50, j),
        93 => happy_goto(p, action_925, j),
        100 => happy_goto(p, action_6, j),
        104 => happy_goto(p, action_7, j),
        106 => happy_goto(p, action_8, j),
        107 => happy_goto(p, action_9, j),
        108 => happy_goto(p, action_10, j),
        109 => happy_goto(p, action_11, j),
        110 => happy_goto(p, action_12, j),
        111 => happy_goto(p, action_13, j),
        112 => happy_goto(p, action_14, j),
        113 => happy_goto(p, action_15, j),
        114 => happy_goto(p, action_16, j),
        115 => happy_goto(p, action_17, j),
        116 => happy_goto(p, action_18, j),
        117 => happy_goto(p, action_19, j),
        118 => happy_goto(p, action_20, j),
        119 => happy_goto(p, action_21, j),
        120 => happy_goto(p, action_655, j),
        127 => happy_goto(p, action_24, j),
        128 => happy_goto(p, action_25, j),
        _ => happy_fail(p, j)
    }
}

fn action_863(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_387(p, j)
    }
}

fn action_864(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_357(p, j)
    }
}

fn action_865(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_26, j),
        144 => happy_shift(p, action_27, j),
        145 => happy_shift(p, action_28, j),
        146 => happy_shift(p, action_29, j),
        147 => happy_shift(p, action_30, j),
        148 => happy_shift(p, action_31, j),
        149 => happy_shift(p, action_32, j),
        150 => happy_shift(p, action_33, j),
        153 => happy_shift(p, action_34, j),
        164 => happy_shift(p, action_35, j),
        184 => happy_shift(p, action_36, j),
        205 => happy_shift(p, action_37, j),
        221 => happy_shift(p, action_38, j),
        234 => happy_shift(p, action_39, j),
        235 => happy_shift(p, action_40, j),
        236 => happy_shift(p, action_41, j),
        237 => happy_shift(p, action_42, j),
        238 => happy_shift(p, action_43, j),
        241 => happy_shift(p, action_44, j),
        242 => happy_shift(p, action_45, j),
        243 => happy_shift(p, action_46, j),
        244 => happy_shift(p, action_47, j),
        245 => happy_shift(p, action_48, j),
        246 => happy_shift(p, action_49, j),
        247 => happy_shift(p, action_50, j),
        100 => happy_goto(p, action_6, j),
        104 => happy_goto(p, action_7, j),
        106 => happy_goto(p, action_261, j),
        107 => happy_goto(p, action_9, j),
        108 => happy_goto(p, action_10, j),
        109 => happy_goto(p, action_11, j),
        110 => happy_goto(p, action_12, j),
        111 => happy_goto(p, action_13, j),
        112 => happy_goto(p, action_14, j),
        113 => happy_goto(p, action_15, j),
        114 => happy_goto(p, action_16, j),
        115 => happy_goto(p, action_17, j),
        116 => happy_goto(p, action_18, j),
        117 => happy_goto(p, action_19, j),
        118 => happy_goto(p, action_20, j),
        119 => happy_goto(p, action_262, j),
        126 => happy_goto(p, action_924, j),
        127 => happy_goto(p, action_24, j),
        128 => happy_goto(p, action_25, j),
        _ => happy_fail(p, j)
    }
}

fn action_866(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_26, j),
        140 => happy_shift(p, action_657, j),
        143 => happy_shift(p, action_658, j),
        144 => happy_shift(p, action_27, j),
        145 => happy_shift(p, action_28, j),
        146 => happy_shift(p, action_29, j),
        147 => happy_shift(p, action_30, j),
        148 => happy_shift(p, action_31, j),
        149 => happy_shift(p, action_32, j),
        150 => happy_shift(p, action_33, j),
        153 => happy_shift(p, action_34, j),
        164 => happy_shift(p, action_35, j),
        181 => happy_shift(p, action_659, j),
        182 => happy_shift(p, action_923, j),
        184 => happy_shift(p, action_36, j),
        205 => happy_shift(p, action_37, j),
        221 => happy_shift(p, action_38, j),
        234 => happy_shift(p, action_39, j),
        235 => happy_shift(p, action_40, j),
        236 => happy_shift(p, action_41, j),
        237 => happy_shift(p, action_42, j),
        238 => happy_shift(p, action_75, j),
        239 => happy_shift(p, action_76, j),
        241 => happy_shift(p, action_44, j),
        242 => happy_shift(p, action_45, j),
        243 => happy_shift(p, action_46, j),
        244 => happy_shift(p, action_47, j),
        245 => happy_shift(p, action_48, j),
        246 => happy_shift(p, action_49, j),
        247 => happy_shift(p, action_50, j),
        93 => happy_goto(p, action_861, j),
        96 => happy_goto(p, action_862, j),
        97 => happy_goto(p, action_652, j),
        98 => happy_goto(p, action_653, j),
        99 => happy_goto(p, action_654, j),
        100 => happy_goto(p, action_6, j),
        104 => happy_goto(p, action_7, j),
        106 => happy_goto(p, action_8, j),
        107 => happy_goto(p, action_9, j),
        108 => happy_goto(p, action_10, j),
        109 => happy_goto(p, action_11, j),
        110 => happy_goto(p, action_12, j),
        111 => happy_goto(p, action_13, j),
        112 => happy_goto(p, action_14, j),
        113 => happy_goto(p, action_15, j),
        114 => happy_goto(p, action_16, j),
        115 => happy_goto(p, action_17, j),
        116 => happy_goto(p, action_18, j),
        117 => happy_goto(p, action_19, j),
        118 => happy_goto(p, action_20, j),
        119 => happy_goto(p, action_21, j),
        120 => happy_goto(p, action_655, j),
        127 => happy_goto(p, action_24, j),
        128 => happy_goto(p, action_25, j),
        131 => happy_goto(p, action_656, j),
        _ => happy_fail(p, j)
    }
}

fn action_867(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_343(p, j)
    }
}

fn action_868(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_340(p, j)
    }
}

fn action_869(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_373(p, j)
    }
}

fn action_870(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_371(p, j)
    }
}

fn action_871(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_374(p, j)
    }
}

fn action_872(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_376(p, j)
    }
}

fn action_873(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        141 => happy_shift(p, action_922, j),
        _ => happy_fail(p, j)
    }
}

fn action_874(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_53(p, j)
    }
}

fn action_875(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        141 => happy_shift(p, action_921, j),
        _ => happy_fail(p, j)
    }
}

fn action_876(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        141 => happy_shift(p, action_920, j),
        _ => happy_fail(p, j)
    }
}

fn action_877(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_26, j),
        144 => happy_shift(p, action_27, j),
        145 => happy_shift(p, action_28, j),
        146 => happy_shift(p, action_29, j),
        147 => happy_shift(p, action_30, j),
        148 => happy_shift(p, action_31, j),
        149 => happy_shift(p, action_32, j),
        150 => happy_shift(p, action_33, j),
        153 => happy_shift(p, action_34, j),
        164 => happy_shift(p, action_35, j),
        184 => happy_shift(p, action_36, j),
        205 => happy_shift(p, action_37, j),
        221 => happy_shift(p, action_38, j),
        234 => happy_shift(p, action_39, j),
        235 => happy_shift(p, action_40, j),
        236 => happy_shift(p, action_41, j),
        237 => happy_shift(p, action_42, j),
        238 => happy_shift(p, action_43, j),
        241 => happy_shift(p, action_44, j),
        242 => happy_shift(p, action_45, j),
        243 => happy_shift(p, action_46, j),
        244 => happy_shift(p, action_47, j),
        245 => happy_shift(p, action_48, j),
        246 => happy_shift(p, action_49, j),
        247 => happy_shift(p, action_50, j),
        100 => happy_goto(p, action_6, j),
        104 => happy_goto(p, action_7, j),
        106 => happy_goto(p, action_8, j),
        107 => happy_goto(p, action_9, j),
        108 => happy_goto(p, action_10, j),
        109 => happy_goto(p, action_11, j),
        110 => happy_goto(p, action_12, j),
        111 => happy_goto(p, action_13, j),
        112 => happy_goto(p, action_14, j),
        113 => happy_goto(p, action_15, j),
        114 => happy_goto(p, action_16, j),
        115 => happy_goto(p, action_17, j),
        116 => happy_goto(p, action_18, j),
        117 => happy_goto(p, action_19, j),
        118 => happy_goto(p, action_20, j),
        119 => happy_goto(p, action_21, j),
        120 => happy_goto(p, action_22, j),
        122 => happy_goto(p, action_919, j),
        127 => happy_goto(p, action_24, j),
        128 => happy_goto(p, action_25, j),
        _ => happy_fail(p, j)
    }
}

fn action_878(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        140 => happy_shift(p, action_828, j),
        237 => happy_shift(p, action_42, j),
        30 => happy_goto(p, action_918, j),
        128 => happy_goto(p, action_827, j),
        _ => happy_fail(p, j)
    }
}

fn action_879(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        180 => happy_shift(p, action_917, j),
        _ => happy_fail(p, j)
    }
}

fn action_880(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        140 => happy_shift(p, action_828, j),
        237 => happy_shift(p, action_42, j),
        28 => happy_goto(p, action_916, j),
        29 => happy_goto(p, action_825, j),
        30 => happy_goto(p, action_826, j),
        128 => happy_goto(p, action_827, j),
        _ => happy_reduce_76(p, j)
    }
}

fn action_881(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_62(p, j)
    }
}

fn action_882(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        139 => happy_shift(p, action_915, j),
        _ => happy_fail(p, j)
    }
}

fn action_883(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        139 => happy_shift(p, action_914, j),
        _ => happy_fail(p, j)
    }
}

fn action_884(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_59(p, j)
    }
}

fn action_885(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_252(p, j)
    }
}

fn action_886(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_205(p, j)
    }
}

fn action_887(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_26, j),
        144 => happy_shift(p, action_27, j),
        145 => happy_shift(p, action_28, j),
        146 => happy_shift(p, action_29, j),
        147 => happy_shift(p, action_30, j),
        148 => happy_shift(p, action_31, j),
        149 => happy_shift(p, action_32, j),
        150 => happy_shift(p, action_33, j),
        153 => happy_shift(p, action_34, j),
        164 => happy_shift(p, action_35, j),
        184 => happy_shift(p, action_36, j),
        205 => happy_shift(p, action_37, j),
        221 => happy_shift(p, action_38, j),
        234 => happy_shift(p, action_39, j),
        235 => happy_shift(p, action_40, j),
        236 => happy_shift(p, action_41, j),
        237 => happy_shift(p, action_42, j),
        238 => happy_shift(p, action_43, j),
        241 => happy_shift(p, action_44, j),
        242 => happy_shift(p, action_45, j),
        243 => happy_shift(p, action_46, j),
        244 => happy_shift(p, action_47, j),
        245 => happy_shift(p, action_48, j),
        246 => happy_shift(p, action_49, j),
        247 => happy_shift(p, action_50, j),
        100 => happy_goto(p, action_6, j),
        104 => happy_goto(p, action_7, j),
        106 => happy_goto(p, action_261, j),
        107 => happy_goto(p, action_9, j),
        108 => happy_goto(p, action_10, j),
        109 => happy_goto(p, action_11, j),
        110 => happy_goto(p, action_12, j),
        111 => happy_goto(p, action_13, j),
        112 => happy_goto(p, action_14, j),
        113 => happy_goto(p, action_15, j),
        114 => happy_goto(p, action_16, j),
        115 => happy_goto(p, action_17, j),
        116 => happy_goto(p, action_18, j),
        117 => happy_goto(p, action_19, j),
        118 => happy_goto(p, action_20, j),
        119 => happy_goto(p, action_262, j),
        126 => happy_goto(p, action_913, j),
        127 => happy_goto(p, action_24, j),
        128 => happy_goto(p, action_25, j),
        _ => happy_fail(p, j)
    }
}

fn action_888(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_201(p, j)
    }
}

fn action_889(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_479, j),
        150 => happy_shift(p, action_480, j),
        167 => happy_shift(p, action_801, j),
        238 => happy_shift(p, action_142, j),
        60 => happy_goto(p, action_912, j),
        75 => happy_goto(p, action_800, j),
        76 => happy_goto(p, action_103, j),
        77 => happy_goto(p, action_104, j),
        78 => happy_goto(p, action_485, j),
        _ => happy_fail(p, j)
    }
}

fn action_890(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_528, j),
        150 => happy_shift(p, action_529, j),
        167 => happy_shift(p, action_810, j),
        238 => happy_shift(p, action_142, j),
        239 => happy_shift(p, action_231, j),
        59 => happy_goto(p, action_911, j),
        66 => happy_goto(p, action_809, j),
        68 => happy_goto(p, action_221, j),
        69 => happy_goto(p, action_222, j),
        70 => happy_goto(p, action_223, j),
        71 => happy_goto(p, action_224, j),
        72 => happy_goto(p, action_225, j),
        73 => happy_goto(p, action_226, j),
        75 => happy_goto(p, action_527, j),
        76 => happy_goto(p, action_103, j),
        77 => happy_goto(p, action_104, j),
        78 => happy_goto(p, action_485, j),
        _ => happy_fail(p, j)
    }
}

fn action_891(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        240 => happy_shift(p, action_144, j),
        134 => happy_goto(p, action_894, j),
        _ => happy_reduce_198(p, j)
    }
}

fn action_892(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_208(p, j)
    }
}

fn action_893(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_26, j),
        144 => happy_shift(p, action_27, j),
        145 => happy_shift(p, action_28, j),
        146 => happy_shift(p, action_29, j),
        147 => happy_shift(p, action_30, j),
        148 => happy_shift(p, action_31, j),
        149 => happy_shift(p, action_32, j),
        150 => happy_shift(p, action_33, j),
        153 => happy_shift(p, action_34, j),
        164 => happy_shift(p, action_35, j),
        184 => happy_shift(p, action_36, j),
        205 => happy_shift(p, action_37, j),
        221 => happy_shift(p, action_38, j),
        234 => happy_shift(p, action_39, j),
        235 => happy_shift(p, action_40, j),
        236 => happy_shift(p, action_41, j),
        237 => happy_shift(p, action_42, j),
        238 => happy_shift(p, action_43, j),
        241 => happy_shift(p, action_44, j),
        242 => happy_shift(p, action_45, j),
        243 => happy_shift(p, action_46, j),
        244 => happy_shift(p, action_47, j),
        245 => happy_shift(p, action_48, j),
        246 => happy_shift(p, action_49, j),
        247 => happy_shift(p, action_50, j),
        100 => happy_goto(p, action_6, j),
        104 => happy_goto(p, action_7, j),
        106 => happy_goto(p, action_261, j),
        107 => happy_goto(p, action_9, j),
        108 => happy_goto(p, action_10, j),
        109 => happy_goto(p, action_11, j),
        110 => happy_goto(p, action_12, j),
        111 => happy_goto(p, action_13, j),
        112 => happy_goto(p, action_14, j),
        113 => happy_goto(p, action_15, j),
        114 => happy_goto(p, action_16, j),
        115 => happy_goto(p, action_17, j),
        116 => happy_goto(p, action_18, j),
        117 => happy_goto(p, action_19, j),
        118 => happy_goto(p, action_20, j),
        119 => happy_goto(p, action_262, j),
        126 => happy_goto(p, action_910, j),
        127 => happy_goto(p, action_24, j),
        128 => happy_goto(p, action_25, j),
        _ => happy_fail(p, j)
    }
}

fn action_894(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_210(p, j)
    }
}

fn action_895(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_326(p, j)
    }
}

fn action_896(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        141 => happy_shift(p, action_909, j),
        _ => happy_fail(p, j)
    }
}

fn action_897(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_321(p, j)
    }
}

fn action_898(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_214(p, j)
    }
}

fn action_899(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_220(p, j)
    }
}

fn action_900(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_89(p, j)
    }
}

fn action_901(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_483(p, j)
    }
}

fn action_902(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_26, j),
        144 => happy_shift(p, action_27, j),
        145 => happy_shift(p, action_28, j),
        146 => happy_shift(p, action_29, j),
        147 => happy_shift(p, action_30, j),
        148 => happy_shift(p, action_31, j),
        149 => happy_shift(p, action_32, j),
        150 => happy_shift(p, action_33, j),
        153 => happy_shift(p, action_34, j),
        164 => happy_shift(p, action_35, j),
        184 => happy_shift(p, action_36, j),
        205 => happy_shift(p, action_37, j),
        221 => happy_shift(p, action_38, j),
        234 => happy_shift(p, action_39, j),
        235 => happy_shift(p, action_40, j),
        236 => happy_shift(p, action_41, j),
        237 => happy_shift(p, action_42, j),
        238 => happy_shift(p, action_43, j),
        241 => happy_shift(p, action_44, j),
        242 => happy_shift(p, action_45, j),
        243 => happy_shift(p, action_46, j),
        244 => happy_shift(p, action_47, j),
        245 => happy_shift(p, action_48, j),
        246 => happy_shift(p, action_49, j),
        247 => happy_shift(p, action_50, j),
        100 => happy_goto(p, action_6, j),
        104 => happy_goto(p, action_7, j),
        106 => happy_goto(p, action_907, j),
        107 => happy_goto(p, action_9, j),
        108 => happy_goto(p, action_10, j),
        109 => happy_goto(p, action_11, j),
        110 => happy_goto(p, action_12, j),
        111 => happy_goto(p, action_13, j),
        112 => happy_goto(p, action_14, j),
        113 => happy_goto(p, action_15, j),
        114 => happy_goto(p, action_16, j),
        115 => happy_goto(p, action_17, j),
        116 => happy_goto(p, action_18, j),
        117 => happy_goto(p, action_19, j),
        118 => happy_goto(p, action_20, j),
        119 => happy_goto(p, action_262, j),
        126 => happy_goto(p, action_908, j),
        127 => happy_goto(p, action_24, j),
        128 => happy_goto(p, action_25, j),
        _ => happy_fail(p, j)
    }
}

fn action_903(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_275, j),
        144 => happy_shift(p, action_27, j),
        145 => happy_shift(p, action_28, j),
        146 => happy_shift(p, action_29, j),
        147 => happy_shift(p, action_30, j),
        148 => happy_shift(p, action_31, j),
        149 => happy_shift(p, action_32, j),
        150 => happy_shift(p, action_33, j),
        153 => happy_shift(p, action_34, j),
        164 => happy_shift(p, action_35, j),
        184 => happy_shift(p, action_36, j),
        205 => happy_shift(p, action_37, j),
        221 => happy_shift(p, action_38, j),
        234 => happy_shift(p, action_39, j),
        235 => happy_shift(p, action_40, j),
        236 => happy_shift(p, action_41, j),
        237 => happy_shift(p, action_42, j),
        238 => happy_shift(p, action_43, j),
        241 => happy_shift(p, action_44, j),
        242 => happy_shift(p, action_45, j),
        243 => happy_shift(p, action_46, j),
        244 => happy_shift(p, action_47, j),
        245 => happy_shift(p, action_48, j),
        246 => happy_shift(p, action_49, j),
        247 => happy_shift(p, action_50, j),
        248 => happy_shift(p, action_906, j),
        100 => happy_goto(p, action_6, j),
        104 => happy_goto(p, action_7, j),
        106 => happy_goto(p, action_904, j),
        107 => happy_goto(p, action_9, j),
        127 => happy_goto(p, action_24, j),
        128 => happy_goto(p, action_25, j),
        130 => happy_goto(p, action_905, j),
        _ => happy_fail(p, j)
    }
}

fn action_904(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_487(p, j)
    }
}

fn action_905(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_486(p, j)
    }
}

fn action_906(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_470(p, j)
    }
}

fn action_907(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        168 => happy_shift(p, action_319, j),
        169 => happy_shift(p, action_320, j),
        170 => happy_shift(p, action_321, j),
        171 => happy_shift(p, action_322, j),
        172 => happy_shift(p, action_323, j),
        173 => happy_shift(p, action_324, j),
        174 => happy_shift(p, action_325, j),
        175 => happy_shift(p, action_326, j),
        176 => happy_shift(p, action_327, j),
        177 => happy_shift(p, action_328, j),
        178 => happy_shift(p, action_329, j),
        121 => happy_goto(p, action_935, j),
        _ => happy_reduce_408(p, j)
    }
}

fn action_908(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_488(p, j)
    }
}

fn action_909(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_322(p, j)
    }
}

fn action_910(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_209(p, j)
    }
}

fn action_911(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        240 => happy_shift(p, action_144, j),
        132 => happy_goto(p, action_934, j),
        133 => happy_goto(p, action_152, j),
        134 => happy_goto(p, action_111, j),
        _ => happy_reduce_473(p, j)
    }
}

fn action_912(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        240 => happy_shift(p, action_144, j),
        134 => happy_goto(p, action_894, j),
        _ => happy_reduce_200(p, j)
    }
}

fn action_913(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_206(p, j)
    }
}

fn action_914(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_26, j),
        144 => happy_shift(p, action_27, j),
        145 => happy_shift(p, action_28, j),
        146 => happy_shift(p, action_29, j),
        147 => happy_shift(p, action_30, j),
        148 => happy_shift(p, action_31, j),
        149 => happy_shift(p, action_32, j),
        150 => happy_shift(p, action_33, j),
        153 => happy_shift(p, action_34, j),
        164 => happy_shift(p, action_35, j),
        180 => happy_shift(p, action_61, j),
        181 => happy_shift(p, action_62, j),
        184 => happy_shift(p, action_36, j),
        187 => happy_shift(p, action_63, j),
        189 => happy_shift(p, action_64, j),
        191 => happy_shift(p, action_65, j),
        194 => happy_shift(p, action_66, j),
        196 => happy_shift(p, action_67, j),
        197 => happy_shift(p, action_68, j),
        204 => happy_shift(p, action_69, j),
        205 => happy_shift(p, action_37, j),
        206 => happy_shift(p, action_70, j),
        207 => happy_shift(p, action_71, j),
        218 => happy_shift(p, action_72, j),
        221 => happy_shift(p, action_38, j),
        225 => happy_shift(p, action_73, j),
        233 => happy_shift(p, action_74, j),
        234 => happy_shift(p, action_39, j),
        235 => happy_shift(p, action_40, j),
        236 => happy_shift(p, action_41, j),
        237 => happy_shift(p, action_42, j),
        238 => happy_shift(p, action_75, j),
        239 => happy_shift(p, action_76, j),
        241 => happy_shift(p, action_44, j),
        242 => happy_shift(p, action_45, j),
        243 => happy_shift(p, action_46, j),
        244 => happy_shift(p, action_47, j),
        245 => happy_shift(p, action_48, j),
        246 => happy_shift(p, action_49, j),
        247 => happy_shift(p, action_50, j),
        12 => happy_goto(p, action_933, j),
        13 => happy_goto(p, action_52, j),
        14 => happy_goto(p, action_53, j),
        22 => happy_goto(p, action_54, j),
        23 => happy_goto(p, action_55, j),
        24 => happy_goto(p, action_56, j),
        25 => happy_goto(p, action_57, j),
        26 => happy_goto(p, action_58, j),
        100 => happy_goto(p, action_6, j),
        104 => happy_goto(p, action_7, j),
        106 => happy_goto(p, action_8, j),
        107 => happy_goto(p, action_9, j),
        108 => happy_goto(p, action_10, j),
        109 => happy_goto(p, action_11, j),
        110 => happy_goto(p, action_12, j),
        111 => happy_goto(p, action_13, j),
        112 => happy_goto(p, action_14, j),
        113 => happy_goto(p, action_15, j),
        114 => happy_goto(p, action_16, j),
        115 => happy_goto(p, action_17, j),
        116 => happy_goto(p, action_18, j),
        117 => happy_goto(p, action_19, j),
        118 => happy_goto(p, action_20, j),
        119 => happy_goto(p, action_21, j),
        120 => happy_goto(p, action_22, j),
        122 => happy_goto(p, action_59, j),
        127 => happy_goto(p, action_24, j),
        128 => happy_goto(p, action_25, j),
        131 => happy_goto(p, action_60, j),
        _ => happy_fail(p, j)
    }
}

fn action_915(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_26, j),
        144 => happy_shift(p, action_27, j),
        145 => happy_shift(p, action_28, j),
        146 => happy_shift(p, action_29, j),
        147 => happy_shift(p, action_30, j),
        148 => happy_shift(p, action_31, j),
        149 => happy_shift(p, action_32, j),
        150 => happy_shift(p, action_33, j),
        153 => happy_shift(p, action_34, j),
        164 => happy_shift(p, action_35, j),
        180 => happy_shift(p, action_61, j),
        181 => happy_shift(p, action_62, j),
        184 => happy_shift(p, action_36, j),
        187 => happy_shift(p, action_63, j),
        189 => happy_shift(p, action_64, j),
        191 => happy_shift(p, action_65, j),
        194 => happy_shift(p, action_66, j),
        196 => happy_shift(p, action_67, j),
        197 => happy_shift(p, action_68, j),
        204 => happy_shift(p, action_69, j),
        205 => happy_shift(p, action_37, j),
        206 => happy_shift(p, action_70, j),
        207 => happy_shift(p, action_71, j),
        218 => happy_shift(p, action_72, j),
        221 => happy_shift(p, action_38, j),
        225 => happy_shift(p, action_73, j),
        233 => happy_shift(p, action_74, j),
        234 => happy_shift(p, action_39, j),
        235 => happy_shift(p, action_40, j),
        236 => happy_shift(p, action_41, j),
        237 => happy_shift(p, action_42, j),
        238 => happy_shift(p, action_75, j),
        239 => happy_shift(p, action_76, j),
        241 => happy_shift(p, action_44, j),
        242 => happy_shift(p, action_45, j),
        243 => happy_shift(p, action_46, j),
        244 => happy_shift(p, action_47, j),
        245 => happy_shift(p, action_48, j),
        246 => happy_shift(p, action_49, j),
        247 => happy_shift(p, action_50, j),
        12 => happy_goto(p, action_932, j),
        13 => happy_goto(p, action_52, j),
        14 => happy_goto(p, action_53, j),
        22 => happy_goto(p, action_54, j),
        23 => happy_goto(p, action_55, j),
        24 => happy_goto(p, action_56, j),
        25 => happy_goto(p, action_57, j),
        26 => happy_goto(p, action_58, j),
        100 => happy_goto(p, action_6, j),
        104 => happy_goto(p, action_7, j),
        106 => happy_goto(p, action_8, j),
        107 => happy_goto(p, action_9, j),
        108 => happy_goto(p, action_10, j),
        109 => happy_goto(p, action_11, j),
        110 => happy_goto(p, action_12, j),
        111 => happy_goto(p, action_13, j),
        112 => happy_goto(p, action_14, j),
        113 => happy_goto(p, action_15, j),
        114 => happy_goto(p, action_16, j),
        115 => happy_goto(p, action_17, j),
        116 => happy_goto(p, action_18, j),
        117 => happy_goto(p, action_19, j),
        118 => happy_goto(p, action_20, j),
        119 => happy_goto(p, action_21, j),
        120 => happy_goto(p, action_22, j),
        122 => happy_goto(p, action_59, j),
        127 => happy_goto(p, action_24, j),
        128 => happy_goto(p, action_25, j),
        131 => happy_goto(p, action_60, j),
        _ => happy_fail(p, j)
    }
}

fn action_916(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        139 => happy_shift(p, action_930, j),
        167 => happy_shift(p, action_931, j),
        _ => happy_fail(p, j)
    }
}

fn action_917(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_71(p, j)
    }
}

fn action_918(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_79(p, j)
    }
}

fn action_919(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        139 => happy_shift(p, action_929, j),
        _ => happy_fail(p, j)
    }
}

fn action_920(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        237 => happy_shift(p, action_42, j),
        128 => happy_goto(p, action_928, j),
        _ => happy_fail(p, j)
    }
}

fn action_921(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        237 => happy_shift(p, action_42, j),
        128 => happy_goto(p, action_927, j),
        _ => happy_fail(p, j)
    }
}

fn action_922(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_377(p, j)
    }
}

fn action_923(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_344(p, j)
    }
}

fn action_924(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        141 => happy_shift(p, action_926, j),
        _ => happy_fail(p, j)
    }
}

fn action_925(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_351(p, j)
    }
}

fn action_926(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_360(p, j)
    }
}

fn action_927(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_943, j),
        _ => happy_fail(p, j)
    }
}

fn action_928(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_942, j),
        _ => happy_fail(p, j)
    }
}

fn action_929(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_80(p, j)
    }
}

fn action_930(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        180 => happy_shift(p, action_941, j),
        _ => happy_fail(p, j)
    }
}

fn action_931(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        237 => happy_shift(p, action_42, j),
        31 => happy_goto(p, action_939, j),
        128 => happy_goto(p, action_940, j),
        _ => happy_fail(p, j)
    }
}

fn action_932(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        16 => happy_goto(p, action_938, j),
        _ => happy_reduce_41(p, j)
    }
}

fn action_933(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_63(p, j)
    }
}

fn action_934(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_202(p, j)
    }
}

fn action_935(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_275, j),
        144 => happy_shift(p, action_27, j),
        145 => happy_shift(p, action_28, j),
        146 => happy_shift(p, action_29, j),
        147 => happy_shift(p, action_30, j),
        148 => happy_shift(p, action_31, j),
        149 => happy_shift(p, action_32, j),
        150 => happy_shift(p, action_33, j),
        153 => happy_shift(p, action_34, j),
        164 => happy_shift(p, action_35, j),
        184 => happy_shift(p, action_36, j),
        205 => happy_shift(p, action_37, j),
        221 => happy_shift(p, action_38, j),
        234 => happy_shift(p, action_39, j),
        235 => happy_shift(p, action_40, j),
        236 => happy_shift(p, action_41, j),
        237 => happy_shift(p, action_42, j),
        238 => happy_shift(p, action_43, j),
        241 => happy_shift(p, action_44, j),
        242 => happy_shift(p, action_45, j),
        243 => happy_shift(p, action_46, j),
        244 => happy_shift(p, action_47, j),
        245 => happy_shift(p, action_48, j),
        246 => happy_shift(p, action_49, j),
        247 => happy_shift(p, action_50, j),
        248 => happy_shift(p, action_906, j),
        100 => happy_goto(p, action_6, j),
        104 => happy_goto(p, action_7, j),
        106 => happy_goto(p, action_936, j),
        107 => happy_goto(p, action_9, j),
        127 => happy_goto(p, action_24, j),
        128 => happy_goto(p, action_25, j),
        130 => happy_goto(p, action_937, j),
        _ => happy_fail(p, j)
    }
}

fn action_936(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_489(p, j)
    }
}

fn action_937(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_490(p, j)
    }
}

fn action_938(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_64(p, j)
    }
}

fn action_939(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        139 => happy_shift(p, action_946, j),
        179 => happy_shift(p, action_947, j),
        _ => happy_fail(p, j)
    }
}

fn action_940(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_83(p, j)
    }
}

fn action_941(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_72(p, j)
    }
}

fn action_942(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_26, j),
        144 => happy_shift(p, action_27, j),
        145 => happy_shift(p, action_28, j),
        146 => happy_shift(p, action_29, j),
        147 => happy_shift(p, action_30, j),
        148 => happy_shift(p, action_31, j),
        149 => happy_shift(p, action_32, j),
        150 => happy_shift(p, action_33, j),
        153 => happy_shift(p, action_34, j),
        164 => happy_shift(p, action_35, j),
        184 => happy_shift(p, action_36, j),
        205 => happy_shift(p, action_37, j),
        221 => happy_shift(p, action_38, j),
        234 => happy_shift(p, action_39, j),
        235 => happy_shift(p, action_40, j),
        236 => happy_shift(p, action_41, j),
        237 => happy_shift(p, action_42, j),
        238 => happy_shift(p, action_43, j),
        241 => happy_shift(p, action_44, j),
        242 => happy_shift(p, action_45, j),
        243 => happy_shift(p, action_46, j),
        244 => happy_shift(p, action_47, j),
        245 => happy_shift(p, action_48, j),
        246 => happy_shift(p, action_49, j),
        247 => happy_shift(p, action_50, j),
        100 => happy_goto(p, action_6, j),
        104 => happy_goto(p, action_7, j),
        106 => happy_goto(p, action_8, j),
        107 => happy_goto(p, action_9, j),
        108 => happy_goto(p, action_10, j),
        109 => happy_goto(p, action_11, j),
        110 => happy_goto(p, action_12, j),
        111 => happy_goto(p, action_13, j),
        112 => happy_goto(p, action_14, j),
        113 => happy_goto(p, action_15, j),
        114 => happy_goto(p, action_16, j),
        115 => happy_goto(p, action_17, j),
        116 => happy_goto(p, action_18, j),
        117 => happy_goto(p, action_19, j),
        118 => happy_goto(p, action_20, j),
        119 => happy_goto(p, action_21, j),
        120 => happy_goto(p, action_22, j),
        122 => happy_goto(p, action_945, j),
        127 => happy_goto(p, action_24, j),
        128 => happy_goto(p, action_25, j),
        _ => happy_fail(p, j)
    }
}

fn action_943(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        138 => happy_shift(p, action_26, j),
        144 => happy_shift(p, action_27, j),
        145 => happy_shift(p, action_28, j),
        146 => happy_shift(p, action_29, j),
        147 => happy_shift(p, action_30, j),
        148 => happy_shift(p, action_31, j),
        149 => happy_shift(p, action_32, j),
        150 => happy_shift(p, action_33, j),
        153 => happy_shift(p, action_34, j),
        164 => happy_shift(p, action_35, j),
        184 => happy_shift(p, action_36, j),
        205 => happy_shift(p, action_37, j),
        221 => happy_shift(p, action_38, j),
        234 => happy_shift(p, action_39, j),
        235 => happy_shift(p, action_40, j),
        236 => happy_shift(p, action_41, j),
        237 => happy_shift(p, action_42, j),
        238 => happy_shift(p, action_43, j),
        241 => happy_shift(p, action_44, j),
        242 => happy_shift(p, action_45, j),
        243 => happy_shift(p, action_46, j),
        244 => happy_shift(p, action_47, j),
        245 => happy_shift(p, action_48, j),
        246 => happy_shift(p, action_49, j),
        247 => happy_shift(p, action_50, j),
        100 => happy_goto(p, action_6, j),
        104 => happy_goto(p, action_7, j),
        106 => happy_goto(p, action_8, j),
        107 => happy_goto(p, action_9, j),
        108 => happy_goto(p, action_10, j),
        109 => happy_goto(p, action_11, j),
        110 => happy_goto(p, action_12, j),
        111 => happy_goto(p, action_13, j),
        112 => happy_goto(p, action_14, j),
        113 => happy_goto(p, action_15, j),
        114 => happy_goto(p, action_16, j),
        115 => happy_goto(p, action_17, j),
        116 => happy_goto(p, action_18, j),
        117 => happy_goto(p, action_19, j),
        118 => happy_goto(p, action_20, j),
        119 => happy_goto(p, action_21, j),
        120 => happy_goto(p, action_22, j),
        122 => happy_goto(p, action_944, j),
        127 => happy_goto(p, action_24, j),
        128 => happy_goto(p, action_25, j),
        _ => happy_fail(p, j)
    }
}

fn action_944(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        139 => happy_shift(p, action_951, j),
        _ => happy_fail(p, j)
    }
}

fn action_945(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        139 => happy_shift(p, action_950, j),
        _ => happy_fail(p, j)
    }
}

fn action_946(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        180 => happy_shift(p, action_949, j),
        _ => happy_fail(p, j)
    }
}

fn action_947(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        237 => happy_shift(p, action_42, j),
        128 => happy_goto(p, action_948, j),
        _ => happy_fail(p, j)
    }
}

fn action_948(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_84(p, j)
    }
}

fn action_949(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_73(p, j)
    }
}

fn action_950(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_82(p, j)
    }
}

fn action_951(p: &mut Parser, i: isize, j: isize) -> Res<Cont> {
    match i {
        _ => happy_reduce_81(p, j)
    }
}

fn happy_reduce_4(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 1, 7, happy_reduction_4, i)
}

fn happy_reduction_4(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap()) {
        HappyAbsSyn::NT8(mut happy_var_1) => {
                      let decls = happy_var_1;
                      if decls.len() == 0 {
                          let name = p.new_name();
                          let pos = Rc::new(p.pos_clone());
                          let nodeinfo = NodeInfo::new(pos.clone(), pos, 0, name);
                          Ok(box CTranslationUnit(decls, nodeinfo))
                      } else {
                          with_pos!(p, decls[0], |at| box CTranslationUnit(decls, at))
                      }
        }.map(HappyAbsSyn::NT7),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_5(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_0(p, 8, happy_reduction_5(), i)
}

fn happy_reduction_5() -> HappyAbsSyn {
    HappyAbsSyn::NT8(vec![])
}


fn happy_reduce_6(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_2(p, 8, happy_reduction_6, i)
}

fn happy_reduction_6(happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_2, happy_x_1) {
        (_, HappyAbsSyn::NT8(mut happy_var_1)) => HappyAbsSyn::NT8({happy_var_1}),
        _ => unreachable!()
    }
}


fn happy_reduce_7(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_2(p, 8, happy_reduction_7, i)
}

fn happy_reduction_7(happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_2, happy_x_1) {
        (HappyAbsSyn::NT9(mut happy_var_2), HappyAbsSyn::NT8(mut happy_var_1)) => HappyAbsSyn::NT8({appended(happy_var_1, *happy_var_2)}),
        _ => unreachable!()
    }
}


fn happy_reduce_8(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_1(p, 9, happy_reduction_8, i)
}

fn happy_reduction_8(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn::NT10(mut happy_var_1) => HappyAbsSyn::NT9({box CFDefExt(*happy_var_1)}),
        _ => unreachable!()
    }
}


fn happy_reduce_9(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_1(p, 9, happy_reduction_9, i)
}

fn happy_reduction_9(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn::NT32(mut happy_var_1) => HappyAbsSyn::NT9({box CDeclExt(*happy_var_1)}),
        _ => unreachable!()
    }
}


fn happy_reduce_10(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_2(p, 9, happy_reduction_10, i)
}

fn happy_reduction_10(happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_2, happy_x_1) {
        (HappyAbsSyn::NT9(mut happy_var_2), _) => HappyAbsSyn::NT9({happy_var_2}),
        _ => unreachable!()
    }
}


fn happy_reduce_11(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 5, 9, happy_reduction_11, i)
}

fn happy_reduction_11(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, _, HappyAbsSyn::NT128(mut happy_var_3), _, HappyAbsSyn::Terminal(mut happy_var_1)) => { with_pos!(p, happy_var_1, |at| box CAsmExt(*happy_var_3, at))
        }.map(HappyAbsSyn::NT9),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_12(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 2, 10, happy_reduction_12, i)
}

fn happy_reduction_12(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn::NT12(mut happy_var_2), HappyAbsSyn::NT11(mut happy_var_1)) => { p.leave_scope(); with_pos!(p, happy_var_1, |at| box CFunctionDef(vec![], happy_var_1, vec![], happy_var_2, at))
        }.map(HappyAbsSyn::NT10),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_13(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 3, 10, happy_reduction_13, i)
}

fn happy_reduction_13(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn::NT12(mut happy_var_3), HappyAbsSyn::NT11(mut happy_var_2), HappyAbsSyn::NT132(mut happy_var_1)) => { p.leave_scope(); with_pos!(p, happy_var_1, |at| box CFunctionDef(lift_attrs(happy_var_1), happy_var_2, vec![], happy_var_3, at))
        }.map(HappyAbsSyn::NT10),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_14(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 3, 10, happy_reduction_14, i)
}

fn happy_reduction_14(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn::NT12(mut happy_var_3), HappyAbsSyn::NT11(mut happy_var_2), HappyAbsSyn::NT37(mut happy_var_1)) => { p.leave_scope(); with_pos!(p, happy_var_1, |at| box CFunctionDef(happy_var_1, happy_var_2, vec![], happy_var_3, at))
        }.map(HappyAbsSyn::NT10),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_15(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 3, 10, happy_reduction_15, i)
}

fn happy_reduction_15(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn::NT12(mut happy_var_3), HappyAbsSyn::NT11(mut happy_var_2), HappyAbsSyn::NT37(mut happy_var_1)) => { p.leave_scope(); with_pos!(p, happy_var_1, |at| box CFunctionDef(happy_var_1, happy_var_2, vec![], happy_var_3, at))
        }.map(HappyAbsSyn::NT10),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_16(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 3, 10, happy_reduction_16, i)
}

fn happy_reduction_16(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn::NT12(mut happy_var_3), HappyAbsSyn::NT11(mut happy_var_2), HappyAbsSyn::NT37(mut happy_var_1)) => { p.leave_scope(); with_pos!(p, happy_var_1, |at| box CFunctionDef(happy_var_1, happy_var_2, vec![], happy_var_3, at))
        }.map(HappyAbsSyn::NT10),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_17(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 3, 10, happy_reduction_17, i)
}

fn happy_reduction_17(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn::NT12(mut happy_var_3), HappyAbsSyn::NT11(mut happy_var_2), HappyAbsSyn::NT65(mut happy_var_1)) => { p.leave_scope(); with_pos!(p, happy_var_1, |at| box CFunctionDef(lift_type_quals(happy_var_1), happy_var_2, vec![], happy_var_3, at))
        }.map(HappyAbsSyn::NT10),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_18(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 4, 10, happy_reduction_18, i)
}

fn happy_reduction_18(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn::NT12(mut happy_var_4), HappyAbsSyn::NT11(mut happy_var_3), HappyAbsSyn::NT132(mut happy_var_2), HappyAbsSyn::NT65(mut happy_var_1)) => { p.leave_scope(); with_pos!(p, happy_var_1, |at| box CFunctionDef(add_vecs(lift_type_quals(happy_var_1), lift_attrs(happy_var_2)), happy_var_3, vec![], happy_var_4, at))
        }.map(HappyAbsSyn::NT10),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_19(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 3, 10, happy_reduction_19, i)
}

fn happy_reduction_19(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn::NT12(mut happy_var_3), HappyAbsSyn::NT33(mut happy_var_2), HappyAbsSyn::NT11(mut happy_var_1)) => { with_pos!(p, happy_var_1, |at| box CFunctionDef(vec![], happy_var_1, happy_var_2, happy_var_3, at))
        }.map(HappyAbsSyn::NT10),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_20(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 4, 10, happy_reduction_20, i)
}

fn happy_reduction_20(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn::NT12(mut happy_var_4), HappyAbsSyn::NT33(mut happy_var_3), HappyAbsSyn::NT11(mut happy_var_2), HappyAbsSyn::NT132(mut happy_var_1)) => { with_pos!(p, happy_var_2, |at| box CFunctionDef(lift_attrs(happy_var_1), happy_var_2, happy_var_3, happy_var_4, at))
        }.map(HappyAbsSyn::NT10),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_21(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 4, 10, happy_reduction_21, i)
}

fn happy_reduction_21(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn::NT12(mut happy_var_4), HappyAbsSyn::NT33(mut happy_var_3), HappyAbsSyn::NT11(mut happy_var_2), HappyAbsSyn::NT37(mut happy_var_1)) => { with_pos!(p, happy_var_1, |at| box CFunctionDef(happy_var_1, happy_var_2, happy_var_3, happy_var_4, at))
        }.map(HappyAbsSyn::NT10),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_22(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 4, 10, happy_reduction_22, i)
}

fn happy_reduction_22(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn::NT12(mut happy_var_4), HappyAbsSyn::NT33(mut happy_var_3), HappyAbsSyn::NT11(mut happy_var_2), HappyAbsSyn::NT37(mut happy_var_1)) => { with_pos!(p, happy_var_1, |at| box CFunctionDef(happy_var_1, happy_var_2, happy_var_3, happy_var_4, at))
        }.map(HappyAbsSyn::NT10),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_23(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 4, 10, happy_reduction_23, i)
}

fn happy_reduction_23(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn::NT12(mut happy_var_4), HappyAbsSyn::NT33(mut happy_var_3), HappyAbsSyn::NT11(mut happy_var_2), HappyAbsSyn::NT37(mut happy_var_1)) => { with_pos!(p, happy_var_1, |at| box CFunctionDef(happy_var_1, happy_var_2, happy_var_3, happy_var_4, at))
        }.map(HappyAbsSyn::NT10),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_24(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 4, 10, happy_reduction_24, i)
}

fn happy_reduction_24(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn::NT12(mut happy_var_4), HappyAbsSyn::NT33(mut happy_var_3), HappyAbsSyn::NT11(mut happy_var_2), HappyAbsSyn::NT65(mut happy_var_1)) => { with_pos!(p, happy_var_1, |at| box CFunctionDef(lift_type_quals(happy_var_1), happy_var_2, happy_var_3, happy_var_4, at))
        }.map(HappyAbsSyn::NT10),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_25(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 5, 10, happy_reduction_25, i)
}

fn happy_reduction_25(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn::NT12(mut happy_var_5), HappyAbsSyn::NT33(mut happy_var_4), HappyAbsSyn::NT11(mut happy_var_3), HappyAbsSyn::NT132(mut happy_var_2), HappyAbsSyn::NT65(mut happy_var_1)) => { with_pos!(p, happy_var_1, |at| box CFunctionDef(add_vecs(lift_type_quals(happy_var_1), lift_attrs(happy_var_2)), happy_var_3, happy_var_4, happy_var_5, at))
        }.map(HappyAbsSyn::NT10),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_26(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 1, 11, happy_reduction_26, i)
}

fn happy_reduction_26(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap()) {
        HappyAbsSyn::NT66(mut happy_var_1) => {
            let declr = happy_var_1.reverse();
            p.enter_scope();
            p.do_func_param_decl_ident(&declr);
            Ok(declr)
        }.map(HappyAbsSyn::NT11),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_27(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_1(p, 12, happy_reduction_27, i)
}

fn happy_reduction_27(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn::NT12(mut happy_var_1) => HappyAbsSyn::NT12({happy_var_1}),
        _ => unreachable!()
    }
}


fn happy_reduce_28(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_1(p, 12, happy_reduction_28, i)
}

fn happy_reduction_28(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn::NT12(mut happy_var_1) => HappyAbsSyn::NT12({happy_var_1}),
        _ => unreachable!()
    }
}


fn happy_reduce_29(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_1(p, 12, happy_reduction_29, i)
}

fn happy_reduction_29(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn::NT12(mut happy_var_1) => HappyAbsSyn::NT12({happy_var_1}),
        _ => unreachable!()
    }
}


fn happy_reduce_30(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_1(p, 12, happy_reduction_30, i)
}

fn happy_reduction_30(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn::NT12(mut happy_var_1) => HappyAbsSyn::NT12({happy_var_1}),
        _ => unreachable!()
    }
}


fn happy_reduce_31(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_1(p, 12, happy_reduction_31, i)
}

fn happy_reduction_31(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn::NT12(mut happy_var_1) => HappyAbsSyn::NT12({happy_var_1}),
        _ => unreachable!()
    }
}


fn happy_reduce_32(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_1(p, 12, happy_reduction_32, i)
}

fn happy_reduction_32(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn::NT12(mut happy_var_1) => HappyAbsSyn::NT12({happy_var_1}),
        _ => unreachable!()
    }
}


fn happy_reduce_33(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 1, 12, happy_reduction_33, i)
}

fn happy_reduction_33(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap()) {
        HappyAbsSyn::NT26(mut happy_var_1) => { with_pos!(p, happy_var_1, |at| box CAsm(happy_var_1, at))
        }.map(HappyAbsSyn::NT12),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_34(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 4, 13, happy_reduction_34, i)
}

fn happy_reduction_34(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn::NT12(mut happy_var_4), HappyAbsSyn::NT132(mut happy_var_3), _, HappyAbsSyn::NT131(mut happy_var_1)) => { with_pos!(p, happy_var_1, |at| box CLabel(happy_var_1, happy_var_4, happy_var_3, at))
        }.map(HappyAbsSyn::NT12),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_35(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 4, 13, happy_reduction_35, i)
}

fn happy_reduction_35(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn::NT12(mut happy_var_4), _, HappyAbsSyn::NT100(mut happy_var_2), HappyAbsSyn::Terminal(mut happy_var_1)) => { with_pos!(p, happy_var_1, |at| box CCase(happy_var_2, happy_var_4, at))
        }.map(HappyAbsSyn::NT12),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_36(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 3, 13, happy_reduction_36, i)
}

fn happy_reduction_36(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn::NT12(mut happy_var_3), _, HappyAbsSyn::Terminal(mut happy_var_1)) => { with_pos!(p, happy_var_1, |at| box CDefault(happy_var_3, at))
        }.map(HappyAbsSyn::NT12),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_37(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 6, 13, happy_reduction_37, i)
}

fn happy_reduction_37(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn::NT12(mut happy_var_6), _, HappyAbsSyn::NT100(mut happy_var_4), _, HappyAbsSyn::NT100(mut happy_var_2), HappyAbsSyn::Terminal(mut happy_var_1)) => { with_pos!(p, happy_var_1, |at| box CCases(happy_var_2, happy_var_4, happy_var_6, at))
        }.map(HappyAbsSyn::NT12),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_38(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 5, 14, happy_reduction_38, i)
}

fn happy_reduction_38(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, _, HappyAbsSyn::NT17(mut happy_var_3), _, HappyAbsSyn::Terminal(mut happy_var_1)) => { with_pos!(p, happy_var_1, |at| box CCompound(vec![], happy_var_3, at))
        }.map(HappyAbsSyn::NT12),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_39(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 6, 14, happy_reduction_39, i)
}

fn happy_reduction_39(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, _, HappyAbsSyn::NT17(mut happy_var_4), HappyAbsSyn::NT21(mut happy_var_3), _, HappyAbsSyn::Terminal(mut happy_var_1)) => { with_pos!(p, happy_var_1, |at| box CCompound(happy_var_3, happy_var_4, at))
        }.map(HappyAbsSyn::NT12),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_40(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 0, 15, happy_reduction_40, i)
}

fn happy_reduction_40(p: &mut Parser) -> Res<HappyAbsSyn> {
    match () {
        () => { Ok(p.enter_scope())
        }.map(HappyAbsSyn::NT15),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_41(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 0, 16, happy_reduction_41, i)
}

fn happy_reduction_41(p: &mut Parser) -> Res<HappyAbsSyn> {
    match () {
        () => { Ok(p.leave_scope())
        }.map(HappyAbsSyn::NT15),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_42(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_0(p, 17, happy_reduction_42(), i)
}

fn happy_reduction_42() -> HappyAbsSyn {
    HappyAbsSyn::NT17(vec![])
}


fn happy_reduce_43(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_2(p, 17, happy_reduction_43, i)
}

fn happy_reduction_43(happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_2, happy_x_1) {
        (HappyAbsSyn::NT18(mut happy_var_2), HappyAbsSyn::NT17(mut happy_var_1)) => HappyAbsSyn::NT17({appended(happy_var_1, *happy_var_2)}),
        _ => unreachable!()
    }
}


fn happy_reduce_44(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_1(p, 18, happy_reduction_44, i)
}

fn happy_reduction_44(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn::NT12(mut happy_var_1) => HappyAbsSyn::NT18({box CBlockStmt(*happy_var_1)}),
        _ => unreachable!()
    }
}


fn happy_reduce_45(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_1(p, 18, happy_reduction_45, i)
}

fn happy_reduction_45(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn::NT18(mut happy_var_1) => HappyAbsSyn::NT18({happy_var_1}),
        _ => unreachable!()
    }
}


fn happy_reduce_46(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_1(p, 19, happy_reduction_46, i)
}

fn happy_reduction_46(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn::NT32(mut happy_var_1) => HappyAbsSyn::NT18({box CBlockDecl(*happy_var_1)}),
        _ => unreachable!()
    }
}


fn happy_reduce_47(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_1(p, 19, happy_reduction_47, i)
}

fn happy_reduction_47(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn::NT10(mut happy_var_1) => HappyAbsSyn::NT18({box CNestedFunDef(*happy_var_1)}),
        _ => unreachable!()
    }
}


fn happy_reduce_48(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_2(p, 19, happy_reduction_48, i)
}

fn happy_reduction_48(happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_2, happy_x_1) {
        (HappyAbsSyn::NT18(mut happy_var_2), _) => HappyAbsSyn::NT18({happy_var_2}),
        _ => unreachable!()
    }
}


fn happy_reduce_49(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 3, 20, happy_reduction_49, i)
}

fn happy_reduction_49(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn::NT12(mut happy_var_3), HappyAbsSyn::NT11(mut happy_var_2), HappyAbsSyn::NT37(mut happy_var_1)) => { p.leave_scope(); with_pos!(p, happy_var_1, |at| box CFunctionDef(happy_var_1, happy_var_2, vec![], happy_var_3, at))
        }.map(HappyAbsSyn::NT10),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_50(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 3, 20, happy_reduction_50, i)
}

fn happy_reduction_50(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn::NT12(mut happy_var_3), HappyAbsSyn::NT11(mut happy_var_2), HappyAbsSyn::NT37(mut happy_var_1)) => { p.leave_scope(); with_pos!(p, happy_var_1, |at| box CFunctionDef(happy_var_1, happy_var_2, vec![], happy_var_3, at))
        }.map(HappyAbsSyn::NT10),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_51(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 3, 20, happy_reduction_51, i)
}

fn happy_reduction_51(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn::NT12(mut happy_var_3), HappyAbsSyn::NT11(mut happy_var_2), HappyAbsSyn::NT37(mut happy_var_1)) => { p.leave_scope(); with_pos!(p, happy_var_1, |at| box CFunctionDef(happy_var_1, happy_var_2, vec![], happy_var_3, at))
        }.map(HappyAbsSyn::NT10),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_52(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 3, 20, happy_reduction_52, i)
}

fn happy_reduction_52(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn::NT12(mut happy_var_3), HappyAbsSyn::NT11(mut happy_var_2), HappyAbsSyn::NT65(mut happy_var_1)) => { p.leave_scope(); with_pos!(p, happy_var_1, |at| box CFunctionDef(lift_type_quals(happy_var_1), happy_var_2, vec![], happy_var_3, at))
        }.map(HappyAbsSyn::NT10),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_53(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 4, 20, happy_reduction_53, i)
}

fn happy_reduction_53(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn::NT12(mut happy_var_4), HappyAbsSyn::NT11(mut happy_var_3), HappyAbsSyn::NT132(mut happy_var_2), HappyAbsSyn::NT65(mut happy_var_1)) => { p.leave_scope(); with_pos!(p, happy_var_1, |at| box CFunctionDef(add_vecs(lift_type_quals(happy_var_1), lift_attrs(happy_var_2)),
                                                                   happy_var_3, vec![], happy_var_4, at))
        }.map(HappyAbsSyn::NT10),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_54(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_3(p, 21, happy_reduction_54, i)
}

fn happy_reduction_54(happy_x_3: HappyAbsSyn, happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_3, happy_x_2, happy_x_1) {
        (_, HappyAbsSyn::NT21(mut happy_var_2), _) => HappyAbsSyn::NT21({happy_var_2}),
        _ => unreachable!()
    }
}


fn happy_reduce_55(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_reduce(p, 4, 21, happy_reduction_55, i)
}

fn happy_reduction_55(p: &mut Parser) {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, HappyAbsSyn::NT21(mut happy_var_3), _, HappyAbsSyn::NT21(mut happy_var_1)) => {            p.stack.push(HappyAbsSyn::NT21({add_vecs(happy_var_1, happy_var_3)})); }
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_56(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 1, 22, happy_reduction_56, i)
}

fn happy_reduction_56(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap()) {
        HappyAbsSyn::Terminal(mut happy_var_1) => { with_pos!(p, happy_var_1, |at| box CExpr(None, at))
        }.map(HappyAbsSyn::NT12),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_57(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 2, 22, happy_reduction_57, i)
}

fn happy_reduction_57(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, HappyAbsSyn::NT100(mut happy_var_1)) => { with_pos!(p, happy_var_1, |at| box CExpr(Some(happy_var_1), at))
        }.map(HappyAbsSyn::NT12),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_58(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 5, 23, happy_reduction_58, i)
}

fn happy_reduction_58(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn::NT12(mut happy_var_5), _, HappyAbsSyn::NT100(mut happy_var_3), _, HappyAbsSyn::Terminal(mut happy_var_1)) => { with_pos!(p, happy_var_1, |at| box CIf(happy_var_3, happy_var_5, None, at))
        }.map(HappyAbsSyn::NT12),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_59(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 7, 23, happy_reduction_59, i)
}

fn happy_reduction_59(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn::NT12(mut happy_var_7), _, HappyAbsSyn::NT12(mut happy_var_5), _, HappyAbsSyn::NT100(mut happy_var_3), _, HappyAbsSyn::Terminal(mut happy_var_1)) => { with_pos!(p, happy_var_1, |at| box CIf(happy_var_3, happy_var_5, Some(happy_var_7), at))
        }.map(HappyAbsSyn::NT12),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_60(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 5, 23, happy_reduction_60, i)
}

fn happy_reduction_60(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn::NT12(mut happy_var_5), _, HappyAbsSyn::NT100(mut happy_var_3), _, HappyAbsSyn::Terminal(mut happy_var_1)) => { with_pos!(p, happy_var_1, |at| box CSwitch(happy_var_3, happy_var_5, at))
        }.map(HappyAbsSyn::NT12),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_61(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 5, 24, happy_reduction_61, i)
}

fn happy_reduction_61(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn::NT12(mut happy_var_5), _, HappyAbsSyn::NT100(mut happy_var_3), _, HappyAbsSyn::Terminal(mut happy_var_1)) => { with_pos!(p, happy_var_1, |at| box CWhile(happy_var_3, happy_var_5, false, at))
        }.map(HappyAbsSyn::NT12),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_62(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 7, 24, happy_reduction_62, i)
}

fn happy_reduction_62(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, _, HappyAbsSyn::NT100(mut happy_var_5), _, _, HappyAbsSyn::NT12(mut happy_var_2), HappyAbsSyn::Terminal(mut happy_var_1)) => { with_pos!(p, happy_var_1, |at| box CWhile(happy_var_5, happy_var_2, true, at))
        }.map(HappyAbsSyn::NT12),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_63(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 9, 24, happy_reduction_63, i)
}

fn happy_reduction_63(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn::NT12(mut happy_var_9), _, HappyAbsSyn::NT124(mut happy_var_7), _, HappyAbsSyn::NT124(mut happy_var_5), _, HappyAbsSyn::NT124(mut happy_var_3), _, HappyAbsSyn::Terminal(mut happy_var_1)) => { with_pos!(p, happy_var_1, |at| box CFor(Left(happy_var_3), happy_var_5, happy_var_7, happy_var_9, at))
        }.map(HappyAbsSyn::NT12),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_64(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 10, 24, happy_reduction_64, i)
}

fn happy_reduction_64(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, HappyAbsSyn::NT12(mut happy_var_9), _, HappyAbsSyn::NT124(mut happy_var_7), _, HappyAbsSyn::NT124(mut happy_var_5), HappyAbsSyn::NT32(mut happy_var_4), _, _, HappyAbsSyn::Terminal(mut happy_var_1)) => { with_pos!(p, happy_var_1, |at| box CFor(Right(happy_var_4), happy_var_5, happy_var_7, happy_var_9, at))
        }.map(HappyAbsSyn::NT12),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_65(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 3, 25, happy_reduction_65, i)
}

fn happy_reduction_65(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, HappyAbsSyn::NT131(mut happy_var_2), HappyAbsSyn::Terminal(mut happy_var_1)) => { with_pos!(p, happy_var_1, |at| box CGoto(happy_var_2, at))
        }.map(HappyAbsSyn::NT12),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_66(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 4, 25, happy_reduction_66, i)
}

fn happy_reduction_66(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, HappyAbsSyn::NT100(mut happy_var_3), _, HappyAbsSyn::Terminal(mut happy_var_1)) => { with_pos!(p, happy_var_1, |at| box CGotoPtr(happy_var_3, at))
        }.map(HappyAbsSyn::NT12),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_67(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 2, 25, happy_reduction_67, i)
}

fn happy_reduction_67(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, HappyAbsSyn::Terminal(mut happy_var_1)) => { with_pos!(p, happy_var_1, |at| box CCont(at))
        }.map(HappyAbsSyn::NT12),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_68(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 2, 25, happy_reduction_68, i)
}

fn happy_reduction_68(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, HappyAbsSyn::Terminal(mut happy_var_1)) => { with_pos!(p, happy_var_1, |at| box CBreak(at))
        }.map(HappyAbsSyn::NT12),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_69(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 3, 25, happy_reduction_69, i)
}

fn happy_reduction_69(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, HappyAbsSyn::NT124(mut happy_var_2), HappyAbsSyn::Terminal(mut happy_var_1)) => { with_pos!(p, happy_var_1, |at| box CReturn(happy_var_2, at))
        }.map(HappyAbsSyn::NT12),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_70(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 6, 26, happy_reduction_70, i)
}

fn happy_reduction_70(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, _, HappyAbsSyn::NT128(mut happy_var_4), _, HappyAbsSyn::NT27(mut happy_var_2), HappyAbsSyn::Terminal(mut happy_var_1)) => { with_pos!(p, happy_var_1, |at| box CAssemblyStatement(happy_var_2, happy_var_4, vec![], vec![], vec![], at))
        }.map(HappyAbsSyn::NT26),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_71(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 8, 26, happy_reduction_71, i)
}

fn happy_reduction_71(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, _, HappyAbsSyn::NT28(mut happy_var_6), _, HappyAbsSyn::NT128(mut happy_var_4), _, HappyAbsSyn::NT27(mut happy_var_2), HappyAbsSyn::Terminal(mut happy_var_1)) => { with_pos!(p, happy_var_1, |at| box CAssemblyStatement(happy_var_2, happy_var_4, happy_var_6, vec![], vec![], at))
        }.map(HappyAbsSyn::NT26),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_72(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 10, 26, happy_reduction_72, i)
}

fn happy_reduction_72(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, _, HappyAbsSyn::NT28(mut happy_var_8), _, HappyAbsSyn::NT28(mut happy_var_6), _, HappyAbsSyn::NT128(mut happy_var_4), _, HappyAbsSyn::NT27(mut happy_var_2), HappyAbsSyn::Terminal(mut happy_var_1)) => { with_pos!(p, happy_var_1, |at| box CAssemblyStatement(happy_var_2, happy_var_4, happy_var_6, happy_var_8, vec![], at))
        }.map(HappyAbsSyn::NT26),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_73(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 12, 26, happy_reduction_73, i)
}

fn happy_reduction_73(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, _, HappyAbsSyn::NT31(mut happy_var_10), _, HappyAbsSyn::NT28(mut happy_var_8), _, HappyAbsSyn::NT28(mut happy_var_6), _, HappyAbsSyn::NT128(mut happy_var_4), _, HappyAbsSyn::NT27(mut happy_var_2), HappyAbsSyn::Terminal(mut happy_var_1)) => { with_pos!(p, happy_var_1, |at| box CAssemblyStatement(happy_var_2, happy_var_4, happy_var_6, happy_var_8, happy_var_10, at))
        }.map(HappyAbsSyn::NT26),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_74(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_0(p, 27, happy_reduction_74(), i)
}

fn happy_reduction_74() -> HappyAbsSyn {
    HappyAbsSyn::NT27(None)
}


fn happy_reduce_75(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_1(p, 27, happy_reduction_75, i)
}

fn happy_reduction_75(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn::NT64(mut happy_var_1) => HappyAbsSyn::NT27({Some(happy_var_1)}),
        _ => unreachable!()
    }
}


fn happy_reduce_76(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_0(p, 28, happy_reduction_76(), i)
}

fn happy_reduction_76() -> HappyAbsSyn {
    HappyAbsSyn::NT28(vec![])
}


fn happy_reduce_77(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_1(p, 28, happy_reduction_77, i)
}

fn happy_reduction_77(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn::NT28(mut happy_var_1) => HappyAbsSyn::NT28({happy_var_1}),
        _ => unreachable!()
    }
}


fn happy_reduce_78(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_1(p, 29, happy_reduction_78, i)
}

fn happy_reduction_78(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn::NT30(mut happy_var_1) => HappyAbsSyn::NT28({vec![*happy_var_1]}),
        _ => unreachable!()
    }
}


fn happy_reduce_79(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_3(p, 29, happy_reduction_79, i)
}

fn happy_reduction_79(happy_x_3: HappyAbsSyn, happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_3, happy_x_2, happy_x_1) {
        (HappyAbsSyn::NT30(mut happy_var_3), _, HappyAbsSyn::NT28(mut happy_var_1)) => HappyAbsSyn::NT28({appended(happy_var_1, *happy_var_3)}),
        _ => unreachable!()
    }
}


fn happy_reduce_80(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 4, 30, happy_reduction_80, i)
}

fn happy_reduction_80(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, HappyAbsSyn::NT100(mut happy_var_3), _, HappyAbsSyn::NT128(mut happy_var_1)) => { with_pos!(p, happy_var_1, |at| box CAssemblyOperand(None, happy_var_1, happy_var_3, at))
        }.map(HappyAbsSyn::NT30),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_81(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 7, 30, happy_reduction_81, i)
}

fn happy_reduction_81(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, HappyAbsSyn::NT100(mut happy_var_6), _, HappyAbsSyn::NT128(mut happy_var_4), _, HappyAbsSyn::Terminal(CTokIdent(_, mut happy_var_2)), HappyAbsSyn::Terminal(mut happy_var_1)) => { with_pos!(p, happy_var_1, |at| box CAssemblyOperand(Some(happy_var_2), happy_var_4, happy_var_6, at))
        }.map(HappyAbsSyn::NT30),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_82(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 7, 30, happy_reduction_82, i)
}

fn happy_reduction_82(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, HappyAbsSyn::NT100(mut happy_var_6), _, HappyAbsSyn::NT128(mut happy_var_4), _, HappyAbsSyn::Terminal(CTokTyIdent(_, mut happy_var_2)), HappyAbsSyn::Terminal(mut happy_var_1)) => { with_pos!(p, happy_var_1, |at| box CAssemblyOperand(Some(happy_var_2), happy_var_4, happy_var_6, at))
        }.map(HappyAbsSyn::NT30),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_83(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_1(p, 31, happy_reduction_83, i)
}

fn happy_reduction_83(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn::NT128(mut happy_var_1) => HappyAbsSyn::NT31({vec![*happy_var_1]}),
        _ => unreachable!()
    }
}


fn happy_reduce_84(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_3(p, 31, happy_reduction_84, i)
}

fn happy_reduction_84(happy_x_3: HappyAbsSyn, happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_3, happy_x_2, happy_x_1) {
        (HappyAbsSyn::NT128(mut happy_var_3), _, HappyAbsSyn::NT31(mut happy_var_1)) => HappyAbsSyn::NT31({appended(happy_var_1, *happy_var_3)}),
        _ => unreachable!()
    }
}


fn happy_reduce_85(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 2, 32, happy_reduction_85, i)
}

fn happy_reduction_85(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, HappyAbsSyn::NT37(mut happy_var_1)) => { with_pos!(p, happy_var_1, |at| box CDecl(happy_var_1, vec![], at))
        }.map(HappyAbsSyn::NT32),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_86(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 2, 32, happy_reduction_86, i)
}

fn happy_reduction_86(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, HappyAbsSyn::NT37(mut happy_var_1)) => { with_pos!(p, happy_var_1, |at| box CDecl(happy_var_1, vec![], at))
        }.map(HappyAbsSyn::NT32),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_87(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 2, 32, happy_reduction_87, i)
}

fn happy_reduction_87(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, HappyAbsSyn::NT32(mut happy_var_1)) => {
            unwrap_let! { CDecl(declspecs, dies, at) = { *happy_var_1 };
                p.with_length(at, |at| box CDecl(declspecs, rev_vec(dies), at))
            }
        }.map(HappyAbsSyn::NT32),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_88(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 2, 32, happy_reduction_88, i)
}

fn happy_reduction_88(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, HappyAbsSyn::NT32(mut happy_var_1)) => {
            unwrap_let! { CDecl(declspecs, dies, at) = { *happy_var_1 };
                p.with_length(at, |at| box CDecl(declspecs, rev_vec(dies), at))
            }
        }.map(HappyAbsSyn::NT32),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_89(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 7, 32, happy_reduction_89, i)
}

fn happy_reduction_89(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, _, HappyAbsSyn::NT128(mut happy_var_5), _, HappyAbsSyn::NT100(mut happy_var_3), _, HappyAbsSyn::Terminal(mut happy_var_1)) => { with_pos!(p, happy_var_1, |at| box CStaticAssert(happy_var_3, *happy_var_5, at))
        }.map(HappyAbsSyn::NT32),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_90(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_0(p, 33, happy_reduction_90(), i)
}

fn happy_reduction_90() -> HappyAbsSyn {
    HappyAbsSyn::NT33(vec![])
}


fn happy_reduce_91(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_2(p, 33, happy_reduction_91, i)
}

fn happy_reduction_91(happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_2, happy_x_1) {
        (HappyAbsSyn::NT32(mut happy_var_2), HappyAbsSyn::NT33(mut happy_var_1)) => HappyAbsSyn::NT33({appended(happy_var_1, *happy_var_2)}),
        _ => unreachable!()
    }
}


fn happy_reduce_92(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 4, 34, happy_reduction_92, i)
}

fn happy_reduction_92(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn::NT94(mut happy_var_4), HappyAbsSyn::NT35(mut happy_var_3), HappyAbsSyn::NT66(mut happy_var_2), HappyAbsSyn::NT37(mut happy_var_1)) => {
            let declspecs = happy_var_1;
            let declr = happy_var_2.with_asm_name_attrs(*happy_var_3)?;
            p.do_decl_ident(&declspecs, &declr);
            with_pos!(p, declspecs, |at| box CDecl(declspecs, vec![(Some(declr.reverse()), happy_var_4, None)], at))
        }.map(HappyAbsSyn::NT32),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_93(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 4, 34, happy_reduction_93, i)
}

fn happy_reduction_93(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn::NT94(mut happy_var_4), HappyAbsSyn::NT35(mut happy_var_3), HappyAbsSyn::NT66(mut happy_var_2), HappyAbsSyn::NT65(mut happy_var_1)) => {
            let declspecs = lift_type_quals(happy_var_1);
            let declr = happy_var_2.with_asm_name_attrs(*happy_var_3)?;
            p.do_decl_ident(&declspecs, &declr);
            with_pos!(p, declspecs, |at| box CDecl(declspecs, vec![(Some(declr.reverse()), happy_var_4, None)], at))
        }.map(HappyAbsSyn::NT32),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_94(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 5, 34, happy_reduction_94, i)
}

fn happy_reduction_94(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn::NT94(mut happy_var_5), HappyAbsSyn::NT35(mut happy_var_4), HappyAbsSyn::NT66(mut happy_var_3), HappyAbsSyn::NT132(mut happy_var_2), HappyAbsSyn::NT65(mut happy_var_1)) => {
            let declspecs = lift_type_quals(happy_var_1);
            let declr = happy_var_3.with_asm_name_attrs(*happy_var_4)?;
            p.do_decl_ident(&declspecs, &declr);
            with_pos!(p, declspecs, |at| box CDecl(add_vecs(declspecs, lift_attrs(happy_var_2)),
                                                   vec![(Some(declr.reverse()), happy_var_5, None)], at))
        }.map(HappyAbsSyn::NT32),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_95(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 4, 34, happy_reduction_95, i)
}

fn happy_reduction_95(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn::NT94(mut happy_var_4), HappyAbsSyn::NT35(mut happy_var_3), HappyAbsSyn::NT66(mut happy_var_2), HappyAbsSyn::NT132(mut happy_var_1)) => {
            let declspecs = lift_attrs(happy_var_1);
            let declr = happy_var_2.with_asm_name_attrs(*happy_var_3)?;
            p.do_decl_ident(&declspecs, &declr);
            with_pos!(p, declspecs, |at| box CDecl(declspecs, vec![(Some(declr.reverse()), happy_var_4, None)], at))
        }.map(HappyAbsSyn::NT32),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_96(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 6, 34, happy_reduction_96, i)
}

fn happy_reduction_96(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn::NT94(mut happy_var_6), HappyAbsSyn::NT35(mut happy_var_5), HappyAbsSyn::NT66(mut happy_var_4), HappyAbsSyn::NT132(mut happy_var_3), _, HappyAbsSyn::NT32(mut happy_var_1)) => {
            unwrap_let! { CDecl(declspecs, dies, at) = { *happy_var_1 };
                let (f, s) = { *happy_var_5 };
                let declr = happy_var_4.with_asm_name_attrs((f, add_vecs(s, happy_var_3)))?;
                p.do_decl_ident(&declspecs, &declr);
                p.with_length(at, |at| box CDecl(declspecs, prepend((Some(declr.reverse()), happy_var_6, None), dies), at))
            }
        }.map(HappyAbsSyn::NT32),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_97(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_2(p, 35, happy_reduction_97, i)
}

fn happy_reduction_97(happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_2, happy_x_1) {
        (HappyAbsSyn::NT132(mut happy_var_2), HappyAbsSyn::NT67(mut happy_var_1)) => HappyAbsSyn::NT35({box (happy_var_1, happy_var_2)}),
        _ => unreachable!()
    }
}


fn happy_reduce_98(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 4, 36, happy_reduction_98, i)
}

fn happy_reduction_98(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn::NT94(mut happy_var_4), HappyAbsSyn::NT35(mut happy_var_3), HappyAbsSyn::NT66(mut happy_var_2), HappyAbsSyn::NT37(mut happy_var_1)) => {
            let declr = happy_var_2.with_asm_name_attrs(*happy_var_3)?;
            p.do_decl_ident(&happy_var_1, &declr);
            with_pos!(p, happy_var_1, |at| box CDecl(happy_var_1, vec![(Some(declr.reverse()), happy_var_4, None)], at))
        }.map(HappyAbsSyn::NT32),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_99(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 4, 36, happy_reduction_99, i)
}

fn happy_reduction_99(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn::NT94(mut happy_var_4), HappyAbsSyn::NT35(mut happy_var_3), HappyAbsSyn::NT66(mut happy_var_2), HappyAbsSyn::NT37(mut happy_var_1)) => {
            let declr = happy_var_2.with_asm_name_attrs(*happy_var_3)?;
            p.do_decl_ident(&happy_var_1, &declr);
            with_pos!(p, happy_var_1, |at| box CDecl(happy_var_1, vec![(Some(declr.reverse()), happy_var_4, None)], at))
        }.map(HappyAbsSyn::NT32),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_100(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 6, 36, happy_reduction_100, i)
}

fn happy_reduction_100(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn::NT94(mut happy_var_6), HappyAbsSyn::NT35(mut happy_var_5), HappyAbsSyn::NT66(mut happy_var_4), HappyAbsSyn::NT132(mut happy_var_3), _, HappyAbsSyn::NT32(mut happy_var_1)) => {
            unwrap_let! { CDecl(declspecs, dies, at) = { *happy_var_1 };
                let (f, s) = { *happy_var_5 };
                let declr = happy_var_4.with_asm_name_attrs((f, add_vecs(s, happy_var_3)))?;
                p.do_decl_ident(&declspecs, &declr);
                Ok(box CDecl(declspecs, prepend((Some(declr.reverse()), happy_var_6, None), dies), at))
            }
        }.map(HappyAbsSyn::NT32),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_101(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_1(p, 37, happy_reduction_101, i)
}

fn happy_reduction_101(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn::NT37(mut happy_var_1) => HappyAbsSyn::NT37({happy_var_1}),
        _ => unreachable!()
    }
}


fn happy_reduce_102(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_1(p, 37, happy_reduction_102, i)
}

fn happy_reduction_102(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn::NT37(mut happy_var_1) => HappyAbsSyn::NT37({happy_var_1}),
        _ => unreachable!()
    }
}


fn happy_reduce_103(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_1(p, 37, happy_reduction_103, i)
}

fn happy_reduction_103(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn::NT37(mut happy_var_1) => HappyAbsSyn::NT37({happy_var_1}),
        _ => unreachable!()
    }
}


fn happy_reduce_104(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_1(p, 38, happy_reduction_104, i)
}

fn happy_reduction_104(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn::NT39(mut happy_var_1) => HappyAbsSyn::NT37({vec![*happy_var_1]}),
        _ => unreachable!()
    }
}


fn happy_reduce_105(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_2(p, 38, happy_reduction_105, i)
}

fn happy_reduction_105(happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_2, happy_x_1) {
        (HappyAbsSyn::NT39(mut happy_var_2), HappyAbsSyn::NT132(mut happy_var_1)) => HappyAbsSyn::NT37({appended(lift_attrs(happy_var_1), *happy_var_2)}),
        _ => unreachable!()
    }
}


fn happy_reduce_106(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_2(p, 38, happy_reduction_106, i)
}

fn happy_reduction_106(happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_2, happy_x_1) {
        (HappyAbsSyn::NT39(mut happy_var_2), HappyAbsSyn::NT65(mut happy_var_1)) => HappyAbsSyn::NT37({appended(map(CTypeQual, happy_var_1), *happy_var_2)}),
        _ => unreachable!()
    }
}


fn happy_reduce_107(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_3(p, 38, happy_reduction_107, i)
}

fn happy_reduction_107(happy_x_3: HappyAbsSyn, happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_3, happy_x_2, happy_x_1) {
        (HappyAbsSyn::NT39(mut happy_var_3), HappyAbsSyn::NT132(mut happy_var_2), HappyAbsSyn::NT65(mut happy_var_1)) => HappyAbsSyn::NT37({appended(add_vecs(map(CTypeQual, happy_var_1), lift_attrs(happy_var_2)), *happy_var_3)}),
        _ => unreachable!()
    }
}


fn happy_reduce_108(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_2(p, 38, happy_reduction_108, i)
}

fn happy_reduction_108(happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_2, happy_x_1) {
        (HappyAbsSyn::NT39(mut happy_var_2), HappyAbsSyn::NT37(mut happy_var_1)) => HappyAbsSyn::NT37({appended(happy_var_1, *happy_var_2)}),
        _ => unreachable!()
    }
}


fn happy_reduce_109(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_2(p, 38, happy_reduction_109, i)
}

fn happy_reduction_109(happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_2, happy_x_1) {
        (HappyAbsSyn::NT132(mut happy_var_2), HappyAbsSyn::NT37(mut happy_var_1)) => HappyAbsSyn::NT37({add_trailing_attrs(happy_var_1, happy_var_2)}),
        _ => unreachable!()
    }
}


fn happy_reduce_110(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_1(p, 39, happy_reduction_110, i)
}

fn happy_reduction_110(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn::NT41(mut happy_var_1) => HappyAbsSyn::NT39({box CStorageSpec(*happy_var_1)}),
        _ => unreachable!()
    }
}


fn happy_reduce_111(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_1(p, 39, happy_reduction_111, i)
}

fn happy_reduction_111(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn::NT64(mut happy_var_1) => HappyAbsSyn::NT39({box CTypeQual(*happy_var_1)}),
        _ => unreachable!()
    }
}


fn happy_reduce_112(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_1(p, 39, happy_reduction_112, i)
}

fn happy_reduction_112(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn::NT42(mut happy_var_1) => HappyAbsSyn::NT39({box CFunSpec(*happy_var_1)}),
        _ => unreachable!()
    }
}


fn happy_reduce_113(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_1(p, 39, happy_reduction_113, i)
}

fn happy_reduction_113(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn::NT43(mut happy_var_1) => HappyAbsSyn::NT39({box CAlignSpec(*happy_var_1)}),
        _ => unreachable!()
    }
}


fn happy_reduce_114(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_1(p, 40, happy_reduction_114, i)
}

fn happy_reduction_114(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn::NT41(mut happy_var_1) => HappyAbsSyn::NT39({box CStorageSpec(*happy_var_1)}),
        _ => unreachable!()
    }
}


fn happy_reduce_115(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_1(p, 40, happy_reduction_115, i)
}

fn happy_reduction_115(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn::NT42(mut happy_var_1) => HappyAbsSyn::NT39({box CFunSpec(*happy_var_1)}),
        _ => unreachable!()
    }
}


fn happy_reduce_116(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_1(p, 40, happy_reduction_116, i)
}

fn happy_reduction_116(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn::NT43(mut happy_var_1) => HappyAbsSyn::NT39({box CAlignSpec(*happy_var_1)}),
        _ => unreachable!()
    }
}


fn happy_reduce_117(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 1, 41, happy_reduction_117, i)
}

fn happy_reduction_117(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap()) {
        HappyAbsSyn::Terminal(mut happy_var_1) => { with_pos!(p, happy_var_1, |at| box CTypedef(at))
        }.map(HappyAbsSyn::NT41),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_118(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 1, 41, happy_reduction_118, i)
}

fn happy_reduction_118(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap()) {
        HappyAbsSyn::Terminal(mut happy_var_1) => { with_pos!(p, happy_var_1, |at| box CExtern(at))
        }.map(HappyAbsSyn::NT41),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_119(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 1, 41, happy_reduction_119, i)
}

fn happy_reduction_119(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap()) {
        HappyAbsSyn::Terminal(mut happy_var_1) => { with_pos!(p, happy_var_1, |at| box CStatic(at))
        }.map(HappyAbsSyn::NT41),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_120(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 1, 41, happy_reduction_120, i)
}

fn happy_reduction_120(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap()) {
        HappyAbsSyn::Terminal(mut happy_var_1) => { with_pos!(p, happy_var_1, |at| box CAuto(at))
        }.map(HappyAbsSyn::NT41),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_121(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 1, 41, happy_reduction_121, i)
}

fn happy_reduction_121(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap()) {
        HappyAbsSyn::Terminal(mut happy_var_1) => { with_pos!(p, happy_var_1, |at| box CRegister(at))
        }.map(HappyAbsSyn::NT41),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_122(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 1, 41, happy_reduction_122, i)
}

fn happy_reduction_122(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap()) {
        HappyAbsSyn::Terminal(mut happy_var_1) => { with_pos!(p, happy_var_1, |at| box CThread(at))
        }.map(HappyAbsSyn::NT41),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_123(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 1, 42, happy_reduction_123, i)
}

fn happy_reduction_123(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap()) {
        HappyAbsSyn::Terminal(mut happy_var_1) => { with_pos!(p, happy_var_1, |at| box CInlineQual(at))
        }.map(HappyAbsSyn::NT42),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_124(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 1, 42, happy_reduction_124, i)
}

fn happy_reduction_124(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap()) {
        HappyAbsSyn::Terminal(mut happy_var_1) => { with_pos!(p, happy_var_1, |at| box CNoreturnQual(at))
        }.map(HappyAbsSyn::NT42),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_125(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 4, 43, happy_reduction_125, i)
}

fn happy_reduction_125(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, HappyAbsSyn::NT32(mut happy_var_3), _, HappyAbsSyn::Terminal(mut happy_var_1)) => { with_pos!(p, happy_var_1, |at| box CAlignAsType(happy_var_3, at))
        }.map(HappyAbsSyn::NT43),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_126(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 4, 43, happy_reduction_126, i)
}

fn happy_reduction_126(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, HappyAbsSyn::NT100(mut happy_var_3), _, HappyAbsSyn::Terminal(mut happy_var_1)) => { with_pos!(p, happy_var_1, |at| box CAlignAsExpr(happy_var_3, at))
        }.map(HappyAbsSyn::NT43),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_127(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_1(p, 44, happy_reduction_127, i)
}

fn happy_reduction_127(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn::NT37(mut happy_var_1) => HappyAbsSyn::NT37({happy_var_1}),
        _ => unreachable!()
    }
}


fn happy_reduce_128(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_1(p, 44, happy_reduction_128, i)
}

fn happy_reduction_128(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn::NT37(mut happy_var_1) => HappyAbsSyn::NT37({happy_var_1}),
        _ => unreachable!()
    }
}


fn happy_reduce_129(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_1(p, 44, happy_reduction_129, i)
}

fn happy_reduction_129(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn::NT37(mut happy_var_1) => HappyAbsSyn::NT37({happy_var_1}),
        _ => unreachable!()
    }
}


fn happy_reduce_130(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 1, 45, happy_reduction_130, i)
}

fn happy_reduction_130(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap()) {
        HappyAbsSyn::Terminal(mut happy_var_1) => { with_pos!(p, happy_var_1, |at| box CVoidType(at))
        }.map(HappyAbsSyn::NT45),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_131(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 1, 45, happy_reduction_131, i)
}

fn happy_reduction_131(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap()) {
        HappyAbsSyn::Terminal(mut happy_var_1) => { with_pos!(p, happy_var_1, |at| box CCharType(at))
        }.map(HappyAbsSyn::NT45),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_132(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 1, 45, happy_reduction_132, i)
}

fn happy_reduction_132(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap()) {
        HappyAbsSyn::Terminal(mut happy_var_1) => { with_pos!(p, happy_var_1, |at| box CShortType(at))
        }.map(HappyAbsSyn::NT45),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_133(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 1, 45, happy_reduction_133, i)
}

fn happy_reduction_133(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap()) {
        HappyAbsSyn::Terminal(mut happy_var_1) => { with_pos!(p, happy_var_1, |at| box CIntType(at))
        }.map(HappyAbsSyn::NT45),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_134(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 1, 45, happy_reduction_134, i)
}

fn happy_reduction_134(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap()) {
        HappyAbsSyn::Terminal(mut happy_var_1) => { with_pos!(p, happy_var_1, |at| box CLongType(at))
        }.map(HappyAbsSyn::NT45),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_135(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 1, 45, happy_reduction_135, i)
}

fn happy_reduction_135(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap()) {
        HappyAbsSyn::Terminal(mut happy_var_1) => { with_pos!(p, happy_var_1, |at| box CFloatType(at))
        }.map(HappyAbsSyn::NT45),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_136(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 1, 45, happy_reduction_136, i)
}

fn happy_reduction_136(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap()) {
        HappyAbsSyn::Terminal(mut happy_var_1) => { with_pos!(p, happy_var_1, |at| box CDoubleType(at))
        }.map(HappyAbsSyn::NT45),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_137(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 1, 45, happy_reduction_137, i)
}

fn happy_reduction_137(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap()) {
        HappyAbsSyn::Terminal(mut happy_var_1) => { with_pos!(p, happy_var_1, |at| box CSignedType(at))
        }.map(HappyAbsSyn::NT45),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_138(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 1, 45, happy_reduction_138, i)
}

fn happy_reduction_138(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap()) {
        HappyAbsSyn::Terminal(mut happy_var_1) => { with_pos!(p, happy_var_1, |at| box CUnsigType(at))
        }.map(HappyAbsSyn::NT45),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_139(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 1, 45, happy_reduction_139, i)
}

fn happy_reduction_139(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap()) {
        HappyAbsSyn::Terminal(mut happy_var_1) => { with_pos!(p, happy_var_1, |at| box CBoolType(at))
        }.map(HappyAbsSyn::NT45),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_140(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 1, 45, happy_reduction_140, i)
}

fn happy_reduction_140(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap()) {
        HappyAbsSyn::Terminal(mut happy_var_1) => { with_pos!(p, happy_var_1, |at| box CComplexType(at))
        }.map(HappyAbsSyn::NT45),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_141(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 1, 45, happy_reduction_141, i)
}

fn happy_reduction_141(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap()) {
        HappyAbsSyn::Terminal(mut happy_var_1) => { with_pos!(p, happy_var_1, |at| box CInt128Type(at))
        }.map(HappyAbsSyn::NT45),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_142(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 1, 45, happy_reduction_142, i)
}

fn happy_reduction_142(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap()) {
        HappyAbsSyn::Terminal(mut happy_var_1) => { with_pos!(p, happy_var_1, |at| box CFloat128Type(at))
        }.map(HappyAbsSyn::NT45),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_143(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_2(p, 46, happy_reduction_143, i)
}

fn happy_reduction_143(happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_2, happy_x_1) {
        (HappyAbsSyn::NT45(mut happy_var_2), HappyAbsSyn::NT37(mut happy_var_1)) => HappyAbsSyn::NT37({appended(happy_var_1, CTypeSpec(*happy_var_2))}),
        _ => unreachable!()
    }
}


fn happy_reduce_144(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_2(p, 46, happy_reduction_144, i)
}

fn happy_reduction_144(happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_2, happy_x_1) {
        (HappyAbsSyn::NT41(mut happy_var_2), HappyAbsSyn::NT37(mut happy_var_1)) => HappyAbsSyn::NT37({appended(happy_var_1, CStorageSpec(*happy_var_2))}),
        _ => unreachable!()
    }
}


fn happy_reduce_145(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_2(p, 46, happy_reduction_145, i)
}

fn happy_reduction_145(happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_2, happy_x_1) {
        (HappyAbsSyn::NT39(mut happy_var_2), HappyAbsSyn::NT37(mut happy_var_1)) => HappyAbsSyn::NT37({appended(happy_var_1, *happy_var_2)}),
        _ => unreachable!()
    }
}


fn happy_reduce_146(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_2(p, 46, happy_reduction_146, i)
}

fn happy_reduction_146(happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_2, happy_x_1) {
        (HappyAbsSyn::NT45(mut happy_var_2), HappyAbsSyn::NT37(mut happy_var_1)) => HappyAbsSyn::NT37({appended(happy_var_1, CTypeSpec(*happy_var_2))}),
        _ => unreachable!()
    }
}


fn happy_reduce_147(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_2(p, 46, happy_reduction_147, i)
}

fn happy_reduction_147(happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_2, happy_x_1) {
        (HappyAbsSyn::NT132(mut happy_var_2), HappyAbsSyn::NT37(mut happy_var_1)) => HappyAbsSyn::NT37({add_trailing_attrs(happy_var_1, happy_var_2)}),
        _ => unreachable!()
    }
}


fn happy_reduce_148(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_1(p, 47, happy_reduction_148, i)
}

fn happy_reduction_148(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn::NT45(mut happy_var_1) => HappyAbsSyn::NT37({vec![CTypeSpec(*happy_var_1)]}),
        _ => unreachable!()
    }
}


fn happy_reduce_149(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_2(p, 47, happy_reduction_149, i)
}

fn happy_reduction_149(happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_2, happy_x_1) {
        (HappyAbsSyn::NT45(mut happy_var_2), HappyAbsSyn::NT132(mut happy_var_1)) => HappyAbsSyn::NT37({appended(lift_attrs(happy_var_1), CTypeSpec(*happy_var_2))}),
        _ => unreachable!()
    }
}


fn happy_reduce_150(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_2(p, 47, happy_reduction_150, i)
}

fn happy_reduction_150(happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_2, happy_x_1) {
        (HappyAbsSyn::NT45(mut happy_var_2), HappyAbsSyn::NT65(mut happy_var_1)) => HappyAbsSyn::NT37({appended(map(CTypeQual, happy_var_1), CTypeSpec(*happy_var_2))}),
        _ => unreachable!()
    }
}


fn happy_reduce_151(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_3(p, 47, happy_reduction_151, i)
}

fn happy_reduction_151(happy_x_3: HappyAbsSyn, happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_3, happy_x_2, happy_x_1) {
        (HappyAbsSyn::NT45(mut happy_var_3), HappyAbsSyn::NT132(mut happy_var_2), HappyAbsSyn::NT65(mut happy_var_1)) => HappyAbsSyn::NT37({appended(add_vecs(map(CTypeQual, happy_var_1), lift_attrs(happy_var_2)), CTypeSpec(*happy_var_3))}),
        _ => unreachable!()
    }
}


fn happy_reduce_152(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_2(p, 47, happy_reduction_152, i)
}

fn happy_reduction_152(happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_2, happy_x_1) {
        (HappyAbsSyn::NT64(mut happy_var_2), HappyAbsSyn::NT37(mut happy_var_1)) => HappyAbsSyn::NT37({appended(happy_var_1, CTypeQual(*happy_var_2))}),
        _ => unreachable!()
    }
}


fn happy_reduce_153(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_2(p, 47, happy_reduction_153, i)
}

fn happy_reduction_153(happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_2, happy_x_1) {
        (HappyAbsSyn::NT45(mut happy_var_2), HappyAbsSyn::NT37(mut happy_var_1)) => HappyAbsSyn::NT37({appended(happy_var_1, CTypeSpec(*happy_var_2))}),
        _ => unreachable!()
    }
}


fn happy_reduce_154(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_2(p, 47, happy_reduction_154, i)
}

fn happy_reduction_154(happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_2, happy_x_1) {
        (HappyAbsSyn::NT132(mut happy_var_2), HappyAbsSyn::NT37(mut happy_var_1)) => HappyAbsSyn::NT37({add_trailing_attrs(happy_var_1, happy_var_2)}),
        _ => unreachable!()
    }
}


fn happy_reduce_155(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_2(p, 48, happy_reduction_155, i)
}

fn happy_reduction_155(happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_2, happy_x_1) {
        (HappyAbsSyn::NT45(mut happy_var_2), HappyAbsSyn::NT37(mut happy_var_1)) => HappyAbsSyn::NT37({appended(happy_var_1, CTypeSpec(*happy_var_2))}),
        _ => unreachable!()
    }
}


fn happy_reduce_156(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_2(p, 48, happy_reduction_156, i)
}

fn happy_reduction_156(happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_2, happy_x_1) {
        (HappyAbsSyn::NT41(mut happy_var_2), HappyAbsSyn::NT37(mut happy_var_1)) => HappyAbsSyn::NT37({appended(happy_var_1, CStorageSpec(*happy_var_2))}),
        _ => unreachable!()
    }
}


fn happy_reduce_157(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_2(p, 48, happy_reduction_157, i)
}

fn happy_reduction_157(happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_2, happy_x_1) {
        (HappyAbsSyn::NT39(mut happy_var_2), HappyAbsSyn::NT37(mut happy_var_1)) => HappyAbsSyn::NT37({appended(happy_var_1, *happy_var_2)}),
        _ => unreachable!()
    }
}


fn happy_reduce_158(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_2(p, 48, happy_reduction_158, i)
}

fn happy_reduction_158(happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_2, happy_x_1) {
        (HappyAbsSyn::NT132(mut happy_var_2), HappyAbsSyn::NT37(mut happy_var_1)) => HappyAbsSyn::NT37({add_trailing_attrs(happy_var_1, happy_var_2)}),
        _ => unreachable!()
    }
}


fn happy_reduce_159(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_1(p, 49, happy_reduction_159, i)
}

fn happy_reduction_159(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn::NT45(mut happy_var_1) => HappyAbsSyn::NT37({vec![CTypeSpec(*happy_var_1)]}),
        _ => unreachable!()
    }
}


fn happy_reduce_160(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_2(p, 49, happy_reduction_160, i)
}

fn happy_reduction_160(happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_2, happy_x_1) {
        (HappyAbsSyn::NT45(mut happy_var_2), HappyAbsSyn::NT132(mut happy_var_1)) => HappyAbsSyn::NT37({appended(lift_attrs(happy_var_1), CTypeSpec(*happy_var_2))}),
        _ => unreachable!()
    }
}


fn happy_reduce_161(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_2(p, 49, happy_reduction_161, i)
}

fn happy_reduction_161(happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_2, happy_x_1) {
        (HappyAbsSyn::NT45(mut happy_var_2), HappyAbsSyn::NT65(mut happy_var_1)) => HappyAbsSyn::NT37({appended(map(CTypeQual, happy_var_1), CTypeSpec(*happy_var_2))}),
        _ => unreachable!()
    }
}


fn happy_reduce_162(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_3(p, 49, happy_reduction_162, i)
}

fn happy_reduction_162(happy_x_3: HappyAbsSyn, happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_3, happy_x_2, happy_x_1) {
        (HappyAbsSyn::NT45(mut happy_var_3), HappyAbsSyn::NT132(mut happy_var_2), HappyAbsSyn::NT65(mut happy_var_1)) => HappyAbsSyn::NT37({appended(add_vecs(map(CTypeQual, happy_var_1), lift_attrs(happy_var_2)), CTypeSpec(*happy_var_3))}),
        _ => unreachable!()
    }
}


fn happy_reduce_163(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_2(p, 49, happy_reduction_163, i)
}

fn happy_reduction_163(happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_2, happy_x_1) {
        (HappyAbsSyn::NT64(mut happy_var_2), HappyAbsSyn::NT37(mut happy_var_1)) => HappyAbsSyn::NT37({appended(happy_var_1, CTypeQual(*happy_var_2))}),
        _ => unreachable!()
    }
}


fn happy_reduce_164(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_2(p, 49, happy_reduction_164, i)
}

fn happy_reduction_164(happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_2, happy_x_1) {
        (HappyAbsSyn::NT132(mut happy_var_2), HappyAbsSyn::NT37(mut happy_var_1)) => HappyAbsSyn::NT37({add_trailing_attrs(happy_var_1, happy_var_2)}),
        _ => unreachable!()
    }
}


fn happy_reduce_165(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_2(p, 50, happy_reduction_165, i)
}

fn happy_reduction_165(happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_2, happy_x_1) {
        (HappyAbsSyn::NT41(mut happy_var_2), HappyAbsSyn::NT37(mut happy_var_1)) => HappyAbsSyn::NT37({appended(happy_var_1, CStorageSpec(*happy_var_2))}),
        _ => unreachable!()
    }
}


fn happy_reduce_166(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 2, 50, happy_reduction_166, i)
}

fn happy_reduction_166(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn::Terminal(CTokTyIdent(_, mut happy_var_2)), HappyAbsSyn::NT37(mut happy_var_1)) => { with_pos!(p, happy_var_2, |at| appended(happy_var_1, CTypeSpec(CTypeDef(happy_var_2, at))))
        }.map(HappyAbsSyn::NT37),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_167(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 5, 50, happy_reduction_167, i)
}

fn happy_reduction_167(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, HappyAbsSyn::NT100(mut happy_var_4), _, HappyAbsSyn::Terminal(mut happy_var_2), HappyAbsSyn::NT37(mut happy_var_1)) => { with_pos!(p, happy_var_2, |at| appended(happy_var_1, CTypeSpec(CTypeOfExpr(happy_var_4, at))))
        }.map(HappyAbsSyn::NT37),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_168(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 5, 50, happy_reduction_168, i)
}

fn happy_reduction_168(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, HappyAbsSyn::NT32(mut happy_var_4), _, HappyAbsSyn::Terminal(mut happy_var_2), HappyAbsSyn::NT37(mut happy_var_1)) => { with_pos!(p, happy_var_2, |at| appended(happy_var_1, CTypeSpec(CTypeOfType(happy_var_4, at))))
        }.map(HappyAbsSyn::NT37),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_169(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_2(p, 50, happy_reduction_169, i)
}

fn happy_reduction_169(happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_2, happy_x_1) {
        (HappyAbsSyn::NT39(mut happy_var_2), HappyAbsSyn::NT37(mut happy_var_1)) => HappyAbsSyn::NT37({appended(happy_var_1, *happy_var_2)}),
        _ => unreachable!()
    }
}


fn happy_reduce_170(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_2(p, 50, happy_reduction_170, i)
}

fn happy_reduction_170(happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_2, happy_x_1) {
        (HappyAbsSyn::NT132(mut happy_var_2), HappyAbsSyn::NT37(mut happy_var_1)) => HappyAbsSyn::NT37({add_trailing_attrs(happy_var_1, happy_var_2)}),
        _ => unreachable!()
    }
}


fn happy_reduce_171(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 1, 51, happy_reduction_171, i)
}

fn happy_reduction_171(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap()) {
        HappyAbsSyn::Terminal(CTokTyIdent(_, mut happy_var_1)) => { with_pos!(p, happy_var_1, |at| vec![CTypeSpec(CTypeDef(happy_var_1, at))])
        }.map(HappyAbsSyn::NT37),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_172(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 4, 51, happy_reduction_172, i)
}

fn happy_reduction_172(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, HappyAbsSyn::NT100(mut happy_var_3), _, HappyAbsSyn::Terminal(mut happy_var_1)) => { with_pos!(p, happy_var_1, |at| vec![CTypeSpec(CTypeOfExpr(happy_var_3, at))])
        }.map(HappyAbsSyn::NT37),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_173(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 4, 51, happy_reduction_173, i)
}

fn happy_reduction_173(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, HappyAbsSyn::NT32(mut happy_var_3), _, HappyAbsSyn::Terminal(mut happy_var_1)) => { with_pos!(p, happy_var_1, |at| vec![CTypeSpec(CTypeOfType(happy_var_3, at))])
        }.map(HappyAbsSyn::NT37),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_174(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 2, 51, happy_reduction_174, i)
}

fn happy_reduction_174(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn::Terminal(CTokTyIdent(_, mut happy_var_2)), HappyAbsSyn::NT65(mut happy_var_1)) => { with_pos!(p, happy_var_2, |at| appended(map(CTypeQual, happy_var_1), CTypeSpec(CTypeDef(happy_var_2, at))))
        }.map(HappyAbsSyn::NT37),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_175(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 5, 51, happy_reduction_175, i)
}

fn happy_reduction_175(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, HappyAbsSyn::NT100(mut happy_var_4), _, HappyAbsSyn::Terminal(mut happy_var_2), HappyAbsSyn::NT65(mut happy_var_1)) => { with_pos!(p, happy_var_2, |at| appended(map(CTypeQual, happy_var_1), CTypeSpec(CTypeOfExpr(happy_var_4, at))))
        }.map(HappyAbsSyn::NT37),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_176(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 5, 51, happy_reduction_176, i)
}

fn happy_reduction_176(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, HappyAbsSyn::NT32(mut happy_var_4), _, HappyAbsSyn::Terminal(mut happy_var_2), HappyAbsSyn::NT65(mut happy_var_1)) => { with_pos!(p, happy_var_2, |at| appended(map(CTypeQual, happy_var_1), CTypeSpec(CTypeOfType(happy_var_4, at))))
        }.map(HappyAbsSyn::NT37),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_177(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 2, 51, happy_reduction_177, i)
}

fn happy_reduction_177(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn::Terminal(CTokTyIdent(_, mut happy_var_2)), HappyAbsSyn::NT132(mut happy_var_1)) => { with_pos!(p, happy_var_2, |at| appended(lift_attrs(happy_var_1), CTypeSpec(CTypeDef(happy_var_2, at))))
        }.map(HappyAbsSyn::NT37),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_178(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 5, 51, happy_reduction_178, i)
}

fn happy_reduction_178(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, HappyAbsSyn::NT100(mut happy_var_4), _, _, HappyAbsSyn::NT132(mut happy_var_1)) => { with_pos!(p, happy_var_1, |at| appended(lift_attrs(happy_var_1), CTypeSpec(CTypeOfExpr(happy_var_4, at))))
        }.map(HappyAbsSyn::NT37),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_179(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 5, 51, happy_reduction_179, i)
}

fn happy_reduction_179(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, HappyAbsSyn::NT32(mut happy_var_4), _, HappyAbsSyn::Terminal(mut happy_var_2), HappyAbsSyn::NT132(mut happy_var_1)) => { with_pos!(p, happy_var_2, |at| appended(lift_attrs(happy_var_1), CTypeSpec(CTypeOfType(happy_var_4, at))))
        }.map(HappyAbsSyn::NT37),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_180(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 3, 51, happy_reduction_180, i)
}

fn happy_reduction_180(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn::Terminal(CTokTyIdent(_, mut happy_var_3)), HappyAbsSyn::NT132(mut happy_var_2), HappyAbsSyn::NT65(mut happy_var_1)) => { with_pos!(p, happy_var_3, |at| appended(add_vecs(map(CTypeQual, happy_var_1), lift_attrs(happy_var_2)),
                                          CTypeSpec(CTypeDef(happy_var_3, at))))
        }.map(HappyAbsSyn::NT37),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_181(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 6, 51, happy_reduction_181, i)
}

fn happy_reduction_181(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, HappyAbsSyn::NT100(mut happy_var_5), _, HappyAbsSyn::Terminal(mut happy_var_3), HappyAbsSyn::NT132(mut happy_var_2), HappyAbsSyn::NT65(mut happy_var_1)) => { with_pos!(p, happy_var_3, |at| appended(add_vecs(map(CTypeQual, happy_var_1), lift_attrs(happy_var_2)),
                                          CTypeSpec(CTypeOfExpr(happy_var_5, at))))
        }.map(HappyAbsSyn::NT37),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_182(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 6, 51, happy_reduction_182, i)
}

fn happy_reduction_182(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, HappyAbsSyn::NT32(mut happy_var_5), _, HappyAbsSyn::Terminal(mut happy_var_3), HappyAbsSyn::NT132(mut happy_var_2), HappyAbsSyn::NT65(mut happy_var_1)) => { with_pos!(p, happy_var_3, |at| appended(add_vecs(map(CTypeQual, happy_var_1), lift_attrs(happy_var_2)),
                                          CTypeSpec(CTypeOfType(happy_var_5, at))))
        }.map(HappyAbsSyn::NT37),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_183(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_2(p, 51, happy_reduction_183, i)
}

fn happy_reduction_183(happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_2, happy_x_1) {
        (HappyAbsSyn::NT64(mut happy_var_2), HappyAbsSyn::NT37(mut happy_var_1)) => HappyAbsSyn::NT37({appended(happy_var_1, CTypeQual(*happy_var_2))}),
        _ => unreachable!()
    }
}


fn happy_reduce_184(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_2(p, 51, happy_reduction_184, i)
}

fn happy_reduction_184(happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_2, happy_x_1) {
        (HappyAbsSyn::NT132(mut happy_var_2), HappyAbsSyn::NT37(mut happy_var_1)) => HappyAbsSyn::NT37({add_trailing_attrs(happy_var_1, happy_var_2)}),
        _ => unreachable!()
    }
}


fn happy_reduce_185(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 1, 52, happy_reduction_185, i)
}

fn happy_reduction_185(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap()) {
        HappyAbsSyn::NT53(mut happy_var_1) => { with_pos!(p, happy_var_1, |at| box CSUType(happy_var_1, at))
        }.map(HappyAbsSyn::NT45),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_186(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 1, 52, happy_reduction_186, i)
}

fn happy_reduction_186(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap()) {
        HappyAbsSyn::NT61(mut happy_var_1) => { with_pos!(p, happy_var_1, |at| box CEnumType(happy_var_1, at))
        }.map(HappyAbsSyn::NT45),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_187(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 6, 53, happy_reduction_187, i)
}

fn happy_reduction_187(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, HappyAbsSyn::NT33(mut happy_var_5), _, HappyAbsSyn::NT131(mut happy_var_3), HappyAbsSyn::NT132(mut happy_var_2), HappyAbsSyn::NT54(mut happy_var_1)) => { with_pos!(p, happy_var_1, |at| box CStructureUnion(happy_var_1.into_inner(), Some(happy_var_3), Some(happy_var_5), happy_var_2, at))
        }.map(HappyAbsSyn::NT53),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_188(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 5, 53, happy_reduction_188, i)
}

fn happy_reduction_188(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, HappyAbsSyn::NT33(mut happy_var_4), _, HappyAbsSyn::NT132(mut happy_var_2), HappyAbsSyn::NT54(mut happy_var_1)) => { with_pos!(p, happy_var_1, |at| box CStructureUnion(happy_var_1.into_inner(), None,     Some(happy_var_4), happy_var_2, at))
        }.map(HappyAbsSyn::NT53),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_189(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 3, 53, happy_reduction_189, i)
}

fn happy_reduction_189(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn::NT131(mut happy_var_3), HappyAbsSyn::NT132(mut happy_var_2), HappyAbsSyn::NT54(mut happy_var_1)) => { with_pos!(p, happy_var_1, |at| box CStructureUnion(happy_var_1.into_inner(), Some(happy_var_3), None,     happy_var_2, at))
        }.map(HappyAbsSyn::NT53),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_190(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_1(p, 54, happy_reduction_190, i)
}

fn happy_reduction_190(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn::Terminal(mut happy_var_1) => HappyAbsSyn::NT54({Located::new(CStructTag, happy_var_1)}),
        _ => unreachable!()
    }
}


fn happy_reduce_191(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_1(p, 54, happy_reduction_191, i)
}

fn happy_reduction_191(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn::Terminal(mut happy_var_1) => HappyAbsSyn::NT54({Located::new(CUnionTag, happy_var_1)}),
        _ => unreachable!()
    }
}


fn happy_reduce_192(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_0(p, 55, happy_reduction_192(), i)
}

fn happy_reduction_192() -> HappyAbsSyn {
    HappyAbsSyn::NT33(vec![])
}


fn happy_reduce_193(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_2(p, 55, happy_reduction_193, i)
}

fn happy_reduction_193(happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_2, happy_x_1) {
        (_, HappyAbsSyn::NT33(mut happy_var_1)) => HappyAbsSyn::NT33({happy_var_1}),
        _ => unreachable!()
    }
}


fn happy_reduce_194(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_2(p, 55, happy_reduction_194, i)
}

fn happy_reduction_194(happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_2, happy_x_1) {
        (HappyAbsSyn::NT32(mut happy_var_2), HappyAbsSyn::NT33(mut happy_var_1)) => HappyAbsSyn::NT33({appended(happy_var_1, *happy_var_2)}),
        _ => unreachable!()
    }
}


fn happy_reduce_195(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_2(p, 56, happy_reduction_195, i)
}

fn happy_reduction_195(happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_2, happy_x_1) {
        (_, HappyAbsSyn::NT32(mut happy_var_1)) => HappyAbsSyn::NT32({unwrap_let! { CDecl(declspecs, dies, at) = { *happy_var_1 };
                box CDecl(declspecs, rev_vec(dies), at)
            }}),
        _ => unreachable!()
    }
}


fn happy_reduce_196(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_2(p, 56, happy_reduction_196, i)
}

fn happy_reduction_196(happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_2, happy_x_1) {
        (_, HappyAbsSyn::NT32(mut happy_var_1)) => HappyAbsSyn::NT32({unwrap_let! { CDecl(declspecs, dies, at) = { *happy_var_1 };
                box CDecl(declspecs, rev_vec(dies), at)
            }}),
        _ => unreachable!()
    }
}


fn happy_reduce_197(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_2(p, 56, happy_reduction_197, i)
}

fn happy_reduction_197(happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_2, happy_x_1) {
        (HappyAbsSyn::NT32(mut happy_var_2), _) => HappyAbsSyn::NT32({happy_var_2}),
        _ => unreachable!()
    }
}


fn happy_reduce_198(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 3, 57, happy_reduction_198, i)
}

fn happy_reduction_198(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn::NT59(mut happy_var_3), HappyAbsSyn::NT132(mut happy_var_2), HappyAbsSyn::NT65(mut happy_var_1)) => {
            with_pos!(p, happy_var_1, match happy_var_3 {
                (d, s) => |at| box CDecl(add_vecs(lift_type_quals(happy_var_1), lift_attrs(happy_var_2)), vec![(d, None, s)], at)
            })
        }.map(HappyAbsSyn::NT32),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_199(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 2, 57, happy_reduction_199, i)
}

fn happy_reduction_199(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn::NT59(mut happy_var_2), HappyAbsSyn::NT132(mut happy_var_1)) => {
            with_pos!(p, happy_var_1, match happy_var_2 {
                (d, s) => |at| box CDecl(lift_attrs(happy_var_1), vec![(d, None, s)], at),
            })
        }.map(HappyAbsSyn::NT32),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_200(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_reduce(p, 4, 57, happy_reduction_200, i)
}

fn happy_reduction_200(p: &mut Parser) {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn::NT59(mut happy_var_4), HappyAbsSyn::NT132(mut happy_var_3), _, HappyAbsSyn::NT32(mut happy_var_1)) => {            p.stack.push(HappyAbsSyn::NT32({unwrap_let! { box CDecl(_, ref mut dies, _) = happy_var_1;
                match happy_var_4 {
                    (Some(d), s) => dies.insert(0, (Some(append_obj_attrs(happy_var_3, d)), None, s)),
                    (None, s)    => dies.insert(0, (None, None, s)),
                }
            }
            happy_var_1})); }
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_201(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 3, 58, happy_reduction_201, i)
}

fn happy_reduction_201(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn::NT132(mut happy_var_3), HappyAbsSyn::NT59(mut happy_var_2), HappyAbsSyn::NT37(mut happy_var_1)) => {
            with_pos!(p, happy_var_1, move |at| box match happy_var_2 {
                (Some(d), s) => {
                    CDecl(happy_var_1, vec![(Some(append_obj_attrs(happy_var_3, d)), None, s)], at)
                },
                (None, s) => {
                    CDecl(happy_var_1, vec![(None, None, s)], at)
                },
            })
        }.map(HappyAbsSyn::NT32),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_202(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_reduce(p, 5, 58, happy_reduction_202, i)
}

fn happy_reduction_202(p: &mut Parser) {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn::NT132(mut happy_var_5), HappyAbsSyn::NT59(mut happy_var_4), HappyAbsSyn::NT132(mut happy_var_3), _, HappyAbsSyn::NT32(mut happy_var_1)) => {            p.stack.push(HappyAbsSyn::NT32({unwrap_let! { box CDecl(_, ref mut dies, _) = happy_var_1;
                match happy_var_4 {
                    (Some(d), s) => dies.insert(0, (Some(append_obj_attrs(add_vecs(happy_var_3, happy_var_5), d)), None, s)),
                    (None, s)    => dies.insert(0, (None, None, s)),
                }
            }
            happy_var_1})); }
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_203(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 1, 58, happy_reduction_203, i)
}

fn happy_reduction_203(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap()) {
        HappyAbsSyn::NT37(mut happy_var_1) => { with_pos!(p, happy_var_1, |at| box CDecl(happy_var_1, vec![], at))
        }.map(HappyAbsSyn::NT32),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_204(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_1(p, 59, happy_reduction_204, i)
}

fn happy_reduction_204(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn::NT66(mut happy_var_1) => HappyAbsSyn::NT59({(Some(happy_var_1.reverse()), None)}),
        _ => unreachable!()
    }
}


fn happy_reduce_205(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_2(p, 59, happy_reduction_205, i)
}

fn happy_reduction_205(happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_2, happy_x_1) {
        (HappyAbsSyn::NT100(mut happy_var_2), _) => HappyAbsSyn::NT59({(None, Some(happy_var_2))}),
        _ => unreachable!()
    }
}


fn happy_reduce_206(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_3(p, 59, happy_reduction_206, i)
}

fn happy_reduction_206(happy_x_3: HappyAbsSyn, happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_3, happy_x_2, happy_x_1) {
        (HappyAbsSyn::NT100(mut happy_var_3), _, HappyAbsSyn::NT66(mut happy_var_1)) => HappyAbsSyn::NT59({(Some(happy_var_1.reverse()), Some(happy_var_3))}),
        _ => unreachable!()
    }
}


fn happy_reduce_207(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_1(p, 60, happy_reduction_207, i)
}

fn happy_reduction_207(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn::NT66(mut happy_var_1) => HappyAbsSyn::NT59({(Some(happy_var_1.reverse()), None)}),
        _ => unreachable!()
    }
}


fn happy_reduce_208(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_2(p, 60, happy_reduction_208, i)
}

fn happy_reduction_208(happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_2, happy_x_1) {
        (HappyAbsSyn::NT100(mut happy_var_2), _) => HappyAbsSyn::NT59({(None, Some(happy_var_2))}),
        _ => unreachable!()
    }
}


fn happy_reduce_209(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_3(p, 60, happy_reduction_209, i)
}

fn happy_reduction_209(happy_x_3: HappyAbsSyn, happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_3, happy_x_2, happy_x_1) {
        (HappyAbsSyn::NT100(mut happy_var_3), _, HappyAbsSyn::NT66(mut happy_var_1)) => HappyAbsSyn::NT59({(Some(happy_var_1.reverse()), Some(happy_var_3))}),
        _ => unreachable!()
    }
}


fn happy_reduce_210(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_2(p, 60, happy_reduction_210, i)
}

fn happy_reduction_210(happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_2, happy_x_1) {
        (HappyAbsSyn::NT132(mut happy_var_2), HappyAbsSyn::NT59(mut happy_var_1)) => HappyAbsSyn::NT59({if let (Some(ref mut decl), _) = happy_var_1 { decl.3.extend(happy_var_2); }
            happy_var_1}),
        _ => unreachable!()
    }
}


fn happy_reduce_211(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 5, 61, happy_reduction_211, i)
}

fn happy_reduction_211(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, HappyAbsSyn::NT62(mut happy_var_4), _, HappyAbsSyn::NT132(mut happy_var_2), HappyAbsSyn::Terminal(mut happy_var_1)) => { with_pos!(p, happy_var_1, |at| box CEnumeration(None,     Some(happy_var_4), happy_var_2, at))
        }.map(HappyAbsSyn::NT61),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_212(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 6, 61, happy_reduction_212, i)
}

fn happy_reduction_212(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, _, HappyAbsSyn::NT62(mut happy_var_4), _, HappyAbsSyn::NT132(mut happy_var_2), HappyAbsSyn::Terminal(mut happy_var_1)) => { with_pos!(p, happy_var_1, |at| box CEnumeration(None,     Some(happy_var_4), happy_var_2, at))
        }.map(HappyAbsSyn::NT61),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_213(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 6, 61, happy_reduction_213, i)
}

fn happy_reduction_213(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, HappyAbsSyn::NT62(mut happy_var_5), _, HappyAbsSyn::NT131(mut happy_var_3), HappyAbsSyn::NT132(mut happy_var_2), HappyAbsSyn::Terminal(mut happy_var_1)) => { with_pos!(p, happy_var_1, |at| box CEnumeration(Some(happy_var_3), Some(happy_var_5), happy_var_2, at))
        }.map(HappyAbsSyn::NT61),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_214(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 7, 61, happy_reduction_214, i)
}

fn happy_reduction_214(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, _, HappyAbsSyn::NT62(mut happy_var_5), _, HappyAbsSyn::NT131(mut happy_var_3), HappyAbsSyn::NT132(mut happy_var_2), HappyAbsSyn::Terminal(mut happy_var_1)) => { with_pos!(p, happy_var_1, |at| box CEnumeration(Some(happy_var_3), Some(happy_var_5), happy_var_2, at))
        }.map(HappyAbsSyn::NT61),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_215(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 3, 61, happy_reduction_215, i)
}

fn happy_reduction_215(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn::NT131(mut happy_var_3), HappyAbsSyn::NT132(mut happy_var_2), HappyAbsSyn::Terminal(mut happy_var_1)) => { with_pos!(p, happy_var_1, |at| box CEnumeration(Some(happy_var_3), None, happy_var_2, at))
        }.map(HappyAbsSyn::NT61),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_216(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_1(p, 62, happy_reduction_216, i)
}

fn happy_reduction_216(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn::NT63(mut happy_var_1) => HappyAbsSyn::NT62({vec![happy_var_1]}),
        _ => unreachable!()
    }
}


fn happy_reduce_217(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_3(p, 62, happy_reduction_217, i)
}

fn happy_reduction_217(happy_x_3: HappyAbsSyn, happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_3, happy_x_2, happy_x_1) {
        (HappyAbsSyn::NT63(mut happy_var_3), _, HappyAbsSyn::NT62(mut happy_var_1)) => HappyAbsSyn::NT62({appended(happy_var_1, happy_var_3)}),
        _ => unreachable!()
    }
}


fn happy_reduce_218(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_1(p, 63, happy_reduction_218, i)
}

fn happy_reduction_218(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn::NT131(mut happy_var_1) => HappyAbsSyn::NT63({(happy_var_1, None)}),
        _ => unreachable!()
    }
}


fn happy_reduce_219(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_2(p, 63, happy_reduction_219, i)
}

fn happy_reduction_219(happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_2, happy_x_1) {
        (_, HappyAbsSyn::NT131(mut happy_var_1)) => HappyAbsSyn::NT63({(happy_var_1, None)}),
        _ => unreachable!()
    }
}


fn happy_reduce_220(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_reduce(p, 4, 63, happy_reduction_220, i)
}

fn happy_reduction_220(p: &mut Parser) {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn::NT100(mut happy_var_4), _, _, HappyAbsSyn::NT131(mut happy_var_1)) => {            p.stack.push(HappyAbsSyn::NT63({(happy_var_1, Some(happy_var_4))})); }
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_221(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_3(p, 63, happy_reduction_221, i)
}

fn happy_reduction_221(happy_x_3: HappyAbsSyn, happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_3, happy_x_2, happy_x_1) {
        (HappyAbsSyn::NT100(mut happy_var_3), _, HappyAbsSyn::NT131(mut happy_var_1)) => HappyAbsSyn::NT63({(happy_var_1, Some(happy_var_3))}),
        _ => unreachable!()
    }
}


fn happy_reduce_222(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 1, 64, happy_reduction_222, i)
}

fn happy_reduction_222(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap()) {
        HappyAbsSyn::Terminal(mut happy_var_1) => { with_pos!(p, happy_var_1, |at| box CConstQual(at))
        }.map(HappyAbsSyn::NT64),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_223(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 1, 64, happy_reduction_223, i)
}

fn happy_reduction_223(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap()) {
        HappyAbsSyn::Terminal(mut happy_var_1) => { with_pos!(p, happy_var_1, |at| box CVolatQual(at))
        }.map(HappyAbsSyn::NT64),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_224(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 1, 64, happy_reduction_224, i)
}

fn happy_reduction_224(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap()) {
        HappyAbsSyn::Terminal(mut happy_var_1) => { with_pos!(p, happy_var_1, |at| box CRestrQual(at))
        }.map(HappyAbsSyn::NT64),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_225(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 1, 64, happy_reduction_225, i)
}

fn happy_reduction_225(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap()) {
        HappyAbsSyn::Terminal(mut happy_var_1) => { with_pos!(p, happy_var_1, |at| box CNullableQual(at))
        }.map(HappyAbsSyn::NT64),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_226(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 1, 64, happy_reduction_226, i)
}

fn happy_reduction_226(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap()) {
        HappyAbsSyn::Terminal(mut happy_var_1) => { with_pos!(p, happy_var_1, |at| box CNonnullQual(at))
        }.map(HappyAbsSyn::NT64),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_227(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 1, 64, happy_reduction_227, i)
}

fn happy_reduction_227(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap()) {
        HappyAbsSyn::Terminal(mut happy_var_1) => { with_pos!(p, happy_var_1, |at| box CAtomicQual(at))
        }.map(HappyAbsSyn::NT64),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_228(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_2(p, 65, happy_reduction_228, i)
}

fn happy_reduction_228(happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_2, happy_x_1) {
        (HappyAbsSyn::NT64(mut happy_var_2), HappyAbsSyn::NT132(mut happy_var_1)) => HappyAbsSyn::NT65({appended(map(|q| CAttrQual(box q), happy_var_1), *happy_var_2)}),
        _ => unreachable!()
    }
}


fn happy_reduce_229(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_2(p, 65, happy_reduction_229, i)
}

fn happy_reduction_229(happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_2, happy_x_1) {
        (HappyAbsSyn::NT64(mut happy_var_2), HappyAbsSyn::NT65(mut happy_var_1)) => HappyAbsSyn::NT65({appended(happy_var_1, *happy_var_2)}),
        _ => unreachable!()
    }
}


fn happy_reduce_230(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_3(p, 65, happy_reduction_230, i)
}

fn happy_reduction_230(happy_x_3: HappyAbsSyn, happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_3, happy_x_2, happy_x_1) {
        (HappyAbsSyn::NT64(mut happy_var_3), HappyAbsSyn::NT132(mut happy_var_2), HappyAbsSyn::NT65(mut happy_var_1)) => HappyAbsSyn::NT65({appended(add_vecs(happy_var_1, map(|q| CAttrQual(box q), happy_var_2)), *happy_var_3)}),
        _ => unreachable!()
    }
}


fn happy_reduce_231(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_1(p, 66, happy_reduction_231, i)
}

fn happy_reduction_231(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn::NT66(mut happy_var_1) => HappyAbsSyn::NT66({happy_var_1}),
        _ => unreachable!()
    }
}


fn happy_reduce_232(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_1(p, 66, happy_reduction_232, i)
}

fn happy_reduction_232(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn::NT66(mut happy_var_1) => HappyAbsSyn::NT66({happy_var_1}),
        _ => unreachable!()
    }
}


fn happy_reduce_233(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_0(p, 67, happy_reduction_233(), i)
}

fn happy_reduction_233() -> HappyAbsSyn {
    HappyAbsSyn::NT67(None)
}


fn happy_reduce_234(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_reduce(p, 4, 67, happy_reduction_234, i)
}

fn happy_reduction_234(p: &mut Parser) {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, HappyAbsSyn::NT128(mut happy_var_3), _, _) => {            p.stack.push(HappyAbsSyn::NT67({Some(happy_var_3)})); }
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_235(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_1(p, 68, happy_reduction_235, i)
}

fn happy_reduction_235(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn::NT66(mut happy_var_1) => HappyAbsSyn::NT66({happy_var_1}),
        _ => unreachable!()
    }
}


fn happy_reduce_236(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_1(p, 68, happy_reduction_236, i)
}

fn happy_reduction_236(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn::NT66(mut happy_var_1) => HappyAbsSyn::NT66({happy_var_1}),
        _ => unreachable!()
    }
}


fn happy_reduce_237(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 1, 69, happy_reduction_237, i)
}

fn happy_reduction_237(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap()) {
        HappyAbsSyn::Terminal(CTokTyIdent(_, mut happy_var_1)) => { with_pos!(p, happy_var_1, |at| CDeclrR::from_var(happy_var_1, at))
        }.map(HappyAbsSyn::NT66),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_238(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 2, 69, happy_reduction_238, i)
}

fn happy_reduction_238(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn::NT88(mut happy_var_2), HappyAbsSyn::Terminal(CTokTyIdent(_, mut happy_var_1))) => { with_pos!(p, happy_var_1, |at| { happy_var_2(CDeclrR::from_var(happy_var_1, at)) })
        }.map(HappyAbsSyn::NT66),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_239(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_1(p, 69, happy_reduction_239, i)
}

fn happy_reduction_239(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn::NT66(mut happy_var_1) => HappyAbsSyn::NT66({happy_var_1}),
        _ => unreachable!()
    }
}


fn happy_reduce_240(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_1(p, 70, happy_reduction_240, i)
}

fn happy_reduction_240(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn::NT66(mut happy_var_1) => HappyAbsSyn::NT66({happy_var_1}),
        _ => unreachable!()
    }
}


fn happy_reduce_241(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 2, 70, happy_reduction_241, i)
}

fn happy_reduction_241(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn::NT66(mut happy_var_2), HappyAbsSyn::Terminal(mut happy_var_1)) => { with_pos!(p, happy_var_1, |at| happy_var_2.ptr_declr(vec![], at))
        }.map(HappyAbsSyn::NT66),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_242(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 3, 70, happy_reduction_242, i)
}

fn happy_reduction_242(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn::NT66(mut happy_var_3), HappyAbsSyn::NT132(mut happy_var_2), HappyAbsSyn::Terminal(mut happy_var_1)) => { p.with_attribute(happy_var_1, happy_var_2, |at| happy_var_3.ptr_declr(vec![], at))
        }.map(HappyAbsSyn::NT66),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_243(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 3, 70, happy_reduction_243, i)
}

fn happy_reduction_243(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn::NT66(mut happy_var_3), HappyAbsSyn::NT65(mut happy_var_2), HappyAbsSyn::Terminal(mut happy_var_1)) => { with_pos!(p, happy_var_1, |at| happy_var_3.ptr_declr(happy_var_2, at))
        }.map(HappyAbsSyn::NT66),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_244(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 4, 70, happy_reduction_244, i)
}

fn happy_reduction_244(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn::NT66(mut happy_var_4), HappyAbsSyn::NT132(mut happy_var_3), HappyAbsSyn::NT65(mut happy_var_2), HappyAbsSyn::Terminal(mut happy_var_1)) => { p.with_attribute(happy_var_1, happy_var_3, |at| happy_var_4.ptr_declr(happy_var_2, at))
        }.map(HappyAbsSyn::NT66),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_245(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_3(p, 71, happy_reduction_245, i)
}

fn happy_reduction_245(happy_x_3: HappyAbsSyn, happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_3, happy_x_2, happy_x_1) {
        (_, HappyAbsSyn::NT66(mut happy_var_2), _) => HappyAbsSyn::NT66({happy_var_2}),
        _ => unreachable!()
    }
}


fn happy_reduce_246(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_reduce(p, 4, 71, happy_reduction_246, i)
}

fn happy_reduction_246(p: &mut Parser) {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn::NT88(mut happy_var_4), _, HappyAbsSyn::NT66(mut happy_var_2), _) => {            p.stack.push(HappyAbsSyn::NT66({happy_var_4(happy_var_2)})); }
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_247(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_reduce(p, 4, 71, happy_reduction_247, i)
}

fn happy_reduction_247(p: &mut Parser) {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, HappyAbsSyn::NT66(mut happy_var_3), HappyAbsSyn::NT132(mut happy_var_2), _) => {            p.stack.push(HappyAbsSyn::NT66({happy_var_3.append_attrs(happy_var_2)})); }
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_248(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_reduce(p, 5, 71, happy_reduction_248, i)
}

fn happy_reduction_248(p: &mut Parser) {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn::NT88(mut happy_var_5), _, HappyAbsSyn::NT66(mut happy_var_3), HappyAbsSyn::NT132(mut happy_var_2), _) => {            p.stack.push(HappyAbsSyn::NT66({happy_var_5(happy_var_3).append_attrs(happy_var_2)})); }
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_249(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_1(p, 72, happy_reduction_249, i)
}

fn happy_reduction_249(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn::NT66(mut happy_var_1) => HappyAbsSyn::NT66({happy_var_1}),
        _ => unreachable!()
    }
}


fn happy_reduce_250(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 4, 72, happy_reduction_250, i)
}

fn happy_reduction_250(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, HappyAbsSyn::NT66(mut happy_var_3), _, HappyAbsSyn::Terminal(mut happy_var_1)) => { with_pos!(p, happy_var_1, |at| happy_var_3.ptr_declr(vec![], at))
        }.map(HappyAbsSyn::NT66),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_251(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 5, 72, happy_reduction_251, i)
}

fn happy_reduction_251(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, HappyAbsSyn::NT66(mut happy_var_4), _, HappyAbsSyn::NT65(mut happy_var_2), HappyAbsSyn::Terminal(mut happy_var_1)) => { with_pos!(p, happy_var_1, |at| happy_var_4.ptr_declr(happy_var_2, at))
        }.map(HappyAbsSyn::NT66),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_252(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 6, 72, happy_reduction_252, i)
}

fn happy_reduction_252(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, HappyAbsSyn::NT66(mut happy_var_5), _, HappyAbsSyn::NT132(mut happy_var_3), HappyAbsSyn::NT65(mut happy_var_2), HappyAbsSyn::Terminal(mut happy_var_1)) => { p.with_attribute(happy_var_1, happy_var_3, |at| happy_var_5.ptr_declr(happy_var_2, at))
        }.map(HappyAbsSyn::NT66),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_253(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 2, 72, happy_reduction_253, i)
}

fn happy_reduction_253(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn::NT66(mut happy_var_2), HappyAbsSyn::Terminal(mut happy_var_1)) => { with_pos!(p, happy_var_1, |at| happy_var_2.ptr_declr(vec![], at))
        }.map(HappyAbsSyn::NT66),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_254(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 3, 72, happy_reduction_254, i)
}

fn happy_reduction_254(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn::NT66(mut happy_var_3), HappyAbsSyn::NT65(mut happy_var_2), HappyAbsSyn::Terminal(mut happy_var_1)) => { with_pos!(p, happy_var_1, |at| happy_var_3.ptr_declr(happy_var_2, at))
        }.map(HappyAbsSyn::NT66),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_255(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 4, 72, happy_reduction_255, i)
}

fn happy_reduction_255(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn::NT66(mut happy_var_4), HappyAbsSyn::NT132(mut happy_var_3), HappyAbsSyn::NT65(mut happy_var_2), HappyAbsSyn::Terminal(mut happy_var_1)) => { p.with_attribute(happy_var_1, happy_var_3, |at| happy_var_4.ptr_declr(happy_var_2, at))
        }.map(HappyAbsSyn::NT66),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_256(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_3(p, 73, happy_reduction_256, i)
}

fn happy_reduction_256(happy_x_3: HappyAbsSyn, happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_3, happy_x_2, happy_x_1) {
        (_, HappyAbsSyn::NT66(mut happy_var_2), _) => HappyAbsSyn::NT66({happy_var_2}),
        _ => unreachable!()
    }
}


fn happy_reduce_257(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_reduce(p, 4, 73, happy_reduction_257, i)
}

fn happy_reduction_257(p: &mut Parser) {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, HappyAbsSyn::NT88(mut happy_var_3), HappyAbsSyn::NT66(mut happy_var_2), _) => {            p.stack.push(HappyAbsSyn::NT66({happy_var_3(happy_var_2)})); }
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_258(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_reduce(p, 4, 73, happy_reduction_258, i)
}

fn happy_reduction_258(p: &mut Parser) {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn::NT88(mut happy_var_4), _, HappyAbsSyn::NT66(mut happy_var_2), _) => {            p.stack.push(HappyAbsSyn::NT66({happy_var_4(happy_var_2)})); }
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_259(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 1, 74, happy_reduction_259, i)
}

fn happy_reduction_259(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap()) {
        HappyAbsSyn::Terminal(CTokTyIdent(_, mut happy_var_1)) => { with_pos!(p, &happy_var_1, |at| CDeclrR::from_var(happy_var_1, at))
        }.map(HappyAbsSyn::NT66),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_260(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_3(p, 74, happy_reduction_260, i)
}

fn happy_reduction_260(happy_x_3: HappyAbsSyn, happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_3, happy_x_2, happy_x_1) {
        (_, HappyAbsSyn::NT66(mut happy_var_2), _) => HappyAbsSyn::NT66({happy_var_2}),
        _ => unreachable!()
    }
}


fn happy_reduce_261(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_1(p, 75, happy_reduction_261, i)
}

fn happy_reduction_261(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn::NT66(mut happy_var_1) => HappyAbsSyn::NT66({happy_var_1}),
        _ => unreachable!()
    }
}


fn happy_reduce_262(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_1(p, 75, happy_reduction_262, i)
}

fn happy_reduction_262(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn::NT66(mut happy_var_1) => HappyAbsSyn::NT66({happy_var_1}),
        _ => unreachable!()
    }
}


fn happy_reduce_263(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_1(p, 76, happy_reduction_263, i)
}

fn happy_reduction_263(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn::NT66(mut happy_var_1) => HappyAbsSyn::NT66({happy_var_1}),
        _ => unreachable!()
    }
}


fn happy_reduce_264(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 2, 76, happy_reduction_264, i)
}

fn happy_reduction_264(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn::NT66(mut happy_var_2), HappyAbsSyn::Terminal(mut happy_var_1)) => { with_pos!(p, happy_var_1, |at| happy_var_2.ptr_declr(vec![], at))
        }.map(HappyAbsSyn::NT66),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_265(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 3, 76, happy_reduction_265, i)
}

fn happy_reduction_265(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn::NT66(mut happy_var_3), HappyAbsSyn::NT132(mut happy_var_2), HappyAbsSyn::Terminal(mut happy_var_1)) => { p.with_attribute(happy_var_1, happy_var_2, |at| happy_var_3.ptr_declr(vec![], at))
        }.map(HappyAbsSyn::NT66),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_266(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 3, 76, happy_reduction_266, i)
}

fn happy_reduction_266(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn::NT66(mut happy_var_3), HappyAbsSyn::NT65(mut happy_var_2), HappyAbsSyn::Terminal(mut happy_var_1)) => { with_pos!(p, happy_var_1, |at| happy_var_3.ptr_declr(happy_var_2, at))
        }.map(HappyAbsSyn::NT66),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_267(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 4, 76, happy_reduction_267, i)
}

fn happy_reduction_267(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn::NT66(mut happy_var_4), HappyAbsSyn::NT132(mut happy_var_3), HappyAbsSyn::NT65(mut happy_var_2), HappyAbsSyn::Terminal(mut happy_var_1)) => { p.with_attribute(happy_var_1, happy_var_3, |at| happy_var_4.ptr_declr(happy_var_2, at))
        }.map(HappyAbsSyn::NT66),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_268(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_2(p, 77, happy_reduction_268, i)
}

fn happy_reduction_268(happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_2, happy_x_1) {
        (HappyAbsSyn::NT88(mut happy_var_2), HappyAbsSyn::NT66(mut happy_var_1)) => HappyAbsSyn::NT66({happy_var_2(happy_var_1)}),
        _ => unreachable!()
    }
}


fn happy_reduce_269(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_3(p, 77, happy_reduction_269, i)
}

fn happy_reduction_269(happy_x_3: HappyAbsSyn, happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_3, happy_x_2, happy_x_1) {
        (_, HappyAbsSyn::NT66(mut happy_var_2), _) => HappyAbsSyn::NT66({happy_var_2}),
        _ => unreachable!()
    }
}


fn happy_reduce_270(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_reduce(p, 4, 77, happy_reduction_270, i)
}

fn happy_reduction_270(p: &mut Parser) {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn::NT88(mut happy_var_4), _, HappyAbsSyn::NT66(mut happy_var_2), _) => {            p.stack.push(HappyAbsSyn::NT66({happy_var_4(happy_var_2)})); }
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_271(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_reduce(p, 4, 77, happy_reduction_271, i)
}

fn happy_reduction_271(p: &mut Parser) {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, HappyAbsSyn::NT66(mut happy_var_3), HappyAbsSyn::NT132(mut happy_var_2), _) => {            p.stack.push(HappyAbsSyn::NT66({happy_var_3.append_attrs(happy_var_2)})); }
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_272(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_reduce(p, 5, 77, happy_reduction_272, i)
}

fn happy_reduction_272(p: &mut Parser) {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn::NT88(mut happy_var_5), _, HappyAbsSyn::NT66(mut happy_var_3), HappyAbsSyn::NT132(mut happy_var_2), _) => {            p.stack.push(HappyAbsSyn::NT66({happy_var_5(happy_var_3).append_attrs(happy_var_2)})); }
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_273(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 1, 78, happy_reduction_273, i)
}

fn happy_reduction_273(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap()) {
        HappyAbsSyn::Terminal(CTokIdent(_, mut happy_var_1)) => { with_pos!(p, happy_var_1, |at| CDeclrR::from_var(happy_var_1, at))
        }.map(HappyAbsSyn::NT66),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_274(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_3(p, 78, happy_reduction_274, i)
}

fn happy_reduction_274(happy_x_3: HappyAbsSyn, happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_3, happy_x_2, happy_x_1) {
        (_, HappyAbsSyn::NT66(mut happy_var_2), _) => HappyAbsSyn::NT66({happy_var_2}),
        _ => unreachable!()
    }
}


fn happy_reduce_275(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_reduce(p, 4, 78, happy_reduction_275, i)
}

fn happy_reduction_275(p: &mut Parser) {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, HappyAbsSyn::NT66(mut happy_var_3), HappyAbsSyn::NT132(mut happy_var_2), _) => {            p.stack.push(HappyAbsSyn::NT66({happy_var_3.append_attrs(happy_var_2)})); }
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_276(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_1(p, 79, happy_reduction_276, i)
}

fn happy_reduction_276(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn::NT66(mut happy_var_1) => HappyAbsSyn::NT11({happy_var_1.reverse()}),
        _ => unreachable!()
    }
}


fn happy_reduce_277(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_1(p, 80, happy_reduction_277, i)
}

fn happy_reduction_277(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn::NT66(mut happy_var_1) => HappyAbsSyn::NT66({happy_var_1}),
        _ => unreachable!()
    }
}


fn happy_reduce_278(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 2, 80, happy_reduction_278, i)
}

fn happy_reduction_278(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn::NT66(mut happy_var_2), HappyAbsSyn::Terminal(mut happy_var_1)) => { with_pos!(p, happy_var_1, |at| happy_var_2.ptr_declr(vec![], at))
        }.map(HappyAbsSyn::NT66),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_279(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 3, 80, happy_reduction_279, i)
}

fn happy_reduction_279(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn::NT66(mut happy_var_3), HappyAbsSyn::NT65(mut happy_var_2), HappyAbsSyn::Terminal(mut happy_var_1)) => { with_pos!(p, happy_var_1, |at| happy_var_3.ptr_declr(happy_var_2, at))
        }.map(HappyAbsSyn::NT66),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_280(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 4, 81, happy_reduction_280, i)
}

fn happy_reduction_280(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, HappyAbsSyn::NT21(mut happy_var_3), _, HappyAbsSyn::NT66(mut happy_var_1)) => { with_pos!(p, happy_var_1, |at| happy_var_1.fun_declr(Left(happy_var_3), vec![], at))
        }.map(HappyAbsSyn::NT66),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_281(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_3(p, 81, happy_reduction_281, i)
}

fn happy_reduction_281(happy_x_3: HappyAbsSyn, happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_3, happy_x_2, happy_x_1) {
        (_, HappyAbsSyn::NT66(mut happy_var_2), _) => HappyAbsSyn::NT66({happy_var_2}),
        _ => unreachable!()
    }
}


fn happy_reduce_282(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_reduce(p, 4, 81, happy_reduction_282, i)
}

fn happy_reduction_282(p: &mut Parser) {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn::NT88(mut happy_var_4), _, HappyAbsSyn::NT66(mut happy_var_2), _) => {            p.stack.push(HappyAbsSyn::NT66({happy_var_4(happy_var_2)})); }
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_283(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_0(p, 82, happy_reduction_283(), i)
}

fn happy_reduction_283() -> HappyAbsSyn {
    HappyAbsSyn::NT82((vec![], false))
}


fn happy_reduce_284(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_1(p, 82, happy_reduction_284, i)
}

fn happy_reduction_284(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn::NT33(mut happy_var_1) => HappyAbsSyn::NT82({(happy_var_1, false)}),
        _ => unreachable!()
    }
}


fn happy_reduce_285(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_3(p, 82, happy_reduction_285, i)
}

fn happy_reduction_285(happy_x_3: HappyAbsSyn, happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_3, happy_x_2, happy_x_1) {
        (_, _, HappyAbsSyn::NT33(mut happy_var_1)) => HappyAbsSyn::NT82({(happy_var_1, true)}),
        _ => unreachable!()
    }
}


fn happy_reduce_286(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_1(p, 83, happy_reduction_286, i)
}

fn happy_reduction_286(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn::NT32(mut happy_var_1) => HappyAbsSyn::NT33({vec![*happy_var_1]}),
        _ => unreachable!()
    }
}


fn happy_reduce_287(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_3(p, 83, happy_reduction_287, i)
}

fn happy_reduction_287(happy_x_3: HappyAbsSyn, happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_3, happy_x_2, happy_x_1) {
        (HappyAbsSyn::NT32(mut happy_var_3), _, HappyAbsSyn::NT33(mut happy_var_1)) => HappyAbsSyn::NT33({appended(happy_var_1, *happy_var_3)}),
        _ => unreachable!()
    }
}


fn happy_reduce_288(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 1, 84, happy_reduction_288, i)
}

fn happy_reduction_288(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap()) {
        HappyAbsSyn::NT37(mut happy_var_1) => { with_pos!(p, happy_var_1, |at| box CDecl(happy_var_1, vec![], at))
        }.map(HappyAbsSyn::NT32),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_289(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 2, 84, happy_reduction_289, i)
}

fn happy_reduction_289(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn::NT66(mut happy_var_2), HappyAbsSyn::NT37(mut happy_var_1)) => { with_pos!(p, happy_var_1, |at| box CDecl(happy_var_1, vec![(Some(happy_var_2.reverse()), None, None)], at))
        }.map(HappyAbsSyn::NT32),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_290(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 3, 84, happy_reduction_290, i)
}

fn happy_reduction_290(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn::NT132(mut happy_var_3), HappyAbsSyn::NT66(mut happy_var_2), HappyAbsSyn::NT37(mut happy_var_1)) => { with_pos!(p, happy_var_1, |at| box CDecl(happy_var_1, vec![(Some(happy_var_2.append_attrs(happy_var_3).reverse()), None, None)], at))
        }.map(HappyAbsSyn::NT32),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_291(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 3, 84, happy_reduction_291, i)
}

fn happy_reduction_291(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn::NT132(mut happy_var_3), HappyAbsSyn::NT66(mut happy_var_2), HappyAbsSyn::NT37(mut happy_var_1)) => { with_pos!(p, happy_var_1, |at| box CDecl(happy_var_1, vec![(Some(happy_var_2.append_attrs(happy_var_3).reverse()), None, None)], at))
        }.map(HappyAbsSyn::NT32),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_292(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 1, 84, happy_reduction_292, i)
}

fn happy_reduction_292(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap()) {
        HappyAbsSyn::NT37(mut happy_var_1) => { with_pos!(p, happy_var_1, |at| box CDecl(happy_var_1, vec![], at))
        }.map(HappyAbsSyn::NT32),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_293(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 2, 84, happy_reduction_293, i)
}

fn happy_reduction_293(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn::NT66(mut happy_var_2), HappyAbsSyn::NT37(mut happy_var_1)) => { with_pos!(p, happy_var_1, |at| box CDecl(happy_var_1, vec![(Some(happy_var_2.reverse()), None, None)], at))
        }.map(HappyAbsSyn::NT32),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_294(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 3, 84, happy_reduction_294, i)
}

fn happy_reduction_294(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn::NT132(mut happy_var_3), HappyAbsSyn::NT66(mut happy_var_2), HappyAbsSyn::NT37(mut happy_var_1)) => { with_pos!(p, happy_var_1, |at| box CDecl(happy_var_1, vec![(Some(happy_var_2.append_attrs(happy_var_3).reverse()), None, None)], at))
        }.map(HappyAbsSyn::NT32),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_295(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 1, 84, happy_reduction_295, i)
}

fn happy_reduction_295(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap()) {
        HappyAbsSyn::NT37(mut happy_var_1) => { with_pos!(p, happy_var_1, |at| box CDecl(happy_var_1, vec![], at))
        }.map(HappyAbsSyn::NT32),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_296(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 2, 84, happy_reduction_296, i)
}

fn happy_reduction_296(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn::NT66(mut happy_var_2), HappyAbsSyn::NT37(mut happy_var_1)) => { with_pos!(p, happy_var_1, |at| box CDecl(happy_var_1, vec![(Some(happy_var_2.reverse()), None, None)], at))
        }.map(HappyAbsSyn::NT32),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_297(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 3, 84, happy_reduction_297, i)
}

fn happy_reduction_297(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn::NT132(mut happy_var_3), HappyAbsSyn::NT66(mut happy_var_2), HappyAbsSyn::NT37(mut happy_var_1)) => { with_pos!(p, happy_var_1, |at| box CDecl(happy_var_1, vec![(Some(happy_var_2.append_attrs(happy_var_3).reverse()), None, None)], at))
        }.map(HappyAbsSyn::NT32),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_298(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 3, 84, happy_reduction_298, i)
}

fn happy_reduction_298(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn::NT132(mut happy_var_3), HappyAbsSyn::NT66(mut happy_var_2), HappyAbsSyn::NT37(mut happy_var_1)) => { with_pos!(p, happy_var_1, |at| box CDecl(happy_var_1, vec![(Some(happy_var_2.append_attrs(happy_var_3).reverse()), None, None)], at))
        }.map(HappyAbsSyn::NT32),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_299(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 1, 84, happy_reduction_299, i)
}

fn happy_reduction_299(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap()) {
        HappyAbsSyn::NT65(mut happy_var_1) => { with_pos!(p, happy_var_1, |at| box CDecl(lift_type_quals(happy_var_1), vec![], at))
        }.map(HappyAbsSyn::NT32),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_300(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 2, 84, happy_reduction_300, i)
}

fn happy_reduction_300(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn::NT132(mut happy_var_2), HappyAbsSyn::NT65(mut happy_var_1)) => { with_pos!(p, happy_var_1, |at| box CDecl(add_vecs(lift_type_quals(happy_var_1), lift_attrs(happy_var_2)), vec![], at))
        }.map(HappyAbsSyn::NT32),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_301(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 2, 84, happy_reduction_301, i)
}

fn happy_reduction_301(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn::NT66(mut happy_var_2), HappyAbsSyn::NT65(mut happy_var_1)) => { with_pos!(p, happy_var_1, |at| box CDecl(lift_type_quals(happy_var_1), vec![(Some(happy_var_2.reverse()), None, None)], at))
        }.map(HappyAbsSyn::NT32),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_302(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 3, 84, happy_reduction_302, i)
}

fn happy_reduction_302(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn::NT132(mut happy_var_3), HappyAbsSyn::NT66(mut happy_var_2), HappyAbsSyn::NT65(mut happy_var_1)) => { with_pos!(p, happy_var_1, |at| box CDecl(lift_type_quals(happy_var_1),
                                           vec![(Some(happy_var_2.append_attrs(happy_var_3).reverse()), None, None)], at))
        }.map(HappyAbsSyn::NT32),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_303(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_1(p, 85, happy_reduction_303, i)
}

fn happy_reduction_303(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn::Terminal(CTokIdent(_, mut happy_var_1)) => HappyAbsSyn::NT21({vec![happy_var_1]}),
        _ => unreachable!()
    }
}


fn happy_reduce_304(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_3(p, 85, happy_reduction_304, i)
}

fn happy_reduction_304(happy_x_3: HappyAbsSyn, happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_3, happy_x_2, happy_x_1) {
        (HappyAbsSyn::Terminal(CTokIdent(_, mut happy_var_3)), _, HappyAbsSyn::NT21(mut happy_var_1)) => HappyAbsSyn::NT21({appended(happy_var_1, happy_var_3)}),
        _ => unreachable!()
    }
}


fn happy_reduce_305(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 1, 86, happy_reduction_305, i)
}

fn happy_reduction_305(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap()) {
        HappyAbsSyn::NT37(mut happy_var_1) => { with_pos!(p, happy_var_1, |at| box CDecl(happy_var_1, vec![], at))
        }.map(HappyAbsSyn::NT32),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_306(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 2, 86, happy_reduction_306, i)
}

fn happy_reduction_306(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn::NT66(mut happy_var_2), HappyAbsSyn::NT37(mut happy_var_1)) => { with_pos!(p, happy_var_1, |at| box CDecl(happy_var_1, vec![(Some(happy_var_2.reverse()), None, None)], at))
        }.map(HappyAbsSyn::NT32),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_307(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 2, 86, happy_reduction_307, i)
}

fn happy_reduction_307(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn::NT132(mut happy_var_2), HappyAbsSyn::NT65(mut happy_var_1)) => { with_pos!(p, happy_var_1, |at| box CDecl(add_vecs(lift_type_quals(happy_var_1), lift_attrs(happy_var_2)), vec![], at))
        }.map(HappyAbsSyn::NT32),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_308(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 2, 86, happy_reduction_308, i)
}

fn happy_reduction_308(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn::NT66(mut happy_var_2), HappyAbsSyn::NT65(mut happy_var_1)) => { with_pos!(p, happy_var_1, |at| box CDecl(lift_type_quals(happy_var_1), vec![(Some(happy_var_2.reverse()), None, None)], at))
        }.map(HappyAbsSyn::NT32),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_309(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_1(p, 87, happy_reduction_309, i)
}

fn happy_reduction_309(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn::NT66(mut happy_var_1) => HappyAbsSyn::NT66({happy_var_1}),
        _ => unreachable!()
    }
}


fn happy_reduce_310(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_1(p, 87, happy_reduction_310, i)
}

fn happy_reduction_310(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn::NT66(mut happy_var_1) => HappyAbsSyn::NT66({happy_var_1}),
        _ => unreachable!()
    }
}


fn happy_reduce_311(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_1(p, 87, happy_reduction_311, i)
}

fn happy_reduction_311(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn::NT88(mut happy_var_1) => HappyAbsSyn::NT66({happy_var_1(CDeclrR::empty())}),
        _ => unreachable!()
    }
}


fn happy_reduce_312(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_1(p, 88, happy_reduction_312, i)
}

fn happy_reduction_312(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn::NT88(mut happy_var_1) => HappyAbsSyn::NT88({happy_var_1}),
        _ => unreachable!()
    }
}


fn happy_reduce_313(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 3, 88, happy_reduction_313, i)
}

fn happy_reduction_313(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, HappyAbsSyn::NT82(mut happy_var_2), HappyAbsSyn::Terminal(mut happy_var_1)) => {
            with_pos!(p, happy_var_1, |at| {
                let a: Box<FnBox(Box<CDeclrR>) -> Box<CDeclrR>> = box move |declr: Box<CDeclrR>| {
                    let (params, variadic) = happy_var_2;
                    declr.fun_declr(Right((params, variadic)), vec![], at)
                };
                a
            })
        }.map(HappyAbsSyn::NT88),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_314(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_1(p, 89, happy_reduction_314, i)
}

fn happy_reduction_314(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn::NT88(mut happy_var_1) => HappyAbsSyn::NT88({happy_var_1}),
        _ => unreachable!()
    }
}


fn happy_reduce_315(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_2(p, 89, happy_reduction_315, i)
}

fn happy_reduction_315(happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_2, happy_x_1) {
        (HappyAbsSyn::NT88(mut happy_var_2), HappyAbsSyn::NT88(mut happy_var_1)) => HappyAbsSyn::NT88({box |decl| { happy_var_2(happy_var_1(decl)) }}),
        _ => unreachable!()
    }
}


fn happy_reduce_316(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 3, 90, happy_reduction_316, i)
}

fn happy_reduction_316(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, HappyAbsSyn::NT124(mut happy_var_2), HappyAbsSyn::Terminal(mut happy_var_1)) => {
            with_pos!(p, happy_var_1, |at| {
                let a: Box<FnBox(Box<CDeclrR>) -> Box<CDeclrR>> = box |declr: Box<CDeclrR>| {
                    declr.arr_declr(vec![], false, false, happy_var_2, at)
                };
                a
            })
        }.map(HappyAbsSyn::NT88),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_317(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 4, 90, happy_reduction_317, i)
}

fn happy_reduction_317(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, HappyAbsSyn::NT124(mut happy_var_3), HappyAbsSyn::NT132(mut happy_var_2), HappyAbsSyn::Terminal(mut happy_var_1)) => { p.with_attribute_postfix(happy_var_1, happy_var_2, |at, declr| declr.arr_declr(vec![], false, false, happy_var_3, at))
        }.map(HappyAbsSyn::NT88),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_318(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 4, 90, happy_reduction_318, i)
}

fn happy_reduction_318(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, HappyAbsSyn::NT124(mut happy_var_3), HappyAbsSyn::NT65(mut happy_var_2), HappyAbsSyn::Terminal(mut happy_var_1)) => {
            with_pos!(p, happy_var_1, |at| {
                let a: Box<FnBox(Box<CDeclrR>) -> Box<CDeclrR>> =
                    box |declr: Box<CDeclrR>| declr.arr_declr(happy_var_2, false, false, happy_var_3, at);
                a
            })
        }.map(HappyAbsSyn::NT88),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_319(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 5, 90, happy_reduction_319, i)
}

fn happy_reduction_319(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, HappyAbsSyn::NT124(mut happy_var_4), HappyAbsSyn::NT132(mut happy_var_3), HappyAbsSyn::NT65(mut happy_var_2), HappyAbsSyn::Terminal(mut happy_var_1)) => { p.with_attribute_postfix(happy_var_1, happy_var_3, |at, declr| declr.arr_declr(happy_var_2, false, false, happy_var_4, at))
        }.map(HappyAbsSyn::NT88),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_320(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 5, 90, happy_reduction_320, i)
}

fn happy_reduction_320(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, HappyAbsSyn::NT100(mut happy_var_4), HappyAbsSyn::NT132(mut happy_var_3), _, HappyAbsSyn::Terminal(mut happy_var_1)) => { p.with_attribute_postfix(happy_var_1, happy_var_3, |at, declr| declr.arr_declr(vec![], false, true, Some(happy_var_4), at))
        }.map(HappyAbsSyn::NT88),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_321(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 6, 90, happy_reduction_321, i)
}

fn happy_reduction_321(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, HappyAbsSyn::NT100(mut happy_var_5), HappyAbsSyn::NT132(mut happy_var_4), HappyAbsSyn::NT65(mut happy_var_3), _, HappyAbsSyn::Terminal(mut happy_var_1)) => { p.with_attribute_postfix(happy_var_1, happy_var_4, |at, declr| declr.arr_declr(happy_var_3, false, true, Some(happy_var_5), at))
        }.map(HappyAbsSyn::NT88),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_322(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 7, 90, happy_reduction_322, i)
}

fn happy_reduction_322(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, HappyAbsSyn::NT100(mut happy_var_6), HappyAbsSyn::NT132(mut happy_var_5), _, HappyAbsSyn::NT132(mut happy_var_3), HappyAbsSyn::NT65(mut happy_var_2), HappyAbsSyn::Terminal(mut happy_var_1)) => { p.with_attribute_postfix(happy_var_1, add_vecs(happy_var_3, happy_var_5), |at, declr| declr.arr_declr(happy_var_2, false, true, Some(happy_var_6), at))
        }.map(HappyAbsSyn::NT88),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_323(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 4, 90, happy_reduction_323, i)
}

fn happy_reduction_323(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, HappyAbsSyn::NT132(mut happy_var_3), _, HappyAbsSyn::Terminal(mut happy_var_1)) => { p.with_attribute_postfix(happy_var_1, happy_var_3, |at, declr| declr.arr_declr(vec![], true, false, None, at))
        }.map(HappyAbsSyn::NT88),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_324(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 5, 90, happy_reduction_324, i)
}

fn happy_reduction_324(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, HappyAbsSyn::NT132(mut happy_var_4), _, HappyAbsSyn::NT132(mut happy_var_2), HappyAbsSyn::Terminal(mut happy_var_1)) => { p.with_attribute_postfix(happy_var_1, add_vecs(happy_var_2, happy_var_4), |at, declr|
                             declr.arr_declr(vec![], true, false, None, at))
        }.map(HappyAbsSyn::NT88),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_325(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 5, 90, happy_reduction_325, i)
}

fn happy_reduction_325(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, HappyAbsSyn::NT132(mut happy_var_4), _, HappyAbsSyn::NT65(mut happy_var_2), HappyAbsSyn::Terminal(mut happy_var_1)) => { p.with_attribute_postfix(happy_var_1, happy_var_4, |at, declr| declr.arr_declr(happy_var_2, true, false, None, at))
        }.map(HappyAbsSyn::NT88),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_326(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 6, 90, happy_reduction_326, i)
}

fn happy_reduction_326(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, HappyAbsSyn::NT132(mut happy_var_5), _, HappyAbsSyn::NT132(mut happy_var_3), HappyAbsSyn::NT65(mut happy_var_2), HappyAbsSyn::Terminal(mut happy_var_1)) => { p.with_attribute_postfix(happy_var_1, add_vecs(happy_var_3, happy_var_5), |at, declr| declr.arr_declr(happy_var_2, true, false, None, at))
        }.map(HappyAbsSyn::NT88),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_327(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 1, 91, happy_reduction_327, i)
}

fn happy_reduction_327(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap()) {
        HappyAbsSyn::Terminal(mut happy_var_1) => { with_pos!(p, happy_var_1, |at| CDeclrR::empty().ptr_declr(vec![], at))
        }.map(HappyAbsSyn::NT66),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_328(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 3, 91, happy_reduction_328, i)
}

fn happy_reduction_328(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn::NT132(mut happy_var_3), HappyAbsSyn::NT65(mut happy_var_2), HappyAbsSyn::Terminal(mut happy_var_1)) => { p.with_attribute(happy_var_1, happy_var_3, |at| CDeclrR::empty().ptr_declr(happy_var_2, at))
        }.map(HappyAbsSyn::NT66),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_329(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 2, 91, happy_reduction_329, i)
}

fn happy_reduction_329(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn::NT66(mut happy_var_2), HappyAbsSyn::Terminal(mut happy_var_1)) => { with_pos!(p, happy_var_1, |at| happy_var_2.ptr_declr(vec![], at))
        }.map(HappyAbsSyn::NT66),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_330(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 3, 91, happy_reduction_330, i)
}

fn happy_reduction_330(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn::NT66(mut happy_var_3), HappyAbsSyn::NT65(mut happy_var_2), HappyAbsSyn::Terminal(mut happy_var_1)) => { with_pos!(p, happy_var_1, |at| happy_var_3.ptr_declr(happy_var_2, at))
        }.map(HappyAbsSyn::NT66),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_331(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 2, 91, happy_reduction_331, i)
}

fn happy_reduction_331(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn::NT132(mut happy_var_2), HappyAbsSyn::Terminal(mut happy_var_1)) => { p.with_attribute(happy_var_1, happy_var_2, |at| CDeclrR::empty().ptr_declr(vec![], at))
        }.map(HappyAbsSyn::NT66),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_332(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 3, 91, happy_reduction_332, i)
}

fn happy_reduction_332(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn::NT66(mut happy_var_3), HappyAbsSyn::NT132(mut happy_var_2), HappyAbsSyn::Terminal(mut happy_var_1)) => { p.with_attribute(happy_var_1, happy_var_2, |at| happy_var_3.ptr_declr(vec![], at))
        }.map(HappyAbsSyn::NT66),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_333(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_3(p, 92, happy_reduction_333, i)
}

fn happy_reduction_333(happy_x_3: HappyAbsSyn, happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_3, happy_x_2, happy_x_1) {
        (_, HappyAbsSyn::NT66(mut happy_var_2), _) => HappyAbsSyn::NT66({happy_var_2}),
        _ => unreachable!()
    }
}


fn happy_reduce_334(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_3(p, 92, happy_reduction_334, i)
}

fn happy_reduction_334(happy_x_3: HappyAbsSyn, happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_3, happy_x_2, happy_x_1) {
        (_, HappyAbsSyn::NT66(mut happy_var_2), _) => HappyAbsSyn::NT66({happy_var_2}),
        _ => unreachable!()
    }
}


fn happy_reduce_335(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_3(p, 92, happy_reduction_335, i)
}

fn happy_reduction_335(happy_x_3: HappyAbsSyn, happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_3, happy_x_2, happy_x_1) {
        (_, HappyAbsSyn::NT88(mut happy_var_2), _) => HappyAbsSyn::NT66({happy_var_2(CDeclrR::empty())}),
        _ => unreachable!()
    }
}


fn happy_reduce_336(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_reduce(p, 4, 92, happy_reduction_336, i)
}

fn happy_reduction_336(p: &mut Parser) {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn::NT88(mut happy_var_4), _, HappyAbsSyn::NT66(mut happy_var_2), _) => {            p.stack.push(HappyAbsSyn::NT66({happy_var_4(happy_var_2)})); }
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_337(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_reduce(p, 4, 92, happy_reduction_337, i)
}

fn happy_reduction_337(p: &mut Parser) {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, HappyAbsSyn::NT66(mut happy_var_3), HappyAbsSyn::NT132(mut happy_var_2), _) => {            p.stack.push(HappyAbsSyn::NT66({happy_var_3.append_attrs(happy_var_2)})); }
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_338(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_reduce(p, 4, 92, happy_reduction_338, i)
}

fn happy_reduction_338(p: &mut Parser) {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, HappyAbsSyn::NT66(mut happy_var_3), HappyAbsSyn::NT132(mut happy_var_2), _) => {            p.stack.push(HappyAbsSyn::NT66({happy_var_3.append_attrs(happy_var_2)})); }
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_339(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_reduce(p, 4, 92, happy_reduction_339, i)
}

fn happy_reduction_339(p: &mut Parser) {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, HappyAbsSyn::NT88(mut happy_var_3), HappyAbsSyn::NT132(mut happy_var_2), _) => {            p.stack.push(HappyAbsSyn::NT66({happy_var_3(CDeclrR::empty()).append_attrs(happy_var_2)})); }
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_340(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_reduce(p, 5, 92, happy_reduction_340, i)
}

fn happy_reduction_340(p: &mut Parser) {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn::NT88(mut happy_var_5), _, HappyAbsSyn::NT66(mut happy_var_3), HappyAbsSyn::NT132(mut happy_var_2), _) => {            p.stack.push(HappyAbsSyn::NT66({happy_var_5(happy_var_3).append_attrs(happy_var_2)})); }
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_341(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_2(p, 92, happy_reduction_341, i)
}

fn happy_reduction_341(happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_2, happy_x_1) {
        (HappyAbsSyn::NT132(mut happy_var_2), HappyAbsSyn::NT66(mut happy_var_1)) => HappyAbsSyn::NT66({happy_var_1.append_attrs(happy_var_2)}),
        _ => unreachable!()
    }
}


fn happy_reduce_342(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 1, 93, happy_reduction_342, i)
}

fn happy_reduction_342(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap()) {
        HappyAbsSyn::NT100(mut happy_var_1) => { with_pos!(p, happy_var_1, |at| box CInitExpr(happy_var_1, at))
        }.map(HappyAbsSyn::NT93),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_343(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 3, 93, happy_reduction_343, i)
}

fn happy_reduction_343(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, HappyAbsSyn::NT95(mut happy_var_2), HappyAbsSyn::Terminal(mut happy_var_1)) => { with_pos!(p, happy_var_1, |at| box CInitList(happy_var_2, at))
        }.map(HappyAbsSyn::NT93),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_344(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 4, 93, happy_reduction_344, i)
}

fn happy_reduction_344(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, _, HappyAbsSyn::NT95(mut happy_var_2), HappyAbsSyn::Terminal(mut happy_var_1)) => { with_pos!(p, happy_var_1, |at| box CInitList(happy_var_2, at))
        }.map(HappyAbsSyn::NT93),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_345(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_0(p, 94, happy_reduction_345(), i)
}

fn happy_reduction_345() -> HappyAbsSyn {
    HappyAbsSyn::NT94(None)
}


fn happy_reduce_346(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_2(p, 94, happy_reduction_346, i)
}

fn happy_reduction_346(happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_2, happy_x_1) {
        (HappyAbsSyn::NT93(mut happy_var_2), _) => HappyAbsSyn::NT94({Some(happy_var_2)}),
        _ => unreachable!()
    }
}


fn happy_reduce_347(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_0(p, 95, happy_reduction_347(), i)
}

fn happy_reduction_347() -> HappyAbsSyn {
    HappyAbsSyn::NT95(vec![])
}


fn happy_reduce_348(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_1(p, 95, happy_reduction_348, i)
}

fn happy_reduction_348(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn::NT93(mut happy_var_1) => HappyAbsSyn::NT95({vec![(vec![], happy_var_1)]}),
        _ => unreachable!()
    }
}


fn happy_reduce_349(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_2(p, 95, happy_reduction_349, i)
}

fn happy_reduction_349(happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_2, happy_x_1) {
        (HappyAbsSyn::NT93(mut happy_var_2), HappyAbsSyn::NT96(mut happy_var_1)) => HappyAbsSyn::NT95({vec![(happy_var_1, happy_var_2)]}),
        _ => unreachable!()
    }
}


fn happy_reduce_350(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_3(p, 95, happy_reduction_350, i)
}

fn happy_reduction_350(happy_x_3: HappyAbsSyn, happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_3, happy_x_2, happy_x_1) {
        (HappyAbsSyn::NT93(mut happy_var_3), _, HappyAbsSyn::NT95(mut happy_var_1)) => HappyAbsSyn::NT95({appended(happy_var_1, (vec![], happy_var_3))}),
        _ => unreachable!()
    }
}


fn happy_reduce_351(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_reduce(p, 4, 95, happy_reduction_351, i)
}

fn happy_reduction_351(p: &mut Parser) {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn::NT93(mut happy_var_4), HappyAbsSyn::NT96(mut happy_var_3), _, HappyAbsSyn::NT95(mut happy_var_1)) => {            p.stack.push(HappyAbsSyn::NT95({appended(happy_var_1, (happy_var_3, happy_var_4))})); }
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_352(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_2(p, 96, happy_reduction_352, i)
}

fn happy_reduction_352(happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_2, happy_x_1) {
        (_, HappyAbsSyn::NT96(mut happy_var_1)) => HappyAbsSyn::NT96({happy_var_1}),
        _ => unreachable!()
    }
}


fn happy_reduce_353(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 2, 96, happy_reduction_353, i)
}

fn happy_reduction_353(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, HappyAbsSyn::NT131(mut happy_var_1)) => { with_pos!(p, happy_var_1, |at| vec![CMemberDesig(happy_var_1, at)])
        }.map(HappyAbsSyn::NT96),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_354(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_1(p, 96, happy_reduction_354, i)
}

fn happy_reduction_354(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn::NT98(mut happy_var_1) => HappyAbsSyn::NT96({vec![*happy_var_1]}),
        _ => unreachable!()
    }
}


fn happy_reduce_355(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_1(p, 97, happy_reduction_355, i)
}

fn happy_reduction_355(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn::NT98(mut happy_var_1) => HappyAbsSyn::NT96({vec![*happy_var_1]}),
        _ => unreachable!()
    }
}


fn happy_reduce_356(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_2(p, 97, happy_reduction_356, i)
}

fn happy_reduction_356(happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_2, happy_x_1) {
        (HappyAbsSyn::NT98(mut happy_var_2), HappyAbsSyn::NT96(mut happy_var_1)) => HappyAbsSyn::NT96({appended(happy_var_1, *happy_var_2)}),
        _ => unreachable!()
    }
}


fn happy_reduce_357(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 3, 98, happy_reduction_357, i)
}

fn happy_reduction_357(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, HappyAbsSyn::NT100(mut happy_var_2), HappyAbsSyn::Terminal(mut happy_var_1)) => { with_pos!(p, happy_var_1, |at| box CArrDesig(happy_var_2, at))
        }.map(HappyAbsSyn::NT98),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_358(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 2, 98, happy_reduction_358, i)
}

fn happy_reduction_358(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn::NT131(mut happy_var_2), HappyAbsSyn::Terminal(mut happy_var_1)) => { with_pos!(p, happy_var_1, |at| box CMemberDesig(happy_var_2, at))
        }.map(HappyAbsSyn::NT98),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_359(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_1(p, 98, happy_reduction_359, i)
}

fn happy_reduction_359(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn::NT98(mut happy_var_1) => HappyAbsSyn::NT98({happy_var_1}),
        _ => unreachable!()
    }
}


fn happy_reduce_360(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 5, 99, happy_reduction_360, i)
}

fn happy_reduction_360(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, HappyAbsSyn::NT100(mut happy_var_4), _, HappyAbsSyn::NT100(mut happy_var_2), HappyAbsSyn::Terminal(mut happy_var_1)) => { with_pos!(p, happy_var_1, |at| box CRangeDesig(happy_var_2, happy_var_4, at))
        }.map(HappyAbsSyn::NT98),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_361(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 1, 100, happy_reduction_361, i)
}

fn happy_reduction_361(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap()) {
        HappyAbsSyn::Terminal(CTokIdent(_, mut happy_var_1)) => { with_pos!(p, happy_var_1, |at| box CVar(happy_var_1, at))
        }.map(HappyAbsSyn::NT100),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_362(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_1(p, 100, happy_reduction_362, i)
}

fn happy_reduction_362(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn::NT127(mut happy_var_1) => HappyAbsSyn::NT100({box CConst(happy_var_1)}),
        _ => unreachable!()
    }
}


fn happy_reduce_363(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_1(p, 100, happy_reduction_363, i)
}

fn happy_reduction_363(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn::NT128(mut happy_var_1) => HappyAbsSyn::NT100({box CConst(box CConstant::from_strlit(*happy_var_1))}),
        _ => unreachable!()
    }
}


fn happy_reduce_364(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_3(p, 100, happy_reduction_364, i)
}

fn happy_reduction_364(happy_x_3: HappyAbsSyn, happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_3, happy_x_2, happy_x_1) {
        (_, HappyAbsSyn::NT100(mut happy_var_2), _) => HappyAbsSyn::NT100({happy_var_2}),
        _ => unreachable!()
    }
}


fn happy_reduce_365(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 6, 100, happy_reduction_365, i)
}

fn happy_reduction_365(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, HappyAbsSyn::NT101(mut happy_var_5), _, HappyAbsSyn::NT100(mut happy_var_3), _, HappyAbsSyn::Terminal(mut happy_var_1)) => { with_pos!(p, happy_var_1, |at| box CGenericSelection(happy_var_3, happy_var_5, at))
        }.map(HappyAbsSyn::NT100),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_366(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 3, 100, happy_reduction_366, i)
}

fn happy_reduction_366(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, HappyAbsSyn::NT12(mut happy_var_2), HappyAbsSyn::Terminal(mut happy_var_1)) => { with_pos!(p, happy_var_1, |at| box CStatExpr(happy_var_2, at))
        }.map(HappyAbsSyn::NT100),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_367(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 6, 100, happy_reduction_367, i)
}

fn happy_reduction_367(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, HappyAbsSyn::NT32(mut happy_var_5), _, HappyAbsSyn::NT100(mut happy_var_3), _, HappyAbsSyn::Terminal(mut happy_var_1)) => { with_pos!(p, happy_var_1, |at| box CBuiltinExpr(box CBuiltinVaArg(happy_var_3, happy_var_5, at)))
        }.map(HappyAbsSyn::NT100),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_368(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 6, 100, happy_reduction_368, i)
}

fn happy_reduction_368(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, HappyAbsSyn::NT96(mut happy_var_5), _, HappyAbsSyn::NT32(mut happy_var_3), _, HappyAbsSyn::Terminal(mut happy_var_1)) => { with_pos!(p, happy_var_1, |at| box CBuiltinExpr(box CBuiltinOffsetOf(happy_var_3, happy_var_5, at)))
        }.map(HappyAbsSyn::NT100),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_369(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 6, 100, happy_reduction_369, i)
}

fn happy_reduction_369(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, HappyAbsSyn::NT32(mut happy_var_5), _, HappyAbsSyn::NT32(mut happy_var_3), _, HappyAbsSyn::Terminal(mut happy_var_1)) => { with_pos!(p, happy_var_1, |at| box CBuiltinExpr(box CBuiltinTypesCompatible(happy_var_3, happy_var_5, at)))
        }.map(HappyAbsSyn::NT100),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_370(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 6, 100, happy_reduction_370, i)
}

fn happy_reduction_370(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, HappyAbsSyn::NT32(mut happy_var_5), _, HappyAbsSyn::NT100(mut happy_var_3), _, HappyAbsSyn::Terminal(mut happy_var_1)) => { with_pos!(p, happy_var_1, |at| box CBuiltinExpr(box CBuiltinConvertVector(happy_var_3, happy_var_5, at)))
        }.map(HappyAbsSyn::NT100),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_371(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_3(p, 101, happy_reduction_371, i)
}

fn happy_reduction_371(happy_x_3: HappyAbsSyn, happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_3, happy_x_2, happy_x_1) {
        (HappyAbsSyn::NT102(mut happy_var_3), _, HappyAbsSyn::NT101(mut happy_var_1)) => HappyAbsSyn::NT101({appended(happy_var_1, happy_var_3)}),
        _ => unreachable!()
    }
}


fn happy_reduce_372(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_1(p, 101, happy_reduction_372, i)
}

fn happy_reduction_372(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn::NT102(mut happy_var_1) => HappyAbsSyn::NT101({vec![happy_var_1]}),
        _ => unreachable!()
    }
}


fn happy_reduce_373(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_3(p, 102, happy_reduction_373, i)
}

fn happy_reduction_373(happy_x_3: HappyAbsSyn, happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_3, happy_x_2, happy_x_1) {
        (HappyAbsSyn::NT100(mut happy_var_3), _, HappyAbsSyn::NT32(mut happy_var_1)) => HappyAbsSyn::NT102({(Some(happy_var_1), happy_var_3)}),
        _ => unreachable!()
    }
}


fn happy_reduce_374(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_3(p, 102, happy_reduction_374, i)
}

fn happy_reduction_374(happy_x_3: HappyAbsSyn, happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_3, happy_x_2, happy_x_1) {
        (HappyAbsSyn::NT100(mut happy_var_3), _, _) => HappyAbsSyn::NT102({(None, happy_var_3)}),
        _ => unreachable!()
    }
}


fn happy_reduce_375(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 1, 103, happy_reduction_375, i)
}

fn happy_reduction_375(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap()) {
        HappyAbsSyn::NT131(mut happy_var_1) => { with_pos!(p, happy_var_1, |at| vec![CMemberDesig(happy_var_1, at)])
        }.map(HappyAbsSyn::NT96),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_376(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 3, 103, happy_reduction_376, i)
}

fn happy_reduction_376(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn::NT131(mut happy_var_3), _, HappyAbsSyn::NT96(mut happy_var_1)) => { with_pos!(p, happy_var_3, |at| appended(happy_var_1, CMemberDesig(happy_var_3, at)))
        }.map(HappyAbsSyn::NT96),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_377(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 4, 103, happy_reduction_377, i)
}

fn happy_reduction_377(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, HappyAbsSyn::NT100(mut happy_var_3), _, HappyAbsSyn::NT96(mut happy_var_1)) => { with_pos!(p, happy_var_3, |at| appended(happy_var_1, CArrDesig(happy_var_3, at)))
        }.map(HappyAbsSyn::NT96),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_378(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_1(p, 104, happy_reduction_378, i)
}

fn happy_reduction_378(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn::NT100(mut happy_var_1) => HappyAbsSyn::NT100({happy_var_1}),
        _ => unreachable!()
    }
}


fn happy_reduce_379(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 4, 104, happy_reduction_379, i)
}

fn happy_reduction_379(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, HappyAbsSyn::NT100(mut happy_var_3), _, HappyAbsSyn::NT100(mut happy_var_1)) => { with_pos!(p, happy_var_1, |at| box CIndex(happy_var_1, happy_var_3, at))
        }.map(HappyAbsSyn::NT100),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_380(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 3, 104, happy_reduction_380, i)
}

fn happy_reduction_380(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, _, HappyAbsSyn::NT100(mut happy_var_1)) => { with_pos!(p, happy_var_1, |at| box CCall(happy_var_1, vec![], at))
        }.map(HappyAbsSyn::NT100),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_381(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 4, 104, happy_reduction_381, i)
}

fn happy_reduction_381(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, HappyAbsSyn::NT105(mut happy_var_3), _, HappyAbsSyn::NT100(mut happy_var_1)) => { with_pos!(p, happy_var_1, |at| box CCall(happy_var_1, happy_var_3, at))
        }.map(HappyAbsSyn::NT100),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_382(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 3, 104, happy_reduction_382, i)
}

fn happy_reduction_382(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn::NT131(mut happy_var_3), _, HappyAbsSyn::NT100(mut happy_var_1)) => { with_pos!(p, happy_var_1, |at| box CMember(happy_var_1, happy_var_3, false, at))
        }.map(HappyAbsSyn::NT100),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_383(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 3, 104, happy_reduction_383, i)
}

fn happy_reduction_383(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn::NT131(mut happy_var_3), _, HappyAbsSyn::NT100(mut happy_var_1)) => { with_pos!(p, happy_var_1, |at| box CMember(happy_var_1, happy_var_3, true, at))
        }.map(HappyAbsSyn::NT100),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_384(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 2, 104, happy_reduction_384, i)
}

fn happy_reduction_384(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, HappyAbsSyn::NT100(mut happy_var_1)) => { with_pos!(p, happy_var_1, |at| box CUnary(CPostIncOp, happy_var_1, at))
        }.map(HappyAbsSyn::NT100),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_385(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 2, 104, happy_reduction_385, i)
}

fn happy_reduction_385(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, HappyAbsSyn::NT100(mut happy_var_1)) => { with_pos!(p, happy_var_1, |at| box CUnary(CPostDecOp, happy_var_1, at))
        }.map(HappyAbsSyn::NT100),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_386(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 6, 104, happy_reduction_386, i)
}

fn happy_reduction_386(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, HappyAbsSyn::NT95(mut happy_var_5), _, _, HappyAbsSyn::NT32(mut happy_var_2), HappyAbsSyn::Terminal(mut happy_var_1)) => { with_pos!(p, happy_var_1, |at| box CCompoundLit(happy_var_2, happy_var_5, at))
        }.map(HappyAbsSyn::NT100),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_387(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 7, 104, happy_reduction_387, i)
}

fn happy_reduction_387(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, _, HappyAbsSyn::NT95(mut happy_var_5), _, _, HappyAbsSyn::NT32(mut happy_var_2), HappyAbsSyn::Terminal(mut happy_var_1)) => { with_pos!(p, happy_var_1, |at| box CCompoundLit(happy_var_2, happy_var_5, at))
        }.map(HappyAbsSyn::NT100),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_388(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_1(p, 105, happy_reduction_388, i)
}

fn happy_reduction_388(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn::NT100(mut happy_var_1) => HappyAbsSyn::NT105({vec![*happy_var_1]}),
        _ => unreachable!()
    }
}


fn happy_reduce_389(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_3(p, 105, happy_reduction_389, i)
}

fn happy_reduction_389(happy_x_3: HappyAbsSyn, happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_3, happy_x_2, happy_x_1) {
        (HappyAbsSyn::NT100(mut happy_var_3), _, HappyAbsSyn::NT105(mut happy_var_1)) => HappyAbsSyn::NT105({appended(happy_var_1, *happy_var_3)}),
        _ => unreachable!()
    }
}


fn happy_reduce_390(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_1(p, 106, happy_reduction_390, i)
}

fn happy_reduction_390(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn::NT100(mut happy_var_1) => HappyAbsSyn::NT100({happy_var_1}),
        _ => unreachable!()
    }
}


fn happy_reduce_391(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 2, 106, happy_reduction_391, i)
}

fn happy_reduction_391(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn::NT100(mut happy_var_2), HappyAbsSyn::Terminal(mut happy_var_1)) => { with_pos!(p, happy_var_1, |at| box CUnary(CPreIncOp, happy_var_2, at))
        }.map(HappyAbsSyn::NT100),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_392(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 2, 106, happy_reduction_392, i)
}

fn happy_reduction_392(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn::NT100(mut happy_var_2), HappyAbsSyn::Terminal(mut happy_var_1)) => { with_pos!(p, happy_var_1, |at| box CUnary(CPreDecOp, happy_var_2, at))
        }.map(HappyAbsSyn::NT100),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_393(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_2(p, 106, happy_reduction_393, i)
}

fn happy_reduction_393(happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_2, happy_x_1) {
        (HappyAbsSyn::NT100(mut happy_var_2), _) => HappyAbsSyn::NT100({happy_var_2}),
        _ => unreachable!()
    }
}


fn happy_reduce_394(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 2, 106, happy_reduction_394, i)
}

fn happy_reduction_394(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn::NT100(mut happy_var_2), HappyAbsSyn::NT107(mut happy_var_1)) => { with_pos!(p, happy_var_1, |at| box CUnary(happy_var_1.into_inner(), happy_var_2, at))
        }.map(HappyAbsSyn::NT100),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_395(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 2, 106, happy_reduction_395, i)
}

fn happy_reduction_395(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn::NT100(mut happy_var_2), HappyAbsSyn::Terminal(mut happy_var_1)) => { with_pos!(p, happy_var_1, |at| box CSizeofExpr(happy_var_2, at))
        }.map(HappyAbsSyn::NT100),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_396(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 4, 106, happy_reduction_396, i)
}

fn happy_reduction_396(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, HappyAbsSyn::NT32(mut happy_var_3), _, HappyAbsSyn::Terminal(mut happy_var_1)) => { with_pos!(p, happy_var_1, |at| box CSizeofType(happy_var_3, at))
        }.map(HappyAbsSyn::NT100),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_397(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 2, 106, happy_reduction_397, i)
}

fn happy_reduction_397(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn::NT100(mut happy_var_2), HappyAbsSyn::Terminal(mut happy_var_1)) => { with_pos!(p, happy_var_1, |at| box CAlignofExpr(happy_var_2, at))
        }.map(HappyAbsSyn::NT100),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_398(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 4, 106, happy_reduction_398, i)
}

fn happy_reduction_398(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, HappyAbsSyn::NT32(mut happy_var_3), _, HappyAbsSyn::Terminal(mut happy_var_1)) => { with_pos!(p, happy_var_1, |at| box CAlignofType(happy_var_3, at))
        }.map(HappyAbsSyn::NT100),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_399(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 2, 106, happy_reduction_399, i)
}

fn happy_reduction_399(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn::NT100(mut happy_var_2), HappyAbsSyn::Terminal(mut happy_var_1)) => { with_pos!(p, happy_var_1, |at| box CComplexReal(happy_var_2, at))
        }.map(HappyAbsSyn::NT100),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_400(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 2, 106, happy_reduction_400, i)
}

fn happy_reduction_400(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn::NT100(mut happy_var_2), HappyAbsSyn::Terminal(mut happy_var_1)) => { with_pos!(p, happy_var_1, |at| box CComplexImag(happy_var_2, at))
        }.map(HappyAbsSyn::NT100),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_401(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 2, 106, happy_reduction_401, i)
}

fn happy_reduction_401(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn::NT131(mut happy_var_2), HappyAbsSyn::Terminal(mut happy_var_1)) => { with_pos!(p, happy_var_1, |at| box CLabAddrExpr(happy_var_2, at))
        }.map(HappyAbsSyn::NT100),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_402(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_1(p, 107, happy_reduction_402, i)
}

fn happy_reduction_402(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn::Terminal(mut happy_var_1) => HappyAbsSyn::NT107({Located::new(CAdrOp,  happy_var_1)}),
        _ => unreachable!()
    }
}


fn happy_reduce_403(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_1(p, 107, happy_reduction_403, i)
}

fn happy_reduction_403(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn::Terminal(mut happy_var_1) => HappyAbsSyn::NT107({Located::new(CIndOp,  happy_var_1)}),
        _ => unreachable!()
    }
}


fn happy_reduce_404(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_1(p, 107, happy_reduction_404, i)
}

fn happy_reduction_404(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn::Terminal(mut happy_var_1) => HappyAbsSyn::NT107({Located::new(CPlusOp, happy_var_1)}),
        _ => unreachable!()
    }
}


fn happy_reduce_405(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_1(p, 107, happy_reduction_405, i)
}

fn happy_reduction_405(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn::Terminal(mut happy_var_1) => HappyAbsSyn::NT107({Located::new(CMinOp,  happy_var_1)}),
        _ => unreachable!()
    }
}


fn happy_reduce_406(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_1(p, 107, happy_reduction_406, i)
}

fn happy_reduction_406(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn::Terminal(mut happy_var_1) => HappyAbsSyn::NT107({Located::new(CCompOp, happy_var_1)}),
        _ => unreachable!()
    }
}


fn happy_reduce_407(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_1(p, 107, happy_reduction_407, i)
}

fn happy_reduction_407(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn::Terminal(mut happy_var_1) => HappyAbsSyn::NT107({Located::new(CNegOp,  happy_var_1)}),
        _ => unreachable!()
    }
}


fn happy_reduce_408(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_1(p, 108, happy_reduction_408, i)
}

fn happy_reduction_408(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn::NT100(mut happy_var_1) => HappyAbsSyn::NT100({happy_var_1}),
        _ => unreachable!()
    }
}


fn happy_reduce_409(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 4, 108, happy_reduction_409, i)
}

fn happy_reduction_409(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn::NT100(mut happy_var_4), _, HappyAbsSyn::NT32(mut happy_var_2), HappyAbsSyn::Terminal(mut happy_var_1)) => { with_pos!(p, happy_var_1, |at| box CCast(happy_var_2, happy_var_4, at))
        }.map(HappyAbsSyn::NT100),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_410(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_1(p, 109, happy_reduction_410, i)
}

fn happy_reduction_410(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn::NT100(mut happy_var_1) => HappyAbsSyn::NT100({happy_var_1}),
        _ => unreachable!()
    }
}


fn happy_reduce_411(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 3, 109, happy_reduction_411, i)
}

fn happy_reduction_411(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn::NT100(mut happy_var_3), _, HappyAbsSyn::NT100(mut happy_var_1)) => { with_pos!(p, happy_var_1, |at| box CBinary(CMulOp, happy_var_1, happy_var_3, at))
        }.map(HappyAbsSyn::NT100),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_412(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 3, 109, happy_reduction_412, i)
}

fn happy_reduction_412(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn::NT100(mut happy_var_3), _, HappyAbsSyn::NT100(mut happy_var_1)) => { with_pos!(p, happy_var_1, |at| box CBinary(CDivOp, happy_var_1, happy_var_3, at))
        }.map(HappyAbsSyn::NT100),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_413(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 3, 109, happy_reduction_413, i)
}

fn happy_reduction_413(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn::NT100(mut happy_var_3), _, HappyAbsSyn::NT100(mut happy_var_1)) => { with_pos!(p, happy_var_1, |at| box CBinary(CRmdOp, happy_var_1, happy_var_3, at))
        }.map(HappyAbsSyn::NT100),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_414(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_1(p, 110, happy_reduction_414, i)
}

fn happy_reduction_414(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn::NT100(mut happy_var_1) => HappyAbsSyn::NT100({happy_var_1}),
        _ => unreachable!()
    }
}


fn happy_reduce_415(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 3, 110, happy_reduction_415, i)
}

fn happy_reduction_415(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn::NT100(mut happy_var_3), _, HappyAbsSyn::NT100(mut happy_var_1)) => { with_pos!(p, happy_var_1, |at| box CBinary(CAddOp, happy_var_1, happy_var_3, at))
        }.map(HappyAbsSyn::NT100),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_416(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 3, 110, happy_reduction_416, i)
}

fn happy_reduction_416(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn::NT100(mut happy_var_3), _, HappyAbsSyn::NT100(mut happy_var_1)) => { with_pos!(p, happy_var_1, |at| box CBinary(CSubOp, happy_var_1, happy_var_3, at))
        }.map(HappyAbsSyn::NT100),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_417(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_1(p, 111, happy_reduction_417, i)
}

fn happy_reduction_417(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn::NT100(mut happy_var_1) => HappyAbsSyn::NT100({happy_var_1}),
        _ => unreachable!()
    }
}


fn happy_reduce_418(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 3, 111, happy_reduction_418, i)
}

fn happy_reduction_418(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn::NT100(mut happy_var_3), _, HappyAbsSyn::NT100(mut happy_var_1)) => { with_pos!(p, happy_var_1, |at| box CBinary(CShlOp, happy_var_1, happy_var_3, at))
        }.map(HappyAbsSyn::NT100),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_419(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 3, 111, happy_reduction_419, i)
}

fn happy_reduction_419(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn::NT100(mut happy_var_3), _, HappyAbsSyn::NT100(mut happy_var_1)) => { with_pos!(p, happy_var_1, |at| box CBinary(CShrOp, happy_var_1, happy_var_3, at))
        }.map(HappyAbsSyn::NT100),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_420(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_1(p, 112, happy_reduction_420, i)
}

fn happy_reduction_420(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn::NT100(mut happy_var_1) => HappyAbsSyn::NT100({happy_var_1}),
        _ => unreachable!()
    }
}


fn happy_reduce_421(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 3, 112, happy_reduction_421, i)
}

fn happy_reduction_421(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn::NT100(mut happy_var_3), _, HappyAbsSyn::NT100(mut happy_var_1)) => { with_pos!(p, happy_var_1, |at| box CBinary(CLeOp, happy_var_1, happy_var_3, at))
        }.map(HappyAbsSyn::NT100),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_422(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 3, 112, happy_reduction_422, i)
}

fn happy_reduction_422(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn::NT100(mut happy_var_3), _, HappyAbsSyn::NT100(mut happy_var_1)) => { with_pos!(p, happy_var_1, |at| box CBinary(CGrOp, happy_var_1, happy_var_3, at))
        }.map(HappyAbsSyn::NT100),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_423(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 3, 112, happy_reduction_423, i)
}

fn happy_reduction_423(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn::NT100(mut happy_var_3), _, HappyAbsSyn::NT100(mut happy_var_1)) => { with_pos!(p, happy_var_1, |at| box CBinary(CLeqOp, happy_var_1, happy_var_3, at))
        }.map(HappyAbsSyn::NT100),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_424(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 3, 112, happy_reduction_424, i)
}

fn happy_reduction_424(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn::NT100(mut happy_var_3), _, HappyAbsSyn::NT100(mut happy_var_1)) => { with_pos!(p, happy_var_1, |at| box CBinary(CGeqOp, happy_var_1, happy_var_3, at))
        }.map(HappyAbsSyn::NT100),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_425(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_1(p, 113, happy_reduction_425, i)
}

fn happy_reduction_425(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn::NT100(mut happy_var_1) => HappyAbsSyn::NT100({happy_var_1}),
        _ => unreachable!()
    }
}


fn happy_reduce_426(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 3, 113, happy_reduction_426, i)
}

fn happy_reduction_426(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn::NT100(mut happy_var_3), _, HappyAbsSyn::NT100(mut happy_var_1)) => { with_pos!(p, happy_var_1, |at| box CBinary(CEqOp, happy_var_1, happy_var_3, at))
        }.map(HappyAbsSyn::NT100),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_427(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 3, 113, happy_reduction_427, i)
}

fn happy_reduction_427(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn::NT100(mut happy_var_3), _, HappyAbsSyn::NT100(mut happy_var_1)) => { with_pos!(p, happy_var_1, |at| box CBinary(CNeqOp, happy_var_1, happy_var_3, at))
        }.map(HappyAbsSyn::NT100),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_428(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_1(p, 114, happy_reduction_428, i)
}

fn happy_reduction_428(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn::NT100(mut happy_var_1) => HappyAbsSyn::NT100({happy_var_1}),
        _ => unreachable!()
    }
}


fn happy_reduce_429(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 3, 114, happy_reduction_429, i)
}

fn happy_reduction_429(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn::NT100(mut happy_var_3), _, HappyAbsSyn::NT100(mut happy_var_1)) => { with_pos!(p, happy_var_1, |at| box CBinary(CAndOp, happy_var_1, happy_var_3, at))
        }.map(HappyAbsSyn::NT100),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_430(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_1(p, 115, happy_reduction_430, i)
}

fn happy_reduction_430(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn::NT100(mut happy_var_1) => HappyAbsSyn::NT100({happy_var_1}),
        _ => unreachable!()
    }
}


fn happy_reduce_431(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 3, 115, happy_reduction_431, i)
}

fn happy_reduction_431(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn::NT100(mut happy_var_3), _, HappyAbsSyn::NT100(mut happy_var_1)) => { with_pos!(p, happy_var_1, |at| box CBinary(CXorOp, happy_var_1, happy_var_3, at))
        }.map(HappyAbsSyn::NT100),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_432(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_1(p, 116, happy_reduction_432, i)
}

fn happy_reduction_432(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn::NT100(mut happy_var_1) => HappyAbsSyn::NT100({happy_var_1}),
        _ => unreachable!()
    }
}


fn happy_reduce_433(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 3, 116, happy_reduction_433, i)
}

fn happy_reduction_433(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn::NT100(mut happy_var_3), _, HappyAbsSyn::NT100(mut happy_var_1)) => { with_pos!(p, happy_var_1, |at| box CBinary(COrOp, happy_var_1, happy_var_3, at))
        }.map(HappyAbsSyn::NT100),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_434(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_1(p, 117, happy_reduction_434, i)
}

fn happy_reduction_434(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn::NT100(mut happy_var_1) => HappyAbsSyn::NT100({happy_var_1}),
        _ => unreachable!()
    }
}


fn happy_reduce_435(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 3, 117, happy_reduction_435, i)
}

fn happy_reduction_435(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn::NT100(mut happy_var_3), _, HappyAbsSyn::NT100(mut happy_var_1)) => { with_pos!(p, happy_var_1, |at| box CBinary(CLndOp, happy_var_1, happy_var_3, at))
        }.map(HappyAbsSyn::NT100),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_436(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_1(p, 118, happy_reduction_436, i)
}

fn happy_reduction_436(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn::NT100(mut happy_var_1) => HappyAbsSyn::NT100({happy_var_1}),
        _ => unreachable!()
    }
}


fn happy_reduce_437(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 3, 118, happy_reduction_437, i)
}

fn happy_reduction_437(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn::NT100(mut happy_var_3), _, HappyAbsSyn::NT100(mut happy_var_1)) => { with_pos!(p, happy_var_1, |at| box CBinary(CLorOp, happy_var_1, happy_var_3, at))
        }.map(HappyAbsSyn::NT100),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_438(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_1(p, 119, happy_reduction_438, i)
}

fn happy_reduction_438(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn::NT100(mut happy_var_1) => HappyAbsSyn::NT100({happy_var_1}),
        _ => unreachable!()
    }
}


fn happy_reduce_439(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 5, 119, happy_reduction_439, i)
}

fn happy_reduction_439(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn::NT100(mut happy_var_5), _, HappyAbsSyn::NT100(mut happy_var_3), _, HappyAbsSyn::NT100(mut happy_var_1)) => { with_pos!(p, happy_var_1, |at| box CCond(happy_var_1, Some(happy_var_3), happy_var_5, at))
        }.map(HappyAbsSyn::NT100),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_440(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 4, 119, happy_reduction_440, i)
}

fn happy_reduction_440(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn::NT100(mut happy_var_4), _, _, HappyAbsSyn::NT100(mut happy_var_1)) => { with_pos!(p, happy_var_1, |at| box CCond(happy_var_1, None, happy_var_4, at))
        }.map(HappyAbsSyn::NT100),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_441(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_1(p, 120, happy_reduction_441, i)
}

fn happy_reduction_441(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn::NT100(mut happy_var_1) => HappyAbsSyn::NT100({happy_var_1}),
        _ => unreachable!()
    }
}


fn happy_reduce_442(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 3, 120, happy_reduction_442, i)
}

fn happy_reduction_442(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn::NT100(mut happy_var_3), HappyAbsSyn::NT121(mut happy_var_2), HappyAbsSyn::NT100(mut happy_var_1)) => { with_pos!(p, happy_var_1, |at| box CAssign(happy_var_2.into_inner(), happy_var_1, happy_var_3, at))
        }.map(HappyAbsSyn::NT100),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_443(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_1(p, 121, happy_reduction_443, i)
}

fn happy_reduction_443(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn::Terminal(mut happy_var_1) => HappyAbsSyn::NT121({Located::new(CAssignOp, happy_var_1)}),
        _ => unreachable!()
    }
}


fn happy_reduce_444(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_1(p, 121, happy_reduction_444, i)
}

fn happy_reduction_444(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn::Terminal(mut happy_var_1) => HappyAbsSyn::NT121({Located::new(CMulAssOp, happy_var_1)}),
        _ => unreachable!()
    }
}


fn happy_reduce_445(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_1(p, 121, happy_reduction_445, i)
}

fn happy_reduction_445(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn::Terminal(mut happy_var_1) => HappyAbsSyn::NT121({Located::new(CDivAssOp, happy_var_1)}),
        _ => unreachable!()
    }
}


fn happy_reduce_446(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_1(p, 121, happy_reduction_446, i)
}

fn happy_reduction_446(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn::Terminal(mut happy_var_1) => HappyAbsSyn::NT121({Located::new(CRmdAssOp, happy_var_1)}),
        _ => unreachable!()
    }
}


fn happy_reduce_447(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_1(p, 121, happy_reduction_447, i)
}

fn happy_reduction_447(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn::Terminal(mut happy_var_1) => HappyAbsSyn::NT121({Located::new(CAddAssOp, happy_var_1)}),
        _ => unreachable!()
    }
}


fn happy_reduce_448(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_1(p, 121, happy_reduction_448, i)
}

fn happy_reduction_448(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn::Terminal(mut happy_var_1) => HappyAbsSyn::NT121({Located::new(CSubAssOp, happy_var_1)}),
        _ => unreachable!()
    }
}


fn happy_reduce_449(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_1(p, 121, happy_reduction_449, i)
}

fn happy_reduction_449(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn::Terminal(mut happy_var_1) => HappyAbsSyn::NT121({Located::new(CShlAssOp, happy_var_1)}),
        _ => unreachable!()
    }
}


fn happy_reduce_450(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_1(p, 121, happy_reduction_450, i)
}

fn happy_reduction_450(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn::Terminal(mut happy_var_1) => HappyAbsSyn::NT121({Located::new(CShrAssOp, happy_var_1)}),
        _ => unreachable!()
    }
}


fn happy_reduce_451(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_1(p, 121, happy_reduction_451, i)
}

fn happy_reduction_451(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn::Terminal(mut happy_var_1) => HappyAbsSyn::NT121({Located::new(CAndAssOp, happy_var_1)}),
        _ => unreachable!()
    }
}


fn happy_reduce_452(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_1(p, 121, happy_reduction_452, i)
}

fn happy_reduction_452(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn::Terminal(mut happy_var_1) => HappyAbsSyn::NT121({Located::new(CXorAssOp, happy_var_1)}),
        _ => unreachable!()
    }
}


fn happy_reduce_453(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_1(p, 121, happy_reduction_453, i)
}

fn happy_reduction_453(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn::Terminal(mut happy_var_1) => HappyAbsSyn::NT121({Located::new(COrAssOp,  happy_var_1)}),
        _ => unreachable!()
    }
}


fn happy_reduce_454(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_1(p, 122, happy_reduction_454, i)
}

fn happy_reduction_454(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn::NT100(mut happy_var_1) => HappyAbsSyn::NT100({happy_var_1}),
        _ => unreachable!()
    }
}


fn happy_reduce_455(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 3, 122, happy_reduction_455, i)
}

fn happy_reduction_455(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn::NT105(mut happy_var_3), _, HappyAbsSyn::NT100(mut happy_var_1)) => { with_pos!(p, happy_var_3, |at| box CComma(prepend(*happy_var_1, happy_var_3), at))
        }.map(HappyAbsSyn::NT100),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_456(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_1(p, 123, happy_reduction_456, i)
}

fn happy_reduction_456(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn::NT100(mut happy_var_1) => HappyAbsSyn::NT105({vec![*happy_var_1]}),
        _ => unreachable!()
    }
}


fn happy_reduce_457(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_3(p, 123, happy_reduction_457, i)
}

fn happy_reduction_457(happy_x_3: HappyAbsSyn, happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_3, happy_x_2, happy_x_1) {
        (HappyAbsSyn::NT100(mut happy_var_3), _, HappyAbsSyn::NT105(mut happy_var_1)) => HappyAbsSyn::NT105({appended(happy_var_1, *happy_var_3)}),
        _ => unreachable!()
    }
}


fn happy_reduce_458(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_0(p, 124, happy_reduction_458(), i)
}

fn happy_reduction_458() -> HappyAbsSyn {
    HappyAbsSyn::NT124(None)
}


fn happy_reduce_459(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_1(p, 124, happy_reduction_459, i)
}

fn happy_reduction_459(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn::NT100(mut happy_var_1) => HappyAbsSyn::NT124({Some(happy_var_1)}),
        _ => unreachable!()
    }
}


fn happy_reduce_460(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_0(p, 125, happy_reduction_460(), i)
}

fn happy_reduction_460() -> HappyAbsSyn {
    HappyAbsSyn::NT124(None)
}


fn happy_reduce_461(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_1(p, 125, happy_reduction_461, i)
}

fn happy_reduction_461(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn::NT100(mut happy_var_1) => HappyAbsSyn::NT124({Some(happy_var_1)}),
        _ => unreachable!()
    }
}


fn happy_reduce_462(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_1(p, 126, happy_reduction_462, i)
}

fn happy_reduction_462(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn::NT100(mut happy_var_1) => HappyAbsSyn::NT100({happy_var_1}),
        _ => unreachable!()
    }
}


fn happy_reduce_463(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 1, 127, happy_reduction_463, i)
}

fn happy_reduction_463(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap()) {
        HappyAbsSyn::Terminal(mut happy_var_1) => { with_pos!(p, happy_var_1, move |at| unwrap_let! { CTokILit(_, i) = happy_var_1; box CIntConst(i, at) })
        }.map(HappyAbsSyn::NT127),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_464(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 1, 127, happy_reduction_464, i)
}

fn happy_reduction_464(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap()) {
        HappyAbsSyn::Terminal(mut happy_var_1) => { with_pos!(p, happy_var_1, move |at| unwrap_let! { CTokCLit(_, c) = happy_var_1; box CCharConst(c, at) })
        }.map(HappyAbsSyn::NT127),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_465(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 1, 127, happy_reduction_465, i)
}

fn happy_reduction_465(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap()) {
        HappyAbsSyn::Terminal(mut happy_var_1) => { with_pos!(p, happy_var_1, move |at| unwrap_let! { CTokFLit(_, f) = happy_var_1; box CFloatConst(f, at) })
        }.map(HappyAbsSyn::NT127),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_466(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 1, 128, happy_reduction_466, i)
}

fn happy_reduction_466(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap()) {
        HappyAbsSyn::Terminal(mut happy_var_1) => { with_pos!(p, happy_var_1, move |at| unwrap_let! { CTokSLit(_, s) = happy_var_1; box CStringLiteral(s, at) })
        }.map(HappyAbsSyn::NT128),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_467(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 2, 128, happy_reduction_467, i)
}

fn happy_reduction_467(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (HappyAbsSyn::NT129(mut happy_var_2), HappyAbsSyn::Terminal(mut happy_var_1)) => { with_pos!(p, happy_var_1, move |at| unwrap_let! { CTokSLit(_, s) = happy_var_1;
                                                    box CStringLiteral(CString::concat(prepend(s, happy_var_2)), at) })
        }.map(HappyAbsSyn::NT128),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_468(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_1(p, 129, happy_reduction_468, i)
}

fn happy_reduction_468(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn::Terminal(mut happy_var_1) => HappyAbsSyn::NT129({unwrap_let! { CTokSLit(_, s) = happy_var_1; vec![s] }}),
        _ => unreachable!()
    }
}


fn happy_reduce_469(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_2(p, 129, happy_reduction_469, i)
}

fn happy_reduction_469(happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_2, happy_x_1) {
        (HappyAbsSyn::Terminal(mut happy_var_2), HappyAbsSyn::NT129(mut happy_var_1)) => HappyAbsSyn::NT129({unwrap_let! { CTokSLit(_, s) = happy_var_2; appended(happy_var_1, s) }}),
        _ => unreachable!()
    }
}


fn happy_reduce_470(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_1(p, 130, happy_reduction_470, i)
}

fn happy_reduction_470(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn::Terminal(CTokClangC(_, ClangCTok::CVersion(mut happy_var_1))) => HappyAbsSyn::NT130({happy_var_1}),
        _ => unreachable!()
    }
}


fn happy_reduce_471(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_1(p, 131, happy_reduction_471, i)
}

fn happy_reduction_471(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn::Terminal(CTokIdent(_, mut happy_var_1)) => HappyAbsSyn::NT131({happy_var_1}),
        _ => unreachable!()
    }
}


fn happy_reduce_472(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_1(p, 131, happy_reduction_472, i)
}

fn happy_reduction_472(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn::Terminal(CTokTyIdent(_, mut happy_var_1)) => HappyAbsSyn::NT131({happy_var_1}),
        _ => unreachable!()
    }
}


fn happy_reduce_473(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_0(p, 132, happy_reduction_473(), i)
}

fn happy_reduction_473() -> HappyAbsSyn {
    HappyAbsSyn::NT132(vec![])
}


fn happy_reduce_474(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_1(p, 132, happy_reduction_474, i)
}

fn happy_reduction_474(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn::NT132(mut happy_var_1) => HappyAbsSyn::NT132({happy_var_1}),
        _ => unreachable!()
    }
}


fn happy_reduce_475(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_1(p, 133, happy_reduction_475, i)
}

fn happy_reduction_475(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn::NT132(mut happy_var_1) => HappyAbsSyn::NT132({happy_var_1}),
        _ => unreachable!()
    }
}


fn happy_reduce_476(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_2(p, 133, happy_reduction_476, i)
}

fn happy_reduction_476(happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_2, happy_x_1) {
        (HappyAbsSyn::NT132(mut happy_var_2), HappyAbsSyn::NT132(mut happy_var_1)) => HappyAbsSyn::NT132({add_vecs(happy_var_1, happy_var_2)}),
        _ => unreachable!()
    }
}


fn happy_reduce_477(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_reduce(p, 6, 134, happy_reduction_477, i)
}

fn happy_reduction_477(p: &mut Parser) {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, _, HappyAbsSyn::NT132(mut happy_var_4), _, _, _) => {            p.stack.push(HappyAbsSyn::NT132({happy_var_4})); }
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_478(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_1(p, 135, happy_reduction_478, i)
}

fn happy_reduction_478(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn::NT136(mut happy_var_1) => HappyAbsSyn::NT132({happy_var_1.map_or(vec![], |a| vec![*a])}),
        _ => unreachable!()
    }
}


fn happy_reduce_479(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_3(p, 135, happy_reduction_479, i)
}

fn happy_reduction_479(happy_x_3: HappyAbsSyn, happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_3, happy_x_2, happy_x_1) {
        (HappyAbsSyn::NT136(mut happy_var_3), _, HappyAbsSyn::NT132(mut happy_var_1)) => HappyAbsSyn::NT132({if let Some(a) = happy_var_3 { appended(happy_var_1, *a) } else { happy_var_1 }}),
        _ => unreachable!()
    }
}


fn happy_reduce_480(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_0(p, 136, happy_reduction_480(), i)
}

fn happy_reduction_480() -> HappyAbsSyn {
    HappyAbsSyn::NT136(None)
}


fn happy_reduce_481(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 1, 136, happy_reduction_481, i)
}

fn happy_reduction_481(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap()) {
        HappyAbsSyn::Terminal(CTokIdent(_, mut happy_var_1)) => { with_pos!(p, happy_var_1, |at| Some(box CAttribute(happy_var_1, vec![], at)))
        }.map(HappyAbsSyn::NT136),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_482(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 1, 136, happy_reduction_482, i)
}

fn happy_reduction_482(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap()) {
        HappyAbsSyn::Terminal(mut happy_var_1) => { with_pos!(p, happy_var_1, |at| Some(box CAttribute(Ident::internal("const".into()), vec![], at)))
        }.map(HappyAbsSyn::NT136),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_483(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 4, 136, happy_reduction_483, i)
}

fn happy_reduction_483(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, HappyAbsSyn::NT105(mut happy_var_3), _, HappyAbsSyn::Terminal(CTokIdent(_, mut happy_var_1))) => { with_pos!(p, happy_var_1, |at| Some(box CAttribute(happy_var_1, happy_var_3, at)))
        }.map(HappyAbsSyn::NT136),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_484(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_result_reduce(p, 3, 136, happy_reduction_484, i)
}

fn happy_reduction_484(p: &mut Parser) -> Res<HappyAbsSyn> {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, _, HappyAbsSyn::Terminal(CTokIdent(_, mut happy_var_1))) => { with_pos!(p, happy_var_1, |at| Some(box CAttribute(happy_var_1, vec![], at)))
        }.map(HappyAbsSyn::NT136),
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_485(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_1(p, 137, happy_reduction_485, i)
}

fn happy_reduction_485(happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_1) {
        HappyAbsSyn::NT100(mut happy_var_1) => HappyAbsSyn::NT105({vec![*happy_var_1]}),
        _ => unreachable!()
    }
}


fn happy_reduce_486(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_3(p, 137, happy_reduction_486, i)
}

fn happy_reduction_486(happy_x_3: HappyAbsSyn, happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_3, happy_x_2, happy_x_1) {
        (_, _, _) => HappyAbsSyn::NT105({vec![]}),
        _ => unreachable!()
    }
}


fn happy_reduce_487(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_3(p, 137, happy_reduction_487, i)
}

fn happy_reduction_487(happy_x_3: HappyAbsSyn, happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_3, happy_x_2, happy_x_1) {
        (_, _, _) => HappyAbsSyn::NT105({vec![]}),
        _ => unreachable!()
    }
}


fn happy_reduce_488(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_spec_reduce_3(p, 137, happy_reduction_488, i)
}

fn happy_reduction_488(happy_x_3: HappyAbsSyn, happy_x_2: HappyAbsSyn, happy_x_1: HappyAbsSyn) -> HappyAbsSyn {
    match (happy_x_3, happy_x_2, happy_x_1) {
        (HappyAbsSyn::NT100(mut happy_var_3), _, HappyAbsSyn::NT105(mut happy_var_1)) => HappyAbsSyn::NT105({appended(happy_var_1, *happy_var_3)}),
        _ => unreachable!()
    }
}


fn happy_reduce_489(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_reduce(p, 5, 137, happy_reduction_489, i)
}

fn happy_reduction_489(p: &mut Parser) {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, _, _, _, HappyAbsSyn::NT105(mut happy_var_1)) => {            p.stack.push(HappyAbsSyn::NT105({happy_var_1})); }
        _ => panic!("irrefutable pattern")
    }
}


fn happy_reduce_490(p: &mut Parser, i: isize) -> Res<Cont> {
    happy_reduce(p, 5, 137, happy_reduction_490, i)
}

fn happy_reduction_490(p: &mut Parser) {
    match (p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap(), p.stack.pop().unwrap()) {
        (_, _, _, _, HappyAbsSyn::NT105(mut happy_var_1)) => {            p.stack.push(HappyAbsSyn::NT105({happy_var_1})); }
        _ => panic!("irrefutable pattern")
    }
}


fn happy_new_token(p: &mut Parser) -> Res<Cont> {
    p.token = lex(p)?;
    let action = p.state;
    match p.token {
        CTokEof => action(p, 249, 249),
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
        CTokFloat128(_) => action(p, 203, 203),
        CTokFor(_) => action(p, 204, 204),
        CTokGeneric(_) => action(p, 205, 205),
        CTokGoto(_) => action(p, 206, 206),
        CTokIf(_) => action(p, 207, 207),
        CTokInline(_) => action(p, 208, 208),
        CTokInt(_) => action(p, 209, 209),
        CTokInt128(_) => action(p, 210, 210),
        CTokLong(_) => action(p, 211, 211),
        CTokLabel(_) => action(p, 212, 212),
        CTokNoreturn(_) => action(p, 213, 213),
        CTokNullable(_) => action(p, 214, 214),
        CTokNonnull(_) => action(p, 215, 215),
        CTokRegister(_) => action(p, 216, 216),
        CTokRestrict(_) => action(p, 217, 217),
        CTokReturn(_) => action(p, 218, 218),
        CTokShort(_) => action(p, 219, 219),
        CTokSigned(_) => action(p, 220, 220),
        CTokSizeof(_) => action(p, 221, 221),
        CTokStatic(_) => action(p, 222, 222),
        CTokStaticAssert(_) => action(p, 223, 223),
        CTokStruct(_) => action(p, 224, 224),
        CTokSwitch(_) => action(p, 225, 225),
        CTokTypedef(_) => action(p, 226, 226),
        CTokTypeof(_) => action(p, 227, 227),
        CTokThread(_) => action(p, 228, 228),
        CTokUnion(_) => action(p, 229, 229),
        CTokUnsigned(_) => action(p, 230, 230),
        CTokVoid(_) => action(p, 231, 231),
        CTokVolatile(_) => action(p, 232, 232),
        CTokWhile(_) => action(p, 233, 233),
        CTokCLit(_, _) => action(p, 234, 234),
        CTokILit(_, _) => action(p, 235, 235),
        CTokFLit(_, _) => action(p, 236, 236),
        CTokSLit(_, _) => action(p, 237, 237),
        CTokIdent(_, _) => action(p, 238, 238),
        CTokTyIdent(_, _) => action(p, 239, 239),
        CTokGnuC(_, GnuCTok::Attr) => action(p, 240, 240),
        CTokGnuC(_, GnuCTok::Ext) => action(p, 241, 241),
        CTokGnuC(_, GnuCTok::ComplexReal) => action(p, 242, 242),
        CTokGnuC(_, GnuCTok::ComplexImag) => action(p, 243, 243),
        CTokGnuC(_, GnuCTok::VaArg) => action(p, 244, 244),
        CTokGnuC(_, GnuCTok::Offsetof) => action(p, 245, 245),
        CTokGnuC(_, GnuCTok::TyCompat) => action(p, 246, 246),
        CTokClangC(_, ClangCTok::ConvertVector) => action(p, 247, 247),
        CTokClangC(_, ClangCTok::CVersion(_)) => action(p, 248, 248),
    }
}

fn happy_error_<T>(p: &mut Parser, _: isize) -> Res<T> {
    happy_error(p)
}


pub fn translation_unit(p: &mut Parser) -> Res<Box<CTranslUnit>> {
    let x = happy_parse(p, action_0)?;
    match x {
        HappyAbsSyn::NT7(z) => Ok(z),
        _ => unreachable!()
    }
}

pub fn external_declaration(p: &mut Parser) -> Res<Box<CExtDecl>> {
    let x = happy_parse(p, action_1)?;
    match x {
        HappyAbsSyn::NT9(z) => Ok(z),
        _ => unreachable!()
    }
}

pub fn statement(p: &mut Parser) -> Res<Box<CStat>> {
    let x = happy_parse(p, action_2)?;
    match x {
        HappyAbsSyn::NT12(z) => Ok(z),
        _ => unreachable!()
    }
}

pub fn expression(p: &mut Parser) -> Res<Box<CExpr>> {
    let x = happy_parse(p, action_3)?;
    match x {
        HappyAbsSyn::NT100(z) => Ok(z),
        _ => unreachable!()
    }
}

#[inline]
fn rev_vec<T>(mut a: Vec<T>) -> Vec<T> {
    a.reverse();
    a
}

#[inline]
fn prepend<T>(t: T, mut a: Vec<T>) -> Vec<T> {
    a.insert(0, t);
    a
}

#[inline]
fn add_vecs<T>(mut a: Vec<T>, mut b: Vec<T>) -> Vec<T> {
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
fn lift_type_quals(quals: Vec<CTypeQual>) -> Vec<CDeclSpec> {
    map(CTypeQual, quals)
}

#[inline]
fn lift_attrs(attrs: Vec<CAttribute<NodeInfo>>) -> Vec<CDeclSpec> {
    map(|attr| CTypeQual(CAttrQual(box attr)), attrs)
}

#[inline]
fn append_obj_attrs(mut new_attrs: Vec<CAttr>, mut declr: Box<CDeclr>) -> Box<CDeclr> {
    declr.3.append(&mut new_attrs);
    declr
}

fn add_trailing_attrs(mut declspecs: Vec<CDeclSpec>, mut new_attrs: Vec<CAttr>) -> Vec<CDeclSpec> {
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
        declspecs.append(&mut lift_attrs(new_attrs));
    }
    declspecs
}

fn happy_error<T>(p: &mut Parser) -> Res<T> {
    parse_error(p)
}
// Original location: "templates/GenericTemplate.hs", line 1

// -----------------------------------------------------------------------------
// Some convenient typedefs

use std::mem;

const ERROR_TOK: isize = 1;

enum Cont {
    Loop(isize, isize),
    NewToken,
    Accept(isize),
}

// Types to be defined by the user: Token, Error, State

type Res<T> = Result<T, Error>;
type Action = fn(&mut Parser, isize, isize) -> Res<Cont>;
type Stack = Vec<HappyAbsSyn>;

pub struct Parser {
    pub user: State,
    token: Token,
    stack: Stack,
    state: Action,
    states: Vec<Action>,
}

impl Parser {
    pub fn exec<F, T>(initial_state: State, do_parse: F) -> Res<(State, T)>
        where F: FnOnce(&mut Parser) -> Res<T>
    {
        let mut parser = Parser {
            user: initial_state,
            token: EOF_TOK,
            state: happy_invalid,
            states: vec![],
            stack: vec![]
        };
        let res = do_parse(&mut parser)?;
        Ok((parser.user, res))
    }
}

fn happy_invalid(_: &mut Parser, _: isize, _: isize) -> Res<Cont> {
    panic!("parser not initialized correctly")
}

// -----------------------------------------------------------------------------
// Starting the parse

fn happy_parse(p: &mut Parser, start_state: Action) -> Res<HappyAbsSyn> {
    p.state = start_state;
    p.states.clear();
    p.stack.clear();
    p.stack.push(HappyAbsSyn::ErrorToken(0));
    let mut cont = Cont::NewToken;

    loop {
        cont = match cont {
            Cont::Loop(i, j) => (p.state)(p, i, j)?,
            Cont::NewToken => happy_new_token(p)?,
            Cont::Accept(j) => return happy_accept(p, j),
        }
    }
}

// -----------------------------------------------------------------------------
// Accepting the parse
//
// If the current token is ERROR_TOK, it means we've just accepted a partial
// parse (a %partial parser).  We must ignore the saved token on the top of
// the stack in this case.

fn happy_accept(p: &mut Parser, j: isize) -> Res<HappyAbsSyn> {
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

fn happy_shift(p: &mut Parser, new_state: Action, i: isize) -> Res<Cont> {
    match i {
        ERROR_TOK => {
            let x = p.stack.pop().unwrap();
            let i = match x {
                HappyAbsSyn::ErrorToken(i) => i,
                _ => unreachable!(),
            };

            p.states.push(new_state);
            p.state = new_state;
            Ok(Cont::Loop(i, i))
        }
        _ => {
            p.states.push(p.state);
            p.stack.push(HappyAbsSyn::Terminal(mem::replace(&mut p.token, EOF_TOK)));
            p.state = new_state;
            Ok(Cont::NewToken)
        },
    }
}

// -----------------------------------------------------------------------------
// happyReduce is specialised for the common cases.

fn happy_spec_reduce_0(p: &mut Parser, nt: isize, val: HappyAbsSyn, j: isize) -> Res<Cont> {
    match j {
        ERROR_TOK => happy_fail(p, ERROR_TOK),
        j => {
            p.states.push(p.state);
            p.stack.push(val);
            Ok(Cont::Loop(nt, j))
        },
    }
}

fn happy_spec_reduce_1(p: &mut Parser, nt: isize,
                       reducer: fn(HappyAbsSyn) -> HappyAbsSyn, j: isize) -> Res<Cont> {
    match j {
        ERROR_TOK => happy_fail(p, ERROR_TOK),
        j => {
            let v1 = p.stack.pop().unwrap();
            p.state = *p.states.last().unwrap();
            let val = reducer(v1);
            p.stack.push(val);
            Ok(Cont::Loop(nt, j))
        }
    }
}

fn happy_spec_reduce_2(p: &mut Parser, nt: isize,
                       reducer: fn(HappyAbsSyn, HappyAbsSyn) -> HappyAbsSyn, j: isize) -> Res<Cont> {
    match j {
        ERROR_TOK => happy_fail(p, ERROR_TOK),
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

fn happy_spec_reduce_3(p: &mut Parser, nt: isize,
                       reducer: fn(HappyAbsSyn, HappyAbsSyn, HappyAbsSyn) -> HappyAbsSyn,
                       j: isize) -> Res<Cont> {
    match j {
        ERROR_TOK => happy_fail(p, ERROR_TOK),
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

fn happy_reduce(p: &mut Parser, k: isize, nt: isize, reducer: fn(&mut Parser), j: isize) -> Res<Cont> {
    match j {
        ERROR_TOK => happy_fail(p, ERROR_TOK),
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

fn happy_result_reduce(p: &mut Parser, k: isize, nt: isize,
                       reducer: fn(&mut Parser) -> Res<HappyAbsSyn>, j: isize) -> Res<Cont> {
    match j {
        ERROR_TOK => happy_fail(p, ERROR_TOK),
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

fn happy_goto(p: &mut Parser, action: Action, j: isize) -> Res<Cont> {
    p.state = action;
    action(p, j, j)
}

// -----------------------------------------------------------------------------
// Error recovery (ERROR_TOK is the error token)

fn happy_fail(p: &mut Parser, i: isize) -> Res<Cont> {
    match i {
        ERROR_TOK if p.stack.len() > 0 => happy_error_(p, i),
        i => {
            p.stack.push(HappyAbsSyn::ErrorToken(i));
            (p.state)(p, ERROR_TOK, ERROR_TOK)
        },
    }
}

// end of Happy Template.
