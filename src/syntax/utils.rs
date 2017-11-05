// Original file: "Utils.hs"
// File auto-generated using Corollary.

use corollary_support::*;

use syntax::ast::*;
use data::ident::Ident;

pub fn getSubStmts(stat: CStat) -> Vec<CStat> {
    match stat {
        CLabel(_, box s, _, _) => vec![s],
        CCase(_, box s, _) => vec![s],
        CCases(_, _, box s, _) => vec![s],
        CDefault(box s, _) => vec![s],
        CExpr(_, _) => vec![],
        CCompound(_, body, _) => __concatMap!(compoundSubStmts, body),
        CIf(_, box sthen, selse, _) => {
            let sthen2 = sthen.clone();
            selse
                .map(|s| vec![sthen, *s])
                .unwrap_or(vec![sthen2])
        },
        CSwitch(_, box s, _) => vec![s],
        CWhile(_, box s, _, _) => vec![s],
        CFor(_, _, _, box s, _) => vec![s],
        CGoto(_, _) => vec![],
        CGotoPtr(_, _) => vec![],
        CCont(_) => vec![],
        CBreak(_) => vec![],
        CReturn(_, _) => vec![],
        CAsm(_, _) => vec![],
    }
}

pub fn mapSubStmts(_0: fn(CStat) -> bool, _1: fn(CStat) -> CStat, _2: CStat) -> CStat {
    match (_0, _1, _2) {
        (stop, _, ref s) if stop(s.clone()) => {
            s.clone()
        }
        (stop, f, CLabel(i, box s, attrs, ni)) => f((CLabel(i, box (mapSubStmts(stop, f, s)), attrs, ni))),
        (stop, f, CCase(e, box s, ni)) => f((CCase(e, box (mapSubStmts(stop, f, s)), ni))),
        (stop, f, CCases(e1, e2, box s, ni)) => f((CCases(e1, e2, box (mapSubStmts(stop, f, s)), ni))),
        (stop, f, CDefault(box s, ni)) => f((CDefault(box (mapSubStmts(stop, f, s)), ni))),
        (stop, f, CCompound(ls, body, ni)) => {
            f((CCompound(ls, (__map!(|x| { mapBlockItemStmts(stop, f, x) }, body)), ni)))
        }
        (stop, f, CIf(e, box sthen, selse, ni)) => {
            f((CIf(e,
                   box (mapSubStmts(stop, f, sthen)),
                   selse.map(|x| { box mapSubStmts(stop, f, *x) }),
                   ni)))
        }
        (stop, f, CSwitch(e, box s, ni)) => f((CSwitch(e, box (mapSubStmts(stop, f, s)), ni))),
        (stop, f, CWhile(e, box s, isdo, ni)) => f((CWhile(e, box (mapSubStmts(stop, f, s)), isdo, ni))),
        (stop, f, CFor(i, t, a, box s, ni)) => f((CFor(i, t, a, box (mapSubStmts(stop, f, s)), ni))),
        (_, f, s) => f(s),
    }
}

pub fn mapBlockItemStmts(_0: fn(CStat) -> bool,
                         _1: fn(CStat) -> CStat,
                         _2: CBlockItem)
                         -> CBlockItem {
    match (_0, _1, _2) {
        (stop, f, CBlockStmt(s)) => CBlockStmt((mapSubStmts(stop, f, s))),
        (_, _, bi) => bi,
    }
}

pub fn compoundSubStmts(item: CBlockItem) -> Vec<CStat> {
    match item {
        CBlockStmt(s) => vec![s],
        CBlockDecl(_) => vec![],
        CNestedFunDef(_) => vec![],
    }
}

pub fn getLabels(stat: CStat) -> Vec<Ident> {
    match stat {
        CLabel(l, box s, _, _) => __op_concat(l, getLabels(s)),
        CCompound(ls, body, _) => {
            __op_forwardslash(
                __concatMap!(|x| { __concatMap!(getLabels, compoundSubStmts(x)) }, body),
                ls)
        }
        stmt => __concatMap!(getLabels, (getSubStmts(stmt))),
    }
}
