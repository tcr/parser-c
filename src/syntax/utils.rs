// Original file: "Utils.hs"
// File auto-generated using Corollary.

use syntax::ast::*;
use data::ident::Ident;

pub fn getSubStmts(stat: &CStat) -> Vec<&CStat> {
    match *stat {
        CLabel(_, box ref s, _, _) => vec![s],
        CCase(_, box ref s, _) => vec![s],
        CCases(_, _, box ref s, _) => vec![s],
        CDefault(box ref s, _) => vec![s],
        CCompound(_, ref body, _) => body.iter().filter_map(compoundSubStmt).collect(),
        CIf(_, box ref sthen, ref selse, _) => match *selse {
            None => vec![sthen],
            Some(ref selse) => vec![sthen, selse]
        },
        CSwitch(_, box ref s, _) => vec![s],
        CWhile(_, box ref s, _, _) => vec![s],
        CFor(_, _, _, box ref s, _) => vec![s],
        CGoto(..) | CGotoPtr(..) | CCont(..) | CBreak(..) | CReturn(..) |
        CAsm(..) | CExpr(..) => vec![],
    }
}

pub fn mapSubStmts<S, F>(stop: S, f: F, x: CStat) -> CStat
    where S: Fn(&CStat) -> bool, F: Fn(CStat) -> CStat
{
    let f = &f;
    let stop = &stop;
    if stop(&x) { return x; }
    match x {
        CLabel(i, box s, attrs, ni) => f(CLabel(i, box mapSubStmts(stop, f, s), attrs, ni)),
        CCase(e, box s, ni) => f(CCase(e, box mapSubStmts(stop, f, s), ni)),
        CCases(e1, e2, box s, ni) => f(CCases(e1, e2, box mapSubStmts(stop, f, s), ni)),
        CDefault(box s, ni) => f(CDefault(box mapSubStmts(stop, f, s), ni)),
        CCompound(ls, body, ni) => {
            f(CCompound(ls, body.into_iter().map(|x| mapBlockItemStmts(stop, f, x)).collect(), ni))
        }
        CIf(e, box sthen, selse, ni) => {
            f(CIf(e,
                  box mapSubStmts(stop, f, sthen),
                  selse.map(|x| box mapSubStmts(stop, f, *x)),
                  ni))
        }
        CSwitch(e, box s, ni) => f(CSwitch(e, box mapSubStmts(stop, f, s), ni)),
        CWhile(e, box s, isdo, ni) => f(CWhile(e, box mapSubStmts(stop, f, s), isdo, ni)),
        CFor(i, t, a, box s, ni) => f(CFor(i, t, a, box mapSubStmts(stop, f, s), ni)),
        s => f(s),
    }
}

pub fn mapBlockItemStmts<S, F>(stop: S, f: F, bi: CBlockItem) -> CBlockItem
    where S: Fn(&CStat) -> bool, F: Fn(CStat) -> CStat
{
    match bi {
        CBlockStmt(s) => CBlockStmt(mapSubStmts(stop, f, s)),
        _ => bi,
    }
}

pub fn compoundSubStmt(item: &CBlockItem) -> Option<&CStat> {
    match *item {
        CBlockStmt(ref s) => Some(s),
        CBlockDecl(..) | CNestedFunDef(..) => None,
    }
}

pub fn getLabels(stat: &CStat) -> Vec<Ident> {
    fn inner(v: &mut Vec<Ident>, stat: &CStat) {
        match *stat {
            CLabel(ref l, box ref s, _, _) => {
                v.push(l.clone());
                inner(v, s);
            }
            CCompound(ref ls, ref body, _) => {
                let mut labels = vec![];
                for stmt in body {
                    if let Some(stmt) = compoundSubStmt(stmt) {
                        inner(&mut labels, stmt);
                    }
                }
                for l in ls {
                    labels.remove_item(l);
                }
                v.append(&mut labels);
            }
            _ => {
                for substmt in getSubStmts(stat) {
                    inner(v, substmt)
                }
            }
        }
    }
    let mut labels = vec![];
    inner(&mut labels, stat);
    labels
}
