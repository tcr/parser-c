// Original file: "RList.hs"
// File auto-generated using Corollary.

#[derive(Clone)]
pub struct Reversed<T>(pub T);

pub fn snoc<T>(Reversed(mut xs): Reversed<Vec<T>>, x: T) -> Reversed<Vec<T>> {
    Reversed({ xs.insert(0, x); xs })
}

pub mod RList {
    use super::Reversed;
    use corollary_support::*;

    pub fn empty<a>() -> Reversed<Vec<a>> {
        Reversed(vec![])
    }

    pub fn singleton<a>(x: a) -> Reversed<Vec<a>> {
        Reversed(vec![x])
    }

    pub fn rappend<a>(Reversed(xs): Reversed<Vec<a>>, mut ys: Vec<a>) -> Reversed<Vec<a>> {
        ys.reverse();
        Reversed(__op_addadd(ys, xs))
    }

    pub fn appendr<a>(mut xs: Vec<a>, Reversed(ys): Reversed<Vec<a>>) -> Reversed<Vec<a>> {
        xs.reverse();
        Reversed(__op_addadd(ys, xs))
    }

    pub fn rappendr<a>(Reversed(xs): Reversed<Vec<a>>,
                    Reversed(ys): Reversed<Vec<a>>)
                    -> Reversed<Vec<a>> {
        Reversed(__op_addadd(ys, xs))
    }

    pub fn rmap<a, b>(f: fn(a) -> b, Reversed(xs): Reversed<Vec<a>>) -> Reversed<Vec<b>> {
        Reversed(__map!(f, xs))
    }

    pub fn reverse<a>(Reversed(mut xs): Reversed<Vec<a>>) -> Vec<a> {
        xs.reverse();
        xs
    }

    pub fn viewr<a>(_0: Reversed<Vec<a>>) -> (Reversed<Vec<a>>, a) {
        let mut xs = _0.0;
        if xs.is_empty() {
            panic!("viewr: empty RList");
        } else {
            let x = xs.remove(0);
            (Reversed(xs), x)
        }
    }

    // temporary API
    pub fn get_mut<a>(_0: &mut Reversed<Vec<a>>, idx: usize) -> Option<&mut a> {
        _0.0.get_mut(idx)
    }
}
