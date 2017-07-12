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

    pub fn empty<T>() -> Reversed<Vec<T>> {
        Reversed(vec![])
    }

    pub fn singleton<T>(x: T) -> Reversed<Vec<T>> {
        Reversed(vec![x])
    }

    pub fn rappend<T>(Reversed(xs): Reversed<Vec<T>>, mut ys: Vec<T>) -> Reversed<Vec<T>> {
        ys.reverse();
        Reversed(__op_addadd(ys, xs))
    }

    pub fn appendr<T>(mut xs: Vec<T>, Reversed(ys): Reversed<Vec<T>>) -> Reversed<Vec<T>> {
        xs.reverse();
        Reversed(__op_addadd(ys, xs))
    }

    pub fn rappendr<T>(Reversed(xs): Reversed<Vec<T>>,
                    Reversed(ys): Reversed<Vec<T>>)
                    -> Reversed<Vec<T>> {
        Reversed(__op_addadd(ys, xs))
    }

    pub fn rmap<T, U>(f: fn(T) -> U, Reversed(xs): Reversed<Vec<T>>) -> Reversed<Vec<U>> {
        Reversed(__map!(f, xs))
    }

    pub fn reverse<T>(Reversed(mut xs): Reversed<Vec<T>>) -> Vec<T> {
        xs.reverse();
        xs
    }

    pub fn viewr<T>(_0: Reversed<Vec<T>>) -> (Reversed<Vec<T>>, T) {
        let mut xs = _0.0;
        if xs.is_empty() {
            panic!("viewr: empty RList");
        } else {
            let x = xs.remove(0);
            (Reversed(xs), x)
        }
    }

    // temporary API
    pub fn get_mut<T>(_0: &mut Reversed<Vec<T>>, idx: usize) -> Option<&mut T> {
        _0.0.get_mut(idx)
    }
}
