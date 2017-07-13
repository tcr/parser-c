#![allow(non_snake_case)]
#![allow(unused_variables)]
#![allow(non_camel_case_types)]

pub trait OpConcatable {
    type Item;
    fn concat(self, right: Self::Item) -> Self;
}
pub fn __op_concat<A: OpConcatable>(left: A::Item, right: A) -> A {
    OpConcatable::concat(right, left)
}
impl<A> OpConcatable for Vec<A> {
    type Item = A;
    fn concat(mut self, right: Self::Item) -> Self {
        self.insert(0, right);
        self
    }
}


pub fn __op_forwardslash<A, B>(left: A, right: B) -> B {
    // TODO delete first occurrence of every element in B from A
    right
}

#[macro_export]
macro_rules! __map {
    ($fn: expr) => {
        //TODO reject this
        panic!("need two arguments for map")
    };
    ($fn: expr, $target: expr) => {
        $target.into_iter()
            .map($fn)
            .collect::<Vec<_>>()
    }
}

#[macro_export]
macro_rules! __concatMap {
    ($fn: expr, $target: expr) => {
        $target.into_iter()
            .flat_map($fn)
            .collect::<Vec<_>>()
    }
}
