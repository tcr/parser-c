#![allow(non_snake_case)]
#![allow(unused_variables)]
#![allow(non_camel_case_types)]

extern crate num;

use num::ToPrimitive;
use std;
use std::str::FromStr;

pub trait OpAddable {
    fn add(self, right: Self) -> Self;
}

pub fn __op_addadd<A: OpAddable>(left: A, right: A) -> A {
    OpAddable::add(left, right)
}

impl<A> OpAddable for Vec<A> {
    fn add(mut self, right: Self) -> Self {
        self.extend(right);
        self
    }
}


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


#[macro_export]
macro_rules! __assign {
    ($left: expr, {
        $($field_name:ident: $field_type:expr),+ $(,)*
    }) => {
        // TODO
        {
            let mut left = $left;
            $( left.$field_name = $field_type; )+
            left
        }
    }
}



#[derive(Debug)]
pub enum ExitCode {
    ExitSuccess,
    ExitFailure(isize),
}
pub use self::ExitCode::*;

pub fn __op_forwardslash<A, B>(left: A, right: B) -> B {
    // TODO
    right
}

pub fn __op_assign_div(l: isize, r: isize) -> isize {
    l / r
}

pub fn __break_str<F: Fn(char) -> bool>(cond: F, input: String) -> (String, String) {
    let mut left = vec![];
    let mut right = vec![];
    for item in input.chars() {
        if right.is_empty() && cond(item) {
            left.push(item);
        } else {
            right.push(item);
        }
    }
    (left.into_iter().collect(), right.into_iter().collect())
}

pub fn any<T: Clone, F: Fn(T) -> bool>(cond: F, input: Vec<T>) -> bool {
    input.iter()
        .any(|x| cond(x.clone()))
}

pub fn isJust<T>(input: Option<T>) -> bool {
    input.is_some()
}

pub fn tail_str(input: String) -> String {
    input.chars().skip(1).collect()
}

pub fn hasExtension(fp: FilePath) -> bool {
    // TODO
    false
}

pub fn replaceExtension(fp: FilePath, ext: &str) -> FilePath {
    // TODO
    fp
}

pub fn addExtension(fp: FilePath, ext: &str) -> FilePath {
    // TODO
    fp
}

pub fn takeWhile_str<F: Fn(char) -> bool>(cond: F, input: String) -> String {
    let mut left = vec![];
    for item in input.chars() {
        if cond(item.clone()) {
            left.push(item);
        } else {
            return left.into_iter().collect();
        }
    }
    left.into_iter().collect()
}

pub fn drop_str(len: isize, input: String) -> String {
    input.chars().skip(len as usize).collect()
}

// Map stuff


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


// IO fns

#[allow(dead_code)]
#[derive(Clone)]
pub struct FilePath {
    pub path: String,
}

impl From<String> for FilePath {
    fn from(value: String) -> Self {
        FilePath {
            path: value
        }
    }
}

impl From<FilePath> for String {
    fn from(value: FilePath) -> Self {
        value.path
    }
}

impl ToString for FilePath {
    fn to_string(&self) -> String {
        return self.path.clone()
    }
}

pub struct FileHandle {
    pub path: (),
}

pub fn openTempFile(t: FilePath, template: FilePath) -> (FilePath, FileHandle) {
    // TODO
    (FilePath {
        path: "".to_string()
    }, FileHandle {
        path: ()
    })
}

pub fn hClose(h: FileHandle) {
    // TODO
}

pub fn removeFile(p: FilePath) {
    // TODO
}

pub fn getTemporaryDirectory() -> FilePath {
    // TODO
    FilePath {
        path: "TODO".to_string()
    }
}

pub fn takeFileName(h: FilePath) -> FilePath {
    // TODO
    h
}

// TODO what do we do here:

pub fn maybe<A, B, F: Fn(A) -> B>(default_: B, method: F, maybe: Option<A>) -> B {
    maybe.map(|x| method(x)).unwrap_or(default_)
}

// Array things

pub fn __op_array_index<T>(mut arr: Vec<T>, idx: isize) -> T {
    arr.remove(idx as usize)
}
