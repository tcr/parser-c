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


pub mod List {
    pub fn reverse<A>(mut input: Vec<A>) -> Vec<A> {
        input.reverse();
        input
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


use std::fmt::Display;
pub fn show<A: Display>(a: A) -> String {
    format!("{}", a)
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

pub fn null<T>(input: Vec<T>) -> bool {
    input.is_empty()
}

pub fn lines(input: String) -> Vec<String> {
    input.lines().map(|x| x.to_string()).collect()
}

pub fn unlines(input: Vec<String>) -> String {
    input.join("\n")
}

pub fn ord(input: char) -> isize {
    input as isize
}

pub fn isAscii(input: char) -> bool {
    // TODO
    false
}

pub fn isPrint(input: char) -> bool {
    // TODO
    false
}

pub fn isOctDigit(input: char) -> bool {
    // TODO
    false
}

pub fn isDigit(input: char) -> bool {
    // TODO
    false
}



pub fn init_str(input: String) -> String {
    let mut v: Vec<_> = input.chars().collect();
    v.pop();
    v.into_iter().collect()
}

pub fn tail_str(input: String) -> String {
    input.chars().skip(1).collect()
}

pub fn fst<A, B>(input: (A, B)) -> A {
    input.0
}

pub fn flip<A, B, C, F: Fn(A, B) -> C>(input: F, b: B, a: A) -> C {
    input(a, b)
}

pub fn take_str(len: isize, input: String) -> String {
    input.chars().take(len as usize).collect()
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


pub fn fromIntegral(left: isize) -> isize {
    left
}

pub fn drop_str(len: isize, input: String) -> String {
    input.chars().skip(len as usize).collect()
}

pub fn chr(input: isize) -> char {
    input as u8 as char
}

// bits

pub fn setBit(left: isize, right: isize) -> isize {
    left | (1 << right)
}

pub fn clearBit(left: isize, right: isize) -> isize {
    left & !(1 << right)
}

pub fn testBit(left: isize, right: isize) -> bool {
    left & (1 << right) != 0
}


// ShowS, ReadS

pub trait ShowS {
    fn show_s(&self, String) -> String;
}


pub struct showOct(pub isize);
impl ShowS for showOct {
    fn show_s(&self, input: String) -> String {
        format!("{:o}{}", self.0, input)
    }
}

pub struct showHex(pub isize);
impl ShowS for showHex {
    fn show_s(&self, input: String) -> String {
        format!("{:x}{}", self.0, input)
    }
}

pub struct showString(pub String);
impl ShowS for showString {
    fn show_s(&self, input: String) -> String {
        format!("{}{}", self.0, input)
    }
}


pub trait ReadS<A> {
    fn read_s(&self) -> Vec<(A, String)>;
    fn map<F: Fn((isize, String)) -> (isize, String)>(self, f: F) -> Self where Self: Sized {
        // TODO
        self
    }
}

// TODO
use std::fmt;
impl<A> fmt::Display for ReadS<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "...")
    }
}

pub struct readHex(pub String);
impl ReadS<isize> for readHex {
    fn read_s(&self) -> Vec<(isize, String)> {
        // TODO
        vec![]
    }
}
impl ReadS<char> for readHex {
    fn read_s(&self) -> Vec<(char, String)> {
        // TODO
        vec![]
    }
}

pub struct readOct(pub String);
impl ReadS<isize> for readOct {
    fn read_s(&self) -> Vec<(isize, String)> {
        // TODO
        vec![]
    }
}
impl ReadS<char> for readOct {
    fn read_s(&self) -> Vec<(char, String)> {
        // TODO
        vec![]
    }
}

pub struct readDec(pub String);
impl ReadS<isize> for readDec {
    fn read_s(&self) -> Vec<(isize, String)> {
        if let Ok(left) = isize::from_str(&self.0) {
            let right = self.0.chars().skip_while(|x| x.is_digit(10)).collect();
            vec![(left, right)]
        } else {
            vec![]
        }
    }
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
