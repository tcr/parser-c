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

#[derive(Clone, Debug)]
pub enum Either<A, B> {
    Left(A),
    Right(B)
}
pub use self::Either::*;

impl<A, B> Either<A, B> {
    pub fn map<C, F: Fn(B) -> C>(self, f: F) -> Either<A, C> {
        match self {
            Either::Right(b) => Either::Right(f(b)),
            Either::Left(a) => Either::Left(a),
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

pub trait Lengthable {
    fn get_len(&self) -> isize;
}
pub fn length<A: Lengthable>(left: A) -> isize {
    Lengthable::get_len(&left)
}
impl<T> Lengthable for Vec<T> {
    fn get_len(&self) -> isize {
        self.len() as isize
    }
}

pub trait Bindable<T> {
    fn bind_it(self, right: T) -> Self;
}
pub fn __op_bind<A: Bindable<B>, B>(left: A, b: B) -> A {
    Bindable::bind_it(left, b)
}
impl<T: Display> Bindable<T> for String {
    fn bind_it(mut self, right: T) -> Self {
        // TODO
        self.push_str(&format!("{}", right));
        self
    }
}


pub fn __op_forwardslash<A, B>(left: A, right: B) -> B {
    // TODO
    right
}

pub fn __op_dollarnot<A, B>(left: A, right: B) -> B {
    // TODO
    right
}


pub fn union<A: PartialEq>(mut left: Vec<A>, right: Vec<A>) -> Vec<A> {
    for item in right {
        if left.iter().position(|x| *x == item).is_none() {
            left.push(item);
        }
    }
    left
}

pub fn toInteger<T: Display>(left: T) -> isize {
    // TODO
    0
}

pub fn fromInteger(left: isize) -> String {
    // TODO
    "".to_string()
}

pub fn shiftL(l: isize, r: isize) -> isize {
    l << r
}

pub fn shiftR(l: isize, r: isize) -> isize {
    l >> r
}

pub fn fromEnum<A: ToPrimitive>(arg: A) -> isize {
    arg.to_isize().unwrap()
}

pub fn __op_dotted_and(l: isize, r: isize) -> isize {
    l & r
}

pub fn __op_dotted_or(l: isize, r: isize) -> isize {
    l | r
}

pub fn __op_assign_div(l: isize, r: isize) -> isize {
    l / r
}

pub fn __op_tuple2<A, B>(left: A, right: B) -> (A, B) {
    (left, right)
}

pub fn __op_power(l: isize, r: isize) -> isize {
    //TODO
    l
}

pub fn __mod(l: isize, r: isize) -> isize {
    // TODO
    l
}

pub fn not(left: bool) -> bool {
    !left
}

pub fn __break<T: Clone, F: Fn(T) -> bool>(cond: F, input: Vec<T>) -> (Vec<T>, Vec<T>) {
    let mut left = vec![];
    let mut right = vec![];
    for item in input.into_iter() {
        if right.is_empty() && cond(item.clone()) {
            left.push(item);
        } else {
            right.push(item);
        }
    }
    (left, right)
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

pub fn head(input: Vec<char>) -> char {
    input[0]
}

pub fn head_str(input: String) -> char {
    input.chars().nth(0).unwrap()
}

pub fn init(mut input: Vec<char>) -> Vec<char> {
    input.pop();
    input
}

pub fn init_str(input: String) -> String {
    let mut v: Vec<_> = input.chars().collect();
    v.pop();
    v.into_iter().collect()
}

pub fn tail(input: Vec<char>) -> Vec<char> {
    input[1..].to_vec()
}

pub fn tail_str(input: String) -> String {
    input.chars().skip(1).collect()
}

pub fn fst<A, B>(input: (A, B)) -> A {
    input.0
}

pub fn snd<A, B>(input: (A, B)) -> B {
    input.1
}

pub fn flip<A, B, C, F: Fn(A, B) -> C>(input: F, b: B, a: A) -> C {
    input(a, b)
}

pub fn take(len: isize, input: Vec<String>) {
    // TODO
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


pub fn takeWhile<T: Clone, F: Fn(T) -> bool>(cond: F, input: Vec<T>) -> Vec<T> {
    let mut left = vec![];
    for item in input.into_iter() {
        if cond(item.clone()) {
            left.push(item);
        } else {
            return left
        }
    }
    left
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

pub fn drop<T>(len: isize, mut input: Vec<T>) -> Vec<T> {
    for _ in 0..len {
        input.remove(0);
    }
    input
}

pub fn drop_str(len: isize, input: String) -> String {
    input.chars().skip(len as usize).collect()
}

pub fn dropWhile<F: Fn(char) -> bool>(cond: F, input: String) -> String {
    let mut out = vec![];
    for item in input.chars() {
        if cond(item.clone()) && out.is_empty() {
            // skip
        } else {
            out.push(item);
        }
    }
    out.into_iter().collect()
}

pub fn span<F: Fn(char) -> bool>(cond: F, input: String) -> (String, String) {
    let mut left = vec![];
    let mut right = vec![];
    for item in input.chars() {
        if cond(item.clone()) && right.is_empty() {
            left.push(item);
        } else {
            right.push(item);
        }
    }
    (left.into_iter().collect(), right.into_iter().collect())
}

pub fn chr(input: isize) -> char {
    input as u8 as char
}

pub fn id<A>(input: A) -> A {
    input
}

pub fn __boxed_chars(input: String) -> Box<[char]> {
    input.chars().collect::<Vec<_>>().into_boxed_slice()
}

pub fn __boxed_slice<T: Sized>(input: Vec<T>) -> Box<[T]> {
    input.into_boxed_slice()
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






// Monads

pub fn __return<A: Into<B>, B>(left: A) -> B {
    left.into()
}
// pub trait Functor {
//   fmap = liftM
// }

// pub trait Applicative P where
//   pure = return
//   (<*>) = ap

// pub trait Monad<P> {
//   fn ret(Self) -> Self;
//   fn bind(Self) -> Self;
//   fn fail(m) -> Self;
// }






















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






// BSC


// Char8
//TODO make this deal with u8's, not chars
pub mod BSC {
    pub fn head(input: Vec<u8>) -> char {
        input[0] as char
    }

    pub fn tail(input: Vec<u8>) -> Vec<u8> {
        if input.len() > 0 {
            input[1..].to_vec()
        } else {
            vec![]
        }
    }

    pub fn null(input: Vec<u8>) -> bool {
        input.is_empty()
    }

    pub fn lines(input: Vec<u8>) -> Vec<Vec<u8>> {
        //TODO
        vec![]
    }

    pub fn pack(input: String) -> Vec<u8> {
        input.chars().map(|x| x as u8).collect()
    }

    pub fn unpack(input: Vec<u8>) -> String {
        input.into_iter().map(|x| x as char).collect()
    }

    pub fn take(len: isize, input: Vec<u8>) -> Vec<u8> {
        input.into_iter().take(len as usize).collect()
    }
}

// ByteString
pub mod BSW {
    use FilePath;

    pub fn null(input: Vec<u8>) -> bool {
        input.is_empty()
    }

    pub fn head(input: Vec<u8>) -> u8 {
        input[0]
    }

    pub fn tail(input: Vec<u8>) -> Vec<u8> {
        if input.len() > 0 {
            input[1..].to_vec()
        } else {
            vec![]
        }
    }

    pub fn readFile(f: FilePath) -> Vec<u8> {
        use std::fs::File;
        use std::io::Read;

        // TODO
        let mut items = vec![];
        File::open(f.path).unwrap().read_to_end(&mut items).unwrap();
        items
    }
}

pub type ByteString = Vec<u8>;

pub type Word8 = u8;



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
macro_rules! __fmap {
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

#[macro_export]
macro_rules! __foldr {
    ($fn: expr, $target: expr) => {
        $target.into_iter()
            .map($fn)
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

pub fn bracket<A, B, C>(a: A, b: B, c: C) -> C {
    // TODO these are all methods
    c
}


pub fn seq<A, B>(a: A, b: B) -> B {
    // we don't do lazy
    b
}






// Map things

pub mod Map {
    pub fn insert<T>(mut okay: Vec<T>, key: isize, value: T) -> Vec<T> {
        okay.push(value);
        okay
    }

    pub fn lookup<T>(value: T, inside: Vec<T>) -> isize {
        //TODO
        0
    }
}


use std::hash::Hash;
use std::fmt::Debug;
use std::collections::HashSet;

#[derive(Clone, Debug)]
pub struct Set<T: Eq + Hash>(HashSet<T>);

impl<T: Eq + Hash + Debug> Set<T> {
    pub fn member(item: T, list: Self) -> bool {
        list.0.contains(&item)
    }

    pub fn fromList(list: Vec<T>) -> Self {
        // TODO
        Set(HashSet::new())
    }

    pub fn insert(item: T, mut list: Self) -> Self {
        list.0.insert(item);
        list
    }

    pub fn delete(item: T, mut list: Self) -> Self {
        list.0.remove(&item);
        list
    }
}


// Array things

pub fn array<T>(dim: (isize, isize), mut list: Vec<(isize, T)>) -> Vec<T> {
    // Only supports an ordered array for now
    list.sort_by(|a, b| a.0.cmp(&b.0));
    assert_eq!(list.last().unwrap().0, dim.1 - 1);
    list.into_iter().map(|x| x.1).collect()
}

pub fn listArray<T>(dim: (isize, isize), list: Vec<T>) -> Vec<T> {
    list
}

pub struct Array<T, U> {
    idx: Vec<T>,
    inner: Vec<U>,
}

pub fn __op_array_index<T>(mut arr: Vec<T>, idx: isize) -> T {
    arr.remove(idx as usize)
}

pub fn __op_rshift(left: isize, right: isize) {
    // TODO
    unreachable!();
}



