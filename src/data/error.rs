// Original file: "Error.hs"
// File auto-generated using Corollary.

#[macro_use]
use corollary_support::*;

// NOTE: These imports are advisory. You probably need to change them to support Rust.
// use Data::Typeable;
// use Language::C::Data::Node;
// use Language::C::Data::Position;

use std::iter;
use std::fmt::Write;

use data::position::*;
use data::node::*;
use data::position::Pos;

#[derive(Eq, Ord, PartialEq, PartialOrd, Debug)]
pub enum ErrorLevel {
    LevelWarn,
    LevelError,
    LevelFatal,
}
pub use self::ErrorLevel::*;

impl ::std::fmt::Display for ErrorLevel {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

pub fn isHardError<E: Error>(e: E) -> bool {
    errorLevel(e) > LevelWarn
}

#[derive(Debug)]
pub struct ErrorInfo(pub ErrorLevel, pub Position, pub Vec<String>);


pub fn mkErrorInfo(lvl: ErrorLevel, msg: String, node: NodeInfo) -> ErrorInfo {
    ErrorInfo(lvl, node.into_pos(), lines(msg))
}

#[derive(Debug)]
pub struct CError(pub Box<Error>);

// errors in Language.C are instance of 'Error'
use std::fmt::Debug;
pub trait Error
    where Self: Debug
{
    // obtain source location etc. of an error
    fn errorInfo(self) -> ErrorInfo;
    // wrap error in 'CError'
    fn toError(self) -> CError;
    // try to cast a generic 'CError' to the specific error type
    fn fromError(c: CError) -> Option<Box<Self>> where Self: Sized;
    // modify the error level
    fn changeErrorLevel(self, lvl: ErrorLevel) -> Self
        where Self: Sized + Clone
    {
        if errorLevel(self.clone()) == lvl {
            self
        } else {
            panic!("changeErrorLevel: not possible for {:?}", self);
        }
    }
}

//TODO
// instance Show CError where
//     show (CError e) = show e
impl Error for CError {
    fn errorInfo(self) -> ErrorInfo {
        // TODO
        unreachable!()
        // self.0.errorInfo()
    }

    fn toError(self) -> CError {
        self
    }

    fn fromError(c: CError) -> Option<Box<Self>> {
        Some(box c)
    }

    fn changeErrorLevel(self, lvl: ErrorLevel) -> Self {
        //TODO
        // CError(box self.0.changeErrorLevel(lvl))
        unreachable!()
    }
}

pub fn errorPos<E: Error>(e: E) -> Position {
    let ErrorInfo(_, pos, _) = e.errorInfo();
    pos
}

pub fn errorLevel<E: Error>(e: E) -> ErrorLevel {
    let ErrorInfo(lvl, _, _) = e.errorInfo();
    lvl
}

pub fn errorMsg<E: Error>(e: E) -> Vec<String> {
    let ErrorInfo(_, _, msgs) = e.errorInfo();
    msgs
}

#[derive(Debug)]
pub struct UnsupportedFeature(pub String, pub Position);

// pub trait Error<T: UnsupportedFeature> {
//     fn errorInfo (UnsupportedFeature msg pos) = ErrorInfo LevelError pos (lines msg)
// }
// instance Show UnsupportedFeature where show = showError "Unsupported Feature"


pub fn unsupportedFeature<P: Pos>(msg: String, a: P) -> UnsupportedFeature {
    UnsupportedFeature(msg, a.into_pos())
}

pub fn unsupportedFeature_(msg: String) -> UnsupportedFeature {
    UnsupportedFeature(msg, Position::internal())
}

#[derive(Debug)]
pub struct UserError(pub ErrorInfo);


pub fn userErr(msg: String) -> UserError {
    UserError(ErrorInfo(LevelError, Position::internal(), lines(msg)))
}

pub fn showError<E: Error>(short_msg: String, e: E) -> String {
    showErrorInfo(short_msg, e.errorInfo())
}


pub const INDENT: &str = "  ";

pub const INTERNAL_ERR_PREFIX: &str = "parser-c : Internal Error\n\
     This is propably a bug, and should be reported at \
     https://github.com/tcr/parser-c";


pub fn showErrorInfo(short_msg: String, ErrorInfo(level, pos, mut msgs): ErrorInfo) -> String {
    let mut res = String::new();

    if pos.isSource() {
        write!(res, "{}:{}: (column {}) ", pos.file(), pos.row(), pos.column()).unwrap();
    } else {
        write!(res, "{}:: ", pos).unwrap();
    }
    write!(res, "[{}]", level).unwrap();
    if !short_msg.is_empty() {
        msgs.insert(0, short_msg);
    }
    if msgs.is_empty() {
        internalErr("No short message or error message provided.");
    }
    for msg in msgs {
        write!(res, "{}>>> {}\n", INDENT, msg).unwrap();
    }

    res
}

pub fn internalErr(msg: &str) -> ! {
    let mut res = String::new();
    for line in msg.split('\n') {
        write!(res, "{}{}\n", INDENT, line).unwrap();
    }
    panic!("{}\n{}", INTERNAL_ERR_PREFIX, res);
}
