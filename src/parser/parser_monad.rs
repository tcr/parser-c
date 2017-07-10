// Original file: "ParserMonad.hs"
// File auto-generated using Corollary.

#[macro_use]
use corollary_support::*;

// NOTE: These imports are advisory. You probably need to change them to support Rust.
// use Language::C::Data::Error;
// use internalErr;
// use Language::C::Data::Position;
// use Position;
// use Language::C::Data::InputStream;
// use Language::C::Data::Name;
// use Name;
// use Language::C::Data::Ident;
// use Ident;
// use Language::C::Parser::Tokens;
// use CToken;
// use Control::Applicative;
// use Applicative;
// use Control::Monad;
// use liftM;
// use Data::Set;
// use Set;
// use Data::Set;

use data::position::Position;
use data::position::Position::NoPosition;
use parser::tokens::*;
use data::input_stream::*;
use data::ident::Ident;
use data::name::{Name, NameSupply};
use data::error::*;
use std::boxed::FnBox;
use std::rc::Rc;
use std::mem;
use std::collections::HashSet;
use std::iter::FromIterator;

#[derive(Debug)]
pub struct ParseError(pub (Vec<String>, Position));

// instance Show ParseError where
//     show (ParseError (msgs,pos)) = showErrorInfo "Syntax Error !" (ErrorInfo LevelError pos msgs)

pub enum ParseResult<a> {
    POk(PState, a),
    PFailed(Vec<String>, Position),
}
pub use self::ParseResult::*;

pub struct PState {
    curPos: Position,
    curInput: InputStream,
    prevToken: Option<CToken>,
    savedToken: Option<CToken>,
    nameSupply: NameSupply,
    tyidents: HashSet<Ident>,
    scopes: Vec<HashSet<Ident>>,
}

#[must_use]
pub struct P<a>(Box<FnBox(PState) -> ParseResult<a>>);

pub fn unP<a>(p: P<a>) -> Box<FnBox(PState) -> ParseResult<a>> {
    p.0
}

pub fn execParser<a>(P(parser): P<a>,
                     input: InputStream,
                     pos: Position,
                     builtins: Vec<Ident>,
                     names: NameSupply)
                     -> Result<(a, NameSupply), ParseError> {

    let initialState = PState {
        curPos: pos,
        curInput: input,
        prevToken: None,
        savedToken: None,
        nameSupply: names,
        tyidents: HashSet::from_iter(builtins),
        scopes: vec![],
    };

    match parser(initialState) {
        PFailed(message, errpos) => Err(ParseError((message, errpos))),
        POk(st, result) => Ok((result, st.nameSupply)),
    }
}

pub fn returnP<T: 'static>(a: T) -> P<T> {
    P(box move |s| POk(s, a))
}

pub fn thenP<T: 'static, U: 'static>(P(m): P<T>, k: Box<FnBox(T) -> P<U>>) -> P<U> {
    P(box move |s| match m(s) {
        POk(s_q, a) => unP(k(a))(s_q),
        PFailed(err, pos) => PFailed(err, pos),
    })
}

pub fn failP<T>(pos: Position, msg: Vec<String>) -> P<T> {
    P(box move |_| PFailed(msg, pos))
}

pub fn seqP<a: 'static, b: 'static>(a: P<a>, b: P<b>) -> P<b> {
    thenP(a, box move |_| b)
}

pub fn getNewName() -> P<Name> {
    P(box move |mut s: PState| {
        let n = s.nameSupply.next().unwrap();
        POk(s, n)
    })
}


pub fn setPos(pos: Position) -> P<()> {
    P(box move |mut s: PState| {
        s.curPos = pos;
        POk(s, ())
    })
}

pub fn getPos() -> P<Position> {
    P(box move |s: PState| {
        let pos = s.curPos.clone();
        POk(s, pos)
    })
}

pub fn addTypedef(ident: Ident) -> P<()> {
    P(box move |mut s: PState| {
        s.tyidents.insert(ident);
        POk(s, ())
    })
}

pub fn shadowTypedef(ident: Ident) -> P<()> {
    P(box move |mut s: PState| {
        s.tyidents.remove(&ident);
        POk(s, ())
    })
}

pub fn isTypeIdent(ident: Ident) -> P<bool> {
    P(box move |s: PState| {
        let is_member = s.tyidents.contains(&ident);
        POk(s, is_member)
    })
}

pub fn enterScope() -> P<()> {
    P(box move |mut s: PState| {
        s.scopes.insert(0, s.tyidents.clone());
        POk(s, ())
    })
}

pub fn leaveScope() -> P<()> {
    P(box |mut s: PState| {
        if s.scopes.is_empty() {
            panic!("leaveScope: already in global scope");
        } else {
            s.tyidents = s.scopes.remove(0);
            POk(s, ())
        }
    })
}

pub fn getInput() -> P<InputStream> {
    P(box |s: PState| {
        let input = s.curInput.clone();
        POk(s, input)
    })
}

pub fn setInput(i: InputStream) -> P<()> {
    P(box move |mut s: PState| {
        s.curInput = i;
        POk(s, ())
    })
}

pub fn getLastToken() -> P<CToken> {
    P(box |s: PState| {
        let tok = s.prevToken.clone().expect("touched undefined token");
        POk(s, tok)
    })
}

pub fn getSavedToken() -> P<CToken> {
    P(box |s: PState| {
        let tok = s.savedToken.clone().expect("touched undefined token");
        POk(s, tok)
    })
}

pub fn setLastToken(tk: CToken) -> P<()> {
    match tk {
        CTokEof => {
            P(box |mut s: PState| {
                s.savedToken = s.prevToken.clone();
                POk(s, ())
            })
        }
        tok => {
            P(box move |mut s: PState| {
                s.savedToken = mem::replace(&mut s.prevToken, Some(tok));
                POk(s, ())
            })
        }
    }
}

pub fn handleEofToken() -> P<()> {
    P(box |mut s: PState| {
        s.savedToken = s.prevToken.clone();
        POk(s, ())
    })
}

pub fn getCurrentPosition() -> P<Position> {
    P(box |s: PState| {
        let pos = s.curPos.clone();
        POk(s, pos)
    })
}
