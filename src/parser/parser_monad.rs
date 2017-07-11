// Original file: "ParserMonad.hs"
// File auto-generated using Corollary.

#[macro_use]
use corollary_support::*;

use std::boxed::FnBox;
use std::rc::Rc;
use std::mem;
use std::collections::HashSet;
use std::iter::FromIterator;

use data::position::Position;
use data::position::Position::NoPosition;
use parser::tokens::*;
use data::input_stream::*;
use data::ident::Ident;
use data::name::{Name, NameSupply, new_name_supply};
use data::error::*;
use parser::parser::{HappyAbsSyn, State, States, Stack, Cont};

#[derive(Debug)]
pub struct ParseError(pub (Vec<String>, Position));

impl ParseError {
    pub fn new(pos: Position, msgs: Vec<String>) -> ParseError {
        ParseError((msgs, pos))
    }
}

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

// monad compatibility
pub type P<T> = Result<T, ParseError>;

pub struct Parser {
    pstate: PState,
    pub token: CToken,
    pub state: State,
    pub states: States,
    pub stack: Stack,
}

fn invalid_action(p: &mut Parser, i: isize, j: isize) -> P<Cont> {
    panic!("parser not initialized correctly")
}

pub fn execParser<F, T>(input: InputStream, pos: Position, builtins: Vec<Ident>, names: NameSupply,
                        do_parse: F) -> Result<(T, NameSupply), ParseError>
    where F: FnOnce(&mut Parser) -> P<T>
{
    let initial_state = PState {
        curPos: pos,
        curInput: input,
        prevToken: None,
        savedToken: None,
        nameSupply: names,
        tyidents: HashSet::from_iter(builtins),
        scopes: vec![],
    };
    let mut parser = Parser { pstate: initial_state,
                              token: CToken::CTokEof,
                              state: invalid_action,
                              states: vec![],
                              stack: vec![] };
    do_parse(&mut parser).map(|res| (res, new_name_supply()))// parser.pstate.nameSupply)) // TODO
}

impl Parser {
    pub fn getNewName(&mut self) -> Name {
        self.pstate.nameSupply.next().unwrap()
    }

    pub fn setPos(&mut self, pos: Position) {
        self.pstate.curPos = pos;
    }

    pub fn getPos(&self) -> Position {  // TODO borrow
        self.pstate.curPos.clone()
    }

    pub fn addTypedef(&mut self, ident: Ident) {
        self.pstate.tyidents.insert(ident);
    }

    pub fn shadowTypedef(&mut self, ident: Ident) {
        self.pstate.tyidents.remove(&ident);
    }

    pub fn isTypeIdent(&self, ident: &Ident) -> bool {
        self.pstate.tyidents.contains(ident)
    }

    pub fn enterScope(&mut self) {
        self.pstate.scopes.insert(0, self.pstate.tyidents.clone());
    }

    pub fn leaveScope(&mut self) {
        if self.pstate.scopes.is_empty() {
            panic!("leaveScope: already in global scope");
        } else {
            self.pstate.tyidents = self.pstate.scopes.remove(0);
        }
    }

    pub fn getInput(&self) -> InputStream {   // TODO borrow
        self.pstate.curInput.clone()
    }

    pub fn setInput(&mut self, i: InputStream) {
        self.pstate.curInput = i;
    }

    pub fn getLastToken(&self) -> CToken {   // TODO borrow
        self.pstate.prevToken.clone().expect("touched undefined token")
    }

    pub fn getSavedToken(&self) -> CToken {   // TODO borrow
        self.pstate.savedToken.clone().expect("touched undefined token")
    }

    pub fn setLastToken(&mut self, tk: CToken) {
        match tk {
            CTokEof => {
                self.pstate.savedToken = self.pstate.prevToken.clone();
            }
            tok => {
                self.pstate.savedToken = mem::replace(&mut self.pstate.prevToken,
                                                      Some(tok));
            }
        }
    }

    pub fn handleEofToken(&mut self) -> () {
        self.pstate.savedToken = self.pstate.prevToken.clone();
    }
}
