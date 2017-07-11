// Original file: "ParserMonad.hs"
// File auto-generated using Corollary.

#[macro_use]
use corollary_support::*;

use std::mem;
use std::rc::Rc;
use std::boxed::FnBox;
use std::collections::HashSet;
use std::iter::FromIterator;

use data::r_list::{Reversed, RList};
use data::node::{NodeInfo, CNode};
use data::position::{Position, Pos};
use data::input_stream::InputStream;
use data::ident::Ident;
use data::name::{Name, NameSupply, new_name_supply};
use parser::tokens::{CToken, CTokEof, movePosLenOfTok};
use parser::parser::Parser;
use syntax::ast::{CDeclSpec, CTypedef, CStorageSpec, CAttribute, CDerivedDeclr,
                  CStringLiteral, CPtrDeclr, CAttrQual, CArrDeclr, CFunDeclr};

#[derive(Debug)]
pub struct ParseError(pub (Vec<String>, Position));

impl ParseError {
    pub fn new(pos: Position, msgs: Vec<String>) -> ParseError {
        ParseError((msgs, pos))
    }
}

pub struct PState {
    curPos: Position,
    curInput: InputStream,
    prevToken: Option<CToken>,
    savedToken: Option<CToken>,
    nameSupply: NameSupply,
    tyidents: HashSet<Ident>,
    scopes: Vec<HashSet<Ident>>,
}

pub fn execParser<F, T>(input: InputStream, pos: Position, builtins: Vec<Ident>, names: NameSupply,
                        do_parse: F) -> Result<(T, NameSupply), ParseError>
    where F: FnOnce(&mut Parser) -> Result<T, ParseError>
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
    let (state, res) = Parser::exec(initial_state, do_parse)?;
    Ok((res, state.nameSupply))
}

impl Parser {
    pub fn getNewName(&mut self) -> Name {
        self.user.nameSupply.next().unwrap()
    }

    pub fn setPos(&mut self, pos: Position) {
        self.user.curPos = pos;
    }

    pub fn getPos(&self) -> Position {  // TODO borrow
        self.user.curPos.clone()
    }

    pub fn addTypedef(&mut self, ident: Ident) {
        self.user.tyidents.insert(ident);
    }

    pub fn shadowTypedef(&mut self, ident: Ident) {
        self.user.tyidents.remove(&ident);
    }

    pub fn isTypeIdent(&self, ident: &Ident) -> bool {
        self.user.tyidents.contains(ident)
    }

    pub fn enterScope(&mut self) {
        self.user.scopes.insert(0, self.user.tyidents.clone());
    }

    pub fn leaveScope(&mut self) {
        if self.user.scopes.is_empty() {
            panic!("leaveScope: already in global scope");
        } else {
            self.user.tyidents = self.user.scopes.remove(0);
        }
    }

    pub fn getInput(&self) -> InputStream {   // TODO borrow
        self.user.curInput.clone()
    }

    pub fn setInput(&mut self, i: InputStream) {
        self.user.curInput = i;
    }

    pub fn getLastToken(&self) -> CToken {   // TODO borrow
        self.user.prevToken.clone().expect("touched undefined token")
    }

    pub fn getSavedToken(&self) -> CToken {   // TODO borrow
        self.user.savedToken.clone().expect("touched undefined token")
    }

    pub fn setLastToken(&mut self, tk: CToken) {
        match tk {
            CTokEof => {
                self.user.savedToken = self.user.prevToken.clone();
            }
            tok => {
                self.user.savedToken = mem::replace(&mut self.user.prevToken,
                                                    Some(tok));
            }
        }
    }

    pub fn handleEofToken(&mut self) -> () {
        self.user.savedToken = self.user.prevToken.clone();
    }

    pub fn doDeclIdent(&mut self, declspecs: &[CDeclSpec], CDeclrR(mIdent, _, _, _, _): CDeclrR) {
        let is_typedef = |declspec: &CDeclSpec| match *declspec {
            CStorageSpec(CTypedef(_)) => true,
            _ => false,
        };

        match mIdent {
            None => (),
            Some(ident) => {
                if declspecs.iter().any(is_typedef) {
                    self.addTypedef(ident)
                } else {
                    self.shadowTypedef(ident)
                }
            },
        }
    }

    pub fn withNodeInfo<T: 'static, N: Pos>(&mut self, node: N, mkAttrNode: Box<FnBox(NodeInfo) -> T>)
                                            -> Result<T, ParseError> {
        let name = self.getNewName();
        let lastTok = self.getSavedToken();
        let firstPos = node.into_pos();
        let attrs = NodeInfo::new(firstPos, movePosLenOfTok(lastTok), name);
        Ok(mkAttrNode(attrs))
    }

    pub fn withLength<T: Clone + 'static>(&mut self, nodeinfo: NodeInfo,
                                          mkAttrNode: Box<FnBox(NodeInfo) -> T>) -> Result<T, ParseError> {
        let lastTok = self.getSavedToken();
        let firstPos = nodeinfo.pos().clone();
        let attrs = NodeInfo::new(firstPos, movePosLenOfTok(lastTok),
                                  nodeinfo.name().unwrap_or_else(|| panic!("nameOfNode")));
        Ok(mkAttrNode(attrs))
    }

    pub fn withAttribute<N: Pos>(&mut self, node: N, cattrs: Vec<CAttribute<NodeInfo>>,
                                 mkDeclrNode: Box<FnBox(NodeInfo) -> CDeclrR>) -> Result<CDeclrR, ParseError> {
        let name = self.getNewName();
        let attrs = NodeInfo::with_pos_name(node.into_pos(), name);
        let newDeclr = appendDeclrAttrs(cattrs.clone(), mkDeclrNode(attrs));
        Ok(newDeclr)
    }

    pub fn withAttributePF<N: Pos + 'static>(&mut self, node: N, cattrs: Vec<CAttribute<NodeInfo>>,
                                             mkDeclrCtor: Box<Fn(NodeInfo, CDeclrR) -> CDeclrR>)
                                             -> Result<Rc<Box<Fn(CDeclrR) -> CDeclrR>>, ParseError>
    {
        let mkDeclrCtor = Rc::new(mkDeclrCtor);
        let name = self.getNewName();
        let attrs = NodeInfo::with_pos_name(node.into_pos(), name);
        let newDeclr: Rc<Box<Fn(CDeclrR) -> CDeclrR>> = Rc::new(box move |_0| {
            appendDeclrAttrs(cattrs.clone(), mkDeclrCtor(attrs.clone(), _0))
        });
        Ok(newDeclr)
    }
}

#[derive(Clone)]
pub struct CDeclrR(pub Option<Ident>,
                   pub Reversed<Vec<CDerivedDeclr>>,
                   pub Option<CStringLiteral<NodeInfo>>,
                   pub Vec<CAttribute<NodeInfo>>,
                   pub NodeInfo);

impl CNode for CDeclrR {
    fn node_info(&self) -> &NodeInfo {
        &self.4
    }
    fn into_node_info(self) -> NodeInfo {
        self.4
    }
}

pub fn appendDeclrAttrs(newAttrs: Vec<CAttribute<NodeInfo>>, declr: CDeclrR) -> CDeclrR {
    let CDeclrR(ident, Reversed(mut inner), asmname, cattrs, at) = declr;
    if inner.len() == 0 {
        CDeclrR(ident, RList::empty(), asmname, __op_addadd(cattrs, newAttrs), at)
    } else {
        let x = inner.remove(0);
        let xs = inner;
        let appendAttrs = |_0| match _0 {
            CPtrDeclr(typeQuals, at) =>
                CPtrDeclr(__op_addadd(typeQuals, __map!(CAttrQual, newAttrs)), at),
            CArrDeclr(typeQuals, arraySize, at) =>
                CArrDeclr(__op_addadd(typeQuals, __map!(CAttrQual, newAttrs)), arraySize, at),
            CFunDeclr(parameters, cattrs, at) =>
                CFunDeclr(parameters, __op_addadd(cattrs, newAttrs), at),
        };
        CDeclrR(ident, Reversed(__op_concat(appendAttrs(x), xs)), asmname, cattrs, at)
    }
}
