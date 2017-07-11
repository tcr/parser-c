// Original file: "ParserMonad.hs"
// File auto-generated using Corollary.

#[macro_use]
use corollary_support::*;

use std::mem;
use std::rc::Rc;
use std::boxed::FnBox;
use std::collections::HashSet;
use std::iter::FromIterator;

use data::r_list::{Reversed, RList, snoc};
use data::node::{NodeInfo, CNode};
use data::position::{Position, Pos};
use data::input_stream::InputStream;
use data::ident::Ident;
use data::name::{Name, NameSupply, new_name_supply};
use parser::tokens::{CToken, CTokEof, movePosLenOfTok};
use parser::parser::Parser;
use syntax::ast::*;

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

    pub fn doDeclIdent(&mut self, declspecs: &[CDeclSpec], declr: CDeclrR) {
        let is_typedef = |declspec: &CDeclSpec| match *declspec {
            CStorageSpec(CTypedef(_)) => true,
            _ => false,
        };

        match declr.ident {
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
        let newDeclr = mkDeclrNode(attrs).appendAttrs(cattrs);
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
            mkDeclrCtor(attrs.clone(), _0).appendAttrs(cattrs.clone())
        });
        Ok(newDeclr)
    }
}

#[derive(Clone)]
pub struct CDeclrR {
    ident: Option<Ident>,
    inner: Reversed<Vec<CDerivedDeclr>>,
    asmname: Option<CStringLiteral<NodeInfo>>,
    cattrs: Vec<CAttribute<NodeInfo>>,
    at: NodeInfo,
}

impl CNode for CDeclrR {
    fn node_info(&self) -> &NodeInfo {
        &self.at
    }
    fn into_node_info(self) -> NodeInfo {
        self.at
    }
}

impl CDeclrR {

    pub fn empty() -> CDeclrR {
        CDeclrR { ident: None, inner: RList::empty(), asmname: None, cattrs: vec![],
                  at: NodeInfo::undef() }
    }

    pub fn from_var(ident: Ident, ni: NodeInfo) -> CDeclrR {
        CDeclrR { ident: Some(ident), inner: RList::empty(), asmname: None, cattrs: vec![], at: ni }
    }

    pub fn setAsmName(mut self, mAsmName: Option<CStringLiteral<NodeInfo>>) -> Result<CDeclrR, ParseError> {
        if self.asmname.is_none() {
            self.asmname = mAsmName;
            Ok(self)
        } else if mAsmName.is_none() {
            Ok(self)
        } else {
            let newname = mAsmName.unwrap();
            let oldname = self.asmname.as_ref().unwrap();
            Err(ParseError::new(
                newname.pos().clone(),
                vec!["Duplicate assembler name: ".to_string(),
                     oldname.0.to_string(), newname.0.to_string()]))
        }
    }

    pub fn withAsmNameAttrs(self, (mAsmName, newAttrs): (Option<CStringLiteral<NodeInfo>>,
                                                         Vec<CAttribute<NodeInfo>>))
                            -> Result<CDeclrR, ParseError> {
        self.appendObjAttrs(newAttrs).setAsmName(mAsmName)
    }

    pub fn funDeclr(mut self, params: Either<Vec<Ident>, (Vec<CDecl>, bool)>,
                    cattrs: Vec<CAttribute<NodeInfo>>, at: NodeInfo) -> CDeclrR {
        self.inner = snoc(self.inner, CFunDeclr(params, cattrs, at));
        self
    }

    pub fn arrDeclr(mut self, tyquals: Vec<CTypeQual>, var_sized: bool, static_size: bool,
                    size_expr_opt: Option<CExpr>, at: NodeInfo) -> CDeclrR {
        let arr_sz = match size_expr_opt {
            Some(e) => CArrSize(static_size, e),
            None => CNoArrSize(var_sized)
        };
        self.inner = snoc(self.inner, CArrDeclr(tyquals, arr_sz, at));
        self
    }

    pub fn appendAttrs(mut self, mut newAttrs: Vec<CAttribute<NodeInfo>>) -> Self {
        match RList::get_mut(&mut self.inner, 0) {
            None => self.cattrs.append(&mut newAttrs),
            Some(&mut CPtrDeclr(ref mut typeQuals, _)) => {
                typeQuals.extend(newAttrs.into_iter().map(CAttrQual))
            }
            Some(&mut CArrDeclr(ref mut typeQuals, _, _)) => {
                typeQuals.extend(newAttrs.into_iter().map(CAttrQual))
            }
            Some(&mut CFunDeclr(_, ref mut cattrs, _)) => {
                 cattrs.append(&mut newAttrs)
            }
        }
        self
    }

    pub fn appendObjAttrs(mut self, mut newAttrs: Vec<CAttribute<NodeInfo>>) -> CDeclrR {
        self.cattrs.append(&mut newAttrs);
        self
    }

    pub fn reverse(self) -> CDeclarator<NodeInfo> {
        let CDeclrR { ident, inner: reversedDDs, asmname, cattrs, at } = self;
        CDeclarator(ident, RList::reverse(reversedDDs), asmname, cattrs, at)
    }
}

// This is a free function since it is used with partial_1!()
pub fn ptrDeclr(mut slf: CDeclrR, tyquals: Vec<CTypeQual>, at: NodeInfo) -> CDeclrR {
    slf.inner = snoc(slf.inner, CPtrDeclr(tyquals, at));
    slf
}
