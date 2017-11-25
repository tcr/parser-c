// Original file: "Parser.hs"
// File auto-generated using Corollary.

pub mod builtin;
pub mod lexer;
pub mod parser;
pub mod tokens;

use std::error::Error;
use std::mem;
use std::fmt;
use std::boxed::FnBox;
use std::iter::FromIterator;
use std::collections::HashSet;
use either::Either;

pub use parser::parser::Parser;
use parser::parser::translUnitP;
use parser::builtin::*;
use parser::tokens::{CToken, CTokEof};
use syntax::ast::*;
use syntax::constants::showCString;
use data::name::{Name, NameSupply, new_name_supply};
use data::ident::Ident;
use data::node::{NodeInfo, CNode};
use data::position::{Position, Pos};
use data::input_stream::InputStream;

#[derive(Debug)]
pub struct ParseError {
    kind: ParseErrorKind,
    msg: String,
    pos: Position,
}

#[derive(Debug)]
pub enum ParseErrorKind {
    Lexical,
    Syntax,
}

impl ParseError {
    pub fn lexical(pos: Position, msg: String) -> ParseError {
        ParseError { kind: ParseErrorKind::Lexical, msg, pos }
    }

    pub fn syntax(pos: Position, msg: String) -> ParseError {
        ParseError { kind: ParseErrorKind::Syntax, msg, pos }
    }
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} at {}: {}", self.description(), self.pos, self.msg)
    }
}

impl Error for ParseError {
    fn description(&self) -> &str {
        match self.kind {
            ParseErrorKind::Lexical => "Lexical error",
            ParseErrorKind::Syntax => "Syntax error",
        }
    }
}

pub struct PState {
    curInput: (Position, InputStream),
    tokLen: usize,
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
        curInput: (pos, input),
        tokLen: 0,
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

    // lexer related API

    pub fn getInput(&mut self) -> &mut (Position, InputStream) {
        &mut self.user.curInput
    }

    pub fn setPos(&mut self, pos: Position) {
        self.user.curInput.0 = pos;
    }

    pub fn getPosClone(&self) -> Position {
        self.user.curInput.0.clone()
    }

    pub fn setLastTokLen(&mut self, len: usize) {
        self.user.tokLen = len;
    }

    pub fn getTokString(&self) -> &str {
        self.user.curInput.1.last_string(self.user.tokLen)
    }

    // parser related API

    pub fn addTypedef(&mut self, ident: Ident) {
        self.user.tyidents.insert(ident);
    }

    pub fn shadowTypedef(&mut self, ident: &Ident) {
        self.user.tyidents.remove(ident);
    }

    pub fn isTypeIdent(&self, ident: &Ident) -> bool {
        self.user.tyidents.contains(ident)
    }

    pub fn enterScope(&mut self) {
        self.user.scopes.insert(0, self.user.tyidents.clone());
    }

    pub fn leaveScope(&mut self) {
        assert!(!self.user.scopes.is_empty(), "leaveScope: already in global scope");
        self.user.tyidents = self.user.scopes.remove(0);
    }

    pub fn getLastToken(&self) -> CToken {
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

    pub fn doDeclIdent(&mut self, declspecs: &[CDeclSpec], declr: &CDeclrR) {
        let is_typedef = |declspec: &CDeclSpec| match *declspec {
            CStorageSpec(CTypedef(_)) => true,
            _ => false,
        };

        match declr.ident {
            None => (),
            Some(ref ident) => {
                if declspecs.iter().any(is_typedef) {
                    self.addTypedef(ident.clone())
                } else {
                    self.shadowTypedef(ident)
                }
            },
        }
    }

    pub fn doFuncParamDeclIdent(&mut self, decl: &CDeclr) {
        if decl.1.is_empty() {
            return;
        }
        if let CFunDeclr(Either::Right((ref params, _)), _, _) = decl.1[0] {
            for param in params {
                if let CDecl(_, ref dle, _) = *param {
                    for dl in dle {
                        if let (Some(ref declr), _, _,) = *dl {
                            if let Some(ref ident) = declr.0 {
                                self.shadowTypedef(ident);
                            }
                        }
                    }
                }
            }
        }
    }

    pub fn withLength<T, F>(&mut self, nodeinfo: NodeInfo, mkAttrNode: F) -> Result<T, ParseError>
        where F: FnOnce(NodeInfo) -> T
    {
        let lastTok = self.getSavedToken();
        let firstPos = nodeinfo.pos().clone();
        let attrs = NodeInfo::new(firstPos, lastTok.into_pos_len(),
                                  nodeinfo.name().unwrap_or_else(|| panic!("nameOfNode")));
        Ok(mkAttrNode(attrs))
    }

    pub fn withAttribute<N: Pos, F>(&mut self, node: N, cattrs: Vec<CAttribute<NodeInfo>>,
                                    mkDeclrNode: F) -> Result<CDeclrR, ParseError>
        where F: FnOnce(NodeInfo) -> CDeclrR
    {
        let name = self.getNewName();
        let attrs = NodeInfo::with_pos_name(node.into_pos(), name);
        let newDeclr = mkDeclrNode(attrs).appendAttrs(cattrs);
        Ok(newDeclr)
    }

    pub fn withAttributePF<N: Pos, F>(&mut self, node: N, cattrs: Vec<CAttribute<NodeInfo>>,
                                      mkDeclrCtor: F) -> Result<Box<FnBox(CDeclrR) -> CDeclrR>, ParseError>
        where F: FnOnce(NodeInfo, CDeclrR) -> CDeclrR + 'static
    {
        let name = self.getNewName();
        let attrs = NodeInfo::with_pos_name(node.into_pos(), name);
        let newDeclr: Box<FnBox(CDeclrR) -> CDeclrR> = box move |_0| {
            mkDeclrCtor(attrs.clone(), _0).appendAttrs(cattrs.clone())
        };
        Ok(newDeclr)
    }
}

#[derive(Debug, Clone)]
pub struct CDeclrR {
    ident: Option<Ident>,
    inner: Vec<CDerivedDeclr>,
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
        CDeclrR { ident: None, inner: vec![], asmname: None, cattrs: vec![],
                  at: NodeInfo::undef() }
    }

    pub fn from_var(ident: Ident, ni: NodeInfo) -> CDeclrR {
        CDeclrR { ident: Some(ident), inner: vec![], asmname: None, cattrs: vec![], at: ni }
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
            Err(ParseError::syntax(
                newname.pos().clone(),
                format!("Duplicate assembler name: {}, {}",
                        showCString(&oldname.0), showCString(&newname.0))))
        }
    }

    pub fn withAsmNameAttrs(self, (mAsmName, newAttrs): (Option<CStringLiteral<NodeInfo>>,
                                                         Vec<CAttribute<NodeInfo>>))
                            -> Result<CDeclrR, ParseError> {
        self.appendObjAttrs(newAttrs).setAsmName(mAsmName)
    }

    pub fn funDeclr(mut self, params: Either<Vec<Ident>, (Vec<CDecl>, bool)>,
                    cattrs: Vec<CAttribute<NodeInfo>>, at: NodeInfo) -> CDeclrR {
        self.inner.push(CFunDeclr(params, cattrs, at));
        self
    }

    pub fn arrDeclr(mut self, tyquals: Vec<CTypeQual>, var_sized: bool, static_size: bool,
                    size_expr_opt: Option<CExpr>, at: NodeInfo) -> CDeclrR {
        let arr_sz = match size_expr_opt {
            Some(e) => CArrSize(static_size, e),
            None => CNoArrSize(var_sized)
        };
        self.inner.push(CArrDeclr(tyquals, arr_sz, at));
        self
    }

    pub fn ptrDeclr(mut self: CDeclrR, tyquals: Vec<CTypeQual>, at: NodeInfo) -> CDeclrR {
        self.inner.push(CPtrDeclr(tyquals, at));
        self
    }

    pub fn appendAttrs(mut self, mut newAttrs: Vec<CAttribute<NodeInfo>>) -> Self {
        match self.inner.last_mut() {
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
        let CDeclrR { ident, inner, asmname, cattrs, at } = self;
        CDeclarator(ident, inner, asmname, cattrs, at)
    }
}

pub fn parseC(input: InputStream, initialPosition: Position) -> Result<CTranslUnit, ParseError> {
    execParser(input, initialPosition, builtinTypeNames(), new_name_supply(), translUnitP).map(|x| x.0)
}

pub fn execParser_<T, F>(do_parse: F, input: InputStream, pos: Position) -> Result<T, ParseError>
    where T: 'static, F: Fn(&mut Parser) -> Result<T, ParseError>
{
    execParser(input, pos, builtinTypeNames(), new_name_supply(), do_parse).map(|x| x.0)
}

pub fn ptrDeclr(mut slf: CDeclrR, tyquals: Vec<CTypeQual>, at: NodeInfo) -> CDeclrR {
    slf.inner.push(CPtrDeclr(tyquals, at));
    slf
}
