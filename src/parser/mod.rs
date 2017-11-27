// Cut down on number of warnings until we manage it.
//#![allow(non_snake_case)]
// Original file: "Parser.hs"
// File auto-generated using Corollary.

pub mod builtin;
pub mod lexer;
pub mod parser;
pub mod tokens;

use std::error::Error;
use std::mem;
use std::fmt;
use std::io;
use std::rc::Rc;
use std::boxed::FnBox;
use std::iter::FromIterator;
use std::collections::HashSet;
use either::Either;

pub use parser::parser::Parser;
use parser::parser::translation_unit;
use parser::builtin::builtin_type_names;
use parser::tokens::{CToken, CTokEof};
use syntax::ast::*;
use data::name::{Name, NameSupply, new_name_supply};
use data::ident::Ident;
use data::node::{NodeInfo, CNode};
use data::position::{Position, Pos};
use data::input_stream::InputStream;

#[derive(Debug)]
pub enum ParseError {
    Input(Box<Error>),
    Lexical(String, Rc<Position>),
    Syntax(String, Rc<Position>),
}

impl ParseError {
    pub fn input(inner: Box<Error>) -> ParseError {
        ParseError::Input(inner)
    }

    pub fn lexical<T: Into<Rc<Position>>>(pos: T, msg: String) -> ParseError {
        ParseError::Lexical(msg, pos.into())
    }

    pub fn syntax<T: Into<Rc<Position>>>(pos: T, msg: String) -> ParseError {
        ParseError::Syntax(msg, pos.into())
    }
}

impl From<io::Error> for ParseError {
    fn from(err: io::Error) -> Self {
        ParseError::input(box err as Box<Error>)
    }
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            ParseError::Input(ref e) => write!(f, "Input error: {}", e),
            ParseError::Lexical(ref msg, ref pos) |
            ParseError::Syntax(ref msg, ref pos) =>
                write!(f, "{}: {}: {}", pos, self.description(), msg),
        }
    }
}

impl Error for ParseError {
    fn description(&self) -> &str {
        match *self {
            ParseError::Input(_) => "input error",
            ParseError::Lexical(..) => "lexical error",
            ParseError::Syntax(..) => "syntax error",
        }
    }
}

pub struct PState {
    input: (Position, InputStream),
    prev_tok_len: usize,
    prev_token: Option<CToken>,
    saved_token: Option<CToken>,
    name_supply: NameSupply,
    type_idents: HashSet<Ident>,
    scopes: Vec<HashSet<Ident>>,
}

pub fn exec_parser<F, T>(input: InputStream, pos: Position, builtins: Vec<Ident>, names: NameSupply,
                         do_parse: F) -> Result<(T, NameSupply), ParseError>
    where F: FnOnce(&mut Parser) -> Result<T, ParseError>
{
    let initial_state = PState {
        input: (pos, input),
        prev_tok_len: 0,
        prev_token: None,
        saved_token: None,
        name_supply: names,
        type_idents: HashSet::from_iter(builtins),
        scopes: vec![],
    };
    let (state, res) = Parser::exec(initial_state, do_parse)?;
    Ok((res, state.name_supply))
}

pub fn exec_parser_simple<T, F>(do_parse: F, input: InputStream, pos: Position)
                                -> Result<T, ParseError>
    where F: Fn(&mut Parser) -> Result<T, ParseError>
{
    exec_parser(input, pos, builtin_type_names(), new_name_supply(), do_parse).map(|x| x.0)
}

pub fn parse(input: InputStream, initial_pos: Position) -> Result<CTranslUnit, ParseError> {
    exec_parser(input, initial_pos, builtin_type_names(), new_name_supply(),
                translation_unit).map(|x| *x.0)
}

impl Parser {
    pub fn new_name(&mut self) -> Name {
        self.user.name_supply.next().unwrap()
    }

    // lexer related API

    pub fn input(&mut self) -> &mut (Position, InputStream) {
        &mut self.user.input
    }

    pub fn set_pos(&mut self, pos: Position) {
        self.user.input.0 = pos;
    }

    pub fn pos_clone(&self) -> Position {
        self.user.input.0.clone()
    }

    pub fn set_last_tok_len(&mut self, len: usize) {
        self.user.prev_tok_len = len;
    }

    pub fn tok_str(&self) -> &str {
        self.user.input.1.last_string(self.user.prev_tok_len)
    }

    // parser related API

    fn add_typedef(&mut self, ident: Ident) {
        self.user.type_idents.insert(ident);
    }

    fn shadow_typedef(&mut self, ident: &Ident) {
        self.user.type_idents.remove(ident);
    }

    pub fn is_type_ident(&self, ident: &Ident) -> bool {
        self.user.type_idents.contains(ident)
    }

    pub fn enter_scope(&mut self) {
        // TODO: a way to avoid clone here?
        self.user.scopes.insert(0, self.user.type_idents.clone());
    }

    pub fn leave_scope(&mut self) {
        assert!(!self.user.scopes.is_empty(), "leave_scope: already in global scope");
        self.user.type_idents = self.user.scopes.remove(0);
    }

    pub fn last_token(&self) -> &CToken {
        self.user.prev_token.as_ref().expect("touched undefined token")
    }

    pub fn saved_token(&self) -> &CToken {
        self.user.saved_token.as_ref().expect("touched undefined token")
    }

    pub fn set_last_token(&mut self, tk: &CToken) {
        match *tk {
            CTokEof => {
                self.user.saved_token = self.user.prev_token.clone();
            }
            ref tok => {
                self.user.saved_token = mem::replace(&mut self.user.prev_token,
                                                     Some(tok.clone()));
            }
        }
    }

    pub fn handle_eof_token(&mut self) -> () {
        self.user.saved_token = self.user.prev_token.clone();
    }

    pub fn do_decl_ident(&mut self, declspecs: &[CDeclSpec], declr: &CDeclrR) {
        let is_typedef = |declspec: &CDeclSpec| match *declspec {
            CStorageSpec(CTypedef(_)) => true,
            _ => false,
        };

        match declr.ident {
            None => (),
            Some(ref ident) => {
                if declspecs.iter().any(is_typedef) {
                    self.add_typedef(ident.clone())
                } else {
                    self.shadow_typedef(ident)
                }
            },
        }
    }

    pub fn do_func_param_decl_ident(&mut self, decl: &CDeclr) {
        if decl.1.is_empty() {
            return;
        }
        if let CFunDeclr(Either::Right((ref params, _)), _, _) = decl.1[0] {
            for param in params {
                if let CDecl(_, ref dle, _) = *param {
                    for dl in dle {
                        if let (Some(ref declr), _, _,) = *dl {
                            if let Some(ref ident) = declr.0 {
                                self.shadow_typedef(ident);
                            }
                        }
                    }
                }
            }
        }
    }

    pub fn with_length<T, F>(&mut self, nodeinfo: NodeInfo, mk_attr_node: F) -> Result<T, ParseError>
        where F: FnOnce(NodeInfo) -> T
    {
        let last_tok = self.saved_token();
        let first_pos = nodeinfo.pos();
        let (last_pos, last_len) = last_tok.pos_len();
        let attrs = NodeInfo::new(first_pos, last_pos, last_len,
                                  nodeinfo.name().unwrap_or_else(|| panic!("nameOfNode")));
        Ok(mk_attr_node(attrs))
    }

    pub fn with_attribute<N: Pos, F>(&mut self, node: N, cattrs: Vec<CAttr>, mk_declr_node: F)
                                     -> Result<Box<CDeclrR>, ParseError>
        where F: FnOnce(NodeInfo) -> Box<CDeclrR>
    {
        let name = self.new_name();
        let attrs = NodeInfo::with_pos_name(node.pos(), name);
        let new_declr = mk_declr_node(attrs).append_attrs(cattrs);
        Ok(new_declr)
    }

    pub fn with_attribute_postfix<N, F>(&mut self, node: N, cattrs: Vec<CAttr>, mk_declr_ctor: F)
                                        -> Result<Box<FnBox(Box<CDeclrR>) -> Box<CDeclrR>>, ParseError>
        where N: Pos, F: FnOnce(NodeInfo, Box<CDeclrR>) -> Box<CDeclrR> + 'static
    {
        let name = self.new_name();
        let attrs = NodeInfo::with_pos_name(node.pos(), name);
        let new_declr: Box<FnBox(Box<CDeclrR>) -> Box<CDeclrR>> = box move |declr| {
            mk_declr_ctor(attrs, declr).append_attrs(cattrs)
        };
        Ok(new_declr)
    }
}

#[derive(Debug, Clone)]
pub struct CDeclrR {
    ident: Option<Ident>,
    inner: Vec<CDerivedDeclr>,
    asmname: Option<Box<CStrLit>>,
    cattrs: Vec<CAttr>,
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
    pub fn empty() -> Box<CDeclrR> {
        box CDeclrR { ident: None, inner: vec![], asmname: None, cattrs: vec![],
                      at: NodeInfo::undef() }
    }

    pub fn from_var(ident: Ident, ni: NodeInfo) -> Box<CDeclrR> {
        box CDeclrR { ident: Some(ident), inner: vec![], asmname: None, cattrs: vec![], at: ni }
    }

    pub fn set_asm_name(mut self, asm_name: Option<Box<CStrLit>>) -> Result<CDeclrR, ParseError> {
        if self.asmname.is_none() {
            self.asmname = asm_name;
            Ok(self)
        } else if asm_name.is_none() {
            Ok(self)
        } else {
            let newname = asm_name.unwrap();
            let oldname = self.asmname.as_ref().unwrap();
            Err(ParseError::syntax(newname.pos(),
                                   format!("Duplicate assembler name: {}, {}",
                                           oldname.0, newname.0)))
        }
    }

    pub fn with_asm_name_attrs(self, (asm_name, new_attrs): (Option<Box<CStrLit>>, Vec<CAttr>))
                               -> Result<CDeclrR, ParseError> {
        self.append_obj_attrs(new_attrs).set_asm_name(asm_name)
    }

    pub fn fun_declr(mut self: Box<Self>, params: Either<Vec<Ident>, (Vec<CDecl>, bool)>,
                     cattrs: Vec<CAttr>, at: NodeInfo) -> Box<Self> {
        self.inner.push(CFunDeclr(params, cattrs, at));
        self
    }

    pub fn arr_declr(mut self: Box<Self>, tyquals: Vec<CTypeQual>, var_sized: bool, static_size: bool,
                     size_expr_opt: Option<Box<CExpr>>, at: NodeInfo) -> Box<Self> {
        let arr_sz = match size_expr_opt {
            Some(e) => CArrSize(static_size, e),
            None => CNoArrSize(var_sized)
        };
        self.inner.push(CArrDeclr(tyquals, arr_sz, at));
        self
    }

    pub fn ptr_declr(mut self: Box<CDeclrR>, tyquals: Vec<CTypeQual>, at: NodeInfo) -> Box<Self> {
        self.inner.push(CPtrDeclr(tyquals, at));
        self
    }

    pub fn append_attrs(mut self: Box<Self>, mut new_attrs: Vec<CAttr>) -> Box<Self> {
        if self.inner.is_empty() {
            self.cattrs.append(&mut new_attrs);
        } else {
            match self.inner.last_mut().unwrap() {
                &mut CPtrDeclr(ref mut type_quals, _) => {
                    type_quals.extend(new_attrs.into_iter().map(|q| CAttrQual(box q)))
                }
                &mut CArrDeclr(ref mut type_quals, _, _) => {
                    type_quals.extend(new_attrs.into_iter().map(|q| CAttrQual(box q)))
                }
                &mut CFunDeclr(_, ref mut cattrs, _) => {
                    cattrs.append(&mut new_attrs)
                }
            }
        }
        self
    }

    pub fn append_obj_attrs(mut self, mut new_attrs: Vec<CAttr>) -> CDeclrR {
        self.cattrs.append(&mut new_attrs);
        self
    }

    pub fn reverse(self) -> Box<CDeclr> {
        let CDeclrR { ident, inner, asmname, cattrs, at } = self;
        box CDeclarator(ident, inner, asmname, cattrs, at)
    }
}
