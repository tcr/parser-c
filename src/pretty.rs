//! Pretty printing

use std::{fmt, io, mem, ops, slice};
use std::borrow::Cow;
use std::rc::Rc;
use std::collections::HashSet;
use either::Either;

use syntax::ast::*;
use syntax::ops::*;
use data::ident::Ident;
use data::position::Pos;
use syntax::constants::{showCChar, showCString, showCInteger, showCFloat};

const INDENT: isize = 4;

// ---------------------------------------------------------------------------------------
// Public API

/// Pretty print any AST element and return a string.
pub fn pretty_to_string<P: Pretty>(p: &P) -> String {
    let mut result = Vec::new();
    render(&p.pretty(), 100, &mut result);
    String::from_utf8(result).expect("wrote invalid utf8")
}

/// Pretty print any AST element to a Write instance.
pub fn pretty<P: Pretty, W: io::Write>(p: &P, w: &mut W) {
    render(&p.pretty(), 100, w)
}

/// Pretty print the given tranlation unit, but replace declarations from header
/// files with `#include` directives.
///
/// The resulting file may not compile (because of missing `#define` directives
/// and similar things), but is very useful for testing, as otherwise the pretty
/// printed file will be cluttered with declarations from system headers.
pub fn pretty_using_include<W: io::Write>(unit: &CTranslUnit, w: &mut W) {
    let mut header_files = HashSet::new();
    let mut doc = empty();
    for decl in &unit.0 {
        match decl.pos().file() {
            Some(ref f) if f.ends_with(".h") => {
                if header_files.insert(f.clone()) {
                    doc = doc.above(text("#include \"") + text(f.to_string()) + '"');
                }
            }
            _ => doc = doc.above(decl.pretty())
        }
    }
    if !header_files.is_empty() {
        doc = text("/* Warning: The #include directives in this file aren't necessarily correct. */")
            .above(doc);
    }
    render(&doc, 100, w)
}


// ---------------------------------------------------------------------------------------
// A very cut-down version of Haskell's Text.PrettyPrint.HughesPJ module.

/// Represents a pretty-printed document.
#[derive(Clone, PartialEq, Eq)]
pub struct Doc<'a>(Rc<EDoc<'a>>);

// Do not wrap debug output in Doc()
impl<'a> fmt::Debug for Doc<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.0.fmt(f)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum EDoc<'a> {
    /// Empty document.
    Empty,
    /// A piece of text (either a character or a string).
    Text(Text<'a>),
    /// The given document, indented by some margin.
    Nest(isize, Doc<'a>),
    /// Vertical composition.
    Above(Doc<'a>, Doc<'a>),
    /// Horizontal composition.
    Beside(Doc<'a>, Doc<'a>),
    /// A list of items, separated either vertically or horizontally.
    /// If ListStyle is Auto, the length of the content determines the
    /// actually rendered style.
    /// (This is the only case where we look at the line length.)
    List(ListStyle, bool, Vec<Doc<'a>>),
}
use self::EDoc::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ListStyle {
    Auto,
    Horizontal,
    Vertical,
}
use self::ListStyle::*;

#[derive(Debug, Clone, PartialEq, Eq)]
enum Text<'a> {
    Chr(char),
    Str(&'a str),
    String(Rc<String>),
}

// Pre-generate whitespace for indentation.
macro_rules! make_spaces {
    () => { "" };
    ($s: tt $($t: tt)*) => {
        concat!("          ", make_spaces!($($t)*))
    };
}

const SPACES: &str = make_spaces!(,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,);

#[inline]
fn mk(edoc: EDoc) -> Doc {
    Doc(Rc::new(edoc))
}

impl<'a> Doc<'a> {
    fn is_empty(&self) -> bool {
        *self.0 == Empty
    }

    /// Return the minimum width of self (i.e., Auto-style lists rendered
    /// vertically).
    fn width(&self) -> isize {
        match *self.0 {
            Empty => 0,
            Text(Text::Chr(_)) => 1,
            Text(Text::Str(st)) => st.len() as isize,
            Text(Text::String(ref st)) => st.len() as isize,
            Above(ref p, ref q) => p.width().max(q.width()),
            Beside(ref p, ref q) => p.width() + q.width(),
            Nest(k, ref p) => k + p.width(),
            List(Horizontal, sp, ref list) =>
                list.iter().map(|el| el.width()).sum::<isize>() +
                    if sp { list.len() as isize - 1 } else { 0 },
            // Assume Auto == Vertical
            List(_, _, ref list) =>
                list.iter().map(|el| el.width()).max().unwrap_or(0),
        }
    }

    /// Compose documents vertically.  Checks for Empty on either side.
    fn above(self, other: Doc<'a>) -> Doc<'a> {
        if other.is_empty() { self }
        else if self.is_empty() { other }
        else { mk(Above(self, other)) }
    }

    /// Compose documents horizontally.  Checks for Empty on either side.
    fn beside(self, other: Doc<'a>) -> Doc<'a> {
        if other.is_empty() { self }
        else if self.is_empty() { other }
        else { mk(Beside(self, other)) }
    }

    /// Nest a document.
    fn nest(self, k: isize) -> Doc<'a> {
        if self.is_empty() { self }
        else { mk(Nest(k, self)) }
    }

    /// Return an iterator that inserts `self` after each element of the given
    /// iterator.
    fn punctuate<I, It>(self, it: I) -> Punctuate<'a, I::IntoIter, It>
        where I: IntoIterator<Item=It>, It: Into<Doc<'a>>
    {
        let mut it = it.into_iter();
        Punctuate { sep: self, next: it.next(), it: it }
    }
}

struct Punctuate<'a, I, It> where I: Iterator<Item=It>, It: Into<Doc<'a>> {
    sep: Doc<'a>,
    next: Option<It>,
    it: I,
}

impl<'a, I, It> Iterator for Punctuate<'a, I, It>
    where I: Iterator<Item=It>, It: Into<Doc<'a>>
{
    type Item = Doc<'a>;
    fn next(&mut self) -> Option<Doc<'a>> {
        if self.next.is_none() { return None; }
        match mem::replace(&mut self.next, self.it.next()) {
            None => None,
            Some(el) => if self.next.is_none() {
                Some(el.into())
            } else {
                Some(el.into() + self.sep.clone())
            },
        }
    }
}


impl<'a> ops::Add<Doc<'a>> for Doc<'a> {
    type Output = Doc<'a>;

    #[inline]
    fn add(self, other: Doc<'a>) -> Self::Output {
        self.beside(other)
    }
}

impl<'a> ops::Add<&'a str> for Doc<'a> {
    type Output = Doc<'a>;

    #[inline]
    fn add(self, s: &'a str) -> Self::Output {
        self.beside(mk(Text(Text::Str(s))))
    }
}

impl<'a> ops::Add<char> for Doc<'a> {
    type Output = Doc<'a>;

    #[inline]
    fn add(self, ch: char) -> Self::Output {
        self.beside(mk(Text(Text::Chr(ch))))
    }
}


struct MaybeSpaceDoc<'a> {
    left: Doc<'a>,
}

#[derive(Clone, Copy)]
struct MaybeSpace;

const SP: MaybeSpace = MaybeSpace;

impl<'a> ops::Add<MaybeSpace> for Doc<'a> {
    type Output = MaybeSpaceDoc<'a>;

    #[inline]
    fn add(self, _: MaybeSpace) -> Self::Output {
        MaybeSpaceDoc { left: self }
    }
}

impl<'a> ops::Add<Doc<'a>> for MaybeSpaceDoc<'a> {
    type Output = Doc<'a>;

    #[inline]
    fn add(self, right: Doc<'a>) -> Self::Output {
        if self.left.is_empty() { right }
        else if right.is_empty() { self.left }
        else { self.left.beside(mk(Text(Text::Chr(' ')))).beside(right) }
    }
}


fn empty<'a>() -> Doc<'a> {
    mk(Empty)
}

fn chr<'a>(ch: char) -> Doc<'a> {
    mk(Text(Text::Chr(ch)))
}

fn text<'a, T>(s: T) -> Doc<'a> where T: Into<Cow<'a, str>> {
    match s.into() {
        Cow::Owned(s) => mk(Text(Text::String(Rc::new(s)))),
        Cow::Borrowed(s) => mk(Text(Text::Str(s))),
    }
}

fn parens<'a>(doc: Doc<'a>) -> Doc<'a> {
    chr('(') + doc + chr(')')
}

fn brackets<'a>(doc: Doc<'a>) -> Doc<'a> {
    chr('[') + doc + chr(']')
}

fn braces<'a>(doc: Doc<'a>) -> Doc<'a> {
    chr('{') + doc + chr('}')
}

fn maybe_doc<'a, F, T: 'a>(f: F, opt: &'a Option<T>) -> Doc<'a>
    where F: FnOnce(&'a T) -> Doc<'a>
{
    opt.as_ref().map_or_else(empty, f)
}

fn hcat<'a, I, It>(it: I) -> Doc<'a>
    where I: IntoIterator<Item=It>, It: Into<Doc<'a>>
{
    let v: Vec<_> = it.into_iter().map(Into::into).collect();
    mk(if v.is_empty() { Empty } else { List(Horizontal, false, v) })
}

fn hsep<'a, I, It>(it: I) -> Doc<'a>
    where I: IntoIterator<Item=It>, It: Into<Doc<'a>>
{
    let v: Vec<_> = it.into_iter().map(Into::into).collect();
    mk(if v.is_empty() { Empty } else { List(Horizontal, true, v) })
}

fn vcat<'a, I, It>(it: I) -> Doc<'a>
    where I: IntoIterator<Item=It>, It: Into<Doc<'a>>
{
    let v: Vec<_> = it.into_iter().map(Into::into).collect();
    mk(if v.is_empty() { Empty } else { List(Vertical, false, v) })
}

fn list<'a, I, It>(it: I) -> Doc<'a>
    where I: IntoIterator<Item=It>, It: Into<Doc<'a>>
{
    let v: Vec<_> = it.into_iter().map(Into::into).collect();
    mk(if v.is_empty() { Empty } else { List(Auto, true, v) })
}


struct Renderer<'w, W: io::Write + 'w> {
    writer: &'w mut W,
    line_len: isize,
    line_pos: isize,
    indent: isize,
}

impl<'w, W: io::Write + 'w> Renderer<'w, W> {
    fn write(&mut self, st: &str) -> io::Result<()> {
        self.line_pos += st.len() as isize;
        self.writer.write_all(st.as_bytes())
    }

    fn write_spaces(&mut self, mut n: isize) -> io::Result<()> {
        while n > 0 {
            let k = n.min(SPACES.len() as isize);
            self.writer.write_all(SPACES[..k as usize].as_bytes())?;
            n -= k;
        }
        Ok(())
    }

    fn newline(&mut self) -> io::Result<()> {
        self.writer.write_all(b"\n")?;
        let n = self.indent;
        self.line_pos = n;
        self.write_spaces(n)
    }

    fn render<'a>(&mut self, doc: &Doc<'a>) -> io::Result<()> {
        match *doc.0 {
            Empty => {},
            Text(Text::Chr(ch)) => {
                self.line_pos += 1;
                self.writer.write_all(ch.encode_utf8(&mut [0; 4]).as_bytes())?
            },
            Text(Text::Str(st)) => self.write(st)?,
            Text(Text::String(ref st)) => self.write(st)?,
            Beside(ref p, ref q) => {
                self.render(p)?;
                self.render(q)?;
            }
            Above(ref p, ref q) => {
                self.render(p)?;
                self.newline()?;
                self.render(q)?;
            }
            Nest(k, ref p) => {
                self.indent += k;
                self.write_spaces(k)?;
                self.line_pos += k;
                self.render(p)?;
                self.indent -= k;
            }
            List(style, space_sep, ref list) => {
                let style = match style {
                    Horizontal => Horizontal,
                    Vertical => Vertical,
                    Auto => {
                        // check horizontal width
                        let max_len = self.line_len - self.line_pos;
                        if list.iter().map(|el| el.width()).sum::<isize>() > max_len {
                            Vertical
                        } else {
                            Horizontal
                        }
                    }
                };
                let n = list.len();
                if style == Horizontal {
                    for (i, el) in list.iter().enumerate() {
                        self.render(el)?;
                        if space_sep && i < n - 1 {
                            self.write(" ")?;
                        }
                    }
                } else if style == Vertical {
                    let prev_indent = self.indent;
                    self.indent = self.line_pos;
                    for (i, el) in list.iter().enumerate() {
                        self.render(el)?;
                        if i < n - 1 {
                            self.newline()?;
                        }
                    }
                    self.indent = prev_indent;
                }
            }
        }
        Ok(())
    }
}

fn render<'a, W: io::Write>(doc: &Doc<'a>, line_len: isize, w: &mut W) {
    let mut r = Renderer { line_len, indent: 0, line_pos: 0, writer: w };
    r.render(doc).expect("error rendering pretty-printed document")
}


// ---------------------------------------------------------------------------------------
// parser-c specific Pretty trait

/// Implemented by AST nodes that can be pretty-printed.
pub trait Pretty {
    /// Render self with default precedence context.
    fn pretty<'a>(&'a self) -> Doc<'a> {
        self.pretty_prec(0)
    }

    /// Render self with given precedence context.
    fn pretty_prec<'a>(&'a self, _prec: i32) -> Doc<'a> {
        self.pretty()
    }
}

/// Pretty-print the optional value, otherwise empty document.
#[inline]
fn maybe_pretty<'a, P: 'a>(opt: &'a Option<P>) -> Doc<'a>
    where P: Pretty
{
    maybe_doc(Pretty::pretty, opt)
}

/// Parenthesize the document if the outer context has higher precedence.
fn maybe_paren<'a>(prec1: i32, prec2: i32, doc: Doc<'a>) -> Doc<'a> {
    if prec1 <= prec2 { doc } else { parens(doc) }
}

/// Non-separated list of pretty-printable items.
fn pretty_cat<'a, I, P: 'a>(it: I) -> Doc<'a>
    where I: IntoIterator<Item=&'a P>, P: Pretty
{
    hcat(it.into_iter().map(Pretty::pretty))
}

/// Comma-separated list of pretty-printable items.
fn pretty_comma<'a, I, P: 'a>(it: I) -> Punctuate<'a, impl Iterator<Item=Doc<'a>>, Doc<'a>>
    where I: IntoIterator<Item=&'a P>, P: Pretty
{
    chr(',').punctuate(it.into_iter().map(Pretty::pretty))
}

/// Space-separated list of pretty-printable items.
fn pretty_space_sep<'a, I, P: 'a>(it: I) -> Doc<'a>
    where I: IntoIterator<Item=&'a P>, P: Pretty
{
    hsep(it.into_iter().map(Pretty::pretty))
}

/// Newline-separated list of pretty-printable items.
fn pretty_nl_sep<'a, I, P: 'a>(it: I) -> Doc<'a>
    where I: IntoIterator<Item=&'a P>, P: Pretty
{
    vcat(it.into_iter().map(Pretty::pretty))
}

/// A C block, formatted vertically.
fn block<'a>(inner: Doc<'a>) -> Doc<'a> {
    chr('{').above(inner).above(chr('}'))
}

// ---------------------------------------------------------------------------------------
// Pretty implementations

impl<P> Pretty for Box<P> where P: Pretty {
    fn pretty<'a>(&'a self) -> Doc<'a> {
        (**self).pretty()
    }

    fn pretty_prec<'a>(&'a self, prec: i32) -> Doc<'a> {
        (**self).pretty_prec(prec)
    }
}

impl Pretty for Ident {
    fn pretty<'a>(&'a self) -> Doc<'a> {
        text(self.as_str())
    }
}

impl Pretty for CTranslUnit {
    fn pretty<'a>(&'a self) -> Doc<'a> {
        pretty_nl_sep(&self.0) + '\n'
    }
}

impl Pretty for CExtDecl {
    fn pretty<'a>(&'a self) -> Doc<'a> {
        match *self {
            CDeclExt(ref decl) => decl.pretty() + ';',
            CFDefExt(ref fund) => fund.pretty(),
            CAsmExt(ref asm_stmt, _) => text("asm") + parens(asm_stmt.pretty()) + ';',
        }
    }
}

fn pretty_body<'a>(body: &'a CStat) -> Doc<'a> {
    // make sure we render a statement body with braces in all cases
    if let CCompound(..) = *body {
        body.pretty_prec(-1)
    } else {
        block(body.pretty())
    }
}

impl Pretty for CFunDef {
    fn pretty<'a>(&'a self) -> Doc<'a> {
        let CFunctionDef(ref declspecs, ref declr, ref decls, ref stat, _) = *self;
        (
            pretty_space_sep(declspecs) + SP + declr.pretty()
        ).above(
            vcat(decls.iter().map(|v| v.pretty() + ';')).nest(INDENT)
        ).above(
            // make sure we render the body with braces in all cases
            pretty_body(stat)
        )
    }
}

fn pretty_else<'a>(body: &'a CStat) -> Doc<'a> {
    if let CIf(ref else_if_expr, ref else_if_stat, ref else_stat, _) = *body {
        (
            text("else if ") + parens(else_if_expr.pretty())
        ).above(
            pretty_body(else_if_stat)
        ).above(
            maybe_doc(|e| pretty_else(e), else_stat)
        )
    } else {
        text("else ").above(pretty_body(body))
    }
}

impl Pretty for CStat {
    fn pretty<'a>(&'a self) -> Doc<'a> {
        match *self {
            CLabel(ref ident, ref stat, ref cattrs, _) =>
                (ident.pretty() + ':' + SP + pretty_attrlist(cattrs)).above(stat.pretty()),
            CCase(ref expr, ref stat, _) =>
                (text("case ") + expr.pretty() + ':').above(stat.pretty()),
            CCases(ref expr1, ref expr2, ref stat, _) =>
                (text("case ") + expr1.pretty() + " ... " + expr2.pretty() + ':').above(stat.pretty()),
            CDefault(ref stat, _) =>
                text("default:").above(stat.pretty()),
            CExpr(ref expr, _) =>
                (maybe_pretty(expr) + ';').nest(INDENT),
            CCompound(..) => self.pretty_prec(0),
            CIf(ref expr, ref stat, ref estat, _) => {
                let statement = (
                    text("if ") + parens(expr.pretty())
                ).above(
                    pretty_body(stat)
                ).above(
                    maybe_doc(|e| pretty_else(e), estat)
                );
                statement.nest(INDENT)
            }
            CSwitch(ref expr, ref stat, _) =>
                (text("switch ") + parens(expr.pretty())).above(stat.pretty_prec(-1)).nest(INDENT),
            CWhile(ref expr, ref stat, false, _) =>
                (text("while ") + parens(expr.pretty())).above(stat.pretty_prec(-1)).nest(INDENT),
            CWhile(ref expr, ref stat, true, _) =>
                text("do ").above(stat.pretty_prec(-1))
                           .above(text("while ") + parens(expr.pretty()) + ';')
                           .nest(INDENT),
            CFor(ref for_init, ref cond, ref step, ref stat, _) => {
                let for_expr =
                    for_init.as_ref().either(|opt_exp| maybe_pretty(opt_exp), |decl| decl.pretty()) +
                    ';' + SP + maybe_pretty(cond) + ';' + SP + maybe_pretty(step);
                (text("for ") + parens(for_expr)).above(stat.pretty_prec(-1)).nest(INDENT)
            },
            CGoto(ref ident, _) =>
                (text("goto ") + ident.pretty() + ';').nest(INDENT),
            CGotoPtr(ref expr, _) =>
                (text("goto *") + expr.pretty_prec(30) + ';').nest(INDENT),
            CCont(_) => text("continue;").nest(INDENT),
            CBreak(_) => text("break;").nest(INDENT),
            CReturn(None, _) => text("return;").nest(INDENT),
            CReturn(Some(ref expr), _) => (text("return ") + expr.pretty() + ';').nest(INDENT),
            CAsm(ref asm_stmt, _) => asm_stmt.pretty(),
        }
    }

    fn pretty_prec<'a>(&'a self, prec: i32) -> Doc<'a> {
        match *self {
            CCompound(ref local_labels, ref bis, _) => {
                let pretty_labels = if local_labels.is_empty() { empty() } else {
                    vcat(local_labels.iter().map(|l| text("__label__ ") + l.pretty() + ';'))
                };
                let inner = block(pretty_labels.above(pretty_nl_sep(bis)));
                if prec == -1 { inner } else { inner.nest(INDENT) }
            }
            _ => self.pretty()
        }
    }
}

impl Pretty for CAsmStmt {
    fn pretty<'a>(&'a self) -> Doc<'a> {
        let CAssemblyStatement(ref ty_qual, ref expr, ref out_ops,
                               ref in_ops, ref clobbers, _) = *self;
        let inner = expr.pretty() +
            if in_ops.is_empty() && out_ops.is_empty() && clobbers.is_empty() {
                empty()
            } else {
                chr(':') + hsep(pretty_comma(out_ops)) + SP + text(":") + SP +
                    hsep(pretty_comma(in_ops)) + SP +
                    if clobbers.is_empty() { empty() } else {
                        chr(':') + SP + hsep(pretty_comma(clobbers))
                    }
            };
        (text("__asm__ ") + maybe_pretty(ty_qual) + parens(inner) + ';').nest(INDENT)
    }
}

impl Pretty for CAsmOperand {
    fn pretty<'a>(&'a self) -> Doc<'a> {
        let CAssemblyOperand(ref arg_name, ref cnstr, ref expr, _) = *self;
        maybe_doc(|n| brackets(n.pretty()), arg_name) + cnstr.pretty() + SP +
            parens(expr.pretty())
    }
}

// TODO: Check need of __extension__
impl Pretty for CBlockItem {
    fn pretty<'a>(&'a self) -> Doc<'a> {
        match *self {
            CBlockStmt(ref stat) => stat.pretty(),
            CBlockDecl(ref decl) => (decl.pretty() + ';').nest(INDENT),
            CNestedFunDef(ref fundef) => fundef.pretty().nest(INDENT),
        }
    }
}

impl Pretty for CDecl {
    // CAVEAT:
    // we may not print __attribute__s directly after typespecs,
    // as this may change the semantics of the declaration.
    // The parser fixes this, but to avoid hard-to-track code generator
    // errors, we enforce this invariant on the AST level.
    fn pretty<'a>(&'a self) -> Doc<'a> {
        match *self {
            CDecl(ref specs, ref divs, _) => {
                for item in specs.windows(2) {
                    if let &[CTypeSpec(ref ty), CTypeQual(CAttrQual(_))] = item {
                        if ty.isSUEDef() {
                            panic!("AST Invariant violated: __attribute__ specifier following struct/union/enum");
                        }
                    }
                }
                let pretty_divs = hsep(chr(',').punctuate(divs.iter().map(|&(ref declr, ref initializer, ref expr)| {
                    maybe_doc(|d| pretty_declarator(d, 0, false), declr) + SP +
                        maybe_doc(|e| text(": ") + e.pretty(), expr) + SP +
                    {
                        let attrlist = match *declr {
                            None => &[][..],
                            Some(box CDeclarator(_, _, _, ref cattrs, _)) => cattrs,
                        };
                        pretty_attrlist(attrlist)
                    } + SP + maybe_doc(|i| text("= ") + i.pretty(), initializer)
                })));
                pretty_space_sep(specs) + SP + pretty_divs
            }
            CStaticAssert(ref expr, ref string, _) =>
                text("_Static_assert") + parens(expr.pretty() + ',' + SP + string.pretty())
        }
    }
}

impl Pretty for CDeclSpec {
    fn pretty<'a>(&'a self) -> Doc<'a> {
        match *self {
            CStorageSpec(ref sp) => sp.pretty(),
            CTypeSpec(ref sp) => sp.pretty(),
            CTypeQual(ref qu) => qu.pretty(),
            CFunSpec(ref fs) => fs.pretty(),
            CAlignSpec(ref sa) => sa.pretty(),
        }
    }
}

impl Pretty for CAlignSpec {
    fn pretty<'a>(&'a self) -> Doc<'a> {
        match *self {
            CAlignAsType(ref decl, _) => text("_Alignas") + parens(decl.pretty()),
            CAlignAsExpr(ref expr, _) => text("_Alignas") + parens(expr.pretty()),
        }
    }
}

impl Pretty for CStorageSpec {
    fn pretty<'a>(&'a self) -> Doc<'a> {
        match *self {
            CAuto(_) => text("auto"),
            CRegister(_) => text("register"),
            CStatic(_) => text("static"),
            CExtern(_) => text("extern"),
            CTypedef(_) => text("typedef"),
            CThread(_) => text("_Thread_local"),
        }
    }
}

impl Pretty for CTypeSpec {
    fn pretty<'a>(&'a self) -> Doc<'a> {
        match *self {
            CVoidType(_) => text("void"),
            CCharType(_) => text("char"),
            CShortType(_) => text("short"),
            CIntType(_) => text("int"),
            CLongType(_) => text("long"),
            CFloatType(_) => text("float"),
            CFloat128Type(_) => text("__float128"),
            CDoubleType(_) => text("double"),
            CSignedType(_) => text("signed"),
            CUnsigType(_) => text("unsigned"),
            CBoolType(_) => text("_Bool"),
            CComplexType(_) => text("_Complex"),
            CInt128Type(_) => text("__int128"),
            CSUType(ref su, _) => su.pretty(),
            CEnumType(ref enum_, _) => enum_.pretty(),
            CTypeDef(ref ident, _) => ident.pretty(),
            CTypeOfExpr(ref expr, _) => text("typeof") + parens(expr.pretty()),
            CTypeOfType(ref decl, _) => text("typeof") + parens(decl.pretty()),
            CAtomicType(ref decl, _) => text("_Atomic") + parens(decl.pretty()),
        }
    }
}

impl Pretty for CTypeQual {
    fn pretty<'a>(&'a self) -> Doc<'a> {
        match *self {
            CConstQual(_) => text("const"),
            CVolatQual(_) => text("volatile"),
            CRestrQual(_) => text("__restrict"),
            CAtomicQual(_) => text("_Atomic"),
            CAttrQual(ref attr) => pretty_attrlist(slice::from_ref(attr)),
            CNullableQual(_) => text("_Nullable"),
            CNonnullQual(_) => text("_Nonnull"),
        }
    }
}

impl Pretty for CFunSpec {
    fn pretty<'a>(&'a self) -> Doc<'a> {
        match *self {
            CInlineQual(_) => text("inline"),
            CNoreturnQual(_) => text("_Noreturn"),
        }
    }
}

impl Pretty for CStructUnion {
    fn pretty<'a>(&'a self) -> Doc<'a> {
        let CStructureUnion(ref tag, ref ident, ref declrs, ref cattrs, _) = *self;
        let head = tag.pretty() + SP + pretty_attrlist(cattrs) + SP + maybe_pretty(ident);
        match *declrs {
            None => head,
            Some(ref decls) if decls.is_empty() => head + " { }",
            Some(ref decls) => {
                vcat(vec![
                    head + " {",
                    list(decls.iter().map(|decl| decl.pretty() + ';')).nest(INDENT),
                    chr('}')
                ])
            },
        }
    }
}

impl Pretty for CStructTag {
    fn pretty<'a>(&'a self) -> Doc<'a> {
        match *self {
            CStructTag => text("struct"),
            CUnionTag => text("union"),
        }
    }
}


impl Pretty for CEnum {
    fn pretty<'a>(&'a self) -> Doc<'a> {
        let CEnumeration(ref enum_ident, ref vals, ref cattrs, _) = *self;
        let head = text("enum") + SP + pretty_attrlist(cattrs) + SP + maybe_pretty(enum_ident);
        match *vals {
            None => head,
            Some(ref vals) => {
                let v = chr(',').punctuate(vals.iter().map(|&(ref ident, ref expr)| {
                    ident.pretty() + maybe_doc(|e| text(" = ") + e.pretty(), expr)
                }));
                vcat(vec![
                    head + " {",
                    list(v).nest(INDENT),
                    chr('}'),
                ])
            }
        }
    }
}

fn pp_decl<'a>(declrs: &'a [CDerivedDeclr], name: &'a Option<Ident>, prec: i32)
               -> Doc<'a> {
    match declrs {
        &[] => maybe_pretty(name),
        &[ref rest.., CPtrDeclr(ref quals, _)] => {
            let inner = if quals.is_empty() {
                chr('*') + pp_decl(rest, name, 5)
            } else {
                chr('*') + pretty_space_sep(quals) + SP + pp_decl(rest, name, 5)
            };
            maybe_paren(prec, 5, inner)
        }
        &[ref rest.., CArrDeclr(ref quals, ref size, _)] => {
            let inner = pp_decl(rest, name, 6) +
                brackets(pretty_space_sep(quals) + SP + size.pretty());
            maybe_paren(prec, 6, inner)
        }
        &[ref rest.., CFunDeclr(ref params, ref fun_attrs, _)] => {
            let pretty_params = match *params {
                Either::Right((ref decls, is_variadic)) =>
                    list(pretty_comma(decls)) +
                        if is_variadic { text(", ...") } else { empty() },
                Either::Left(ref old_style_ids) =>
                    list(pretty_comma(old_style_ids)),
            };
            let pretty_attrs = if fun_attrs.is_empty() {
                pp_decl(rest, name, 6)
            } else {
                parens(pretty_attrlist(fun_attrs) + pp_decl(rest, name, 5))
            };
            pretty_attrs + parens(pretty_params)
        }
    }
}

fn pretty_declarator<'a>(declr: &'a CDeclr, prec: i32, show_attrs: bool)
                         -> Doc<'a> {

    let CDeclarator(ref name, ref derived_declrs, ref asmname, ref cattrs, _) = *declr;
    pp_decl(derived_declrs, name, prec) + SP +
        maybe_doc(|n| text("__asm__") + parens(n.pretty()), asmname) +
        if show_attrs { pretty_attrlist(cattrs) } else { empty() }
}

impl Pretty for CDeclr {
    fn pretty_prec<'a>(&'a self, prec: i32) -> Doc<'a> {
        pretty_declarator(self, prec, true)
    }
}

impl Pretty for CArrSize {
    fn pretty<'a>(&'a self) -> Doc<'a> {
        match *self {
            CNoArrSize(complete_type) => if complete_type { chr('*') } else { empty() },
            CArrSize(static_mod, ref expr) =>
                (if static_mod { text("static ") } else { empty() }) + expr.pretty(),
        }
    }
}

impl Pretty for CInit {
    fn pretty<'a>(&'a self) -> Doc<'a> {
        match *self {
            CInitExpr(ref expr, _) => expr.pretty(),
            CInitList(ref initl, _) => {
                let initlist = initl.iter().map(|&(ref desigs, ref initializer)| if desigs.is_empty() {
                    initializer.pretty()
                } else {
                    pretty_space_sep(desigs) + " = " + initializer.pretty()
                });
                braces(list(chr(',').punctuate(initlist)))
            }
        }
    }
}

impl Pretty for CDesignator {
    fn pretty<'a>(&'a self) -> Doc<'a> {
        match *self {
            CArrDesig(ref expr, _) => brackets(expr.pretty()),
            CMemberDesig(ref ident, _) => chr('.') + ident.pretty(),
            CRangeDesig(ref expr1, ref expr2, _) =>
                brackets(expr1.pretty() + " ... " + expr2.pretty()),
        }
    }
}

impl Pretty for CAttr {
    fn pretty<'a>(&'a self) -> Doc<'a> {
        let CAttribute(ref name, ref params, _) = *self;
        if params.is_empty() {
            name.pretty()
        } else {
            name.pretty() + parens(list(pretty_comma(params)))
        }
    }
}

/// Pretty-print an attribute list.
fn pretty_attrlist<'a>(attrs: &'a [CAttr]) -> Doc<'a> {
    if attrs.is_empty() {
        empty()
    } else {
        text("__attribute__") + parens(parens(hsep(pretty_comma(attrs))))
    }
}

impl Pretty for CExpr {
    fn pretty_prec<'a>(&'a self, prec: i32) -> Doc<'a> {
        match *self {
            CComma(ref exprs, _) => {
                let inner = exprs.iter().map(|ex| ex.pretty_prec(2));
                maybe_paren(prec, -1, hsep(chr(',').punctuate(inner)))
            }
            CAssign(ref op, ref expr1, ref expr2, _) => {
                // NB: parens required around operator in lhs, even though precedence is higher
                let inner = expr1.pretty_prec(20) + SP + op.pretty() + SP + expr2.pretty_prec(2);
                maybe_paren(prec, 2, inner)
            }
            CCond(ref expr1, ref expr2, ref expr3, _) => {
                // NB: assignment only has a higher precedence if cond is on the rhs
                let inner = expr1.pretty_prec(4) + " ? " + maybe_pretty(expr2) + " : " +
                    expr3.pretty_prec(4);
                maybe_paren(prec, 2, inner)
            }
            CBinary(ref op, ref expr1, ref expr2, _) => {
                let op_prec = bin_prec(op);
                let inner = expr1.pretty_prec(op_prec) + SP + op.pretty() + SP +
                    expr2.pretty_prec(op_prec + 1);
                maybe_paren(prec, op_prec, inner)
            }
            CCast(ref decl, ref expr, _) => {
                let inner = parens(decl.pretty()) + expr.pretty_prec(25);
                maybe_paren(prec, 25, inner)
            }
            CUnary(CPostIncOp, ref expr, _) => {
                maybe_paren(prec, 26, expr.pretty_prec(26) + "++")
            }
            CUnary(CPostDecOp, ref expr, _) => {
                maybe_paren(prec, 26, expr.pretty_prec(26) + "--")
            }
            CUnary(ref op, ref expr, _) => {
                if let box CUnary(..) = *expr {
                    // parens aren't necessary, but look nicer imho
                    maybe_paren(prec, 25, op.pretty() + parens(expr.pretty_prec(25)))
                } else {
                    maybe_paren(prec, 25, op.pretty() + expr.pretty_prec(25))
                }
            }
            CSizeofExpr(ref expr, _) => {
                maybe_paren(prec, 25, text("sizeof") + parens(expr.pretty()))
            }
            CSizeofType(ref decl, _) => {
                maybe_paren(prec, 25, text("sizeof") + parens(decl.pretty()))
            }
            CAlignofExpr(ref expr, _) => {
                maybe_paren(prec, 25, text("__alignof") + parens(expr.pretty()))
            }
            CAlignofType(ref decl, _) => {
                maybe_paren(prec, 25, text("__alignof") + parens(decl.pretty()))
            }
            CComplexReal(ref expr, _) => {
                let inner = text("__real__") + parens(expr.pretty());
                maybe_paren(prec, 25, inner)
            }
            CComplexImag(ref expr, _) => {
                let inner = text("__imag__") + parens(expr.pretty());
                maybe_paren(prec, 25, inner)
            }
            CIndex(ref expr1, ref expr2, _) => {
                let inner = expr1.pretty_prec(26) + brackets(expr2.pretty());
                maybe_paren(prec, 26, inner)
            }
            CCall(ref expr, ref args, _) => {
                let inner = expr.pretty_prec(30) + parens(list(pretty_comma(args)));
                maybe_paren(prec, 30, inner)
            }
            CMember(ref expr, ref ident, deref, _) => {
                let inner = expr.pretty_prec(26) + (if deref { "->" } else { "." }) + ident.pretty();
                maybe_paren(prec, 26, inner)
            }
            CVar(ref ident, _) => {
                ident.pretty()
            }
            CConst(ref constant) => {
                constant.pretty()
            }
            CCompoundLit(ref decl, ref initl, _) => {
                let initlist = initl.iter().map(|&(ref mems, ref initializer)| if mems.is_empty() {
                    initializer.pretty()
                } else {
                    pretty_cat(mems) + " = " + initializer.pretty()
                });
                parens(decl.pretty()) + SP + braces(hsep(chr(',').punctuate(initlist)))
            }
            CStatExpr(ref stat, _) => {
                parens(stat.pretty())
            }
            CLabAddrExpr(ref ident, _) => {
                text("&&") + ident.pretty()
            }
            CGenericSelection(ref expr, ref assoc_list, _) => {
                let mut genlist = vec![expr.pretty()];
                for &(ref mty, ref expr1) in assoc_list {
                    genlist.push(
                        mty.as_ref().map_or_else(|| text("default"), |v| v.pretty()) +
                            ": " + expr1.pretty()
                    );
                }
                text("_Generic") + parens(hsep(chr(',').punctuate(genlist)))
            }
            CBuiltinExpr(ref builtin) => {
                builtin.pretty()
            }
        }
    }
}

impl Pretty for CBuiltin {
    fn pretty<'a>(&'a self) -> Doc<'a> {
        match *self {
            CBuiltinVaArg(ref expr, ref ty_name, _) =>
                text("__builtin_va_arg") + parens(expr.pretty() + ", " + ty_name.pretty()),
            CBuiltinOffsetOf(ref ty_name, ref parts, _) => match parts.as_slice() {
                // The first desig has to be a member field.
                &[CMemberDesig(ref field1, _), ref rest..] =>
                    text("__builtin_offsetof") + parens(ty_name.pretty() + ", " +
                                                        field1.pretty() + pretty_cat(rest)),
                _ => panic!("Inconsistent AST: Cannot interpret designators in offsetOf")
            },
            CBuiltinTypesCompatible(ref ty1, ref ty2, _) =>
                text("__builtin_types_compatible_p") + parens(ty1.pretty() + ", " + ty2.pretty()),
            CBuiltinConvertVector(ref expr, ref ty, _) =>
                text("__builtin_convertvector") + parens(expr.pretty() + ", " + ty.pretty()),
        }
    }
}

impl Pretty for CAssignOp {
    fn pretty<'a>(&'a self) -> Doc<'a> {
        text(match *self {
            CAssignOp => "=",
            CMulAssOp => "*=",
            CDivAssOp => "/=",
            CRmdAssOp => "%=",
            CAddAssOp => "+=",
            CSubAssOp => "-=",
            CShlAssOp => "<<=",
            CShrAssOp => ">>=",
            CAndAssOp => "&=",
            CXorAssOp => "^=",
            COrAssOp  => "|=",
        })
    }
}

impl Pretty for CBinaryOp {
    fn pretty<'a>(&'a self) -> Doc<'a> {
        text(match *self {
            CMulOp => "*",
            CDivOp => "/",
            CRmdOp => "%",
            CAddOp => "+",
            CSubOp => "-",
            CShlOp => "<<",
            CShrOp => ">>",
            CLeOp  => "<",
            CGrOp  => ">",
            CLeqOp => "<=",
            CGeqOp => ">=",
            CEqOp  => "==",
            CNeqOp => "!=",
            CAndOp => "&",
            CXorOp => "^",
            COrOp  => "|",
            CLndOp => "&&",
            CLorOp => "||",
        })
    }
}

impl Pretty for CUnaryOp {
    fn pretty<'a>(&'a self) -> Doc<'a> {
        text(match *self {
            CPreIncOp  => "++",
            CPreDecOp  => "--",
            CPostIncOp => "++",
            CPostDecOp => "--",
            CAdrOp     => "&",
            CIndOp     => "*",
            CPlusOp    => "+",
            CMinOp     => "-",
            CCompOp    => "~",
            CNegOp     => "!",
        })
    }
}

impl Pretty for CConst {
    fn pretty<'a>(&'a self) -> Doc<'a> {
        match *self {
            CIntConst(ref int, _) => text(showCInteger(int)),
            CCharConst(ref chr, _) => text(showCChar(chr)),
            CFloatConst(ref flt, _) => text(showCFloat(flt)),
            CStrConst(ref string, _) => text(showCString(string)),
        }
    }
}

impl Pretty for CStrLit {
    fn pretty<'a>(&'a self) -> Doc<'a> {
        text(showCString(&self.0))
    }
}

fn bin_prec(op: &CBinaryOp) -> i32 {
    match *op {
        CMulOp => 20,
        CDivOp => 20,
        CRmdOp => 20,
        CAddOp => 19,
        CSubOp => 19,
        CShlOp => 18,
        CShrOp => 18,
        CLeOp  => 17,
        CGrOp  => 17,
        CLeqOp => 17,
        CGeqOp => 17,
        CEqOp  => 16,
        CNeqOp => 16,
        CAndOp => 15,
        CXorOp => 14,
        COrOp  => 13,
        CLndOp => 12,
        CLorOp => 11,
    }
}
