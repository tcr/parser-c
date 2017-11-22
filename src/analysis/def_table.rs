// Original file: "DefTable.hs"
// File auto-generated using Corollary.

use std::collections::HashMap;
use std::collections::BTreeMap;
use std::hash::Hash;
use either::Either;
use either::Either::*;

use data::ident::Ident;
use data::name::*;
use data::ident::*;
use data::node::*;
use analysis::name_space_map::*;
use analysis::sem_rep::*;

pub type IdentEntry = Either<TypeDef, IdentDecl>;

pub fn identOfTyDecl(entry: &IdentEntry) -> Ident {
    entry.as_ref().either(identOfTypeDef, declIdent)
}

// TODO: use a similar dedicated data structure?
type NameMap<V> = HashMap<Name, V>;

#[derive(Clone)]
pub enum TagFwdDecl {
    CompDecl(CompTypeRef),
    EnumDecl(EnumTypeRef),
}
pub use self::TagFwdDecl::*;

impl HasSUERef for TagFwdDecl {
    fn sueRef(&self) -> SUERef {
        match *self {
            CompDecl(ref ctr) => ctr.sueRef(),
            EnumDecl(ref etr) => etr.sueRef(),
        }
    }
}

impl CNode for TagFwdDecl {
    fn node_info(&self) -> &NodeInfo {
        match *self {
            CompDecl(ref ctr) => ctr.node_info(),
            EnumDecl(ref etr) => etr.node_info(),
        }
    }
    fn into_node_info(self) -> NodeInfo {
        match self {
            CompDecl(ctr) => ctr.into_node_info(),
            EnumDecl(etr) => etr.into_node_info(),
        }
    }
}

pub type TagEntry = Either<TagFwdDecl, TagDef>;

pub struct DefTable {
    identDecls: NameSpaceMap<Ident, IdentEntry>,
    tagDecls: NameSpaceMap<SUERef, TagEntry>,
    labelDefs: NameSpaceMap<Ident, Ident>,
    memberDecls: NameSpaceMap<Ident, MemberDecl>,
    pub refTable: NameMap<Name>, //TODO
    typeTable: NameMap<Type>,
}

impl DefTable {
    pub fn new() -> DefTable {
        DefTable { identDecls: NameSpaceMap::new(),
                   tagDecls: NameSpaceMap::new(),
                   labelDefs: NameSpaceMap::new(),
                   memberDecls: NameSpaceMap::new(),
                   refTable: NameMap::default(),
                   typeTable: NameMap::default() }
    }

    pub fn lookupIdent(&self, ident: &Ident) -> Option<&IdentEntry> {
        self.identDecls.lookupName(ident)
    }

    pub fn lookupTag(&self, sue_ref: &SUERef) -> Option<&TagEntry> {
        self.tagDecls.lookupName(sue_ref)
    }

    pub fn lookupLabel(&self, ident: &Ident) -> Option<&Ident> {
        self.labelDefs.lookupName(ident)
    }

    pub fn lookupIdentInner(&self, ident: &Ident) -> Option<&IdentEntry> {
        self.identDecls.lookupInnermostScope(ident)
    }

    pub fn lookupTagInner(&self, sue_ref: &SUERef) -> Option<&TagEntry> {
        self.tagDecls.lookupInnermostScope(sue_ref)
    }

    pub fn insertType(&mut self, n: Name, t: Type) {
        self.typeTable.insert(n, t);
    }

    pub fn lookupType(&self, n: Name) -> Option<&Type> {
        self.typeTable.get(&n)
    }

    pub fn defineScopedIdent(&mut self, ident: Ident, def: IdentDecl) -> DeclarationStatus<IdentEntry> {
        self.defineScopedIdentWhen(|_| true, ident, def)
    }

    pub fn globalDefs(&self) -> GlobalDecls {
        let gtags = self.tagDecls.globalNames().iter().filter_map(|(k, v)| {
            match *v {
                Left(_) => None,
                Right(ref y) => Some((k.clone(), y.clone()))
            }
        }).collect();
        let mut decls = GlobalDecls {
            gObjs: BTreeMap::new(),
            gTags: gtags,
            gTypeDefs: BTreeMap::new(),
        };
        let global_names = self.identDecls.globalNames();
        for (ident, what) in global_names {
            match *what {
                Left(ref tydef) => { decls.gTypeDefs.insert(ident.clone(), tydef.clone()); }
                Right(ref obj) => { decls.gObjs.insert(ident.clone(), obj.clone()); }
            }
        }
        decls
    }

    pub fn inFileScope(&self) -> bool {
        !(self.identDecls.hasLocalNames() || self.labelDefs.hasLocalNames())
    }

    pub fn declareTag(&mut self, sueref: SUERef, decl: TagFwdDecl) -> DeclarationStatus<TagEntry> {
        match self.lookupTag(&sueref) {
            None => (),
            Some(old_def) if tagKind(old_def) == tagKind(&Left(decl)) =>
                return KeepDef(old_def.clone()),
            Some(old_def) =>
                return KindMismatch(old_def.clone()),
        }
        self.tagDecls.defLocal(sueref, Left(decl));
        NewDecl
    }

    pub fn defineScopedIdentWhen<F>(&mut self, override_def: F, ident: Ident, def: IdentDecl)
                                    -> DeclarationStatus<IdentEntry>
        where F: Fn(&IdentDecl) -> bool
    {

        let new_def = Right(def);

        let old_decls = self.identDecls.clone();
        let old_decl_opt = old_decls.lookupInnermostScope(&ident).cloned();

        let doOverride = |def: &IdentEntry| match *def {
            Left(_) => false,
            Right(ref old_def) => override_def(old_def),
        };

        self.identDecls.defLocal(ident.clone(), new_def.clone());

        if old_decl_opt.as_ref().map_or(false, |old_decl| !compatIdentEntry(old_decl, &new_def)) {
            KindMismatch(old_decl_opt.unwrap())
        } else if old_decl_opt.as_ref().map_or(true, doOverride) {
            defRedeclStatusLocal(compatIdentEntry, &ident, new_def,
                                 old_decl_opt, &self.identDecls)
        } else {
            self.identDecls = old_decls;
            old_decl_opt.map_or(NewDecl, KeepDef)
        }
    }

    pub fn defineTypeDef(&mut self, ident: Ident, tydef: TypeDef) -> DeclarationStatus<IdentEntry> {
        let old_decl = self.identDecls.defLocal(ident, Left(tydef.clone()));
        defRedeclStatus(compatIdentEntry, Left(tydef), old_decl)
    }

    pub fn defineGlobalIdent(&mut self, ident: Ident, def: IdentDecl) -> DeclarationStatus<IdentEntry> {
        let old_decl = self.identDecls.defGlobal(ident, Right(def.clone()));
        defRedeclStatus(compatIdentEntry, Right(def), old_decl)
    }

    pub fn defineTag(&mut self, sueref: SUERef, def: TagDef) -> DeclarationStatus<TagEntry> {
        let old_decl = self.tagDecls.defLocal(sueref.clone(), Right(def.clone()));

        let redeclStatus = match old_decl {
            Some(fwd_decl @ Left(_)) => {
                if tagKind(&fwd_decl) == tagKind(&Right(def)) {
                    NewDecl
                } else {
                    KindMismatch(fwd_decl)
                }
            }
            _ => {
                defRedeclStatusLocal(compatTagEntry,
                                     &sueref,
                                     Right(def),
                                     old_decl,
                                     &self.tagDecls)
            }
        };

        redeclStatus
    }

    pub fn defineLabel(&mut self, ident: Ident) -> DeclarationStatus<Ident> {
        let old_label = self.labelDefs.defLocal(ident.clone(), ident);
        old_label.map_or(NewDecl, Redeclared)
    }

    pub fn enterLocalScope(&mut self) {
        self.identDecls.enterNewScope();
        self.tagDecls.enterNewScope();
    }

    pub fn leaveLocalScope(&mut self) {
        self.identDecls.leaveScope();
        self.tagDecls.leaveScope();
    }

    pub fn enterFunctionScope(&mut self) {
        self.labelDefs.enterNewScope();
        self.enterLocalScope();
    }

    pub fn leaveFunctionScope(&mut self) {
        self.labelDefs.leaveScope();
        self.leaveLocalScope();
    }

    pub fn enterBlockScope(&mut self) {
        self.labelDefs.enterNewScope();
        self.enterLocalScope();
    }

    pub fn leaveBlockScope(&mut self) {
        self.labelDefs.leaveScope();
        self.leaveLocalScope();
    }

    pub fn enterMemberDecl(&mut self) {
        self.memberDecls.enterNewScope();
    }

    pub fn leaveMemberDecl(&mut self) -> Vec<MemberDecl> {
        self.memberDecls.leaveScope().into_iter().map(|kv| kv.1).collect()
    }
}

#[derive(Clone, Debug)]
pub enum DeclarationStatus<T> {
    NewDecl,
    Redeclared(T),
    KeepDef(T),
    Shadowed(T),
    KindMismatch(T),
}
pub use self::DeclarationStatus::*;

pub fn declStatusDescr<T>(decl: DeclarationStatus<T>) -> &'static str {
    match decl {
        NewDecl => "new",
        Redeclared(_) => "redeclared",
        KeepDef(_) => "keep old",
        Shadowed(_) => "shadowed",
        KindMismatch(_) => "kind mismatch",
    }
}

pub fn compatIdentEntry(entry: &IdentEntry, other: &IdentEntry) -> bool {
    match (entry, other) {
        (&Left(_), &Left(_)) => true,
        (&Left(_), &Right(_)) | (&Right(_), &Left(_)) => false,
        (&Right(ref def), &Right(ref other_def)) => match (def, other_def) {
            // TODO: all true?!
            (&EnumeratorDef(_), &EnumeratorDef(_)) => true,
            (&EnumeratorDef(_), _) => true,
            (_, &EnumeratorDef(_)) => true,
            (_, _) => true,
        },
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum TagEntryKind {
    CompKind(CompTyKind),
    EnumKind,
}
pub use self::TagEntryKind::*;

pub fn tagKind(tag: &TagEntry) -> TagEntryKind {
    match *tag {
        Left(CompDecl(ref cd)) => CompKind(cd.compTag()),
        Left(EnumDecl(_)) => EnumKind,
        Right(CompDef(ref cd)) => CompKind(cd.compTag()),
        Right(EnumDef(_)) => EnumKind,
    }
}

pub fn compatTagEntry(te1: &TagEntry, te2: &TagEntry) -> bool {
    tagKind(te1) == tagKind(te2)
}

pub fn defRedeclStatus<V: Clone, F>(sameKind: F, def: V, old_decl: Option<V>) -> DeclarationStatus<V>
    where F: Fn(&V, &V) -> bool
{
    match old_decl {
        None => NewDecl,
        Some(def_q) => {
            if sameKind(&def, &def_q) {
                Redeclared(def_q)
            } else {
                KindMismatch(def_q)
            }
        }
    }
}

pub fn defRedeclStatusLocal<K: Ord, V: Clone, F>(sameKind: F, ident: &K, def: V,
                                                 old_decl: Option<V>, nsm: &NameSpaceMap<K, V>)
                                                 -> DeclarationStatus<V>
    where F: Fn(&V, &V) -> bool
{
    match defRedeclStatus(sameKind, def, old_decl) {
        NewDecl => {
            match nsm.lookupName(ident) {
                Some(shadowed) => Shadowed(shadowed.clone()),
                None => NewDecl,
            }
        }
        redecl => redecl,
    }
}

fn mergeHashMap<K: Hash + Eq, V>(mut h1: HashMap<K, V>, h2: HashMap<K, V>) -> HashMap<K, V> {
    for (k, v) in h2 {
        h1.insert(k, v);
    }
    h1
}

pub fn mergeDefTable(dt1: DefTable, dt2: DefTable) -> DefTable {
    DefTable {
        identDecls: mergeNameSpace(dt1.identDecls, dt2.identDecls),
        tagDecls: mergeNameSpace(dt1.tagDecls, dt2.tagDecls),
        labelDefs: mergeNameSpace(dt1.labelDefs, dt2.labelDefs),
        memberDecls: mergeNameSpace(dt1.memberDecls, dt2.memberDecls),
        refTable: mergeHashMap(dt1.refTable, dt2.refTable),
        typeTable: mergeHashMap(dt1.typeTable, dt2.typeTable),
    }
}
