// Original file: "TravMonad.hs"
// File auto-generated using Corollary.

use std::mem;
use either::Either::*;

use data::error::Error;
use data::node::*;
use data::name::*;
use analysis::def_table::*;
use analysis::sem_rep::*;
use analysis::sem_error::*;
use analysis::builtins::*;
use analysis::type_utils::*;
use data::ident::*;
use data::error::*;


pub fn isDeclaration(decl: &IdentDecl) -> bool {
    match *decl {
        Declaration(_) => true,
        _ => false,
    }
}

pub fn mismatchErr(ctx: &str, expect: &str, found: &str) -> String {
    format!("{}: Expected {}, but found: {}", ctx, expect, found)
}

pub fn hadHardErrors(errors: &[CError]) -> bool {
    errors.iter().any(isHardError)
}

pub fn travErrors<S>(a: &mut TravState<S>) -> Vec<CError> {
    let mut errors = mem::replace(&mut a.rerrors, vec![]);
    errors.reverse();
    errors
}

pub fn checkCompatibleTypes(_: Type, _: Type) -> Result<(), TypeMismatch> {
    // TODO: is that all?
    Ok(())
}

fn redefErr<N: CNode, O: CNode>(name: Ident, lvl: ErrorLevel, new: N, old: O,
                                kind: RedefKind) -> TravResult<()> {
    Err(redefinition(lvl,
                     name.to_string(),
                     kind,
                     new.into_node_info(),
                     old.into_node_info()).toError())
}

fn linkageErr(new_def: IdentDecl, old_def: IdentDecl) -> TravResult<()> {
    let kind = match (declLinkage(&new_def), declLinkage(&old_def)) {
        (NoLinkage, _) => NoLinkageOld,
        _ => DisagreeLinkage,
    };
    redefErr(declIdent(&new_def), LevelError, new_def, old_def, kind)
}


#[derive(Clone, Copy)]
pub enum CLanguage {
    C89,
    C99,
    GNU89,
    GNU99,
}

pub struct TravOptions {
    language: CLanguage,
}

type ExtDeclHandler<S> = Fn(&mut TravState<S>, DeclEvent);

pub struct TravState<S> {
    symbolTable: DefTable,
    rerrors: Vec<CError>,
    nameGenerator: Box<Iterator<Item=Name>>,
    userState: S,
    options: TravOptions,
}

impl<S> TravState<S> {
    fn new(userst: S) -> TravState<S> {
        TravState {
            symbolTable: DefTable::new(),
            rerrors: Vec::new(),
            nameGenerator: new_name_supply(),
            userState: userst,
            options: TravOptions { language: CLanguage::C99 },
        }
    }
}

pub struct Trav<S> {
    state: TravState<S>,
    doHandleExtDecl: Box<ExtDeclHandler<S>>,
}

pub fn runTrav<T, S, F>(state: S, mut do_traversal: F) -> Result<(T, TravState<S>), Vec<CError>>
    where F: FnMut(&mut Trav<S>) -> TravResult<T>
{
    let mut trav = Trav { state: TravState::new(state), doHandleExtDecl: Box::new(|_, _| ()) };
    trav.state.symbolTable = builtins();
    match do_traversal(&mut trav) {
        Err(trav_err) => Err(vec![trav_err]),
        Ok(_) if hadHardErrors(&trav.state.rerrors) => Err(travErrors(&mut trav.state)),
        Ok(v) => Ok((v, trav.state)),
    }
}

pub fn runTrav_<T, F>(mut do_traversal: F) -> Result<(T, Vec<CError>), Vec<CError>>
    where F: FnMut(&mut Trav<()>) -> TravResult<T>
{
    runTrav((), |trav| {
        let v = do_traversal(trav)?;
        Ok((v, travErrors(&mut trav.state)))
    }).map(|x| x.0)
}

pub type TravResult<T> = Result<T, CError>;

impl<S> Trav<S> {
    pub fn checkRedef<T: CNode, T1: CNode>(&mut self, subject: String, new_decl: T,
                                           redecl_status: DeclarationStatus<T1>) -> TravResult<()> {
        match redecl_status {
            NewDecl => Ok(()),
            Redeclared(old_def) => {
                Err(redefinition(LevelError,
                                 subject,
                                 DuplicateDef,
                                 new_decl.into_node_info(),
                                 old_def.into_node_info()).toError())
            }
            KindMismatch(old_def) => {
                Err(redefinition(LevelError,
                                 subject,
                                 DiffKindRedecl,
                                 new_decl.into_node_info(),
                                 old_def.into_node_info()).toError())
            }
            Shadowed(_old_def) => Ok(()),
            KeepDef(_old_def) => Ok(()),
        }
    }

    pub fn handleDecl(&mut self, decl: DeclEvent) -> TravResult<()> {
        (self.doHandleExtDecl)(&mut self.state, decl);
        Ok(())
    }

    pub fn handleTagDecl(&mut self, decl: TagFwdDecl) -> TravResult<()> {
        let redecl = self.state.symbolTable.declareTag(decl.sueRef(), decl.clone());
        self.checkRedef(decl.sueRef().to_string(), decl, redecl)
    }

    pub fn handleTagDef(&mut self, def: TagDef) -> TravResult<()> {
        let redecl = self.state.symbolTable.defineTag(def.sueRef(), def.clone());
        self.checkRedef(def.sueRef().to_string(), def.clone(), redecl)?;
        self.handleDecl(TagEvent(def))
    }

    pub fn handleEnumeratorDef(&mut self, enumerator: Enumerator) -> TravResult<()> {
        let ident = declIdent(&enumerator);
        let redecl = self.state.symbolTable.defineScopedIdent(ident.clone(), EnumeratorDef(enumerator));
        self.checkRedef(ident.clone().to_string(), ident, redecl)
    }

    pub fn handleTypeDef(&mut self, typeDef: TypeDef) -> TravResult<()> {
        match typeDef.clone() {
            TypeDef(ident, t1, ..) => {
                let redecl = self.state.symbolTable.defineTypeDef(ident.clone(), typeDef.clone());

                match redecl {
                    Redeclared(Left(TypeDef(_, ref t2, _, _))) if sameType(t1, t2.clone()) => (),
                    _ => self.checkRedef(ident.to_string(), typeDef.clone(), redecl)?,
                };
                self.handleDecl(TypeDefEvent(typeDef))
            }
        }
    }

    pub fn handleAsmBlock(&mut self, asm: AsmBlock) -> TravResult<()> {
        self.handleDecl(AsmEvent(asm))
    }

    pub fn _checkIdentTyRedef(&mut self, _0: IdentEntry, _1: DeclarationStatus<IdentEntry>) -> TravResult<()> {
        match (_0, _1) {
            (Right(decl), status) => self.checkVarRedef(decl, status),
            (Left(tydef), KindMismatch(old_def)) => {
                redefErr(identOfTypeDef(&tydef), LevelError, tydef, old_def, DiffKindRedecl)
            }
            (Left(tydef), Redeclared(old_def)) => {
                redefErr(identOfTypeDef(&tydef), LevelError, tydef, old_def, DuplicateDef)
            }
            (Left(_tydef), _) => Ok(()),
        }
    }

    pub fn checkVarRedef(&mut self, def: IdentDecl, redecl: DeclarationStatus<IdentEntry>) -> TravResult<()> {
        let new_ty = declType(&def);

        fn canBeOverwritten(def: &IdentDecl) -> bool {
            match *def {
                Declaration(_) => true,
                ObjectDef(ref od) => od.isTentative(),
                _ => false,
            }
        }

        fn agreeOnLinkage(new_def: &IdentDecl, old_def: &IdentDecl) -> bool {
            if declStorage(old_def) == FunLinkage(InternalLinkage) {
                true
            } else if !declStorage(new_def).hasLinkage() || !declStorage(old_def).hasLinkage() {
                false
            } else if declLinkage(new_def) != declLinkage(old_def) {
                false
            } else {
                true
            }
        }

        match redecl {
            KindMismatch(old_def) => {
                redefErr(declIdent(&def), LevelError, def, old_def, DiffKindRedecl)
            }
            KeepDef(Right(old_def)) => {
                if !agreeOnLinkage(&def, &old_def) {
                    linkageErr(def, old_def)
                } else {
                    checkCompatibleTypes(new_ty, declType(&old_def)).map_err(Error::toError)
                }
            }
            Redeclared(Right(old_def)) => {
                if !agreeOnLinkage(&def, &old_def) {
                    linkageErr(def, old_def)
                } else if !canBeOverwritten(&old_def) {
                    redefErr(declIdent(&def), LevelError, def, old_def, DuplicateDef)
                } else {
                    checkCompatibleTypes(new_ty, declType(&old_def)).map_err(Error::toError)
                }
            }
            _ => Ok(()),
        }
    }

    pub fn handleVarDecl(&mut self, is_local: bool, decl: Decl) -> TravResult<()> {
        let def = self.enterDecl(decl, |_| false)?;
        self.handleDecl(if is_local { LocalEvent(def) } else { DeclEvent(def) })
    }

    pub fn handleParamDecl(&mut self, decl: ParamDecl) -> TravResult<()> {
        match decl.clone() {
            AbstractParamDecl(_, _) => self.handleDecl(ParamEvent(decl)),
            ParamDecl(vardecl, node) => {
                let def = ObjectDef(ObjDef(vardecl, None, node));

                let redecl = self.state.symbolTable.defineScopedIdent(declIdent(&def), def.clone());

                self.checkVarRedef(def, redecl)?;
                self.handleDecl(ParamEvent(decl))
            }
        }
    }

    pub fn enterDecl<F>(&mut self, decl: Decl, cond: F) -> TravResult<IdentDecl>
        where F: Fn(&IdentDecl) -> bool
    {
        let def = Declaration(decl);
        let redecl = self.state.symbolTable.defineScopedIdentWhen(cond, declIdent(&def), def.clone());
        self.checkVarRedef(def.clone(), redecl)?;
        Ok(def)
    }

    pub fn handleFunDef(&mut self, ident: Ident, fun_def: FunDef) -> TravResult<()> {
        let def = FunctionDef(fun_def);

        let redecl = self.state.symbolTable.defineScopedIdentWhen(isDeclaration, ident, def.clone());

        self.checkVarRedef(def.clone(), redecl)?;
        self.handleDecl(DeclEvent(def))
    }

    pub fn handleObjectDef(&mut self, local: bool, ident: Ident, obj_def: ObjDef) -> TravResult<()> {
        fn isTentativeDef(def: &IdentDecl) -> bool {
            match *def {
                ObjectDef(ref object_def) => object_def.isTentative(),
                _ => false,
            }
        }

        let def = ObjectDef(obj_def);

        let redecl = {
            let shouldOverride = |old: &IdentDecl| {
                isDeclaration(old) || !isTentativeDef(&def) || isTentativeDef(old)
            };
            self.state.symbolTable.defineScopedIdentWhen(shouldOverride, ident, def.clone())
        };

        self.checkVarRedef(def.clone(), redecl)?;
        self.handleDecl(if local { LocalEvent(def) } else { DeclEvent(def) })
    }

    pub fn enterPrototypeScope(&mut self) {
        self.state.symbolTable.enterBlockScope();
    }

    pub fn leavePrototypeScope(&mut self) {
        self.state.symbolTable.leaveBlockScope();
    }

    pub fn enterFunctionScope(&mut self) {
        self.state.symbolTable.enterFunctionScope();
    }

    pub fn leaveFunctionScope(&mut self) {
        self.state.symbolTable.leaveFunctionScope();
    }

    pub fn enterBlockScope(&mut self) {
        self.state.symbolTable.enterBlockScope();
    }

    pub fn leaveBlockScope(&mut self) {
        self.state.symbolTable.leaveBlockScope();
    }

    pub fn lookupTypeDef(&mut self, ident: Ident) -> TravResult<Type> {
        match self.state.symbolTable.lookupIdent(&ident).cloned() {
            None => {
                Err(invalidAST(ident.node_info().clone(),
                               format!("unbound typeDef: {}", ident.to_string())).toError())
            }
            Some(Right(d)) => {
                Err(invalidAST(ident.node_info().clone(),
                               format!("wrong kind of object: expected typedef but found {} \
                                        (for identifier `{}')",
                                       objKindDescr(&d), ident.to_string())).toError())
            }
            Some(Left(TypeDef(def_ident, ty, _, _))) => {
                self.addRef(ident, def_ident);
                Ok(ty)
            }
        }
    }

    pub fn lookupObject(&mut self, ident: Ident) -> TravResult<Option<IdentDecl>> {
        match self.state.symbolTable.lookupIdent(&ident).cloned() {
            None => Ok(None),
            Some(Right(objdef)) => {
                self.addRef(ident, objdef.clone());
                Ok(Some(objdef))
            }
            Some(Left(_)) => {
                Err(invalidAST(ident.node_info().clone(),
                               mismatchErr("lookupObject", "an object", "a typeDef")).toError())
            }
        }
    }

    pub fn addRef<U: CNode, D: CNode>(&mut self, use_: U, def: D) {
        match (use_.into_node_info(), def.into_node_info()) {
            (NodeInfo(_, _, useName), NodeInfo(_, _, defName)) => {
                self.state.symbolTable.refTable.insert(useName, defName);
            }
            _ => {}
        }
    }

    pub fn createSUERef(&mut self, node_info: NodeInfo, ident: Option<Ident>) -> TravResult<SUERef> {
        match ident {
            Some(ident) => Ok(NamedRef(ident)),
            None => if let Some(name) = node_info.clone().name() {
                Ok(AnonymousRef(name))
            } else {
                Err(invalidAST(node_info, "struct/union/enum definition without unique name".into()).toError())
            }
        }
    }

    pub fn recordError<E: Error + 'static>(&mut self, err: E) {
        self.state.rerrors.push(err.toError());
    }

    pub fn handleTravError<T>(&mut self, v: TravResult<T>) -> TravResult<Option<T>> {
        match v {
            Ok(v) => Ok(Some(v)),
            Err(e) => {
                self.recordError(e);
                Ok(None)
            }
        }
    }

    pub fn warn<E: Error + Clone + 'static>(&mut self, err: E) {
        self.recordError(err.changeErrorLevel(LevelWarn))
    }

    pub fn withExtDeclHandler(&mut self, handler: Box<ExtDeclHandler<S>>) {
        // TODO: should this be scoped (take closure)?
        self.doHandleExtDecl = handler;
    }

    pub fn modifyUserState<F>(&mut self, mut f: F)
        where F: FnMut(&mut S)
    {
        f(&mut self.state.userState)
    }

    pub fn getUserState(&self) -> &S {
        &self.state.userState
    }

    pub fn modifyOptions<F>(&mut self, mut f: F)
        where F: FnMut(&mut TravOptions)
    {
        f(&mut self.state.options)
    }

    pub fn generateName(&mut self) -> Name {
        self.state.nameGenerator.next().unwrap()
    }
}
