#![feature(proc_macro)]
#![recursion_limit="128"]

extern crate proc_macro;
extern crate syn;
#[macro_use] extern crate quote;

use syn::Body;
use proc_macro::TokenStream;
use quote::{ToTokens, Tokens};

#[proc_macro_derive(CNode)]
pub fn cnodeable(input: TokenStream) -> TokenStream {
    // Construct a string representation of the type definition
    let s = input.to_string();

    // Parse the string representation
    let ast = syn::parse_macro_input(&s).unwrap();

    // Build the impl
    let gen = impl_cnodeable(&ast);

    // Return the generated impl
    gen.parse().unwrap()
}

fn impl_cnodeable(ast: &syn::MacroInput) -> quote::Tokens {
    if ast.generics.ty_params.len() != 1 {
        panic!("Expected type with 1 type parameter.");
    }
    let ty_param = ast.generics.ty_params[0].ident.to_string();
    let pub_ty_param = format!("pub {}", ty_param);
    match ast.body {
        Body::Enum(ref variants) => {
            let mut arms = Vec::new();
            let mut arms_ref = Vec::new();
            for var in variants {
                // get args, opaque everything except for node, return node
                let mut has_node = false;
                let mut args = Vec::new();
                let mut args_ref = Vec::new();
                if let &syn::VariantData::Tuple(ref inner) = &var.data {
                    for item in inner {
                        let mut tokens = Tokens::new();
                        item.to_tokens(&mut tokens);
                        let arg = tokens.to_string();
                        if arg == ty_param {
                            has_node = true;
                            args.push(quote!(node));
                            args_ref.push(quote!(ref node));
                        } else {
                            args.push(quote!(_));
                            args_ref.push(quote!(_));
                        }
                    }
                } else {
                    unreachable!("Expected enum tuple.");
                }
                let name = &var.ident;
                if !has_node {
                    arms.push(quote! { #name ( node ) => node.into_node_info(), });
                    arms_ref.push(quote! { #name ( ref node ) => node.node_info(), });
                } else {
                    arms.push(quote! { #name ( #(#args),* ) => node, });
                    arms_ref.push(quote! { #name ( #(#args_ref),* ) => node, });
                }
            }

            let name = &ast.ident;
            quote! {
                impl CNode for #name<NodeInfo> {
                    fn node_info(&self) -> &NodeInfo {
                        match *self {
                            #(#arms_ref)*
                        }
                    }
                    fn into_node_info(self) -> NodeInfo {
                        match self {
                            #(#arms)*
                        }
                    }
                }
            }
        }
        Body::Struct(ref var) => {
            // get args, opaque everything except for node, return node
            let node_pos: Option<usize> = if let &syn::VariantData::Tuple(ref inner) = var {
                inner.iter().position(|item| {
                    let mut tokens = Tokens::new();
                    item.to_tokens(&mut tokens);
                    let arg = tokens.to_string();
                    arg == ty_param || arg == pub_ty_param
                })
            } else {
                unreachable!("Expected struct tuple.");
            };

            let (args, args_ref) = if node_pos.is_none() { //&& var.len() == 1 {
                (quote!( (self.0).into_node_info() ),
                 quote!( (self.0).node_info() ))
            } else if let Some(pos) = node_pos {
                let expr = syn::Ident::from(pos);
                (quote!( self.#expr ),
                 quote!( &self.#expr ))
            } else {
                unreachable!("Expected struct entry to be valid");
            };

            let name = &ast.ident;
            quote! {
                impl CNode for #name<NodeInfo> {
                    fn node_info(&self) -> &NodeInfo {
                        #args_ref
                    }
                    fn into_node_info(self) -> NodeInfo {
                        #args
                    }
                }
            }
        }
    }
}

#[proc_macro_derive(Pos)]
pub fn impl_pos(input: TokenStream) -> TokenStream {
    let s = input.to_string();
    let ast = syn::parse_macro_input(&s).unwrap();
    let name = &ast.ident;

    let gen = match ast.body {
        Body::Struct(..) => panic!("Expected an enum."),
        Body::Enum(ref variants) => {
            let mut arms = Vec::new();
            for var in variants {
                let name = &var.ident;
                if name != "CTokEof" {
                    arms.push(quote! {
                        #name (ref pl, ..) => pl.0.clone(),
                    });
                }
            }
            quote! {
                use std::rc::Rc;
                impl Pos for #name {
                    fn pos(&self) -> Rc<Position> {
                        match *self {
                            #(#arms)*
                            _ => panic!("CToken::pos: EOF"),
                        }
                    }
                }
            }
        }
    };
    gen.parse().unwrap()
}

#[proc_macro_derive(NodeFunctor)]
pub fn impl_node_functor(input: TokenStream) -> TokenStream {

    let s = input.to_string();
    let ast = syn::parse_macro_input(&s).unwrap();
    let name = &ast.ident;

    if ast.generics.ty_params.len() != 1 {
        panic!("Expected type with 1 type parameter.");
    }
    let ty_param = ast.generics.ty_params[0].ident.to_string();
    let pub_ty_param = format!("pub {}", ty_param);
    let angle_ty_param = format!("< {} >", ty_param);

    let collect_pats_exprs = |data: &syn::VariantData| -> (Vec<Tokens>, Vec<Tokens>) {
        let mut pats = Vec::new();
        let mut exprs = Vec::new();
        if let &syn::VariantData::Tuple(ref inner) = data {
            for (i, field) in inner.iter().enumerate() {
                let mut tokens = Tokens::new();
                field.to_tokens(&mut tokens);
                let arg = tokens.to_string();
                if arg == ty_param || arg == pub_ty_param {
                    pats.push(quote!( a ));
                    exprs.push(quote!( f(a) ));
                } else {
                    let id = syn::Ident::from(format!("_{}", i));
                    pats.push(quote!( #id ));
                    if arg.contains(&angle_ty_param) {
                        exprs.push(quote!( #id.fmap(f) ));
                    } else {
                        exprs.push(quote!( #id ));
                    }
                }
            }
        } else {
            unreachable!("Expected tuple struct/enum variant.");
        }
        (pats, exprs)
    };

    let gen = match ast.body {
        Body::Struct(ref vardata) => {
            let (pats, exprs) = collect_pats_exprs(vardata);
            quote! {
                impl<A, B> NodeFunctor<A, B> for #name<A> {
                    type Output = #name<B>;
                    fn fmap<F: Fn(A) -> B>(self, f: &F) -> Self::Output {
                        let #name(#(#pats),*) = self;
                        #name(#(#exprs),*)
                    }
                }
            }
        }
        Body::Enum(ref variants) => {
            let arms = variants.iter().map(|var| {
                let name = &var.ident;
                let (pats, exprs) = collect_pats_exprs(&var.data);
                quote! { #name(#(#pats),*) => #name(#(#exprs),*) }
            });
            quote! {
                impl<A, B> NodeFunctor<A, B> for #name<A> {
                    type Output = #name<B>;
                    fn fmap<F: Fn(A) -> B>(self, f: &F) -> Self::Output {
                        match self {
                            #(#arms),*
                        }
                    }
                }
            }
        }
    };
    // println!("{}", gen);
    gen.parse().unwrap()
}

#[proc_macro_derive(Traverse)]
pub fn impl_generic_node(input: TokenStream) -> TokenStream {

    let s = input.to_string();
    let ast = syn::parse_macro_input(&s).unwrap();
    let name = &ast.ident;

    if ast.generics.ty_params.len() != 1 {
        panic!("Expected type with 1 type parameter.");
    }
    let ty_param = ast.generics.ty_params[0].ident.to_string();
    let pub_ty_param = format!("pub {}", ty_param);
    let angle_ty_param = format!("< {} >", ty_param);

    let collect_pats_exprs = |data: &syn::VariantData| -> (Vec<Tokens>, Vec<Tokens>) {
        let mut refmutpats = Vec::new();
        let mut calls = Vec::new();
        if let &syn::VariantData::Tuple(ref inner) = data {
            for (i, field) in inner.iter().enumerate() {
                let mut tokens = Tokens::new();
                field.to_tokens(&mut tokens);
                let arg = tokens.to_string();
                if arg == ty_param || arg == pub_ty_param {
                    refmutpats.push(quote!( _ ));
                } else {
                    let id = syn::Ident::from(format!("_{}", i));
                    refmutpats.push(quote!( ref mut #id ));
                    if arg.contains(&angle_ty_param) {
                        calls.push(quote!( #id.traverse(f); ));
                    }
                }
            }
        } else {
            unreachable!("Expected tuple struct/enum variant.");
        }
        (refmutpats, calls)
    };

    let gen = match ast.body {
        Body::Struct(ref vardata) => {
            let (refmutpats, calls) = collect_pats_exprs(vardata);
            quote! {
                impl Traverse for #name<NodeInfo> {
                    fn traverse<F: Fn(&mut Any)>(&mut self, f: &F) {
                        f(self);
                        let #name(#(#refmutpats),*) = *self;
                        #(#calls)*
                    }
                }
            }
        }
        Body::Enum(ref variants) => {
            let refmutarms = variants.iter().map(|var| {
                let name = &var.ident;
                let (refmutpats, calls) = collect_pats_exprs(&var.data);
                quote! { #name(#(#refmutpats),*) => { #(#calls)* } }
            });
            quote! {
                impl Traverse for #name<NodeInfo> {
                    fn traverse<F: Fn(&mut Any)>(&mut self, f: &F) {
                        f(self);
                        match *self {
                            #(#refmutarms),*
                        }
                    }
                }
            }
        }
    };
    gen.parse().unwrap()
}

#[proc_macro_derive(Equiv)]
pub fn impl_equiv(input: TokenStream) -> TokenStream {

    let s = input.to_string();
    let ast = syn::parse_macro_input(&s).unwrap();
    let name = &ast.ident;

    if ast.generics.ty_params.len() != 1 {
        panic!("Expected type with 1 type parameter.");
    }
    let ty_param = ast.generics.ty_params[0].ident.to_string();
    let pub_ty_param = format!("pub {}", ty_param);

    let collect_pats_exprs = |data: &syn::VariantData| -> (Vec<Tokens>, Vec<Tokens>, Vec<Tokens>) {
        let mut pats1 = Vec::new();
        let mut pats2 = Vec::new();
        let mut exprs = Vec::new();
        if let &syn::VariantData::Tuple(ref inner) = data {
            for (i, field) in inner.iter().enumerate() {
                let mut tokens = Tokens::new();
                field.to_tokens(&mut tokens);
                let arg = tokens.to_string();
                if arg == ty_param || arg == pub_ty_param {
                    pats1.push(quote!( _ ));
                    pats2.push(quote!( _ ));
                    continue;
                }
                let id1 = syn::Ident::from(format!("_{}", 2*i));
                pats1.push(quote!( ref #id1 ));
                let id2 = syn::Ident::from(format!("_{}", 2*i + 1));
                pats2.push(quote!( ref #id2 ));
                exprs.push(quote!( #id1.equiv(#id2) ));
            }
        } else {
            unreachable!("Expected tuple struct/enum variant.");
        }
        (pats1, pats2, exprs)
    };

    let gen = match ast.body {
        Body::Struct(ref vardata) => {
            let (pats1, pats2, exprs) = collect_pats_exprs(vardata);
            let strng = name.to_string();
            quote! {
                impl<I> Equiv for #name<I> where I: ::std::fmt::Debug {
                    fn equiv(&self, other: &Self) -> bool {
                        let #name(#(#pats1),*) = *self;
                        let #name(#(#pats2),*) = *other;
                        if !( #(#exprs)&&* ) { println!(#strng);
                                               println!("{:?}\n{:?}", self, other);
                                               false } else { true }
                    }
                }
            }
        }
        Body::Enum(ref variants) => {
            let nstrng = name.to_string();
            let arms = variants.iter().map(|var| {
                let name = &var.ident;
                let strng = nstrng.clone() + "::" + &name.to_string();
                let (pats1, pats2, exprs) = collect_pats_exprs(&var.data);
                if exprs.is_empty() {
                    quote! { (&#name(#(#pats1),*), &#name(#(#pats2),*)) => true, }
                } else {
                    quote! { (&#name(#(#pats1),*), &#name(#(#pats2),*)) =>
                                if !( #(#exprs)&&* ) { println!(#strng); false } else { true },
                    }
                }
            });
            quote! {
                impl<I> Equiv for #name<I> where I: ::std::fmt::Debug {
                    fn equiv(&self, other: &Self) -> bool {
                        match (self, other) {
                            #(#arms)*
                            _ => { println!(#nstrng); false }
                        }
                    }
                }
            }
        }
    };
    // println!("{}", gen);
    gen.parse().unwrap()
}
