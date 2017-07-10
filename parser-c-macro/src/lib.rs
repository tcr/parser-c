#![feature(proc_macro)]
#![allow(unused_imports)]

extern crate proc_macro;
extern crate syn;
#[macro_use] extern crate quote;

use syn::{ Ident, Body, Variant, VariantData };
use proc_macro::TokenStream;
use quote::{ToTokens, Tokens};

#[proc_macro]
pub fn refute(input: TokenStream) -> TokenStream {
    let input = input.to_string();

// 35834 |    refute!( pub fn happyReduction_23<t>(HappyStk(HappyAbsSyn12(happy_var_4), box HappyStk(HappyAbsSyn33(happy_var_3), box HappyStk(HappyAbsSyn11(happy_var_2), box HappyStk(HappyAbsSyn38(happy_var_1), box happyRest)))): HappyStk<HappyAbsSyn>, tk: t) -> P<HappyAbsSyn> {

    let mut ast = syn::parse_item(&input).unwrap();

    match ast.node {
        syn::ItemKind::Fn(ref mut decl, _, _, _, _, ref mut body) => {
            let mut pat_expand = vec![];
            for (i, arg) in decl.inputs.iter_mut().enumerate() {
                match *arg {
                    syn::FnArg::Captured(ref mut pat, _) => {
                        pat_expand.push(pat.clone());
                        *pat = syn::Pat::Ident(syn::BindingMode::ByValue(syn::Mutability::Immutable),
                                               syn::Ident::new(format!("_{}", i)), None);
                    }
                    _ => {
                        println!("TODO");
                    }
                }
            }

            let body_inner: syn::Block = (**body).clone();

            *body = Box::new(syn::Block {
                stmts: vec![
                    syn::Stmt::Expr(Box::new(syn::Expr {
                        node: syn::ExprKind::Match(
                            Box::new(syn::parse_expr(&format!(
                                "({})",
                                (0..pat_expand.len()).map(|x| format!("{{ _{} }}", x))
                                                     .collect::<Vec<_>>().join(", ")
                            )).unwrap()),
                            vec![
                                syn::Arm {
                                    attrs: vec![],
                                    pats: if pat_expand.len() == 1 {
                                        pat_expand
                                    } else {
                                        vec![syn::Pat::Tuple(pat_expand, None)]
                                    },
                                    guard: None,
                                    body: Box::new(syn::Expr {
                                        node: syn::ExprKind::Block(syn::BlockCheckMode::Default, body_inner),
                                        attrs: vec![],
                                    })
                                },
                                syn::Arm {
                                    attrs: vec![],
                                    pats: vec![syn::Pat::Wild],
                                    guard: None,
                                    body: Box::new(syn::parse_expr(r#"panic!("Irrefutable pattern!")"#).unwrap()),
                                }
                            ],
                        ),
                        attrs: vec![],
                    })),
                ],
            });
        }
        _ => {
            panic!("Unexpected item, expected fn");
        }
    }

    let mut args = Tokens::new();
    ast.to_tokens(&mut args);

    // if input.find("happyReduction_315").is_some() {
    //     println!("OH {:?}", args.to_string());
    // }

    args.parse().unwrap()
}

#[proc_macro_derive(CNodeable)]
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
                        if arg == "a" {
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
                    arg == "a" || arg == "pub a"
                })
            } else {
                unreachable!("Expected struct tuple.");
            };

            let (args, args_ref) = if node_pos.is_none() { //&& var.len() == 1 {
                (quote!( (self.0).into_node_info() ),
                 quote!( (self.0).node_info() ))
            } else if let Some(pos) = node_pos {
                // #pos in quote! produces "1usize" which is not a valid tuple index
                let expr = syn::parse_expr(&format!("{}", pos)).unwrap();
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
