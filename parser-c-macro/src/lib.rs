#![feature(proc_macro)]

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
                    arg == ty_param || arg == format!("pub {}", ty_param)
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

#[proc_macro_derive(Pos)]
pub fn impl_pos(input: TokenStream) -> TokenStream {
    let s = input.to_string();
    let ast = syn::parse_macro_input(&s).unwrap();
    let name = &ast.ident;

    let gen = match ast.body {
        Body::Struct(..) => panic!("Expected an enum."),
        Body::Enum(ref variants) => {
            let mut arms = Vec::new();
            let mut arms_ref = Vec::new();
            for var in variants {
                let name = &var.ident;
                if name != "CTokEof" {
                    arms.push(quote! { #name ((pos, _), ..) => pos, });
                    arms_ref.push(quote! { #name ((ref pos, _), ..) => pos, });
                }
            }
            quote! {
                impl Pos for #name {
                    fn pos(&self) -> &Position {
                        match *self {
                            #(#arms_ref)*
                            _ => panic!("tokenPos: EOF"),
                        }
                    }
                    fn into_pos(self) -> Position {
                        match self {
                            #(#arms)*
                            _ => panic!("tokenPos: EOF"),
                        }
                    }
                }
            }
        }
    };
    gen.parse().unwrap()
}
