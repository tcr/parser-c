// Original file: "Builtin.hs"
// File auto-generated using Corollary.

#[macro_use]
use corollary_support::*;
use data::ident::Ident;

// NOTE: These imports are advisory. You probably need to change them to support Rust.
// use Language::C::Data::Ident;
// use Ident;

pub fn builtinTypeNames() -> Vec<Ident> {
    vec![Ident::builtin("__builtin_va_list".into())]
}
