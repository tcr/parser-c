// Original file: "Name.hs"
// File auto-generated using Corollary.

#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd, Hash)]
pub struct Name(usize);

pub type NameSupply = Box<Iterator<Item=Name>>;

pub fn new_name_supply() -> NameSupply {
    Box::new((0..).map(Name))
}
