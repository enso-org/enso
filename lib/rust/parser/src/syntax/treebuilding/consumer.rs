use crate::syntax::Item;
use crate::syntax::Token;
use crate::syntax::Tree;



pub trait ItemConsumer<'s> {
    fn push_item(&mut self, tree: Item<'s>);
}

pub trait TreeConsumer<'s> {
    fn push_tree(&mut self, tree: Tree<'s>);
}

pub trait TokenConsumer<'s> {
    fn push_token(&mut self, token: Token<'s>);
}

pub trait Finish {
    type Result;

    fn finish(&mut self) -> Self::Result;
}
