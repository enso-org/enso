use enso_prelude::*;

use crate::source;
use crate::syntax::token;
use crate::syntax::BlockHierarchyConsumer;
use crate::syntax::Finish;
use crate::syntax::GroupHierarchyConsumer;
use crate::syntax::NewlineConsumer;
use crate::syntax::Token;
use crate::syntax::TokenConsumer;


// =========================
// === Collecting Tokens ===
// =========================

impl<'s> TokenConsumer<'s> for Vec<Token<'s>> {
    fn push_token(&mut self, token: Token<'s>) {
        self.push(token);
    }
}

impl<'s> NewlineConsumer<'s> for Vec<Token<'s>> {
    fn push_newline(&mut self, token: token::Newline<'s>) {
        self.push(token.into());
    }
}

impl<'s> BlockHierarchyConsumer for Vec<Token<'s>> {
    fn start_block(&mut self) {
        self.push(Token(source::Offset::default(), default(), token::Variant::block_start()));
    }

    fn end_block(&mut self) {
        self.push(Token(source::Offset::default(), default(), token::Variant::block_end()));
    }
}

impl<'s> GroupHierarchyConsumer<'s> for Vec<Token<'s>> {
    fn start_group(&mut self, open: token::OpenSymbol<'s>) {
        self.push(open.into())
    }

    fn end_group(&mut self, close: token::CloseSymbol<'s>) {
        self.push(close.into())
    }
}

impl<'s> Finish for Vec<Token<'s>> {
    type Result = Vec<Token<'s>>;

    fn finish(&mut self) -> Self::Result {
        mem::take(self)
    }
}
