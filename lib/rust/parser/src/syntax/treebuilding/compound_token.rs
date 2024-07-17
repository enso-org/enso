use enso_prelude::*;

use crate::syntax;
use crate::syntax::token;
use crate::syntax::treebuilding::consumer::Finish;
use crate::syntax::treebuilding::consumer::TokenConsumer;
use crate::syntax::treebuilding::consumer::TreeConsumer;
use crate::syntax::Token;



// ================================
// === Compound token assembler ===
// ================================

/// Recognizes lexical tokens that are indivisible, and assembles them into trees.
#[derive(Default, Debug)]
pub struct AssembleCompoundTokens<'s, T> {
    compounding: Option<CompoundToken<'s>>,
    inner:       T,
}

#[derive(Debug)]
enum CompoundToken<'s> {
    TextLiteral(TextLiteralBuilder<'s>),
}

impl<'s, T: TreeConsumer<'s> + TokenConsumer<'s>> TokenConsumer<'s>
    for AssembleCompoundTokens<'s, T>
{
    fn push_token(&mut self, token: Token<'s>) {
        match (&mut self.compounding, token.variant) {
            (this @ None, token::Variant::TextStart(variant)) => {
                let token = token.with_variant(variant);
                *this = Some(CompoundToken::TextLiteral(TextLiteralBuilder {
                    open:     token,
                    newline:  default(),
                    elements: default(),
                }));
            }
            (
                Some(CompoundToken::TextLiteral(TextLiteralBuilder {
                    newline: newline @ None,
                    ..
                })),
                token::Variant::TextInitialNewline(_),
            ) => {
                let token = token::newline(token.left_offset, token.code);
                *newline = Some(token);
            }
            (
                Some(CompoundToken::TextLiteral(TextLiteralBuilder { elements, .. })),
                token::Variant::TextSection(variant),
            ) => {
                let token = token.with_variant(variant);
                let element = syntax::tree::TextElement::Section { text: token };
                elements.push(element);
            }
            (
                Some(CompoundToken::TextLiteral(TextLiteralBuilder { elements, .. })),
                token::Variant::TextEscape(variant),
            ) => {
                let token = token.with_variant(variant);
                let element = syntax::tree::TextElement::Escape { token };
                elements.push(element);
            }
            (
                Some(CompoundToken::TextLiteral(TextLiteralBuilder { elements, .. })),
                token::Variant::TextNewline(_),
            ) => {
                let token = token::newline(token.left_offset, token.code);
                let element = syntax::tree::TextElement::Newline { newline: token };
                elements.push(element);
            }
            (this @ Some(CompoundToken::TextLiteral(_)), token::Variant::TextEnd(variant)) => {
                let builder = match mem::take(this) {
                    Some(CompoundToken::TextLiteral(builder)) => builder,
                    _ => unreachable!(),
                };
                let close = token.with_variant(variant);
                self.inner.push_tree(builder.finish(Some(close)));
            }
            (_, token::Variant::TextStart(_)) => unreachable!(),
            (_, token::Variant::TextInitialNewline(_)) => unreachable!(),
            (_, token::Variant::TextSection(_)) => unreachable!(),
            (_, token::Variant::TextEscape(_)) => unreachable!(),
            (_, token::Variant::TextNewline(_)) => unreachable!(),
            (_, token::Variant::TextEnd(_)) => unreachable!(),
            _ => self.inner.push_token(token),
        }
    }
}

impl<'s, T: TreeConsumer<'s>> TreeConsumer<'s> for AssembleCompoundTokens<'s, T> {
    fn push_tree(&mut self, mut tree: syntax::Tree<'s>) {
        match (&mut self.compounding, &mut tree.variant) {
            (
                Some(CompoundToken::TextLiteral(TextLiteralBuilder { elements, .. })),
                box syntax::tree::Variant::TextLiteral(syntax::tree::TextLiteral {
                    open: None,
                    newline: None,
                    elements: rhs_elements,
                    close: None,
                }),
            ) => {
                match rhs_elements.first_mut() {
                    Some(syntax::tree::TextElement::Splice { open, .. }) =>
                        open.left_offset += tree.span.left_offset,
                    _ => unreachable!(),
                }
                elements.append(rhs_elements);
            }
            _ => {
                self.flush();
                self.inner.push_tree(tree);
            }
        }
    }
}

impl<'s, T: TreeConsumer<'s>> AssembleCompoundTokens<'s, T> {
    fn flush(&mut self) {
        if let Some(CompoundToken::TextLiteral(builder)) = mem::take(&mut self.compounding) {
            self.inner.push_tree(builder.finish(None))
        }
    }
}

impl<'s, T: TreeConsumer<'s> + Finish> Finish for AssembleCompoundTokens<'s, T> {
    type Result = T::Result;

    fn finish(&mut self) -> Self::Result {
        self.flush();
        self.inner.finish()
    }
}


// === Text literal builder ===

#[derive(Debug)]
struct TextLiteralBuilder<'s> {
    open:     token::TextStart<'s>,
    newline:  Option<token::Newline<'s>>,
    elements: Vec<syntax::tree::TextElement<'s>>,
}

impl<'s> TextLiteralBuilder<'s> {
    fn finish(self, close: Option<token::TextEnd<'s>>) -> syntax::Tree<'s> {
        let Self { open, newline, elements } = self;
        if open.code.starts_with('#') {
            assert_eq!(newline, None);
            let doc = syntax::tree::DocComment { open, elements, newlines: default() };
            syntax::Tree::documented(doc, default())
        } else {
            let close =
                close.and_then(|close| if close.code.is_empty() { None } else { Some(close) });
            syntax::Tree::text_literal(Some(open), newline, elements, close)
        }
    }
}
