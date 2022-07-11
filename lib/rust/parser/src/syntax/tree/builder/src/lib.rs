//! Definition of a macro allowing building mock AST structures, mostly useful for testing.

// === Features ===
#![feature(proc_macro_span)]
// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
// === Non-Standard Linter Configuration ===
#![allow(clippy::option_map_unit_fn)]
#![allow(clippy::precedence)]
#![allow(dead_code)]
#![deny(unconditional_recursion)]
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unused_import_braces)]
#![warn(unused_qualifications)]

use proc_macro2::TokenStream;
use quote::quote;
use std::mem;



/// A macro allowing building mock AST structures, mostly useful for testing.
///
/// Currently supported syntax:
///
/// - `a b c` Application of arguments. Arguments are applied in-order, from left to right. Here,
///   this expression would be the same as `[[a b] c]`.
///
/// - `a [b c] d` Grouping syntax that does not produce AST group expression. Here, `b c` is just
///   the first argument passed to `a`.
///
/// - `{if} a {then} b {else} c` Multi-segment application. All segments should be enclosed in curly
///   braces. You can also place segments in quotes, like `{"("} a {")"}`.
#[proc_macro]
pub fn ast_builder(tokens: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let output = expr(tokens, None);
    let output = quote!(crate::syntax::Tree::block(vec![#output]));
    output.into()
}


struct Segment {
    header: TokenStream,
    body:   TokenStream,
}

impl Segment {
    fn new(header: TokenStream) -> Self {
        let body = quote!();
        Self { header, body }
    }
}

fn expr(tokens: proc_macro::TokenStream, parent_spacing: Option<usize>) -> TokenStream {
    use proc_macro::TokenTree::*;
    let mut output = quote! {};
    let mut prefix: Option<TokenStream> = None;
    let mut segments: Vec<Segment> = vec![];
    let mut current_segment: Option<Segment> = None;
    let mut last_column: Option<usize> = None;
    let app_to_output = |output: &mut TokenStream, tok| {
        if output.is_empty() {
            *output = tok;
        } else {
            *output = quote! {syntax::Tree::app(#output,#tok)};
        }
    };
    let mut inherited_spacing = parent_spacing.unwrap_or(0);
    for token in tokens {
        let spacing = last_column.map(|t| token.span().start().column - t).unwrap_or(0);
        let spacing = spacing + inherited_spacing;
        inherited_spacing = 0;
        last_column = Some(token.span().end().column);
        match &token {
            // a b c ...
            Ident(ident) => {
                let ident = ident.to_string();
                let spacing = " ".repeat(spacing);
                app_to_output(
                    &mut output,
                    quote! {crate::syntax::Tree::ident(crate::syntax::Token(#spacing, #ident, syntax::token::Variant::new_ident_unchecked(#ident)))},
                );
            }
            // {if} a {then} b {else} c
            // {"("} a {")"}
            Group(group) if group.delimiter() == proc_macro::Delimiter::Brace => {
                if let Some(mut current_segment) = mem::take(&mut current_segment) {
                    current_segment.body = mem::take(&mut output);
                    segments.push(current_segment);
                } else if !output.is_empty() {
                    prefix = Some(mem::take(&mut output));
                }
                let ident = group.stream().to_string();
                let spacing = " ".repeat(spacing);
                current_segment = Some(Segment::new(
                    quote! { Token(#spacing, #ident, syntax::token::Variant::new_ident_unchecked(#ident).into())},
                )); // Token::symbol
            }
            // a [b c] d
            Group(group) if group.delimiter() == proc_macro::Delimiter::Bracket => {
                app_to_output(&mut output, expr(group.stream(), Some(spacing)));
            }
            _ => panic!("Unsupported token {:?}", token),
        }
    }
    if let Some(mut current_segment) = current_segment {
        current_segment.body = mem::take(&mut output);
        segments.push(current_segment);
        let segments: Vec<TokenStream> = segments
            .into_iter()
            .map(|t| {
                let header = t.header;
                let body = t.body;
                let body = if !body.is_empty() {
                    quote!(Some(syntax::Tree::opr_section_boundary(#body)))
                } else {
                    quote!(None)
                };
                quote! { syntax::tree::MultiSegmentAppSegment { header: #header, body: #body } }
            })
            .collect();
        let pfx = prefix
            .map(|t| quote! {Some(Box::new(syntax::Tree::opr_section_boundary(#t)))})
            .unwrap_or_else(|| quote! {None});
        let segments = quote! {NonEmptyVec::try_from(vec![#(#segments),*]).unwrap()};
        output = quote! {
            syntax::Tree::multi_segment_app (#pfx, #segments)
        }
    }
    output
}
