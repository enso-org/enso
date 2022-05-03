use crate::prelude::*;

use enso_macro_utils::field_names;
use enso_macro_utils::identifier_sequence;
use enso_macro_utils::index_sequence;
use enso_macro_utils::path_matching_ident;
use std::mem;
use syn::Attribute;
use syn::Data;
use syn::DataEnum;
use syn::DataStruct;
use syn::DeriveInput;
use syn::Fields;
use syn::Ident;
use syn::Lit;
use syn::Meta;
use syn::MetaNameValue;
use syn::NestedMeta;
use syn::Variant;
use syn::WhereClause;
use syn::WherePredicate;



pub struct Segment {
    header: TokenStream,
    body:   TokenStream,
}

impl Segment {
    pub fn new(header: TokenStream) -> Self {
        let body = quote!();
        Self { header, body }
    }
}

pub fn expr(tokens: proc_macro::TokenStream) -> TokenStream {
    use proc_macro::TokenTree::*;
    let mut output = quote! {};
    let mut prefix: Option<TokenStream> = None;
    let mut segments: Vec<Segment> = vec![];
    let mut current_segment: Option<Segment> = None;
    let app_to_output = |output: &mut TokenStream, tok| {
        if output.is_empty() {
            *output = tok;
        } else {
            *output = quote! {Ast::app(#output,#tok)};
        }
    };
    for token in tokens {
        // println!("!!! {:#?}", token);
        match token {
            // a b c ...
            Ident(ident) => {
                let ident = ident.to_string();
                app_to_output(&mut output, quote! {test::ident(#ident)});
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
                let body = group.stream().to_string();
                current_segment = Some(Segment::new(quote! {Token::ident(#body)})); // Token::symbol
            }
            // a [b c] d
            Group(group) if group.delimiter() == proc_macro::Delimiter::Bracket => {
                app_to_output(&mut output, expr(group.stream()));
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
                    quote!(Some(Ast::opr_section_boundary(#body)))
                } else {
                    quote!(None)
                };
                quote! { MultiSegmentAppSegment { header: #header, body: #body } }
            })
            .collect();
        let pfx = prefix
            .map(|t| quote! {Some(Box::new(Ast::opr_section_boundary(#t)))})
            .unwrap_or_else(|| quote! {None});
        let segments = quote! {NonEmptyVec::try_from(vec![#(#segments),*]).unwrap()};
        output = quote! {
            location::With::new_with_len(
                Bytes::from(0),
                AstData::MultiSegmentApp(MultiSegmentApp {prefix: #pfx, segments: #segments})
            )
        }
    }
    output
}

pub fn run(tokens: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let output = expr(tokens);
    let output = quote!(Ast::opr_section_boundary(#output));
    // println!("{:#}", output);
    output.into()
}
