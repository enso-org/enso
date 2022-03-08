use crate::prelude::*;

use enso_macro_utils::path_segment_generic_args;
use proc_macro2::TokenStream;
use quote::quote;
use syn::punctuated::Punctuated;
use syn::Expr;
use syn::GenericArgument;
use syn::PathSegment;
use syn::Token;



/// Generates `HasTokens` implementations for spaceless AST that panics when used.
pub fn spaceless_ast(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let target = syn::parse::<PathSegment>(input).unwrap();
    let ty_args = path_segment_generic_args(&target);
    let ret = quote! {
        impl<#(#ty_args),*> HasTokens for #target {
            fn feed_to(&self, consumer:&mut impl TokenConsumer) {
                panic!("HasTokens not supported for Spaceless AST!")
            }
        }
    };
    ret.into()
}

/// Inner logic for `derive_has_tokens`.
pub fn derive_for_enum(decl: &syn::DeriveInput, data: &syn::DataEnum) -> TokenStream {
    let ident = &decl.ident;
    let params = decl.generics.params.iter().collect_vec();
    let token_arms = data.variants.iter().map(|v| {
        let con_ident = &v.ident;
        quote!( #ident::#con_ident (elem) => elem.feed_to(consumer) )
    });
    let ret = quote! {
        impl<#(#params:HasTokens),*> HasTokens for #ident<#(#params),*> {
            fn feed_to(&self, consumer:&mut impl TokenConsumer) {
                match self { #(#token_arms),* }
            }
        }
    };
    ret
}

/// Structure representing input to macros like `has_tokens!`.
///
/// Basically it consists of a typename (with optional generic arguments) and
/// sequence of expressions that yield values we use to obtain sub-HasTokens.
pub struct TokenDescription {
    pub ty:      PathSegment,
    pub ty_args: Vec<GenericArgument>,
    pub exprs:   Vec<Expr>,
}

impl syn::parse::Parse for TokenDescription {
    /// Parser user-provided input to macro into out structure.
    ///
    /// First should go a type for which implementation is to be provided,
    /// then arbitrary sequence of expressions.
    /// Panics on invalid input, which is actually fair for a macro code.
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let ty: PathSegment = input.parse()?;
        input.parse::<Option<syn::token::Comma>>()?;
        let exprs = Punctuated::<Expr, Token![,]>::parse_terminated(input)?;
        let exprs = exprs.iter().cloned().collect::<Vec<_>>();
        let ty_args = path_segment_generic_args(&ty);
        let ty_args = ty_args.into_iter().cloned().collect(); // get rid of &
        Ok(TokenDescription { ty, ty_args, exprs })
    }
}

impl TokenDescription {
    /// Fills a trait implementation template with given methods.
    pub fn make_impl(&self, trait_name: &str, methods: &TokenStream) -> TokenStream {
        let trait_name = syn::parse_str::<syn::TypePath>(trait_name).unwrap();
        let ty = &self.ty;
        let ty_args = &self.ty_args;
        quote! {
            impl<#(#ty_args:#trait_name),*> #trait_name for #ty {
                #methods
            }
        }
    }

    /// Generates `HasTokens` instance using user-provided input.
    pub fn has_tokens(&self) -> TokenStream {
        let exprs = &self.exprs;
        self.make_impl("HasTokens", &quote! {
            fn feed_to(&self, consumer:&mut impl TokenConsumer) {
                #(#exprs.feed_to(consumer);)*
            }
        })
    }
}
