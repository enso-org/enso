use quote::quote;
use syn::Attribute;
use syn::DeriveInput;
use syn::Meta;

/// Derives [crate::syntax::Finish]; the implementation will call `self.flush()`, and then return
/// the result of `self.inner.finish()`.
#[proc_macro_derive(Finish)]
pub fn derive_finish(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    derive_finish_impl(input.into()).into()
}

/// Derives [crate::syntax::expression::consumer::GroupHierarchyConsumer] by calling the
/// corresponding functions on `self.inner`.
#[proc_macro_derive(GroupHierarchyConsumer)]
pub fn derive_group_hierarchy_consumer(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    derive_group_hierarchy_consumer_impl(input.into()).into()
}

/// Derives [crate::syntax::expression::consumer::ScopeHierarchyConsumer] by calling the
/// corresponding functions on `self.inner`.
///
/// The attribute `#[scope_hierarchy_consumer(...)]` must be provided with one of the following
/// options:
/// - `Forward`: Each derived function will only call the corresponding function on `self.inner`.
/// - `FlushAndForward`: Each derived function will call `self.flush`, and then call the
///   corresponding function on `self.inner`.
#[proc_macro_derive(ScopeHierarchyConsumer, attributes(scope_hierarchy_consumer))]
pub fn derive_scope_hierarchy_consumer(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    derive_scope_hierarchy_consumer_impl(input.into()).into()
}

/// Derives [crate::syntax::expression::types::OperatorConsumer] by calling the corresponding
/// functions on `self.inner`.
///
/// The attribute `#[operator_consumer(...)]` must be provided with one of the following options:
/// - `Forward`: Each derived function will only call the corresponding function on `self.inner`.
/// - `FlushAndForward`: Each derived function will call `self.flush`, and then call the
///   corresponding function on `self.inner`.
#[proc_macro_derive(OperatorConsumer, attributes(operator_consumer))]
pub fn derive_operator_consumer(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    derive_operator_consumer_impl(input.into()).into()
}

/// Derives [crate::syntax::expression::types::OperandConsumer] by calling the corresponding
/// functions on `self.inner`.
///
/// The attribute `#[operand_consumer(...)]` must be provided with one of the following options:
/// - `Forward`: Each derived function will only call the corresponding function on `self.inner`.
/// - `FlushAndForward`: Each derived function will call `self.flush`, and then call the
///   corresponding function on `self.inner`.
#[proc_macro_derive(OperandConsumer, attributes(operand_consumer))]
pub fn derive_operand_consumer(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    derive_operand_consumer_impl(input.into()).into()
}

/// Derives [crate::syntax::expression::whitespace::SpacingLookaheadTokenConsumer] by calling the
/// corresponding functions on `self.inner`.
///
/// The attribute `#[token_consumer(...)]` must be provided with one of the following options:
/// - `Forward`: Each derived function will only call the corresponding function on `self.inner`.
/// - `FlushAndForward`: Each derived function will call `self.flush`, and then call the
///   corresponding function on `self.inner`.
#[proc_macro_derive(SpacingLookaheadTokenConsumer, attributes(token_consumer))]
pub fn derive_spacing_lookahead_token_consumer(
    input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    derive_spacing_lookahead_token_consumer_impl(input.into()).into()
}

/// Derives [crate::syntax::expression::whitespace::SpacingLookaheadTreeConsumer] by calling the
/// corresponding functions on `self.inner`.
///
/// The attribute `#[tree_consumer(...)]` must be provided with one of the following options:
/// - `Forward`: Each derived function will only call the corresponding function on `self.inner`.
/// - `FlushAndForward`: Each derived function will call `self.flush`, and then call the
///   corresponding function on `self.inner`.
#[proc_macro_derive(SpacingLookaheadTreeConsumer, attributes(tree_consumer))]
pub fn derive_spacing_lookahead_tree_consumer(
    input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    derive_spacing_lookahead_tree_consumer_impl(input.into()).into()
}

/// Derives [crate::syntax::expression::reducer::ApplyToOperand] by calling a `self.apply` method
/// that operates on a `Tree`. The `apply_to_operand` implementation will use `Tree::from` to
/// convert the operand input into a tree, and `Tree::into` to convert the tree output to an
/// operand.
#[proc_macro_derive(ApplyToOperand)]
pub fn derive_apply_to_operand(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    derive_apply_to_operand_impl(input.into()).into()
}

fn derive_finish_impl(input: proc_macro2::TokenStream) -> proc_macro2::TokenStream {
    let input = syn::parse2::<DeriveInput>(input).unwrap();
    let ident = input.ident;
    let generics = input.generics;
    let (flush_constraint, do_flush) = flush_syntax();
    quote! {
        impl #generics crate::syntax::Finish for #ident #generics
        where #flush_constraint Inner: crate::syntax::Finish {
            type Result = Inner::Result;

            fn finish(&mut self) -> Self::Result {
                #do_flush
                use crate::syntax::Finish;
                self.inner.finish()
            }
        }
    }
}

fn derive_group_hierarchy_consumer_impl(
    input: proc_macro2::TokenStream,
) -> proc_macro2::TokenStream {
    let input = syn::parse2::<DeriveInput>(input).unwrap();
    let ident = input.ident;
    let generics = input.generics;
    let (flush_constraint, do_flush) = flush_syntax();
    quote! {
        impl #generics crate::syntax::expression::consumer::GroupHierarchyConsumer<'s>
        for #ident #generics
        where
            #flush_constraint
            Inner: crate::syntax::expression::consumer::GroupHierarchyConsumer<'s>
        {
            fn start_group(&mut self, open: crate::token::OpenSymbol<'s>) {
                #do_flush
                use crate::syntax::expression::consumer::GroupHierarchyConsumer;
                self.inner.start_group(open);
            }

            fn end_group(&mut self, close: Option<crate::token::CloseSymbol<'s>>) {
                #do_flush
                use crate::syntax::expression::consumer::GroupHierarchyConsumer;
                self.inner.end_group(close);
            }
        }
    }
}

fn flush_syntax() -> (proc_macro2::TokenStream, proc_macro2::TokenStream) {
    (
        quote! { Self: crate::syntax::Flush, },
        quote! {
            use crate::syntax::Flush;
            self.flush();
        },
    )
}

fn require_flush_specification<'a>(
    ident: &str,
    attrs: impl IntoIterator<Item = &'a Attribute>,
) -> (Option<proc_macro2::TokenStream>, Option<proc_macro2::TokenStream>) {
    let mut flush = None;
    for attr in attrs {
        match &attr.meta {
            Meta::List(list) if list.path.is_ident(ident) => {
                match list.tokens.to_string().as_str() {
                    "FlushAndForward" => flush = Some(true),
                    "Forward" => flush = Some(false),
                    _ => panic!(),
                }
            }
            _ => (),
        }
    }
    match flush.unwrap() {
        true => {
            let (constraint, action) = flush_syntax();
            (Some(constraint), Some(action))
        }
        false => (None, None),
    }
}

fn derive_scope_hierarchy_consumer_impl(
    input: proc_macro2::TokenStream,
) -> proc_macro2::TokenStream {
    let input = syn::parse2::<DeriveInput>(input).unwrap();
    let ident = input.ident;
    let generics = input.generics;
    let (flush_constraint, do_flush) =
        require_flush_specification("scope_hierarchy_consumer", &input.attrs);
    quote! {
        impl #generics crate::syntax::expression::consumer::ScopeHierarchyConsumer
        for #ident #generics
        where
            #flush_constraint
            Inner: crate::syntax::expression::consumer::ScopeHierarchyConsumer
        {
            type Result = Inner::Result;

            fn start_scope(&mut self) {
                #do_flush
                use crate::syntax::expression::consumer::ScopeHierarchyConsumer;
                self.inner.start_scope()
            }

            fn end_scope(&mut self) -> Inner::Result {
                #do_flush
                use crate::syntax::expression::consumer::ScopeHierarchyConsumer;
                self.inner.end_scope()
            }
        }
    }
}

fn derive_operator_consumer_impl(input: proc_macro2::TokenStream) -> proc_macro2::TokenStream {
    let input = syn::parse2::<DeriveInput>(input).unwrap();
    let ident = input.ident;
    let generics = input.generics;
    let (flush_constraint, do_flush) =
        require_flush_specification("operator_consumer", &input.attrs);
    quote! {
        impl #generics crate::syntax::expression::types::OperatorConsumer<'s> for #ident #generics
        where
            #flush_constraint
            Inner: crate::syntax::expression::types::OperatorConsumer<'s>
        {
            fn push_operator(&mut self, operator: crate::syntax::expression::types::Operator<'s>) {
                #do_flush
                use crate::syntax::expression::types::OperatorConsumer;
                self.inner.push_operator(operator);
            }
        }
    }
}

fn derive_operand_consumer_impl(input: proc_macro2::TokenStream) -> proc_macro2::TokenStream {
    let input = syn::parse2::<DeriveInput>(input).unwrap();
    let ident = input.ident;
    let generics = input.generics;
    let (flush_constraint, do_flush) =
        require_flush_specification("operand_consumer", &input.attrs);
    quote! {
        impl #generics crate::syntax::expression::types::OperandConsumer<'s> for #ident #generics
        where
            #flush_constraint
            Inner: crate::syntax::expression::types::OperandConsumer<'s>
        {
            fn push_operand(&mut self, operand: crate::syntax::expression::types::Operand<'s>) {
                #do_flush
                use crate::syntax::expression::types::OperandConsumer;
                self.inner.push_operand(operand);
            }
        }
    }
}

fn derive_spacing_lookahead_token_consumer_impl(
    input: proc_macro2::TokenStream,
) -> proc_macro2::TokenStream {
    let input = syn::parse2::<DeriveInput>(input).unwrap();
    let ident = input.ident;
    let generics = input.generics;
    let (flush_constraint, do_flush) = require_flush_specification("token_consumer", &input.attrs);
    quote! {
        impl #generics crate::syntax::expression::whitespace::SpacingLookaheadTokenConsumer<'s>
        for #ident #generics
        where
            #flush_constraint
            Inner: crate::syntax::expression::whitespace::SpacingLookaheadTokenConsumer<'s>
        {
            fn push_token(
                &mut self,
                token: crate::syntax::Token<'s>,
                following_spacing: Option<crate::syntax::expression::whitespace::Spacing>
            ) {
                #do_flush
                use crate::syntax::expression::whitespace::SpacingLookaheadTokenConsumer;
                self.inner.push_token(token, following_spacing);
            }
        }
    }
}

fn derive_spacing_lookahead_tree_consumer_impl(
    input: proc_macro2::TokenStream,
) -> proc_macro2::TokenStream {
    let input = syn::parse2::<DeriveInput>(input).unwrap();
    let ident = input.ident;
    let generics = input.generics;
    let (flush_constraint, do_flush) = require_flush_specification("tree_consumer", &input.attrs);
    quote! {
        impl #generics crate::syntax::expression::whitespace::SpacingLookaheadTreeConsumer<'s>
        for #ident #generics
        where
            #flush_constraint
            Inner: crate::syntax::expression::whitespace::SpacingLookaheadTreeConsumer<'s>
        {
            fn push_tree(
                &mut self,
                token: crate::syntax::Tree<'s>,
                following_spacing: Option<crate::syntax::expression::whitespace::Spacing>
            ) {
                #do_flush
                use crate::syntax::expression::whitespace::SpacingLookaheadTreeConsumer;
                self.inner.push_tree(token, following_spacing);
            }
        }
    }
}

fn derive_apply_to_operand_impl(input: proc_macro2::TokenStream) -> proc_macro2::TokenStream {
    let input = syn::parse2::<DeriveInput>(input).unwrap();
    let ident = input.ident;
    let generics = input.generics;
    quote! {
        impl #generics crate::syntax::expression::reducer::ApplyToOperand<'s> for #ident #generics {
            fn apply_to_operand(
                self,
                operand: Option<crate::syntax::expression::types::Operand<'s>>
            ) -> crate::syntax::expression::types::Operand<'s> {
                self.apply(operand.map(Tree::from)).into()
            }
        }
    }
}
