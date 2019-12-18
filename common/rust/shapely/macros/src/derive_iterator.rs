use prelude::*;

use quote::quote;
use proc_macro2::{TokenStream,Ident,Span};
use macro_utils::{fields_list,type_matches,type_depends_on};
use inflector::Inflector;

/// Returns token that refers to the field.
///
/// It is the field name for named field and field index for unnamed fields.
pub fn field_ident_token(field:&syn::Field, index:syn::Index) -> TokenStream {
    match &field.ident {
        Some(ident) => quote!(#ident),
        None        => quote!(#index),
    }
}

/// Returns identifiers of fields with type matching `target_param`.
///
/// If the struct is tuple-like, returns index pseudo-identifiers.
pub fn matching_fields
( data:&syn::DataStruct
, target_param:&syn::GenericParam
) -> Vec<TokenStream> {
    let fields           = fields_list(&data.fields);
    let indexed_fields   = fields.iter().enumerate();
    let ret              = indexed_fields.filter_map(|(i,f)| {
        let type_matched = type_matches(&f.ty, target_param);
        type_matched.as_some_from(|| field_ident_token(&f, i.into()))
    }).collect::<Vec<_>>();
    ret
}

/// Does enum variant depend on given type.
pub fn variant_depends_on
(var:&syn::Variant, target_param:&syn::GenericParam) -> bool {
    var.fields.iter().any(|field| type_depends_on(&field.ty, target_param))
}

/// Parts of derivation output that are specific to enum- or struct- target.
pub struct OutputParts<'ast> {
    pub iterator_tydefs  : TokenStream,
    pub iter_body        : TokenStream,
    pub iter_body_mut    : TokenStream,
    pub iterator_params  : Vec<&'ast syn::GenericParam>,
}

/// Common data used when generating derived Iterator impls.
///
/// Examples are given for `pub struct Foo<S, T> { foo: T }`
pub struct DerivingIterator<'ast> {
    pub data          : &'ast syn::Data,         // { foo: T }
    pub ident         : &'ast syn::Ident,        // Foo
    pub params        : Vec<&'ast syn::GenericParam>, // <S, T>
    pub t_iterator    : syn::Ident,              // FooIterator
    pub t_iterator_mut: syn::Ident,              // FooIteratorMut
    pub iterator      : syn::Ident,              // foo_iterator
    pub iterator_mut  : syn::Ident,              // foo_iterator_mut
    pub target_param  : &'ast syn::GenericParam  // T
}

impl DerivingIterator<'_> {
    pub fn new<'ast>
    (decl: &'ast syn::DeriveInput, target_param : &'ast syn::GenericParam)
     -> DerivingIterator<'ast> {
        let data           = &decl.data;
        let params         = decl.generics.params.iter().collect::<Vec<_>>();
        let ident          = &decl.ident;
        let t_iterator     = format!("{}Iterator"    , ident);
        let t_iterator_mut = format!("{}IteratorMut" , ident);
        let iterator       = t_iterator.to_snake_case();
        let iterator_mut   = t_iterator_mut.to_snake_case();
        let t_iterator     = Ident::new(&t_iterator     , Span::call_site());
        let t_iterator_mut = Ident::new(&t_iterator_mut , Span::call_site());
        let iterator       = Ident::new(&iterator       , Span::call_site());
        let iterator_mut   = Ident::new(&iterator_mut   , Span::call_site());
        DerivingIterator {
            data,
            ident,
            params,
            t_iterator,
            t_iterator_mut,
            iterator,
            iterator_mut,
            target_param
        }
    }

    /// Handles all enum-specific parts.
    pub fn prepare_parts_enum(&self, data:&syn::DataEnum) -> OutputParts {
        let t_iterator      = &self.t_iterator;
        let t_iterator_mut  = &self.t_iterator_mut;
        let ident           = &self.ident;
        let target_param    = &self.target_param;
        let iterator_params = vec!(self.target_param);
        let iterator_tydefs = quote!(
            // type FooIterator<'t, U> =
            //     Box<dyn Iterator<Item=&'t U> + 't>;
            // type FooIteratorMut<'t, U> =
            //     Box<dyn Iterator<Item=&'t mut U> + 't>;
            type #t_iterator<'t, #(#iterator_params),*>  =
                Box<dyn Iterator<Item=&'t #target_param> + 't>;
            type #t_iterator_mut<'t, #(#iterator_params),*> =
                Box<dyn Iterator<Item=&'t mut #target_param> + 't>;
        );
        // For types that use target type parameter, refer to their
        // `IntoIterator` implementation. Otherwise, use `EmptyIterator`.
        let arms = data.variants.iter().map(|var| {
            let con = &var.ident;
            let iter = if variant_depends_on(var, target_param) {
                quote!(elem.into_iter())
            }
            else {
                quote!(shapely::EmptyIterator::new())
            };
            quote!(#ident::#con(elem) => Box::new(#iter))
        });

        // match t {
        //     Foo::Con1(elem) => Box::new(elem.into_iter()),
        //     Foo::Con2(elem) => Box::new(shapely::EmptyIterator::new()),
        // }
        let iter_body       = quote!( match t {  #(#arms,)*  } );
        let iter_body_mut   = iter_body.clone();
        OutputParts{iterator_tydefs,iter_body,iter_body_mut,iterator_params}
    }

    /// Handles all struct-specific parts.
    pub fn prepare_parts_struct(&self, data:&syn::DataStruct) -> OutputParts {
        let t_iterator = &self.t_iterator;
        let t_iterator_mut = &self.t_iterator_mut;
        let target_param = &self.target_param;
        let iterator_params = self.params.clone();
        let iterator_tydefs = quote!(
            // type FooIterator<'t, T>    = impl Iterator<Item = &'t T>;
            // type FooIteratorMut<'t, T> = impl Iterator<Item = &'t mut T>;
            type #t_iterator<'t, #(#iterator_params),*> =
                impl Iterator<Item = &'t #target_param>;
            type #t_iterator_mut<'t, #(#iterator_params),*> =
                impl Iterator<Item = &'t mut #target_param>;
        );
        let matched_fields = matching_fields(data, target_param);

        // shapely::EmptyIterator::new()
        let empty_body = quote! { shapely::EmptyIterator::new() };

        // shapely::GeneratingIterator(move || {
        //     yield &t.foo;
        // })
        let body = quote! {
            shapely::GeneratingIterator
            (move || { #(yield &t.#matched_fields;)* })
        };

        // shapely::GeneratingIterator(move || {
        //     yield &mut t.foo;
        // })
        let body_mut = quote! {
            shapely::GeneratingIterator
            (move || { #(yield &mut t.#matched_fields;)* })
        };

        let (iter_body, iter_body_mut) = match matched_fields.is_empty() {
            true => (empty_body.clone(), empty_body),
            false => (body, body_mut)
        };
        OutputParts{iterator_tydefs,iter_body,iter_body_mut,iterator_params}
    }

    /// Handles common (between enum and struct) code and assembles it all
    /// into a final derivation output.
    pub fn assemble_output(&self, parts:OutputParts) -> TokenStream {
        let iterator_tydefs = &parts.iterator_tydefs;
        let iter_body       = &parts.iter_body;
        let iter_body_mut   = &parts.iter_body_mut;
        let iterator_params = &parts.iterator_params;
        let iterator        = &self.iterator;
        let iterator_mut    = &self.iterator_mut;
        let t_iterator      = &self.t_iterator;
        let t_iterator_mut  = &self.t_iterator_mut;
        let params          = &self.params;
        let ident           = &self.ident;
        let target_param    = &self.target_param;

        quote!{
            #iterator_tydefs

            // pub fn foo_iterator<'t, T>
            // (t: &'t Foo<T>) -> FooIterator<'t, T> {
            //    shapely::GeneratingIterator(move || {
            //        yield &t.foo;
            //    })
            // }
            pub fn #iterator<'t, #(#params),*>
            (t: &'t #ident<#(#params),*>)
             -> #t_iterator<'t, #(#iterator_params),*> {
                #iter_body
            }

            // pub fn foo_iterator_mut<'t, T>
            // (t: &'t mut Foo<T>) -> FooIteratorMut<'t, T> {
            //    shapely::GeneratingIterator(move || {
            //        yield &t.foo;
            //    })
            // }
            pub fn #iterator_mut<'t, #(#params),*>
            (t: &'t mut #ident<#(#params),*>)
            -> #t_iterator_mut<'t, #(#iterator_params),*> {
                #iter_body_mut
            }

            // impl<'t, T> IntoIterator for &'t Foo<T> {
            //     type Item     = &'t T;
            //     type IntoIter = FooIterator<'t, T>;
            //     fn into_iter(self) -> FooIterator<'t, T> {
            //         foo_iterator(self)
            //     }
            // }
            impl<'t, #(#params),*> IntoIterator for &'t #ident<#(#params),*> {
                type Item     = &'t #target_param;
                type IntoIter = #t_iterator<'t, #(#iterator_params),*>;
                fn into_iter(self) -> #t_iterator<'t, #(#iterator_params),*> {
                    #iterator(self)
                }
            }

            // impl<'t, T> IntoIterator for &'t mut Foo<T> {
            //     type Item     = &'t mut T;
            //     type IntoIter = FooIteratorMut<'t, T>;
            //     fn into_iter(self) -> FooIteratorMut<'t, T> {
            //         foo_iterator_mut(self)
            //     }
            // }
            impl<'t, #(#params),*>
            IntoIterator for &'t mut #ident<#(#params),*> {
                type Item     = &'t mut #target_param;
                type IntoIter = #t_iterator_mut<'t, #(#iterator_params),*>;
                fn into_iter
                (self) -> #t_iterator_mut<'t, #(#iterator_params),*> {
                    #iterator_mut(self)
                }
            }

            // impl Foo<T> {
            //     pub fn iter(&self) -> FooIterator<'_, T> {
            //         #foo_iterator(self)
            //     }
            //     pub fn iter_mut(&mut self) -> FooIteratorMut<'_, T> {
            //         #foo_iterator_mut (self)
            //     }
            // }
            impl<#(#params),*> #ident<#(#params),*> {
                pub fn iter(&self) -> #t_iterator<'_, #(#iterator_params),*> {
                    #iterator(self)
                }
                pub fn iter_mut
                (&mut self) -> #t_iterator_mut<'_, #(#iterator_params),*> {
                    #iterator_mut(self)
                }
            }
        }
    }

    pub fn output(&self) -> TokenStream {
        let parts = match self.data {
            syn::Data::Struct(data) => self.prepare_parts_struct(data),
            syn::Data::Enum  (data) => self.prepare_parts_enum  (data),
            _                       =>
                panic!("Only Structs and Enums can derive(Iterator)!"),
        };
        self.assemble_output(parts)
    }
}

pub fn derive
(decl: &syn::DeriveInput, target_param : &syn::GenericParam) -> TokenStream {
    let derive = DerivingIterator::new(decl,target_param);
    derive.output()
}

// Note [Expansion Example]
// ~~~~~~~~~~~~~~~~~~~~~~~~
// In order to make the definition easier to read, an example expansion of the
// following definition was provided for each quotation:
//
// #[derive(Iterator)]
// pub struct Foo<S, T> { foo: T }
//
// For examples that are enum-specific rather than struct-specific, the
// following definition is assumed:
//
// #[derive(Iterator)]
// pub enum Foo<T> {
//     Con1(Bar<T>),
//     Con2(Baz),
// }

