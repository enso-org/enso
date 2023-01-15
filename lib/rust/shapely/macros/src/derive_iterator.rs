use crate::prelude::*;

use boolinator::Boolinator;
use enso_macro_utils::field_ident_token;
use enso_macro_utils::fields_list;
use enso_macro_utils::ty_path_type_args;
use enso_macro_utils::type_depends_on;
use enso_macro_utils::type_matches;
use enso_macro_utils::variant_depends_on;
use inflector::Inflector;
use itertools::Itertools;



// =============
// === IsMut ===
// =============

/// Describes whether a mutable or immutable iterator is being derived.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum IsMut {
    Mutable,
    Immutable,
}

impl IsMut {
    fn is_mut(self) -> bool {
        self == IsMut::Mutable
    }

    /// Returns `mut` token for mutable iterator derivation.
    fn to_token(self) -> Option<syn::Token![mut]> {
        self.is_mut().as_some(<syn::Token![mut]>::default())
    }

    /// Name of method for generating iterator.
    fn iter_method(self) -> TokenStream {
        if self.is_mut() {
            quote!(iter_mut)
        } else {
            quote!(iter)
        }
    }
}


// ======================
// === DependentValue ===
// ======================

/// A value dependent on out target parameter.
///
/// Helper methods can be used to generate code yielding values from this.
pub struct DependentValue<'t> {
    /// Type of the value (ref-stripped).
    pub ty:           &'t syn::Type,
    /// Tokens yielding the value.
    pub value:        TokenStream,
    /// Parameter type we want to iterate over.
    pub target_param: &'t syn::GenericParam,
    /// Are the value yielded as reference.
    pub through_ref:  bool,
}

impl<'t> DependentValue<'t> {
    /// Returns Some when type is dependent and None otherwise.
    pub fn try_new(
        ty: &'t syn::Type,
        value: TokenStream,
        target_param: &'t syn::GenericParam,
    ) -> Option<DependentValue<'t>> {
        if type_depends_on(ty, target_param) {
            Some(DependentValue { ty, value, target_param, through_ref: false })
        } else {
            None
        }
    }

    /// Collects dependent sub-values from the tuple value.
    pub fn collect_tuple(
        tuple: &'t syn::TypeTuple,
        target_param: &'t syn::GenericParam,
    ) -> Vec<DependentValue<'t>> {
        tuple
            .elems
            .iter()
            .enumerate()
            .filter_map(|(ix, ty)| {
                let ix = syn::Index::from(ix);
                let ident = quote!(t.#ix);
                DependentValue::try_new(ty, ident, target_param)
            })
            .collect()
    }

    /// Generates code yielding all values of target type accessible from this
    /// value.
    pub fn yield_value(&self, is_mut: IsMut) -> TokenStream {
        match self.ty {
            syn::Type::Tuple(tuple) => self.yield_tuple_value(tuple, is_mut),
            syn::Type::Path(path) =>
                if type_matches(self.ty, self.target_param) {
                    self.yield_direct_value(is_mut)
                } else {
                    self.yield_dependent_ty_path_value(path, is_mut)
                },
            _ => panic!(
                "Don't know how to yield value of type {} from type {}",
                repr(&self.target_param),
                repr(&self.ty)
            ),
        }
    }

    /// Code yielding value that directly matches the target parameter type.
    pub fn yield_direct_value(&self, is_mut: IsMut) -> TokenStream {
        let value = &self.value;
        let opt_mut = is_mut.to_token();
        let opt_ref = (!self.through_ref).as_some(quote!( & #opt_mut ));

        // yield &mut value;
        quote!(  yield #opt_ref #value; )
    }

    /// Code yielding values from tuple dependent on the target parameter type.
    pub fn yield_tuple_value(&self, ty: &syn::TypeTuple, is_mut: IsMut) -> TokenStream {
        let value = &self.value;
        let mut_kwd = is_mut.to_token();
        let subfields = DependentValue::collect_tuple(ty, self.target_param);
        let yield_sub = subfields.iter().map(|f| f.yield_value(is_mut)).collect_vec();

        // yield &mut t.0;
        // yield &mut t.2;
        quote!( {
            let t = & #mut_kwd #value;
            #(#yield_sub)*
        })
    }

    /// Obtain the type of iterator-yielded value.
    ///
    /// Panics when given a type which is not supported for derivation, like
    /// having dependent type on the non-last position.
    pub fn type_path_elem_type(&self, ty_path: &'t syn::TypePath) -> &syn::Type {
        let mut type_args = ty_path_type_args(ty_path);
        let last_arg = match type_args.pop() {
            Some(arg) => arg,
            None => panic!("Type {} has no segments!", repr(&ty_path)),
        };

        // Last and only last type argument is dependent.
        for non_last_segment in type_args {
            assert!(
                !type_depends_on(non_last_segment, self.target_param),
                "Type {} has non-last argument {} that depends on {}",
                repr(ty_path),
                repr(non_last_segment),
                repr(self.target_param)
            );
        }
        assert!(type_depends_on(last_arg, self.target_param));
        last_arg
    }

    /// Code yielding values from data dependent on the target parameter type.
    pub fn yield_dependent_ty_path_value(
        &self,
        ty_path: &'t syn::TypePath,
        is_mut: IsMut,
    ) -> TokenStream {
        let opt_mut = is_mut.to_token();
        let elem_ty = self.type_path_elem_type(ty_path);
        let elem = quote!(t);

        let elem_info = DependentValue {
            value:        elem.clone(),
            target_param: self.target_param,
            ty:           elem_ty,
            through_ref:  true,
        };
        let yield_elem = elem_info.yield_value(is_mut);
        let value = &self.value;
        let iter_method = if is_mut.is_mut() { quote!(iter_mut) } else { quote!(iter) };

        quote! {
            for #opt_mut #elem in #value.#iter_method() {
                #yield_elem
            }
        }
    }

    /// Describe relevant fields of the struct definition.
    pub fn collect_struct(
        data: &'t syn::DataStruct,
        target_param: &'t syn::GenericParam,
    ) -> Vec<DependentValue<'t>> {
        let fields = fields_list(&data.fields);
        let dep_field = fields.iter().enumerate().filter_map(|(i, f)| {
            let ident = field_ident_token(f, i.into());
            let value = quote!(t.#ident);
            DependentValue::try_new(&f.ty, value, target_param)
        });
        dep_field.collect()
    }
}

/// Parts of derivation output that are specific to enum- or struct- target.
pub struct OutputParts<'ast> {
    pub iterator_tydefs: TokenStream,
    pub iter_body:       TokenStream,
    pub iterator_params: Vec<&'ast syn::GenericParam>,
}

/// Common data used when generating derived Iterator impls.
///
/// Examples are given for `pub struct Foo<S, T> { foo: T }`
pub struct DerivingIterator<'ast> {
    pub data:         &'ast syn::Data,              // { foo: T }
    pub ident:        &'ast syn::Ident,             // Foo
    pub params:       Vec<&'ast syn::GenericParam>, // <S, T>
    pub t_iterator:   syn::Ident,                   // FooIterator{Mut}
    pub iterator:     syn::Ident,                   // foo_iterator{_mut}
    pub target_param: &'ast syn::GenericParam,      // T
    pub is_mut:       IsMut,                        // are we mutable iterator?
}

impl DerivingIterator<'_> {
    pub fn new<'ast>(
        decl: &'ast syn::DeriveInput,
        target_param: &'ast syn::GenericParam,
        is_mut: IsMut,
    ) -> DerivingIterator<'ast> {
        let mut_or_not = if is_mut.is_mut() { "Mut" } else { "" };
        let data = &decl.data;
        let params = decl.generics.params.iter().collect();
        let ident = &decl.ident;
        let t_iterator = format!("{ident}Iterator{mut_or_not}");
        let iterator = t_iterator.to_snake_case();
        let t_iterator = syn::Ident::new(&t_iterator, Span::call_site());
        let iterator = syn::Ident::new(&iterator, Span::call_site());
        DerivingIterator { data, ident, params, t_iterator, iterator, target_param, is_mut }
    }

    /// Handles all enum-specific parts.
    pub fn prepare_parts_enum(&self, data: &syn::DataEnum) -> OutputParts {
        let opt_mut = &self.is_mut.to_token();
        let t_iterator = &self.t_iterator;
        let ident = &self.ident;
        let target_param = &self.target_param;
        let iterator_params = vec![self.target_param];
        let iterator_tydefs = quote!(
            // type FooIterator<'t, U> =
            //     Box<dyn Iterator<Item=&'t U> + 't>;
            // type FooIteratorMut<'t, U> =
            //     Box<dyn Iterator<Item=&'t mut U> + 't>;
            type #t_iterator<'t, #(#iterator_params),*>  =
                Box<dyn Iterator<Item=&'t #opt_mut #target_param> + 't>;
        );
        // For types that use target type parameter, refer to their
        // `IntoIterator` implementation. Otherwise, use an empty iterator.
        let arms = data.variants.iter().map(|var| {
            let con = &var.ident;
            let iter = if variant_depends_on(var, target_param) {
                quote!(elem.into_iter())
            } else {
                quote!(std::iter::empty())
            };
            quote!(#ident::#con(elem) => Box::new(#iter))
        });

        // match t {
        //     Foo::Con1(elem) => Box::new(elem.into_iter()),
        //     Foo::Con2(elem) => Box::new(std::iter::empty()),
        // }
        let iter_body = quote!( match t {  #(#arms,)*  } );
        OutputParts { iterator_tydefs, iter_body, iterator_params }
    }

    /// Handles all struct-specific parts.
    pub fn prepare_parts_struct(&self, data: &syn::DataStruct) -> OutputParts {
        let opt_mut = &self.is_mut.to_token();
        let t_iterator = &self.t_iterator;
        let target_param = &self.target_param;
        let iterator_params = self.params.clone();
        let iterator_tydefs = quote!(
            // type FooIterator<'t, T>    = impl Iterator<Item = &'t T>;
            // type FooIteratorMut<'t, T> = impl Iterator<Item = &'t mut T>;
            type #t_iterator<'t, #(#iterator_params: 't),*> =
                impl Iterator<Item = &'t #opt_mut #target_param>;
        );
        let matched_fields = DependentValue::collect_struct(data, target_param);
        let yield_fields =
            matched_fields.iter().map(|field| field.yield_value(self.is_mut)).collect_vec();

        // std::iter::empty()
        let empty_body = quote! { std::iter::empty() };

        // enso-shapely::GeneratingIterator(move || {
        //     yield &t.foo;
        // })
        // enso-shapely::GeneratingIterator(move || {
        //     yield &mut t.foo;
        // })
        let body = quote! {
            enso_shapely::GeneratingIterator
            (move || { #(#yield_fields)* })
        };

        let iter_body = if matched_fields.is_empty() { empty_body } else { body };
        OutputParts { iterator_tydefs, iter_body, iterator_params }
    }

    /// Handles common (between enum and struct) code and assembles it all
    /// into a final derivation output.
    #[allow(clippy::cognitive_complexity)]
    pub fn assemble_output(&self, parts: OutputParts) -> TokenStream {
        let iterator_tydefs = &parts.iterator_tydefs;
        let iter_body = &parts.iter_body;
        let iterator_params = &parts.iterator_params;
        let opt_mut = &self.is_mut.to_token();
        let iterator = &self.iterator;
        let t_iterator = &self.t_iterator;
        let params = &self.params;
        let ident = &self.ident;
        let target_param = &self.target_param;
        let iter_method = &self.is_mut.iter_method();

        quote! {
            #iterator_tydefs

            // pub fn foo_iterator<'t, T>
            // (t: &'t Foo<T>) -> FooIterator<'t, T> {
            //    enso-shapely::GeneratingIterator(move || {
            //        yield &t.foo;
            //    })
            // }
            // pub fn foo_iterator_mut<'t, T>
            // (t: &'t mut Foo<T>) -> FooIteratorMut<'t, T> {
            //    enso-shapely::GeneratingIterator(move || {
            //        yield &t.foo;
            //    })
            // }
            pub fn #iterator<'t, #(#params),*>
            (t: &'t #opt_mut #ident<#(#params),*>)
             -> #t_iterator<'t, #(#iterator_params),*> {
                #iter_body
            }

            // impl<'t, T>
            // IntoIterator for &'t Foo<T> {
            //     type Item     = &'t T;
            //     type IntoIter = FooIterator<'t, T>;
            //     fn into_iter(self) -> FooIterator<'t, T> {
            //         foo_iterator(self)
            //     }
            // }
            //
            // impl<'t, T>
            // IntoIterator for &'t mut Foo<T> {
            //     type Item     = &'t mut T;
            //     type IntoIter = FooIteratorMut<'t, T>;
            //     fn into_iter(self) -> FooIteratorMut<'t, T> {
            //         foo_iterator_mut(self)
            //     }
            // }
            impl<'t, #(#params),*>
            IntoIterator for &'t #opt_mut #ident<#(#params),*> {
                type Item     = &'t #opt_mut #target_param;
                type IntoIter = #t_iterator<'t, #(#iterator_params),*>;
                fn into_iter(self) -> #t_iterator<'t, #(#iterator_params),*> {
                    #iterator(self)
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
                pub fn #iter_method
                (& #opt_mut self) -> #t_iterator<'_, #(#iterator_params),*> {
                    #iterator(self)
                }
            }
        }
    }

    /// Generates the code that derives desired iterator.
    pub fn output(&self) -> TokenStream {
        let parts = match self.data {
            syn::Data::Struct(data) => self.prepare_parts_struct(data),
            syn::Data::Enum(data) => self.prepare_parts_enum(data),
            _ => panic!("Only Structs and Enums can derive(Iterator)!"),
        };
        self.assemble_output(parts)
    }
}

/// Common implementation for deriving iterator through `derive(Iterator)` and
/// `derive(IteratorMut)`.
pub fn derive(input: proc_macro::TokenStream, is_mut: IsMut) -> proc_macro::TokenStream {
    let decl = syn::parse_macro_input!(input as syn::DeriveInput);
    let params = &decl.generics.params.iter().collect::<Vec<_>>();
    let output = match params.last() {
        Some(last_param) => {
            let der = DerivingIterator::new(&decl, last_param, is_mut);
            der.output()
        }
        None => TokenStream::new(),
    };
    output.into()
}

// Note [Expansion Example]
// ~~~~~~~~~~~~~~~~~~~~~~~~
// In order to make the definition easier to read, an example expansion of the
// following definition was provided for each quotation:
//
// #[derive(Iterator)]
// pub struct Foo<S, T> { foo: T }
//
// When different output is generated for mutable and immutable content, both
// expansions are presented.
//
// For examples that are enum-specific rather than struct-specific, the
// following definition is assumed:
//
// #[derive(Iterator)]
// pub enum Foo<T> {
//     Con1(Bar<T>),
//     Con2(Baz),
// }
