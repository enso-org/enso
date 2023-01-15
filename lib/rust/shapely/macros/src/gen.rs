//! A macro allowing generation of variety of things. For now it supports a few cases, however, it
//! is meant to be a hub for other extensions. See the docs in [`lib.rs`] to learn more.

use crate::prelude::*;

use paste::paste;



const MODIFY_NAME_PREFIX: &str = "modify_";
const UPDATE_NAME_PREFIX: &str = "update_";
const SETTER_NAME_PREFIX: &str = "set_";



// ===================
// === Opts Parser ===
// ===================

macro_rules! gen_opts {
    (
        $(#$meta:tt)*
        struct $name:ident {
            $($field:ident: {$($param:ident),*}),* $(,)?
        }
    ) => { paste! {
        $(#$meta)*
        #[derive(Debug, Default)]
        struct $name {
            $($field: Option<[<$field:camel>]>),*
        }

        impl Opts {
            $(
                fn [<mk_ $field>](&mut self) -> &mut [<$field:camel>] {
                    if self.$field.is_none() {
                        self.$field = Some([<$field:camel>]::default());
                    }
                    self.$field.as_mut().unwrap()
                }
            )*

            fn parse(&mut self, name: &str, param: Option<(String, String)>) {
                match name {
                    $(stringify!($field) => self.[<mk_ $field>]().parse(param),)*
                    _ => panic!("Unknown option: {}", name),
                }
            }
        }

        $(
            #[derive(Debug, Default)]
            struct [<$field:camel>] {
                $([<param_ $param>]: Option<String>),*
            }

            impl [<$field:camel>] {
                fn parse(&mut self, param: Option<(String, String)>) {
                    match param {
                        Some((name, _value)) => match name.as_str() {
                            $(stringify!($param) => self.[<param_ $param>] = Some(_value),)*
                            _ => panic!("Unknown parameter: {}", name),
                        },
                        None =>{},
                    }
                }
            }
        )*
    }};
}

gen_opts! {
    // A mapping between options and their parameters. For example, if the macro was called as
    // `#[gen(set(trait="MyTrait"))]`, then `options` will contain `("set" -> ("trait" -> "MyTrait")`.
    struct Opts {
        debug: {},
        set: {trait, fn},
        update: {},
        skip_fields: {}
    }
}

fn parse_args(args: &syn::AttributeArgs) -> Opts {
    let mut opts = Opts::default();
    for arg in args {
        match arg {
            syn::NestedMeta::Meta(meta) => match meta {
                syn::Meta::Path(path) => {
                    let opt_name = path.segments[0].ident.to_string();
                    opts.parse(&opt_name, None);
                }
                syn::Meta::List(list) => {
                    let opt_name = list.path.segments[0].ident.to_string();
                    for nested in &list.nested {
                        match nested {
                            syn::NestedMeta::Meta(syn::Meta::NameValue(val)) => {
                                let param_name = val.path.segments[0].ident.to_string();
                                let lit = &val.lit;
                                let lit = quote!(#lit).to_string();
                                let param_value = lit
                                    .strip_prefix('"')
                                    .unwrap()
                                    .strip_suffix('"')
                                    .unwrap()
                                    .to_string();
                                opts.parse(&opt_name, Some((param_name, param_value)));
                            }
                            syn::NestedMeta::Meta(syn::Meta::Path(path)) => {
                                let param_name = path.segments[0].ident.to_string();
                                opts.parse(&opt_name, Some((param_name, "".to_string())));
                            }
                            _ => panic!("Unrecognized argument [1]: {arg:#?}"),
                        }
                    }
                }
                _ => panic!("Unrecognized argument [2]: {arg:#?}"),
            },
            _ => panic!("Unrecognized argument [3]: {arg:#?}"),
        }
    }
    opts
}



// ===============================
// === Special Cases Discovery ===
// ===============================

enum SpecialCase {
    Vector(TokenStream, Vec<&'static str>),
}

fn discover_special_cases(arg: &syn::Type) -> Option<SpecialCase> {
    match arg {
        syn::Type::Path(tp) => {
            let segment = &tp.path.segments[0];
            let name = segment.ident.to_string();
            if name.starts_with("Vector") {
                match &segment.arguments {
                    syn::PathArguments::AngleBracketed(args) => {
                        let args = &args.args;
                        let tokens = quote!(#args);
                        let fields = match name.as_str() {
                            "Vector2" => vec!["x", "y"],
                            "Vector3" => vec!["x", "y", "z"],
                            "Vector4" => vec!["x", "y", "z", "w"],
                            _ => panic!("Unknown vector type: {name}"),
                        };
                        Some(SpecialCase::Vector(tokens, fields))
                    }
                    _ => None,
                }
            } else {
                None
            }
        }
        _ => None,
    }
}



// ========================
// === Main entry point ===
// ========================

pub fn run(
    args: proc_macro::TokenStream,
    input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let args = syn::parse_macro_input!(args as syn::AttributeArgs);
    let opts = parse_args(&args);

    let input_fn = syn::parse_macro_input!(input as syn::ImplItemMethod);
    let input_fn_name = &input_fn.sig.ident;
    let input_fn_name_str = input_fn_name.to_string();
    if !input_fn_name_str.starts_with(MODIFY_NAME_PREFIX) {
        panic!("Method name must start with '{MODIFY_NAME_PREFIX}'");
    }

    let core_fn_name = &input_fn_name_str[MODIFY_NAME_PREFIX.len()..];
    let fn_inputs = &input_fn.sig.inputs;
    let arg = match &fn_inputs[1] {
        syn::FnArg::Typed(pat_type) => match &*pat_type.ty {
            syn::Type::ImplTrait(impl_trait) => {
                let bound = &impl_trait.bounds[0];
                match bound {
                    syn::TypeParamBound::Lifetime(_) => {
                        panic!("Lifetime bound is not supported");
                    }
                    syn::TypeParamBound::Trait(t) => {
                        let segment = &t.path.segments[0];
                        match &segment.arguments {
                            syn::PathArguments::Parenthesized(args) => {
                                let arg = &args.inputs[0];
                                match &arg {
                                    syn::Type::Reference(tp) => &*tp.elem,
                                    _ => panic!("err"),
                                }
                            }
                            _ => panic!("err"),
                        }
                    }
                }
            }
            _ => panic!("The argument must be of type 'impl FnOnce(&mut T)'."),
        },
        _ => panic!("Expected a typed argument"),
    };

    let mut out = vec![];

    if opts.update.is_some() {
        let mut update_fn = input_fn.clone();
        let mut out_tp = arg.clone();
        if let Some(set_trait) = opts.set.as_ref().and_then(|t| t.param_trait.as_ref()) {
            let set_trait: syn::Type = syn::parse_str(set_trait).unwrap();
            out_tp = syn::parse_str("Out").unwrap();
            update_fn.sig.generics.params.push(syn::parse_quote!(#out_tp: #set_trait));
        }
        update_fn.sig.ident = quote::format_ident!("{UPDATE_NAME_PREFIX}{core_fn_name}");
        update_fn.block = match &opts.set.as_ref().and_then(|t| t.param_fn.as_ref()) {
            None => syn::parse_quote! {{ self.#input_fn_name(|t| *t = f(*t)) }},
            Some(fn_name) => {
                let fn_expr: syn::Expr = syn::parse_str(fn_name).unwrap();
                syn::parse_quote! {{ self.#input_fn_name(|t| *t = f(*t).#fn_expr) }}
            }
        };
        update_fn.sig.inputs[1] = syn::parse_quote! {f: impl FnOnce(#arg) -> #out_tp};
        out.push(quote! {#update_fn});
    }

    if let Some(opts_set) = opts.set.as_ref() {
        let mut set_fn = input_fn.clone();
        set_fn.sig.ident = quote::format_ident!("{SETTER_NAME_PREFIX}{core_fn_name}");
        set_fn.block = match opts_set.param_fn.as_ref() {
            None => syn::parse_quote! {{ self.#input_fn_name(|t| *t = v.into()) }},
            Some(fn_name) => {
                let fn_expr: syn::Expr = syn::parse_str(fn_name).unwrap();
                syn::parse_quote! {{ self.#input_fn_name(|t| *t = v.#fn_expr) }}
            }
        };
        set_fn.sig.inputs[1] = match opts_set.param_trait.as_ref() {
            None => syn::parse_quote! {v: impl Into<#arg>},
            Some(trait_str) => {
                let trait_ty: syn::Type = syn::parse_str(trait_str).unwrap();
                syn::parse_quote! {v: impl #trait_ty}
            }
        };
        out.push(quote! {#set_fn});

        if opts.skip_fields.is_none() {
            if let Some(SpecialCase::Vector(vec_param, vec_fields)) = discover_special_cases(arg) {
                for vec_field in vec_fields {
                    let mut modify_fn = input_fn.clone();
                    modify_fn.sig.ident =
                        quote::format_ident!("{MODIFY_NAME_PREFIX}{core_fn_name}_{vec_field}");
                    let vec_field = quote::format_ident!("{vec_field}");
                    modify_fn.block = syn::parse_quote! {{
                        self.#input_fn_name(|t| f(&mut t.#vec_field))
                    }};
                    modify_fn.sig.inputs[1] = syn::parse_quote! {f: impl FnOnce(&mut #vec_param)};
                    out.push(quote! {
                        #[enso_shapely::gen(update, set)]
                        #modify_fn
                    });
                }
            }
        }
    }


    let output = quote! {
        #input_fn
        #(#out)*
    };

    let out: proc_macro::TokenStream = output.into();

    if opts.debug.is_some() {
        panic!("{}", out);
    }
    out
}
