use crate::prelude::*;


const MODIFY_NAME_PREFIX: &str = "modify_";
const UPDATE_NAME_PREFIX: &str = "update_";
const SETTER_NAME_PREFIX: &str = "set_";

pub fn run(
    args: proc_macro::TokenStream,
    input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let mut debug = false;
    let mut gen_update = false;
    let mut gen_set = false;
    let mut gen_set_trait = None;
    let mut gen_set_fn = None;

    let args = syn::parse_macro_input!(args as syn::AttributeArgs);

    for arg in args {
        match arg {
            syn::NestedMeta::Meta(meta) => match meta {
                syn::Meta::Path(path) => match path.segments[0].ident.to_string().as_str() {
                    "debug" => debug = true,
                    "update" => gen_update = true,
                    "set" => gen_set = true,
                    _ => panic!("Unknown argument"),
                },
                syn::Meta::List(list) => match list.path.segments[0].ident.to_string().as_str() {
                    "update" => gen_update = true,
                    "set" => {
                        gen_set = true;
                        for nested in &list.nested {
                            match nested {
                                syn::NestedMeta::Meta(syn::Meta::NameValue(val)) => {
                                    match val.path.segments[0].ident.to_string().as_str() {
                                        "trait" => {
                                            let lit = &val.lit;
                                            let lit = quote!(#lit).to_string();
                                            let lit = lit
                                                .strip_prefix("\"")
                                                .unwrap()
                                                .strip_suffix("\"")
                                                .unwrap();
                                            let tp: syn::Type = syn::parse_str(lit).unwrap();
                                            gen_set_trait = Some(tp);
                                        }
                                        "fn" => {
                                            let lit = &val.lit;
                                            let lit = quote!(#lit).to_string();
                                            let lit = lit
                                                .strip_prefix("\"")
                                                .unwrap()
                                                .strip_suffix("\"")
                                                .unwrap();
                                            let expr: syn::Expr = syn::parse_str(lit).unwrap();
                                            gen_set_fn = Some(expr);
                                        }
                                        _ => panic!("Unknown argument."),
                                    }
                                }
                                _ => panic!("Unknown argument."),
                            }
                        }
                    }
                    _ => panic!("Unknown argument."),
                },
                _ => panic!("Unrecognized argument."),
            },
            _ => panic!("Unrecognized argument."),
        }
    }
    let modify_fn = syn::parse_macro_input!(input as syn::ImplItemMethod);
    let modify_fn_name = &modify_fn.sig.ident;
    let modify_fn_name_str = modify_fn_name.to_string();
    if !modify_fn_name_str.starts_with(MODIFY_NAME_PREFIX) {
        panic!("Method name must start with '{}'", MODIFY_NAME_PREFIX);
    }

    let core_fn_name = &modify_fn_name_str[MODIFY_NAME_PREFIX.len()..];

    let fn_inputs = &modify_fn.sig.inputs;
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

    // if debug {
    let arg_params = match arg {
        syn::Type::Path(tp) => {
            let segment = &tp.path.segments[0];
            match segment.ident.to_string().as_str() {
                "Vector2" => match &segment.arguments {
                    syn::PathArguments::AngleBracketed(args) => Some(&args.args),
                    _ => None,
                },
                _ => None,
            }
        }
        _ => None,
    };

    let mut out = vec![];

    if gen_update {
        let mut update_fn = modify_fn.clone();
        update_fn.sig.ident = quote::format_ident!("{}{}", UPDATE_NAME_PREFIX, core_fn_name);
        update_fn.block = syn::parse_quote! {{
            self.#modify_fn_name(|t| *t = f(*t))
        }};
        update_fn.sig.inputs[1] = syn::parse_quote! {f: impl FnOnce(#arg) -> #arg};
        out.push(quote! {#update_fn});
    }

    if gen_set {
        let mut set_fn = modify_fn.clone();
        set_fn.sig.ident = quote::format_ident!("{}{}", SETTER_NAME_PREFIX, core_fn_name);
        set_fn.block = match gen_set_fn {
            None => syn::parse_quote! {{ self.#modify_fn_name(|t| *t = v.into()) }},
            Some(t) => syn::parse_quote! {{ self.#modify_fn_name(|t| *t = v.#t) }},
        };
        set_fn.sig.inputs[1] = match gen_set_trait {
            None => syn::parse_quote! {v: impl Into<#arg>},
            Some(t) => syn::parse_quote! {v: impl #t},
        };
        out.push(quote! {#set_fn});

        if let Some(params) = arg_params {
            let mut modify_x_fn = modify_fn.clone();
            modify_x_fn.sig.ident =
                quote::format_ident!("{}{}_x", MODIFY_NAME_PREFIX, core_fn_name);
            modify_x_fn.block = syn::parse_quote! {{
                self.#modify_fn_name(|t| f(&mut t.x))
            }};
            modify_x_fn.sig.inputs[1] = syn::parse_quote! {f: impl FnOnce(&mut #params)};
            out.push(quote! {
                #[enso_shapely::gen(update, set)]
                #modify_x_fn
            });

            let mut modify_y_fn = modify_fn.clone();
            modify_y_fn.sig.ident =
                quote::format_ident!("{}{}_y", MODIFY_NAME_PREFIX, core_fn_name);
            modify_y_fn.block = syn::parse_quote! {{
                self.#modify_fn_name(|t| f(&mut t.y))
            }};
            modify_y_fn.sig.inputs[1] = syn::parse_quote! {f: impl FnOnce(&mut #params)};
            out.push(quote! {
                #[enso_shapely::gen(update, set)]
                #modify_y_fn
            });
        }
    }


    let output = quote! {
        #modify_fn
        #(#out)*
    };

    let out: proc_macro::TokenStream = output.into();

    if debug {
        panic!("{}", out);
    }
    out
}
