//! Implementation of [`wrap_async_fn`], a helper for macros that operate on async functions.

use quote::ToTokens;
use syn::visit_mut;
use syn::visit_mut::VisitMut;



// ========================
// === VisitMut helpers ===
// ========================

/// Used in a `impl VisitMut` to block descent into inner function items.
macro_rules! ignore_inner_fn_items {
    () => {
        fn visit_item_fn_mut(&mut self, _: &mut syn::ItemFn) {}
    };
}



// =====================
// === wrap_async_fn ===
// =====================

/// Convert an `async fn` into a `fn` returning a `Future`, implemented with an `async` block.
///
/// The output is functionally equivalent to the input (except the output won't `impl Send` even if
/// the original `async fn` did); this is useful as a basis for further transformation.
pub fn wrap_async_fn(func: &mut syn::ItemFn) {
    // Wrap the body.
    let block = &func.block;
    let ret_ty = match &func.sig.output {
        syn::ReturnType::Default => quote::quote! { () },
        syn::ReturnType::Type(_, ty) => ty.into_token_stream(),
    };
    let body = quote::quote! {{
        (async move {
            let result: #ret_ty = #block;
            result
        })
    }};
    func.block = syn::parse2(body).unwrap();

    // Transform the signature.
    let output_ty = match &func.sig.output {
        syn::ReturnType::Default => quote::quote! { () },
        syn::ReturnType::Type(_, ty) => ty.to_token_stream(),
    };
    let output_lifetime = syn::Lifetime::new("'__profiler_out", proc_macro2::Span::call_site());
    let lifetimes = explicitize_lifetimes(&mut func.sig);
    let output = if lifetimes.is_empty() {
        quote::quote! {
            -> impl std::future::Future<Output=#output_ty>
        }
    } else {
        // Bound all inputs on the output lifetime.
        let type_bound = syn::TypeParamBound::Lifetime(output_lifetime.clone());
        for param in &mut func.sig.generics.params {
            match param {
                syn::GenericParam::Lifetime(def) => def.bounds.push(output_lifetime.clone()),
                syn::GenericParam::Type(def) => def.bounds.push(type_bound.clone()),
                syn::GenericParam::Const(_) => (),
            }
        }
        for arg in &mut func.sig.inputs {
            if let syn::FnArg::Typed(syn::PatType { ty, .. }) = arg {
                if let syn::Type::ImplTrait(def) = ty.as_mut() {
                    def.bounds.push(type_bound.clone());
                }
            }
        }
        // Add a definition for the output lifetime.
        let lifetime_def = syn::LifetimeParam::new(output_lifetime.clone());
        func.sig.generics.params.insert(0, syn::GenericParam::Lifetime(lifetime_def));
        // Apply the output lifetime to the output.
        quote::quote! {
            -> impl std::future::Future<Output=#output_ty> + #output_lifetime
        }
    };
    func.sig.asyncness = None;
    func.sig.output = syn::parse2(output).unwrap();
}


// === Lifetime transformation ===

/// Make all lifetimes in function signature explicit.
///
/// Returns the lifetimes used in the function signature.
fn explicitize_lifetimes(sig: &mut syn::Signature) -> Vec<syn::Lifetime> {
    // Go through the args; find:
    // - anonymous lifetime: '_
    // - implicit lifetimes: &foo
    // - explicit lifetimes: &'a
    // Make all input lifetimes explicit:
    // - Use new lifetime explicitly in arg list.
    // - Define new lifetime in generic params.
    let mut input_transformer = ExplicitizeInputLifetimes::default();
    for input in &mut sig.inputs {
        input_transformer.visit_fn_arg_mut(input);
    }
    let ExplicitizeInputLifetimes { new_lifetimes, existing_lifetimes } = input_transformer;
    let mut all_lifetimes = existing_lifetimes;
    all_lifetimes.extend_from_slice(&new_lifetimes);
    let new_lifetimes = new_lifetimes
        .into_iter()
        .map(|lt| syn::GenericParam::Lifetime(syn::LifetimeParam::new(lt)));
    sig.generics.params.extend(new_lifetimes);
    // There are two cases where output lifetimes may be elided:
    // - There's exactly one lifetime in the inputs.
    // - There's a receiver with a lifetime.
    // If either case occurs, make any implicit output lifetimes explicit.
    let default_lifetime = if all_lifetimes.len() == 1 {
        Some(all_lifetimes[0].clone())
    } else {
        get_receiver_lifetime(sig).cloned()
    };
    if let Some(lifetime) = default_lifetime {
        ExplicitizeOutputLifetimes { lifetime }.visit_return_type_mut(&mut sig.output);
    }
    all_lifetimes
}

#[derive(Default)]
struct ExplicitizeInputLifetimes {
    new_lifetimes:      Vec<syn::Lifetime>,
    existing_lifetimes: Vec<syn::Lifetime>,
}

impl ExplicitizeInputLifetimes {
    fn gen_lifetime(&mut self) -> syn::Lifetime {
        let name = format!("'__profiler{}", self.new_lifetimes.len());
        let new = syn::Lifetime::new(&name, proc_macro2::Span::call_site());
        self.new_lifetimes.push(new.clone());
        new
    }

    fn visit_elidable_lifetime(&mut self, lifetime: &mut Option<syn::Lifetime>) {
        match lifetime {
            Some(lifetime) => self.visit_lifetime_mut(lifetime),
            None => *lifetime = Some(self.gen_lifetime()),
        }
    }
}

impl visit_mut::VisitMut for ExplicitizeInputLifetimes {
    ignore_inner_fn_items!();

    // Handles 'x in generic parameters in types of non-self arguments.
    fn visit_lifetime_mut(&mut self, lifetime: &mut syn::Lifetime) {
        let name = lifetime.ident.to_string();
        if &name == "_" {
            *lifetime = self.gen_lifetime();
        } else {
            self.existing_lifetimes.push(lifetime.clone());
        }
    }

    // Handles &self.
    fn visit_receiver_mut(&mut self, receiver: &mut syn::Receiver) {
        if let Some((_, lifetime)) = &mut receiver.reference {
            self.visit_elidable_lifetime(lifetime);
        }
    }

    // Handles & in types of non-self arguments.
    fn visit_type_reference_mut(&mut self, type_reference: &mut syn::TypeReference) {
        self.visit_elidable_lifetime(&mut type_reference.lifetime);
    }
}

struct ExplicitizeOutputLifetimes {
    lifetime: syn::Lifetime,
}

impl visit_mut::VisitMut for ExplicitizeOutputLifetimes {
    ignore_inner_fn_items!();

    // Handles 'x in generic parameters in types.
    fn visit_lifetime_mut(&mut self, lifetime: &mut syn::Lifetime) {
        if &lifetime.ident.to_string() == "_" {
            *lifetime = self.lifetime.clone();
        }
    }

    // Handles & in types.
    fn visit_type_reference_mut(&mut self, type_reference: &mut syn::TypeReference) {
        if type_reference.lifetime.is_none() {
            type_reference.lifetime = Some(self.lifetime.clone());
        }
    }
}

fn get_receiver_lifetime(sig: &syn::Signature) -> Option<&syn::Lifetime> {
    match sig.inputs.first() {
        Some(syn::FnArg::Receiver(syn::Receiver {
            reference: Some((_, Some(lifetime))), ..
        })) => Some(lifetime),
        _ => None,
    }
}
