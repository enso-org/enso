use crate::prelude::*;

use proc_macro2::Ident;



pub fn overlappable(
    attrs: proc_macro::TokenStream,
    input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let _attrs: TokenStream = attrs.into();
    let decl = syn::parse_macro_input!(input as syn::ItemImpl);
    //    let mut path = decl.trait_.unwrap().1.clone();
    //    let path     = path.segments.last_mut().iter().map(|ident| {
    //        Ident::new(&format!("MarketCtx_{}", repr(ident)) , Span::call_site());
    //    });

    let mut marker_ctx_impl = decl;
    let mut trait_ = marker_ctx_impl.trait_.as_mut();
    trait_.iter_mut().for_each(|t| {
        let path = &mut t.1;
        path.segments.last_mut().iter_mut().for_each(|s| {
            let rr = repr(&s);
            s.ident = Ident::new(&format!("MarketCtx_{rr}"), Span::call_site());
        });
    });

    //    let mut marker_ctx_impl = decl.clone();
    //    let path = &mut marker_ctx_impl.trait_.as_mut().unwrap().1;
    //    path.segments.last_mut().iter_mut().for_each(|s| {
    //        let rr = repr(&s);
    //        s.ident = Ident::new(&format!("MarketCtx_{}", rr) , Span::call_site());
    //    });

    //    let name   = repr(path);

    //    let marker_ctx_impl = syn::ItemImpl {
    //        .. decl
    //    };


    let _output_tmp = quote! {
        #marker_ctx_impl
    };
    let output = quote! {};
    //    println!("------------------");
    //    println!("{}", output_tmp);
    output.into()
}
