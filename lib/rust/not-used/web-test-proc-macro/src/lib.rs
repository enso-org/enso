#![warn(unsafe_code)]
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]

extern crate proc_macro;

use proc_macro::TokenStream;
use quote::quote;
use syn::*;

// FIXME: Parse proc_macro args to read the following info:
// #[web_test(dimensions(320.0, 240.0)) and
// #[web_test(no_container)].

// ===================
// === #[web_test] ===
// ===================

/// #[web_test] creates a [320.0, 240.0] div with id = fn_name and appends it
/// into the document.
/// # Example
/// ```rust,compile_fail
/// use web_test::web_test;
/// use web_test::web_configure;
/// use ensogl::system::web::get_element_by_id;
///
/// web_configure!(run_in_browser);
///
/// #[web_test]
/// fn get_identified_element() {
///     // #[web_test] creates a <div id="get_identified_element"></div>
///     assert!(get_element_by_id("get_identified_element").is_ok());
/// }
/// ```
#[proc_macro_attribute]
pub fn web_test(_args: TokenStream, input: TokenStream) -> TokenStream {
    if let Ok(mut parsed) = syn::parse::<ItemFn>(input.clone()) {
        let fn_string = format!("{}", parsed.sig.ident);
        let code = format!("Container::new(\"Tests\", \"{}\", 320.0, 240.0);", fn_string);

        if let Ok(stmt) = parse_str::<Stmt>(&code) {
            // We insert Container::new("Tests", fn_name, 320.0, 240.0)
            // at the beginning of the function block.
            parsed.block.stmts.insert(0, stmt);

            let output = quote! {
                #[wasm_bindgen_test]
                #parsed
            };
            output.into()
        } else {
            input
        }
    } else {
        input
    }
}


// ====================
// === #[web_bench] ===
// ====================

/// #[web_bench] creates a benchmark div with a toggle button.
/// # Example
/// ```rust,compile_fail
/// use web_test::web_bench;
/// use web_test::web_configure;
/// use ensogl::system::web::get_element_by_id;
/// use ensogl::system::web::dyn_into;
/// use web_sys::HtmlElement;
///
/// web_configure!(run_in_browser);
///
/// #[web_bench]
/// fn test_performance(b: &mut Bencher) {
///     let element = get_element_by_id("test_performance").expect("div");
///     let element : HtmlElement = dyn_into(element).expect("HtmlElement");
///
///     let numbers : Vec<_> = (1 ..= 1000).collect();
///     b.iter(move || {
///         let ans = numbers.iter().fold(0, |acc, x| acc + x);
///         element.set_inner_html(&format!("Answer: {}", ans));
///     })
/// }
/// ```
#[proc_macro_attribute]
pub fn web_bench(_args: TokenStream, input: TokenStream) -> TokenStream {
    if let Ok(parsed) = parse::<ItemFn>(input.clone()) {
        use proc_macro2::*;
        let input: TokenStream = input.into();

        let fn_ident = parsed.sig.ident;
        let fn_string = format!("{}", fn_ident);
        let fn_benchmark_str = format!("{}_benchmark", fn_string);
        let fn_benchmark = Ident::new(&fn_benchmark_str, Span::call_site());

        let output = quote! {
            #[wasm_bindgen_test]
            fn #fn_benchmark() {
                let container = BenchContainer::new(#fn_string, 320.0, 240.0);
                let mut b = Bencher::new(container);
                #fn_ident(&mut b);
            }
            #input
        };
        output.into()
    } else {
        input
    }
}
