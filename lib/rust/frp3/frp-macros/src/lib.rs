#![feature(proc_macro_span)]

use proc_macro::Delimiter;
use proc_macro::Group;
use proc_macro::Literal;
use proc_macro::Punct;
use proc_macro::Spacing;
use proc_macro::TokenStream;
use proc_macro::TokenTree;

#[proc_macro]
pub fn divide_on_terminator(item: TokenStream) -> TokenStream {
    let mut tokens = item.into_iter().peekable();
    let tokens = tokens.by_ref();

    let call_def = tokens.next().expect("Missing call definition");
    let TokenTree::Group(call_def) = call_def else {
        panic!("Expected call to be a group");
    };
    let mut call_def = call_def.stream().into_iter();
    let call = call_def.next().expect("Missing call");
    let args = call_def.next().expect("Missing args");
    let TokenTree::Group(call) = call else {
        panic!("Expected call to be a group");
    };
    let TokenTree::Group(args) = args else {
        panic!("Expected call to be a group");
    };

    let mut split = TokenStream::new();
    while let Some(next_token) = tokens.peek() {
        let span = next_token.span();
        let line = span.start().line as u32;

        let mut all_args = TokenStream::new();
        all_args.extend([TokenTree::Literal(Literal::u32_unsuffixed(line))]);
        all_args.extend(args.stream());
        all_args.extend(
            tokens.take_while(|t| !matches! { t, TokenTree::Punct(p) if p.as_char() == ';'}),
        );
        let arg_list = TokenTree::Group(Group::new(Delimiter::Parenthesis, all_args));
        split.extend(call.stream());
        split.extend([
            TokenTree::Punct(Punct::new('!', Spacing::Joint)),
            arg_list,
            TokenTree::Punct(Punct::new(';', Spacing::Joint)),
        ]);

        // let debug_lit = format!("{:?} {:?}", span.start(), span.end());
        // split.extend(dbg(&debug_lit));
    }

    // let mut expanded = TokenStream::new();
    // expanded.extend(call.stream());
    // expanded.extend([
    //     TokenTree::Punct(Punct::new('!', Spacing::Joint)),
    //     TokenTree::Group(Group::new(Delimiter::Parenthesis, call_group)),
    // ]);
    split
}

// fn dbg(literal: &str) -> TokenStream {
//     let lit = TokenTree::Literal(Literal::string(literal));
//     [
//         TokenTree::Ident(Ident::new("dbg", Span::call_site())),
//         TokenTree::Punct(Punct::new('!', Spacing::Joint)),
//         TokenTree::Group(Group::new(Delimiter::Parenthesis, lit.into())),
//         TokenTree::Punct(Punct::new(';', Spacing::Joint)),
//     ]
//     .into_iter()
//     .collect()
// }
