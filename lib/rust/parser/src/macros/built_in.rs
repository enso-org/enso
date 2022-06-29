use crate::macros::pattern::*;
use crate::macros::*;



// =======================
// === Built-in macros ===
// =======================

/// All built-in macro definitions.
pub fn all() -> resolver::MacroMatchTree<'static> {
    let mut macro_map = resolver::MacroMatchTree::default();
    // macro_map.register(if_then());
    // macro_map.register(if_then_else());
    macro_map.register(group());
    macro_map.register(type_def());
    macro_map
}

/// If-then-else macro definition.
pub fn if_then_else<'s>() -> Definition<'s> {
    crate::macro_definition! {("if", Everything, "then", Everything, "else", Everything)}
}

/// If-then macro definition.
pub fn if_then<'s>() -> Definition<'s> {
    crate::macro_definition! {("if", Everything, "then", Everything)}
}

/// Group macro definition.
pub fn group<'s>() -> Definition<'s> {
    crate::macro_definition! {("(", Everything, ")", Nothing)}
}

/// New type definition macro definition.
pub fn type_def<'s>() -> Definition<'s> {
    use pattern::*;
    #[rustfmt::skip]
    let pattern = 
        Identifier / "name" % "type name" >>
        Many((Identifier & "type parameter")/ "param" ) % "type parameters" >>
        Block(
            Many(Identifier / "constructor") % "type constructors" >> 
            Everything
        ) % "type definition body";
    // 
    // let pattern = pat! {
    //     Identifier @ name ? "type name" >>
    //         Many((Identifier ?? "type parameter") @ param ) ? "type parameters" >>
    //         Block(
    //             Many(Identifier ?? "constructor") ? "type constructors" >>
    //             Everything
    //         ) ? "type definition body";
    //     };
    // let pattern2 = Everything;
    crate::macro_definition! {
        ("type", pattern)
        type_def_body
    }
}

fn type_def_body<'s>(matched_segments: NonEmptyVec<MatchedSegment<'s>>) -> syntax::Tree<'s> {
    let segment = matched_segments.to_vec().pop().unwrap();
    println!(">>>");
    println!("{:#?}", segment);
    println!(">>>");
    let match_tree = segment.result.into_match_tree();
    println!("{:#?}", match_tree);
    println!("\n\n-------------");

    let mut v = match_tree.view();
    println!("{:#?}", v.query("name"));

    let params = v.nested().query("param").unwrap();
    let params = params
        .into_iter()
        .map(|tokens| resolver::resolve_operator_precedence(tokens.clone()))
        .collect_vec();
    println!("{:#?}", params);

    syntax::Tree::type_def(segment.header, params)
}
