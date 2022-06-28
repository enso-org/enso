use crate::macros::pattern::*;
use crate::macros::*;



// =======================
// === Built-in macros ===
// =======================

pub fn all() -> resolver::MacroMatchTree<'static> {
    let mut macro_map = resolver::MacroMatchTree::default();
    // macro_map.register(macro_if_then());
    // macro_map.register(macro_if_then_else());
    // macro_map.register(macro_group());
    // macro_map.register(macro_lambda());
    macro_map.register(macro_type_def());
    macro_map
}

fn macro_if_then_else<'s>() -> Definition<'s> {
    crate::macro_definition! {("if", Everything, "then", Everything, "else", Everything)}
}

fn macro_if_then<'s>() -> Definition<'s> {
    crate::macro_definition! {("if", Everything, "then", Everything)}
}

fn macro_group<'s>() -> Definition<'s> {
    crate::macro_definition! {("(", Everything, ")", Nothing)}
}

fn macro_type_def<'s>() -> Definition<'s> {
    use pattern::*;
    let pattern = Identifier / "name" % "type name"
        >> (Identifier / "param").many() % "type parameters"
        >> Block((Identifier / "constructor").many() % "type constructors" >> Everything)
            % "type definition body";
    // let pattern2 = Everything;
    crate::macro_definition! {
        ("type", pattern)
        ttt
    }
}

fn ttt<'s>(matched_segments: NonEmptyVec<MatchedSegment<'s>>) -> syntax::Tree<'s> {
    let header = matched_segments.first().header.clone();
    println!(">>>");
    println!("{:#?}", matched_segments);
    println!(">>>");
    println!("{:#?}", matched_segments.mapped(|t| t.result.into_match_tree()));
    syntax::Tree::type_def(header)
}
