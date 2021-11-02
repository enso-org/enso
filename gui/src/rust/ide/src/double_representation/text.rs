//! A module with functions used to support working with text representation of the language.

use crate::prelude::*;

use ast::IdMap;
use data::text::Size;
use data::text::Span;



// ================
// === Text API ===
// ================

/// Update IdMap to reflect the recent code change.
pub fn apply_code_change_to_id_map(
    id_map: &mut IdMap,
    change: &data::text::TextChange,
    code: &str,
) {
    // TODO [mwu]
    //   The initial provisional algorithm received some changes to better behave in our typical
    //   editor use-cases, i.e. to keep node ids when editing its expression. However, this came
    //   at price of not properly keeping other sub-ids on parts of the node line.
    //   In future, better and cleaner algorithm will need to be provided, likely with a different
    //   API. Because of such expected rewrite and deeper restructuring, we don't really want to
    //   spend much time on refactoring this function right now, even if it could be made nicer.


    let removed = change.replaced_span();
    let inserted = change.inserted.as_str();
    let new_code = change.applied(code);
    let non_white = |c: char| !c.is_whitespace();
    let logger = logger::DefaultWarningLogger::new("apply_code_change_to_id_map");
    let vector = &mut id_map.vec;
    let inserted_size = Size::from_text(inserted);

    info!(logger, "Old code:\n```\n{code}\n```");
    info!(logger, "New code:\n```\n{new_code}\n```");
    info!(logger, "Updating the ID map with the following text edit: {change:?}.");

    // Remove all entries fully covered by the removed span.
    vector.drain_filter(|(span, _)| removed.contains_span(span));

    // If the edited section ends up being the trailing part of AST node, how many bytes should be
    // trimmed from the id. Precalculated, as is constant in the loop below.
    let to_trim_back = {
        let last_non_white = inserted.rfind(non_white);
        let inserted_len = || inserted.len();
        let length_to_last_non_white = |index| inserted.len() - index - 1;
        Size::new(last_non_white.map_or_else(inserted_len, length_to_last_non_white))
    };
    // As above but for the front side.
    let to_trim_front = {
        let first_non_white = inserted.find(non_white);
        let ret = first_non_white.unwrap_or_else(|| inserted.len());
        Size::new(ret)
    };

    let inserted_non_white = inserted.chars().any(non_white);

    // In case of collisions (when, after resizing spans, multiple ids for the same span are
    // present), the mappings from this map will be preferred over other ones.
    //
    // This is needed for edits like: `foo f` => `foo` — the earlier `foo` in `foo f` also has a
    // id map entry, however we want it to be consistently shadowed by the id from the whole App
    // expression.
    let mut preferred: HashMap<Span, ast::Id> = default();

    for (span, id) in vector.iter_mut() {
        // These
        let mut trim_front = false;
        let mut trim_back = false;
        let initial_span = *span;
        info!(logger, "Processing @{span}: `{&code[*span]}`.");
        if span.index > removed.end() {
            debug!(logger, "Node after the edited region.");
            // AST node starts after edited region — it will be simply shifted.
            let code_between = &code[Span::from(removed.end()..span.index)];
            span.move_left(removed.size);
            span.move_right(inserted_size);

            // If there are only spaces between current AST symbol and insertion, extend the symbol.
            // This is for cases like line with `foo ` being changed into `foo j`.
            debug!(logger, "Between: `{code_between}`.");
            if all_spaces(code_between) && inserted_non_white {
                debug!(logger, "Will extend the node leftwards.");
                span.extend_left(inserted_size);
                span.extend_left(Size::from_text(code_between));
                trim_front = true;
            }
        } else if span.index >= removed.index {
            // AST node starts inside the edited region. It doesn't end strictly inside it.
            debug!(logger, "Node overlapping with the end of the edited region.");
            let removed_before = span.index - removed.index;
            span.move_left(removed_before);
            span.shrink_right(removed.size - removed_before);
            span.extend_right(inserted_size);
            trim_front = true;
        } else if span.end() >= removed.index {
            // AST node starts before the edited region and reaches (or possibly goes past) its end.
            debug!(logger, "Node overlapping with the beginning of the edited region.");
            if span.end() <= removed.end() {
                trim_back = true;
            }
            let removed_chars = (span.end() - removed.index).min(removed.size);
            span.shrink_right(removed_chars);
            span.extend_right(inserted_size);
        } else {
            debug!(logger, "Node before the edited region.");
            // If there are only spaces between current AST symbol and insertion, extend the symbol.
            // This is for cases like line with `foo ` being changed into `foo j`.
            let between = &code[Span::from(span.end()..removed.index)];
            if all_spaces(between) && inserted_non_white {
                debug!(logger, "Will extend ");
                span.size += Size::from_text(between) + inserted_size;
                trim_back = true;
            }
        }

        if trim_front && to_trim_front.non_empty() {
            span.shrink_left(to_trim_front);
            debug!(logger, "Trimming front {to_trim_front} bytes.");
        }

        if trim_back {
            if to_trim_back.non_empty() {
                span.shrink_right(to_trim_back);
                debug!(logger, "Trimming back {to_trim_back} bytes.");
            }
            let new_repr = &new_code[*span];
            // Trim trailing spaces
            let spaces = spaces_size(new_repr.chars().rev());
            if spaces.non_empty() {
                debug!(logger, "Additionally trimming {spaces} trailing spaces.");
                debug!(logger, "The would-be code: `{new_repr}`.");
                span.shrink_right(spaces);
            }
        }

        // If we edited front or end of an AST node, its extended (or shrunk) span will be
        // preferred.
        if trim_front || trim_back {
            preferred.insert(*span, *id);
        }

        info!(
            logger,
            "Processing for id {id}: {initial_span} ->\t{span}.\n\
        Code: `{&code[initial_span]}` => `{&new_code[*span]}`"
        );
    }

    // If non-preferred entry collides with the preferred one, remove the former.
    vector.drain_filter(|(span, id)| {
        preferred.get(span).map(|preferred_id| id != preferred_id).unwrap_or(false)
    });
}



// ===============
// === Helpers ===
// ===============

/// Returns the byte length of leading space characters sequence.
fn spaces_size(itr: impl Iterator<Item = char>) -> Size {
    Size::new(itr.take_while(|c| *c == ' ').fold(0, |acc, _| acc + 1))
}

/// Checks if the given string slice contains only space charactesr.
fn all_spaces(text: &str) -> bool {
    text.chars().all(|c| c == ' ')
}



// =============
// === Tests ===
// =============

#[cfg(test)]
mod test {
    use super::*;

    use crate::double_representation::module;

    use ast::HasIdMap;
    use data::text::Index;
    use data::text::TextChange;
    use enso_prelude::default;
    use parser::Parser;
    use uuid::Uuid;

    /// A sample text edit used to test "text api" properties.
    ///
    /// See `from_markdown` constructor function for helper markdown description.
    struct Case {
        /// The initial enso program code.
        pub code:   String,
        /// The edit made to the initial code.
        pub change: TextChange,
    }

    impl Case {
        /// Markdown supports currently a single edit in the given code piece. It must be of form
        /// `«aa⎀bb»` which reads "replace `aa` with `bb`".
        fn from_markdown(marked_code: impl AsRef<str>) -> Case {
            let marked_code = marked_code.as_ref();
            let index_of = |c| marked_code.find(c);

            const START: char = '«';
            const INSERTION: char = '⎀';
            const END: char = '»';

            match (index_of(START), index_of(INSERTION), index_of(END)) {
                (Some(start), insertion, Some(end)) => {
                    assert!(start < end, "Markdown markers discovered in wrong order.");
                    let erased_finish = insertion.unwrap_or(end);
                    let code = {
                        let prefix = &marked_code[..start];
                        let erased = &marked_code[start + START.len_utf8()..erased_finish];
                        let suffix = &marked_code[end + END.len_utf8()..];
                        String::from_iter([prefix, erased, suffix].iter().copied())
                    };

                    let inserted_code = insertion.map_or("", |insertion| {
                        &marked_code[insertion + INSERTION.len_utf8()..end]
                    });
                    let removed_span = Range {
                        start: Index::new(start),
                        end:   Index::new(erased_finish - START.len_utf8()),
                    };
                    let change = TextChange::replace(removed_span, inserted_code.to_string());
                    Case { code, change }
                }
                _ => panic!("Invalid markdown in the marked code: {}.", marked_code),
            }
        }

        /// Code after applying the change
        fn resulting_code(&self) -> String {
            self.change.applied(&self.code)
        }

        /// Checks if the text operation described by this case keeps the node IDs intact.
        ///
        /// See `assert_same_node_ids` for details.
        fn assert_edit_keeps_main_node_ids(&self, parser: &Parser) {
            let ast1 = parser.parse_module(&self.code, default()).unwrap();
            let mut id_map = ast1.id_map();

            apply_code_change_to_id_map(&mut id_map, &self.change, &self.code);
            let code2 = self.resulting_code();

            let ast2 = parser.parse_module(&code2, id_map.clone()).unwrap();
            assert_same_node_ids(&ast1, &ast2);
        }
    }

    /// Pretty prints the code of module with a single function named `main`. The lines should
    /// contain unindented main function's block lines.
    fn to_main(lines: impl IntoIterator<Item: AsRef<str>>) -> String {
        let mut ret = "main = ".to_string();
        for line in lines {
            ret.push_str(&format!("\n    {}", line.as_ref()))
        }
        ret
    }

    /// Returns the IDs of nodes in the `main` function in their order of line appearance.
    fn main_nodes(module: &ast::known::Module) -> Vec<Uuid> {
        use double_representation::definition::*;
        use double_representation::graph::GraphInfo;
        let id = Id::new_plain_name("main");
        let definition = module::get_definition(module, &id).unwrap();
        let graph = GraphInfo::from_definition(definition);
        let nodes = graph.nodes();
        nodes.into_iter().map(|node| node.id()).collect()
    }

    /// Checks that both module AST contain `main` function that has the same sequence of node IDs,
    /// as described by the `main_nodes` function.
    fn assert_same_node_ids(ast1: &ast::known::Module, ast2: &ast::known::Module) {
        let ids1 = main_nodes(ast1);
        let ids2 = main_nodes(ast2);
        DEBUG!("IDs1: {ids1:?}");
        DEBUG!("IDs2: {ids2:?}");
        assert_eq!(ids1, ids2);
    }

    #[test]
    fn test_case_markdown() {
        let case = Case::from_markdown("foo«aa⎀bb»c");
        assert_eq!(case.code, "fooaac");
        assert_eq!(case.change.inserted, "bb");
        assert_eq!(case.change.replaced, Index::new(3)..Index::new(5));
        assert_eq!(case.resulting_code(), "foobbc");

        let case = Case::from_markdown("foo«aa»c");
        assert_eq!(case.code, "fooaac");
        assert_eq!(case.change.inserted, "");
        assert_eq!(case.change.replaced, Index::new(3)..Index::new(5));
        assert_eq!(case.resulting_code(), "fooc");
    }

    #[wasm_bindgen_test]
    fn applying_code_changes_to_id_map() {
        let parser = Parser::new_or_panic();

        // All the cases describe edit to a middle line in three line main definition.
        let cases = [
            "a = \"«⎀f»foo\"",
            "a = \"«⎀ »foo\"",
            "a = \"foo«⎀ »\"",
            "a = \"foo«⎀f»\"",
            "a = \"«f»foo\"",
            "a = \"« »foo\"",
            "a = \"foo« »\"",
            "a = \"foo«f»\"",
            "a = «f»foo",
            "a = «⎀f»foo",
            "a = «f»foo",
            "a = «⎀ »foo",
            "a = « »foo",
            "a = «⎀f» foo",
            "a = foo«⎀ »",
            "a = foo«⎀\n»",
            "a = foo «⎀\n»",
            "a = foo «⎀j»",
            "a = foo «j»",
            "a = foo«⎀j»",
            // Same as above but not in an assignment form
            "\"«⎀f»foo\"",
            "\"«⎀ »foo\"",
            "\"foo«⎀ »\"",
            "\"foo«⎀f»\"",
            "\"«f»foo\"",
            "\"« »foo\"",
            "\"foo« »\"",
            "\"foo«f»\"",
            "«⎀f»foo",
            "«f»foo",
            // Commented out tests below would fail because of leading whitespace breaking the
            // block structure.
            // "«⎀ »foo",
            // "« »foo",
            // "«⎀f» foo",
            "foo«⎀ »",
            "foo«⎀\n»",
            "foo «⎀\n»",
            "foo «⎀j»",
            "foo «j»",
            "foo«⎀j»",
        ];

        for case in cases.iter() {
            let all_nodes = ["previous", case, "next"];
            let main_def = to_main(all_nodes.iter());
            let case = Case::from_markdown(main_def);
            case.assert_edit_keeps_main_node_ids(&parser);
        }
    }
}
