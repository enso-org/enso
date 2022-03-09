//! A crate with all functions used to synchronize different representations of our language

#![feature(associated_type_bounds)]
#![feature(drain_filter)]
#![feature(iter_order_by)]
#![feature(option_result_contains)]
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unused_import_braces)]
#![warn(unused_qualifications)]
#![warn(unsafe_code)]
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]

use crate::prelude::*;

use crate::definition::DefinitionName;
use crate::definition::ScopeKind;

use ast::crumbs::InfixCrumb;
use ast::crumbs::Located;
use ast::known;
use ast::macros::DocumentationCommentAst;
use ast::opr;
use ast::prefix;
use ast::Ast;

pub mod alias_analysis;
pub mod connection;
pub mod definition;
pub mod graph;
pub mod identifier;
pub mod module;
pub mod node;
pub mod project;
pub mod refactorings;
pub mod text;
pub mod tp;

#[cfg(test)]
pub mod test_utils;



// ===============
// === Prelude ===
// ===============

/// Common types that should be visible across the whole IDE crate.
pub mod prelude {
    pub use ast::prelude::*;
    pub use enso_logger::*;
    pub use enso_prelude::*;

    #[cfg(test)]
    pub use wasm_bindgen_test::wasm_bindgen_test;
    #[cfg(test)]
    pub use wasm_bindgen_test::wasm_bindgen_test_configure;
}



// ==============
// === Consts ===
// ==============

/// Indentation value from language specification:
///
/// Indentation: Indentation is four spaces, and all tabs are converted to 4 spaces. This is not
/// configurable on purpose.
///
/// Link: https://github.com/enso-org/enso/blob/develop/doc/syntax/encoding.md
pub const INDENT: usize = 4;



// ========================
// === Discerning Lines ===
// ========================

/// What kind of node or definition a line should be treated as.
#[derive(Clone, Debug)]
pub enum LineKind {
    /// Definition is a binding, which defines a new entity with arguments.
    Definition {
        /// The binding that introduces the definition.
        ast:  known::Infix,
        /// Name of this definition. Includes typename, if this is an extension method.
        name: Located<DefinitionName>,
        /// Arguments for this definition. Does not include any implicit ones (e.g. no `this`).
        args: Vec<Located<Ast>>,
    },
    /// Node in a binding form.
    ExpressionAssignment {
        /// Ast of the whole binding.
        ast: known::Infix,
    },
    /// Node consisting of a plain expression, with no pattern binding.
    ExpressionPlain {
        /// Ast of the whole expression.
        ast: Ast,
    },
    /// Documentation comment lines are not nodes.
    /// Instead, they are discovered and processed as part of nodes that follow them.
    DocumentationComment {
        /// The comment representation.
        documentation: DocumentationCommentAst,
    },
}

impl LineKind {
    /// Tell how the given line (described by an Ast) should be treated.
    // TODO [mwu] This method deserves unit tests of its own.
    pub fn discern(ast: &Ast, kind: ScopeKind) -> Self {
        use LineKind::*;

        // First of all, if non-empty line is not an infix (i.e. binding) it can be only a node or
        // a documentation comment.
        let ast = match opr::to_assignment(ast) {
            Some(infix) => infix,
            None =>
                return if let Some(documentation) = DocumentationCommentAst::new(ast) {
                    // e.g. `## My comment.`
                    DocumentationComment { documentation }
                } else {
                    // The simplest form of node, e.g. `Point 5 10`
                    ExpressionPlain { ast: ast.clone_ref() }
                },
        };

        // Assignment can be either nodes or definitions. To discern, we check the left hand side.
        // For definition it is a prefix chain, where first is the name, then arguments (if
        // explicit). For node it is a pattern, either in a form of Var without args on Cons
        // application.
        let crumb = InfixCrumb::LeftOperand;
        let lhs = Located::new(crumb, prefix::Chain::from_ast_non_strict(&ast.larg));
        let name = lhs
            .entered(|chain| {
                let name_ast = chain.located_func();
                name_ast.map(DefinitionName::from_ast)
            })
            .into_opt();

        // If this is a pattern match, `name` will fail to construct and we'll treat line as a node.
        // e.g. for `Point x y = get_point …`
        let name = match name {
            Some(name) => name,
            None => return ExpressionAssignment { ast },
        };

        let args = lhs
            .enumerate_args()
            .map(|Located { crumbs, item }| {
                // We already in the left side of assignment, so we need to prepend this crumb.
                let crumbs = lhs.crumbs.clone().into_iter().chain(crumbs);
                let ast = item.clone();
                Located::new(crumbs, ast)
            })
            .collect_vec();

        // Note [Scope Differences]
        if kind == ScopeKind::NonRoot {
            // 1. Not an extension method but an old setter syntax. Currently not supported in the
            // language, treated as node with invalid pattern.
            // e.g. `point.x = 5`
            let is_setter = !name.extended_target.is_empty();
            // 2. No explicit args -- this is a proper node, not a definition.
            // e.g. `point = Point 5 10`
            let is_node = args.is_empty();
            if is_setter || is_node {
                return ExpressionAssignment { ast };
            }
        };

        Definition { ast, name, args }
    }
}

// Note [Scope Differences]
// ========================
// When we are in definition scope (as opposed to global scope) certain patterns should not be
// considered to be function definitions. These are:
// 1. Expressions like "Int.x = …". In module, they'd be treated as extension methods. In
//    definition scope they are treated as invalid constructs (setter syntax in the old design).
// 2. Expression like "foo = 5". In module, this is treated as method definition (with implicit
//    this parameter). In definition, this is just a node (evaluated expression).

#[cfg(test)]
mod tests {
    use super::*;

    use crate::definition::DefinitionProvider;

    use ast::macros::DocumentationCommentInfo;
    use parser::Parser;


    /// Expect `main` method, where first line is a documentation comment.
    /// The text of this comment should match the expected one.
    fn run_case(parser: &Parser, code: &str, expected_comment_text: &str) {
        let ast = parser.parse_module(code, default()).unwrap();
        let main_id = definition::Id::new_plain_name("main");
        let main = module::get_definition(&ast, &main_id).unwrap();
        let lines = main.block_lines();
        let first_line = lines[0].transpose_ref().unwrap();
        let doc = DocumentationCommentInfo::new(&first_line, main.indent()).unwrap();
        let text = doc.pretty_text();
        assert_eq!(text, expected_comment_text);

        // Now, if we convert our pretty text to code, will it be the same as original line?
        let code = DocumentationCommentInfo::text_to_repr(main.indent(), &text);
        let ast2 = parser.parse_line(&code).unwrap();
        let doc2 = DocumentationCommentInfo::new(&ast2.as_ref(), main.indent())
            .unwrap_or_else(|| panic!("Failed to parse `{code}` as comment"));
        assert_eq!(doc.line().repr(), doc2.line().repr())
    }

    #[wasm_bindgen_test]
    fn parse_single_line_comment() {
        let parser = parser::Parser::new_or_panic();

        // Typical single line case.
        let code = r#"
main =
    ## Single line
    node"#;
        let expected = " Single line";
        run_case(&parser, code, expected);

        // Single line case without space after `##`.
        let code = r#"
main =
    ##Single line
    node"#;
        let expected = "Single line";
        run_case(&parser, code, expected);

        // Single line case with a single trailing space after `##`.
        let code = r#"
main =
    ## 
    node"#;
        let expected = " ";
        run_case(&parser, code, expected);

        // Single line case without content.
        let code = r#"
main =
    ##
    node"#;
        let expected = "";
        run_case(&parser, code, expected);
    }

    #[wasm_bindgen_test]
    fn parse_multi_line_comment() {
        let parser = parser::Parser::new_or_panic();
        let code = r#"
main =
    ## First line
       Second line
    node"#;
        let expected = " First line\n Second line";
        run_case(&parser, code, expected);
    }
}
