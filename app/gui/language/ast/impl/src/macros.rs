//! Utilities for dealing with macro-related parts of AST and language, including `Match` shape and
//! such constructs as lambda expressions.

use crate::prelude::*;

use crate::crumbs::Located;
use crate::known;
use crate::BlockLine;


// ==============
// === Export ===
// ==============

pub mod skip_and_freeze;



// ==================================
// === Recognized Macros Keywords ===
// ==================================

/// The keyword introducing an qualified import declaration. See:
/// https://enso.org/docs/developer/enso/syntax/imports.html#import-syntax
pub const QUALIFIED_IMPORT_KEYWORD: &str = "import";

/// The keyword introducing an unqualified import declaration.
pub const UNQUALIFIED_IMPORT_KEYWORD: &str = "from";



// ========================
// === Disable Comments ===
// ========================

/// Check if this AST is a disabling comment.
pub fn is_disable_comment(ast: &Ast) -> bool {
    if let crate::Shape::Tree(tree) = ast.shape()
        && tree.type_info == crate::TreeType::ExpressionWithComment
        && !tree.span_info.iter().any(|e| matches!(e, crate::SpanSeed::Child(_))) {
        true
    } else {
        false
    }
}



// ==============================
// === Documentation Comments ===
// ==============================

// === Ast Description ===

/// Describes the AST of a documentation comment.
#[derive(Clone, Debug)]
pub struct DocumentationCommentAst {
    ast:      known::Tree,
    rendered: ImString,
}

impl DocumentationCommentAst {
    /// Interpret given Ast as a documentation comment. Return `None` if it is not recognized.
    pub fn new(ast: &Ast) -> Option<Self> {
        let ast = crate::known::Tree::try_from(ast).ok()?;
        if let crate::TreeType::Documentation { rendered } = &ast.type_info {
            let rendered = rendered.clone();
            Some(DocumentationCommentAst { ast, rendered })
        } else {
            None
        }
    }
}


// === Line Description ===

/// Describes the line with a documentation comment.
#[derive(Clone, Debug)]
pub struct DocumentationCommentLine {
    /// Stores the documentation AST and the trailing whitespace length.
    line:     BlockLine<known::Tree>,
    rendered: ImString,
}

impl DocumentationCommentLine {
    /// Try constructing from a line. Return `None` if this line has no documentation comment.
    pub fn new(line: &BlockLine<&Ast>) -> Option<Self> {
        let doc_ast_opt = DocumentationCommentAst::new(line.elem);
        doc_ast_opt.map(|doc_ast| Self::from_doc_ast(doc_ast, line.off))
    }

    /// Treat given documentation AST as the line with a given trailing whitespace.
    pub fn from_doc_ast(ast_doc: DocumentationCommentAst, off: usize) -> Self {
        Self { line: BlockLine { elem: ast_doc.ast, off }, rendered: ast_doc.rendered }
    }

    /// Get the line with this comment.
    fn line(&self) -> &BlockLine<known::Tree> {
        &self.line
    }

    /// Convenience function that throws away some information to return the line description that
    /// is used in AST blocks.
    fn block_line(&self) -> BlockLine<Option<Ast>> {
        self.line.as_ref().map(|known_ast| Some(known_ast.ast().clone_ref()))
    }
}


// === Full Description ===

/// Structure holding the documentation comment AST and related information necessary to deal with
/// them.
#[derive(Clone, Debug)]
pub struct DocumentationCommentInfo {
    /// Description of the line with the documentation comment.
    pub line:         DocumentationCommentLine,
    /// The absolute indent of the block that contains the line with documentation comment.
    pub block_indent: usize,
}

impl DocumentationCommentInfo {
    /// Try to obtain information about a documentation comment line from block with a given indent.
    pub fn new(line: &BlockLine<&Ast>, block_indent: usize) -> Option<Self> {
        Some(Self { line: DocumentationCommentLine::new(line)?, block_indent })
    }

    /// Get the line with this comment.
    pub fn line(&self) -> &BlockLine<known::Tree> {
        self.line.line()
    }

    /// Get the documentation comment's AST.
    pub fn ast(&self) -> known::Tree {
        self.line.line.elem.clone_ref()
    }

    /// Convenience function that throws away some information to return the line description that
    /// is used in AST blocks.
    pub fn block_line(&self) -> BlockLine<Option<Ast>> {
        self.line.block_line()
    }

    /// Get the documentation text.
    ///
    /// The text is pretty printed as per UI perspective--leading whitespace is stripped from all
    /// lines up to the column following comment introducer (`##`).
    pub fn pretty_text(&self) -> ImString {
        self.line.rendered.clone()
    }

    /// Generates the source code text of the comment line from a pretty text.
    pub fn text_to_repr(context_indent: usize, text: &str) -> String {
        let indent = " ".repeat(context_indent);
        let mut lines = text.lines();
        // First line must always exist, even for an empty comment.
        let first_line = format!("##{}", lines.next().unwrap_or_default());
        let other_lines = lines.map(|line| format!("{indent}   {line}"));
        let mut out_lines = std::iter::once(first_line).chain(other_lines);
        out_lines.join("\n")
    }
}

/// Check if given Ast stores a documentation comment.
pub fn is_documentation_comment(ast: &Ast) -> bool {
    DocumentationCommentAst::new(ast).is_some()
}



// ===============
// === Lambdas ===
// ===============

/// Describes the lambda-expression's pieces: the argument and the body.
#[allow(missing_docs)]
#[derive(Clone, Debug)]
pub struct LambdaInfo<'a> {
    pub arg:  Located<&'a Ast>,
    pub body: Located<&'a Ast>,
}

/// Describes the given Ast as lambda, if this is a matched `->` builtin macro.
pub fn as_lambda(ast: &Ast) -> Option<LambdaInfo> {
    if let crate::Shape::Tree(crate::Tree { type_info: crate::TreeType::Lambda, .. }) = ast.shape()
    {
        let mut iter = ast.iter_subcrumbs().map(|crumb| ast.get_located(crumb).unwrap());
        let arg = iter.next().unwrap();
        let body = iter.next().unwrap();
        Some(LambdaInfo { arg, body })
    } else {
        None
    }
}
