//! Utilities for dealing with macro-related parts of AST and language, including `Match` shape and
//! such constructs as lambda expressions.

use crate::prelude::*;

use crate::crumbs::AmbiguousCrumb;
use crate::crumbs::Located;
use crate::crumbs::MatchCrumb;
use crate::known;
use crate::BlockLine;
use crate::Shifted;


// ==============
// === Export ===
// ==============

pub mod skip_and_freeze;



// ==================================
// === Recognized Macros Keywords ===
// ==================================

/// The keyword introducing a disabled code line.
pub const DISABLING_COMMENT_INTRODUCER: &str = "#";

/// The keyword introducing a documentation block.
pub const DOCUMENTATION_COMMENT_INTRODUCER: &str = "##";

/// The keyword introducing an qualified import declaration. See:
/// https://dev.enso.org/docs/enso/syntax/imports.html#import-syntax
pub const QUALIFIED_IMPORT_KEYWORD: &str = "import";

/// The keyword introducing an unqualified import declaration.
pub const UNQUALIFIED_IMPORT_KEYWORD: &str = "from";

/// The keyword introducing an unqualified export declaration.
pub const QUALIFIED_EXPORT_KEYWORD: &str = "export";



// ========================
// === Disable Comments ===
// ========================

/// Try Interpreting the line as disabling comment. Return the text after `#`.
pub fn as_disable_comment(ast: &Ast) -> Option<String> {
    let r#match = crate::known::Match::try_from(ast).ok()?;
    let first_segment = &r#match.segs.head;
    if crate::identifier::name(&first_segment.head) == Some(DISABLING_COMMENT_INTRODUCER) {
        Some(first_segment.body.repr())
    } else {
        None
    }
}

/// Check if this AST is a disabling comment.
pub fn is_disable_comment(ast: &Ast) -> bool {
    as_disable_comment(ast).is_some()
}



// ==============================
// === Documentation Comments ===
// ==============================

// === Ast Description ===

/// Describes the AST of a documentation comment.
#[derive(Clone, Debug)]
pub struct DocumentationCommentAst {
    ast:  known::Match,
    body: crate::MacroPatternMatch<Shifted<Ast>>,
}

impl DocumentationCommentAst {
    /// Interpret given Ast as a documentation comment. Return `None` if it is not recognized.
    pub fn new(ast: &Ast) -> Option<Self> {
        let ast = crate::known::Match::try_from(ast).ok()?;
        let first_segment = &ast.segs.head;
        let introducer = crate::identifier::name(&first_segment.head)?;
        if introducer == DOCUMENTATION_COMMENT_INTRODUCER {
            let body = first_segment.body.clone_ref();
            Some(DocumentationCommentAst { ast, body })
        } else {
            None
        }
    }

    /// Get the documentation comment's AST.
    pub fn ast(&self) -> known::Match {
        self.ast.clone_ref()
    }
}


// === Line Description ===

/// Describes the line with a documentation comment.
#[derive(Clone, Debug, Shrinkwrap)]
pub struct DocumentationCommentLine {
    /// Stores the documentation AST and the trailing whitespace length.
    #[shrinkwrap(main_field)]
    line: BlockLine<known::Match>,
    body: crate::MacroPatternMatch<Shifted<Ast>>,
}

impl DocumentationCommentLine {
    /// Try constructing from a line. Return `None` if this line has no documentation comment.
    pub fn new(line: &BlockLine<&Ast>) -> Option<Self> {
        let doc_ast_opt = DocumentationCommentAst::new(line.elem);
        doc_ast_opt.map(|doc_ast| Self::from_doc_ast(doc_ast, line.off))
    }

    /// Treat given documentation AST as the line with a given trailing whitespace.
    pub fn from_doc_ast(ast_doc: DocumentationCommentAst, off: usize) -> Self {
        Self { line: BlockLine { elem: ast_doc.ast, off }, body: ast_doc.body }
    }

    /// Get the documentation comment's AST.
    pub fn ast(&self) -> known::Match {
        self.line.elem.clone_ref()
    }

    /// Get the line with this comment.
    pub fn line(&self) -> &BlockLine<known::Match> {
        &self.line
    }

    /// Convenience function that throws away some information to return the line description that
    /// is used in AST blocks.
    pub fn block_line(&self) -> BlockLine<Option<Ast>> {
        self.line.as_ref().map(|known_ast| Some(known_ast.ast().clone_ref()))
    }
}


// === Full Description ===

/// Structure holding the documentation comment AST and related information necessary to deal with
/// them.
#[derive(Clone, Debug, Shrinkwrap)]
pub struct DocumentationCommentInfo {
    /// Description of the line with the documentation comment.
    #[shrinkwrap(main_field)]
    pub line:         DocumentationCommentLine,
    /// The absolute indent of the block that contains the line with documentation comment.
    pub block_indent: usize,
}

impl DocumentationCommentInfo {
    /// Try to obtain information about a documentation comment line from block with a given indent.
    pub fn new(line: &BlockLine<&Ast>, block_indent: usize) -> Option<Self> {
        Some(Self { line: DocumentationCommentLine::new(line)?, block_indent })
    }

    /// Get the documentation text.
    ///
    /// The text is pretty printed as per UI perspective -- all lines leading whitespace is stripped
    /// up to the column following comment introducer (`##`).
    pub fn pretty_text(&self) -> String {
        let mut repr = self.body.repr();
        // Trailing whitespace must be maintained.
        repr.extend(std::iter::repeat(' ').take(self.line.off));
        let indent = self.block_indent + DOCUMENTATION_COMMENT_INTRODUCER.len();
        let old = format!("\n{}", " ".repeat(indent));
        let new = "\n";
        repr.replace(&old, new)
    }

    /// Generates the source code text of the comment line from a pretty text.
    pub fn text_to_repr(context_indent: usize, text: &str) -> String {
        let indent = " ".repeat(context_indent);
        let mut lines = text.lines();
        // First line must always exist, even for an empty comment.
        let first_line = format!("##{}", lines.next().unwrap_or_default());
        let other_lines = lines.map(|line| iformat!("{indent}  {line}"));
        let mut out_lines = std::iter::once(first_line).chain(other_lines);
        out_lines.join("\n")
    }
}


impl AsRef<Ast> for DocumentationCommentInfo {
    fn as_ref(&self) -> &Ast {
        self.line.elem.ast()
    }
}

impl Display for DocumentationCommentInfo {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.pretty_text())
    }
}

/// Check if given Ast stores a documentation comment.
pub fn is_documentation_comment(ast: &Ast) -> bool {
    DocumentationCommentAst::new(ast).is_some()
}



// ===============
// === Imports ===
// ===============

/// If the given AST node is an import declaration, returns it as a Match (which is the only shape
/// capable of storing import declarations). Returns `None` otherwise.
pub fn ast_as_import_match(ast: &Ast) -> Option<known::Match> {
    let macro_match = known::Match::try_from(ast).ok()?;
    is_match_import(&macro_match).then_some(macro_match)
}

/// If the given AST node is a qualified import declaration (`import <module name>`), returns it as
/// a Match (which is the only shape capable of storing import declarations). Returns `None`
/// otherwise.
pub fn is_match_qualified_import(ast: &known::Match) -> bool {
    let segment = &ast.segs.head;
    let keyword = crate::identifier::name(&segment.head);
    keyword.contains_if(|str| *str == QUALIFIED_IMPORT_KEYWORD)
}

/// If the given AST node is an unqualified import declaration (`from <module name> import <...>`),
/// returns it as a Match (which is the only shape capable of storing import declarations). Returns
/// `None` otherwise.
pub fn is_match_unqualified_import(ast: &known::Match) -> bool {
    let first_segment = &ast.segs.head;
    let first_keyword = crate::identifier::name(&first_segment.head);
    let second_segment = &ast.segs.tail.first();
    let second_keyword = second_segment.and_then(|s| crate::identifier::name(&s.head));
    first_keyword == Some(UNQUALIFIED_IMPORT_KEYWORD)
        && second_keyword == Some(QUALIFIED_IMPORT_KEYWORD)
}

/// Check if the given macro match node is an import declaration.
pub fn is_match_import(ast: &known::Match) -> bool {
    is_match_qualified_import(ast) || is_match_unqualified_import(ast)
}

/// Check if the given ast node is an import declaration.
pub fn is_ast_import(ast: &Ast) -> bool {
    ast_as_import_match(ast).is_some()
}



// ===============
// === Lambdas ===
// ===============

/// Describes the lambda-expression's three pieces: the argument, the arrow operator and the body.
#[allow(missing_docs)]
#[derive(Clone, Debug)]
pub struct LambdaInfo<'a> {
    pub arg:  Located<&'a Ast>,
    pub opr:  Located<&'a Ast>,
    pub body: Located<&'a Ast>,
}

/// If this is the builtin macro for `->` (lambda expression), returns it as known `Match`.
pub fn as_lambda_match(ast: &Ast) -> Option<known::Match> {
    let macro_match = known::Match::try_from(ast).ok()?;
    let segment = &macro_match.segs.head;
    crate::opr::is_arrow_opr(&segment.head).then_some(macro_match)
}

/// Describes the given Ast as lambda, if this is a matched `->` builtin macro.
pub fn as_lambda(ast: &Ast) -> Option<LambdaInfo> {
    let _ = as_lambda_match(ast)?;
    let mut child_iter = ast.iter_subcrumbs();
    let arg = ast.get_located(child_iter.next()?).ok()?;
    let opr = ast.get_located(child_iter.next()?).ok()?;
    let body = ast.get_located(child_iter.next()?).ok()?;
    let is_arrow = crate::opr::is_arrow_opr(opr.item);
    is_arrow.then_some(LambdaInfo { arg, opr, body })
}



// ===================
// === Match Utils ===
// ===================

impl crate::Match<Ast> {
    /// Iterates matched ASTs. Skips segment heads ("keywords").
    /// For example, for `(a)` it iterates only over `a`, skkipping segment heads `(` and `)`.
    pub fn iter_pat_match_subcrumbs(&self) -> impl Iterator<Item = MatchCrumb> + '_ {
        self.iter_subcrumbs().filter(|crumb| {
            use crate::crumbs::SegmentMatchCrumb;
            match crumb {
                MatchCrumb::Segs { val, .. } => val != &SegmentMatchCrumb::Head,
                _ => true,
            }
        })
    }
}



// =======================
// === Ambiguous Utils ===
// =======================

impl crate::Ambiguous<Ast> {
    /// Iterates matched ASTs. Skips segment heads ("keywords").
    /// For example, for `(a)` it iterates only over `a`, skkipping segment heads `(` and `)`.
    pub fn iter_pat_match_subcrumbs(&self) -> impl Iterator<Item = AmbiguousCrumb> + '_ {
        self.iter_subcrumbs()
            .filter(|crumb| crumb.field != crate::crumbs::AmbiguousSegmentCrumb::Head)
    }
}
