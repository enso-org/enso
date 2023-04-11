//! Code for node discovery and other node-related tasks.

use crate::prelude::*;

use crate::context_switch::ContextSwitchExpression;
use crate::definition::ScopeKind;
use crate::LineKind;

use ast::crumbs::Crumbable;
use ast::enumerate_non_empty_lines;
use ast::known;
use ast::macros::skip_and_freeze::prefix_macro_body;
use ast::macros::skip_and_freeze::prepend_with_macro;
use ast::macros::skip_and_freeze::preserving_skip;
use ast::macros::skip_and_freeze::preserving_skip_and_freeze;
use ast::macros::skip_and_freeze::without_macros;
use ast::macros::skip_and_freeze::MacrosInfo;
use ast::macros::skip_and_freeze::FREEZE_MACRO_IDENTIFIER;
use ast::macros::skip_and_freeze::SKIP_MACRO_IDENTIFIER;
use ast::macros::DocumentationCommentInfo;
use ast::macros::DocumentationCommentLine;
use ast::Ast;
use ast::BlockLine;
use std::cmp::Ordering;



/// Node Id is the AST ID attached to the node's expression.
pub type Id = ast::Id;



// =============
// === Error ===
// =============

#[allow(missing_docs)]
#[derive(Clone, Copy, Fail, Debug)]
#[fail(display = "Node with ID {} was not found.", id)]
pub struct IdNotFound {
    pub id: Id,
}

/// Indices of lines belonging to a node.
#[derive(Clone, Copy, Debug)]
pub struct NodeLocation {
    /// Documentation comment line index, if present.
    pub documentation_line: Option<usize>,
    /// Main line is a line that contains the node's expression.
    pub main_line:          usize,
}

impl PartialEq for NodeLocation {
    fn eq(&self, other: &Self) -> bool {
        self.partial_cmp(other) == Some(Ordering::Equal)
    }
}

impl PartialOrd for NodeLocation {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.main_line.partial_cmp(&other.main_line)
    }
}

impl NodeLocation {
    /// Index for the first line belonging to the node.
    pub fn first(&self) -> usize {
        self.documentation_line.unwrap_or(self.main_line)
    }

    /// Index for the last line belonging to the node.
    pub fn last(&self) -> usize {
        self.main_line
    }

    /// Inclusive range between first and last node's lines.
    ///
    /// Note that while a node can contain at most two lines, they may be interspersed by a
    /// number of blank lines.
    pub fn range(start: NodeLocation, last: NodeLocation) -> RangeInclusive<usize> {
        start.first()..=last.last()
    }
}



// ===============
// === General ===
// ===============

/// Information about the node coupled with its location within a block.
#[derive(Clone, Debug, Deref)]
pub struct LocatedNode {
    /// Line index in the block. Zero for inline definition nodes.
    pub index: NodeLocation,
    /// Information about the node.
    #[deref]
    pub node:  NodeInfo,
}

/// Tests if given line contents can be seen as node with a given id
pub fn is_main_line_of(line: &BlockLine<Option<Ast>>, id: Id) -> bool {
    let main_line = MainLine::from_block_line(line);
    main_line.contains_if(|main_line| main_line.id() == id)
}

/// Searches for `NodeInfo` with the associated `id` index in `lines`.
///
/// Returns an error if the Id is not found.
pub fn locate<'a>(
    lines: impl IntoIterator<Item = &'a BlockLine<Option<Ast>>> + 'a,
    context_indent: usize,
    id: Id,
) -> FallibleResult<LocatedNode> {
    Ok(locate_many(lines, context_indent, [id])?.remove(&id).unwrap())
}

/// Obtain located node information for multiple nodes in a single pass.
///
/// If any of the looked for nodes is not found, `Err` is returned.
/// Any `Ok(…)` return value is guaranteed to have length equal to `looked_for` argument.
pub fn locate_many<'a>(
    lines: impl IntoIterator<Item = &'a BlockLine<Option<Ast>>> + 'a,
    context_indent: usize,
    looked_for: impl IntoIterator<Item = Id>,
) -> FallibleResult<HashMap<ast::Id, LocatedNode>> {
    let mut looked_for = looked_for.into_iter().collect::<HashSet<_>>();

    let mut ret = HashMap::new();
    // Skip empty lines, there are no nodes.
    // However, indices are important.
    let lines_iter = enumerate_non_empty_lines(lines);
    let nodes = NodeIterator { lines_iter, context_indent };
    for node in nodes {
        if looked_for.remove(&node.id()) {
            ret.insert(node.id(), node);
        }

        if looked_for.is_empty() {
            break;
        }
    }

    if let Some(id) = looked_for.into_iter().next() {
        Err(IdNotFound { id }.into())
    } else {
        Ok(ret)
    }
}



// ===================
// === NodeAstInfo ===
// ===================

/// The information about AST content of the node.
#[derive(Debug, Clone, PartialEq, Default)]
pub struct NodeAstInfo {
    /// Information about SKIP and FREEZE macros used in the code.
    pub macros_info:    MacrosInfo,
    /// Existing context switch expression, if any.
    pub context_switch: Option<ContextSwitchExpression>,
}

impl NodeAstInfo {
    /// Constructor.
    pub fn from_ast(ast: &Ast) -> Self {
        let macros_info = MacrosInfo::from_ast(ast);
        let without_macros = without_macros(ast);
        let context_switch = ContextSwitchExpression::parse(&without_macros);
        Self { macros_info, context_switch }
    }

    /// Specifies the count of AST crumbs to bypass in order to reach the displayed AST node.
    pub fn ast_crumbs_to_skip(&self) -> usize {
        let skip_for_context_switch_expr = self.context_switch.as_ref().map_or(0, |_| 1);
        let skip_for_macros = self.macros_info.macros_count();
        skip_for_macros + skip_for_context_switch_expr
    }
}



// ================
// === MainLine ===
// ================

/// Representation of the main line of the node (as opposed to a documentation line).
///
/// Each node must have exactly one main line.
/// Main line always contains an expression, either directly or under binding. The expression ID
/// must be set and it serves as the whole node's expression.
#[derive(Debug, Clone, Deref, DerefMut)]
pub struct MainLine {
    /// Node AST, contains a node's expression and an optional pattern binding.
    #[deref]
    #[deref_mut]
    pub ast:      NodeAst,
    /// Additional information about the AST.
    pub ast_info: NodeAstInfo,
}

impl MainLine {
    /// Tries to interpret the whole binding as a node. Right-hand side will become node's
    /// expression.
    pub fn new_binding(infix: known::Infix) -> Option<MainLine> {
        infix.rarg.id?;
        let ast_info = NodeAstInfo::from_ast(&infix.rarg);
        let ast = NodeAst::Binding { infix };
        Some(Self { ast, ast_info })
    }

    /// Tries to interpret AST as node, treating whole AST as an expression.
    pub fn new_expression(ast: Ast) -> Option<MainLine> {
        ast.id?;
        let ast_info = NodeAstInfo::from_ast(&ast);
        let ast = NodeAst::Expression { ast };
        // TODO what if we are given an assignment.
        Some(Self { ast, ast_info })
    }

    /// Tries to interpret AST as node, treating whole AST as a node's primary line.
    pub fn from_ast(ast: &Ast) -> Option<MainLine> {
        // By definition, there are no nodes in the root scope.
        // Being a node's line, we may assume that this is not a root scope.
        let scope = ScopeKind::NonRoot;
        Self::from_discerned_line(LineKind::discern(ast, scope))
    }

    /// Try retrieving node information from an already discerned line data.
    pub fn from_discerned_line(line: LineKind) -> Option<MainLine> {
        match line {
            LineKind::ExpressionPlain { ast } => Self::new_expression(ast),
            LineKind::ExpressionAssignment { ast } => Self::new_binding(ast),
            LineKind::Definition { .. } => None,
            LineKind::DocumentationComment { .. } => None,
        }
    }

    /// Tries to interpret AST as node, treating whole AST as an expression.
    pub fn from_block_line(line: &BlockLine<Option<Ast>>) -> Option<MainLine> {
        Self::from_ast(line.elem.as_ref()?)
    }
}



// ================
// === NodeInfo ===
// ================

/// Iterator over indexed line ASTs that yields nodes.
#[derive(Clone, Debug)]
pub struct NodeIterator<'a, T: Iterator<Item = (usize, BlockLine<&'a Ast>)> + 'a> {
    /// Input iterator that yields pairs (line index, line's Ast).
    pub lines_iter:     T,
    /// Absolute indent of lines in the block we are iterating over.
    pub context_indent: usize,
}

impl<'a, T: Iterator<Item = (usize, BlockLine<&'a Ast>)> + 'a> Iterator for NodeIterator<'a, T> {
    type Item = LocatedNode;

    fn next(&mut self) -> Option<Self::Item> {
        let mut indexed_documentation = None;
        for (index, line) in &mut self.lines_iter {
            match LineKind::discern(line.elem, ScopeKind::NonRoot) {
                LineKind::DocumentationComment { documentation } => {
                    let doc_line = DocumentationCommentLine::from_doc_ast(documentation, line.off);
                    let documentation = DocumentationCommentInfo {
                        line:         doc_line,
                        block_indent: self.context_indent,
                    };
                    indexed_documentation = Some((index, documentation));
                }
                LineKind::Definition { .. } => {
                    // Non-node entity consumes any previous documentation.
                    indexed_documentation = None;
                }
                line =>
                    if let Some(main_line) = MainLine::from_discerned_line(line) {
                        let (documentation_line, documentation) = match indexed_documentation {
                            Some((index, documentation)) => (Some(index), Some(documentation)),
                            None => (None, None),
                        };

                        let node = NodeInfo { documentation, main_line };
                        let index = NodeLocation { main_line: index, documentation_line };

                        return Some(LocatedNode { index, node });
                    },
            }
        }
        None
    }
}

/// Information about node, including both its main line (i.e. line with expression) and optionally
/// attached documentation comment.
#[derive(Clone, Debug, Deref, DerefMut)]
pub struct NodeInfo {
    /// If the node has doc comment attached, it will be represented here.
    pub documentation: Option<DocumentationCommentInfo>,
    /// Primary node AST that contains node's expression and optional pattern binding.
    #[deref]
    #[deref_mut]
    pub main_line:     MainLine,
}

impl NodeInfo {
    /// Check if a given non-empty line's AST belongs to this node.
    pub fn contains_line(&self, line_ast: &Ast) -> bool {
        // TODO refactor these two lambdas into methods
        let expression_id_matches =
            || MainLine::from_ast(line_ast).as_ref().map(|ml| ml.id()).contains(&self.id());
        let doc_comment_id_matches = || match (self.doc_comment_id(), line_ast.id) {
            (Some(node_doc_id), Some(line_ast_id)) => node_doc_id == line_ast_id,
            _ => false,
        };
        expression_id_matches() || doc_comment_id_matches()
    }

    /// Get the AST ID of the line with the node comment (if present).
    pub fn doc_comment_id(&self) -> Option<ast::Id> {
        self.documentation.as_ref().and_then(|comment| comment.ast().id())
    }

    /// Construct node information for a single line, without documentation.
    pub fn from_main_line_ast(ast: &Ast) -> Option<Self> {
        let main_line = MainLine::from_ast(ast)?;
        let documentation = None;
        Some(Self { documentation, main_line })
    }

    /// Obtain documentation text.
    pub fn documentation_text(&self) -> Option<ImString> {
        self.documentation.as_ref().map(|doc| doc.pretty_text())
    }

    /// The info about macro calls in the expression.
    pub fn macros_info(&self) -> &MacrosInfo {
        &self.main_line.ast_info.macros_info
    }

    // Modify AST, adding or removing `SKIP` macro call. Does nothing if [`skip`] argument already
    /// matches the inner state.
    pub fn set_skip(&mut self, skip: bool) {
        if skip != self.macros_info().skip {
            if skip {
                self.main_line.add_skip_macro();
            } else {
                self.main_line.remove_skip_macro();
            }
            self.main_line.ast_info.macros_info.skip = skip;
        }
    }

    /// Modify AST, adding or removing `FREEZE` macro call. Does nothing if [`skip`] argument
    /// already matches the inner state.
    pub fn set_freeze(&mut self, freeze: bool) {
        if freeze != self.macros_info().freeze {
            if freeze {
                self.main_line.add_freeze_macro();
            } else {
                self.main_line.remove_freeze_macro();
            }
            self.main_line.ast_info.macros_info.freeze = freeze;
        }
    }

    /// Clear the pattern (left side of assignment) for node.
    ///
    /// If it is already an Expression node, no change is done.
    pub fn clear_pattern(&mut self) {
        self.main_line.clear_pattern();
    }

    /// Add context switch expression to the node. Replaces the existing one, if any.
    pub fn set_context_switch(&mut self, context_switch_expr: ContextSwitchExpression) {
        if self.main_line.ast_info.context_switch.is_some() {
            self.clear_context_switch_expression();
        }
        self.main_line.modify_expression(|ast| {
            *ast = preserving_skip_and_freeze(ast, |ast| {
                let prefix = context_switch_expr.to_ast();
                let infix = ast::Infix {
                    larg: prefix,
                    loff: 1,
                    opr:  ast::opr::right_assoc().into(),
                    roff: 1,
                    rarg: ast.clone(),
                };
                *ast = infix.into();
            });
        });
        self.main_line.ast_info.context_switch = Some(context_switch_expr);
    }

    /// Remove existing context switch expression from the node.
    pub fn clear_context_switch_expression(&mut self) {
        if self.main_line.ast_info.context_switch.is_some() {
            self.main_line.modify_expression(|ast| {
                *ast = preserving_skip_and_freeze(ast, |ast| {
                    if ContextSwitchExpression::parse(ast).is_some() {
                        let crumb = ast::crumbs::InfixCrumb::RightOperand.into();
                        let rarg = ast.get(&crumb).unwrap_or(ast);
                        *ast = rarg.clone();
                    }
                });
            });
            self.main_line.ast_info.context_switch = None;
        }
    }
}

/// AST of a single node.
#[derive(Clone, Debug)]
#[allow(missing_docs)]
pub enum NodeAst {
    /// Code with assignment, e.g. `foo = 2 + 2`
    Binding { infix: known::Infix },
    /// Code without assignment (no variable binding), e.g. `2 + 2`.
    Expression { ast: Ast },
}

impl NodeAst {
    /// Node's unique ID.
    pub fn id(&self) -> Id {
        // Panic must not happen, as the only available constructors checks that
        // there is an ID present.
        self.whole_expression().id.expect("Node AST must bear an ID")
    }

    /// Updates the node's AST so the node bears the given ID.
    pub fn set_id(&mut self, new_id: Id) {
        match self {
            NodeAst::Binding { ref mut infix, .. } => {
                let new_rarg = infix.rarg.with_id(new_id);
                let set = infix.set(&ast::crumbs::InfixCrumb::RightOperand.into(), new_rarg);
                *infix = set.expect(
                    "Internal error: setting infix operand should always \
                                     succeed.",
                );
            }
            NodeAst::Expression { ref mut ast, .. } => {
                *ast = ast.with_id(new_id);
            }
        };
    }

    /// Represents the visible portion of a node's expression. This excludes SKIP and FREEZE macro
    /// calls, as well as any context switch expressions.
    pub fn expression(&self) -> Ast {
        let ast = without_macros(self.whole_expression());
        ContextSwitchExpression::without_expression(&ast)
    }

    /// AST of the node's expression. Typically no external user wants to access it directly. Use
    /// [`Self::expression`] instead.
    fn whole_expression(&self) -> &Ast {
        match self {
            NodeAst::Binding { infix, .. } => &infix.rarg,
            NodeAst::Expression { ast, .. } => ast,
        }
    }

    /// AST of the node's pattern (assignment's left-hand side).
    pub fn pattern(&self) -> Option<&Ast> {
        match self {
            NodeAst::Binding { infix, .. } => Some(&infix.larg),
            NodeAst::Expression { .. } => None,
        }
    }

    /// Set AST of the node's expression. Maintains ID.
    pub fn set_expression(&mut self, expression: Ast) {
        self.modify_expression(move |ast| {
            *ast = preserving_skip_and_freeze(ast, |ast| *ast = expression.clone());
        });
    }

    /// The whole AST of node.
    pub fn ast(&self) -> &Ast {
        match self {
            NodeAst::Binding { infix, .. } => infix.into(),
            NodeAst::Expression { ast, .. } => ast,
        }
    }

    /// Set the pattern (left side of assignment) for node. If it is an Expression node, the
    /// assignment infix will be introduced.
    pub fn set_pattern(&mut self, pattern: Ast) {
        match self {
            NodeAst::Binding { infix, .. } => {
                // Setting infix operand never fails.
                infix.update_shape(|infix| infix.larg = pattern);
            }
            NodeAst::Expression { ast, .. } => {
                let infix = ast::Infix {
                    larg: pattern,
                    loff: 1,
                    opr:  Ast::opr("="),
                    roff: 1,
                    rarg: ast.clone(),
                };
                let infix = known::Infix::new(infix, None);
                *self = NodeAst::Binding { infix };
            }
        }
    }

    /// Modify expression, preserving the AST ID.
    fn modify_expression(&mut self, f: impl FnOnce(&mut Ast)) {
        let id = self.id();
        match self {
            Self::Binding { infix, .. } => {
                infix.update_shape(|infix| f(&mut infix.rarg));
            }
            Self::Expression { ast, .. } => f(ast),
        }
        self.set_id(id);
    }

    /// Add [`SKIP`] macro call to the AST. Preserves the expression ID and [`FREEZE`] macro calls.
    fn add_skip_macro(&mut self) {
        self.modify_expression(|ast| {
            prepend_with_macro(ast, SKIP_MACRO_IDENTIFIER);
        });
    }

    /// Remove [`SKIP`] macro call from the AST. Preserves the expression ID and [`FREEZE`] macro
    /// calls.
    fn remove_skip_macro(&mut self) {
        self.modify_expression(|ast| {
            *ast = prefix_macro_body(ast);
        });
    }

    /// Add [`FREEZE`] macro call to the AST. Preserves the expression ID and [`SKIP`] macro calls.
    fn add_freeze_macro(&mut self) {
        self.modify_expression(|ast| {
            *ast = preserving_skip(ast, |ast| prepend_with_macro(ast, FREEZE_MACRO_IDENTIFIER));
        });
    }

    /// Remove [`FREEZE`] macro call from the AST. Preserves the expression ID and [`SKIP`] macro
    /// calls.
    fn remove_freeze_macro(&mut self) {
        self.modify_expression(|ast| {
            *ast = preserving_skip(ast, |ast| {
                *ast = prefix_macro_body(ast);
            });
        });
    }

    /// See [`NodeInfo::clear_pattern`]. Preserves the [`MacrosInfo`].
    pub fn clear_pattern(&mut self) {
        match self {
            NodeAst::Binding { infix } =>
                *self = NodeAst::Expression { ast: infix.rarg.clone_ref() },
            NodeAst::Expression { .. } => {}
        }
    }
}

impl ast::HasTokens for NodeAst {
    fn feed_to(&self, consumer: &mut impl ast::TokenConsumer) {
        self.ast().feed_to(consumer)
    }
}



// =============
// === Tests ===
// =============

#[cfg(test)]
mod tests {
    use crate::context_switch::Context;
    use crate::context_switch::ContextSwitch;

    use super::*;

    use ast::opr::predefined::ASSIGNMENT;

    const ENABLE_CONTEXT: &str = "Standard.Base.Runtime.with_enabled_context";
    const DISABLE_CONTEXT: &str = "Standard.Base.Runtime.with_disabled_context";
    const OUTPUT_CONTEXT: &str = "Standard.Base.Runtime.Context.Output";

    fn expect_node(ast: Ast, expression_text: &str, id: Id) {
        let node_info = NodeInfo::from_main_line_ast(&ast).expect("expected a node");
        assert_eq!(node_info.expression().repr(), expression_text);
        assert_eq!(node_info.id(), id);
    }

    #[test]
    fn expression_node_test() {
        // expression: `4`
        let id = Id::new_v4();
        let ast = Ast::new(ast::Number { base: None, int: "4".into() }, Some(id));
        expect_node(ast, "4", id);
    }

    #[test]
    fn binding_node_test() {
        // expression: `foo = 4`
        let id = Id::new_v4();
        let number = ast::Number { base: None, int: "4".into() };
        let larg = Ast::var("foo");
        let rarg = Ast::new(number, Some(id));
        let ast = Ast::infix(larg, ASSIGNMENT, rarg);
        expect_node(ast, "4", id);
    }

    #[test]
    fn set_expression_binding() {
        let ast = Ast::infix(Ast::var("foo"), "=", Ast::number(4).with_new_id());
        assert_eq!(ast.repr(), "foo = 4");

        let mut node = NodeInfo::from_main_line_ast(&ast).expect("expected a node");
        let id = node.id();
        node.set_expression(Ast::var("bar"));
        assert_eq!(node.expression().repr(), "bar");
        assert_eq!(node.ast().repr(), "foo = bar");
        assert_eq!(node.id(), id);
    }

    #[test]
    fn set_expression_plain() {
        let ast = Ast::number(4).with_new_id();
        assert_eq!(ast.repr(), "4");

        let mut node = NodeInfo::from_main_line_ast(&ast).expect("expected a node");
        let id = node.id();
        node.set_expression(Ast::var("bar"));
        assert_eq!(node.expression().repr(), "bar");
        assert_eq!(node.ast().repr(), "bar");
        assert_eq!(node.id(), id);
    }

    #[test]
    fn clearing_pattern_test() {
        // expression: `foo = 4`
        let id = Id::new_v4();
        let number = ast::Number { base: None, int: "4".into() };
        let larg = Ast::var("foo");
        let rarg = Ast::new(number, Some(id));
        let ast = Ast::infix(larg, ASSIGNMENT, rarg);

        let mut node = NodeInfo::from_main_line_ast(&ast).unwrap();
        assert_eq!(node.repr(), "foo = 4");
        assert_eq!(node.id(), id);
        node.clear_pattern();
        assert_eq!(node.repr(), "4");
        assert_eq!(node.id(), id);
        node.clear_pattern();
        assert_eq!(node.repr(), "4");
        assert_eq!(node.id(), id);
    }

    #[test]
    fn setting_pattern_on_expression_node_test() {
        let id = uuid::Uuid::new_v4();
        let line_ast = Ast::number(2).with_id(id);
        let mut node = NodeInfo::from_main_line_ast(&line_ast).unwrap();
        assert_eq!(node.repr(), "2");
        assert_eq!(node.id(), id);

        node.set_pattern(Ast::var("foo"));

        assert_eq!(node.repr(), "foo = 2");
        assert_eq!(node.id(), id);
    }

    #[test]
    fn setting_pattern_on_binding_node_test() {
        let id = uuid::Uuid::new_v4();
        let larg = Ast::var("foo");
        let rarg = Ast::var("bar").with_id(id);
        let line_ast = Ast::infix(larg, ASSIGNMENT, rarg);
        let mut node = NodeInfo::from_main_line_ast(&line_ast).unwrap();

        assert_eq!(node.repr(), "foo = bar");
        assert_eq!(node.id(), id);

        node.set_pattern(Ast::var("baz"));

        assert_eq!(node.repr(), "baz = bar");
        assert_eq!(node.id(), id);
    }

    #[test]
    fn adding_skip_macro_test() {
        let id = uuid::Uuid::new_v4();
        let larg = Ast::var("foo");
        let rarg = Ast::var("bar").with_id(id);
        let line_ast = Ast::infix(larg, ASSIGNMENT, rarg);
        let mut node = NodeInfo::from_main_line_ast(&line_ast).unwrap();

        assert_eq!(node.repr(), "foo = bar");
        assert_eq!(node.id(), id);

        node.set_skip(true);

        assert_eq!(node.repr(), format!("foo = {SKIP_MACRO_IDENTIFIER} bar"));
        assert_eq!(node.id(), id);

        node.set_skip(false);

        assert_eq!(node.repr(), format!("foo = bar"));
        assert_eq!(node.id(), id);
    }

    #[test]
    fn adding_freeze_macro_test() {
        let id = uuid::Uuid::new_v4();
        let larg = Ast::var("foo");
        let rarg = Ast::var("bar").with_id(id);
        let line_ast = Ast::infix(larg, ASSIGNMENT, rarg);
        let mut node = NodeInfo::from_main_line_ast(&line_ast).unwrap();

        assert_eq!(node.repr(), "foo = bar");
        assert_eq!(node.id(), id);

        node.set_freeze(true);

        assert_eq!(node.repr(), format!("foo = {FREEZE_MACRO_IDENTIFIER} bar"));
        assert_eq!(node.id(), id);

        node.set_freeze(false);

        assert_eq!(node.repr(), format!("foo = bar"));
        assert_eq!(node.id(), id);
    }

    #[test]
    fn mixing_skip_and_freeze_macros_test() {
        let id = uuid::Uuid::new_v4();
        let larg = Ast::var("foo");
        let rarg = Ast::var("bar").with_id(id);
        let line_ast = Ast::infix(larg, ASSIGNMENT, rarg);
        let mut node = NodeInfo::from_main_line_ast(&line_ast).unwrap();

        assert_eq!(node.repr(), "foo = bar");
        node.set_skip(true);
        assert_eq!(node.repr(), format!("foo = {SKIP_MACRO_IDENTIFIER} bar"));

        node.set_freeze(true);
        assert_eq!(
            node.repr(),
            format!("foo = {SKIP_MACRO_IDENTIFIER} {FREEZE_MACRO_IDENTIFIER} bar")
        );

        node.set_freeze(false);
        assert_eq!(node.repr(), format!("foo = {SKIP_MACRO_IDENTIFIER} bar"));

        node.set_skip(false);
        assert_eq!(node.repr(), format!("foo = bar"));
        assert_eq!(node.id(), id);
    }

    #[test]
    fn execution_context_switch_expressions() {
        let node = || {
            let id = uuid::Uuid::new_v4();
            let larg = Ast::var("foo");
            let rarg = Ast::var("bar").with_id(id);
            let line_ast = Ast::infix(larg, ASSIGNMENT, rarg);
            let node = NodeInfo::from_main_line_ast(&line_ast).unwrap();
            assert_eq!(node.repr(), "foo = bar");
            assert!(node.main_line.ast_info.context_switch.is_none());
            node
        };
        fn test_round_trip(
            mut node: NodeInfo,
            expected: &str,
            context_switch: ContextSwitchExpression,
        ) {
            let original_repr = node.repr();
            node.set_context_switch(context_switch.clone());
            assert_eq!(node.main_line.ast_info.context_switch, Some(context_switch));
            assert_eq!(node.repr(), expected);
            node.clear_context_switch_expression();
            assert_eq!(node.repr(), original_repr);
            assert!(node.main_line.ast_info.context_switch.is_none());
        }

        let expected = format!("foo = {ENABLE_CONTEXT} {OUTPUT_CONTEXT} \"design\" <| bar");
        test_round_trip(node(), &expected, ContextSwitchExpression {
            switch:      ContextSwitch::Enable,
            context:     Context::Output,
            environment: "design".into(),
        });
        let expected = format!("foo = {DISABLE_CONTEXT} {OUTPUT_CONTEXT} \"design\" <| bar");
        test_round_trip(node(), &expected, ContextSwitchExpression {
            switch:      ContextSwitch::Disable,
            context:     Context::Output,
            environment: "design".into(),
        });
        let expected = format!("foo = {ENABLE_CONTEXT} {OUTPUT_CONTEXT} \"env\" <| bar");
        test_round_trip(node(), &expected, ContextSwitchExpression {
            switch:      ContextSwitch::Enable,
            context:     Context::Output,
            environment: "env".into(),
        });
    }

    #[test]
    fn mixing_skip_and_freeze_and_context_switch() {
        // TODO: Doesn't work because of the broken parsing for SKIP and FREEZE.
        // See https://github.com/enso-org/enso/issues/5572
        //
        // let parser = Parser::new();
        // let line = format!("foo = {SKIP_MACRO_IDENTIFIER} {FREEZE_MACRO_IDENTIFIER}
        // {ENABLE_CONTEXT} {OUTPUT_CONTEXT} \"env\" <| bar")
        // let ast = parser.parse_line_ast(line).unwrap();
        // let node = NodeInfo::from_main_line_ast(&ast).unwrap();
        // assert!(node.ast_info. context_switch.is_some());
        // assert!(node.ast_info.macros_info.skip);
        // assert!(node.ast_info.macros_info.freeze);


        let foo_bar = || {
            let id = uuid::Uuid::new_v4();
            let larg = Ast::var("foo");
            let rarg = Ast::var("bar").with_id(id);
            let line_ast = Ast::infix(larg, ASSIGNMENT, rarg);
            NodeInfo::from_main_line_ast(&line_ast).unwrap()
        };
        let context_switch = || format!("{ENABLE_CONTEXT} {OUTPUT_CONTEXT} \"env\"");

        #[derive(Debug)]
        struct Case {
            skip:   bool,
            freeze: bool,
        }

        #[rustfmt::skip]
        let cases =
            vec![
                Case { skip: true, freeze: false },
                Case { skip: false, freeze: true },
                Case { skip: true, freeze: true },
            ];

        for case in cases {
            let mut node = foo_bar();
            node.set_skip(case.skip);
            node.set_freeze(case.freeze);
            node.set_context_switch(ContextSwitchExpression {
                switch:      ContextSwitch::Enable,
                context:     Context::Output,
                environment: "env".into(),
            });

            let expected = format!(
                "foo = {skip}{space}{freeze} {context_switch} <| bar",
                space = if case.skip && case.freeze { " " } else { "" },
                skip = if case.skip { SKIP_MACRO_IDENTIFIER } else { "" },
                freeze = if case.freeze { FREEZE_MACRO_IDENTIFIER } else { "" },
                context_switch = context_switch(),
            );
            assert_eq!(node.repr(), expected, "{case:?}");
            assert!(node.main_line.ast_info.context_switch.is_some());

            node.clear_context_switch_expression();
            assert!(node.main_line.ast_info.context_switch.is_none());
            let expected = format!(
                "foo = {skip}{space}{freeze} bar",
                space = if case.skip && case.freeze { " " } else { "" },
                skip = if case.skip { SKIP_MACRO_IDENTIFIER } else { "" },
                freeze = if case.freeze { FREEZE_MACRO_IDENTIFIER } else { "" },
            );
            assert_eq!(node.repr(), expected, "{case:?}");
        }
    }
}
