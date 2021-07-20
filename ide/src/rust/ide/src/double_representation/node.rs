//! Code for node discovery and other node-related tasks.

use crate::prelude::*;

use ast::Ast;
use ast::crumbs::Crumbable;
use ast::known;
/// Node Id is the Ast Id attached to the node's expression.
pub type Id = ast::Id;



// =============
// === Error ===
// =============

#[allow(missing_docs)]
#[derive(Clone,Copy,Fail,Debug)]
#[fail(display="Node with ID {} was not found.", id)]
pub struct IdNotFound {pub id:Id}



// ===============
// === General ===
// ===============

/// Tests if given line contents can be seen as node with a given id
pub fn is_node_by_id(line:&ast::BlockLine<Option<Ast>>, id:ast::Id) -> bool {
    let node_info  = NodeInfo::from_block_line(line);
    node_info.contains_if(|node| node.id() == id)
}

/// Searches for `NodeInfo` with the associated `id` index in `lines`. Returns an error if
/// the Id is not found.
pub fn index_in_lines(lines:&[ast::BlockLine<Option<Ast>>], id:ast::Id) -> FallibleResult<usize> {
    let position = lines.iter().position(|line| is_node_by_id(line,id));
    position.ok_or_else(|| IdNotFound{id}.into())
}



// ================
// === NodeInfo ===
// ================

/// Description of the node that consists of all information locally available about node.
/// Nodes are required to bear IDs. This enum should never contain an ast of node without id set.
#[derive(Clone,Debug)]
#[allow(missing_docs)]
pub enum NodeInfo {
    /// Code with assignment, e.g. `foo = 2 + 2`
    Binding { infix: known::Infix },
    /// Code without assignment (no variable binding), e.g. `2 + 2`.
    Expression { ast: Ast },
}

impl NodeInfo {
    /// Tries to interpret the whole binding as a node. Right-hand side will become node's
    /// expression.
    pub fn new_binding(infix:known::Infix) -> Option<NodeInfo> {
        infix.rarg.id?;
        Some(NodeInfo::Binding {infix})
    }

    /// Tries to interpret AST as node, treating whole AST as an expression.
    pub fn new_expression(ast:Ast) -> Option<NodeInfo> {
        ast.id?;
        Some(NodeInfo::Expression {ast})
    }

    /// Tries to interpret AST as node, treating whole AST as an expression.
    pub fn from_line_ast(ast:&Ast) -> Option<NodeInfo> {
        if let Some(infix) = ast::opr::to_assignment(ast) {
            Self::new_binding(infix)
        } else {
            Self::new_expression(ast.clone())
        }
    }

    /// Tries to interpret AST as node, treating whole AST as an expression.
    pub fn from_block_line(line:&ast::BlockLine<Option<Ast>>) -> Option<NodeInfo> {
        Self::from_line_ast(line.elem.as_ref()?)
    }

    /// Node's unique ID.
    pub fn id(&self) -> Id {
        // Panic must not happen, as the only available constructors checks that
        // there is an ID present.
        self.expression().id.expect("Node AST must bear an ID")
    }

    /// Updates the node's AST so the node bears the given ID.
    pub fn set_id(&mut self, new_id:Id) {
        match self {
            NodeInfo::Binding{ref mut infix} => {
                let new_rarg = infix.rarg.with_id(new_id);
                let set      = infix.set(&ast::crumbs::InfixCrumb::RightOperand.into(),new_rarg);
                *infix = set.expect("Internal error: setting infix operand should always \
                                     succeed.");
            }
            NodeInfo::Expression{ref mut ast} => {
                *ast = ast.with_id(new_id);
            }
        };
    }

    /// AST of the node's expression.
    pub fn expression(&self) -> &Ast {
        match self {
            NodeInfo::Binding   {infix} => &infix.rarg,
            NodeInfo::Expression{ast}   => ast,
        }
    }

    /// AST of the node's pattern (assignment's left-hand side).
    pub fn pattern(&self) -> Option<&Ast> {
        match self {
            NodeInfo::Binding   {infix} => Some(&infix.larg),
            NodeInfo::Expression{..}    => None,
        }
    }

    /// Mutable AST of the node's expression. Maintains ID.
    pub fn set_expression(&mut self, expression:Ast) {
        let id = self.id();
        match self {
            NodeInfo::Binding{ref mut infix}  =>
                infix.update_shape(|infix| infix.rarg = expression),
            NodeInfo::Expression{ref mut ast} => *ast = expression,
        };
        // Id might have been overwritten by the AST we have set. Now we restore it.
        self.set_id(id);
    }

    /// The whole AST of node.
    pub fn ast(&self) -> &Ast {
        match self {
            NodeInfo::Binding   {infix} => infix.into(),
            NodeInfo::Expression{ast}   => ast,
        }
    }

    /// Set the pattern (left side of assignment) for node. If it is an Expression node, the
    /// assignment infix will be introduced.
    pub fn set_pattern(&mut self, pattern:Ast) {
        match self {
            NodeInfo::Binding {infix} => {
                // Setting infix operand never fails.
                infix.update_shape(|infix| infix.larg = pattern)
            }
            NodeInfo::Expression {ast} => {
                let infix = ast::Infix {
                    larg : pattern,
                    loff : 1,
                    opr  : Ast::opr("="),
                    roff : 1,
                    rarg : ast.clone(),
                };
                let infix = known::Infix::new(infix, None);
                *self = NodeInfo::Binding {infix};
            }
        }

    }

    /// Clear the pattern (left side of assignment) for node.
    ///
    /// If it is already an Expression node, no change is done.
    pub fn clear_pattern(&mut self) {
        match self {
            NodeInfo::Binding {infix} => {
                *self = NodeInfo::Expression {ast:infix.rarg.clone_ref()}
            }
            NodeInfo::Expression {..} => {}
        }

    }
}

impl ast::HasTokens for NodeInfo {
    fn feed_to(&self, consumer:&mut impl ast::TokenConsumer) {
        self.ast().feed_to(consumer)
    }
}



// =============
// === Tests ===
// =============

#[cfg(test)]
mod tests {
    use super::*;

    use ast::opr::predefined::ASSIGNMENT;

    fn expect_node(ast:Ast, expression_text:&str, id:Id) {
        let node_info = NodeInfo::from_line_ast(&ast).expect("expected a node");
        assert_eq!(node_info.expression().repr(),expression_text);
        assert_eq!(node_info.id(), id);
    }

    #[test]
    fn expression_node_test() {
        // expression: `4`
        let id = Id::new_v4();
        let ast = Ast::new(ast::Number { base:None, int: "4".into()}, Some(id));
        expect_node(ast,"4",id);
    }

    #[test]
    fn set_expression_binding() {
        let ast = Ast::infix(Ast::var("foo"),"=",Ast::number(4).with_new_id());
        assert_eq!(ast.repr(), "foo = 4");

        let mut node = NodeInfo::from_line_ast(&ast).expect("expected a node");
        let id       = node.id();
        node.set_expression(Ast::var("bar"));
        assert_eq!(node.expression().repr(), "bar");
        assert_eq!(node.ast().repr(), "foo = bar");
        assert_eq!(node.id(), id);
    }

    #[test]
    fn set_expression_plain() {
        let ast = Ast::number(4).with_new_id();
        assert_eq!(ast.repr(), "4");

        let mut node = NodeInfo::from_line_ast(&ast).expect("expected a node");
        let id       = node.id();
        node.set_expression(Ast::var("bar"));
        assert_eq!(node.expression().repr(), "bar");
        assert_eq!(node.ast().repr(), "bar");
        assert_eq!(node.id(), id);
    }

    #[test]
    fn binding_node_test() {
        // expression: `foo = 4`
        let id = Id::new_v4();
        let number = ast::Number { base:None, int: "4".into()};
        let larg   = Ast::var("foo");
        let rarg   = Ast::new(number, Some(id));
        let ast    = Ast::infix(larg,ASSIGNMENT,rarg);
        expect_node(ast,"4",id);
    }

    #[test]
    fn clearing_pattern_test() {
        // expression: `foo = 4`
        let id = Id::new_v4();
        let number = ast::Number { base:None, int: "4".into()};
        let larg   = Ast::var("foo");
        let rarg   = Ast::new(number, Some(id));
        let ast    = Ast::infix(larg,ASSIGNMENT,rarg);

        let mut node = NodeInfo::from_line_ast(&ast).unwrap();
        assert_eq!(node.repr(),"foo = 4");
        assert_eq!(node.id(),id);
        node.clear_pattern();
        assert_eq!(node.repr(),"4");
        assert_eq!(node.id(),id);
        node.clear_pattern();
        assert_eq!(node.repr(),"4");
        assert_eq!(node.id(),id);
    }

    #[test]
    fn setting_pattern_on_expression_node_test() {
        let id       = uuid::Uuid::new_v4();
        let line_ast = Ast::number(2).with_id(id);
        let mut node = NodeInfo::from_line_ast(&line_ast).unwrap();
        assert_eq!(node.repr(), "2");
        assert_eq!(node.id(),id);

        node.set_pattern(Ast::var("foo"));

        assert_eq!(node.repr(), "foo = 2");
        assert_eq!(node.id(),id);
    }

    #[test]
    fn setting_pattern_on_binding_node_test() {
        let id       = uuid::Uuid::new_v4();
        let larg     = Ast::var("foo");
        let rarg     = Ast::var("bar").with_id(id);
        let line_ast = Ast::infix(larg,ASSIGNMENT,rarg);
        let mut node = NodeInfo::from_line_ast(&line_ast).unwrap();

        assert_eq!(node.repr(), "foo = bar");
        assert_eq!(node.id(),id);

        node.set_pattern(Ast::var("baz"));

        assert_eq!(node.repr(), "baz = bar");
        assert_eq!(node.id(),id);
    }
}
