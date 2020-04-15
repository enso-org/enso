//! Utilities for dealing with operators and Ast nodes related to them, like `Infix`, `Section*`.

use crate::prelude::*;

use crate::Ast;
use crate::Shape;
use crate::assoc::Assoc;
use crate::crumbs::Crumb;
use crate::crumbs::Located;
use crate::known;

/// Identifiers of operators with special meaning for IDE.
pub mod predefined {
    /// Used to create type paths (like `Int.+` or `IO.println`).
    pub const ACCESS : &str = ".";
    /// Used to create bindings, e.g. `add a b = a + b` or `foo = 5`.
    pub const ASSIGNMENT : &str = "=";
    /// Used to create lambda expressions, e.g. `a -> b -> a + b`.
    pub const ARROW : &str = "->";
}

/// Checks if the given AST has Opr shape with the name matching given string.
pub fn is_opr_named(ast:&Ast, name:impl Str) -> bool {
    let opr_opt = known::Opr::try_from(ast).ok();
    opr_opt.contains_if(|opr| opr.name == name.as_ref())
}

/// Checks if given Ast is an assignment operator identifier.
pub fn is_assignment_opr(ast:&Ast) -> bool {
    is_opr_named(ast,predefined::ASSIGNMENT)
}

/// Checks if given Ast is an assignment operator identifier.
pub fn is_arrow_opr(ast:&Ast) -> bool {
    is_opr_named(ast,predefined::ARROW)
}

/// If given Ast is a specific infix operator application, returns it.
pub fn to_specific_infix(ast:&Ast, name:&str) -> Option<known::Infix> {
    let infix = known::Infix::try_from(ast).ok()?;
    is_opr_named(&infix.opr,name).then(infix)
}

/// If given Ast is an assignment infix expression, returns it as Some known::Infix.
pub fn to_assignment(ast:&Ast) -> Option<known::Infix> {
    to_specific_infix(ast,predefined::ASSIGNMENT)
}

/// If given Ast is an arrow infix expression, returns it as Some known::Infix.
pub fn to_arrow(ast:&Ast) -> Option<known::Infix> {
    to_specific_infix(ast,predefined::ARROW)
}

/// Checks if a given node is an assignment infix expression.
pub fn is_assignment(ast:&Ast) -> bool {
    let infix = known::Infix::try_from(ast);
    infix.map(|infix| is_assignment_opr(&infix.opr)).unwrap_or(false)
}



// ===========================
// === Chain-related types ===
// ===========================

/// Infix operator operand. Optional, as we deal with Section* nodes as well.
pub type Operand = Option<Located<Ast>>;

/// Infix operator standing between (optional) operands.
pub type Operator = Located<known::Opr>;

/// Creates `Operand` from `ast` with position relative to the given `parent` node.
pub fn make_operand(parent:&Located<Ast>, crumb:impl Into<Crumb>, child:&Ast) -> Operand {
    Some(parent.descendant(crumb.into(),child.clone()))
}

/// Creates `Operator` from `ast` with position relative to the given `parent` node.
pub fn make_operator(parent:&Located<Ast>, crumb:impl Into<Crumb>, opr:&Ast) -> Option<Operator> {
    let opr = known::Opr::try_from(opr).ok()?;
    Some(parent.descendant(crumb.into(),opr))
}

/// Describes associativity of the given operator AST.
fn assoc(ast:&known::Opr) -> Assoc {
    Assoc::of(&ast.name)
}



// ========================
// === GeneralizedInfix ===
// ========================

/// An abstraction over `Infix` and all `SectionSth` nodes. Stores crumb locations for all its ASTs.
#[derive(Clone,Debug)]
pub struct GeneralizedInfix {
    /// Left operand, if present.
    pub left  : Operand,
    /// The operator, always present.
    pub opr   : Operator,
    /// Right operand, if present.
    pub right : Operand,
}

impl GeneralizedInfix {
    /// As `try_new` but assumes that this expression is located at the root crumb location.
    pub fn try_new_root(ast:&Ast) -> Option<GeneralizedInfix> {
        GeneralizedInfix::try_new(&Located::new_root(ast.clone()))
    }

    /// Tries interpret given AST node as GeneralizedInfix. Returns None, if Ast is not any kind of
    /// application on infix operator.
    pub fn try_new(ast:&Located<Ast>) -> Option<GeneralizedInfix> {
        use crate::crumbs::InfixCrumb;
        use crate::crumbs::SectionLeftCrumb;
        use crate::crumbs::SectionRightCrumb;
        use crate::crumbs::SectionSidesCrumb;

        match ast.shape() {
            Shape::Infix(infix) => Some(GeneralizedInfix{
                left  : make_operand (ast,InfixCrumb::LeftOperand, &infix.larg),
                opr   : make_operator(ast,InfixCrumb::Operator,    &infix.opr)?,
                right : make_operand (ast,InfixCrumb::RightOperand,&infix.rarg),
            }),
            Shape::SectionLeft(left) => Some(GeneralizedInfix{
                left  : make_operand (ast,SectionLeftCrumb::Arg,&left.arg),
                opr   : make_operator(ast,SectionLeftCrumb::Opr,&left.opr)?,
                right : None,
            }),
            Shape::SectionRight(right) => Some(GeneralizedInfix{
                left  : None,
                opr   : make_operator(ast,SectionRightCrumb::Opr,&right.opr)?,
                right : make_operand (ast,SectionRightCrumb::Arg,&right.arg),
            }),
            Shape::SectionSides(sides) => Some(GeneralizedInfix{
                left  : None,
                opr   : make_operator(ast,SectionSidesCrumb,&sides.opr)?,
                right : None,
            }),
            _ => None,
        }
    }

    /// Associativity of the operator used in this infix expression.
    pub fn assoc(&self) -> Assoc {
        assoc(&self.opr.item)
    }

    /// Identifier name  of the operator used in this infix expression.
    pub fn name(&self) -> &str {
        &self.opr.name
    }

    /// The self operand, target of the application.
    pub fn target_operand(&self) -> Operand {
        match self.assoc() {
            Assoc::Left  => self.left.clone(),
            Assoc::Right => self.right.clone(),
        }
    }

    /// Operand other than self.
    pub fn argument_operand(&self) -> Operand {
        match self.assoc() {
            Assoc::Left  => self.right.clone(),
            Assoc::Right => self.left.clone(),
        }
    }

    /// Converts chain of infix applications using the same operator into `Chain`.
    /// Sample inputs are `x,y,x` or `a+b+` or `+5+5+5`. Note that `Sides*` nodes
    /// are also supported, along the `Infix` nodes.
    pub fn flatten(&self) -> Chain {
        let target = self.target_operand();
        let rest   = ChainElement {
            operator : self.opr.clone(),
            operand  : self.argument_operand()
        };

        let target_subtree_infix = target.clone().and_then(|ast| {
            GeneralizedInfix::try_new(&ast)
        });
        let mut target_subtree_flat = match target_subtree_infix {
            Some(target_infix) if target_infix.name() == self.name() =>
                target_infix.flatten(),
            _ => Chain { target, args:Vec::new(), operator:self.opr.item.clone() },
        };

        target_subtree_flat.args.push(rest);
        target_subtree_flat
    }
}



// =============
// === Chain ===
// =============

/// Result of flattening infix operator chain, like `a+b+c` or `Foo.Bar.Baz`.
#[derive(Clone,Debug)]
pub struct Chain {
    /// The primary application target (left- or right-most operand, depending on
    /// operators associativity).
    pub target : Operand,
    /// Subsequent operands applied to the `target`.
    pub args   : Vec<ChainElement>,
    /// Operator AST. Generally all operators in the chain should be the same (except for id).
    /// It is not specified which exactly operator's in the chain this AST belongs to.
    pub operator : known::Opr,
}

impl Chain {
    /// If this is infix, it flattens whole chain and returns result.
    /// Otherwise, returns None.
    pub fn try_new(ast:&Ast) -> Option<Chain> {
        GeneralizedInfix::try_new_root(&ast).map(|infix| infix.flatten())
    }

    /// Flattens infix chain if this is infix application of given operator.
    pub fn try_new_of(ast:&Ast, operator:&str) -> Option<Chain> {
        let infix = GeneralizedInfix::try_new_root(&ast)?;
        (infix.name() == operator).as_some_from(|| infix.flatten())
    }

    /// Iterates over &Located<Ast>, beginning with target (this argument) and then subsequent
    /// arguments.
    pub fn enumerate_operands<'a>(&'a self) -> impl Iterator<Item=&'a Located<Ast>> + 'a {
        let this = std::iter::once(&self.target);
        let args = self.args.iter().map(|elem| &elem.operand);
        this.chain(args).flatten()
    }

    /// Iterates over &Located<Ast>, beginning with target (this argument) and then subsequent
    /// arguments.
    pub fn enumerate_operators<'a>(&'a self) -> impl Iterator<Item=&'a Located<known::Opr>> + 'a {
        self.args.iter().map(|elem| &elem.operator)
    }
}

/// Element of the infix application chain, i.e. operator and its operand.
#[derive(Clone,Debug)]
pub struct ChainElement {
    #[allow(missing_docs)]
    pub operator : Operator,
    /// Operand on the opposite side to `this` argument.
    /// Depending on operator's associativity it is either right (for left-associative operators)
    /// or on the left side of operator.
    pub operand  : Operand,
}


#[cfg(test)]
mod tests {
    use super::*;

    fn expect_at(root_ast:&Ast, operand:&Operand, expected_ast:&Ast) {
        assert_eq!(&operand.as_ref().unwrap().item,expected_ast);
        let crumbs = &operand.as_ref().unwrap().crumbs;
        let ast    = root_ast.get_traversing(crumbs).unwrap();
        assert_eq!(ast, expected_ast, "expected `{}` at crumbs `{:?}` for `{}`",
                   expected_ast.repr(), crumbs, root_ast.repr());
    }

    #[test]
    fn infix_chain_tests() {
        let a               = Ast::var("a");
        let b               = Ast::var("b");
        let c               = Ast::var("c");
        let a_plus_b        = Ast::infix(a.clone(),"+",b.clone());
        let a_plus_b_plus_c = Ast::infix(a_plus_b.clone(),"+",c.clone());
        let chain           = Chain::try_new(&a_plus_b_plus_c).unwrap();
        expect_at(&a_plus_b_plus_c,&chain.target,&a);
        expect_at(&a_plus_b_plus_c,&chain.args[0].operand,&b);
        expect_at(&a_plus_b_plus_c,&chain.args[1].operand,&c);
    }

    #[test]
    fn infix_chain_tests_right() {
        let a                 = Ast::var("a");
        let b                 = Ast::var("b");
        let c                 = Ast::var("c");
        let b_comma_c         = Ast::infix(b.clone(),",",c.clone());
        let a_comma_b_comma_c = Ast::infix(a.clone(),",",b_comma_c.clone());
        let chain             = Chain::try_new(&a_comma_b_comma_c).unwrap();
        expect_at(&a_comma_b_comma_c,&chain.target,&c);
        expect_at(&a_comma_b_comma_c,&chain.args[0].operand,&b);
        expect_at(&a_comma_b_comma_c,&chain.args[1].operand,&a);
    }
}
