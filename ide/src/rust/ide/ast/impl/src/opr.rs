//! Utilities for dealing with operators and Ast nodes related to them, like `Infix`, `Section*`.

use crate::prelude::*;

use crate::Ast;
use crate::assoc::Assoc;
use crate::known;
use crate::Shape;

/// Identifiers of operators with special meaning for IDE.
pub mod predefined {
    /// Used to create bindings, e.g. `add a b = a + b` or `foo = 5`.
    pub const ASSIGNMENT : &str = "=";
    /// Used to create type paths (like `Int.+` or `IO.println`).
    pub const ACCESS : &str = ".";
}

/// Checks if given Ast is an assignment operator identifier.
pub fn is_assignment_opr(ast:&Ast) -> bool {
    let opr_opt = known::Opr::try_from(ast);
    opr_opt.map(|opr| opr.name == predefined::ASSIGNMENT).unwrap_or(false)
}

/// If given Ast is an assignment operator, returns it as Some known::Infix.
pub fn to_assignment(ast:&Ast) -> Option<known::Infix> {
    let infix = known::Infix::try_from(ast).ok()?;
    is_assignment_opr(&infix.opr).then(infix)
}

/// Infix operator operand. Optional, as we deal with Section* nodes as well.
pub type Operand = Option<Ast>;

/// Infix operator standing between (optional) operands.
pub type Operator = known::Opr;



// ========================
// === GeneralizedInfix ===
// ========================

/// An abstraction over `Infix` and all `SectionSth` nodes.
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
    /// Tries interpret given AST node as GeneralizedInfix. Returns None, if Ast is not any kind of
    /// application on infix operator.
    pub fn try_new(ast:&Ast) -> Option<GeneralizedInfix> {
        match ast.shape() {
            Shape::Infix(infix) => Some(GeneralizedInfix{
                left  : Some(infix.larg.clone()),
                opr   : known::Opr::try_from(&infix.opr).ok()?,
                right : Some(infix.rarg.clone()),
            }),
            Shape::SectionLeft(left) => Some(GeneralizedInfix{
                left  : Some(left.arg.clone()),
                opr   : known::Opr::try_from(&left.opr).ok()?,
                right : None,
            }),
            Shape::SectionRight(right) => Some(GeneralizedInfix{
                left  : None,
                opr   : known::Opr::try_from(&right.opr).ok()?,
                right : Some(right.arg.clone()),
            }),
            Shape::SectionSides(sides) => Some(GeneralizedInfix{
                left  : None,
                opr   : known::Opr::try_from(&sides.opr).ok()?,
                right : None,
            }),
            _ => None,
        }
    }

    /// Associativity of the operator used in this infix expression.
    pub fn assoc(&self) -> Assoc {
        Assoc::of(&self.name())
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
            _ => Chain { target, args:Vec::new() },
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
}

impl Chain {
    /// If this is infix, it flattens whole chain and returns result.
    /// Otherwise, returns None.
    pub fn try_new(ast:&Ast) -> Option<Chain> {
        GeneralizedInfix::try_new(ast).map(|infix| infix.flatten())
    }

    /// Flattens infix chain if this is infix application of given operator.
    pub fn try_new_of(ast:&Ast, operator:&str) -> Option<Chain> {
        let infix = GeneralizedInfix::try_new(ast)?;
        (infix.name() == operator).as_some_from(|| infix.flatten())
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
