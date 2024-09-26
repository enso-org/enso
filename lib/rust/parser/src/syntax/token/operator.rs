use crate::syntax::token::*;

use crate::lexer::analyze_non_syntactic_operator;
use crate::syntax::operator::SectionTermination;



/// Properties of an operator that are identified when lexing.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Default)]
pub struct OperatorProperties {
    // Precedence / associativity
    binary_infix_precedence: Option<Precedence>,
    unary_prefix_precedence: Option<Precedence>,
    is_value_operation:      bool,
    is_right_associative:    bool,
    // Special properties
    lhs_section_termination: Option<SectionTermination>,
    is_modifier:             bool,
    is_compile_time:         bool,
    rhs_is_non_expression:   bool,
}

pub fn is_syntactic_binary_operator(variant: &Variant) -> bool {
    use Variant::*;
    matches!(
        variant,
        AssignmentOperator(_) | TypeAnnotationOperator(_) | ArrowOperator(_) | CommaOperator(_)
    )
}

impl OperatorProperties {
    /// Construct an operator with default properties.
    pub fn new() -> Self {
        default()
    }

    /// Construct a value-level operator.
    pub fn value() -> Self {
        Self { is_value_operation: true, ..default() }
    }

    /// Construct a functional operator.
    pub fn functional() -> Self {
        Self { is_value_operation: false, ..default() }
    }

    /// Return a copy of this operator, with the given binary infix precedence.
    pub fn with_binary_infix_precedence(self, value: u8) -> Self {
        let precedence = Precedence { value };
        debug_assert!(precedence > Precedence::min());
        debug_assert!(value & 0x80 == 0);
        debug_assert!((value + 1) & 0x80 == 0);
        Self { binary_infix_precedence: Some(precedence), ..self }
    }

    /// Return a copy of this operator, with unary prefix parsing allowed.
    pub fn with_unary_prefix_mode(self, precedence: Precedence) -> Self {
        debug_assert!(precedence > Precedence::min());
        Self { unary_prefix_precedence: Some(precedence), ..self }
    }

    /// Mark the operator as a value-level operation, as opposed to functional.
    pub fn as_value_operation(self) -> Self {
        Self { is_value_operation: true, ..self }
    }

    /// Return whether the operator is a value-level operation, as opposed to functional.
    pub fn is_value_operation(&self) -> bool {
        self.is_value_operation
    }

    /// Return a copy of this operator, modified to be flagged as right associative.
    pub fn as_right_associative(self) -> Self {
        Self { is_right_associative: true, ..self }
    }

    /// Return a copy of this operator, modified to be flagged as a modified-assignment operator.
    pub fn as_modifier(self) -> Self {
        Self { is_modifier: true, ..self }
    }

    /// Return this operator's binary infix precedence, if it has one.
    pub fn binary_infix_precedence(&self) -> Option<Precedence> {
        self.binary_infix_precedence
    }

    /// Return this operator's unary prefix precedence, if it has one.
    pub fn unary_prefix_precedence(&self) -> Option<Precedence> {
        self.unary_prefix_precedence
    }

    /// Return whether this operator can form operator sections.
    pub fn can_form_section(&self) -> bool {
        !self.is_compile_time
    }

    /// Return the LHS operator-section/template-function behavior of this operator.
    pub fn lhs_section_termination(&self) -> Option<SectionTermination> {
        self.lhs_section_termination
    }

    /// Return whether this operator is a modified-assignment operator.
    pub fn is_modifier(&self) -> bool {
        self.is_modifier
    }

    /// Return this operator's associativity.
    pub fn associativity(&self) -> Associativity {
        match self.is_right_associative {
            false => Associativity::Left,
            true => Associativity::Right,
        }
    }

    /// Whether the RHS is an expression; if true, the operator may introduce a body block.
    pub fn rhs_is_expression(&self) -> bool {
        !self.rhs_is_non_expression
    }
}

/// Operator-like tokens have operator properties, including normal operators and syntactic
/// operators.
trait HasOperatorProperties {
    /// Return the properties of this operator.
    fn operator_properties(&self) -> OperatorProperties;
}

/// If a token is operator-like, it has associated properties.
pub trait TokenOperatorProperties {
    /// Return a value if this token is operator-like.
    fn operator_properties(&self) -> Option<OperatorProperties>;
}

impl<'s, Variant: HasOperatorProperties> HasOperatorProperties for Token<'s, Variant> {
    fn operator_properties(&self) -> OperatorProperties {
        self.variant.operator_properties()
    }
}

impl<'s> TokenOperatorProperties for Token<'s> {
    fn operator_properties(&self) -> Option<OperatorProperties> {
        Some(match self.variant {
            Variant::Operator(_) => analyze_non_syntactic_operator(self.code.repr.0),
            Variant::AssignmentOperator(op) => op.operator_properties(),
            Variant::TypeAnnotationOperator(op) => op.operator_properties(),
            Variant::ArrowOperator(op) => op.operator_properties(),
            Variant::AnnotationOperator(op) => op.operator_properties(),
            Variant::AutoscopeOperator(op) => op.operator_properties(),
            Variant::NegationOperator(op) => op.operator_properties(),
            Variant::LambdaOperator(op) => op.operator_properties(),
            Variant::DotOperator(op) => op.operator_properties(),
            Variant::SuspensionOperator(op) => op.operator_properties(),
            Variant::CommaOperator(op) => op.operator_properties(),
            _ => return None,
        })
    }
}

impl HasOperatorProperties for variant::AssignmentOperator {
    fn operator_properties(&self) -> OperatorProperties {
        OperatorProperties {
            binary_infix_precedence: Some(Precedence { value: 1 }),
            lhs_section_termination: Some(SectionTermination::Unwrap),
            is_right_associative: true,
            is_compile_time: true,
            ..default()
        }
    }
}

impl HasOperatorProperties for variant::TypeAnnotationOperator {
    fn operator_properties(&self) -> OperatorProperties {
        OperatorProperties {
            binary_infix_precedence: Some(Precedence { value: 2 }),
            lhs_section_termination: Some(SectionTermination::Reify),
            is_right_associative: true,
            is_compile_time: true,
            rhs_is_non_expression: true,
            ..default()
        }
    }
}

impl HasOperatorProperties for variant::ArrowOperator {
    fn operator_properties(&self) -> OperatorProperties {
        OperatorProperties {
            binary_infix_precedence: Some(Precedence { value: 2 }),
            lhs_section_termination: Some(SectionTermination::Unwrap),
            is_right_associative: true,
            is_compile_time: true,
            ..default()
        }
    }
}

impl HasOperatorProperties for variant::AnnotationOperator {
    fn operator_properties(&self) -> OperatorProperties {
        OperatorProperties {
            unary_prefix_precedence: Some(Precedence::max()),
            is_right_associative: true,
            is_compile_time: true,
            rhs_is_non_expression: true,
            ..default()
        }
    }
}

impl HasOperatorProperties for variant::AutoscopeOperator {
    fn operator_properties(&self) -> OperatorProperties {
        OperatorProperties {
            unary_prefix_precedence: Some(Precedence::min_valid()),
            is_compile_time: true,
            rhs_is_non_expression: true,
            ..default()
        }
    }
}

impl HasOperatorProperties for variant::NegationOperator {
    fn operator_properties(&self) -> OperatorProperties {
        OperatorProperties {
            is_value_operation: true,
            unary_prefix_precedence: Some(Precedence::unary_minus()),
            ..default()
        }
    }
}

impl HasOperatorProperties for variant::LambdaOperator {
    fn operator_properties(&self) -> OperatorProperties {
        OperatorProperties {
            unary_prefix_precedence: Some(Precedence::min_valid()),
            is_compile_time: true,
            ..default()
        }
    }
}

impl HasOperatorProperties for variant::DotOperator {
    fn operator_properties(&self) -> OperatorProperties {
        OperatorProperties { binary_infix_precedence: Some(Precedence { value: 80 }), ..default() }
    }
}

impl HasOperatorProperties for variant::SuspensionOperator {
    fn operator_properties(&self) -> OperatorProperties {
        OperatorProperties {
            unary_prefix_precedence: Some(Precedence::max()),
            is_compile_time: true,
            rhs_is_non_expression: true,
            ..default()
        }
    }
}

impl HasOperatorProperties for variant::CommaOperator {
    fn operator_properties(&self) -> OperatorProperties {
        OperatorProperties {
            binary_infix_precedence: Some(Precedence { value: 1 }),
            is_compile_time: true,
            rhs_is_non_expression: true,
            ..default()
        }
    }
}

/// Value that can be compared to determine which operator will bind more tightly within an
/// expression.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Precedence {
    /// A numeric value determining precedence order.
    pub(super) value: u8,
}

impl Precedence {
    /// Return a precedence that is lower than the precedence of any operator.
    pub fn min() -> Self {
        Precedence { value: 0 }
    }

    /// Return the precedence for any operator.
    pub fn min_valid() -> Self {
        Precedence { value: 1 }
    }

    /// Return a precedence that is not lower than any other precedence.
    pub fn max() -> Self {
        Precedence { value: 100 }
    }

    /// Return the precedence of application.
    pub fn application() -> Self {
        Precedence { value: 80 }
    }

    /// Return the precedence of @annotations.
    pub fn annotation() -> Self {
        Precedence { value: 79 }
    }

    /// Return the precedence of unary minus.
    pub fn unary_minus() -> Self {
        Precedence { value: 79 }
    }

    /// Return the precedence of unary minus when applied to a numeric literal.
    pub fn unary_minus_numeric_literal() -> Self {
        Precedence { value: 80 }
    }

    /// Return the value as a number.
    pub fn into_u8(self) -> u8 {
        self.value
    }
}

/// Associativity (left or right).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Associativity {
    /// Left-associative.
    Left,
    /// Right-associative.
    Right,
}
