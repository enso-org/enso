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
    match variant {
        AssignmentOperator(_) | TypeAnnotationOperator(_) | ArrowOperator(_) | CommaOperator(_) =>
            true,
        Operator(_)
        | DotOperator(_)
        | UnaryOperator(_)
        | AnnotationOperator(_)
        | AutoscopeOperator(_)
        | LambdaOperator(_)
        | SuspensionOperator(_)
        | NegationOperator(_)
        | Newline(_)
        | OpenSymbol(_)
        | CloseSymbol(_)
        | BlockStart(_)
        | BlockEnd(_)
        | Wildcard(_)
        | SuspendedDefaultArguments(_)
        | Ident(_)
        | Digits(_)
        | NumberBase(_)
        | PrivateKeyword(_)
        | TypeKeyword(_)
        | ForeignKeyword(_)
        | AllKeyword(_)
        | CaseKeyword(_)
        | OfKeyword(_)
        | TextStart(_)
        | TextEnd(_)
        | TextSection(_)
        | TextEscape(_)
        | TextInitialNewline(_)
        | TextNewline(_)
        | Invalid(_) => false,
    }
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
    pub fn with_binary_infix_precedence(self, precedence: Precedence) -> Self {
        Self { binary_infix_precedence: Some(precedence), ..self }
    }

    /// Return a copy of this operator, with unary prefix parsing allowed.
    pub fn with_unary_prefix_mode(self, precedence: Precedence) -> Self {
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
            binary_infix_precedence: Some(Precedence::Assignment),
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
            binary_infix_precedence: Some(Precedence::TypeAnnotation),
            lhs_section_termination: Some(SectionTermination::Reify),
            is_compile_time: true,
            rhs_is_non_expression: true,
            ..default()
        }
    }
}

impl HasOperatorProperties for variant::ArrowOperator {
    fn operator_properties(&self) -> OperatorProperties {
        OperatorProperties {
            binary_infix_precedence: Some(Precedence::Arrow),
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
            unary_prefix_precedence: Some(Precedence::Annotation),
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
            unary_prefix_precedence: Some(Precedence::Assignment),
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
            unary_prefix_precedence: Some(Precedence::Negation),
            ..default()
        }
    }
}

impl HasOperatorProperties for variant::LambdaOperator {
    fn operator_properties(&self) -> OperatorProperties {
        OperatorProperties {
            unary_prefix_precedence: Some(Precedence::Assignment),
            is_compile_time: true,
            ..default()
        }
    }
}

impl HasOperatorProperties for variant::DotOperator {
    fn operator_properties(&self) -> OperatorProperties {
        OperatorProperties { binary_infix_precedence: Some(Precedence::Application), ..default() }
    }
}

impl HasOperatorProperties for variant::SuspensionOperator {
    fn operator_properties(&self) -> OperatorProperties {
        OperatorProperties {
            unary_prefix_precedence: Some(Precedence::Annotation),
            is_compile_time: true,
            rhs_is_non_expression: true,
            ..default()
        }
    }
}

impl HasOperatorProperties for variant::CommaOperator {
    fn operator_properties(&self) -> OperatorProperties {
        OperatorProperties {
            binary_infix_precedence: Some(Precedence::Assignment),
            is_compile_time: true,
            rhs_is_non_expression: true,
            ..default()
        }
    }
}

/// Value that can be compared to determine which operator will bind more tightly within an
/// expression.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[repr(u8)]
#[allow(missing_docs)]
pub enum Precedence {
    Assignment = 1,
    TypeAnnotation,
    Arrow,
    Not,
    Logical,
    Equality,
    Functional,
    BitwiseOr,
    BitwiseAnd,
    Inequality,
    Addition,
    Multiplication,
    Exponentiation,
    OtherUserOperator,
    Negation,
    Application,
    Annotation,
    // NOTE: The highest value must not exceed 0x7e--see usage of `into_u8`.
}

impl Precedence {
    /// Return the value as a number.
    pub fn into_u8(self) -> u8 {
        self as u8
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
