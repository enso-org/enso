//! A module containing utilities for managing execution context switch expressions.
//!
//! The context switch expressions are used to enable or disable a certain context for a single
//! node.
//!
//! For example, the following code:
//! ```ignore
//! operator11 = Runtime.with_enabled_context Context.Output environment="design" <|
//!     operator10.write "C:\Temp\Test.xlsx"
//! ```
//! will enable the [`Context::Output`] context for the `operator11` node in `design` environment.

use crate::prelude::*;

use crate::name::QualifiedName;
use crate::name::QualifiedNameRef;

use ast::known;
use ast::opr::qualified_name_chain;



// =================
// === Constants ===
// =================

/// FQN for the [`Context::Output`].
const OUTPUT: [&str; 5] = ["Standard", "Base", "Runtime", "Context", "Output"];
/// FQN for the `Runtime.with_enabled_context`, which corresponds to [`ContextSwitch::Enable`].
const ENABLE: [&str; 4] = ["Standard", "Base", "Runtime", "with_enabled_context"];
/// FQN for the `Runtime.with_disabled_context`, which corresponds to [`ContextSwitch::Disable`].
const DISABLE: [&str; 4] = ["Standard", "Base", "Runtime", "with_disabled_context"];



// ===============================
// === ContextSwitchExpression ===
// ===============================

// === ContextSwitch

/// The two possible context switches.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[allow(missing_docs)]
pub enum ContextSwitch {
    Enable,
    Disable,
}

impl<'a> TryFrom<QualifiedNameRef<'a>> for ContextSwitch {
    type Error = anyhow::Error;

    fn try_from(qualified_name: QualifiedNameRef<'a>) -> Result<Self, Self::Error> {
        // Unwraps are safe, because this function is tested.
        let enable_name = QualifiedName::from_all_segments(ENABLE).unwrap();
        let disable_name = QualifiedName::from_all_segments(DISABLE).unwrap();

        if qualified_name == enable_name {
            Ok(ContextSwitch::Enable)
        } else if qualified_name == disable_name {
            Ok(ContextSwitch::Disable)
        } else {
            Err(anyhow::anyhow!("Invalid context switch name: {:?}.", qualified_name))
        }
    }
}


// === Context ===

/// Available execution contexts.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[allow(missing_docs)]
pub enum Context {
    Output,
}

impl<'a> TryFrom<QualifiedNameRef<'a>> for Context {
    type Error = anyhow::Error;

    fn try_from(qualified_name: QualifiedNameRef<'a>) -> Result<Self, Self::Error> {
        // Unwrap is safe, because this function is tested.
        let output_name = QualifiedName::from_all_segments(OUTPUT).unwrap();

        if qualified_name == output_name {
            Ok(Context::Output)
        } else {
            Err(anyhow::anyhow!("Invalid context name: {:?}.", qualified_name))
        }
    }
}


// === Environment ===

im_string_newtype! {
    /// The name of the execution environment.
    Environment
}


// === ContextSwitchExpression ===

/// A representation of a single context switch expression.
#[derive(Debug, Clone, PartialEq)]
#[allow(missing_docs)]
pub struct ContextSwitchExpression {
    pub switch:      ContextSwitch,
    pub context:     Context,
    pub environment: Environment,
}

impl ContextSwitchExpression {
    /// Parse the context switch expression from the given AST.
    pub fn parse(ast: &Ast) -> Option<Self> {
        let infix = known::Infix::try_new(ast.clone()).ok()?;
        if ast::opr::is_right_assoc_opr(&infix.opr) {
            let prefix = ast::prefix::Chain::from_ast(&infix.larg)?;
            let context_switch = QualifiedName::try_from(&prefix.func).ok()?;
            let switch = ContextSwitch::try_from(context_switch.as_ref()).ok()?;
            if let [context, environment] = &prefix.args[..] {
                let context_name = QualifiedName::try_from(&context.sast.wrapped).ok()?;
                let context = Context::try_from(context_name.as_ref()).ok()?;
                let environment = environment.sast.wrapped.clone();
                let environment = known::Tree::try_from(environment).ok();
                let environment = environment.map(|t| t.as_text().map(Into::into));
                let environment = environment.flatten()?;
                Some(ContextSwitchExpression { switch, context, environment })
            } else {
                None
            }
        } else {
            None
        }
    }

    /// Convert the context switch expression to an AST.
    pub fn to_ast(&self) -> Ast {
        let func = match self.switch {
            ContextSwitch::Enable => ENABLE,
            ContextSwitch::Disable => DISABLE,
        };
        let context = match self.context {
            Context::Output => OUTPUT,
        };
        let func = qualified_name_chain(func.into_iter()).unwrap().into_ast();
        let context = qualified_name_chain(context.into_iter()).unwrap().into_ast();
        let environment = format!("\"{}\"", self.environment.deref());
        let environment: Ast = ast::Tree::text(environment).into();
        let args = vec![context, environment];
        ast::prefix::Chain::new(func, args).into_ast()
    }
}



// =============
// === Tests ===
// =============

#[cfg(test)]
mod tests {
    use super::*;
    use parser::Parser;

    #[test]
    fn test_recognizing_execution_context_switch() {
        #[derive(Debug)]
        struct Case {
            input:    &'static str,
            expected: Option<ContextSwitchExpression>,
        }

        #[rustfmt::skip]
        let cases = vec![
            Case {
                input: "foo",
                expected: None,
            },
            Case {
                input: "foo <| bar",
                expected: None,
            },
            Case {
                input: "foo <| bar <| baz",
                expected: None,
            },
            Case {
                input: "Runtime.with_enabled_context blabla <| bar",
                expected: None,
            },
            Case {
                input: "Runtime.with_enabled_context Context.Output \"context\" <| bar",
                expected: None,
            },
            Case {
                input: "Standard.Base.Runtime.with_enabled_context Context.Output \"context\" <| bar",
                expected: None,
            },
            Case {
                input: "Standard.Base.Runtime.with_enabled_context Standard.Base.Runtime.Context.Output \"context\" <| bar",
                expected: Some(ContextSwitchExpression {
                    switch:  ContextSwitch::Enable,
                    context: Context::Output,
                    environment: "context".into(),
                }),
            },
            Case {
                input: "Standard.Base.Runtime.with_disabled_context Standard.Base.Runtime.Context.Output \"context_name\" <| bar",
                expected: Some(ContextSwitchExpression {
                    switch:  ContextSwitch::Disable,
                    context: Context::Output,
                    environment: "context_name".into(),
                }),
            },
            Case {
                input: "Standard.Base.Runtime.with_disabled_context Standard.Base.Runtime.Context.Output \"context_name\" <| bar <| baz",
                expected: Some(ContextSwitchExpression {
                    switch:  ContextSwitch::Disable,
                    context: Context::Output,
                    environment: "context_name".into(),
                }),
            },
        ];

        let parser = Parser::new();
        for case in cases.iter() {
            let ast = parser.parse_line_ast(case.input).unwrap();
            let result = ContextSwitchExpression::parse(&ast);
            assert_eq!(result, case.expected, "{case:?}");
        }
    }
}
