//! Utilities for dealing with `Prefix` application Ast nodes.

use crate::prelude::*;

use crate::known;
use crate::Ast;

#[derive(Clone,Debug)]
/// Result of flattening a sequence of prefix applications.
pub struct Chain {
    /// The function (initial application target)
    pub func : Ast,
    /// Subsequent arguments applied over the function.
    pub args : Vec<Ast>
}

impl Chain {
    /// Translates calls like `a b c` that generate nested prefix chain like
    /// App(App(a,b),c) into flat list where first element is the function and
    /// then arguments are placed: `{func:a, args:[b,c]}`.
    pub fn new(ast:&known::Prefix) -> Chain {
        fn run(ast:&known::Prefix, mut acc: &mut Vec<Ast>) {
            match known::Prefix::try_from(&ast.func) {
                Ok(lhs_app) => run(&lhs_app, &mut acc),
                _           => acc.push(ast.func.clone()),
            }
            acc.push(ast.arg.clone())
        }

        let mut parts = Vec::new();
        run(ast,&mut parts);

        let func = parts.remove(0);
        let args = parts; // remaining parts are args
        Chain {func,args}
    }

    /// As new but if the AST is not a prefix, interprets is a function with an
    /// empty arguments list.
    pub fn new_non_strict(ast:&Ast) -> Chain {
        if let Ok(ref prefix) = known::Prefix::try_from(ast) {
            // Case like `a b c`
            Self::new(prefix)
        } else if let Ok(ref section) = known::SectionRight::try_from(ast) {
            // Case like `+ a b`
            let func = section.opr.clone();
            let right_chain = Chain::new_non_strict(&section.arg);
            let mut args = vec![right_chain.func];
            args.extend(right_chain.args);
            Chain {func,args}
        } else {
            // Case like `a`
            let func = ast.clone();
            let args = Vec::new();
            Chain {func,args}
        }
    }
}
