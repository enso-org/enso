//! Module with alias analysis — allows telling what identifiers are used and introduced by each
//! node in the graph.

use crate::prelude::*;

use crate::definition::DefinitionInfo;
use crate::definition::ScopeKind;

use ast::crumbs::Crumb;
use ast::crumbs::InfixCrumb;
use ast::crumbs::Located;
use std::borrow::Borrow;


// ==============
// === Export ===
// ==============

#[cfg(test)]
pub mod test_utils;



// =======================
// === IdentifierUsage ===
// =======================

/// Description of how some node is interacting with the graph's scope.
#[derive(Clone, Debug, Default)]
pub struct IdentifierUsage {
    /// Identifiers from the graph's scope that node is using.
    pub introduced: Vec<Located<String>>,
    /// Identifiers that node introduces into the parent scope.
    pub used:       Vec<Located<String>>,
}

impl IdentifierUsage {
    /// Returns all identifiers that are either used from or introduced into the scope.
    pub fn all_identifiers(&self) -> Vec<Located<String>> {
        self.introduced.iter().chain(self.used.iter()).cloned().collect()
    }
}



// ================
// === Analysis ===
// ================


// === Helper Datatypes ===

/// Says whether the identifier occurrence introduces it into scope or uses it from scope.
#[allow(missing_docs)]
#[derive(Clone, Copy, Debug, Display, PartialEq, Eq)]
pub enum OccurrenceKind {
    Used,
    Introduced,
}

/// If the current context in the AST processor is a pattern context.
// TODO [mwu] Refer to the specification once it is merged.
#[allow(missing_docs)]
#[derive(Clone, Copy, Debug, Display, PartialEq, Eq)]
pub enum Context {
    NonPattern,
    Pattern,
}

/// Represents scope and information about identifiers usage within it.
#[derive(Clone, Debug, Default)]
pub struct Scope {
    #[allow(missing_docs)]
    pub symbols: IdentifierUsage,
}

impl Scope {
    /// Iterates over identifiers that are used in this scope but are not introduced in this scope
    /// i.e. the identifiers that parent scope must provide.
    pub fn used_from_parent(self) -> impl Iterator<Item = Located<String>> {
        let available = self.symbols.introduced.into_iter().map(|located_name| located_name.item);
        let available = available.collect::<HashSet<_>>();
        let all_used = self.symbols.used.into_iter();
        all_used.filter(move |name| !available.contains(&name.item))
    }

    /// Drops the information about nested child scope by:
    /// 1) disregarding any usage of identifiers introduced in the child scope;
    /// 2) propagating all non-shadowed identifier usage from this scope into this scope usage list.
    fn coalesce_child(&mut self, child: Scope) {
        let symbols_to_use = child.used_from_parent();
        self.symbols.used.extend(symbols_to_use);
    }
}


// === AliasAnalyzer ===

/// Traverser AST and analyzes identifier usage.
#[derive(Clone, Debug, Default)]
pub struct AliasAnalyzer {
    /// Root scope for this analyzer.
    pub root_scope:   Scope,
    /// Stack of scopes that shadow the root one.
    shadowing_scopes: Vec<Scope>,
    /// Stack of context. Lack of any context information is considered non-pattern context.
    context:          Vec<Context>,
    /// Current location, relative to the input AST root.
    location:         Vec<Crumb>,
}

impl AliasAnalyzer {
    /// Creates a new analyzer.
    pub fn new() -> AliasAnalyzer {
        AliasAnalyzer::default()
    }

    /// Adds items to the target vector, calls the callback `f` then removes the items.
    fn with_items_added<T, R>(
        &mut self,
        vec: impl Fn(&mut Self) -> &mut Vec<T>,
        items: impl IntoIterator<Item: Into<T>>,
        f: impl FnOnce(&mut Self) -> R,
    ) -> R {
        let original_count = vec(self).len();
        vec(self).extend(items.into_iter().map(|item| item.into()));
        let ret = f(self);
        vec(self).truncate(original_count);
        ret
    }

    /// Pushes a new scope, then runs a given `f` function. Once it finished, scope is removed and
    /// its unshadowed variable usage propagated onto the current scope.
    fn in_new_scope(&mut self, f: impl FnOnce(&mut Self)) {
        let scope = Scope::default();
        self.shadowing_scopes.push(scope);
        f(self);
        let scope = self.shadowing_scopes.pop().unwrap();
        self.current_scope_mut().coalesce_child(scope);
    }

    /// Temporarily sets contest and invokes `f` within it.
    fn in_context(&mut self, context: Context, f: impl FnOnce(&mut Self)) {
        self.with_items_added(|this| &mut this.context, iter::once(context), f);
    }

    /// Enters a new location (relative to the current one), invokes `f`, leaves the location.
    fn in_location<Cs, F, R>(&mut self, crumbs: Cs, f: F) -> R
    where
        Cs: IntoIterator<Item = Crumb>,
        F: FnOnce(&mut Self) -> R, {
        self.with_items_added(|this| &mut this.location, crumbs, f)
    }

    /// Enters a new location (relative to the current one), invokes `f`, leaves the location.
    fn in_location_of<T, F, R>(&mut self, located_item: &Located<T>, f: F) -> R
    where F: FnOnce(&mut Self) -> R {
        self.in_location(located_item.crumbs.iter().cloned(), f)
    }

    /// Obtains a mutable reference to the current scope.
    fn current_scope_mut(&mut self) -> &mut Scope {
        self.shadowing_scopes.last_mut().unwrap_or(&mut self.root_scope)
    }

    /// Returns the current context kind. (pattern or not)
    fn current_context(&self) -> Context {
        self.context.last().copied().unwrap_or(Context::NonPattern)
    }

    /// Records identifier occurrence in the current scope.
    fn record_identifier(&mut self, kind: OccurrenceKind, identifier: String) {
        let identifier = Located::new(self.location.clone(), identifier);
        let symbols = &mut self.current_scope_mut().symbols;
        let target = match kind {
            OccurrenceKind::Used => &mut symbols.used,
            OccurrenceKind::Introduced => &mut symbols.introduced,
        };
        target.push(identifier)
    }

    /// Checks if we are currently in the pattern context.
    fn is_in_pattern(&self) -> bool {
        self.current_context() == Context::Pattern
    }

    /// If given AST is an identifier, records its occurrence.
    /// Returns boolean saying if the identifier was recorded.
    fn try_recording_identifier(&mut self, kind: OccurrenceKind, ast: &Ast) -> bool {
        let name = ast::identifier::name(ast);
        name.map(|name| self.record_identifier(kind, name.to_owned())).is_some()
    }

    /// If the given located AST-like entity is an identifier, records its occurrence.
    fn store_if_name<'a, T>(&mut self, kind: OccurrenceKind, located: Located<T>) -> bool
    where T: Into<&'a Ast> + 'a + Copy {
        let ast = located.item.into();
        self.in_location_of(&located, |this| this.try_recording_identifier(kind, ast))
    }

    /// Processes the given AST, while crumb is temporarily pushed to the current location.
    fn process_subtree_at(&mut self, crumb: impl Into<Crumb>, subtree: &Ast) {
        self.in_location(crumb.into(), |this| this.process_ast(subtree))
    }

    /// Processes the given AST, while crumb is temporarily pushed to the current location.
    fn process_located_ast(&mut self, located_ast: &Located<impl Borrow<Ast>>) {
        self.in_location_of(located_ast, |this| this.process_ast(located_ast.item.borrow()))
    }

    /// Processes subtrees of the given AST denoted by given crumbs
    pub fn process_given_subtrees<C>(&mut self, ast: &C, crumbs: impl Iterator<Item = C::Crumb>)
    where
        C: Crumbable,
        C::Crumb: Into<Crumb>, {
        for crumb in crumbs {
            // Failure should never happen but we don't really care enough to crash anything
            // otherwise.
            if let Ok(subtree) = ast.get(&crumb) {
                self.process_subtree_at(crumb.into(), subtree)
            }
        }
    }

    /// Processes all subtrees of the given AST in their respective locations.
    pub fn process_subtrees(&mut self, ast: &impl Crumbable) {
        for (crumb, ast) in ast.enumerate() {
            self.process_subtree_at(crumb, ast)
        }
    }

    /// Processes the given AST, along with its subtree.
    ///
    /// This is the primary function that is recursively being called as the AST is being traversed.
    pub fn process_ast(&mut self, ast: &Ast) {
        if let Some(definition) = DefinitionInfo::from_line_ast(ast, ScopeKind::NonRoot, default())
        {
            self.process_definition(&definition)
        } else if let Some(assignment) = ast::opr::to_assignment(ast) {
            self.process_assignment(&assignment);
        } else if let Some(lambda) = ast::macros::as_lambda(ast) {
            self.process_lambda(&lambda);
        } else if let Ok(macro_match) = ast::known::Match::try_from(ast) {
            // Macros (except for lambdas which were covered in the previous check) never introduce
            // new scopes or different context. We skip the keywords ("if" in "if-then-else" is not
            // an identifier) and process the matched subtrees as usual.
            self.process_given_subtrees(macro_match.shape(), macro_match.iter_pat_match_subcrumbs())
        } else if let Ok(ambiguous) = ast::known::Ambiguous::try_from(ast) {
            self.process_given_subtrees(ambiguous.shape(), ambiguous.iter_pat_match_subcrumbs())
        } else if self.is_in_pattern() {
            // We are in the pattern (be it a lambda's or assignment's left side). Three options:
            // 1) This is a destructuring pattern match using infix syntax, like `head,tail`.
            // 2) This is a destructuring pattern match with prefix syntax, like `Point x y`.
            // 3) This is a single AST node, like `foo` or `Foo`.
            // (the possibility of definition has been already excluded)
            if let Some(infix_chain) = ast::opr::Chain::try_new(ast) {
                // Infix always acts as pattern-match in left-side.
                for operand in infix_chain.enumerate_non_empty_operands() {
                    self.process_located_ast(&operand.map(|operand| &operand.arg))
                }
                for operator in infix_chain.enumerate_operators() {
                    // Operators in infix positions are treated as constructors, i.e. they are used.
                    self.store_if_name(OccurrenceKind::Used, operator);
                }
            } else if let Some(prefix_chain) = ast::prefix::Chain::from_ast(ast) {
                // Constructor we match against is used. Its arguments introduce names.
                if ast::known::Cons::try_from(&prefix_chain.func).is_ok() {
                    self.store_if_name(OccurrenceKind::Used, prefix_chain.located_func());
                }

                // Arguments introduce names, we ignore function name.
                // Arguments will just introduce names in pattern context.
                for argument in prefix_chain.enumerate_args() {
                    self.process_located_ast(&argument)
                }
            } else {
                // Single AST node on the assignment LHS. Deal with identifiers, otherwise
                // recursively process subtrees.
                match ast.shape() {
                    ast::Shape::Cons(_) => {
                        self.try_recording_identifier(OccurrenceKind::Used, ast);
                    }
                    ast::Shape::Var(_) => {
                        self.try_recording_identifier(OccurrenceKind::Introduced, ast);
                    }
                    _ => {
                        self.process_subtrees(ast);
                    }
                }
            }
        } else {
            // Non-pattern context.
            if ast::known::Block::try_from(ast).is_ok() {
                self.in_new_scope(|this| this.process_subtrees(ast))
            } else if self.try_recording_identifier(OccurrenceKind::Used, ast) {
                // Plain identifier: we just added as the condition side-effect.
                // No need to do anything more.
            } else {
                self.process_subtrees(ast);
            }
        }
    }

    fn process_definition(&mut self, definition: &DefinitionInfo) {
        // Handle the definition name.
        self.in_location(definition.name.crumbs.clone(), |this|
            // We take the base name (ignoring extension components) and mark it as introduced.
            this.in_location(definition.name.name.crumbs.clone(), |this| {
                let name = definition.name.name.item.clone();
                this.record_identifier(OccurrenceKind::Introduced,name);
            }));


        // The scoping for definitions is not entirely clean (should each argument introduce a new
        // subscope?) but we do not really care that much. Mostly we are just interested in knowing
        // what identifiers are taken in / introduced into the parent scope.
        // What happens in the definition body, stays in the definition body.
        self.in_new_scope(|this| {
            // Args are just patterns.
            this.in_context(Context::Pattern, |this| {
                for arg in &definition.args {
                    this.process_located_ast(arg)
                }
            });
            this.process_located_ast(&definition.body());
        });
    }

    /// Processes the assignment AST node. Left side is pattern, right side is business as usual.
    fn process_assignment(&mut self, assignment: &ast::known::Infix) {
        self.in_context(Context::Pattern, |this| {
            this.process_subtree_at(InfixCrumb::LeftOperand, &assignment.larg)
        });
        self.process_subtree_at(InfixCrumb::RightOperand, &assignment.rarg);
    }

    /// Processes the matched lambda macro. Argument is in pattern context, and the whole lambda is
    /// a new scope.
    fn process_lambda(&mut self, lambda: &ast::macros::LambdaInfo) {
        self.in_new_scope(|this| {
            this.in_context(Context::Pattern, |this| this.process_located_ast(&lambda.arg));
            this.process_located_ast(&lambda.body)
        })
    }
}

/// Describes identifiers that code represented by AST introduces into the graph and identifiers
/// from graph's scope that code uses. This logic serves as a base for connection discovery,
/// where ASTs are typically the node's ASTs.
pub fn analyze_ast(ast: &Ast) -> IdentifierUsage {
    let mut analyzer = AliasAnalyzer::new();
    analyzer.process_ast(ast);
    analyzer.root_scope.symbols
}

/// Describes variable usage within a given Ast-like crumbable entity.
pub fn analyze_crumbable(crumbable: &impl Crumbable) -> IdentifierUsage {
    let mut analyzer = AliasAnalyzer::default();
    analyzer.process_subtrees(crumbable);
    analyzer.root_scope.symbols
}



// =============
// === Tests ===
// =============

#[cfg(test)]
mod tests {
    use super::test_utils::*;
    use super::*;

    wasm_bindgen_test_configure!(run_in_browser);

    /// Checks if actual observed sequence of located identifiers matches the expected one.
    /// Expected identifiers are described as code spans in the node's text representation.
    fn validate_identifiers(
        name: impl Str,
        ast: &Ast,
        expected: Vec<Range<usize>>,
        actual: &[Located<String>],
    ) {
        let mut checker = IdentifierValidator::new(name, ast, expected);
        checker.validate_identifiers(actual);
    }

    /// Runs the test for the given test case description.
    fn run_case(parser: &parser_scala::Parser, case: Case) {
        debug!("\n===========================================================================\n");
        debug!("Case: {}", case.code);
        let ast = parser.parse_line_ast(&case.code).unwrap();
        let result = analyze_ast(&ast);
        debug!("Analysis results: {result:?}");
        validate_identifiers("introduced", &ast, case.expected_introduced, &result.introduced);
        validate_identifiers("used", &ast, case.expected_used, &result.used);
    }

    /// Runs the test for the test case expressed using markdown notation. See `Case` for details.
    fn run_markdown_case(parser: &parser_scala::Parser, marked_code: impl AsRef<str>) {
        debug!("Running test case for {}", marked_code.as_ref());
        let case = Case::from_markdown(marked_code.as_ref());
        run_case(parser, case)
    }

    #[wasm_bindgen_test]
    fn test_alias_analysis() {
        let parser = parser_scala::Parser::new_or_panic();
        let test_cases = [
            "»foo«",
            "«five» = 5",
            "»Five« = 5",
            "«foo» = »bar«",
            "«foo» = »foo« »+« »bar«",
            "«foo» = »Bar«",
            "5 = »Bar«",
            "«sum» = »a« »+« »b«",
            "»Point« «x» «u» = »point«",
            "«x» »,« «y» = »pair«",
            r"«inc» =
                »foo« »+« 1",
            r"«inc» =
                foo = 2
                foo »+« 1",
            // Below should know that "foo + 1" does not uses "foo" from scope.
            // That requires at least partial support for definitions.
            r"«inc» =
                foo x = 2
                foo »+« 1",
            // === Macros Match ===
            "a -> a",
            "a -> »b«",
            "»A« -> »b«",
            "a -> »A« -> a",
            "a -> a -> »A«",
            "x»,«y -> »B«",
            "x»,«y -> y",
            "x »,« »Y« -> _",
            "(»foo«)",
            "(«foo») = (»bar«)",
            "if »A« then »B«",
            "if »a« then »b« else »c«",
            "case »foo« of\n    »Number« a -> a\n    »Wildcard« -> »bar«\n    a»,«b -> a",
            // === Macros Ambiguous ===
            "(»foo«",
            "if »a«",
            "case »a«",
            // "->»a«", // TODO [mwu] restore (and implement) when parser is able to parse this
            // "a ->",  // TODO [mwu] restore (and implement) when parser is able to parse this

            // === Definition ===
            "«foo» a b c = »foo« a »d«",
            "«foo» a b c = d -> a d",
            "«foo» a (»Point« x y) c = »foo« a x »d«",
        ];
        for case in &test_cases {
            run_markdown_case(&parser, case)
        }
    }
}
