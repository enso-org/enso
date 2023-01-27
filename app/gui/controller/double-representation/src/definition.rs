//! Code for definition discovery in the blocks, finding definition by name and related utilities.

use crate::prelude::*;

use crate::LineKind;
use crate::INDENT;

use ast::crumbs::ChildAst;
use ast::crumbs::Crumbable;
use ast::crumbs::InfixCrumb;
use ast::crumbs::Located;
use ast::known;
use ast::opr;
use parser_scala::Parser;
use std::iter::FusedIterator;



// =====================
// === Definition Id ===
// =====================

#[allow(missing_docs)]
#[derive(Copy, Fail, Clone, Debug)]
#[fail(display = "Encountered an empty definition ID. It must contain at least one crumb.")]
pub struct EmptyDefinitionId;

/// Crumb describes step that needs to be done when going from context (for graph being a module)
/// to the target.
// TODO [mwu]
//  Currently we support only entering named definitions.
pub type Crumb = DefinitionName;

/// Identifies graph in the module.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Id {
    /// Sequence of traverses from module root up to the identified graph.
    pub crumbs: Vec<Crumb>,
}

impl Id {
    /// Creates a new graph identifier consisting of a single crumb.
    pub fn new_single_crumb(crumb: DefinitionName) -> Id {
        let crumbs = vec![crumb];
        Id { crumbs }
    }

    /// Creates a new identifier with a single plain name.
    pub fn new_plain_name(name: impl Str) -> Id {
        Self::new_plain_names(std::iter::once(name.into()))
    }

    /// Creates a new identifier from a sequence of plain definition names.
    pub fn new_plain_names<S>(names: impl IntoIterator<Item = S>) -> Id
    where S: ToString {
        let crumbs =
            names.into_iter().map(|name| DefinitionName::new_plain(name.to_string())).collect_vec();
        Id { crumbs }
    }
}

impl Display for Id {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut iter = self.crumbs.iter();
        if let Some(crumb) = iter.next() {
            write!(f, "{}", crumb)?
        }
        for crumb in iter {
            write!(f, "⮚{}", crumb)?
        }
        Ok(())
    }
}



// =============
// === Error ===
// =============

#[derive(Fail, Debug)]
#[fail(
    display = "Cannot set Block lines because no line with Some(Ast) was found. Block must have \
at least one non-empty line."
)]
struct MissingLineWithAst;

#[allow(missing_docs)]
#[derive(Fail, Clone, Debug)]
#[fail(display = "Cannot find definition child with id {:?}.", _0)]
pub struct CannotFindChild(Crumb);



// =================
// === ScopeKind ===
// =================

/// Describes the kind of code block (scope) to which definition can belong.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ScopeKind {
    /// Module scope is a file's top-level block.
    Root,
    /// Any other block, e.g. introduced as body of some definition binding.
    NonRoot,
}



// ======================
// === DefinitionName ===
// ======================

/// Structure representing definition name. If this is an extension method, extended type is
/// also included.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct DefinitionName {
    /// Name of the function itself.
    pub name:            Located<String>,
    /// Used when definition is an extension method. Then it stores the segments
    /// of the extended target type path.
    pub extended_target: Vec<Located<String>>,
}

impl DefinitionName {
    /// Creates a new name consisting of a single unqualified identifier (not an explicit extension
    /// method).
    pub fn new_plain(name: impl Into<String>) -> DefinitionName {
        let name = Located::new_root(name.into());
        DefinitionName { name, extended_target: default() }
    }

    /// Creates a new explicit extension method name.
    pub fn new_method(extended_atom: impl Str, name: impl Str) -> DefinitionName {
        let extended_atom = Located::new(InfixCrumb::LeftOperand, extended_atom.into());
        let name = Located::new(InfixCrumb::RightOperand, name.into());
        let extended_target = vec![extended_atom];
        DefinitionName { name, extended_target }
    }

    /// Tries describing given Ast piece as a definition name. Typically, passed Ast
    /// should be the binding's left-hand side.
    ///
    /// Returns `None` if is not name-like entity.
    pub fn from_ast(ast: &Ast) -> Option<DefinitionName> {
        let accessor_chain = opr::as_access_chain(ast);
        let (extended_target, name) = match accessor_chain {
            Some(accessor_chain) => {
                // Not really clear how the incomplete names should be supported. For now we just
                // reject them. When use-cases appear, this check might need to be relaxed.
                if !accessor_chain.all_operands_set() {
                    return None;
                }

                let mut pieces = Vec::new();
                for piece in accessor_chain.enumerate_non_empty_operands() {
                    let name = ast::identifier::name(&piece.item.arg)?.to_owned();
                    pieces.push(piece.map(|_| name));
                }

                let name = pieces.pop()?;
                (pieces, name)
            }
            None => {
                let name = match ast.shape() {
                    ast::Shape::Var(var) => Some(var.name.as_str()),
                    ast::Shape::Opr(opr) => Some(opr.name.as_str()),
                    ast::Shape::SectionSides(sides) => ast::identifier::name(&sides.opr),
                    // Shape::Cons is intentionally omitted.
                    // It serves to pattern-match, not as definition name.
                    _ => None,
                }?;
                let name = Located::new_root(name.to_owned());
                (Vec::new(), name)
            }
        };
        Some(DefinitionName { name, extended_target })
    }

    /// Iterate over name segments of this name, left to right.
    pub fn name_segments(&self) -> impl Iterator<Item = &str> {
        let path = self.extended_target.iter().map(|segment| segment.as_str());
        let last = std::iter::once(self.name.as_str());
        path.chain(last)
    }

    /// Get AST of this name.
    pub fn ast(&self, parser: &Parser) -> FallibleResult<Ast> {
        // We can't assume that string pieces we have are valid identifiers.
        // But neither this is our responsibility. If it parses, we can store it in the Ast.
        parser.parse_line_ast(self.to_string())
    }

    /// Checks if the given definition name is a method defined on given expected atom name.
    ///
    /// E.g. `Main.foo` is a method of `Main`. Also, if `Main` is a name of module, than root-scoped
    /// definition named `foo` will be treated as an extension method on `Main`. To handle such case
    ///  properly, function takes `parent_name` - it is a name of module or type where the
    /// definition is located.
    pub fn method_of(&self, parent_name: &str, expected_atom: &str) -> bool {
        if self.extended_target.is_empty() {
            parent_name == expected_atom
        } else {
            self.explicitly_extends_type(expected_atom)
        }
    }

    /// Check if this name is an explicit extension method for given atom.
    ///
    /// For example `Int.add` is an extension method for `Int`, whereas plain name `add` is not.
    pub fn explicitly_extends_type(&self, expected_typename: &str) -> bool {
        let expected_segments = ast::opr::name_segments(expected_typename);
        let segments = &self.extended_target;
        expected_segments.eq_by(segments, |lhs, rhs| lhs == rhs.item.as_str())
    }
}

impl Display for DefinitionName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let text = self.name_segments().join(ast::opr::predefined::ACCESS);
        write!(f, "{}", text)
    }
}



// ======================
// === DefinitionInfo ===
// ======================

/// Information about definition binding.
#[derive(Clone, Debug)]
pub struct DefinitionInfo {
    /// The whole definition. It is an Infix shape with `=` operator. Its left-hand side is
    /// an App.
    pub ast:            known::Infix,
    /// Name of this definition. Includes typename, if this is an extension method.
    pub name:           Located<DefinitionName>,
    /// Arguments for this definition. Does not include any implicit ones (e.g. no `this`).
    pub args:           Vec<Located<Ast>>,
    /// The absolute indentation of the code block that introduced this definition.
    pub context_indent: usize,
}

impl DefinitionInfo {
    /// Returns the definition body, i.e. Ast standing on the assignment's right-hand side.
    pub fn body(&self) -> Located<&Ast> {
        Located::new(InfixCrumb::RightOperand, &self.ast.rarg)
    }

    /// Gets the definition block lines. If `body` is a `Block`, it returns its `BlockLine`s,
    /// concatenating `empty_lines`, `first_line` and `lines`, in this exact order. If `body` is
    /// `Infix`, it returns a single `BlockLine`.
    pub fn block_lines(&self) -> Vec<ast::BlockLine<Option<Ast>>> {
        if let Ok(block) = known::Block::try_from(*self.body()) {
            block.iter_all_lines().map(|line| line.map_opt(CloneRef::clone_ref)).collect()
        } else {
            let elem = Some((*self.body()).clone());
            let off = 0;
            vec![ast::BlockLine { elem, off }]
        }
    }

    /// Sets the definition block lines. `lines` must contain at least one non-empty line to
    /// succeed.
    pub fn set_block_lines(
        &mut self,
        mut lines: Vec<ast::BlockLine<Option<Ast>>>,
    ) -> FallibleResult {
        // FIXME [mwu]
        //  This doesn't deal correctly with offsets, but I have no idea how it should behave,
        //  as the current parser's behavior and AST is inconsistent.
        //  Basically `empty_lines` currently use absolute offsets, while `BlockLines` use relative
        //  offsets. This is not desirable, as e.g. an empty line in the middle of block is not
        //  possible to express with the current AST (it won't round-trip).

        let indent = self.indent();
        let mut empty_lines = Vec::new();
        let mut line = lines.pop_front().ok_or(MissingLineWithAst)?;
        while line.elem.is_none() {
            empty_lines.push(line.off + indent);
            line = lines.pop_front().ok_or(MissingLineWithAst)?;
        }
        let elem = line.elem.ok_or(MissingLineWithAst)?;
        let off = line.off;
        let first_line = ast::BlockLine { elem, off };
        let is_orphan = false;
        let ty = ast::BlockType::Discontinuous {};
        let block = ast::Block { ty, indent, empty_lines, first_line, lines, is_orphan };
        let body_ast = Ast::new(block, None);
        self.set_body_ast(body_ast);
        Ok(())
    }

    /// Sets the definition body to expression with given Ast.
    pub fn set_body_ast(&mut self, expression: Ast) {
        self.ast.update_shape(|infix| {
            // Keep at least one space after `=` for inline expressions, so it
            // doesn't look ugly when converting from previous block definition body.
            match expression.shape() {
                ast::Shape::Block(_) => {}
                _ => infix.roff = std::cmp::max(1, infix.roff),
            }
            infix.rarg = expression;
        })
    }

    /// Tries to interpret a root line (i.e. the AST being placed in a line directly in the module
    /// scope) as a definition.
    pub fn from_root_line(line: &ast::BlockLine<Option<Ast>>) -> Option<DefinitionInfo> {
        Self::from_root_line_ast(line.elem.as_ref()?)
    }

    /// Tries to interpret a root line's AST as a definition.
    pub fn from_root_line_ast(ast: &Ast) -> Option<DefinitionInfo> {
        let indent = 0;
        Self::from_line_ast(ast, ScopeKind::Root, indent)
    }

    /// Tries to interpret `Line`'s `Ast` as a function definition.
    ///
    /// Assumes that the AST represents the contents of line (and not e.g. right-hand side of
    /// some binding or other kind of subtree).
    pub fn from_line_ast(
        ast: &Ast,
        kind: ScopeKind,
        context_indent: usize,
    ) -> Option<DefinitionInfo> {
        if let LineKind::Definition { ast, args, name } = LineKind::discern(ast, kind) {
            Some(DefinitionInfo { ast, name, args, context_indent })
        } else {
            None
        }
    }
}

/// Definition stored under some known crumbs path.
pub type ChildDefinition = Located<DefinitionInfo>;

/// Tries to add a new crumb to current path to obtain a deeper child.
/// Its crumbs will accumulate both current crumbs and the passed one.
pub fn resolve_single_name(def: ChildDefinition, id: &Crumb) -> FallibleResult<ChildDefinition> {
    let child = def.item.def_iter().find_by_name(id)?;
    Ok(def.into_descendant(child))
}



// =================
// === Iterators ===
// =================

// === DefinitionIterator ===

/// Iterator that iterates over child definitions.
#[allow(missing_debug_implementations)]
pub struct DefinitionIterator<'a> {
    /// Iterator going over ASTs of potential child definitions.
    pub iterator:   Box<dyn Iterator<Item = ChildAst<'a>> + 'a>,
    /// What kind of scope are we getting our ASTs from.
    pub scope_kind: ScopeKind,
    /// Absolute indentation of the child ASTs we iterate over.
    pub indent:     usize,
}

impl<'a> Iterator for DefinitionIterator<'a> {
    type Item = ChildDefinition;
    fn next(&mut self) -> Option<Self::Item> {
        let scope_kind = self.scope_kind;
        let indent = self.indent;
        self.iterator.find_map(|ChildAst { item, crumbs }| {
            let definition_opt = DefinitionInfo::from_line_ast(item, scope_kind, indent);
            definition_opt.map(|def| ChildDefinition::new(crumbs, def))
        })
    }
}

impl<'a> DefinitionIterator<'a> {
    /// Yields vector of all child definition infos, discarding the crumbs.
    pub fn infos_vec(self) -> Vec<DefinitionInfo> {
        self.map(|child_def| child_def.item).collect_vec()
    }

    /// Looks up direct child definition by given name.
    pub fn find_by_name(
        mut self,
        name: &DefinitionName,
    ) -> Result<ChildDefinition, CannotFindChild> {
        let err = || CannotFindChild(name.clone());
        self.find(|child_def| &*child_def.item.name == name).ok_or_else(err)
    }
}


// === RecursiveDefinitionIterator ===

/// An recursive iterator over child definitions, returned by
/// [`DefinitionProvider::recursive_def_iter`].
#[derive(Clone, Debug, Default)]
pub struct RecursiveDefinitionIterator {
    stack: Vec<ChildDefinition>,
}

impl Iterator for RecursiveDefinitionIterator {
    type Item = ChildDefinition;

    fn next(&mut self) -> Option<Self::Item> {
        let next_item = self.stack.pop();
        if let Some(some_item) = &next_item {
            self.stack.extend(some_item.def_iter())
        }
        next_item
    }
}

impl FusedIterator for RecursiveDefinitionIterator {}


// ==========================
// === DefinitionProvider ===
// ==========================



/// An entity that contains lines that we want to interpret as definitions.
pub trait DefinitionProvider {
    /// Absolute indentation level of the scope of this provider's body.
    /// (i.e. the indents of the child definitions)
    fn indent(&self) -> usize;

    /// What kind of scope this is.
    fn scope_kind(&self) -> ScopeKind;

    /// Iterator going over all line-like Ast's that can hold a child definition.
    fn enumerate_asts<'a>(&'a self) -> Box<dyn Iterator<Item = ChildAst<'a>> + 'a>;

    /// Returns a scope iterator allowing browsing definition provided under this provider.
    fn def_iter(&self) -> DefinitionIterator {
        let iterator = self.enumerate_asts();
        let scope_kind = self.scope_kind();
        let indent = self.indent();
        DefinitionIterator { iterator, scope_kind, indent }
    }

    /// Returns an iterator iterating recursively over definitions provided by this provider and
    /// their nested definitions (as [`ChildDefinition`] is also a provider).
    fn recursive_def_iter(&self) -> RecursiveDefinitionIterator {
        RecursiveDefinitionIterator { stack: self.def_iter().collect() }
    }
}

impl DefinitionProvider for known::Block {
    fn indent(&self) -> usize {
        self.indent
    }

    fn scope_kind(&self) -> ScopeKind {
        ScopeKind::NonRoot
    }

    fn enumerate_asts<'a>(&'a self) -> Box<dyn Iterator<Item = ChildAst<'a>> + 'a> {
        self.ast().children()
    }
}

impl DefinitionProvider for DefinitionInfo {
    fn indent(&self) -> usize {
        match self.ast.rarg.shape() {
            ast::Shape::Block(block) => block.indent,
            // If definition has no block of its own, it does not introduce any children and
            // returned value here is not used anywhere currently. Might matter in the future,
            // when we deal with lambdas. Anyway, whatever block we might introduce, it should
            // be more indented than our current context.
            _ => self.context_indent + INDENT,
        }
    }

    fn scope_kind(&self) -> ScopeKind {
        ScopeKind::NonRoot
    }

    fn enumerate_asts<'a>(&'a self) -> Box<dyn Iterator<Item = ChildAst<'a>> + 'a> {
        use ast::crumbs::Crumb;
        match self.ast.rarg.shape() {
            ast::Shape::Block(_) => {
                let parent_crumb = Crumb::Infix(InfixCrumb::RightOperand);
                let rarg = &self.ast.rarg;
                let iter = rarg.enumerate().map(move |(crumb, ast)| {
                    let crumbs = vec![parent_crumb.clone(), crumb];
                    ChildAst::new(crumbs, ast)
                });
                Box::new(iter)
            }
            _ => Box::new(std::iter::empty()),
        }
    }
}

impl DefinitionProvider for ChildDefinition {
    fn indent(&self) -> usize {
        self.item.indent()
    }

    fn scope_kind(&self) -> ScopeKind {
        self.item.scope_kind()
    }

    fn enumerate_asts<'a>(&'a self) -> Box<dyn Iterator<Item = ChildAst<'a>> + 'a> {
        Box::new(
            self.item
                .enumerate_asts()
                .map(|child_ast| self.descendant(child_ast.crumbs, child_ast.item)),
        )
    }
}



// =============
// === ToAdd ===
// =============

/// Describes a definition to be created.
///
/// Type meant to be easily constructable in the code and be translated into AST.
/// Information-wise it corresponds to DefinitionInfo.
#[derive(Clone, Debug)]
pub struct ToAdd {
    /// The name of the introduced definition. May represent plain identifier or an extension.
    /// E.g. `add` or `Int.add`.
    pub name:                     DefinitionName,
    /// Names of explicit parameters. `this` parameter must not be included.
    pub explicit_parameter_names: Vec<String>,
    /// The first non-empty line of the definition body.
    pub body_head:                Ast,
    /// Further definition body lines. `None` represents an empty line.
    pub body_tail:                Vec<Option<Ast>>,
}

impl ToAdd {
    /// Create ToAdd description from an arbitrary AST.
    pub fn new_with_body(
        name: DefinitionName,
        explicit_parameter_names: Vec<String>,
        body: Ast,
    ) -> Self {
        let (body_head, body_tail) = match body.shape() {
            ast::Shape::Block(ast::Block { first_line, lines, .. }) => (
                first_line.elem.clone_ref(),
                lines.iter().map(|line| line.elem.as_ref().cloned()).collect_vec(),
            ),
            _ => (body.clone_ref(), default()),
        };
        ToAdd { name, explicit_parameter_names, body_head, body_tail }
    }

    /// The definition's head, i.e. the left-hand side of the primary assignment.
    pub fn head(&self, parser: &Parser) -> FallibleResult<Ast> {
        let name = self.name.ast(parser)?;
        let args = self.explicit_parameter_names.iter().map(Ast::var);
        let head = ast::prefix::Chain::new(name, args).into_ast();
        Ok(head)
    }

    /// The definition's body, i.e. the right-hand side of the primary assignment.
    pub fn body(&self, scope_indent: usize) -> Ast {
        // Assignment always must be in a block
        if self.body_tail.is_empty() && !ast::opr::is_assignment(&self.body_head) {
            self.body_head.clone_ref()
        } else {
            let mut block = ast::Block::from_lines(&self.body_head, &self.body_tail);
            block.indent = scope_indent + INDENT;
            Ast::from(block)
        }
    }

    /// Generate the definition's Ast from the description.
    pub fn ast(&self, scope_indent: usize, parser: &Parser) -> FallibleResult<Ast> {
        let body = self.body(scope_indent);
        let body_is_block = matches!(body.shape(), ast::Shape::Block { .. });
        let infix_shape = ast::Infix {
            larg: self.head(parser)?,
            loff: 1,
            opr:  Ast::opr(ast::opr::predefined::ASSIGNMENT),
            roff: if body_is_block { 0 } else { 1 },
            rarg: body,
        };
        let ast = Ast::from(infix_shape);
        Ok(ast)
    }
}



// =============
// === Tests ===
// =============

#[cfg(test)]
mod tests {
    use super::*;

    use crate::module;
    use crate::INDENT;

    use wasm_bindgen_test::wasm_bindgen_test;

    wasm_bindgen_test::wasm_bindgen_test_configure!(run_in_browser);

    fn assert_eq_strings(lhs: Vec<impl Str>, rhs: Vec<impl Str>) {
        let lhs = lhs.iter().map(|s| s.as_ref()).collect_vec();
        let rhs = rhs.iter().map(|s| s.as_ref()).collect_vec();
        assert_eq!(lhs, rhs)
    }

    fn to_names(defs: &[DefinitionInfo]) -> Vec<String> {
        defs.iter().map(|def| def.name.to_string()).collect()
    }

    fn indented(line: impl Display) -> String {
        format!("    {line}")
    }

    #[wasm_bindgen_test]
    fn generating_definition_to_add() {
        let parser = Parser::new_or_panic();
        let mut to_add = ToAdd {
            name:                     DefinitionName::new_method("Main", "add"),
            explicit_parameter_names: vec!["arg1".into(), "arg2".into()],
            body_head:                Ast::infix_var("ret", "=", "arg2"),
            body_tail:                default(),
        };

        // First, if we generate definition with single line and it is assignment,
        // it should be placed in a block of its own.
        let ast = to_add.ast(4, &parser).unwrap();
        assert_eq!(ast.repr(), "Main.add arg1 arg2 =\n        ret = arg2");

        // Now the single line body will be non-assignment, so it will be safe to place inline.
        to_add.body_head = Ast::infix_var("arg1", "+", "arg2");
        let ast = to_add.ast(4, &parser).unwrap();
        assert_eq!(ast.repr(), "Main.add arg1 arg2 = arg1 + arg2");

        // Having more than a single line always requires a block.
        to_add.body_tail.push(Some(Ast::infix_var("arg1", "-", "arg2")));
        let ast = to_add.ast(4, &parser).unwrap();
        // Note 8 spaces indents for definition block lines (as the parent scope was at 4).
        // Also, note that there is no space after the definition's assignment operator.
        assert_eq!(ast.repr(), "Main.add arg1 arg2 =\n        arg1 + arg2\n        arg1 - arg2");
    }

    #[wasm_bindgen_test]
    fn definition_name_tests() {
        let parser = parser_scala::Parser::new_or_panic();
        let ast = parser.parse_line_ast("Foo.Bar.baz").unwrap();
        let name = DefinitionName::from_ast(&ast).unwrap();

        assert_eq!(*name.name, "baz");
        assert_eq!(name.extended_target[0].as_str(), "Foo");
        assert_eq!(name.extended_target[1].as_str(), "Bar");

        assert_eq!(ast.get_traversing(&name.name.crumbs).unwrap().repr(), "baz");
        assert_eq!(ast.get_traversing(&name.extended_target[0].crumbs).unwrap().repr(), "Foo");
        assert_eq!(ast.get_traversing(&name.extended_target[1].crumbs).unwrap().repr(), "Bar");
    }

    #[wasm_bindgen_test]
    fn definition_name_rejecting_incomplete_names() {
        let parser = parser_scala::Parser::new_or_panic();
        let ast = parser.parse_line_ast("Foo. .baz").unwrap();
        assert!(DefinitionName::from_ast(&ast).is_none());
    }

    #[wasm_bindgen_test]
    fn definition_info_name() {
        let parser = parser_scala::Parser::new_or_panic();
        let ast = parser.parse_line_ast("Foo.bar a b c = baz").unwrap();
        let definition = DefinitionInfo::from_root_line_ast(&ast).unwrap();

        assert_eq!(definition.name.to_string(), "Foo.bar");
        assert_eq!(ast.get_traversing(&definition.name.crumbs).unwrap().repr(), "Foo.bar");
    }

    #[wasm_bindgen_test]
    fn located_definition_args() {
        let parser = parser_scala::Parser::new_or_panic();
        let ast = parser.parse_line_ast("foo bar baz = a + b + c").unwrap();
        let definition = DefinitionInfo::from_root_line_ast(&ast).unwrap();
        let (arg0, arg1) = definition.args.expect_tuple();

        use ast::crumbs;
        use ast::crumbs::InfixCrumb::*;
        use ast::crumbs::PrefixCrumb::*;
        assert_eq!(arg0.crumbs, crumbs![LeftOperand, Func, Arg]);
        assert_eq!(arg1.crumbs, crumbs![LeftOperand, Arg]);

        assert_eq!(arg0.item.repr(), "bar");
        assert_eq!(ast.get_traversing(&arg0.crumbs).unwrap(), &arg0.item);
        assert_eq!(arg1.item.repr(), "baz");
        assert_eq!(ast.get_traversing(&arg1.crumbs).unwrap(), &arg1.item);
    }

    #[wasm_bindgen_test]
    fn match_is_not_definition() {
        let cons = Ast::cons("Foo");
        let arg = Ast::number(5);
        let lhs = Ast::prefix(cons, arg.clone());
        let rhs = Ast::var("bar");
        let ast = Ast::infix(lhs, "=", rhs.clone());

        // Not a definition, it is a pattern match/
        assert_eq!(ast.repr(), "Foo 5 = bar");
        let def_opt = DefinitionInfo::from_line_ast(&ast, ScopeKind::NonRoot, INDENT);
        assert!(def_opt.is_none());

        let var = Ast::var("foo");
        let lhs = Ast::prefix(var, arg);
        let ast = Ast::infix(lhs, "=", rhs);

        // Now it is a definition.
        assert_eq!(ast.repr(), "foo 5 = bar");
        let def_opt = DefinitionInfo::from_line_ast(&ast, ScopeKind::NonRoot, INDENT);
        assert!(def_opt.is_some());
    }

    #[wasm_bindgen_test]
    fn list_definition_test() {
        let parser = parser_scala::Parser::new_or_panic();

        // TODO [mwu]
        //  Due to a parser bug, extension methods defining operators cannot be currently
        //  correctly recognized. When it is fixed, the following should be also supported
        //  and covered in test: `Int.+ a = _` and `Int.+ = _`.
        //  Issue link: https://github.com/enso-org/enso/issues/565
        let definition_lines = vec![
            "main = _",
            "Foo.Bar.foo = _",
            "Foo.Bar.baz a b = _",
            "+ = _",
            "bar = _",
            "add a b = 50",
            "* a b = _",
        ];
        let expected_def_names_in_module =
            vec!["main", "Foo.Bar.foo", "Foo.Bar.baz", "+", "bar", "add", "*"];
        // In definition there are no extension methods nor arg-less definitions.
        let expected_def_names_in_def = vec!["add", "*"];

        // === Program with definitions in root ===
        let program = definition_lines.join("\n");
        let module = parser.parse_module(program, default()).unwrap();
        let definitions = module.def_iter().infos_vec();
        assert_eq_strings(to_names(&definitions), expected_def_names_in_module);

        // Check that definition can be found and their body is properly described.
        let add_name = DefinitionName::new_plain("add");
        let add = module.def_iter().find_by_name(&add_name).expect("failed to find `add` function");
        let body = known::Number::try_from(*add.body()).expect("add body should be a Block");
        assert_eq!(body.int, "50");

        // === Program with definition in `some_func`'s body `Block` ===
        let indented_lines = definition_lines.iter().map(indented).collect_vec();
        let program = format!("some_func arg1 arg2 =\n{}", indented_lines.join("\n"));
        let module = parser.parse_module(program, default()).unwrap();
        let root_defs = module.def_iter().infos_vec();
        let (only_def,) = root_defs.expect_tuple();
        assert_eq!(&only_def.name.to_string(), "some_func");
        let body_block = known::Block::try_from(*only_def.body()).unwrap();
        let nested_defs = body_block.def_iter().infos_vec();
        assert_eq_strings(to_names(&nested_defs), expected_def_names_in_def);
    }

    #[wasm_bindgen_test]
    fn finding_root_definition() {
        let program_to_expected_main_pos = vec![
            ("main = bar", 0),
            ("\nmain = bar", 1),
            ("\n\nmain = bar", 2),
            ("foo = bar\nmain = bar", 1),
            ("foo = bar\n\nmain = bar", 2),
        ];

        let parser = parser_scala::Parser::new_or_panic();
        let main_id = Id::new_plain_name("main");
        for (program, expected_line_index) in program_to_expected_main_pos {
            let module = parser.parse_module(program, default()).unwrap();
            let location = module::locate(&module, &main_id).unwrap();
            let (crumb,) = location.crumbs.expect_tuple();
            match crumb {
                ast::crumbs::Crumb::Module(m) => assert_eq!(m.line_index, expected_line_index),
                _ => panic!("Expected module crumb, got: {:?}.", crumb),
            }
        }
    }

    #[wasm_bindgen_test]
    fn getting_nested_definition() {
        let program = r"
main =
    foo = 2
    add a b = a + b
    baz arg =
        subbaz arg = 4
    baz2 arg =
        subbaz2 = 4

    add foo bar";

        let module = parser_scala::Parser::new_or_panic().parse_module(program, default()).unwrap();
        let check_def = |id, expected_body| {
            let definition = module::get_definition(&module, &id).unwrap();
            assert_eq!(definition.body().repr(), expected_body);
        };
        let check_not_found = |id| assert!(module::get_definition(&module, &id).is_err());

        check_def(Id::new_plain_names(["main", "add"]), "a + b");
        check_def(Id::new_plain_names(["main", "baz"]), "\n        subbaz arg = 4");
        check_def(Id::new_plain_names(["main", "baz", "subbaz"]), "4");

        // Node are not definitions
        check_not_found(Id::new_plain_names(["main", "foo"]));
        check_not_found(Id::new_plain_names(["main", "baz2", "subbaz2"]));
    }
}
