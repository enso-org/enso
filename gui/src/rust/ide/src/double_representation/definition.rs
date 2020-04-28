//! Code for definition discovery in the blocks, finding definition by name and related utilities.

use crate::prelude::*;

use ast::crumbs::ChildAst;
use ast::crumbs::Crumbable;
use ast::known;
use ast::prefix;
use ast::opr;



// =====================
// === Definition Id ===
// =====================

/// Crumb describes step that needs to be done when going from context (for graph being a module)
/// to the target.
// TODO [mwu]
//  Currently we support only entering named definitions.
pub type Crumb = DefinitionName;

/// Identifies graph in the module.
#[derive(Clone,Debug,Eq,Hash,PartialEq)]
pub struct Id {
    /// Sequence of traverses from module root up to the identified graph.
    pub crumbs : Vec<Crumb>,
}

impl Id {
    /// Creates a new graph identifier consisting of a single crumb.
    pub fn new_single_crumb(crumb:DefinitionName) -> Id {
        let crumbs = vec![crumb];
        Id {crumbs}
    }

    /// Creates a new identifier with a single plain name.
    pub fn new_plain_name(name:impl Str) -> Id {
        Self::new_plain_names(std::iter::once(name.into()))
    }

    /// Creates a new identifier from a sequence of plain definition names.
    pub fn new_plain_names<S>(names:impl IntoIterator<Item = S>) -> Id
        where S:ToString {
        let crumbs = names.into_iter().map(|name| {
            DefinitionName::new_plain(name.to_string())
        }).collect_vec();
        Id {crumbs}
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
        };
        Ok(())
    }
}



// ===============================
// === Finding Graph In Module ===
// ===============================

#[allow(missing_docs)]
#[derive(Fail,Clone,Debug)]
#[fail(display="Cannot find definition child by id {:?}.",_0)]
pub struct CannotFindChild(Crumb);

#[allow(missing_docs)]
#[derive(Copy,Fail,Clone,Debug)]
#[fail(display="Encountered an empty definition ID. They must contain at least one crumb.")]
pub struct EmptyDefinitionId;

/// Looks up graph in the module.
pub fn traverse_for_definition
(ast:&ast::known::Module, id:&Id) -> FallibleResult<DefinitionInfo> {
    Ok(locate(ast, id)?.item)
}

/// Traverses the given sequence of crumbs, looking up the definition by given ID.
pub fn locate(ast:&ast::known::Module, id:&Id) -> FallibleResult<ChildDefinition> {
    let mut crumbs_iter = id.crumbs.iter();
    // Not exactly regular - first crumb is a little special, because module is not a definition
    // nor a children.
    let first_crumb = crumbs_iter.next().ok_or(EmptyDefinitionId)?;
    let mut child = ast.def_iter().find_by_name(&first_crumb)?;
    for crumb in crumbs_iter {
        child = resolve_single_name(child,crumb)?;
    }
    Ok(child)
}



// =============
// === Error ===
// =============

#[derive(Fail,Debug)]
#[fail(display="Cannot set Block lines because no line with Some(Ast) was found. Block must have \
at least one non-empty line.")]
struct MissingLineWithAst;



// =================
// === ScopeKind ===
// =================

/// Describes the kind of code block (scope) to which definition can belong.
#[derive(Clone,Copy,Debug,PartialEq)]
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
#[derive(Clone,Debug,Eq,Hash,PartialEq)]
pub struct DefinitionName {
    /// Used when definition is an extension method. Then it stores the segments
    /// of the extended target type path.
    pub extended_target : Vec<String>,
    /// Name of the function itself.
    pub name : String,
}

impl DefinitionName {
    /// Creates a new name consisting of a single identifier, without any extension target.
    pub fn new_plain(name:impl Str) -> DefinitionName {
        DefinitionName {name:name.into(), extended_target:default()}
    }

    /// Tries describing given Ast piece as a definition name. Typically, passed Ast
    /// should be the binding's left-hand side.
    ///
    /// Returns `None` if is not name-like entity.
    pub fn from_ast(ast:&Ast) -> Option<DefinitionName> {
        let accessor_chain = opr::Chain::try_new_of(ast,opr::predefined::ACCESS);
        let (extended_target,name) = match accessor_chain {
            Some(accessor_chain) => {
                let mut args = vec![ast::identifier::name(&accessor_chain.target?.arg)?.clone()];
                for arg in accessor_chain.args.iter() {
                    let arg_ast = arg.operand.as_ref()?;
                    args.push(ast::identifier::name(&arg_ast.arg)?.clone())
                }
                let name = args.pop()?;
                (args,name)
            }
            None => {
                let name = match ast.shape() {
                    ast::Shape::Var         (var)   => Some(&var.name),
                    ast::Shape::Opr         (opr)   => Some(&opr.name),
                    ast::Shape::SectionSides(sides) => ast::identifier::name(&sides.opr),
                    // Shape::Cons is intentionally omitted.
                    // It serves to pattern-match, not as definition name.
                    _ => None
                }?;
                (Vec::new(), name.clone())
            }
        };
        Some(DefinitionName {extended_target,name})
    }
}

impl Display for DefinitionName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut pieces = self.extended_target.iter().map(|s| s.as_str()).collect_vec();
        pieces.push(&self.name);
        let text = pieces.join(opr::predefined::ACCESS);
        write!(f, "{}", text)
    }
}



// ======================
// === DefinitionInfo ===
// ======================

/// Information about definition binding.
#[derive(Clone,Debug)]
pub struct DefinitionInfo {
    /// The whole definition. It is an Infix shape with `=` operator. Its left-hand side is
    /// an App.
    pub ast:known::Infix,
    /// Name of this definition. Includes typename, if this is an extension method.
    pub name:DefinitionName,
    /// Arguments for this definition. Does not include any implicit ones (e.g. no `this`).
    pub args:Vec<Ast>,
    /// The absolute indentation of the code block that introduced this definition.
    pub context_indent:usize
}

impl DefinitionInfo {
    /// Returns the definition body, i.e. Ast standing on the assignment's right-hand side.
    pub fn body(&self) -> Ast {
        self.ast.rarg.clone()
    }

    /// Gets the definition block lines. If `body` is a `Block`, it returns its `BlockLine`s,
    /// concatenating `empty_lines`, `first_line` and `lines`, in this exact order. If `body` is
    /// `Infix`, it returns a single `BlockLine`.
    pub fn block_lines(&self) -> FallibleResult<Vec<ast::BlockLine<Option<Ast>>>> {
        if let Ok(block) = known::Block::try_from(self.body()) {
            Ok(block.all_lines())
        } else {
            let elem = Some(self.body());
            let off  = 0;
            Ok(vec![ast::BlockLine{elem,off}])
        }
    }

    /// Sets the definition block lines. `lines` must contain at least one non-empty line to
    /// succeed.
    pub fn set_block_lines
    (&mut self, mut lines:Vec<ast::BlockLine<Option<Ast>>>) -> FallibleResult<()> {
        // FIXME [mwu]
        //  This doesn't deal correctly with offsets, but I have no idea how it should behave,
        //  as the current parser's behavior and AST is inconsistent.
        //  Basically `empty_lines` currently use absolute offsets, while `BlockLines` use relative
        //  offsets. This is not desirable, as e.g. an empty line in the middle of block is not
        //  possible to express with the current AST (it won't round-trip).

        let indent          = self.context_indent + double_representation::INDENT;
        let mut empty_lines = Vec::new();
        let mut line        = lines.pop_front().ok_or(MissingLineWithAst)?;
        while let None = line.elem {
            empty_lines.push(line.off + indent);
            line = lines.pop_front().ok_or(MissingLineWithAst)?;
        }
        let elem       = line.elem.ok_or(MissingLineWithAst)?;
        let off        = line.off;
        let first_line = ast::BlockLine {elem,off};
        let is_orphan  = false;
        let ty         = ast::BlockType::Discontinuous {};
        let block      = ast::Block {empty_lines,first_line,lines,indent,is_orphan,ty};
        let body_ast   = Ast::new(block,None);
        self.set_body_ast(body_ast);
        Ok(())
    }

    /// Sets the definition body to expression with given Ast.
    pub fn set_body_ast(&mut self, expression:Ast) {
        self.ast.update_shape(|infix| {
            // Keep at least one space after `=` for inline expressions, so it
            // doesn't look ugly when converting from previous block definition body.
            match expression.shape() {
                ast::Shape::Block(_) => {}
                _                    => infix.roff = std::cmp::max(1,infix.roff),
            }
            infix.rarg = expression;
        })
    }

    /// Tries to interpret a root line (i.e. the AST being placed in a line directly in the module
    /// scope) as a definition.
    pub fn from_root_line(line:&ast::BlockLine<Option<Ast>>) -> Option<DefinitionInfo> {
        let indent = 0;
        Self::from_line_ast(line.elem.as_ref()?,ScopeKind::Root,indent)
    }

    /// Tries to interpret `Line`'s `Ast` as a function definition.
    ///
    /// Assumes that the AST represents the contents of line (and not e.g. right-hand side of
    /// some binding or other kind of subtree).
    pub fn from_line_ast
    (ast:&Ast, kind:ScopeKind, context_indent:usize) -> Option<DefinitionInfo> {
        let infix  = opr::to_assignment(ast)?;
        // There two cases - function name is either a Var or operator.
        // If this is a Var, we have Var, optionally under a Prefix chain with args.
        // If this is an operator, we have SectionRight with (if any prefix in arguments).
        let lhs  = prefix::Chain::new_non_strict(&infix.larg);
        let name = DefinitionName::from_ast(&lhs.func)?;
        let args = lhs.args.into_iter().map(|sast| sast.wrapped).collect_vec();
        let ret  = DefinitionInfo {ast:infix,name,args,context_indent};

        // Note [Scope Differences]
        if kind == ScopeKind::NonRoot {
            // 1. Not an extension method but setter.
            let is_setter = !ret.name.extended_target.is_empty();
            // 2. No explicit args -- this is a node, not a definition.
            let is_node = ret.args.is_empty();
            if is_setter || is_node {
                None
            } else {
                Some(ret)
            }
        } else {
            Some(ret)
        }
    }
}

// Note [Scope Differences]
// ========================
// When we are in definition scope (as opposed to global scope) certain patterns should not be
// considered to be function definitions. These are:
// 1. Expressions like "Int.x = …". In module, they'd be treated as extension methods. In
//    definition scope they are treated as accessor setters.
// 2. Expression like "foo = 5". In module, this is treated as method definition (with implicit
//    this parameter). In definition, this is just a node (evaluated expression).

/// Definition stored under some known crumbs path.
pub type ChildDefinition = ast::crumbs::Located<DefinitionInfo>;

/// Tries to add a new crumb to current path to obtain a deeper child.
/// Its crumbs will accumulate both current crumbs and the passed one.
pub fn resolve_single_name(def:ChildDefinition, id:&Crumb) -> FallibleResult<ChildDefinition> {
    let child = def.item.def_iter().find_by_name(id)?;
    Ok(def.into_descendant(child))
}



// ==========================
// === DefinitionIterator ===
// ==========================

/// Iterator that iterates over child definitions.
#[allow(missing_debug_implementations)]
pub struct DefinitionIterator<'a> {
    /// Iterator going over ASTs of potential child definitions.
    pub iterator   : Box<dyn Iterator<Item = ChildAst<'a>>+'a>,
    /// What kind of scope are we getting our ASTs from.
    pub scope_kind : ScopeKind,
    /// Absolute indentation of the child ASTs we iterate over.
    pub indent     : usize,
}

impl<'a> Iterator for DefinitionIterator<'a> {
    type Item = ChildDefinition;
    fn next(&mut self) -> Option<Self::Item> {
        let scope_kind = self.scope_kind;
        let indent     = self.indent;
        self.iterator.find_map(|ChildAst {item,crumbs}| {
            let definition_opt = DefinitionInfo::from_line_ast(item,scope_kind,indent);
            definition_opt.map(|def| ChildDefinition::new(crumbs,def))
        })
    }
}

impl<'a> DefinitionIterator<'a> {
    /// Yields vector of all child definition infos, discarding the crumbs.
    pub fn infos_vec(self) -> Vec<DefinitionInfo> {
        self.map(|child_def| child_def.item).collect_vec()
    }

    /// Looks up direct child definition by given name.
    pub fn find_by_name(mut self, name:&DefinitionName) -> Result<ChildDefinition,CannotFindChild> {
        let err = || CannotFindChild(name.clone());
        self.find(|child_def| &child_def.item.name == name).ok_or_else(err)
    }
}



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
    fn enumerate_asts<'a>(&'a self) -> Box<dyn Iterator<Item = ChildAst<'a>>+'a>;

    /// Returns a scope iterator allowing browsing definition provided under this provider.
    fn def_iter(&self) -> DefinitionIterator {
        let iterator   = self.enumerate_asts();
        let scope_kind = self.scope_kind();
        let indent     = self.indent();
        DefinitionIterator {iterator,scope_kind,indent}
    }
}

/// Enumerates all AST being a direct children of the given AST node.
pub fn ast_direct_children<'a>
(ast:&'a impl Crumbable) -> Box<dyn Iterator<Item = ChildAst<'a>>+'a> {
    let iter = ast.enumerate().map(|(crumb,ast)| {
        ChildAst::new(crumb,ast)
    });
    Box::new(iter)
}

impl DefinitionProvider for known::Module {
    fn indent(&self) -> usize { 0 }

    fn scope_kind(&self) -> ScopeKind { ScopeKind::Root }

    fn enumerate_asts<'a>(&'a self) -> Box<dyn Iterator<Item = ChildAst<'a>>+'a> {
        ast_direct_children(self.ast())
    }
}

impl DefinitionProvider for known::Block {
    fn indent(&self) -> usize { self.indent }

    fn scope_kind(&self) -> ScopeKind { ScopeKind::NonRoot }

    fn enumerate_asts<'a>(&'a self) -> Box<dyn Iterator<Item = ChildAst<'a>>+'a> {
        ast_direct_children(self.ast())
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
            _ => self.context_indent + double_representation::INDENT,
        }
    }

    fn scope_kind(&self) -> ScopeKind { ScopeKind::NonRoot }

    fn enumerate_asts<'a>(&'a self) -> Box<dyn Iterator<Item = ChildAst<'a>>+'a> {
        use ast::crumbs::Crumb;
        use ast::crumbs::InfixCrumb;
        match self.ast.rarg.shape() {
            ast::Shape::Block(_) => {
                let parent_crumb = Crumb::Infix(InfixCrumb::RightOperand);
                let rarg         = &self.ast.rarg;
                let iter         = rarg.enumerate().map(move |(crumb,ast)| {
                    let crumbs = vec![parent_crumb.clone(),crumb];
                    ChildAst::new(crumbs,ast)
                });
                Box::new(iter)
            }
            _ => Box::new(std::iter::empty())
        }
    }
}



// =============
// === Tests ===
// =============

#[cfg(test)]
mod tests {
    use super::*;

    use crate::double_representation::INDENT;

    use utils::test::ExpectTuple;
    use wasm_bindgen_test::wasm_bindgen_test;

    wasm_bindgen_test::wasm_bindgen_test_configure!(run_in_browser);

    fn assert_eq_strings(lhs:Vec<impl Str>, rhs:Vec<impl Str>) {
        let lhs = lhs.iter().map(|s| s.as_ref()).collect_vec();
        let rhs = rhs.iter().map(|s| s.as_ref()).collect_vec();
        assert_eq!(lhs,rhs)
    }

    fn to_names(defs:&Vec<DefinitionInfo>) -> Vec<String> {
        defs.iter().map(|def| def.name.to_string()).collect()
    }

    fn indented(line:impl Display) -> String {
        iformat!("    {line}")
    }

    #[test]
    fn match_is_not_definition() {
        let cons = Ast::cons("Foo");
        let arg  = Ast::number(5);
        let lhs  = Ast::prefix(cons, arg.clone());
        let rhs  = Ast::var("bar");
        let ast  = Ast::infix(lhs, "=", rhs.clone());

        // Not a definition, it is a pattern match/
        assert_eq!(ast.repr(), "Foo 5 = bar");
        let def_opt = DefinitionInfo::from_line_ast(&ast,ScopeKind::NonRoot,INDENT);
        assert!(def_opt.is_none());

        let var = Ast::var("foo");
        let lhs = Ast::prefix(var, arg);
        let ast = Ast::infix(lhs, "=", rhs);

        // Now it is a definition.
        assert_eq!(ast.repr(), "foo 5 = bar");
        let def_opt = DefinitionInfo::from_line_ast(&ast,ScopeKind::NonRoot,INDENT);
        assert!(def_opt.is_some());
    }

    #[wasm_bindgen_test]
    fn list_definition_test() {
        let parser = parser::Parser::new_or_panic();

        // TODO [mwu]
        //  Due to a parser bug, extension methods defining operators cannot be currently
        //  correctly recognized. When it is fixed, the following should be also supported
        //  and covered in test: `Int.+ a = _` and `Int.+ = _`.
        //  Issue link: https://github.com/luna/enso/issues/565
        let definition_lines = vec![
            "main = _",
            "Foo.Bar.foo = _",
            "Foo.Bar.baz a b = _",
            "+ = _",
            "bar = _",
            "add a b = 50",
            "* a b = _",
        ];
        let expected_def_names_in_module = vec![
            "main","Foo.Bar.foo","Foo.Bar.baz","+","bar","add","*"
        ];
        // In definition there are no extension methods nor arg-less definitions.
        let expected_def_names_in_def = vec!["add", "*"];

        // === Program with definitions in root ===
        let program     = definition_lines.join("\n");
        let module      = parser.parse_module(program, default()).unwrap();
        let definitions = module.def_iter().infos_vec();
        assert_eq_strings(to_names(&definitions),expected_def_names_in_module);

        // Check that definition can be found and their body is properly described.
        let add_name = DefinitionName::new_plain("add");
        let add      = module.def_iter().find_by_name(&add_name).expect("failed to find `add` function");
        let body     = known::Number::try_new(add.body()).expect("add body should be a Block");
        assert_eq!(body.int,"50");

        // === Program with definition in `some_func`'s body `Block` ===
        let indented_lines = definition_lines.iter().map(indented).collect_vec();
        let program        = format!("some_func arg1 arg2 =\n{}", indented_lines.join("\n"));
        let module         = parser.parse_module(program,default()).unwrap();
        let root_defs      = module.def_iter().infos_vec();
        let (only_def,)    = root_defs.expect_tuple();
        assert_eq!(&only_def.name.to_string(),"some_func");
        let body_block  = known::Block::try_from(only_def.body()).unwrap();
        let nested_defs = body_block.def_iter().infos_vec();
        assert_eq_strings(to_names(&nested_defs),expected_def_names_in_def);
    }

    #[wasm_bindgen_test]
    fn finding_root_definition() {
        let program_to_expected_main_pos = vec![
            ("main = bar",              0),
            ("\nmain = bar",            1),
            ("\n\nmain = bar",          2),
            ("foo = bar\nmain = bar",   1),
            ("foo = bar\n\nmain = bar", 2),
        ];

        let parser  = parser::Parser::new_or_panic();
        let main_id = Id::new_plain_name("main");
        for (program,expected_line_index) in program_to_expected_main_pos {
            let module   = parser.parse_module(program,default()).unwrap();
            let location = locate(&module, &main_id).unwrap();
            let (crumb,) = location.crumbs.expect_tuple();
            match crumb {
                ast::crumbs::Crumb::Module(m) => assert_eq!(m.line_index, expected_line_index),
                _                             => panic!("Expected module crumb, got: {:?}.", crumb)
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

        let module    = parser::Parser::new_or_panic().parse_module(program,default()).unwrap();
        let check_def = |id, expected_body| {
            let definition = traverse_for_definition(&module,&id).unwrap();
            assert_eq!(definition.body().repr(), expected_body);
        };
        let check_not_found = |id| {
            assert!(traverse_for_definition(&module,&id).is_err())
        };

        check_def(Id::new_plain_names(&["main","add"]), "a + b");
        check_def(Id::new_plain_names(&["main","baz"]), "\n        subbaz arg = 4");
        check_def(Id::new_plain_names(&["main","baz","subbaz"]), "4");

        // Node are not definitions
        check_not_found(Id::new_plain_names(&["main", "foo"]));
        check_not_found(Id::new_plain_names(&["main","baz2","subbaz2"]));
    }
}
