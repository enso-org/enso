//! Code for definition discovery in the blocks, finding definition by name and related utilities.

use crate::prelude::*;

use ast::Ast;
use ast::HasRepr;
use ast::Shape;
use ast::known;
use ast::prefix;
use ast::opr;



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



// ==================
// === Identifier ===
// ==================

/// Checks if given Ast node can be used to represent identifier being part of definition name.
pub fn is_identifier(ast:&Ast) -> bool {
    match ast.shape() {
        Shape::Var          {..} => true,
        Shape::Cons         {..} => true,
        Shape::SectionSides {..} => true,
        Shape::Opr          {..} => true,
        _                        => false,
    }
}

/// Retrieves the identifier's name, if the Ast node is an identifier. Otherwise, returns None.
pub fn identifier_name(ast:&Ast) -> Option<String> {
    is_identifier(ast).then(ast.repr())
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
                let mut args = vec![identifier_name(&accessor_chain.target?)?];
                for arg in accessor_chain.args.iter() {
                    let arg_ast = arg.operand.as_ref()?;
                    args.push(identifier_name(arg_ast)?)
                }
                let name = args.pop()?;
                (args,name)
            }
            None => {
                (Vec::new(), identifier_name(ast)?)
            }
        };
        Some(DefinitionName {extended_target,name})
    }
}

impl ToString for DefinitionName {
    fn to_string(&self) -> String {
        let mut pieces = self.extended_target.iter().map(|s| s.as_str()).collect_vec();
        pieces.push(&self.name);
        pieces.join(opr::predefined::ACCESS)
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
    pub ast: known::Infix,
    /// Name of this definition. Includes typename, if this is an extension method.
    pub name: DefinitionName,
    /// Arguments for this definition. Does not include any implicit ones (e.g. no `this`).
    pub args: Vec<Ast>,
}

impl DefinitionInfo {
    /// Returns the definition body, i.e. Ast standing on the assignment's right-hand side.
    pub fn body(&self) -> Ast {
        self.ast.rarg.clone()
    }

    /// Tries to interpret `Line`'s contents as a function definition.
    pub fn from_line
    (line:&ast::BlockLine<Option<Ast>>, kind:ScopeKind) -> Option<DefinitionInfo> {
        let ast = line.elem.as_ref()?;
        Self::from_line_ast(ast,kind)
    }

    /// Tries to interpret `Line`'s `Ast` as a function definition.
    ///
    /// Assumes that the AST represents the contents of line (and not e.g. right-hand side of
    /// some binding or other kind of subtree).
    pub fn from_line_ast(ast:&Ast, kind:ScopeKind) -> Option<DefinitionInfo> {
        let infix  = opr::to_assignment(ast)?;
        // There two cases - function name is either a Var or operator.
        // If this is a Var, we have Var, optionally under a Prefix chain with args.
        // If this is an operator, we have SectionRight with (if any prefix in arguments).
        let lhs  = prefix::Chain::new_non_strict(&infix.larg);
        let name = DefinitionName::from_ast(&lhs.func)?;
        let args = lhs.args;
        let ret  = DefinitionInfo {ast:infix,name,args};

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



// ==========================
// === DefinitionProvider ===
// ==========================

/// An entity that contains lines that we want to interpret as definitions.
pub trait DefinitionProvider {
    /// What kind of scope this is.
    fn scope_kind() -> ScopeKind;

    /// Iterates over non-empty lines' ASTs.
    fn line_asts<'a>(&'a self) -> Box<dyn Iterator<Item=&'a Ast> + 'a>;

    /// Lists all the definitions in the entity.
    fn list_definitions(&self) -> Vec<DefinitionInfo> {
        self.line_asts().flat_map(|ast| {
            DefinitionInfo::from_line_ast(ast,Self::scope_kind())
        }).collect()
    }

    /// Tries to find definition by given name in the entity.
    fn find_definition(&self, name:&DefinitionName) -> Option<DefinitionInfo> {
        self.line_asts().find_map(|ast| {
            let definition = DefinitionInfo::from_line_ast(ast, Self::scope_kind())?;
            let matches    = &definition.name == name;
            matches.as_some(definition)
        })}
}

impl DefinitionProvider for known::Module {
    fn scope_kind() -> ScopeKind { ScopeKind::Root }
    fn line_asts<'a>(&'a self) -> Box<dyn Iterator<Item=&'a Ast> + 'a> {
        Box::new(self.iter())
    }
}

impl DefinitionProvider for known::Block {
    fn scope_kind() -> ScopeKind { ScopeKind::NonRoot }
    fn line_asts<'a>(&'a self) -> Box<dyn Iterator<Item=&'a Ast> + 'a> {
        Box::new(self.iter())
    }
}



// =============
// === Tests ===
// =============

#[cfg(test)]
mod tests {
    use super::*;
    use parser::api::IsParser;
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

    #[wasm_bindgen_test]
    fn list_definition_test() {
        let mut parser = parser::Parser::new_or_panic();

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

        // === Program with defnitions in root ===
        let program     = definition_lines.join("\n");
        let module      = parser.parse_module(program.into(), default()).unwrap();
        let definitions = module.list_definitions();
        assert_eq_strings(to_names(&definitions),expected_def_names_in_module);

        // Check that definition can be found and their body is properly described.
        let add_name = DefinitionName::new_plain("add");
        let add      = module.find_definition(&add_name).expect("failed to find `add` function");
        let body     = known::Number::try_new(add.body()).expect("add body should be a Block");
        assert_eq!(body.int,"50");

        // === Program with definition in `some_func`'s body `Block` ===
        let indented_lines = definition_lines.iter().map(indented).collect_vec();
        let program        = format!("some_func arg1 arg2 =\n{}", indented_lines.join("\n"));
        let module         = parser.parse_module(program,default()).unwrap();
        let root_defs      = module.list_definitions();
        let (only_def,)    = root_defs.expect_tuple();
        assert_eq!(&only_def.name.to_string(),"some_func");
        let body_block  = known::Block::try_from(only_def.body()).unwrap();
        let nested_defs = body_block.list_definitions();
        assert_eq_strings(to_names(&nested_defs),expected_def_names_in_def);
    }
}
