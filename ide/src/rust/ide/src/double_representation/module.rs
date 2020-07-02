//! Code for module-level double representation processing.

use crate::prelude::*;

use crate::double_representation::definition;
use crate::double_representation::definition::DefinitionProvider;

use ast::crumbs::ChildAst;
use ast::known;
use enso_protocol::language_server;



// ==============
// === Errors ===
// ==============

#[allow(missing_docs)]
#[derive(Fail,Clone,Debug)]
#[fail(display="Cannot find method with pointer {:?}.",_0)]
pub struct CannotFindMethod(language_server::MethodPointer);

#[allow(missing_docs)]
#[derive(Copy,Fail,Clone,Debug)]
#[fail(display="Encountered an empty definition ID. It must contain at least one crumb.")]
pub struct EmptyDefinitionId;



// ========================
// === Module Utilities ===
// ========================

/// Looks up graph in the module.
pub fn get_definition
(ast:&known::Module, id:&definition::Id) -> FallibleResult<definition::DefinitionInfo> {
    Ok(locate(ast, id)?.item)
}

/// Traverses the module's definition tree following the given Id crumbs, looking up the definition.
pub fn locate
(ast:&known::Module, id:&definition::Id) -> FallibleResult<definition::ChildDefinition> {
    let mut crumbs_iter = id.crumbs.iter();
    // Not exactly regular - we need special case for the first crumb as it is not a definition nor
    // a children. After this we can go just from one definition to another.
    let first_crumb = crumbs_iter.next().ok_or(EmptyDefinitionId)?;
    let mut child = ast.def_iter().find_by_name(&first_crumb)?;
    for crumb in crumbs_iter {
        child = definition::resolve_single_name(child,crumb)?;
    }
    Ok(child)
}

/// Get a definition ID that points to a method matching given pointer.
///
/// The module is assumed to be in the file identified by the `method.file` (for the purpose of
/// desugaring implicit extensions methods for modules).
pub fn lookup_method
(ast:&known::Module, method:&language_server::MethodPointer) -> FallibleResult<definition::Id> {
    let module_path = model::module::Path::from_file_path(method.file.clone())?;
    let explicitly_extends_looked_type = method.defined_on_type == module_path.module_name();

    for child in ast.def_iter() {
        let child_name : &definition::DefinitionName = &child.name.item;
        let name_matches = child_name.name.item == method.name;
        let type_matches = match child_name.extended_target.as_slice() {
            []         => explicitly_extends_looked_type,
            [typename] => typename.item == method.defined_on_type,
            _          => child_name.explicitly_extends_type(&method.defined_on_type),
        };
        if name_matches && type_matches {
            return Ok(definition::Id::new_single_crumb(child_name.clone()))
        }
    }

    Err(CannotFindMethod(method.clone()).into())
}

impl DefinitionProvider for known::Module {
    fn indent(&self) -> usize { 0 }

    fn scope_kind(&self) -> definition::ScopeKind { definition::ScopeKind::Root }

    fn enumerate_asts<'a>(&'a self) -> Box<dyn Iterator<Item = ChildAst<'a>>+'a> {
        self.ast().children()
    }
}



// ============
// === Test ===
// ============

#[cfg(test)]
mod tests {
    use super::*;

    use crate::double_representation::definition::DefinitionName;

    use enso_protocol::language_server::MethodPointer;
    use enso_protocol::language_server::Path;
    use wasm_bindgen_test::wasm_bindgen_test;

    wasm_bindgen_test::wasm_bindgen_test_configure!(run_in_browser);

    #[wasm_bindgen_test]
    fn implicit_method_resolution() {
        let parser = parser::Parser::new_or_panic();
        let foo_method = MethodPointer {
            defined_on_type : "Main".into(),
            file            : Path::new(default(),&["src","Main.enso"]),
            name            : "foo".into(),
        };

        let expect_find = |code,expected:definition::Id| {
            let module = parser.parse_module(code,default()).unwrap();
            let result = lookup_method(&module,&foo_method);
            assert_eq!(result.unwrap().to_string(),expected.to_string());

            // TODO [mwu]
            //  We should be able to use `assert_eq!(result.unwrap(),expected);`
            //  But we can't, because definition::Id uses located fields and crumbs won't match.
            //  Eventually we'll likely need to split definition names into located and unlocated
            //  ones. Definition ID should not require any location info.
        };

        let expect_not_found = |code| {
            let module = parser.parse_module(code,default()).unwrap();
            lookup_method(&module,&foo_method).expect_err("expected method not found");
        };

        // Implicit module extension method.
        let id = definition::Id::new_plain_name("foo");
        expect_find("foo a b = a + b", id);
        // Explicit module extension method
        let id = definition::Id::new_single_crumb(DefinitionName::new_method("Main","foo"));
        expect_find("Main.foo a b = a + b", id);

        expect_not_found("bar a b = a + b");
    }
}
