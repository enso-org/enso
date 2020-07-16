//! Code for module-level double representation processing.

use crate::prelude::*;

use crate::double_representation::definition;
use crate::double_representation::definition::DefinitionProvider;

use ast::crumbs::ChildAst;
use ast::crumbs::ModuleCrumb;
use ast::known;
use ast::BlockLine;
use enso_protocol::language_server;
use data::text::ByteIndex;


// =====================
// === QualifiedName ===
// =====================

#[allow(missing_docs)]
#[derive(Clone,Copy,Debug,Fail)]
pub enum InvalidQualifiedName {
    #[fail(display="No module segment in qualified name.")]
    NoModuleSegment,
}

/// Module's qualified name is used in some of the Language Server's APIs, like
/// `VisualisationConfiguration`.
///
/// Qualified name is constructed as follows:
/// `ProjectName.<directories_between_src_and_enso_file>.<file_without_ext>`
///
/// See https://dev.enso.org/docs/distribution/packaging.html for more information about the
/// package structure.
#[derive(Clone,Debug,Shrinkwrap)]
pub struct QualifiedName {
    #[shrinkwrap(main_field)]
    text      : String,
    name_part : Range<ByteIndex>,
}

impl QualifiedName {
    /// Build a module's full qualified name from its name segments and the project name.
    ///
    /// ```
    /// use ide::model::module::QualifiedName;
    ///
    /// let name = QualifiedName::from_segments("Project",&["Main"]).unwrap();
    /// assert_eq!(name.to_string(), "Project.Main");
    /// ```
    pub fn from_segments
    (project_name:impl Str, module_segments:impl IntoIterator<Item:AsRef<str>>)
    -> FallibleResult<QualifiedName> {
        let project_name     = std::iter::once(project_name.into());
        let module_segments  = module_segments.into_iter();
        let module_segments  = module_segments.map(|segment| segment.as_ref().to_string());
        let mut all_segments = project_name.chain(module_segments);
        let text             = all_segments.join(ast::opr::predefined::ACCESS);
        Ok(text.try_into()?)
    }

    /// Get the unqualified name of the module.
    pub fn name(&self) -> &str {
        &self.text[self.name_part.start.value..self.name_part.end.value]
    }
}

impl TryFrom<String> for QualifiedName {
    type Error = InvalidQualifiedName;

    fn try_from(text:String) -> Result<Self,Self::Error> {
        let error      = InvalidQualifiedName::NoModuleSegment;
        let name_start = text.rfind(ast::opr::predefined::ACCESS).ok_or(error)? + 1;
        let name_part  = ByteIndex::new(name_start)..ByteIndex::new(text.len());
        Ok(QualifiedName {text,name_part})
    }
}

impl Display for QualifiedName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(&self.text,f)
    }
}

impl PartialEq<QualifiedName> for QualifiedName {
    fn eq(&self,rhs:&QualifiedName) -> bool {
        self.text == rhs.text
    }
}

impl Eq for QualifiedName {}


// ==================
// === ImportInfo ===
// ==================

/// Representation of a single import declaration.
// TODO [mwu]
// Currently only supports the unqualified imports like `import Foo.Bar`. Qualified, restricted and
// and hiding imports are not supported by the parser yet. In future when parser and engine
// supports them, this structure should be adjusted as well.
#[derive(Clone,Debug,PartialEq)]
pub struct ImportInfo {
    /// The segments of the qualified name of the imported target.
    ///
    /// This field is not Qualified name to cover semantically illegal imports.
    pub target:Vec<String>
}

impl ImportInfo {
    /// Construct from a string describing an import target, like `"Foo.Bar"`.
    pub fn from_target_str(name:impl AsRef<str>) -> Self {
        let name   = name.as_ref().trim();
        let target = if name.is_empty() {
            Vec::new()
        } else {
            name.split(ast::opr::predefined::ACCESS).map(Into::into).collect()
        };
        ImportInfo {target}
    }

    /// Construct from a module qualified name like `"Foo.Bar"` that describes imported target.
    pub fn from_qualified_name(name:&QualifiedName) -> Self {
        Self::from_target_str(name.as_str())
    }

    /// Obtain the qualified name of the imported module.
    pub fn qualified_name(&self) -> FallibleResult<QualifiedName> {
        Ok(self.target.join(ast::opr::predefined::ACCESS).try_into()?)
    }

    /// Construct from an AST. Fails if the Ast is not an import declaration.
    pub fn from_ast(ast:&Ast) -> Option<Self> {
        let macro_match = known::Match::try_from(ast).ok()?;
        Self::from_match(macro_match)
    }

    /// Construct from a macro match AST. Fails if the Ast is not an import declaration.
    pub fn from_match(ast:known::Match) -> Option<Self> {
        ast::macros::is_match_import(&ast).then_with(|| {
            ImportInfo::from_target_str(ast.segs.head.body.repr().trim())
        })
    }
}

impl Display for ImportInfo {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let target = self.target.join(ast::opr::predefined::ACCESS);
        write!(f, "{} {}",ast::macros::IMPORT_KEYWORD,target)
    }
}



// ==============
// === Errors ===
// ==============

#[derive(Clone,Debug,Fail)]
#[fail(display="Import `{}` was not found in the module.",_0)]
#[allow(missing_docs)]
pub struct ImportNotFound(pub ImportInfo);

#[derive(Clone,Copy,Debug,Fail)]
#[fail(display="Line index is out of bounds.")]
#[allow(missing_docs)]
pub struct LineIndexOutOfBounds;



// ============
// === Info ===
// ============

/// Wrapper allowing getting information about the module and updating it.
#[derive(Clone,Debug)]
pub struct Info {
    #[allow(missing_docs)]
    pub ast:known::Module,
}

impl Info {
    /// Iterate over all lines in module that contain an import declaration.
    pub fn enumerate_imports<'a>(&'a self) -> impl Iterator<Item=(ModuleCrumb, ImportInfo)> + 'a {
        let children = self.ast.shape().enumerate();
        children.filter_map(|(crumb,ast)| Some((crumb,ImportInfo::from_ast(ast)?)))
    }

    /// Iterate over all import declarations in the module.
    ///
    /// If the caller wants to know *where* the declarations are, use `enumerate_imports`.
    pub fn iter_imports<'a>(&'a self) -> impl Iterator<Item=ImportInfo> + 'a {
        self.enumerate_imports().map(|(_,import)| import)
    }

    /// Add a new line to the module's block.
    ///
    /// Note that indices are the "module line" indices, which usually are quite different from text
    /// API line indices (because nested blocks doesn't count as separate "module lines").
    pub fn add_line(&mut self, index:usize, ast:Option<Ast>) {
        let line = BlockLine::new(ast);
        self.ast.update_shape(|shape| shape.lines.insert(index,line))
    }

    /// Remove line with given index.
    ///
    /// Returns removed line. Fails if the index is out of bounds.
    pub fn remove_line(&mut self, index:usize) -> FallibleResult<BlockLine<Option<Ast>>> {
        self.ast.update_shape(|shape| {
            shape.lines.try_remove(index).ok_or_else(|| LineIndexOutOfBounds.into())
        })
    }

    /// Remove a line that matches given import description.
    ///
    /// If there is more than one line matching, only the first one will be removed.
    /// Fails if there is no import matching given argument.
    pub fn remove_import(&mut self, to_remove:&ImportInfo) -> FallibleResult<()> {
        let lookup_result = self.enumerate_imports().find(|(_,import)| import == to_remove);
        let (crumb,_)     = lookup_result.ok_or_else(|| ImportNotFound(to_remove.clone()))?;
        self.remove_line(crumb.line_index)?;
        Ok(())
    }

    /// Add a new import declaration to a module.
    // TODO [mwu]
    //   Ideally we should not require parser but should use some sane way of generating AST from
    //   the `ImportInfo` value.
    pub fn add_import(&mut self, parser:&parser::Parser, to_add:ImportInfo) -> usize {
        // Find last import that is not "after" the added one lexicographically.
        let previous_import = self.enumerate_imports().take_while(|(_,import)| {
            to_add.target > import.target
        }).last();

        let index_to_place_at = previous_import.map_or(0,|(crumb,_)| crumb.line_index + 1);
        let import_ast        = parser.parse_line(to_add.to_string()).unwrap();
        self.add_line(index_to_place_at,Some(import_ast));
        index_to_place_at
    }

    #[cfg(test)]
    pub fn expect_code(&self,expected_code:impl AsRef<str>) {
        assert_eq!(self.ast.repr(),expected_code.as_ref());
    }
}



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
    fn import_listing() {
        let parser   = parser::Parser::new_or_panic();
        let expect_imports = |code:&str, expected:&[&[&str]]| {
            let ast      = parser.parse_module(code,default()).unwrap();
            let info = Info {ast};
            let imports = info.iter_imports().collect_vec();
            assert_eq!(imports.len(), expected.len());
            for (import,expected_segments) in imports.iter().zip(expected) {
                itertools::assert_equal(import.target.iter(),expected_segments.iter());
            }
        };

        expect_imports("import", &[&[]]);
        expect_imports("import Foo", &[&["Foo"]]);
        expect_imports("import Foo.Bar", &[&["Foo","Bar"]]);
        expect_imports("foo = bar\nimport Foo.Bar", &[&["Foo","Bar"]]);
        expect_imports("import Foo.Bar\nfoo=bar\nimport Foo.Bar", &[&["Foo","Bar"],&["Foo","Bar"]]);
    }

    #[wasm_bindgen_test]
    fn import_adding_and_removing() {
        let parser   = parser::Parser::new_or_panic();
        let code     = "import Foo.Bar.Baz";
        let ast      = parser.parse_module(code,default()).unwrap();
        let mut info = Info { ast };
        let import   = |code| {
            let ast = parser.parse_line(code).unwrap();
            ImportInfo::from_ast(&ast).unwrap()
        };

        info.add_import(&parser,import("import Bar.Gar"));
        info.expect_code("import Bar.Gar\nimport Foo.Bar.Baz");
        info.add_import(&parser,import("import Gar.Bar"));
        info.expect_code("import Bar.Gar\nimport Foo.Bar.Baz\nimport Gar.Bar");

        info.remove_import(&ImportInfo::from_target_str("Foo.Bar.Baz")).unwrap();
        info.expect_code("import Bar.Gar\nimport Gar.Bar");
        info.remove_import(&ImportInfo::from_target_str("Foo.Bar.Baz")).unwrap_err();
        info.expect_code("import Bar.Gar\nimport Gar.Bar");
        info.remove_import(&ImportInfo::from_target_str("Gar.Bar")).unwrap();
        info.expect_code("import Bar.Gar");
        info.remove_import(&ImportInfo::from_target_str("Bar.Gar")).unwrap();
        info.expect_code("");

        info.add_import(&parser,import("import Bar.Gar"));
        info.expect_code("import Bar.Gar");
    }

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
