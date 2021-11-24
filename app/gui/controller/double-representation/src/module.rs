//! Code for module-level double representation processing.

use crate::prelude::*;

use crate::alias_analysis;
use crate::definition;
use crate::definition::DefinitionProvider;
use crate::identifier;
use crate::identifier::Identifier;
use crate::identifier::LocatedName;
use crate::identifier::ReferentName;
use crate::project;
use crate::tp;

use ast::constants::keywords::HERE;
use ast::constants::PROJECTS_MAIN_MODULE;
use ast::crumbs::ChildAst;
use ast::crumbs::ModuleCrumb;
use ast::known;
use ast::BlockLine;
use engine_protocol::language_server;
use enso_text::unit::*;
use serde::Deserialize;
use serde::Serialize;



// ==============
// === Errors ===
// ==============

#[derive(Clone, Debug, Fail)]
#[fail(display = "Import `{}` was not found in the module.", _0)]
#[allow(missing_docs)]
pub struct ImportNotFound(pub ImportInfo);

#[derive(Clone, Copy, Debug, Fail)]
#[fail(display = "Line index is out of bounds.")]
#[allow(missing_docs)]
pub struct LineIndexOutOfBounds;

#[allow(missing_docs)]
#[derive(Clone, Copy, Debug, Fail)]
pub enum InvalidQualifiedName {
    #[fail(display = "No module segment in module's qualified name.")]
    NoModuleSegment,
}

#[allow(missing_docs)]
#[derive(Fail, Clone, Debug)]
#[fail(display = "Cannot find method with pointer {:?}.", _0)]
pub struct CannotFindMethod(language_server::MethodPointer);

#[allow(missing_docs)]
#[derive(Copy, Fail, Clone, Debug)]
#[fail(display = "Encountered an empty definition ID. It must contain at least one crumb.")]
pub struct EmptyDefinitionId;

#[allow(missing_docs)]
#[derive(Fail, Clone, Debug)]
#[fail(display = "The definition with crumbs {:?} is not a direct child of the module.", _0)]
pub struct NotDirectChild(ast::Crumbs);



// ==========
// === Id ===
// ==========

/// The segments of module name. Allow finding module in the project.
///
/// Example: `["Parent","Module_Name"]`
///
/// Includes segments of module path but *NOT* the project name (see: `QualifiedName`).
#[derive(Clone, Debug, Shrinkwrap, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Id {
    /// The vector can be empty - in that case we point to the module called `Main`.
    segments: Vec<ReferentName>,
}

impl Display for Id {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = self.segments.iter().map(AsRef::<str>::as_ref).join(".");
        write!(f, "{}", s)
    }
}

impl Id {
    /// Construct a module's ID value from a name segments sequence.
    ///
    /// Fails if the given sequence is empty.
    pub fn new(segments: impl IntoIterator<Item = ReferentName>) -> Id {
        let segments = segments.into_iter().collect_vec();
        Id { segments }
    }

    /// Construct a module's ID value from a name segments sequence.
    ///
    /// Fails if the sequence is empty or if any of the segments is not a valid referent name.
    pub fn try_new(segments: impl IntoIterator<Item: AsRef<str>>) -> FallibleResult<Id> {
        let texts = segments.into_iter();
        let names = texts.map(|text| ReferentName::new(text.as_ref()));
        let segments = names.collect::<Result<Vec<_>, _>>()?;
        Ok(Self::new(segments))
    }

    /// Get the segments of the module's path. They correspond to the module's file parent
    /// directories, relative to the project's main source directory.
    ///
    /// The names are ordered beginning with the root one. The last one is the direct parent of the
    /// target module's file. The module name itself is not included.
    pub fn parent_segments(&self) -> &[ReferentName] {
        &self.segments[..self.segments.len().saturating_sub(1)]
    }

    /// Consume the [`Id`] and returns the inner representation of segments.
    pub fn into_segments(self) -> Vec<ReferentName> {
        self.segments
    }

    /// Get the name of a module identified by this value.
    pub fn name(&self) -> ReferentName {
        self.segments
            .iter()
            .last()
            .cloned()
            .unwrap_or_else(|| ReferentName::new(PROJECTS_MAIN_MODULE).unwrap())
    }

    /// Access module name segments.
    pub fn segments(&self) -> &Vec<ReferentName> {
        &self.segments
    }
}



// =====================
// === QualifiedName ===
// =====================

/// Module's qualified name is used in some of the Language Server's APIs, like
/// `VisualisationConfiguration`. Logically it is a special case of [`tp::QualifiedName`] and has
/// defined `PartialEq<tp::QualifiedName`.
///
/// Qualified name is constructed as follows:
/// `ProjectName.<directories_between_src_and_enso_file>.<file_without_ext>`
///
/// See https://dev.enso.org/docs/distribution/packaging.html for more information about the
/// package structure.
#[derive(Clone, Debug, Deserialize, Serialize, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[serde(into = "String")]
#[serde(try_from = "String")]
pub struct QualifiedName {
    /// The first segment in the full qualified name. May be a project name or a keyword like
    pub project_name: project::QualifiedName,
    /// The module id: all segments in full qualified name but the first (which is a project name).
    pub id:           Id,
}

impl QualifiedName {
    /// Build a module's qualified name from the project name and module's path.
    pub fn new(project_name: project::QualifiedName, id: Id) -> QualifiedName {
        QualifiedName { project_name, id }
    }

    /// Create a qualified name for the project's main module.
    ///
    /// It is special, as its name consists only from the project name, unlike other modules'
    /// qualified names.
    pub fn new_main(project_name: project::QualifiedName) -> QualifiedName {
        Self::new(project_name, Id::new(std::iter::empty()))
    }

    /// Constructs a qualified name from its text representation.
    ///
    /// Fails, if the text is not a valid module's qualified name.
    pub fn from_text(text: impl AsRef<str>) -> FallibleResult<Self> {
        use ast::opr::predefined::ACCESS;

        let text = text.as_ref();
        let segments = text.split(ACCESS).collect_vec();
        if let [namespace_name, project_name, id_segments @ ..] = segments.as_slice() {
            let project_name =
                project::QualifiedName::from_segments(*namespace_name, *project_name)?;
            let id = Id::try_new(id_segments)?;
            Ok(Self::new(project_name, id))
        } else {
            Err(InvalidQualifiedName::NoModuleSegment.into())
        }
    }

    /// Build a module's full qualified name from its name segments and the project name.
    ///
    /// ```
    /// # use enso_prelude::*;
    /// #
    /// # use double_representation::module::QualifiedName;
    ///
    /// let name =
    ///     QualifiedName::from_segments("local.Project".try_into().unwrap(), &["Main"]).unwrap();
    /// assert_eq!(name.to_string(), "local.Project.Main");
    /// ```
    pub fn from_segments(
        project_name: project::QualifiedName,
        module_segments: impl IntoIterator<Item: AsRef<str>>,
    ) -> FallibleResult<QualifiedName> {
        let project_name = std::iter::once(project_name.into());
        let module_segments = module_segments.into_iter();
        let module_segments = module_segments.map(|segment| segment.as_ref().to_string());
        let mut all_segments = project_name.chain(module_segments);
        let text = all_segments.join(ast::opr::predefined::ACCESS);
        text.try_into()
    }

    /// Build a module's full qualified name from its name segments and the project name.
    ///
    /// ```
    /// # use double_representation::module::QualifiedName;
    ///
    /// let name = QualifiedName::from_all_segments(&["Project", "Main"]).unwrap();
    /// assert_eq!(name.to_string(), "Project.Main");
    /// ```
    pub fn from_all_segments(
        segments: impl IntoIterator<Item: AsRef<str>>,
    ) -> FallibleResult<QualifiedName> {
        let mut iter = segments.into_iter();
        let namespace = iter.next().map(|name| name.as_ref().to_owned());
        let project_name = iter.next().map(|name| name.as_ref().to_owned());
        let typed_project_name = match (namespace, project_name) {
            (Some(ns), Some(name)) => project::QualifiedName::from_segments(ns, name),
            _ => Err(InvalidQualifiedName::NoModuleSegment.into()),
        };
        Self::from_segments(typed_project_name?, iter)
    }

    /// Get the module's name. It is also the module's typename.
    pub fn name(&self) -> ReferentName {
        if self.id.segments.is_empty() {
            self.project_name.project.clone()
        } else {
            self.id.name()
        }
    }

    /// Get the name of project owning this module.
    pub fn project_name(&self) -> &ReferentName {
        &self.project_name.project
    }

    /// Get the module's identifier.
    pub fn id(&self) -> &Id {
        &self.id
    }

    /// Get all segments of the fully qualified name.
    pub fn segments(&self) -> impl Iterator<Item = &str> {
        self.project_name.segments().chain(self.id.segments.iter().map(|seg| seg.as_ref()))
    }

    /// Remove the main module segment from the qualified name.
    ///
    /// The main module does not need to be explicitly mentioned, so the modified structure will
    /// still identify the same entity.
    ///
    /// ```
    /// # use double_representation::module::QualifiedName;
    /// let mut name_with_main = QualifiedName::from_text("ns.Proj.Main").unwrap();
    /// let mut name_without_main = QualifiedName::from_text("ns.Proj.Foo.Bar").unwrap();
    /// let mut main_but_not_project_main = QualifiedName::from_text("ns.Proj.Foo.Main").unwrap();
    ///
    /// name_with_main.remove_main_module_segment();
    /// name_without_main.remove_main_module_segment();
    /// main_but_not_project_main.remove_main_module_segment();
    ///
    /// assert_eq!(name_with_main.to_string(), "ns.Proj");
    /// assert_eq!(name_without_main.to_string(), "ns.Proj.Foo.Bar");
    /// assert_eq!(main_but_not_project_main.to_string(), "ns.Proj.Foo.Main");
    /// ```
    pub fn remove_main_module_segment(&mut self) {
        if self.id.segments == [PROJECTS_MAIN_MODULE] {
            self.id.segments.pop();
        }
    }
}

impl TryFrom<&str> for QualifiedName {
    type Error = failure::Error;

    fn try_from(text: &str) -> Result<Self, Self::Error> {
        Self::from_text(text)
    }
}

impl TryFrom<String> for QualifiedName {
    type Error = failure::Error;

    fn try_from(text: String) -> Result<Self, Self::Error> {
        Self::from_text(text)
    }
}

impl TryFrom<engine_protocol::language_server::MethodPointer> for QualifiedName {
    type Error = failure::Error;

    fn try_from(
        method: engine_protocol::language_server::MethodPointer,
    ) -> Result<Self, Self::Error> {
        Self::try_from(method.module)
    }
}

impl TryFrom<&engine_protocol::language_server::MethodPointer> for QualifiedName {
    type Error = failure::Error;

    fn try_from(
        method: &engine_protocol::language_server::MethodPointer,
    ) -> Result<Self, Self::Error> {
        Self::try_from(method.module.clone())
    }
}

impl From<QualifiedName> for String {
    fn from(name: QualifiedName) -> Self {
        String::from(&name)
    }
}

impl From<&QualifiedName> for String {
    fn from(name: &QualifiedName) -> Self {
        let segments = name.id.segments.iter().map(|rn| rn.as_ref());
        name.project_name.segments().chain(segments).join(ast::opr::predefined::ACCESS)
    }
}

impl Display for QualifiedName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let text = String::from(self);
        fmt::Display::fmt(&text, f)
    }
}

impl PartialEq<tp::QualifiedName> for QualifiedName {
    fn eq(&self, other: &tp::QualifiedName) -> bool {
        self.project_name == other.project_name
            && self.id.parent_segments() == other.module_segments.as_slice()
            && self.id.name().as_str() == other.name
    }
}



// ==================
// === ImportInfo ===
// ==================

/// Representation of a single import declaration.
// TODO [mwu]
// Currently only supports the unqualified imports like `import Foo.Bar`. Qualified, restricted and
// and hiding imports are not supported by the parser yet. In future when parser and engine
// supports them, this structure should be adjusted as well.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ImportInfo {
    /// The segments of the qualified name of the imported target.
    ///
    /// This field is not Qualified name to cover semantically illegal imports that are possible to
    /// be typed in and are representable in the text.
    /// This includes targets with too few segments or segments not being valid referent names.
    pub target: Vec<String>,
}

impl ImportInfo {
    /// Construct from a string describing an import target, like `"Foo.Bar"`.
    pub fn from_target_str(name: impl AsRef<str>) -> Self {
        let name = name.as_ref().trim();
        let target = if name.is_empty() {
            Vec::new()
        } else {
            name.split(ast::opr::predefined::ACCESS).map(Into::into).collect()
        };
        ImportInfo { target }
    }

    /// Construct from a module qualified name like `"Foo.Bar"` that describes imported target.
    pub fn from_qualified_name(name: &QualifiedName) -> Self {
        let target = name.segments().map(|segment| segment.to_string()).collect();
        Self { target }
    }

    /// Obtain the qualified name of the imported module.
    pub fn qualified_name(&self) -> FallibleResult<QualifiedName> {
        QualifiedName::from_all_segments(&self.target)
    }

    /// Construct from an AST. Fails if the Ast is not an import declaration.
    pub fn from_ast(ast: &Ast) -> Option<Self> {
        let macro_match = known::Match::try_from(ast).ok()?;
        Self::from_match(macro_match)
    }

    /// Construct from a macro match AST. Fails if the Ast is not an import declaration.
    pub fn from_match(ast: known::Match) -> Option<Self> {
        ast::macros::is_match_import(&ast)
            .then(|| ImportInfo::from_target_str(ast.segs.head.body.repr().trim()))
    }
}

impl Display for ImportInfo {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let target = self.target.join(ast::opr::predefined::ACCESS);
        write!(f, "{} {}", ast::macros::QUALIFIED_IMPORT_KEYWORD, target)
    }
}



// ============
// === Info ===
// ============

/// Wrapper allowing getting information about the module and updating it.
#[derive(Clone, Debug)]
pub struct Info {
    #[allow(missing_docs)]
    pub ast: known::Module,
}

impl Info {
    /// Generate a name for a definition that can be introduced without side-effects.
    ///
    /// The name shall be generated by appending number to the given base string.
    pub fn generate_name(&self, base: &str) -> FallibleResult<Identifier> {
        let used_names = self.used_names();
        let used_names = used_names.into_iter().map(|name| name.item);
        identifier::generate_name(base, used_names)
    }

    /// Identifiers introduced or referred to in the module's scope.
    ///
    /// Introducing identifier not included on this list should have no side-effects on the name
    /// resolution in the code in this graph.
    pub fn used_names(&self) -> Vec<LocatedName> {
        let usage = alias_analysis::analyze_crumbable(self.ast.shape());
        usage.all_identifiers()
    }

    /// Iterate over all lines in module that contain an import declaration.
    pub fn enumerate_imports(&self) -> impl Iterator<Item = (ModuleCrumb, ImportInfo)> + '_ {
        let children = self.ast.shape().enumerate();
        children.filter_map(|(crumb, ast)| Some((crumb, ImportInfo::from_ast(ast)?)))
    }

    /// Iterate over all import declarations in the module.
    ///
    /// If the caller wants to know *where* the declarations are, use `enumerate_imports`.
    pub fn iter_imports(&self) -> impl Iterator<Item = ImportInfo> + '_ {
        self.enumerate_imports().map(|(_, import)| import)
    }

    /// Add a new line to the module's block.
    ///
    /// Note that indices are the "module line" indices, which usually are quite different from text
    /// API line indices (because nested blocks doesn't count as separate "module lines").
    pub fn add_line(&mut self, index: usize, ast: Option<Ast>) {
        let line = BlockLine::new(ast);
        self.ast.update_shape(|shape| shape.lines.insert(index, line))
    }

    /// Remove line with given index.
    ///
    /// Returns removed line. Fails if the index is out of bounds.
    pub fn remove_line(&mut self, index: usize) -> FallibleResult<BlockLine<Option<Ast>>> {
        self.ast.update_shape(|shape| {
            shape.lines.try_remove(index).ok_or_else(|| LineIndexOutOfBounds.into())
        })
    }

    /// Remove a line that matches given import description.
    ///
    /// If there is more than one line matching, only the first one will be removed.
    /// Fails if there is no import matching given argument.
    pub fn remove_import(&mut self, to_remove: &ImportInfo) -> FallibleResult {
        let lookup_result = self.enumerate_imports().find(|(_, import)| import == to_remove);
        let (crumb, _) = lookup_result.ok_or_else(|| ImportNotFound(to_remove.clone()))?;
        self.remove_line(crumb.line_index)?;
        Ok(())
    }

    /// Add a new import declaration to a module.
    ///
    /// This function will try to keep imports in lexicographic order. It returns the index where
    /// import was added (index of import - an element on the list returned by `enumerate_imports`).
    // TODO [mwu]
    //   Ideally we should not require parser but should use some sane way of generating AST from
    //   the `ImportInfo` value.
    pub fn add_import(&mut self, parser: &parser::Parser, to_add: ImportInfo) -> usize {
        // Find last import that is not "after" the added one lexicographically.
        let previous_import =
            self.enumerate_imports().take_while(|(_, import)| to_add.target > import.target).last();

        let index_to_place_at = previous_import.map_or(0, |(crumb, _)| crumb.line_index + 1);
        let import_ast = parser.parse_line_ast(to_add.to_string()).unwrap();
        self.add_line(index_to_place_at, Some(import_ast));
        index_to_place_at
    }

    /// Add a new import if the module is not already imported.
    pub fn add_module_import(
        &mut self,
        here: &QualifiedName,
        parser: &parser::Parser,
        to_add: &QualifiedName,
    ) {
        let imports = self.iter_imports().collect_vec();
        DEBUG!("add_module_import: {to_add} in {imports:?}");
        let is_here = to_add == here;
        let import = ImportInfo::from_qualified_name(to_add);
        let already_imported = self.iter_imports().any(|imp| imp == import);
        if !is_here && !already_imported {
            self.add_import(parser, import);
        }
    }

    /// Place the line with given AST in the module's body.
    ///
    /// Unlike `add_line` (which is more low-level) will introduce empty lines around introduced
    /// line and describes the added line location in relation to other definitions.
    ///
    /// Typically used to place lines with definitions in the module.
    pub fn add_ast(&mut self, ast: Ast, location: Placement) -> FallibleResult {
        #[derive(Clone, Copy, Debug, Eq, PartialEq)]
        enum BlankLinePlacement {
            Before,
            After,
            None,
        }
        let blank_line = match location {
            _ if self.ast.lines.is_empty() => BlankLinePlacement::None,
            Placement::Begin => BlankLinePlacement::After,
            Placement::End => BlankLinePlacement::Before,
            Placement::After(_) => BlankLinePlacement::Before,
            Placement::Before(_) => BlankLinePlacement::After,
        };

        let mut index = match location {
            Placement::Begin => 0,
            Placement::End => self.ast.lines.len(),
            Placement::Before(next_def) => locate_line_with(&self.ast, &next_def)?.line_index,
            Placement::After(next_def) => locate_line_with(&self.ast, &next_def)?.line_index + 1,
        };

        let mut add_line = |ast_opt: Option<Ast>| {
            self.add_line(index, ast_opt);
            index += 1;
        };

        if blank_line == BlankLinePlacement::Before {
            add_line(None);
        }
        add_line(Some(ast));
        if blank_line == BlankLinePlacement::After {
            add_line(None);
        }

        Ok(())
    }

    /// Add a new method definition to the module.
    pub fn add_method(
        &mut self,
        method: definition::ToAdd,
        location: Placement,
        parser: &parser::Parser,
    ) -> FallibleResult {
        let no_indent = 0;
        let definition_ast = method.ast(no_indent, parser)?;
        self.add_ast(definition_ast, location)
    }

    /// Updates the given definition using the passed invokable.
    pub fn update_definition(
        &mut self,
        id: &definition::Id,
        f: impl FnOnce(definition::DefinitionInfo) -> FallibleResult<definition::DefinitionInfo>,
    ) -> FallibleResult {
        let definition = locate(&self.ast, id)?;
        let new_definition = f(definition.item)?;
        let new_ast = new_definition.ast.into();
        self.ast = self.ast.set_traversing(&definition.crumbs, new_ast)?;
        Ok(())
    }

    #[cfg(test)]
    pub fn expect_code(&self, expected_code: impl AsRef<str>) {
        assert_eq!(self.ast.repr(), expected_code.as_ref());
    }
}

impl From<known::Module> for Info {
    fn from(ast: known::Module) -> Self {
        Info { ast }
    }
}



// =================
// === Placement ===
// =================

/// Structure describing where to place something being added to the module.
#[derive(Clone, Debug, PartialEq)]
pub enum Placement {
    /// Place at the beginning of the module.
    Begin,
    /// Place at the end of the module.
    End,
    /// Place after given definition;
    Before(definition::Crumb),
    /// Place before given definition;
    After(definition::Crumb),
}



// =======================
// === ChildDefinition ===
// =======================

/// Represents information about a definition being a direct child of this module, including its
/// location.
///
/// Internally it is `definition::ChildDefinition` with only a single `ModuleCrumb` as location.
#[derive(Clone, Debug, Shrinkwrap)]
pub struct ChildDefinition(definition::ChildDefinition);

impl ChildDefinition {
    fn try_retrieving_crumb(child: &definition::ChildDefinition) -> Option<ModuleCrumb> {
        match child.crumbs.as_slice() {
            [ast::crumbs::Crumb::Module(crumb)] => Some(*crumb),
            _ => None,
        }
    }

    /// Try constructing value from `definition::ChildDefinition`. Fails if it is not a direct child
    /// of a module.
    pub fn new(child: definition::ChildDefinition) -> Result<Self, NotDirectChild> {
        if Self::try_retrieving_crumb(&child).is_some() {
            Ok(Self(child))
        } else {
            Err(NotDirectChild(child.crumbs))
        }
    }

    /// The location of this definition child in the module.
    pub fn crumb(&self) -> ModuleCrumb {
        // Safe, because our only constructor checks that this works. This is the type's invariant.
        Self::try_retrieving_crumb(&self.0).unwrap()
    }
}

impl TryFrom<definition::ChildDefinition> for ChildDefinition {
    type Error = NotDirectChild;
    fn try_from(value: definition::ChildDefinition) -> Result<Self, Self::Error> {
        Self::new(value)
    }
}



// ========================
// === Module Utilities ===
// ========================

/// Looks up graph in the module.
pub fn get_definition(
    ast: &known::Module,
    id: &definition::Id,
) -> FallibleResult<definition::DefinitionInfo> {
    Ok(locate(ast, id)?.item)
}

/// Locate the line with given definition and return crumb that denotes it.
///
/// Fails if there is no matching definition being a direct child of the module.
pub fn locate_line_with(
    ast: &known::Module,
    crumb: &definition::Crumb,
) -> FallibleResult<ModuleCrumb> {
    locate_child(ast, crumb).map(|child| child.crumb())
}

/// Locate the definition being the module's direct child.
pub fn locate_child(
    ast: &known::Module,
    crumb: &definition::Crumb,
) -> FallibleResult<ChildDefinition> {
    let child = ast.def_iter().find_by_name(crumb)?;
    Ok(ChildDefinition::try_from(child)?)
}

/// Traverses the module's definition tree following the given Id crumbs, looking up the definition.
pub fn locate(
    ast: &known::Module,
    id: &definition::Id,
) -> FallibleResult<definition::ChildDefinition> {
    let mut crumbs_iter = id.crumbs.iter();
    // Not exactly regular - we need special case for the first crumb as it is not a definition nor
    // a children. After this we can go just from one definition to another.
    let first_crumb = crumbs_iter.next().ok_or(EmptyDefinitionId)?;
    let mut child = ast.def_iter().find_by_name(first_crumb)?;
    for crumb in crumbs_iter {
        child = definition::resolve_single_name(child, crumb)?;
    }
    Ok(child)
}

/// Get a definition ID that points to a method matching given pointer.
///
/// The module is assumed to be in the file identified by the `method.file` (for the purpose of
/// desugaring implicit extensions methods for modules).
///
/// The `module_name` parameter is the name of the module that contains `ast`. It affects how the
/// `here` keyword is resolved.
pub fn lookup_method(
    module_name: &QualifiedName,
    ast: &known::Module,
    method: &language_server::MethodPointer,
) -> FallibleResult<definition::Id> {
    let qualified_typename = tp::QualifiedName::from_text(&method.defined_on_type)?;
    let accept_here_methods = module_name == &qualified_typename;
    let method_module_name = QualifiedName::try_from(method)?;
    let implicit_extension_allowed = method.defined_on_type == method_module_name.to_string();
    for child in ast.def_iter() {
        let child_name = &child.name.item;
        let name_matches = child_name.name.item == method.name;
        let type_matches = match child_name.extended_target.as_slice() {
            [] => implicit_extension_allowed,
            [typename] => {
                let explicit_type_matching = typename.item == qualified_typename.name;
                let here_extension_matching = typename.item == HERE && accept_here_methods;
                explicit_type_matching || here_extension_matching
            }
            _ => child_name.explicitly_extends_type(&method.defined_on_type),
        };
        if name_matches && type_matches {
            return Ok(definition::Id::new_single_crumb(child_name.clone()));
        }
    }

    Err(CannotFindMethod(method.clone()).into())
}

/// Get a span in module's text representation where the given definition is located.
pub fn definition_span(
    ast: &known::Module,
    id: &definition::Id,
) -> FallibleResult<enso_text::Range<Bytes>> {
    let location = locate(ast, id)?;
    ast.range_of_descendant_at(&location.crumbs)
}

impl DefinitionProvider for known::Module {
    fn indent(&self) -> usize {
        0
    }

    fn scope_kind(&self) -> definition::ScopeKind {
        definition::ScopeKind::Root
    }

    fn enumerate_asts<'a>(&'a self) -> Box<dyn Iterator<Item = ChildAst<'a>> + 'a> {
        self.ast().children()
    }
}



// ============
// === Test ===
// ============

#[cfg(test)]
mod tests {
    use super::*;

    use crate::definition::DefinitionName;

    use engine_protocol::language_server::MethodPointer;
    use wasm_bindgen_test::wasm_bindgen_test;

    wasm_bindgen_test::wasm_bindgen_test_configure!(run_in_browser);

    #[test]
    fn qualified_name_validation() {
        assert!(QualifiedName::try_from("namespace.project.Name").is_err());
        assert!(QualifiedName::try_from("namespace.Project.name").is_err());
        assert!(QualifiedName::try_from("namespace.").is_err());
        assert!(QualifiedName::try_from(".Name").is_err());
        assert!(QualifiedName::try_from(".").is_err());
        assert!(QualifiedName::try_from("").is_err());
        assert!(QualifiedName::try_from("namespace.Project.Name").is_ok());
        assert!(QualifiedName::try_from("namespace.Project.Name.Sub").is_ok());
    }

    #[wasm_bindgen_test]
    fn import_listing() {
        let parser = parser::Parser::new_or_panic();
        let expect_imports = |code: &str, expected: &[&[&str]]| {
            let ast = parser.parse_module(code, default()).unwrap();
            let info = Info { ast };
            let imports = info.iter_imports().collect_vec();
            assert_eq!(imports.len(), expected.len());
            for (import, expected_segments) in imports.iter().zip(expected) {
                itertools::assert_equal(import.target.iter(), expected_segments.iter());
            }
        };

        // TODO [mwu] waiting for fix https://github.com/enso-org/enso/issues/1016
        //   expect_imports("import", &[&[]]);
        expect_imports("import Foo", &[&["Foo"]]);
        expect_imports("import Foo.Bar", &[&["Foo", "Bar"]]);
        expect_imports("foo = bar\nimport Foo.Bar", &[&["Foo", "Bar"]]);
        expect_imports("import Foo.Bar\nfoo=bar\nimport Foo.Bar", &[&["Foo", "Bar"], &[
            "Foo", "Bar",
        ]]);
    }

    #[wasm_bindgen_test]
    fn import_adding_and_removing() {
        let parser = parser::Parser::new_or_panic();
        let code = "import Foo.Bar.Baz";
        let ast = parser.parse_module(code, default()).unwrap();
        let mut info = Info { ast };
        let import = |code| {
            let ast = parser.parse_line_ast(code).unwrap();
            ImportInfo::from_ast(&ast).unwrap()
        };

        info.add_import(&parser, import("import Bar.Gar"));
        info.expect_code("import Bar.Gar\nimport Foo.Bar.Baz");
        info.add_import(&parser, import("import Gar.Bar"));
        info.expect_code("import Bar.Gar\nimport Foo.Bar.Baz\nimport Gar.Bar");

        info.remove_import(&ImportInfo::from_target_str("Foo.Bar.Baz")).unwrap();
        info.expect_code("import Bar.Gar\nimport Gar.Bar");
        info.remove_import(&ImportInfo::from_target_str("Foo.Bar.Baz")).unwrap_err();
        info.expect_code("import Bar.Gar\nimport Gar.Bar");
        info.remove_import(&ImportInfo::from_target_str("Gar.Bar")).unwrap();
        info.expect_code("import Bar.Gar");
        info.remove_import(&ImportInfo::from_target_str("Bar.Gar")).unwrap();
        info.expect_code("");

        info.add_import(&parser, import("import Bar.Gar"));
        info.expect_code("import Bar.Gar");
    }

    #[wasm_bindgen_test]
    fn implicit_method_resolution() {
        let parser = parser::Parser::new_or_panic();
        let module_name =
            QualifiedName::from_all_segments(&["local", "ProjectName", "Main"]).unwrap();
        let expect_find = |method: &MethodPointer, code, expected: &definition::Id| {
            let module = parser.parse_module(code, default()).unwrap();
            let result = lookup_method(&module_name, &module, method);
            assert_eq!(result.unwrap().to_string(), expected.to_string());

            // TODO [mwu]
            //  We should be able to use `assert_eq!(result.unwrap(),expected);`
            //  But we can't, because definition::Id uses located fields and crumbs won't match.
            //  Eventually we'll likely need to split definition names into located and unlocated
            //  ones. Definition ID should not require any location info.
        };

        let expect_not_found = |method: &MethodPointer, code| {
            let module = parser.parse_module(code, default()).unwrap();
            lookup_method(&module_name, &module, method).expect_err("expected method not found");
        };


        // === Lookup the Main (local module type) extension method ===

        let ptr = MethodPointer {
            defined_on_type: "local.ProjectName.Main".into(),
            module:          "local.ProjectName.Main".into(),
            name:            "foo".into(),
        };

        // Implicit module extension method.
        let id = definition::Id::new_plain_name("foo");
        expect_find(&ptr, "foo a b = a + b", &id);
        // Explicit module extension method.
        let id = definition::Id::new_single_crumb(DefinitionName::new_method("Main", "foo"));
        expect_find(&ptr, "Main.foo a b = a + b", &id);
        // Explicit extensions using "here" keyword.
        let id = definition::Id::new_single_crumb(DefinitionName::new_method("here", "foo"));
        expect_find(&ptr, "here.foo a b = a + b", &id);
        // Matching name but extending wrong type.
        expect_not_found(&ptr, "Number.foo a b = a + b");
        // Mismatched name.
        expect_not_found(&ptr, "bar a b = a + b");


        // === Lookup the Int (non-local type) extension method ===

        let ptr = MethodPointer {
            defined_on_type: "std.Base.Main.Number".into(),
            module:          "local.ProjectName.Main".into(),
            name:            "foo".into(),
        };

        expect_not_found(&ptr, "foo a b = a + b");
        let id = definition::Id::new_single_crumb(DefinitionName::new_method("Number", "foo"));
        expect_find(&ptr, "Number.foo a b = a + b", &id);
        expect_not_found(&ptr, "Text.foo a b = a + b");
        expect_not_found(&ptr, "here.foo a b = a + b");
        expect_not_found(&ptr, "bar a b = a + b");
    }

    #[wasm_bindgen_test]
    fn test_definition_location() {
        let code = r"
some def =
    first line
    second line

other def =
    first line
    second line
    nested def =
        nested body
    last line of other def

last def = inline expression";

        let parser = parser::Parser::new_or_panic();
        let module = parser.parse_module(code, default()).unwrap();
        let module = Info { ast: module };

        let id = definition::Id::new_plain_name("other");
        let span = definition_span(&module.ast, &id).unwrap();
        assert!(code[span].ends_with("last line of other def\n"));

        let id = definition::Id::new_plain_name("last");
        let span = definition_span(&module.ast, &id).unwrap();
        assert!(code[span].ends_with("inline expression"));

        let id = definition::Id::new_plain_names(&["other", "nested"]);
        let span = definition_span(&module.ast, &id).unwrap();
        assert!(code[span].ends_with("nested body"));
    }

    #[wasm_bindgen_test]
    fn add_method() {
        let parser = parser::Parser::new_or_panic();
        let module = r#"Main.method1 arg = body

main = here.method1 10"#;

        let module = Info::from(parser.parse_module(module, default()).unwrap());
        let method1_id = DefinitionName::new_method("Main", "method1");
        let main_id = DefinitionName::new_plain("main");
        let to_add = definition::ToAdd {
            name:                     DefinitionName::new_method("Main", "add"),
            explicit_parameter_names: vec!["arg1".into(), "arg2".into()],
            body_head:                Ast::infix_var("arg1", "+", "arg2"),
            body_tail:                default(),
        };

        let repr_after_insertion = |location| {
            let mut module = module.clone();
            module.add_method(to_add.clone(), location, &parser).unwrap();
            module.ast.repr()
        };

        let expected = r#"Main.add arg1 arg2 = arg1 + arg2

Main.method1 arg = body

main = here.method1 10"#;
        assert_eq!(repr_after_insertion(Placement::Begin), expected);

        let expected = r#"Main.method1 arg = body

main = here.method1 10

Main.add arg1 arg2 = arg1 + arg2"#;
        assert_eq!(repr_after_insertion(Placement::End), expected);

        let expected = r#"Main.method1 arg = body

Main.add arg1 arg2 = arg1 + arg2

main = here.method1 10"#;
        assert_eq!(repr_after_insertion(Placement::After(method1_id.clone())), expected);

        assert_eq!(
            repr_after_insertion(Placement::Before(method1_id.clone())),
            repr_after_insertion(Placement::Begin)
        );
        assert_eq!(
            repr_after_insertion(Placement::After(method1_id.clone())),
            repr_after_insertion(Placement::Before(main_id.clone()))
        );
        assert_eq!(
            repr_after_insertion(Placement::After(main_id.clone())),
            repr_after_insertion(Placement::End)
        );

        // TODO [mwu]
        //  This test doesn't include multi-lines functions, as the result may seem somewhat
        // unexpected  due to the way that parser assigns blank lines to the former block
        // rather than module.  If anyone will care, we might revisit this after the parser
        // 2.0 rewrite.
    }
}
