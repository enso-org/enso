//! Module with the Suggestion Database Entry and all structures related to.

use crate::prelude::*;

use crate::SuggestionDatabase;

use ast::opr;
use convert_case::Case;
use convert_case::Casing;
use double_representation::import;
use double_representation::module::MethodId;
use double_representation::name;
use double_representation::name::QualifiedName;
use double_representation::name::QualifiedNameRef;
use engine_protocol::language_server;
use engine_protocol::language_server::FieldUpdate;
use engine_protocol::language_server::SuggestionsDatabaseModification;
use enso_doc_parser::DocSection;
use enso_text::Location;
use language_server::types::FieldAction;


// ==============
// === Export ===
// ==============

pub use language_server::types::SuggestionEntryArgument as Argument;
pub use language_server::types::SuggestionId as Id;
pub use language_server::types::SuggestionsDatabaseUpdate as Update;



// =================
// === Constants ===
// =================

/// Key of the keyed [`language_server::types::DocSection`] containing a name of an icon in its
/// body.
const ICON_DOC_SECTION_KEY: &str = "Icon";



// ==============
// === Errors ===
// ==============

#[allow(missing_docs)]
#[derive(Debug, Fail, Clone)]
#[fail(display = "Argument index {} is invalid for suggestion entry named {}.", index, name)]
pub struct InvalidArgumentIndex {
    pub name:  String,
    pub index: usize,
}

#[allow(missing_docs)]
#[derive(Copy, Clone, Debug, Fail)]
#[fail(display = "Invalid update for field {}.", _0)]
pub struct InvalidFieldUpdate(pub &'static str);

#[allow(missing_docs)]
#[derive(Debug, Fail, Clone)]
#[fail(display = "Entry named {} does not represent a method.", _0)]
pub struct NotAMethod(pub String);

#[allow(missing_docs)]
#[derive(Debug, Fail, Clone)]
#[fail(display = "Entry named {} is described as method but does not have a `self` parameter.", _0)]
pub struct MissingSelfOnMethod(pub String);



// ==================
// === ModuleSpan ===
// ==================

/// A span in a module identified by qualified name.
///
/// Span uses UTF-16 code units as units of measurement, so it is compatible with the format used
/// internally by the suggestion database entries.
#[allow(missing_docs)]
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ModuleSpan {
    pub module: QualifiedName,
    pub span:   Location<enso_text::Utf16CodeUnit>,
}



// ================
// === IconName ===
// ================

/// Name of an icon. The name is composed of words with unspecified casing.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IconName {
    /// Internally the name is kept in PascalCase to optimize converting into
    /// [`component_group_view::icon::Id`].
    pascal_cased: ImString,
}

impl IconName {
    /// Construct from a name formatted in snake_case.
    pub fn from_snake_case(s: impl AsRef<str>) -> Self {
        let pascal_cased = s.as_ref().from_case(Case::Snake).to_case(Case::Pascal).into();
        Self { pascal_cased }
    }

    /// Convert to a name formatted in snake_case.
    pub fn to_snake_case(&self) -> ImString {
        self.pascal_cased.from_case(Case::Pascal).to_case(Case::Snake).into()
    }

    /// Convert to a name formatted in PascalCase.
    pub fn to_pascal_case(&self) -> ImString {
        self.pascal_cased.clone()
    }
}



// ==============
// === Import ===
// ==============

/// An import of a single name.
///
/// The import added by inserting [entries](Entry) always imports single name.
///
/// This can be thought of as a special case of [`import::Info`], which allows us simpler checking
/// if given entity was already imported.
#[allow(missing_docs)]
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Import {
    /// `import <module>`. Only module entries can be imported using this import.
    Qualified { module: QualifiedName },
    /// `from <module> import <name>`.
    Unqualified { module: QualifiedName, name: ImString },
}

impl Import {
    /// Check if the imported entity is also imported by the `existing_import`.
    pub fn covered_by(&self, existing_import: &import::Info) -> bool {
        let parent_module = || match self {
            Import::Qualified { module } => module.parent(),
            Import::Unqualified { module, .. } => Some(module.as_ref()),
        };
        let name = || match self {
            Import::Qualified { module } => module.alias_name(),
            Import::Unqualified { name, .. } => name,
        };
        match &existing_import.imported {
            import::ImportedNames::Module { alias: None } => match self {
                Self::Qualified { module } => *module == existing_import.module,
                Self::Unqualified { module, name } => match existing_import.module.as_slice() {
                    [parent @ .., existing_name] => *module == parent && name == existing_name,
                    _ => false,
                },
            },
            import::ImportedNames::All => parent_module().contains(&existing_import.module),
            import::ImportedNames::List { names } =>
                parent_module().contains(&existing_import.module) && names.contains(&**name()),
            import::ImportedNames::AllExcept { not_imported } =>
                parent_module().contains(&existing_import.module)
                    && !not_imported.contains(&**name()),
            _ => false,
        }
    }
}

impl From<Import> for import::Info {
    fn from(import: Import) -> Self {
        match import {
            Import::Qualified { module } => Self::new_qualified(module),
            Import::Unqualified { module, name } => Self::new_single_name(module, name),
        }
    }
}



// =============
// === Entry ===
// =============

// === Kind ===

/// A type of suggestion entry.
#[derive(Copy, Clone, Debug, Eq, PartialEq, ForEachVariant)]
#[allow(missing_docs)]
pub enum Kind {
    Type,
    Constructor,
    Function,
    Local,
    Method,
    Module,
}


// === Scope ===

/// Describes the visibility range of some entry (i.e. identifier available as suggestion).
///
/// Methods are visible "Everywhere", as they are imported on a module level, so they are not
/// specific to any particular span in the module file.
/// However local variables and local function have limited visibility.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Scope {
    /// The entry is visible in the whole module where it was defined. It can be also brought to
    /// other modules by import declarations.
    Everywhere,
    /// Local symbol that is visible only in a particular section of the module where it has been
    /// defined.
    ///
    /// We are using UTF-16 codepoints because this is what Language Server speaks.
    /// To convert to bytes (or other system) one would need to know the whole module code which
    /// is not available to the suggestions database.
    #[allow(missing_docs)]
    InModule { range: RangeInclusive<Location<enso_text::Utf16CodeUnit>> },
}


// === Entry ===

/// The Suggestion Database Entry.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Entry {
    /// A type of suggestion.
    pub kind:          Kind,
    /// A module where the suggested object is defined.
    pub defined_in:    QualifiedName,
    /// A name of suggested object.
    pub name:          String,
    /// Argument lists of suggested object (atom or function). If the object does not take any
    /// arguments, the list is empty.
    pub arguments:     Vec<Argument>,
    /// A type returned by the suggested object.
    pub return_type:   QualifiedName,
    /// A module reexporting this entity.
    pub reexported_in: Option<QualifiedName>,
    /// A list of documentation sections associated with object.
    pub documentation: Vec<DocSection>,
    /// A type of the "self" argument. This field is `None` for non-method suggestions.
    pub self_type:     Option<QualifiedName>,
    /// A flag set to true if the method is a static or module method.
    pub is_static:     bool,
    /// A scope where this suggestion is visible.
    pub scope:         Scope,
    /// A name of a custom icon to use when displaying the entry.
    pub icon_name:     Option<IconName>,
}


// === Entry Construction ===

impl Entry {
    /// Create new entry with required parameters only.
    ///
    /// The entry will be flagged as non-static, with [`Scope::Everywhere`] and all optional fields
    /// will be [`None`].
    pub fn new(
        kind: Kind,
        defined_in: impl Into<QualifiedName>,
        name: impl Into<String>,
        return_type: impl Into<QualifiedName>,
    ) -> Self {
        Self {
            kind,
            defined_in: defined_in.into(),
            name: name.into(),
            arguments: vec![],
            return_type: return_type.into(),
            is_static: false,
            reexported_in: None,
            documentation: default(),
            self_type: None,
            scope: Scope::Everywhere,
            icon_name: None,
        }
    }

    /// Create new non-extension, non-module method.
    ///
    /// As the method is not an extension, its assumed it's defined in the parent module of
    /// `on_type`.
    pub fn new_nonextension_method(
        on_type: QualifiedName,
        name: impl Into<String>,
        return_type: QualifiedName,
        is_static: bool,
    ) -> Self {
        let module = on_type.parent().unwrap_or(on_type.as_ref()).to_owned();
        Self {
            self_type: Some(on_type),
            is_static,
            ..Self::new(Kind::Method, module, name, return_type)
        }
    }

    /// Create new module method.
    ///
    /// The module method has self_type equal to module where it's defined, and is always static.
    pub fn new_module_method(
        defined_in: QualifiedName,
        name: impl Into<String>,
        return_type: QualifiedName,
    ) -> Self {
        Self {
            self_type: Some(defined_in.clone()),
            is_static: true,
            ..Self::new(Kind::Method, defined_in, name, return_type)
        }
    }

    /// Create new method entry.
    pub fn new_method(
        defined_in: QualifiedName,
        on_type: QualifiedName,
        name: impl Into<String>,
        return_type: QualifiedName,
        is_static: bool,
    ) -> Self {
        Self {
            self_type: Some(on_type),
            is_static,
            ..Self::new(Kind::Method, defined_in, name, return_type)
        }
    }

    /// Create new type entry.
    pub fn new_type(defined_in: QualifiedName, name: impl Into<String>) -> Self {
        let name = name.into();
        let return_type = defined_in.clone().new_child(&name);
        Self::new(Kind::Type, defined_in, name, return_type)
    }

    /// Create new constructor entry.
    pub fn new_constructor(of_type: QualifiedName, name: impl Into<String>) -> Self {
        let defined_in_module = of_type.parent().unwrap_or(of_type.as_ref()).to_owned();
        Self {
            self_type: Some(of_type.clone()),
            is_static: true,
            ..Self::new(Kind::Constructor, defined_in_module, name, of_type)
        }
    }

    /// Create new local function entry.
    pub fn new_function(
        defined_in: QualifiedName,
        name: impl Into<String>,
        return_type: QualifiedName,
        scope_range: RangeInclusive<Location<enso_text::Utf16CodeUnit>>,
    ) -> Self {
        Self {
            scope: Scope::InModule { range: scope_range },
            ..Self::new(Kind::Function, defined_in, name, return_type)
        }
    }

    /// Create new local variable entry.
    pub fn new_local(
        defined_in: QualifiedName,
        name: impl Into<String>,
        return_type: QualifiedName,
        scope_range: RangeInclusive<Location<enso_text::Utf16CodeUnit>>,
    ) -> Self {
        Self {
            scope: Scope::InModule { range: scope_range },
            ..Self::new(Kind::Local, defined_in, name, return_type)
        }
    }

    /// Create new module entry.
    pub fn new_module(full_name: QualifiedName) -> Self {
        let name = full_name.name().to_owned();
        let return_type = full_name.clone();
        Self::new(Kind::Module, full_name, name, return_type)
    }

    /// Takes self and returns it with new argument list set.
    pub fn with_arguments(mut self, arguments: impl IntoIterator<Item = Argument>) -> Self {
        self.arguments = arguments.into_iter().collect();
        self
    }

    /// Takes self and returns it with new [`reexported_in`] value.
    pub fn reexported_in(mut self, module: QualifiedName) -> Self {
        self.reexported_in = Some(module);
        self
    }

    /// Takes self and returns it with new [`documentation`] value;
    pub fn with_doc_sections(mut self, sections: Vec<DocSection>) -> Self {
        self.documentation = sections;
        self
    }

    /// Takes self and adds new [`documentation`] section.
    pub fn with_doc_section(mut self, section: DocSection) -> Self {
        self.documentation.push(section);
        self
    }

    /// Takes self and returns it with new [`icon_name`] value.
    pub fn with_icon(mut self, icon_name: IconName) -> Self {
        self.icon_name = Some(icon_name);
        self
    }
}


// === Inserting Code and Imports ===

impl Entry {
    /// Check if this entry has self type same as the given identifier.
    pub fn has_self_type<TypeName>(&self, self_type: &TypeName) -> bool
    where TypeName: PartialEq<QualifiedName> {
        self.self_type.contains(self_type)
    }

    /// Returns the code which is inserted by picking this entry as suggestion.
    pub fn code_to_insert(&self, generate_this: bool) -> Cow<str> {
        match self.kind {
            Kind::Method if generate_this && self.is_static => self.code_with_static_this().into(),
            Kind::Method if generate_this =>
                format!("_{}{}", opr::predefined::ACCESS, self.name).into(),
            Kind::Constructor => self.code_with_static_this().into(),
            Kind::Module => self.defined_in.alias_name().as_str().into(),
            _ => Cow::from(&self.name),
        }
    }

    fn code_with_static_this(&self) -> String {
        if let Some(self_type) = &self.self_type {
            format!("{}{}{}", self_type.alias_name(), opr::predefined::ACCESS, self.name)
        } else {
            format!("_{}{}", opr::predefined::ACCESS, self.name)
        }
    }

    /// Return the list of required imports to have the code inserted by picking this entry working.
    pub fn required_imports(
        &self,
        db: &SuggestionDatabase,
        in_module: QualifiedNameRef,
    ) -> SmallVec<[Import; 2]> {
        let defined_in_same_module = self.defined_in == in_module;
        match self.kind {
            Kind::Method => {
                let self_type_entry = self.self_type_entry(db);
                let self_type_module = self_type_entry.as_ref().map(|st| &st.defined_in);
                let is_extension_method =
                    self_type_module.map_or(false, |stm| *stm != self.defined_in);
                let extension_method_import = is_extension_method.and_option_from(|| {
                    self.defined_in_entry(db).map(|e| e.required_imports(db, in_module.clone_ref()))
                });
                let self_type_import = self
                    .is_static
                    .and_option_from(|| self_type_entry.map(|e| e.required_imports(db, in_module)));
                extension_method_import
                    .into_iter()
                    .chain(self_type_import.into_iter())
                    .flatten()
                    .collect()
            }
            Kind::Constructor => self
                .self_type_entry(db)
                .map(|e| e.required_imports(db, in_module))
                .into_iter()
                .flatten()
                .collect(),
            Kind::Module | Kind::Type if defined_in_same_module => default(),
            Kind::Module => {
                let import = if let Some(reexport) = &self.reexported_in {
                    Import::Unqualified {
                        module: reexport.clone(),
                        name:   self.name.as_str().into(),
                    }
                } else {
                    Import::Qualified { module: self.defined_in.clone() }
                };
                iter::once(import).collect()
            }
            Kind::Type => {
                let imported_from = self.reexported_in.as_ref().unwrap_or(&self.defined_in);
                iter::once(Import::Unqualified {
                    module: imported_from.clone(),
                    name:   self.name.as_str().into(),
                })
                .collect()
            }
            Kind::Function | Kind::Local => default(),
        }
    }

    fn self_type_entry(&self, db: &SuggestionDatabase) -> Option<Rc<Entry>> {
        let self_type_ref = self.self_type.as_ref();
        let lookup = self_type_ref.and_then(|tp| db.lookup_by_qualified_name(tp));
        lookup.map(|(_, entry)| entry)
    }

    fn defined_in_entry(&self, db: &SuggestionDatabase) -> Option<Rc<Entry>> {
        let lookup = db.lookup_by_qualified_name(&self.defined_in);
        lookup.map(|(_, entry)| entry)
    }
}


// === Other Properties ===

impl Entry {
    /// Return the Method Id of suggested method.
    ///
    /// Returns none, if this is not suggestion for a method.
    pub fn method_id(&self) -> Option<MethodId> {
        if self.kind != Kind::Method {
            None
        } else {
            self.self_type.as_ref().map(|self_type| MethodId {
                module:          self.defined_in.clone(),
                defined_on_type: self_type.clone(),
                name:            self.name.clone(),
            })
        }
    }

    /// Checks if entry is visible at given location in a specific module.
    pub fn is_visible_at(&self, location: &ModuleSpan) -> bool {
        let ModuleSpan { module, span } = location;
        match &self.scope {
            Scope::Everywhere => true,
            Scope::InModule { range } => self.defined_in == *module && range.contains(span),
        }
    }

    /// Checks if entry name matches the given name. The matching is case-insensitive.
    pub fn matches_name(&self, name: impl Str) -> bool {
        self.name.to_lowercase() == name.as_ref().to_lowercase()
    }

    /// Generate information about invoking this entity for span tree context.
    pub fn invocation_info(
        &self,
        suggestion_db: &SuggestionDatabase,
        parser: &parser::Parser,
    ) -> span_tree::generate::context::CalledMethodInfo {
        let parameters = self
            .arguments
            .iter()
            .map(|arg| to_span_tree_param(arg, suggestion_db, parser))
            .collect();
        let is_static = self.is_static;
        let is_constructor = matches!(self.kind, Kind::Constructor);
        span_tree::generate::context::CalledMethodInfo {
            is_static,
            is_constructor,
            parameters,
            ..default()
        }
    }

    /// Get the full qualified name of the entry.
    pub fn qualified_name(&self) -> QualifiedName {
        if self.kind == Kind::Module {
            self.defined_in.clone()
        } else {
            let parent_path = match self.kind {
                Kind::Method | Kind::Constructor =>
                    self.self_type.as_ref().unwrap_or(&self.defined_in),
                Kind::Function | Kind::Local | Kind::Type => &self.defined_in,
                Kind::Module => unreachable!(),
            };
            parent_path.clone().new_child(&self.name)
        }
    }

    /// Get the fully qualified name of the parent module of this entry. Returns [`None`] if
    /// the entry represents a top-level module.
    pub fn parent_module(&self) -> Option<QualifiedNameRef> {
        match self.kind {
            Kind::Module => self.defined_in.parent(),
            _ => Some(self.defined_in.as_ref()),
        }
    }

    /// Returns true if this entry is a main module of the project.
    pub fn is_main_module(&self) -> bool {
        match self.kind {
            Kind::Module => self.defined_in.is_main_module(),
            _ => false,
        }
    }
}


// === Handling LanguageServer Types ===

impl Entry {
    /// Create entry from the structure deserialized from the Language Server responses.
    pub fn from_ls_entry(mut entry: language_server::types::SuggestionEntry) -> Self {
        use language_server::types::SuggestionEntry::*;

        fn to_qualified_name(s: String) -> QualifiedName {
            s.as_str().try_into().unwrap_or_else(|_| {
                let standard_base = name::project::QualifiedName::standard_base_library();
                error!(
                    "Invalid qualified name \"{s}\" received from the Engine in SuggestionEntry. \
                    Assuming an entity in {standard_base} project"
                );
                QualifiedName::new_child(standard_base.into(), s)
            })
        }

        let documentation = match &entry {
            Type { documentation, .. }
            | Constructor { documentation, .. }
            | Method { documentation, .. }
            | Module { documentation, .. }
            | Function { documentation, .. }
            | Local { documentation, .. } =>
                documentation.as_ref().map(|s| s.as_ref()).unwrap_or_default(),
        };
        let doc_sections = enso_doc_parser::parse(documentation);
        let icon_name = find_icon_name_in_doc_sections(&doc_sections);
        let reexported_in: Option<QualifiedName> = match &mut entry {
            Type { reexport: Some(reexport), .. }
            | Constructor { reexport: Some(reexport), .. }
            | Method { reexport: Some(reexport), .. }
            | Module { reexport: Some(reexport), .. } =>
                Some(to_qualified_name(mem::take(reexport))),
            _ => None,
        };
        let mut this = match entry {
            Type { name, module, params, .. } =>
                Self::new_type(to_qualified_name(module), name).with_arguments(params),
            Constructor { name, arguments, return_type, .. } =>
                Self::new_constructor(to_qualified_name(return_type), name)
                    .with_arguments(arguments),
            Method { name, module, self_type, return_type, is_static, arguments, .. } =>
                Self::new_method(
                    to_qualified_name(module),
                    to_qualified_name(self_type),
                    name,
                    to_qualified_name(return_type),
                    is_static,
                )
                .with_arguments(arguments),
            Function { name, module, return_type, scope, .. } => Self::new_function(
                to_qualified_name(module),
                name,
                to_qualified_name(return_type),
                scope.into(),
            ),
            Local { name, module, return_type, scope, .. } => Self::new_local(
                to_qualified_name(module),
                name,
                to_qualified_name(return_type),
                scope.into(),
            ),
            Module { module, .. } => Self::new_module(to_qualified_name(module)),
        };
        this.documentation = doc_sections;
        this.icon_name = icon_name;
        this.reexported_in = reexported_in;
        this
    }

    /// Apply modification to the entry.
    pub fn apply_modifications(
        &mut self,
        modification: SuggestionsDatabaseModification,
    ) -> Vec<failure::Error> {
        let m = modification;
        let module = m.module.map(|f| f.try_map(QualifiedName::from_text)).transpose();
        let return_type = m.return_type.map(|f| f.try_map(QualifiedName::from_text)).transpose();
        let self_type = m.self_type.map(|f| f.try_map(QualifiedName::from_text)).transpose();
        let reexport = m.reexport.map(|f| f.try_map(QualifiedName::from_text)).transpose();
        let docs = m.documentation.map(|docs| docs.map(|docs| enso_doc_parser::parse(&docs)));
        let update_results = [
            return_type
                .and_then(|m| Entry::apply_field_update("return_type", &mut self.return_type, m)),
            Entry::apply_default_field_update("documentation", &mut self.documentation, docs),
            module.and_then(|m| Entry::apply_field_update("module", &mut self.defined_in, m)),
            self_type
                .and_then(|s| Entry::apply_opt_field_update("self_type", &mut self.self_type, s)),
            self.apply_scope_update(m.scope),
            reexport.and_then(|r| {
                Entry::apply_opt_field_update("reexport", &mut self.reexported_in, r)
            }),
        ];
        let other_update_errors = update_results.into_iter().filter_map(|res| res.err());
        let arg_update_errors = m.arguments.into_iter().flat_map(|arg| self.apply_arg_update(arg));
        arg_update_errors.chain(other_update_errors).collect_vec()
    }

    fn apply_arg_update(
        &mut self,
        update: language_server::types::SuggestionArgumentUpdate,
    ) -> Vec<failure::Error> {
        use language_server::types::SuggestionArgumentUpdate as Update;
        let error = |index| {
            let name = self.name.clone();
            vec![failure::Error::from(InvalidArgumentIndex { name, index })]
        };
        match update {
            Update::Add { index, .. } if index > self.arguments.len() => error(index),
            Update::Remove { index } | Update::Modify { index, .. }
                if index >= self.arguments.len() =>
                error(index),
            Update::Add { index, argument } => {
                self.arguments.insert(index, argument);
                vec![]
            }
            Update::Remove { index } => {
                self.arguments.remove(index);
                vec![]
            }
            Update::Modify { index, name, repr_type, is_suspended, has_default, default_value } => {
                let arg = &mut self.arguments[index];
                type E = Entry;
                let results = [
                    E::apply_field_update("name", &mut arg.name, name),
                    E::apply_field_update("repr_type", &mut arg.repr_type, repr_type),
                    E::apply_field_update("is_suspended", &mut arg.is_suspended, is_suspended),
                    E::apply_field_update("has_default", &mut arg.has_default, has_default),
                    E::apply_opt_field_update(
                        "default_value",
                        &mut arg.default_value,
                        default_value,
                    ),
                ];
                SmallVec::from_buf(results).into_iter().filter_map(|res| res.err()).collect_vec()
            }
        }
    }

    fn apply_scope_update(
        &mut self,
        update: Option<FieldUpdate<language_server::types::SuggestionEntryScope>>,
    ) -> FallibleResult {
        if let Some(update) = update {
            let err = || Err(failure::Error::from(InvalidFieldUpdate("scope")));
            match &mut self.scope {
                Scope::Everywhere => err(),
                Scope::InModule { range } =>
                    if let Some(value) = update.value {
                        *range = value.into();
                        Ok(())
                    } else {
                        err()
                    },
            }
        } else {
            Ok(())
        }
    }

    fn apply_field_update<T>(
        field_name: &'static str,
        field: &mut T,
        update: Option<FieldUpdate<T>>,
    ) -> FallibleResult {
        let err = InvalidFieldUpdate(field_name);
        if let Some(update) = update {
            match update.tag {
                FieldAction::Set => {
                    *field = update.value.ok_or(err)?;
                    Ok(())
                }
                FieldAction::Remove => Err(err.into()),
            }
        } else {
            Ok(())
        }
    }

    fn apply_opt_field_update<T>(
        field_name: &'static str,
        field: &mut Option<T>,
        update: Option<FieldUpdate<T>>,
    ) -> FallibleResult {
        let err = InvalidFieldUpdate(field_name);
        if let Some(update) = update {
            match update.tag {
                FieldAction::Set => *field = Some(update.value.ok_or(err)?),
                FieldAction::Remove => *field = None,
            }
        }
        Ok(())
    }

    /// Apply an update to a field that can be removed by settings its value to its type's default.
    fn apply_default_field_update<T: Default>(
        field_name: &'static str,
        field: &mut T,
        update: Option<FieldUpdate<T>>,
    ) -> FallibleResult {
        let err = InvalidFieldUpdate(field_name);
        if let Some(update) = update {
            *field = match update.tag {
                FieldAction::Set => update.value.ok_or(err)?,
                FieldAction::Remove => default(),
            };
        }
        Ok(())
    }
}

impl TryFrom<&Entry> for language_server::MethodPointer {
    type Error = failure::Error;
    fn try_from(entry: &Entry) -> FallibleResult<Self> {
        let is_method_or_constructor = matches!(entry.kind, Kind::Method | Kind::Constructor);
        is_method_or_constructor.ok_or_else(|| NotAMethod(entry.name.clone()))?;
        let missing_this_err = || MissingSelfOnMethod(entry.name.clone());
        let defined_on_type = entry.self_type.clone().ok_or_else(missing_this_err)?;
        Ok(language_server::MethodPointer {
            defined_on_type: defined_on_type.into(),
            module:          entry.defined_in.to_string(),
            name:            entry.name.clone(),
        })
    }
}

impl TryFrom<Entry> for language_server::MethodPointer {
    type Error = failure::Error;
    fn try_from(entry: Entry) -> FallibleResult<Self> {
        language_server::MethodPointer::try_from(&entry)
    }
}



// ===============
// === Helpers ===
// ===============


// === SpanTree helpers ===

/// Converts the information about function parameter from suggestion database into the form used
/// by the span tree nodes.
pub fn to_span_tree_param(
    param_info: &Argument,
    db: &SuggestionDatabase,
    parser: &parser::Parser,
) -> span_tree::ArgumentInfo {
    let tag_values = argument_tag_values(&param_info.tag_values, db, parser);
    span_tree::ArgumentInfo {
        // TODO [mwu] Check if database actually do must always have both of these filled.
        name: Some(param_info.name.clone()),
        tp: Some(param_info.repr_type.clone()),
        call_id: None,
        tag_values,
    }
}

enum TagValueResolution<'a> {
    Resolved(Rc<Entry>, ast::opr::Chain),
    Parsed(ast::opr::Chain),
    Unresolved(&'a str),
}

fn resolve_tag_value<'a>(
    raw_expression: &'a str,
    db: &SuggestionDatabase,
    parser: &parser::Parser,
) -> TagValueResolution<'a> {
    let qualified_name = QualifiedName::from_text(raw_expression).ok();
    if let Some(mut qualified_name) = qualified_name {
        let entry = db.lookup_by_qualified_name(&qualified_name).or_else(move || {
            // Second try at lookup. Enum values usually have the module and type name deduplicated,
            // but the resolution database does not take that into account. So we need to manually
            // expand the name and try again.
            let last_segment = qualified_name.pop_segment()?;
            let segment_to_duplicate = qualified_name.pop_segment()?;
            qualified_name.push_segment(segment_to_duplicate.clone());
            qualified_name.push_segment(segment_to_duplicate);
            qualified_name.push_segment(last_segment);
            db.lookup_by_qualified_name(&qualified_name)
        });

        let resolved = entry.and_then(|(_, entry)| {
            let qualified_name: String = entry.qualified_name().to_string();
            let ast = parser.parse_line_ast(qualified_name).ok()?;
            let chain = ast::opr::as_access_chain(&ast)?;
            Some(TagValueResolution::Resolved(entry, chain))
        });

        if let Some(resolved) = resolved {
            return resolved;
        }
    }

    let chain =
        parser.parse_line_ast(raw_expression).ok().and_then(|ast| ast::opr::as_access_chain(&ast));
    if let Some(chain) = chain {
        return TagValueResolution::Parsed(chain);
    }

    TagValueResolution::Unresolved(raw_expression)
}

/// Generate tag value suggestion list from argument entry data, shortening labels as appropriate.
pub fn argument_tag_values(
    raw_expressions: &[String],
    db: &SuggestionDatabase,
    parser: &parser::Parser,
) -> Vec<span_tree::TagValue> {
    let resolved = raw_expressions.iter().map(|e| resolve_tag_value(e, db, parser)).collect_vec();
    let mut only_access_chains_iter = resolved.iter().filter_map(|r| match r {
        TagValueResolution::Resolved(_, chain) => Some(chain),
        TagValueResolution::Parsed(chain) => Some(chain),
        _ => None,
    });

    // Gather a list of expression reprs from first access chain. Includes each infix element
    // that can be considered for removal.
    let operands: Option<Vec<String>> = only_access_chains_iter.next().map(|chain| {
        let mut operands = chain
            .enumerate_operands()
            .map(|operand| operand.map_or_default(|op| op.arg.repr()))
            .collect_vec();
        // Pop last chain element, as we never want to strip it from the label.
        let last = operands.pop();

        // If the last chain element is a "Value" literal, we want to preserve one extra chain
        // element before it.
        if last.map_or(false, |last| last == "Value") {
            operands.pop();
        }
        operands
    });

    // Find the number of operands that are common for all access chains.
    let common_operands_count: usize = operands.map_or(0, |common_operands| {
        only_access_chains_iter.fold(common_operands.len(), |common_so_far, chain| {
            let operand_reprs =
                chain.enumerate_operands().map(|op| op.map_or_default(|op| op.arg.repr()));
            operand_reprs
                .zip(&common_operands[0..common_so_far])
                .take_while(|(repr, common_repr)| repr == *common_repr)
                .count()
        })
    });

    let chain_to_label = move |chain: &ast::opr::Chain| {
        if common_operands_count > 0 {
            let mut chain = chain.clone();
            chain.erase_leading_operands(common_operands_count);
            Some(chain.into_ast().repr())
        } else {
            None
        }
    };

    resolved
        .into_iter()
        .map(|resolution| match resolution {
            TagValueResolution::Resolved(entry, chain) => {
                let label = chain_to_label(&chain);
                let qualified_name = entry.qualified_name();
                let parent_module = qualified_name.parent();
                let required_import = parent_module.as_ref().map(|n| n.to_string());

                let expression = if let Some(parent) = parent_module {
                    let in_module_name = qualified_name.name();
                    let parent_name = parent.name();
                    [parent_name, in_module_name].join(opr::predefined::ACCESS)
                } else {
                    qualified_name.to_string()
                };
                let expression =
                    if entry.arguments.is_empty() { expression } else { format!("({expression})") };

                span_tree::TagValue { required_import, expression, label }
            }
            TagValueResolution::Parsed(chain) => {
                let label = chain_to_label(&chain);
                let expression = chain.into_ast().repr();

                span_tree::TagValue { required_import: None, expression, label }
            }
            TagValueResolution::Unresolved(expression) => span_tree::TagValue {
                required_import: None,
                expression:      expression.to_owned(),
                label:           None,
            },
        })
        .collect_vec()
}



// === Entry helpers ===

fn find_icon_name_in_doc_sections<'a, I>(doc_sections: I) -> Option<IconName>
where I: IntoIterator<Item = &'a DocSection> {
    doc_sections.into_iter().find_map(|section| match section {
        DocSection::Keyed { key, body } if key == ICON_DOC_SECTION_KEY => {
            let icon_name = IconName::from_snake_case(body);
            let as_snake_case = icon_name.to_snake_case();
            if as_snake_case.as_str() != body.as_str() || !body.is_case(Case::Snake) {
                let msg = format!(
                    "The icon name {body} used in the {ICON_DOC_SECTION_KEY} section of the \
                    documentation of a component is not a valid, losslessly-convertible snake_case \
                    identifier. The component may be displayed with a different icon than expected."
                );
                warn!("{msg}");
            }
            Some(icon_name)
        }
        _ => None,
    })
}



// =============
// === Tests ===
// =============

#[cfg(test)]
mod test {
    use super::*;

    use engine_protocol::language_server::SuggestionArgumentUpdate;
    use parser::Parser;

    use crate::mock;
    use crate::mock_suggestion_database;
    use enso_text::index::Line;
    use enso_text::Utf16CodeUnit;

    #[test]
    fn code_from_entry() {
        let module_name: QualifiedName = "local.Project.Module".try_into().unwrap();
        let main_module_name: QualifiedName = "local.Project.Main".try_into().unwrap();
        let tp_name: QualifiedName = "local.Project.Test_Type".try_into().unwrap();
        let return_type: QualifiedName = "Standard.Base.Number".try_into().unwrap();
        let scope = Location { line: Line(3), offset: Utf16CodeUnit(0) }..=Location {
            line:   Line(10),
            offset: Utf16CodeUnit(0),
        };
        let tp = Entry::new_type(module_name.clone(), "Type");
        let constructor = Entry::new_constructor(tp_name.clone(), "Constructor");
        let method =
            Entry::new_nonextension_method(tp_name.clone(), "method", return_type.clone(), false);
        let static_method =
            Entry::new_nonextension_method(tp_name, "static_method", return_type.clone(), true);
        let module_method =
            Entry::new_module_method(module_name.clone(), "module_method", return_type.clone());
        let main_module_method = Entry::new_module_method(
            main_module_name.clone(),
            "module_method",
            return_type.clone(),
        );
        let function = Entry::new_function(
            module_name.clone(),
            "function",
            return_type.clone(),
            scope.clone(),
        );
        let local = Entry::new_local(module_name.clone(), "local", return_type, scope);
        let module = Entry::new_module(module_name);
        let main_module = Entry::new_module(main_module_name);

        for generate_this in [true, false] {
            assert_eq!(tp.code_to_insert(generate_this), "Type");
            assert_eq!(function.code_to_insert(generate_this), "function");
            assert_eq!(local.code_to_insert(generate_this), "local");
            assert_eq!(module.code_to_insert(generate_this), "Module");
            assert_eq!(main_module.code_to_insert(generate_this), "Project");
            assert_eq!(constructor.code_to_insert(generate_this), "Test_Type.Constructor");
        }
        assert_eq!(method.code_to_insert(false), "method");
        assert_eq!(method.code_to_insert(true), "_.method");
        assert_eq!(static_method.code_to_insert(false), "static_method");
        assert_eq!(static_method.code_to_insert(true), "Test_Type.static_method");
        assert_eq!(module_method.code_to_insert(false), "module_method");
        assert_eq!(module_method.code_to_insert(true), "Module.module_method");
        assert_eq!(main_module_method.code_to_insert(false), "module_method");
        assert_eq!(main_module_method.code_to_insert(true), "Project.module_method");
    }

    #[test]
    fn required_imports_of_entry() {
        let db = mock::standard_db_mock();
        const CURRENT_MODULE_VARIANTS: [&str; 2] =
            ["local.Project.Submodule", "local.Another_Project"];
        let current_modules =
            CURRENT_MODULE_VARIANTS.map(|txt| QualifiedName::try_from(txt).unwrap());

        let expect_imports = |entry: &Entry, current_module: &QualifiedName, expected: &[&str]| {
            let entry_imports = entry.required_imports(&db, current_module.as_ref()).into_iter();
            let imports_as_strings = entry_imports.map(|entry_import| {
                let import: import::Info = entry_import.into();
                import.to_string()
            });
            assert_eq!(imports_as_strings.collect_vec().as_slice(), expected);
        };
        let expect_no_import = |entry: &Entry, current_module: &QualifiedName| {
            assert!(entry.required_imports(&db, current_module.as_ref()).is_empty())
        };

        let number = db.lookup_by_qualified_name_str("Standard.Base.Number").unwrap();
        let some = db.lookup_by_qualified_name_str("Standard.Base.Maybe.Some").unwrap();
        let method = db.lookup_by_qualified_name_str("Standard.Base.Maybe.is_some").unwrap();
        let static_method = db
            .lookup_by_qualified_name_str("local.Project.Submodule.TestType.static_method")
            .unwrap();
        let module_method =
            db.lookup_by_qualified_name_str("local.Project.Submodule.module_method").unwrap();
        let submodule = db.lookup_by_qualified_name_str("local.Project.Submodule").unwrap();

        let defined_in = "local.Project.Submodule".try_into().unwrap();
        let on_type = "Standard.Base.Number".try_into().unwrap();
        let return_type = "Standard.Base.Boolean".try_into().unwrap();
        let extension_method =
            Rc::new(Entry::new_method(defined_in, on_type, "extension_method", return_type, true));

        for cm in &current_modules {
            expect_imports(&number, cm, &["from Standard.Base import Number"]);
            expect_imports(&some, cm, &["from Standard.Base import Maybe"]);
            expect_no_import(&method, cm);
        }

        expect_imports(&static_method, &current_modules[0], &[]);
        expect_imports(&module_method, &current_modules[0], &[]);
        expect_imports(&submodule, &current_modules[0], &[]);
        expect_imports(&extension_method, &current_modules[0], &[
            "from Standard.Base import Number",
        ]);

        expect_imports(&static_method, &current_modules[1], &[
            "from local.Project.Submodule import TestType",
        ]);
        expect_imports(&module_method, &current_modules[1], &["import local.Project.Submodule"]);
        expect_imports(&submodule, &current_modules[1], &["import local.Project.Submodule"]);
        expect_imports(&extension_method, &current_modules[1], &[
            "import local.Project.Submodule",
            "from Standard.Base import Number",
        ]);
    }

    #[test]
    fn required_imports_of_reexported_entries() {
        let db = mock_suggestion_database! {
            Standard.Base {
                mod Data {
                    #[reexported_in("Standard.Base".try_into().unwrap())]
                    type Type {
                        Type (a);
                        static fn static_method() -> Standard.Base.Boolean;
                    }

                    #[reexported_in("Standard.Base".try_into().unwrap())]
                    mod Submodule {
                        static fn module_method() -> local.Project.Submodule.TestType;
                    }
                }
            }
        };
        let current_module = QualifiedName::from_text("local.Project").unwrap();
        let expect_imports = |entry: Rc<Entry>, expected: &[&str]| {
            let entry_imports = entry.required_imports(&db, current_module.as_ref()).into_iter();
            let imports_as_strings = entry_imports.map(|entry_import| {
                let import: import::Info = entry_import.into();
                import.to_string()
            });
            assert_eq!(imports_as_strings.collect_vec().as_slice(), expected);
        };

        let tp = db.lookup_by_qualified_name_str("Standard.Base.Data.Type").unwrap();
        let constructor = db.lookup_by_qualified_name_str("Standard.Base.Data.Type.Type").unwrap();
        let static_method =
            db.lookup_by_qualified_name_str("Standard.Base.Data.Type.static_method").unwrap();
        let module_method =
            db.lookup_by_qualified_name_str("Standard.Base.Data.Submodule.module_method").unwrap();
        let submodule = db.lookup_by_qualified_name_str("Standard.Base.Data.Submodule").unwrap();

        expect_imports(tp, &["from Standard.Base import Type"]);
        expect_imports(constructor, &["from Standard.Base import Type"]);
        expect_imports(static_method, &["from Standard.Base import Type"]);
        expect_imports(module_method, &["from Standard.Base import Submodule"]);
        expect_imports(submodule, &["from Standard.Base import Submodule"]);
    }

    #[test]
    fn method_id_from_entry() {
        let module: QualifiedName = "test.Test.Test".try_into().unwrap();
        let self_type: QualifiedName = "Standard.Base.Number".try_into().unwrap();
        let return_type: QualifiedName = "Standard.Base.Number".try_into().unwrap();
        let non_method = Entry::new_function(
            module.clone(),
            "function",
            return_type.clone(),
            default()..=default(),
        );
        let method =
            Entry::new_method(module.clone(), self_type.clone(), "method", return_type, false);
        let expected = MethodId { module, defined_on_type: self_type, name: "method".to_owned() };
        assert_eq!(non_method.method_id(), None);
        assert_eq!(method.method_id(), Some(expected));
    }

    /// Test the result of the [`Entry::qualified_name`] method when applied to entries with
    /// different values of [`Entry::kind`].
    #[test]
    fn qualified_name_of_entry() {
        fn expect(entry: Entry, expected_qualified_name: &str) {
            let entry_qualified_name = entry.qualified_name();
            assert_eq!(entry_qualified_name.to_string(), expected_qualified_name);
        }
        let defined_in = "TestProject.TestModule".try_into().unwrap();
        let tp = Entry::new_type(defined_in, "TestType");
        expect(tp, "TestProject.TestModule.TestType");

        let of_type = "TestProject.TestModule.TestType".try_into().unwrap();
        let constructor = Entry::new_constructor(of_type, "TestConstructor");
        expect(constructor, "TestProject.TestModule.TestType.TestConstructor");

        let on_type = "Standard.Builtins.Main.System".try_into().unwrap();
        let return_type = "Standard.Builtins.Main.System_Process_Result".try_into().unwrap();
        let method = Entry::new_nonextension_method(on_type, "create_process", return_type, true);
        expect(method, "Standard.Builtins.System.create_process");

        let module = Entry::new_module("local.Unnamed_6.Main".try_into().unwrap());
        expect(module, "local.Unnamed_6");

        let defined_in = "local.Unnamed_6.Main".try_into().unwrap();
        let return_type = "Standard.Base.Data.Vector.Vector".try_into().unwrap();
        let local = Entry::new_local(defined_in, "operator1", return_type, default()..=default());
        expect(local, "local.Unnamed_6.operator1");

        let defined_in = "NewProject.NewModule".try_into().unwrap();
        let return_type = "Standard.Base.Data.Vector.Vector".try_into().unwrap();
        let function =
            Entry::new_function(defined_in, "testFunction1", return_type, default()..=default());
        expect(function, "NewProject.NewModule.testFunction1");
    }

    /// Test [`find_icon_name_in_doc_sections`] function extracting a name of an icon from the body
    /// of a keyed [`DocSection`] which has its key equal to the `Icon` string.
    #[test]
    fn find_icon_name_in_doc_section_with_icon_key() {
        use enso_doc_parser::DocSection;
        let doc_sections = [
            DocSection::Paragraph { body: "Some paragraph.".into() },
            DocSection::Keyed { key: "NotIcon".into(), body: "example_not_icon_body".into() },
            DocSection::Keyed { key: "Icon".into(), body: "example_icon_name".into() },
            DocSection::Paragraph { body: "Another paragraph.".into() },
        ];
        let icon_name = find_icon_name_in_doc_sections(&doc_sections).unwrap();
        assert_eq!(icon_name.to_pascal_case(), "ExampleIconName");
    }

    /// Test case-insensitive comparison of [`IconName`] values and case-insensitiveness when
    /// converting [`IconName`] values to PascalCase.
    #[test]
    fn icon_name_case_insensitiveness() {
        let name_from_small_snake_case = IconName::from_snake_case("an_example_name");
        let name_from_mixed_snake_case = IconName::from_snake_case("aN_EXAMPLE_name");
        const PASCAL_CASE_NAME: &str = "AnExampleName";
        assert_eq!(name_from_small_snake_case, name_from_mixed_snake_case);
        assert_eq!(name_from_small_snake_case.to_pascal_case(), PASCAL_CASE_NAME);
        assert_eq!(name_from_mixed_snake_case.to_pascal_case(), PASCAL_CASE_NAME);
    }

    struct ApplyModificationTest {
        modified_entry: Entry,
        expected_entry: Entry,
    }

    impl ApplyModificationTest {
        fn new() -> Self {
            let defined_in = "local.Project.Module".try_into().unwrap();
            let on_type = "local.Project.Module.Type".try_into().unwrap();
            let return_type = "Standard.Base.Number".try_into().unwrap();
            let argument = Argument {
                name:          "x".to_owned(),
                repr_type:     "Standard.Base.Any".to_owned(),
                is_suspended:  false,
                has_default:   false,
                default_value: None,
                tag_values:    Vec::new(),
            };
            let entry = Entry::new_method(defined_in, on_type, "entry", return_type, true)
                .with_arguments(vec![argument]);
            Self { modified_entry: entry.clone(), expected_entry: entry }
        }

        fn check_modification(
            &mut self,
            modification: SuggestionsDatabaseModification,
        ) -> Vec<failure::Error> {
            let result = self.modified_entry.apply_modifications(modification);
            assert_eq!(self.modified_entry, self.expected_entry);
            result
        }
    }


    #[test]
    fn applying_empty_modification() {
        let mut test = ApplyModificationTest::new();
        assert!(test.check_modification(default()).is_empty());
    }

    #[test]
    fn applying_simple_fields_modification() {
        let mut test = ApplyModificationTest::new();
        let new_documentation = "NewDocumentation";
        let modification = SuggestionsDatabaseModification {
            arguments:     vec![],
            module:        Some(FieldUpdate::set("local.Project.NewModule".to_owned())),
            self_type:     Some(FieldUpdate::set("local.Project.NewModule.NewType".to_owned())),
            return_type:   Some(FieldUpdate::set(
                "local.Project.NewModule.NewReturnType".to_owned(),
            )),
            documentation: Some(FieldUpdate::set(new_documentation.to_owned())),
            scope:         None,
            reexport:      Some(FieldUpdate::set("local.Project.NewReexport".to_owned())),
        };
        test.expected_entry.defined_in = "local.Project.NewModule".try_into().unwrap();
        test.expected_entry.self_type = Some("local.Project.NewModule.NewType".try_into().unwrap());
        test.expected_entry.return_type =
            "local.Project.NewModule.NewReturnType".try_into().unwrap();
        test.expected_entry.documentation = enso_doc_parser::parse(new_documentation);
        test.expected_entry.reexported_in = Some("local.Project.NewReexport".try_into().unwrap());
        let result = test.check_modification(modification);
        assert!(result.is_empty());
    }

    #[test]
    fn removing_field_values() {
        let mut test = ApplyModificationTest::new();
        let modification = SuggestionsDatabaseModification {
            documentation: Some(FieldUpdate::remove()),
            ..default()
        };
        let result = test.check_modification(modification);
        assert!(result.is_empty());
    }

    #[test]
    fn partially_invalid_update() {
        let mut test = ApplyModificationTest::new();
        let new_scope = Location { line: Line(0), offset: Utf16CodeUnit(0) }..=Location {
            line:   Line(10),
            offset: Utf16CodeUnit(10),
        };
        let modification = SuggestionsDatabaseModification {
            module: Some(FieldUpdate::set("local.Project.NewModule".to_owned())),
            documentation: Some(FieldUpdate::remove()),
            scope: Some(FieldUpdate::set(new_scope.into())),
            ..default()
        };
        test.expected_entry.defined_in = "local.Project.NewModule".try_into().unwrap();
        let result = test.check_modification(modification);
        assert_eq!(result.len(), 1);
    }

    #[test]
    fn adding_and_modifying_argument() {
        let mut test = ApplyModificationTest::new();
        let new_argument = Argument {
            name:          "new_arg".to_string(),
            repr_type:     "local.Project.NewReturnType".to_string(),
            is_suspended:  false,
            has_default:   false,
            default_value: None,
            tag_values:    Vec::new(),
        };
        let add_argument =
            SuggestionArgumentUpdate::Add { index: 1, argument: new_argument.clone() };
        let modify_argument = SuggestionArgumentUpdate::Modify {
            index:         0,
            name:          Some(FieldUpdate::set("new_name".to_string())),
            repr_type:     Some(FieldUpdate::set("local.Project.NewReturnType".to_string())),
            is_suspended:  None,
            has_default:   None,
            default_value: None,
        };
        test.expected_entry.arguments[0].name = "new_name".to_string();
        test.expected_entry.arguments[0].repr_type =
            "local.Project.NewReturnType".try_into().unwrap();
        test.expected_entry.arguments.push(new_argument);
        let modification = SuggestionsDatabaseModification {
            arguments: vec![add_argument, modify_argument],
            ..default()
        };
        let result = test.check_modification(modification);
        assert!(result.is_empty())
    }

    #[test]
    fn remove_argument() {
        let mut test = ApplyModificationTest::new();
        let remove_argument = SuggestionArgumentUpdate::Remove { index: 0 };
        test.expected_entry.arguments.pop();
        let modification =
            SuggestionsDatabaseModification { arguments: vec![remove_argument], ..default() };
        let result = test.check_modification(modification);
        assert!(result.is_empty());
    }

    fn run_tag_value_test_case(expression_and_expected_label: &[(&str, Option<&str>)]) {
        let parser = Parser::new();
        let db = SuggestionDatabase::new_empty();
        let expressions =
            expression_and_expected_label.iter().map(|(expr, _)| expr.to_string()).collect_vec();
        let tag_values = argument_tag_values(&expressions, &db, &parser);
        let expected_values = expression_and_expected_label
            .iter()
            .map(|(expression, label)| span_tree::TagValue {
                required_import: None,
                expression:      expression.to_string(),
                label:           label.map(ToString::to_string),
            })
            .collect_vec();

        assert_eq!(tag_values, expected_values);
    }

    #[test]
    fn tag_value_shortening_single_entry() {
        run_tag_value_test_case(&[("Location.Start", Some("Start"))]);
    }

    #[test]
    fn tag_value_shortening_single_entry_with_value() {
        run_tag_value_test_case(&[("Foo.Bar.Value", Some("Bar.Value"))]);
    }

    #[test]
    fn tag_value_shortening_common_prefix() {
        run_tag_value_test_case(&[
            ("Location.Start", Some("Start")),
            ("Location.End", Some("End")),
            ("Location.Both", Some("Both")),
        ]);
    }

    #[test]
    fn tag_value_shortening_multiple_elements() {
        run_tag_value_test_case(&[
            ("A.B.C.D", Some("C.D")),
            ("A.B.C.E", Some("C.E")),
            ("A.B.F.G.H", Some("F.G.H")),
        ]);
    }

    #[test]
    fn tag_value_shortening_no_prefix() {
        run_tag_value_test_case(&[("Foo.Bar", None), ("Foo.Baz", None), ("Baz.Qux", None)]);
    }

    #[test]
    fn tag_value_non_infix() {
        run_tag_value_test_case(&[("Foo Bar", None), ("Foo Baz", None), ("Baz Qux", None)]);
    }


    #[test]
    fn tag_value_some_infix() {
        run_tag_value_test_case(&[
            ("Foo.Bar", Some("Bar")),
            ("Foo.Baz", Some("Baz")),
            ("Baz Qux", None),
        ]);
    }
}
