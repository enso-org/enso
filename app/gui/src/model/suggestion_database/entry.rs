//! Module with the Suggestion Database Entry and all structures related to.

use crate::prelude::*;

use crate::model::module::MethodId;

use ast::opr;
use convert_case::Case;
use convert_case::Casing;
use double_representation::identifier::ReferentName;
use double_representation::import;
use double_representation::module;
use double_representation::tp;
use engine_protocol::language_server;
use engine_protocol::language_server::FieldUpdate;
use engine_protocol::language_server::SuggestionsDatabaseModification;
use enso_text::Location;
use language_server::types::FieldAction;
use std::collections::BTreeSet;


// ==============
// === Export ===
// ==============

use crate::model::SuggestionDatabase;
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



// =====================
// === QualifiedName ===
// =====================

im_string_newtype! {
    /// A single segment of a [`QualifiedName`] of an [`Entry`].
    QualifiedNameSegment
}

/// A fully qualified name of an [`Entry`].
#[derive(Debug, Default, Clone, PartialEq, Eq)]
#[allow(missing_docs)]
pub struct QualifiedName {
    pub segments: Vec<QualifiedNameSegment>,
}

impl From<&str> for QualifiedName {
    fn from(name: &str) -> Self {
        name.split(ast::opr::predefined::ACCESS).collect()
    }
}

impl From<String> for QualifiedName {
    fn from(name: String) -> Self {
        name.as_str().into()
    }
}

impl From<QualifiedName> for String {
    fn from(name: QualifiedName) -> Self {
        String::from(&name)
    }
}

impl From<&QualifiedName> for String {
    fn from(name: &QualifiedName) -> Self {
        name.into_iter().map(|s| s.deref()).join(ast::opr::predefined::ACCESS)
    }
}

impl From<module::QualifiedName> for QualifiedName {
    fn from(name: module::QualifiedName) -> Self {
        name.segments().collect()
    }
}

impl Display for QualifiedName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let text = String::from(self);
        Display::fmt(&text, f)
    }
}

impl<T> FromIterator<T> for QualifiedName
where T: Into<QualifiedNameSegment>
{
    fn from_iter<I>(iter: I) -> Self
    where I: IntoIterator<Item = T> {
        let segments = iter.into_iter().map(|s| s.into()).collect();
        Self { segments }
    }
}

impl<'a> IntoIterator for &'a QualifiedName {
    type Item = &'a QualifiedNameSegment;
    type IntoIter = std::slice::Iter<'a, QualifiedNameSegment>;
    fn into_iter(self) -> Self::IntoIter {
        self.segments.iter()
    }
}



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
    pub module: module::QualifiedName,
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



// =============
// === Entry ===
// =============

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

/// Represents code snippet and the imports needed for it to work.
/// Typically is module-specific, as different modules may require different imports.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct CodeToInsert<'a> {
    /// Code to be inserted.
    pub code:   Cow<'a, str>,
    /// An import which should be added to have the code working.
    pub import: Option<import::Info>,
}

/// The Suggestion Database Entry.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Entry {
    /// A type of suggestion.
    pub kind:               Kind,
    /// A module where the suggested object is defined.
    pub defined_in:         module::QualifiedName,
    /// A name of suggested object.
    pub name:               String,
    /// Argument lists of suggested object (atom or function). If the object does not take any
    /// arguments, the list is empty.
    pub arguments:          Vec<Argument>,
    /// A type returned by the suggested object.
    pub return_type:        tp::QualifiedName,
    /// A module reexporting this entity.
    pub reexported_in:      Option<module::QualifiedName>,
    /// A HTML documentation associated with object.
    pub documentation_html: Option<String>,
    /// A type of the "self" argument. This field is `None` for non-method suggestions.
    pub self_type:          Option<tp::QualifiedName>,
    /// A flag set to true if the method is a static or module method.
    pub is_static:          bool,
    /// A scope where this suggestion is visible.
    pub scope:              Scope,
    /// A name of a custom icon to use when displaying the entry.
    pub icon_name:          Option<IconName>,
}

impl Entry {
    pub fn new(
        kind: Kind,
        defined_in: module::QualifiedName,
        name: impl Into<String>,
        return_type: tp::QualifiedName,
    ) -> Self {
        Self {
            kind,
            defined_in,
            name: name.into(),
            arguments: vec![],
            return_type,
            is_static: false,
            reexported_in: None,
            documentation_html: None,
            self_type: None,
            scope: Scope::Everywhere,
            icon_name: None,
        }
    }

    pub fn new_method(
        on_type: tp::QualifiedName,
        name: impl Into<String>,
        return_type: tp::QualifiedName,
        is_static: bool,
    ) -> Self {
        let module = on_type.parent_module();
        Self {
            self_type: Some(on_type),
            is_static,
            ..Self::new(Kind::Method, module, name, return_type)
        }
    }

    pub fn new_extension_method(
        defined_in: module::QualifiedName,
        on_type: tp::QualifiedName,
        name: impl Into<String>,
        return_type: tp::QualifiedName,
        is_static: bool,
    ) -> Self {
        Self {
            self_type: Some(on_type),
            is_static,
            ..Self::new(Kind::Method, defined_in, name, return_type)
        }
    }

    pub fn new_type(defined_in: module::QualifiedName, name: impl Into<String>) -> Self {
        let name = name.into();
        let return_type = tp::QualifiedName::new_module_member(defined_in.clone(), name.clone());
        Self::new(Kind::Type, defined_in, name, return_type)
    }

    pub fn new_constructor(of_type: tp::QualifiedName, name: impl Into<String>) -> Self {
        let defined_in_module = of_type.parent_module();
        Self {
            self_type: Some(on_type),
            is_static: true,
            ..Self::new(Kind::Constructor, defined_in_module, name, of_type)
        }
    }

    pub fn new_function(
        defined_in: module::QualifiedName,
        name: impl Into<String>,
        return_type: tp::QualifiedName,
        scope_range: RangeInclusive<Location<enso_text::Utf16CodeUnit>>,
    ) -> Self {
        Self {
            scope: Scope::InModule { range: scope_range },
            ..Self::new(Kind::Function, defined_in, name, return_type)
        }
    }

    pub fn new_local(
        defined_in: module::QualifiedName,
        name: impl Into<String>,
        return_type: impl Into<String>,
        scope_range: RangeInclusive<Location<enso_text::Utf16CodeUnit>>,
    ) -> Self {
        Self {
            scope: Scope::InModule { range: scope_range },
            ..Self::new(Kind::Local, defined_in, name, return_type)
        }
    }

    pub fn new_module(full_name: module::QualifiedName) -> Self {
        let name = String::from(full_name.name());
        let return_type = (&name).into();
        Self::new(Kind::Module, full_name, name, return_type)
    }

    pub fn with_arguments(mut self, arguments: impl IntoIterator<Item = Argument>) -> Self {
        self.arguments = arguments.into_iter().collect();
        self
    }

    pub fn reexported_in(mut self, module: module::QualifiedName) -> Self {
        self.reexported_in = Some(module);
        self
    }

    pub fn with_documentation(mut self, html: String) -> Self {
        self.documentation_html = Some(html);
        self
    }

    pub fn with_icon(mut self, icon_name: IconName) -> Self {
        self.icon_name = Some(icon_name);
        self
    }

    /// Check if this entry has self type same as the given identifier.
    pub fn has_self_type<TypeName>(&self, self_type: &TypeName) -> bool
    where TypeName: PartialEq<tp::QualifiedName> {
        self.self_type.contains(self_type)
    }

    pub fn code_to_insert(&self, generate_this: bool) -> Cow<str> {
        match self.kind {
            Kind::Method if generate_this && self.is_static =>
                if let Some(self_type) = &self.self_type {
                    format!("{}{}{}", self_type.name, opr::predefined::ACCESS, self.name).into()
                } else {
                    format!("_{}{}", opr::predefined::ACCESS, self.name).into()
                },
            Kind::Method if generate_this =>
                format!("_{}{}", opr::predefined::ACCESS, self.name).into(),
            _ => Cow::from(&self.name),
        }
    }

    pub fn required_imports(&self, db: &SuggestionDatabase) -> Option<import::Info> {
        match self.kind {
            //TODO[ao] extension methods.
            Kind::Method if self.is_static => {
                warn!("Checking imports of {self:?}");
                let self_type_entry =
                    self.self_type.as_ref().and_then(|tp| db.lookup_by_qualified_name(tp));
                if let Some((_, entry)) = self_type_entry {
                    entry.required_imports(db)
                } else {
                    self.self_type
                        .as_ref()
                        .map(|t| import::Info::new_single_name(&t.parent_module(), t.name.clone()))
                }
            }
            Kind::Method => None,
            Kind::Module =>
                if let Some(reexport) = &self.reexported_in {
                    Some(import::Info::new_single_name(reexport, self.name.clone()))
                } else {
                    Some(import::Info::new_qualified(&self.defined_in))
                },
            Kind::Atom => {
                let imported_from = self.reexported_in.as_ref().unwrap_or(&self.defined_in);
                Some(import::Info::new_single_name(imported_from, self.name.clone()))
            }
            Kind::Function | Kind::Local => None,
        }
    }

    /// Returns the code which should be inserted to Searcher input when suggestion is picked,
    /// omitting module name.
    pub fn code_to_insert_skip_module(&self) -> String {
        self.name.clone()
    }

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
    pub fn invocation_info(&self) -> span_tree::generate::context::CalledMethodInfo {
        self.into()
    }

    /// Check if this is a regular method (i.e. non-extension method).
    pub fn is_regular_method(&self) -> bool {
        let is_method = self.kind == Kind::Method;
        let not_extension = self.self_type.contains_if(|this| this.in_module(&self.defined_in))
            || self.self_type.contains(&self.defined_in);
        is_method && not_extension
    }

    /// Get the full qualified name of the entry.
    pub fn qualified_name(&self) -> QualifiedName {
        match self.kind {
            Kind::Method => match &self.self_type {
                Some(t) => chain_iter_and_entry_name(t, self).collect(),
                None => {
                    let msg = format!(
                        "Cannot construct a fully qualified name for the suggestion database \
                        entry {self:?}. Every entry with the 'Method' kind should have a self \
                        type set, but this entry is missing the self type."
                    );
                    error!("{msg}");
                    default()
                }
            },
            Kind::Module => self.defined_in.into_iter().collect(),
            _ => chain_iter_and_entry_name(&self.defined_in, self).collect(),
        }
    }

    /// Get the fully qualified name of the parent module of this entry. Returns [`None`] if
    /// the entry represents a top-level module.
    pub fn parent_module(&self) -> Option<module::QualifiedName> {
        match self.kind {
            Kind::Module => self.defined_in.parent_module(),
            _ => Some(self.defined_in.clone()),
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
    pub fn from_ls_entry(
        mut entry: language_server::types::SuggestionEntry,
    ) -> FallibleResult<Self> {
        use language_server::types::SuggestionEntry::*;
        let (documentation, icon_name) = match &mut entry {
            Atom { documentation, documentation_html, documentation_sections, .. }
            | Method { documentation, documentation_html, documentation_sections, .. }
            | Module { documentation, documentation_html, documentation_sections, .. } => {
                let documentation =
                    Self::make_html_docs(mem::take(documentation), mem::take(documentation_html));
                let icon_name = find_icon_name_in_doc_sections(&*documentation_sections);
                (documentation, icon_name)
            }
            _ => (None, None),
        };
        let reexported_in: Option<module::QualifiedName> = match &mut entry {
            Atom { reexport: Some(reexport), .. }
            | Method { reexport: Some(reexport), .. }
            | Module { reexport: Some(reexport), .. } => Some(mem::take(reexport).try_into()?),
            _ => None,
        };
        let mut this = match entry {
            Atom { name, module, return_type, arguments, .. } =>
                Self::new_atom(module.try_into()?, name, return_type).with_arguments(arguments),
            Method { name, module, self_type, return_type, is_static, arguments, .. } =>
                Self::new_method(
                    module.try_into()?,
                    self_type.try_into()?,
                    name,
                    return_type,
                    is_static,
                )
                .with_arguments(arguments),
            Function { name, module, return_type, scope, .. } =>
                Self::new_function(module.try_into()?, name, return_type, scope.into()),
            Local { name, module, return_type, scope, .. } =>
                Self::new_local(module.try_into()?, name, return_type, scope.into()),
            Module { module, .. } => Self::new_module(module.try_into()?),
        };
        this.documentation_html = documentation;
        this.icon_name = icon_name;
        this.reexported_in = reexported_in;
        Ok(this)
    }

    /// Returns the documentation in html depending on the information received from the Engine.
    ///
    /// Depending on the engine version, we may receive the documentation in HTML format already,
    /// or the raw text which needs to be parsed. This function takes two fields of
    /// [`language_server::types::SuggestionEntry`] and depending on availability, returns the
    /// HTML docs fields, or parsed raw docs field.
    fn make_html_docs(docs: Option<String>, docs_html: Option<String>) -> Option<String> {
        if docs_html.is_some() {
            docs_html
        } else {
            docs.map(|docs| {
                let parser = parser_scala::DocParser::new();
                match parser {
                    Ok(p) => {
                        let output = p.generate_html_doc_pure((*docs).to_string());
                        output.unwrap_or(docs)
                    }
                    Err(_) => docs,
                }
            })
        }
    }

    /// Apply modification to the entry.
    pub fn apply_modifications(
        &mut self,
        modification: SuggestionsDatabaseModification,
    ) -> Vec<failure::Error> {
        let m = modification;
        let module = m.module.map(|f| f.try_map(module::QualifiedName::from_text)).transpose();
        let self_type = m.self_type.map(|f| f.try_map(tp::QualifiedName::from_text)).transpose();
        let reexport = m.reexport.map(|f| f.try_map(module::QualifiedName::from_text)).transpose();
        let other_update_results = [
            Entry::apply_field_update("return_type", &mut self.return_type, m.return_type),
            Entry::apply_opt_field_update(
                "documentation_html",
                &mut self.documentation_html,
                m.documentation_html,
            ),
            module.and_then(|m| Entry::apply_field_update("module", &mut self.defined_in, m)),
            self_type
                .and_then(|s| Entry::apply_opt_field_update("self_type", &mut self.self_type, s)),
            self.apply_scope_update(m.scope),
            reexport.and_then(|r| {
                Entry::apply_opt_field_update("reexport", &mut self.reexported_in, r)
            }),
        ];
        let other_update_results = SmallVec::from_buf(other_update_results).into_iter();
        let other_update_errors = other_update_results.filter_map(|res| res.err());
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
}

impl TryFrom<language_server::types::SuggestionEntry> for Entry {
    type Error = failure::Error;
    fn try_from(entry: language_server::types::SuggestionEntry) -> FallibleResult<Self> {
        Self::from_ls_entry(entry)
    }
}

impl TryFrom<&Entry> for language_server::MethodPointer {
    type Error = failure::Error;
    fn try_from(entry: &Entry) -> FallibleResult<Self> {
        (entry.kind == Kind::Method).ok_or_else(|| NotAMethod(entry.name.clone()))?;
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

impl From<&Entry> for span_tree::generate::context::CalledMethodInfo {
    fn from(entry: &Entry) -> span_tree::generate::context::CalledMethodInfo {
        let parameters = entry.arguments.iter().map(to_span_tree_param).collect();
        span_tree::generate::context::CalledMethodInfo { parameters }
    }
}



// ===============
// === Helpers ===
// ===============


// === SpanTree helpers ===

/// Converts the information about function parameter from suggestion database into the form used
/// by the span tree nodes.
pub fn to_span_tree_param(param_info: &Argument) -> span_tree::ArgumentInfo {
    span_tree::ArgumentInfo {
        // TODO [mwu] Check if database actually do must always have both of these filled.
        name: Some(param_info.name.clone()),
        tp:   Some(param_info.repr_type.clone()),
    }
}


// === Entry helpers ===

fn chain_iter_and_entry_name<'a>(
    iter: impl IntoIterator<Item = &'a str>,
    entry: &'a Entry,
) -> impl Iterator<Item = &'a str> {
    iter.into_iter().chain(iter::once(entry.name.as_str()))
}

fn find_icon_name_in_doc_sections<'a, I>(doc_sections: I) -> Option<IconName>
where I: IntoIterator<Item = &'a language_server::types::DocSection> {
    use language_server::types::DocSection;
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
    use web_sys::self_;

    use enso_text::unit::*;

    #[test]
    fn code_from_entry() {
        fn expect(entry: &Entry, expected_code: &str, expected_imports: &[&module::QualifiedName]) {
            let code_to_insert = entry.code_to_insert(current_module, generate_this);
            assert_eq!(code_to_insert.code, expected_code);
            assert_eq!(code_to_insert.imports.iter().collect_vec().as_slice(), expected_imports);
        }
        let module = module::QualifiedName::from_text("local.Project.Main").unwrap();
        let tp = tp::QualifiedName::from_text("local.Project.Test_Type").unwrap();
        let atom = Entry::new_atom(main_module.clone(), "Atom", tp.clone());
        let return_type = "Standard.Base.Number";
        let scope = Location { line: 3.line(), offset: 0.utf16_code_unit() }..=Location {
            line:   10.line,
            offset: 0.utf16_code_unit(),
        };
        let method =
            Entry::new_method(main_module.clone(), tp.clone(), "method", return_type, false);
        let static_method =
            Entry::new_method(main_module.clone(), tp.clone(), "static_method", return_type, true);
        let module_method = Entry::new_method(
            main_module.clone(),
            main_module.clone().into(),
            "module_method",
            return_type,
            true,
        );
        let function = Entry::new_function(module.clone(), "function", return_type, scope.clone());
        let local = Entry::new_local(module, "local", return_type, scope);

        for generate_this in [true, false] {
            assert_eq!(atom.code_to_insert(generate_this), "Atom");
            assert_eq!(function.code_to_insert(generate_this), "function");
            assert_eq!(local.code_to_insert(generate_this), "local");
        }
        assert_eq!(method.code_to_insert(false), "method");
        assert_eq!(method.code_to_insert(true), "_.method");
        assert_eq!(static_method.code_to_insert(false), "static_method");
        assert_eq!(static_method.code_to_insert(true), "Number.static_method");
        assert_eq!(module_method.code_to_insert(false), "module_method");
        assert_eq!(module_method.code_to_insert(true), "Project.static_method");
    }

    #[test]
    fn method_id_from_entry() {
        let module = module::QualifiedName::from_text("test.Test.Test").unwrap();
        let self_type = tp::QualifiedName::from_text("Standard.Base.Number").unwrap();
        let non_method = Entry::new_function(module.clone(), "function", "Number", default());
        let method =
            Entry::new_method(module.clone(), self_type.clone(), "method", "Number", false);
        let expected = MethodId { module, defined_on_type: self_type, name: "method".to_owned() };
        assert_eq!(non_method.method_id(), None);
        assert_eq!(method.method_id(), Some(expected));
    }

    /// Test the result of the [`Entry::qualified_name`] method when applied to entries with
    /// different values of [`Entry::kind`].
    #[test]
    fn qualified_name_of_entry() {
        fn expect(entry: Entry, qualified_name: &str) {
            let entry_qualified_name = entry.qualified_name();
            let expected_qualified_name = qualified_name.split('.').collect();
            assert_eq!(entry_qualified_name, expected_qualified_name);
        }
<<<<<<< HEAD
        let tpe = language_server::SuggestionEntry::Type {
            name:                   "TextType".to_string(),
            module:                 "TestProject.TestModule".to_string(),
            params:                 vec![],
            parent_type:            Some("Any".to_string()),
            documentation:          None,
            documentation_html:     None,
            documentation_sections: default(),
            external_id:            None,
        };
        expect(tpe, "TestProject.TestModule.TextType");
        let constructor = language_server::SuggestionEntry::Constructor {
            name:                   "TextConstructor".to_string(),
            module:                 "TestProject.TestModule".to_string(),
            arguments:              vec![],
            return_type:            "TestConstructor".to_string(),
            documentation:          None,
            documentation_html:     None,
            documentation_sections: default(),
            external_id:            None,
        };
        expect(constructor, "TestProject.TestModule.TextConstructor");
        let method = language_server::SuggestionEntry::Method {
            name:                   "create_process".to_string(),
            module:                 "Standard.Builtins.Main".to_string(),
            self_type:              "Standard.Builtins.Main.System".to_string(),
            arguments:              vec![],
            return_type:            "Standard.Builtins.Main.System_Process_Result".to_string(),
            documentation:          None,
            documentation_html:     None,
            documentation_sections: default(),
            external_id:            None,
        };

        expect(method, "Standard.Builtins.Main.System.create_process");

        // Module
        let module = Entry::new_module("local.Unnamed_6.Main".try_inro().unwrap());
        expect(module, "local.Unnamed_6.Main");

        // Local
        let defined_in = "local.Unnamed_6.Main".try_inro().unwrap();
        let return_type = "Standard.Base.Data.Vector.Vector".to_owned();
        let local = Entry::new_local(defined_in, "operator1", return_type, default());
        expect(local, "local.Unnamed_6.Main.operator1");

        // Function
        let defined_in = "NewProject.NewModule".try_into().unwrap();
        let return_type = "Standard.Base.Data.Vector.Vector".to_owned();
        let function = Entry::new_function(defined_in, "testFunction1", return_type, default());
        expect(function, "NewProject.NewModule.testFunction1");
    }

    /// Test the results of converting a [`QualifiedName`] to a string using various methods.
    #[test]
    fn qualified_name_to_string() {
        let qualified_name = QualifiedName::from_iter(["Foo", "Bar"]);
        assert_eq!(qualified_name.to_string(), "Foo.Bar".to_string());
        assert_eq!(String::from(qualified_name), "Foo.Bar".to_string());
    }

    /// Test [`find_icon_name_in_doc_sections`] function extracting a name of an icon from the body
    /// of a keyed [`DocSection`] which has its key equal to the `Icon` string.
    #[test]
    fn find_icon_name_in_doc_section_with_icon_key() {
        use language_server::types::DocSection;
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
}
