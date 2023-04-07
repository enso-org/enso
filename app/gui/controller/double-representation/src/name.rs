//! The structures representing the name paths which may appear in Enso code.

use crate::prelude::*;

use crate::module;

use ast::constants::PROJECTS_MAIN_MODULE;
use ast::opr::predefined::ACCESS;
use enso_prelude::serde_reexports::Deserialize;
use enso_prelude::serde_reexports::Serialize;
use std::cmp::Ordering;


// ==============
// === Export ===
// ==============

pub mod project;



// ==============
// === Errors ===
// ==============

#[allow(missing_docs)]
#[derive(Copy, Clone, Debug, Fail)]
pub enum InvalidQualifiedName {
    #[fail(display = "The qualified name is empty.")]
    EmptyName,
    #[fail(display = "No namespace in project qualified name.")]
    NoNamespace,
    #[fail(display = "Invalid namespace in project qualified name.")]
    InvalidNamespace,
    #[fail(display = "Too many segments in project qualified name.")]
    TooManySegments,
}

#[allow(missing_docs)]
#[derive(Copy, Clone, Debug, Fail)]
#[fail(display = "No qualified name found in AST")]
pub struct QualifiedNameNotFoundInAst;



// ================
// === NamePath ===
// ================

/// Representation of name path: the list of segments which is separated by dots in the code.
pub type NamePath = Vec<ImString>;

/// Reference to [`NamePath`] or its fragment.
pub type NamePathRef<'a> = &'a [ImString];



// =====================
// === QualifiedName ===
// =====================

/// A QualifiedName template without specified type of segment's list container.
///
/// Usually you should use one of its specialization: the owned [`QualifiedName`] or borrowed
/// [`QualifiedNameRef`].
#[derive(Clone, Debug, Default, Deserialize, Hash, Serialize)]
#[serde(into = "String")]
#[serde(try_from = "String")]
#[serde(bound(
    serialize = "Self: Into<String>, Segments: Clone",
    deserialize = "Self: TryFrom<String, Error: Display>"
))]
pub struct QualifiedNameTemplate<Segments> {
    project: project::QualifiedName,
    path:    Segments,
}

/// The Fully Qualified Name of language's entity (type, module, method, etc.).
///
/// It's represented in the code as list of identifiers separated by dots, where two first segments
/// are project namespace and name.
///
/// This structure removes project's main module name from the path upon construction to avoid
/// having different [`QualifiedName`]s representing same logical path (what helps when we want to,
/// for example, look up things by their qualified name.
pub type QualifiedName = QualifiedNameTemplate<NamePath>;

/// A reference to [`QualifiedName`] or ots fragment.
pub type QualifiedNameRef<'a> = QualifiedNameTemplate<NamePathRef<'a>>;

impl_clone_ref_as_clone!(['a] QualifiedNameRef<'a>);


// === Construction ===

impl<Segments> QualifiedNameTemplate<Segments> {
    fn new(project: project::QualifiedName, path: Segments) -> Self {
        Self { project, path }
    }
}

impl QualifiedName {
    /// Create a qualified name for the project's main module.
    pub fn new_main(project: project::QualifiedName) -> Self {
        Self::new(project, default())
    }

    /// Create a qualified name for module in `project` identified by `id`.
    pub fn new_module(project: project::QualifiedName, id: module::Id) -> Self {
        let without_main = id.into_iter().skip_while(|s| s == PROJECTS_MAIN_MODULE);
        Self::new(project, without_main.collect_vec())
    }

    /// Create a qualified name with new segment pushed at end of the path.
    pub fn new_child(mut self, child: impl Into<ImString>) -> Self {
        self.push_segment(child);
        self
    }

    /// Constructs a qualified name from its text representation.
    ///
    /// Note, that there is no guarantee that `QualifiedName::from_text(s).to_string() = s`, as the
    /// `Main` segment is removed upon constructing [`QualifiedName`].
    ///
    /// Fails, if the text is not a valid name.
    ///
    /// # Example
    ///
    /// ```rust
    /// use double_representation::name::QualifiedName;
    /// let name = QualifiedName::from_text("ns.Project.Module.Type").unwrap();
    /// assert_eq!(name.project().namespace, "ns");
    /// assert_eq!(name.project().project, "Project");
    /// assert_eq!(name.path()[0], "Module");
    /// assert_eq!(name.path()[1], "Type");
    ///
    /// // The "Main" module segment is removed.
    /// let main_module_name = QualifiedName::from_text("ns.Project.Main.Type").unwrap();
    /// assert_eq!(main_module_name.to_string(), "ns.Project.Type");
    /// ```
    pub fn from_text(text: impl AsRef<str>) -> FallibleResult<Self> {
        let text = text.as_ref();
        Self::from_all_segments(text.split(ACCESS))
    }

    /// Build a module's full qualified name from its name segments and the project name.
    ///
    /// ```
    /// # use double_representation::name::QualifiedName;
    ///
    /// let name = QualifiedName::from_all_segments(["Project", "Main"]).unwrap();
    /// assert_eq!(name.to_string(), "Project.Main");
    /// ```
    pub fn from_all_segments<Seg>(segments: impl IntoIterator<Item = Seg>) -> FallibleResult<Self>
    where for<'s> Seg: Into<ImString> + PartialEq<&'s str> {
        let mut iter = segments.into_iter().map(|name| name.into());
        let project_name = match (iter.next(), iter.next()) {
            (Some(ns), Some(name)) => project::QualifiedName::new(ns, name),
            _ => return Err(InvalidQualifiedName::NoNamespace.into()),
        };
        let without_main = iter.skip_while(|s| *s == PROJECTS_MAIN_MODULE);
        Ok(Self::new(project_name, without_main.collect()))
    }
}


// === Methods Shared By QualifiedName and QualifiedNameRef ===

impl<Segments: AsRef<[ImString]>> QualifiedNameTemplate<Segments> {
    /// The project name segments.
    pub fn project(&self) -> &project::QualifiedName {
        &self.project
    }

    /// The path part of the qualified name - everything what goes after the project name.
    pub fn path(&self) -> &[ImString] {
        self.path.as_ref()
    }

    /// Get the entity's name. In case of Main module it's `Main`, not the project name.
    pub fn name(&self) -> &str {
        self.path.as_ref().last().map_or(PROJECTS_MAIN_MODULE, ImString::as_str)
    }

    /// Get the entity's name as visible in the code. In case of Main module it's the project name,
    /// not `Main`.
    pub fn alias_name(&self) -> &ImString {
        let module_name = (!self.is_main_module()).and_option_from(|| self.path.as_ref().last());
        module_name.unwrap_or(&self.project.project)
    }

    /// Check if the name refers to some project's Main module.
    pub fn is_main_module(&self) -> bool {
        self.path.as_ref().is_empty()
    }

    /// The iterator over name's segments (including project namespace and name).
    pub fn segments(&self) -> impl Iterator<Item = &ImString> {
        self.project.segments().chain(self.path.as_ref())
    }

    /// The iterator over name's segments (including project namespace and name) with added
    /// [`PROJECTS_MAIN_MODULE`] segment in case of the main module.
    pub fn segments_with_main_segment(&self) -> impl Iterator<Item = &str> {
        let main_segment = self.is_main_module().then_some(PROJECTS_MAIN_MODULE);
        self.segments().map(|s| s.as_str()).chain(main_segment)
    }

    /// Return the module identifier pointed by this qualified name.
    pub fn module_id(&self) -> module::Id {
        let module_path = self.path.as_ref();
        let parent_modules = &module_path[0..module_path.len().saturating_sub(1)];
        module::Id { name: self.name().into(), parent_modules: parent_modules.to_vec() }
    }

    /// Check if the name refers to entity defined/reexported in library's main module.
    pub fn is_top_element(&self) -> bool {
        self.path.as_ref().len() == 1
    }

    /// Return the qualified name referring to same project and some fragment of the [`path`] part.
    pub fn sub_path(
        &self,
        range: impl SliceIndex<[ImString], Output = [ImString]>,
    ) -> QualifiedNameRef {
        QualifiedNameRef { project: self.project.clone_ref(), path: &self.path.as_ref()[range] }
    }

    /// Split the qualified into two parts: the qualified name of a nth parent module and the
    /// remaining access chain of requested length. Returns `None` if the requested access chain
    /// length is too long to split off.
    pub fn split_chain(&self, access_chain_length: usize) -> Option<(QualifiedNameRef, String)> {
        let path = self.path.as_ref();
        if access_chain_length >= path.len() {
            return None;
        }

        let (path, chain) = path.split_at(path.len() - access_chain_length);
        let parent_name = QualifiedNameRef { project: self.project.clone_ref(), path };
        let chain = chain.iter().map(|s| s.as_str()).join(ACCESS);
        Some((parent_name, chain))
    }

    /// Return the [`QualifiedNameRef`] referring to the this name's parent.
    ///
    /// ```rust
    /// use double_representation::name::QualifiedName;
    /// let name = QualifiedName::from_text("ns.Project.Module.Type").unwrap();
    /// let parent = QualifiedName::from_text("ns.Project.Module").unwrap();
    /// assert_eq!(name.parent(), Some(parent.as_ref()));
    /// ```
    pub fn parent(&self) -> Option<QualifiedNameRef> {
        let shorter_len = self.path.as_ref().len().checked_sub(1)?;
        Some(self.sub_path(0..shorter_len))
    }

    /// Returns an iterator over all parent entities. The `self` is not included.
    ///
    /// ```rust
    /// use double_representation::name::QualifiedName;
    /// let name = QualifiedName::from_text("ns.Project.Module.Type").unwrap();
    /// let parents: Vec<String> = name.parents().map(|qn| qn.to_string()).collect();
    /// assert_eq!(parents, vec!["ns.Project.Module", "ns.Project"]);
    /// ```
    pub fn parents(&self) -> impl Iterator<Item = QualifiedNameRef> {
        let mut path_upper_bounds = (0..self.path.as_ref().len()).rev();
        iter::from_fn(move || {
            let upper_bound = path_upper_bounds.next()?;
            Some(self.sub_path(0..upper_bound))
        })
    }

    /// Convert to [`QualifiedNameRef`].
    pub fn as_ref(&self) -> QualifiedNameRef {
        QualifiedNameRef { project: self.project.clone_ref(), path: self.path.as_ref() }
    }

    /// Create a new owned version of this qualified name.
    pub fn to_owned(&self) -> QualifiedName {
        QualifiedName { project: self.project.clone_ref(), path: self.path.as_ref().into() }
    }

    /// Convert qualified name to [`String`] adding the [`PROJECTS_MAIN_SEGMENT`] at the end in case
    /// of main module.
    ///
    /// ```rust
    /// use double_representation::name::QualifiedName;
    /// let name = QualifiedName::from_text("ns.Project").unwrap();
    /// assert_eq!(name.to_string_with_main_segment(), "ns.Project.Main");
    /// ```
    pub fn to_string_with_main_segment(&self) -> String {
        self.segments_with_main_segment().join(ACCESS)
    }
}


// === Owned QualifiedName only Methods ===

impl QualifiedName {
    /// Add a segment to this qualified name.
    ///
    /// Because the [`QualifiedName`] always omit the "Main" module name in its path, this function
    /// may result in leaving exactly the same name as before (see an example).
    ///
    /// ```rust
    /// use double_representation::name::QualifiedName;
    /// let mut name = QualifiedName::from_text("ns.Proj.Foo").unwrap();
    /// name.push_segment("Bar");
    /// assert_eq!(name.to_string(), "ns.Proj.Foo.Bar");
    ///
    /// let mut name = QualifiedName::from_text("ns.Proj").unwrap();
    /// name.push_segment("Main");
    /// assert_eq!(name.to_string(), "ns.Proj");
    /// ```
    pub fn push_segment(&mut self, name: impl Into<ImString>) {
        let name = name.into();
        if name != PROJECTS_MAIN_MODULE || !self.path.is_empty() {
            self.path.push(name);
        }
    }

    /// Remove a segment to this qualified name.
    ///
    /// ```rust
    /// use double_representation::name::QualifiedName;
    /// use enso_prelude::ImString;
    ///
    /// let mut name = QualifiedName::from_text("ns.Proj.Foo").unwrap();
    /// assert_eq!(name.pop_segment(), Some(ImString::new("Foo")));
    /// assert_eq!(name.pop_segment(), None);
    /// ```
    pub fn pop_segment(&mut self) -> Option<ImString> {
        self.path.pop()
    }
}


// === Conversion from AST ===

impl TryFrom<&Ast> for QualifiedName {
    type Error = failure::Error;

    fn try_from(ast: &Ast) -> Result<Self, Self::Error> {
        let segments = ast::opr::Chain::try_new(ast)
            .ok_or(QualifiedNameNotFoundInAst)?
            .as_qualified_name_segments()
            .ok_or(QualifiedNameNotFoundInAst)?;
        Self::from_all_segments(segments)
    }
}


// === Conversions From and Into String ===

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

impl TryFrom<&String> for QualifiedName {
    type Error = failure::Error;

    fn try_from(text: &String) -> Result<Self, Self::Error> {
        Self::from_text(text)
    }
}


impl From<QualifiedName> for String {
    fn from(name: QualifiedName) -> Self {
        String::from(&name)
    }
}

impl From<&QualifiedName> for String {
    fn from(name: &QualifiedName) -> Self {
        name.to_string()
    }
}

impl<Segments: AsRef<[ImString]>> Display for QualifiedNameTemplate<Segments> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.segments().format(ACCESS))
    }
}


// === Conversion Between Name Representations

impl From<project::QualifiedName> for QualifiedName {
    fn from(project: project::QualifiedName) -> Self {
        Self::new_main(project)
    }
}

impl From<QualifiedName> for NamePath {
    fn from(qualified: QualifiedName) -> Self {
        qualified.into_iter().collect()
    }
}

impl<'a> From<&'a QualifiedName> for NamePath {
    fn from(qualified: &'a QualifiedName) -> Self {
        qualified.segments().cloned().collect()
    }
}

impl<'a, 'b> From<&'a QualifiedNameRef<'b>> for NamePath {
    fn from(qualified: &'a QualifiedNameRef<'b>) -> Self {
        qualified.segments().cloned().collect()
    }
}

impl<'a> From<&'a QualifiedName> for QualifiedNameRef<'a> {
    fn from(qualified: &'a QualifiedName) -> Self {
        qualified.as_ref()
    }
}


// === Conversion Into Iterator ===

impl<'a, 'b> IntoIterator for &'a QualifiedNameRef<'b> {
    type Item = &'a ImString;
    type IntoIter = impl Iterator<Item = &'a ImString>;
    fn into_iter(self) -> Self::IntoIter {
        self.segments()
    }
}

impl<'a> IntoIterator for &'a QualifiedName {
    type Item = &'a ImString;
    type IntoIter = impl Iterator<Item = &'a ImString>;
    fn into_iter(self) -> Self::IntoIter {
        self.segments()
    }
}

impl IntoIterator for QualifiedName {
    type Item = ImString;
    type IntoIter = impl Iterator<Item = ImString>;
    fn into_iter(self) -> Self::IntoIter {
        iter::once(self.project.namespace).chain(iter::once(self.project.project)).chain(self.path)
    }
}


// === Comparing Various Name Representations ===

impl<S1: AsRef<[ImString]>, S2: AsRef<[ImString]>> PartialEq<QualifiedNameTemplate<S1>>
    for QualifiedNameTemplate<S2>
{
    fn eq(&self, other: &QualifiedNameTemplate<S1>) -> bool {
        self.project == other.project && self.path.as_ref() == other.path.as_ref()
    }
}

impl<Segments: AsRef<[ImString]>> Eq for QualifiedNameTemplate<Segments> {}

impl<Segments: AsRef<[ImString]>> PartialEq<project::QualifiedName>
    for QualifiedNameTemplate<Segments>
{
    fn eq(&self, other: &project::QualifiedName) -> bool {
        self.project == *other && self.path.as_ref().is_empty()
    }
}

impl<Segments: AsRef<[ImString]>> PartialEq<NamePath> for QualifiedNameTemplate<Segments> {
    fn eq(&self, other: &NamePath) -> bool {
        self.segments().eq(other.iter())
    }
}

impl<Segments: AsRef<[ImString]>> PartialEq<QualifiedNameTemplate<Segments>> for NamePath {
    fn eq(&self, other: &QualifiedNameTemplate<Segments>) -> bool {
        other == self
    }
}

impl<'a, Segments: AsRef<[ImString]>> PartialEq<NamePathRef<'a>>
    for QualifiedNameTemplate<Segments>
{
    fn eq(&self, other: &NamePathRef<'a>) -> bool {
        self.segments().eq(other.iter())
    }
}

impl<Segments: AsRef<[ImString]>> PartialOrd for QualifiedNameTemplate<Segments> {
    fn partial_cmp(&self, other: &QualifiedNameTemplate<Segments>) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl<Segments: AsRef<[ImString]>> Ord for QualifiedNameTemplate<Segments> {
    fn cmp(&self, other: &QualifiedNameTemplate<Segments>) -> Ordering {
        self.project.cmp(&other.project).then(self.path.as_ref().cmp(other.path.as_ref()))
    }
}
