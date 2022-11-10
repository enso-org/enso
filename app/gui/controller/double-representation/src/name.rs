use crate::prelude::*;

use crate::module;
use ast::constants::PROJECTS_MAIN_MODULE;
use ast::opr::predefined::ACCESS;
use enso_prelude::serde_reexports::Deserialize;
use enso_prelude::serde_reexports::Serialize;

pub mod project;

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

pub type NamePath = Vec<ImString>;
pub type NamePathRef<'a> = &'a [ImString];

#[derive(Clone, Debug, Default, Deserialize, Eq, Hash, Ord, PartialEq, PartialOrd, Serialize)]
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

pub type QualifiedName = QualifiedNameTemplate<NamePath>;
pub type QualifiedNameRef<'a> = QualifiedNameTemplate<NamePathRef<'a>>;

impl_clone_ref_as_clone!(['a] QualifiedNameRef<'a>);

impl<Segments> QualifiedNameTemplate<Segments> {
    pub fn new(project: project::QualifiedName, path: Segments) -> Self {
        Self { project, path }
    }
}

impl QualifiedName {
    /// Create a qualified name for the project's main module.
    pub fn new_main(project: project::QualifiedName) -> Self {
        Self::new(project, default())
    }

    pub fn new_module(project: project::QualifiedName, id: module::Id) -> Self {
        Self::new(project, id.into())
    }

    pub fn new_child(mut self, child: impl Into<ImString>) -> Self {
        self.push_segment(child);
        self
    }

    /// Constructs a qualified name from its text representation.
    ///
    /// Fails, if the text is not a valid module's qualified name.
    pub fn from_text(text: impl AsRef<str>) -> FallibleResult<Self> {
        let text = text.as_ref();
        Self::from_all_segments(text.split(ACCESS))
    }

    /// Build a module's full qualified name from its name segments and the project name.
    ///
    /// ```
    /// # use double_representation::module::QualifiedName;
    ///
    /// let name = QualifiedName::from_all_segments(&["Project", "Main"]).unwrap();
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

impl<Segments: AsRef<[ImString]>> QualifiedNameTemplate<Segments> {
    pub fn project(&self) -> &project::QualifiedName {
        &self.project
    }

    pub fn path(&self) -> &[ImString] {
        self.path.as_ref()
    }

    /// Get the entity's name. In case of Main module it's `Main`, not the project name.
    pub fn name(&self) -> &str {
        self.path.as_ref().last().map_or(PROJECTS_MAIN_MODULE, ImString::as_str)
    }

    /// Get the entity's name. In case of Main module it's the project name, not `Main`.
    pub fn alias_name(&self) -> &ImString {
        let module_name = (!self.is_main_module()).and_option_from(|| self.path.as_ref().last());
        module_name.unwrap_or(&self.project.project)
    }

    /// Check if the name refers to some project's Main module.
    pub fn is_main_module(&self) -> bool {
        self.path.as_ref().is_empty()
    }

    /// The iterator over name's segments.
    pub fn segments(&self) -> impl Iterator<Item = &ImString> {
        self.project.segments().chain(self.path.as_ref())
    }

    pub fn module_id(&self) -> module::Id {
        let module_path = self.path.as_ref();
        let parent_modules = &module_path[0..module_path.len().saturating_sub(1)];
        module::Id { name: self.name().into(), parent_modules: parent_modules.to_vec() }
    }

    /// Check if the name refers to entity defined/reexported in library's main module.
    pub fn is_top_element(&self) -> bool {
        self.path.as_ref().is_empty()
    }

    pub fn sub_path(
        &self,
        range: impl SliceIndex<[ImString], Output = [ImString]>,
    ) -> QualifiedNameRef {
        QualifiedNameRef { project: self.project.clone_ref(), path: &self.path.as_ref()[range] }
    }

    pub fn parent(&self) -> Option<QualifiedNameRef> {
        let shorter_len = self.path.as_ref().len().checked_sub(1)?;
        Some(self.sub_path(0..shorter_len))
    }

    /// Returns an iterator over all parent modules. The `self` is not included.
    pub fn parents(&self) -> impl Iterator<Item = QualifiedNameRef> {
        let mut path_upper_bounds = (0..self.path.as_ref().len()).rev();
        iter::from_fn(move || {
            let upper_bound = path_upper_bounds.next()?;
            Some(self.sub_path(0..upper_bound))
        })
    }

    pub fn as_ref(&self) -> QualifiedNameRef {
        QualifiedNameRef { project: self.project.clone_ref(), path: self.path.as_ref() }
    }

    pub fn to_owned(&self) -> QualifiedName {
        QualifiedName { project: self.project.clone_ref(), path: self.path.as_ref().into() }
    }
}

impl QualifiedName {
    /// Add a segment to this qualified name.
    ///
    /// ```
    /// # use double_representation::identifier::ReferentName;
    /// # use double_representation::module::QualifiedName;
    ///
    /// let mut name = QualifiedName::from_text("ns.Proj.Foo").unwrap();
    /// let bar_segment = ReferentName::new("Bar").unwrap();
    ///
    /// name.push_segment(bar_segment);
    /// assert_eq!(name.to_string(), "ns.Proj.Foo.Bar");
    /// ```
    pub fn push_segment(&mut self, name: impl Into<ImString>) {
        self.path.push(name.into());
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

#[cfg(test)]
impl From<&str> for QualifiedName {
    fn from(text: &str) -> Self {
        Self::from_text(text).unwrap()
    }
}

#[cfg(test)]
impl From<String> for QualifiedName {
    fn from(text: String) -> Self {
        Self::from_text(text).unwrap()
    }
}


impl From<project::QualifiedName> for QualifiedName {
    fn from(project: project::QualifiedName) -> Self {
        Self::new_main(project)
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

impl<Segments: AsRef<[ImString]>> Display for QualifiedNameTemplate<Segments> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.segments().format(ACCESS))
    }
}

impl PartialEq<project::QualifiedName> for QualifiedName {
    fn eq(&self, other: &project::QualifiedName) -> bool {
        self.as_ref() == *other
    }
}

impl<'a> PartialEq<project::QualifiedName> for QualifiedNameRef<'a> {
    fn eq(&self, other: &project::QualifiedName) -> bool {
        self.project == *other && self.path.is_empty()
    }
}
