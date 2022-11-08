use crate::prelude::*;

use crate::module;
use ast::constants::PROJECTS_MAIN_MODULE;
use ast::opr::predefined::ACCESS;
use enso_prelude::serde_reexports::Deserialize;
use enso_prelude::serde_reexports::Serialize;

pub mod project;

#[allow(missing_docs)]
#[derive(Clone, Debug, Fail)]
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


#[derive(Clone, Debug, Default, Deserialize, Eq, Ord, PartialEq, PartialOrd, Serialize)]
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

pub type QualifiedName = QualifiedNameTemplate<Vec<ImString>>;
pub type QualifiedNameRef<'a> = QualifiedNameTemplate<&'a [ImString]>;

impl<Segments> QualifiedNameTemplate<Segments> {
    pub fn new(project: project::QualifiedName, path: Segments) -> Self {
        Self { project, path }
    }

    /// Get the name of project owning this entity.
    pub fn project_name(&self) -> &str {
        self.project.project.as_str()
    }
}

impl<Segments: AsRef<[ImString]>> QualifiedNameTemplate<Segments> {
    /// Get the entity's name. In case of Main module it's the `Main`, not the project name.
    pub fn name(&self) -> &str {
        self.path.as_ref().last().map_or(PROJECTS_MAIN_MODULE, ImString::as_str)
    }

    /// The iterator over name's segments.
    pub fn segments(&self) -> impl Iterator<Item = &ImString> {
        self.project.segments().chain(self.path.as_ref())
    }

    pub fn module_id(&self) -> &module::Id {
        let module_path = self.path.as_ref();
        let parent_modules = &module_path[0..module_path.len().saturating_sub(1)];
        module::Id {
            name:           self.name().into(),
            parent_modules: parent_modules.iter().cloned().collect(),
        }
    }

    /// Check if the name refers to some project's Main module.
    pub fn is_main_module(&self) -> bool {
        match self.path.as_ref() {
            [] => true,
            [single_segment] if *single_segment == PROJECTS_MAIN_MODULE => true,
            _ => false,
        }
    }

    /// Check if the name refers to entity defined/reexported in library's main module.
    pub fn is_top_element(&self) -> bool {
        !self.is_main_module() && self.path.as_ref().len() <= 1
    }

    pub fn parent(&self) -> Option<QualifiedNameRef> {
        let segments = self.path.as_ref();
        let shorter_len = segments.len().checked_sub(1)?;
        Some(QualifiedNameRef {
            project: self.project.clone_ref(),
            path:    &segments[0..shorter_len],
        })
    }

    /// Returns an iterator over all parent modules. The `self` is not included.
    pub fn parents(&self) -> impl Iterator<Item = QualifiedNameRef> {
        let mut path_upper_bounds = (0..self.path.as_ref().len()).rev();
        iter::from_fn(move || {
            let upper_bound = path_upper_bounds.next()?;
            Some(QualifiedNameRef {
                project: self.project.clone_ref(),
                path:    &self.path.as_ref()[0..upper_bound],
            })
        })
    }

    pub fn as_ref(&self) -> QualifiedNameRef {
        QualifiedNameRef { project: self.project.clone_ref(), path: self.path.as_ref() }
    }
}

impl QualifiedName {
    /// Create a qualified name for the project's main module.
    pub fn new_main(project: project::QualifiedName) -> Self {
        Self::new(project, default())
    }

    /// Constructs a qualified name from its text representation.
    ///
    /// Fails, if the text is not a valid module's qualified name.
    pub fn from_text(text: impl AsRef<str>) -> FallibleResult<Self> {
        let text = text.as_ref();
        let segments = text.split(ACCESS).collect_vec();
        match segments.as_slice() {
            [namespace_name, project_name, id_segments @ ..] => {
                let project = project::QualifiedName::new(*namespace_name, *project_name);
                let path = id_segments.iter().map(|&s| s.into()).collect();
                Ok(Self::new(project, path))
            }
            [] => Err(InvalidQualifiedName::EmptyName.into()),
            [_] => Err(InvalidQualifiedName::NoNamespace.into()),
        }
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
        segments: impl IntoIterator<Item: Into<ImString>>,
    ) -> FallibleResult<QualifiedName> {
        let mut iter = segments.into_iter().map(|name| name.into());
        let namespace = iter.next();
        let project_name = iter.next();
        let typed_project_name = match (namespace, project_name) {
            (Some(ns), Some(name)) => Ok(project::QualifiedName::new(ns, name)),
            _ => FallibleResult::Err(InvalidQualifiedName::NoNamespace.into()),
        };
        Ok(Self::new(typed_project_name?, iter.collect()))
    }

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
        if self.path.first().contains(&&PROJECTS_MAIN_MODULE) {
            self.path.pop_front();
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
