//! This module contains all structures which describes Module state (code, ast, metadata).

pub mod plain;
pub mod synchronized;

pub use double_representation::module::Id;
pub use double_representation::module::QualifiedName;
pub use double_representation::tp::QualifiedName as TypeQualifiedName;

use crate::prelude::*;

use crate::controller::FilePath;

use ast::constants::LANGUAGE_FILE_EXTENSION;
use ast::constants::SOURCE_DIRECTORY;
use double_representation::definition::DefinitionInfo;
use double_representation::identifier::ReferentName;
use double_representation::project;
use engine_protocol::language_server::MethodPointer;
use flo_stream::Subscriber;
use parser::api::ParsedSourceFile;
use parser::api::SourceFile;
use parser::Parser;
use serde::Deserialize;
use serde::Serialize;



// ============
// == Errors ==
// ============

/// Failure for missing node metadata.
#[derive(Debug, Clone, Copy, Fail)]
#[fail(display = "Node with ID {} was not found in metadata.", _0)]
pub struct NodeMetadataNotFound(pub ast::Id);

/// Failed attempt to tread a file path as a module path.
#[derive(Clone, Debug, Fail)]
#[fail(display = "The path `{}` is not a valid module path. {}", path, issue)]
pub struct InvalidModulePath {
    /// The path that is not a valid module path.
    path:  FilePath,
    /// The reason why the path is not a valid module path.
    issue: ModulePathViolation,
}

/// Describes possible reasons why a `FilePath` cannot be recognized as a `ModulePath`.
#[allow(missing_docs)]
#[derive(Clone, Copy, Debug, Fail)]
pub enum ModulePathViolation {
    #[fail(display = "The module file path needs to contain at least two segments.")]
    NotEnoughSegments,
    #[fail(display = "The module file path should start with the sources directory.")]
    NotInSourceDirectory,
    #[fail(display = "The module file must have a proper language extension.")]
    WrongFileExtension,
}



// ===============
// === Aliases ===
// ===============

/// A specialization of text change used in module's text changes across controllers.
pub type TextChange = enso_text::Change<enso_text::unit::Bytes, String>;



// ============
// === Path ===
// ============

/// Path identifying module's file in the Language Server.
///
/// The `file_path` contains at least two segments:
/// * the first one is a source directory in the project (see `SOURCE_DIRECTORY`);
/// * the last one is a source file with the module's contents;
/// * all the ones between (if present) are names of the parent modules.
#[derive(Clone, CloneRef, Debug, Eq, Hash, PartialEq, Shrinkwrap)]
pub struct Path {
    file_path: Rc<FilePath>,
}

impl Path {
    /// Get the file name of the module with given name.
    pub fn module_filename(name: &ReferentName) -> String {
        iformat!("{name}.{LANGUAGE_FILE_EXTENSION}")
    }

    /// Build module's path in a filesystem under given root ID.
    pub fn from_id(root_id: Uuid, id: &Id) -> Path {
        // We prepend source directory and replace trailing segment with filename.
        let src_dir = std::iter::once(SOURCE_DIRECTORY.to_owned());
        let dirs = id.parent_segments().iter().map(ToString::to_string);
        let filename = std::iter::once(Self::module_filename(&id.name()));
        let segments = src_dir.chain(dirs).chain(filename).collect();
        let path = FilePath { root_id, segments };
        Path { file_path: Rc::new(path) }
    }

    /// Get path to the module with given qualified name under given root ID.
    pub fn from_name(root_id: Uuid, name: &QualifiedName) -> Path {
        Self::from_id(root_id, name.id())
    }

    /// Get a path of the module that defines given method.
    pub fn from_method(root_id: Uuid, method: &MethodPointer) -> FallibleResult<Self> {
        let name = QualifiedName::try_from(method)?;
        Ok(Self::from_name(root_id, &name))
    }

    /// Check if the given file path is a valid module path.
    pub fn validate_path(file_path: &FilePath) -> FallibleResult {
        use ModulePathViolation::*;
        let error = |issue| {
            let path = file_path.clone();
            move || InvalidModulePath { path, issue }
        };

        if let [ref src_dir, ref dirs @ .., _] = *file_path.segments.as_slice() {
            (src_dir == SOURCE_DIRECTORY).ok_or_else(error(NotInSourceDirectory))?;
            for dir in dirs {
                ReferentName::validate(dir)?;
            }
            let correct_extension = file_path.extension() == Some(LANGUAGE_FILE_EXTENSION);
            correct_extension.ok_or_else(error(WrongFileExtension))?;
            ReferentName::validate(file_path.file_stem().unwrap_or_default())?;
            Ok(())
        } else {
            Err(error(NotEnoughSegments)().into())
        }
    }

    /// Create a path from the file path. Returns Err if given path is not a valid module file.
    pub fn from_file_path(file_path: FilePath) -> FallibleResult<Self> {
        Self::validate_path(&file_path)?;
        let file_path = Rc::new(file_path);
        Ok(Path { file_path })
    }

    /// Creates a module path from the module's qualified name segments.
    /// Name segments should only cover the module names, excluding the project name.
    ///
    /// E.g. `["Main"]` -> `//root_id/src/Main.enso`
    pub fn from_name_segments(
        root_id: Uuid,
        name_segments: impl IntoIterator<Item: AsRef<str>>,
    ) -> FallibleResult<Path> {
        let segment_results = name_segments.into_iter().map(|s| ReferentName::new(s.as_ref()));
        let segments = segment_results.collect::<Result<Vec<_>, _>>()?;
        let id = Id::new(segments);
        Ok(Self::from_id(root_id, &id))
    }

    /// Get the module's identifier.
    pub fn id(&self) -> Id {
        if let [ref _src, ref dirs @ .., _] = *self.file_path.segments.as_slice() {
            // Path must designate a valid module and must be able to designate any valid module.
            // Therefore, unwraps in this method are safe.
            let parent_segments = dirs.iter().map(ReferentName::new).map(Result::unwrap);
            let final_segment = std::iter::once(self.module_name());
            let segments = parent_segments.chain(final_segment);
            Id::new(segments)
        } else {
            // Class invariant guarantees at least two segments in path.
            panic!("Unreachable")
        }
    }

    /// Get the file path.
    pub fn file_path(&self) -> &FilePath {
        &*self.file_path
    }

    /// Gives the file name for the given module name.
    ///
    /// E.g. "Main" -> "Main.enso"
    pub fn name_to_file_name(name: impl Str) -> String {
        format!("{}.{}", name.as_ref(), LANGUAGE_FILE_EXTENSION)
    }

    /// Get the module name from path.
    ///
    /// The module name is a filename without extension.
    pub fn module_name(&self) -> ReferentName {
        // The file stem existence should be checked during construction.
        // The path must also designate a file that is a valid module name.
        ReferentName::new(self.file_path.file_stem().unwrap()).unwrap()
    }

    /// Create a module path consisting of a single segment, based on a given module name.
    /// The `default` is used for a root id.
    pub fn from_mock_module_name(name: impl Str) -> Self {
        let file_name = Self::name_to_file_name(name);
        let src_dir = SOURCE_DIRECTORY.to_string();
        let file_path = FilePath::new(default(), &[src_dir, file_name]);
        Self::from_file_path(file_path).unwrap()
    }

    /// Obtain a pointer to a method of the module (i.e. extending the module's atom).
    ///
    /// Note that this cannot be used for a method extending other atom than this module.
    pub fn method_pointer(
        &self,
        project_name: project::QualifiedName,
        method_name: impl Str,
    ) -> MethodPointer {
        let module = String::from(self.qualified_module_name(project_name));
        let defined_on_type = module.clone();
        let name = method_name.into();
        MethodPointer { module, defined_on_type, name }
    }

    /// Obtain a module's full qualified name from the path and the project name.
    ///
    /// ```
    /// use enso_gui::model::module::Path;
    /// use enso_gui::model::module::QualifiedName;
    /// use enso_gui::prelude::*;
    ///
    /// let path = Path::from_name_segments(default(), &["Main"]).unwrap();
    /// assert_eq!(path.to_string(), "//00000000-0000-0000-0000-000000000000/src/Main.enso");
    /// let name = path.qualified_module_name("local.Project".try_into().unwrap());
    /// assert_eq!(name.to_string(), "local.Project.Main");
    /// ```
    pub fn qualified_module_name(&self, project_name: project::QualifiedName) -> QualifiedName {
        let non_src_directories = &self.file_path.segments[1..self.file_path.segments.len() - 1];
        let non_src_directories = non_src_directories.iter().map(|dirname| dirname.as_str());
        let module_name = self.module_name();
        let module_name = std::iter::once(module_name.as_ref());
        let module_segments = non_src_directories.chain(module_name);
        // The module path during creation should be checked for at least one module segment.
        QualifiedName::from_segments(project_name, module_segments).unwrap()
    }
}

impl PartialEq<FilePath> for Path {
    fn eq(&self, other: &FilePath) -> bool {
        self.file_path.deref().eq(other)
    }
}

impl TryFrom<FilePath> for Path {
    type Error = failure::Error;

    fn try_from(value: FilePath) -> Result<Self, Self::Error> {
        Path::from_file_path(value)
    }
}

impl Display for Path {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(&self.file_path, f)
    }
}

impl From<Path> for Id {
    fn from(path: Path) -> Self {
        path.id()
    }
}



// ====================
// === Notification ===
// ====================

/// Notification about change in module content.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum NotificationKind {
    /// The whole content is invalidated.
    Invalidate,
    /// The code has been edited. That involves also a change in module's id_map.
    CodeChanged {
        /// The code change description.
        change:            TextChange,
        /// Information about line:col position of replaced fragment.
        replaced_location: enso_text::Range<enso_text::Location>,
    },
    /// The metadata (e.g. some node's position) has been changed.
    MetadataChanged,
}

/// Notification about change in module content.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Notification {
    /// The expected state of the source file at the point when this notification is emit.
    ///
    /// This information is necessary for notifying LS about edits -- in case of failure the
    /// synchronization handler must be able to perform full invalidation. The model state cannot
    /// be used for this as LS communication is asynchronous and model state can already be
    /// modified further.
    pub new_file: SourceFile,
    /// Describes the notified event.
    pub kind:     NotificationKind,
}



// ==============
// == Metadata ==
// ==============

/// Mapping between ID and metadata.
#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub struct Metadata {
    /// Metadata used within ide.
    #[serde(default, deserialize_with = "enso_prelude::deserialize_or_default")]
    pub ide: IdeMetadata,
    #[serde(flatten)]
    /// Metadata of other users of ParsedSourceFile<Metadata> API.
    /// Ide should not modify this part of metadata.
    rest:    serde_json::Value,
}

impl parser::api::Metadata for Metadata {}

impl Default for Metadata {
    fn default() -> Self {
        Metadata {
            ide:  default(),
            // We cannot default to unit, because it cannot be flattened, so calling
            // `Metadata::default().serialize()` will result in an error.
            rest: serde_json::Value::Object(default()),
        }
    }
}

/// Project-level metadata. It is stored as part of the project's main module's metadata.
#[derive(Clone, Debug, Default, Deserialize, PartialEq, Serialize)]
pub struct ProjectMetadata {
    /// The execution context of the displayed graph editor.
    #[serde(default, deserialize_with = "enso_prelude::deserialize_or_default")]
    pub call_stack: Vec<model::execution_context::LocalCall>,
}

/// Metadata that belongs to ide.
#[derive(Clone, Debug, Default, Deserialize, PartialEq, Serialize)]
pub struct IdeMetadata {
    /// Metadata that belongs to nodes.
    #[serde(deserialize_with = "enso_prelude::deserialize_or_default")]
    node:    HashMap<ast::Id, NodeMetadata>,
    /// The project metadata. This is stored only in the main module's metadata.
    #[serde(default, deserialize_with = "enso_prelude::deserialize_or_default")]
    project: Option<ProjectMetadata>,
}

/// Metadata of specific node.
#[derive(Clone, Debug, Default, Deserialize, PartialEq, Serialize)]
pub struct NodeMetadata {
    /// Position in x,y coordinates.
    #[serde(default, deserialize_with = "enso_prelude::deserialize_or_default")]
    pub position:        Option<Position>,
    /// A method which user intends this node to be, e.g. by picking specific suggestion in
    /// Searcher Panel.
    ///
    /// The methods may be defined for different types, so the name alone don't specify them.
    #[serde(default, deserialize_with = "enso_prelude::deserialize_or_default")]
    pub intended_method: Option<MethodId>,
    /// Information about uploading file.
    ///
    /// Designed to be present in nodes created by dragging and dropping files in IDE. Contains
    /// information about file and upload progress.
    #[serde(default, deserialize_with = "enso_prelude::deserialize_or_default")]
    pub uploading_file:  Option<UploadingFile>,
    /// Was node selected in the view.
    #[serde(default)]
    pub selected:        bool,
    /// Information about enabled visualization. Exact format is defined by the integration layer.
    #[serde(default)]
    pub visualization:   serde_json::Value,
}

/// Used for storing node position.
#[derive(Copy, Clone, Debug, Default, PartialEq, Serialize, Deserialize)]
pub struct Position {
    /// Vector storing coordinates of the visual position.
    pub vector: Vector2<f32>,
}

impl Position {
    /// Creates a new position with given coordinates.
    pub fn new(x: f32, y: f32) -> Position {
        let vector = Vector2::new(x, y);
        Position { vector }
    }

    /// Calculate the mean position from the given sequence.
    ///
    /// If the input sequence is empty, returns the default value.
    pub fn mean(iter: impl Iterator<Item = Position>) -> Position {
        let mut count = 0;
        let mut accum_pos = Position::default();
        for position in iter {
            count += 1;
            accum_pos += position;
        }
        if count > 0 {
            accum_pos / (count as f32)
        } else {
            default()
        }
    }

    /// Order positions by `y` coordinate. `NaN`s are considered equal.
    // We silence the clippy warning, as we specifically want to accept parameters by reference.
    // The function is meant to be used in iterator API and it passes arguments by value.
    #[allow(clippy::trivially_copy_pass_by_ref)]
    pub fn ord_by_y(pos1: &Position, pos2: &Position) -> std::cmp::Ordering {
        pos1.vector.y.partial_cmp(&pos2.vector.y).unwrap_or(std::cmp::Ordering::Equal)
    }
}

impl Add for Position {
    type Output = Position;
    fn add(self, rhs: Self) -> Self::Output {
        Position { vector: self.vector + rhs.vector }
    }
}

impl std::ops::AddAssign for Position {
    fn add_assign(&mut self, rhs: Self) {
        self.vector += rhs.vector
    }
}

impl<T> Div<T> for Position
where
    f32: Div<T, Output = f32>,
    T: Copy,
{
    type Output = Position;
    fn div(self, rhs: T) -> Self::Output {
        Position::new(self.vector.x / rhs, self.vector.y / rhs)
    }
}

impl From<Vector2<f32>> for Position {
    fn from(value: Vector2) -> Self {
        Position::new(value.x, value.y)
    }
}

/// A structure identifying a method.
///
/// It is very similar to MethodPointer from language_server API, however it may point to the method
/// outside the currently opened project.
#[derive(Clone, Debug, Deserialize, Eq, Hash, PartialEq, Serialize)]
#[allow(missing_docs)]
pub struct MethodId {
    pub module:          QualifiedName,
    pub defined_on_type: TypeQualifiedName,
    pub name:            String,
}

/// Uploading File Information
///
/// May be stored in node metadata, if the node's expression is reading content of file still
/// uploaded to the project directory.
#[allow(missing_docs)]
#[derive(Clone, Debug, Deserialize, Eq, Hash, PartialEq, Serialize)]
pub struct UploadingFile {
    /// The name of file dropped in IDE.
    pub name:           String,
    /// The file's destination name. May differ from original name due to conflict with files
    /// already present on the remote.
    pub remote_name:    Option<String>,
    pub size:           u64,
    /// The number of bytes already uploaded. It _can_ exceed the `size` value, because the file
    /// may change during upload.
    pub bytes_uploaded: u64,
    pub error:          Option<String>,
}


// ==============
// === Module ===
// ==============

/// A type describing content of the module: the ast and metadata.
pub type Content = ParsedSourceFile<Metadata>;

/// Module model API.
pub trait API: Debug + model::undo_redo::Aware {
    /// Subscribe for notifications about text representation changes.
    fn subscribe(&self) -> Subscriber<Notification>;


    // === Getters ===
    /// Get the module path.
    fn path(&self) -> &Path;

    /// Get module sources as a string, which contains both code and metadata.
    fn serialized_content(&self) -> FallibleResult<SourceFile>;

    /// Get module's ast.
    fn ast(&self) -> ast::known::Module;

    /// Obtains definition information for given graph id.
    fn find_definition(
        &self,
        id: &double_representation::graph::Id,
    ) -> FallibleResult<DefinitionInfo>;

    /// Returns metadata for given node, if present.
    fn node_metadata(&self, id: ast::Id) -> FallibleResult<NodeMetadata>;


    // === Setters ===

    /// Update whole content of the module.
    fn update_whole(&self, content: Content) -> FallibleResult;

    /// Update ast in module controller.
    fn update_ast(&self, ast: ast::known::Module) -> FallibleResult;

    /// Updates AST after code change.
    ///
    /// May return Error when new code causes parsing errors, or when parsed code does not produce
    /// Module ast.
    fn apply_code_change(
        &self,
        change: TextChange,
        parser: &Parser,
        new_id_map: ast::IdMap,
    ) -> FallibleResult;

    /// Sets metadata for given node.
    fn set_node_metadata(&self, id: ast::Id, data: NodeMetadata) -> FallibleResult;

    /// Removes metadata of given node and returns them.
    fn remove_node_metadata(&self, id: ast::Id) -> FallibleResult<NodeMetadata>;

    /// Modify metadata of given node.
    ///
    /// If ID doesn't have metadata, empty (default) metadata is inserted. Inside callback you
    /// should use only the data passed as argument; don't use functions of this controller for
    /// getting and setting metadata for the same node.
    fn with_node_metadata(
        &self,
        id: ast::Id,
        fun: Box<dyn FnOnce(&mut NodeMetadata) + '_>,
    ) -> FallibleResult;

    /// This method exists as a monomorphication for [`with_project_metadata`]. Users are encouraged
    /// to use it rather then this method.
    ///
    /// Access project's metadata with a given function. Fails, if the project's metadata are not
    /// set in this module.
    fn boxed_with_project_metadata(&self, fun: Box<dyn FnOnce(&ProjectMetadata) + '_>);

    /// This method exists as a monomorphication for [`update_project_metadata`]. Users are
    /// encouraged to use it rather then this method.
    ///
    /// Borrow mutably the project's metadata and update it with a given function.
    fn boxed_update_project_metadata(
        &self,
        fun: Box<dyn FnOnce(&mut ProjectMetadata) + '_>,
    ) -> FallibleResult;


    // === Utils ===

    /// Get the module's identifier.
    fn id(&self) -> Id {
        self.path().id()
    }

    /// Get a definition ID that points to a method matching given pointer.
    ///
    /// The module is assumed to be in the file identified by the `method.file` (for the purpose of
    /// desugaring implicit extensions methods for modules).
    fn lookup_method(
        &self,
        project_name: project::QualifiedName,
        method: &MethodPointer,
    ) -> FallibleResult<double_representation::definition::Id> {
        let name = self.path().qualified_module_name(project_name);
        let ast = self.ast();
        double_representation::module::lookup_method(&name, &ast, method)
    }

    /// Get the double representation information about module.
    fn info(&self) -> double_representation::module::Info {
        double_representation::module::Info::from(self.ast())
    }

    /// Returns self as any. Used for casting down the [`Module`] object.
    fn as_any(&self) -> &dyn Any;
}

/// Trait for methods that cannot be defined in `API` because it is a trait object.
pub trait APIExt: API {
    /// Access project's metadata with a given function.
    ///
    /// Fails, if the project's metadata are not set in this module.
    fn with_project_metadata<R>(&self, fun: impl FnOnce(&ProjectMetadata) -> R) -> R {
        let mut ret = None;
        self.boxed_with_project_metadata(Box::new(|metadata| {
            ret = Some(fun(metadata));
        }));
        // The `with_project_metadata_internal` always calls the callback once, so it must fill
        // `ret` with necessary data.
        ret.unwrap()
    }

    /// Borrow mutably the project's metadata and update it with a given function.
    fn update_project_metadata(&self, fun: impl FnOnce(&mut ProjectMetadata)) -> FallibleResult {
        self.boxed_update_project_metadata(Box::new(fun))
    }
}

impl<T: API + ?Sized> APIExt for T {}

/// The general, shared Module Model handle.
pub type Module = Rc<dyn API>;
/// Module Model which does not do anything besides storing data.
pub type Plain = plain::Module;
/// Module Model which synchronizes all changes with Language Server.
pub type Synchronized = synchronized::Module;



// ============
// === Test ===
// ============

#[cfg(test)]
pub mod test {
    use super::*;

    use std::str::FromStr;

    pub fn expect_code(module: &dyn API, expected_code: impl AsRef<str>) {
        let code = module.ast().repr();
        assert_eq!(code, expected_code.as_ref())
    }

    /// Data from which module model is usually created in test scenarios.
    #[derive(Clone, Debug)]
    pub struct MockData {
        pub path:     Path,
        pub code:     String,
        pub id_map:   ast::IdMap,
        pub metadata: Metadata,
    }

    impl Default for MockData {
        fn default() -> Self {
            Self {
                path:     crate::test::mock::data::module_path(),
                code:     crate::test::mock::data::CODE.to_owned(),
                id_map:   default(),
                metadata: default(),
            }
        }
    }

    impl MockData {
        pub fn plain(
            &self,
            parser: &Parser,
            repository: Rc<model::undo_redo::Repository>,
        ) -> Module {
            let ast = parser.parse_module(self.code.clone(), self.id_map.clone()).unwrap();
            let logger = Logger::new("MockModule");
            let module =
                Plain::new(logger, self.path.clone(), ast, self.metadata.clone(), repository);
            Rc::new(module)
        }
    }

    pub fn plain_from_code(code: impl Into<String>) -> Module {
        let urm = default();
        MockData { code: code.into(), ..default() }.plain(&parser::Parser::new_or_panic(), urm)
    }

    #[test]
    fn module_path_conversion() {
        let path = FilePath::new(default(), &["src", "Main.enso"]);
        assert!(Path::from_file_path(path).is_ok());

        let path = FilePath::new(default(), &["src", "Main.txt"]);
        assert!(Path::from_file_path(path).is_err());

        let path = FilePath::new(default(), &["src", "main.txt"]);
        assert!(Path::from_file_path(path).is_err());
    }

    #[test]
    fn module_path_validation() {
        assert!(Path::from_file_path(FilePath::new(default(), &["src", "Main.enso"])).is_ok());

        assert!(Path::from_file_path(FilePath::new(default(), &["surce", "Main.enso"])).is_err());
        assert!(Path::from_file_path(FilePath::new(default(), &["src", "Main"])).is_err());
        assert!(Path::from_file_path(FilePath::new(default(), &["src", ""])).is_err());
        assert!(Path::from_file_path(FilePath::new(default(), &["src", "main.enso"])).is_err());
    }

    #[test]
    fn module_qualified_name() {
        let namespace = "n";
        let project_name = "P";
        let project_name = project::QualifiedName::from_segments(namespace, project_name).unwrap();
        let root_id = default();
        let file_path = FilePath::new(root_id, &["src", "Foo", "Bar.enso"]);
        let module_path = Path::from_file_path(file_path).unwrap();
        let qualified = module_path.qualified_module_name(project_name);
        assert_eq!(qualified.to_string(), "n.P.Foo.Bar");
    }

    #[wasm_bindgen_test]
    fn outdated_metadata_parses() {
        // Metadata here will fail to serialize because `File` is not a valid qualified name.
        // Expected behavior is that invalid metadata parts will be filled with defaults.
        let code = r#"main = 5


#### METADATA ####
[]
{"ide":{"node":{"bd891b65-4c2f-4c05-bc3b-6077b4417cc1":{"position":{"vector":[-75.5,52]},"intended_method":{"module":"Base.System.File","defined_on_type":"File","name":"read"}}}}}"#;
        let result = Parser::new_or_panic().parse_with_metadata::<Metadata>(code.into());
        let file = result.unwrap();
        assert_eq!(file.ast.repr(), "main = 5");
        assert_eq!(file.metadata.ide.node.len(), 1);
        let id = ast::Id::from_str("bd891b65-4c2f-4c05-bc3b-6077b4417cc1").unwrap();
        let node = file.metadata.ide.node.get(&id).unwrap();
        assert_eq!(node.position, Some(Position::new(-75.5, 52.0)));
        assert_eq!(node.intended_method, None);
        assert_eq!(file.metadata.rest, serde_json::Value::Object(default()));
    }
}
