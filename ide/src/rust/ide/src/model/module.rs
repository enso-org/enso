//! This module contains all structures which describes Module state (code, ast, metadata).

pub mod plain;
pub mod synchronized;

use crate::prelude::*;

use crate::constants::LANGUAGE_FILE_EXTENSION;
use crate::constants::SOURCE_DIRECTORY;
use crate::controller::FilePath;
use crate::double_representation::definition::DefinitionInfo;

use data::text::TextChange;
use data::text::TextLocation;
use enso_protocol::language_server::MethodPointer;
use flo_stream::Subscriber;
use parser::api::SourceFile;
use parser::api::ParsedSourceFile;
use parser::Parser;
use serde::Serialize;
use serde::Deserialize;

pub use double_representation::module::QualifiedName;

// ============
// == Errors ==
// ============

/// Failure for missing node metadata.
#[derive(Debug,Clone,Copy,Fail)]
#[fail(display="Node with ID {} was not found in metadata.", _0)]
pub struct NodeMetadataNotFound(pub ast::Id);

/// Failed attempt to tread a file path as a module path.
#[derive(Clone,Debug,Fail)]
#[fail(display = "The path `{}` is not a valid module path. {}",path,issue)]
pub struct InvalidModulePath {
    /// The path that is not a valid module path.
    path  : FilePath,
    /// The reason why the path is not a valid modile path.
    issue : ModulePathViolation
}

/// Describes possible reasons why a `FilePath` cannot be recognized as a `ModulePath`.
#[allow(missing_docs)]
#[derive(Clone,Copy,Debug,Fail)]
pub enum ModulePathViolation {
    #[fail(display="The module filename should be capitalized.")]
    NonCapitalizedFileName,
    #[fail(display="The path contains an empty segment which is not allowed.")]
    ContainsEmptySegment,
    #[fail(display="The file path does not contain any segments, while it should be non-empty.")]
    ContainsNoSegments,
    #[fail(display="The module file path should start with the sources directory.")]
    NotInSourceDirectory,
    #[fail(display="The module file must have a proper language extension.")]
    WrongFileExtension,
}

/// Happens if an empty segments list is provided as qualified module name.
#[derive(Clone,Copy,Debug,Fail)]
#[fail(display="No qualified name segments were provided.")]
pub struct EmptyQualifiedName;



// ============
// === Path ===
// ============

/// Path identifying module's file in the Language Server.
///
/// The `file_path` contains at least two segments:
/// * the first one is a source directory in the project (see `SOURCE_DIRECTORY`);
/// * the last one is a source file with the module's contents.
#[derive(Clone,CloneRef,Debug,Eq,Hash,PartialEq,Shrinkwrap)]
pub struct Path {
    file_path:Rc<FilePath>,
}

impl Path {
    /// Create a path from the file path. Returns Err if given path is not a valid module file.
    pub fn from_file_path(file_path:FilePath) -> Result<Self,InvalidModulePath> {
        use ModulePathViolation::*;
        let error             = |issue| {
            let path = file_path.clone();
            move || InvalidModulePath {path,issue}
        };
        let correct_extension = file_path.extension() == Some(LANGUAGE_FILE_EXTENSION);
        correct_extension.ok_or_else(error(WrongFileExtension))?;
        let file_name       = file_path.file_name().ok_or_else(error(ContainsNoSegments))?;
        let name_first_char = file_name.chars().next().ok_or_else(error(ContainsEmptySegment))?;
        name_first_char.is_uppercase().ok_or_else(error(NonCapitalizedFileName))?;
        let is_in_src = file_path.segments.first().contains_if(|name| *name == SOURCE_DIRECTORY);
        is_in_src.ok_or_else(error(NotInSourceDirectory))?;
        let file_path = Rc::new(file_path);
        Ok(Path {file_path})
    }

    /// Creates a module path from the module's qualified name segments.
    /// Name segments should only cover the module names, excluding the project name.
    ///
    /// E.g. `["Main"]` -> `//root_id/src/Main.enso`
    pub fn from_name_segments
    (root_id:Uuid, name_segments:impl IntoIterator<Item:AsRef<str>>) -> FallibleResult<Path> {
        let mut segments : Vec<String> = vec![SOURCE_DIRECTORY.into()];
        segments.extend(name_segments.into_iter().map(|segment| segment.as_ref().to_string()));
        let module_file = segments.last_mut().ok_or(EmptyQualifiedName)?;
        module_file.push('.');
        module_file.push_str(LANGUAGE_FILE_EXTENSION);
        let file_path = Rc::new(FilePath {root_id,segments});
        Ok(Path {file_path})
    }

    /// Get the file path.
    pub fn file_path(&self) -> &FilePath {
        &*self.file_path
    }

    /// Gives the file name for the given module name.
    ///
    /// E.g. "Main" -> "Main.enso"
    pub fn name_to_file_name(name:impl Str) -> String {
        format!("{}.{}",name.as_ref(),LANGUAGE_FILE_EXTENSION)
    }

    /// Get the module name from path.
    ///
    /// The module name is a filename without extension.
    pub fn module_name(&self) -> &str {
        // The file stem existence should be checked during construction.
        self.file_path.file_stem().unwrap()
    }

    /// Create a module path consisting of a single segment, based on a given module name.
    /// The `default` is used for a root id.
    pub fn from_mock_module_name(name:impl Str) -> Self {
        let file_name   = Self::name_to_file_name(name);
        let src_dir     = SOURCE_DIRECTORY.to_string();
        let file_path   = FilePath::new(default(),&[src_dir,file_name]);
        Self::from_file_path(file_path).unwrap()
    }

    /// Obtain a pointer to a method of the module (i.e. extending the module's atom).
    ///
    /// Note that this cannot be used for a method extending other atom than this module.
    pub fn method_pointer(&self, method_name:impl Str) -> MethodPointer {
        MethodPointer {
            defined_on_type : self.module_name().into(),
            name            : method_name.into(),
            file            : self.file_path.deref().clone(),
        }
    }

    /// Obtain a module's full qualified name from the path and the project name.
    ///
    /// ```
    /// use ide::prelude::*;
    /// use ide::model::module::QualifiedName;
    /// use ide::model::module::Path;
    ///
    /// let path = Path::from_name_segments(default(),&["Main"]).unwrap();
    /// assert_eq!(path.to_string(),"//00000000-0000-0000-0000-000000000000/src/Main.enso");
    /// let name = path.qualified_module_name("Project");
    /// assert_eq!(name.to_string(),"Project.Main");
    /// ```
    pub fn qualified_module_name(&self, project_name:impl Str) -> QualifiedName {
        let non_src_directories = &self.file_path.segments[1..self.file_path.segments.len()-1];
        let non_src_directories = non_src_directories.iter().map(|dirname| dirname.as_str());
        let module_name         = std::iter::once(self.module_name());
        let module_segments     = non_src_directories.chain(module_name);
        // The module path during creation should be checked for at least one module segment.
        QualifiedName::from_segments(project_name,module_segments).unwrap()
    }
}

impl PartialEq<FilePath> for Path {
    fn eq(&self, other:&FilePath) -> bool {
        self.file_path.deref().eq(other)
    }
}

impl TryFrom<FilePath> for Path {
    type Error = InvalidModulePath;

    fn try_from(value:FilePath) -> Result<Self, Self::Error> {
        Path::from_file_path(value)
    }
}

impl Display for Path {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(&self.file_path, f)
    }
}

impl TryFrom<MethodPointer> for Path {
    type Error = InvalidModulePath;

    fn try_from(value:MethodPointer) -> Result<Self, Self::Error> {
        value.file.try_into()
    }
}


// ====================
// === Notification ===
// ====================

/// Notification about change in module content.
#[derive(Clone,Debug,Eq,PartialEq)]
pub enum Notification {
    /// The whole content is invalidated.
    Invalidate,
    /// The code has been edited. That involves also a change in module's id_map.
    CodeChanged {
        /// The code change description.
        change:TextChange,
        /// Information about line:col position of replaced fragment.
        replaced_location:Range<TextLocation>
    },
    /// The metadata (e.g. some node's position) has been changed.
    MetadataChanged,
}



// ==============
// == Metadata ==
// ==============

/// Mapping between ID and metadata.
#[derive(Debug,Clone,Deserialize,Serialize)]
pub struct Metadata {
    /// Metadata used within ide.
    #[serde(default="default")]
    pub ide : IdeMetadata,
    #[serde(flatten)]
    /// Metadata of other users of ParsedSourceFile<Metadata> API.
    /// Ide should not modify this part of metadata.
    rest : serde_json::Value,
}

impl parser::api::Metadata for Metadata {}

impl Default for Metadata {
    fn default() -> Self {
        Metadata {
            ide : default(),
            // We cannot default to unit, because it cannot be flattened, so calling
            // `Metadata::default().serialize()` will result in an error.
            rest : serde_json::Value::Object(default()),
        }
    }
}

/// Metadata that belongs to ide.
#[derive(Debug,Clone,Default,Deserialize,Serialize)]
pub struct IdeMetadata {
    /// Metadata that belongs to nodes.
    node : HashMap<ast::Id,NodeMetadata>
}

/// Metadata of specific node.
#[derive(Debug,Clone,Copy,Default,Serialize,Deserialize,Shrinkwrap)]
pub struct NodeMetadata {
    /// Position in x,y coordinates.
    pub position: Option<Position>
}

/// Used for storing node position.
#[derive(Copy,Clone,Debug,PartialEq,Serialize,Deserialize)]
pub struct Position {
    /// Vector storing coordinates of the visual position.
    pub vector:Vector2<f32>
}

impl Position {
    /// Creates a new position with given coordinates.
    pub fn new(x:f32, y:f32) -> Position {
        let vector = Vector2::new(x,y);
        Position {vector}
    }
}



// ==============
// === Module ===
// ==============

/// A type describing content of the module: the ast and metadata.
pub type Content = ParsedSourceFile<Metadata>;

/// Module model API.
pub trait API:Debug {
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
    fn find_definition
    (&self,id:&double_representation::graph::Id) -> FallibleResult<DefinitionInfo>;

    /// Returns metadata for given node, if present.
    fn node_metadata(&self, id:ast::Id) -> FallibleResult<NodeMetadata>;


// === Setters ===

    /// Update whole content of the module.
    fn update_whole(&self, content:Content);

    /// Update ast in module controller.
    fn update_ast(&self, ast:ast::known::Module);

    /// Updates AST after code change.
    ///
    /// May return Error when new code causes parsing errors, or when parsed code does not produce
    /// Module ast.
    fn apply_code_change
    (&self, change:TextChange, parser:&Parser, new_id_map:ast::IdMap) -> FallibleResult<()>;

    /// Sets metadata for given node.
    fn set_node_metadata(&self, id:ast::Id, data:NodeMetadata);

    /// Removes metadata of given node and returns them.
    fn remove_node_metadata(&self, id:ast::Id) -> FallibleResult<NodeMetadata>;

    /// Modify metadata of given node.
    ///
    /// If ID doesn't have metadata, empty (default) metadata is inserted. Inside callback you
    /// should use only the data passed as argument; don't use functions of this controller for
    /// getting and setting metadata for the same node.
    fn with_node_metadata(&self, id:ast::Id, fun:Box<dyn FnOnce(&mut NodeMetadata) + '_>);
}

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

    pub fn expect_code(module:&dyn API, expected_code:impl AsRef<str>) {
        let code = module.ast().repr();
        assert_eq!(code,expected_code.as_ref())
    }

    /// Data from which module model is usually created in test scenarios.
    #[derive(Clone,Debug)]
    pub struct MockData {
        pub path     : Path,
        pub code     : String,
        pub id_map   : ast::IdMap,
        pub metadata : Metadata,
    }

    impl Default for MockData {
        fn default() -> Self {
            Self {
                path     : crate::test::mock::data::module_path(),
                code     : crate::test::mock::data::CODE.to_owned(),
                id_map   : default(),
                metadata : default(),
            }
        }
    }

    impl MockData {
        pub fn plain(&self, parser:&Parser) -> Module {
            let ast    = parser.parse_module(self.code.clone(),self.id_map.clone()).unwrap();
            let module = Plain::new(self.path.clone(),ast,self.metadata.clone());
            Rc::new(module)
        }
    }

    pub fn plain_from_code(code:impl Into<String>) -> Module {
        MockData {
            code : code.into(),
            ..default()
        }.plain(&parser::Parser::new_or_panic())
    }

    #[test]
    fn module_path_conversion() {
        let path = FilePath::new(default(), &["src","Main.enso"]);
        assert!(Path::from_file_path(path).is_ok());

        let path = FilePath::new(default(), &["src","Main.txt"]);
        assert!(Path::from_file_path(path).is_err());

        let path = FilePath::new(default(), &["src","main.txt"]);
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
        let project_name = "P";
        let root_id      = default();
        let file_path    = FilePath::new(root_id, &["src", "Foo", "Bar.enso"]);
        let module_path  = Path::from_file_path(file_path).unwrap();
        let qualified    = module_path.qualified_module_name(project_name);
        assert_eq!(*qualified, "P.Foo.Bar");
    }
}
