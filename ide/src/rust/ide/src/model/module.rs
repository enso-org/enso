//! This module contains all structures which describes Module state (code, ast, metadata).
use crate::prelude::*;

use crate::constants::LANGUAGE_FILE_EXTENSION;
use crate::constants::SOURCE_DIRECTORY;
use crate::controller::FilePath;
use crate::double_representation::definition::DefinitionInfo;
use crate::notification;

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

/// A structure describing the module.
///
/// It implements internal mutability pattern, so the state may be shared between different
/// controllers. Each change in module will emit notification for each module representation
/// (text and graph).
#[derive(Debug)]
pub struct Module {
    content       : RefCell<Content>,
    notifications : notification::Publisher<Notification>,
}

impl Default for Module {
    fn default() -> Self {
        let ast = ast::known::Module::new(ast::Module{lines:default()},None);
        Self::new(ast,default())
    }
}

impl Module {
    /// Create state with given content.
    pub fn new(ast:ast::known::Module, metadata:Metadata) -> Self {
        Module {
            content       : RefCell::new(ParsedSourceFile{ast,metadata}),
            notifications : default(),
        }
    }

    /// Subscribe for notifications about text representation changes.
    pub fn subscribe(&self) -> Subscriber<Notification> {
        self.notifications.subscribe()
    }

    /// Create module state from given code, id_map and metadata.
    #[cfg(test)]
    pub fn from_code_or_panic<S:ToString>
    (code:S, id_map:ast::IdMap, metadata:Metadata) -> Self {
        let parser = parser::Parser::new_or_panic();
        let ast    = parser.parse(code.to_string(),id_map).unwrap().try_into().unwrap();
        Self::new(ast,metadata)
    }
}


// === Access to Module Content ===

impl Module {
    /// Get module sources as a string, which contains both code and metadata.
    pub fn serialized_content(&self) -> FallibleResult<SourceFile> {
        self.content.borrow().serialize().map_err(|e| e.into())
    }

    /// Get module's ast.
    pub fn ast(&self) -> ast::known::Module {
        self.content.borrow().ast.clone_ref()
    }

    /// Obtains definition information for given graph id.
    pub fn find_definition
    (&self,id:&double_representation::graph::Id) -> FallibleResult<DefinitionInfo> {
        let ast = self.content.borrow().ast.clone_ref();
        double_representation::module::get_definition(&ast, id)
    }

    /// Returns metadata for given node, if present.
    pub fn node_metadata(&self, id:ast::Id) -> FallibleResult<NodeMetadata> {
        let data = self.content.borrow().metadata.ide.node.get(&id).cloned();
        data.ok_or_else(|| NodeMetadataNotFound(id).into())
    }
}


// === Setters ===

impl Module {

    /// Update whole content of the module.
    pub fn update_whole(&self, content:Content) {
        *self.content.borrow_mut() = content;
        self.notify(Notification::Invalidate);
    }

    /// Update ast in module controller.
    pub fn update_ast(&self, ast:ast::known::Module) {
        self.content.borrow_mut().ast  = ast;
        self.notify(Notification::Invalidate);
    }

    /// Updates AST after code change.
    ///
    /// May return Error when new code causes parsing errors, or when parsed code does not produce
    /// Module ast.
    pub fn apply_code_change
    (&self, change:TextChange, parser:&Parser, new_id_map:ast::IdMap) -> FallibleResult<()> {
        let mut code          = self.ast().repr();
        let replaced_location = TextLocation::convert_range(&code,&change.replaced);
        change.apply(&mut code);
        let new_ast = parser.parse(code,new_id_map)?.try_into()?;
        self.content.borrow_mut().ast = new_ast;
        self.notify(Notification::CodeChanged {change,replaced_location});
        Ok(())
    }

    /// Sets metadata for given node.
    pub fn set_node_metadata(&self, id:ast::Id, data:NodeMetadata) {
        self.content.borrow_mut().metadata.ide.node.insert(id, data);
        self.notify(Notification::MetadataChanged);
    }

    /// Removes metadata of given node and returns them.
    pub fn remove_node_metadata(&self, id:ast::Id) -> FallibleResult<NodeMetadata> {
        let lookup = self.content.borrow_mut().metadata.ide.node.remove(&id);
        let data   = lookup.ok_or_else(|| NodeMetadataNotFound(id))?;
        self.notify(Notification::MetadataChanged);
        Ok(data)
    }

    /// Modify metadata of given node.
    ///
    /// If ID doesn't have metadata, empty (default) metadata is inserted. Inside callback you
    /// should use only the data passed as argument; don't use functions of this controller for
    /// getting and setting metadata for the same node.
    pub fn with_node_metadata(&self, id:ast::Id, fun:impl FnOnce(&mut NodeMetadata)) {
        let lookup   = self.content.borrow_mut().metadata.ide.node.remove(&id);
        let mut data = lookup.unwrap_or_default();
        fun(&mut data);
        self.content.borrow_mut().metadata.ide.node.insert(id, data);
        self.notify(Notification::MetadataChanged);
    }

    fn notify(&self, notification:Notification) {
        let notify  = self.notifications.publish(notification);
        executor::global::spawn(notify);
    }
}



// ============
// === Test ===
// ============

#[cfg(test)]
mod test {
    use super::*;

    use crate::executor::test_utils::TestWithLocalPoolExecutor;

    use data::text;
    use uuid::Uuid;
    use wasm_bindgen_test::wasm_bindgen_test;

    #[wasm_bindgen_test]
    fn applying_code_change() {
        let mut test = TestWithLocalPoolExecutor::set_up();
        test.run_task(async {
            let module = Module::from_code_or_panic("2 + 2",default(),default());
            let change = TextChange {
                replaced: text::Index::new(2)..text::Index::new(5),
                inserted: "- abc".to_string(),
            };
            module.apply_code_change(change,&Parser::new_or_panic(),default()).unwrap();
            assert_eq!("2 - abc",module.ast().repr());
        });
    }

    #[wasm_bindgen_test]
    fn notifying() {
        let mut test = TestWithLocalPoolExecutor::set_up();
        test.run_task(async {
            let module                 = Module::default();
            let mut subscription       = module.subscribe();

            // Ast update
            let new_line       = Ast::infix_var("a","+","b");
            let new_module_ast = Ast::one_line_module(new_line);
            let new_module_ast = ast::known::Module::try_new(new_module_ast).unwrap();
            module.update_ast(new_module_ast.clone_ref());
            assert_eq!(Some(Notification::Invalidate), subscription.next().await);

            // Code change
            let change = TextChange {
                replaced: text::Index::new(0)..text::Index::new(1),
                inserted: "foo".to_string(),
            };
            module.apply_code_change(change.clone(),&Parser::new_or_panic(),default()).unwrap();
            let replaced_location = TextLocation{line:0, column:0}..TextLocation{line:0, column:1};
            let notification      = Notification::CodeChanged {change,replaced_location};
            assert_eq!(Some(notification), subscription.next().await);

            // Metadata update
            let id            = Uuid::new_v4();
            let node_metadata = NodeMetadata {position:Some(Position::new(1.0, 2.0))};
            module.set_node_metadata(id.clone(),node_metadata.clone());
            assert_eq!(Some(Notification::MetadataChanged), subscription.next().await);
            module.remove_node_metadata(id.clone()).unwrap();
            assert_eq!(Some(Notification::MetadataChanged), subscription.next().await);
            module.with_node_metadata(id.clone(),|md| *md = node_metadata.clone());
            assert_eq!(Some(Notification::MetadataChanged), subscription.next().await);

            // Whole update
            let mut metadata = Metadata::default();
            metadata.ide.node.insert(id,node_metadata);
            module.update_whole(ParsedSourceFile{ast:new_module_ast, metadata});
            assert_eq!(Some(Notification::Invalidate), subscription.next().await);

            // No more notifications emitted
            drop(module);
            assert_eq!(None, subscription.next().await);
        });
    }

    #[test]
    fn handling_metadata() {
        let mut test = TestWithLocalPoolExecutor::set_up();
        test.run_task(async {
            let module = Module::default();

            let id         = Uuid::new_v4();
            let initial_md = module.node_metadata(id.clone());
            assert!(initial_md.is_err());

            let md_to_set = NodeMetadata {position:Some(Position::new(1.0, 2.0))};
            module.set_node_metadata(id.clone(),md_to_set.clone());
            assert_eq!(md_to_set.position, module.node_metadata(id.clone()).unwrap().position);

            let new_pos = Position::new(4.0, 5.0);
            module.with_node_metadata(id.clone(), |md| {
                assert_eq!(md_to_set.position, md.position);
                md.position = Some(new_pos);
            });
            assert_eq!(Some(new_pos), module.node_metadata(id).unwrap().position);
        });
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
