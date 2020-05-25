//! This module contains all structures which describes Module state (code, ast, metadata).
use crate::prelude::*;

use crate::notification;
use crate::double_representation::definition::DefinitionInfo;

use data::text::TextChange;
use data::text::TextLocation;
use flo_stream::MessagePublisher;
use flo_stream::Subscriber;
use parser::api::SourceFile;
use parser::api::ParsedSourceFile;
use parser::Parser;
use serde::Serialize;
use serde::Deserialize;



// ============
// == Errors ==
// ============

/// Failure for missing node metadata.
#[derive(Debug,Clone,Copy,Fail)]
#[fail(display="Node with ID {} was not found in metadata.", _0)]
pub struct NodeMetadataNotFound(pub ast::Id);



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
    notifications : RefCell<notification::Publisher<Notification>>,
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
        self.notifications.borrow_mut().subscribe()
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
        double_representation::definition::traverse_for_definition(&ast,id)
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
        let replaced_indices  = change.replaced.start.value..change.replaced.end.value;
        let replaced_location = TextLocation::convert_range(&code,&change.replaced);

        code.replace_range(replaced_indices,&change.inserted);
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
        let notify  = self.notifications.borrow_mut().publish(notification);
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
}
