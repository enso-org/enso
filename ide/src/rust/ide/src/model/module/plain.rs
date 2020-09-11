//! The plain Module Model.

use crate::prelude::*;

use parser::api::{ParsedSourceFile, SourceFile};
use crate::model::module::{Metadata, NodeMetadata, NodeMetadataNotFound, Path};
use crate::model::module::Notification;
use crate::notification;
use flo_stream::Subscriber;
use crate::double_representation::definition::DefinitionInfo;
use crate::model::module::Content;
use data::text::{TextChange, TextLocation};
use parser::Parser;

/// A structure describing the module.
///
/// It implements internal mutability pattern, so the state may be shared between different
/// controllers. Each change in module will emit notification for each module representation
/// (text and graph).
#[derive(Debug)]
pub struct Module {
    path          : Path,
    content       : RefCell<Content>,
    notifications : notification::Publisher<Notification>,
}

impl Module {
    /// Create state with given content.
    pub fn new(path:Path, ast:ast::known::Module, metadata:Metadata) -> Self {
        Module {
            path,
            content       : RefCell::new(ParsedSourceFile{ast,metadata}),
            notifications : default(),
        }
    }
}

impl model::module::API for Module {
    fn subscribe(&self) -> Subscriber<Notification> {
        self.notifications.subscribe()
    }

    fn path(&self) -> &Path {
        &self.path
    }

    fn serialized_content(&self) -> FallibleResult<SourceFile> {
        self.content.borrow().serialize().map_err(|e| e.into())
    }

    fn ast(&self) -> ast::known::Module {
        self.content.borrow().ast.clone_ref()
    }

    fn find_definition
    (&self,id:&double_representation::graph::Id) -> FallibleResult<DefinitionInfo> {
        let ast = self.content.borrow().ast.clone_ref();
        double_representation::module::get_definition(&ast, id)
    }

    fn node_metadata(&self, id:ast::Id) -> FallibleResult<NodeMetadata> {
        let data = self.content.borrow().metadata.ide.node.get(&id).cloned();
        data.ok_or_else(|| NodeMetadataNotFound(id).into())
    }

    fn update_whole(&self, content:Content) {
        *self.content.borrow_mut() = content;
        self.notifications.notify(Notification::Invalidate);
    }

    fn update_ast(&self, ast:ast::known::Module) {
        self.content.borrow_mut().ast  = ast;
        self.notifications.notify(Notification::Invalidate);
    }

    fn apply_code_change
    (&self, change:TextChange, parser:&Parser, new_id_map:ast::IdMap) -> FallibleResult<()> {
        let mut code          = self.ast().repr();
        let replaced_location = TextLocation::convert_range(&code,&change.replaced);
        change.apply(&mut code);
        let new_ast = parser.parse(code,new_id_map)?.try_into()?;
        self.content.borrow_mut().ast = new_ast;
        self.notifications.notify(Notification::CodeChanged {change,replaced_location});
        Ok(())
    }

    fn set_node_metadata(&self, id:ast::Id, data:NodeMetadata) {
        self.content.borrow_mut().metadata.ide.node.insert(id, data);
        self.notifications.notify(Notification::MetadataChanged);
    }

    fn remove_node_metadata(&self, id:ast::Id) -> FallibleResult<NodeMetadata> {
        let lookup = self.content.borrow_mut().metadata.ide.node.remove(&id);
        let data   = lookup.ok_or_else(|| NodeMetadataNotFound(id))?;
        self.notifications.notify(Notification::MetadataChanged);
        Ok(data)
    }

    fn with_node_metadata(&self, id:ast::Id, fun:Box<dyn FnOnce(&mut NodeMetadata) + '_>) {
        let lookup   = self.content.borrow_mut().metadata.ide.node.remove(&id);
        let mut data = lookup.unwrap_or_default();
        fun(&mut data);
        self.content.borrow_mut().metadata.ide.node.insert(id, data);
        self.notifications.notify(Notification::MetadataChanged);
    }
}



// =============
// === Tests ===
// =============

#[cfg(test)]
mod test {
    use super::*;

    use crate::executor::test_utils::TestWithLocalPoolExecutor;
    use crate::model::module::Position;

    use data::text;
    use utils::test::traits::*;

    #[wasm_bindgen_test]
    fn applying_code_change() {
        let _test  = TestWithLocalPoolExecutor::set_up();
        let module = model::module::test::plain_from_code("2 + 2");
        let change = TextChange {
            replaced: text::Index::new(2)..text::Index::new(5),
            inserted: "- abc".to_string(),
        };
        module.apply_code_change(change,&Parser::new_or_panic(),default()).unwrap();
        assert_eq!("2 - abc",module.ast().repr());
    }

    #[wasm_bindgen_test]
    fn notifying() {
        let mut test         = TestWithLocalPoolExecutor::set_up();
        let module           = model::module::test::plain_from_code("");
        let mut subscription = module.subscribe().boxed_local();
        subscription.expect_pending();

        // Ast update
        let new_line       = Ast::infix_var("a","+","b");
        let new_module_ast = Ast::one_line_module(new_line);
        let new_module_ast = ast::known::Module::try_new(new_module_ast).unwrap();
        module.update_ast(new_module_ast.clone_ref());
        test.run_until_stalled();
        assert_eq!(Some(Notification::Invalidate), test.expect_completion(subscription.next()));
        subscription.expect_pending();

        // Code change
        let change = TextChange {
            replaced: text::Index::new(0)..text::Index::new(1),
            inserted: "foo".to_string(),
        };
        module.apply_code_change(change.clone(),&Parser::new_or_panic(),default()).unwrap();
        test.run_until_stalled();
        let replaced_location = TextLocation{line:0, column:0}..TextLocation{line:0, column:1};
        let notification      = Notification::CodeChanged {change,replaced_location};
        assert_eq!(Some(notification),test.expect_completion(subscription.next()));
        subscription.expect_pending();

        // Metadata update
        let id            = Uuid::new_v4();
        let node_metadata = NodeMetadata {position:Some(Position::new(1.0, 2.0)),..default()};
        module.set_node_metadata(id.clone(),node_metadata.clone());
        assert_eq!(Some(Notification::MetadataChanged),test.expect_completion(subscription.next()));
        subscription.expect_pending();
        module.remove_node_metadata(id.clone()).unwrap();
        assert_eq!(Some(Notification::MetadataChanged),test.expect_completion(subscription.next()));
        subscription.expect_pending();
        module.with_node_metadata(id.clone(),Box::new(|md| *md = node_metadata.clone()));
        assert_eq!(Some(Notification::MetadataChanged),test.expect_completion(subscription.next()));
        subscription.expect_pending();

        // Whole update
        let mut metadata = Metadata::default();
        metadata.ide.node.insert(id,node_metadata);
        module.update_whole(ParsedSourceFile{ast:new_module_ast, metadata});
        assert_eq!(Some(Notification::Invalidate), test.expect_completion(subscription.next()));
        subscription.expect_pending();

        // No more notifications emitted
        drop(module);
        assert_eq!(None, test.expect_completion(subscription.next()));
    }

    #[wasm_bindgen_test]
    fn handling_metadata() {
        let _test  = TestWithLocalPoolExecutor::set_up();
        let module = model::module::test::plain_from_code("");

        let id         = Uuid::new_v4();
        let initial_md = module.node_metadata(id.clone());
        assert!(initial_md.is_err());

        let md_to_set = NodeMetadata {position:Some(Position::new(1.0, 2.0)),..default()};
        module.set_node_metadata(id.clone(),md_to_set.clone());
        assert_eq!(md_to_set.position, module.node_metadata(id.clone()).unwrap().position);

        let new_pos = Position::new(4.0, 5.0);
        module.with_node_metadata(id.clone(), Box::new(|md| {
            assert_eq!(md_to_set.position, md.position);
            md.position = Some(new_pos);
        }));
        assert_eq!(Some(new_pos), module.node_metadata(id).unwrap().position);
    }
}
