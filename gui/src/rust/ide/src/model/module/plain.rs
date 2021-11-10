//! The plain Module Model.

use crate::prelude::*;

use crate::model::module::Content;
use crate::model::module::Metadata;
use crate::model::module::NodeMetadata;
use crate::model::module::NodeMetadataNotFound;
use crate::model::module::Notification;
use crate::model::module::NotificationKind;
use crate::model::module::Path;
use crate::model::module::ProjectMetadata;
use crate::notification;

use data::text::TextChange;
use data::text::TextLocation;
use double_representation::definition::DefinitionInfo;
use flo_stream::Subscriber;
use parser::api::ParsedSourceFile;
use parser::api::SourceFile;
use parser::Parser;

/// A structure describing the module.
///
/// It implements internal mutability pattern, so the state may be shared between different
/// controllers. Each change in module will emit notification for each module representation
/// (text and graph).
#[derive(Debug)]
pub struct Module {
    logger:        Logger,
    path:          Path,
    content:       RefCell<Content>,
    notifications: notification::Publisher<Notification>,
    repository:    Rc<model::undo_redo::Repository>,
}

impl Module {
    /// Create state with given content.
    pub fn new(
        parent: impl AnyLogger,
        path: Path,
        ast: ast::known::Module,
        metadata: Metadata,
        repository: Rc<model::undo_redo::Repository>,
    ) -> Self {
        Module {
            logger: Logger::new_sub(parent, path.to_string()),
            content: RefCell::new(ParsedSourceFile { ast, metadata }),
            notifications: default(),
            path,
            repository,
        }
    }

    /// Replace the module's content with the new value and emit notification of given kind.
    ///
    /// Fails if the `new_content` is so broken that it cannot be serialized to text. In such case
    /// the module's state is guaranteed to remain unmodified and the notification will not be
    /// emitted.
    fn set_content(&self, new_content: Content, kind: NotificationKind) -> FallibleResult {
        if new_content == *self.content.borrow() {
            debug!(self.logger, "Ignoring spurious update.");
            return Ok(());
        }
        trace!(self.logger, "Updating module's content: {kind:?}. New content:\n{new_content}");
        let transaction = self.repository.transaction("Setting module's content");
        transaction.fill_content(self.id(), self.content.borrow().clone());

        // We want the line below to fail before changing state.
        let new_file = new_content.serialize()?;
        let notification = Notification { new_file, kind };
        self.content.replace(new_content);
        self.notifications.notify(notification);
        Ok(())
    }

    /// Use `f` to update the module's content.
    ///
    /// # Panics
    ///
    ///  This method is intended as internal implementation helper.
    /// `f` gets the borrowed `content`, so any attempt to borrow it directly or transitively from
    /// `self.content` again will panic.
    fn update_content<R>(
        &self,
        kind: NotificationKind,
        f: impl FnOnce(&mut Content) -> R,
    ) -> FallibleResult<R> {
        let mut content = self.content.borrow().clone();
        let ret = f(&mut content);
        self.set_content(content, kind)?;
        Ok(ret)
    }

    /// Use `f` to update the module's content.
    ///
    /// # Panics
    ///
    ///  This method is intended as internal implementation helper.
    /// `f` gets the borrowed `content`, so any attempt to borrow it directly or transitively from
    /// `self.content` again will panic.
    fn try_updating_content<R>(
        &self,
        kind: NotificationKind,
        f: impl FnOnce(&mut Content) -> FallibleResult<R>,
    ) -> FallibleResult<R> {
        let mut content = self.content.borrow().clone();
        let ret = f(&mut content)?;
        self.set_content(content, kind)?;
        Ok(ret)
    }

    /// Get the module's ID.
    pub fn id(&self) -> model::module::Id {
        self.path.id()
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

    fn find_definition(
        &self,
        id: &double_representation::graph::Id,
    ) -> FallibleResult<DefinitionInfo> {
        let ast = self.content.borrow().ast.clone_ref();
        double_representation::module::get_definition(&ast, id)
    }

    fn node_metadata(&self, id: ast::Id) -> FallibleResult<NodeMetadata> {
        let data = self.content.borrow().metadata.ide.node.get(&id).cloned();
        data.ok_or_else(|| NodeMetadataNotFound(id).into())
    }

    fn update_whole(&self, content: Content) -> FallibleResult {
        self.set_content(content, NotificationKind::Invalidate)
    }

    fn update_ast(&self, ast: ast::known::Module) -> FallibleResult {
        self.update_content(NotificationKind::Invalidate, |content| content.ast = ast)
    }

    fn apply_code_change(
        &self,
        change: TextChange,
        parser: &Parser,
        new_id_map: ast::IdMap,
    ) -> FallibleResult {
        let code = self.ast().repr();
        let replaced_location = TextLocation::convert_range(&code, &change.replaced);
        let new_code = change.applied(&code);
        let new_ast = parser.parse(new_code, new_id_map)?.try_into()?;
        let notification = NotificationKind::CodeChanged { change, replaced_location };
        self.update_content(notification, |content| content.ast = new_ast)
    }

    fn set_node_metadata(&self, id: ast::Id, data: NodeMetadata) -> FallibleResult {
        self.update_content(NotificationKind::MetadataChanged, |content| {
            let _ = content.metadata.ide.node.insert(id, data);
        })
    }

    fn remove_node_metadata(&self, id: ast::Id) -> FallibleResult<NodeMetadata> {
        self.try_updating_content(NotificationKind::MetadataChanged, |content| {
            let lookup = content.metadata.ide.node.remove(&id);
            lookup.ok_or_else(|| NodeMetadataNotFound(id).into())
        })
    }

    fn with_node_metadata(
        &self,
        id: ast::Id,
        fun: Box<dyn FnOnce(&mut NodeMetadata) + '_>,
    ) -> FallibleResult {
        self.update_content(NotificationKind::MetadataChanged, |content| {
            let lookup = content.metadata.ide.node.remove(&id);
            let mut data = lookup.unwrap_or_default();
            fun(&mut data);
            content.metadata.ide.node.insert(id, data);
        })
    }

    fn boxed_with_project_metadata(&self, fun: Box<dyn FnOnce(&ProjectMetadata) + '_>) {
        let content = self.content.borrow();
        if let Some(metadata) = content.metadata.ide.project.as_ref() {
            fun(metadata)
        } else {
            fun(&default())
        }
    }

    fn boxed_update_project_metadata(
        &self,
        fun: Box<dyn FnOnce(&mut ProjectMetadata) + '_>,
    ) -> FallibleResult {
        self.update_content(NotificationKind::MetadataChanged, |content| {
            let mut data = content.metadata.ide.project.clone().unwrap_or_default();
            fun(&mut data);
            content.metadata.ide.project = Some(data);
        })
    }
}

impl model::undo_redo::Aware for Module {
    fn undo_redo_repository(&self) -> Rc<model::undo_redo::Repository> {
        self.repository.clone()
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
        let _test = TestWithLocalPoolExecutor::set_up();
        let module = model::module::test::plain_from_code("2 + 2");
        let change = TextChange {
            replaced: text::Index::new(2)..text::Index::new(5),
            inserted: "- abc".to_string(),
        };
        module.apply_code_change(change, &Parser::new_or_panic(), default()).unwrap();
        assert_eq!("2 - abc", module.ast().repr());
    }

    #[wasm_bindgen_test]
    fn notifying() {
        let mut test = TestWithLocalPoolExecutor::set_up();
        let module = model::module::test::plain_from_code("");
        let mut subscription = module.subscribe().boxed_local();
        subscription.expect_pending();

        let mut expect_notification = |kind: NotificationKind| {
            let notification = test.expect_completion(subscription.next()).unwrap();
            assert_eq!(kind, notification.kind);
            assert_eq!(module.serialized_content().unwrap(), notification.new_file);
            // We expect exactly one notification. There should be no more.
            subscription.expect_pending();
        };

        // Ast update
        let new_line = Ast::infix_var("a", "+", "b");
        let new_module_ast = Ast::one_line_module(new_line);
        let new_module_ast = ast::known::Module::try_new(new_module_ast).unwrap();
        module.update_ast(new_module_ast.clone_ref()).unwrap();
        expect_notification(NotificationKind::Invalidate);

        // Code change
        let change = TextChange {
            replaced: text::Index::new(0)..text::Index::new(1),
            inserted: "foo".to_string(),
        };
        module.apply_code_change(change.clone(), &Parser::new_or_panic(), default()).unwrap();
        let replaced_location =
            TextLocation { line: 0, column: 0 }..TextLocation { line: 0, column: 1 };
        expect_notification(NotificationKind::CodeChanged { change, replaced_location });

        // Metadata update
        let id = Uuid::new_v4();
        let node_metadata = NodeMetadata { position: Some(Position::new(1.0, 2.0)), ..default() };
        module.set_node_metadata(id.clone(), node_metadata.clone()).unwrap();
        expect_notification(NotificationKind::MetadataChanged);

        module.remove_node_metadata(id.clone()).unwrap();
        expect_notification(NotificationKind::MetadataChanged);

        module.with_node_metadata(id.clone(), Box::new(|md| *md = node_metadata.clone())).unwrap();
        expect_notification(NotificationKind::MetadataChanged);

        // Whole update
        let mut metadata = Metadata::default();
        metadata.ide.node.insert(id, node_metadata);
        module.update_whole(ParsedSourceFile { ast: new_module_ast, metadata }).unwrap();
        expect_notification(NotificationKind::Invalidate);

        // No more notifications emitted
        drop(module);
        assert_eq!(None, test.expect_completion(subscription.next()));
    }

    #[wasm_bindgen_test]
    fn handling_metadata() {
        let _test = TestWithLocalPoolExecutor::set_up();
        let module = model::module::test::plain_from_code("");

        let id = Uuid::new_v4();
        let initial_md = module.node_metadata(id.clone());
        assert!(initial_md.is_err());

        let md_to_set = NodeMetadata { position: Some(Position::new(1.0, 2.0)), ..default() };
        module.set_node_metadata(id.clone(), md_to_set.clone()).unwrap();
        assert_eq!(md_to_set.position, module.node_metadata(id.clone()).unwrap().position);

        let new_pos = Position::new(4.0, 5.0);
        module
            .with_node_metadata(
                id.clone(),
                Box::new(|md| {
                    assert_eq!(md_to_set.position, md.position);
                    md.position = Some(new_pos);
                }),
            )
            .unwrap();
        assert_eq!(Some(new_pos), module.node_metadata(id).unwrap().position);
    }
}
