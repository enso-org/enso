//! A Wrapper for module which synchronizes opening/closing and all changes with Language Server.

use crate::prelude::*;
use enso_text::index::*;

use crate::model::module::Content;
use crate::model::module::ImportMetadata;
use crate::model::module::NodeMetadata;
use crate::model::module::Notification;
use crate::model::module::NotificationKind;
use crate::model::module::Path;
use crate::model::module::ProjectMetadata;
use crate::model::module::TextChange;
use crate::model::module::API;

use ast::IdMap;
use double_representation::definition::DefinitionInfo;
use double_representation::graph::Id;
use double_representation::import;
use engine_protocol::language_server;
use engine_protocol::language_server::FileEdit;
use engine_protocol::language_server::TextEdit;
use engine_protocol::types::Sha3_224;
use enso_text::text;
use enso_text::Location;
use enso_text::Range;
use flo_stream::Subscriber;
use parser::api::SourceFile;
use parser::Parser;



// =======================
// === Content Summary ===
// =======================

/// The minimal information about module's content, required to do properly invalidation of opened
/// module in Language Server.
#[derive(Clone, Debug, Eq, PartialEq)]
struct ContentSummary {
    digest:      Sha3_224,
    /// The Engine's Locations are in java Characters, which are equal to UTF16 Code Units.
    /// See https://docs.oracle.com/javase/8/docs/api/java/lang/Character.html
    end_of_file: Location<enso_text::Utf16CodeUnit>,
}

impl ContentSummary {
    fn new(text: &text::Rope) -> Self {
        let parts = text.rope.iter_chunks(..).map(|s| s.as_bytes());
        let end_location_bytes = text.last_line_end_location();
        let end_of_file = text.utf16_code_unit_location_of_location(end_location_bytes);
        Self { digest: Sha3_224::from_parts(parts), end_of_file }
    }
}

/// The information about module's content. In addition to minimal summery defined in
/// `ContentSummary` it adds information about sections, what enables efficient updates after code
/// and metadata changes.
#[derive(Clone, Debug, Deref)]
struct ParsedContentSummary {
    #[deref]
    summary:  ContentSummary,
    source:   text::Rope,
    code:     Range<Location<Byte>>,
    id_map:   Range<Location<Byte>>,
    metadata: Range<Location<Byte>>,
}

impl ParsedContentSummary {
    /// Get summary from `SourceFile`.
    fn from_source(source: &SourceFile) -> Self {
        let content = text::Rope::from(&source.content);
        let to_location = |i: Byte| content.offset_to_location_snapped(i);
        let code = source.code.map(to_location);
        let id_map = source.id_map.map(to_location);
        let metadata = source.metadata.map(to_location);
        ParsedContentSummary {
            summary: ContentSummary::new(&content),
            source: content,
            code,
            id_map,
            metadata,
        }
    }

    // Get fragment of string with code.
    pub fn code_slice(&self) -> text::Rope {
        self.slice(&self.code)
    }

    /// Get fragment of string with id map.
    pub fn id_map_slice(&self) -> text::Rope {
        self.slice(&self.id_map)
    }

    /// Get fragment of string with metadata.
    pub fn metadata_slice(&self) -> text::Rope {
        self.slice(&self.metadata)
    }

    pub fn id_map_engine_range(&self) -> Range<Location<enso_text::Utf16CodeUnit>> {
        self.id_map.map(|l| self.source.utf16_code_unit_location_of_location(l))
    }

    pub fn metadata_engine_range(&self) -> Range<Location<enso_text::Utf16CodeUnit>> {
        self.metadata.map(|l| self.source.utf16_code_unit_location_of_location(l))
    }

    fn slice(&self, range: &Range<Location<Byte>>) -> text::Rope {
        let start_ix = self.source.location_offset_snapped(range.start);
        let end_ix = self.source.location_offset_snapped(range.end);
        self.source.sub(Range::new(start_ix, end_ix))
    }
}

/// The information about module's state currently held in LanguageServer.
#[derive(Clone, Debug, Default)]
enum LanguageServerContent {
    /// The content is synchronized with our module state after last fully handled notification.
    Synchronized(ParsedContentSummary),
    /// The content is not known due to an unrecognized error received from the Language Server
    /// after applying the last update. We don't know if, and to what extent it was applied.
    #[default]
    Unknown,
}



// ===========================
// === Synchronized Module ===
// ===========================

/// A Module which state is synchronized with Language Server using its textual API.
///
/// This struct owns  `model::Module`, load the state during creation and updates LS about all
/// changes done to it. On drop the module is closed in the Language Server.
///
/// See also (enso protocol documentation)
/// [https://github.com/enso-org/enso/blob/develop/docs/language-server/protocol-language-server.md].
#[derive(Debug)]
pub struct Module {
    model:           model::module::Plain,
    parser:          Parser,
    ls_content:      Rc<RefCell<LanguageServerContent>>,
    language_server: Rc<language_server::Connection>,
}


// === Public API ===

impl Module {
    /// Open the module.
    ///
    /// This function will open the module in Language Server and schedule task which will send
    /// updates about module's change to Language Server.
    #[profile(Detail)]
    pub async fn open(
        path: Path,
        language_server: Rc<language_server::Connection>,
        parser: Parser,
        repository: Rc<model::undo_redo::Repository>,
        read_only: Rc<Cell<bool>>,
    ) -> FallibleResult<Rc<Self>> {
        let file_path = path.file_path().clone();
        info!("Opening module {file_path}");
        let opened = language_server.client.open_text_file(&file_path).await?;
        let content: text::Rope = (&opened.content).into();
        info!("Read content of the module {path}, digest is {:?}", opened.current_version);

        let end_of_file_byte = content.last_line_end_location();
        let ls_content_summary = ContentSummary {
            digest:      opened.current_version,
            end_of_file: content.utf16_code_unit_location_of_location(end_of_file_byte),
        };

        let source = parser.parse_with_metadata(opened.content);
        // We set ls_content field as default, because it will be replaced anyway upon initial
        // invalidation.
        let ls_content = default();
        let metadata = source.metadata;
        let ast = source.ast;
        let model = model::module::Plain::new(path, ast, metadata, repository, read_only);
        let this = Rc::new(Module { model, ls_content, parser, language_server });

        // The parsed source may introduce changes in metadata (most prominently the missing AST ids
        // are added), so immediately the content in our model differs from Language Server State.
        // We need to sent an initial invalidation.
        let actual_content = this.model.serialized_content()?;
        let initial_invalidation = this.full_invalidation(ls_content_summary, actual_content);
        let runner = Self::runner(this.clone_ref());
        executor::global::spawn(initial_invalidation.then(move |()| runner));
        Ok(this)
    }

    /// Create a module mock.
    pub fn mock(model: model::module::Plain) -> Rc<Self> {
        let client = language_server::MockClient::default();
        client.expect.close_text_file(|_| Ok(()));
        // We don't expect any other call, because we don't execute `runner()`.
        let language_server = language_server::Connection::new_mock_rc(client);
        let ls_content = default();
        let parser = Parser::new();
        Rc::new(Module { model, language_server, ls_content, parser })
    }

    /// Reopen file in the Language Server.
    ///
    /// After reopening we update the LS state with the model's current content.
    pub async fn reopen_file(&self, new_file: SourceFile) -> FallibleResult {
        let file_path = self.path();
        info!("Reopening file {file_path}.");
        if let Err(error) = self.language_server.client.close_text_file(file_path).await {
            error!("Error while reopening file {file_path}: Closing operation failed: {error} Trying to open the file anyway.");
        }
        let opened = self.language_server.client.open_text_file(file_path).await?;
        let content = opened.content.into();
        let summary = ContentSummary::new(&content);

        self.full_invalidation(summary, new_file).await;
        Ok(())
    }

    /// Apply text changes received from the language server.
    pub async fn apply_text_change_from_ls(&self, edits: Vec<TextEdit>) -> FallibleResult {
        let mut content: text::Rope = self.serialized_content()?.content.into();
        for TextEdit { range, text } in edits {
            let start = content.location_of_utf16_code_unit_location_snapped(range.start.into());
            let end = content.location_of_utf16_code_unit_location_snapped(range.end.into());
            let start = <Byte as enso_text::FromInContextSnapped<&text::Rope, Location<Byte>>>::from_in_context_snapped(&content, start);
            let end = <Byte as enso_text::FromInContextSnapped<&text::Rope, Location<Byte>>>::from_in_context_snapped(&content, end);
            let range = Range { start, end };
            let change = TextChange { range, text };
            content.apply_change(change);
        }
        // We don't need to spent our resources of computing what exactly the Language Server state
        // looks like, because it will be soon updated anyway - see [`set_module_content_from_ls`]
        // documentation. Just in case, we set it as unknown for now.
        self.ls_content.replace(LanguageServerContent::Unknown);
        self.set_module_content_from_ls(content).await
    }

    /// Update the module with content received from the language server. This function takes the
    /// file content as reloaded from the language server, or reconstructed from a `text/didChange`
    /// notification. It parses the new file content and updates the module with the parsed content,
    /// sending `NotificationKind::Reloaded` notification.
    ///
    /// The module content changes during parsing, and the language server is notified of this
    /// change.
    async fn set_module_content_from_ls(&self, content: text::Rope) -> FallibleResult {
        let transaction = self.undo_redo_repository().transaction("Setting module's content");
        transaction.fill_content(self.id(), self.content().borrow().clone());
        let parsed_source = self.parser.parse_with_metadata(content.to_string());
        let source = parsed_source.serialize()?;
        self.content().replace(parsed_source);
        let summary = ContentSummary::new(&content);
        let change = TextEdit::from_prefix_postfix_differences(&content, &source.content);
        let notify_ls = self.notify_language_server(summary.digest, &source, vec![change], true);
        let notification = Notification::new(source, NotificationKind::Reloaded);
        self.notify(notification);
        notify_ls.await;
        Ok(())
    }
}

impl API for Module {
    fn subscribe(&self) -> Subscriber<Notification> {
        self.model.subscribe()
    }

    fn path(&self) -> &Path {
        self.model.path()
    }

    fn serialized_content(&self) -> FallibleResult<SourceFile> {
        self.model.serialized_content()
    }

    fn ast(&self) -> ast::known::Module {
        self.model.ast()
    }

    fn find_definition(&self, id: &Id) -> FallibleResult<DefinitionInfo> {
        self.model.find_definition(id)
    }

    fn node_metadata(&self, id: ast::Id) -> FallibleResult<NodeMetadata> {
        self.model.node_metadata(id)
    }

    fn update_whole(&self, content: Content) -> FallibleResult {
        self.model.update_whole(content)
    }

    fn update_ast(&self, ast: ast::known::Module) -> FallibleResult {
        self.model.update_ast(ast)
    }

    fn apply_code_change(
        &self,
        change: TextChange,
        parser: &Parser,
        new_id_map: IdMap,
    ) -> FallibleResult {
        self.model.apply_code_change(change, parser, new_id_map)
    }

    fn set_node_metadata(&self, id: ast::Id, data: NodeMetadata) -> FallibleResult {
        self.model.set_node_metadata(id, data)
    }

    fn remove_node_metadata(&self, id: ast::Id) -> FallibleResult<NodeMetadata> {
        self.model.remove_node_metadata(id)
    }

    fn with_node_metadata(
        &self,
        id: ast::Id,
        fun: Box<dyn FnOnce(&mut NodeMetadata) + '_>,
    ) -> FallibleResult {
        self.model.with_node_metadata(id, fun)
    }

    fn with_import_metadata(
        &self,
        id: import::Id,
        fun: Box<dyn FnOnce(&mut ImportMetadata) + '_>,
    ) -> FallibleResult {
        self.model.with_import_metadata(id, fun)
    }

    fn all_import_metadata(&self) -> Vec<(import::Id, ImportMetadata)> {
        self.model.all_import_metadata()
    }

    fn remove_import_metadata(&self, id: import::Id) -> FallibleResult<ImportMetadata> {
        self.model.remove_import_metadata(id)
    }

    fn boxed_with_project_metadata(&self, fun: Box<dyn FnOnce(&ProjectMetadata) + '_>) {
        self.model.boxed_with_project_metadata(fun)
    }

    fn boxed_update_project_metadata(
        &self,
        fun: Box<dyn FnOnce(&mut ProjectMetadata) + '_>,
    ) -> FallibleResult {
        self.model.boxed_update_project_metadata(fun)
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn restore_temporary_changes(&self) -> FallibleResult {
        self.model.restore_temporary_changes()
    }

    fn reopen_file_in_language_server(&self) -> BoxFuture<FallibleResult> {
        let file = self.model.content().borrow().serialize();
        async { self.reopen_file(file?).await }.boxed_local()
    }
}


// === Synchronizing Language Server ===

impl Module {
    /// Listen for all module changes and sends proper updates to the Language Server.
    ///
    /// This function returns once the  notifications from underlying [module model](plain::Module)
    /// stream is exhausted. The `self` argument is kept as `Weak` when waiting for next
    /// notification, so there is no risk of leak.
    async fn runner(self: Rc<Self>) {
        let mut subscriber = self.model.subscribe();
        let weak = Rc::downgrade(&self);
        drop(self);
        loop {
            let notification = subscriber.next().await;
            let this = weak.upgrade();
            match (notification, this) {
                (Some(notification), Some(this)) => {
                    debug!("Processing a notification: {notification:?}");
                    this.handle_notification(notification).await;
                }
                _ => break,
            }
        }
    }

    /// Send to LanguageServer update about received notification about module, and update the
    /// [`ls_content`] field accordingly.
    async fn handle_notification(&self, notification: Notification) {
        let Notification { new_file, kind, profiler } = notification;
        let _profiler = profiler::start_debug!(profiler, "handle_notification");
        let current_ls_content = self.ls_content.take();
        debug!("Handling notification when known LS content is {current_ls_content:?}.");
        match current_ls_content {
            LanguageServerContent::Unknown => {
                if let Err(error) = profiler::await_!(self.reopen_file(new_file), _profiler) {
                    error!("Error while reloading module model: {error}");
                }
            }
            LanguageServerContent::Synchronized(summary) => match kind {
                NotificationKind::Invalidate =>
                    profiler::await_!(self.partial_invalidation(summary, new_file), _profiler),
                NotificationKind::CodeChanged { change, replaced_location } => {
                    let to_engine_location =
                        |l: Location<Byte>| summary.source.utf16_code_unit_location_of_location(l);
                    let code_change = TextEdit {
                        range: replaced_location.map(to_engine_location).into(),
                        text:  change.text,
                    };
                    let id_map_change = TextEdit {
                        range: summary.id_map_engine_range().into(),
                        text:  new_file.id_map_slice().to_string(),
                    };
                    //id_map goes first, because code change may alter its position.
                    let edits = vec![id_map_change, code_change];
                    let summary_digest = summary.summary.digest;
                    let notify_ls =
                        self.notify_language_server(summary_digest, &new_file, edits, true);
                    profiler::await_!(notify_ls, _profiler)
                }
                NotificationKind::MetadataChanged => {
                    let edits = vec![TextEdit {
                        range: summary.metadata_engine_range().into(),
                        text:  new_file.metadata_slice().to_string(),
                    }];
                    let summary_digest = summary.summary.digest;
                    let notify_ls =
                        self.notify_language_server(summary_digest, &new_file, edits, false);
                    profiler::await_!(notify_ls, _profiler)
                }
                NotificationKind::Reloaded => {}
            },
        }
    }

    /// Send update to Language Server with the entire file content.
    #[profile(Debug)]
    fn full_invalidation(
        &self,
        ls_content: ContentSummary,
        new_file: SourceFile,
    ) -> impl Future<Output = ()> + 'static {
        debug!("Handling full invalidation: {ls_content:?}.");
        let range = Range::new(Location::default(), ls_content.end_of_file);
        let edits = vec![TextEdit { range: range.into(), text: new_file.content.clone() }];
        self.notify_language_server(ls_content.digest, &new_file, edits, true)
    }

    fn edit_for_snipped(
        start: &Location<Byte>,
        source: text::Rope,
        target: text::Rope,
    ) -> Option<TextEdit> {
        // This is an implicit assumption that always seems to be true. Otherwise finding the
        // correct location for the final edit would be more complex.
        debug_assert_eq!(start.offset, 0.byte());

        let edit = TextEdit::from_prefix_postfix_differences(&source, &target);
        (edit.range.start != edit.range.end || !edit.text.is_empty())
            .as_some_from(|| edit.move_by_lines(start.line.value))
    }

    fn edit_for_code(ls_content: &ParsedContentSummary, new_file: &SourceFile) -> Option<TextEdit> {
        Self::edit_for_snipped(
            &ls_content.code.start,
            ls_content.code_slice(),
            new_file.code_slice().into(),
        )
    }

    fn edit_for_metadata(
        ls_content: &ParsedContentSummary,
        new_file: &SourceFile,
    ) -> Option<TextEdit> {
        Self::edit_for_snipped(
            &ls_content.metadata.start,
            ls_content.metadata_slice(),
            new_file.metadata_slice().into(),
        )
    }

    fn edit_for_idmap(
        ls_content: &ParsedContentSummary,
        new_file: &SourceFile,
    ) -> Option<TextEdit> {
        Self::edit_for_snipped(
            &ls_content.id_map.start,
            ls_content.id_map_slice(),
            new_file.id_map_slice().into(),
        )
    }

    /// Send update to Language Server with the changed file content.
    ///
    /// Note that a heuristic is used to determine the changed file content. The indicated change
    /// might not be the minimal diff, but will contain all changes.
    #[profile(Debug)]
    fn partial_invalidation(
        &self,
        ls_content: ParsedContentSummary,
        new_file: SourceFile,
    ) -> impl Future<Output = ()> + 'static {
        debug!("Handling partial invalidation: {ls_content:?}.");
        let edits = vec![
            //id_map and metadata go first, because code change may alter their position.
            Self::edit_for_idmap(&ls_content, &new_file),
            Self::edit_for_metadata(&ls_content, &new_file),
            Self::edit_for_code(&ls_content, &new_file),
        ]
        .into_iter()
        .flatten()
        .collect_vec();
        self.notify_language_server(ls_content.summary.digest, &new_file, edits, true)
    }

    /// This is a helper function with all common logic regarding sending the update to
    /// Language Server.
    ///
    /// The current value of `Self::ls_content` field s not used in this function, but replaced
    /// with the new one basing on `new_file` argument.
    #[profile(Debug)]
    fn notify_language_server(
        &self,
        ls_content_digest: Sha3_224,
        new_file: &SourceFile,
        edits: Vec<TextEdit>,
        execute: bool,
    ) -> impl Future<Output = ()> + 'static {
        let summary = ParsedContentSummary::from_source(new_file);
        let edit = FileEdit {
            edits,
            path: self.path().file_path().clone(),
            old_version: ls_content_digest,
            new_version: Sha3_224::new(new_file.content.as_bytes()),
        };
        let ls_future_reply = self.language_server.client.apply_text_file_edit(&edit, &execute);
        let ls_content = self.ls_content.clone_ref();
        async move {
            debug!("Notifying LS with edit: {edit:#?}.");
            match ls_future_reply.await {
                Ok(()) => {
                    debug!("Updating the LS content digest to: {:?}", summary);
                    ls_content.replace(LanguageServerContent::Synchronized(summary));
                }
                Err(err) => {
                    error!("Error during sending text change to Language Server: {err}");
                    ls_content.replace(LanguageServerContent::Unknown);
                }
            }
        }
    }
}

impl Drop for Module {
    fn drop(&mut self) {
        let file_path = self.path().file_path().clone();
        let language_server = self.language_server.clone_ref();
        executor::global::spawn(async move {
            let result = language_server.client.close_text_file(&file_path).await;
            if let Err(err) = result {
                error!("Error when closing module file {file_path}: {err}");
            }
        });
    }
}

impl Deref for Module {
    type Target = model::module::Plain;

    fn deref(&self) -> &Self::Target {
        &self.model
    }
}

impl model::undo_redo::Aware for Module {
    fn undo_redo_repository(&self) -> Rc<model::undo_redo::Repository> {
        self.model.undo_redo_repository()
    }
}



// ============
// === Test ===
// ============

#[cfg(test)]
pub mod test {
    use super::*;

    use crate::test::mock;
    use crate::test::Runner;

    use engine_protocol::language_server::FileEdit;
    use engine_protocol::language_server::MockClient;
    use engine_protocol::language_server::Position;
    use engine_protocol::language_server::TextRange;
    use enso_text::text;
    use enso_text::Change;
    use json_rpc::error::RpcError;
    use json_rpc::expect_call;


    // === LsClientSetup ===

    // Ensures that subsequent LS text operations form a consistent series of versions.
    #[derive(Clone, Debug)]
    struct LsClientSetup {
        path:               Path,
        current_ls_content: Rc<CloneCell<text::Rope>>,
        current_ls_version: Rc<CloneCell<Sha3_224>>,
    }

    impl LsClientSetup {
        fn new(path: Path, initial_content: impl Into<text::Rope>) -> Self {
            let current_ls_content = initial_content.into();
            let current_ls_version =
                Sha3_224::from_parts(current_ls_content.iter_chunks(..).map(|ch| ch.as_bytes()));
            debug!("Initial content:\n===\n{current_ls_content}\n===");
            Self {
                path,
                current_ls_content: Rc::new(CloneCell::new(current_ls_content)),
                current_ls_version: Rc::new(CloneCell::new(current_ls_version)),
            }
        }

        /// Create a setup initialized with the data from the given mock description.
        fn new_for_mock_data(data: &crate::test::mock::Unified) -> Self {
            Self::new(data.module_path.clone(), data.get_code())
        }

        /// Set up general text edit expectation in the mock client.
        ///
        /// This edit can be anything (full invalidation, code or metadata edit). The given function
        /// should perform any necessary assertions on the `FileEdit` and provide the result that
        /// the mock client will reply with.
        fn expect_some_edit(
            &self,
            client: &mut MockClient,
            f: impl FnOnce(&FileEdit) -> json_rpc::Result<()> + 'static,
        ) {
            let this = self.clone();
            client.expect.apply_text_file_edit(move |edits, _execute| {
                let content_so_far = this.current_ls_content.get();
                let result = f(edits);
                let new_content = apply_edits(content_so_far, edits);
                let actual_old = this.current_ls_version.get();
                let actual_new =
                    Sha3_224::from_parts(new_content.iter_chunks(..).map(|s| s.as_bytes()));
                debug!("Actual digest:   {actual_old} => {actual_new}");
                debug!("Declared digest: {} => {}", edits.old_version, edits.new_version);
                debug!("New content:\n===\n{new_content}\n===");
                assert_eq!(&edits.path, this.path.file_path());
                assert_eq!(edits.old_version, actual_old);
                assert_eq!(edits.new_version, actual_new);
                if result.is_ok() {
                    this.current_ls_content.set(new_content);
                    this.current_ls_version.set(actual_new);
                } else {
                }
                result
            });
        }

        /// The single text edit with accompanying metadata idmap changes.
        fn expect_edit_with_metadata(
            &self,
            client: &mut MockClient,
            f: impl FnOnce(&TextEdit) -> json_rpc::Result<()> + 'static,
        ) {
            let this = self.clone();
            self.expect_some_edit(client, move |edit| {
                if let [edit_idmap, edit_code] = edit.edits.as_slice() {
                    let code_so_far = this.current_ls_content.get();
                    let file_so_far = SourceFile::new((&code_so_far).into());
                    // TODO [mwu]
                    //  Currently this assumes that the whole idmap is replaced at each edit.
                    //  This code should be adjusted, if partial metadata updates are implemented.
                    let idmap_range = file_so_far.id_map.map(|x| {
                        let location_bytes = code_so_far.offset_to_location_snapped(x);
                        code_so_far.utf16_code_unit_location_of_location(location_bytes)
                    });
                    let idmap_range = TextRange::from(idmap_range);
                    assert_eq!(edit_idmap.range, idmap_range);
                    assert!(SourceFile::looks_like_idmap(&edit_idmap.text));
                    f(edit_code)
                } else {
                    // This test assumes that expected single file edit consists from two text
                    // edits: first for idmap adjustment and the second for the actual code edit.
                    panic!("Expected exactly two edits.");
                }
            });
        }

        /// Set up expectation that the content will be fully invalidated. The mock client will
        /// report a success.
        fn expect_full_invalidation(&self, client: &mut MockClient) {
            self.expect_full_invalidation_result(client, Ok(()))
        }

        /// Set up expectation that the content will be fully invalidated. The mock client will
        /// report a given result.
        fn expect_full_invalidation_result(
            &self,
            client: &mut MockClient,
            result: json_rpc::Result<()>,
        ) {
            let this = self.clone();
            self.expect_some_edit(client, move |edits| {
                let (edit,) = edits.edits.iter().expect_tuple();
                assert_eq!(edit.range, this.whole_document_range());
                result
            });
        }

        fn whole_document_range(&self) -> TextRange {
            let code_so_far = self.current_ls_content.get();
            let end_of_file_bytes = code_so_far.last_line_end_location();
            let end_of_file = code_so_far.utf16_code_unit_location_of_location(end_of_file_bytes);
            TextRange { start: Position { line: 0, character: 0 }, end: end_of_file.into() }
        }

        /// Update the language server file content with a `TextEdit`.
        fn update_ls_content(&self, edit: &TextEdit) {
            let old_content = self.current_ls_content.get();
            let new_content = apply_edit(old_content, edit);
            let new_version =
                Sha3_224::from_parts(new_content.iter_chunks(..).map(|s| s.as_bytes()));
            debug!("Updated content:\n===\n{new_content}\n===");
            self.current_ls_content.set(new_content);
            self.current_ls_version.set(new_version);
        }
    }

    fn apply_edit(code: impl Into<text::Rope>, edit: &TextEdit) -> text::Rope {
        let mut code = code.into();
        let start = code.location_of_utf16_code_unit_location_snapped(edit.range.start.into());
        let start_byte = code.location_offset_snapped(start);
        let end = code.location_of_utf16_code_unit_location_snapped(edit.range.end.into());
        let end_byte = code.location_offset_snapped(end);
        let change = Change { range: Range::new(start_byte, end_byte), text: edit.text.clone() };
        code.apply_change(change);
        code
    }

    fn apply_edits(code: impl Into<text::Rope>, file_edit: &FileEdit) -> text::Rope {
        let initial = code.into();
        file_edit.edits.iter().fold(initial, |content, edit| apply_edit(&content, edit))
    }


    // === Test cases ===

    #[test]
    fn handling_notifications() {
        // The test starts with code as below. Then it replaces the whole AST to print "Test".
        // Then partial edit happens to change Test into Test 2.
        // Tested logic is:
        // * there is an initial invalidation after opening the module
        // * replacing AST causes invalidation
        // * localized text edit emits similarly localized synchronization updates.
        // * modifying the code fails if the read-only mode is enabled.
        let initial_code = "main =\n    println \"Hello World!\"";
        let mut data = crate::test::mock::Unified::new();
        data.set_code(initial_code);
        let read_only: Rc<Cell<bool>> = default();
        // We do actually care about sharing `data` between `test` invocations, as it stores the
        // Parser which is time-consuming to construct.
        let test = |runner: &mut Runner| {
            let module_path = data.module_path.clone();
            let edit_handler = Rc::new(LsClientSetup::new(module_path, initial_code));
            let mut fixture = data.fixture_customize(|data, client, _| {
                client.require_all_calls();
                data.expect_opening_module(client);
                data.expect_closing_module(client);
                // Opening module and metadata generation.
                edit_handler.expect_full_invalidation(client);
                if !read_only.get() {
                    // Explicit AST update.
                    edit_handler.expect_some_edit(client, |edit| {
                        assert!(edit.edits.last().map_or(false, |edit| edit.text.contains("Test")));
                        Ok(())
                    });
                    // Replacing `Test` with `Test 2`
                    edit_handler.expect_edit_with_metadata(client, |edit| {
                        assert_eq!(edit.text, "Test 2");
                        assert_eq!(edit.range, TextRange {
                            start: Position { line: 1, character: 13 },
                            end:   Position { line: 1, character: 17 },
                        });
                        Ok(())
                    });
                }
            });
            fixture.read_only.set(read_only.get());

            let parser = data.parser.clone();
            let module = fixture.synchronized_module();

            let new_content = "main =\n    println \"Test\"";
            let new_ast = parser.parse_module(new_content, default()).unwrap();
            let res = module.update_ast(new_ast);
            if read_only.get() {
                assert!(res.is_err());
            } else {
                assert!(res.is_ok());
            }
            runner.perhaps_run_until_stalled(&mut fixture);
            let change = TextChange { range: (20..24).into(), text: "Test 2".to_string() };
            let res = module.apply_code_change(change, &Parser::new(), default());
            if read_only.get() {
                assert!(res.is_err());
            } else {
                assert!(res.is_ok());
            }
            fixture.run_until_stalled()
        };

        read_only.set(false);
        Runner::run(test);
        read_only.set(true);
        Runner::run(test);
    }

    #[test]
    fn handling_language_server_file_changes() {
        // The test starts with code as below, followed by a full invalidation replacing the whole
        // AST to print "Test". Then the file is changed on the language server side, simulating
        // what would happen if a snapshot is restored from the VCS. The applied `TextEdit` is then
        // used to update the local module content independently and synchronize it with the
        // language server.
        let initial_code = "main =\n    println \"Hello World!\"";
        let mut data = crate::test::mock::Unified::new();
        data.set_code(initial_code);
        let test = |_runner: &mut Runner| {
            let module_path = data.module_path.clone();
            let edit_handler = Rc::new(LsClientSetup::new(module_path, initial_code));
            let mut fixture = data.fixture_customize(|data, client, _| {
                data.expect_opening_module(client);
                data.expect_closing_module(client);
                // Opening module and metadata generation.
                edit_handler.expect_full_invalidation(client);
                // Explicit AST update.
                edit_handler.expect_some_edit(client, |edit| {
                    assert!(edit.edits.last().map_or(false, |edit| edit.text.contains("Test")));
                    Ok(())
                });
                // Expect an edit that changes metadata due to parsing of the reloaded module
                // content.
                edit_handler.expect_some_edit(client, |edits| {
                    let edit_code = &edits.edits[0];
                    // The metadata changed is on line 5.
                    assert_eq!(edit_code.range.start.line, 5);
                    assert_eq!(edit_code.range.end.line, 5);
                    Ok(())
                });
            });

            let parser = data.parser.clone();
            let module = fixture.synchronized_module();

            let new_content = "main =\n    println \"Test\"".to_string();
            let new_ast = parser.parse_module(new_content, default()).unwrap();
            module.update_ast(new_ast).unwrap();
            fixture.run_until_stalled();
            // Replace `Test` with `Test 2` on the language server side and provide a `FileEditList`
            // containing the file changes.
            let edit = TextEdit {
                text:  "Test 2".into(),
                range: TextRange {
                    start: Position { line: 1, character: 13 },
                    end:   Position { line: 1, character: 17 },
                },
            };
            edit_handler.update_ls_content(&edit);
            fixture.run_until_stalled();
            module.apply_text_change_from_ls(vec![edit]).boxed_local().expect_ready().unwrap();
            fixture.run_until_stalled();
        };

        Runner::run(test);
    }

    /// A template for tests checking situation after edit failure due to connectivity issues.
    ///
    /// The test will create model, and - after opening module and performing initialization - will
    /// apply three simple text updates:
    /// 1. The first gets error from the language server
    /// 2. The second should perform reloading file due to error from point 1.
    /// 3. The third outcome depends on the results of the reopening
    ///
    /// The template sets up expectations for initialization and first text change and runs the
    /// `checks_for_edits_after_failure` closure, which should set up expectations for points 2 and
    /// 3.
    fn handling_notification_after_failure_template(
        mut checks_for_edits_after_failure: impl FnMut(&mock::Unified, &LsClientSetup, &mut MockClient),
    ) {
        let initial_code = "main =\n    println \"Hello World!\"";
        let mut data = crate::test::mock::Unified::new();
        data.set_code(initial_code);
        let test = |runner: &mut Runner| {
            let edit_handler = LsClientSetup::new_for_mock_data(&data);
            let mut fixture = data.fixture_customize(|data, client, _| {
                client.require_all_calls();
                data.expect_opening_module(client);
                data.expect_closing_module(client);
                // Opening module and metadata generation.
                edit_handler.expect_full_invalidation(client);

                // Applying code update.
                edit_handler.expect_some_edit(client, |_| Err(RpcError::LostConnection));

                // Checks
                checks_for_edits_after_failure(data, &edit_handler, client);
            });

            let (_module, controller) = fixture.synchronized_module_w_controller();
            runner.perhaps_run_until_stalled(&mut fixture);
            let change = TextChange { range: (20..24).into(), text: "Test 2".to_string() };
            controller.apply_code_change(change.clone()).unwrap();
            runner.perhaps_run_until_stalled(&mut fixture);
            controller.apply_code_change(change.clone()).unwrap();
            runner.perhaps_run_until_stalled(&mut fixture);
            controller.apply_code_change(change).unwrap();
            fixture.run_until_stalled();
        };
        Runner::run(test);
    }

    #[test]
    fn handling_notification_after_failure() {
        handling_notification_after_failure_template(|data, edit_handler, client| {
            let current_ls_content = edit_handler.current_ls_content.clone_ref();
            data.expect_closing_module(client);
            data.expect_opening_module_with_content(client, move || {
                Ok(current_ls_content.get().to_string())
            });
            edit_handler.expect_some_edit(client, |_| Ok(()));

            // Next change is applied normally
            edit_handler.expect_some_edit(client, |_| Ok(()));
        })
    }

    #[test]
    fn handling_file_reopening_after_closing_failure() {
        handling_notification_after_failure_template(|data, edit_handler, client| {
            let path = data.module_path.file_path().clone();
            // Error while closing file should not stop us from continuing reload.
            expect_call!(client.close_text_file(path=path) => Err(RpcError::LostConnection));
            let current_ls_content = edit_handler.current_ls_content.clone_ref();
            data.expect_opening_module_with_content(client, move || {
                Ok(current_ls_content.get().to_string())
            });
            edit_handler.expect_some_edit(client, |_| Ok(()));

            // Next change is applied normally
            edit_handler.expect_some_edit(client, |_| Ok(()));
        })
    }

    #[test]
    fn handling_file_reopening_after_reopening_failure() {
        handling_notification_after_failure_template(|data, edit_handler, client| {
            data.expect_closing_module(client);
            data.expect_opening_module_with_content(client, move || Err(RpcError::LostConnection));

            // As the reopening failed, next change shall again try to reopen file.
            let current_ls_content = edit_handler.current_ls_content.clone_ref();
            data.expect_closing_module(client);
            data.expect_opening_module_with_content(client, move || {
                Ok(current_ls_content.get().to_string())
            });
            edit_handler.expect_some_edit(client, |_| Ok(()));
        })
    }

    #[test]
    fn handling_file_reopening_after_reopening_initial_update_failure() {
        handling_notification_after_failure_template(|data, edit_handler, client| {
            let current_ls_content = edit_handler.current_ls_content.clone_ref();
            data.expect_closing_module(client);
            data.expect_opening_module_with_content(client, move || {
                Ok(current_ls_content.get().to_string())
            });
            edit_handler.expect_some_edit(client, |_| Err(RpcError::LostConnection));

            // The initial update failed, so the next change should try to reopen again.
            let current_ls_content = edit_handler.current_ls_content.clone_ref();
            data.expect_closing_module(client);
            data.expect_opening_module_with_content(client, move || {
                Ok(current_ls_content.get().to_string())
            });
            edit_handler.expect_some_edit(client, |_| Ok(()));
        })
    }


    #[test]
    fn handle_insertion_edits_bug180558676() {
        let source = text::Rope::from("from Standard.Base import all\n\nmain =\n    operator1 = 0.up_to 100 . to_vector . map .noise\n    operator1.sort\n");
        let target = text::Rope::from("from Standard.Base import all\nimport Standard.Visualization\n\nmain =\n    operator1 = 0.up_to 100 . to_vector . map .noise\n    operator1.sort\n");
        let edit = Module::edit_for_snipped(
            &Location { line: 0.into(), offset: 0.into() },
            source,
            target,
        );
        let expected = Some(TextEdit {
            range: TextRange {
                start: Position { line: 1, character: 0 },
                end:   Position { line: 1, character: 0 },
            },
            text:  "import Standard.Visualization\n".to_string(),
        });
        assert_eq!(edit, expected);
    }
}
