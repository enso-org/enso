//! Project controller.
//!
//! Responsible for owning any remote connection clients, and providing controllers for specific
//! files and modules. Expected to live as long as the project remains open in the IDE.

use crate::prelude::*;

use crate::model::module::ProjectMetadata;

use double_representation::name::project;
use double_representation::name::QualifiedName;
use engine_protocol::binary;
use engine_protocol::language_server;
use engine_protocol::language_server::ContentRoot;
use flo_stream::Subscriber;
use mockall::automock;
use parser::Parser;
use uuid::Uuid;


// ==============
// === Export ===
// ==============

pub mod synchronized;



// =============
// === Model ===
// =============

/// The API of the Project Model.
#[automock]
pub trait API: Debug {
    /// Project's name
    fn name(&self) -> ImString;

    /// Project's qualified name
    fn qualified_name(&self) -> project::QualifiedName;

    /// Whether the read-only mode is enabled for the project.
    ///
    /// Read-only mode forbids certain operations, like renaming the project or editing the code
    /// through the IDE.
    fn read_only(&self) -> bool;

    /// Set the read-only mode for the project.
    fn set_read_only(&self, read_only: bool);

    /// Get Language Server JSON-RPC Connection for this project.
    fn json_rpc(&self) -> Rc<language_server::Connection>;

    /// Get Language Server binary Connection for this project.
    fn binary_rpc(&self) -> Rc<binary::Connection>;

    /// Get the engine's version of the project.
    fn engine_version(&self) -> semver::Version;

    /// Get the instance of parser that is set up for this project.
    fn parser(&self) -> Parser;

    /// Get the visualization controller.
    fn visualization(&self) -> &controller::Visualization;

    /// Get the suggestions database.
    fn suggestion_db(&self) -> Rc<model::SuggestionDatabase>;

    /// Get the list of all content roots attached to the project.
    fn content_roots(&self) -> Vec<Rc<ContentRoot>>;

    /// Get content root by id.
    fn content_root_by_id(&self, id: Uuid) -> FallibleResult<Rc<ContentRoot>>;

    /// Returns a model of module opened from file.
    #[allow(clippy::needless_lifetimes)] // Note: Needless lifetimes
    fn module<'a>(
        &'a self,
        path: crate::model::module::Path,
    ) -> BoxFuture<'a, FallibleResult<model::Module>>;

    /// Creates a new execution context with given definition as a root; and registers the context
    /// for receiving update.
    #[allow(clippy::needless_lifetimes)] // Note: Needless lifetimes
    fn create_execution_context<'a>(
        &'a self,
        root_definition: language_server::MethodPointer,
        context_id: model::execution_context::Id,
    ) -> BoxFuture<'a, FallibleResult<model::ExecutionContext>>;

    /// Set a new project name.
    #[allow(clippy::needless_lifetimes)] // Note: Needless lifetimes
    fn rename_project<'a>(&'a self, name: String) -> BoxFuture<'a, FallibleResult<()>>;

    /// Returns the primary content root id for this project.
    fn project_content_root_id(&self) -> Uuid {
        self.json_rpc().project_root().id()
    }

    /// Generates full module's qualified name that includes the leading project name segment.
    fn qualified_module_name(&self, path: &model::module::Path) -> QualifiedName {
        path.qualified_module_name(self.qualified_name())
    }

    /// Get qualified name of the project's `Main` module.
    ///
    /// This module is special, as it needs to be referred by the project name itself.
    fn main_module(&self) -> QualifiedName {
        let name = self.qualified_name();
        QualifiedName::new_main(name)
    }

    /// Get the file path of the project's `Main` module.
    fn main_module_path(&self) -> model::module::Path {
        let main_name = self.main_module();
        let content_root_id = self.project_content_root_id();
        model::module::Path::from_id(content_root_id, &main_name.module_id())
    }

    /// Get a model of the project's main module.
    #[allow(clippy::needless_lifetimes)] // Note: Needless lifetimes
    fn main_module_model<'a>(&'a self) -> BoxFuture<'a, FallibleResult<model::Module>> {
        async move {
            let main_path = self.main_module_path();
            self.module(main_path).await
        }
        .boxed_local()
    }

    /// Subscribe for notifications about project-level events.
    fn subscribe(&self) -> Subscriber<Notification>;

    /// Access undo-redo manager.
    fn urm(&self) -> Rc<model::undo_redo::Manager>;
}

/// Trait for methods that cannot be defined in `API` because it is a trait object.
pub trait APIExt: API {
    /// Access project's metadata with the given function.
    ///
    /// Fails if there is no main module or if it has no project metadata.
    fn with_project_metadata<'a, R>(
        &'a self,
        f: impl FnOnce(&ProjectMetadata) -> R + 'a,
    ) -> BoxFuture<FallibleResult<R>> {
        async move { Ok(self.main_module_model().await?.with_project_metadata(f)) }.boxed_local()
    }
}

impl<T: API> APIExt for T {}

// Note: Needless lifetimes
// ~~~~~~~~~~~~~~~~~~~~~~~~
// Clippy complains about using explicit lifetimes, however the suggested change breaks compilation.
// This is because mockall library generates a `MockAPI` struct based on this code.
// However, so far, I (mwu) have failed to reduce to self-contained repro that could be presented
// to mockall library authors. Further effort needed.

impl Debug for MockAPI {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Mocked Project Model")
    }
}

/// The general, shared Project Model handle.
pub type Project = Rc<dyn API>;
/// Project Model which synchronizes all changes with Language Server.
pub type Synchronized = synchronized::Project;



// ====================
// === Notification ===
// ====================

/// Notification emitted by the project model.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Notification {
    /// One of the backend connections has been lost.
    ConnectionLost(BackendConnection),
    /// Indicates that the project VCS status has changed.
    VcsStatusChanged(VcsStatus),
    /// Indicates that the project has finished execution sucessfully.
    ExecutionComplete,
    /// Indicates failure of the project execution.
    ExecutionFailed,
}

/// Denotes one of backend connections used by a project.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BackendConnection {
    /// The text connection used to transfer JSON messages.
    LanguageServerJson,
    /// The binary conneection used to transfer FlatBuffers messages.
    LanguageServerBinary,
}

/// The VCS status indicates whether the project has been modified compared the most recent VCS
/// snapshot.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum VcsStatus {
    /// The project has been modified since the last VCS snapshot.
    Dirty,
    /// The project is in the same state as the last VCS snapshot.
    Clean,
}



// ============
// === Test ===
// ============

#[cfg(test)]
pub mod test {
    use super::*;

    use futures::future::ready;

    /// Sets up parser expectation on the mock project.
    pub fn expect_parser(project: &mut MockAPI, parser: &Parser) {
        let parser = parser.clone_ref();
        project.expect_parser().returning_st(move || parser.clone_ref());
    }

    /// Sets up module expectation on the mock project, returning a given module.
    pub fn expect_module(project: &mut MockAPI, module: model::Module) {
        let module_path = module.path().clone_ref();
        project
            .expect_module()
            .withf_st(move |path| path == &module_path)
            .returning_st(move |_path| ready(Ok(module.clone_ref())).boxed_local());
    }

    /// Sets up execution context expectation on the mock project, returning a given context.
    pub fn expect_execution_ctx(project: &mut MockAPI, ctx: model::ExecutionContext) {
        let ctx2 = ctx.clone_ref();
        project
            .expect_create_execution_context()
            .withf_st(move |root_definition, _context_id| root_definition == &ctx.current_method())
            .returning_st(move |_root_definition, _context_id| {
                ready(Ok(ctx2.clone_ref())).boxed_local()
            });
    }

    /// Sets up project root id expectation on the mock project, returning a given id.
    pub fn expect_root_id(project: &mut MockAPI, root_id: Uuid) {
        project.expect_project_content_root_id().return_const(root_id);
    }

    /// Sets up suggestion database expectation on the mock project, returning a given database.
    pub fn expect_suggestion_db(
        project: &mut MockAPI,
        suggestion_db: Rc<model::SuggestionDatabase>,
    ) {
        project.expect_suggestion_db().returning_st(move || suggestion_db.clone_ref());
    }

    /// Sets up JSON RPC expectation on the mock project, returning a given connection.
    pub fn expect_json_rpc(project: &mut MockAPI, json_rpc: Rc<language_server::Connection>) {
        project.expect_json_rpc().returning_st(move || json_rpc.clone_ref());
    }

    /// Sets up binary RPC expectation on the mock project, returning a given connection.
    pub fn expect_binary_rpc(project: &mut MockAPI, binary_rpc: Rc<binary::Connection>) {
        project.expect_binary_rpc().returning_st(move || binary_rpc.clone_ref());
    }

    /// Sets up name expectation on the mock project, returning a given name.
    pub fn expect_name(project: &mut MockAPI, name: impl Into<ImString>) {
        let name = name.into();
        project.expect_name().returning_st(move || name.clone());
    }

    pub fn expect_qualified_name(project: &mut MockAPI, name: &project::QualifiedName) {
        let name = name.clone();
        project.expect_qualified_name().returning_st(move || name.clone());
    }

    pub fn expect_qualified_module_name(project: &mut MockAPI) {
        let name = project.qualified_name();
        project.expect_qualified_module_name().returning_st(move |path: &model::module::Path| {
            path.qualified_module_name(name.clone())
        });
    }

    pub fn expect_read_only(project: &mut MockAPI, read_only: Rc<Cell<bool>>) {
        project.expect_read_only().returning_st(move || read_only.get());
    }
}
