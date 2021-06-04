#![allow(clippy::ptr_arg)] // workaround for https://github.com/asomers/mockall/issues/58

//! Project controller.
//!
//! Responsible for owning any remote connection clients, and providing controllers for specific
//! files and modules. Expected to live as long as the project remains open in the IDE.
pub mod synchronized;

use crate::prelude::*;

use enso_protocol::binary;
use enso_protocol::language_server;
use flo_stream::Subscriber;
use mockall::automock;
use parser::Parser;
use uuid::Uuid;



// =============
// === Model ===
// =============

/// The API of the Project Model.
#[automock]
pub trait API:Debug {
    /// Project's name
    // TODO [mwu] This should return Rc<ReferentName>.
    fn name(&self) -> ImString;

    /// Get Language Server JSON-RPC Connection for this project.
    fn json_rpc(&self) -> Rc<language_server::Connection>;

    /// Get Language Server binary Connection for this project.
    fn binary_rpc(&self) -> Rc<binary::Connection>;

    /// Get the engine's version of the project.
    fn engine_version(&self) -> &semver::Version;

    /// Get the instance of parser that is set up for this project.
    fn parser(&self) -> Parser;

    /// Get the visualization controller.
    fn visualization(&self) -> &controller::Visualization;

    /// Get the suggestions database.
    fn suggestion_db(&self) -> Rc<model::SuggestionDatabase>;

    /// Returns a model of module opened from file.
    #[allow(clippy::needless_lifetimes)] // Note: Needless lifetimes
    fn module<'a>
    (&'a self, path:crate::model::module::Path) -> BoxFuture<'a,FallibleResult<model::Module>>;

    /// Creates a new execution context with given definition as a root; and registers the context
    /// for receiving update.
    #[allow(clippy::needless_lifetimes)] // Note: Needless lifetimes
    fn create_execution_context<'a>
    (&'a self, root_definition:language_server::MethodPointer)
    -> BoxFuture<'a,FallibleResult<model::ExecutionContext>>;

    /// Set a new project name.
    #[allow(clippy::needless_lifetimes)] // Note: Needless lifetimes
    fn rename_project<'a>(&'a self, name:String) -> BoxFuture<'a,FallibleResult<()>>;

    /// Returns the primary content root id for this project.
    fn content_root_id(&self) -> Uuid {
        self.json_rpc().content_root()
    }

    /// Generates full module's qualified name that includes the leading project name segment.
    fn qualified_module_name
    (&self, path:&model::module::Path) -> crate::model::module::QualifiedName {
        path.qualified_module_name(self.name().deref())
    }

    /// Get qualified name of the project's `Main` module.
    ///
    /// This module is special, as it needs to be referred by the project name itself.
    fn main_module(&self) -> FallibleResult<model::module::QualifiedName> {
        let main = std::iter::once(controller::project::INITIAL_MODULE_NAME);
        model::module::QualifiedName::from_segments(self.name(),main)

        // TODO [mwu] The code below likely should be preferred but does not work
        //            because language server does not support using project name
        //            for project's main module in some contexts.
        //            This is tracked by: https://github.com/enso-org/enso/issues/1543
        // use model::module::QualifiedName;
        // ReferentName::try_from(self.name().as_str())
        //     .map(QualifiedName::new_main)
        //     .map_err(Into::into)
    }

    /// Subscribe for notifications about project-level events.
    fn subscribe(&self) -> Subscriber<Notification>;
}

// Note: Needless lifetimes
// ~~~~~~~~~~~~~~~~~~~~~~~~
// Clippy complains about using explicit lifetimes, however the suggested change breaks compilation.
// This is because mockall library generates a `MockAPI` struct based on this code.
// However, so far, I (mwu) have failed to reduce to self-contained repro that could be presented
// to mockall library authors. Further effort needed.

impl Debug for MockAPI {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f,"Mocked Project Model")
    }
}

/// The general, shared Project Model handle.
pub type Project      = Rc<dyn API>;
/// Project Model which synchronizes all changes with Language Server.
pub type Synchronized = synchronized::Project;



// ====================
// === Notification ===
// ====================

/// Notification emitted by the project model.
#[derive(Clone,Copy,Debug,PartialEq)]
pub enum Notification {
    /// One of the backend connections has been lost.
    ConnectionLost(BackendConnection)
}

/// Denotes one of backend connections used by a project.
#[derive(Clone,Copy,Debug,PartialEq)]
pub enum BackendConnection {
    /// The text connection used to transfer JSON messages.
    LanguageServerJson,
    /// The binary conneection used to transfer FlatBuffers messages.
    LanguageServerBinary,
}



// ============
// === Test ===
// ============

#[cfg(test)]
pub mod test {
    use super::*;

    use futures::future::ready;

    /// Sets up parser expectation on the mock project.
    pub fn expect_parser(project:&mut MockAPI, parser:&Parser) {
        let parser = parser.clone_ref();
        project.expect_parser().returning_st(move || parser.clone_ref());
    }

    /// Sets up module expectation on the mock project, returning a give module.
    pub fn expect_module(project:&mut MockAPI, module:model::Module) {
        let module_path = module.path().clone_ref();
        project.expect_module()
            .withf_st    (move |path| path == &module_path)
            .returning_st(move |_path| ready(Ok(module.clone_ref())).boxed_local());
    }

    /// Sets up module expectation on the mock project, returning a give module.
    pub fn expect_execution_ctx(project:&mut MockAPI, ctx:model::ExecutionContext) {
        let ctx2 = ctx.clone_ref();
        project.expect_create_execution_context()
            .withf_st    (move |root_definition| root_definition == &ctx.current_method())
            .returning_st(move |_root_definition| ready(Ok(ctx2.clone_ref())).boxed_local());
    }

    /// Sets up module expectation on the mock project, returning a give module.
    pub fn expect_root_id(project:&mut MockAPI, root_id:Uuid) {
        project.expect_content_root_id().return_const(root_id);
    }

    /// Sets up module expectation on the mock project, returning a given module.
    pub fn expect_suggestion_db(project:&mut MockAPI, suggestion_db:Rc<model::SuggestionDatabase>) {
        project.expect_suggestion_db().returning_st(move || suggestion_db.clone_ref());
    }

    /// Sets up module expectation on the mock project, returning a give module.
    pub fn expect_json_rpc(project:&mut MockAPI, json_rpc:Rc<language_server::Connection>) {
        project.expect_json_rpc().returning_st(move || json_rpc.clone_ref());
    }

    /// Sets up module expectation on the mock project, returning a give module.
    pub fn expect_name(project:&mut MockAPI, name:impl Into<String>) {
        let name = ImString::new(name);
        project.expect_name().returning_st(move || name.clone_ref());
    }
}
