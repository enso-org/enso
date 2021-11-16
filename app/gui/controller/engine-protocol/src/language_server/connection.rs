//! Module for utilities regarding establishing and storing the Language Server RPC connection.

use crate::prelude::*;

use crate::language_server::types::ContentRoot;
use crate::language_server::MockClient;
use crate::language_server::API;

use uuid::Uuid;



// ==============
// === Errors ===
// ==============

#[allow(missing_docs)]
#[derive(Fail, Debug)]
#[fail(display = "Failed to initialize language server RPC connection: {}.", _0)]
pub struct FailedToInitializeProtocol(failure::Error);

#[allow(missing_docs)]
#[derive(Fail, Clone, Copy, Debug)]
#[fail(display = "Language Server provided no content roots.")]
pub struct MissingContentRoots;



// ==================
// === Connection ===
// ==================

/// An established, initialized connection to language server's RPC endpoint.
#[derive(Derivative)]
#[derivative(Debug)]
pub struct Connection {
    /// The ID of the client.
    pub client_id: Uuid,
    /// LS client that has already initialized protocol.
    #[derivative(Debug = "ignore")]
    pub client:    Box<dyn API>,
    /// The Project content root, being an only obligatory content root received.
    project_root:  ContentRoot,
    /// Content roots obtained during initialization other than the `project_root`].
    content_roots: Vec<ContentRoot>,
}

impl Connection {
    /// Takes a client, generates ID for it and initializes the protocol.
    pub async fn new(client: impl API + 'static, client_id: Uuid) -> FallibleResult<Self> {
        let client = Box::new(client);
        let init_response = client.init_protocol_connection(&client_id).await;
        let init_response = init_response.map_err(|e| FailedToInitializeProtocol(e.into()))?;
        let mut content_roots = init_response.content_roots;
        let project_root = Self::extract_project_root(&mut content_roots)?;
        Ok(Connection { client_id, client, project_root, content_roots })
    }

    fn extract_project_root(content_roots: &mut Vec<ContentRoot>) -> FallibleResult<ContentRoot> {
        let opt_index =
            content_roots.iter().position(|cr| matches!(cr, ContentRoot::Project { .. }));
        let index = opt_index.ok_or(MissingContentRoots)?;
        Ok(content_roots.drain(index..=index).next().unwrap())
    }

    /// Creates a connection which wraps a mock client.
    pub fn new_mock(client: MockClient) -> Connection {
        Connection {
            client:        Box::new(client),
            client_id:     default(),
            project_root:  ContentRoot::Project { id: default() },
            content_roots: default(),
        }
    }

    /// Creates a Rc handle to a connection which wraps a mock client.
    pub fn new_mock_rc(client: MockClient) -> Rc<Connection> {
        Rc::new(Self::new_mock(client))
    }

    /// Returns the first content root.
    pub fn project_root(&self) -> &ContentRoot {
        &self.project_root
    }

    /// Lists all content roots for this LS connection.
    pub fn content_roots(&self) -> impl Iterator<Item = &ContentRoot> {
        std::iter::once(&self.project_root).chain(self.content_roots.iter())
    }
}

impl Deref for Connection {
    type Target = dyn API;
    fn deref(&self) -> &Self::Target {
        self.client.as_ref()
    }
}

impl DerefMut for Connection {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.client.deref_mut()
    }
}
