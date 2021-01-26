//! Module for utilities regarding establishing and storing the Language Server RPC connection.

use crate::prelude::*;

use crate::language_server::MockClient;
use crate::language_server::API;

use uuid::Uuid;
use utils::fail::FallibleResult;



// ==============
// === Errors ===
// ==============

#[allow(missing_docs)]
#[derive(Fail,Debug)]
#[fail(display="Failed to initialize language server RPC connection: {}.",_0)]
pub struct FailedToInitializeProtocol(failure::Error);

#[allow(missing_docs)]
#[derive(Fail,Clone,Copy,Debug)]
#[fail(display="Language Server provided no content roots.")]
pub struct MissingContentRoots;



// ==================
// === Connection ===
// ==================

/// An established, initialized connection to language server's RPC endpoint.
#[derive(Derivative)]
#[derivative(Debug)]
pub struct Connection {
    /// The ID of the client.
    pub client_id:Uuid,
    /// LS client that has already initialized protocol.
    #[derivative(Debug="ignore")]
    pub client:Box<dyn API>,
    /// Content roots obtained during initialization. Guaranteed to be non-empty.
    content_roots:Vec<Uuid>,
}

impl Connection {
    /// Takes a client, generates ID for it and initializes the protocol.
    pub async fn new(client:impl API + 'static, client_id:Uuid) -> FallibleResult<Self> {
        let client        = Box::new(client);
        let init_response = client.init_protocol_connection(&client_id).await;
        let init_response = init_response.map_err(|e| FailedToInitializeProtocol(e.into()))?;
        let content_roots = init_response.content_roots;
        if content_roots.is_empty() {
            Err(MissingContentRoots.into())
        } else {
            Ok(Connection {client_id,client,content_roots})
        }
    }

    /// Creates a connection which wraps a mock client.
    pub fn new_mock(client:MockClient) -> Connection {
        Connection {
            client        : Box::new(client),
            client_id     : default(),
            content_roots : vec![default()],
        }
    }

    /// Creates a Rc handle to a connection which wraps a mock client.
    pub fn new_mock_rc(client:MockClient) -> Rc<Connection> {
        Rc::new(Self::new_mock(client))
    }

    /// Returns the first content root.
    pub fn content_root(&self) -> Uuid {
        // Guaranteed to be non-empty thanks to check in `new` and implementation of `new_mock`.
        self.content_roots[0]
    }

    /// Lists all content roots for this LS connection.
    pub fn content_roots(&self) -> &Vec<Uuid> {
        &self.content_roots
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
