//! Module for utilities regarding establishing and storing the Language Server RPC connection.

use crate::prelude::*;

use crate::binary::MockClient;
use crate::binary::API;



// ==============
// === Errors ===
// ==============

#[allow(missing_docs)]
#[derive(Fail, Debug)]
#[fail(display = "Failed to initialize language server binary connection: {}", _0)]
pub struct FailedToInitializeProtocol(failure::Error);



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
}

impl Connection {
    /// Takes a client, generates ID for it and initializes the protocol.
    pub async fn new(client: impl API + 'static, client_id: Uuid) -> FallibleResult<Self> {
        let init_response = client.init(client_id).await;
        init_response.map_err(FailedToInitializeProtocol)?;
        let client = Box::new(client);
        Ok(Connection { client_id, client })
    }

    /// Creates a connection which wraps a mock client.
    pub fn new_mock(client: MockClient) -> Connection {
        Connection { client: Box::new(client), client_id: default() }
    }

    /// Creates a Rc handle to a connection which wraps a mock client.
    pub fn new_mock_rc(client: MockClient) -> Rc<Connection> {
        Rc::new(Self::new_mock(client))
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



// =============
// === Tests ===
// =============

#[cfg(test)]
mod tests {
    use super::*;

    use crate::binary::MockClient;
    use futures::task::LocalSpawnExt;
    use json_rpc::error::RpcError;
    use mockall::predicate::*;

    fn ready<T: 'static>(t: impl Into<T>) -> StaticBoxFuture<T> {
        futures::future::ready(t.into()).boxed_local()
    }

    #[test]
    fn test_connection() {
        let case = async {
            let client_id = Uuid::from_u128(159);
            let mock_returning = |ret: FallibleResult| {
                let mut mock = MockClient::new();
                mock.expect_init().with(eq(client_id)).times(1).return_once(|_| ready(ret));
                mock
            };

            let ok = Ok(());
            assert!(Connection::new(mock_returning(ok), client_id).await.is_ok());

            let err = Err(RpcError::new_remote_error(0, "ErrorMessage").into());
            assert!(Connection::new(mock_returning(err), client_id).await.is_err());
        };
        let mut pool = futures::executor::LocalPool::new();
        pool.spawner().spawn_local(case).unwrap();
        pool.run();
    }
}
