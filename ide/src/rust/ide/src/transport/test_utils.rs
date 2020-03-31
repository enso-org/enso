use crate::prelude::*;

use crate::executor::test_utils::TestWithLocalPoolExecutor;

use json_rpc::test_util::transport::mock::MockTransport;
use serde::Serialize;


#[derive(Debug)]
pub struct TestWithMockedTransport {
    with_executor_fixture : TestWithLocalPoolExecutor,
    transport             : MockTransport,
    next_response_id      : json_rpc::handler::IdGenerator,
}

impl TestWithMockedTransport {
    pub fn set_up(transport:&MockTransport) -> Self {
        Self {
            with_executor_fixture : TestWithLocalPoolExecutor::set_up(),
            transport             : transport.clone_ref(),
            next_response_id      : json_rpc::handler::IdGenerator::new(),
        }
    }

    pub fn run_test<TestBody>(&mut self, test:TestBody)
    where TestBody : Future<Output=()> + 'static {
        self.with_executor_fixture.run_task(test);
    }

    pub fn when_stalled_send_response(&mut self, result:impl Serialize) {
        let mut transport = self.transport.clone_ref();
        let id            = self.next_response_id.generate();
        let result        = json_rpc::messages::Message::new_success(id,result);
        self.with_executor_fixture.when_stalled(move ||
            transport.mock_peer_message(result)
        );
    }
}
