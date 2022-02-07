use crate::prelude::*;

use crate::executor::test_utils::TestWithLocalPoolExecutor;

use json_rpc::test_util::transport::mock::MockTransport;
use serde::Serialize;


#[derive(Debug)]
pub struct TestWithMockedTransport {
    with_executor_fixture: TestWithLocalPoolExecutor,
    transport:             MockTransport,
    next_response_id:      json_rpc::handler::IdGenerator,
}

impl Deref for TestWithMockedTransport {
    type Target = TestWithLocalPoolExecutor;
    fn deref(&self) -> &Self::Target {
        &self.with_executor_fixture
    }
}

impl DerefMut for TestWithMockedTransport {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.with_executor_fixture
    }
}

impl TestWithMockedTransport {
    pub fn set_up(transport: &MockTransport) -> Self {
        Self {
            with_executor_fixture: TestWithLocalPoolExecutor::set_up(),
            transport:             transport.clone_ref(),
            next_response_id:      json_rpc::handler::IdGenerator::new(),
        }
    }

    pub fn run_test<TestBody>(&mut self, test: TestBody)
    where TestBody: Future<Output = ()> + 'static {
        self.with_executor_fixture.run_task(test);
    }

    pub fn when_stalled_send_response(&mut self, result: impl Serialize) {
        let id = self.next_response_id.generate();
        let message = json_rpc::messages::Message::new_success(id, result);
        self.when_stalled_send_message(message);
    }

    pub fn when_stalled_send_error(&mut self, code: i64, message: impl Into<String>) {
        let id = self.next_response_id.generate();
        let message = json_rpc::messages::Message::<()>::new_error(id, code, message.into(), None);
        self.when_stalled_send_message(message);
    }

    pub fn when_stalled_send_text_message(&mut self, message: impl Into<String>) {
        let mut transport = self.transport.clone_ref();
        self.with_executor_fixture.when_stalled(move || transport.mock_peer_text_message(message));
    }

    pub fn when_stalled_send_message(&mut self, message: impl Serialize) {
        let text = serde_json::to_string(&message).unwrap();
        self.when_stalled_send_text_message(text);
    }
}
