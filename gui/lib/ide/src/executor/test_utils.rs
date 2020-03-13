
use crate::prelude::*;

use futures::executor;
use crate::executor::global::set_spawner;
use crate::executor::global::spawn;



#[derive(Debug)]
pub struct TestWithLocalPoolExecutor {
    executor    : executor::LocalPool,
    is_finished : Rc<Cell<bool>>,
}

impl TestWithLocalPoolExecutor {
    pub fn set_up() -> Self {
        let executor    = executor::LocalPool::new();
        let is_finished = Rc::new(Cell::new(false));

        set_spawner(executor.spawner());
        Self {executor,is_finished}
    }

    pub fn run_test<Test>(&mut self, test:Test)
    where Test : Future<Output=()> + 'static {
        let is_finished_clone = self.is_finished.clone_ref();
        spawn(async move {
            test.await;
            is_finished_clone.set(true);
        });
    }

    pub fn when_stalled<Callback>(&mut self, callback:Callback)
    where Callback : FnOnce() {
        self.executor.run_until_stalled();
        if !self.is_finished.get() {
            callback();
        }
    }
}

impl Drop for TestWithLocalPoolExecutor {
    fn drop(&mut self) {
        // We should be able to finish test.
        self.executor.run_until_stalled();
        assert!(self.is_finished.get());
    }
}
