//! Utilities for testing executor-dependent code.

use crate::prelude::*;

use futures::executor;
use crate::executor::global::set_spawner;
use crate::executor::global::spawn;



/// A fixture for tests which makes able to run part of tests as asynchronous tasks in
/// LocalPoolExecutor. All spawned task will be run before dropping this structure - if some
/// task will be blocked, panic will be raised.
#[derive(Debug)]
pub struct TestWithLocalPoolExecutor {
    executor           : executor::LocalPool,
    running_task_count : Rc<Cell<usize>>,
}

impl TestWithLocalPoolExecutor {
    /// Set up the test fixture.
    pub fn set_up() -> Self {
        let executor           = executor::LocalPool::new();
        let running_task_count = Rc::new(Cell::new(0));

        set_spawner(executor.spawner());
        Self {executor,running_task_count}
    }

    /// Spawn new task in executor.
    pub fn run_task<Task>(&mut self, task: Task)
    where Task : Future<Output=()> + 'static {
        self.running_task_count.set(self.running_task_count.get() + 1);
        let running_tasks_clone = self.running_task_count.clone_ref();
        spawn(async move {
            task.await;
            running_tasks_clone.set(running_tasks_clone.get() - 1);
        });
    }

    /// Run all tasks until executor is stalled, and run callback then.
    ///
    /// This callback may for instance do some operations on mocks which unblocks task spawned
    /// in executor.
    pub fn when_stalled<Callback>(&mut self, callback:Callback)
    where Callback : FnOnce() {
        self.executor.run_until_stalled();
        if self.running_task_count.get() > 0 {
            callback();
        }
    }

    /// Run all tasks until executor is stalled, and spawn a new task then.
    ///
    /// This structure is useful to ensure, that some task will be in progress before another task
    /// will be spawned, so we can test more specific asynchronous scenarios.
    pub fn when_stalled_run_task<Task>(&mut self, task : Task)
    where Task : Future<Output=()> + 'static {
        self.executor.run_until_stalled();
        if self.running_task_count.get() > 0 {
            self.run_task(task);
        }
    }
}

impl Drop for TestWithLocalPoolExecutor {
    fn drop(&mut self) {
        // We should be able to finish test.
        self.executor.run_until_stalled();
        assert_eq!(0,self.running_task_count.get(),"Executor dropped before tasks are complete!");
    }
}
