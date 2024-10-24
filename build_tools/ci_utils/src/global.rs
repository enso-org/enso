use crate::prelude::*;

use indicatif::MultiProgress;
use indicatif::ProgressBar;
use indicatif::WeakProgressBar;
use std::sync::LazyLock;
use std::sync::Mutex;
use std::time::Duration;
use tokio::task::JoinHandle;



const REFRESHES_PER_SECOND: u32 = 100;

#[derive_where(Debug)]
struct GlobalState {
    /// A globally-shared reference to the multi-progress bar.
    ///
    /// All progress bars must be added to this multi-progress bar. This ensures that the progress
    /// bars are displayed in a way that does not interfere with tracing log output.
    mp:            MultiProgress,
    #[derive_where(skip)]
    bars:          Vec<WeakProgressBar>,
    _tick_thread:  std::thread::JoinHandle<()>,
    ongoing_tasks: Vec<JoinHandle<Result>>,
}

impl GlobalState {
    pub fn tick(&mut self) {
        let mut to_remove = vec![];
        for (index, bar) in self.bars.iter().enumerate() {
            if let Some(bar) = bar.upgrade() {
                bar.tick()
            } else {
                to_remove.push(index)
            }
        }

        for to_remove in to_remove.iter().rev() {
            self.bars.remove(*to_remove);
        }
    }
}

impl Default for GlobalState {
    fn default() -> Self {
        GlobalState {
            mp:            MultiProgress::new(),
            bars:          default(),
            _tick_thread:  std::thread::spawn(|| {
                GLOBAL.lock().unwrap().tick();
                std::thread::sleep(Duration::from_secs(1) / REFRESHES_PER_SECOND);
            }),
            ongoing_tasks: default(),
        }
    }
}

static GLOBAL: LazyLock<Mutex<GlobalState>> = LazyLock::new(default);

/// Suspends (i.e., hides) the global [`MultiProgress`] bar, executes the given closure, then
/// resumes (i.e., shows) the [`MultiProgress`] bar.
///
/// The [`MultiProgress`] bar must be suspended before logs are written or flushed to the console.
/// If logs are written or flushed without suspending the [`MultiProgress`] bar, they will clobber
/// the [`MultiProgress`] bar.
///
/// The reason this function is provided rather than letting callers clone the [`MultiProgress`] bar
/// and suspend it themselves is to avoid deadlocks. The [`MultiProgress`] bar contains an
/// [`RwLock`] internally. The [`GLOBAL`] state owns the [`MultiProgress`] bar and is itself wrapped
/// in a [`Mutex`]. This means that if [`MultiProgress`] bar is cloned, a data race can happen,
/// since there are two locks, one of which nests the other.
///
/// [`MultiProgress`]: indicatif::MultiProgress
/// [`GLOBAL`]: GLOBAL
/// [`RwLock`]: std::sync::RwLock
/// [`Mutex`]: std::sync::Mutex
pub fn with_suspend_multi_progress_bar<T>(f: impl FnOnce() -> T) -> T {
    GLOBAL.lock().unwrap().mp.suspend(f)
}

pub fn progress_bar(f: impl FnOnce() -> ProgressBar) -> ProgressBar {
    let ret = f();
    let ret = GLOBAL.lock().unwrap().mp.add(ret);
    GLOBAL.lock().unwrap().bars.push(ret.downgrade());
    ret
}

pub fn new_spinner(message: impl Into<Cow<'static, str>>) -> ProgressBar {
    let ret = progress_bar(ProgressBar::new_spinner);
    ret.set_message(message);
    ret
}

pub fn println(msg: impl AsRef<str>) {
    if let Ok(state) = GLOBAL.lock() {
        if !indicatif::ProgressDrawTarget::stderr().is_hidden() {
            let _ = state.mp.println(msg);
            return;
        }
    };
    println!("{}", msg.as_ref());
}

pub fn spawn(name: impl AsRef<str>, f: impl Future<Output = Result> + Send + 'static) {
    // info!("Spawning a new global task named '{}'.", name.as_ref());
    let join_handle = tokio::task::spawn(f.instrument(info_span!("task", name = name.as_ref())));
    // let join_handle = tokio::task::Builder::new().name(name.as_ref()).spawn(f);
    GLOBAL.lock().unwrap().ongoing_tasks.push(join_handle);
}

pub fn take_ongoing_tasks() -> Vec<JoinHandle<Result>> {
    std::mem::take(&mut GLOBAL.lock().unwrap().ongoing_tasks)
}

pub async fn complete_tasks() -> Result {
    debug!("Waiting for remaining tasks to complete.");
    loop {
        let tasks = take_ongoing_tasks();
        if tasks.is_empty() {
            break;
        }
        info!("Found {} tasks to wait upon.", tasks.len());
        futures::future::try_join_all(tasks).await?;
    }
    debug!("All pending tasks have been completed.");
    Ok(())
}
