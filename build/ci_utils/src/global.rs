use crate::prelude::*;

use crate::future::try_join_all;
use crate::future::AsyncPolicy;

use indicatif::MultiProgress;
use indicatif::ProgressBar;
use indicatif::WeakProgressBar;
use std::sync::LazyLock;
use std::sync::Mutex;
use std::time::Duration;
use tokio::task::JoinHandle;



/// Turns given text into a static string.
///
/// This can be useful for passing runtime-generated strings to APIs that expect static lifetime
/// texts, like the `clap` library.
///
/// This effectively leaks memory, though if the function is called multiple times with the same
/// argument, it will allocate only once.
pub fn store_static_text(text: impl AsRef<str>) -> &'static str {
    lazy_static! {
        pub static ref STRING_STORAGE: Mutex<HashSet<&'static str>> = default();
    }
    STRING_STORAGE.lock().unwrap().get_or_insert_with(text.as_ref(), |text| Box::leak(text.into()))
}

const REFRESHES_PER_SECOND: u32 = 100;

#[derive(derivative::Derivative)]
#[derivative(Debug)]
struct GlobalState {
    /// A globally-shared reference to the multi-progress bar.
    ///
    /// All progress bars must be added to this multi-progress bar. This ensures that the progress
    /// bars are displayed in a way that does not interfere with tracing log output.
    mp:            MultiProgress,
    #[derivative(Debug = "ignore")]
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
                std::thread::sleep(Duration::SECOND / REFRESHES_PER_SECOND);
            }),
            ongoing_tasks: default(),
        }
    }
}

static GLOBAL: LazyLock<Mutex<GlobalState>> = LazyLock::new(default);

/// Returns a reference to the global multi-progress bar.
pub fn multi_progress_bar() -> MultiProgress {
    GLOBAL.lock().unwrap().mp.clone()
}

pub fn progress_bar(f: impl FnOnce() -> ProgressBar) -> ProgressBar {
    let ret = f();
    let ret = GLOBAL.lock().unwrap().mp.add(ret);
    GLOBAL.lock().unwrap().bars.push(ret.downgrade());
    ret
}

pub fn new_spinner(message: impl Into<Cow<'static, str>>) -> ProgressBar {
    let ret = progress_bar(indicatif::ProgressBar::new_spinner);
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
        try_join_all(tasks, AsyncPolicy::FutureParallelism).await?;
    }
    debug!("All pending tasks have been completed.");
    Ok(())
}


//
// pub fn complete_tasks(rt: &Runtime) -> Result {
//     info!("Waiting for remaining tasks to complete.");
//     while let tasks = std::mem::replace(&mut GLOBAL.lock().unwrap().ongoing_tasks, default()) &&
// !tasks.is_empty() {         let tasks = try_join_all(tasks, AsyncPolicy::FutureParallelism);
//          rt.block_on(tasks)?;
//     }
//     Ok(())
// }
