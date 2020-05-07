//! Module defining registry of module's states.

use crate::prelude::*;

use crate::notification::Publisher;

use flo_stream::Subscriber;
use flo_stream::MessagePublisher;



// =============
// === Error ===
// =============

/// An error indicating that another task of loading module results in error.
///
/// Unfortunately we cannot pass the original error, because it might not implement traits requred
/// for sending through `Publisher`.
#[derive(Clone,Debug,Fail)]
#[fail(display="Error while loading module")]
struct LoadingError {}



// =============
// === Entry ===
// =============

/// Register entry. It may be handle to the loaded State, or information that module is already
/// loaded by another task with endpoint for receiving notifications about load finish.
#[derive(Clone)]
enum Entry<Handle> {
    Loaded(Handle),
    Loading(Subscriber<LoadedNotification>),
}

/// Notification that module state was loaded.
type LoadedNotification = Result<(), LoadingError>;
type StrongEntry = Entry<Rc  <model::Module>>;
type WeakEntry   = Entry<Weak<model::Module>>;

impl WeakElement for WeakEntry {
    type Strong = StrongEntry;

    fn new(view: &Self::Strong) -> Self {
        match view {
            Entry::Loaded(handle) => Entry::Loaded(Rc::downgrade(handle)),
            Entry::Loading(sub)   => Entry::Loading(sub.resubscribe()),
        }
    }

    fn view(&self) -> Option<Self::Strong> {
        match self {
            Entry::Loaded(handle) => handle.upgrade().map(Entry::Loaded),
            Entry::Loading(sub)   => Some(Entry::Loading(sub.resubscribe()))
        }
    }
}

impl<Handle:CloneRef> CloneRef for Entry<Handle> {}
impl<Handle:Debug> Debug for Entry<Handle> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            Entry::Loaded(handle) => write!(f, "Entry::Loaded({:?})", handle),
            Entry::Loading(_)     => write!(f, "Entry::Loading"),
        }
    }
}


// ================
// === Registry ===
// ================

/// A path describing module location, used as key in Registry.
pub type ModulePath = controller::module::Path;

/// Modules' States Registry
///
/// This is a centralized registry for getting Module's states handles, which ensures that each
/// module loaded will have only one representation in memory, what is essential to keep all
/// components synchronized.
///
/// It covers the case with modules loaded asynchronously. When two task will request for module
/// at the same time, only one will start the actual loading, and the second will wait for the
/// first. So the module will be actually loaded only once.
///
/// Of course the registry is meant to being shared between many tasks, so it implements the
/// internal mutability pattern.
#[derive(Debug,Default)]
pub struct Registry {
    registry: RefCell<WeakValueHashMap<ModulePath,WeakEntry>>
}

impl Registry {

    /// Get module or load.
    ///
    /// This functions return handle to module under `path` if it's already loaded. If it is in
    /// loading state (because another task is loading it asynchronously), it will be wait for that
    /// loading to finish. If it's not present nor loaded, this function will load the module by
    /// awaiting `loader` parameter. There is guarantee, that loader will be not polled in any other
    /// case.
    pub async fn get_or_load<F>
    (&self, path:ModulePath, loader:F) -> FallibleResult<Rc<model::Module>>
    where F : Future<Output=FallibleResult<Rc<model::Module>>> {
        match self.get(&path).await? {
            Some(state) => Ok(state),
            None        => Ok(self.load(path,loader).await?)
        }
    }

    async fn get(&self, path:&ModulePath) -> Result<Option<Rc<model::Module>>,LoadingError> {
        loop {
            let entry = self.registry.borrow_mut().get(&path);
            match entry {
                Some(Entry::Loaded(state)) => { break Ok(Some(state)); },
                Some(Entry::Loading(mut sub)) => {
                    // Wait for loading to be finished.
                    sub.next().await.unwrap()?;
                }
                None => { break Ok(None); }
            }
        }

    }

    async fn load<F,E>(&self, path:ModulePath, loader:F) -> Result<Rc<model::Module>,E>
    where F : Future<Output=Result<Rc<model::Module>,E>> {
        let mut publisher = Publisher::default();
        self.registry.borrow_mut().insert(path.clone(), Entry::Loading(publisher.subscribe()));

        let result = loader.await;
        with(self.registry.borrow_mut(), |mut registry| {
            match &result {
                Ok(state) => registry.insert(path, Entry::Loaded(state.clone_ref())),
                Err(_)    => registry.remove(&path),
            };
        });
        let message = match &result {
            Ok(_)  => Ok(()),
            Err(_) => Err(LoadingError{}),
        };
        publisher.publish(message).await;
        result
    }
}



// ============
// === Test ===
// ============

#[cfg(test)]
mod test {
    use super::*;

    use crate::executor::test_utils::TestWithLocalPoolExecutor;

    #[test]
    fn getting_module() {
        let mut test = TestWithLocalPoolExecutor::set_up();
        test.run_task(async move {
            let line     = ast::Ast::infix_var("a", "+", "b");
            let ast      = ast::Ast::one_line_module(line);
            let state    = Rc::new(model::Module::new(ast.try_into().unwrap(),default()));
            let registry = Rc::new(Registry::default());
            let expected = state.clone_ref();
            let path     = ModulePath::new(default(),&["test"]);

            let loader = async move { Ok(state) };
            let module = registry.get_or_load(path.clone(),loader).await.unwrap();
            assert!(Rc::ptr_eq(&expected,&module));

            let loader = async move { unreachable!("Should not call loader second time!") };
            let module = registry.get_or_load(path,loader).await.unwrap();
            assert!(Rc::ptr_eq(&expected,&module));
        });
    }

    #[test]
    fn getting_module_during_load() {
        let line      = ast::Ast::infix_var("a", "+", "b");
        let ast       = ast::Ast::one_line_module(line);
        let state1    = Rc::new(model::Module::new(ast.try_into().unwrap(),default()));
        let state2    = state1.clone_ref();
        let registry1 = Rc::new(Registry::default());
        let registry2 = registry1.clone_ref();
        let path1     = ModulePath::new(default(),&["test"]);
        let path2     = path1.clone();

        let (loaded_send, loaded_recv) = futures::channel::oneshot::channel::<()>();

        let mut test = TestWithLocalPoolExecutor::set_up();
        test.run_task(async move {
            let expected = state1.clone_ref();
            let loader = async move {
                loaded_recv.await.unwrap();
                Ok(state1)
            };
            let module = registry1.get_or_load(path1,loader).await.unwrap();
            assert!(Rc::ptr_eq(&expected,&module));
        });
        test.when_stalled_run_task(async move {
            let loader = async move { unreachable!("Should not call loader second time!"); };
            let module = registry2.get_or_load(path2,loader).await.unwrap();
            assert!(Rc::ptr_eq(&state2,&module));
        });
        test.when_stalled(move || {
            loaded_send.send(()).unwrap();
        });
    }
}
