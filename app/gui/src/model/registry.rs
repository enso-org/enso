//! Module defining registry of models which are loading asynchronously.

use crate::prelude::*;

use flo_stream::Subscriber;
use notification::Publisher;



// =============
// === Error ===
// =============

/// An error indicating that another task of loading item results in error.
///
/// Unfortunately we cannot pass the original error, because it might not implement traits requred
/// for sending through `Publisher`.
#[derive(Clone, Debug, Fail)]
#[fail(display = "Error while loading item")]
struct LoadingError {}



// =============
// === Entry ===
// =============

/// Register entry. It may be handle to the loaded State, or information that the item is currently
/// loading by another task with endpoint for receiving notifications about loading finish.
#[derive(Clone)]
enum Entry<Handle> {
    Loaded(Handle),
    Loading(Subscriber<LoadedNotification>),
}

/// Notification that item was loaded.
type LoadedNotification = Result<(), LoadingError>;

type StrongEntry<T> = Entry<Rc<T>>;
type WeakEntry<T> = Entry<Weak<T>>;

impl<T> WeakElement for WeakEntry<T> {
    type Strong = StrongEntry<T>;

    fn new(view: &Self::Strong) -> Self {
        match view {
            Entry::Loaded(handle) => Entry::Loaded(Rc::downgrade(handle)),
            Entry::Loading(sub) => Entry::Loading(sub.resubscribe()),
        }
    }

    fn view(&self) -> Option<Self::Strong> {
        match self {
            Entry::Loaded(handle) => handle.upgrade().map(Entry::Loaded),
            Entry::Loading(sub) => Some(Entry::Loading(sub.resubscribe())),
        }
    }
}

impl<Handle: Clone + CloneRef> CloneRef for Entry<Handle> {
    fn clone_ref(&self) -> Self {
        self.clone()
    }
}

impl<Handle: Debug> Debug for Entry<Handle> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            Entry::Loaded(handle) => write!(f, "Entry::Loaded({handle:?})"),
            Entry::Loading(_) => write!(f, "Entry::Loading"),
        }
    }
}


// ================
// === Registry ===
// ================

/// Model's Registry
///
/// This is a centralized registry of asynchronously loaded model instances. When two task will
/// request for item under same key at the same time, only one will start the actual loading, and
/// the second will wait for the first.
///
/// Of course the registry is meant to being shared between many places, so it implements the
/// internal mutability pattern.
#[derive(Debug, Derivative)]
#[derivative(Default(bound = ""))]
pub struct Registry<K, V>
where K: Eq + Hash {
    registry: RefCell<WeakValueHashMap<K, WeakEntry<V>>>,
}

impl<K, V> Registry<K, V>
where K: Clone + Eq + Hash
{
    /// Get item under the key, or load it.
    ///
    /// This functions return handle to item under `ket` if it's already loaded. If it is in
    /// loading state (because another task is loading it asynchronously), it will be wait for that
    /// loading to finish. If it's not present nor loaded, this function will load item by
    /// awaiting `loader` parameter. It is guaranteed, that loader will be not polled in any other
    /// case.
    pub async fn get_or_load<F>(&self, key: K, loader: F) -> FallibleResult<Rc<V>>
    where F: Future<Output = FallibleResult<Rc<V>>> {
        match self.get(&key).await? {
            Some(state) => Ok(state),
            None => Ok(self.load(key, loader).await?),
        }
    }

    /// Get item under the key.
    ///
    /// This functions return handle to item under `key` if it's already loaded. If it is in
    /// loading state (because another task is loading it asynchronously), it will be wait for that
    /// loading to finish.
    pub async fn get(&self, key: &K) -> FallibleResult<Option<Rc<V>>> {
        loop {
            let entry = self.registry.borrow_mut().get(key);
            match entry {
                Some(Entry::Loaded(state)) => {
                    break Ok(Some(state));
                }
                Some(Entry::Loading(mut sub)) => {
                    // Wait for loading to be finished.
                    sub.next().await.unwrap()?;
                }
                None => {
                    break Ok(None);
                }
            }
        }
    }

    async fn load<F, E>(&self, key: K, loader: F) -> Result<Rc<V>, E>
    where F: Future<Output = Result<Rc<V>, E>> {
        let publisher = Publisher::default();
        self.registry.borrow_mut().insert(key.clone(), Entry::Loading(publisher.subscribe()));

        let result = loader.await;
        with(self.registry.borrow_mut(), |mut registry| {
            match &result {
                Ok(state) => registry.insert(key, Entry::Loaded(state.clone_ref())),
                Err(_) => registry.remove(&key),
            };
        });
        let message = match &result {
            Ok(_) => Ok(()),
            Err(_) => Err(LoadingError {}),
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

    type ModulePath = model::module::Path;
    type Registry = super::Registry<ModulePath, model::module::Plain>;

    #[test]
    fn getting_module() {
        let mut test = TestWithLocalPoolExecutor::set_up();
        test.run_task(async move {
            let line = ast::Ast::infix_var("a", "+", "b");
            let ast = ast::Ast::one_line_module(line).try_into().unwrap();
            let path = ModulePath::from_mock_module_name("Test");
            let urm = default();
            let state = Rc::new(model::module::Plain::new(path.clone(), ast, default(), urm));
            let registry = Registry::default();
            let expected = state.clone_ref();

            let loader = async move { Ok(state) };
            let module = registry.get_or_load(path.clone(), loader).await.unwrap();
            assert!(Rc::ptr_eq(&expected, &module));

            let loader = async move { unreachable!("Should not call loader second time!") };
            let module = registry.get_or_load(path, loader).await.unwrap();
            assert!(Rc::ptr_eq(&expected, &module));
        });
    }

    #[test]
    fn getting_module_during_load() {
        let line = ast::Ast::infix_var("a", "+", "b");
        let ast = ast::Ast::one_line_module(line).try_into().unwrap();
        let path1 = ModulePath::from_mock_module_name("Test");
        let path2 = path1.clone();
        let urm = default();
        let state1 = Rc::new(model::module::Plain::new(path1.clone_ref(), ast, default(), urm));
        let state2 = state1.clone_ref();
        let registry1 = Rc::new(Registry::default());
        let registry2 = registry1.clone_ref();

        let (loaded_send, loaded_recv) = futures::channel::oneshot::channel::<()>();

        let mut test = TestWithLocalPoolExecutor::set_up();
        test.run_task(async move {
            let expected = state1.clone_ref();
            let loader = async move {
                loaded_recv.await.unwrap();
                Ok(state1)
            };
            let module = registry1.get_or_load(path1, loader).await.unwrap();
            assert!(Rc::ptr_eq(&expected, &module));
        });
        test.when_stalled_run_task(async move {
            let loader = async move {
                unreachable!("Should not call loader second time!");
            };
            let module = registry2.get_or_load(path2, loader).await.unwrap();
            assert!(Rc::ptr_eq(&state2, &module));
        });
        test.when_stalled(move || {
            loaded_send.send(()).unwrap();
        });
    }
}
