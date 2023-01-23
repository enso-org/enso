//! A utility with a single input allowing emitting values of different types and multiple
//! outputs, one per type.

use crate::prelude::*;

use crate::AnyData;

use std::any::TypeId;



// ===========
// === Fan ===
// ===========

struct FanOutput {
    stream: AnyData,
    runner: Rc<dyn Fn(&AnyData, &AnyData)>,
}

impl Debug for FanOutput {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "FanOutput")
    }
}

/// A utility with a single input allowing emitting values of different types and multiple
/// outputs, one per type. See tests to learn how to use it.
#[allow(missing_docs)]
#[derive(Debug, Clone, CloneRef)]
pub struct Fan {
    pub source: crate::Any<AnyData>,
    network:    crate::WeakNetwork,
    map:        Map,
}

type Map = Rc<RefCell<HashMap<TypeId, FanOutput>>>;

impl Fan {
    /// Constructor.
    pub fn new(network: &crate::Network) -> Self {
        let map: Map = default();
        crate::extend! { network
            source <- any_mut::<AnyData>();
            eval source([map] (event) {
                let map_borrow = map.borrow();
                if let Some(fan_output) = map_borrow.get(&(*event.data).type_id()) {
                    let stream = fan_output.stream.clone_ref();
                    let runner = fan_output.runner.clone_ref();
                    // The map borrow must be dropped before runner is called, because a new fan
                    // output might be created during its execution. That acquires a mutable borrow
                    // of the map and would panic.
                    drop(map_borrow);
                    (runner)(&stream, event);
                }
            });
        }
        let network = network.downgrade();
        Self { source, network, map }
    }

    /// Get the output stream of the fan for the given type. See docs of [`Fan`] for an example.
    ///
    /// # Safety
    /// This implementation is safe because we remember the [`TypeId`] when erasing the type, and we
    /// query the right function based on the [`TypeId`] when emitting the value. When changing this
    /// implementation, please remember to test it rigorously.
    #[allow(unsafe_code)]
    pub fn output<T: crate::Data>(&self) -> crate::Stream<T> {
        if let Some(network) = self.network.upgrade() {
            let id = TypeId::of::<T>();
            let mut map = self.map.borrow_mut();
            let fan_output = map.entry(id).or_insert_with(|| {
                crate::extend! { network
                    output <- source::<T>();
                }
                let stream = AnyData::new(output);
                let runner = Rc::new(|stream: &AnyData, event: &AnyData| {
                    let stream = if cfg!(debug_assertions) {
                        stream.downcast_ref::<crate::Source<T>>().unwrap()
                    } else {
                        unsafe { stream.downcast_ref_unchecked::<crate::Source<T>>() }
                    };
                    let event = if cfg!(debug_assertions) {
                        event.downcast_ref::<T>().unwrap()
                    } else {
                        unsafe { event.downcast_ref_unchecked::<T>() }
                    };
                    stream.emit(event.clone());
                });
                FanOutput { stream, runner }
            });
            unsafe {
                fan_output.stream.downcast_ref_unchecked::<crate::Source<T>>().clone_ref().into()
            }
        } else {
            crate::Source::new().into()
        }
    }

    /// Emit a new event to the fan. You should prefer using the [`Fan::source`] when emitting
    /// events from an FRP network. See docs of [`Fan`] for an example.
    pub fn emit(&self, event: &AnyData) {
        self.source.emit(event.clone());
    }
}



// =============
// === Tests ===
// =============

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test1() {
        let network = crate::Network::new("network");
        let fan = Fan::new(&network);
        let out_usize = fan.output::<usize>();
        let out_string = fan.output::<String>();
        let last_usize: Rc<RefCell<usize>> = default();
        let last_string: Rc<RefCell<String>> = default();

        crate::extend! { network
            source_usize <- source::<usize>();
            source_string <- source::<String>();
            fan.source <+ source_usize.any_data();
            fan.source <+ source_string.any_data();
            eval out_usize ([last_usize](t) *last_usize.borrow_mut() = *t);
            eval out_string ([last_string](t) *last_string.borrow_mut() = t.clone());
        }

        assert_eq!(*last_usize.borrow(), 0);
        assert_eq!(*last_string.borrow(), "");

        source_usize.emit(1);
        assert_eq!(*last_usize.borrow(), 1);
        assert_eq!(*last_string.borrow(), "");

        source_string.emit("foo".to_string());
        assert_eq!(*last_usize.borrow(), 1);
        assert_eq!(*last_string.borrow(), "foo");

        source_usize.emit(2);
        assert_eq!(*last_usize.borrow(), 2);
        assert_eq!(*last_string.borrow(), "foo");

        source_string.emit("bar".to_string());
        assert_eq!(*last_usize.borrow(), 2);
        assert_eq!(*last_string.borrow(), "bar");
    }
}
