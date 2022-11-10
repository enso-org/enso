/// A utility with a single input allowing emitting values of different types and multiple
/// outputs, one per type.
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
/// outputs, one per type.
///
/// Consider the following example:
/// ```text
/// let fan = Fan::new(&network);
/// crate::extend! { network
///     source1 <- source::<usize>();
///     source2 <- source::<String>();
///     fan.source <+ source1.any_data(); // `any_data` hides the type
///     fan.source <+ source2.any_data(); // `any_data` hides the type
///     trace fan.output::<usize>();
///     trace fan.output::<String>();
/// }
/// ```
#[allow(missing_docs)]
#[derive(Debug)]
pub struct Fan {
    pub source: crate::Any<AnyData>,
    map:        RefCell<HashMap<TypeId, FanOutput>>,
}

impl Fan {
    /// Constructor.
    pub fn new(network: &crate::Network) -> Self {
        crate::extend! { network
            source <- any(...);
        }
        let map = default();
        Self { source, map }
    }

    /// Get the output stream of the fan for the given type. See docs of [`Fan`] for an example.
    pub fn output<T: crate::Data>(&self, network: &crate::Network) -> crate::Source<T> {
        let id = TypeId::of::<T>();
        let mut map = self.map.borrow_mut();
        let fan_output = map.entry(id).or_insert_with(|| {
            crate::extend! { network
                output <- source::<T>();
            }
            let stream = AnyData::new(output);
            let runner = Rc::new(|stream: &AnyData, event: &AnyData| {
                let stream = unsafe { stream.downcast_ref_unchecked::<crate::Source<T>>() };
                let event = unsafe { event.downcast_ref_unchecked::<T>() };
                stream.emit(event.clone());
            });
            FanOutput { stream, runner }
        });
        unsafe { fan_output.stream.downcast_ref_unchecked::<crate::Source<T>>().clone_ref() }
    }

    /// Emit a new event to the fan. You should prefer using the [`Fan::source`] when emitting
    /// events from an FRP network. See docs of [`Fan`] for an example.
    pub fn emit(&self, event: &AnyData) {
        if let Some(fan_output) = self.map.borrow_mut().get(&(*event.data).type_id()) {
            (fan_output.runner)(&fan_output.stream, event);
        }
    }
}
