//! [`ProfilingStatuses`] can be used to collect the profiling statuses of all nodes in a graph. It
//! exposes their minimum and maximum running times through its FRP endpoints. The structure needs
//! to be updated whenever a node is added or deleted or changes its profiling status.

use crate::prelude::*;

use crate::node;
use crate::NodeId;

use bimap::BiBTreeMap;
use enso_frp as frp;
use ordered_float::OrderedFloat;



// =====================
// === FRP Endpoints ===
// =====================

ensogl::define_endpoints! {
    Input {
        /// Informs the `Statuses` collection about the profiling status of a node.
        set(NodeId,node::profiling::Status),

        /// Removes a node's information from the collection.
        remove (NodeId)
    }
    Output {
        /// The minimum running time of any node in milliseconds. Is positive infinity if no status
        /// was registered.
        min_duration (f32),

        /// The maximum running time of any node in milliseconds. Is 0.0 if no status was
        /// registered.
        max_duration (f32),
    }
}



// ================
// === Statuses ===
// ================

/// Can be used to track the execution statuses of all nodes in the graph editor. Exposes the
/// minimum and maximum running time through FRP endpoints.
#[derive(Debug, Clone, CloneRef, Default)]
pub struct Statuses {
    frp:       Frp,
    durations: Rc<RefCell<BiBTreeMap<NodeId, OrderedFloat<f32>>>>,
}

impl Deref for Statuses {
    type Target = Frp;

    fn deref(&self) -> &Self::Target {
        &self.frp
    }
}

impl Statuses {
    /// Creates a new `Statuses` collection.
    pub fn new() -> Self {
        let frp = Frp::new();
        let durations = Rc::new(RefCell::new(BiBTreeMap::<NodeId, OrderedFloat<f32>>::new()));
        let network = &frp.network;

        frp::extend! { network
            min_and_max_from_set <- frp.set.map(f!([durations]((node,status)) {
                match status {
                    node::profiling::Status::Finished {duration} => {
                        durations.borrow_mut().insert(*node,OrderedFloat(*duration));
                    },
                    _ => {
                        durations.borrow_mut().remove_by_left(node);
                    },
                };
                Self::min_and_max(&durations.borrow())
            }));

            min_and_max_from_remove <- frp.remove.map(f!([durations](node) {
                durations.borrow_mut().remove_by_left(node);
                Self::min_and_max(&durations.borrow())
            }));

            min_and_max             <- any(&min_and_max_from_set,&min_and_max_from_remove);
            frp.source.min_duration <+ min_and_max._0().on_change();
            frp.source.max_duration <+ min_and_max._1().on_change();
        }

        frp.source.min_duration.emit(Self::min_and_max(durations.borrow().deref()).0);
        frp.source.max_duration.emit(Self::min_and_max(durations.borrow().deref()).1);

        Self { frp, durations }
    }

    fn min_and_max(durations: &BiBTreeMap<NodeId, OrderedFloat<f32>>) -> (f32, f32) {
        let mut durations = durations.right_values().copied();

        let min = durations.next().map(OrderedFloat::into_inner).unwrap_or(f32::INFINITY);
        let max = durations.last().map(OrderedFloat::into_inner).unwrap_or(0.0);
        (min, max)
    }
}
