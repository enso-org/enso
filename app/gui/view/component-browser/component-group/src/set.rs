//! Provides a multi component group wrapper, an abstraction used to propagate FRP events from
//! multiple component groups.
//!
//! The wrapper can work with all types of component groups using [`Group`] enum. Currently we
//! support one-column component groups and wide (multi-column) component groups.
//!
//! See [`Wrapper`] docs.

use ensogl::prelude::*;

use crate::entry;
use crate::wide;
use crate::View;

use enso_frp as frp;
use ensogl::data::OptVec;



// =============
// === Group ===
// =============

/// One of the possible component group types.
#[derive(Debug, Clone, CloneRef, From)]
#[allow(missing_docs)]
pub enum Group {
    /// A one-column component group with a header. See [`View`] docs.
    OneColumn(View),
    /// A multi-column component group without a header. See [`wide::View`] docs.
    Wide(wide::View),
}

impl Group {
    fn focus(&self) {
        match self {
            Group::OneColumn(group) => group.focus(),
            Group::Wide(group) => group.focus(),
        }
    }

    fn defocus(&self) {
        match self {
            Group::OneColumn(group) => group.defocus(),
            Group::Wide(group) => group.defocus(),
        }
    }

    fn is_mouse_over(&self) -> &frp::Sampler<bool> {
        match self {
            Group::OneColumn(group) => &group.is_mouse_over,
            Group::Wide(group) => &group.is_mouse_over,
        }
    }
}



/// ========================
/// === PropagatedEvents ===
/// ========================

/// Transform
/// ```ignore
/// propagate_frp!{ network, group, self,
///     (suggestion_accepted, move |e| (id, *e)),
///     (expression_accepted, move |e| (id, *e)),
/// }
/// ```
/// into
/// ```ignore
/// frp::extend! { network
///     self.suggestion_accepted <+ group.suggestion_accepted.gate(&group.focused).map(move |e| (id, *e));
///     self.expression_accepted <+ group.expression_accepted.gate(&group.focused).map(move |e| (id, *e));
/// }
/// ```
macro_rules! propagate_frp {
    ($network:ident, $group:ident, $events:ident, $(($endpoint:ident, $expr:expr)),*) => {
        frp::extend! { $network
            $($events.$endpoint <+ $group.$endpoint.gate(&$group.focused).map($expr);)*
        }
    }
}

/// Generate a struct with a set of FRP endpoints and provide a helper `attach` method that allows
/// to connect two such structs. See [`PropagatedEvents`] docs.
macro_rules! propagated_events {
    (
        $(#$meta:tt)* struct $ident:ident {
            $($endpoint:ident : $endpoint_type:ty),* $(,)?
        }
    ) => {
        $(#$meta)*
        pub struct $ident {
            network: frp::Network,
            $(pub $endpoint : frp::Any<$endpoint_type>),*
        }

        impl $ident {
            /// Constructor.
            #[allow(clippy::new_without_default)]
            pub fn new() -> Self {
                let network = frp::Network::new(stringify!($ident));
                frp::extend! { network
                    $($endpoint <- any_mut();)*
                }
                Self {
                    network,
                    $($endpoint),*
                }
            }

            /// Attach all endpoints of `other` to corresponding endpoints of `self`.
            pub fn attach(&self, other: &$ident) {
                $(self.$endpoint.attach(&other.$endpoint);)*
            }
        }
    }
}

propagated_events! {
    /// A set of FRP endpoints that we want to propagate from component groups. Each field
    /// corresponds to a certain FRP output of the component group. It's output type is modified by
    /// adding a [`GroupId`] so that we can understand which group the event came from.
    #[derive(Debug, Clone, CloneRef)]
    #[allow(missing_docs)]
    struct PropagatedEvents {
        mouse_in_group:            GroupId,
        selected_entry:            Option<(GroupId, entry::Id)>,
        suggestion_accepted:       (GroupId, entry::Id),
        expression_accepted:       (GroupId, entry::Id),
        is_header_selected:        (GroupId, bool),
        header_accepted:           GroupId,
        selection_position_target: (GroupId, Vector2<f32>),
        focused:                   (GroupId, bool),
    }
}


// ===============
// === Wrapper ===
// ===============

newtype_prim! {
    /// An index of the component group.
    GroupId(usize);
}

/// The storage for the groups inside Wrapper. We store both a [`Group`] itself to manage its focus
/// and [`PropagatedEvents`] to propagate its FRP outputs.
type Groups = Rc<RefCell<OptVec<(Group, PropagatedEvents), GroupId>>>;

/// A wrapper around the FRP outputs of the several component groups.
///
/// This wrapper does two things:
/// 1. Propagates FRP events from multiple component groups adding the information which group
///    emitted the event. ([`GroupId`]) Only focused group events are propagated, events from
///    non-focused component groups are silently ignored.
/// 2. Ensures that only a single component group remains focused. This is done by emitting
///    `defocus` events for every other component group. The group that is hovered by mouse
///    (`mouse_in_group` endpoint) becomes focused.
///
/// A wrapper stores a list of "managed" component groups internally. Only groups in this list are
/// propagating their events and change their focus automatically. Use [`Wrapper::add`] and
/// [`Wrapper::remove`] methods to add/remove groups to/from this list.
#[allow(missing_docs)]
#[derive(Debug, Clone, CloneRef, Deref)]
pub struct Wrapper {
    groups: Groups,
    #[deref]
    events: PropagatedEvents,
}

impl Default for Wrapper {
    fn default() -> Self {
        Self::new()
    }
}

impl Wrapper {
    /// Constructor.
    pub fn new() -> Self {
        let groups: Groups = default();
        let events = PropagatedEvents::new();
        let network = &events.network;

        frp::extend! { network
            eval events.mouse_in_group((group_id) {
                groups.borrow().iter_enumerate().for_each(|(idx, (g, _))| {
                    if idx != *group_id { g.defocus(); }
                });
                if let Some((g, _)) = groups.borrow().safe_index(*group_id) { g.focus(); }
            });
        }

        Self { groups, events }
    }

    /// Start managing a new group. Returned [`GroupId`] is non-unique, it might be reused by new
    /// groups if this one removed by calling [`Self::remove`].
    pub fn add(&self, group: Group) -> GroupId {
        let events = PropagatedEvents::new();
        self.events.attach(&events);
        let id = self.groups.borrow_mut().insert((group.clone_ref(), events.clone_ref()));
        self.setup_frp_propagation(&group, id, events);
        id
    }

    /// Stop managing of a group. A freed [`GroupId`] might be reused by new groups later.
    pub fn remove(&self, group_id: GroupId) {
        self.groups.borrow_mut().remove(group_id);
    }

    fn setup_frp_propagation(&self, group: &Group, id: GroupId, events: PropagatedEvents) {
        let network = &self.events.network;
        frp::extend! { network
            let mouse_in = group.is_mouse_over().clone_ref();
            events.mouse_in_group <+ mouse_in.on_change().on_true().map(move |_| id);
        }
        match group {
            Group::OneColumn(group) => {
                frp::extend! { network
                    events.focused <+ group.focused.map(move |f| (id, *f));
                }
                propagate_frp! { network, group, events,
                    (selected_entry, move |e| e.map(|e| (id, e))),
                    (suggestion_accepted, move |e| (id, *e)),
                    (expression_accepted, move |e| (id, *e)),
                    (selection_position_target, move |p| (id, *p)),
                    (is_header_selected, move |h| (id, *h)),
                    (header_accepted, move |_| id)
                }
            }
            Group::Wide(group) => {
                frp::extend! { network
                    events.focused <+ group.focused.map(move |f| (id, *f));
                }
                propagate_frp! { network, group, events,
                    (selected_entry, move |e| e.map(|e| (id, e))),
                    (suggestion_accepted, move |e| (id, *e)),
                    (expression_accepted, move |e| (id, *e)),
                    (selection_position_target, move |p| (id, *p))
                }
            }
        }
    }
}
