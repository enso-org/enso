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
use crate::Selected;
use crate::View;

use enso_frp as frp;



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
    /// Focus group.
    pub fn focus(&self) {
        match self {
            Group::OneColumn(group) => group.focus(),
            Group::Wide(group) => group.focus(),
        }
    }

    /// Defocus group.
    pub fn defocus(&self) {
        match self {
            Group::OneColumn(group) => group.defocus(),
            Group::Wide(group) => group.defocus(),
        }
    }

    /// An FRP stream of `is_mouse_over` events.
    pub fn is_mouse_over(&self) -> &frp::Sampler<bool> {
        match self {
            Group::OneColumn(group) => &group.is_mouse_over,
            Group::Wide(group) => &group.is_mouse_over,
        }
    }

    /// Position of the display object.
    pub fn position(&self) -> Vector3 {
        match self {
            Group::OneColumn(group) => group.position(),
            Group::Wide(group) => group.position(),
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
        selected:                  Option<(GroupId, Selected)>,
        suggestion_accepted:       (GroupId, entry::Id),
        expression_accepted:       (GroupId, entry::Id),
        header_accepted:           GroupId,
        selection_position_target: (GroupId, Vector2<f32>),
        selection_size:            (GroupId, Vector2<f32>),
        selection_corners_radius:  (GroupId, f32),
        focused:                   (GroupId, bool),
    }
}


// ===============
// === Wrapper ===
// ===============

/// A Component Groups List Section identifier.
#[derive(Copy, Clone, Debug, Default, Eq, Hash, PartialEq)]
pub enum SectionId {
    /// The "Favorite Tools" section.
    #[default]
    Favorites,
    /// The "Local Scope" section.
    LocalScope,
    /// The "Sub-Modules" section.
    SubModules,
}

/// A Group identifier. If `section` is [`SectionId::LocalScope`], the `index` should be 0, as that
/// section has always only one group.
#[allow(missing_docs)]
#[derive(Copy, Clone, Debug, Default, Eq, Hash, PartialEq)]
pub struct GroupId {
    pub section: SectionId,
    pub index:   usize,
}

impl GroupId {
    /// Get id of the only group in "Local Scope" section.
    pub fn local_scope_group() -> Self {
        GroupId { section: SectionId::LocalScope, index: default() }
    }
}

/// The storage for the groups inside Wrapper. We store both a [`Group`] itself to manage its focus
/// and [`PropagatedEvents`] to propagate its FRP outputs.
type Groups = Rc<RefCell<HashMap<GroupId, (Group, PropagatedEvents)>>>;

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
                groups.borrow().iter().for_each(|(id, (g, _))| {
                    if id != group_id { g.defocus(); }
                });
                if let Some((g, _)) = groups.borrow().get(group_id) { g.focus(); }
            });
        }

        Self { groups, events }
    }

    /// Start managing a new group. If there is already managed group under given id, the old group
    /// is no longer managed, and it's returned. Returned [`GroupId`] is non-unique, it might be
    /// reused by new groups if this one removed by calling [`Self::remove`].
    pub fn add(&self, id: GroupId, group: Group) -> Option<Group> {
        let events = PropagatedEvents::new();
        self.events.attach(&events);
        let old = self.groups.borrow_mut().insert(id, (group.clone_ref(), events.clone_ref()));
        self.setup_frp_propagation(&group, id, events);
        old.map(|(group, _)| group)
    }

    /// Get a group by a [`GroupId`].
    pub fn get(&self, id: &GroupId) -> Option<Group> {
        self.groups.borrow().get(id).map(|(group, _)| group).map(CloneRef::clone_ref)
    }

    /// Stop managing of a group. A freed [`GroupId`] might be reused by new groups later.
    pub fn remove(&self, group_id: GroupId) {
        self.groups.borrow_mut().remove(&group_id);
    }

    /// Stop managing all groups of a section.
    pub fn remove_section(&self, section: SectionId) {
        self.groups.borrow_mut().retain(|&id, _| id.section != section);
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
                    (selected, move |s| s.map(|s| (id, s))),
                    (suggestion_accepted, move |e| (id, *e)),
                    (expression_accepted, move |e| (id, *e)),
                    (selection_position_target, move |p| (id, *p)),
                    (selection_size, move |p| (id, *p)),
                    (selection_corners_radius, move |r| (id, *r)),
                    (header_accepted, move |_| id)
                }
            }
            Group::Wide(group) => {
                frp::extend! { network
                    events.focused <+ group.focused.map(move |f| (id, *f));
                }
                propagate_frp! { network, group, events,
                    (selected, move |s| s.map(|s| (id, s))),
                    (suggestion_accepted, move |e| (id, *e)),
                    (expression_accepted, move |e| (id, *e)),
                    (selection_position_target, move |p| (id, *p)),
                    (selection_size, move |p| (id, *p)),
                    (selection_corners_radius, move |r| (id, *r))
                }
            }
        }
    }
}
