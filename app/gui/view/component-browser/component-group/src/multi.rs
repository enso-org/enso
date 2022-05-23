//! Provides a multi component group wrapper, an abstraction used to propagate FRP events from
//! multiple component groups.
//!
//! See [`Wrapper`] docs.


use ensogl_core::prelude::*;

use enso_frp as frp;
use ensogl_core::display::scene::Scene;

use crate::entry;
use crate::wide;
use crate::View;



// =============
// === Group ===
// =============

/// One of the possible component group types.
#[derive(Debug, Clone, CloneRef)]
#[allow(missing_docs)]
pub enum Group {
    Regular(View),
    Wide(wide::View),
}

impl From<View> for Group {
    fn from(view: View) -> Self {
        Group::Regular(view)
    }
}

impl From<wide::View> for Group {
    fn from(view: wide::View) -> Self {
        Group::Wide(view)
    }
}

impl Group {
    fn focus(&self) {
        match self {
            Group::Regular(group) => group.focus(),
            Group::Wide(group) => group.focus(),
        }
    }

    fn defocus(&self) {
        match self {
            Group::Regular(group) => group.defocus(),
            Group::Wide(group) => group.defocus(),
        }
    }

    fn to_object_space(&self, scene: &Scene, pos: Vector2) -> Vector2 {
        match self {
            Group::Regular(group) => scene.screen_to_object_space(group, pos),
            Group::Wide(group) => scene.screen_to_object_space(group, pos),
        }
    }

    /// Whether the `pos` (object-space) is inside the component group borders.
    fn is_inside(&self, pos: Vector2) -> bool {
        match self {
            Group::Regular(group) => group.model().is_inside(pos),
            Group::Wide(group) => group.model().is_inside(pos),
        }
    }
}



/// =====================
/// === propagate_frp ===
/// =====================

/// Transforms
/// ```ignore
/// propagate_frp!{ network, group,
///     (suggestion_accepted, move |e| (id, *e)),
///     (expression_accepted, move |e| (id, *e)),
/// }
/// ```
/// into
/// ```ignore
/// frp::extend! { network
///     suggestion_accepted <+ group.suggestion_accepted.gate(&group.focused).map(move |e| (id, *e));
///     expression_accepted <+ group.expression_accepted.gate(&group.focused).map(move |e| (id, *e));
/// }
/// ```
macro_rules! propagate_frp {
    ($network:ident, $group:ident, $(($endpoint:ident, $expr:expr)),*) => {
        frp::extend! { $network
            $($endpoint <+ $group.$endpoint.gate(&$group.focused).map($expr);)*
        }
    }
}



// ===============
// === Wrapper ===
// ===============

newtype_prim! {
    /// An index of the component group.
    GroupId(usize);
}

/// A wrapper around the FRP outputs of the several component groups.
///
/// Each field in this struct corresponds to certain component group FRP output with additional
/// [`GroupId`] attached. [`GroupId`] is a simple index inside `groups` iterator passed to the
/// [`Wrapper::new`] constructor. Only focused component groups propagate their events. At all
/// times only a single group remains focused.
///
/// In addition to propagating FRP outputs, the [`Wrapper`] also emits `group.focus()` and
/// `group.defocus()` when the mouse moves in or out the `group` shape.
#[allow(missing_docs)]
#[derive(Debug)]
pub struct Wrapper {
    pub selected_entry:            frp::Stream<Option<(GroupId, entry::Id)>>,
    pub suggestion_accepted:       frp::Stream<(GroupId, entry::Id)>,
    pub expression_accepted:       frp::Stream<(GroupId, entry::Id)>,
    pub is_header_selected:        frp::Stream<(GroupId, bool)>,
    pub header_accepted:           frp::Stream<GroupId>,
    pub selection_position_target: frp::Stream<(GroupId, Vector2<f32>)>,
    pub focused:                   frp::Stream<(GroupId, bool)>,
}

impl Wrapper {
    /// Constructor.
    pub fn new(scene: &Scene, network: &frp::Network, groups: impl Iterator<Item = Group>) -> Self {
        let mouse = &scene.mouse.frp;
        let groups = Rc::new(groups.collect::<Vec<_>>());
        frp::extend! { network
            mouse_in_group <- any(...);
            selected_entry <- any(...);
            suggestion_accepted <- any(...);
            expression_accepted <- any(...);
            is_header_selected <- any(...);
            header_accepted <- any(...);
            selection_position_target <- any(...);
            focused <- any(...);
        }

        for (i, group) in groups.iter().enumerate() {
            let id = GroupId::from(i);
            frp::extend! { network
                mouse_in <- map(&mouse.position,f!([scene,group](pos) {
                    let pos_obj_space = group.to_object_space(&scene, *pos);
                    group.is_inside(pos_obj_space)
                }));
                mouse_in_group <+ mouse_in.on_change().on_true().map(move |_| id);
            }
            match group {
                Group::Regular(group) => {
                    frp::extend! { network
                        focused <+ group.focused.map(move |f| (id, *f));
                    }
                    propagate_frp! { network, group,
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
                        focused <+ group.focused.map(move |f| (id, *f));
                    }
                    propagate_frp! { network, group,
                        (selected_entry, move |e| e.map(|e| (id, e))),
                        (suggestion_accepted, move |e| (id, *e)),
                        (expression_accepted, move |e| (id, *e)),
                        (selection_position_target, move |p| (id, *p))
                    }
                }
            }
        }

        frp::extend! { network
            eval mouse_in_group((group_id) {
                groups.iter().for_each(|g| {
                    g.defocus();
                });
                groups[usize::from(group_id)].focus();
            });
        }
        let selected_entry = selected_entry.into();
        let suggestion_accepted = suggestion_accepted.into();
        let expression_accepted = expression_accepted.into();
        let is_header_selected = is_header_selected.into();
        let header_accepted = header_accepted.into();
        let selection_position_target = selection_position_target.into();
        let focused = focused.into();

        Self {
            selected_entry,
            suggestion_accepted,
            expression_accepted,
            is_header_selected,
            header_accepted,
            selection_position_target,
            focused,
        }
    }
}
