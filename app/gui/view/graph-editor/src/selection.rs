//! Module that contains the logic sor selecting nodes. This includes selecting single nodes
//! by clicking on them separately, as well as click+drag for selecting with a selection area.
mod bounding_box;

use ensogl::prelude::*;

pub use bounding_box::BoundingBox;

use crate::NodeId;
use crate::Nodes;
use crate::TouchState;

use ensogl::frp;
use ensogl::gui::cursor;
use ensogl::gui::cursor::Cursor;



// ============
// === Mode ===
// ============

/// Possible selection modes.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Mode {
    /// Select a single node when clicking on it. Deselects all other nodes.
    Normal,
    /// Toggle the selection state of the select node without changing the selection state of
    ///other node. This allows to add and remove nodes from the current selection.
    Multi,
    /// Add selected nodes to the set of currently selected nodes.
    Merge,
    /// Remove selected nodes from the set of currently selected nodes.
    Subtract,
    /// Invert the selection state of the selected nodes.
    Inverse,
}

impl Mode {
    /// Return whether an element should be selected, if a selection was triggered through single
    /// (not area) selection and had the given `was_selected` status before.
    fn single_should_select(self, was_selected: bool) -> bool {
        match self {
            Self::Normal => true,
            Self::Merge => true,
            Self::Multi => !was_selected,
            Self::Inverse => !was_selected,
            _ => false,
        }
    }

    /// Return whether an element should be deselected, if a deselection was triggered through
    /// single selection and had the given `was_selected` status before.
    fn single_should_deselect(self, was_selected: bool) -> bool {
        match self {
            Self::Subtract => true,
            Self::Multi => was_selected,
            Self::Inverse => was_selected,
            _ => false,
        }
    }

    /// Return whether an element should be selected, if a selection was triggered through an area
    /// selection and had the given `was_selected` status before.
    fn area_should_select(self, was_selected: bool) -> bool {
        match self {
            Self::Normal => true,
            Self::Merge => true,
            Self::Multi => true,
            Self::Inverse => !was_selected,
            _ => false,
        }
    }

    /// Return whether an element should be deselected, if a deselection was triggered  through an
    /// area selection and had the given `was_selected` status before.
    fn area_should_deselect(self, was_selected: bool) -> bool {
        match self {
            Self::Subtract => true,
            Self::Multi => was_selected,
            Self::Inverse => was_selected,
            _ => false,
        }
    }
}

impl Default for Mode {
    fn default() -> Self {
        Self::Normal
    }
}



// ==========================
// === TemporarySelection ===
// ==========================

/// Struct that stores the initial selection state of a node. Used to restore the selection state
/// after a temporary selection has been undone, for example when an area selection is shrunk to no
/// longer encompass the node.
///
/// This implements `Hash` and `Eq` based only on the ndoe identity to avoid overwriting the initial
/// selection state in a set of `TemporarySelection`s. This allows the first added element for a
/// node to be preserved.
#[derive(Clone, Copy, Default, Debug, Eq)]
pub struct TemporarySelection {
    node:         NodeId,
    was_selected: bool,
}

impl TemporarySelection {
    fn new(node: NodeId, was_selected: bool) -> Self {
        Self { node, was_selected }
    }
}

impl PartialEq for TemporarySelection {
    fn eq(&self, other: &Self) -> bool {
        self.node == other.node
    }
}

impl Hash for TemporarySelection {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.node.hash(state)
    }
}

// ===============
// === NodeSet ===
// ===============

/// Set of `TemporarySelection` items. Used to keep track of the nodes currently selected with an
/// ongoing area selection.
mod node_set {
    use crate::selection::TemporarySelection;
    use ensogl::frp;
    use ensogl::prelude::*;

    type SetItem = TemporarySelection;

    ensogl::define_endpoints! {
        Input {
            insert(SetItem),
            remove(SetItem),
            /// Remove the nodes that are not contained in the given Vec.
            remove_difference_with_vec(Vec<SetItem>),
            /// Empties the set without emitting `removed` events.
            reset(),
        }

        Output {
            // Emitted when an element that has not been in the set, is added to the set.
            // It is NOT emitted if an element that was added was already part of the set.
            added(SetItem),
            // Emitted when an element that has been in the set, is removed from the set.
            // It is NOT emitted if an element that was removed was not part of the set.
            removed(SetItem),
       }
    }

    #[derive(Clone, CloneRef, Debug, Default)]
    struct Model {
        set: Rc<RefCell<HashSet<SetItem>>>,
    }

    impl Model {
        fn insert(&self, value: SetItem) -> bool {
            self.set.borrow_mut().insert(value)
        }

        fn remove(&self, value: SetItem) -> bool {
            self.set.borrow_mut().remove(&value)
        }

        fn difference(&self, other: &HashSet<SetItem>) -> Vec<SetItem> {
            self.set.borrow_mut().difference(other).cloned().collect()
        }

        fn reset(&self) {
            self.set.borrow_mut().clear()
        }
    }

    #[derive(Clone, CloneRef, Debug)]
    pub struct Set {
        frp:   Rc<Frp>,
        model: Rc<Model>,
    }

    impl Set {
        pub fn new() -> Self {
            let frp = Rc::new(Frp::new());
            let model = Rc::new(Model::default());
            Self { frp, model }.init()
        }

        fn init(self) -> Self {
            let network = &self.frp.network;
            let frp = &self.frp;
            let set = &self.model;
            frp::extend! { network
                eval_ frp.reset (set.reset());
                to_remove <- frp.remove_difference_with_vec.map(f!((values)
                    set.difference(&values.clone().into_iter().collect())));
                node_to_remvoe <= to_remove;
                frp.remove <+ node_to_remvoe;

                was_inserted     <- frp.insert.map(f!((value) set.insert(*value)));
                frp.source.added <+ frp.insert.gate(&was_inserted);

                was_removed        <- frp.remove.map(f!((value) set.remove(*value)));
                frp.source.removed <+ frp.remove.gate(&was_removed);

            }
            self
        }
    }

    impl Deref for Set {
        type Target = Frp;
        fn deref(&self) -> &Self::Target {
            &self.frp
        }
    }
}

fn get_nodes_in_bounding_box(bounding_box: &BoundingBox, nodes: &Nodes) -> Vec<NodeId> {
    let nodes_raw = nodes.all.raw.as_ref().borrow();
    nodes_raw
        .iter()
        .filter_map(|(id, node)| {
            bounding_box.intersects(&node.view.frp.bounding_box.value()).as_some(*id)
        })
        .collect()
}

/// Return an FRP endpoint that indicates the current selection mode. This method sets up the logic
/// for deriving the selection mode from the graph editor FRP.
pub fn get_mode(network: &frp::Network, editor: &crate::FrpEndpoints) -> frp::stream::Stream<Mode> {
    frp::extend! { network

    let multi_select_flag = crate::enable_disable_toggle
        ( network
        , &editor.enable_node_multi_select
        , &editor.disable_node_multi_select
        , &editor.toggle_node_multi_select
        );

    let merge_select_flag = crate::enable_disable_toggle
        ( network
        , &editor.enable_node_merge_select
        , &editor.disable_node_merge_select
        , &editor.toggle_node_merge_select
        );

    let subtract_select_flag = crate::enable_disable_toggle
        ( network
        , &editor.enable_node_subtract_select
        , &editor.disable_node_subtract_select
        , &editor.toggle_node_subtract_select
        );

    let inverse_select_flag = crate::enable_disable_toggle
        ( network
        , &editor.enable_node_inverse_select
        , &editor.disable_node_inverse_select
        , &editor.toggle_node_inverse_select
        );

    selection_mode <- all_with4
        (&multi_select_flag,&merge_select_flag,&subtract_select_flag,&inverse_select_flag,
        |multi,merge,subtract,inverse| {
            if      *multi    { Mode::Multi }
            else if *merge    { Mode::Merge }
            else if *subtract { Mode::Subtract }
            else if *inverse  { Mode::Inverse }
            else              { Mode::Normal }
        }
    );
    }
    selection_mode
}



// ==================
// === Controller ===
// ==================

/// Selection Controller that handles the logic for selecting and deselecting nodes in the graph
/// editor.
#[derive(Debug, Clone, CloneRef)]
pub struct Controller {
    network:                frp::Network,
    cursor_selection_nodes: node_set::Set,

    pub enable_area_selection: frp::Source<bool>,

    pub cursor_style:   frp::stream::Stream<cursor::Style>,
    pub area_selection: frp::stream::Stream<bool>,
}

impl Controller {
    pub fn new(
        editor: &crate::FrpEndpoints,
        cursor: &Cursor,
        mouse: &frp::io::Mouse,
        touch: &TouchState,
        nodes: &Nodes,
    ) -> Self {
        let network = frp::Network::new("selection::Controller");
        let selection_mode = get_mode(&network, editor);
        let cursor_selection_nodes = node_set::Set::new();


        frp::extend! { network
            deselect_all_nodes      <- any_(...);

            enable_area_selection  <- source();

            // ===  Graph Editor Internal API ===
            eval editor.select_node   ((node_id) nodes.select(node_id));
            eval editor.deselect_node ((node_id)  nodes.select(node_id));
            editor.source.node_selected   <+  editor.select_node;
            editor.source.node_deselected <+ editor.deselect_node;

            // ===  Selection Box & Mouse IO ===
            on_press_style   <- mouse.down_primary . constant(cursor::Style::new_press());
            on_release_style <- mouse.up_primary . constant(cursor::Style::default());

            edit_mode          <- bool(&editor.edit_mode_off,&editor.edit_mode_on);
            not_edit_mode      <- edit_mode.not();
            should_area_select <- not_edit_mode && enable_area_selection;

            drag_start  <- touch.background.is_down.on_true().gate(&should_area_select);
            is_dragging <- bool(&mouse.up_primary,&drag_start);
            drag_end    <- is_dragging.on_false();
            drag_start  <- is_dragging.on_true();
            let area_selection = is_dragging.clone_ref();

            mouse_on_down_position <- mouse.position.sample(&mouse.down_primary);
            selection_size_down    <- mouse.position.map2(&mouse_on_down_position,|m,n|{m-n});
            selection_size         <- selection_size_down.gate(&touch.background.is_down).gate(&should_area_select);
            cursor_selection_start <- selection_size.map(|p|
                    cursor::Style::new_with_all_fields_default().press().box_selection(Vector2::new(p.x,p.y)));
            cursor_selection_end   <- mouse.up_primary . constant(cursor::Style::default());
            cursor_selection       <- any (cursor_selection_start,cursor_selection_end);

            cursor_on_down_position <- cursor.frp.scene_position.sample(&mouse.down_primary);
            should_update_drag      <- is_dragging && touch.background.is_down;
            cursor_drag_position    <- cursor.frp.scene_position.gate(&should_update_drag).on_change();

            scene_bounding_box      <- cursor_drag_position.map2(&cursor_on_down_position,
                |&m,&n|{
                // The dragged position is the center of the bounding box. Thus we need to offset the
                // corner point by the distance to the origin.
                let half      = m - n;
                let m_correct = n + 2.0 * half;
                let mut bounding_box = BoundingBox::from_corners(n.xy(),m_correct.xy());
                // Since the cursor has some extent we need to account for its size.
                bounding_box.grow_x(cursor::SIDES_PADDING / 2.0);
                bounding_box.grow_y(cursor::SIDES_PADDING / 2.0);
                bounding_box
                }
            );

            nodes_in_bb <- scene_bounding_box.map(f!([nodes](bb) get_nodes_in_bounding_box(bb,&nodes)));
            nodes_in_bb <- nodes_in_bb.map(f!([nodes](nodes_selected) {
                nodes_selected.clone().into_iter().map(|node|{
                     let is_selected = nodes.is_selected(node);
                    TemporarySelection::new(node,is_selected)
                }).collect()
            }));
            node_info <= nodes_in_bb;


            // === Selection Box Handling ===

            keep_selection     <- selection_mode.map(|t| *t != Mode::Normal);
            deselect_on_select <- drag_start.gate_not(&keep_selection);
            deselect_all_nodes <+ deselect_on_select;

            cursor_selection_nodes.insert <+ node_info;
            cursor_selection_nodes.remove_difference_with_vec <+ nodes_in_bb;

            cursor_selection_nodes.reset <+ drag_end;

            // Node enters selection area, select depending on selection mode.
            node_added    <- cursor_selection_nodes.added.map(|node_info| node_info.node);
            should_select <- cursor_selection_nodes.added.map2(&selection_mode,
                |info,mode| mode.area_should_select(info.was_selected)
            );
            should_deselect <- cursor_selection_nodes.added.map2(&selection_mode,
                |info,mode| mode.area_should_deselect(info.was_selected)
            );

            editor.source.node_selected   <+ node_added.gate(&should_select);
            editor.source.node_deselected <+ node_added.gate(&should_deselect);

            // Node leaves selection area, revert to previous selection state.
            node_removed <- cursor_selection_nodes.removed.map(f!([](node_info) {
                if !node_info.was_selected { Some(node_info.node) } else {None}
            })).unwrap();

            editor.source.node_deselected <+ node_removed;


            // ===  Single Node Selection Box & Mouse IO ===

            should_not_select       <- edit_mode || editor.some_edge_endpoints_unset;
            node_to_select_non_edit <- touch.nodes.selected.gate_not(&should_not_select);
            node_to_select_edit     <- touch.nodes.down.gate(&edit_mode);
            node_to_select          <- any(node_to_select_non_edit,
                                           node_to_select_edit);
            node_was_selected       <- node_to_select.map(f!((id) nodes.selected.contains(id)));

            should_select <- node_to_select.map3(&selection_mode,&node_was_selected,
                |_,mode,was_selected| mode.single_should_select(*was_selected)
            );

            should_deselect <- node_to_select.map3(&selection_mode,&node_was_selected,
                |_,mode,was_selected| mode.single_should_deselect(*was_selected)
            );

            deselect_on_select      <- node_to_select.gate_not(&keep_selection);
            deselect_all_nodes      <+ deselect_on_select;
            deselect_all_nodes      <+ editor.deselect_all_nodes;

            deselect_on_bg_press    <- touch.background.selected.gate_not(&keep_selection);
            deselect_all_nodes      <+ deselect_on_bg_press;
            all_nodes_to_deselect   <= deselect_all_nodes.map(f_!(nodes.selected.mem_take()));
            editor.source.node_deselected <+ all_nodes_to_deselect;

            node_selected           <- node_to_select.gate(&should_select);
            node_deselected         <- node_to_select.gate(&should_deselect);
            editor.source.node_selected   <+ node_selected;
            editor.source.node_deselected <+ node_deselected;

            // ===  Output bindings ===
            cursor_style <- any(on_press_style,on_release_style,cursor_selection);

        }

        // Init defaults.
        enable_area_selection.emit(true);

        Controller {
            network,
            cursor_selection_nodes,
            enable_area_selection,
            cursor_style,
            area_selection,
        }
    }
}
