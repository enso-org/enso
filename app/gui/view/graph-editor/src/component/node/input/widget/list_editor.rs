//! Module dedicated to the [List Editor widget](Widget).

// FIXME[ao]: This code miss important documentation (e.g. for `Element`, `DragData` and `ListItem`)
//  and may be unreadable at some places. It should be improved in several next debugging PRs.

use super::prelude::*;
use crate::prelude::*;

use crate::component::node::input::area::TEXT_SIZE;
use crate::layers::CommonLayers;

use ensogl::display;
use ensogl::Animation;
use ensogl_component::list_editor::ListEditor;
use span_tree::node::Kind;
use std::collections::hash_map::Entry;



// =================
// === Constants ===
// =================

/// The type name that enables the list editor widget.
const VECTOR_TYPE: &str = "Standard.Base.Data.Vector.Vector";

/// Extra space to the left and right of the list that will respond to mouse events. The list
/// insertion points will only be shown within the list bounding box extended by this margin.
const LIST_HOVER_MARGIN: f32 = 4.0;

const ITEMS_GAP: f32 = 10.0;
const INSERT_HOVER_MARGIN: f32 = 4.0;
const ITEM_HOVER_MARGIN: f32 = (ITEMS_GAP - INSERT_HOVER_MARGIN * 2.0) * 0.5;
const INSERTION_OFFSET: f32 = ITEMS_GAP * 0.5;

// ===============
// === Element ===
// ===============

#[derive(Debug, display::Object)]
struct Element {
    display_object: object::Instance,
    content:        object::Instance,
    #[allow(dead_code)]
    background:     Rectangle,
    expr_range:     Range<usize>,
    item_crumb:     usize,
    alive:          Option<()>,
}

struct DragData {
    child_id:      WidgetIdentity,
    element:       Element,
    expression:    String,
    #[allow(dead_code)]
    owned_subtree: Vec<(WidgetIdentity, TreeNode)>,
}

impl Debug for DragData {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("DragData")
            .field("child_id", &self.child_id)
            .field("element", &self.element)
            .field("expression", &self.expression)
            .field("owned_subtree.len", &self.owned_subtree.len())
            .finish()
    }
}

#[derive(Clone, Copy, Hash, PartialEq, Eq, Debug)]
enum ElementIdentity {
    UniqueBase(IdentityBase),
    FullIdentity(WidgetIdentity),
}

#[derive(Clone, CloneRef, display::Object)]
struct ListItem {
    child_id:       Immutable<WidgetIdentity>,
    element_id:     Immutable<ElementIdentity>,
    display_object: object::Instance,
    drag_data:      Rc<RefCell<Option<DragData>>>,
}

impl Debug for ListItem {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("ListItem").field("element_id", &self.child_id).finish()
    }
}

#[derive(Debug)]
struct ListItemCandidate {
    compare_id: ElementIdentity,
    assigned:   bool,
    item:       ListItem,
}

impl PartialEq for ListItem {
    fn eq(&self, other: &Self) -> bool {
        self.child_id == other.child_id
    }
}

impl ListItem {
    fn take_drag_data(&self) -> Option<DragData> {
        let mut borrow = self.drag_data.borrow_mut();
        let can_take = matches!(&*borrow, Some(data) if data.child_id == *self.child_id);
        can_take.and_option_from(|| borrow.take())
    }
}

impl Element {
    fn new(layers: &CommonLayers) -> Self {
        let display_object = object::Instance::new_named("Element");
        let content = object::Instance::new_named("Content");
        let background = Rectangle::new();
        background.set_color(display::shape::INVISIBLE_HOVER_COLOR);
        background.allow_grow().set_alignment_left_center();
        content.use_auto_layout().set_children_alignment_left_center();
        layers.hover.add(&background);
        display_object.replace_children(&[background.display_object(), &content]);
        Self {
            display_object,
            content,
            background,
            expr_range: default(),
            alive: default(),
            item_crumb: default(),
        }
    }
}

/// A model for the vector editor widget.
#[derive(Debug, display::Object)]
pub struct Widget {
    display_object: object::Instance,
    network:        frp::Network,
    reconfigured:   frp::Any,
    model:          Rc<RefCell<Model>>,
}

impl Widget {
    fn init_list_updates(self, ctx: &ConfigContext) -> Self {
        let widgets_frp = ctx.frp();
        let network = &self.network;
        let model = &self.model;
        let list = self.model.borrow().list.clone_ref();
        let scene = scene();
        let on_down = scene.on_event::<mouse::Down>();
        let on_up = scene.on_event::<mouse::Up>();
        let reconfigured = self.reconfigured.clone_ref();

        frp::extend! { network
            init <- source_();

            // Adding elements.
            requested_new <- list.request_new_item.filter_map(|resp| resp.gui_interaction_payload());
            eval requested_new(
                [model, widgets_frp] (idx) model.borrow_mut().on_new_item(*idx, &widgets_frp)
            );

            // Inserting dragged elements.
            inserted_by_user <- list.on_item_added.filter_map(|resp| resp.gui_interaction_payload());
            widgets_frp.value_changed <+ inserted_by_user.filter_map(f!([list, model](index) {
                let item = list.item_at(*index)?;
                model.borrow_mut().on_item_added(item, *index)
            }));

            // Removing dragged elements.
            removed_by_user <- list.on_item_removed.filter_map(|resp| resp.clone().gui_interaction_payload());
            remove_request <- removed_by_user.filter_map(f!([model] ((_, item)) {
                let item = item.borrow_mut().take()?;
                model.borrow_mut().on_item_removed(item)
            }));
            widgets_frp.transfer_ownership <+ remove_request._1();
            widgets_frp.value_changed <+ remove_request._0().map(|crumb| (crumb.clone(), None));

            mouse_is_up <- bool(&on_down, &on_up);
            ports_made_visible <- widgets_frp.set_ports_visible.on_true();
            ports_made_invisible <- widgets_frp.set_ports_visible.on_false();
            gated_invisible <- ports_made_invisible.buffered_gate(&mouse_is_up);
            ports_were_active_during_interaction <- bool(&gated_invisible, &ports_made_visible);

            // Enable list interactions only under specific conditions:
            // - We are not dragging an edge, and it was not dragged during this mouse click.
            // - The widgets are not set to read-only mode.
            // - The user is not about to switch to edit mode.
            // - The mouse is hovering the list interaction bounding box.
            enable_interaction <- all_with4(
                &ports_were_active_during_interaction,
                &widgets_frp.set_edit_ready_mode,
                &widgets_frp.set_read_only,
                &init,
                |ports, edit, read_only, _| !ports && !edit && !read_only
            );
            let list_enter = list.on_event::<mouse::Enter>();
            let list_leave = list.on_event::<mouse::Leave>();
            is_hovered <- bool(&list_leave, &list_enter).debounce().on_change();
            enable_insertion <- enable_interaction && is_hovered;
            list.enable_all_insertion_points <+ enable_insertion;
            list.enable_last_insertion_point <+ enable_insertion;
            list.enable_dragging <+ enable_interaction;

            // Animate margins around the list that are added when it has elements.
            let margin_anim = Animation::new(network);
            has_elements <- reconfigured.map(f_!([list] !list.is_empty()));
            margin_anim.target <+ has_elements
                .map(|has| has.then_val_or_default(INSERTION_OFFSET));

            update_margin <- all(margin_anim.value, reconfigured)._0().debounce();
            eval update_margin((margin) model.borrow().set_side_margin(*margin));
        }

        init.emit(());

        self
    }
}

#[derive(Debug)]
struct Model {
    self_id:                WidgetIdentity,
    list:                   ListEditor<ListItem>,
    /// Root element of the last child widget with [`span_tree::node::Kind::InsertionPoint`] node.
    /// It is stored in the model to allow animating its margins within the FRP network.
    append_insertion_point: Option<object::Instance>,
    #[allow(dead_code)]
    background:             Rectangle,
    elements:               HashMap<ElementIdentity, Element>,
    default_value:          DefaultValue,
    expression:             String,
    crumbs:                 span_tree::Crumbs,
    drag_data_rc:           Rc<RefCell<Option<DragData>>>,
    insertion_indices:      Vec<usize>,
    insert_with_brackets:   bool,
}

impl Model {
    fn new(ctx: &ConfigContext, display_object: &object::Instance) -> Self {
        let list = ListEditor::new(&ctx.app().cursor);
        list.gap(ITEMS_GAP);
        list.set_size_hug_y(TEXT_SIZE).allow_grow_y();
        display_object.use_auto_layout().set_children_alignment_left_center();
        let background = Rectangle::new();
        background.set_color(display::shape::INVISIBLE_HOVER_COLOR);
        background.allow_grow().set_alignment_left_center();
        background.set_margin_xy((-LIST_HOVER_MARGIN, 0.0));
        list.add_child(&background);

        Self {
            self_id: ctx.info.identity,
            list,
            background,
            append_insertion_point: default(),
            elements: default(),
            default_value: default(),
            expression: default(),
            crumbs: default(),
            drag_data_rc: default(),
            insertion_indices: default(),
            insert_with_brackets: default(),
        }
    }

    fn configure(&mut self, root: &object::Instance, cfg: &Config, mut ctx: ConfigContext) {
        self.expression.clear();
        self.default_value = cfg.item_default.clone();
        self.expression.push_str(ctx.span_expression());
        self.crumbs = ctx.span_node.crumbs.clone();


        // Right now, nested list editors are broken. Prevent them from being created. Whenever
        // a nested list editor is requested, we instead use a hierarchical widget to display the
        // child list items as ordinary expressions.
        let ext = ctx.get_extension_or_default::<Extension>();
        let already_in_list = ext.already_in_list;
        ctx.set_extension(Extension { already_in_list: true });

        if already_in_list {
            let child = ctx.builder.child_widget(ctx.span_node, ctx.info.nesting_level);
            root.replace_children(&[&child.root_object]);
        } else if ctx.span_node.is_insertion_point() {
            self.insert_with_brackets = true;
            self.configure_insertion_point(root, ctx)
        } else {
            self.configure_vector(root, cfg, ctx)
        }
    }

    fn configure_insertion_point(&mut self, root: &object::Instance, ctx: ConfigContext) {
        self.elements.clear();
        self.list.clear();
        self.append_insertion_point = None;
        let insertion_point = ctx.builder.child_widget(ctx.span_node, ctx.info.nesting_level);
        root.replace_children(&[&*insertion_point, self.list.display_object()]);
        set_margins(self.list.display_object(), 0.0, 0.0);
    }

    fn configure_vector(&mut self, root: &object::Instance, cfg: &Config, ctx: ConfigContext) {
        ctx.builder.manage_child_margins();
        let nest = ctx.info.nesting_level.next();
        let child_config = cfg.item_widget.as_deref();

        let mut build_child_widget = |i, config, hover_padding: f32| {
            let node = ctx.span_node.clone().child(i).expect("invalid index");
            ctx.builder.override_port_hover_padding(Some(hover_padding));
            ctx.builder.child_widget_of_type(node, nest, config)
        };

        let mut open_bracket = None;
        let mut close_bracket = None;
        let mut last_insert_crumb = None;
        let mut list_items = SmallVec::<[ListItemCandidate; 16]>::new();
        let mut new_items_range: Option<Range<usize>> = None;

        let insert_config = Configuration::active_insertion_point();
        let insert_config = Some(&insert_config);

        let list_id_base = ctx.info.identity.base;

        self.insertion_indices.clear();
        for (index, child) in ctx.span_node.node.children.iter().enumerate() {
            let node = &child.node;
            let start = child.parent_offset.value as usize;
            let range = start..start + node.size.value as usize;
            let expr = &self.expression[range.clone()];

            match node.kind {
                Kind::Token if expr == "[" && open_bracket.is_none() => {
                    let child = build_child_widget(index, None, 0.0);
                    open_bracket = Some(child.root_object);
                }
                Kind::Token if expr == "]" && close_bracket.is_none() => {
                    let child = build_child_widget(index, None, 0.0);
                    close_bracket = Some(child.root_object);
                }
                Kind::InsertionPoint(_) => {
                    last_insert_crumb = Some(index);
                }
                Kind::Argument(_) if last_insert_crumb.is_some() => {
                    let insert_index = last_insert_crumb.take().unwrap();
                    let insert =
                        build_child_widget(insert_index, insert_config, INSERT_HOVER_MARGIN);
                    let item = build_child_widget(index, child_config, ITEM_HOVER_MARGIN);

                    let id = match list_id_base == item.info.identity.base {
                        true => ElementIdentity::FullIdentity(item.info.identity),
                        false => ElementIdentity::UniqueBase(item.info.identity.base),
                    };

                    let entry = self.elements.entry(id);
                    let exists = matches!(entry, Entry::Occupied(_));
                    if !exists {
                        let i = list_items.len();
                        new_items_range =
                            Some(new_items_range.map_or(i..i + 1, |r| r.start..i + 1));
                    }

                    let element = entry.or_insert_with(|| Element::new(&ctx.layers));
                    set_margins(&insert, -INSERTION_OFFSET, INSERTION_OFFSET);
                    element.alive = Some(());
                    element.item_crumb = index;
                    element.expr_range = range;
                    element.content.replace_children(&[&*insert, &*item]);
                    self.insertion_indices.push(insert_index);
                    let item = ListItem {
                        child_id:       Immutable(item.info.identity),
                        element_id:     Immutable(id),
                        display_object: element.display_object.clone(),
                        drag_data:      self.drag_data_rc.clone(),
                    };
                    list_items.push(ListItemCandidate { compare_id: id, assigned: exists, item });
                }
                _ => {}
            }
        }

        let new_items_range = new_items_range.unwrap_or(0..0);
        let mut non_assigned_items = list_items[new_items_range].iter_mut().filter(|c| !c.assigned);

        self.elements.retain(|key, child| {
            let alive = child.alive.take().is_some();
            if !alive {
                if let Some(candidate) = non_assigned_items.next() {
                    candidate.compare_id = *key;
                }
            }
            alive
        });
        self.insertion_indices.extend(last_insert_crumb);
        self.insert_with_brackets = open_bracket.is_none() && close_bracket.is_none();

        self.append_insertion_point = last_insert_crumb
            .map(|index| build_child_widget(index, insert_config, INSERT_HOVER_MARGIN).root_object);

        let (open_bracket, close_bracket) = open_bracket.zip(close_bracket).unzip();
        let children: SmallVec<[&object::Instance; 4]> = [
            open_bracket.as_ref(),
            Some(self.list.display_object()),
            self.append_insertion_point.as_ref(),
            close_bracket.as_ref(),
        ]
        .into_iter()
        .flatten()
        .collect();

        root.replace_children(&children);

        let mut need_reposition = false;

        let current_items = self.list.items();
        list_diff(
            &current_items,
            &list_items,
            |old, new| *old.element_id == new.compare_id,
            |op| match op {
                DiffOp::Delete { at, old, present_later } =>
                    if present_later.is_some()
                        || list_items.iter().any(|i| i.item.display_object == old.display_object)
                    {
                        self.list.take_item_no_reposition(at);
                        need_reposition = true;
                    } else {
                        self.list.trash_item_at(at);
                    },
                DiffOp::Insert { at, new } => {
                    self.list.insert_item_no_reposition(at, new.item.clone_ref());
                    need_reposition = true;
                }
                DiffOp::Update { at, old, new } =>
                    if old.display_object() != new.item.display_object()
                        || old.child_id != new.item.child_id
                    {
                        self.list.replace_item_no_reposition(at, new.item.clone_ref());
                        need_reposition = true;
                    },
            },
        );

        if need_reposition {
            self.list.reposition_items();
        }
    }

    fn on_item_removed(&mut self, item: ListItem) -> Option<(span_tree::Crumbs, TransferRequest)> {
        let element = self.elements.get(&item.element_id)?;
        let crumbs = self.crumbs.sub(element.item_crumb);
        let request = TransferRequest {
            new_owner:     self.self_id,
            to_transfer:   *item.child_id,
            whole_subtree: true,
        };
        Some((crumbs, request))
    }

    fn receive_ownership_of_dragged_item(
        &mut self,
        req: TransferRequest,
        owned_subtree: Vec<(WidgetIdentity, TreeNode)>,
    ) {
        let as_full = ElementIdentity::FullIdentity(req.to_transfer);
        let as_unique = ElementIdentity::UniqueBase(req.to_transfer.base);
        let element_id = if self.elements.contains_key(&as_unique) { as_unique } else { as_full };
        let element = self.elements.remove(&element_id);
        if let Some(element) = element {
            let expression = self.expression[element.expr_range.clone()].to_owned();
            let drag_data =
                DragData { child_id: req.to_transfer, element, expression, owned_subtree };
            self.drag_data_rc.replace(Some(drag_data));
        } else {
            error!("Grabbed item not found.");
        }
    }

    fn on_item_added(
        &mut self,
        item: ListItem,
        at: usize,
    ) -> Option<(span_tree::Crumbs, Option<ImString>)> {
        let element_id = *item.element_id;
        let drag_data = item.take_drag_data()?;
        let expression: ImString = drag_data.expression.into();
        self.elements.insert(element_id, drag_data.element);
        match &self.insertion_indices[..] {
            &[] => Some((self.crumbs.clone(), Some(expression))),
            ids => ids.get(at).map(|idx| (self.crumbs.sub(*idx), Some(expression))),
        }
    }

    fn on_new_item(&mut self, at: usize, frp: &WidgetsFrp) {
        let (mut expression, import) = match &self.default_value {
            DefaultValue::Tag(tag) =>
                (tag.expression.clone().into(), tag.required_import.as_ref().map(ImString::from)),
            DefaultValue::Expression(expr) => (expr.clone(), None),
            DefaultValue::StaticExpression(expr) => (expr.into(), None),
        };

        if self.insert_with_brackets {
            expression = format!("[{expression}]").into();
        }

        let insertion = match &self.insertion_indices[..] {
            &[] => Some((self.crumbs.clone(), Some(expression))),
            ids => ids.get(at).map(|idx| (self.crumbs.sub(*idx), Some(expression))),
        };

        if let Some(insertion) = insertion {
            if let Some(import) = import {
                frp.request_import.emit(import);
            }
            frp.value_changed.emit(insertion);
        }
    }

    fn set_side_margin(&self, side_margin: f32) {
        match self.append_insertion_point.as_ref() {
            Some(insert) => {
                set_margins(insert, side_margin, 0.0);
                set_margins(self.list.display_object(), side_margin, 0.0);
            }
            None => {
                set_margins(self.list.display_object(), side_margin, side_margin);
            }
        }
    }
}

fn set_margins(object: &object::Instance, left: f32, right: f32) {
    let margin = object.margin().x();
    let current_left = margin.start.as_pixels();
    let current_right = margin.end.as_pixels();
    if current_left != Some(left) || current_right != Some(right) {
        object.set_margin_left(left);
        object.set_margin_right(right);
    }
}

#[derive(Debug, Clone, PartialEq)]
/// VectorEditor widget configuration options.
pub struct Config {
    /// Configuration of inner element widgets. If not present, the child widget types have to be
    /// automatically inferred.
    #[allow(dead_code)]
    pub item_widget:  Option<Rc<Configuration>>,
    /// Default expression to insert when adding new elements.
    #[allow(dead_code)]
    pub item_default: DefaultValue,
}

/// The value to insert when adding a new element using an "plus" cursor.
#[derive(Debug, Clone, PartialEq)]
pub enum DefaultValue {
    /// Use a tag value, both inserting its expression and requesting import if necessary.
    Tag(span_tree::TagValue),
    /// Use an arbitrary expression.
    Expression(ImString),
    /// Use a statically defined expression. Allows not allocating a new string for each new
    /// widget on every reconfigure.
    StaticExpression(&'static str),
}

impl Default for DefaultValue {
    fn default() -> Self {
        DefaultValue::StaticExpression("_")
    }
}

impl From<ImString> for DefaultValue {
    fn from(s: ImString) -> Self {
        DefaultValue::Expression(s)
    }
}

impl SpanWidget for Widget {
    type Config = Config;

    fn match_node(ctx: &ConfigContext) -> Score {
        let is_placeholder = ctx.span_node.is_expected_argument();
        let decl_type = ctx.span_node.kind.tp().map(|t| t.as_str());

        let first_decl_is_vector =
            || decl_type.map_or(false, |t| t.trim_start_matches('(').starts_with(VECTOR_TYPE));

        let type_may_be_vector = || {
            let usage_type = ctx.info.usage_type.as_ref().map(|t| t.as_str());
            decl_type.map_or(false, |t| t.contains(VECTOR_TYPE))
                || usage_type.map_or(false, |t| t.contains(VECTOR_TYPE))
        };

        let is_vector_expression = || {
            let node_expr = ctx.span_expression();
            let looks_like_vector = node_expr.starts_with('[')
                && node_expr.ends_with(']')
                && matches!(ctx.span_node.tree_type, Some(ast::TreeType::Expression));

            // We now know that the node expression begins with `[`. Check if the node actually
            // contains a token as a direct child at its location. It will not be the case for
            // expressions that contain nested arrays within the first chain element, but are not
            // an array themselves (e.g. `[1, 2] + [3, 4]`).
            let children = &ctx.span_node.children;
            looks_like_vector
                && children.iter().take_while(|c| c.parent_offset.value == 0).any(|c| c.is_token())
        };

        match () {
            _ if ctx.info.connection.is_some() => Score::Mismatch,
            _ if is_placeholder && first_decl_is_vector() => Score::Perfect,
            _ if is_placeholder && type_may_be_vector() => Score::Good,
            _ if is_placeholder => Score::OnlyOverride,
            _ if is_vector_expression() => Score::Perfect,
            _ => Score::Mismatch,
        }
    }

    fn default_config(ctx: &ConfigContext) -> Configuration<Self::Config> {
        let default_tag = ctx.span_node.kind.tag_values().and_then(|t| t.first());
        let item_default = match default_tag {
            Some(tag) => DefaultValue::Tag(tag.clone()),
            None => {
                let usage_type = ctx.info.usage_type.as_ref().map(|t| t.as_str());
                let decl_type = ctx.span_node.kind.tp().map(|t| t.as_str());
                let decl_or_usage = decl_type.or(usage_type);
                DefaultValue::StaticExpression(infer_default_value_from_type(decl_or_usage))
            }
        };
        Configuration::always(Config { item_widget: None, item_default })
    }

    fn new(_: &Config, ctx: &ConfigContext) -> Self {
        let display_object = object::Instance::new_named("widget::ListEditor");
        let model = Model::new(ctx, &display_object);
        let network = frp::Network::new("widget::ListEditor");
        let reconfigured = network.any_mut("reconfigured");
        Self { display_object, network, reconfigured, model: Rc::new(RefCell::new(model)) }
            .init_list_updates(ctx)
    }

    fn configure(&mut self, cfg: &Config, ctx: ConfigContext) {
        let mut model = self.model.borrow_mut();
        model.configure(&self.display_object, cfg, ctx);
        self.reconfigured.emit(());
    }

    fn receive_ownership(&mut self, req: TransferRequest, nodes: Vec<(WidgetIdentity, TreeNode)>) {
        let mut model = self.model.borrow_mut();
        model.receive_ownership_of_dragged_item(req, nodes);
    }
}

#[derive(PartialEq)]
enum DiffOp<'old, 'new, O, N> {
    Delete { at: usize, old: &'old O, present_later: Option<usize> },
    Insert { at: usize, new: &'new N },
    Update { at: usize, old: &'old O, new: &'new N },
}

fn list_diff<'old, 'new, O, N>(
    old_elements: &'old [O],
    new_elements: &'new [N],
    cmp: impl Fn(&O, &N) -> bool,
    mut f: impl FnMut(DiffOp<'old, 'new, O, N>),
) {
    // Indices for next elements to process in both lists.
    let mut current_old = 0;
    let mut current_new = 0;

    // The current index of insertion or deletion in the list that is being processed. Effectively
    // the number of insertions or equal pairs that have been processed so far.
    let mut at = 0;

    while current_old < old_elements.len() && current_new < new_elements.len() {
        let old = &old_elements[current_old];
        let new = &new_elements[current_new];

        // Next pair of elements are equal, so we don't need to do anything.
        if cmp(old, new) {
            f(DiffOp::Update { at, old, new });
            current_old += 1;
            current_new += 1;
            at += 1;
            continue;
        }

        let remaining_old = &old_elements[current_old + 1..];
        let remaining_new = &new_elements[current_new + 1..];

        let old_still_in_new_list = remaining_new.iter().any(|new| cmp(old, new));
        if !old_still_in_new_list {
            f(DiffOp::Delete { at, old, present_later: None });
            current_old += 1;
            continue;
        }

        let index_in_remaining_old = remaining_old.iter().position(|old| cmp(old, new));
        match index_in_remaining_old {
            // Not present in old, thus it is an insertion.
            None => {
                f(DiffOp::Insert { at, new });
                at += 1;
                current_new += 1;
                continue;
            }
            // Present in old. Delete all elements in between and insert the matching one.
            Some(advance) => {
                f(DiffOp::Delete { at, old, present_later: Some(current_old + advance + 1) });
                for k in 0..advance {
                    let present_later = remaining_old[k + 1..].iter().position(|old| cmp(old, new));
                    f(DiffOp::Delete { at, old: &remaining_old[k], present_later });
                }
                current_old += advance + 1;
                let old = &old_elements[current_old];
                let new = &new_elements[current_new];
                f(DiffOp::Update { at, old, new });
                current_old += 1;
                current_new += 1;
                at += 1;
                continue;
            }
        }
    }
    while current_old < old_elements.len() {
        f(DiffOp::Delete { at, old: &old_elements[current_old], present_later: None });
        current_old += 1;
    }
    while current_new < new_elements.len() {
        f(DiffOp::Insert { at, new: &new_elements[current_new] });
        current_new += 1;
        at += 1;
    }
}


// =================
// === Extension ===
// =================

#[derive(Clone, Copy, Debug, Default)]
struct Extension {
    already_in_list: bool,
}

// ======================
// === Type inference ===
// ======================

/// Given an optional expected argument type, infer the default vector insertion value.
pub fn infer_default_value_from_type(typename: Option<&str>) -> &'static str {
    let variant = typename.map_or(DefaultVariant::NotDefined, |typename| {
        let possible_types = typename
            .split(split_type_groups())
            .map(remove_outer_parentheses)
            .filter_map(|t| t.strip_prefix(VECTOR_TYPE))
            .flat_map(|t| t.split(split_type_groups()).map(remove_outer_parentheses));

        possible_types.fold(DefaultVariant::NotDefined, |acc, ty| acc.fold(ty))
    });
    variant.to_default_value()
}

fn remove_outer_parentheses(s: &str) -> &str {
    let s = s.trim();
    s.strip_prefix('(').and_then(|s| s.strip_suffix(')')).map(|s| s.trim()).unwrap_or(s)
}

fn split_type_groups() -> impl FnMut(char) -> bool {
    let mut depth = 0i32;
    move |c| match c {
        '(' => {
            depth += 1;
            false
        }
        ')' => {
            depth -= 1;
            false
        }
        '|' if depth == 0 => true,
        _ => false,
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
enum DefaultVariant {
    NotDefined,
    Numeric,
    Boolean,
    Text,
    Any,
}

impl DefaultVariant {
    fn from_single(ty: &str) -> Option<Self> {
        match ty.strip_prefix("Standard.Base.Data.")? {
            "Numbers.Integer" | "Numbers.Decimal" | "Numbers.Number" => Some(Self::Numeric),
            "Boolean.Boolean" => Some(Self::Boolean),
            "Text.Text" => Some(Self::Text),
            "Any" => Some(Self::Any),
            _ => None,
        }
    }

    fn fold(self, ty: &str) -> Self {
        match (self, Self::from_single(ty)) {
            (a, None) => a,
            (a, Some(b)) if a == b => a,
            (Self::NotDefined, Some(b)) => b,
            (Self::Text, _) | (_, Some(Self::Text)) => Self::Text,
            _ => DefaultVariant::Any,
        }
    }

    fn to_default_value(self) -> &'static str {
        match self {
            Self::Numeric => "0",
            Self::Boolean => "False",
            Self::Text => "''",
            Self::Any | Self::NotDefined => "_",
        }
    }
}
