//! Module dedicated to the [List Editor widget](Widget).

// FIXME[ao]: This code miss important documentation (e.g. for `Element`, `DragData` and `ListItem`)
//  and may be unreadable at some places. It should be improved in several next debugging PRs.

use crate::prelude::*;

use crate::component::node::input::area::TEXT_SIZE;
use crate::component::node::input::widget::Configuration;
use crate::component::node::input::widget::TransferRequest;
use crate::component::node::input::widget::TreeNode;
use crate::component::node::input::widget::WidgetIdentity;

use ensogl::display;
use ensogl::display::object;
use ensogl::display::world::with_context;
use ensogl_component::list_editor::ListEditor;
use span_tree::node::Kind;
use std::fmt::Write;



// ===============
// === Element ===
// ===============

#[derive(Debug)]
struct Element {
    display_object: object::Instance,
    content:        object::Instance,
    #[allow(dead_code)]
    background:     display::shape::Rectangle,
    expr_range:     Range<usize>,
    item_crumb:     usize,
    alive:          Option<()>,
}

#[derive(Debug)]
struct DragData {
    element_id:    WidgetIdentity,
    element:       Element,
    expression:    String,
    #[allow(dead_code)]
    owned_subtree: Vec<(WidgetIdentity, TreeNode)>,
}

#[derive(Debug, Clone, CloneRef)]
struct ListItem {
    element_id:     Immutable<WidgetIdentity>,
    display_object: object::Instance,
    drag_data:      Rc<RefCell<Option<DragData>>>,
}

impl PartialEq for ListItem {
    fn eq(&self, other: &Self) -> bool {
        self.element_id == other.element_id
    }
}

impl ListItem {
    fn take_drag_data(&self) -> Option<DragData> {
        let mut borrow = self.drag_data.borrow_mut();
        let can_take = matches!(&*borrow, Some(data) if data.element_id == *self.element_id);
        can_take.and_option_from(|| borrow.take())
    }
}

impl display::Object for ListItem {
    fn display_object(&self) -> &object::Instance {
        &self.display_object
    }
}

impl Element {
    fn new() -> Self {
        let display_object = object::Instance::new_named("Element");
        let content = object::Instance::new_named("Content");
        let background = display::shape::Rectangle::new();
        background.set_color(display::shape::INVISIBLE_HOVER_COLOR);
        background.allow_grow().set_alignment_left_center();
        content.use_auto_layout().set_children_alignment_left_center();
        with_context(|ctx| ctx.layers.label.add(&background));
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

impl display::Object for Element {
    fn display_object(&self) -> &object::Instance {
        &self.display_object
    }
}

/// A model for the vector editor widget.
#[derive(Clone, CloneRef, Debug)]
pub struct Widget {
    display_object: object::Instance,
    network:        frp::Network,
    model:          Rc<RefCell<Model>>,
}

impl Widget {
    fn init_list_updates(self, ctx: &super::ConfigContext) -> Self {
        let widgets_frp = ctx.frp();
        let network = &self.network;
        let model = &self.model;
        let list = self.model.borrow().list.clone_ref();

        frp::extend! { network
            // Adding elements.
            requested_new <- list.request_new_item.filter_map(|resp| resp.gui_interaction_payload());
            widgets_frp.value_changed <+ requested_new.filter_map(f!((idx) model.borrow_mut().on_new_item(*idx)));

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
        }
        self
    }
}

#[derive(Debug)]
struct Model {
    self_id:           WidgetIdentity,
    list:              ListEditor<ListItem>,
    elements:          HashMap<WidgetIdentity, Element>,
    default_value:     String,
    expression:        String,
    crumbs:            span_tree::Crumbs,
    drag_data_rc:      Rc<RefCell<Option<DragData>>>,
    received_drag:     Option<DragData>,
    insertion_indices: Vec<usize>,
}

impl Model {
    fn new(ctx: &super::ConfigContext, display_object: &object::Instance) -> Self {
        let list = ListEditor::new(&ctx.app().cursor);
        list.set_size_hug_y(TEXT_SIZE).allow_grow_y();
        display_object.use_auto_layout().set_children_alignment_left_center();
        Self {
            self_id: ctx.info.identity,
            list,
            elements: default(),
            default_value: default(),
            expression: default(),
            crumbs: default(),
            drag_data_rc: default(),
            received_drag: default(),
            insertion_indices: default(),
        }
    }

    fn configure(&mut self, root: &object::Instance, cfg: &Config, mut ctx: super::ConfigContext) {
        self.expression.clear();
        self.default_value.clear();
        self.expression.push_str(ctx.expression_at(ctx.span_node.span()));
        self.crumbs = ctx.span_node.crumbs.clone();

        // Right now, nested list editors are broken. Prevent them from being created. Whenever
        // a nested list editor is requested, we instead use a hierarchical widget to display the
        // child list items as ordinary expressions.
        let ext = ctx.get_extension_or_default::<Extension>();
        let already_in_list = ext.already_in_list;
        ctx.set_extension(Extension { already_in_list: true });

        if already_in_list {
            let child = ctx.builder.child_widget_of_type(
                ctx.span_node,
                ctx.info.nesting_level,
                Some(&super::Configuration::always(super::hierarchy::Config)),
            );
            root.replace_children(&[&child.root_object]);
        } else if ctx.span_node.is_insertion_point() {
            write!(self.default_value, "[{}]", cfg.item_default).unwrap();
            self.configure_insertion_point(root, ctx)
        } else {
            self.default_value.push_str(&cfg.item_default);
            self.configure_vector(root, cfg, ctx)
        }
    }

    fn configure_insertion_point(&mut self, root: &object::Instance, ctx: super::ConfigContext) {
        self.elements.clear();
        let insertion_point = ctx.builder.child_widget_of_type(
            ctx.span_node,
            ctx.info.nesting_level,
            Some(&super::Configuration::always(super::label::Config)),
        );
        root.replace_children(&[self.list.display_object(), &*insertion_point]);
    }

    fn configure_vector(
        &mut self,
        root: &object::Instance,
        cfg: &Config,
        ctx: super::ConfigContext,
    ) {
        let no_nest = ctx.info.nesting_level;
        let nest = ctx.info.nesting_level.next();
        let child_config = cfg.item_widget.as_deref();

        let mut build_child_widget = |i, nest, config, allow_margin: bool| {
            let mut node = ctx.span_node.clone().child(i).expect("invalid index");
            if !allow_margin {
                node.sibling_offset = 0.into();
            }
            ctx.builder.child_widget_of_type(node, nest, config)
        };

        let mut open_bracket = None;
        let mut close_bracket = None;
        let mut last_insert_crumb = None;
        let mut list_items = SmallVec::<[ListItem; 16]>::new();

        self.insertion_indices.clear();
        for (index, child) in ctx.span_node.node.children.iter().enumerate() {
            let node = &child.node;
            let start = child.parent_offset.value as usize;
            let range = start..start + node.size.value as usize;
            let expr = &self.expression[range.clone()];

            match node.kind {
                Kind::Token if expr == "[" && open_bracket.is_none() => {
                    let child = build_child_widget(index, no_nest, None, true);
                    open_bracket = Some(child.root_object);
                }
                Kind::Token if expr == "]" && close_bracket.is_none() => {
                    let child = build_child_widget(index, no_nest, None, false);
                    close_bracket = Some(child.root_object);
                }
                Kind::InsertionPoint(_) => {
                    last_insert_crumb = Some(index);
                }
                Kind::Argument(_) if last_insert_crumb.is_some() => {
                    let insert_index = last_insert_crumb.take().unwrap();
                    let insert = build_child_widget(insert_index, no_nest, None, false);
                    let item = build_child_widget(index, nest, child_config, false);
                    let element = self.elements.entry(item.id).or_insert_with(|| {
                        self.received_drag.take().map_or_else(Element::new, |d| d.element)
                    });
                    element.alive = Some(());
                    element.item_crumb = index;
                    element.expr_range = range;
                    element.content.replace_children(&[&*insert, &*item]);
                    self.insertion_indices.push(insert_index);
                    list_items.push(ListItem {
                        element_id:     Immutable(item.id),
                        display_object: element.display_object.clone(),
                        drag_data:      self.drag_data_rc.clone(),
                    });
                }
                _ => {}
            }
        }

        self.elements.retain(|_, child| child.alive.take().is_some());
        self.insertion_indices.extend(last_insert_crumb);

        let current_items = self.list.items();
        list_diff(&current_items, &list_items, |op| match op {
            DiffOp::Delete { at, old, present_later } =>
                if present_later.is_some()
                    || list_items.iter().any(|i| i.display_object == old.display_object)
                {
                    self.list.take_item(at);
                } else {
                    self.list.remove(at);
                },
            DiffOp::Insert { at, new } => {
                self.list.insert_item(at, new.clone_ref());
            }
            DiffOp::Update { at, old, new } =>
                if old.display_object() != new.display_object() {
                    self.list.replace_item(at, new.clone_ref());
                },
        });

        let append_insert = last_insert_crumb
            .map(|index| build_child_widget(index, no_nest, None, false).root_object);
        let (open_bracket, close_bracket) = open_bracket.zip(close_bracket).unzip();
        let mut children = SmallVec::<[&object::Instance; 4]>::new();
        children.extend(&open_bracket);
        children.push(self.list.display_object());
        children.extend(&append_insert);
        children.extend(&close_bracket);
        root.replace_children(&children);
    }

    fn on_item_removed(&mut self, item: ListItem) -> Option<(span_tree::Crumbs, TransferRequest)> {
        let element = self.elements.get(&item.element_id)?;
        let crumbs = self.crumbs.sub(element.item_crumb);
        let request = TransferRequest {
            new_owner:     self.self_id,
            to_transfer:   *item.element_id,
            whole_subtree: true,
        };
        Some((crumbs, request))
    }

    fn receive_ownership_of_dragged_item(
        &mut self,
        req: TransferRequest,
        owned_subtree: Vec<(WidgetIdentity, TreeNode)>,
    ) {
        let element_id = req.to_transfer;
        let element = self.elements.remove(&element_id);
        if let Some(element) = element {
            let expression = self.expression[element.expr_range.clone()].to_owned();
            let drag_data = DragData { element_id, element, expression, owned_subtree };
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
        self.received_drag = item.take_drag_data();
        let expression: ImString = mem::take(&mut self.received_drag.as_mut()?.expression).into();
        match &self.insertion_indices[..] {
            &[] => Some((self.crumbs.clone(), Some(expression))),
            ids => ids.get(at).map(|idx| (self.crumbs.sub(*idx), Some(expression))),
        }
    }

    fn on_new_item(&mut self, at: usize) -> Option<(span_tree::Crumbs, Option<ImString>)> {
        let expression: ImString = self.default_value.clone().into();
        match &self.insertion_indices[..] {
            &[] => Some((self.crumbs.clone(), Some(expression))),
            ids => ids.get(at).map(|idx| (self.crumbs.sub(*idx), Some(expression))),
        }
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
    pub item_default: ImString,
}

impl super::SpanWidget for Widget {
    type Config = Config;

    fn root_object(&self) -> &object::Instance {
        &self.display_object
    }

    fn new(_: &Config, ctx: &super::ConfigContext) -> Self {
        console_log!("NEW");
        let display_object = object::Instance::new_named("widget::ListEditor");
        let model = Model::new(ctx, &display_object);
        let network = frp::Network::new("widget::ListEditor");
        Self { display_object, network, model: Rc::new(RefCell::new(model)) }.init_list_updates(ctx)
    }

    fn configure(&mut self, cfg: &Config, ctx: super::ConfigContext) {
        console_log!("CONFIGURE");
        let mut model = self.model.borrow_mut();
        model.configure(&self.display_object, cfg, ctx);
    }

    fn receive_ownership(&mut self, req: TransferRequest, nodes: Vec<(WidgetIdentity, TreeNode)>) {
        let mut model = self.model.borrow_mut();
        model.receive_ownership_of_dragged_item(req, nodes);
    }
}

#[derive(PartialEq)]
enum DiffOp<'old, 'new, T> {
    Delete { at: usize, old: &'old T, present_later: Option<usize> },
    Insert { at: usize, new: &'new T },
    Update { at: usize, old: &'old T, new: &'new T },
}

fn list_diff<'old, 'new, T>(
    old_elements: &'old [T],
    new_elements: &'new [T],
    mut f: impl FnMut(DiffOp<'old, 'new, T>),
) where
    T: PartialEq,
{
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
        if old == new {
            f(DiffOp::Update { at, old, new });
            current_old += 1;
            current_new += 1;
            at += 1;
            continue;
        }

        let remaining_old = &old_elements[current_old + 1..];
        let remaining_new = &new_elements[current_new + 1..];

        let old_still_in_new_list = remaining_new.contains(old);
        if !old_still_in_new_list {
            f(DiffOp::Delete { at, old, present_later: None });
            current_old += 1;
            continue;
        }

        let index_in_remaining_old = remaining_old.iter().position(|x| x == new);
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
                    let present_later = remaining_old[k + 1..].iter().position(|old| old == new);
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
