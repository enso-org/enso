//! Module dedicated to the List Editor widget. The main structure is [`Model`] which is one of
//! the [KindModel](crate::component::node::widget::KindModel) variants.
//!
//! Currently the view is a simple [`Elements`] component, which will be replaced with a rich
//! view in [future tasks](https://github.com/enso-org/enso/issues/5631).

use crate::prelude::*;

use crate::component::node::input::widget::single_choice::triangle;
use crate::component::node::input::widget::single_choice::ACTIVATION_SHAPE_SIZE;
use crate::component::node::input::widget::Configuration;
use crate::component::node::input::widget::TransferRequest;
use crate::component::node::input::widget::TreeNode;
use crate::component::node::input::widget::WidgetIdentity;
use crate::component::node::input::widget::WidgetsFrp;

use crate::component::node::input::area::TEXT_SIZE;
use ensogl::application::Application;
use ensogl::control::io::mouse;
use ensogl::display;
use ensogl::display::object::event;
use ensogl::display::shape::StyleWatch;
use ensogl::display::world::with_context;
use ensogl_component::list_editor::ListEditor;
use ensogl_component::text::Text;
use ensogl_hardcoded_theme as theme;
use span_tree::node::InsertionPointType;


// ==============
// === Widget ===
// ==============

ensogl::define_endpoints_2! {
    Input {
        current_value(Option<ImString>),
        elements_default_value(ImString),
    }
}

#[derive(Clone, CloneRef, Debug)]
struct Element {
    widget_id:              Immutable<WidgetIdentity>,
    code:                   ImString,
    display_object:         display::object::Instance,
    content:                display::object::Instance,
    background:             display::shape::Rectangle,
    insertion_point_before: display::object::Instance,
    element:                display::object::Instance,
    owned_subtree:          Rc<RefCell<Vec<(WidgetIdentity, TreeNode)>>>,
}

impl Element {
    fn new(
        widget_id: WidgetIdentity,
        code: ImString,
        element: display::object::Instance,
        insertion_point_before: display::object::Instance,
    ) -> Self {
        let widget_id = Immutable(widget_id);
        let display_object = display::object::Instance::new();
        let content = display::object::Instance::new();
        let background = display::shape::Rectangle::new();
        background.set_color(display::shape::INVISIBLE_HOVER_COLOR);
        background.allow_grow().set_alignment_left_center();
        content.use_auto_layout();
        content.set_children_alignment_left_center().justify_content_center_y();
        content.replace_children(&[&insertion_point_before, &element]);
        display_object.replace_children(&[background.display_object(), &content]);
        with_context(|ctx| ctx.layers.label.add(&background));
        Self {
            widget_id,
            code,
            display_object,
            content,
            background,
            element,
            insertion_point_before,
            owned_subtree: default(),
        }
    }

    fn update(
        &mut self,
        code: ImString,
        element: display::object::Instance,
        insertion_point_before: display::object::Instance,
    ) {
        self.content.replace_children(&[&insertion_point_before, &element]);
        self.insertion_point_before = insertion_point_before;
        self.element = element;
        self.code = code;
        self.owned_subtree.take();
    }

    fn take_ownership(&self, owned: Vec<(WidgetIdentity, TreeNode)>) {
        *self.owned_subtree.borrow_mut() = owned;
    }
}

impl display::Object for Element {
    fn display_object(&self) -> &display::object::Instance {
        &self.display_object
    }
}

/// A model for the vector editor widget.
///
/// Currently it displays an activation shape (a triangle) which, on click, displays the widget
/// view. The view is a [`ListEditor`] - see its documentation for available GUI actions. Currently
/// only adding new elements is supported.
///
/// The component does not handle nested arrays well. They should be fixed once [integrated into
/// new widget hierarchy](https://github.com/enso-org/enso/issues/5923).
#[derive(Clone, Debug)]
pub struct Widget {
    config_frp:             Frp,
    display_object:         display::object::Instance,
    list:                   ListEditor<Element>,
    insertion_point_append: Option<display::object::Instance>,
    crumbs_to_insert:       Rc<RefCell<HashMap<usize, span_tree::Crumbs>>>,
    crumbs_to_remove:       Rc<RefCell<HashMap<usize, span_tree::Crumbs>>>,
    elements:               HashMap<WidgetIdentity, Element>,
}

impl Widget {
    /// A gap between the `activation_shape` and `elements` view.
    const GAP: f32 = 3.0;

    fn init_list_updates(self, ctx: &super::ConfigContext, widgets_frp: &WidgetsFrp) -> Self {
        let config_frp = &self.config_frp;
        let network = &config_frp.network;
        let list = &self.list;
        let crumbs_to_insert = &self.crumbs_to_insert;
        let crumbs_to_remove = &self.crumbs_to_remove;
        let my_id = ctx.info.identity;

        frp::extend! { network
            // Inserting elements.
            inserted_by_user <- list.on_item_added.filter_map(|resp| resp.clone().gui_interaction_payload());
            code_inserted_by_user <- inserted_by_user.filter_map(f!([list](index) list.items().get(*index).map(|e| e.code.clone_ref())));
            requested_insert <- list.request_new_item.filter_map(|resp| resp.clone().gui_interaction_payload());
            default_code <- config_frp.elements_default_value.sample(&requested_insert);
            code_to_insert <- any(code_inserted_by_user, default_code);
            insert <- any(inserted_by_user, requested_insert);
            insert_st_crumb <- insert.filter_map(f!((index) crumbs_to_insert.borrow().get(index).cloned()));
            widgets_frp.value_changed <+ insert_st_crumb.map2(&code_to_insert, |crumb, val| (crumb.clone(), Some(val.clone_ref())));

            // Removing elements.
            removed_by_user <- list.on_item_removed.filter_map(|resp| resp.clone().gui_interaction_payload());
            widgets_frp.transfer_ownership <+ removed_by_user.filter_map(move |(_, element)| Some(TransferRequest {
                to_transfer: *element.borrow().as_ref()?.widget_id,
                new_owner: my_id,
                whole_subtree: true,
            }));
            remove <- removed_by_user._0();
            remove_st_crumb <- remove.filter_map(f!((index) crumbs_to_remove.borrow().get(index).cloned()));
            widgets_frp.value_changed <+ remove_st_crumb.map(|crumb| (crumb.clone(), None));
        }
        self
    }

    fn configure_insertion_point(&mut self, cfg: &Config, ctx: super::ConfigContext) {
        self.config_frp.elements_default_value(format!("[{}]", cfg.item_default));

        *self.crumbs_to_insert.borrow_mut() =
            [(0, ctx.span_node.crumbs.clone())].into_iter().collect();
        self.crumbs_to_remove.borrow_mut().clear();
        self.elements.clear();
        self.list.replace_list(iter::empty());

        let insertion_point = ctx.builder.child_widget_of_type(
            ctx.span_node,
            ctx.info.nesting_level,
            Some(&super::Configuration::always(super::label::Config)),
        );
        self.display_object
            .replace_children(&[self.list.display_object(), &insertion_point.root_object]);
        self.insertion_point_append = Some(insertion_point.root_object);
    }

    fn configure_vector(&mut self, cfg: &Config, ctx: super::ConfigContext) {
        self.config_frp.elements_default_value(&cfg.item_default);

        let child_level = ctx.info.nesting_level.next_if(ctx.span_node.is_argument());

        let left_token =
            ctx.span_node.clone().children_iter().next().filter(|node| node.is_token());
        let left_token_widget =
            left_token.map(|node| ctx.builder.child_widget(node, child_level).root_object);
        let right_token =
            ctx.span_node.clone().children_iter().last().filter(|node| node.is_token());
        let right_token_widget =
            right_token.map(|node| ctx.builder.child_widget(node, child_level).root_object);

        let element_nodes = ctx.span_node.clone().children_iter().filter(|node| node.is_argument());
        let mut insertion_points =
            ctx.span_node.clone().children_iter().filter(|node| node.is_insertion_point()).fuse();
        let mut old_elements = mem::take(&mut self.elements);
        let children =
            element_nodes.zip(&mut insertion_points).map(|(element_node, insertion_point)| {
                let insertion_point_before = ctx.builder.child_widget(insertion_point, child_level);
                let code = ImString::new(ctx.expression_at(element_node.span()));
                let element = ctx.builder.child_widget_of_type(
                    element_node,
                    child_level,
                    cfg.item_widget.as_deref(),
                );
                let element = match old_elements.remove(&element.id) {
                    Some(mut existing) => {
                        existing.update(
                            code,
                            element.root_object,
                            insertion_point_before.root_object,
                        );
                        existing
                    }
                    None => Element::new(
                        element.id,
                        code,
                        element.root_object,
                        insertion_point_before.root_object,
                    ),
                };
                self.elements.insert(*element.widget_id, element.clone_ref());
                element
            });
        self.list.replace_list(children);
        self.insertion_point_append = insertion_points
            .next()
            .map(|node| ctx.builder.child_widget(node, child_level).root_object);
        let display_children = left_token_widget
            .as_ref()
            .into_iter()
            .chain(iter::once(self.list.display_object()))
            .chain(self.insertion_point_append.as_ref())
            .chain(right_token_widget.as_ref());
        self.display_object.replace_children(&display_children.collect_vec());

        let crumbs_to_insert = ctx
            .span_node
            .clone()
            .children_iter()
            .filter(|node| node.is_insertion_point())
            .enumerate()
            .map(|(index, node)| (index, node.crumbs.clone()))
            .collect();
        *self.crumbs_to_insert.borrow_mut() = crumbs_to_insert;

        let crumbs_to_remove = ctx
            .span_node
            .children_iter()
            .filter(|node| node.is_argument())
            .enumerate()
            .map(|(index, node)| (index, node.crumbs.clone()))
            .collect();
        *self.crumbs_to_remove.borrow_mut() = crumbs_to_remove
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

    fn root_object(&self) -> &display::object::Instance {
        &self.display_object
    }

    fn new(_: &Config, ctx: &super::ConfigContext) -> Self {
        console_log!("NEW");
        let display_object = display::object::Instance::new();
        let list = ListEditor::new(&ctx.app().cursor);
        // list.push(Element::new(default(), default(), default(), default()));
        // list.replace_list(iter::empty());
        list.set_size_hug_y(TEXT_SIZE).allow_grow_y();
        display_object.use_auto_layout().set_alignment_left_center();
        display_object.add_child(&list);

        let widgets_frp = &ctx.builder.frp;
        Self {
            config_frp: Frp::new(),
            display_object,
            list,
            insertion_point_append: None,
            crumbs_to_insert: default(),
            crumbs_to_remove: default(),
            elements: default(),
        }
        .init_list_updates(ctx, widgets_frp)
    }

    fn configure(&mut self, cfg: &Config, ctx: super::ConfigContext) {
        console_log!("CONFIGURE");
        let current_value: Option<ImString> = Some(ctx.expression_at(ctx.span_node.span()).into());
        self.config_frp.current_value(current_value);
        if ctx.span_node.is_insertion_point() {
            self.configure_insertion_point(cfg, ctx)
        } else {
            self.configure_vector(cfg, ctx)
        }
    }

    fn receive_ownership(&mut self, req: TransferRequest, nodes: Vec<(WidgetIdentity, TreeNode)>) {
        if let Some(element) = self.elements.get(&req.to_transfer) {
            element.take_ownership(nodes);
        }
    }
}
