//! Module dedicated to the List Editor widget. The main structure is [`Model`] which is one of
//! the [KindModel](crate::component::node::widget::KindModel) variants.
//!
//! Currently the view is a simple [`Elements`] component, which will be replaced with a rich
//! view in [future tasks](https://github.com/enso-org/enso/issues/5631).

use crate::prelude::*;

use crate::component::node::input::widget::single_choice::triangle;
use crate::component::node::input::widget::single_choice::ACTIVATION_SHAPE_SIZE;
use crate::component::node::input::widget::Configuration;
use crate::component::node::input::widget::WidgetIdentity;
use crate::component::node::input::widget::WidgetsFrp;

use ensogl::application::Application;
use ensogl::control::io::mouse;
use ensogl::display;
use ensogl::display::object::event;
use ensogl::display::shape::StyleWatch;
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
    insertion_point_before: display::object::Instance,
    element:                display::object::Instance,
}

impl display::Object for Element {
    fn display_object(&self) -> &display::object::Instance {
        self.element.display_object()
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
}

impl Widget {
    /// A gap between the `activation_shape` and `elements` view.
    const GAP: f32 = 3.0;

    fn init_list_updates(self, widgets_frp: &WidgetsFrp) -> Self {
        let config_frp = &self.config_frp;
        let network = &config_frp.network;
        let list = &self.list;
        let crumbs_to_insert = &self.crumbs_to_insert;
        let crumbs_to_remove = &self.crumbs_to_remove;


        frp::extend! { network
            // Inserting elements.
            inserted_by_user <- list.on_item_added.filter_map(|resp| resp.clone().gui_interaction_payload());
            requested_insert <- list.request_new_item.filter_map(|resp| resp.clone().gui_interaction_payload());
            insert <- any(inserted_by_user, requested_insert);
            insert_st_crumb <- insert.filter_map(f!((index) crumbs_to_insert.borrow().get(index).cloned()));
            widgets_frp.value_changed <+ insert_st_crumb.map2(&config_frp.elements_default_value, |crumb, val| (crumb.clone(), Some(val.clone_ref())));

            // Removing elements.
            removed_by_user <- list.on_item_removed.filter_map(|resp| resp.clone().gui_interaction_payload());
            remove <- removed_by_user._0();
            remove_st_crumb <- remove.filter_map(f!((index) crumbs_to_remove.borrow().get(index).cloned()));
            widgets_frp.value_changed <+ remove_st_crumb.map(|crumb| (crumb.clone(), None));
        }
        self
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
        let display_object = display::object::Instance::new();
        let list = ListEditor::new(&ctx.app().cursor);
        display_object.use_auto_layout();
        display_object.add_child(&list);

        let widgets_frp = &ctx.builder.frp;
        Self {
            config_frp: Frp::new(),
            display_object,
            list,
            insertion_point_append: None,
            crumbs_to_insert: default(),
            crumbs_to_remove: default(),
        }
        .init_list_updates(widgets_frp)
    }

    fn configure(&mut self, cfg: &Config, ctx: super::ConfigContext) {
        let current_value: Option<ImString> = Some(ctx.expression_at(ctx.span_node.span()).into());
        self.config_frp.current_value(current_value);
        self.config_frp.elements_default_value(&cfg.item_default);

        let child_level = ctx.info.nesting_level.next_if(ctx.span_node.is_argument());

        let element_nodes = ctx.span_node.clone().children_iter().filter(|node| node.is_argument());
        let mut insertion_points =
            ctx.span_node.clone().children_iter().filter(|node| node.is_insertion_point()).fuse();
        let children =
            element_nodes.zip(&mut insertion_points).map(|(element_node, insertion_point)| {
                let insertion_point_before = ctx.builder.child_widget(insertion_point, child_level);
                let element = ctx.builder.child_widget_of_type(
                    element_node,
                    child_level,
                    cfg.item_widget.as_deref(),
                );
                element.add_child(&insertion_point_before.root_object);
                Element {
                    element:                element.root_object,
                    insertion_point_before: insertion_point_before.root_object,
                }
            });
        self.list.replace_list(children);
        if let Some(insertion_point) = insertion_points.next() {
            let insertion_point_append = ctx.builder.child_widget(insertion_point, child_level);
            self.display_object.add_child(&insertion_point_append.root_object);
            self.insertion_point_append = Some(insertion_point_append.root_object);
        }

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
