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
    config_frp:       Frp,
    list:             ListEditor<display::object::Instance>,
    insertion_points: Rc<RefCell<HashMap<usize, span_tree::Crumbs>>>,
}

impl Widget {
    /// A gap between the `activation_shape` and `elements` view.
    const GAP: f32 = 3.0;

    fn init_list_updates(self, app: &Application, widgets_frp: &WidgetsFrp) -> Self {
        let config_frp = &self.config_frp;
        let network = &config_frp.network;
        let list = &self.list;
        let insertion_points = &self.insertion_points;



        frp::extend! { TRACE_ALL network
            // Inserting elements.
            trace list.on_item_added;
            trace list.on_item_removed;
            trace list.request_new_item;

            inserted_by_user <- list.on_item_added.filter_map(|resp| resp.clone().gui_interaction_payload());
            inserted_by_user_index <- inserted_by_user._0();
            requested_insert <- list.request_new_item.filter_map(|resp| resp.clone().gui_interaction_payload());
            insert <- any(inserted_by_user_index, requested_insert);
            insert_st_crumb <- insert.filter_map(f!((index) insertion_points.borrow().get(index).cloned()));
            widgets_frp.value_changed <+ insert_st_crumb.map2(&config_frp.elements_default_value, |crumb, val| (crumb.clone(), Some(val.clone_ref())));

            // Removing elements.
            removed_by_user <- list.on_item_removed.filter_map(|resp| resp.clone().gui_interaction_payload());
            remove <- removed_by_user._0();
            remove_st_crumb <- remove.filter_map(f!((index) insertion_points.borrow().get(index).cloned()));
            widgets_frp.value_changed <+ remove_st_crumb.map(|crumb| (crumb.clone(), None));
        }
        self
    }

    fn update_list(&mut self, children: impl Iterator<Item = super::Child>) {
        self.list.replace_list(children.map(|child| child.root_object));
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
        self.list.display_object()
    }

    fn new(_: &Config, ctx: &super::ConfigContext) -> Self {
        let list = ListEditor::new(&ctx.app().cursor);
        let config_frp = Frp::new();
        let insertion_points = default();
        Self { config_frp, list, insertion_points } //.init_list_updates(app, widgets_frp)
    }

    fn configure(&mut self, cfg: &Config, ctx: super::ConfigContext) {
        let current_value: Option<ImString> = Some(ctx.expression_at(ctx.span_node.span()).into());
        self.config_frp.current_value(current_value);
        self.config_frp.elements_default_value(&cfg.item_default);

        let child_level = ctx.info.nesting_level.next_if(ctx.span_node.is_argument());
        let children_iter = ctx.span_node.clone().children_iter();
        let children = children_iter.filter(|node| !node.is_token()).map(|node| {
            ctx.builder.child_widget_of_type(node, child_level, cfg.item_widget.as_deref())
        });
        self.update_list(children);

        let element_count =
            ctx.span_node.clone().children_iter().filter(|node| node.is_argument()).count();
        use span_tree::node::InsertionPoint;
        use span_tree::node::InsertionPointType;
        use span_tree::node::Kind;
        let insertion_points = ctx
            .span_node
            .children_iter()
            .filter_map(|node| match node.kind {
                Kind::InsertionPoint(InsertionPoint {
                    kind: InsertionPointType::BeforeArgument(index),
                    ..
                }) => Some((index, node.crumbs.clone())),
                Kind::InsertionPoint(InsertionPoint {
                    kind: InsertionPointType::Append, ..
                }) => Some((element_count, node.crumbs.clone())),
                _ => None,
            })
            .collect();
        *self.insertion_points.borrow_mut() = insertion_points;
    }
}
