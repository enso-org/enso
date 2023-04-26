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



// ==============
// === Widget ===
// ==============

ensogl::define_endpoints_2! {
    Input {
        current_value(Option<ImString>),
        current_crumbs(span_tree::Crumbs),
    }
}

// #[derive(Clone, CloneRef, Debug, Deref)]
// struct Element {
//     #[deref]
//     display_object: display::object::Instance,
//     widget:         Rc<RefCell<super::Child>>,
// }
//
// impl Element {
//     fn new(widget: super::Child) -> Self {
//         let display_object = display::object::Instance::new();
//         display_object.add_child(&widget.root_object);
//         let widget = Rc::new(RefCell::new(widget));
//         Self { display_object, widget }
//     }
//
//     fn set_widget(&self, widget: super::Child) {
//         self.display_object.replace_children(&[widget.root_object.clone_ref()]);
//         *self.widget.borrow_mut() = widget;
//     }
//
//     fn widget_id(&self) -> WidgetIdentity {
//         self.widget.borrow().id
//     }
// }
//
// impl display::Object for Element {
//     fn display_object(&self) -> &display::object::Instance {
//         &self.display_object
//     }
// }

/// A model for the vector editor widget.
///
/// Currently it displays an activation shape (a triangle) which, on click, displays the widget
/// view. The view is a [`ListEditor`] - see its documentation for available GUI actions. Currently
/// only adding new elements is supported.
///
/// The component does not handle nested arrays well. They should be fixed once [integrated into
/// new widget hierarchy](https://github.com/enso-org/enso/issues/5923).
#[derive(Clone, CloneRef, Debug)]
pub struct Widget {
    config_frp: Frp,
    list:       ListEditor<display::object::Instance>,
}

impl Widget {
    /// A gap between the `activation_shape` and `elements` view.
    const GAP: f32 = 3.0;

    fn init_list_updates(self, app: &Application, widgets_frp: &WidgetsFrp) -> Self {
        let config_frp = &self.config_frp;
        let network = &config_frp.network;
        let list = &self.list;
        frp::extend! { network
            init <- source_();
        }
        init.emit(());
        self
    }

    fn update_list(&mut self, children: impl Iterator<Item = super::Child>) {
        // let mut widgets_kept = 0;
        // let mut children = children.fuse();
        // for element in self.list.items() {
        //     match children.next() {
        //         Some(child) => {
        //             widgets_kept += 1;
        //             if child.id != element.widget_id() {
        //                 element.set_widget(child);
        //             }
        //         }
        //         None => {
        //             self.list.remove(widgets_kept);
        //         }
        //     }
        // }
        // for child in children {
        //     let element = Element::new(child);
        //     self.list.push(element);
        // }
        self.list.replace_list(children.map(|child| child.root_object));
    }

    // fn construct_code(list: &ListEditor<>) -> String {
    //     let subwidgets = list.items().into_iter();
    //     let mut subwidgets_codes = subwidgets.map(|sub| sub.content.value().to_string());
    //     format!("[{}]", subwidgets_codes.join(","))
    // }
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
        Self { config_frp, list } //.init_list_updates(app, widgets_frp)
    }

    fn configure(&mut self, cfg: &Config, ctx: super::ConfigContext) {
        let current_value: Option<ImString> = Some(ctx.expression_at(ctx.span_node.span()).into());
        self.config_frp.current_value(current_value);
        self.config_frp.current_crumbs(ctx.span_node.crumbs.clone());

        let child_level = ctx.info.nesting_level.next_if(ctx.span_node.is_argument());
        let children_iter = ctx.span_node.children_iter();
        let children = children_iter.filter(|node| node.is_argument()).map(|node| {
            ctx.builder.child_widget_of_type(node, child_level, cfg.item_widget.as_deref())
        });
        self.update_list(children);
    }
}
