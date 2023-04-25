//! Module dedicated to the List Editor widget. The main structure is [`Model`] which is one of
//! the [KindModel](crate::component::node::widget::KindModel) variants.
//!
//! Currently the view is a simple [`Elements`] component, which will be replaced with a rich
//! view in [future tasks](https://github.com/enso-org/enso/issues/5631).

use crate::prelude::*;

use crate::component::node::input::widget::single_choice::triangle;
use crate::component::node::input::widget::single_choice::ACTIVATION_SHAPE_SIZE;
use crate::component::node::input::widget::Configuration;
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
    config_frp:       Frp,
    display_object:   display::object::Instance,
    child_container:  display::object::Instance,
    list_container:   display::object::Instance,
    activation_shape: triangle::View,
    list:             ListEditor<Text>,
}

impl Widget {
    /// A gap between the `activation_shape` and `elements` view.
    const GAP: f32 = 3.0;

    /// Create Model for Vector Editor widget.
    pub fn new(app: &Application, widgets_frp: &WidgetsFrp, styles: &StyleWatch) -> Self {
        let display_object = display::object::Instance::new();
        let list_container = display::object::Instance::new();
        let child_container = display::object::Instance::new();
        let activation_shape = triangle::View::new();
        let list = ListEditor::new(&app.cursor);

        let toggle_color = styles.get_color(theme::widget::activation_shape::connected);
        activation_shape.set_size(ACTIVATION_SHAPE_SIZE);
        activation_shape.color.set(toggle_color.into());

        display_object.add_child(&child_container);
        display_object.add_child(&list_container);
        display_object.add_child(&activation_shape);
        display_object
            .use_auto_layout()
            .set_column_count(1)
            .set_gap_y(Self::GAP)
            .set_children_alignment_center();
        display_object.set_size_hug();

        let config_frp = Frp::new();
        Self { config_frp, display_object, child_container, list_container, activation_shape, list }
            .init_toggle(widgets_frp)
            .init_list_updates(app, widgets_frp)
    }

    fn init_toggle(self, widgets_frp: &WidgetsFrp) -> Self {
        let network = &self.config_frp.network;
        let display_object = &self.display_object;
        let list_container = &self.list_container;
        let list = &self.list;
        let dot_clicked = self.activation_shape.on_event::<mouse::Down>();
        let focus_in = self.display_object.on_event::<event::FocusIn>();
        let focus_out = self.display_object.on_event::<event::FocusOut>();

        frp::extend! { network
            init <- source_();
            set_focused <- dot_clicked.map(f!([display_object](_) !display_object.is_focused()));
            eval set_focused([display_object](focus) match focus {
                true => display_object.focus(),
                false => display_object.blur(),
            });

            readonly_set <- widgets_frp.set_read_only.on_true();
            do_open <- focus_in.gate_not(&widgets_frp.set_read_only);
            do_close <- any_(focus_out, readonly_set);
            is_open <- bool(&do_close, &do_open).on_change();

            eval is_open([list_container, list](open) match open {
                true => list_container.add_child(&list),
                false => list_container.remove_child(&list),
            });
        }
        init.emit(());
        self
    }

    fn init_list_updates(self, app: &Application, widgets_frp: &WidgetsFrp) -> Self {
        let config_frp = &self.config_frp;
        let network = &config_frp.network;
        let list = &self.list;
        frp::extend! { network
            init <- source_();
            value <- all(config_frp.current_value, init)._0();
            non_empty_value <- value.filter_map(|v| v.clone());
            empty_value <- value.filter_map(|v| v.is_none().then_some(()));
            eval non_empty_value ([list, app](val) Self::update_list(&app, val.as_str(), &list));
            eval_ empty_value ([list] Self::clear_list(&list));

            code_changed_by_user <-
                list.request_new_item.map(f_!([app, list] Self::push_new_element(&app, &list)));
            value_changed <- code_changed_by_user.map(f_!([list] {
                Some(ImString::new(Self::construct_code(&list)))
            }));
            widgets_frp.value_changed <+ value_changed.map2(&config_frp.current_crumbs,
                move |t: &Option<ImString>, crumbs: &span_tree::Crumbs| (crumbs.clone(), t.clone())
            );

        }
        init.emit(());
        self
    }

    fn clear_list(list: &ListEditor<Text>) {
        for _ in 0..list.items().len() {
            list.remove(0);
        }
    }

    fn update_list(app: &Application, code: &str, list: &ListEditor<Text>) {
        let mut codes = Self::parse_array_code(code).fuse();
        let mut widgets_kept = 0;
        for widget in list.items() {
            match codes.next() {
                Some(code) => {
                    widgets_kept += 1;
                    if widget.content.value().to_string() != code {
                        widget.set_content(ImString::new(code));
                    }
                }
                None => {
                    list.remove(widgets_kept);
                }
            }
        }
        for code in codes {
            let widget = Text::new(app);
            widget.set_content(ImString::new(code));
            app.display.default_scene.layers.label.add(&widget);
            list.push(widget);
        }
    }

    fn push_new_element(app: &Application, list: &ListEditor<Text>) {
        let widget = Text::new(app);
        widget.set_content("_");
        list.push(widget);
    }

    fn construct_code(list: &ListEditor<Text>) -> String {
        let subwidgets = list.items().into_iter();
        let mut subwidgets_codes = subwidgets.map(|sub| sub.content.value().to_string());
        format!("[{}]", subwidgets_codes.join(","))
    }

    fn parse_array_code(code: &str) -> impl Iterator<Item = &str> {
        let looks_like_array = code.starts_with('[') && code.ends_with(']');
        let opt_iterator = looks_like_array.then(|| {
            let without_braces = code.trim_start_matches([' ', '[']).trim_end_matches([' ', ']']);
            let elements_with_trailing_spaces = without_braces.split(',');
            elements_with_trailing_spaces.map(|s| s.trim())
        });
        opt_iterator.into_iter().flatten()
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
        Self::new(ctx.app(), ctx.frp(), ctx.styles())
    }

    fn configure(&mut self, _: &Config, ctx: super::ConfigContext) {
        let current_value: Option<ImString> = Some(ctx.expression_at(ctx.span_node.span()).into());
        self.config_frp.current_value(current_value);
        self.config_frp.current_crumbs(ctx.span_node.crumbs.clone());

        let child_level = ctx.info.nesting_level.next_if(ctx.span_node.is_argument());
        let label_meta = super::Configuration::always(super::label::Config);
        let child = ctx.builder.child_widget_of_type(ctx.span_node, child_level, Some(&label_meta));
        self.child_container.replace_children(&[child]);
    }
}
