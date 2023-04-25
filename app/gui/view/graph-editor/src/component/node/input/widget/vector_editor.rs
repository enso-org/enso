//! Module dedicated to the Vector Editor widget. The main structure is [`Model`] which is one of
//! the [KindModel](crate::component::node::widget::KindModel) variants.
//!
//! Currently the view is a simle [`Elements`] component, which will be replaced with a rich
//! view in [future tasks](https://github.com/enso-org/enso/issues/5631).

use crate::prelude::*;

use crate::component::node::input::widget::triangle;
use crate::component::node::input::widget::SampledFrp;
use crate::component::node::input::widget::ACTIVATION_SHAPE_COLOR;
use crate::component::node::input::widget::ACTIVATION_SHAPE_SIZE;

use ensogl::application::Application;
use ensogl::control::io::mouse;
use ensogl::data::color;
use ensogl::display;
use ensogl_component::list_editor::ListEditor;
use ensogl_component::text::Text;



// =============
// === Model ===
// =============

/// A model for the vector editor widget.
///
/// Currently it displays an activation shape (a triangle) which, on click, displays the widget
/// view. The view is a [`ListEditor`] - see its documentation for available GUI actions. Currently
/// only adding new elements is supported.
///
/// The component does not handle nested arrays well. They should be fixed once [integrated into
/// new widget hierarchy](https://github.com/enso-org/enso/issues/5923).
#[derive(Clone, CloneRef, Debug)]
pub struct Model {
    network:           frp::Network,
    display_object:    display::object::Instance,
    list_container:    display::object::Instance,
    activation_shape:  triangle::View,
    list:              ListEditor<Text>,
    /// FRP input informing about the port size.
    pub set_port_size: frp::Source<Vector2>,
}

impl Model {
    /// A gap between the `activation_shape` and `elements` view.  
    const GAP: f32 = 3.0;

    /// Create Model for Vector Editor widget.
    pub fn new(app: &Application, parent: &display::object::Instance, frp: &SampledFrp) -> Self {
        let network = frp::Network::new("vector_editor::Model");
        let display_object = display::object::Instance::new();
        let list_container = display::object::Instance::new();
        let activation_shape = triangle::View::new();
        let list = ListEditor::new(&app.cursor);

        activation_shape.set_size(ACTIVATION_SHAPE_SIZE);
        display_object.add_child(&list_container);
        display_object.add_child(&activation_shape);
        display_object
            .use_auto_layout()
            .set_column_count(1)
            .set_gap_y(Self::GAP)
            .set_children_alignment_center();
        display_object.set_size_hug();
        parent.add_child(&display_object);

        frp::extend! { network
            set_port_size <- source::<Vector2>();
        }

        Self { network, display_object, list_container, activation_shape, list, set_port_size }
            .init_toggle(frp)
            .init_list_updates(app, frp)
            .init_port_size_update()
    }

    fn init_toggle(self, frp: &SampledFrp) -> Self {
        let network = &self.network;
        let display_object = &self.display_object;
        let activation_shape = &self.activation_shape;
        let list_container = &self.list_container;
        let list = &self.list;
        let dot_clicked = self.activation_shape.on_event::<mouse::Down>();

        frp::extend! { network
            init <- source_();
            toggle_focus <- dot_clicked.map(f!([display_object](_) !display_object.is_focused()));
            set_focused <- any(toggle_focus, frp.set_focused);
            eval set_focused([display_object, list_container, list](focus) match focus {
                true => {
                    display_object.focus();
                    list_container.add_child(&list);
                },
                false => {
                    display_object.blur();
                    list_container.remove_child(&list);
                },
            });

            set_visible <- all(&frp.set_visible, &init)._0();
            shape_alpha <- set_visible.map(|visible| if *visible { 1.0 } else { 0.0 });
            shape_color <- shape_alpha.map(|a| ACTIVATION_SHAPE_COLOR.with_alpha(*a));
            eval shape_color([activation_shape] (color) {
                activation_shape.color.set(color::Rgba::from(color).into());
            });
        }
        init.emit(());
        self
    }

    fn init_list_updates(self, app: &Application, frp: &SampledFrp) -> Self {
        let network = &self.network;
        let list = &self.list;
        frp::extend! { network
            init <- source_();
            value <- all(frp.set_current_value, init)._0();
            non_empty_value <- value.filter_map(|v| v.clone());
            empty_value <- value.filter_map(|v| v.is_none().then_some(()));
            eval non_empty_value ([list, app](val) Self::update_list(&app, val.as_str(), &list));
            eval_ empty_value ([list] Self::clear_list(&list));

            code_changed_by_user <-
                list.request_new_item.map(f_!([app, list] Self::push_new_element(&app, &list)));
            frp.out_value_changed <+ code_changed_by_user.map(f_!([list] {
                Some(ImString::new(Self::construct_code(&list)))
            }));
        }
        init.emit(());
        self
    }

    fn init_port_size_update(self) -> Self {
        let network = &self.network;
        let display_object = &self.display_object;
        let on_transformed = self.display_object.on_transformed.clone_ref();
        let set_port_size = &self.set_port_size;
        frp::extend! { network
            widget_size <- on_transformed.map(f!((()) display_object.computed_size())).on_change();
            port_and_widget_size <- all(set_port_size, &widget_size);
            eval port_and_widget_size ([display_object]((port_sz, sz)) {
                display_object.set_x(port_sz.x() / 2.0 - sz.x() / 2.0);
                display_object.set_y(-port_sz.y() - sz.y() - 5.0);
            });
        }
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
